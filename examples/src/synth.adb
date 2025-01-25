with Interfaces;
with Tresses; use Tresses;
with Tresses.Macro;
with Tresses.FX.Reverb;

with Noise_Nugget_SDK.MIDI;

with Ada.Unchecked_Conversion;

package body Synth is

   We_Got_MIDI_Input : Boolean := False
     with Volatile;

   package Reverb_Pck is new Tresses.FX.Reverb;

   TM : Tresses.Macro.Instance;
   Reverb : Reverb_Pck.Instance;
   Last_On : MIDI.MIDI_Key := 0;

   Buffer_A, Buffer_B : Tresses.Mono_Buffer (1 .. 64);
   Buffer_L, Buffer_R : Tresses.Mono_Buffer (1 .. 64);

   type Stereo_Point is record
      L, R : Tresses.Mono_Point;
   end record
     with Size => 32;

   type Stereo_Buffer is array (Buffer_A'Range) of Stereo_Point
     with Size => 64 * 32;

   type Buffer_Id is mod 2;
   Flip_Buffers : array (Buffer_Id) of Stereo_Buffer :=
     (others => (others => (0, 0)));
   Ready_To_Play : Buffer_Id := Buffer_Id'First;
   New_Buffer_Needed : Boolean := True
     with Volatile;

   Flip_Buffers_Input : array (Buffer_Id) of Stereo_Buffer :=
     (others => (others => (0, 0)));
   Ready_To_Read : Buffer_Id := Buffer_Id'First;

   function MIDI_Param (Val : MIDI.MIDI_Data) return Param_Range
   is (Param_Range (Val) *
       (Param_Range'Last / Param_Range (MIDI.MIDI_Data'Last)));

   -------------
   -- Note_On --
   -------------

   procedure Note_On (Key : MIDI.MIDI_Key; Velocity : MIDI.MIDI_Data) is
   begin
      TM.Set_Pitch (Tresses.MIDI_Pitch (Key));
      TM.Note_On (MIDI_Param (Velocity));
      Last_On := Key;
   end Note_On;

   --------------
   -- Note_Off --
   --------------

   procedure Note_Off (Key : MIDI.MIDI_Key) is
      use MIDI;
   begin
      if Last_On = Key then
         TM.Note_Off;
      end if;
   end Note_Off;

   -------------------------
   -- Handle_MIDI_Message --
   -------------------------

   procedure Handle_MIDI_Message (Msg : MIDI.Message) is
   begin
      case Msg.Kind is

         when MIDI.Note_On =>
            We_Got_MIDI_Input := True;
            Note_On (Msg.Key, Msg.Velocity);

         when MIDI.Note_Off =>
            Note_Off (Msg.Key);

         when MIDI.Continous_Controller =>
            case Msg.Controller is
               when 0 =>
                  TM.Set_Param (1, MIDI_Param (Msg.Controller_Value));
               when 1 =>
                  TM.Set_Param (2, MIDI_Param (Msg.Controller_Value));
               when 2 =>
                  TM.Set_Param (3, MIDI_Param (Msg.Controller_Value));
               when 3 =>
                  TM.Set_Param (4, MIDI_Param (Msg.Controller_Value));
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
   end Handle_MIDI_Message;

   ---------------------
   -- Output_Callback --
   ---------------------

   procedure Output_Callback (Buffer             : out System.Address;
                       Stereo_Point_Count : out HAL.UInt32)
   is
      Selected_Buffer : constant Buffer_Id := Ready_To_Play;
   begin
      Buffer := Flip_Buffers (Selected_Buffer)'Address;
      Stereo_Point_Count := Flip_Buffers (Selected_Buffer)'Length;
      New_Buffer_Needed := True;
   end Output_Callback;

   --------------------
   -- Input_Callback --
   --------------------

   procedure Input_Callback (Buffer             : out System.Address;
                             Stereo_Point_Count : out HAL.UInt32)
   is
      Selected_Buffer : constant Buffer_Id := Ready_To_Read;
   begin
      Buffer := Flip_Buffers_Input (Selected_Buffer)'Address;
      Stereo_Point_Count := Flip_Buffers (Selected_Buffer)'Length;

      Ready_To_Read := Ready_To_Read + 1;
   end Input_Callback;

   -------------------
   -- Update_Buffer --
   -------------------

   procedure Update_Buffer is

      use Interfaces;

      procedure Process_MIDI_Input
      is new Noise_Nugget_SDK.MIDI.For_Each_Input_Message
        (Handle_MIDI_Message);

      Buffer_To_Write : Buffer_Id;
   begin

      if New_Buffer_Needed then
         Buffer_To_Write := Ready_To_Play + 1;
         New_Buffer_Needed := False;

         Process_MIDI_Input;

         TM.Render (Buffer_A, Buffer_B);

         for Index in Stereo_Buffer'Range loop
            Buffer_L (Index) :=
              Flip_Buffers_Input (Ready_To_Read)(Index).L +
              Buffer_A (Index) / 2;

            Buffer_R (Index) :=
              Flip_Buffers_Input (Ready_To_Read)(Index).R +
              Buffer_A (Index) / 2;
         end loop;

         --  Reverb_Pck.Set_Amount (Reverb,  Param_Range (0.2 * 32_767.0));
         --  Reverb_Pck.Set_Gain (Reverb,  Param_Range (0.8 * 32_767.0));
         --  Reverb_Pck.Set_Low_Pass (Reverb,  Param_Range (0.3 * 32_767.0));
         Reverb_Pck.Process (Reverb, Buffer_L, Buffer_R);

         for Index in Stereo_Buffer'Range loop
            Flip_Buffers (Buffer_To_Write) (Index).L := Buffer_L (Index);
            Flip_Buffers (Buffer_To_Write) (Index).R := Buffer_R (Index);
         end loop;

         Ready_To_Play := Buffer_To_Write;
      end if;
   end Update_Buffer;

   --------------------
   -- Got_MIDI_Input --
   --------------------

   function Got_MIDI_Input return Boolean
   is (We_Got_MIDI_Input);

begin
   TM.Set_Engine (Tresses.Voice_Pluck_Bass);
   TM.Set_Param (1, Param_Range'Last / 2);
   TM.Set_Param (2, Param_Range'Last / 2);
   TM.Set_Param (3, 0);
   TM.Set_Param (4, Param_Range'Last / 2);
end Synth;
