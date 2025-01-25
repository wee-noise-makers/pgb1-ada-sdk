with Ada.Unchecked_Conversion;

with System.Storage_Elements;
with Noise_Nugget_SDK.MIDI;
with Noise_Nugget_SDK.Audio;
with MIDI;
with Tresses.Resources;
with RP.Multicore.FIFO;
with RP.Device;
with RP.DMA;
with RP2040_SVD.Interrupts;
with RP.Timer;

with Tresses.Macro;
with Tresses.Interfaces;
with Tresses.FX.Reverb;
with Tresses.FX.Voice.Stereo_Reverb;
with Tresses.FX.Voice.Stereo_Bitcrusher;
with Tresses.FX.Voice.Stereo_Filter;
with Tresses.FX.Voice.Stereo_Overdrive;

package body Tresses_Perf_Core_1 is

   package MIDI_Inst is new Noise_Nugget_SDK.MIDI
     (UART           => RP.Device.UART_1'Access,
      UART_Interrupt => RP2040_SVD.Interrupts.UART1_Interrupt,
      DMA_TX_Trigger => RP.DMA.UART1_TX,
      TX_Pin         => 8,
      RX_Pin         => 9);

   package Reverb_Pck is new Tresses.FX.Reverb;
   package Stereo_Reverb is new Tresses.FX.Voice.Stereo_Reverb (Reverb_Pck);

   type Perf_Mode is (Synth, FX);
   Current_Mode : Perf_Mode := Perf_Mode'First;

   TM : Tresses.Macro.Instance;

   type FX_Access
   is access all Tresses.Interfaces.Four_Params_Stereo_FX'Class;

   Reverb_Buffer : aliased Reverb_Pck.Reverb_Buffer;
   Reverb_Inst : aliased Stereo_Reverb.Instance (Reverb_Buffer'Access);
   Bitcrush_Inst : aliased Tresses.FX.Voice.Stereo_Bitcrusher.Instance;
   Drive_Inst : aliased Tresses.FX.Voice.Stereo_Overdrive.Instance;
   Filter_Inst : aliased Tresses.FX.Voice.Stereo_Filter.Instance;

   FX_Voices : constant array (FX_Kind) of FX_Access :=
     (FX_Reverb   => Reverb_Inst'Access,
      FX_Bitcrush => Bitcrush_Inst'Access,
      FX_Drive    => Drive_Inst'Access,
      FX_Filter   => Filter_Inst'Access);

   Start_Time, End_Time : RP.Timer.Time;
   G_CPU_Load : Float := 0.0 with Volatile, Atomic;
   G_Max_CPU_Load : Float := 0.0 with Volatile, Atomic;

   procedure Main;

   -----------------
   -- Entry_Point --
   -----------------

   function Entry_Point return HAL.UInt32
   is (HAL.UInt32 (System.Storage_Elements.To_Integer (Main'Address)));

   Last_On : MIDI.MIDI_Key := 0;

   Buffer_A, Buffer_B : Tresses.Mono_Buffer (1 .. Synth_Buffer_Size);

   type Stereo_Point is record
      L, R : Tresses.Mono_Point;
   end record
     with Size => 32;

   type Stereo_Buffer is array (Buffer_A'Range) of Stereo_Point
     with Size => Synth_Buffer_Size * 32;

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

   ---------------
   -- Set_Param --
   ---------------

   procedure Set_Param (Id : Param_Id; Val : Param_Range) is
   begin
      Params (Id) := Val;
      TM.Set_Param (Id, Val);
      for FX of FX_Voices loop
         FX.Set_Param (Id, Val);
      end loop;
   end Set_Param;

   ------------
   -- Engine --
   ------------

   function Engine return String is
   begin
      case Current_Mode is
         when Synth => return Tresses.Img (TM.Engine);
         when FX    =>
            case Current_FX is
               when FX_Reverb   => return "Reverb";
               when FX_Filter   => return "Filter";
               when FX_Bitcrush => return "Bitcrush";
               when FX_Drive    => return "Drive";
            end case;
      end case;
   end Engine;

   -----------------------
   -- Param_Short_Label --
   -----------------------

   function Param_Short_Label (Id : Tresses.Param_Id)
                               return Tresses.Short_Label
   is
   begin
      case Current_Mode is
         when Synth => return TM.Param_Short_Label (Id);
         when FX => return FX_Voices (Current_FX).Param_Short_Label (Id);
      end case;
   end Param_Short_Label;

   --------------
   -- CPU_Load --
   --------------

   function CPU_Load return Float
   is (G_CPU_Load);

   function Max_CPU_Load return Float
   is (G_Max_CPU_Load);

   -------------------------
   -- Handle_MIDI_Message --
   -------------------------

   procedure Handle_MIDI_Message (Msg : MIDI.Message) is
      use MIDI;
   begin
      case Msg.Kind is

         when MIDI.Note_On =>
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

               when 4 =>
                  case Msg.Controller_Value is
                     when 0 | 1 =>
                        case Current_Mode is
                           when Synth =>
                              if TM.Engine = Engines'Last then
                                 Current_Mode := FX;
                                 Current_FX := FX_Kind'First;
                              else
                                 TM.Next_Engine;
                              end if;
                           when FX =>
                              if Current_FX = FX_Kind'Last then
                                 Current_Mode := Synth;
                                 TM.Set_Engine (Engines'First);
                              else
                                 Current_FX := FX_Kind'Succ (Current_FX);
                              end if;
                        end case;

                        G_Max_CPU_Load := 0.0;

                     when 2 | 3 =>
                        case Current_Mode is
                           when Synth =>
                              if TM.Engine = Engines'First then
                                 Current_Mode := FX;
                                 Current_FX := FX_Kind'Last;
                              else
                                 TM.Prev_Engine;
                              end if;
                           when FX =>
                              if Current_FX = FX_Kind'First then
                                 Current_Mode := Synth;
                                 TM.Set_Engine (Engines'Last);
                              else
                                 Current_FX := FX_Kind'Pred (Current_FX);
                              end if;
                        end case;

                        G_Max_CPU_Load := 0.0;
                     when others => null;
                  end case;

               when 5 .. 8 =>
                  declare
                     Id : constant Param_Id :=
                       Param_Id (Msg.Controller - 4);

                     Small_Delta : constant := 1000;
                     Big_Delta   : constant := 5000;

                     Val : Param_Range := Params (Id);
                  begin

                     case Msg.Controller_Value is
                        when CC_Plus_Small =>
                           if Val <= Param_Range'Last - Small_Delta then
                              Val := Val + Small_Delta;
                           end if;
                        when CC_Plus_Big =>
                           if Val <= Param_Range'Last - Big_Delta then
                              Val := Val + Big_Delta;
                           end if;

                        when CC_Minus_Small =>
                           if Val >= Param_Range'First + Small_Delta then
                              Val := Val - Small_Delta;
                           end if;
                        when CC_Minus_Big =>
                           if Val >= Param_Range'First + Big_Delta then
                              Val := Val - Big_Delta;
                           end if;

                        when others => null;
                     end case;

                     Set_Param (Id, Val);
                  end;
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

      procedure Process_MIDI_Input
      is new MIDI_Inst.For_Each_Input_Message
        (Handle_MIDI_Message);

      Buffer_To_Write : Buffer_Id;
   begin

      if New_Buffer_Needed then
         Buffer_To_Write := Ready_To_Play + 1;
         New_Buffer_Needed := False;

         Process_MIDI_Input;

         Start_Time := RP.Timer.Clock;
         case Current_Mode is
            when Synth =>
               TM.Render (Buffer_A, Buffer_B);

            when FX =>

               for Index in Stereo_Buffer'Range loop
                  Buffer_A (Index) :=
                    Flip_Buffers_Input (Ready_To_Play) (Index).L;
                  Buffer_B (Index) :=
                    Flip_Buffers_Input (Ready_To_Play) (Index).R;
               end loop;

               FX_Voices (Current_FX).Render (Buffer_A, Buffer_B);

         end case;
         End_Time := RP.Timer.Clock;

         for Index in Stereo_Buffer'Range loop
            Flip_Buffers (Buffer_To_Write) (Index).L := Buffer_A (Index);
            Flip_Buffers (Buffer_To_Write) (Index).R := Buffer_A (Index);
         end loop;

         Ready_To_Play := Buffer_To_Write;

         declare
            use RP.Timer;

            Synthesis_Duration : constant RP.Timer.Time :=
              End_Time - Start_Time;

            Synthesis_Duration_Float : constant Float :=
              Float (Synthesis_Duration) / 1_000_000.0;

            Synthesized_Time : constant Float :=
              (1.0 / Tresses.Resources.SAMPLE_RATE_REAL *
                 Float (Synth_Buffer_Size));
         begin
            G_CPU_Load :=
              (Synthesis_Duration_Float / Synthesized_Time) * 100.0;

            if G_CPU_Load > G_Max_CPU_Load then
               G_Max_CPU_Load := G_CPU_Load;
            end if;
         end;
      end if;
   end Update_Buffer;

   ----------
   -- Main --
   ----------

   procedure Main is
      pragma Warnings (Off, "have different sizes");
      function To_MIDI_Msg
      is new Ada.Unchecked_Conversion (HAL.UInt32, MIDI.Message);
      pragma Warnings (On, "have different sizes");

      Data : HAL.UInt32;
   begin
      if not Noise_Nugget_SDK.Audio.Start
        (Tresses.Resources.SAMPLE_RATE,
         Output_Callback => Output_Callback'Access,
         Input_Callback  => Input_Callback'Access)
      then
         raise Program_Error with "MDM";
      end if;

      Noise_Nugget_SDK.Audio.Set_HP_Volume (0.7, 0.7);

      Set_Param (1, 16_000);
      Set_Param (2, 16_000);
      Set_Param (3, 16_000);
      Set_Param (4, 16_000);

      loop
         Update_Buffer;

         while RP.Multicore.FIFO.Try_Pop (Data) loop
            declare
               use MIDI;

               Msg : constant Message := To_MIDI_Msg (Data);
            begin
               Handle_MIDI_Message (Msg);
            end;
         end loop;
      end loop;
   end Main;

end Tresses_Perf_Core_1;
