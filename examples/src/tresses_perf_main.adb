with Core_1_Utils;
with Tresses_Perf_Core_1;

with HAL; use HAL;
with MIDI; use MIDI;
with Ada.Unchecked_Conversion;
with RP.Multicore.FIFO;
with RP.Device;

with PGB1; use PGB1;
with Noise_Nugget_SDK; use Noise_Nugget_SDK;

procedure Tresses_Perf_Main is

   package Core_1 renames Tresses_Perf_Core_1;

   function CPU_Load_Img (L : Float) return String is
      type Load_Img is delta 0.01 range 0.0 .. 10000.0;
      --  Use a fixed point type to get a 'Img without
      --  scientific notation...

      Clamp : Load_Img;

   begin
      if L > Float (Load_Img'Last) then
         Clamp := Load_Img'Last;
      elsif L < Float (Load_Img'First) then
         Clamp := Load_Img'First;
      else
         Clamp := Load_Img (L);
      end if;

      return Clamp'Img;
   end CPU_Load_Img;

   procedure Send_MIDI (M : MIDI.Message) is
      pragma Warnings (Off, "have different sizes");
      function To_U32 is new Ada.Unchecked_Conversion (MIDI.Message,
                                                       HAL.UInt32);
      pragma Warnings (On, "have different sizes");

   begin
      RP.Multicore.FIFO.Push_Blocking (To_U32 (M));
   end Send_MIDI;

   procedure Send_CC (Ctrl : MIDI_Data; Value : MIDI_Data) is
   begin
      Send_MIDI ((Kind => MIDI.Continous_Controller,
                  Chan => 0,
                  Controller => Ctrl,
                  Controller_Value => Value));
   end Send_CC;

   procedure Send_Note_On (Key : MIDI_Key) is
   begin
      Send_MIDI ((Kind => Note_On,
                  Chan => 0,
                  Key => Key,
                  Velocity => MIDI_Data'Last));
   end Send_Note_On;

   procedure Send_Note_Off (Key : MIDI_Key) is
   begin
      Send_MIDI ((Kind => Note_Off,
                  Chan => 0,
                  Key => Key,
                  Velocity => MIDI_Data'Last));
   end Send_Note_Off;

   type Settings is (Engine, P1, P2, P3, P4);

   function Img (S : Settings) return String
   is (case S is
          when Engine => Core_1.Engine,
          when P1 =>
             Core_1.Param_Short_Label (1) & Core_1.Params (1)'Img,
          when P2 =>
             Core_1.Param_Short_Label (2) & Core_1.Params (2)'Img,
          when P3 =>
             Core_1.Param_Short_Label (3) & Core_1.Params (3)'Img,
          when P4 =>
             Core_1.Param_Short_Label (4) & Core_1.Params (4)'Img);

   Selected : Settings := Settings'First;

   Octave_Down : Button renames PGB1.B1;
   Octave_Up : Button renames PGB1.B8;

   subtype Keys is MIDI_Key range 0 .. 12;
   Keys_Buttons : constant array (Keys) of Button :=
     (0  => PGB1.B9,
      1  => PGB1.B2,
      2  => PGB1.B10,
      3  => PGB1.B3,
      4  => PGB1.B11,
      5  => PGB1.B12,
      6  => PGB1.B5,
      7  => PGB1.B13,
      8  => PGB1.B6,
      9  => PGB1.B14,
      10 => PGB1.B7,
      11 => PGB1.B15,
      12 => PGB1.B16);

   Octave_Offset : MIDI_Key := MIDI.C3;

   Events : PGB1.BM_Definition.Button_Event_Array;
begin

   LED_Strip.Clear;
   --  White keys
   for L in LED_Strip.LED_Id range 16 .. 23 loop
      LED_Strip.Set_RGB (L, 100, 100, 100);
   end loop;
   --  Black keys
   for L in LED_Strip.LED_Id range 7 .. 8 loop
      LED_Strip.Set_Hue (L, LED_Strip.Cyan);
   end loop;
   for L in LED_Strip.LED_Id range 10 .. 12 loop
      LED_Strip.Set_Hue (L, LED_Strip.Cyan);
   end loop;

   LED_Strip.Set_Hue (6, LED_Strip.Green);
   LED_Strip.Set_Hue (13, LED_Strip.Red);
   LED_Strip.Update;

   --  Start Core 1 running the FX
   Core_1_Utils.Start (Tresses_Perf_Core_1.Entry_Point);

   loop
      PGB1.Button_State.Update;
      Events := PGB1.Button_State.Events;

      if Events (PAD_Up) = Falling then
         Selected := (if Selected = Settings'First
                      then Settings'Last
                      else Settings'Pred (Selected));
      elsif Events (PAD_Down) = Falling then
         Selected := (if Selected = Settings'Last
                      then Settings'First
                      else Settings'Succ (Selected));
      end if;

      if Events (PAD_Left) = Falling then
         Send_CC (4 + Selected'Enum_Rep,
                  (if Events (PGB1.PAD_A) = Down
                   then Core_1.CC_Minus_Small
                   else  Core_1.CC_Minus_Big));
      elsif Events (PAD_Right) = Falling then
         Send_CC (4 + Selected'Enum_Rep,
                  (if Events (PGB1.PAD_A) = Down
                   then Core_1.CC_Plus_Small
                   else  Core_1.CC_Plus_Big));
      end if;

      if Events (Octave_Down) = Falling then
         if Octave_Offset >= 12 then
            Octave_Offset := Octave_Offset - 12;
         end if;
      elsif Events (Octave_Up) = Falling then
         if Octave_Offset <= MIDI_Key'Last - 24 then
            Octave_Offset := Octave_Offset + 12;
         end if;
      end if;

      for K in Keys loop
         if Events (Keys_Buttons (K)) = Falling then
            Send_Note_On (Octave_Offset + K);
         elsif Events (Keys_Buttons (K)) = Rising then
            Send_Note_Off (Octave_Offset + K);
         end if;
      end loop;

      Screen.Clear;
      for Set in Settings loop
         Print (0, 10 * Set'Enum_Rep,
                (if Selected = Set then ">" else " ") & Img (Set));
      end loop;

      Print (0, 50, "Oct:" & MIDI_Key'Image (Octave_Offset / 12));
      Print (63, 20, "Load:" & CPU_Load_Img (Tresses_Perf_Core_1.CPU_Load));
      Print (63, 30, "Max:" & CPU_Load_Img (Tresses_Perf_Core_1.Max_CPU_Load));

      Screen.Update;
      RP.Device.Timer.Delay_Milliseconds (30);
   end loop;
end Tresses_Perf_Main;
