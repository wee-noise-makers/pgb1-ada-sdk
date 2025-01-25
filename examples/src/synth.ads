with MIDI;
with HAL;
with System;

package Synth is

   procedure Note_On (Key : MIDI.MIDI_Key; Velocity : MIDI.MIDI_Data);
   procedure Note_Off (Key : MIDI.MIDI_Key);

   procedure Output_Callback (Buffer             : out System.Address;
                              Stereo_Point_Count : out HAL.UInt32);

   procedure Input_Callback (Buffer             : out System.Address;
                             Stereo_Point_Count : out HAL.UInt32);

   procedure Update_Buffer;

   function Got_MIDI_Input return Boolean;

end Synth;
