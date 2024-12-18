with RP.Device;
with RP.DMA;

with Noise_Nugget_SDK.Screen.SSD1306;
with Noise_Nugget_SDK.Button_Matrix_Definition;
with Noise_Nugget_SDK.Button_Matrix;
with Noise_Nugget_SDK.WS2812;

package PGB1 is

   type Button is (B1, B2, B3, B4, B5, B6, B7, B8,
                   B9, B10, B11, B12, B13, B14, B15, B16,
                   Rec, Play,
                   Menu, Func, Step_Button, Track_Button, Pattern_Button,
                   Song_Button,
                   PAD_Up, PAD_Down, PAD_Left, PAD_Right, PAD_A, PAD_B);

   package Screen is new Noise_Nugget_SDK.Screen.SSD1306
     (SPI         => RP.Device.SPI_1'Access,
      DMA_Trigger => RP.DMA.SPI1_TX,
      N_Reset_Pin => 13,
      DC_Pin      => 12,
      SCK_Pin     => 10,
      MOSI_Pin    => 11);

   Font_Width : constant := 6;
   Font_Height : constant := 8;

   procedure Print (X_Offset    : Integer;
                    Y_Offset    : Integer;
                    C           : Character);

   procedure Print (X_Offset    : Integer;
                    Y_Offset    : Integer;
                    Str         : String);

   package BM_Definition is new Noise_Nugget_SDK.Button_Matrix_Definition
     (Row_Count    => 6,
      Column_Count => 5,
      Button_Id_Type => Button);

   package Button_State is new Noise_Nugget_SDK.Button_Matrix
     (Definition => BM_Definition,
      Column_Pins => (1 => 21,
                      2 => 22,
                      3 => 26,
                      4 => 23,
                      5 => 29),
      Row_Pins    => (1 => 20,
                      2 => 18,
                      3 => 19,
                      4 => 24,
                      5 => 25,
                      6 => 27),
      Mapping     => (PAD_Left       => (1, 1),
                      Track_Button   => (1, 2),
                      Step_Button    => (1, 3),
                      Rec            => (1, 4),
                      Play           => (1, 5),
                      PAD_A          => (1, 6),
                      PAD_Down       => (2, 1),
                      B1             => (2, 2),
                      B9             => (2, 3),
                      B16            => (2, 4),
                      B8             => (2, 5),
                      Func           => (2, 6),
                      PAD_Up         => (3, 1),
                      B2             => (3, 2),
                      B10            => (3, 3),
                      B15            => (3, 4),
                      B7             => (3, 5),
                      Pattern_Button => (3, 6),
                      PAD_Right      => (4, 1),
                      B3             => (4, 2),
                      B11            => (4, 3),
                      B14            => (4, 4),
                      B6             => (4, 5),
                      Song_Button    => (4, 6),
                      Menu           => (5, 1),
                      B4             => (5, 2),
                      B12            => (5, 3),
                      B13            => (5, 4),
                      B5             => (5, 5),
                      PAD_B          => (5, 6))
     );

   subtype LED is Button range B1 .. Song_Button;

   LED_Position : constant array (LED) of Positive :=
     (B1             => 6,
      B2             => 7,
      B3             => 8,
      B4             => 9,
      B5             => 10,
      B6             => 11,
      B7             => 12,
      B8             => 13,
      B9             => 16,
      B10            => 17,
      B11            => 18,
      B12            => 19,
      B13            => 20,
      B14            => 21,
      B15            => 22,
      B16            => 23,
      Rec            => 24,
      Play           => 14,
      Menu           => 1,
      Func           => 4,
      Step_Button    => 15,
      Track_Button   => 5,
      Pattern_Button => 3,
      Song_Button    => 2);

   package LED_Strip is new Noise_Nugget_SDK.WS2812
     (Pin => 14,
      Number_Of_LEDs => LED_Position'Length);

end PGB1;
