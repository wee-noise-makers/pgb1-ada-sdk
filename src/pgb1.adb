with HAL;

package body PGB1 is

   Font_Data : constant HAL.UInt8_Array (1 .. 412) :=
     (
      187, 214, 205, 231, 125, 253, 255, 255, 255, 141, 59, 130, 11, 38, 136,
      241, 255, 251, 123, 140, 17, 70, 12, 64, 116, 113, 56, 239, 92, 132,
      17, 70, 224, 156, 115, 14, 196, 30, 247, 207, 223, 255, 247, 239, 247,
      251, 246, 252, 255, 255, 255, 255, 255, 255, 255, 255, 207, 157, 255,
      174, 53, 176, 118, 239, 222, 251, 255, 127, 93, 118, 119, 250, 254,
      156, 203, 121, 255, 237, 156, 115, 206, 122, 239, 220, 190, 214, 19,
      231, 156, 115, 110, 59, 231, 156, 123, 189, 223, 250, 251, 247, 255,
      253, 245, 253, 255, 125, 255, 255, 255, 255, 127, 255, 255, 255, 255,
      125, 223, 191, 43, 208, 183, 222, 125, 213, 254, 255, 111, 179, 223,
      174, 208, 95, 231, 114, 238, 224, 190, 231, 156, 119, 222, 123, 183,
      175, 246, 138, 57, 231, 156, 223, 206, 185, 90, 111, 223, 215, 253,
      29, 101, 76, 113, 7, 153, 111, 219, 84, 70, 24, 74, 136, 206, 57, 230,
      64, 223, 183, 239, 95, 227, 222, 127, 223, 8, 62, 248, 91, 237, 123,
      237, 131, 59, 134, 255, 253, 127, 183, 1, 232, 29, 132, 2, 236, 203,
      189, 82, 14, 58, 24, 183, 115, 106, 239, 221, 247, 253, 255, 63, 230,
      141, 139, 142, 237, 91, 183, 98, 206, 185, 236, 183, 115, 110, 182, 235,
      247, 83, 251, 131, 213, 171, 223, 87, 237, 252, 255, 102, 123, 63, 240,
      220, 118, 47, 231, 14, 238, 86, 206, 121, 231, 189, 115, 251, 106, 239,
      140, 243, 74, 191, 237, 156, 170, 187, 247, 125, 255, 255, 193, 121, 7,
      118, 112, 251, 230, 173, 156, 131, 161, 199, 237, 156, 218, 115, 247,
      125, 239, 255, 21, 58, 246, 239, 222, 123, 255, 220, 221, 238, 238, 58,
      183, 221, 205, 123, 255, 253, 149, 115, 206, 122, 239, 220, 182, 214,
      59, 231, 188, 181, 110, 187, 74, 220, 246, 253, 222, 255, 191, 115, 206,
      249, 253, 220, 182, 117, 43, 231, 252, 235, 111, 153, 170, 242, 238,
      125, 223, 191, 127, 221, 79, 250, 215, 255, 239, 63, 247, 24, 65, 188,
      49, 238, 152, 127, 191, 191, 239, 232, 96, 196, 192, 7, 23, 179, 3, 206,
      69, 159, 92, 220, 113, 59, 183, 65, 188, 241, 131, 31, 98, 12, 113, 71,
      23, 115, 139, 202, 69, 255, 58, 188, 105, 87, 195, 193, 220, 249, 251);

   Font_W : constant := 470;
   Font_H : constant := 7;
   type Bit_Array is array (Positive range <>) of Boolean
     with Pack;

   Bit_Data : Bit_Array (1 .. Font_W * Font_H)
     with Address => Font_Data'Address;

   -----------
   -- Print --
   -----------

   procedure Print (X_Offset    : Integer;
                    Y_Offset    : Integer;
                    C           : Character)
   is
      Index : constant Integer := Character'Pos (C) - Character'Pos ('!');
      Bitmap_Offset : constant Integer := Index * 5;

      function Color (X, Y : Integer) return Boolean;

      -----------
      -- Color --
      -----------

      function Color (X, Y : Integer) return Boolean is
      begin
         if Index in 0 .. 93 and then X in 0 .. 4 and then Y in 0 .. 6 then
            return not Bit_Data (1 + X + Bitmap_Offset + Y * Font_W);
         else
            return False;
         end if;
      end Color;

   begin
      Draw_Loop : for X in 0 .. 5 loop
         for Y in 0 .. 6 loop

            if Y + Y_Offset in 0 .. Screen.Height - 1 then
               if X + X_Offset > Screen.Width - 1 then
                  exit Draw_Loop;
               elsif X + X_Offset >= 0
                 and then
                   Y + Y_Offset in 0 .. Screen.Height - 1
               then
                  Screen.Set_Pixel (Screen.Pix_X (X + X_Offset),
                                    Screen.Pix_Y (Y + Y_Offset),
                                    Color (X, Y));
               end if;
            end if;
         end loop;
      end loop Draw_Loop;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (X_Offset    : Integer;
                    Y_Offset    : Integer;
                    Str         : String)
   is
      X : Integer := X_Offset;
   begin
      for C of Str loop
         Print (X, Y_Offset, C);
         X := X + Font_Width;
      end loop;
   end Print;

end PGB1;
