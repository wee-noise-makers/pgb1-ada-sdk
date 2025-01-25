with HAL;

with Tresses; use Tresses;

package Tresses_Perf_Core_1 is

   Synth_Buffer_Size : constant := 64;

   Params : Param_Array  := (others => Param_Range'Last / 2);

   function Entry_Point return HAL.UInt32;

   type FX_Kind is (FX_Reverb, FX_Bitcrush, FX_Drive, FX_Filter);
   Current_FX : FX_Kind := FX_Kind'First
     with Volatile;

   function Engine return String;

   function Param_Short_Label (Id : Tresses.Param_Id)
                               return Tresses.Short_Label;

   function CPU_Load return Float;
   function Max_CPU_Load return Float;

   CC_Plus_Small  : constant := 0;
   CC_Plus_Big    : constant := 1;
   CC_Minus_Small : constant := 2;
   CC_Minus_Big   : constant := 3;
end Tresses_Perf_Core_1;
