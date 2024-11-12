package STM32.CORDIC.Polling is

   procedure Calculate_CORDIC_Function
     (This     : in out CORDIC_Coprocessor;
      Argument : UInt32_Array;
      Result   : out UInt32_Array);
   --  Polls the CORDIC directly to get the calculated function result.

   procedure Calculate_CORDIC_Function
     (This     : in out CORDIC_Coprocessor;
      Argument : UInt16_Array;
      Result   : out UInt16_Array);
   --  Polls the CORDIC directly to get the calculated function result.

end STM32.CORDIC.Polling;
