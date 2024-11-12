package body STM32.CORDIC.Polling is

   -------------------------------
   -- Calculate_CORDIC_Function --
   -------------------------------

   procedure Calculate_CORDIC_Function
     (This     : in out CORDIC_Coprocessor;
      Argument : UInt32_Array;
      Result   : out UInt32_Array)
   is
      --  Test if data width is 32 bit
      pragma Assert (This.CSR.ARGSIZE = True, "Invalid data size");

      Operation : constant CORDIC_Function := Get_CORDIC_Function (This);
   begin
      case Operation is
         when Cosine | Sine | Phase | Modulus =>
            --  Two 32 bit arguments
            This.WDATA := Argument (1);
            This.WDATA := Argument (2);
         when Hyperbolic_Cosine | Hyperbolic_Sine | Arctangent |
              Hyperbolic_Arctangent | Natural_Logarithm | Square_Root =>
            --  One 32 bit argument
            This.WDATA := Argument (1);
      end case;

      while not This.CSR.RRDY loop
         null;
      end loop;

      case Operation is
         when Cosine | Sine | Phase | Modulus |
              Hyperbolic_Cosine | Hyperbolic_Sine =>
            --  Two 32 bit results
            Result (1) := This.RDATA;
            Result (2) := This.RDATA;
         when Arctangent | Hyperbolic_Arctangent | Natural_Logarithm | Square_Root =>
            --  One 32 bit result
            Result (1) := This.RDATA;
      end case;
   end Calculate_CORDIC_Function;

   -------------------------------
   -- Calculate_CORDIC_Function --
   -------------------------------

   procedure Calculate_CORDIC_Function
     (This     : in out CORDIC_Coprocessor;
      Argument : UInt16_Array;
      Result   : out UInt16_Array)
   is
      --  Test if data width is 16 bit
      pragma Assert (This.CSR.ARGSIZE = False, "Invalid data size");

      Operation : constant CORDIC_Function := Get_CORDIC_Function (This);
      Data : UInt32;
   begin
      case Operation is
         when Cosine | Sine | Phase | Modulus =>
            --  Two 16 bit argument
            Data := UInt32 (Argument (2));
            Data := Shift_Left (Data, 16) or UInt32 (Argument (1));
            This.WDATA := Data;
         when Hyperbolic_Cosine | Hyperbolic_Sine | Arctangent |
              Hyperbolic_Arctangent | Natural_Logarithm | Square_Root =>
            --  One 16 bit argument
            This.WDATA := UInt32 (Argument (1));
      end case;

      while not This.CSR.RRDY loop
         null;
      end loop;

      Data := This.RDATA;
      case Operation is
         when Cosine | Sine | Phase | Modulus |
              Hyperbolic_Cosine | Hyperbolic_Sine =>
            --  Two 16 bit results
            Result (1) := UInt16 (Data);
            Result (2) := UInt16 (Shift_Right (Data, 16));
         when Arctangent | Hyperbolic_Arctangent | Natural_Logarithm | Square_Root =>
            --  One 32 bit result
            Result (1) := UInt16 (Data);
      end case;
   end Calculate_CORDIC_Function;

end STM32.CORDIC.Polling;
