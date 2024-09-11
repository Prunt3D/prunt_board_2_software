with Ada.Unchecked_Conversion;

private with STM32_SVD.CORDIC;

package STM32.CORDIC is
   pragma Elaborate_Body;

   type CORDIC_Coprocessor is limited private;

   type CORDIC_Function is
     (Cosine,
      Sine,
      Phase,
      Modulus,
      Arctangent,
      Hyperbolic_Cosine,
      Hyperbolic_Sine,
      Hyperbolic_Arctangent,
      Natural_Logarithm,
      Square_Root)
     with Size => 4;

   procedure Set_CORDIC_Function
     (This  : in out CORDIC_Coprocessor;
      Value : CORDIC_Function)
     with Post => Get_CORDIC_Function (This) = Value;
   --  Set the function for the next calculation.

   function Get_CORDIC_Function (This : CORDIC_Coprocessor)
     return CORDIC_Function;
   --  Returns the function to be calculated.

   type CORDIC_Iterations is
     (Iteration_4,
      Iteration_8,
      Iteration_12,
      Iteration_16,
      Iteration_20,
      Iteration_24);

   for CORDIC_Iterations use
     (Iteration_4  => 1,
      Iteration_8  => 2,
      Iteration_12 => 3,
      Iteration_16 => 4,
      Iteration_20 => 5,
      Iteration_24 => 6);

   procedure Set_CORDIC_Precision
     (This  : in out CORDIC_Coprocessor;
      Value : CORDIC_Iterations);
   --  The precision of the result is dependent on the number of CORDIC
   --  iterations. To determine the number of iterations needed for a given
   --  accuracy refer to RM0440 rev 6 Chapter 17.3.5 Table 115 at pg 479. The
   --  value written in this register is (Number of iterations) / 4. Note that
   --  for most functions, the recommended range for this field is 3 to 6.

   procedure Set_CORDIC_Scaling_Factor
     (This  : in out CORDIC_Coprocessor;
      Value : UInt3);
   --  The value of this field indicates the scaling factor applied to the
   --  arguments and/or results. A value n implies that the arguments have been
   --  multiplied by a factor 2**-n, and/or the results need to be multiplied by
   --  2**n. Refer to RM0440 rev 6 Section 17.3.2 pg 469 for the applicability
   --  of the scaling factor for each function and the appropriate range.

   type CORDIC_Data_Size is (Data_32_Bit, Data_16_Bit);

   procedure Set_CORDIC_Data_Size
     (This  : in out CORDIC_Coprocessor;
      Value : CORDIC_Data_Size)
     with Post => Get_CORDIC_Data_Size (This) = Value;

   function Get_CORDIC_Data_Size
     (This  : CORDIC_Coprocessor)
      return CORDIC_Data_Size;

   type CORDIC_Arguments_Number is (One_32_Bit, Two_32_Bit);

   procedure Set_CORDIC_Arguments_Number
     (This  : in out CORDIC_Coprocessor;
      Value : CORDIC_Arguments_Number)
     with Post => Get_CORDIC_Arguments_Number (This) = Value;
   --  Set the number of arguments expected by the CORDIC_WDATA register:
   --  Only one 32-bit write (or two 16-bit values if ARGSIZE = 1) is needed
   --  for the next calculation.
   --  Two 32-bit values must be written to the CORDIC_WDATA register to
   --  trigger the next calculation.

   function Get_CORDIC_Arguments_Number
     (This  : CORDIC_Coprocessor)
      return CORDIC_Arguments_Number;

   procedure Set_CORDIC_Results_Number
     (This  : in out CORDIC_Coprocessor;
      Value : CORDIC_Arguments_Number)
     with Post => Get_CORDIC_Results_Number (This) = Value;
   --  Set the number of results in the CORDIC_RDATA register:
   --  Only one 32-bit value (or two 16-bit values if RESSIZE = 1) is
   --  transferred to the CORDIC_RDATA register on completion of the next
   --  calculation. One read from CORDIC_RDATA resets the RRDY flag.
   --  Two 32-bit values are transferred to the CORDIC_RDATA register on
   --  completion of the next calculation. Two reads from CORDIC_RDATA are
   --  necessary to reset the RRDY flag.

   function Get_CORDIC_Results_Number
     (This : CORDIC_Coprocessor)
      return CORDIC_Arguments_Number;

   procedure Configure_CORDIC_Coprocessor
     (This      : in out CORDIC_Coprocessor;
      Operation : CORDIC_Function;
      Precision : CORDIC_Iterations := Iteration_20;
      Scaling   : UInt3 := 0;
      Data_Size : CORDIC_Data_Size);
   --  Several functions take two input arguments (ARG1 and ARG2) and some
   --  generate two results (RES1 and RES2) simultaneously. See RM0440 rev 6
   --  chapter 17.3.2 Table 102.

   function Get_CORDIC_Data (This : CORDIC_Coprocessor) return UInt32;

   --  The CORDIC operates in fixed point signed integer format. Input and
   --  output values can be either q1.31 or q1.15. See RM0440 rev 6 chapter
   --  17.3.3.
   type Q1_31 is delta 2.0**(-31) range -1.0 .. 1.0 - 2.0**(-31)
     with Size => 32;
   --  In q1.31 format, numbers are represented by one sign bit and 31
   --  fractional bits (binary decimal places). The numeric range is therefore
   --  -1 (0x80000000) to 1 - 2**-31 (0x7FFFFFFF).
   type Q1_15 is delta 2.0**(-15) range -1.0 .. 1.0 - 2.0**(-15)
     with Size => 16;
   --  In q1.15 format, the numeric range is -1 (0x8000) to 1 - 2**-15 (0x7FFF).
   --  This format has the advantage that two input arguments can be packed
   --  into a single 32-bit write, and two results can be fetched in one 32-bit
   --  read.

   --  The input (WDATA) and output (RDATA) data of the CORDIC uses UInt32
   --  to represent the fixed point values. So we need to convert the type
   --  Fraction to UInt and vice-versa.
   function Q1_31_To_UInt32 is new
     Ada.Unchecked_Conversion (Q1_31, UInt32);
   function UInt32_To_Q1_31 is new
     Ada.Unchecked_Conversion (UInt32, Q1_31);
   function Q1_15_To_UInt16 is new
     Ada.Unchecked_Conversion (Q1_15, UInt16);
   function UInt16_To_Q1_15 is new
     Ada.Unchecked_Conversion (UInt16, Q1_15);

   type CORDIC_Status is (Result_Ready);

   function Status
     (This : CORDIC_Coprocessor;
      Flag : CORDIC_Status) return Boolean;

   procedure Set_Interrupt
     (This   : in out CORDIC_Coprocessor;
      Enable : Boolean)
     with Post => Interrupt_Enabled (This) = Enable;
   --  An interrupt request is generated whenever the RRDY flag is set.

   function Interrupt_Enabled
     (This : CORDIC_Coprocessor) return Boolean;

   type CORDIC_DMA is (Read_DMA, Write_DMA);

   procedure Set_DMA
     (This   : in out CORDIC_Coprocessor;
      DMA    : CORDIC_DMA;
      Enable : Boolean)
     with Post => DMA_Enabled (This, DMA) = Enable;
   --  Requests are generated on the DMA read channel whenever the RRDY flag
   --  is set and on the DMA write channel whenever no operation is pendig.

   function DMA_Enabled
     (This   : CORDIC_Coprocessor;
      DMA    : CORDIC_DMA) return Boolean;

private

   type CORDIC_Coprocessor is new STM32_SVD.CORDIC.CORDIC_Peripheral;

end STM32.CORDIC;

