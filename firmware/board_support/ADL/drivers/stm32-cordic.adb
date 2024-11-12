
package body STM32.CORDIC is

   -------------------------
   -- Set_CORDIC_Function --
   -------------------------

   procedure Set_CORDIC_Function
     (This  : in out CORDIC_Coprocessor;
      Value : CORDIC_Function)
   is
   begin
      This.CSR.FUNC := Value'Enum_Rep;
   end Set_CORDIC_Function;

   -------------------------
   -- Get_CORDIC_Function --
   -------------------------

   function Get_CORDIC_Function (This : CORDIC_Coprocessor)
     return CORDIC_Function
   is
   begin
      return CORDIC_Function'Val (This.CSR.FUNC);
   end Get_CORDIC_Function;

   --------------------------
   -- Set_CORDIC_Precision --
   --------------------------

   procedure Set_CORDIC_Precision
     (This  : in out CORDIC_Coprocessor;
      Value : CORDIC_Iterations)
   is
   begin
      This.CSR.PRECISION := Value'Enum_Rep;
   end Set_CORDIC_Precision;

   -------------------------------
   -- Set_CORDIC_Scaling_Factor --
   -------------------------------

   procedure Set_CORDIC_Scaling_Factor
     (This  : in out CORDIC_Coprocessor;
      Value : UInt3)
   is
   begin
      This.CSR.SCALE := Value;
   end Set_CORDIC_Scaling_Factor;

   --------------------------
   -- Set_CORDIC_Data_Size --
   --------------------------

   procedure Set_CORDIC_Data_Size
     (This  : in out CORDIC_Coprocessor;
      Value : CORDIC_Data_Size)
   is
   begin
      This.CSR.ARGSIZE := Value = Data_16_Bit;
      This.CSR.RESSIZE := Value = Data_16_Bit;
   end Set_CORDIC_Data_Size;

   --------------------------
   -- Get_CORDIC_Data_Size --
   --------------------------

   function Get_CORDIC_Data_Size
     (This  : CORDIC_Coprocessor)
      return CORDIC_Data_Size
   is
   begin
      return (if This.CSR.ARGSIZE then Data_16_Bit else Data_32_Bit);
   end Get_CORDIC_Data_Size;

   ---------------------------------
   -- Set_CORDIC_Arguments_Number --
   ---------------------------------

   procedure Set_CORDIC_Arguments_Number
     (This  : in out CORDIC_Coprocessor;
      Value : CORDIC_Arguments_Number)
   is
   begin
      This.CSR.NARGS := Value = Two_32_Bit;
   end Set_CORDIC_Arguments_Number;

   ---------------------------------
   -- Get_CORDIC_Arguments_Number --
   ---------------------------------

   function Get_CORDIC_Arguments_Number
     (This  : CORDIC_Coprocessor)
      return CORDIC_Arguments_Number
   is
   begin
      return (if This.CSR.NARGS then Two_32_Bit else One_32_Bit);
   end Get_CORDIC_Arguments_Number;

   -------------------------------
   -- Set_CORDIC_Results_Number --
   -------------------------------

   procedure Set_CORDIC_Results_Number
     (This  : in out CORDIC_Coprocessor;
      Value : CORDIC_Arguments_Number)
   is
   begin
      This.CSR.NRES := Value = Two_32_Bit;
   end Set_CORDIC_Results_Number;

   -------------------------------
   -- Get_CORDIC_Results_Number --
   -------------------------------

   function Get_CORDIC_Results_Number
     (This  : CORDIC_Coprocessor)
      return CORDIC_Arguments_Number
   is
   begin
      return (if This.CSR.NRES then Two_32_Bit else One_32_Bit);
   end Get_CORDIC_Results_Number;

   -----------------------------------
   -- Configure_CORDIC_Coprocesssor --
   -----------------------------------

   procedure Configure_CORDIC_Coprocessor
     (This      : in out CORDIC_Coprocessor;
      Operation : CORDIC_Function;
      Precision : CORDIC_Iterations := Iteration_20;
      Scaling   : UInt3 := 0;
      Data_Size : CORDIC_Data_Size)
   is
   begin
      Set_CORDIC_Function (This, Operation);
      Set_CORDIC_Precision (This, Precision);
      Set_CORDIC_Scaling_Factor (This, Scaling);
      Set_CORDIC_Data_Size (This, Data_Size);

      case Operation is
         when Cosine | Sine | Phase | Modulus =>
            case Data_Size is
               when Data_32_Bit =>
                  This.CSR.NARGS := True; --  Two_32_Bit Arguments
                  This.CSR.NRES := True; --  Two_32_Bit Results
               when Data_16_Bit =>
                  This.CSR.NARGS := False; --  One_32_Bit Argument
                  This.CSR.NRES := False; --  One_32_Bit Result
            end case;
         when Hyperbolic_Cosine | Hyperbolic_Sine =>
            case Data_Size is
               when Data_32_Bit =>
                  This.CSR.NARGS := False; --  One_32_Bit Argument
                  This.CSR.NRES := True; --  Two_32_Bit Results
               when Data_16_Bit =>
                  This.CSR.NARGS := False; --  One_32_Bit Argument
                  This.CSR.NRES := False; --  One_32_Bit Result
            end case;
         when Arctangent | Hyperbolic_Arctangent | Natural_Logarithm | Square_Root =>
            case Data_Size is
               when Data_32_Bit =>
                  This.CSR.NARGS := False; --  One_32_Bit Argument
                  This.CSR.NRES := False; --  One_32_Bit Result
               when Data_16_Bit =>
                  This.CSR.NARGS := False; --  One_32_Bit Argument
                  This.CSR.NRES := False; --  One_32_Bit Result
            end case;
      end case;

   end Configure_CORDIC_Coprocessor;

   ---------------------
   -- Get_CORDIC_Data --
   ---------------------

   function Get_CORDIC_Data (This : CORDIC_Coprocessor) return UInt32 is
   begin
      return This.RDATA;
   end Get_CORDIC_Data;

   ------------
   -- Status --
   ------------

   function Status
     (This : CORDIC_Coprocessor;
      Flag : CORDIC_Status) return Boolean
   is
   begin
      case Flag is
         when Result_Ready =>
            return This.CSR.RRDY;
      end case;
   end Status;

   -------------------
   -- Set_Interrupt --
   -------------------

   procedure Set_Interrupt
     (This   : in out CORDIC_Coprocessor;
      Enable : Boolean)
   is
   begin
      This.CSR.IEN := Enable;
   end Set_Interrupt;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This   : CORDIC_Coprocessor) return Boolean
   is
   begin
      return This.CSR.IEN;
   end Interrupt_Enabled;

   -------------
   -- Set_DMA --
   -------------

   procedure Set_DMA
     (This   : in out CORDIC_Coprocessor;
      DMA    : CORDIC_DMA;
      Enable : Boolean)
   is
   begin
      case DMA is
         when Read_DMA =>
            This.CSR.DMAREN := Enable;
         when Write_DMA =>
            This.CSR.DMAWEN := Enable;
      end case;
   end Set_DMA;

   -----------------
   -- DMA_Enabled --
   -----------------

   function DMA_Enabled
     (This   : CORDIC_Coprocessor;
      DMA    : CORDIC_DMA)
      return Boolean
   is
   begin
      case DMA is
         when Read_DMA =>
            return This.CSR.DMAREN;
         when Write_DMA =>
            return This.CSR.DMAWEN;
      end case;
   end DMA_Enabled;

end STM32.CORDIC;
