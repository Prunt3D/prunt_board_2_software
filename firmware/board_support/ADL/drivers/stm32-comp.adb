with STM32.SYSCFG;
with Ada.Real_Time;

package body STM32.COMP is

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out Comparator) is
      use Ada.Real_Time;
   begin
      --  Enable clock for the COMP peripheral
      STM32.SYSCFG.Enable_SYSCFG_Clock;
      --  There is no COMP-dedicated clock enable control bit in the RCC
      --  controller. Reset and clock enable bits are common for COMP and
      --  SYSCFG. See RM0440 pg 781 chapter 24.3.3.

      This.CSR.EN := True;
      --  Delay 5 us for COMP startup time. See DS12288 Rev 5 chapter 5.3.22
      --  Comparator characteristics.
      declare
         Start_Time : Time := Clock;
      begin
         loop
            exit when Clock > Start_Time + Microseconds (5);
         end loop;
      end;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out Comparator) is
   begin
      This.CSR.EN := False;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : Comparator) return Boolean is
   begin
      return This.CSR.EN;
   end Enabled;

   ------------------------------
   -- Set_Inverting_Input_Port --
   ------------------------------

   procedure Set_Inverting_Input_Port
     (This  : in out Comparator;
      Input : Inverting_Input_Port) is
   begin
      This.CSR.INMSEL := Input'Enum_Rep;
   end Set_Inverting_Input_Port;

   ----------------------
   -- Get_I_Input_Port --
   ----------------------

   function Get_Inverting_Input_Port
     (This : Comparator) return Inverting_Input_Port
   is
   begin
      return Inverting_Input_Port'Val (This.CSR.INMSEL);
   end Get_Inverting_Input_Port;

   ---------------------------------
   -- Set_NonInverting_Input_Port --
   ---------------------------------

   procedure Set_NonInverting_Input_Port
     (This  : in out Comparator;
      Input : NonInverting_Input_Port) is
   begin
      This.CSR.INPSEL := Input = Option_2;
   end Set_NonInverting_Input_Port;

   -----------------------
   -- Get_NI_Input_Port --
   -----------------------

   function Get_NonInverting_Input_Port
     (This : Comparator) return NonInverting_Input_Port
   is
   begin
      return NonInverting_Input_Port'Val (Boolean'Pos (This.CSR.INPSEL));
   end Get_NonInverting_Input_Port;

   -------------------------
   -- Set_Output_Polarity --
   -------------------------

   procedure Set_Output_Polarity (This   : in out Comparator;
                                  Output : Output_Polarity) is
   begin
      This.CSR.POL := Output = Inverted;
   end Set_Output_Polarity;

   -------------------------
   -- Get_Output_Polarity --
   -------------------------

   function Get_Output_Polarity (This : Comparator) return Output_Polarity is
   begin
      return Output_Polarity'Val (Boolean'Pos (This.CSR.POL));
   end Get_Output_Polarity;

   -------------------------------
   -- Set_Comparator_Hysteresis --
   -------------------------------

   procedure Set_Comparator_Hysteresis (This  : in out Comparator;
                                        Value : Comparator_Hysteresis) is
   begin
      This.CSR.HYST := Value'Enum_Rep;
   end Set_Comparator_Hysteresis;

   -------------------------------
   -- Get_Comparator_Hysteresis --
   -------------------------------

   function Get_Comparator_Hysteresis (This : Comparator)
                                        return Comparator_Hysteresis is
   begin
      return Comparator_Hysteresis'Val (This.CSR.HYST);
   end Get_Comparator_Hysteresis;

   -------------------------
   -- Set_Output_Blanking --
   -------------------------

   procedure Set_Output_Blanking (This   : in out Comparator;
                                  Output : Output_Blanking) is
   begin
      This.CSR.BLANKSEL := Output'Enum_Rep;
   end Set_Output_Blanking;

   -------------------------
   -- Get_Output_Blanking --
   -------------------------

   function Get_Output_Blanking (This : Comparator) return Output_Blanking is
   begin
      return Output_Blanking'Val (This.CSR.BLANKSEL);
   end Get_Output_Blanking;

   --------------------------
   -- Configure_Comparator --
   --------------------------

   procedure Configure_Comparator
     (This  : in out Comparator;
      Param : Init_Parameters)
   is
   begin
      Set_Inverting_Input_Port (This, Param.Input_Minus);
      Set_NonInverting_Input_Port (This, Param.Input_Plus);
      Set_Output_Polarity (This, Param.Output_Pol);
      Set_Comparator_Hysteresis (This, Param.Hysteresis);
      Set_Output_Blanking (This, Param.Blanking_Source);
   end Configure_Comparator;

   ---------------------------------
   -- Set_Vrefint_Scaler_Resistor --
   ---------------------------------

   procedure Set_Vrefint_Scaler_Resistor
     (This    : in out Comparator;
      Enabled : Boolean)
   is
   begin
      This.CSR.BRGEN := Enabled;
   end Set_Vrefint_Scaler_Resistor;

   ---------------------------------
   -- Get_Vrefint_Scaler_Resistor --
   ---------------------------------

   function Get_Vrefint_Scaler_Resistor (This : Comparator) return Boolean is
   begin
      return This.CSR.BRGEN;
   end Get_Vrefint_Scaler_Resistor;

   ------------------------
   -- Set_Vrefint_Scaler --
   ------------------------

   procedure Set_Vrefint_Scaler
     (This    : in out Comparator;
      Enabled : Boolean)
   is
      use Ada.Real_Time;
   begin
      This.CSR.SCALEN := Enabled;
      --  Delay for COMP scaler bridge voltage stabilization. See DS12288 Rev 5
      --  chapter 5.3.22 Comparator characteristics.
      delay until Clock + Microseconds (200);
   end Set_Vrefint_Scaler;

   ------------------------
   -- Get_Vrefint_Scaler --
   ------------------------

   function Get_Vrefint_Scaler (This : Comparator) return Boolean is
   begin
      return This.CSR.SCALEN;
   end Get_Vrefint_Scaler;

   ---------------------------
   -- Get_Comparator_Output --
   ---------------------------

   function Get_Comparator_Output
     (This : Comparator) return Comparator_Output is
   begin
      return Comparator_Output'Val (Boolean'Pos (This.CSR.VALUE));
   end Get_Comparator_Output;

   -------------------------
   -- Set_Lock_Comparator --
   -------------------------

   procedure Set_Lock_Comparator (This : in out Comparator) is
   begin
      This.CSR.LOCK := True;
   end Set_Lock_Comparator;

   -------------------------
   -- Get_Lock_Comparator --
   -------------------------

   function Get_Lock_Comparator (This : Comparator) return Boolean is
   begin
      return This.CSR.LOCK;
   end Get_Lock_Comparator;

end STM32.COMP;
