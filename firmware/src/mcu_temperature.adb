with STM32.ADC;    use STM32.ADC;
with STM32.Device; use STM32.Device;
with HAL;          use HAL;
with STM32_SVD.ADC;
with STM32_SVD;

package body MCU_Temperature is

   procedure Init is
      ADC_5_Internal : aliased STM32_SVD.ADC.ADC1_Peripheral with
        Volatile, Import, Address => STM32_SVD.ADC5_Base;
   begin
      Init_Checker.Report_Init_Started;

      Enable_Clock (ADC_5);

      Disable (ADC_5);

      Configure_Common_Properties
        (This           => ADC_5,
         Mode           => Independent,
         Prescaler      => Div_256,
         Clock_Mode     => PCLK2_Div_4,
         DMA_Mode       => Disabled,
         Sampling_Delay => Sampling_Delay_5_Cycles);

      Calibrate (ADC_5, Single_Ended);

      Configure_Unit (ADC_5, ADC_Resolution_12_Bits, Right_Aligned);

      Configure_Regular_Conversions
        (This        => ADC_5,
         Continuous  => True,
         Trigger     => Software_Triggered,
         Conversions => [1 => (Channel => 4, Sample_Time => Sample_640P5_Cycles)]);

      ADC_5_Internal.CFGR.OVRMOD := True;

      Enable (ADC_5);

      Start_Conversion (ADC_5);

      loop
         exit when Conversion_Value (ADC_5) /= 0;
      end loop;

      Init_Checker.Report_Init_Done;
   end Init;

   function Last_Temperature return Temperature is
   begin
      Init_Checker.Raise_If_Init_Not_Done;

      return Dimensionless (Conversion_Value (ADC_5)) * Slope + Offset;
   end Last_Temperature;

end MCU_Temperature;
