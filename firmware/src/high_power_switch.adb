with Hardware_Configuration; use Hardware_Configuration;
with STM32.GPIO;             use STM32.GPIO;
with STM32.ADC;              use STM32.ADC;
with STM32.Device;           use STM32.Device;
with Ada.Real_Time;          use Ada.Real_Time;
with HAL;                    use HAL;

package body High_Power_Switch is

   procedure Init is
   begin
      Init_Checker.Report_Init_Started;

      Enable_Clock (High_Power_Switch_ADC);

      Disable (High_Power_Switch_ADC);

      Configure_Common_Properties
        (This           => High_Power_Switch_ADC,
         Mode           => Independent,
         Prescaler      => Div_1,
         Clock_Mode     => PCLK2_Div_4,
         DMA_Mode       => Disabled,
         Sampling_Delay => Sampling_Delay_5_Cycles);

      Calibrate (High_Power_Switch_ADC, Single_Ended);

      Configure_Unit (High_Power_Switch_ADC, ADC_Resolution_12_Bits, Right_Aligned);
      High_Power_Switch_ADC_Internal.CFGR2.ROVSE := True; --  Regular oversampling
      High_Power_Switch_ADC_Internal.CFGR2.OVSS  := 4; --  4 bit shift
      High_Power_Switch_ADC_Internal.CFGR2.OVSR  := 2#111#; --  256 samples

      Configure_Regular_Conversions
        (This        => High_Power_Switch_ADC,
         Continuous  => False,
         Trigger     => Software_Triggered,
         Conversions => (1 => (Channel => High_Power_Switch_ADC_Channel, Sample_Time => Sample_640P5_Cycles)));

      Clear (High_Power_Switch_Output_Point);
      Configure_IO
        (High_Power_Switch_Output_Point,
         (Mode => Mode_Out, Resistors => Floating, Output_Type => Push_Pull, Speed => Speed_25MHz));
      Configure_IO (High_Power_Switch_Input_Point, (Mode => Mode_Analog, Resistors => Floating));

      Init_Checker.Report_Init_Done;
   end Init;

   procedure Wait_For_Power_Good is
   begin
      Init_Checker.Raise_If_Init_Not_Done;

      Enable (High_Power_Switch_ADC);

      declare
         Last_Bad_Time : Time := Clock;
      begin
         loop
            Clear_Status (High_Power_Switch_ADC, Regular_Channel_Conversion_Completed);
            Start_Conversion (High_Power_Switch_ADC);
            loop
               exit when Status (High_Power_Switch_ADC, Regular_Channel_Conversion_Completed);
               delay until Clock; --  Force a yield.
            end loop;
            Clear_Status (High_Power_Switch_ADC, Regular_Channel_Conversion_Completed);

            if Conversion_Value (High_Power_Switch_ADC) < 10_000 then --  Approximately 0.5V
               Last_Bad_Time := Clock;
            end if;
            exit when Clock > Last_Bad_Time + Seconds (3);
         end loop;
      end;

      Disable (High_Power_Switch_ADC);
   end Wait_For_Power_Good;

   procedure Enable is
   begin
      Init_Checker.Raise_If_Init_Not_Done;

      Set (High_Power_Switch_Output_Point);
   end Enable;

   procedure Disable is
   begin
      Init_Checker.Raise_If_Init_Not_Done;

      Clear (High_Power_Switch_Output_Point);
   end Disable;

end High_Power_Switch;
