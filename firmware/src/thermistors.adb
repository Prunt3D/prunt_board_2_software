with STM32.GPIO;    use STM32.GPIO;
with STM32.Device;  use STM32.Device;
with STM32.ADC;     use STM32.ADC;
with STM32.DMA;     use STM32.DMA;
with HAL;           use HAL;
with Heaters;
with Server_Communication;
with System.Machine_Reset;
with Ada.Exceptions;
with GNAT.Source_Info;
with Ada.Real_Time; use Ada.Real_Time;
with Last_Chance_Handler;

package body Thermistors is

   procedure Init is
   begin
      ADC_Handler.Init;
   end Init;

   --  Using an access type here is inconvenient, but GCC will try to put the entire large array on the stack since
   --  we are passing it to a protected procedure.
   procedure Setup (Thermistor_Curves : access Thermistor_Curves_Array; Heater_Map : Heater_Thermistor_Map) is
   begin
      ADC_Handler.Setup (Thermistor_Curves, Heater_Map);
   end Setup;

   procedure Start_ISR_Loop is
   begin
      ADC_Handler.Start_ISR_Loop;
   end Start_ISR_Loop;

   function Last_Reported_Temperature (Thermistor : Thermistor_Name) return Temperature is
   begin
      return ADC_Handler.Last_Reported_Temperature (Thermistor);
   end Last_Reported_Temperature;

   protected body ADC_Handler is
      procedure Init is
      begin
         if Init_Done then
            raise Constraint_Error with "Tried to call thermistor Init more than once.";
         end if;

         Enable_Clock (Thermistor_ADC);
         Enable_Clock (Thermistor_DMA_Controller);

         for C of Curves loop
            for P of C loop
               P := (Temp => Bad_Reading_Indicator, Value => 0);
            end loop;
         end loop;

         Disable (Thermistor_ADC);

         Configure_Common_Properties
           (This           => Thermistor_ADC,
            Mode           => Independent,
            Prescaler      => Div_1,
            Clock_Mode     => PCLK2_Div_1,
            DMA_Mode       => Disabled,
            Sampling_Delay => Sampling_Delay_5_Cycles);

         Calibrate (Thermistor_ADC, Single_Ended);

         Configure_Unit (Thermistor_ADC, ADC_Resolution_12_Bits, Right_Aligned);
         Thermistor_ADC_Internal.CFGR2.ROVSE := True;   --  Regular oversampling
         Thermistor_ADC_Internal.CFGR2.OVSS  := 4;      --  4 bit shift
         Thermistor_ADC_Internal.CFGR2.OVSR  := 2#111#; --  256 samples

         Configure_Regular_Conversions
           (This        => Thermistor_ADC,
            Continuous  => False,
            Trigger     => Software_Triggered,
            Conversions => [for I in 1 .. Thermistor_Name'Pos (Thermistor_Name'Last) + 1 =>
              (Channel => Thermistor_ADC_Channels (Thermistor_Name'Val (I - 1)), Sample_Time => Sample_640P5_Cycles)]);

         Enable (Thermistor_ADC);

         for Thermistor in Thermistor_Name loop
            Configure_IO (Thermistor_GPIO_Points (Thermistor), (Mode => Mode_Analog, Resistors => Floating));
         end loop;

         Configure
           (Thermistor_DMA_Controller,
            Thermistor_DMA_Stream,
            (Channel                      => Thermistor_DMA_Channel,
             Direction                    => Peripheral_To_Memory,
             Increment_Peripheral_Address => False,
             Increment_Memory_Address     => True,
             Peripheral_Data_Format       => HalfWords,
             Memory_Data_Format           => HalfWords,
             Operation_Mode               => Normal_Mode,
             Priority                     => Thermistor_DMA_Priority,
             Memory_Burst_Size            => Memory_Burst_Single,
             Peripheral_Burst_Size        => Peripheral_Burst_Single));

         Enable_DMA (Thermistor_ADC);

         Init_Done := True;
      end Init;

      procedure Setup (Thermistor_Curves : access Thermistor_Curves_Array; Heater_Map : Heater_Thermistor_Map) is
      begin
         if not Init_Done then
            raise Constraint_Error with "Tried to call thermistor Setup before Init.";
         end if;

         if Setup_Done then
            raise Constraint_Error with "Tried to call thermistor Setup more than once.";
         end if;

         for Thermistor in Thermistor_Name loop
            for I in Thermistor_Curve_Index loop
               Curves (Thermistor) (I) :=
                 (Temp  => Dimensionless (Thermistor_Curves (Thermistor) (I).Temp) * celcius,
                  Value => Thermistor_Curves (Thermistor) (I).Value);
            end loop;
         end loop;
         Heater_Thermistors := Heater_Map;

         Setup_Done := True;
      end Setup;

      function Last_Reported_Temperature (Thermistor : Thermistor_Name) return Temperature is
      begin
         return Last_Temperatures (Thermistor);
      end Last_Reported_Temperature;

      procedure Start_ISR_Loop is
      begin
         if not Setup_Done then
            raise Constraint_Error with "Tried to start thermistor ISR loop before calling Setup.";
         end if;

         if ISR_Loop_Started then
            raise Constraint_Error with "Tried to start thermistor ISR loop more than once.";
         end if;

         ISR_Loop_Started := True;

         Start_Conversion;
      end Start_ISR_Loop;

      procedure Start_Conversion is
      begin
         Start_Transfer_with_Interrupts
           (This               => Thermistor_DMA_Controller,
            Stream             => Thermistor_DMA_Stream,
            Source             => Data_Register_Address (Thermistor_ADC),
            Destination        => ADC_Results'Address,
            Data_Count         => ADC_Results'Length,
            Enabled_Interrupts => [Transfer_Complete_Interrupt => True, others => False]);
         Start_Conversion (Thermistor_ADC);
      end Start_Conversion;

      function Interpolate (ADC_Val : ADC_Value; Thermistor : Thermistor_Name) return Temperature is
         Curve : Float_Thermistor_Curve renames Curves (Thermistor);
         Left  : Thermistor_Curve_Index := Thermistor_Curve'First;
         Right : Thermistor_Curve_Index := Thermistor_Curve'Last - 1;
         Mid   : Thermistor_Curve_Index;
      begin
         while Left <= Right loop
            Mid := Left + (Right - Left) / 2;
            if ADC_Val >= Curve (Mid).Value and then ADC_Val <= Curve (Mid + 1).Value then
               exit;
            elsif ADC_Val < Curve (Mid).Value then
               if Mid = Left then
                  return Bad_Reading_Indicator;
               end if;
               Right := Mid - 1;
            else
               if Mid = Left then
                  return Bad_Reading_Indicator;
               end if;
               Left := Mid + 1;
            end if;
         end loop;

         declare
            Lower_Point : constant Float_Thermistor_Point := Curve (Mid);
            Upper_Point : constant Float_Thermistor_Point := Curve (Mid + 1);
         begin
            if Lower_Point.Temp = Upper_Point.Temp then
               return Lower_Point.Temp;
            else
               return
                 Lower_Point.Temp +
                 (Upper_Point.Temp - Lower_Point.Temp) / Dimensionless (Upper_Point.Value - Lower_Point.Value) *
                 Dimensionless (ADC_Val - Lower_Point.Value);
            end if;
         end;
      end Interpolate;

      procedure End_Of_Sequence_Handler is
      begin
         Clear_All_Status (Thermistor_DMA_Controller, Thermistor_DMA_Stream);
         Start_Conversion;

         for Thermistor in Thermistor_Name loop
            Accumulators (Thermistor) := @ + Accumulator_Type (ADC_Results (Thermistor));
         end loop;

         if Step = Accumulator_Step'Last then
            for Thermistor in Thermistor_Name loop
               Last_Temperatures (Thermistor) :=
                 Interpolate
                   (ADC_Value
                      (Accumulator_Type
                         (Accumulators (Thermistor) / Accumulator_Type (Accumulator_Step'Last))),
                    Thermistor);
            end loop;

            --  We only raise an exception if a bad thermistor reading is to be used for a heater.
            for Heater in Heater_Name loop
               if Last_Temperatures (Heater_Thermistors (Heater)) = Bad_Reading_Indicator then
                  raise Bad_Reading_Error
                    with "Thermistor reading out of range for " & Heater_Thermistors (Heater)'Image;
               end if;
            end loop;

            for Heater in Heater_Name loop
               Heaters.Update_Reading (Heater, Last_Temperatures (Heater_Thermistors (Heater)));
            end loop;

            Accumulators := (others => 0);

            Step := Accumulator_Step'First;
         else
            Step := @ + 1;
         end if;
      exception
         when E : others =>
            Last_Chance_Handler.Last_Chance_Handler (E);
      end End_Of_Sequence_Handler;
   end ADC_Handler;

end Thermistors;
