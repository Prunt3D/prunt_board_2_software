with Messages;               use Messages;
with Hardware_Configuration; use Hardware_Configuration;
with Physical_Types;         use Physical_Types;

package Thermistors is

   Loop_Frequency : constant Frequency := 150_000_000.0 * hertz / (4.0 * 128.0 * 4.0 * (640.5 + 12.5) * 28.0);
   --  150 = ADC clock frequency.
   --  4 = ADC clock divider.
   --  128 = Oversampling.
   --  4 = Thermistor count.
   --  640.5 = Sample time.
   --  12.5 = Successive approximation time.
   --  28 = Software oversampling.
   --  Not included: Time to restart after interrupt.
   --
   --  How these values were chosen:
   --
   --  The STM32G474 datasheet states that the sample-and-hold capacitor has a typical value of 5pF, but no maximum is
   --  provided, so let us assume that the sample-and-hold capacitor is 7.5pF and always starts at 0V. In a worst case
   --  scenario, the thermistor resistance is infinite and therefore the target voltage across the capacitor is 3.3V.
   --
   --  With the above values, the equivalent resistance of the sample-and-hold capacitor is:
   --  1 / (7.5pF × 150MHz / (4 × (640.5 + 12.5))) = 1 / (7.5pF × 57.4kHz) = 2.3MOhm
   --
   --  The rate at which we cycle through thermistors is 150MHz / (4 × 4 × 128 × (640.5 + 12.5)) = 112Hz. This is fast
   --  enough that the AC voltage across the 2uF capacitor on the thermistor input is approximately an asymmetric
   --  triangle wave with an average voltage of 4.4mV, which is around 4.4°C on a PT1000 between at 500°C or a 100k
   --  3950 at 350°C. The peak-to-peak voltage of the waveform is 1.2mV.
   --
   --  The amount of software oversampling was chosen to give a good balance between a fast loop frequency and a low
   --  noise level.

   procedure Init;
   procedure Setup (Thermistor_Curves : access Thermistor_Curves_Array; Heater_Map : Heater_Thermistor_Map);
   procedure Start_ISR_Loop;
   function Last_Reported_Temperature (Thermistor : Thermistor_Name) return Temperature;

   Bad_Reading_Error : exception;

private

   Bad_Reading_Indicator : constant Temperature := 1_000_000_000.0 * celsius;

   type ADC_16 is mod 2**16 with
     Size => 16;

   type ADC_Results_Type is array (Thermistor_Name) of ADC_16 with
     Alignment => 2, Pack, Volatile, Volatile_Components;
   ADC_Results : aliased ADC_Results_Type;

   type Float_Reported_Temperatures is array (Thermistor_Name) of Temperature;

   type Heater_Boolean_Map is array (Heater_Name) of Boolean;

   type Float_Thermistor_Point is record
      Temp  : Temperature;
      Value : ADC_Value;
   end record;

   type Float_Thermistor_Curve is array (Thermistor_Curve_Index) of Float_Thermistor_Point;
   type Float_Thermistor_Curves_Array is array (Thermistor_Name) of Float_Thermistor_Curve;

   type Accumulator_Step is range 1 .. 28;
   type Accumulator_Type is range 0 .. Accumulator_Step'Last * 2**16 - 1;
   type Accumulator_Array_Type is array (Thermistor_Name) of Accumulator_Type;

   protected ADC_Handler with
     Linker_Section => ".ccmbss.thermistor_curves", Interrupt_Priority => Thermistor_DMA_Interrupt_Priority
   is
      procedure Init;
      procedure Setup (Thermistor_Curves : access Thermistor_Curves_Array; Heater_Map : Heater_Thermistor_Map);
      procedure Start_ISR_Loop;
      function Last_Reported_Temperature (Thermistor : Thermistor_Name) return Temperature;
   private
      Curves             : Float_Thermistor_Curves_Array;
      Heater_Thermistors : Heater_Thermistor_Map;
      Last_Temperatures  : Float_Reported_Temperatures := (others => Bad_Reading_Indicator);
      Init_Done          : Boolean                     := False;
      ISR_Loop_Started   : Boolean                     := False;
      Setup_Done         : Boolean                     := False;
      Current_Thermistor : Thermistor_Name             := Thermistor_Name'First;
      Accumulators       : Accumulator_Array_Type      := (others => 0);
      Step               : Accumulator_Step            := Accumulator_Step'First;

      function Interpolate (ADC_Val : ADC_Value; Thermistor : Thermistor_Name) return Temperature;
      procedure Start_Conversion;

      procedure End_Of_Sequence_Handler with
        Attach_Handler => Thermistor_DMA_Interrupt_ID;
   end ADC_Handler;

end Thermistors;
