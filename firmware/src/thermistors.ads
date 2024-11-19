with Messages;               use Messages;
with Hardware_Configuration; use Hardware_Configuration;
with Physical_Types;         use Physical_Types;

package Thermistors is

   Loop_Frequency : constant Frequency := 150_000_000.0 * hertz / (1024.0 * 32.0 * 4.0 * (247.5 + 12.5) * 1.0);
   --  150 = ADC clock frequency.
   --  1024 = ADC clock divider.
   --  256 = Oversampling.
   --  4 = Thermistor count.
   --  247.5 = Sample time.
   --  12.5 = Successive approximation time.
   --  1 = Software oversampling.
   --  Not included: Time to restart after interrupt.

   procedure Init;
   procedure Setup (Thermistor_Curves : access Thermistor_Curves_Array; Heater_Map : Heater_Thermistor_Map);
   procedure Start_ISR_Loop;
   function Last_Reported_Temperature (Thermistor : Thermistor_Name) return Temperature;

   Bad_Reading_Error : exception;

private

   Bad_Reading_Indicator : constant Temperature := 1_000_000_000.0 * celcius;

   type ADC_16 is mod 2**16 with
     Size => 16;

   type ADC_Results_Type is array (1 .. 1) of ADC_16 with
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

   type Software_Oversample_Count is range 1 .. 2;

   type Accumulator_Type is range 0 .. Software_Oversample_Count'Last * ADC_Results_Type'Length * 2**16 - 1;

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
      Accumulator        : Accumulator_Type            := 0;
      Step               : Software_Oversample_Count   := Software_Oversample_Count'First;

      function Interpolate (ADC_Val : ADC_Value; Thermistor : Thermistor_Name) return Temperature;
      procedure Start_Conversion;

      procedure End_Of_Sequence_Handler with
        Attach_Handler => Thermistor_DMA_Interrupt_ID;
   end ADC_Handler;

end Thermistors;
