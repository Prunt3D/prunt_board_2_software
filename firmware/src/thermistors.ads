with Messages;               use Messages;
with Hardware_Configuration; use Hardware_Configuration;
with Physical_Types;         use Physical_Types;

package Thermistors is

   Loop_Frequency : constant Frequency := (150_000_000.0 / 4.0) * hertz / (256.0 * 4.0 * 640.5);
   --  150/4 = ADC clock frequency.
   --  256 = Oversampling.
   --  4 = Thermistor count.
   --  640.5 = Sample time.
   --  This probably is a bit off, I have not checked exactly how the ADC timings work.

   procedure Init;
   procedure Setup (Thermistor_Curves : access Thermistor_Curves_Array; Heater_Map : Heater_Thermistor_Map);
   procedure Start_ISR_Loop;
   function Last_Reported_Temperature (Thermistor : Thermistor_Name) return Temperature;

   Bad_Reading_Error : exception;

private

   Bad_Reading_Indicator : constant Temperature := 1_000_000_000.0 * celcius;

   type ADC_Results_Type is array (Thermistor_Name) of ADC_Value with
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

      function Interpolate (ADC_Val : ADC_Value; Thermistor : Thermistor_Name) return Temperature;
      procedure Start_Conversion;

      procedure End_Of_Sequence_Handler with
        Attach_Handler => Thermistor_DMA_Interrupt_ID;
   end ADC_Handler;

end Thermistors;
