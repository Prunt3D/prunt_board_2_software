with Physical_Types; use Physical_Types;
with Init_Checkers;
with System;

package MCU_Temperature is

   procedure Init;
   function Last_Temperature return Temperature;

private

   Init_Checker : Init_Checkers.Init_Checker;

   type ADC_16 is mod 2**16 with
     Size => 16;

   TS_CAL1_ADC         : constant ADC_16 with
     Import, Address => System'To_Address (16#1FFF_75A8#);
   TS_CAL2_ADC         : constant ADC_16 with
     Import, Address => System'To_Address (16#1FFF_75CA#);
   TS_CAL1_Temperature : constant Temperature := 30.0 * celcius;
   TS_CAL2_Temperature : constant Temperature := 130.0 * celcius;
   TS_CAL_Voltage      : constant Voltage     := 3.0 * volt;
   Board_Voltage       : constant Voltage     := 3.3 * volt;

   Slope  : constant Temperature :=
     (TS_CAL2_Temperature - TS_CAL1_Temperature) / (Dimensionless (TS_CAL2_ADC) - Dimensionless (TS_CAL1_ADC)) *
     (Board_Voltage / TS_CAL_Voltage);
   Offset : constant Temperature :=
     TS_CAL1_Temperature -
     (TS_CAL2_Temperature - TS_CAL1_Temperature) / (Dimensionless (TS_CAL2_ADC) - Dimensionless (TS_CAL1_ADC)) *
       Dimensionless (TS_CAL1_ADC);

end MCU_Temperature;
