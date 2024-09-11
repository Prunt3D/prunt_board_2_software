pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32G474xx.svd

pragma Restrictions (No_Elaboration_Code);

with HAL;
with System;

package STM32_SVD.DBGMCU is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype IDCODE_DEV_ID_Field is HAL.UInt16;
   subtype IDCODE_REV_ID_Field is HAL.UInt16;

   --  MCU Device ID Code Register
   type IDCODE_Register is record
      --  Read-only. Device Identifier
      DEV_ID : IDCODE_DEV_ID_Field;
      --  Read-only. Revision Identifier
      REV_ID : IDCODE_REV_ID_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IDCODE_Register use record
      DEV_ID at 0 range 0 .. 15;
      REV_ID at 0 range 16 .. 31;
   end record;

   subtype CR_TRACE_MODE_Field is HAL.UInt2;

   --  Debug MCU Configuration Register
   type CR_Register is record
      --  Debug Sleep Mode
      DBG_SLEEP     : Boolean := False;
      --  Debug Stop Mode
      DBG_STOP      : Boolean := False;
      --  Debug Standby Mode
      DBG_STANDBY   : Boolean := False;
      --  unspecified
      Reserved_3_4  : HAL.UInt2 := 16#0#;
      --  Trace pin assignment control
      TRACE_IOEN    : Boolean := False;
      --  Trace pin assignment control
      TRACE_MODE    : CR_TRACE_MODE_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      DBG_SLEEP     at 0 range 0 .. 0;
      DBG_STOP      at 0 range 1 .. 1;
      DBG_STANDBY   at 0 range 2 .. 2;
      Reserved_3_4  at 0 range 3 .. 4;
      TRACE_IOEN    at 0 range 5 .. 5;
      TRACE_MODE    at 0 range 6 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  APB Low Freeze Register 1
   type APB1L_FZ_Register is record
      --  Debug Timer 2 stopped when Core is halted
      DBG_TIMER2_STOP  : Boolean := False;
      --  TIM3 counter stopped when core is halted
      DBG_TIM3_STOP    : Boolean := False;
      --  TIM4 counter stopped when core is halted
      DBG_TIM4_STOP    : Boolean := False;
      --  TIM5 counter stopped when core is halted
      DBG_TIM5_STOP    : Boolean := False;
      --  Debug Timer 6 stopped when Core is halted
      DBG_TIMER6_STOP  : Boolean := False;
      --  TIM7 counter stopped when core is halted
      DBG_TIM7_STOP    : Boolean := False;
      --  unspecified
      Reserved_6_9     : HAL.UInt4 := 16#0#;
      --  Debug RTC stopped when Core is halted
      DBG_RTC_STOP     : Boolean := False;
      --  Debug Window Wachdog stopped when Core is halted
      DBG_WWDG_STOP    : Boolean := False;
      --  Debug Independent Wachdog stopped when Core is halted
      DBG_IWDG_STOP    : Boolean := False;
      --  unspecified
      Reserved_13_20   : HAL.UInt8 := 16#0#;
      --  I2C1 SMBUS timeout mode stopped when core is halted
      DBG_I2C1_STOP    : Boolean := False;
      --  I2C2 SMBUS timeout mode stopped when core is halted
      DBG_I2C2_STOP    : Boolean := False;
      --  unspecified
      Reserved_23_29   : HAL.UInt7 := 16#0#;
      --  I2C3 SMBUS timeout mode stopped when core is halted
      DBG_I2C3_STOP    : Boolean := False;
      --  LPTIM1 counter stopped when core is halted
      DBG_LPTIMER_STOP : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1L_FZ_Register use record
      DBG_TIMER2_STOP  at 0 range 0 .. 0;
      DBG_TIM3_STOP    at 0 range 1 .. 1;
      DBG_TIM4_STOP    at 0 range 2 .. 2;
      DBG_TIM5_STOP    at 0 range 3 .. 3;
      DBG_TIMER6_STOP  at 0 range 4 .. 4;
      DBG_TIM7_STOP    at 0 range 5 .. 5;
      Reserved_6_9     at 0 range 6 .. 9;
      DBG_RTC_STOP     at 0 range 10 .. 10;
      DBG_WWDG_STOP    at 0 range 11 .. 11;
      DBG_IWDG_STOP    at 0 range 12 .. 12;
      Reserved_13_20   at 0 range 13 .. 20;
      DBG_I2C1_STOP    at 0 range 21 .. 21;
      DBG_I2C2_STOP    at 0 range 22 .. 22;
      Reserved_23_29   at 0 range 23 .. 29;
      DBG_I2C3_STOP    at 0 range 30 .. 30;
      DBG_LPTIMER_STOP at 0 range 31 .. 31;
   end record;

   --  APB Low Freeze Register 2
   type APB1H_FZ_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  DBG_I2C4_STOP
      DBG_I2C4_STOP : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1H_FZ_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      DBG_I2C4_STOP at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  APB High Freeze Register
   type APB2_FZ_Register is record
      --  unspecified
      Reserved_0_10   : HAL.UInt11 := 16#0#;
      --  TIM1 counter stopped when core is halted
      DBG_TIM1_STOP   : Boolean := False;
      --  unspecified
      Reserved_12_12  : HAL.Bit := 16#0#;
      --  TIM8 counter stopped when core is halted
      DBG_TIM8_STOP   : Boolean := False;
      --  unspecified
      Reserved_14_15  : HAL.UInt2 := 16#0#;
      --  TIM15 counter stopped when core is halted
      DBG_TIM15_STOP  : Boolean := False;
      --  TIM16 counter stopped when core is halted
      DBG_TIM16_STOP  : Boolean := False;
      --  TIM17 counter stopped when core is halted
      DBG_TIM17_STOP  : Boolean := False;
      --  unspecified
      Reserved_19_19  : HAL.Bit := 16#0#;
      --  TIM20counter stopped when core is halted
      DBG_TIM20_STOP  : Boolean := False;
      --  unspecified
      Reserved_21_25  : HAL.UInt5 := 16#0#;
      --  DBG_HRTIM0_STOP
      DBG_HRTIM0_STOP : Boolean := False;
      --  DBG_HRTIM0_STOP
      DBG_HRTIM1_STOP : Boolean := False;
      --  DBG_HRTIM0_STOP
      DBG_HRTIM2_STOP : Boolean := False;
      --  DBG_HRTIM0_STOP
      DBG_HRTIM3_STOP : Boolean := False;
      --  unspecified
      Reserved_30_31  : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB2_FZ_Register use record
      Reserved_0_10   at 0 range 0 .. 10;
      DBG_TIM1_STOP   at 0 range 11 .. 11;
      Reserved_12_12  at 0 range 12 .. 12;
      DBG_TIM8_STOP   at 0 range 13 .. 13;
      Reserved_14_15  at 0 range 14 .. 15;
      DBG_TIM15_STOP  at 0 range 16 .. 16;
      DBG_TIM16_STOP  at 0 range 17 .. 17;
      DBG_TIM17_STOP  at 0 range 18 .. 18;
      Reserved_19_19  at 0 range 19 .. 19;
      DBG_TIM20_STOP  at 0 range 20 .. 20;
      Reserved_21_25  at 0 range 21 .. 25;
      DBG_HRTIM0_STOP at 0 range 26 .. 26;
      DBG_HRTIM1_STOP at 0 range 27 .. 27;
      DBG_HRTIM2_STOP at 0 range 28 .. 28;
      DBG_HRTIM3_STOP at 0 range 29 .. 29;
      Reserved_30_31  at 0 range 30 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Debug support
   type DBGMCU_Peripheral is record
      --  MCU Device ID Code Register
      IDCODE   : aliased IDCODE_Register;
      --  Debug MCU Configuration Register
      CR       : aliased CR_Register;
      --  APB Low Freeze Register 1
      APB1L_FZ : aliased APB1L_FZ_Register;
      --  APB Low Freeze Register 2
      APB1H_FZ : aliased APB1H_FZ_Register;
      --  APB High Freeze Register
      APB2_FZ  : aliased APB2_FZ_Register;
   end record
     with Volatile;

   for DBGMCU_Peripheral use record
      IDCODE   at 16#0# range 0 .. 31;
      CR       at 16#4# range 0 .. 31;
      APB1L_FZ at 16#8# range 0 .. 31;
      APB1H_FZ at 16#C# range 0 .. 31;
      APB2_FZ  at 16#10# range 0 .. 31;
   end record;

   --  Debug support
   DBGMCU_Periph : aliased DBGMCU_Peripheral
     with Import, Address => DBGMCU_Base;

end STM32_SVD.DBGMCU;
