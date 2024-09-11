------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2018, AdaCore                     --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of STMicroelectronics nor the names of its       --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    stm32g474xx.h                                                 --
--   @author  Julio C. Gobbi                                                --
--   @version V1.0.0                                                        --
--   @date    24-August-2021                                                 --
--   @brief   CMSIS STM32F334xx Device Peripheral Access Layer Header File. --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides declarations for devices on the STM32G474xx MCUs
--  manufactured by ST Microelectronics.  For example, an STM32G474RE.

with System;         use System; --  Enable for SPI, COMP and OPAMP

with STM32_SVD;      use STM32_SVD;
with STM32_SVD.COMP; --  Enable for COMP
with STM32_SVD.OPAMP; --  Enable for OPAMP
with STM32_SVD.SAI; --  Enable for SAI

with STM32.GPIO;     use STM32.GPIO;
with STM32.ADC;      use STM32.ADC;
with STM32.DAC;      use STM32.DAC;
with STM32.CRC;      use STM32.CRC;
with STM32.RNG;      use STM32.RNG;
with STM32.CORDIC;   use STM32.CORDIC;
with STM32.FMAC;     use STM32.FMAC;
with STM32.DMA;      use STM32.DMA;
with STM32.USARTs;   use STM32.USARTs;
with STM32.SPI;      use STM32.SPI;
with STM32.SPI.DMA;  use STM32.SPI.DMA;
with STM32.I2C;      use STM32.I2C;
with STM32.I2S;      use STM32.I2S;
with STM32.RTC;      use STM32.RTC;
with STM32.Timers;   use STM32.Timers;
with STM32.LPTimers; use STM32.LPTimers;
with STM32.HRTimers; use STM32.HRTimers;
with STM32.OPAMP;    use STM32.OPAMP;
with STM32.COMP;     use STM32.COMP;
with STM32.CAN;      use STM32.CAN;

package STM32.Device is
   pragma Elaborate_Body;

   Unknown_Device : exception;
   --  Raised by the routines below for a device passed as an actual parameter
   --  when that device is not present on the given hardware instance.

   -----------------------
   -- CPU Clock Sources --
   -----------------------

   HSE_VALUE : constant := 16_000_000;
   --  High-Speed external oscillator in Hz

   LSE_VALUE : constant := 32_768;
   --  Low-Speed external oscillator in Hz

   HSI_VALUE : constant := 16_000_000;
   --  High-Speed internal oscillator in Hz

   --  HSI48_VALUE : constant := 48_000_000;
   --  High-Speed internal 48 MHz oscillator in Hz

   LSI_VALUE : constant := 32_000;
   --  Low-Speed internal oscillator in Hz

   I2SCLK : constant := 12_288_000;
   --  I2S_CKIN external frequency

   ----------
   -- GPIO --
   ----------

   procedure Enable_Clock (This : aliased GPIO_Port);
   procedure Enable_Clock (Point : GPIO_Point);
   procedure Enable_Clock (Points : GPIO_Points);

   procedure Reset (This : aliased GPIO_Port)
     with Inline;
   procedure Reset (Point : GPIO_Point)
     with Inline;
   procedure Reset (Points : GPIO_Points)
     with Inline;

   function GPIO_Port_Representation (Port : GPIO_Port) return UInt4
     with Inline;

   GPIO_A : aliased GPIO_Port with Import, Volatile, Address => GPIOA_Base;
   GPIO_B : aliased GPIO_Port with Import, Volatile, Address => GPIOB_Base;
   GPIO_C : aliased GPIO_Port with Import, Volatile, Address => GPIOC_Base;
   GPIO_D : aliased GPIO_Port with Import, Volatile, Address => GPIOD_Base;
   GPIO_E : aliased GPIO_Port with Import, Volatile, Address => GPIOE_Base;
   GPIO_F : aliased GPIO_Port with Import, Volatile, Address => GPIOF_Base;
   GPIO_G : aliased GPIO_Port with Import, Volatile, Address => GPIOG_Base;

   PA0  : aliased GPIO_Point := (GPIO_A'Access, Pin_0);
   PA1  : aliased GPIO_Point := (GPIO_A'Access, Pin_1);
   PA2  : aliased GPIO_Point := (GPIO_A'Access, Pin_2);
   PA3  : aliased GPIO_Point := (GPIO_A'Access, Pin_3);
   PA4  : aliased GPIO_Point := (GPIO_A'Access, Pin_4);
   PA5  : aliased GPIO_Point := (GPIO_A'Access, Pin_5);
   PA6  : aliased GPIO_Point := (GPIO_A'Access, Pin_6);
   PA7  : aliased GPIO_Point := (GPIO_A'Access, Pin_7);
   PA8  : aliased GPIO_Point := (GPIO_A'Access, Pin_8);
   PA9  : aliased GPIO_Point := (GPIO_A'Access, Pin_9);
   PA10 : aliased GPIO_Point := (GPIO_A'Access, Pin_10);
   PA11 : aliased GPIO_Point := (GPIO_A'Access, Pin_11);
   PA12 : aliased GPIO_Point := (GPIO_A'Access, Pin_12);
   PA13 : aliased GPIO_Point := (GPIO_A'Access, Pin_13);
   PA14 : aliased GPIO_Point := (GPIO_A'Access, Pin_14);
   PA15 : aliased GPIO_Point := (GPIO_A'Access, Pin_15);
   PB0  : aliased GPIO_Point := (GPIO_B'Access, Pin_0);
   PB1  : aliased GPIO_Point := (GPIO_B'Access, Pin_1);
   PB2  : aliased GPIO_Point := (GPIO_B'Access, Pin_2);
   PB3  : aliased GPIO_Point := (GPIO_B'Access, Pin_3);
   PB4  : aliased GPIO_Point := (GPIO_B'Access, Pin_4);
   PB5  : aliased GPIO_Point := (GPIO_B'Access, Pin_5);
   PB6  : aliased GPIO_Point := (GPIO_B'Access, Pin_6);
   PB7  : aliased GPIO_Point := (GPIO_B'Access, Pin_7);
   PB8  : aliased GPIO_Point := (GPIO_B'Access, Pin_8);
   PB9  : aliased GPIO_Point := (GPIO_B'Access, Pin_9);
   PB10 : aliased GPIO_Point := (GPIO_B'Access, Pin_10);
   PB11 : aliased GPIO_Point := (GPIO_B'Access, Pin_11);
   PB12 : aliased GPIO_Point := (GPIO_B'Access, Pin_12);
   PB13 : aliased GPIO_Point := (GPIO_B'Access, Pin_13);
   PB14 : aliased GPIO_Point := (GPIO_B'Access, Pin_14);
   PB15 : aliased GPIO_Point := (GPIO_B'Access, Pin_15);
   PC0  : aliased GPIO_Point := (GPIO_C'Access, Pin_0);
   PC1  : aliased GPIO_Point := (GPIO_C'Access, Pin_1);
   PC2  : aliased GPIO_Point := (GPIO_C'Access, Pin_2);
   PC3  : aliased GPIO_Point := (GPIO_C'Access, Pin_3);
   PC4  : aliased GPIO_Point := (GPIO_C'Access, Pin_4);
   PC5  : aliased GPIO_Point := (GPIO_C'Access, Pin_5);
   PC6  : aliased GPIO_Point := (GPIO_C'Access, Pin_6);
   PC7  : aliased GPIO_Point := (GPIO_C'Access, Pin_7);
   PC8  : aliased GPIO_Point := (GPIO_C'Access, Pin_8);
   PC9  : aliased GPIO_Point := (GPIO_C'Access, Pin_9);
   PC10 : aliased GPIO_Point := (GPIO_C'Access, Pin_10);
   PC11 : aliased GPIO_Point := (GPIO_C'Access, Pin_11);
   PC12 : aliased GPIO_Point := (GPIO_C'Access, Pin_12);
   PC13 : aliased GPIO_Point := (GPIO_C'Access, Pin_13);
   PC14 : aliased GPIO_Point := (GPIO_C'Access, Pin_14);
   PC15 : aliased GPIO_Point := (GPIO_C'Access, Pin_15);
   PD0  : aliased GPIO_Point := (GPIO_D'Access, Pin_0);
   PD1  : aliased GPIO_Point := (GPIO_D'Access, Pin_1);
   PD2  : aliased GPIO_Point := (GPIO_D'Access, Pin_2);
   PD3  : aliased GPIO_Point := (GPIO_D'Access, Pin_3);
   PD4  : aliased GPIO_Point := (GPIO_D'Access, Pin_4);
   PD5  : aliased GPIO_Point := (GPIO_D'Access, Pin_5);
   PD6  : aliased GPIO_Point := (GPIO_D'Access, Pin_6);
   PD7  : aliased GPIO_Point := (GPIO_D'Access, Pin_7);
   PD8  : aliased GPIO_Point := (GPIO_D'Access, Pin_8);
   PD9  : aliased GPIO_Point := (GPIO_D'Access, Pin_9);
   PD10 : aliased GPIO_Point := (GPIO_D'Access, Pin_10);
   PD11 : aliased GPIO_Point := (GPIO_D'Access, Pin_11);
   PD12 : aliased GPIO_Point := (GPIO_D'Access, Pin_12);
   PD13 : aliased GPIO_Point := (GPIO_D'Access, Pin_13);
   PD14 : aliased GPIO_Point := (GPIO_D'Access, Pin_14);
   PD15 : aliased GPIO_Point := (GPIO_D'Access, Pin_15);
   PE0  : aliased GPIO_Point := (GPIO_E'Access, Pin_0);
   PE1  : aliased GPIO_Point := (GPIO_E'Access, Pin_1);
   PE2  : aliased GPIO_Point := (GPIO_E'Access, Pin_2);
   PE3  : aliased GPIO_Point := (GPIO_E'Access, Pin_3);
   PE4  : aliased GPIO_Point := (GPIO_E'Access, Pin_4);
   PE5  : aliased GPIO_Point := (GPIO_E'Access, Pin_5);
   PE6  : aliased GPIO_Point := (GPIO_E'Access, Pin_6);
   PE7  : aliased GPIO_Point := (GPIO_E'Access, Pin_7);
   PE8  : aliased GPIO_Point := (GPIO_E'Access, Pin_8);
   PE9  : aliased GPIO_Point := (GPIO_E'Access, Pin_9);
   PE10 : aliased GPIO_Point := (GPIO_E'Access, Pin_10);
   PE11 : aliased GPIO_Point := (GPIO_E'Access, Pin_11);
   PE12 : aliased GPIO_Point := (GPIO_E'Access, Pin_12);
   PE13 : aliased GPIO_Point := (GPIO_E'Access, Pin_13);
   PE14 : aliased GPIO_Point := (GPIO_E'Access, Pin_14);
   PE15 : aliased GPIO_Point := (GPIO_E'Access, Pin_15);
   PF0  : aliased GPIO_Point := (GPIO_F'Access, Pin_0);
   PF1  : aliased GPIO_Point := (GPIO_F'Access, Pin_1);
   PF2  : aliased GPIO_Point := (GPIO_F'Access, Pin_2);
   PF3  : aliased GPIO_Point := (GPIO_F'Access, Pin_3);
   PF4  : aliased GPIO_Point := (GPIO_F'Access, Pin_4);
   PF5  : aliased GPIO_Point := (GPIO_F'Access, Pin_5);
   PF6  : aliased GPIO_Point := (GPIO_F'Access, Pin_6);
   PF7  : aliased GPIO_Point := (GPIO_F'Access, Pin_7);
   PF8  : aliased GPIO_Point := (GPIO_F'Access, Pin_8);
   PF9  : aliased GPIO_Point := (GPIO_F'Access, Pin_9);
   PF10 : aliased GPIO_Point := (GPIO_F'Access, Pin_10);
   PF11 : aliased GPIO_Point := (GPIO_F'Access, Pin_11);
   PF12 : aliased GPIO_Point := (GPIO_F'Access, Pin_12);
   PF13 : aliased GPIO_Point := (GPIO_F'Access, Pin_13);
   PF14 : aliased GPIO_Point := (GPIO_F'Access, Pin_14);
   PF15 : aliased GPIO_Point := (GPIO_F'Access, Pin_15);
   PG0  : aliased GPIO_Point := (GPIO_G'Access, Pin_0);
   PG1  : aliased GPIO_Point := (GPIO_G'Access, Pin_1);
   PG2  : aliased GPIO_Point := (GPIO_G'Access, Pin_2);
   PG3  : aliased GPIO_Point := (GPIO_G'Access, Pin_3);
   PG4  : aliased GPIO_Point := (GPIO_G'Access, Pin_4);
   PG5  : aliased GPIO_Point := (GPIO_G'Access, Pin_5);
   PG6  : aliased GPIO_Point := (GPIO_G'Access, Pin_6);
   PG7  : aliased GPIO_Point := (GPIO_G'Access, Pin_7);
   PG8  : aliased GPIO_Point := (GPIO_G'Access, Pin_8);
   PG9  : aliased GPIO_Point := (GPIO_G'Access, Pin_9);
   PG10 : aliased GPIO_Point := (GPIO_G'Access, Pin_10);
   PG11 : aliased GPIO_Point := (GPIO_G'Access, Pin_11);
   PG12 : aliased GPIO_Point := (GPIO_G'Access, Pin_12);
   PG13 : aliased GPIO_Point := (GPIO_G'Access, Pin_13);
   PG14 : aliased GPIO_Point := (GPIO_G'Access, Pin_14);
   PG15 : aliased GPIO_Point := (GPIO_G'Access, Pin_15);

   GPIO_AF_RTC_50Hz_0  : constant GPIO_Alternate_Function;
   GPIO_AF_MCO_0       : constant GPIO_Alternate_Function;
   GPIO_AF_TAMPER_0    : constant GPIO_Alternate_Function;
   GPIO_AF_SWJ_0       : constant GPIO_Alternate_Function;
   GPIO_AF_TRACE_0     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM2_1      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM5_1      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM15_1     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM16_1     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM17_1     : constant GPIO_Alternate_Function;
   GPIO_AF_LPTIM1_1    : constant GPIO_Alternate_Function;
   GPIO_AF_I2C1_2      : constant GPIO_Alternate_Function;
   GPIO_AF_I2C3_2      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM1_2      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM2_2      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM3_2      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM4_2      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM5_2      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM8_2      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM15_2     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM20_2     : constant GPIO_Alternate_Function;
   GPIO_AF_COMP1_2     : constant GPIO_Alternate_Function;
   GPIO_AF_SPI1_3      : constant GPIO_Alternate_Function;
   GPIO_AF_I2C3_3      : constant GPIO_Alternate_Function;
   GPIO_AF_I2C4_3      : constant GPIO_Alternate_Function;
   GPIO_AF_SAI1_3      : constant GPIO_Alternate_Function;
   GPIO_AF_USB_3       : constant GPIO_Alternate_Function;
   GPIO_AF_HRTIM1_3    : constant GPIO_Alternate_Function;
   GPIO_AF_TIM8_3      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM15_3     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM20_3     : constant GPIO_Alternate_Function;
   GPIO_AF_COMP3_3     : constant GPIO_Alternate_Function;
   GPIO_AF_I2C1_4      : constant GPIO_Alternate_Function;
   GPIO_AF_I2C2_4      : constant GPIO_Alternate_Function;
   GPIO_AF_I2C3_4      : constant GPIO_Alternate_Function;
   GPIO_AF_I2C4_4      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM1_4      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM8_4      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM16_4     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM17_4     : constant GPIO_Alternate_Function;
   GPIO_AF_SPI1_5      : constant GPIO_Alternate_Function;
   GPIO_AF_SPI2_5      : constant GPIO_Alternate_Function;
   GPIO_AF_SPI3_5      : constant GPIO_Alternate_Function;
   GPIO_AF_SPI4_5      : constant GPIO_Alternate_Function;
   GPIO_AF_I2S2_5      : constant GPIO_Alternate_Function;
   GPIO_AF_I2S3_5      : constant GPIO_Alternate_Function;
   GPIO_AF_I2C4_5      : constant GPIO_Alternate_Function;
   GPIO_AF_UART4_5     : constant GPIO_Alternate_Function;
   GPIO_AF_UART5_5     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM8_5      : constant GPIO_Alternate_Function;
   GPIO_AF_INFRARED_5  : constant GPIO_Alternate_Function;
   GPIO_AF_SPI2_6      : constant GPIO_Alternate_Function;
   GPIO_AF_SPI3_6      : constant GPIO_Alternate_Function;
   GPIO_AF_I2S2_6      : constant GPIO_Alternate_Function;
   GPIO_AF_I2S3_6      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM1_6      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM5_6      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM8_6      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM20_6     : constant GPIO_Alternate_Function;
   GPIO_AF_INFRARED_6  : constant GPIO_Alternate_Function;
   GPIO_AF_USART1_7    : constant GPIO_Alternate_Function;
   GPIO_AF_USART2_7    : constant GPIO_Alternate_Function;
   GPIO_AF_USART3_7    : constant GPIO_Alternate_Function;
   GPIO_AF_FDCAN_7     : constant GPIO_Alternate_Function;
   GPIO_AF_COMP5_7     : constant GPIO_Alternate_Function;
   GPIO_AF_COMP6_7     : constant GPIO_Alternate_Function;
   GPIO_AF_COMP7_7     : constant GPIO_Alternate_Function;
   GPIO_AF_I2C3_8      : constant GPIO_Alternate_Function;
   GPIO_AF_I2C4_8      : constant GPIO_Alternate_Function;
   GPIO_AF_UART4_8     : constant GPIO_Alternate_Function;
   GPIO_AF_UART5_8     : constant GPIO_Alternate_Function;
   GPIO_AF_LPUART1_8   : constant GPIO_Alternate_Function;
   GPIO_AF_COMP1_8     : constant GPIO_Alternate_Function;
   GPIO_AF_COMP2_8     : constant GPIO_Alternate_Function;
   GPIO_AF_COMP3_8     : constant GPIO_Alternate_Function;
   GPIO_AF_COMP4_8     : constant GPIO_Alternate_Function;
   GPIO_AF_COMP5_8     : constant GPIO_Alternate_Function;
   GPIO_AF_COMP6_8     : constant GPIO_Alternate_Function;
   GPIO_AF_COMP7_8     : constant GPIO_Alternate_Function;
   GPIO_AF_FDCAN1_9    : constant GPIO_Alternate_Function;
   GPIO_AF_FDCAN2_9    : constant GPIO_Alternate_Function;
   GPIO_AF_TIM1_9      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM8_9      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM15_9     : constant GPIO_Alternate_Function;
   GPIO_AF_SPI1_10     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM1_10     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM2_10     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM3_10     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM4_10     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM7_10     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM8_10     : constant GPIO_Alternate_Function;
   GPIO_AF_LPTIM1_11   : constant GPIO_Alternate_Function;
   GPIO_AF_TIM1_11     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM8_11     : constant GPIO_Alternate_Function;
   GPIO_AF_FDCAN1_11   : constant GPIO_Alternate_Function;
   GPIO_AF_FDCAN3_11   : constant GPIO_Alternate_Function;
   GPIO_AF_FMC_12      : constant GPIO_Alternate_Function;
   GPIO_AF_LPUART1_12  : constant GPIO_Alternate_Function;
   GPIO_AF_SAI1_12     : constant GPIO_Alternate_Function;
   GPIO_AF_HRTIM1_12   : constant GPIO_Alternate_Function;
   GPIO_AF_TIM1_12     : constant GPIO_Alternate_Function;
   GPIO_AF_SAI1_13     : constant GPIO_Alternate_Function;
   GPIO_AF_HRTIM1_13   : constant GPIO_Alternate_Function;
   GPIO_AF_OPAMP2_13   : constant GPIO_Alternate_Function;
   GPIO_AF_UART4_14    : constant GPIO_Alternate_Function;
   GPIO_AF_UART5_14    : constant GPIO_Alternate_Function;
   GPIO_AF_SAI1_14     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM2_14     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM15_14    : constant GPIO_Alternate_Function;
   GPIO_AF_UCPD1_14    : constant GPIO_Alternate_Function;
   GPIO_AF_EVENTOUT_15 : constant GPIO_Alternate_Function;

   ---------
   -- ADC --
   ---------

   ADC_1 : aliased Analog_To_Digital_Converter
     with Volatile, Import, Address => ADC1_Base;
   ADC_2 : aliased Analog_To_Digital_Converter
     with Volatile, Import, Address => ADC2_Base;
   ADC_3 : aliased Analog_To_Digital_Converter
     with Volatile, Import, Address => ADC3_Base;
   ADC_4 : aliased Analog_To_Digital_Converter
     with Volatile, Import, Address => ADC4_Base;
   ADC_5 : aliased Analog_To_Digital_Converter
     with Volatile, Import, Address => ADC5_Base;

   Temperature_Channel : constant Analog_Input_Channel := 16;
   Temperature_Sensor  : constant ADC_Point :=
     (ADC_1'Access, Channel => Temperature_Channel);
   --  The internal temperature sensor (VTS) is intenally connected to
   --  ADC1_INP16 and ADC5_INP4. See RM0440 rev 6 pg 621 chapter 21.4.11.
   --  The VSENSESEL bit in the ADCxCCR register is used to switch to the
   --  temperature sensor.
   --  see RM0440 rev 6 pg 683, section 21.4.31, also pg 606.

   VRef_Channel : constant Analog_Input_Channel := 18;
   VRef_Sensor  : constant ADC_Point :=
     (ADC_1'Access, Channel => VRef_Channel);
   --  The internal reference voltage (VREFINT) is internally connected
   --  to ADC1_INP18, ADC3_INP18, ADC4_INP18 and ADC5_INP18.
   --  The VREFEN bit in the ADCxCCR register is used to switch to VREFINT.
   --  see RM0440 rev 6 pg 686, section 21.4.33, also pg 606.

   VBat_Channel : constant Analog_Input_Channel := 17;
   VBat_Sensor  : constant ADC_Point :=
     (ADC_1'Access, Channel => VBat_Channel);
   --  The internal temperature sensor (VTS) is intenally connected to
   --  ADC1_INP17, ADC3_INP17 and ADC5_INP17. See RM0440 rev 6 pg 621
   --  chapter 21.4.11.
   --  The VBATSEL bit in the ADCxCCR register is used to switch to the
   --  battery voltage.
   VBat : constant ADC_Point := (ADC_1'Access, Channel => VBat_Channel);

   VBat_Bridge_Divisor : constant := 3;
   --  The VBAT pin is internally connected to a bridge divider. The actual
   --  voltage is the raw conversion value * the divisor. See section 21.4.32,
   --  pg 685 of the RM0440 rev 6.

   procedure Enable_Clock (This : aliased Analog_To_Digital_Converter);

   procedure Reset_All_ADC_Units;

   type ADC_Clock_Source is (SYSCLK, PLLP);

   procedure Select_Clock_Source (This   : Analog_To_Digital_Converter;
                                  Source : ADC_Clock_Source);
   --  Set ADC12 or ADC345 Clock Mux Source.

   function Read_Clock_Source (This : Analog_To_Digital_Converter)
     return ADC_Clock_Source;
   --  Return ADC12 or ADC345 Clock Mux Source.

   ---------
   -- DAC --
   ---------

   DAC_1 : aliased Digital_To_Analog_Converter
     with Import, Volatile, Address => DAC1_Base;
   DAC_2 : aliased Digital_To_Analog_Converter
     with Import, Volatile, Address => DAC2_Base;
   DAC_3 : aliased Digital_To_Analog_Converter
     with Import, Volatile, Address => DAC3_Base;
   DAC_4 : aliased Digital_To_Analog_Converter
     with Import, Volatile, Address => DAC4_Base;

   DAC_1_OUT_1_IO : GPIO_Point renames PA4;
   DAC_1_OUT_2_IO : GPIO_Point renames PA5;
   DAC_2_OUT_1_IO : GPIO_Point renames PA6;

   procedure Enable_Clock
     (This : aliased Digital_To_Analog_Converter)
     with Inline;

   procedure Reset
     (This : aliased Digital_To_Analog_Converter)
     with Inline;

   -----------
   -- Audio --
   -----------

   subtype SAI_Port is STM32_SVD.SAI.SAI_Peripheral;

   SAI_1 : SAI_Port renames STM32_SVD.SAI.SAI_Periph;

   procedure Enable_Clock (This : SAI_Port);
   procedure Reset (This : SAI_Port);

   type SAI_Clock_Source is (SYSCLK, PLLQ, I2S_CKIN, HSI16);

   procedure Select_Clock_Source (This   : SAI_Port;
                                  Source : SAI_Clock_Source);
   --  Set SAI Clock Mux source.

   function Read_Clock_Source (This : SAI_Port) return SAI_Clock_Source;
   --  Return SAI Clock Mux source.

   function Get_Clock_Frequency (This : SAI_Port) return UInt32;

   ---------
   -- CRC --
   ---------

   CRC_Unit : CRC_32 with Import, Volatile, Address => CRC_Base;

   procedure Enable_Clock (This : CRC_32) with Inline;

   procedure Disable_Clock (This : CRC_32) with Inline;

   procedure Reset (This : CRC_32);

   ---------
   -- RNG --
   ---------

   RNG_Unit : RNG_Generator
     with Import, Volatile, Address => RNG_Base;

   procedure Enable_Clock (This : RNG_Generator) with Inline;

   procedure Disable_Clock (This : RNG_Generator) with Inline;

   procedure Reset (This : RNG_Generator);

   ------------
   -- CORDIC --
   ------------

   CORDIC_Unit : CORDIC_Coprocessor
     with Import, Volatile, Address => CORDIC_Base;

   procedure Enable_Clock (This : CORDIC_Coprocessor) with Inline;

   procedure Disable_Clock (This : CORDIC_Coprocessor) with Inline;

   procedure Reset (This : CORDIC_Coprocessor);

   ----------
   -- FMAC --
   ----------

   FMAC_Unit : FMAC_Accelerator with Import, Volatile, Address => FMAC_Base;

   procedure Enable_Clock (This : FMAC_Accelerator) with Inline;

   procedure Disable_Clock (This : FMAC_Accelerator) with Inline;

   procedure Reset (This : FMAC_Accelerator);

   ---------
   -- DMA --
   ---------

   DMA_1 : aliased DMA_Controller
     with Import, Volatile, Address => DMA1_Base;
   DMA_2 : aliased DMA_Controller
     with Import, Volatile, Address => DMA2_Base;

   procedure Enable_Clock (This : aliased DMA_Controller);
   procedure Reset (This : aliased DMA_Controller);

   -----------
   -- USART --
   -----------

   Internal_USART_1 : aliased Internal_USART
     with Import, Volatile, Address => USART1_Base;
   Internal_USART_2 : aliased Internal_USART
       with Import, Volatile, Address => USART2_Base;
   Internal_USART_3 : aliased Internal_USART
     with Import, Volatile, Address => USART3_Base;
   Internal_UART_4 : aliased Internal_USART
     with Import, Volatile, Address => UART4_Base;
   Internal_UART_5 : aliased Internal_USART
     with Import, Volatile, Address => UART5_Base;
   Internal_LPUART_1 : aliased Internal_USART
     with Import, Volatile, Address => LPUART1_Base;

   USART_1  : aliased USART (Internal_USART_1'Access);
   USART_2  : aliased USART (Internal_USART_2'Access);
   USART_3  : aliased USART (Internal_USART_3'Access);
   UART_4   : aliased USART (Internal_UART_4'Access);
   UART_5   : aliased USART (Internal_UART_5'Access);
   LPUART_1 : aliased USART (Internal_LPUART_1'Access);

   procedure Enable_Clock (This : aliased USART);

   procedure Reset (This : aliased USART);

   type USART_Clock_Source is (PCLK, SYSCLK, HSI16, LSE);
   --  Option   USART1    USART2345
   --  PCLK     PCLK2     PCLK1

   procedure Select_Clock_Source (This   : aliased USART;
                                  Source : USART_Clock_Source);

   function Read_Clock_Source (This : aliased USART)
     return USART_Clock_Source;

   function Get_Clock_Frequency (This : USART) return UInt32
     with Inline;
   --  Returns USART clock frequency, in Hertz.

   ---------
   -- CAN --
   ---------

   CAN_1 : aliased CAN_Controller with Volatile, Import, Address => FDCAN1_Base;
   CAN_2 : aliased CAN_Controller with Volatile, Import, Address => FDCAN2_Base;
   CAN_3 : aliased CAN_Controller with Volatile, Import, Address => FDCAN3_Base;

   procedure Enable_Clock (This : aliased CAN_Controller);
   --  There is only one clock for the three CANs.
   procedure Reset (This : aliased CAN_Controller);
   --  There is only one reset for the three CANs.

   type CAN_Clock_Source is (HSE, PLLQ, PCLK);
   --  Option   CAN123
   --  PCLK     PCLK1

   procedure Select_Clock_Source (This   : aliased CAN_Controller;
                                  Source : CAN_Clock_Source);

   function Read_Clock_Source (This : aliased CAN_Controller)
     return CAN_Clock_Source;

   function Get_Clock_Frequency (This : aliased CAN_Controller)
     return UInt32;
   --  Returns FDCAN clock frequency, in Hertz.

   ---------
   -- I2C --
   ---------

   Internal_I2C_Port_1 : aliased Internal_I2C_Port
     with Import, Volatile, Address => I2C1_Base;
   Internal_I2C_Port_2 : aliased Internal_I2C_Port
     with Import, Volatile, Address => I2C2_Base;
   Internal_I2C_Port_3 : aliased Internal_I2C_Port
     with Import, Volatile, Address => I2C3_Base;
   Internal_I2C_Port_4 : aliased Internal_I2C_Port
     with Import, Volatile, Address => I2C4_Base;

   type I2C_Port_Id is (I2C_Id_1, I2C_Id_2, I2C_Id_3, I2C_Id_4);

   I2C_1 : aliased I2C_Port (Internal_I2C_Port_1'Access);
   I2C_2 : aliased I2C_Port (Internal_I2C_Port_2'Access);
   I2C_3 : aliased I2C_Port (Internal_I2C_Port_3'Access);
   I2C_4 : aliased I2C_Port (Internal_I2C_Port_4'Access);

   --  I2C_1_DMA : aliased I2C_Port_DMA (Internal_I2C_Port_1'Access);
   --  I2C_2_DMA : aliased I2C_Port_DMA (Internal_I2C_Port_2'Access);
   --  I2C_3_DMA : aliased I2C_Port_DMA (Internal_I2C_Port_3'Access);
   --  I2C_4_DMA : aliased I2C_Port_DMA (Internal_I2C_Port_4'Access);

   function As_Port_Id (Port : I2C_Port'Class) return I2C_Port_Id with Inline;

   procedure Enable_Clock (This : aliased I2C_Port'Class);
   procedure Enable_Clock (This : I2C_Port_Id);

   procedure Reset (This : I2C_Port'Class);
   procedure Reset (This : I2C_Port_Id);

   type I2C_Clock_Source is (PCLK, SYSCLK, HSI16);

   procedure Select_Clock_Source (This   : I2C_Port'Class;
                                  Source : I2C_Clock_Source);

   procedure Select_Clock_Source (This   : I2C_Port_Id;
                                  Source : I2C_Clock_Source);
   --  Set I2C Clock Mux source.

   function Read_Clock_Source (This : I2C_Port'Class) return I2C_Clock_Source;

   function Read_Clock_Source (This : I2C_Port_Id) return I2C_Clock_Source;
   --  Returns I2C Clock Mux source.

   ---------
   -- SPI --
   ---------

   Internal_SPI_1 : aliased Internal_SPI_Port
     with Import, Volatile, Address => SPI1_Base;
   Internal_SPI_2 : aliased Internal_SPI_Port
     with Import, Volatile, Address => SPI2_Base;
   Internal_SPI_3 : aliased Internal_SPI_Port
     with Import, Volatile, Address => SPI3_Base;
   Internal_SPI_4 : aliased Internal_SPI_Port
     with Import, Volatile, Address => SPI4_Base;

   SPI_1 : aliased SPI_Port (Internal_SPI_1'Access);
   SPI_2 : aliased SPI_Port (Internal_SPI_2'Access);
   SPI_3 : aliased SPI_Port (Internal_SPI_3'Access);
   SPI_4 : aliased SPI_Port (Internal_SPI_4'Access);

   SPI_1_DMA : aliased SPI_Port_DMA (Internal_SPI_1'Access);
   SPI_2_DMA : aliased SPI_Port_DMA (Internal_SPI_2'Access);
   SPI_3_DMA : aliased SPI_Port_DMA (Internal_SPI_3'Access);
   SPI_4_DMA : aliased SPI_Port_DMA (Internal_SPI_4'Access);

   procedure Enable_Clock (This : SPI_Port'Class);
   procedure Reset (This : SPI_Port'Class);

   type SPI_Clock_Source is (SYSCLK, PLLQ, I2S_CKIN, HSI16);

   procedure Select_Clock_Source (This   : SPI_Port'Class;
                                  Source : SPI_Clock_Source)
     with Pre => This'Address = SPI2_Base or This'Address = SPI3_Base,
          Post => Read_Clock_Source (This) = Source;
   --  Set SPI Clock Mux source (the same source for SPI2 .. SPI3).

   function Read_Clock_Source (This : SPI_Port'Class) return SPI_Clock_Source
     with Pre => This'Address = SPI2_Base or This'Address = SPI3_Base;
   --  Return SPI Clock Mux source.

   ---------
   -- I2S --
   ---------

   Internal_I2S_2     : aliased Internal_I2S_Port
     with Import, Volatile, Address => SPI2_Base;
   Internal_I2S_3     : aliased Internal_I2S_Port
     with Import, Volatile, Address => SPI3_Base;

   I2S_2 : aliased I2S_Port (Internal_I2S_2'Access, Extended => False);
   I2S_3 : aliased I2S_Port (Internal_I2S_3'Access, Extended => False);

   procedure Enable_Clock (This : I2S_Port);
   --  The I2S_2 and I2S_3 peripherals use the SPI interface hardware, that are
   --  mapped to SPI2 and SPI3. SPI1 and SPI4 don't have the I2S mode.

   procedure Reset (This : I2S_Port);
   --  The I2S_2 and I2S_3 peripherals use the SPI interface hardware, that are
   --  mapped to SPI2 and SPI3. SPI1 and SPI4 don't have the I2S mode.

   type I2S_Clock_Source is (SYSCLK, PLLQ, I2S_CKIN, HSI16);

   procedure Select_Clock_Source (This   : I2S_Port'Class;
                                  Source : I2S_Clock_Source)
     with Post => Read_Clock_Source (This) = Source;
   --  Set I2S Clock Mux source (the same source for I2S2 .. I2S3).

   function Read_Clock_Source (This : I2S_Port'Class) return I2S_Clock_Source;
   --  Returns I2S Clock Mux source.

   function Get_Clock_Frequency (This : I2S_Port) return UInt32;
   --  Returns I2S clock frequency, in Hertz.

   ---------
   -- RTC --
   ---------

   RTC : aliased RTC_Device;

   procedure Enable_Clock (This : RTC_Device);

   type RTC_Clock_Source is (No_Clock, LSE, LSI, HSE)
     with Size => 2;

   procedure Select_Clock_Source
     (This       : RTC_Device;
      Source     : RTC_Clock_Source)
     with Post => Read_Clock_Source (This) = Source;
   --  Set RTC Clock Mux source. Once the RTC clock source has been selected,
   --  it cannot be changed anymore unless the RTC domain is reset, or unless
   --  a failure is detected on LSE (LSECSSD is set). The BDRST bit can be used
   --  to reset them.
   --  The HSE clock is divided by 32 before entering the RTC to assure it is
   --  < 1 MHz.

   function Read_Clock_Source (This : RTC_Device) return RTC_Clock_Source;
   --  Return RTC Clock Mux source.

   -----------
   -- Timer --
   -----------

   Timer_1   : aliased Timer with Import, Volatile, Address => TIM1_Base;
   Timer_2   : aliased Timer with Import, Volatile, Address => TIM2_Base;
   Timer_3   : aliased Timer with Import, Volatile, Address => TIM3_Base;
   Timer_4   : aliased Timer with Import, Volatile, Address => TIM4_Base;
   Timer_5   : aliased Timer with Import, Volatile, Address => TIM5_Base;
   Timer_6   : aliased Timer with Import, Volatile, Address => TIM6_Base;
   Timer_7   : aliased Timer with Import, Volatile, Address => TIM7_Base;
   Timer_8   : aliased Timer with Import, Volatile, Address => TIM8_Base;
   Timer_15  : aliased Timer with Import, Volatile, Address => TIM15_Base;
   Timer_16  : aliased Timer with Import, Volatile, Address => TIM16_Base;
   Timer_17  : aliased Timer with Import, Volatile, Address => TIM17_Base;
   Timer_20  : aliased Timer with Import, Volatile, Address => TIM20_Base;

   procedure Enable_Clock (This : Timer);

   procedure Reset (This : Timer);

   function Get_Clock_Frequency (This : Timer) return UInt32;
   --  Return the timer input frequency in Hz.

   -------------
   -- LPTimer --
   -------------

   LPTimer_1 : aliased LPTimer
     with Import, Volatile, Address => LPTIMER1_Base;

   procedure Enable_Clock (This : LPTimer);

   procedure Reset (This : LPTimer);

   type LPTimer_Clock_Source_Enum is (PCLK1, LSI, HSI, LSE) with Size => 2;

   type LPTimer_Clock_Source is record
      External : Boolean := False;
      Clock    : LPTimer_Clock_Source_Enum := LPTimer_Clock_Source_Enum'First;
   end record;

   for LPTimer_Clock_Source use record
      External at 0 range 2 .. 2;
      Clock    at 0 range 0 .. 1;
   end record;

   procedure Select_Clock_Source (This   : LPTimer;
                                  Source : LPTimer_Clock_Source);
   --  Set clock to any internal LPTIM Clock Mux source or external through
   --  Input1.

   function Read_Clock_Source (This : LPTimer) return LPTimer_Clock_Source;
   --  Return LPTIM1 Clock Mux source.

   function Get_Clock_Frequency (This : LPTimer) return UInt32;
   --  Return the timer input frequency in Hz.

   -------------
   -- HRTimer --
   -------------

   HRTimer_M : aliased HRTimer_Master
     with Import, Volatile, Address => HRTIM_Master_Base;

   HRTimer_A : aliased HRTimer_Channel
     with Import, Volatile, Address => HRTIM_TIMA_Base;
   HRTimer_B : aliased HRTimer_Channel
     with Import, Volatile, Address => HRTIM_TIMB_Base;
   HRTimer_C : aliased HRTimer_Channel
     with Import, Volatile, Address => HRTIM_TIMC_Base;
   HRTimer_D : aliased HRTimer_Channel
     with Import, Volatile, Address => HRTIM_TIMD_Base;
   HRTimer_E : aliased HRTimer_Channel
     with Import, Volatile, Address => HRTIM_TIME_Base;
   HRTimer_F : aliased HRTimer_Channel
     with Import, Volatile, Address => HRTIM_TIMF_Base;

   procedure Enable_Clock (This : HRTimer_Master);

   procedure Enable_Clock (This : HRTimer_Channel);

   procedure Reset (This : HRTimer_Master);

   procedure Reset (This : HRTimer_Channel);

   function Get_Clock_Frequency (This : HRTimer_Master) return UInt32;
   --  Returns the timer input frequency in Hz.

   function Get_Clock_Frequency (This : HRTimer_Channel) return UInt32;
   --  Returns the timer input frequency in Hz.

   ----------------
   -- Comparator --
   ----------------

   Comp_1 : aliased Comparator
     with Import, Volatile,
     Address => STM32_SVD.COMP.COMP_Periph.C1CSR'Address;
   Comp_2 : aliased Comparator
     with Import, Volatile,
     Address => STM32_SVD.COMP.COMP_Periph.C2CSR'Address;
   Comp_3 : aliased Comparator
     with Import, Volatile,
     Address => STM32_SVD.COMP.COMP_Periph.C3CSR'Address;
   Comp_4 : aliased Comparator
     with Import, Volatile,
     Address => STM32_SVD.COMP.COMP_Periph.C4CSR'Address;
   Comp_5 : aliased Comparator
     with Import, Volatile,
     Address => STM32_SVD.COMP.COMP_Periph.C5CSR'Address;
   Comp_6 : aliased Comparator
     with Import, Volatile,
     Address => STM32_SVD.COMP.COMP_Periph.C6CSR'Address;
   Comp_7 : aliased Comparator
     with Import, Volatile,
     Address => STM32_SVD.COMP.COMP_Periph.C7CSR'Address;

   -----------
   -- OpAmp --
   -----------

   Opamp_1 : aliased Operational_Amplifier
     with Import, Volatile,
     Address => STM32_SVD.OPAMP.OPAMP_Periph.OPAMP1_CSR'Address;
   Opamp_2 : aliased Operational_Amplifier
     with Import, Volatile,
     Address => STM32_SVD.OPAMP.OPAMP_Periph.OPAMP2_CSR'Address;
   Opamp_3 : aliased Operational_Amplifier
     with Import, Volatile,
     Address => STM32_SVD.OPAMP.OPAMP_Periph.OPAMP3_CSR'Address;
   Opamp_4 : aliased Operational_Amplifier
     with Import, Volatile,
     Address => STM32_SVD.OPAMP.OPAMP_Periph.OPAMP4_CSR'Address;
   Opamp_5 : aliased Operational_Amplifier
     with Import, Volatile,
     Address => STM32_SVD.OPAMP.OPAMP_Periph.OPAMP5_CSR'Address;
   Opamp_6 : aliased Operational_Amplifier
     with Import, Volatile,
     Address => STM32_SVD.OPAMP.OPAMP_Periph.OPAMP6_CSR'Address;

   -----------------------------
   -- Reset and Clock Control --
   -----------------------------

   --  See RM0440 rev. 6 pg. 276 chapter 7.2, and pg. 279 for clock tree
   type RCC_System_Clocks is record
      SYSCLK  : UInt32; --  PLLR, PLLCLK
      HCLK    : UInt32;
      PCLK1   : UInt32;
      PCLK2   : UInt32;
      TIMCLK1 : UInt32; --  For TIMs 2 .. 7
      TIMCLK2 : UInt32; --  For TIMs 1, 8, 20, 15 .. 17, HRTIM1
      TIMCLK3 : UInt32; --  For LPTIMs 1 .. 2
      PLLP    : UInt32; --  PLLP
      PLLQ    : UInt32; --  PLLQ
   end record;

   function System_Clock_Frequencies return RCC_System_Clocks;
   --  Returns each RCC system clock frequency in Hz.

private

   GPIO_AF_RTC_50Hz_0  : constant GPIO_Alternate_Function := 0;
   GPIO_AF_MCO_0       : constant GPIO_Alternate_Function := 0;
   GPIO_AF_TAMPER_0    : constant GPIO_Alternate_Function := 0;
   GPIO_AF_SWJ_0       : constant GPIO_Alternate_Function := 0;
   GPIO_AF_TRACE_0     : constant GPIO_Alternate_Function := 0;
   GPIO_AF_TIM2_1      : constant GPIO_Alternate_Function := 1;
   GPIO_AF_TIM5_1      : constant GPIO_Alternate_Function := 1;
   GPIO_AF_TIM15_1     : constant GPIO_Alternate_Function := 1;
   GPIO_AF_TIM16_1     : constant GPIO_Alternate_Function := 1;
   GPIO_AF_TIM17_1     : constant GPIO_Alternate_Function := 1;
   GPIO_AF_LPTIM1_1    : constant GPIO_Alternate_Function := 1;
   GPIO_AF_I2C1_2      : constant GPIO_Alternate_Function := 2;
   GPIO_AF_I2C3_2      : constant GPIO_Alternate_Function := 2;
   GPIO_AF_TIM1_2      : constant GPIO_Alternate_Function := 2;
   GPIO_AF_TIM2_2      : constant GPIO_Alternate_Function := 2;
   GPIO_AF_TIM3_2      : constant GPIO_Alternate_Function := 2;
   GPIO_AF_TIM4_2      : constant GPIO_Alternate_Function := 2;
   GPIO_AF_TIM5_2      : constant GPIO_Alternate_Function := 2;
   GPIO_AF_TIM8_2      : constant GPIO_Alternate_Function := 2;
   GPIO_AF_TIM15_2     : constant GPIO_Alternate_Function := 2;
   GPIO_AF_TIM20_2     : constant GPIO_Alternate_Function := 2;
   GPIO_AF_COMP1_2     : constant GPIO_Alternate_Function := 2;
   GPIO_AF_SPI1_3      : constant GPIO_Alternate_Function := 3;
   GPIO_AF_I2C3_3      : constant GPIO_Alternate_Function := 3;
   GPIO_AF_I2C4_3      : constant GPIO_Alternate_Function := 3;
   GPIO_AF_SAI1_3      : constant GPIO_Alternate_Function := 3;
   GPIO_AF_USB_3       : constant GPIO_Alternate_Function := 3;
   GPIO_AF_HRTIM1_3    : constant GPIO_Alternate_Function := 3;
   GPIO_AF_TIM8_3      : constant GPIO_Alternate_Function := 3;
   GPIO_AF_TIM15_3     : constant GPIO_Alternate_Function := 3;
   GPIO_AF_TIM20_3     : constant GPIO_Alternate_Function := 3;
   GPIO_AF_COMP3_3     : constant GPIO_Alternate_Function := 3;
   GPIO_AF_I2C1_4      : constant GPIO_Alternate_Function := 4;
   GPIO_AF_I2C2_4      : constant GPIO_Alternate_Function := 4;
   GPIO_AF_I2C3_4      : constant GPIO_Alternate_Function := 4;
   GPIO_AF_I2C4_4      : constant GPIO_Alternate_Function := 4;
   GPIO_AF_TIM1_4      : constant GPIO_Alternate_Function := 4;
   GPIO_AF_TIM8_4      : constant GPIO_Alternate_Function := 4;
   GPIO_AF_TIM16_4     : constant GPIO_Alternate_Function := 4;
   GPIO_AF_TIM17_4     : constant GPIO_Alternate_Function := 4;
   GPIO_AF_SPI1_5      : constant GPIO_Alternate_Function := 5;
   GPIO_AF_SPI2_5      : constant GPIO_Alternate_Function := 5;
   GPIO_AF_SPI3_5      : constant GPIO_Alternate_Function := 5;
   GPIO_AF_SPI4_5      : constant GPIO_Alternate_Function := 5;
   GPIO_AF_I2S2_5      : constant GPIO_Alternate_Function := 5;
   GPIO_AF_I2S3_5      : constant GPIO_Alternate_Function := 5;
   GPIO_AF_I2C4_5      : constant GPIO_Alternate_Function := 5;
   GPIO_AF_UART4_5     : constant GPIO_Alternate_Function := 5;
   GPIO_AF_UART5_5     : constant GPIO_Alternate_Function := 5;
   GPIO_AF_TIM8_5      : constant GPIO_Alternate_Function := 6;
   GPIO_AF_INFRARED_5  : constant GPIO_Alternate_Function := 6;
   GPIO_AF_SPI2_6      : constant GPIO_Alternate_Function := 6;
   GPIO_AF_SPI3_6      : constant GPIO_Alternate_Function := 6;
   GPIO_AF_I2S2_6      : constant GPIO_Alternate_Function := 6;
   GPIO_AF_I2S3_6      : constant GPIO_Alternate_Function := 6;
   GPIO_AF_TIM1_6      : constant GPIO_Alternate_Function := 6;
   GPIO_AF_TIM5_6      : constant GPIO_Alternate_Function := 6;
   GPIO_AF_TIM8_6      : constant GPIO_Alternate_Function := 6;
   GPIO_AF_TIM20_6     : constant GPIO_Alternate_Function := 6;
   GPIO_AF_INFRARED_6  : constant GPIO_Alternate_Function := 6;
   GPIO_AF_USART1_7    : constant GPIO_Alternate_Function := 7;
   GPIO_AF_USART2_7    : constant GPIO_Alternate_Function := 7;
   GPIO_AF_USART3_7    : constant GPIO_Alternate_Function := 7;
   GPIO_AF_FDCAN_7     : constant GPIO_Alternate_Function := 7;
   GPIO_AF_COMP5_7     : constant GPIO_Alternate_Function := 7;
   GPIO_AF_COMP6_7     : constant GPIO_Alternate_Function := 7;
   GPIO_AF_COMP7_7     : constant GPIO_Alternate_Function := 7;
   GPIO_AF_I2C3_8      : constant GPIO_Alternate_Function := 8;
   GPIO_AF_I2C4_8      : constant GPIO_Alternate_Function := 8;
   GPIO_AF_UART4_8     : constant GPIO_Alternate_Function := 8;
   GPIO_AF_UART5_8     : constant GPIO_Alternate_Function := 8;
   GPIO_AF_LPUART1_8   : constant GPIO_Alternate_Function := 8;
   GPIO_AF_COMP1_8     : constant GPIO_Alternate_Function := 8;
   GPIO_AF_COMP2_8     : constant GPIO_Alternate_Function := 8;
   GPIO_AF_COMP3_8     : constant GPIO_Alternate_Function := 8;
   GPIO_AF_COMP4_8     : constant GPIO_Alternate_Function := 8;
   GPIO_AF_COMP5_8     : constant GPIO_Alternate_Function := 8;
   GPIO_AF_COMP6_8     : constant GPIO_Alternate_Function := 8;
   GPIO_AF_COMP7_8     : constant GPIO_Alternate_Function := 8;
   GPIO_AF_FDCAN1_9    : constant GPIO_Alternate_Function := 9;
   GPIO_AF_FDCAN2_9    : constant GPIO_Alternate_Function := 9;
   GPIO_AF_TIM1_9      : constant GPIO_Alternate_Function := 9;
   GPIO_AF_TIM8_9      : constant GPIO_Alternate_Function := 9;
   GPIO_AF_TIM15_9     : constant GPIO_Alternate_Function := 9;
   GPIO_AF_SPI1_10     : constant GPIO_Alternate_Function := 10;
   GPIO_AF_TIM1_10     : constant GPIO_Alternate_Function := 10;
   GPIO_AF_TIM2_10     : constant GPIO_Alternate_Function := 10;
   GPIO_AF_TIM3_10     : constant GPIO_Alternate_Function := 10;
   GPIO_AF_TIM4_10     : constant GPIO_Alternate_Function := 10;
   GPIO_AF_TIM7_10     : constant GPIO_Alternate_Function := 10;
   GPIO_AF_TIM8_10     : constant GPIO_Alternate_Function := 10;
   GPIO_AF_LPTIM1_11   : constant GPIO_Alternate_Function := 11;
   GPIO_AF_TIM1_11     : constant GPIO_Alternate_Function := 11;
   GPIO_AF_TIM8_11     : constant GPIO_Alternate_Function := 11;
   GPIO_AF_FDCAN1_11   : constant GPIO_Alternate_Function := 11;
   GPIO_AF_FDCAN3_11   : constant GPIO_Alternate_Function := 11;
   GPIO_AF_FMC_12      : constant GPIO_Alternate_Function := 12;
   GPIO_AF_LPUART1_12  : constant GPIO_Alternate_Function := 12;
   GPIO_AF_SAI1_12     : constant GPIO_Alternate_Function := 12;
   GPIO_AF_HRTIM1_12   : constant GPIO_Alternate_Function := 12;
   GPIO_AF_TIM1_12     : constant GPIO_Alternate_Function := 12;
   GPIO_AF_SAI1_13     : constant GPIO_Alternate_Function := 13;
   GPIO_AF_HRTIM1_13   : constant GPIO_Alternate_Function := 13;
   GPIO_AF_OPAMP2_13   : constant GPIO_Alternate_Function := 13;
   GPIO_AF_UART4_14    : constant GPIO_Alternate_Function := 14;
   GPIO_AF_UART5_14    : constant GPIO_Alternate_Function := 14;
   GPIO_AF_SAI1_14     : constant GPIO_Alternate_Function := 14;
   GPIO_AF_TIM2_14     : constant GPIO_Alternate_Function := 14;
   GPIO_AF_TIM15_14    : constant GPIO_Alternate_Function := 14;
   GPIO_AF_UCPD1_14    : constant GPIO_Alternate_Function := 14;
   GPIO_AF_EVENTOUT_15 : constant GPIO_Alternate_Function := 15;

end STM32.Device;
