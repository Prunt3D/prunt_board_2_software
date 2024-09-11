------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2015-2017, AdaCore                        --
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
--   @file    stm32f4xx_hal_adc.h                                           --
--   @author  MCD Application Team                                          --
--   @version V1.3.1                                                        --
--   @date    25-March-2015                                                 --
--   @brief   Header file of ADC HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides interfaces for the analog-to-digital converters on the
--  STM32G4 (ARM Cortex M4F) microcontrollers from ST Microelectronics.

--  Channels are mapped to GPIO_Point values as follows.  See the STM32G474xx
--  datasheet DS1228 rev 5 pg 57 chapter 4.10 Table 12. "STM32G474xB(C,E) pin
--  definitions" in the column "Aditional functions".
--
--  Channel    ADC    ADC    ADC    ADC    ADC
--    #         1      2      3      4      5
--
--    0
--    1        PA0    PA0    PB1    PE14   PA8
--    2        PA1    PA1    PE9    PE15   PA9
--    3        PA2    PA6    PE13   PB12
--    4        PA3    PA7    PE7    PB14
--    5        PB14   PC4    PB13   PB15
--    6        PC0    PC0    PE8    PE8    PE8
--    7        PC1    PC1    PD10   PD10   PD10
--    8        PC2    PC2    PD11   PD11   PD11
--    9        PC3    PC3    PD12   PD12   PD12
--   10        PF0    PF1    PD13   PD13   PD13
--   11        PB12   PC5    PD14   PD14   PD14
--   12               PB2    PB0    PD8    PD8
--   13               PA5           PD9    PD9
--   14        PB11   PB11   PE10   PE10   PE10
--   15               PB15   PE11   PE11   PE11
--   16                      PE12   PE12   PE12
--   17               PA4

with System;        use System;
with Ada.Real_Time; use Ada.Real_Time;

private with STM32_SVD.ADC;

package STM32.ADC is
   pragma Elaborate_Body;

   type Analog_To_Digital_Converter is limited private;

   subtype Analog_Input_Channel is UInt5 range 0 .. 18;

   type ADC_Point is record
      ADC     : access Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel;
   end record;

   VRef_Channel : constant Analog_Input_Channel := 18;
   --  See RM0440 rev 6 pg 621 chapter 21.4.11.
   --  Note available with ADC_1 and ADC_2.

   VBat_Channel : constant Analog_Input_Channel := 17;
   --  See RM0440 rev 6 pg 621 chapter 21.4.11.
   --  Note only available with ADC_1.

   subtype TemperatureSensor_Channel is Analog_Input_Channel;
   --  TODO: ??? The below predicate does not compile with GNAT GPL 2015.
   --  with Static_Predicate => TemperatureSensor_Channel in 16 | VBat_Channel;
   --  See RM pg 389 section 13.3.3. On some MCUs the temperature channel is
   --  the same as the VBat channel, on others it is channel 16. Note only
   --  available with ADC_1.

   ADC_Supply_Voltage : constant := 3000;  -- millivolts
   --  This is the ideal value, likely not the actual.

   procedure Enable (This : in out Analog_To_Digital_Converter) with
     Pre => not Conversion_Started (This) and
            not Injected_Conversion_Started (This),
     Post => Enabled (This);

   procedure Disable (This : in out Analog_To_Digital_Converter) with
     Pre => not Conversion_Started (This) and
            not Injected_Conversion_Started (This),
     Post => not Enabled (This);

   function Enabled (This : Analog_To_Digital_Converter) return Boolean;

   type Input_Convertion_Mode is (Single_Ended, Differential);

   procedure Calibrate
     (This       : in out Analog_To_Digital_Converter;
      Convertion : Input_Convertion_Mode)
     with Pre => not Enabled (This);
   --  Calibration is preliminary to any ADC operation. It removes the offset
   --  error which may vary from chip to chip due to process or bandgap variation.
   --  During the procedure, the ADC calculates a calibration factor which is
   --  7-bit wide and which is applied internally to the ADC until the next ADC
   --  power-off. The calibration factor to be applied for single-ended input
   --  conversions is different from the factor to be applied for differential
   --  input conversions. See RM0440 rev. 8 chapter 21.4.8 Calibration.

   function Get_Calibration_Factor
     (This       : in out Analog_To_Digital_Converter;
      Convertion : Input_Convertion_Mode) return UInt7
     with Pre => Enabled (This);
   --  Read the internal analog calibration factor.

   procedure Set_Calibration_Factor
     (This       : in out Analog_To_Digital_Converter;
      Convertion : Input_Convertion_Mode;
      Value      : UInt7)
     with Pre => Enabled (This) and
                 not Conversion_Started (This) and
                 not Injected_Conversion_Started (This),
          Post => Get_Calibration_Factor (This, Convertion) = Value;
   --  The internal analog calibration is lost each time the power of the ADC is
   --  removed (example, when the product enters in Standby or VBAT mode). In
   --  this case, to avoid spending time recalibrating the ADC, it is possible
   --  to re-write the calibration factor into the ADC_CALFACT register without
   --  recalibrating, supposing that the software has previously saved the
   --  calibration factor delivered during the previous calibration.

   procedure Set_Convertion_Mode
     (This       : in out Analog_To_Digital_Converter;
      Channel    : Analog_Input_Channel;
      Convertion : Input_Convertion_Mode)
     with Pre => not Enabled (This) and
                 not Conversion_Started (This) and
                 not Injected_Conversion_Started (This);
   --  Channels can be configured to be either single-ended input or differential
   --  input by programming DIFSEL[i] bits in the ADC_DIFSEL register.
   --  In single-ended input mode, the analog voltage to be converted for channel
   --  “i” is the difference between the ADCy_INPx external voltage equal to
   --  VINP[i] (positive input) and VREF− (negative input).
   --  In differential input mode, the analog voltage to be converted for channel
   --  “i” is the difference between the ADCy_INPx external voltage positive
   --  input equal to VINP[i], and the ADCy_INNx negative input equal to VINN[i].
   --  See RM0440 rev 8 chapter 21.4.7 Single-ended and differential input channels.

   type ADC_Resolution is
     (ADC_Resolution_12_Bits,  -- 15 ADC Clock cycles
      ADC_Resolution_10_Bits,  -- 12 ADC Clock cycles
      ADC_Resolution_8_Bits,   -- 10 ADC Clock cycles
      ADC_Resolution_6_Bits);  --  8 ADC Clock cycles

   type Data_Alignment is (Right_Aligned, Left_Aligned);

   procedure Configure_Unit
     (This       : in out Analog_To_Digital_Converter;
      Resolution : ADC_Resolution;
      Alignment  : Data_Alignment)
     with Post => Current_Resolution (This) = Resolution and
                  Current_Alignment (This) = Alignment;

   function Current_Resolution (This : Analog_To_Digital_Converter)
      return ADC_Resolution;

   function Current_Alignment (This : Analog_To_Digital_Converter)
      return Data_Alignment;

   type Channel_Sampling_Times is
     (Sample_2P5_Cycles,
      Sample_6P5_Cycles,
      Sample_12P5_Cycles,
      Sample_24P5_Cycles,
      Sample_47P5_Cycles,
      Sample_92P5_Cycles,
      Sample_247P5_Cycles,
      Sample_640P5_Cycles)
     with Size => 3;
   --  The elapsed time between the start of a conversion and the end of
   --  conversion is the sum of the configured sampling time plus the
   --  successive approximation time (SAR = 12.5 for 12 bit) depending on data
   --  resolution. See RM0440 rev 6 chapter 21.4.16 Timing.

   type External_Trigger is
     (Trigger_Disabled,
      Trigger_Rising_Edge,
      Trigger_Falling_Edge,
      Trigger_Both_Edges);

   type Regular_Channel_Rank is new Natural range 1 .. 16;

   type Injected_Channel_Rank is new Natural range 1 .. 4;

   type External_Events_Regular_Group is
     (Option_1,
      Option_2,
      Timer1_CC3_Event,
      Option_4,
      Timer3_TRGO_Event,
      Option_6,
      Option_7,
      Timer8_TRGO_Event,
      Timer8_TRGO2_Event,
      Timer1_TRGO_Event,
      Timer1_TRGO2_Event,
      Timer2_TRGO_Event,
      Timer4_TRGO_Event,
      Timer6_TRGO_Event,
      Timer15_TRGO_Event,
      Option_16,
      Timer20_TRGO_Event,
      Timer20_TRGO2_Event,
      Timer20_CC1_Event,
      Option_20,
      Option_21,
      HRTimer_ADC_TRG1_Event,
      HRTimer_ADC_TRG3_Event,
      HRTimer_ADC_TRG5_Event,
      HRTimer_ADC_TRG6_Event,
      HRTimer_ADC_TRG7_Event,
      HRTimer_ADC_TRG8_Event,
      HRTimer_ADC_TRG9_Event,
      HRTimer_ADC_TRG10_Event,
      LPTimer_OUT_Event,
      Timer7_TRGO_Event)
     with Size => 5;
   --  External triggers for regular channels.
   --  RM0440 Table 163 pg. 629 for ADC12 and Table 165 pg. 631 for ADC345.
   --  Differences between ADC12 and ADC345:
   --  Option    ADC12                       ADC345
   --  1         Timer1_CC1_Event            Timer3_CC1_Event
   --  2         Timer1_CC2_Event            Timer2_CC3_Event
   --  4         Timer2_CC2_Event            Timer8_CC1
   --  6         Timer4_CC4_Event            EXTI_Line2
   --  7         EXTI_Line11                 Timer4_CC1
   --  16        Timer3_CC4_Event            Timer2_CC1
   --  20        Timer20_CC2_Event           HRTimer_ADC_TRG2_Event
   --  21        Timer20_CC3_Event           HRTimer_ADC_TRG4_Event

   for External_Events_Regular_Group use
     (Option_1                => 2#00000#,
      Option_2                => 2#00001#,
      Timer1_CC3_Event        => 2#00010#,
      Option_4                => 2#00011#,
      Timer3_TRGO_Event       => 2#00100#,
      Option_6                => 2#00101#,
      Option_7                => 2#00110#,
      Timer8_TRGO_Event       => 2#00111#,
      Timer8_TRGO2_Event      => 2#01000#,
      Timer1_TRGO_Event       => 2#01001#,
      Timer1_TRGO2_Event      => 2#01010#,
      Timer2_TRGO_Event       => 2#01011#,
      Timer4_TRGO_Event       => 2#01100#,
      Timer6_TRGO_Event       => 2#01101#,
      Timer15_TRGO_Event      => 2#01110#,
      Option_16               => 2#01111#,
      Timer20_TRGO_Event      => 2#10000#,
      Timer20_TRGO2_Event     => 2#10001#,
      Timer20_CC1_Event       => 2#10010#,
      Option_20               => 2#10011#,
      Option_21               => 2#10100#,
      HRTimer_ADC_TRG1_Event  => 2#10101#,
      HRTimer_ADC_TRG3_Event  => 2#10110#,
      HRTimer_ADC_TRG5_Event  => 2#10111#,
      HRTimer_ADC_TRG6_Event  => 2#11000#,
      HRTimer_ADC_TRG7_Event  => 2#11001#,
      HRTimer_ADC_TRG8_Event  => 2#11010#,
      HRTimer_ADC_TRG9_Event  => 2#11011#,
      HRTimer_ADC_TRG10_Event => 2#11100#,
      LPTimer_OUT_Event       => 2#11101#,
      Timer7_TRGO_Event       => 2#11110#);

   type Regular_Channel_Conversion_Trigger (Enabler : External_Trigger) is
      record
         case Enabler is
            when Trigger_Disabled =>
               null;
            when others =>
               Event : External_Events_Regular_Group;
         end case;
      end record;

   Software_Triggered : constant Regular_Channel_Conversion_Trigger :=
     (Enabler => Trigger_Disabled);

   procedure Configure_Regular_Trigger
     (This       : in out Analog_To_Digital_Converter;
      Continuous : Boolean;
      Trigger    : Regular_Channel_Conversion_Trigger);

   type Regular_Channel_Conversion is record
      Channel     : Analog_Input_Channel;
      Sample_Time : Channel_Sampling_Times;
   end record;

   procedure Configure_Regular_Channel
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Rank        : Regular_Channel_Rank;
      Sample_Time : Channel_Sampling_Times);

   procedure Configure_Regular_Channel_Nbr
     (This   : in out Analog_To_Digital_Converter;
      Number : UInt4);

   type Regular_Channel_Conversions is
     array (Regular_Channel_Rank range <>) of Regular_Channel_Conversion;

   procedure Configure_Regular_Conversions
     (This        : in out Analog_To_Digital_Converter;
      Continuous  : Boolean;
      Trigger     : Regular_Channel_Conversion_Trigger;
      Conversions : Regular_Channel_Conversions)
     with
       Pre => Conversions'Length > 0,
       Post =>
         Length_Matches_Expected (This, Conversions) and
         --  if there are multiple channels to be converted, we must want to
         --  scan them so we set Scan_Mode accordingly
         (if Conversions'Length > 1 then Scan_Mode_Enabled (This)) and
         --  The VBat and VRef internal connections are enabled if This is
         --  ADC_1 and the corresponding channels are included in the lists.
         (VBat_May_Be_Enabled (This, Conversions) or else
          VRef_TemperatureSensor_May_Be_Enabled (This, Conversions));
   --  Configures all the regular channel conversions described in the array
   --  Conversions. Note that the order of conversions in the array is the
   --  order in which they are scanned, ie, their index is their "rank" in
   --  the data structure. Note that if the VBat and Temperature channels are
   --  the same channel, then only the VBat conversion takes place and only
   --  that one will be enabled, so we must check the two in that order.

   function Regular_Conversions_Expected (This : Analog_To_Digital_Converter)
     return Natural;
   --  Returns the total number of regular channel conversions specified in the
   --  hardware.

   function Scan_Mode_Enabled (This : Analog_To_Digital_Converter)
     return Boolean;
   --  Returns whether only one channel is converted, or if multiple channels
   --  are converted (i.e., scanned). Note that this is independent of whether
   --  the conversions are continuous.

   type External_Events_Injected_Group is
     (Timer1_TRGO_Event,
      Timer1_CC4_Event,
      Timer2_TRGO_Event,
      Option_4,
      Option_5,
      Timer4_TRGO_Event,
      Option_7,
      Timer8_CC4_Event,
      Timer1_TRGO2_Event,
      Timer8_TRGO_Event,
      Timer8_TRGO2_Event,
      Option_12,
      Timer3_TRGO_Event,
      Option_14,
      Timer6_TRGO_Event,
      Timer15_TRGO_Event,
      Timer20_TRGO_Event,
      Timer20_TRGO2_Event,
      Option_19,
      HRTimer_ADC_TRG2_Event,
      HRTimer_ADC_TRG4_Event,
      HRTimer_ADC_TRG5_Event,
      HRTimer_ADC_TRG6_Event,
      HRTimer_ADC_TRG7_Event,
      HRTimer_ADC_TRG8_Event,
      HRTimer_ADC_TRG9_Event,
      HRTimer_ADC_TRG10_Event,
      Option_28,
      Option_29,
      LPTimer_OUT_Event,
      Timer7_TRGO_Event)
     with Size => 5;
   --  External triggers for injected channels. RM0440 Table 164 pg. 630
   --  and Table 166 pg. 632.
   --  Differences between ADC12 and ADC345:
   --  Option    ADC12                       ADC345
   --  4         Timer2_CC1_Event            Timer8_CC2
   --  5         Timer3_CC4_Event            Timer4_CC3
   --  7         EXTI_Line15                 Timer4_CC4
   --  12        Timer3_CC3_Event            Timer1_CC3
   --  14        Timer3_CC1_Event            EXTI_Line3
   --  19        Timer20_CC4_Event           Timer20_CC2
   --  28        Timer16_CC1_Event           HRTimer_ADC_TRG1
   --  29                                    HRTimer_ADC_TRG3

   for External_Events_Injected_Group use
     (Timer1_TRGO_Event       => 2#00000#,
      Timer1_CC4_Event        => 2#00001#,
      Timer2_TRGO_Event       => 2#00010#,
      Option_4                => 2#00011#,
      Option_5                => 2#00100#,
      Timer4_TRGO_Event       => 2#00101#,
      Option_7                => 2#00110#,
      Timer8_CC4_Event        => 2#00111#,
      Timer1_TRGO2_Event      => 2#01000#,
      Timer8_TRGO_Event       => 2#01001#,
      Timer8_TRGO2_Event      => 2#01010#,
      Option_12               => 2#01011#,
      Timer3_TRGO_Event       => 2#01100#,
      Option_14               => 2#01101#,
      Timer6_TRGO_Event       => 2#01110#,
      Timer15_TRGO_Event      => 2#01111#,
      Timer20_TRGO_Event      => 2#10000#,
      Timer20_TRGO2_Event     => 2#10001#,
      Option_19               => 2#10010#,
      HRTimer_ADC_TRG2_Event  => 2#10011#,
      HRTimer_ADC_TRG4_Event  => 2#10100#,
      HRTimer_ADC_TRG5_Event  => 2#10101#,
      HRTimer_ADC_TRG6_Event  => 2#10110#,
      HRTimer_ADC_TRG7_Event  => 2#10111#,
      HRTimer_ADC_TRG8_Event  => 2#11000#,
      HRTimer_ADC_TRG9_Event  => 2#11001#,
      HRTimer_ADC_TRG10_Event => 2#11010#,
      Option_28               => 2#11011#,
      Option_29               => 2#11100#,
      LPTimer_OUT_Event       => 2#11101#,
      Timer7_TRGO_Event       => 2#11110#);

   type Injected_Channel_Conversion_Trigger (Enabler : External_Trigger) is
      record
         case Enabler is
            when Trigger_Disabled =>
               null;
            when others =>
               Event : External_Events_Injected_Group;
         end case;
      end record;

   Software_Triggered_Injected : constant Injected_Channel_Conversion_Trigger :=
     (Enabler => Trigger_Disabled);

   procedure Configure_Injected_Trigger
     (This          : in out Analog_To_Digital_Converter;
      AutoInjection : Boolean;
      Trigger       : Injected_Channel_Conversion_Trigger)
     with Pre => (if AutoInjection then Trigger = Software_Triggered_Injected) and
                 (if AutoInjection then
                   not Discontinuous_Mode_Injected_Enabled (This));

   type Injected_Channel_Conversion is record
      Channel     : Analog_Input_Channel;
      Sample_Time : Channel_Sampling_Times;
   end record;

   procedure Configure_Injected_Channel
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Rank        : Injected_Channel_Rank;
      Sample_Time : Channel_Sampling_Times);

   procedure Configure_Injected_Channel_Nbr
     (This   : in out Analog_To_Digital_Converter;
      Number : UInt2);

   type Injected_Channel_Conversions is
     array (Injected_Channel_Rank range <>) of Injected_Channel_Conversion;

   procedure Configure_Injected_Conversions
     (This          : in out Analog_To_Digital_Converter;
      AutoInjection : Boolean;
      Trigger       : Injected_Channel_Conversion_Trigger;
      Conversions   : Injected_Channel_Conversions)
     with
       Pre =>
         Conversions'Length > 0 and
         (if AutoInjection then Trigger = Software_Triggered_Injected) and
         (if AutoInjection then
           not Discontinuous_Mode_Injected_Enabled (This)),
       Post =>
         Length_Is_Expected (This, Conversions) and
         --  The VBat and VRef internal connections are enabled if This is
         --  ADC_1 and the corresponding channels are included in the lists.
         (VBat_May_Be_Enabled (This, Conversions)  or else
          VRef_TemperatureSensor_May_Be_Enabled (This, Conversions));
   --  Configures all the injected channel conversions described in the array
   --  Conversions. Note that the order of conversions in the array is the
   --  order in which they are scanned, ie, their index is their "rank" in
   --  the data structure. Note that if the VBat and Temperature channels are
   --  the same channel, then only the VBat conversion takes place and only
   --  that one will be enabled, so we must check the two in that order.

   function Injected_Conversions_Expected (This : Analog_To_Digital_Converter)
     return Natural;
   --  Returns the total number of injected channel conversions to be done.

   subtype Data_Offset is UInt12;
   type Offset_Channel_Rank is new Natural range 1 .. 4;
   type Offset_Signal is (Minus, Plus);

   procedure Configure_Channel_Offset
     (This       : in out Analog_To_Digital_Converter;
      Channel    : Analog_Input_Channel;
      Rank       : Offset_Channel_Rank;
      Offset     : Data_Offset;
      Signal     : Offset_Signal;
      Saturation : Boolean;
      Enabled    : Boolean)
     with Inline;
   --  A maximum number of 4 regular or injected channels may have an offset.
   --  The converted value is decreased by the user-defined offset written in
   --  the bits OFFSETy[11:0]. The result may be a negative value so the read
   --  data is signed and the SEXT bit represents the extended sign value.
   --  With saturation, the offset result can be signed, otherwise it occurs at
   --  0x000 and 0xFFF and the offset result is unsigned. See RM0440 rev 7
   --  chapter 21.4.26 Data management, Offset.

   procedure Set_Channel_Offset
     (This    : in out Analog_To_Digital_Converter;
      Rank    : Offset_Channel_Rank;
      Enabled : Boolean)
     with Inline;
   --  Enable/disable the channel offset.

   function VBat_Enabled (This : Analog_To_Digital_Converter) return Boolean;
   --  Returns whether the hardware has the VBat internal connection enabled.

   function VRef_TemperatureSensor_Enabled
     (This : Analog_To_Digital_Converter) return Boolean;
   --  Returns whether the hardware has the VRef or temperature sensor internal
   --  connection enabled.

   procedure Start_Conversion (This : in out Analog_To_Digital_Converter) with
     Pre => Enabled (This) and Regular_Conversions_Expected (This) > 0;
   --  Starts the conversion(s) for the regular channels.

   procedure Stop_Conversion (This : in out Analog_To_Digital_Converter) with
     Pre => Conversion_Started (This) and Enabled (This);
   --  Stops the conversion(s) for the regular channels.

   function Conversion_Started (This : Analog_To_Digital_Converter)
     return Boolean;
   --  Returns whether the regular channels' conversions have started. Note
   --  that the ADC hardware clears the corresponding bit immediately, as
   --  part of starting.

   function Conversion_Value (This : Analog_To_Digital_Converter)
      return UInt16 with Inline;
   --  Returns the latest regular conversion result for the specified ADC unit.

   function Data_Register_Address (This : Analog_To_Digital_Converter)
     return System.Address
     with Inline;
   --  Returns the address of the ADC Data Register. This is exported
   --  STRICTLY for the sake of clients using DMA. All other
   --  clients of this package should use the Conversion_Value functions!
   --  Seriously, don't use this function otherwise.

   procedure Start_Injected_Conversion
     (This : in out Analog_To_Digital_Converter)
     with Pre => Enabled (This) and Injected_Conversions_Expected (This) > 0;
   --  Note that the ADC hardware clears the corresponding bit immediately, as
   --  part of starting.

   function Injected_Conversion_Started (This : Analog_To_Digital_Converter)
      return Boolean;
   --  Returns whether the injected channels' conversions have started.

   function Injected_Conversion_Value
     (This : Analog_To_Digital_Converter;
      Rank : Injected_Channel_Rank)
      return UInt16
     with Inline;
   --  Returns the latest conversion result for the analog input channel at
   --  the injected sequence position given by Rank on the specified ADC unit.
   --
   --  Note that the offset corresponding to the specified Rank is subtracted
   --  automatically, so check the sign bit for a negative result.

   type CDR_Data is (Master, Slave);

   function Multimode_Conversion_Value
     (This  : Analog_To_Digital_Converter;
      Value : CDR_Data) return UInt16;

   function Multimode_Conversion_Value
     (This : Analog_To_Digital_Converter) return UInt32
     with inline;
   --  Returns the latest ADC_1, ADC_2 or ADC_3, ADC_4, ADC_5 regular channel
   --  conversions' results based on the selected multi ADC mode.

   --  Discontinuous Management  --------------------------------------------------------

   type Discontinuous_Mode_Channel_Count is range 1 .. 8;
   --  Note this uses a biased representation implicitly because the underlying
   --  representational bit values are 0 ... 7.

   procedure Enable_Discontinuous_Mode
     (This    : in out Analog_To_Digital_Converter;
      Regular : Boolean;  -- if False, applies to Injected channels
      Count   : Discontinuous_Mode_Channel_Count)
     with
       Pre => not AutoInjection_Enabled (This),
       Post =>
         (if Regular then
            (Discontinuous_Mode_Regular_Enabled (This)) and
            (not Discontinuous_Mode_Injected_Enabled (This))
          else
            (not Discontinuous_Mode_Regular_Enabled (This)) and
            (Discontinuous_Mode_Injected_Enabled (This)));
   --  Enables discontinuous mode and sets the count. If Regular is True,
   --  enables the mode only for regular channels. If Regular is False, enables
   --  the mode only for Injected channels. The note in RM 13.3.10, pg 393,
   --  says we cannot enable the mode for both regular and injected channels
   --  at the same time, so this flag ensures we follow that rule.

   procedure Disable_Discontinuous_Mode_Regular
     (This : in out Analog_To_Digital_Converter)
      with Post => not Discontinuous_Mode_Regular_Enabled (This);

   procedure Disable_Discontinuous_Mode_Injected
     (This : in out Analog_To_Digital_Converter)
      with Post => not Discontinuous_Mode_Injected_Enabled (This);

   function Discontinuous_Mode_Regular_Enabled
     (This : Analog_To_Digital_Converter)
     return Boolean;

   function Discontinuous_Mode_Injected_Enabled
     (This : Analog_To_Digital_Converter)
      return Boolean;

   function AutoInjection_Enabled
     (This : Analog_To_Digital_Converter)
      return Boolean;

   --  DMA Management  --------------------------------------------------------

   procedure Enable_DMA (This : in out Analog_To_Digital_Converter) with
     Pre => not Conversion_Started (This) and
            not Injected_Conversion_Started (This),
     Post => DMA_Enabled (This);

   procedure Disable_DMA (This : in out Analog_To_Digital_Converter) with
     Pre => not Conversion_Started (This) and
            not Injected_Conversion_Started (This),
     Post => not DMA_Enabled (This);

   function DMA_Enabled (This : Analog_To_Digital_Converter) return Boolean;

   procedure Enable_DMA_After_Last_Transfer
     (This : in out Analog_To_Digital_Converter) with
     Pre => not Conversion_Started (This) and
            not Injected_Conversion_Started (This),
     Post => DMA_Enabled_After_Last_Transfer (This);

   procedure Disable_DMA_After_Last_Transfer
     (This : in out Analog_To_Digital_Converter) with
     Pre => not Conversion_Started (This) and
            not Injected_Conversion_Started (This),
     Post => not DMA_Enabled_After_Last_Transfer (This);

   function DMA_Enabled_After_Last_Transfer
     (This : Analog_To_Digital_Converter)
      return Boolean;

   --  Analog Watchdog  -------------------------------------------------------

   subtype Watchdog_Threshold is UInt12;

   type Analog_Watchdog_Modes is
     (Watchdog_All_Regular_Channels,
      Watchdog_All_Injected_Channels,
      Watchdog_All_Both_Kinds,
      Watchdog_Single_Regular_Channel,
      Watchdog_Single_Injected_Channel,
      Watchdog_Single_Both_Kinds);

   subtype Multiple_Channels_Watchdog is Analog_Watchdog_Modes
     range Watchdog_All_Regular_Channels .. Watchdog_All_Both_Kinds;

   procedure Watchdog_Enable_Channels
     (This : in out Analog_To_Digital_Converter;
      Mode : Multiple_Channels_Watchdog;
      Low  : Watchdog_Threshold;
      High : Watchdog_Threshold)
     with
       Pre  => not Watchdog_Enabled (This),
       Post => Watchdog_Enabled (This);
   --  Enables the watchdog on all channels; channel kind depends on Mode.
   --  A call to this routine is considered a complete configuration of the
   --  watchdog so do not call the other enabler routine (for a single channel)
   --  while this configuration is active. You must first disable the watchdog
   --  if you want to enable the watchdog for a single channel.
   --  see RM0440 rev 6 Chapter 21.4.28, pg 659, Table 169.

   subtype Single_Channel_Watchdog is Analog_Watchdog_Modes
     range Watchdog_Single_Regular_Channel .. Watchdog_Single_Both_Kinds;

   procedure Watchdog_Enable_Channel
     (This    : in out Analog_To_Digital_Converter;
      Mode    : Single_Channel_Watchdog;
      Channel : Analog_Input_Channel;
      Low     : Watchdog_Threshold;
      High    : Watchdog_Threshold)
     with
       Pre  => not Watchdog_Enabled (This),
       Post => Watchdog_Enabled (This);
   --  Enables the watchdog on this single channel, and no others. The kind of
   --  channel depends on Mode. A call to this routine is considered a complete
   --  configuration of the watchdog so do not call the other enabler routine
   --  (for all channels) while this configuration is active. You must
   --  first disable the watchdog if you want to enable the watchdog for
   --  all channels.
   --  see RM0440 rev 6 Chapter 21.4.28, pg 659, Table 169.

   procedure Watchdog_Disable (This : in out Analog_To_Digital_Converter)
     with Post => not Watchdog_Enabled (This);
   --  Whether watching a single channel or all of them, the watchdog is now
   --  disabled.

   function Watchdog_Enabled (This : Analog_To_Digital_Converter)
      return Boolean;

   type Analog_Watchdog_Filtering is
     (No_Filtering,
      Two_Detections,
      Three_Detections,
      Four_Detections,
      Five_Detections,
      Six_Detections,
      Seven_Detections,
      Eight_Detections);
   --   Consecutive detection generates an AWDx flag or an interrupt.

   procedure Watchdog_Enable_Filtering
     (This   : in out Analog_To_Digital_Converter;
      Filter : Analog_Watchdog_Filtering);

   type Analog_Window_Watchdog is (Watchdog_2, Watchdog_3);

   procedure Watchdog_Enable_Channel
     (This     : in out Analog_To_Digital_Converter;
      Watchdog : Analog_Window_Watchdog;
      Channel  : Analog_Input_Channel;
      Low      : Watchdog_Threshold;
      High     : Watchdog_Threshold)
     with
       Pre  => not Conversion_Started (This),
       Post => Watchdog_Enabled (This, Watchdog);
   --  Enable the watchdog 2 or 3 for any selected channel. The channels
   --  selected by AWDxCH must be also selected into the ADC regular or injected
   --  sequence registers SQRi or JSQRi registers. The watchdog is disabled when
   --  none channel is selected.
   --  The watchdog threshold is limited to a resolution of 8 bits, so only the
   --  8 MSBs of the thresholds can be programmed into HTx[7:0] and LTx[7:0].
   --  See RM0440 rev 7, chapter 21.4.28, table 171 for 12- to 6-bit resolutions.

   procedure Watchdog_Disable_Channel
     (This     : in out Analog_To_Digital_Converter;
      Watchdog : Analog_Window_Watchdog;
      Channel  : Analog_Input_Channel)
     with
       Pre  => not Conversion_Started (This);

   procedure Watchdog_Disable
     (This     : in out Analog_To_Digital_Converter;
      Watchdog : Analog_Window_Watchdog)
     with Post => not Watchdog_Enabled (This, Watchdog);
   --  The watchdog is disabled when none channel is selected.

   function Watchdog_Enabled
     (This     : Analog_To_Digital_Converter;
      Watchdog : Analog_Window_Watchdog) return Boolean;
   --  The watchdog is enabled when any channel is selected.

   --  Status Management  -----------------------------------------------------

   type ADC_Status_Flag is
     (ADC_Ready,
      Regular_Channel_Conversion_Completed,
      Regular_Sequence_Conversion_Completed,
      Injected_Channel_Conversion_Completed,
      Injected_Sequence_Conversion_Completed,
      Analog_Watchdog_1_Event_Occurred,
      Analog_Watchdog_2_Event_Occurred,
      Analog_Watchdog_3_Event_Occurred,
      Sampling_Completed,
      Overrun,
      Injected_Context_Queue_Overflow);

   function Status
     (This : Analog_To_Digital_Converter;
      Flag : ADC_Status_Flag)
      return Boolean
     with Inline;
   --  Returns whether Flag is indicated, ie set in the Status Register.

   procedure Clear_Status
     (This : in out Analog_To_Digital_Converter;
      Flag : ADC_Status_Flag)
     with
       Inline,
       Post => not Status (This, Flag);

   procedure Poll_For_Status
     (This    : in out Analog_To_Digital_Converter;
      Flag    : ADC_Status_Flag;
      Success : out Boolean;
      Timeout : Time_Span := Time_Span_Last);
   --  Continuously polls for the specified status flag to be set, up to the
   --  deadline computed by the value of Clock + Timeout. Sets the Success
   --  argument accordingly. The default Time_Span_Last value is the largest
   --  possible value, thereby setting a very long, but not infinite, timeout.

   --  Interrupt Management  --------------------------------------------------

   type ADC_Interrupts is
     (ADC_Ready,
      Regular_Channel_Conversion_Complete,
      Regular_Sequence_Conversion_Complete,
      Injected_Channel_Conversion_Complete,
      Injected_Sequence_Conversion_Complete,
      Analog_Watchdog_1_Event_Occurr,
      Analog_Watchdog_2_Event_Occurr,
      Analog_Watchdog_3_Event_Occurr,
      Sampling_Complete,
      Overrun,
      Injected_Context_Queue_Overflow);

   procedure Enable_Interrupts
     (This   : in out Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
     with
       Inline,
       Post => Interrupt_Enabled (This, Source);

   procedure Disable_Interrupts
     (This   : in out Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
     with
       Inline,
       Post => not Interrupt_Enabled (This, Source);

   function Interrupt_Enabled
     (This   : Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
      return Boolean
     with Inline;

   procedure Clear_Interrupt_Pending
     (This   : in out Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
     with Inline;

   --  Common Properties ------------------------------------------------------

   type ADC_Prescaler is
     (Div_1,
      Div_2,
      Div_4,
      Div_6,
      Div_8,
      Div_10,
      Div_12,
      Div_16,
      Div_32,
      Div_64,
      Div_128,
      Div_256)
     with Size => 4;

   type ADC_Clock_Mode is
     (CLK_ADC,
      PCLK2_Div_1,
      PCLK2_Div_2,
      PCLK2_Div_4);

   type Dual_ADC_DMA_Modes is
     (Disabled,
      DMA_Mode_1,
      DMA_Mode_2);

   for Dual_ADC_DMA_Modes use
     (Disabled   => 2#00#,
      DMA_Mode_1 => 2#10#,
      DMA_Mode_2 => 2#11#);

   type Sampling_Delay_Selections is
     (Sampling_Delay_5_Cycles,
      Sampling_Delay_6_Cycles,
      Sampling_Delay_7_Cycles,
      Sampling_Delay_8_Cycles,
      Sampling_Delay_9_Cycles,
      Sampling_Delay_10_Cycles,
      Sampling_Delay_11_Cycles,
      Sampling_Delay_12_Cycles,
      Sampling_Delay_13_Cycles,
      Sampling_Delay_14_Cycles,
      Sampling_Delay_15_Cycles,
      Sampling_Delay_16_Cycles,
      Sampling_Delay_17_Cycles,
      Sampling_Delay_18_Cycles,
      Sampling_Delay_19_Cycles,
      Sampling_Delay_20_Cycles);

   type Multi_ADC_Mode_Selections is
     (Independent,
      Dual_Combined_Regular_Injected_Simultaneous,
      Dual_Combined_Regular_Simultaneous_Alternate_Trigger,
      Dual_Combined_Interleaved_Injected_Simultaneous,
      Dual_Injected_Simultaneous,
      Dual_Regular_Simultaneous,
      Dual_Interleaved,
      Dual_Alternate_Trigger);
   --  In dual mode, master (ADC1/ADC3) and slave (ADC2/ADC4) ADCs work
   --  together and need only a start conversion on the master channel.
   --  ADC5 is single converter.

   for Multi_ADC_Mode_Selections use
     (Independent                                            => 2#00000#,
      Dual_Combined_Regular_Injected_Simultaneous            => 2#00001#,
      Dual_Combined_Regular_Simultaneous_Alternate_Trigger   => 2#00010#,
      Dual_Combined_Interleaved_Injected_Simultaneous        => 2#00011#,
      Dual_Injected_Simultaneous                             => 2#00101#,
      Dual_Regular_Simultaneous                              => 2#00110#,
      Dual_Interleaved                                       => 2#00111#,
      Dual_Alternate_Trigger                                 => 2#01001#);

   procedure Configure_Common_Properties
     (This           : Analog_To_Digital_Converter;
      Mode           : Multi_ADC_Mode_Selections;
      Prescaler      : ADC_Prescaler;
      Clock_Mode     : ADC_Clock_Mode;
      DMA_Mode       : Dual_ADC_DMA_Modes;
      Sampling_Delay : Sampling_Delay_Selections);
   --  These properties are common to all the ADC units on the board.

   --  These Multi_DMA_Mode commands needs to be separate from the
   --  Configure_Common_Properties procedure for the sake of dealing
   --  with overruns etc.

   procedure Multi_Enable_DMA_After_Last_Transfer
     (This : Analog_To_Digital_Converter)
     with Pre => not Conversion_Started (This),
          Post => Multi_DMA_Enabled_After_Last_Transfer (This);
   --  Make shure to execute this procedure only when conversion is
   --  not started.

   procedure Multi_Disable_DMA_After_Last_Transfer
     (This : Analog_To_Digital_Converter)
     with Pre => not Conversion_Started (This),
          Post => not Multi_DMA_Enabled_After_Last_Transfer (This);
   --  Make shure to execute this procedure only when conversion is
   --  not started.

   function Multi_DMA_Enabled_After_Last_Transfer
     (This : Analog_To_Digital_Converter) return Boolean;

   --  Queries ----------------------------------------------------------------

   function VBat_Conversion
     (This    : Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel)
      return Boolean with Inline;

   function VRef_TemperatureSensor_Conversion
     (This    : Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel)
      return Boolean with Inline;
   --  Returns whether the ADC unit and channel specified are that of a VRef
   --  OR a temperature sensor conversion. Note that one control bit is used
   --  to enable either one, ie it is shared.

   function VBat_May_Be_Enabled
     (This  : Analog_To_Digital_Converter;
      These : Regular_Channel_Conversions)
      return Boolean
   is
     ((for all Conversion of These =>
           (if VBat_Conversion (This, Conversion.Channel)
            then VBat_Enabled (This))));

   function VBat_May_Be_Enabled
     (This  : Analog_To_Digital_Converter;
      These : Injected_Channel_Conversions)
      return Boolean
   is
     ((for all Conversion of These =>
        (if VBat_Conversion (This, Conversion.Channel)
         then VBat_Enabled (This))));

   function VRef_TemperatureSensor_May_Be_Enabled
     (This  : Analog_To_Digital_Converter;
      These : Regular_Channel_Conversions)
      return Boolean
   is
     (for all Conversion of These =>
        (if VRef_TemperatureSensor_Conversion (This, Conversion.Channel)
         then VRef_TemperatureSensor_Enabled (This)));

   function VRef_TemperatureSensor_May_Be_Enabled
     (This  : Analog_To_Digital_Converter;
      These : Injected_Channel_Conversions)
      return Boolean
   is
     (for all Conversion of These =>
        (if VRef_TemperatureSensor_Conversion (This, Conversion.Channel)
         then VRef_TemperatureSensor_Enabled (This)));

   --  The *_Conversions_Expected functions will always return at least the
   --  value 1 because the hardware uses a biased representation (in which
   --  zero indicates the value one, one indicates the value two, and so on).
   --  Therefore, we don't invoke the functions unless we know they will be
   --  greater than zero.

   function Length_Matches_Expected
     (This  : Analog_To_Digital_Converter;
      These : Regular_Channel_Conversions)
      return Boolean
   is
     (if These'Length > 0 then
         Regular_Conversions_Expected (This) = These'Length);

   function Length_Is_Expected
     (This  : Analog_To_Digital_Converter;
      These : Injected_Channel_Conversions)
      return Boolean
   is
     (if These'Length > 0 then
         Injected_Conversions_Expected (This) = These'Length);

private

   ADC_Stabilization                : constant Time_Span := Microseconds (3);
   Temperature_Sensor_Stabilization : constant Time_Span := Microseconds (10);
   --  The RM, section 13.3.6, says stabilization times are required. These
   --  values are specified in the datasheets, eg section 5.3.20, pg 129,
   --  and section 5.3.21, pg 134, of the STM32F405/7xx, DocID022152 Rev 4.

   procedure Enable_VBat_Connection (This : Analog_To_Digital_Converter)
     with Post => VBat_Enabled (This);

   procedure Enable_VRef_TemperatureSensor_Connection
     (This : Analog_To_Digital_Converter)
     with Post => VRef_TemperatureSensor_Enabled (This);
   --  One bit controls both the VRef and the temperature internal connections.

   type Analog_To_Digital_Converter is new STM32_SVD.ADC.ADC1_Peripheral;

   function VBat_Conversion
     (This    : Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel)
      return Boolean
   is (This'Address = STM32_SVD.ADC.ADC1_Periph'Address and
         Channel = VBat_Channel);

   function VRef_TemperatureSensor_Conversion
     (This    : Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel)
      return Boolean
   is (This'Address = STM32_SVD.ADC.ADC1_Periph'Address and
         (Channel in VRef_Channel | TemperatureSensor_Channel));

end STM32.ADC;
