------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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
--   @file    stm32f4xx_hal_dac.h and stm32f4xx_hal_dac_ex.h                --
--   @author  MCD Application Team                                          --
--   @version V1.3.1                                                        --
--   @date    25-March-2015                                                 --
--   @brief   Header file of DAC HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides interfaces for the digital-to-analog converters on the
--  STM32G4 (ARM Cortex M4F) microcontrollers from ST Microelectronics.

with System;                use System;
private with STM32_SVD.DAC;

package STM32.DAC is

   type Digital_To_Analog_Converter is limited private;

   type DAC_Channel is (Channel_1, Channel_2);

   --  Note that DAC_1 Channel 1 is tied to GPIO pin PA4, DAC_1 Channel 2 to PA5
   --  and DAC_2 channel 1 to GPIO pin PA6.

   procedure Enable
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with
       Inline,
       Post => Enabled (This, Channel);
   --  Powers up the channel. The channel is then enabled after a startup
   --  time "Twakeup" specified in the datasheet.
   --
   --  NB: When no hardware trigger has been selected, the value in the
   --  DAC_DHRx register is transfered automatically to the DAC_DORx register.
   --  Therefore, in that case enabling the channel starts the output
   --  conversion on that channel. See the RM0440, section 22.4.5 "DAC
   --  conversion" second and third paragraphs.

   procedure Disable
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with
       Inline,
       Post => not Enabled (This, Channel);
   --  When the software trigger has been selected, disabling the channel stops
   --  the output conversion on that channel.

   function Enabled
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean;

   type HF_Interface_Mode is
     (HF_Disabled,
      HF_AHB_GT_80MHz,
      HF_AHB_GT_160MHz);

   procedure Configure_Interface_Mode
     (This : in out Digital_To_Analog_Converter;
      Mode : HF_Interface_Mode)
     with Post => Read_Interface_Mode (This) = Mode;

   function Read_Interface_Mode
     (This : Digital_To_Analog_Converter)
     return HF_Interface_Mode;

   type DAC_Resolution is (DAC_Resolution_12_Bits, DAC_Resolution_8_Bits);

   Max_12bit_Resolution : constant := 16#0FFF#;
   Max_8bit_Resolution  : constant := 16#00FF#;

   type Data_Alignment is (Left_Aligned, Right_Aligned);

   procedure Set_Output
     (This       : in out Digital_To_Analog_Converter;
      Channel    : DAC_Channel;
      Value      : UInt16;
      Resolution : DAC_Resolution;
      Alignment  : Data_Alignment);
   --  For the specified channel, writes the output Value to the data holding
   --  register within This corresponding to the Resolution and Alignment.
   --
   --  The output voltage = ((Value / Max_nbit_Counts) * VRef+), where VRef+ is
   --  the reference input voltage and the 'n' of Max_nbit_Counts is either 12
   --  or 8.

   type Input_Format is (Unsigned, Signed);
   --  Select the type of input. When signed (2'd complement) the MSB bit
   --  represents the sign.

   procedure Set_Input_Format
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel;
      Format  : Input_Format)
     with Post => Read_Input_Format (This, Channel) = Format;

   function Read_Input_Format
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     return Input_Format;

   procedure Trigger_Conversion_By_Software
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with
       Pre => Trigger_Selection (This, Channel) = Software_Trigger and
              Trigger_Enabled (This, Channel);
   --  Cause the conversion to occur and the output to appear, per 22.4.7 "DAC
   --  trigger selection" in the RM0440. This routine is needed when the
   --  Software Trigger has been selected and the trigger has been enabled,
   --  otherwise no conversion occurs. If you don't enable the trigger any prior
   --  selection has no effect, but note that when no *hardware* trigger is
   --  selected the output happens automatically when the channel is enabled.
   --  See the RM0440 section 22.4.5 "DAC conversion" second and third
   --  paragraphs.

   function Converted_Output_Value
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return UInt16;
   --  Returns the latest output value for the specified channel.

   procedure Set_Dual_Output_Voltages
     (This            : in out Digital_To_Analog_Converter;
      Channel_1_Value : UInt16;
      Channel_2_Value : UInt16;
      Resolution      : DAC_Resolution;
      Alignment       : Data_Alignment);

   type Dual_Channel_Output is record
      Channel_1_Data : UInt16;
      Channel_2_Data : UInt16;
   end record;

   function Converted_Dual_Output_Value (This : Digital_To_Analog_Converter)
     return Dual_Channel_Output;
   --  Returns the combination of the latest output values for both channels.

   type Channel_Mode is
     (Normal_External_Pin_Buffer,
      Normal_External_Pin_OnChip_Peripheral_Buffer,
      Normal_External_Pin,
      Normal_OnChip_Peripheral,
      SampleHold_External_Pin_Buffer,
      SampleHold_External_Pin_OnChip_Peripheral_Buffer,
      SampleHold_External_Pin_OnChip_Peripheral,
      SampleHold_OnChip_Peripheral);

   procedure Configure_Channel_Mode
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel;
      Mode    : Channel_Mode)
     with
       Pre => not Enabled (This, Channel) and
              not Read_Calibration_Mode (This, Channel),
       Post => Read_Channel_Mode (This, Channel) = Mode;

   function Read_Channel_Mode
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     return Channel_Mode;

   procedure Configure_Sample_Hold_Time
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel;
      Sample  : UInt10;
      Hold    : UInt10;
      Refresh : UInt8);
   --  Configure the sample, hold and refresh timing when in Sample & Hold mode
   --  In this mode, the DAC core and all corresponding logic and registers are
   --  driven by the LSI or LSE low-speed clock (dac_hold_ck) in addition to
   --  the dac_hclk clock, so these timings are based on these clock sources.
   --  See UM0440 rev 6 pg. 745 chapter 22.4.12 for timing examples.

   type External_Event_Trigger_Selection is
     (Software_Trigger,
      Option_2,
      Timer_7_Output_Trigger,
      Timer_15_Output_Trigger,
      Timer_2_Output_Trigger,
      Timer_4_Output_Trigger,
      EXTI_Line_9_Trigger,  -- any GPIO_x Pin_9
      Timer_6_Output_Trigger,
      Timer_3_Output_Trigger,
      HRTimer_DAC_Reset_Trigger_1,
      HRTimer_DAC_Reset_Trigger_2,
      HRTimer_DAC_Reset_Trigger_3,
      HRTimer_DAC_Reset_Trigger_4,
      HRTimer_DAC_Reset_Trigger_5,
      HRTimer_DAC_Reset_Trigger_6,
      Option_16);
   --  Option DAC1           DAC2           DAC3           DAC4
   --  2      TIM8_TRG0      TIM8_TRG0      TIM1_TRG0      TIM8_TRG0
   --  16     HRTIM_DAC_TRG1 HRTIM_DAC_TRG2 HRTIM_DAC_TRG3 HRTIM_DAC_TRG1
   --
   --  Refer to UM0440 rev 6 chapter 22.4.2 for DAC pins and internal signals.

   procedure Select_Trigger
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel;
      Trigger : External_Event_Trigger_Selection)
     with
       Pre  => not Trigger_Enabled (This, Channel),  -- per note in RM, pg 435
       Post => Trigger_Selection (This, Channel) = Trigger and
               not Trigger_Enabled (This, Channel);
   --  If the software trigger is selected, output conversion starts once the
   --  channel is enabled.

   function Trigger_Selection
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return External_Event_Trigger_Selection;

   procedure Enable_Trigger
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with Post => Trigger_Enabled (This, Channel);

   procedure Disable_Trigger
     (This : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with Post => not Trigger_Enabled (This, Channel);

   function Trigger_Enabled
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean;

   procedure Enable_DMA
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with Post => DMA_Enabled (This, Channel);

   procedure Disable_DMA
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with Post => not DMA_Enabled (This, Channel);

   function DMA_Enabled
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean;

   procedure Set_DMA_Double_Mode
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel;
      Mode    : Boolean);
   --  Select DMA double data mode (only for dual-channel DACs: DAC1, DAC3 and
   --  DAC4).

   type DAC_Status_Flag is
     (DORB_Output_Channel_1,
      DORB_Output_Channel_2,
      DMA_Underrun_Channel_1,
      DMA_Underrun_Channel_2,
      Calibration_EQGT_Offset_Channel_1,
      Calibration_EQGT_Offset_Channel_2,
      Write_Operation_Busy_Channel_1,
      Write_Operation_Busy_Channel_2);

   function Status
     (This : Digital_To_Analog_Converter;
      Flag : DAC_Status_Flag)
      return Boolean;

   procedure Clear_Status
     (This : in out Digital_To_Analog_Converter;
      Flag : DAC_Status_Flag)
     with
       Inline,
       Post => not Status (This, Flag);

   type DAC_Interrupts is
     (DMA_Underrun_Channel_1,
      DMA_Underrun_Channel_2);

   procedure Enable_Interrupts
     (This : in out Digital_To_Analog_Converter;
      Source : DAC_Interrupts)
     with
       Inline,
       Post => Interrupt_Enabled (This, Source);

   procedure Disable_Interrupts
     (This   : in out Digital_To_Analog_Converter;
      Source : DAC_Interrupts)
     with
       Inline,
       Post => not Interrupt_Enabled (This, Source);

   function Interrupt_Enabled
     (This   : Digital_To_Analog_Converter;
      Source : DAC_Interrupts)
      return Boolean
     with Inline;

   procedure Clear_Interrupt_Pending
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with Inline;

   type Wave_Generation_Selection is
     (No_Wave_Generation,
      Noise_Wave,
      Triangle_Wave,
      Sawtooth_Wave);

   type Noise_Wave_Mask_Selection is
     (LFSR_Unmask_Bit0,
      LFSR_Unmask_Bits1_0,
      LFSR_Unmask_Bits2_0,
      LFSR_Unmask_Bits3_0,
      LFSR_Unmask_Bits4_0,
      LFSR_Unmask_Bits5_0,
      LFSR_Unmask_Bits6_0,
      LFSR_Unmask_Bits7_0,
      LFSR_Unmask_Bits8_0,
      LFSR_Unmask_Bits9_0,
      LFSR_Unmask_Bits10_0,
      LFSR_Unmask_Bits11_0);
   --  Unmask LFSR bits for noise wave generation

   type Triangle_Wave_Amplitude_Selection is
     (Triangle_Amplitude_1,     --  Select max triangle amplitude of 1
      Triangle_Amplitude_3,     --  Select max triangle amplitude of 3
      Triangle_Amplitude_7,     --  Select max triangle amplitude of 7
      Triangle_Amplitude_15,    --  Select max triangle amplitude of 15
      Triangle_Amplitude_31,    --  Select max triangle amplitude of 31
      Triangle_Amplitude_63,    --  Select max triangle amplitude of 63
      Triangle_Amplitude_127,   --  Select max triangle amplitude of 127
      Triangle_Amplitude_255,   --  Select max triangle amplitude of 255
      Triangle_Amplitude_511,   --  Select max triangle amplitude of 511
      Triangle_Amplitude_1023,  --  Select max triangle amplitude of 1023
      Triangle_Amplitude_2047,  --  Select max triangle amplitude of 2047
      Triangle_Amplitude_4095); --  Select max triangle amplitude of 4095

   type Wave_Generation (Kind : Wave_Generation_Selection) is record
      case Kind is
         when No_Wave_Generation =>
            null;
         when Noise_Wave =>
            Mask : Noise_Wave_Mask_Selection;
         when Triangle_Wave .. Sawtooth_Wave =>
            Amplitude : Triangle_Wave_Amplitude_Selection;
      end case;
   end record;

   Wave_Generation_Disabled : constant Wave_Generation :=
     (Kind => No_Wave_Generation);

   procedure Select_Wave_Generation
     (This      : in out Digital_To_Analog_Converter;
      Channel   : DAC_Channel;
      Selection : Wave_Generation)
     with Post => Selected_Wave_Generation (This, Channel) = Selection;

   function Selected_Wave_Generation
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Wave_Generation;

   type Sawtooth_Direction is (Down, Up);

   type Sawtooth_Trigger_Selection is
     (Software_Trigger,
      Option_2,
      Timer_7_Output_Trigger,
      Timer_15_Output_Trigger,
      Timer_2_Output_Trigger,
      Timer_4_Output_Trigger,
      EXTI_Line_10_Trigger,  -- any GPIO_x Pin_10
      Timer_6_Output_Trigger,
      Timer_3_Output_Trigger,
      HRTimer_DAC_Step_Trigger_1,
      HRTimer_DAC_Step_Trigger_2,
      HRTimer_DAC_Step_Trigger_3,
      HRTimer_DAC_Step_Trigger_4,
      HRTimer_DAC_Step_Trigger_5,
      HRTimer_DAC_Step_Trigger_6);
   --  Option DAC1           DAC2           DAC3           DAC4
   --  2      TIM8_TRG0      TIM8_TRG0      TIM1_TRG0      TIM8_TRG0
   --
   --  Refer to UM0440 rev 6 chapter 22.4.2 for DAC pins and internal signals.

   procedure Configure_Sawtooth_Wave_Generation
     (This      : in out Digital_To_Analog_Converter;
      Channel   : DAC_Channel;
      Start_Val : UInt12;
      Start_Trg : Sawtooth_Trigger_Selection;
      Incr_Val  : UInt16;
      Incr_Trg  : Sawtooth_Trigger_Selection;
      Direction : Sawtooth_Direction);

   function Data_Address
     (This       : Digital_To_Analog_Converter;
      Channel    : DAC_Channel;
      Resolution : DAC_Resolution;
      Alignment  : Data_Alignment)
     return Address;
   --  Returns the address of the Data Holding register within This, for the
   --  specified Channel, at the specified Resolution and Alignment.
   --
   --  This function is stricly for use with DMA, all others use the API above.

   procedure Set_Calibration_Mode
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel;
      Mode    : Boolean)
     with Post => Read_Calibration_Mode (This, Channel) = Mode;

   function Read_Calibration_Mode
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     return Boolean;

   function Read_Calibration_Flag
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     return Boolean;

   procedure Set_Offset_Trimming
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel;
      Input   : UInt5);

   procedure Calibrate
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel);
   --  Calibrate the DAC channel. This routine is described in RM0440 pg. 747
   --  chapter 22.4.13 - DAC channel buffer calibration.

private

   type Digital_To_Analog_Converter is new STM32_SVD.DAC.DAC_Peripheral;

end STM32.DAC;
