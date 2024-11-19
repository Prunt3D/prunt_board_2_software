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
--   @file    stm32f4xx_hal_adc.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.3.1                                                        --
--   @date    25-March-2015                                                 --
--   @brief   Header file of ADC HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with STM32_SVD.ADC; use STM32_SVD.ADC, STM32_SVD;

package body STM32.ADC is

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out Analog_To_Digital_Converter) is
   begin
      --  After reset, the ADCs are in deep power-down where its supply is
      --  internally switched off. For normal operation the following bits
      --  should be set. See RM0440 rev 8 sections 21.4.6 and 21.7.3.
      This.CR.DEEPPWD := False; --  deep-sleep mode off
      This.CR.ADVREGEN := True; --  turn on internal voltage regulator

      --  Wait the ADC voltage regulator startup time of 30 us. See data sheet
      --  DS12288 rev. 5 chapter 5.3.19 Analog-to-digital converter characteristics
      --  table 66 ADC Characteristics at symbol tADCVREG_STUP.
      declare
         Start_Time : constant Time := Clock;
      begin
         loop
            exit when Clock > Start_Time + Microseconds (30);
         end loop;
      end;
      --  Avoid using delay statements so this can be used in protected operations.

      --  The software procedure to enable the ADC is described in RM0440 rev 8
      --  Chapter 21.4.9.
      if not This.CR.ADEN then
         --  Clear the ADRDY bit
         This.ISR.ADRDY := True;
         This.CR.ADEN := True;

         loop
            exit when This.ISR.ADRDY = True;
         end loop;
      end if;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out Analog_To_Digital_Converter) is
   begin
      if This.CR.ADEN then
         This.CR.ADDIS := True;
      end if;

      loop
         exit when not This.CR.ADDIS;
      end loop;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : Analog_To_Digital_Converter) return Boolean is
     (This.CR.ADEN);

   ---------------
   -- Calibrate --
   ---------------

   procedure Calibrate
     (This       : in out Analog_To_Digital_Converter;
      Convertion : Input_Convertion_Mode)
   is
   begin
      --  After reset, the ADCs are in deep power-down where its supply is
      --  internally switched off. For normal operation the following bits
      --  should be set. See RM0440 rev 8 sections 21.4.6 and 21.7.3.
      This.CR.DEEPPWD := False; --  deep-sleep mode off
      This.CR.ADVREGEN := True; --  turn on internal voltage regulator

      --  Wait the ADC voltage regulator startup time of 30 us. See data sheet
      --  DS12288 rev. 5 chapter 5.3.19 Analog-to-digital converter characteristics
      --  table 66 ADC Characteristics at symbol tADCVREG_STUP.
      declare
         Start_Time : constant Time := Clock;
      begin
         loop
            exit when Clock > Start_Time + Microseconds (30);
         end loop;
      end;
      --  Avoid using delay statements so this can be used in protected operations.

      This.CR.ADCALDIF := Convertion = Differential;

      --  Start calibration
      This.CR.ADCAL := True;

      loop
         exit when not This.CR.ADCAL;
      end loop;
   end Calibrate;

   ----------------------------
   -- Get_Calibration_Factor --
   ----------------------------

   function Get_Calibration_Factor
     (This       : in out Analog_To_Digital_Converter;
      Convertion : Input_Convertion_Mode) return UInt7
   is
   begin
      if Convertion = Single_Ended then
         return This.CALFACT.CALFACT_S;
      else
         return This.CALFACT.CALFACT_D;
      end if;
   end Get_Calibration_Factor;

   ----------------------------
   -- Set_Calibration_Factor --
   ----------------------------

   procedure Set_Calibration_Factor
     (This       : in out Analog_To_Digital_Converter;
      Convertion : Input_Convertion_Mode;
      Value      : UInt7)
   is
   begin
      if Convertion = Single_Ended then
         This.CALFACT.CALFACT_S := Value;
      else
         This.CALFACT.CALFACT_D := Value;
      end if;
   end Set_Calibration_Factor;

   -------------------------
   -- Set_Convertion_Mode --
   -------------------------

   procedure Set_Convertion_Mode
     (This       : in out Analog_To_Digital_Converter;
      Channel    : Analog_Input_Channel;
      Convertion : Input_Convertion_Mode)
   is
   begin
      case Channel is
         when 0 =>
            null; --  This channel is read-only
         when 1 =>
            This.DIFSEL.DIFSEL_1 := Convertion = Differential;
         when 2 =>
            This.DIFSEL.DIFSEL_2 := Convertion = Differential;
         when 3 =>
            This.DIFSEL.DIFSEL_3 := Convertion = Differential;
         when 4 =>
            This.DIFSEL.DIFSEL_4 := Convertion = Differential;
         when 5 =>
            This.DIFSEL.DIFSEL_5 := Convertion = Differential;
         when 6 =>
            This.DIFSEL.DIFSEL_6 := Convertion = Differential;
         when 7 =>
            This.DIFSEL.DIFSEL_7 := Convertion = Differential;
         when 8 =>
            This.DIFSEL.DIFSEL_8 := Convertion = Differential;
         when 9 =>
            This.DIFSEL.DIFSEL_9 := Convertion = Differential;
         when 10 =>
            This.DIFSEL.DIFSEL_10 := Convertion = Differential;
         when 11 =>
            This.DIFSEL.DIFSEL_11 := Convertion = Differential;
         when 12 =>
            This.DIFSEL.DIFSEL_12 := Convertion = Differential;
         when 13 =>
            This.DIFSEL.DIFSEL_13 := Convertion = Differential;
         when 14 =>
            This.DIFSEL.DIFSEL_14 := Convertion = Differential;
         when 15 =>
            This.DIFSEL.DIFSEL_15 := Convertion = Differential;
         when 16 =>
            This.DIFSEL.DIFSEL_16 := Convertion = Differential;
         when 17 =>
            This.DIFSEL.DIFSEL_17 := Convertion = Differential;
         when 18 =>
            This.DIFSEL.DIFSEL_18 := Convertion = Differential;
      end case;
   end Set_Convertion_Mode;

   --------------------
   -- Configure_Unit --
   --------------------

   procedure Configure_Unit
     (This       : in out Analog_To_Digital_Converter;
      Resolution : ADC_Resolution;
      Alignment  : Data_Alignment)
   is
   begin
      This.CFGR.RES := ADC_Resolution'Enum_Rep (Resolution);
      This.CFGR.ALIGN := Alignment = Left_Aligned;
   end Configure_Unit;

   ------------------------
   -- Current_Resolution --
   ------------------------

   function Current_Resolution
     (This : Analog_To_Digital_Converter)
      return ADC_Resolution
   is (ADC_Resolution'Enum_Val (This.CFGR.RES));

   -----------------------
   -- Current_Alignment --
   -----------------------

   function Current_Alignment
     (This : Analog_To_Digital_Converter)
      return Data_Alignment
   is ((if This.CFGR.ALIGN then Left_Aligned else Right_Aligned));

   -------------------------------
   -- Configure_Regular_Trigger --
   -------------------------------

   procedure Configure_Regular_Trigger
     (This       : in out Analog_To_Digital_Converter;
      Continuous : Boolean;
      Trigger    : Regular_Channel_Conversion_Trigger)
   is
   begin
      This.CFGR.CONT := Continuous;

      if Trigger.Enabler = Trigger_Disabled then
         This.CFGR.EXTSEL := 0;
         This.CFGR.EXTEN := 0;
      else
         This.CFGR.EXTSEL := External_Events_Regular_Group'Enum_Rep (Trigger.Event);
         This.CFGR.EXTEN := External_Trigger'Enum_Rep (Trigger.Enabler);
      end if;
   end Configure_Regular_Trigger;

   -------------------------------
   -- Configure_Regular_Channel --
   -------------------------------

   procedure Configure_Regular_Channel
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Rank        : Regular_Channel_Rank;
      Sample_Time : Channel_Sampling_Times)
   is
   begin
      Set_Sampling_Time (This, Channel, Sample_Time);
      Set_Sequence_Position (This, Channel, Rank);
   end Configure_Regular_Channel;

   -----------------------------------
   -- Configure_Regular_Channel_Nbr --
   -----------------------------------

   procedure Configure_Regular_Channel_Nbr
     (This  : in out Analog_To_Digital_Converter;
      Number : UInt4)
   is
   begin
      This.SQR1.L := Number;
   end Configure_Regular_Channel_Nbr;

   -----------------------------------
   -- Configure_Regular_Conversions --
   -----------------------------------

   procedure Configure_Regular_Conversions
     (This        : in out Analog_To_Digital_Converter;
      Continuous  : Boolean;
      Trigger     : Regular_Channel_Conversion_Trigger;
      Conversions : Regular_Channel_Conversions)
   is
   begin
      This.CFGR.CONT := Continuous;

      if Trigger.Enabler = Trigger_Disabled then
         This.CFGR.EXTSEL := 0;
         This.CFGR.EXTEN := 0;
      else
         This.CFGR.EXTSEL :=
           External_Events_Regular_Group'Enum_Rep (Trigger.Event);
         This.CFGR.EXTEN := External_Trigger'Enum_Rep (Trigger.Enabler);
      end if;

      for Rank in Conversions'Range loop
         declare
            Conversion : Regular_Channel_Conversion renames Conversions (Rank);
         begin
            Configure_Regular_Channel
              (This, Conversion.Channel, Rank, Conversion.Sample_Time);

            --  We check the VBat first because that channel is also used for
            --  the temperature sensor channel on some MCUs, in which case the
            --  VBat conversion is the only one done. This order reflects that
            --  hardware behavior.
            if VBat_Conversion (This, Conversion.Channel) then
               Enable_VBat_Connection (This);
            elsif VRef_TemperatureSensor_Conversion (This, Conversion.Channel)
            then
               Enable_VRef_TemperatureSensor_Connection (This);
            end if;
         end;
      end loop;

      This.SQR1.L := UInt4 (Conversions'Length - 1);  -- biased rep
   end Configure_Regular_Conversions;

   ----------------------------------
   -- Regular_Conversions_Expected --
   ----------------------------------

   function Regular_Conversions_Expected (This : Analog_To_Digital_Converter)
     return Natural is
     (Natural (This.SQR1.L) + 1);

   -----------------------
   -- Scan_Mode_Enabled --
   -----------------------

   function Scan_Mode_Enabled (This : Analog_To_Digital_Converter)
                               return Boolean
     is (This.SQR1.L /= UInt4 (0));

   --------------------------------
   -- Configure_Injected_Trigger --
   --------------------------------

   procedure Configure_Injected_Trigger
     (This          : in out Analog_To_Digital_Converter;
      AutoInjection : Boolean;
      Trigger       : Injected_Channel_Conversion_Trigger)
   is
   begin
      --  Injected channels cannot be converted continuously. The only
      --  exception is when an injected channel is configured to be converted
      --  automatically after regular channels in continuous mode. See note in
      --  RM 13.3.5, pg 390, and "Auto-injection" section on pg 392.
      This.CFGR.JAUTO := AutoInjection;

      if Trigger.Enabler = Trigger_Disabled then
         This.JSQR.JEXTEN := 0;
         This.JSQR.JEXTSEL := 0;
      else
         This.JSQR.JEXTEN := External_Trigger'Enum_Rep (Trigger.Enabler);
         This.JSQR.JEXTSEL := External_Events_Injected_Group'Enum_Rep (Trigger.Event);
      end if;
   end Configure_Injected_Trigger;

   --------------------------------
   -- Configure_Injected_Channel --
   --------------------------------

   procedure Configure_Injected_Channel
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Rank        : Injected_Channel_Rank;
      Sample_Time : Channel_Sampling_Times)
   is
   begin
      Set_Sampling_Time (This, Channel, Sample_Time);
      Set_Injected_Channel_Sequence_Position (This, Channel, Rank);
   end Configure_Injected_Channel;

   ------------------------------------
   -- Configure_Injected_Channel_Nbr --
   ------------------------------------

   procedure Configure_Injected_Channel_Nbr
     (This   : in out Analog_To_Digital_Converter;
      Number : UInt2)
   is
   begin
      This.JSQR.JL := Number;
   end Configure_Injected_Channel_Nbr;

   ------------------------------------
   -- Configure_Injected_Conversions --
   ------------------------------------

   procedure Configure_Injected_Conversions
     (This          : in out Analog_To_Digital_Converter;
      AutoInjection : Boolean;
      Trigger       : Injected_Channel_Conversion_Trigger;
      Conversions   : Injected_Channel_Conversions)
   is
   begin

      --  Injected channels cannot be converted continuously. The only
      --  exception is when an injected channel is configured to be converted
      --  automatically after regular channels in continuous mode. See note in
      --  RM 13.3.5, pg 390, and "Auto-injection" section on pg 392.
      This.CFGR.JAUTO := AutoInjection;

      if Trigger.Enabler = Trigger_Disabled then
         This.JSQR.JEXTEN := 0;
         This.JSQR.JEXTSEL := 0;
      else
         This.JSQR.JEXTEN := External_Trigger'Enum_Rep (Trigger.Enabler);
         This.JSQR.JEXTSEL := External_Events_Injected_Group'Enum_Rep (Trigger.Event);
      end if;

      for Rank in Conversions'Range loop
         declare
            Conversion : Injected_Channel_Conversion renames
              Conversions (Rank);
         begin
            Configure_Injected_Channel
              (This,
               Conversion.Channel,
               Rank,
               Conversion.Sample_Time);

            --  We check the VBat first because that channel is also used for
            --  the temperature sensor channel on some MCUs, in which case the
            --  VBat conversion is the only one done. This order reflects that
            --  hardware behavior.
            if VBat_Conversion (This, Conversion.Channel) then
               Enable_VBat_Connection (This);
            elsif VRef_TemperatureSensor_Conversion (This, Conversion.Channel)
            then
               Enable_VRef_TemperatureSensor_Connection (This);
            end if;
         end;
      end loop;

      This.JSQR.JL := UInt2 (Conversions'Length - 1);  -- biased rep
   end Configure_Injected_Conversions;

   -----------------------------------
   -- Injected_Conversions_Expected --
   -----------------------------------

   function Injected_Conversions_Expected (This : Analog_To_Digital_Converter)
     return Natural is
     (Natural (This.JSQR.JL) + 1);

   ------------------------------
   -- Configure_Channel_Offset --
   ------------------------------

   procedure Configure_Channel_Offset
     (This       : in out Analog_To_Digital_Converter;
      Channel    : Analog_Input_Channel;
      Rank       : Offset_Channel_Rank;
      Offset     : Data_Offset;
      Signal     : Offset_Signal;
      Saturation : Boolean;
      Enabled    : Boolean)
   is
   begin
      case Rank is
         when 1 =>
            This.OFR1.OFFSET1_CH := Channel;
            This.OFR1.OFFSET1 := Offset;
            This.OFR1.OFFSETPOS := Signal = Plus;
            This.OFR1.SATEN := Saturation;
            This.OFR1.OFFSET1_EN := Enabled;
         when 2 =>
            This.OFR2.OFFSET2_CH := Channel;
            This.OFR2.OFFSET2 := Offset;
            This.OFR2.OFFSETPOS := Signal = Plus;
            This.OFR2.SATEN := Saturation;
            This.OFR2.OFFSET2_EN := Enabled;
         when 3 =>
            This.OFR3.OFFSET3_CH := Channel;
            This.OFR3.OFFSET3 := Offset;
            This.OFR3.OFFSETPOS := Signal = Plus;
            This.OFR3.SATEN := Saturation;
            This.OFR3.OFFSET3_EN := Enabled;
         when 4 =>
            This.OFR4.OFFSET4_CH := Channel;
            This.OFR4.OFFSET4 := Offset;
            This.OFR4.OFFSETPOS := Signal = Plus;
            This.OFR4.SATEN := Saturation;
            This.OFR4.OFFSET4_EN := Enabled;
      end case;
   end Configure_Channel_Offset;

   ------------------------
   -- Set_Channel_Offset --
   ------------------------

   procedure Set_Channel_Offset
     (This    : in out Analog_To_Digital_Converter;
      Rank    : Offset_Channel_Rank;
      Enabled : Boolean)
   is
   begin
      case Rank is
         when 1 =>
            This.OFR1.OFFSET1_EN := Enabled;
         when 2 =>
            This.OFR2.OFFSET2_EN := Enabled;
         when 3 =>
            This.OFR3.OFFSET3_EN := Enabled;
         when 4 =>
            This.OFR4.OFFSET4_EN := Enabled;
      end case;
   end Set_Channel_Offset;

   ----------------------------
   -- Enable_VBat_Connection --
   ----------------------------

   procedure Enable_VBat_Connection (This : Analog_To_Digital_Converter) is
   begin
      if This'Address = ADC1_Base or
         This'Address = ADC2_Base
      then
         ADC12_Common_Periph.CCR.VBATSEL := True;
      else
         ADC345_Common_Periph.CCR.VBATSEL := True;
      end if;
   end Enable_VBat_Connection;

   ------------------
   -- VBat_Enabled --
   ------------------

   function VBat_Enabled (This : Analog_To_Digital_Converter) return Boolean is
   begin
      if This'Address = ADC1_Base or
         This'Address = ADC2_Base
      then
         return ADC12_Common_Periph.CCR.VBATSEL;
      else
         return ADC345_Common_Periph.CCR.VBATSEL;
      end if;
   end VBat_Enabled;

   ----------------------------------------------
   -- Enable_VRef_TemperatureSensor_Connection --
   ----------------------------------------------

   procedure Enable_VRef_TemperatureSensor_Connection
     (This : Analog_To_Digital_Converter)
   is
   begin
      if This'Address = ADC1_Base or
         This'Address = ADC2_Base
      then
         ADC12_Common_Periph.CCR.VREFEN := True;
         ADC12_Common_Periph.CCR.VSENSESEL := True;
      else
         ADC345_Common_Periph.CCR.VREFEN := True;
         ADC345_Common_Periph.CCR.VSENSESEL := True;
      end if;
      declare
         Start_Time : constant Time := Clock;
      begin
         loop
            exit when Clock > Start_Time + Temperature_Sensor_Stabilization;
         end loop;
      end;
      --  Avoid using delay statements so this can be used in protected operations.
   end Enable_VRef_TemperatureSensor_Connection;

   ------------------------------------
   -- VRef_TemperatureSensor_Enabled --
   ------------------------------------

   function VRef_TemperatureSensor_Enabled
     (This : Analog_To_Digital_Converter) return Boolean
   is
   begin
      if This'Address = ADC1_Base or
        This'Address = ADC2_Base
      then
         return ADC12_Common_Periph.CCR.VREFEN and
           ADC12_Common_Periph.CCR.VSENSESEL;
      else
         return ADC345_Common_Periph.CCR.VREFEN and
           ADC345_Common_Periph.CCR.VSENSESEL;
      end if;
   end VRef_TemperatureSensor_Enabled;

   ---------------------------------
   -- Configure_Common_Properties --
   ---------------------------------

   procedure Configure_Common_Properties
     (This           : Analog_To_Digital_Converter;
      Mode           : Multi_ADC_Mode_Selections;
      Prescaler      : ADC_Prescaler;
      Clock_Mode     : ADC_Clock_Mode;
      DMA_Mode       : Dual_ADC_DMA_Modes;
      Sampling_Delay : Sampling_Delay_Selections)
   is
   begin
      if This'Address = ADC1_Base or
         This'Address = ADC2_Base
      then
         ADC12_Common_Periph.CCR.DUAL := Mode'Enum_Rep;
         ADC12_Common_Periph.CCR.PRESC := Prescaler'Enum_Rep;
         ADC12_Common_Periph.CCR.CKMODE := Clock_Mode'Enum_Rep;
         ADC12_Common_Periph.CCR.MDMA := DMA_Mode'Enum_Rep;
         ADC12_Common_Periph.CCR.DELAY_k := Sampling_Delay'Enum_Rep;
      else
         ADC345_Common_Periph.CCR.DUAL    := Mode'Enum_Rep;
         ADC345_Common_Periph.CCR.PRESC := Prescaler'Enum_Rep;
         ADC345_Common_Periph.CCR.CKMODE := Clock_Mode'Enum_Rep;
         ADC345_Common_Periph.CCR.MDMA    := DMA_Mode'Enum_Rep;
         ADC345_Common_Periph.CCR.DELAY_k := Sampling_Delay'Enum_Rep;
      end if;
   end Configure_Common_Properties;

   ----------------------
   -- Start_Conversion --
   ----------------------

   procedure Start_Conversion (This : in out Analog_To_Digital_Converter) is
   begin
      if This'Address = ADC1_Base or --  master channel
         This'Address = ADC3_Base or  --  master channel
         This'Address = ADC5_Base --  independent channel
      then
         This.CR.ADSTART := True;
      elsif This'Address = ADC2_Base then --  slave channel
         if Multi_ADC_Mode_Selections'Enum_Val (ADC12_Common_Periph.CCR.DUAL) = Independent
         then
            This.CR.ADSTART := True;
         end if;
      elsif This'Address = ADC4_Base then --  slave channel
         if Multi_ADC_Mode_Selections'Enum_Val (ADC345_Common_Periph.CCR.DUAL) = Independent
         then
            This.CR.ADSTART := True;
         end if;
      end if;
   end Start_Conversion;

   ---------------------
   -- Stop_Conversion --
   ---------------------

   procedure Stop_Conversion (This : in out Analog_To_Digital_Converter) is
   begin
      This.CR.ADSTP := True;
   end Stop_Conversion;

   ------------------------
   -- Conversion_Started --
   ------------------------

   function Conversion_Started (This : Analog_To_Digital_Converter)
      return Boolean
   is
      (This.CR.ADSTART);

   ----------------------
   -- Conversion_Value --
   ----------------------

   function Conversion_Value
     (This : Analog_To_Digital_Converter)
      return UInt16
   is
   begin
      return This.DR.RDATA;
   end Conversion_Value;

   ---------------------------
   -- Data_Register_Address --
   ---------------------------

   function Data_Register_Address
     (This : Analog_To_Digital_Converter)
      return System.Address
   is
      (This.DR'Address);

   -------------------------------
   -- Start_Injected_Conversion --
   -------------------------------

   procedure Start_Injected_Conversion
     (This : in out Analog_To_Digital_Converter)
   is
   begin
      This.CR.JADSTART := True;
   end Start_Injected_Conversion;

   ---------------------------------
   -- Injected_Conversion_Started --
   ---------------------------------

   function Injected_Conversion_Started (This : Analog_To_Digital_Converter)
     return Boolean
   is
     (This.CR.JADSTART);

   -------------------------------
   -- Injected_Conversion_Value --
   -------------------------------

   function Injected_Conversion_Value
     (This : Analog_To_Digital_Converter;
      Rank : Injected_Channel_Rank)
      return UInt16
   is
   begin
      case Rank is
         when 1 =>
            return This.JDR1.JDATA1;
         when 2 =>
            return This.JDR2.JDATA2;
         when 3 =>
            return This.JDR3.JDATA3;
         when 4 =>
            return This.JDR4.JDATA4;
      end case;
   end Injected_Conversion_Value;

   --------------------------------
   -- Multimode_Conversion_Value --
   --------------------------------

   function Multimode_Conversion_Value
     (This  : Analog_To_Digital_Converter;
      Value : CDR_Data) return UInt16
   is
   begin
      case Value is
         when Master =>
            if This'Address = ADC1_Base or
               This'Address = ADC2_Base
            then
               return ADC12_Common_Periph.CDR.RDATA_MST;
            else
               return ADC345_Common_Periph.CDR.RDATA_MST;
            end if;
         when Slave =>
            if This'Address = ADC1_Base or
               This'Address = ADC2_Base
            then
               return ADC12_Common_Periph.CDR.RDATA_SLV;
            else
               return ADC345_Common_Periph.CDR.RDATA_SLV;
            end if;
      end case;
   end Multimode_Conversion_Value;

   --------------------------------
   -- Multimode_Conversion_Value --
   --------------------------------

   function Multimode_Conversion_Value
     (This : Analog_To_Digital_Converter) return UInt32
   is
   begin
      if This'Address = ADC1_Base or
         This'Address = ADC2_Base
      then
         return Shift_Left (UInt32 (ADC12_Common_Periph.CDR.RDATA_MST), 16) or
              UInt32 (ADC12_Common_Periph.CDR.RDATA_SLV);
      else
         return Shift_Left (UInt32 (ADC345_Common_Periph.CDR.RDATA_MST), 16) or
              UInt32 (ADC345_Common_Periph.CDR.RDATA_SLV);
      end if;
   end Multimode_Conversion_Value;

   -------------------------------
   -- Enable_Discontinuous_Mode --
   -------------------------------

   procedure Enable_Discontinuous_Mode
     (This    : in out Analog_To_Digital_Converter;
      Regular : Boolean;  -- if False, enabling for Injected channels
      Count   : Discontinuous_Mode_Channel_Count)
   is
   begin
      if Regular then
         This.CFGR.JDISCEN := False;
         This.CFGR.DISCEN := True;
      else -- Injected
         This.CFGR.DISCEN := False;
         This.CFGR.JDISCEN := True;
      end if;
      This.CFGR.DISCNUM := UInt3 (Count - 1);  -- biased
   end Enable_Discontinuous_Mode;

   ----------------------------------------
   -- Disable_Discontinuous_Mode_Regular --
   ---------------------------------------

   procedure Disable_Discontinuous_Mode_Regular
     (This : in out Analog_To_Digital_Converter)
   is
   begin
      This.CFGR.DISCEN := False;
   end Disable_Discontinuous_Mode_Regular;

   -----------------------------------------
   -- Disable_Discontinuous_Mode_Injected --
   -----------------------------------------

   procedure Disable_Discontinuous_Mode_Injected
     (This : in out Analog_To_Digital_Converter)
   is
   begin
      This.CFGR.JDISCEN := False;
   end Disable_Discontinuous_Mode_Injected;

   ----------------------------------------
   -- Discontinuous_Mode_Regular_Enabled --
   ----------------------------------------

   function Discontinuous_Mode_Regular_Enabled
     (This : Analog_To_Digital_Converter)
      return Boolean
   is (This.CFGR.DISCEN);

   -----------------------------------------
   -- Discontinuous_Mode_Injected_Enabled --
   -----------------------------------------

   function Discontinuous_Mode_Injected_Enabled
     (This : Analog_To_Digital_Converter)
      return Boolean
   is (This.CFGR.JDISCEN);

   ---------------------------
   -- AutoInjection_Enabled --
   ---------------------------

   function AutoInjection_Enabled
     (This : Analog_To_Digital_Converter)
      return Boolean
   is (This.CFGR.JAUTO);

   ----------------
   -- Enable_DMA --
   ----------------

   procedure Enable_DMA (This : in out Analog_To_Digital_Converter) is
   begin
      This.CFGR.DMAEN := True;
   end Enable_DMA;

   -----------------
   -- Disable_DMA --
   -----------------

   procedure Disable_DMA (This : in out Analog_To_Digital_Converter) is
   begin
      This.CFGR.DMAEN := False;
   end Disable_DMA;

   -----------------
   -- DMA_Enabled --
   -----------------

   function DMA_Enabled (This : Analog_To_Digital_Converter) return Boolean is
     (This.CFGR.DMAEN);

   ------------------------------------
   -- Enable_DMA_After_Last_Transfer --
   ------------------------------------

   procedure Enable_DMA_After_Last_Transfer
     (This : in out Analog_To_Digital_Converter)
   is
   begin
      This.CFGR.DMACFG := True;
   end Enable_DMA_After_Last_Transfer;

   -------------------------------------
   -- Disable_DMA_After_Last_Transfer --
   -------------------------------------

   procedure Disable_DMA_After_Last_Transfer
     (This : in out Analog_To_Digital_Converter)
   is
   begin
      This.CFGR.DMACFG := False;
   end Disable_DMA_After_Last_Transfer;

   -------------------------------------
   -- DMA_Enabled_After_Last_Transfer --
   -------------------------------------

   function DMA_Enabled_After_Last_Transfer
     (This : Analog_To_Digital_Converter)
      return Boolean
   is (This.CFGR.DMACFG);

   ------------------------------------------
   -- Multi_Enable_DMA_After_Last_Transfer --
   ------------------------------------------

   procedure Multi_Enable_DMA_After_Last_Transfer
     (This : Analog_To_Digital_Converter)
   is
      Value : UInt2;
   begin
      --  This is a common register. You must choose the value in
      --  accordance to the resolution: 2 for 12 and 10-bit resolution,
      --  3 for 8 and 6-bit resolution.

         if This.CFGR.RES = 0 or --  12 bits
            This.CFGR.RES = 1    --  10 bits
         then
            Value := 2;
         else
            Value := 3;
         end if;

      if This'Address = ADC1_Base or
         This'Address = ADC2_Base
      then
         ADC12_Common_Periph.CCR.MDMA := Value;
      else
         ADC345_Common_Periph.CCR.MDMA := Value;
      end if;
   end Multi_Enable_DMA_After_Last_Transfer;

   -------------------------------------------
   -- Multi_Disable_DMA_After_Last_Transfer --
   -------------------------------------------

   procedure Multi_Disable_DMA_After_Last_Transfer
     (This : Analog_To_Digital_Converter)
   is
   begin
      if This'Address = ADC1_Base or
         This'Address = ADC2_Base
      then
         ADC12_Common_Periph.CCR.MDMA := 0;
      else
         ADC345_Common_Periph.CCR.MDMA := 0;
      end if;
   end Multi_Disable_DMA_After_Last_Transfer;

   -------------------------------------------
   -- Multi_DMA_Enabled_After_Last_Transfer --
   -------------------------------------------

   function Multi_DMA_Enabled_After_Last_Transfer
     (This : Analog_To_Digital_Converter) return Boolean
   is
   begin
      if This'Address = ADC1_Base or
         This'Address = ADC2_Base
      then
         return ADC12_Common_Periph.CCR.MDMA /= 0;
      else
         return ADC345_Common_Periph.CCR.MDMA /= 0;
      end if;
   end Multi_DMA_Enabled_After_Last_Transfer;

   ------------------------------
   -- Watchdog_Enable_Channels --
   ------------------------------

   procedure Watchdog_Enable_Channels
     (This : in out Analog_To_Digital_Converter;
      Mode : Multiple_Channels_Watchdog;
      Low  : Watchdog_Threshold;
      High : Watchdog_Threshold)
   is
   begin
      This.TR1.HT1 := High;
      This.TR1.LT1 := Low;
      --  see RM 13.3.28, pg 258, table 45

      --  Enable all channel mode
      This.CFGR.AWD1SGL := False;
      case Mode is
         when Watchdog_All_Regular_Channels =>
            This.CFGR.AWD1EN := True;
         when Watchdog_All_Injected_Channels =>
            This.CFGR.JAWD1EN := True;
         when Watchdog_All_Both_Kinds =>
            This.CFGR.AWD1EN := True;
            This.CFGR.JAWD1EN := True;
      end case;
   end Watchdog_Enable_Channels;

   -----------------------------
   -- Watchdog_Enable_Channel --
   -----------------------------

   procedure Watchdog_Enable_Channel
     (This    : in out Analog_To_Digital_Converter;
      Mode    : Single_Channel_Watchdog;
      Channel : Analog_Input_Channel;
      Low     : Watchdog_Threshold;
      High    : Watchdog_Threshold)
   is
   begin
      This.TR1.HT1 := High;
      This.TR1.LT1 := Low;

      --  Set then channel
      This.CFGR.AWD1CH := Channel;
      --  Enable single channel mode
      This.CFGR.AWD1SGL := True;

      case Mode is
         when Watchdog_Single_Regular_Channel =>
            This.CFGR.AWD1EN := True;
         when Watchdog_Single_Injected_Channel =>
            This.CFGR.JAWD1EN := True;
         when Watchdog_Single_Both_Kinds =>
            This.CFGR.AWD1EN := True;
            This.CFGR.JAWD1EN := True;
      end case;
   end Watchdog_Enable_Channel;

   ----------------------
   -- Watchdog_Disable --
   ----------------------

   procedure Watchdog_Disable (This : in out Analog_To_Digital_Converter) is
   begin
      This.CFGR.AWD1EN := False;
      This.CFGR.JAWD1EN := False;

      --  clearing the single-channel bit (AWGSDL) is not required to disable,
      --  per the RM table 66, section 13.3.7, pg 391, but seems cleanest
      This.CFGR.AWD1SGL := False;
   end Watchdog_Disable;

   ----------------------
   -- Watchdog_Enabled --
   ----------------------

   function Watchdog_Enabled (This : Analog_To_Digital_Converter)
     return Boolean
   is
      (This.CFGR.AWD1EN or This.CFGR.JAWD1EN);

   -------------------------------
   -- Watchdog_Enable_Filtering --
   -------------------------------

   procedure Watchdog_Enable_Filtering
     (This   : in out Analog_To_Digital_Converter;
      Filter : Analog_Watchdog_Filtering)
   is
   begin
      This.TR1.AWDFILT := Filter'Enum_Rep;
   end Watchdog_Enable_Filtering;

   -----------------------------
   -- Watchdog_Enable_Channel --
   -----------------------------
   procedure Watchdog_Enable_Channel
     (This     : in out Analog_To_Digital_Converter;
      Watchdog : Analog_Window_Watchdog;
      Channel  : Analog_Input_Channel;
      Low      : Watchdog_Threshold;
      High     : Watchdog_Threshold)
   is
   begin
      case Watchdog is
         when Watchdog_2 =>
            This.TR2.HT2 := UInt8 (Shift_Right (UInt16 (High), 4));
            This.TR2.LT2 := UInt8 (Shift_Right (UInt16 (Low), 4));
            This.AWD2CR.AWD2CH := This.AWD2CR.AWD2CH or (2 ** Natural (Channel));
         when Watchdog_3 =>
            This.TR3.HT3 := UInt8 (Shift_Right (UInt16 (High), 4));
            This.TR3.LT3 := UInt8 (Shift_Right (UInt16 (Low), 4));
            This.AWD3CR.AWD3CH := This.AWD3CR.AWD3CH or (2 ** Natural (Channel));
      end case;
   end Watchdog_Enable_Channel;

   ------------------------------
   -- Watchdog_Disable_Channel --
   ------------------------------
   procedure Watchdog_Disable_Channel
     (This     : in out Analog_To_Digital_Converter;
      Watchdog : Analog_Window_Watchdog;
      Channel  : Analog_Input_Channel)
   is
   begin
      case Watchdog is
         when Watchdog_2 =>
            This.AWD2CR.AWD2CH := This.AWD2CR.AWD2CH and not (2 ** Natural (Channel));
         when Watchdog_3 =>
            This.AWD3CR.AWD3CH := This.AWD3CR.AWD3CH and not (2 ** Natural (Channel));
      end case;
   end Watchdog_Disable_Channel;

   ----------------------
   -- Watchdog_Disable --
   ----------------------

   procedure Watchdog_Disable
     (This     : in out Analog_To_Digital_Converter;
      Watchdog : Analog_Window_Watchdog)
   is
   begin
      case Watchdog is
         when Watchdog_2 =>
            This.AWD2CR.AWD2CH := 16#000#;
         when Watchdog_3 =>
            This.AWD3CR.AWD3CH := 16#000#;
      end case;
   end Watchdog_Disable;

   ----------------------
   -- Watchdog_Enabled --
   ----------------------

   function Watchdog_Enabled
     (This     : Analog_To_Digital_Converter;
      Watchdog : Analog_Window_Watchdog) return Boolean
   is
   begin
      case Watchdog is
         when Watchdog_2 =>
            return This.AWD2CR.AWD2CH /= 16#000#;
         when Watchdog_3 =>
            return This.AWD3CR.AWD3CH /= 16#000#;
      end case;
   end Watchdog_Enabled;

   ------------
   -- Status --
   ------------

   function Status
     (This : Analog_To_Digital_Converter;
      Flag : ADC_Status_Flag)
      return Boolean
   is
   begin
      case Flag is
         when ADC_Ready =>
            return This.ISR.ADRDY;
         when Regular_Channel_Conversion_Completed =>
            return This.ISR.EOC;
         when Regular_Sequence_Conversion_Completed =>
            return This.ISR.EOS;
         when Injected_Channel_Conversion_Completed =>
            return This.ISR.JEOC;
         when Injected_Sequence_Conversion_Completed =>
            return This.ISR.JEOS;
         when Analog_Watchdog_1_Event_Occurred =>
            return This.ISR.AWD.Arr (1);
         when Analog_Watchdog_2_Event_Occurred =>
            return This.ISR.AWD.Arr (2);
         when Analog_Watchdog_3_Event_Occurred =>
            return This.ISR.AWD.Arr (3);
         when Sampling_Completed =>
            return This.ISR.EOSMP;
         when Overrun =>
            return This.ISR.OVR;
         when Injected_Context_Queue_Overflow =>
            return This.ISR.JQOVF;
      end case;
   end Status;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status
     (This : in out Analog_To_Digital_Converter;
      Flag : ADC_Status_Flag)
   is
   begin
      case Flag is
         when ADC_Ready =>
            This.ISR.ADRDY := True;
         when Regular_Channel_Conversion_Completed =>
            This.ISR.EOC := True;
         when Regular_Sequence_Conversion_Completed =>
            This.ISR.EOS := True;
         when Injected_Channel_Conversion_Completed =>
            This.ISR.JEOC := True;
         when Injected_Sequence_Conversion_Completed =>
            This.ISR.JEOS := True;
         when Analog_Watchdog_1_Event_Occurred =>
            This.ISR.AWD.Arr (1) := True;
         when Analog_Watchdog_2_Event_Occurred =>
            This.ISR.AWD.Arr (2) := True;
         when Analog_Watchdog_3_Event_Occurred =>
            This.ISR.AWD.Arr (3) := True;
         when Sampling_Completed =>
            This.ISR.EOSMP := True;
         when Overrun =>
            This.ISR.OVR := True;
         when Injected_Context_Queue_Overflow =>
            This.ISR.JQOVF := True;
      end case;
   end Clear_Status;

   ---------------------
   -- Poll_For_Status --
   ---------------------

   procedure Poll_For_Status
     (This    : in out Analog_To_Digital_Converter;
      Flag    : ADC_Status_Flag;
      Success : out Boolean;
      Timeout : Time_Span := Time_Span_Last)
   is
      Deadline : constant Time := Clock + Timeout;
   begin
      Success := False;
      while Clock < Deadline loop
         if Status (This, Flag) then
            Success := True;
            exit;
         end if;
      end loop;
   end Poll_For_Status;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts
     (This   : in out Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
   is
   begin
      case Source is
         when ADC_Ready =>
            This.IER.ADRDYIE := True;
         when Regular_Channel_Conversion_Complete =>
            This.IER.EOCIE := True;
         when Regular_Sequence_Conversion_Complete =>
            This.IER.EOSIE := True;
         when Injected_Channel_Conversion_Complete =>
            This.IER.JEOCIE := True;
         when Injected_Sequence_Conversion_Complete =>
            This.IER.JEOSIE := True;
         when Analog_Watchdog_1_Event_Occurr =>
            This.IER.AWD1IE := True;
         when Analog_Watchdog_2_Event_Occurr =>
            This.IER.AWD2IE := True;
         when Analog_Watchdog_3_Event_Occurr =>
            This.IER.AWD3IE := True;
         when Sampling_Complete =>
            This.IER.EOSMPIE := True;
         when Overrun =>
            This.IER.OVRIE := True;
         when Injected_Context_Queue_Overflow =>
            This.IER.JQOVFIE := True;
      end case;
   end Enable_Interrupts;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This   : Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
     return Boolean
   is
   begin
      case Source is
         when ADC_Ready =>
            return This.IER.ADRDYIE;
         when Regular_Channel_Conversion_Complete =>
            return This.IER.EOCIE;
         when Regular_Sequence_Conversion_Complete =>
            return This.IER.EOSIE;
         when Injected_Channel_Conversion_Complete =>
            return This.IER.JEOCIE;
         when Injected_Sequence_Conversion_Complete =>
            return This.IER.JEOSIE;
         when Analog_Watchdog_1_Event_Occurr =>
            return This.IER.AWD1IE;
         when Analog_Watchdog_2_Event_Occurr =>
            return This.IER.AWD2IE;
         when Analog_Watchdog_3_Event_Occurr =>
            return This.IER.AWD3IE;
         when Sampling_Complete =>
            return This.IER.EOSMPIE;
         when Overrun =>
            return This.IER.OVRIE;
         when Injected_Context_Queue_Overflow =>
            return This.IER.JQOVFIE;
      end case;
   end Interrupt_Enabled;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts
     (This   : in out Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
   is
   begin
      case Source is
         when ADC_Ready =>
            This.IER.ADRDYIE := False;
         when Regular_Channel_Conversion_Complete =>
            This.IER.EOCIE := False;
         when Regular_Sequence_Conversion_Complete =>
            This.IER.EOSIE := False;
         when Injected_Channel_Conversion_Complete =>
            This.IER.JEOCIE := False;
         when Injected_Sequence_Conversion_Complete =>
            This.IER.JEOSIE := False;
         when Analog_Watchdog_1_Event_Occurr =>
            This.IER.AWD1IE := False;
         when Analog_Watchdog_2_Event_Occurr =>
            This.IER.AWD2IE := False;
         when Analog_Watchdog_3_Event_Occurr =>
            This.IER.AWD3IE := False;
         when Sampling_Complete =>
            This.IER.EOSMPIE := False;
         when Overrun =>
            This.IER.OVRIE := False;
         when Injected_Context_Queue_Overflow =>
            This.IER.JQOVFIE := False;
      end case;
   end Disable_Interrupts;

   -----------------------------
   -- Clear_Interrupt_Pending --
   -----------------------------

   procedure Clear_Interrupt_Pending
     (This   : in out Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
   is
   begin
      case Source is
         when ADC_Ready =>
            This.ISR.ADRDY := True;
         when Regular_Channel_Conversion_Complete =>
            This.ISR.EOC := True;
         when Regular_Sequence_Conversion_Complete =>
            This.ISR.EOS := True;
         when Injected_Channel_Conversion_Complete =>
            This.ISR.JEOC := True;
         when Injected_Sequence_Conversion_Complete =>
            This.ISR.JEOS := True;
         when Analog_Watchdog_1_Event_Occurr =>
            This.ISR.AWD.Arr (1) := True;
         when Analog_Watchdog_2_Event_Occurr =>
            This.ISR.AWD.Arr (2) := True;
         when Analog_Watchdog_3_Event_Occurr =>
            This.ISR.AWD.Arr (3) := True;
         when Sampling_Complete =>
            This.ISR.EOSMP := True;
         when Overrun =>
            This.ISR.OVR := True;
         when Injected_Context_Queue_Overflow =>
            This.ISR.JQOVF := True;
      end case;
   end Clear_Interrupt_Pending;

   ---------------------------
   -- Set_Sequence_Position --
   ---------------------------

   procedure Set_Sequence_Position
     (This    : in out Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel;
      Rank    : Regular_Channel_Rank)
   is
   begin
      case Rank is
         when 1 =>
            This.SQR1.SQ1 := Channel;
         when 2 =>
            This.SQR1.SQ2 := Channel;
         when 3 =>
            This.SQR1.SQ3 := Channel;
         when 4 =>
            This.SQR1.SQ4 := Channel;
         when 5 =>
            This.SQR2.SQ5 := Channel;
         when 6 =>
            This.SQR2.SQ6 := Channel;
         when 7 =>
            This.SQR2.SQ7 := Channel;
         when 8 =>
            This.SQR2.SQ8 := Channel;
         when 9 =>
            This.SQR2.SQ9 := Channel;
         when 10 =>
            This.SQR3.SQ10 := Channel;
         when 11 =>
            This.SQR3.SQ11 := Channel;
         when 12 =>
            This.SQR3.SQ12 := Channel;
         when 13 =>
            This.SQR3.SQ13 := Channel;
         when 14 =>
            This.SQR3.SQ14 := Channel;
         when 15 =>
            This.SQR4.SQ15 := Channel;
         when 16 =>
            This.SQR4.SQ16 := Channel;
      end case;
   end Set_Sequence_Position;

   --------------------------------------------
   -- Set_Injected_Channel_Sequence_Position --
   --------------------------------------------

   procedure Set_Injected_Channel_Sequence_Position
     (This    : in out Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel;
      Rank    : Injected_Channel_Rank)
   is
   begin
      case Rank is
         when 1 =>
            This.JSQR.JSQ1 := Channel;
         when 2 =>
            This.JSQR.JSQ2 := Channel;
         when 3 =>
            This.JSQR.JSQ3 := Channel;
         when 4 =>
            This.JSQR.JSQ4 := Channel;
      end case;
   end Set_Injected_Channel_Sequence_Position;

   -----------------------
   -- Set_Sampling_Time --
   -----------------------

   procedure Set_Sampling_Time
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Sample_Time : Channel_Sampling_Times)
   is
   begin
      case Channel is
         when 0 .. 9 =>
            This.SMPR1.SMP.Arr (Natural (Channel)) :=
              Channel_Sampling_Times'Enum_Rep (Sample_Time);
         when 10 .. 18 =>
            This.SMPR2.SMP.Arr (Natural (Channel)) :=
              Channel_Sampling_Times'Enum_Rep (Sample_Time);
      end case;
   end Set_Sampling_Time;

end STM32.ADC;
