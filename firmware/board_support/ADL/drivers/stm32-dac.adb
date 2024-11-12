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
--   @file    stm32f4xx_hal_dac.c and stm32f4xx_hal_dac_ex.c                --
--   @author  MCD Application Team                                          --
--   @version V1.3.1                                                        --
--   @date    25-March-2015                                                 --
--   @brief   Header file of DAC HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Real_Time; use Ada.Real_Time;

with STM32_SVD.DAC; use STM32_SVD.DAC;

package body STM32.DAC is

   ------------
   -- Enable --
   ------------

   procedure Enable
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CR.EN1 := True;
         when Channel_2 =>
            This.CR.EN2 := True;
      end case;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CR.EN1 := False;
         when Channel_2 =>
            This.CR.EN2 := False;
      end case;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean is
   begin
      case Channel is
         when Channel_1 =>
            return This.CR.EN1;
         when Channel_2 =>
            return This.CR.EN2;
      end case;
   end Enabled;

   ----------------------------
   -- Configure_Interface_Mode --
   ----------------------------

   procedure Configure_Interface_Mode
     (This : in out Digital_To_Analog_Converter;
      Mode : HF_Interface_Mode)
   is
   begin
      This.MCR.HFSEL := Mode'Enum_Rep;
   end Configure_Interface_Mode;

   -------------------------
   -- Read_Interface_Mode --
   -------------------------

   function Read_Interface_Mode
     (This : Digital_To_Analog_Converter)
      return HF_Interface_Mode
   is
   begin
      return HF_Interface_Mode'Val (This.MCR.HFSEL);
   end Read_Interface_Mode;

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output
     (This       : in out Digital_To_Analog_Converter;
      Channel    : DAC_Channel;
      Value      : UInt16;
      Resolution : DAC_Resolution;
      Alignment  : Data_Alignment)
   is
   begin
      case Channel is

         when Channel_1 =>
            case Resolution is
               when DAC_Resolution_12_Bits =>
                  case Alignment is
                     when Left_Aligned =>
                        This.DHR12L1.DACC1DHR :=
                          UInt12 (Value and Max_12bit_Resolution);
                     when Right_Aligned =>
                        This.DHR12R1.DACC1DHR :=
                          UInt12 (Value and Max_12bit_Resolution);
                  end case;
               when DAC_Resolution_8_Bits =>
                  This.DHR8R1.DACC1DHR := UInt8 (Value and Max_8bit_Resolution);
            end case;

         when Channel_2 =>
            case Resolution is
               when DAC_Resolution_12_Bits =>
                  case Alignment is
                     when Left_Aligned =>
                        This.DHR12L2.DACC2DHR :=
                          UInt12 (Value and Max_12bit_Resolution);
                     when Right_Aligned =>
                        This.DHR12R2.DACC2DHR :=
                          UInt12 (Value and Max_12bit_Resolution);
                  end case;
               when DAC_Resolution_8_Bits =>
                  This.DHR8R2.DACC2DHR := UInt8 (Value and Max_8bit_Resolution);
            end case;

      end case;
   end Set_Output;

   ----------------------
   -- Set_Input_Format --
   ----------------------

   procedure Set_Input_Format
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel;
      Format  : Input_Format)
   is
   begin
      case Channel is
         when Channel_1 =>
            if Format = Signed then
               This.MCR.SINFORMAT1 := True;
            else
               This.MCR.SINFORMAT1 := False;
            end if;
         when Channel_2 =>
            if Format = Signed then
               This.MCR.SINFORMAT2 := True;
            else
               This.MCR.SINFORMAT2 := False;
            end if;
      end case;
   end Set_Input_Format;

   -----------------------
   -- Read_input_Format --
   -----------------------

   function Read_Input_Format
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Input_Format
   is
   begin
      case Channel is
         when Channel_1 =>
            if This.MCR.SINFORMAT1 then
               return Signed;
            else
               return Unsigned;
            end if;
         when Channel_2 =>
            if This.MCR.SINFORMAT2 then
               return Signed;
            else
               return Unsigned;
            end if;
      end case;
   end Read_Input_Format;

   ------------------------------------
   -- Trigger_Conversion_By_Software --
   ------------------------------------

   procedure Trigger_Conversion_By_Software
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.SWTRGR.SWTRIG.Arr (1) := True; -- cleared by hardware
         when Channel_2 =>
            This.SWTRGR.SWTRIG.Arr (2) := True; -- cleared by hardware
      end case;
   end Trigger_Conversion_By_Software;

   ----------------------------
   -- Converted_Output_Value --
   ----------------------------

   function Converted_Output_Value
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return UInt16
   is
   begin
      case Channel is
         when Channel_1 =>
            return UInt16 (This.DOR1.DACC1DOR);
         when Channel_2 =>
            return UInt16 (This.DOR2.DACC2DOR);
      end case;
   end Converted_Output_Value;

   ------------------------------
   -- Set_Dual_Output_Voltages --
   ------------------------------

   procedure Set_Dual_Output_Voltages
     (This            : in out Digital_To_Analog_Converter;
      Channel_1_Value : UInt16;
      Channel_2_Value : UInt16;
      Resolution      : DAC_Resolution;
      Alignment       : Data_Alignment)
   is
   begin
      case Resolution is
         when DAC_Resolution_12_Bits =>
            case Alignment is
               when Left_Aligned =>
                  This.DHR12LD.DACC1DHR :=
                    UInt12 (Channel_1_Value and Max_12bit_Resolution);
                  This.DHR12LD.DACC2DHR :=
                    UInt12 (Channel_2_Value and Max_12bit_Resolution);
               when Right_Aligned =>
                  This.DHR12RD.DACC1DHR :=
                    UInt12 (Channel_1_Value and Max_12bit_Resolution);
                  This.DHR12RD.DACC2DHR :=
                    UInt12 (Channel_2_Value and Max_12bit_Resolution);
            end case;
         when DAC_Resolution_8_Bits =>
            This.DHR8RD.DACC1DHR :=
              UInt8 (Channel_1_Value and Max_8bit_Resolution);
            This.DHR8RD.DACC2DHR :=
              UInt8 (Channel_2_Value and Max_8bit_Resolution);
      end case;
   end Set_Dual_Output_Voltages;

   ---------------------------------
   -- Converted_Dual_Output_Value --
   ---------------------------------

   function Converted_Dual_Output_Value (This : Digital_To_Analog_Converter)
      return Dual_Channel_Output
   is
      Result : Dual_Channel_Output;
   begin
      Result.Channel_1_Data := UInt16 (This.DOR1.DACC1DOR);
      Result.Channel_2_Data := UInt16 (This.DOR2.DACC2DOR);
      return Result;
   end Converted_Dual_Output_Value;

   ----------------------------
   -- Configure_Channel_Mode --
   ----------------------------

   procedure Configure_Channel_Mode
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel;
      Mode    : Channel_Mode)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.MCR.MODE1 := Mode'Enum_Rep;
         when Channel_2 =>
            This.MCR.MODE2 := Mode'Enum_Rep;
      end case;
   end Configure_Channel_Mode;

   -----------------------
   -- Read_Channel_Mode --
   -----------------------

   function Read_Channel_Mode
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Channel_Mode
   is
   begin
      case Channel is
         when Channel_1 =>
            return Channel_Mode'Val (This.MCR.MODE1);
         when Channel_2 =>
            return Channel_Mode'Val (This.MCR.MODE2);
      end case;
   end Read_Channel_Mode;

   --------------------------------
   --  Configure_SampleHold_Time --
   --------------------------------

   procedure Configure_Sample_Hold_Time
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel;
      Sample  : UInt10;
      Hold    : UInt10;
      Refresh : UInt8)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.SHSR1.TSAMPLE1 := Sample;
            This.SHHR.THOLD1 := Hold;
            This.SHRR.TREFRESH1 := Refresh;
         when Channel_2 =>
            This.SHSR2.TSAMPLE2 := Sample;
            This.SHHR.THOLD2 := Hold;
            This.SHRR.TREFRESH2 := Refresh;
      end case;
   end Configure_Sample_Hold_Time;

   --------------------
   -- Select_Trigger --
   --------------------

   procedure Select_Trigger
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel;
      Trigger : External_Event_Trigger_Selection)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CR.TSEL1 :=
              External_Event_Trigger_Selection'Enum_Rep (Trigger);
         when Channel_2 =>
            This.CR.TSEL2 :=
              External_Event_Trigger_Selection'Enum_Rep (Trigger);
      end case;
   end Select_Trigger;

   -----------------------
   -- Trigger_Selection --
   -----------------------

   function Trigger_Selection
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return External_Event_Trigger_Selection
   is
   begin
      case Channel is
         when Channel_1 =>
            return External_Event_Trigger_Selection'Val (This.CR.TSEL1);
         when Channel_2 =>
            return External_Event_Trigger_Selection'Val (This.CR.TSEL2);
      end case;
   end Trigger_Selection;

   --------------------
   -- Enable_Trigger --
   --------------------

   procedure Enable_Trigger
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CR.TEN1 := True;
         when Channel_2 =>
            This.CR.TEN2 := True;
      end case;
   end Enable_Trigger;

   ---------------------
   -- Disable_Trigger --
   ---------------------

   procedure Disable_Trigger
     (This : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CR.TEN1 := False;
         when Channel_2 =>
            This.CR.TEN2 := False;
      end case;
   end Disable_Trigger;

   ---------------------
   -- Trigger_Enabled --
   ---------------------

   function Trigger_Enabled
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean
   is
   begin
      case Channel is
         when Channel_1 =>
            return This.CR.TEN1;
         when Channel_2 =>
            return This.CR.TEN2;
      end case;
   end Trigger_Enabled;

   ----------------
   -- Enable_DMA --
   ----------------

   procedure Enable_DMA
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CR.DMAEN1 := True;
         when Channel_2 =>
            This.CR.DMAEN2 := True;
      end case;
   end Enable_DMA;

   -----------------
   -- Disable_DMA --
   -----------------

   procedure Disable_DMA
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CR.DMAEN1 := False;
         when Channel_2 =>
            This.CR.DMAEN2 := False;
      end case;
   end Disable_DMA;

   -----------------
   -- DMA_Enabled --
   -----------------

   function DMA_Enabled
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean
   is
   begin
      case Channel is
         when Channel_1 =>
            return This.CR.DMAEN1;
         when Channel_2 =>
            return This.CR.DMAEN2;
      end case;
   end DMA_Enabled;

   -------------------------
   -- Set_DMA_Double_Mode --
   -------------------------

   procedure Set_DMA_Double_Mode
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel;
      Mode    : Boolean)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.MCR.DMADOUBLE1 := Mode;
         when Channel_2 =>
            This.MCR.DMADOUBLE2 := Mode;
      end case;
   end Set_DMA_Double_Mode;

   ------------
   -- Status --
   ------------

   function Status
     (This : Digital_To_Analog_Converter;
      Flag : DAC_Status_Flag)
      return Boolean
   is
   begin
      case Flag is
         when DORB_Output_Channel_1 =>
            return This.SR.DORSTAT1;
         when DORB_Output_Channel_2 =>
            return This.SR.DORSTAT2;
         when DMA_Underrun_Channel_1 =>
            return This.SR.DMAUDR1;
         when DMA_Underrun_Channel_2 =>
            return This.SR.DMAUDR2;
         when Calibration_EQGT_Offset_Channel_1 =>
            return This.SR.CAL_FLAG1;
         when Calibration_EQGT_Offset_Channel_2 =>
            return This.SR.CAL_FLAG2;
         when Write_Operation_Busy_Channel_1 =>
            return This.SR.BWST1;
         when Write_Operation_Busy_Channel_2 =>
            return This.SR.BWST2;
      end case;
   end Status;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status
     (This : in out Digital_To_Analog_Converter;
      Flag : DAC_Status_Flag)
   is
   begin
      case Flag is
         when DORB_Output_Channel_1 =>
            This.SR.DORSTAT1 := True;
         when DORB_Output_Channel_2 =>
            This.SR.DORSTAT2 := True;
         when DMA_Underrun_Channel_1 =>
            This.SR.DMAUDR1 := True;
         when DMA_Underrun_Channel_2 =>
            This.SR.DMAUDR2 := True;
         when Calibration_EQGT_Offset_Channel_1 =>
            This.SR.CAL_FLAG1 := True;
         when Calibration_EQGT_Offset_Channel_2 =>
            This.SR.CAL_FLAG2 := True;
         when Write_Operation_Busy_Channel_1 =>
            This.SR.BWST1 := True;
         when Write_Operation_Busy_Channel_2 =>
            This.SR.BWST2 := True;
      end case;
   end Clear_Status;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts
     (This : in out Digital_To_Analog_Converter;
      Source : DAC_Interrupts)
   is
   begin
      case Source is
         when DMA_Underrun_Channel_1 =>
            This.CR.DMAUDRIE1 := True;
         when DMA_Underrun_Channel_2 =>
            This.CR.DMAUDRIE2 := True;
      end case;
   end Enable_Interrupts;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts
     (This   : in out Digital_To_Analog_Converter;
      Source : DAC_Interrupts)
   is
   begin
      case Source is
         when DMA_Underrun_Channel_1 =>
            This.CR.DMAUDRIE1 := False;
         when DMA_Underrun_Channel_2 =>
            This.CR.DMAUDRIE2 := False;
      end case;
   end Disable_Interrupts;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This   : Digital_To_Analog_Converter;
      Source : DAC_Interrupts)
      return Boolean
   is
   begin
      case Source is
         when DMA_Underrun_Channel_1 =>
            return This.CR.DMAUDRIE1;
         when DMA_Underrun_Channel_2 =>
            return This.CR.DMAUDRIE2;
      end case;
   end Interrupt_Enabled;

   -----------------------------
   -- Clear_Interrupt_Pending --
   -----------------------------

   procedure Clear_Interrupt_Pending
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.SR.DMAUDR1 := False;
         when Channel_2 =>
            This.SR.DMAUDR2 := False;
      end case;
   end Clear_Interrupt_Pending;

   ----------------------------
   -- Select_Wave_Generation --
   ----------------------------

   procedure Select_Wave_Generation
     (This      : in out Digital_To_Analog_Converter;
      Channel   : DAC_Channel;
      Selection : Wave_Generation)
   is

      function As_UInt4 is new Ada.Unchecked_Conversion
        (Source => Noise_Wave_Mask_Selection, Target => UInt4);

      function As_UInt4 is new Ada.Unchecked_Conversion
        (Source => Triangle_Wave_Amplitude_Selection, Target => UInt4);

   begin
      case Channel is
         when Channel_1 =>
            This.CR.WAVE1 :=
              Wave_Generation_Selection'Enum_Rep (Selection.Kind);
         when Channel_2 =>
            This.CR.WAVE2 :=
              Wave_Generation_Selection'Enum_Rep (Selection.Kind);
      end case;

      case Selection.Kind is

         when No_Wave_Generation =>
            null;

         when Noise_Wave =>
            case Channel is
               when Channel_1 =>
                  This.CR.MAMP1 := As_UInt4 (Selection.Mask);
               when Channel_2 =>
                  This.CR.MAMP2 := As_UInt4 (Selection.Mask);
            end case;

         when Triangle_Wave .. Sawtooth_Wave =>
            case Channel is
               when Channel_1 =>
                  This.CR.MAMP1 := As_UInt4 (Selection.Amplitude);
               when Channel_2 =>
                  This.CR.MAMP2 := As_UInt4 (Selection.Amplitude);
            end case;

      end case;
   end Select_Wave_Generation;

   ------------------------------
   -- Selected_Wave_Generation --
   ------------------------------

   function Selected_Wave_Generation
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Wave_Generation
   is
      Kind : Wave_Generation_Selection;

      function As_Mask is new Ada.Unchecked_Conversion
        (Target => Noise_Wave_Mask_Selection, Source => UInt4);

      function As_Amplitude is new Ada.Unchecked_Conversion
        (Target => Triangle_Wave_Amplitude_Selection, Source => UInt4);

   begin
      case Channel is
         when Channel_1 =>
            Kind := Wave_Generation_Selection'Val (This.CR.WAVE1);
         when Channel_2 =>
            Kind := Wave_Generation_Selection'Val (This.CR.WAVE2);
      end case;
      declare
         Result : Wave_Generation (Kind);
      begin
         case Kind is
            when No_Wave_Generation =>
               null;

            when Noise_Wave =>
               case Channel is
                  when Channel_1 =>
                     Result.Mask := As_Mask (This.CR.MAMP1);
                  when Channel_2 =>
                     Result.Mask := As_Mask (This.CR.MAMP2);
               end case;

            when Triangle_Wave .. Sawtooth_Wave =>
               case Channel is
                  when Channel_1 =>
                     Result.Amplitude := As_Amplitude (This.CR.MAMP1);
                  when Channel_2 =>
                     Result.Amplitude := As_Amplitude (This.CR.MAMP2);
               end case;
         end case;

         return Result;
      end;
   end Selected_Wave_Generation;

   ----------------------------------------
   -- Configure_Sawtooth_Wave_Generation --
   ----------------------------------------

   procedure Configure_Sawtooth_Wave_Generation
     (This      : in out Digital_To_Analog_Converter;
      Channel   : DAC_Channel;
      Start_Val : UInt12;
      Start_Trg : Sawtooth_Trigger_Selection;
      Incr_Val  : UInt16;
      Incr_Trg  : Sawtooth_Trigger_Selection;
      Direction : Sawtooth_Direction)
   is begin
      case Channel is
         when Channel_1 =>
            This.STR1.STRSTDATA1 := Start_Val;
            This.STR1.STINCDATA1 := Incr_Val;
            This.STR1.STDIR1 := Direction = Up;
            This.STMODR.STRSTTRIGSEL1 := Start_Trg'Enum_Rep;
            This.STMODR.STINCTRIGSEL1 := Incr_Trg'Enum_Rep;
         when Channel_2 =>
            This.STR2.STRSTDATA2 := Start_Val;
            This.STR2.STINCDATA2 := Incr_Val;
            This.STR2.STDIR2 := Direction = Up;
            This.STMODR.STRSTTRIGSEL2 := Start_Trg'Enum_Rep;
            This.STMODR.STINCTRIGSEL2 := Incr_Trg'Enum_Rep;
      end case;
   end Configure_Sawtooth_Wave_Generation;

   ------------------
   -- Data_Address --
   ------------------

   function Data_Address
     (This       : Digital_To_Analog_Converter;
      Channel    : DAC_Channel;
      Resolution : DAC_Resolution;
      Alignment  : Data_Alignment)
      return Address
   is
      Result : Address;
   begin
      case Channel is

         when Channel_1 =>
            case Resolution is
               when DAC_Resolution_12_Bits =>
                  case Alignment is
                     when Left_Aligned =>
                        Result := This.DHR12L1'Address;
                     when Right_Aligned =>
                        Result := This.DHR12R1'Address;
                  end case;
               when DAC_Resolution_8_Bits =>
                  Result := This.DHR8R1'Address;
            end case;

         when Channel_2 =>
            case Resolution is
               when DAC_Resolution_12_Bits =>
                  case Alignment is
                     when Left_Aligned =>
                        Result := This.DHR12L2'Address;
                     when Right_Aligned =>
                        Result := This.DHR12R2'Address;
                  end case;
               when DAC_Resolution_8_Bits =>
                  Result := This.DHR8R2'Address;
            end case;

      end case;

      return Result;
   end Data_Address;

   --------------------------
   -- Set_Calibration_Mode --
   --------------------------

   procedure Set_Calibration_Mode
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel;
      Mode    : Boolean)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CR.CEN1 := Mode;
         when Channel_2 =>
            This.CR.CEN2 := Mode;
      end case;
   end Set_Calibration_Mode;

   ---------------------------
   -- Read_Calibration_Mode --
   ---------------------------

   function Read_Calibration_Mode
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean
   is
   begin
      case Channel is
         when Channel_1 =>
            return This.CR.CEN1;
         when Channel_2 =>
            return This.CR.CEN2;
      end case;
   end Read_Calibration_Mode;

   ---------------------------
   -- Read_Calibration_Flag --
   ---------------------------

   function Read_Calibration_Flag
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean
   is
   begin
      case Channel is
         when Channel_1 =>
            return This.SR.CAL_FLAG1;
         when Channel_2 =>
            return This.SR.CAL_FLAG2;
      end case;
   end Read_Calibration_Flag;

   -------------------------
   -- Set_Offset_Trimming --
   -------------------------

   procedure Set_Offset_Trimming
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel;
      Input   : UInt5)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CCR.OTRIM1 := Input;
         when Channel_2 =>
            This.CCR.OTRIM2 := Input;
      end case;
   end Set_Offset_Trimming;

   ---------------
   -- Calibrate --
   ---------------

   procedure Calibrate (This    : in out Digital_To_Analog_Converter;
                        Channel : DAC_Channel)
   is
      Trimoffset : UInt5 := 0;
   begin
      --  1. If the DAC channel is active, write 0 to ENx bit in DAC_CR to
      --  disable the channel.
      if Enabled (This, Channel) then
         Disable (This, Channel);
      end if;

      --  2. Select a mode where the buffer is enabled, by writing to DAC_MCR
      --  register, MODEx[2:0] = 000b or 001b or 100b or 101b.
      Configure_Channel_Mode (This, Channel,
                              Mode => Normal_External_Pin_Buffer);

      --  3. Start the DAC channelx calibration, by setting the CENx bit in
      --  DAC_CR register to 1.
      Set_Calibration_Mode (This, Channel, Mode => True);

      --  4. Apply a trimming algorithm:
      --    a) Write a code into OTRIMx[4:0] bits starting by 00000b.
      --    b) Wait for tTRIM delay (tTRIMmax = 50 us as per Table 74 at pg 155
      --       in STM32G474xx data sheet DS12288 rev 5).
      --    c) Check if CAL_FLAGx bit in DAC_SR is set to 1.
      --    d) If CAL_FLAGx is set to 1, the OTRIMx[4:0] trimming code is found
      --       and can be used during device operation to compensate the output
      --       value, else increment OTRIMx[4:0] and repeat sub-steps from (a)
      --       to (d) again.

      Set_Offset_Trimming (This, Channel, Input => Trimoffset);
      --  Wait the tTRIMmax delay timing specified < 50 us.
      delay until (Clock + Microseconds (50));

      while not Read_Calibration_Flag (This, Channel) loop
         Trimoffset := Trimoffset + 1;
         Set_Offset_Trimming (This, Channel, Input => Trimoffset);
         --  Wait the tTRIMmax delay timing specified < 50 us.
         delay until (Clock + Microseconds (50));
      end loop;

   end Calibrate;

end STM32.DAC;
