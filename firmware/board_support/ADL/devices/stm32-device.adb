------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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
--     3. Neither the name of the copyright holder nor the names of its     --
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
------------------------------------------------------------------------------

--  with System;          use System; --  Disable for SPI, COMP and OPAMP

with STM32_SVD.RCC;   use STM32_SVD.RCC;
with STM32_SVD.CRC;   use STM32_SVD.CRC;
with STM32_SVD.LPTIM; use STM32_SVD.LPTIM;

with STM32.RCC;       use STM32.RCC;

package body STM32.Device is

   HPRE_Presc_Table : constant array (UInt4) of UInt32 :=
     (1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 8, 16, 64, 128, 256, 512);

   PPRE_Presc_Table : constant array (UInt3) of UInt32 :=
     (1, 1, 1, 1, 2, 4, 8, 16);

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased GPIO_Port) is
   begin
      if This'Address = GPIOA_Base then
         RCC_Periph.AHB2ENR.GPIOAEN := True;
      elsif This'Address = GPIOB_Base then
         RCC_Periph.AHB2ENR.GPIOBEN := True;
      elsif This'Address = GPIOC_Base then
         RCC_Periph.AHB2ENR.GPIOCEN := True;
      elsif This'Address = GPIOD_Base then
         RCC_Periph.AHB2ENR.GPIODEN := True;
      elsif This'Address = GPIOE_Base then
         RCC_Periph.AHB2ENR.GPIOEEN := True;
      elsif This'Address = GPIOF_Base then
         RCC_Periph.AHB2ENR.GPIOFEN := True;
      elsif This'Address = GPIOG_Base then
         RCC_Periph.AHB2ENR.GPIOGEN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (Point : GPIO_Point)
   is
   begin
      Enable_Clock (Point.Periph.all);
   end Enable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (Points : GPIO_Points)
   is
   begin
      for Point of Points loop
         Enable_Clock (Point.Periph.all);
      end loop;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased GPIO_Port) is
   begin
      if This'Address = GPIOA_Base then
         RCC_Periph.AHB2RSTR.GPIOARST := True;
         RCC_Periph.AHB2RSTR.GPIOARST := False;
      elsif This'Address = GPIOB_Base then
         RCC_Periph.AHB2RSTR.GPIOBRST := True;
         RCC_Periph.AHB2RSTR.GPIOBRST := False;
      elsif This'Address = GPIOC_Base then
         RCC_Periph.AHB2RSTR.GPIOCRST := True;
         RCC_Periph.AHB2RSTR.GPIOCRST := False;
      elsif This'Address = GPIOD_Base then
         RCC_Periph.AHB2RSTR.GPIODRST := True;
         RCC_Periph.AHB2RSTR.GPIODRST := False;
      elsif This'Address = GPIOE_Base then
         RCC_Periph.AHB2RSTR.GPIOERST := True;
         RCC_Periph.AHB2RSTR.GPIOERST := False;
      elsif This'Address = GPIOF_Base then
         RCC_Periph.AHB2RSTR.GPIOFRST := True;
         RCC_Periph.AHB2RSTR.GPIOFRST := False;
      elsif This'Address = GPIOG_Base then
         RCC_Periph.AHB2RSTR.GPIOGRST := True;
         RCC_Periph.AHB2RSTR.GPIOGRST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Point : GPIO_Point) is
   begin
      Reset (Point.Periph.all);
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Points : GPIO_Points)
   is
      Do_Reset : Boolean;
   begin
      for J in Points'Range loop
         Do_Reset := True;
         for K in Points'First .. J - 1 loop
            if Points (K).Periph = Points (J).Periph then
               Do_Reset := False;

               exit;
            end if;
         end loop;

         if Do_Reset then
            Reset (Points (J).Periph.all);
         end if;
      end loop;
   end Reset;

   ------------------------------
   -- GPIO_Port_Representation --
   ------------------------------

   function GPIO_Port_Representation (Port : GPIO_Port) return UInt4 is
   begin
      --  TODO: rather ugly to have this board-specific range here
      if Port'Address = GPIOA_Base then
         return 0;
      elsif Port'Address = GPIOB_Base then
         return 1;
      elsif Port'Address = GPIOC_Base then
         return 2;
      elsif Port'Address = GPIOD_Base then
         return 3;
      elsif Port'Address = GPIOE_Base then
         return 4;
      elsif Port'Address = GPIOF_Base then
         return 5;
      elsif Port'Address = GPIOG_Base then
         return 6;
      else
         raise Program_Error;
      end if;
   end GPIO_Port_Representation;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased Analog_To_Digital_Converter)
   is
   begin
      if This'Address = ADC1_Base then
         RCC_Periph.AHB2ENR.ADC12EN := True;
      elsif This'Address = ADC2_Base then
         RCC_Periph.AHB2ENR.ADC12EN := True;
      elsif This'Address = ADC3_Base then
         RCC_Periph.AHB2ENR.ADC345EN := True;
      elsif This'Address = ADC4_Base then
         RCC_Periph.AHB2ENR.ADC345EN := True;
      elsif This'Address = ADC5_Base then
         RCC_Periph.AHB2ENR.ADC345EN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -------------------------
   -- Reset_All_ADC_Units --
   -------------------------

   procedure Reset_All_ADC_Units is
   begin
         RCC_Periph.AHB2RSTR.ADC12RST := True;
         RCC_Periph.AHB2RSTR.ADC12RST := False;
         RCC_Periph.AHB2RSTR.ADC345RST := True;
         RCC_Periph.AHB2RSTR.ADC345RST := False;
   end Reset_All_ADC_Units;

   -------------------------
   -- Select_Clock_Source --
   -------------------------

   procedure Select_Clock_Source (This   : Analog_To_Digital_Converter;
                                  Source : ADC_Clock_Source)
   is
   begin
      if This'Address = ADC1_Base or
        This'Address = ADC2_Base
      then
         RCC_Periph.CCIPR.ADC12SEL := Source'Enum_Rep;
      elsif This'Address = ADC3_Base or
        This'Address = ADC4_Base or
        This'Address = ADC5_Base
      then
         RCC_Periph.CCIPR.ADC345SEL := Source'Enum_Rep;
      else
         raise Unknown_Device;
      end if;
   end Select_Clock_Source;

   -----------------------
   -- Read_Clock_Source --
   -----------------------

   function Read_Clock_Source (This : Analog_To_Digital_Converter)
                               return ADC_Clock_Source
   is
   begin
      if This'Address = ADC1_Base or
        This'Address = ADC2_Base
      then
         return ADC_Clock_Source'Val (RCC_Periph.CCIPR.ADC12SEL);
      elsif This'Address = ADC3_Base or
        This'Address = ADC4_Base or
        This'Address = ADC5_Base
      then
         return ADC_Clock_Source'Val (RCC_Periph.CCIPR.ADC345SEL);
      else
         raise Unknown_Device;
      end if;
   end Read_Clock_Source;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock
     (This : aliased Digital_To_Analog_Converter)
   is
   begin
      if This'Address = DAC1_Base then
         RCC_Periph.AHB2ENR.DAC1EN := True;
      elsif This'Address = DAC2_Base then
         RCC_Periph.AHB2ENR.DAC2EN := True;
      elsif This'Address = DAC3_Base then
         RCC_Periph.AHB2ENR.DAC3EN := True;
      elsif This'Address = DAC4_Base then
         RCC_Periph.AHB2ENR.DAC4EN := True;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased Digital_To_Analog_Converter)
   is
   begin
      if This'Address = DAC1_Base then
         RCC_Periph.AHB2RSTR.DAC1RST := True;
         RCC_Periph.AHB2RSTR.DAC1RST := False;
      elsif This'Address = DAC2_Base then
         RCC_Periph.AHB2RSTR.DAC2RST := True;
         RCC_Periph.AHB2RSTR.DAC2RST := False;
      elsif This'Address = DAC3_Base then
         RCC_Periph.AHB2RSTR.DAC3RST := True;
         RCC_Periph.AHB2RSTR.DAC3RST := False;
      elsif This'Address = DAC4_Base then
         RCC_Periph.AHB2RSTR.DAC4RST := True;
         RCC_Periph.AHB2RSTR.DAC4RST := False;
      end if;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : SAI_Port)
   is
   begin
      pragma Assert (This'Address = SAI_Base);
      RCC_Periph.APB2ENR.SAI1EN := True;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : SAI_Port)
   is
   begin
      pragma Assert (This'Address = SAI_Base);
      RCC_Periph.APB2RSTR.SAI1RST := True;
      RCC_Periph.APB2RSTR.SAI1RST := False;
   end Reset;

   -------------------------
   -- Select_Clock_Source --
   -------------------------

   procedure Select_Clock_Source (This   : SAI_Port;
                                  Source : SAI_Clock_Source)
   is
   begin
      pragma Assert (This'Address = SAI_Base);
      RCC_Periph.CCIPR.SAI1SEL := Source'Enum_Rep;
   end Select_Clock_Source;

   ------------------------
   -- Read_Clock_Source --
   ------------------------

   function Read_Clock_Source (This : SAI_Port) return SAI_Clock_Source
   is
   begin
      pragma Assert (This'Address = SAI_Base);
      return SAI_Clock_Source'Val (RCC_Periph.CCIPR.SAI1SEL);
   end Read_Clock_Source;

   -------------------------
   -- Get_Clock_Frequency --
   -------------------------

   function Get_Clock_Frequency (This : SAI_Port) return UInt32
   is
      Input_Selector  : SAI_Clock_Source;
      VCO_Input       : UInt32;
   begin
      if This'Address /= SAI_Base then
         raise Unknown_Device;
      end if;

      Input_Selector := SAI_Clock_Source'Val (RCC_Periph.CCIPR.SAI1SEL);

      case Input_Selector is
         when SYSCLK =>
            VCO_Input := System_Clock_Frequencies.SYSCLK;
         when PLLQ =>
            VCO_Input := System_Clock_Frequencies.PLLQ;
         when I2S_CKIN =>
            VCO_Input := I2SCLK;
         when HSI16 =>
            VCO_Input := HSI_VALUE;
      end case;

      return VCO_Input;
   end Get_Clock_Frequency;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : CRC_32) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.AHB1ENR.CRCEN := True;
   end Enable_Clock;

   -------------------
   -- Disable_Clock --
   -------------------

   procedure Disable_Clock (This : CRC_32) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.AHB1ENR.CRCEN := False;
   end Disable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : CRC_32) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.AHB1RSTR.CRCRST := True;
      RCC_Periph.AHB1RSTR.CRCRST := False;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : RNG_Generator) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.AHB2ENR.RNGEN := True;
   end Enable_Clock;

   -------------------
   -- Disable_Clock --
   -------------------

   procedure Disable_Clock (This : RNG_Generator) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.AHB2ENR.RNGEN := False;
   end Disable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : RNG_Generator) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.AHB2RSTR.RNGRST := True;
      RCC_Periph.AHB2RSTR.RNGRST := False;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : CORDIC_Coprocessor) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.AHB1ENR.CORDICEN := True;
   end Enable_Clock;

   -------------------
   -- Disable_Clock --
   -------------------

   procedure Disable_Clock (This : CORDIC_Coprocessor) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.AHB1ENR.CORDICEN := False;
   end Disable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : CORDIC_Coprocessor) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.AHB1RSTR.CORDICRST := True;
      RCC_Periph.AHB1RSTR.CORDICRST := False;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : FMAC_Accelerator) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.AHB1ENR.FMACEN := True;
   end Enable_Clock;

   -------------------
   -- Disable_Clock --
   -------------------

   procedure Disable_Clock (This : FMAC_Accelerator) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.AHB1ENR.FMACEN := False;
   end Disable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : FMAC_Accelerator) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.AHB1RSTR.FMACRST := True;
      RCC_Periph.AHB1RSTR.FMACRST := False;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased DMA_Controller) is
   begin
      if This'Address = STM32_SVD.DMA1_Base then
         RCC_Periph.AHB1ENR.DMA1EN := True;
      elsif This'Address = STM32_SVD.DMA2_Base then
         RCC_Periph.AHB1ENR.DMA2EN := True;
      else
         raise Unknown_Device;
      end if;
      RCC_Periph.AHB1ENR.DMAMUXEN := True;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased DMA_Controller) is
   begin
      if This'Address = STM32_SVD.DMA1_Base then
         RCC_Periph.AHB1RSTR.DMA1RST := True;
         RCC_Periph.AHB1RSTR.DMA1RST := False;
      elsif This'Address = STM32_SVD.DMA2_Base then
         RCC_Periph.AHB1RSTR.DMA2RST := True;
         RCC_Periph.AHB1RSTR.DMA2RST := False;
      else
         raise Unknown_Device;
      end if;
      RCC_Periph.AHB1RSTR.DMAMUX1RST := True;
      RCC_Periph.AHB1RSTR.DMAMUX1RST := False;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased USART) is
   begin
      if This.Periph.all'Address = USART1_Base then
         RCC_Periph.APB2ENR.USART1EN := True;
      elsif This.Periph.all'Address = USART2_Base then
         RCC_Periph.APB1ENR1.USART2EN := True;
      elsif This.Periph.all'Address = USART3_Base then
         RCC_Periph.APB1ENR1.USART3EN := True;
      elsif This.Periph.all'Address = UART4_Base then
         RCC_Periph.APB1ENR1.UART4EN := True;
      elsif This.Periph.all'Address = UART5_Base then
         RCC_Periph.APB1ENR1.UART5EN := True;
      elsif This.Periph.all'Address = LPUART1_Base then
         RCC_Periph.APB1ENR2.LPUART1EN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased USART) is
   begin
      if This.Periph.all'Address = USART1_Base then
         RCC_Periph.APB2RSTR.USART1RST := True;
         RCC_Periph.APB2RSTR.USART1RST := False;
      elsif This.Periph.all'Address = USART2_Base then
         RCC_Periph.APB1RSTR1.USART2RST := True;
         RCC_Periph.APB1RSTR1.USART2RST := False;
      elsif This.Periph.all'Address = USART3_Base then
         RCC_Periph.APB1RSTR1.USART3RST := True;
         RCC_Periph.APB1RSTR1.USART3RST := False;
      elsif This.Periph.all'Address = UART4_Base then
         RCC_Periph.APB1RSTR1.UART4RST := True;
         RCC_Periph.APB1RSTR1.UART4RST := False;
      elsif This.Periph.all'Address = UART5_Base then
         RCC_Periph.APB1RSTR1.UART5RST := True;
         RCC_Periph.APB1RSTR1.UART5RST := False;
      elsif This.Periph.all'Address = LPUART1_Base then
         RCC_Periph.APB1RSTR2.LPUART1RST := True;
         RCC_Periph.APB1RSTR2.LPUART1RST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   -------------------------
   -- Select_Clock_Source --
   -------------------------

   procedure Select_Clock_Source (This   : aliased USART;
                                  Source : USART_Clock_Source)
   is
   begin
      if This'Address = USART1_Base then
         RCC_Periph.CCIPR.USART1SEL := Source'Enum_Rep;
      elsif This'Address = USART2_Base then
         RCC_Periph.CCIPR.USART2SEL := Source'Enum_Rep;
      elsif This'Address = USART3_Base then
         RCC_Periph.CCIPR.USART3SEL := Source'Enum_Rep;
      elsif This'Address = UART4_Base then
         RCC_Periph.CCIPR.UART4SEL := Source'Enum_Rep;
      elsif This'Address = UART5_Base then
         RCC_Periph.CCIPR.UART5SEL := Source'Enum_Rep;
      elsif This'Address = LPUART1_Base then
         RCC_Periph.CCIPR.LPUART1SEL := Source'Enum_Rep;
      else
         raise Unknown_Device;
      end if;
   end Select_Clock_Source;

   -----------------------
   -- Read_Clock_Source --
   -----------------------

   function Read_Clock_Source (This : aliased USART)
     return USART_Clock_Source
   is
   begin
      if This'Address = USART1_Base then
         return USART_Clock_Source'Val (RCC_Periph.CCIPR.USART1SEL);
      elsif This'Address = USART2_Base then
         return USART_Clock_Source'Val (RCC_Periph.CCIPR.USART2SEL);
      elsif This'Address = USART3_Base then
         return USART_Clock_Source'Val (RCC_Periph.CCIPR.USART3SEL);
      elsif This'Address = UART4_Base then
         return USART_Clock_Source'Val (RCC_Periph.CCIPR.UART4SEL);
      elsif This'Address = UART5_Base then
         return USART_Clock_Source'Val (RCC_Periph.CCIPR.UART5SEL);
      elsif This'Address = LPUART1_Base then
         return USART_Clock_Source'Val (RCC_Periph.CCIPR.LPUART1SEL);
      else
         raise Unknown_Device;
      end if;
   end Read_Clock_Source;

   -------------------------
   -- Get_Clock_Frequency --
   -------------------------

   function Get_Clock_Frequency (This : USART) return UInt32 is
   begin
      if This.Periph.all'Address = USART1_Base
      then
         return System_Clock_Frequencies.PCLK2;
      else
         return System_Clock_Frequencies.PCLK1;
      end if;
   end Get_Clock_Frequency;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased CAN_Controller)
   is
   begin
      if This'Address = FDCAN1_Base or
         This'Address = FDCAN2_Base or
         This'Address = FDCAN3_Base
      then
         RCC_Periph.APB1ENR1.FDCANEN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased CAN_Controller) is
   begin
      if This'Address = FDCAN1_Base or
         This'Address = FDCAN2_Base or
         This'Address = FDCAN3_Base
      then
         RCC_Periph.APB1RSTR1.FDCANRST := True;
         RCC_Periph.APB1RSTR1.FDCANRST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   -------------------------
   -- Select_Clock_Source --
   -------------------------

   procedure Select_Clock_Source (This   : aliased CAN_Controller;
                                  Source : CAN_Clock_Source)
   is
   begin
      if This'Address = FDCAN1_Base or
         This'Address = FDCAN2_Base or
         This'Address = FDCAN3_Base
      then
         RCC_Periph.CCIPR.FDCANSEL := Source'Enum_Rep;
      else
         raise Unknown_Device;
      end if;
   end Select_Clock_Source;

   -----------------------
   -- Read_Clock_Source --
   -----------------------

   function Read_Clock_Source (This : aliased CAN_Controller)
     return CAN_Clock_Source
   is
   begin
      if This'Address = FDCAN1_Base or
         This'Address = FDCAN2_Base or
         This'Address = FDCAN3_Base
      then
         return CAN_Clock_Source'Val (RCC_Periph.CCIPR.FDCANSEL);
      else
         raise Unknown_Device;
      end if;
   end Read_Clock_Source;

   -------------------------
   -- Get_Clock_Frequency --
   -------------------------

   function Get_Clock_Frequency (This : aliased CAN_Controller)
                                 return UInt32
   is
      Source : constant CAN_Clock_Source :=
        CAN_Clock_Source'Val (RCC_Periph.CCIPR.FDCANSEL);
   begin
      if This'Address = FDCAN1_Base or
        This'Address = FDCAN2_Base or
        This'Address = FDCAN3_Base
      then
         case Source is
            when HSE =>
               return HSE_VALUE;
            when PLLQ =>
               return System_Clock_Frequencies.PLLQ;
            when PCLK =>
               return System_Clock_Frequencies.PCLK1;
         end case;
      else
         raise Unknown_Device;
      end if;
   end Get_Clock_Frequency;

   ----------------
   -- As_Port_Id --
   ----------------

   function As_Port_Id (Port : I2C_Port'Class) return I2C_Port_Id is
   begin
      if Port.Periph.all'Address = I2C1_Base then
         return I2C_Id_1;
      elsif Port.Periph.all'Address = I2C2_Base then
         return I2C_Id_2;
      elsif Port.Periph.all'Address = I2C3_Base then
         return I2C_Id_3;
      elsif Port.Periph.all'Address = I2C4_Base then
         return I2C_Id_4;
      else
         raise Unknown_Device;
      end if;
   end As_Port_Id;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased I2C_Port'Class) is
   begin
      Enable_Clock (As_Port_Id (This));
   end Enable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : I2C_Port_Id) is
   begin
      case This is
         when I2C_Id_1 =>
            RCC_Periph.APB1ENR1.I2C1EN := True;
         when I2C_Id_2 =>
            RCC_Periph.APB1ENR1.I2C2EN := True;
         when I2C_Id_3 =>
            RCC_Periph.APB1ENR1.I2C3EN := True;
         when I2C_Id_4 =>
            RCC_Periph.APB1ENR2.I2C4EN := True;
      end case;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : I2C_Port'Class) is
   begin
      Reset (As_Port_Id (This));
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : I2C_Port_Id) is
   begin
      case This is
         when I2C_Id_1 =>
            RCC_Periph.APB1RSTR1.I2C1RST := True;
            RCC_Periph.APB1RSTR1.I2C1RST := False;
         when I2C_Id_2 =>
            RCC_Periph.APB1RSTR1.I2C2RST := True;
            RCC_Periph.APB1RSTR1.I2C2RST := False;
         when I2C_Id_3 =>
            RCC_Periph.APB1RSTR1.I2C3RST := True;
            RCC_Periph.APB1RSTR1.I2C3RST := False;
         when I2C_Id_4 =>
            RCC_Periph.APB1RSTR2.I2C4RST := True;
            RCC_Periph.APB1RSTR2.I2C4RST := False;
      end case;
   end Reset;

   -------------------------
   -- Select_Clock_Source --
   -------------------------

   procedure Select_Clock_Source (This   : I2C_Port'Class;
                                  Source : I2C_Clock_Source)
   is
   begin
      Select_Clock_Source (As_Port_Id (This), Source);
   end Select_Clock_Source;

   -------------------------
   -- Select_Clock_Source --
   -------------------------

   procedure Select_Clock_Source (This   : I2C_Port_Id;
                                  Source : I2C_Clock_Source)
   is
   begin
      case This is
         when I2C_Id_1 =>
            RCC_Periph.CCIPR.I2C1SEL := Source'Enum_Rep;
         when I2C_Id_2 =>
            RCC_Periph.CCIPR.I2C2SEL := Source'Enum_Rep;
         when I2C_Id_3 =>
            RCC_Periph.CCIPR.I2C3SEL := Source'Enum_Rep;
         when I2C_Id_4 =>
            RCC_Periph.CCIPR2.I2C4SEL := Source'Enum_Rep;
      end case;
   end Select_Clock_Source;

   -----------------------
   -- Read_Clock_Source --
   -----------------------

   function Read_Clock_Source (This : I2C_Port'Class) return I2C_Clock_Source
   is
   begin
      return Read_Clock_Source (As_Port_Id (This));
   end Read_Clock_Source;

   ------------------------
   -- Read_Clock_Source --
   ------------------------

   function Read_Clock_Source (This : I2C_Port_Id) return I2C_Clock_Source
   is
   begin
      case This is
         when I2C_Id_1 =>
            return I2C_Clock_Source'Val (RCC_Periph.CCIPR.I2C1SEL);
         when I2C_Id_2 =>
            return I2C_Clock_Source'Val (RCC_Periph.CCIPR.I2C2SEL);
         when I2C_Id_3 =>
            return I2C_Clock_Source'Val (RCC_Periph.CCIPR.I2C3SEL);
         when I2C_Id_4 =>
            return I2C_Clock_Source'Val (RCC_Periph.CCIPR2.I2C4SEL);
      end case;
   end Read_Clock_Source;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : SPI_Port'Class) is
   begin
      if This.Periph.all'Address = SPI1_Base then
         RCC_Periph.APB2ENR.SPI1EN := True;
      elsif This.Periph.all'Address = SPI2_Base then
         RCC_Periph.APB1ENR1.SPI2EN := True;
      elsif This.Periph.all'Address = SPI3_Base then
         RCC_Periph.APB1ENR1.SPI3EN := True;
      elsif This.Periph.all'Address = SPI4_Base then
         RCC_Periph.APB2ENR.SPI4EN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : SPI_Port'Class) is
   begin
      if This.Periph.all'Address = SPI1_Base then
         RCC_Periph.APB2RSTR.SPI1RST := True;
         RCC_Periph.APB2RSTR.SPI1RST := False;
      elsif This.Periph.all'Address = SPI2_Base then
         RCC_Periph.APB1RSTR1.SPI2RST := True;
         RCC_Periph.APB1RSTR1.SPI2RST := False;
      elsif This.Periph.all'Address = SPI3_Base then
         RCC_Periph.APB1RSTR1.SPI3RST := True;
         RCC_Periph.APB1RSTR1.SPI3RST := False;
      elsif This.Periph.all'Address = SPI4_Base then
         RCC_Periph.APB2RSTR.SPI4RST := True;
         RCC_Periph.APB2RSTR.SPI4RST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   -------------------------
   -- Select_Clock_Source --
   -------------------------

   procedure Select_Clock_Source (This   : SPI_Port'Class;
                                  Source : SPI_Clock_Source)
   is
   begin
      if This.Periph.all'Address = SPI1_Base or
         This.Periph.all'Address = SPI4_Base
      then
         null;
      elsif  This.Periph.all'Address = SPI2_Base or
             This.Periph.all'Address = SPI3_Base
      then
         RCC_Periph.CCIPR.I2S23SEL := Source'Enum_Rep;
      else
         raise Unknown_Device;
      end if;
   end Select_Clock_Source;

   ------------------------
   -- Read_Clock_Source --
   ------------------------

   function Read_Clock_Source (This : SPI_Port'Class) return SPI_Clock_Source
   is
   begin
      if This.Periph.all'Address = SPI2_Base or
         This.Periph.all'Address = SPI3_Base
      then
         return SPI_Clock_Source'Val (RCC_Periph.CCIPR.I2S23SEL);
      else
         raise Unknown_Device;
      end if;
   end Read_Clock_Source;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : I2S_Port) is
   begin
      if This.Periph.all'Address = SPI2_Base then
         RCC_Periph.APB1ENR1.SPI2EN := True;
      elsif This.Periph.all'Address = SPI3_Base then
         RCC_Periph.APB1ENR1.SPI3EN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : I2S_Port) is
   begin
      if This.Periph.all'Address = SPI2_Base then
         RCC_Periph.APB1RSTR1.SPI2RST := True;
         RCC_Periph.APB1RSTR1.SPI2RST := False;
      elsif This.Periph.all'Address = SPI3_Base then
         RCC_Periph.APB1RSTR1.SPI3RST := True;
         RCC_Periph.APB1RSTR1.SPI3RST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   -------------------------
   -- Select_Clock_Source --
   -------------------------

   procedure Select_Clock_Source (This   : I2S_Port'Class;
                                  Source : I2S_Clock_Source)
   is
   begin
      if This.Periph.all'Address = SPI2_Base or
        This.Periph.all'Address = SPI3_Base
      then
         RCC_Periph.CCIPR.I2S23SEL := Source'Enum_Rep;
      else
         raise Unknown_Device;
      end if;
   end Select_Clock_Source;

   ------------------------
   -- Read_Clock_Source --
   ------------------------

   function Read_Clock_Source (This : I2S_Port'Class) return I2S_Clock_Source
   is
   begin
      if This.Periph.all'Address = SPI2_Base or
        This.Periph.all'Address = SPI3_Base
      then
         return I2S_Clock_Source'Val (RCC_Periph.CCIPR.I2S23SEL);
      else
         raise Unknown_Device;
      end if;
   end Read_Clock_Source;

   -------------------------
   -- Get_Clock_Frequency --
   -------------------------

   function Get_Clock_Frequency (This : I2S_Port) return UInt32 is
      Source : constant I2S_Clock_Source :=
        I2S_Clock_Source'Val (RCC_Periph.CCIPR.I2S23SEL);
   begin
      if This.Periph.all'Address = SPI2_Base or
        This.Periph.all'Address = SPI3_Base
      then
         case Source is
            when SYSCLK =>
               return System_Clock_Frequencies.SYSCLK;
            when PLLQ =>
               return System_Clock_Frequencies.PLLQ;
            when I2S_CKIN =>
               return I2SCLK;
            when HSI16 =>
               return HSI_VALUE;
         end case;

      else
         raise Unknown_Device;
      end if;
   end Get_Clock_Frequency;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : RTC_Device) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.BDCR.RTCEN := True;
   end Enable_Clock;

   -------------------------
   -- Select_Clock_Source --
   -------------------------

   procedure Select_Clock_Source
     (This       : RTC_Device;
      Source     : RTC_Clock_Source)
   is
   begin
      RCC_Periph.BDCR.RTCSEL := Source'Enum_Rep;
   end Select_Clock_Source;

   ------------------------
   -- Read_Clock_Source --
   ------------------------

   function Read_Clock_Source (This : RTC_Device) return RTC_Clock_Source
   is
      pragma Unreferenced (This);
   begin
      return RTC_Clock_Source'Val (RCC_Periph.BDCR.RTCSEL);
   end Read_Clock_Source;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : Timer) is
   begin
      if This'Address = TIM1_Base then
         RCC_Periph.APB2ENR.TIM1EN := True;
      elsif This'Address = TIM2_Base then
         RCC_Periph.APB1ENR1.TIM2EN := True;
      elsif This'Address = TIM3_Base then
         RCC_Periph.APB1ENR1.TIM3EN := True;
      elsif This'Address = TIM4_Base then
         RCC_Periph.APB1ENR1.TIM4EN := True;
      elsif This'Address = TIM5_Base then
         RCC_Periph.APB1ENR1.TIM5EN := True;
      elsif This'Address = TIM6_Base then
         RCC_Periph.APB1ENR1.TIM6EN := True;
      elsif This'Address = TIM7_Base then
         RCC_Periph.APB1ENR1.TIM7EN := True;
      elsif This'Address = TIM8_Base then
         RCC_Periph.APB2ENR.TIM8EN := True;
      elsif This'Address = TIM15_Base then
         RCC_Periph.APB2ENR.TIM15EN := True;
      elsif This'Address = TIM16_Base then
         RCC_Periph.APB2ENR.TIM16EN := True;
      elsif This'Address = TIM17_Base then
         RCC_Periph.APB2ENR.TIM17EN := True;
      elsif This'Address = TIM20_Base then
         RCC_Periph.APB2ENR.TIM20EN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : Timer) is
   begin
      if This'Address = TIM1_Base then
         RCC_Periph.APB2RSTR.TIM1RST := True;
         RCC_Periph.APB2RSTR.TIM1RST := False;
      elsif This'Address = TIM2_Base then
         RCC_Periph.APB1RSTR1.TIM2RST := True;
         RCC_Periph.APB1RSTR1.TIM2RST := False;
      elsif This'Address = TIM3_Base then
         RCC_Periph.APB1RSTR1.TIM3RST := True;
         RCC_Periph.APB1RSTR1.TIM3RST := False;
      elsif This'Address = TIM4_Base then
         RCC_Periph.APB1RSTR1.TIM4RST := True;
         RCC_Periph.APB1RSTR1.TIM4RST := False;
      elsif This'Address = TIM5_Base then
         RCC_Periph.APB1RSTR1.TIM5RST := True;
         RCC_Periph.APB1RSTR1.TIM5RST := False;
      elsif This'Address = TIM6_Base then
         RCC_Periph.APB1RSTR1.TIM6RST := True;
         RCC_Periph.APB1RSTR1.TIM6RST := False;
      elsif This'Address = TIM7_Base then
         RCC_Periph.APB1RSTR1.TIM7RST := True;
         RCC_Periph.APB1RSTR1.TIM7RST := False;
      elsif This'Address = TIM8_Base then
         RCC_Periph.APB2RSTR.TIM8RST := True;
         RCC_Periph.APB2RSTR.TIM8RST := False;
      elsif This'Address = TIM15_Base then
         RCC_Periph.APB2RSTR.TIM15RST := True;
         RCC_Periph.APB2RSTR.TIM15RST := False;
      elsif This'Address = TIM16_Base then
         RCC_Periph.APB2RSTR.TIM16RST := True;
         RCC_Periph.APB2RSTR.TIM16RST := False;
      elsif This'Address = TIM17_Base then
         RCC_Periph.APB2RSTR.TIM17RST := True;
         RCC_Periph.APB2RSTR.TIM17RST := False;
      elsif This'Address = TIM20_Base then
         RCC_Periph.APB2RSTR.TIM20RST := True;
         RCC_Periph.APB2RSTR.TIM20RST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   -------------------------
   -- Get_Clock_Frequency --
   -------------------------

   function Get_Clock_Frequency (This : Timer) return UInt32 is
   begin
      --  TIMs 2 .. 7
      if This'Address = TIM2_Base or
        This'Address = TIM3_Base or
        This'Address = TIM4_Base or
        This'Address = TIM5_Base or
        This'Address = TIM6_Base or
        This'Address = TIM7_Base
      then
         return System_Clock_Frequencies.TIMCLK1;

      --  TIMs 1, 8, 20, 15 .. 17
      elsif This'Address = TIM1_Base or
        This'Address = TIM8_Base or
        This'Address = TIM15_Base or
        This'Address = TIM16_Base or
        This'Address = TIM17_Base or
        This'Address = TIM20_Base
      then
         return System_Clock_Frequencies.TIMCLK2;
      else
         raise Unknown_Device;
      end if;
   end Get_Clock_Frequency;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : LPTimer) is
   begin
      if This'Address = LPTIMER1_Base then
         RCC_Periph.APB1ENR1.LPTIM1EN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : LPTimer) is
   begin
      if This'Address = LPTIMER1_Base then
         RCC_Periph.APB1RSTR1.LPTIM1RST := True;
         RCC_Periph.APB1RSTR1.LPTIM1RST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   -------------------------
   -- Select_Clock_Source --
   -------------------------

   procedure Select_Clock_Source (This   : LPTimer;
                                  Source : LPTimer_Clock_Source)
   is
   begin
      if This'Address = LPTIMER1_Base then
         LPTIMER1_Periph.CFGR.CKSEL := Source.External;
         RCC_Periph.CCIPR.LPTIM1SEL := Source.Clock'Enum_Rep;
      else
         raise Unknown_Device;
      end if;
   end Select_Clock_Source;

   -----------------------
   -- Read_Clock_Source --
   -----------------------

   function Read_Clock_Source (This : LPTimer) return LPTimer_Clock_Source is
      Source : LPTimer_Clock_Source;
   begin
      if This'Address = LPTIMER1_Base then
         Source.External := LPTIMER1_Periph.CFGR.CKSEL;
         Source.Clock := LPTimer_Clock_Source_Enum'Val (RCC_Periph.CCIPR.LPTIM1SEL);
         return Source;
      else
         raise Unknown_Device;
      end if;
   end Read_Clock_Source;

   -------------------------
   -- Get_Clock_Frequency --
   -------------------------

   function Get_Clock_Frequency (This : LPTimer) return UInt32 is
   begin
      --  LPTIMs 1
      if This'Address = LPTIMER1_Base then
         return System_Clock_Frequencies.TIMCLK3;
      else
         raise Unknown_Device;
      end if;
   end Get_Clock_Frequency;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : HRTimer_Master) is
   begin
      if This'Address = HRTIM_Master_Base then
         RCC_Periph.APB2ENR.HRTIM1EN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : HRTimer_Channel) is
   begin
      if This'Address = HRTIM_TIMA_Base or
         This'Address = HRTIM_TIMB_Base or
         This'Address = HRTIM_TIMC_Base or
         This'Address = HRTIM_TIMD_Base or
         This'Address = HRTIM_TIME_Base or
         This'Address = HRTIM_TIMF_Base
      then
         RCC_Periph.APB2ENR.HRTIM1EN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : HRTimer_Master) is
   begin
      if This'Address = HRTIM_Master_Base then
         RCC_Periph.APB2RSTR.HRTIM1RST := True;
         RCC_Periph.APB2RSTR.HRTIM1RST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : HRTimer_Channel) is
   begin
      if This'Address = HRTIM_TIMA_Base or
         This'Address = HRTIM_TIMB_Base or
         This'Address = HRTIM_TIMC_Base or
         This'Address = HRTIM_TIMD_Base or
         This'Address = HRTIM_TIME_Base or
         This'Address = HRTIM_TIMF_Base
      then
         RCC_Periph.APB2RSTR.HRTIM1RST := True;
         RCC_Periph.APB2RSTR.HRTIM1RST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   -------------------------
   -- Get_Clock_Frequency --
   -------------------------

   function Get_Clock_Frequency (This : HRTimer_Master) return UInt32 is
      pragma Unreferenced (This);
   begin
      return System_Clock_Frequencies.TIMCLK2;
   end Get_Clock_Frequency;

   -------------------------
   -- Get_Clock_Frequency --
   -------------------------

   function Get_Clock_Frequency (This : HRTimer_Channel) return UInt32 is
      pragma Unreferenced (This);
   begin
      return System_Clock_Frequencies.TIMCLK2;
   end Get_Clock_Frequency;

   ------------------------------
   -- System_Clock_Frequencies --
   ------------------------------

   function System_Clock_Frequencies return RCC_System_Clocks
   is
      Source : constant SYSCLK_Clock_Source :=
        SYSCLK_Clock_Source'Enum_Val (RCC_Periph.CFGR.SWS);
      --  Get System_Clock_Mux selection

      Result : RCC_System_Clocks;

      PLLSRC : constant PLL_Clock_Source :=
        PLL_Clock_Source'Val (RCC_Periph.PLLCFGR.PLLSRC);
      --  PLL Source Mux input.
      PLL_Clock_In : UInt32;

      Pllm   : constant UInt32 := UInt32 (RCC_Periph.PLLCFGR.PLLM + 1);
      --  Get the value of Pll M divisor.
      Plln   : constant UInt32 := UInt32 (RCC_Periph.PLLCFGR.PLLN);
      --  Get the value of Pll N multiplier.
      Pllp   : constant UInt32 := UInt32 (RCC_Periph.PLLCFGR.PLLPDIV);
      --  Get the value of Pll P divisor.
      Pllq   : UInt32 := UInt32 (RCC_Periph.PLLCFGR.PLLQ);
      --  Get the value of Pll Q divisor.
      Pllr   : UInt32 := UInt32 (RCC_Periph.PLLCFGR.PLLR);
      --  Get the value of Pll R divisor.
   begin
      --  Get the correct value of Pll Q divisor.
      Pllq := (Pllq + 1) * 2;
      --  Get the correct value of Pll R divisor.
      Pllr := (Pllr + 1) * 2;

      --  PLL Source Mux
      case PLLSRC is
         when PLL_SRC_HSE => --  HSE as PLL source
            PLL_Clock_In := HSE_VALUE;
         when PLL_SRC_HSI => --  HSI as PLL source
            PLL_Clock_In := HSI_VALUE;
         when others => --  No source
            PLL_Clock_In := 0;
      end case;

      --  PLL clock output frequencies
      --  PLLR is PLLCLK that is used for SYSCLK, see below.
      --  When PLLPDIV is set to zero, the division factor of the PLLP
      --  output is defined by the PLLP bit (0 = 7, 1 = 17).
      if Pllp = 16#00# then
         if RCC_Periph.PLLCFGR.PLLP then
            Result.PLLP := (PLL_Clock_In / Pllm * Plln) / 17;
         else
            Result.PLLP := (PLL_Clock_In / Pllm * Plln) / 7;
         end if;
      else
         Result.PLLP := (PLL_Clock_In / Pllm * Plln) / Pllp;
      end if;

      Result.PLLQ := (PLL_Clock_In / Pllm * Plln) / Pllq;

      case Source is
         --  HSI as source
         when SYSCLK_SRC_HSI =>
            Result.SYSCLK := HSI_VALUE;
         --  HSE as source
         when SYSCLK_SRC_HSE =>
            Result.SYSCLK := HSE_VALUE;
         --  PLL as source = PLLCLK
         when SYSCLK_SRC_PLL =>
            --  PLLR output to System Clock Mux (PLLCLK) that defines SYSCLK.
            Result.SYSCLK := (PLL_Clock_In / Pllm * Plln) / Pllr;
      end case;

      declare
         HPRE  : constant UInt4 := RCC_Periph.CFGR.HPRE;
         PPRE1 : constant UInt3 := RCC_Periph.CFGR.PPRE.Arr (1);
         PPRE2 : constant UInt3 := RCC_Periph.CFGR.PPRE.Arr (2);
      begin
         Result.HCLK  := Result.SYSCLK / HPRE_Presc_Table (HPRE);
         Result.PCLK1 := Result.HCLK / PPRE_Presc_Table (PPRE1);
         Result.PCLK2 := Result.HCLK / PPRE_Presc_Table (PPRE2);

         --  Timer clocks
         --  If the APB prescaler (PPRE1, PPRE2 in the RCC_CFGR register)
         --  is configured to a division factor of 1, TIMxCLK = PCLKx.
         --  Otherwise, the timer clock frequencies are set to twice to the
         --  frequency of the APB domain to which the timers are connected:
         --  TIMxCLK = 2 x PCLKx.

         --  TIMs 2 .. 7
         if PPRE_Presc_Table (PPRE1) = 1 then
            Result.TIMCLK1 := Result.PCLK1;
         else
            Result.TIMCLK1 := Result.PCLK1 * 2;
         end if;

         --  TIMs 1, 8, 20, 15 .. 17, HRTIM1
         if PPRE_Presc_Table (PPRE2) = 1 then
            Result.TIMCLK2 := Result.PCLK2;
         else
            Result.TIMCLK2 := Result.PCLK2 * 2;
         end if;

         --  LPTIMs 1 .. 2
         case RCC_Periph.CCIPR.LPTIM1SEL is
            when 0 =>
               Result.TIMCLK3 := Result.PCLK1;
            when 1 =>
               Result.TIMCLK3 := LSI_VALUE;
            when 2 =>
               Result.TIMCLK3 := HSI_VALUE;
            when 3 =>
               Result.TIMCLK3 := LSE_VALUE;
         end case;
      end;

      return Result;
   end System_Clock_Frequencies;

end STM32.Device;
