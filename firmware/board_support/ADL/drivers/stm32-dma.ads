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
--   @file    stm32f4xx_hal_dma.h                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   Header file of DMA HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides definitions for the DMA controllers on the STM32F4 (ARM
--  Cortex M4F) microcontrollers from ST Microelectronics.

--  See Application Note AN4031: "Using the STM32F2 and STM32F4 DMA controller"
--  and Reference Manual RM0090: "STM32F405xx/07xx, STM32F415xx/17xx,
--  STM32F42xxx and STM32F43xxx advanced ARM-based 32-bit MCUs" In the
--  application note, see especially section four, titled "Tips and
--  warnings while programming the DMA controller"

--  The basic call sequence, given a Controller and a Stream, is as follows:

--  1) Configure

--     Configures the Controller and Stream per application requirements. This
--     is the primary setup call, specifying the static characteristics of all
--     the transfers to be performed on the stream, such as the direction, the
--     channel, and so forth. The Controller is disabled after the call.

--  2) Configure_Data_Flow

--     Sets the dynamic parameters of a given transfer, i.e., the source and
--     destination addresses and the number of data items to transfer.

--  3) Enable

--     Enables transfers on the Controller and Stream. Transfers will begin
--     immediately unless programmed otherwise.

--  You can enable some or all DMA interrupts prior to the call to Enable, if
--  required by your usage.

--  Ensure all the status flags are cleared prior to the call to Enable, since
--  a transfer will then begin. This can be accomplished by relying on the fact
--  that the board has just powered-up, by a call to Reset, or by a call to
--  Clear_All_Status.

--  Note that there are convenience routines that do steps two and three:
--     Start_Transfer
--     Start_Transfer_with_Interrupts

pragma Restrictions (No_Elaboration_Code);

with System;         use System;
with Ada.Real_Time;  use Ada.Real_Time;

private with STM32_SVD.DMA;

package STM32.DMA with SPARK_Mode => Off is

   type DMA_Controller is limited private;

   --  Do not change the order of the enumerals in the types in this package.
   --  The underlying canonical representation values are required.

   type DMA_Stream_Selector is
     (Stream_1,
      Stream_2,
      Stream_3,
      Stream_4,
      Stream_5,
      Stream_6,
      Stream_7,
      Stream_8);

   procedure Enable
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
     with Inline;
   --  Before enabling a stream to start a new transfer, the event status flags
   --  corresponding to the stream must be cleared. Note that the unit may not
   --  be enabled by the time the call returns.

   function Enabled
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Boolean with Inline;

   procedure Disable
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
     with
       Post => not Enabled (This, Stream),
       Inline;

   procedure Reset
     (This   : in out DMA_Controller;
      Stream : DMA_Stream_Selector)
     with
       Post =>
         not Enabled (This, Stream)                               and
         Operating_Mode (This, Stream) = Normal_Mode              and
         Current_Items_Number (This, Stream) = 0                  and
         Selected_Channel (This, Stream) = No_Selection           and
         Transfer_Direction (This, Stream) = Peripheral_To_Memory and
         not Circular_Mode (This, Stream)                         and
         Memory_Data_Width (This, Stream) = Bytes                 and
         Peripheral_Data_Width (This, Stream) = Bytes             and
         Priority (This, Stream) = Priority_Low                   and
         (for all Flag in DMA_Status_Flag =>
            not Status (This, Stream, Flag))                      and
         (for all Interrupt in DMA_Interrupt =>
            not Interrupt_Enabled (This, Stream, Interrupt));
       --  In addition,
       --  M_Burst = Memory_Burst_Single and
       --  P_Burst = Peripheral_Burst_Single and
       --  P_Inc_Offset_Size = 0 and
       --  M_Inc_Mode = False and
       --  P_Inc_Mode = False
   --  Also clears the FIFO control register bits except sets bits to show FIFO
   --  is empty, and to set the FIFO filling threshold selection to 1/2 full.

   procedure Configure_Data_Flow
     (This        : DMA_Controller;
      Stream      : DMA_Stream_Selector;
      Source      : Address;
      Destination : Address;
      Data_Count  : UInt16)
     with
       Pre =>
         not Enabled (This, Stream) and
         Valid_Addresses (Source, Destination) and
         Compatible_Alignments (This, Stream, Source, Destination);
   --  Sets the source and destination arguments within the specified stream,
   --  based on the direction previously specified by a call to procedure
   --  Configure.
   --
   --  Sets the number of data items to be transferred (from 0 to 65535) on
   --  the specified stream in the next transfer. This is the volume of data to
   --  be transferred from source to destination. The number specified depends
   --  only on the peripheral data format, as specified by the record component
   --  Peripheral_Data_Format passed to a call to Configure. The value to be
   --  specified is computed as follows:
   --
   --     If the peripheral data format is in units of bytes, the value is
   --     equal to the total number of bytes contained in the data to be sent.
   --
   --     If the peripheral data format is in units of half-words, the value is
   --     1/2 the total number of bytes contained in the data to be sent.
   --
   --     If the peripheral data format is in units of words, the value is
   --     1/4 the total number of bytes contained in the data to be sent.
   --
   --  For example, to send a sequence of characters to a USART, the USART
   --  peripheral format will be in units of bytes so the Data_Count argument
   --  will be the number of characters (bytes) in the string to be sent.
   --  In contrast, on a memory-to-memory transfer the most efficient approach
   --  is to work in units of words. One would therefore specify word units for
   --  the source and destination formats and then specify 1/4 the total number
   --  of bytes involved (assuming a four-UInt8 word).

   procedure Start_Transfer
     (This        : DMA_Controller;
      Stream      : DMA_Stream_Selector;
      Source      : Address;
      Destination : Address;
      Data_Count  : UInt16)
     with
       Pre  =>
         Valid_Addresses (Source, Destination) and
         Compatible_Alignments (This, Stream, Source, Destination) and
         (for all Flag in DMA_Status_Flag =>
             (not Status (This, Stream, Flag)));
   --  Convenience routine: disables the stream, calls Configure_Data_Flow,
   --  and then enables the stream to start the transfer. DMA interrupts are
   --  not enabled by this routine, but could be enabled prior to the call.
   --  The requirement to clear the flags first is due to the fact that
   --  the transfer begins immediately at the end of this routine. The
   --  value specified for Data_Count is as described for procedure
   --  Configure_Data_Flow.

   type DMA_Interrupt is
     (Transfer_Error_Interrupt,
      Half_Transfer_Complete_Interrupt,
      Transfer_Complete_Interrupt,
      Global_Interrupt);

   procedure Enable_Interrupt
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Source : DMA_Interrupt)
     with
       Post => Interrupt_Enabled (This, Stream, Source);
   --  The postcondition should not be relied upon completely because it is
   --  possible, under just the wrong conditions, for the interrupt to be
   --  disabled immediately, prior to return from this routine

   function Interrupt_Enabled
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Source : DMA_Interrupt)
      return Boolean
     with Inline;

   procedure Disable_Interrupt
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Source : DMA_Interrupt)
     with
       Post => not Interrupt_Enabled (This, Stream, Source);

   type Interrupt_Selections is array (DMA_Interrupt) of Boolean;

   procedure Start_Transfer_with_Interrupts
     (This               : DMA_Controller;
      Stream             : DMA_Stream_Selector;
      Source             : Address;
      Destination        : Address;
      Data_Count         : UInt16;
      Enabled_Interrupts : Interrupt_Selections := (others => True))
     with
       Pre =>
          Valid_Addresses (Source, Destination) and
          Compatible_Alignments (This, Stream, Source, Destination) and
          (for all Flag in DMA_Status_Flag =>
              (not Status (This, Stream, Flag)));
   --  Convenience routine: disables the stream, calls Configure_Data_Flow,
   --  enables the selected DMA interrupts (by default, all of them), and
   --  then enables the stream to start the transfer. All the selected DMA
   --  interrupts are enabled, all the others are left unchanged. Interrupts
   --  are selected for enablement by having a True value in the array at their
   --  index location. The requirement to clear the flags first is due to the
   --  fact that the transfer begins immediately at the end of this routine.
   --  The value specified for Data_Count is as described for procedure
   --  Configure_Data_Flow.

   type DMA_Error_Code is
     (DMA_No_Error,
      DMA_Transfer_Error,
      DMA_Timeout_Error,
      DMA_Device_Error);

   procedure Abort_Transfer
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Result : out DMA_Error_Code)
     with Post => not Enabled (This, Stream);
   --  Disables the specified stream and then waits until the request is
   --  effective. If a stream is disabled while a data transfer is ongoing, the
   --  current datum will be transferred and the stream will be disabled only
   --  after the transfer of this single datum completes.

   type DMA_Transfer_Level is
     (Full_Transfer,
      Half_Transfer);

   procedure Poll_For_Completion
     (This           : in out DMA_Controller;
      Stream         : DMA_Stream_Selector;
      Expected_Level : DMA_Transfer_Level;
      Timeout        : Time_Span;
      Result         : out DMA_Error_Code);

   type DMA_Status_Flag is
     (Transfer_Error_Indicated,
      Half_Transfer_Complete_Indicated,
      Transfer_Complete_Indicated,
      Global_Event_Indicated);

   function Status
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Flag   : DMA_Status_Flag)
      return Boolean
     with Inline;
   --  Returns whether the specified status flag is indicated

   procedure Clear_Status
     (This   : in out DMA_Controller;
      Stream : DMA_Stream_Selector;
      Flag   : DMA_Status_Flag)
     with
       Post => not Status (This, Stream, Flag),
       Inline;

   procedure Clear_All_Status
     (This   : in out DMA_Controller;
      Stream : DMA_Stream_Selector)
     with Post => (for all Indicated in DMA_Status_Flag =>
                     not Status (This, Stream, Indicated));

   procedure Set_Items_Number
     (This       : DMA_Controller;
      Stream     : DMA_Stream_Selector;
      Data_Count : UInt16)
     with
       Pre  => not Enabled (This, Stream),
       Post => Current_Items_Number (This, Stream) = Data_Count,
       Inline;
   --  Sets the number of data items to be transferred on the stream.
   --  The Data_Count parameter specifies the number of data items to be
   --  transferred (from 0 to 65535) on the next transfer. The value is
   --  as described for procedure Configure_Data_Flow.

   function Items_Transferred
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return UInt16;
   --  returns the number of items transfetred

   function Current_Items_Number
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return UInt16
     with Inline;
   --  Returns the value of the NDT register. Should not be used directly,
   --  as the meaning changes depending on transfer mode. rather use
   --  Items_Transferred()

   function Circular_Mode
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Boolean
     with Inline;

   type DMA_Channel_Selector is
     (No_Selection,
      DMAMUX_Req_G0, DMAMUX_Req_G1, DMAMUX_Req_G2, DMAMUX_Req_G3,
      ADC1,
      DAC1_CH1, DAC1_CH2,
      TIM6_UP, TIM7_UP,
      SPI1_RX, SPI1_TX, SPI2_RX, SPI2_TX, SPI3_RX, SPI3_TX,
      I2C1_RX, I2C1_TX, I2C2_RX, I2C2_TX, I2C3_RX, I2C3_TX, I2C4_RX, I2C4_TX,
      USART1_RX, USART1_TX, USART2_RX, USART2_TX, USART3_RX, USART3_TX,
      UART4_RX, UART4_TX, UART5_RX, UART5_TX,
      LPUART1_RX, LPUART1_TX,
      ADC2, ADC3, ADC4, ADC5,
      QUADSPI,
      DAC2_CH1,
      TIM1_CH1, TIM1_CH2, TIM1_CH3, TIM1_CH4, TIM1_UP, TIM1_TRIG, TIM1_COM,
      TIM8_CH1, TIM8_CH2, TIM8_CH3, TIM8_CH4, TIM8_UP, TIM8_TRIG, TIM8_COM,
      TIM2_CH1, TIM2_CH2, TIM2_CH3, TIM2_CH4, TIM2_UP,
      TIM3_CH1, TIM3_CH2, TIM3_CH3, TIM3_CH4, TIM3_UP, TIM3_TRIG,
      TIM4_CH1, TIM4_CH2, TIM4_CH3, TIM4_CH4, TIM4_UP,
      TIM5_CH1, TIM5_CH2, TIM5_CH3, TIM5_CH4, TIM5_UP, TIM5_TRIG,
      TIM15_CH1, TIM15_UP, TIM15_TRIG, TIM15_COM,
      TIM16_CH1, TIM16_UP,
      TIM17_CH1, TIM17_UP,
      TIM20_CH1, TIM20_CH2, TIM20_CH3, TIM20_CH4, TIM20_UP,
      AES_IN, AES_OUT,
      TIM20_TRIG, TIM20_COM,
      HRTIM_MASTER,
      HRTIM_TIMA, HRTIM_TIMB, HRTIM_TIMC, HRTIM_TIMD, HRTIM_TIME, HRTIM_TIMF,
      DAC3_CH1, DAC3_CH2,
      DAC4_CH1, DAC4_CH2,
      SPI4_RX, SPI4_TX,
      SAI1_A, SAI1_B,
      FMAC_Read, FMAC_Write,
      Cordic_Read, Cordic_Write,
      UCPD1_RX, UCPD1_TX);

   function Selected_Channel
     (This : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Channel_Selector
     with Inline;

   type DMA_Data_Transfer_Direction is
     (Peripheral_To_Memory,
      Memory_To_Peripheral,
      Memory_To_Memory,
      Peripheral_To_Peripheral);

   function Transfer_Direction
     (This : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Data_Transfer_Direction
     with Inline;
   --  The memory-to-memory mode must not be used in circular mode. Before
   --  enabling a channel in memory-to-memory mode (MEM2MEM = 1), the software
   --  must clear the CIRC bit of the DMA_CCRx register.
   --  See RM0440 rev 6 Chapter 12.4.3 Section "Memory-to-memory mode" pg 411.

   type DMA_Data_Transfer_Widths is
     (Bytes,     --  8 bits
      HalfWords, --  16 bits
      Words);    --  32 bits

   function Peripheral_Data_Width
     (This : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Data_Transfer_Widths
     with Inline;

   function Memory_Data_Width
     (This : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Data_Transfer_Widths
     with Inline;

   type DMA_Mode is
     (Normal_Mode,
      Peripheral_Flow_Control_Mode,
      Circular_Mode);

   function Operating_Mode
     (This : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Mode
     with Inline;

   type DMA_Priority_Level is
     (Priority_Low,
      Priority_Medium,
      Priority_High,
      Priority_Very_High);

   function Priority
     (This : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Priority_Level
     with Inline;

   type DMA_Memory_Burst is
     (Memory_Burst_Single,
      Memory_Burst_Inc4,
      Memory_Burst_Inc8,
      Memory_Burst_Inc16);

   type DMA_Peripheral_Burst is
     (Peripheral_Burst_Single,
      Peripheral_Burst_Inc4,
      Peripheral_Burst_Inc8,
      Peripheral_Burst_Inc16);

   type DMA_Stream_Configuration is record
      --  These are the static, non-varying properties of the transactions
      --  occurring on the streams to which they are applied (by a call to
      --  Configure). Other, varying, properties are specified procedurally.
      --
      --  You are not required to specify a value for every component because
      --  some are only referenced depending on the values for others. Note,
      --  however, that the default values specified do not represent a valid
      --  configuration as a whole.

      Channel : DMA_Channel_Selector := DMA_Channel_Selector'First;
      --  The channel in the multiplexed connections of controllers, streams,
      --  and peripherals. It is vital to note that not all peripherals can
      --  be connected to all streams. The possibilities are organized by
      --  channels, per controller, as specified by the ST Micro Reference
      --  Manual RM0440 rev 6 Chapter 12.3.2 "DMA Request Mapping" tables.

      Direction : DMA_Data_Transfer_Direction := DMA_Data_Transfer_Direction'First;

      Increment_Peripheral_Address : Boolean := False;
      --  Whether the peripheral address value should be incremented
      --  automatically after each transfer

      Increment_Memory_Address : Boolean := False;
      --  Whether the memory address value should be incremented automatically
      --  after each transfer

      Peripheral_Data_Format : DMA_Data_Transfer_Widths := DMA_Data_Transfer_Widths'First;
      --  The units of data (the format) in which the peripheral side of the
      --  transaction is expressed. For example, a USART would work in terms
      --  of bytes. See the description in Configure_Data_Flow.

      Memory_Data_Format : DMA_Data_Transfer_Widths := DMA_Data_Transfer_Widths'First;
      --  The units of data (the format) in which the memory side of the
      --  transaction is expressed. See the description in Configure_Data_Flow.

      Operation_Mode : DMA_Mode := DMA_Mode'First;
      --  The circular mode must not be used in memory-to-memory mode. Before
      --  enabling a channel in circular mode (CIRC = 1), the software must
      --  clear the MEM2MEM bit of the DMA_CCRx register.
      --  See RM0440 rev 6 Chapter 12.4.3 Section "Circular mode" pg 410.

      Priority : DMA_Priority_Level := DMA_Priority_Level'First;
      --  The relative priority of the given stream to all other streams

      Memory_Burst_Size : DMA_Memory_Burst := DMA_Memory_Burst'First;
      --  Specifies the amount of data to be transferred in a single non-
      --  interruptible transaction. Note: The burst mode is possible only if
      --  the address increment mode is enabled.

      Peripheral_Burst_Size : DMA_Peripheral_Burst := DMA_Peripheral_Burst'First;
      --  Specifies the the amount of data to be transferred in
      --  a single non-interruptible transaction. Note: The burst mode is
      --  possible only if the address increment mode is enabled.
   end record;

   procedure Configure
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Config : DMA_Stream_Configuration)
     with Post => not Enabled (This, Stream);
   --  This is the primary stream configuration facility. All the static
   --  properties of the transfers for the given stream are specified here,
   --  and in some cases, nowhere else (such as the channel). The required
   --  relationships between the parameters specified in the record are
   --  not checked, other than by the hardware itself.
   --
   --  Note that not all required properties are specified here. In particular,
   --  because they can vary per transfer, the source and destination
   --  addresses, as well as the number of data items to be transferred,
   --  are specified procedurally via calls to Configure_Data_Flow.

   function Valid_Addresses (Source, Destination : Address) return Boolean is
     (Source /= Null_Address and Destination /= Null_Address and
      Source /= Destination);
   --  Basic sanity checking for the values

   function Aligned (This : Address;  Width : DMA_Data_Transfer_Widths)
      return Boolean with Inline;
   --  Returns whether the address is aligned on a word, half-word, or UInt8
   --  boundary

   function Compatible_Alignments
     (This           : DMA_Controller;
      Stream         : DMA_Stream_Selector;
      Source         : Address;
      Destination    : Address)
      return Boolean is
     (case Transfer_Direction (This, Stream) is
         when Peripheral_To_Memory | Memory_To_Memory =>
            Aligned (Source, Peripheral_Data_Width (This, Stream))
            and
            Aligned (Destination, Memory_Data_Width (This, Stream)),
         when Memory_To_Peripheral | Peripheral_To_Peripheral =>
            Aligned (Source, Memory_Data_Width (This, Stream))
            and
            Aligned (Destination, Peripheral_Data_Width (This, Stream)));
   --  Based on Ref Manual Table 44 and associated text, checks the alignments
   --  of the addresses against the Peripheral_Data_Format (P_Data_Size) and
   --  Memory_Data_Format (M_Data_Size) values for the given stream. We use an
   --  expression function because the semantics are meant to be part of the
   --  spec of the package, visible as a precondition.

   ------------
   -- DMAMUX --
   ------------

   procedure Set_DMAMUX_Synchronization
     (This    : DMA_Controller;
      Stream  : DMA_Stream_Selector;
      Enabled : Boolean)
     with Post => (if Enabled then DMAMUX_Synchronization_Enabled (This, Stream)
                   else not DMAMUX_Synchronization_Enabled (This, Stream));
   --  Enable/disable input synchronization.

   function DMAMUX_Synchronization_Enabled
     (This    : DMA_Controller;
      Stream  : DMA_Stream_Selector) return Boolean;

   procedure Set_DMAMUX_Event
     (This    : DMA_Controller;
      Stream  : DMA_Stream_Selector;
      Enabled : Boolean)
     with Post => (if Enabled then DMAMUX_Event_Enabled (This, Stream)
                   else not DMAMUX_Event_Enabled (This, Stream));
   --  Enable/disable event generation.

   function DMAMUX_Event_Enabled
     (This    : DMA_Controller;
      Stream  : DMA_Stream_Selector) return Boolean;

   type DMAMUX_Synchronization_Selector is
     (EXTILINE0,
      EXTILINE1,
      EXTILINE2,
      EXTILINE3,
      EXTILINE4,
      EXTILINE5,
      EXTILINE6,
      EXTILINE7,
      EXTILINE8,
      EXTILINE9,
      EXTILINE10,
      EXTILINE11,
      EXTILINE12,
      EXTILINE13,
      EXTILINE14,
      EXTILINE15,
      DMAMUX1_CH0_Event,
      DMAMUX1_CH1_Event,
      DMAMUX1_CH2_Event,
      DMAMUX1_CH3_Event,
      LPTIM1_OUT);

   type Input_Polarity is
     (No_Event,
      Rising_Edge,
      Falling_Edge,
      Rising_And_Falling);
   --  Defines the edge polarity of the selected synchronization or trigger input.

   type DMA_Request_Number is
     (Request_1, Request_2, Request_3, Request_4, Request_5, Request_6,
      Request_7, Request_8, Request_9, Request_10, Request_11, Request_12,
      Request_13, Request_14, Request_15, Request_16, Request_17, Request_18,
      Request_19, Request_20, Request_21, Request_22, Request_23, Request_24,
      Request_25, Request_26, Request_27, Request_28, Request_29, Request_30,
      Request_31, Request_32);
   --  Defines the number of DMA requests to forward to the DMA controller after
   --  a synchronization event, and/or the number of DMA requests before an
   --  output event is generated. Also defines the number of DMA requests to be
   --  generated after a trigger event.

   type DMAMUX_Synchronization_Configuration is record
      Input        : DMAMUX_Synchronization_Selector;
      Polarity     : Input_Polarity;
      DMA_Req      : DMA_Request_Number;
   end record;

   procedure Configure_DMAMUX_Synchronization
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector;
      Config : DMAMUX_Synchronization_Configuration)
     with Pre => not DMAMUX_Synchronization_Enabled (This, Stream) and
                 not DMAMUX_Event_Enabled (This, Stream);
   --  When a channel is in this synchronization mode, the selected input DMA
   --  request line is propagated to the multiplexer channel output, once is
   --  detected a programmable rising/falling edge on the selected input
   --  synchronization signal, via the SPOL[1:0] field of the DMAMUX_CxCR
   --  register. See RM0440 Rev 7 chapter 13.4.4 DMAMUX request line multiplexer.

   procedure Set_Synchronization_Overrun_Interrupt
     (This    : DMA_Controller;
      Stream  : DMA_Stream_Selector;
      Enabled : Boolean)
     with Post => (if Enabled then Synchronization_Overrun_Interrupt_Enabled (This, Stream)
                   else not Synchronization_Overrun_Interrupt_Enabled (This, Stream));
   --  The postcondition should not be relied upon completely because it is
   --  possible, under just the wrong conditions, for the interrupt to be
   --  disabled immediately, prior to return from this routine

   function Synchronization_Overrun_Interrupt_Enabled
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Boolean
     with Inline;

   function Synchronization_Overrun_Status
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Boolean;

   procedure Clear_Synchronization_Overrun_Status
     (This   : DMA_Controller;
      Stream : DMA_Stream_Selector)
     with Post => not Synchronization_Overrun_Status (This, Stream);

   type DMAMUX_Req_Gen_Channel is
     (Channel_1,
      Channel_2,
      Channel_3,
      Channel_4);

   procedure Set_DMAMUX_Request_Generator
     (Channel : DMAMUX_Req_Gen_Channel;
      Enabled : Boolean)
     with Post => (if Enabled then DMAMUX_Request_Generator_Enabled (Channel)
                   else not DMAMUX_Request_Generator_Enabled (Channel));
   --  Enable/disable DMA request generator channel.

   function DMAMUX_Request_Generator_Enabled
     (Channel : DMAMUX_Req_Gen_Channel) return Boolean;

   type DMAMUX_Trigger_Selector is
     (EXTILINE0,
      EXTILINE1,
      EXTILINE2,
      EXTILINE3,
      EXTILINE4,
      EXTILINE5,
      EXTILINE6,
      EXTILINE7,
      EXTILINE8,
      EXTILINE9,
      EXTILINE10,
      EXTILINE11,
      EXTILINE12,
      EXTILINE13,
      EXTILINE14,
      EXTILINE15,
      DMAMUX1_CH0_Event,
      DMAMUX1_CH1_Event,
      DMAMUX1_CH2_Event,
      DMAMUX1_CH3_Event,
      LPTIM1_OUT);

   type DMAMUX_Req_Gen_Configuration is record
      Input           : DMAMUX_Trigger_Selector;
      Polarity        : Input_Polarity;
      DMA_Req         : DMA_Request_Number;
   end record;

   procedure Configure_DMAMUX_Request_Generator
     (Channel : DMAMUX_Req_Gen_Channel;
      Config  : DMAMUX_Req_Gen_Configuration)
     with Pre => not DMAMUX_Request_Generator_Enabled (Channel);
   --  The DMAMUX request generator produces DMA requests following trigger
   --  events on its DMA request trigger inputs. Upon the trigger event, the
   --  corresponding generator channel starts generating DMA requests on its
   --  output. Each time the DMAMUX generated request is served by the connected
   --  DMA controller (a served request is deasserted), a built-in (inside the
   --  DMAMUX request generator) DMA request counter is decremented. See RM0440
   --  Rev 7 chapter 13.4.5 DMAMUX request generator.

   procedure Set_Trigger_Overrun_Interrupt
     (Channel : DMAMUX_Req_Gen_Channel;
      Enabled : Boolean)
     with Post => (if Enabled then Trigger_Overrun_Interrupt_Enabled (Channel)
                   else not Trigger_Overrun_Interrupt_Enabled (Channel));
   --  The postcondition should not be relied upon completely because it is
   --  possible, under just the wrong conditions, for the interrupt to be
   --  disabled immediately, prior to return from this routine

   function Trigger_Overrun_Interrupt_Enabled
     (Channel : DMAMUX_Req_Gen_Channel)
      return Boolean
     with Inline;

   function Trigger_Overrun_Status
     (Channel : DMAMUX_Req_Gen_Channel)
      return Boolean;

   procedure Clear_Trigger_Overrun_Status
     (Channel : DMAMUX_Req_Gen_Channel)
     with Post => not Trigger_Overrun_Status (Channel);

private

   type DMA_Controller is new STM32_SVD.DMA.DMA_Peripheral;

end STM32.DMA;
