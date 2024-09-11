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
--   @file    stm32f407xx.h   et al.                                        --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   CMSIS STM32F407xx Device Peripheral Access Layer Header File. --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides register definitions for the STM32 (ARM Cortex M4/7F)
--  microcontrollers from ST Microelectronics.

package STM32.EXTI is

   type External_Line_Number is
     (EXTI_Line_0, --  GPIO
      EXTI_Line_1, --  GPIO
      EXTI_Line_2, --  GPIO
      EXTI_Line_3, --  GPIO
      EXTI_Line_4, --  GPIO
      EXTI_Line_5, --  GPIO
      EXTI_Line_6, --  GPIO
      EXTI_Line_7, --  GPIO
      EXTI_Line_8, --  GPIO
      EXTI_Line_9, --  GPIO
      EXTI_Line_10, --  GPIO
      EXTI_Line_11, --  GPIO
      EXTI_Line_12, --  GPIO
      EXTI_Line_13, --  GPIO
      EXTI_Line_14, --  GPIO
      EXTI_Line_15, --  GPIO
      EXTI_Line_16, --  PVD output
      EXTI_Line_17, --  RTC Alarm event
      EXTI_Line_19, --  RTC tamper and Timestamps
      EXTI_Line_20, --  RTC wakeup timer
      EXTI_Line_22, --  Comparator 2 output
      EXTI_Line_23, --  I2C1 wakeup
      EXTI_Line_25, --  USART1 wakeup
      EXTI_Line_30, --  Comparator 4 output
      EXTI_Line_32); --  Comparator 6 output

   for External_Line_Number use
     (EXTI_Line_0   => 0,
      EXTI_Line_1   => 1,
      EXTI_Line_2   => 2,
      EXTI_Line_3   => 3,
      EXTI_Line_4   => 4,
      EXTI_Line_5   => 5,
      EXTI_Line_6   => 6,
      EXTI_Line_7   => 7,
      EXTI_Line_8   => 8,
      EXTI_Line_9   => 9,
      EXTI_Line_10  => 10,
      EXTI_Line_11  => 11,
      EXTI_Line_12  => 12,
      EXTI_Line_13  => 13,
      EXTI_Line_14  => 14,
      EXTI_Line_15  => 15,
      EXTI_Line_16  => 16,
      EXTI_Line_17  => 17,
      EXTI_Line_19  => 19,
      EXTI_Line_20  => 20,
      EXTI_Line_22  => 22,
      EXTI_Line_23  => 23,
      EXTI_Line_25  => 25,
      EXTI_Line_30  => 30,
      EXTI_Line_32  => 32);

   type External_Triggers is
     (Interrupt_Rising_Edge,
      Interrupt_Falling_Edge,
      Interrupt_Rising_Falling_Edge,
      Event_Rising_Edge,
      Event_Falling_Edge,
      Event_Rising_Falling_Edge);

   subtype Interrupt_Triggers is External_Triggers
      range Interrupt_Rising_Edge .. Interrupt_Rising_Falling_Edge;

   subtype Event_Triggers is External_Triggers
      range Event_Rising_Edge .. Event_Rising_Falling_Edge;

   procedure Enable_External_Interrupt
     (Line    : External_Line_Number;
      Trigger : Interrupt_Triggers)
     with Inline;

   procedure Disable_External_Interrupt (Line : External_Line_Number)
     with Inline;

   procedure Enable_External_Event
     (Line    : External_Line_Number;
      Trigger : Event_Triggers)
     with Inline;

   procedure Disable_External_Event (Line : External_Line_Number)
     with Inline;


   procedure Generate_SWI (Line : External_Line_Number)
     with Inline;

   function External_Interrupt_Pending (Line : External_Line_Number)
     return Boolean
     with Inline;

   procedure Clear_External_Interrupt (Line : External_Line_Number)
     with Inline;

end STM32.EXTI;
