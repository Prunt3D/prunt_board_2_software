------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2017, AdaCore                         --
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

with STM32.Device;  use STM32.Device;

package body STM32.HRPWM is

   procedure Configure_PWM_GPIO
     (Output : GPIO_Point;
      PWM_AF : GPIO_Alternate_Function;
      AF_Speed : Pin_Output_Speeds);

   ---------------------------
   -- Configure_PWM_HRTimer --
   ---------------------------

   procedure Configure_PWM_HRTimer
     (Generator : not null access HRTimer_Channel;
      Frequency : Hertz)
   is
      Computed_Prescaler : HRTimer_Prescaler;
      Computed_Period    : UInt32;
   begin
      Enable_Clock (Generator.all);

      Compute_Prescaler_And_Period
        (Generator.all,
         Requested_Frequency => Frequency,
         Prescaler           => Computed_Prescaler,
         Period              => Computed_Period);

      Configure
        (Generator.all,
         Prescaler     => Computed_Prescaler,
         Period        => UInt16 (Computed_Period),
         Counter_Mode  => Up);

      Set_Register_Preload (Generator.all, True);
   end Configure_PWM_HRTimer;

   --------------------------
   -- Attach_HRPWM_Channel --
   --------------------------

   procedure Attach_HRPWM_Channel
     (This       : in out HRPWM_Modulator;
      Generator  : not null access HRTimer_Channel;
      Compare    : HRTimer_Compare_Number;
      Point      : GPIO_Point;
      PWM_AF     : GPIO_Alternate_Function;
      Polarity   : Channel_Output_Polarity := High;
      Idle_State : Boolean;
      AF_Speed   : Pin_Output_Speeds := Speed_100MHz)
   is
   begin
      This.Generator := Generator;
      This.Compare := Compare;

      Enable_Clock (Point);

      Configure_PWM_GPIO (Point, PWM_AF, AF_Speed);

      Configure_Channel_Output
        (This.Generator.all,
         Mode       => Continuous,
         State      => Disable,
         Compare    => This.Compare,
         Pulse      => 0,
         Output     => Output_1,
         Polarity   => Polarity,
         Idle_State => Idle_State);

      Set_Compare_Value (This.Generator.all, This.Compare, UInt16 (0));

      Disable (This.Generator.all);
   end Attach_HRPWM_Channel;

   --------------------------
   -- Attach_HRPWM_Channel --
   --------------------------

   procedure Attach_HRPWM_Channel
     (This                     : in out HRPWM_Modulator;
      Generator                : not null access HRTimer_Channel;
      Compare                  : HRTimer_Compare_Number;
      Point                    : GPIO_Point;
      Complementary_Point      : GPIO_Point;
      PWM_AF                   : GPIO_Alternate_Function;
      Complementary_PWM_AF     : GPIO_Alternate_Function;
      Polarity                 : Channel_Output_Polarity;
      Idle_State               : Boolean;
      Complementary_Polarity   : Channel_Output_Polarity;
      Complementary_Idle_State : Boolean;
      AF_Speed                 : Pin_Output_Speeds := Speed_100MHz)
   is
   begin
      This.Generator := Generator;
      This.Compare := Compare;

      Enable_Clock (Point);
      Enable_Clock (Complementary_Point);

      Configure_PWM_GPIO (Point, PWM_AF, AF_Speed);
      Configure_PWM_GPIO (Complementary_Point, Complementary_PWM_AF, AF_Speed);

      Configure_Channel_Output
        (This.Generator.all,
         Mode                     => Continuous,
         State                    => Disable,
         Compare                  => This.Compare,
         Pulse                    => 0,
         Polarity                 => Polarity,
         Idle_State               => Idle_State,
         Complementary_Polarity   => Complementary_Polarity,
         Complementary_Idle_State => Complementary_Idle_State);

      Set_Compare_Value (This.Generator.all, This.Compare, UInt16 (0));

      Disable (This.Generator.all);
   end Attach_HRPWM_Channel;

   -------------------
   -- Enable_Output --
   -------------------

   procedure Enable_Output (This : in out HRPWM_Modulator) is
   begin
      Set_Channel_Output (This.Generator.all, Output_1, True);
   end Enable_Output;

   ---------------------------------
   -- Enable_Complementary_Output --
   ---------------------------------

   procedure Enable_Complementary_Output (This : in out HRPWM_Modulator) is
   begin
      Set_Channel_Output (This.Generator.all, Output_2, True);
   end Enable_Complementary_Output;

   --------------------
   -- Output_Enabled --
   --------------------

   function Output_Enabled (This : HRPWM_Modulator) return Boolean is
   begin
      return Channel_Output_Enabled (This.Generator.all, Output_1);
   end Output_Enabled;

   ----------------------------------
   -- Complementary_Output_Enabled --
   ----------------------------------

   function Complementary_Output_Enabled (This : HRPWM_Modulator) return Boolean is
   begin
      return Channel_Output_Enabled (This.Generator.all, Output_2);
   end Complementary_Output_Enabled;

   --------------------
   -- Disable_Output --
   --------------------

   procedure Disable_Output (This : in out HRPWM_Modulator) is
   begin
      Set_Channel_Output (This.Generator.all, Output_1, False);
   end Disable_Output;

   ----------------------------------
   -- Disable_Complementary_Output --
   ----------------------------------

   procedure Disable_Complementary_Output (This : in out HRPWM_Modulator) is
   begin
      Set_Channel_Output (This.Generator.all, Output_2, False);
   end Disable_Complementary_Output;

   ------------------
   -- Set_Polarity --
   ------------------

   procedure Set_Polarity
     (This     : in HRPWM_Modulator;
      Polarity : Channel_Output_Polarity) is
   begin
      Set_Channel_Output_Polarity (This.Generator.all, Output_1, Polarity);
   end Set_Polarity;

   --------------------------------
   -- Set_Complementary_Polarity --
   --------------------------------

   procedure Set_Complementary_Polarity
     (This     : in HRPWM_Modulator;
      Polarity : Channel_Output_Polarity) is
   begin
      Set_Channel_Output_Polarity (This.Generator.all, Output_2, Polarity);
   end Set_Complementary_Polarity;

   ------------------------
   -- Configure_PWM_GPIO --
   ------------------------

   procedure Configure_PWM_GPIO
     (Output : GPIO_Point;
      PWM_AF : GPIO_Alternate_Function;
      AF_Speed : Pin_Output_Speeds)
   is
   begin
      Output.Configure_IO
        ((Mode_AF,
          AF             => PWM_AF,
          Resistors      => Floating,
          AF_Output_Type => Push_Pull,
          AF_Speed       => AF_Speed));
   end Configure_PWM_GPIO;

   --------------------
   -- Set_Duty_Cycle --
   --------------------

   procedure Set_Duty_Cycle
     (This    : in out HRPWM_Modulator;
      Value   : Percentage)
   is
      Pulse : UInt16;
   begin
      This.Duty_Cycle := Value;

      if Value = 0 then
         Set_Compare_Value (This.Generator.all, This.Compare, UInt16'(0));
      else
         --  for a Value of 0, the computation of Pulse wraps around, so we
         --  only compute it when not zero
         Pulse := (Current_Period (This.Generator.all) + 1) * UInt16 (Value) / 100 - 1;
         Set_Compare_Value (This.Generator.all, This.Compare, Pulse);
      end if;
   end Set_Duty_Cycle;

   ------------------------
   -- Current_Duty_Cycle --
   ------------------------

   function Current_Duty_Cycle (This : HRPWM_Modulator) return Percentage is
   begin
      return This.Duty_Cycle;
   end Current_Duty_Cycle;

   -----------------------------
   -- Microseconds_Per_Period --
   -----------------------------

   function Microseconds_Per_Period
     (This : HRPWM_Modulator) return Microseconds
   is
      Result             : UInt32;
      Counter_Frequency  : UInt32;
      Hardware_Frequency : UInt32;

      Period    : constant UInt16 := Current_Period (This.Generator.all) + 1;
      Prescalar : constant UInt16 :=
        2 ** (Current_Prescaler (This.Generator.all)'Enum_Rep);
   begin

      Hardware_Frequency := Get_Clock_Frequency (This.Generator.all);

      Counter_Frequency := (Hardware_Frequency / UInt32 (Prescalar)) / UInt32 (Period);

      Result := 1_000_000 / Counter_Frequency;
      return Result;
   end Microseconds_Per_Period;

   -------------------
   -- Set_Duty_Time --
   -------------------

   procedure Set_Duty_Time
     (This  : in out HRPWM_Modulator;
      Value : Microseconds)
   is
      Pulse         : UInt16;
      Period        : constant UInt32 := UInt32 (Current_Period (This.Generator.all)) + 1;
      uS_Per_Period : constant UInt32 := Microseconds_Per_Period (This);
   begin
      if Value = 0 then
         Set_Compare_Value (This.Generator.all, This.Compare, UInt16'(0));
      else
         --  for a Value of 0, the computation of Pulse wraps around, so we
         --  only compute it when not zero
         Pulse := UInt16 ((Period * Value) / uS_Per_Period) - 1;
         Set_Compare_Value (This.Generator.all, This.Compare, Pulse);
      end if;
   end Set_Duty_Time;

end STM32.HRPWM;
