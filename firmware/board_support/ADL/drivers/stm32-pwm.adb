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

package body STM32.PWM is

   function Timer_Period (This : PWM_Modulator) return UInt32 is
      (Current_Autoreload (This.Generator.all));

   procedure Configure_PWM_GPIO
     (Output : GPIO_Point;
      PWM_AF : GPIO_Alternate_Function;
      AF_Speed : Pin_Output_Speeds);

   --------------------
   -- Set_Duty_Cycle --
   --------------------

   procedure Set_Duty_Cycle
     (This  : in out PWM_Modulator;
      Value : Percentage)
   is
      Pulse16 : UInt16;
      Pulse32 : UInt32;
   begin
      This.Duty_Cycle := Value;

      if Value = 0 then
         Set_Compare_Value (This.Generator.all, This.Channel, UInt16'(0));
      else
         --  for a Value of 0, the computation of Pulse wraps around, so we
         --  only compute it when not zero

         if Has_32bit_CC_Values (This.Generator.all) then
            Pulse32 := UInt32 ((Timer_Period (This) + 1) * UInt32 (Value) / 100) - 1;
            Set_Compare_Value (This.Generator.all, This.Channel, Pulse32);
         else
            Pulse16 := UInt16 ((Timer_Period (This) + 1) * UInt32 (Value) / 100) - 1;
            Set_Compare_Value (This.Generator.all, This.Channel, Pulse16);
         end if;
      end if;
   end Set_Duty_Cycle;

   -------------------
   -- Set_Duty_Time --
   -------------------

   procedure Set_Duty_Time
     (This  : in out PWM_Modulator;
      Value : Microseconds)
   is
      Pulse16       : UInt16;
      Pulse32       : UInt32;
      Period        : constant UInt32 := Timer_Period (This) + 1;
      uS_Per_Period : constant UInt32 := Microseconds_Per_Period (This);
   begin
      if Value = 0 then
         Set_Compare_Value (This.Generator.all, This.Channel, UInt16'(0));
      else
         --  for a Value of 0, the computation of Pulse wraps around, so we
         --  only compute it when not zero
         if Has_32bit_CC_Values (This.Generator.all) then
            Pulse32 := UInt32 ((Period / uS_Per_Period) * Value) - 1;
            Set_Compare_Value (This.Generator.all, This.Channel, Pulse32);
         else
            Pulse16 := UInt16 ((Period * Value) / uS_Per_Period) - 1;
            Set_Compare_Value (This.Generator.all, This.Channel, Pulse16);
         end if;
      end if;
   end Set_Duty_Time;

   ------------------------
   -- Current_Duty_Cycle --
   ------------------------

   function Current_Duty_Cycle (This : PWM_Modulator) return Percentage is
   begin
      return This.Duty_Cycle;
   end Current_Duty_Cycle;

   -------------------------
   -- Configure_PWM_Timer --
   -------------------------

   procedure Configure_PWM_Timer
     (Generator : not null access Timer;
      Frequency : Hertz)
   is
      Computed_Prescaler : UInt32;
      Computed_Period    : UInt32;
   begin
      Enable_Clock (Generator.all);

      Compute_Prescaler_And_Period
        (Generator,
         Requested_Frequency => Frequency,
         Prescaler           => Computed_Prescaler,
         Period              => Computed_Period);

      Computed_Period := Computed_Period - 1;

      Configure
        (Generator.all,
         Prescaler     => UInt16 (Computed_Prescaler),
         Period        => Computed_Period,
         Clock_Divisor => Div1,
         Counter_Mode  => Up);

      Set_Autoreload_Preload (Generator.all, True);

      if Advanced_Timer (Generator.all) then
         Enable_Main_Output (Generator.all);
      end if;

      Enable (Generator.all);
   end Configure_PWM_Timer;

   ------------------------
   -- Attach_PWM_Channel --
   ------------------------

   procedure Attach_PWM_Channel
     (This      : in out PWM_Modulator;
      Generator : not null access Timer;
      Channel   : Timer_Channel;
      Point     : GPIO_Point;
      PWM_AF    : GPIO_Alternate_Function;
      Polarity  : Timer_Output_Compare_Polarity := High;
      AF_Speed  : Pin_Output_Speeds := Speed_100MHz)
   is
   begin
      This.Channel := Channel;
      This.Generator := Generator;

      Enable_Clock (Point);

      Configure_PWM_GPIO (Point, PWM_AF, AF_Speed);

      Configure_Channel_Output
        (This.Generator.all,
         Channel  => Channel,
         Mode     => PWM1,
         State    => Disable,
         Pulse    => 0,
         Polarity => Polarity);

      Set_Compare_Value (This.Generator.all, Channel, UInt16 (0));

      Disable_Channel (This.Generator.all, Channel);
   end Attach_PWM_Channel;

   ------------------------
   -- Attach_PWM_Channel --
   ------------------------

   procedure Attach_PWM_Channel
     (This                     : in out PWM_Modulator;
      Generator                : not null access Timer;
      Channel                  : Timer_Channel;
      Point                    : GPIO_Point;
      Complementary_Point      : GPIO_Point;
      PWM_AF                   : GPIO_Alternate_Function;
      Complementary_PWM_AF     : GPIO_Alternate_Function;
      Polarity                 : Timer_Output_Compare_Polarity;
      Idle_State               : Timer_Capture_Compare_State;
      Complementary_Polarity   : Timer_Output_Compare_Polarity;
      Complementary_Idle_State : Timer_Capture_Compare_State;
      AF_Speed                 : Pin_Output_Speeds := Speed_100MHz)
   is
   begin
      This.Channel := Channel;
      This.Generator := Generator;

      Enable_Clock (Point);
      Enable_Clock (Complementary_Point);

      Configure_PWM_GPIO (Point, PWM_AF, AF_Speed);
      Configure_PWM_GPIO (Complementary_Point, Complementary_PWM_AF, AF_Speed);

      Configure_Channel_Output
        (This.Generator.all,
         Channel                  => Channel,
         Mode                     => PWM1,
         State                    => Disable,
         Pulse                    => 0,
         Polarity                 => Polarity,
         Idle_State               => Idle_State,
         Complementary_Polarity   => Complementary_Polarity,
         Complementary_Idle_State => Complementary_Idle_State);

      Set_Compare_Value (This.Generator.all, Channel, UInt16 (0));

      Disable_Channel (This.Generator.all, Channel);
   end Attach_PWM_Channel;

   -------------------
   -- Enable_Output --
   -------------------

   procedure Enable_Output (This : in out PWM_Modulator) is
   begin
      Enable_Channel (This.Generator.all, This.Channel);
   end Enable_Output;

   ---------------------------------
   -- Enable_Complementary_Output --
   ---------------------------------

   procedure Enable_Complementary_Output (This : in out PWM_Modulator) is
   begin
      Enable_Complementary_Channel (This.Generator.all, This.Channel);
   end Enable_Complementary_Output;

   --------------------
   -- Output_Enabled --
   --------------------

   function Output_Enabled (This : PWM_Modulator) return Boolean is
   begin
      return Channel_Enabled (This.Generator.all, This.Channel);
   end Output_Enabled;

   ----------------------------------
   -- Complementary_Output_Enabled --
   ----------------------------------

   function Complementary_Output_Enabled (This : PWM_Modulator) return Boolean is
   begin
      return Complementary_Channel_Enabled (This.Generator.all, This.Channel);
   end Complementary_Output_Enabled;

   --------------------
   -- Disable_Output --
   --------------------

   procedure Disable_Output (This : in out PWM_Modulator) is
   begin
      Disable_Channel (This.Generator.all, This.Channel);
   end Disable_Output;

   ----------------------------------
   -- Disable_Complementary_Output --
   ----------------------------------

   procedure Disable_Complementary_Output (This : in out PWM_Modulator) is
   begin
      Disable_Complementary_Channel (This.Generator.all, This.Channel);
   end Disable_Complementary_Output;

   ------------------
   -- Set_Polarity --
   ------------------

   procedure Set_Polarity
     (This     : in PWM_Modulator;
      Polarity : in Timer_Output_Compare_Polarity) is
   begin
      Set_Output_Polarity (This.Generator.all, This.Channel, Polarity);
   end Set_Polarity;

   --------------------------------
   -- Set_Complementary_Polarity --
   --------------------------------

   procedure Set_Complementary_Polarity
     (This     : in PWM_Modulator;
      Polarity : in Timer_Output_Compare_Polarity) is
   begin
      Set_Output_Complementary_Polarity (This.Generator.all, This.Channel, Polarity);
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

   -----------------------------
   -- Microseconds_Per_Period --
   -----------------------------

   function Microseconds_Per_Period (This : PWM_Modulator) return Microseconds is
      Result             : UInt32;
      Counter_Frequency  : UInt32;
      Platform_Frequency : UInt32;

      Period    : constant UInt32 := Timer_Period (This) + 1;
      Prescalar : constant UInt16 := Current_Prescaler (This.Generator.all) + 1;
   begin

      Platform_Frequency := Get_Clock_Frequency (This.Generator.all);

      Counter_Frequency := (Platform_Frequency / UInt32 (Prescalar)) / Period;

      Result := 1_000_000 / Counter_Frequency;
      return Result;
   end Microseconds_Per_Period;

end STM32.PWM;
