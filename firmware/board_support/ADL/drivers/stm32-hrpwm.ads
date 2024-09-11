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

--  This file provides a convenient pulse width modulation (PWM) generation
--  abstract data type.  It is a wrapper around the timers' PWM functionality.

--  Example use, with arbitrary hardware selections:

--     Selected_Timer : STM32.HRTimers.HRTimer_Channel renames HRTimer_A;
--
--     Modulator1 : PWM_Modulator;
--     ...
--     --  Note that a single hrtimer can drive only one PWM modulator.
--
--     Frequency : constant Hertz := 30_000;
--     ...
--     Configure_PWM_HRTimer (Selected_Timer'Access, Frequency);
--
--     Modulator1.Attach_PWM_Channel
--       (Selected_Timer'Access,
--        PD13,
--        GPIO_AF_2_TIM4);
--     ...
--     Modulator1.Enable_Output;
--
--     Modulator1.Set_Duty_Cycle (Value);
--     ...

with STM32.GPIO;      use STM32.GPIO;
with STM32.HRTimers;  use STM32.HRTimers;

package STM32.HRPWM is
   pragma Elaborate_Body;

   subtype Hertz is UInt32;

   procedure Configure_PWM_HRTimer
     (Generator : not null access HRTimer_Channel;
      Frequency : Hertz)
     with Post => Enabled (Generator.all);
   --  Configures the specified timer for the requested frequency. Must
   --  be called once (for a given frequency) for each timer used for the
   --  PWM_Modulator objects. May be called more than once, to change the
   --  operating frequency.
   --
   --  Raises Unknown_Timer if Generator.all is not known to the board.
   --  Raises Invalid_Request if Frequency is too high or too low.

   type HRPWM_Modulator is tagged limited private;
   --  An abstraction for PWM modulation using a timer operating at a given
   --  frequency. Essentially a convenience wrapper for the PWM functionality
   --  of the timers.

   procedure Attach_HRPWM_Channel
     (This       : in out HRPWM_Modulator;
      Generator  : not null access HRTimer_Channel;
      Compare    : HRTimer_Compare_Number;
      Point      : GPIO_Point;
      PWM_AF     : GPIO_Alternate_Function;
      Polarity   : Channel_Output_Polarity := High;
      Idle_State : Boolean;
      AF_Speed   : Pin_Output_Speeds := Speed_100MHz)
     with Post => not Output_Enabled (This) and
                  Current_Duty_Cycle (This) = 0;
   --  Initializes the timer associated with This modulator, and the
   --  corresponding GPIO port/pin pair, for PWM output with only the Output_1.

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
     with Post => not Output_Enabled (This) and
                  not Complementary_Output_Enabled (This) and
                  Current_Duty_Cycle (This) = 0;
   --  Initializes the timer associated with This modulator, and the
   --  corresponding GPIO port/pin pairs, for PWM output with complementary
   --  output included.

   procedure Enable_Output (This : in out HRPWM_Modulator)
     with Post => Output_Enabled (This);

   procedure Enable_Complementary_Output (This : in out HRPWM_Modulator)
     with Post => Complementary_Output_Enabled (This);

   procedure Disable_Output (This : in out HRPWM_Modulator)
     with Post => not Output_Enabled (This);

   procedure Disable_Complementary_Output (This : in out HRPWM_Modulator)
     with Post => not Complementary_Output_Enabled (This);

   function Output_Enabled (This : HRPWM_Modulator) return Boolean;

   function Complementary_Output_Enabled
     (This : HRPWM_Modulator) return Boolean;

   procedure Set_Polarity
     (This     : in HRPWM_Modulator;
      Polarity : in Channel_Output_Polarity);
   --  Set the polarity of the output of This modulator.

   procedure Set_Complementary_Polarity
     (This     : in HRPWM_Modulator;
      Polarity : in Channel_Output_Polarity);
   --  Set the polarity of the complimentary output of This modulator.

   subtype Percentage is Integer range 0 .. 100;

   procedure Set_Duty_Cycle
     (This    : in out HRPWM_Modulator;
      Value   : Percentage)
     with
       Inline,
       Post => Current_Duty_Cycle (This) = Value;
   --  Sets the pulse width such that the PWM output is active for the
   --  requested percentage.

   function Current_Duty_Cycle (This : HRPWM_Modulator) return Percentage
     with Inline;

   subtype Microseconds is UInt32;

   function Microseconds_Per_Period (This : HRPWM_Modulator) return Microseconds
     with Inline;
   --  Essentially 1_000_000 / PWM Frequency
   --
   --  For example, if the PWM timer has a requested frequency of 30KHz the
   --  result will be 33. This can be useful to compute the values passed to
   --  Set_Duty_Time.

   procedure Set_Duty_Time
     (This    : in out HRPWM_Modulator;
      Value   : Microseconds)
     with
       Inline,
       Pre => (Value <= Microseconds_Per_Period (This)
               or else raise Invalid_Request with "duty time too high");
   --  Set the pulse width such that the PWM output is active for the specified
   --  number of microseconds.

   Invalid_Request : exception;
   --  Raised when the requested frequency is too high or too low for the given
   --  timer and system clocks when calling Configure_PWM_Timer, or when
   --  the requested time is too high for the specified frequency when calling
   --  Set_Duty_Time.

   Unknown_Timer : exception;
   --  Raised if a timer that is not known to the package is passed to
   --  Configure_PWM_Timer.

private

   type HRPWM_Modulator is tagged limited record
      Generator  : access HRTimer_Channel;
      Compare    : HRTimer_Compare_Number;
      Duty_Cycle : Percentage := 0;
   end record;

end STM32.HRPWM;
