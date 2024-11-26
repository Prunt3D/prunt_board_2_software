with STM32.GPIO;             use STM32.GPIO;
with HAL;                    use HAL;
with STM32.Device;           use STM32.Device;
with STM32.Timers;           use STM32.Timers;
with STM32.LPTimers;         use STM32.LPTimers;
with STM32.COMP;             use STM32.COMP;
with Hardware_Configuration; use Hardware_Configuration;
with STM32.SYSCFG;

package body Fans is

   procedure Init is
   begin
      Fan_Handlers.Init;
   end Init;

   procedure Reconfigure (Fan : Fan_Name; PWM_Frequency : Fixed_Point_Fan_PWM_Frequency) is
   begin
      Fan_Handlers.Reconfigure (Fan, PWM_Frequency);
   end Reconfigure;

   procedure Set_PWM (Fan : Fan_Name; Scale : Fixed_Point_PWM_Scale) is
   begin
      Fan_Handlers.Set_PWM (Fan, Scale);
   end Set_PWM;

   function Get_PWM (Fan : Fan_Name) return PWM_Scale is
   begin
      return Fan_Handlers.Get_PWM (Fan);
   end Get_PWM;

   function Get_Tach_Counter (Fan : Fan_Name) return Tach_Counter is
   begin
      return Fan_Handlers.Get_Tach_Counter (Fan);
   end Get_Tach_Counter;

   protected body Fan_Handlers is
      procedure Init is
         procedure Init_PWM_Timer
           (Tim : in out Timer; Channel : Timer_Channel; Polarity : Timer_Output_Compare_Polarity)
         is
         begin
            Enable_Clock (Tim);
            Disable (Tim); --  The same timer may be used for multiple channels.
            if Advanced_Timer (Tim) then
               Enable_Main_Output (Tim);
            end if;
            Configure (This => Tim, Prescaler => 74, Period => 60_000); --  33.33 Hz
            Configure_Channel_Output
              (This => Tim, Channel => Channel, Mode => PWM1, State => Enable, Pulse => 0, Polarity => Polarity);
            Enable (Tim);
         end Init_PWM_Timer;

         function GPIO_To_NonInverting_Input_Port (Point : GPIO_Point) return NonInverting_Input_Port is
         begin
            if Point = PA1 or Point = PA7 or Point = PA0 or Point = PB0 or Point = PB13 or Point = PB11 or Point = PB14
            then
               return Option_1;
            elsif Point = PB1 or Point = PA3 or Point = PC1 or Point = PE7 or Point = PD12 or Point = PD11 or
              Point = PD14
            then
               return Option_2;
            else
               raise Constraint_Error with "GPIO point not connected to non-inverting comparator input.";
            end if;
         end GPIO_To_NonInverting_Input_Port;

         procedure Init_Tach (Config : Tach_Config) is
         begin
            Configure_IO (Config.Point, (Mode_Analog, Floating));
            Configure_Comparator
              (Config.Comp.all,
              (Input_Minus      => Vrefint,
                Input_Plus      => GPIO_To_NonInverting_Input_Port (Config.Point),
                Hysteresis      => Fifty_mV,
                Blanking_Source => No_Blanking,
                Output_Pol      => Not_Inverted));
            Enable (Config.Comp.all);

            case Config.Kind is
               when Timer_Kind =>
                  Enable_Clock (Config.Tim.all);
                  Set_External_Trigger_Source (Config.Tim.all, Config.Trigger);
                  Configure_External_Clock_Mode1 (Config.Tim.all, NonInverted, Off, No_Filter);
                  Enable (Config.Tim.all);
               when LPTimer_Kind =>
                  Enable_Clock (Config.LPTim.all);
                  Select_Clock_Source (Config.LPTim.all, Internal);
                  Configure_Input_Clock (Config.LPTim.all, Input_1, (Internal => True, Value => Config.Clock));
                  Set_Counter_Clock_Source (Config.LPTim.all, External);
                  Configure_External_Clock (Config.LPTim.all, Rising_Edge, Any_Level_Change);
                  Enable (Config.LPTim.all);
                  Set_Autoreload_Value (Config.LPTim.all, UInt16'Last);
                  Select_Pulse_Mode (Config.LPTim.all, Repetitive);
            end case;
         end Init_Tach;
      begin
         Init_Checker.Report_Init_Started;

         STM32.SYSCFG.Enable_SYSCFG_Clock;
         --  For comparators.

         for Fan in Fan_Name loop
            Init_PWM_Timer (Fan_Timers (Fan).all, Fan_Timer_Channels (Fan), Fan_Timer_Polarities (Fan));

            if Fan_Timer_Complementary (Fan) then
               Enable_Complementary_Channel (Fan_Timers (Fan).all, Fan_Timer_Channels (Fan));
            end if;
            Configure_IO
              (Fan_GPIO_Points (Fan),
               (Mode           => Mode_AF,
                Resistors      => Floating,
                AF_Output_Type => Push_Pull,
                AF_Speed       => Speed_25MHz,
                AF             => Fan_GPIO_AFs (Fan)));

            Init_Tach (Tach_Configs (Fan));
         end loop;

         Init_Checker.Report_Init_Done;
      end Init;

      procedure Reconfigure (Fan : Fan_Name; PWM_Frequency : Fixed_Point_Fan_PWM_Frequency) is
         Prescaler : UInt32;
         Period    : UInt32;
      begin
         Compute_Prescaler_And_Period
           (This                => Fan_Timers (Fan),
            Requested_Frequency => UInt32 (PWM_Frequency),
            Prescaler           => Prescaler,
            Period              => Period);
         Configure (This => Fan_Timers (Fan).all, Prescaler => UInt16 (Prescaler), Period => Period);
      end Reconfigure;

      procedure Set_PWM (Fan : Fan_Name; Scale : Fixed_Point_PWM_Scale) is
      begin
         Init_Checker.Raise_If_Init_Not_Done;

         if Has_32bit_CC_Values ((Fan_Timers (Fan).all)) then
            Set_Compare_Value
              (Fan_Timers (Fan).all,
               Fan_Timer_Channels (Fan),
               UInt32 (Float (Scale) * Float (Current_Autoreload (Fan_Timers (Fan).all))));
         else
            Set_Compare_Value
              (Fan_Timers (Fan).all,
               Fan_Timer_Channels (Fan),
               UInt16 (Float (Scale) * Float (Current_Autoreload (Fan_Timers (Fan).all))));
         end if;
      end Set_PWM;

      function Get_PWM (Fan : Fan_Name) return PWM_Scale is
      begin
         Init_Checker.Raise_If_Init_Not_Done;

         return
           Dimensionless'Min
             (Dimensionless (UInt16'(Current_Capture_Value (Fan_Timers (Fan).all, Fan_Timer_Channels (Fan)))) /
              Dimensionless (Current_Autoreload (Fan_Timers (Fan).all)),
              PWM_Scale'Last);
      end Get_PWM;

      function Get_Tach_Counter (Fan : Fan_Name) return Tach_Counter is
      begin
         Init_Checker.Raise_If_Init_Not_Done;

         case Tach_Configs (Fan).Kind is
            when Timer_Kind =>
               return Tach_Counter (Current_Counter (Tach_Configs (Fan).Tim.all) mod 2**16);
            when LPTimer_Kind =>
               return Tach_Counter (Current_Counter (Tach_Configs (Fan).LPTim.all));
         end case;
      end Get_Tach_Counter;
   end Fan_Handlers;

end Fans;
