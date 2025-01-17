with STM32.GPIO;   use STM32.GPIO;
with HAL;          use HAL;
with STM32.Device; use STM32.Device;
with STM32.Timers; use STM32.Timers;
with STM32.IWDG;
with Server_Communication;
with Ada.Numerics;
with Thermistors;

package body Heaters is

   procedure Init is
      procedure Init_Timer (Tim : in out Timer; Channel : Timer_Channel; Polarity : Timer_Output_Compare_Polarity) is
      begin
         Enable_Clock (Tim);
         Disable (Tim); --  The same timer may be used for multiple channels.
         if Advanced_Timer (Tim) then
            Enable_Main_Output (Tim);
         end if;
         Configure (This => Tim, Prescaler => 2, Period => 50_000); --  1kHz
         Configure_Channel_Output
           (This => Tim, Channel => Channel, Mode => PWM1, State => Enable, Pulse => 0, Polarity => Polarity);
         Enable (Tim);
      end Init_Timer;
   begin
      Init_Checker.Report_Init_Started;

      for Heater in Heater_Name loop
         Heater_Heater_Params (Heater).Set
           ((Kind                       => Disabled_Kind,
             Check_Max_Cumulative_Error => 0.0,
             Check_Gain_Time            => 0.0,
             Check_Minimum_Gain         => 0.0,
             Check_Hysteresis           => 0.0));

         Init_Timer (Heater_Timers (Heater).all, Heater_Timer_Channels (Heater), Heater_Timer_Polarities (Heater));

         Configure_IO
           (Heater_GPIO_Points (Heater),
            (Mode           => Mode_AF,
             Resistors      => Floating,
             AF_Output_Type => Push_Pull,
             AF_Speed       => Speed_25MHz,
             AF             => Heater_GPIO_AFs (Heater)));

         Set_PWM (Heater, 0.0);
      end loop;

      STM32.IWDG.Initialize_Watchdog (STM32.IWDG.Divider_32, 4_000); --  Approximately 4 seconds.
      STM32.IWDG.Start_Watchdog;

      Init_Checker.Report_Init_Done;
   end Init;

   procedure Make_Safe is
   begin
      for Heater in Heater_Name loop
         Configure_IO (Heater_GPIO_Points (Heater), (Mode => Mode_In, Resistors => Pull_Down));
      end loop;
   end Make_Safe;

   procedure Setup (Heater : Heater_Name; Parameters : Heater_Parameters) is
   begin
      Init_Checker.Raise_If_Init_Not_Done;
      --  Not required but may detect an issue elsewhere.

      Heater_Heater_Params (Heater).Set (Parameters);
   end Setup;

   procedure Set_Setpoint (Heater : Heater_Name; Setpoint : Temperature) is
   begin
      Init_Checker.Raise_If_Init_Not_Done;
      --  Not required but may detect an issue elsewhere.

      Heater_Setpoint_Holders (Heater).Set (Setpoint);
      if Setpoint <= 0.0 * celsius then
         Set_PWM (Heater, 0.0);
      end if;
   end Set_Setpoint;

   procedure Set_PWM (Heater : Heater_Name; Scale : PWM_Scale; Override_Forced_Zero : Boolean := False) is
      Setpoint : constant Temperature := Heater_Setpoint_Holders (Heater).Get;
   begin
      if Setpoint <= 0.0 * celsius and not Override_Forced_Zero then
         Set_Compare_Value (Heater_Timers (Heater).all, Heater_Timer_Channels (Heater), UInt16 (0));
      else
         Set_Compare_Value (Heater_Timers (Heater).all, Heater_Timer_Channels (Heater), UInt16 (Scale * 50_001.0));
      end if;
   end Set_PWM;

   function Get_PWM (Heater : Heater_Name) return Dimensionless is
   begin
      return
        Dimensionless (UInt16'(Current_Capture_Value (Heater_Timers (Heater).all, Heater_Timer_Channels (Heater)))) /
        50_001.0;
   end Get_PWM;

   function Get_PWM (Heater : Heater_Name) return Fixed_Point_PWM_Scale is
   begin
      return Fixed_Point_PWM_Scale (Dimensionless'(Get_PWM (Heater)));
   end Get_PWM;

   function Check_If_Autotune_Done (Heater : Heater_Name) return Boolean is
   begin
      case Heater_Heater_Params (Heater).Get.Kind is
         when Disabled_Kind =>
            return True;
         when Bang_Bang_Kind | PID_Kind =>
            Make_Safe;
            raise Constraint_Error with "Heater in unexpected mode.";
         when PID_Autotune_Kind =>
            return False;
      end case;
   end Check_If_Autotune_Done;

   procedure Update_Reading (Heater : Heater_Name; Current_Temperature : Temperature) is
   begin
      Heater_Update_Holders (Heater).Set_Update (Current_Temperature);
   end Update_Reading;

   protected body Heater_Update_Holder is
      procedure Set_Update (Temp : Temperature) is
      begin
         if Update_Ready then
            raise Constraint_Error with "Heater controller did not consume last ADC reading.";
         end if;

         Reading      := Temp;
         Update_Ready := True;
      end Set_Update;

      entry Wait_Next_Reading (Temp : out Temperature) when Update_Ready is
      begin
         Temp         := Reading;
         Update_Ready := False;
      end Wait_Next_Reading;
   end Heater_Update_Holder;

   protected body Heater_Params_Holder is
      procedure Set (Params : Heater_Parameters) is
      begin
         Data := Params;
      end Set;

      function Get return Heater_Parameters is
      begin
         return Data;
      end Get;
   end Heater_Params_Holder;

   protected body Setpoint_Holder is
      procedure Set (Setpoint : Temperature) is
      begin
         Data := Setpoint;
      end Set;

      function Get return Temperature is
      begin
         return Data;
      end Get;
   end Setpoint_Holder;

   task body Heater_Controller is
      Temp   : Temperature;
      Params : Heater_Parameters;
   begin
      Init_Checker.Wait_For_Init;

      loop
         Set_PWM (Heater, 0.0);
         --  We alternatively could leave the PWM value as-is when changing modes. This could be used to more smoothly
         --  switch between PID and MPC, however it is unlikely that anyone will require that, so it is a bit safer to
         --  always force the output off here.

         Params := Heater_Heater_Params (Heater).Get;
         case Params.Kind is
            when Disabled_Kind =>
               Disabled_Loop (Heater);
            when PID_Kind =>
               PIDNN_Loop (Heater);
            when PID_Autotune_Kind =>
               PID_Autotune_Loop (Heater);
            when Bang_Bang_Kind =>
               Bang_Bang_Loop (Heater);
         end case;
      end loop;
   end Heater_Controller;

   protected body Safety_Checker is
      procedure Report_Updated (Updated_Heater : Heater_Name; Current_Temp : Temperature) is
         Ctx      : Safety_Checker_Context renames Contexts (Updated_Heater);
         Setpoint : constant Temperature       := Heater_Setpoint_Holders (Updated_Heater).Get;
         Params   : constant Heater_Parameters := Heater_Heater_Params (Updated_Heater).Get;
      begin
         Ctx.Updated_Since_Last_Reset := False;

         --  Algorithm from Klipper.
         if Current_Temp >= Setpoint - Temperature (Params.Check_Hysteresis) or Setpoint <= 0.0 * celsius then
            Ctx.Approaching_Setpoint := False;
            Ctx.Starting_Approach    := False;
            if Current_Temp <= Setpoint + Temperature (Params.Check_Hysteresis) then
               Ctx.Cumulative_Error := 0.0 * celsius;
            end if;
            Ctx.Last_Setpoint := Setpoint;
         else
            Ctx.Cumulative_Error :=
              Ctx.Cumulative_Error + (Setpoint - Temperature (Params.Check_Hysteresis)) - Current_Temp;
            if not Ctx.Approaching_Setpoint then
               if Setpoint /= Ctx.Last_Setpoint then
                  Ctx.Approaching_Setpoint := True;
                  Ctx.Starting_Approach    := True;
                  Ctx.Goal_Temp            := Current_Temp + Temperature (Params.Check_Minimum_Gain);
                  Ctx.Goal_Time            := Clock + To_Time_Span (Duration (Params.Check_Gain_Time));
               elsif Ctx.Cumulative_Error > Temperature (Params.Check_Max_Cumulative_Error) then
                  Make_Safe;
                  raise Heater_Check_Failure with "Heater " & Updated_Heater'Image & " could not maintain setpoint.";
               end if;
            elsif Current_Temp >= Ctx.Goal_Temp then
               Ctx.Starting_Approach := False;
               Ctx.Cumulative_Error  := 0.0 * celsius;
               Ctx.Goal_Temp         := Current_Temp + Temperature (Params.Check_Minimum_Gain);
               Ctx.Goal_Time         := Clock + To_Time_Span (Duration (Params.Check_Gain_Time));
            elsif Clock >= Ctx.Goal_Time then
               Ctx.Approaching_Setpoint := False;
            elsif Ctx.Starting_Approach then
               Ctx.Goal_Temp :=
                 Temperature'Min (Ctx.Goal_Temp, Current_Temp + Temperature (Params.Check_Minimum_Gain));
            end if;
            Ctx.Last_Setpoint := Setpoint;
         end if;

         Ctx.Updated_Since_Last_Reset := True;

         for Heater in Heater_Name loop
            if not Contexts (Heater).Updated_Since_Last_Reset then
               return;
            end if;
         end loop;

         for Heater in Heater_Name loop
            Contexts (Heater).Updated_Since_Last_Reset := False;
         end loop;

         STM32.IWDG.Reset_Watchdog;
      end Report_Updated;
   end Safety_Checker;

   procedure PID_Loop (Heater : Heater_Name) is
      --  PID algorithm from https://github.com/br3ttb/Arduino-PID-Library
      --  Copyright Brett Beauregard <br3ttb@gmail.com> brettbeauregard.com

      subtype PID_Scale is Inverse_Temperature range 0.0 .. Dimensionless'Last;

      Proportional_Scale : PID_Scale;
      Integral_Scale     : PID_Scale;
      Derivative_Scale   : PID_Scale;
      Output_Sum         : Dimensionless := Get_PWM (Heater);
      Last_Temperature   : Temperature   := -1_000_000.0 * celsius;

      Params : Heater_Parameters;

      Current_Temperature : Temperature;
   begin
      loop
         Params := Heater_Heater_Params (Heater).Get;
         if Params.Kind /= PID_Kind then
            return;
         end if;

         Proportional_Scale := PID_Scale (Params.Proportional_Scale);
         Integral_Scale     := PID_Scale (Params.Integral_Scale) * (hertz / Thermistors.Loop_Frequency);
         Derivative_Scale   := PID_Scale (Params.Derivative_Scale) / (hertz / Thermistors.Loop_Frequency);

         Heater_Update_Holders (Heater).Wait_Next_Reading (Current_Temperature);

         if Last_Temperature = -1_000_000.0 * celsius then
            --  Heater has only just been switched to PID, or something is broken.
            Last_Temperature := Current_Temperature;
         end if;

         declare
            Setpoint : constant Temperature := Heater_Setpoint_Holders (Heater).Get;
            Error    : constant Temperature := Setpoint - Current_Temperature;
            Delta_T  : constant Temperature := Current_Temperature - Last_Temperature;
            Output   : Dimensionless        := Proportional_Scale * Error;
         begin
            Output_Sum := @ + (Integral_Scale * Error);

            if Output_Sum < 0.0 then
               Output_Sum := 0.0;
            elsif Output_Sum > 1.0 then
               Output_Sum := 1.0;
            end if;

            Output := @ + Output_Sum - Derivative_Scale * Delta_T;

            if Output < 0.0 then
               Output := 0.0;
            elsif Output > 1.0 then
               Output := 1.0;
            end if;

            Set_PWM (Heater, Output);

            Last_Temperature := Current_Temperature;

            Safety_Checker.Report_Updated (Heater, Current_Temperature);
         end;
      end loop;
   end PID_Loop;

   procedure PID_Autotune_Loop (Heater : Heater_Name) is
      subtype PID_Scale is Inverse_Temperature range 0.0 .. Dimensionless'Last;

      Start_Time : constant Ada.Real_Time.Time := Clock;
      Loop_Time  : Ada.Real_Time.Time          := Start_Time;

      Bias               : PWM_Scale          := 0.5;
      D                  : PWM_Scale          := 0.5;
      T1                 : Ada.Real_Time.Time := Start_Time;
      T2                 : Ada.Real_Time.Time := Start_Time;
      T_High             : Time_Span          := Seconds (0);
      T_Low              : Time_Span          := Seconds (0);
      Cycles             : Natural            := 0;
      Heating            : Boolean            := True;
      Max_T              : Temperature        := 0.0 * celsius;
      Min_T              : Temperature        := 10_000.0 * celsius;
      Max_Cycles         : Natural;
      Proportional_Scale : PID_Scale          := 0.0 / celsius;
      Integral_Scale     : PID_Scale          := 0.0 / celsius;
      Derivative_Scale   : PID_Scale          := 0.0 / celsius;
      Setpoint           : Temperature;

      Params : Heater_Parameters;

      Current_Temperature : Temperature;
   begin
      --  Algorithm from Marlin.

      Params := Heater_Heater_Params (Heater).Get;
      if Params.Kind /= PID_Autotune_Kind then
         return;
      end if;

      Max_Cycles := Natural (Params.Max_Cycles);
      Setpoint   := Temperature (Params.PID_Tuning_Temperature);

      Heater_Setpoint_Holders (Heater).Set (0.0 * celsius);
      Set_PWM (Heater, Bias, Override_Forced_Zero => True);

      loop
         Params := Heater_Heater_Params (Heater).Get;
         if Params.Kind /= PID_Autotune_Kind then
            Make_Safe;
            raise Constraint_Error with "PID autotune interrupted.";
         end if;

         Heater_Update_Holders (Heater).Wait_Next_Reading (Current_Temperature);
         Loop_Time := Clock;

         if (Current_Temperature > Setpoint + 30.0 * celsius) then
            Make_Safe;
            raise Heater_Check_Failure with "Heater overshot by over 30 C during PID autotune.";
         elsif Loop_Time - T1 > Minutes (20) and Loop_Time - T2 > Minutes (20) then
            Make_Safe;
            raise Heater_Check_Failure with "Heater has taken over 20 minutes to cycle during PID autotune.";
         end if;

         Max_T := Temperature'Max (@, Current_Temperature);
         Min_T := Temperature'Min (@, Current_Temperature);

         if Heating and Current_Temperature > Setpoint and Loop_Time > T2 + Seconds (5) then
            Heating := False;
            Set_PWM (Heater, (Bias - D) / 2.0, Override_Forced_Zero => True);
            T1     := Loop_Time;
            T_High := T1 - T2;
            Max_T  := Setpoint;
         elsif (not Heating) and Current_Temperature < Setpoint and Loop_Time > T1 + Seconds (5) then
            Heating := True;
            T2      := Loop_Time;
            T_Low   := T2 - T1;
            if Cycles > 0 then
               Bias :=
                 @ +
                 (D * Physical_Types.Time (To_Duration (T_High - T_Low)) /
                  Physical_Types.Time (To_Duration (T_High + T_Low)));
               Bias := PWM_Scale'Min (0.92, PWM_Scale'Max (0.08, @));
               if Bias > 0.5 then
                  D := 0.995 - Bias;
               else
                  D := Bias;
               end if;

               Server_Communication.Transmit_String ("Bias=");
               Server_Communication.Transmit_String (Bias'Image);
               Server_Communication.Transmit_String (" D=");
               Server_Communication.Transmit_String (D'Image);
               Server_Communication.Transmit_String (" Max_T=");
               Server_Communication.Transmit_String (Max_T'Image);
               Server_Communication.Transmit_String (" Min_T=");
               Server_Communication.Transmit_String_Line (Min_T'Image);

               if Cycles > 2 then
                  declare
                     Ku : constant Inverse_Temperature := 8.0 * D / (Ada.Numerics.Pi * (Max_T - Min_T));
                     Tu : constant Physical_Types.Time := Dimensionless (To_Duration (T_Low + T_High)) * s;
                  begin
                     Proportional_Scale := Ku * (Dimensionless (Params.Proportional_Tuning_Factor));
                     Integral_Scale     := Proportional_Scale * (2.0 * s / Tu);
                     Derivative_Scale   := Proportional_Scale * Tu * Frequency (Params.Derivative_Tuning_Factor);

                     Server_Communication.Transmit_String ("Ku=");
                     Server_Communication.Transmit_String (Ku'Image);
                     Server_Communication.Transmit_String (" Tu=");
                     Server_Communication.Transmit_String_Line (Tu'Image);
                     Server_Communication.Transmit_String ("P=");
                     Server_Communication.Transmit_String (Proportional_Scale'Image);
                     Server_Communication.Transmit_String (" I=");
                     Server_Communication.Transmit_String (Integral_Scale'Image);
                     Server_Communication.Transmit_String (" D=");
                     Server_Communication.Transmit_String_Line (Derivative_Scale'Image);
                  end;
               end if;
            end if;

            Set_PWM (Heater, (Bias + D) / 2.0, Override_Forced_Zero => True);

            Server_Communication.Transmit_String (Cycles'Image);
            Server_Communication.Transmit_String ("/");
            Server_Communication.Transmit_String_Line (Max_Cycles'Image);

            Cycles := @ + 1;
            Min_T  := Setpoint;
         end if;

         Safety_Checker.Report_Updated (Heater, Current_Temperature);

         if Cycles > Natural'Max (2, Max_Cycles) then
            Set_PWM (Heater, 0.0);
            Server_Communication.Transmit_String_Line ("PID autotune done.");

            Heater_Setpoint_Holders (Heater).Set (0.0 * celsius);
            Heater_Heater_Params (Heater).Set
              ((Kind                       => Disabled_Kind,
                Check_Max_Cumulative_Error => 0.0,
                Check_Gain_Time            => 0.0,
                Check_Minimum_Gain         => 0.0,
                Check_Hysteresis           => 0.0));

            return;
         end if;
      end loop;
   end PID_Autotune_Loop;

   procedure Bang_Bang_Loop (Heater : Heater_Name) is
      Params : Heater_Parameters;

      Hysteresis : Temperature;

      Current_Temperature : Temperature;
      Setpoint            : Temperature;
   begin
      loop
         Params := Heater_Heater_Params (Heater).Get;
         if Params.Kind /= Bang_Bang_Kind then
            return;
         end if;

         Hysteresis := Temperature (Params.Bang_Bang_Hysteresis);

         Heater_Update_Holders (Heater).Wait_Next_Reading (Current_Temperature);
         Setpoint := Heater_Setpoint_Holders (Heater).Get;

         if Current_Temperature > Setpoint + Hysteresis then
            Set_PWM (Heater, 0.0);
         elsif Current_Temperature < Setpoint - Hysteresis then
            Set_PWM (Heater, 1.0);
         end if;

         Safety_Checker.Report_Updated (Heater, Current_Temperature);
      end loop;
   end Bang_Bang_Loop;

   procedure PIDNN_Loop (Heater : Heater_Name) is
      use Dimensionless_Real_Arrays;

      type Mode_Kind is (Setpoint_Change, Frozen_Weights, Training_Weights);

      function Tanh (X : Dimensionless) return Dimensionless renames Dimensionless_Elementary_Functions.Tanh;
      --  We could likely get away with a 1.0 - 2.0 / (e**(2.0 * X) + 1.0) instead of the above slower but correct
      --  implementation, but this code is called infrequently enough that it should not matter.

      function Elementwise_Sqrt (M : Real_Matrix) return Real_Matrix is
      begin
         return Result : Real_Matrix := M do
            for I in Result'Range (1) loop
               for J in Result'Range (2) loop
                  Result (I, J) := Dimensionless_Elementary_Functions.Sqrt (@);
               end loop;
            end loop;
         end return;
      end Elementwise_Sqrt;

      function Elementwise_Divide (A, B : Real_Matrix) return Real_Matrix is
      begin
         if A'Length (1) /= B'Length (1) or A'Length (2) /= B'Length (2) then
            raise Constraint_Error with "Matrices must have same dimensions.";
         end if;

         return Result : Real_Matrix := A do
            for I in Result'Range (1) loop
               for J in Result'Range (2) loop
                  Result (I, J) := A (I, J) / B (I, J);
               end loop;
            end loop;
         end return;
      end Elementwise_Divide;

      subtype Matrix_1_1 is Real_Matrix (1 .. 1, 1 .. 1);
      subtype Matrix_1_3 is Real_Matrix (1 .. 1, 1 .. 3);
      subtype Matrix_3_1 is Real_Matrix (1 .. 3, 1 .. 1);
      subtype Matrix_3_3 is Real_Matrix (1 .. 3, 1 .. 3);

      Proportional_Scale : Dimensionless;
      Integral_Scale     : Dimensionless;
      Derivative_Scale   : Dimensionless;

      Input_To_L1_Weights  : constant Matrix_3_1 := (others => (1 => 1.0 / 3.0));
      L1_To_L2_Weights     : Matrix_3_3          := (others => (others => 1.0));
      L2_To_Output_Weights : constant Matrix_1_3 := (1 => (others => 1.0));

      First_Moment_Decay_Rate        : constant Dimensionless := 0.9;
      Second_Moment_Decay_Rate       : constant Dimensionless := 0.999;
      Learning_Rate                  : constant Dimensionless := 0.000_05;
      Stable_Temperature             : constant Temperature   := 0.1 * celsius;
      Stable_Count_Before_Freeze     : constant Natural       := 40;
      Unstable_Count_Before_Training : constant Natural       := 4;

      First_Moment  : Matrix_3_3 := (others => (others => 0.0));
      Second_Moment : Matrix_3_3 := (others => (others => 0.0));

      Last_L2_Input  : Matrix_3_1 := (others => (1 => 0.0));
      Last_L2_Output : Matrix_3_1 := (others => (1 => 0.0));

      Last_Output      : Dimensionless := Get_PWM (Heater);
      Last_Temperature : Temperature   := -1_000_000.0 * celsius;

      Params : Heater_Parameters;

      Current_Temperature : Temperature;

      Stable_Count   : Natural     := 0;
      Unstable_Count : Natural     := 0;
      Current_Mode   : Mode_Kind   := Setpoint_Change;
      Last_Setpoint  : Temperature := Heater_Setpoint_Holders (Heater).Get;
   begin
      loop
         Params := Heater_Heater_Params (Heater).Get;
         if Params.Kind /= PID_Kind then
            return;
         end if;

         Proportional_Scale := Dimensionless (Params.Proportional_Scale);
         Integral_Scale     := Dimensionless (Params.Integral_Scale) * (hertz / Thermistors.Loop_Frequency);
         Derivative_Scale   := Dimensionless (Params.Derivative_Scale) / (hertz / Thermistors.Loop_Frequency);

         Heater_Update_Holders (Heater).Wait_Next_Reading (Current_Temperature);

         if Last_Temperature = -1_000_000.0 * celsius then
            --  Heater has only just been switched to PIDNN, or something is broken.
            Last_Temperature := Current_Temperature;
         end if;

         declare
            Setpoint : constant Temperature := Heater_Setpoint_Holders (Heater).Get;

            --  TODO: What if we also had the delta-t as an input instead of just the error?
            L1_Input  : constant Matrix_3_1 :=
              (Setpoint - Current_Temperature) / celsius * Input_To_L1_Weights;
            --  L1_Output : constant Matrix_3_1 := (for I in 1 .. 3 => (1 => Tanh (L1_Input (I, 1))));
            --  TODO: Report GNAT bug caused by above line.
            --  L1_Output : constant Matrix_3_1 :=
            --    (1 => (1 => Tanh (L1_Input (1, 1))),
            --     2 => (1 => Tanh (L1_Input (2, 1))),
            --     3 => (1 => Tanh (L1_Input (3, 1))));
            L1_Output : constant Matrix_3_1 := L1_Input;
            L2_Input  : constant Matrix_3_1 := L1_To_L2_Weights * L1_Output;
            L2_Output : constant Matrix_3_1 :=
              (1 => (1 => Proportional_Scale * L2_Input (1, 1)),
               2 => (1 => Last_L2_Output (2, 1) + Integral_Scale * L2_Input (2, 1)),
               3 => (1 => Derivative_Scale * (L2_Input (3, 1) - Last_L2_Input (3, 1))));
            L3_Input  : constant Matrix_1_1 := L2_To_Output_Weights * L2_Output;
            L3_Output : constant PWM_Scale  :=
              Dimensionless'Min (Dimensionless'Max (L3_Input (1, 1), PWM_Scale'First), PWM_Scale'Last);
         begin
            Set_PWM (Heater, L3_Output);

            if Setpoint /= Last_Setpoint or abs (Setpoint - Current_Temperature) > 2.0 * celsius then
               Current_Mode := Setpoint_Change;
            end if;

            case Current_Mode is
               when Setpoint_Change =>
                  Stable_Count   := 0;
                  Unstable_Count := 0;

                  if abs (Setpoint - Current_Temperature) < Stable_Temperature then
                     Current_Mode := Frozen_Weights;
                  end if;
               when Frozen_Weights =>
                  if abs (Setpoint - Current_Temperature) > Stable_Temperature then
                     Unstable_Count := @ + 1;
                     if Unstable_Count > Unstable_Count_Before_Training then
                        Current_Mode   := Training_Weights;
                        Stable_Count   := 0;
                        Unstable_Count := 0;
                     end if;
                  elsif Unstable_Count > 0 then
                     Unstable_Count := @ - 1;
                  end if;
               when Training_Weights =>
                  declare
                     D        : constant Dimensionless :=
                       ((Setpoint - Current_Temperature) / celsius) *
                       Tanh
                         ((Current_Temperature - Last_Temperature) / celsius *
                          (if L3_Output > Last_Output then 1.0 else -1.0)) *
                       (if L3_Output = PWM_Scale'First or L3_Output = PWM_Scale'Last then 0.0 else 1.0);
                     Gradient : constant Matrix_3_3    :=
                       Matrix_3_1'
                         (1 => (1 => L2_To_Output_Weights (1, 1) * D * Proportional_Scale),
                          2 => (1 => L2_To_Output_Weights (1, 2) * D * Integral_Scale),
                          3 => (1 => L2_To_Output_Weights (1, 3) * D * Derivative_Scale)) *
                       Transpose (L1_Output);
                  begin
                     First_Moment     :=
                       First_Moment_Decay_Rate * First_Moment + (1.0 - First_Moment_Decay_Rate) * Gradient;
                     Second_Moment    :=
                       Second_Moment_Decay_Rate * Second_Moment +
                       (1.0 - Second_Moment_Decay_Rate) * (Gradient * Gradient);
                     L1_To_L2_Weights :=
                       L1_To_L2_Weights -
                       Elementwise_Divide
                         ((First_Moment / (1.0 - First_Moment_Decay_Rate) * Learning_Rate),
                          Elementwise_Sqrt
                            (Second_Moment / (1.0 - Second_Moment_Decay_Rate) +
                             Matrix_3_3'(others => (others => 1.0E-8))));
                  end;

                  if abs (Setpoint - Current_Temperature) < Stable_Temperature then
                     Stable_Count := @ + 1;
                     if Stable_Count > Stable_Count_Before_Freeze then
                        Current_Mode   := Frozen_Weights;
                        Stable_Count   := 0;
                        Unstable_Count := 0;
                     end if;
                  else
                     Stable_Count := 0;
                  end if;
            end case;

            Last_L2_Input    := L2_Input;
            Last_L2_Output   := L2_Output;
            Last_Output      := L3_Output;
            Last_Setpoint    := Setpoint;
            Last_Temperature := Current_Temperature;

            Safety_Checker.Report_Updated (Heater, Current_Temperature);
         end;
      end loop;
   end PIDNN_Loop;

   procedure Disabled_Loop (Heater : Heater_Name) is
      Params : Heater_Parameters;

      Current_Temperature : Temperature;
   begin
      loop
         Params := Heater_Heater_Params (Heater).Get;
         if Params.Kind /= Disabled_Kind then
            return;
         end if;

         Heater_Update_Holders (Heater).Wait_Next_Reading (Current_Temperature);

         Safety_Checker.Report_Updated (Heater, Current_Temperature);
      end loop;
   end Disabled_Loop;

end Heaters;
