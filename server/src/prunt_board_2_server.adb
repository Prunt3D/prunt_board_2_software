-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2024 Liam Powell (liam@prunt3d.com)            --
--                                                                         --
--  This program is free software: you can redistribute it and/or modify   --
--  it under the terms of the GNU General Public License as published by   --
--  the Free Software Foundation, either version 3 of the License, or      --
--  (at your option) any later version.                                    --
--                                                                         --
--  This program is distributed in the hope that it will be useful,        --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          --
--  GNU General Public License for more details.                           --
--                                                                         --
--  You should have received a copy of the GNU General Public License      --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.  --
--                                                                         --
-----------------------------------------------------------------------------

with Prunt;       use Prunt;
with Prunt.Controller;
with Ada.Text_IO;
with Ada.Exceptions;
with GNAT.OS_Lib;
with Prunt.Controller_Generic_Types;
with Messages;    use Messages;
with Ada.Command_Line;
with Communications;
with GNAT.Serial_Communications;
with Prunt.Thermistors; use Prunt.Thermistors;
with Prunt.TMC_Types.TMC2240;
with Ada.Containers.Generic_Constrained_Array_Sort;
with Prunt.Heaters; use Prunt.Heaters;

use type Prunt.TMC_Types.TMC2240.UART_Node_Address;

procedure Prunt_Board_2_Server is

   Loop_Move_Multiplier : constant := 1024;

   package My_Controller_Generic_Types is new Prunt.Controller_Generic_Types
     (Stepper_Name      => Stepper_Name,
      Heater_Name       => Heater_Name,
      Thermistor_Name   => Thermistor_Name,
      Fan_Name          => Fan_Name,
      Input_Switch_Name => Input_Switch_Name);

   use My_Controller_Generic_Types;

   function "-" (Left, Right : Stepper_Position) return Stepper_Position is
   begin
      return (for I in Stepper_Name => Left (I) - Right (I));
   end "-";

   function "+" (Left, Right : Stepper_Position) return Stepper_Position is
   begin
      return (for I in Stepper_Name => Left (I) + Right (I));
   end "+";

   function "*" (Left : Stepper_Position; Right : Dimensionless) return Stepper_Position is
   begin
      return (for I in Stepper_Name => Left (I) * Right);
   end "*";

   function Rounding (Left : Stepper_Position) return Stepper_Position is
   begin
      return (for I in Stepper_Name => Dimensionless'Rounding (Left (I)));
   end Rounding;

   procedure Report_Error (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Report_Temperature (Thermistor : Thermistor_Name; Temp : Fixed_Point_Celcius);

   procedure Report_Heater_Power (Heater : Heater_Name; Power : Fixed_Point_PWM_Scale);

   procedure Report_Input_Swtich_State (Switch : Messages.Input_Switch_Name; State : Messages.Input_Switch_State);

   pragma Warnings (Off, "cannot call * before body seen");
   package My_Communications is new Communications
     (Report_Error, Report_Temperature, Report_Heater_Power, Report_Input_Swtich_State);
   pragma Warnings (On, "cannot call * before body seen");

   function Sort_Curve_By_ADC_Value_Comparator (Left, Right : Thermistor_Point) return Boolean is
   begin
      return Left.Value < Right.Value;
   end Sort_Curve_By_ADC_Value_Comparator;

   procedure Sort_Curve_By_ADC_Value is new Ada.Containers.Generic_Constrained_Array_Sort
     (Thermistor_Curve_Index, Thermistor_Point, Thermistor_Curve, Sort_Curve_By_ADC_Value_Comparator);

   procedure Setup
     (Heater_Thermistors : My_Controller_Generic_Types.Heater_Thermistor_Map;
      Thermistors        : Thermistor_Parameters_Array_Type)
   is
      Message : Message_From_Server_Content := (Kind => Setup_Kind, others => <>);
   begin
      for H in Heater_Name loop
         Message.Heater_Thermistors (H) := Heater_Thermistors (H);
      end loop;

      for T in Thermistor_Name loop
         for I in Thermistor_Curve_Index loop
            if Thermistors (T).Kind = Disabled_Kind then
               Message.Thermistor_Curves (T) (I).Value := 0;
               Message.Thermistor_Curves (T) (I).Temp  := Fixed_Point_Celcius'Last;
            else
               declare
                  Temp  : constant Temperature :=
                    Thermistors (T).Minimum_Temperature +
                    (Thermistors (T).Maximum_Temperature - Thermistors (T).Minimum_Temperature) /
                      (Dimensionless (Thermistor_Curve_Index'Last) - Dimensionless (Thermistor_Curve_Index'First)) *
                      (Dimensionless (I) - Dimensionless (Thermistor_Curve_Index'First));
                  R_Top : constant Resistance  := 2_000.0 * ohm;
                  R_Bot : constant Resistance  := Temperature_To_Resistance (Thermistors (T), Temp);
               begin
                  Message.Thermistor_Curves (T) (I).Value := ADC_Value (R_Bot / (R_Bot + R_Top) * (2.0**16 - 1.0));
                  Message.Thermistor_Curves (T) (I).Temp  := Fixed_Point_Celcius (Temp);
               end;
            end if;
         end loop;

         Sort_Curve_By_ADC_Value (Message.Thermistor_Curves (T));
      end loop;

      My_Communications.Runner.Send_Message (Message);

      for H in Heater_Name loop
         Message :=
           (Kind          => Heater_Reconfigure_Kind,
            Index         => <>,
            Heater        => H,
            Heater_Params => (Kind => Disabled_Kind, others => <>));
         My_Communications.Runner.Send_Message (Message);
      end loop;

      Message := (Kind => Enable_High_Power_Switch_Kind, Index => <>);
      My_Communications.Runner.Send_Message (Message);
   end Setup;

   procedure Reconfigure_Heater (Heater : Heater_Name; Params : Prunt.Heaters.Heater_Parameters) is
      Message : Message_From_Server_Content :=
        (Kind => Heater_Reconfigure_Kind, Index => <>, Heater => Heater, Heater_Params => <>);
   begin
      case Params.Kind is
         when Prunt.Heaters.Disabled_Kind =>
            Message.Heater_Params :=
              (Kind                       => Disabled_Kind,
               Check_Max_Cumulative_Error => Fixed_Point_Celcius (Params.Check_Max_Cumulative_Error),
               Check_Gain_Time            => Fixed_Point_Seconds (Params.Check_Gain_Time),
               Check_Minimum_Gain         => Fixed_Point_Celcius (Params.Check_Minimum_Gain),
               Check_Hysteresis           => Fixed_Point_Celcius (Params.Check_Hysteresis));
         when Prunt.Heaters.PID_Kind =>
            Message.Heater_Params :=
              (Kind                       => PID_Kind,
               Check_Max_Cumulative_Error => Fixed_Point_Celcius (Params.Check_Max_Cumulative_Error),
               Check_Gain_Time            => Fixed_Point_Seconds (Params.Check_Gain_Time),
               Check_Minimum_Gain         => Fixed_Point_Celcius (Params.Check_Minimum_Gain),
               Check_Hysteresis           => Fixed_Point_Celcius (Params.Check_Hysteresis),
               Proportional_Scale         => Fixed_Point_PID_Parameter (Params.Proportional_Scale),
               Integral_Scale             => Fixed_Point_PID_Parameter (Params.Integral_Scale),
               Derivative_Scale           => Fixed_Point_PID_Parameter (Params.Derivative_Scale));
         when Prunt.Heaters.Bang_Bang_Kind =>
            Message.Heater_Params :=
              (Kind                       => Bang_Bang_Kind,
               Check_Max_Cumulative_Error => Fixed_Point_Celcius (Params.Check_Max_Cumulative_Error),
               Check_Gain_Time            => Fixed_Point_Seconds (Params.Check_Gain_Time),
               Check_Minimum_Gain         => Fixed_Point_Celcius (Params.Check_Minimum_Gain),
               Check_Hysteresis           => Fixed_Point_Celcius (Params.Check_Hysteresis),
               Bang_Bang_Hysteresis       => Fixed_Point_Celcius (Params.Bang_Bang_Hysteresis));
         when Prunt.Heaters.PID_Autotune_Kind =>
            Message.Heater_Params :=
              (Kind                       => PID_Autotune_Kind,
               Check_Max_Cumulative_Error => Fixed_Point_Celcius (Params.Check_Max_Cumulative_Error),
               Check_Gain_Time            => Fixed_Point_Seconds (Params.Check_Gain_Time),
               Check_Minimum_Gain         => Fixed_Point_Celcius (Params.Check_Minimum_Gain),
               Check_Hysteresis           => Fixed_Point_Celcius (Params.Check_Hysteresis),
               Max_Cycles                 => Messages.PID_Autotune_Cycle_Count (Params.Max_Cycles),
               Proportional_Tuning_Factor => Fixed_Point_PID_Parameter (Params.Proportional_Tuning_Factor),
               Derivative_Tuning_Factor   => Fixed_Point_PID_Parameter (Params.Derivative_Tuning_Factor / hertz),
               PID_Tuning_Temperature     => Fixed_Point_Celcius (Params.PID_Tuning_Temperature));
      end case;

      My_Communications.Runner.Send_Message (Message);
   end Reconfigure_Heater;

   procedure Enable_Stepper (Stepper : Stepper_Name) is
   begin
      My_Communications.Runner.Send_Message ((Kind => Enable_Stepper_Kind, Index => <>, Stepper => Stepper));
   end Enable_Stepper;

   procedure Disable_Stepper (Stepper : Stepper_Name) is
   begin
      My_Communications.Runner.Send_Message ((Kind => Disable_Stepper_Kind, Index => <>, Stepper => Stepper));
   end Disable_Stepper;

   procedure Setup_For_Loop_Move (Switch : Input_Switch_Name; Hit_State : Pin_State) is
   begin
      My_Communications.Runner.Send_Message
        ((Kind              => Loop_Setup_Kind,
          Index             => <>,
          Loop_Input_Switch => Switch,
          Loop_Until_State  => (if Hit_State = Low_State then Low else High)));
   end Setup_For_Loop_Move;

   procedure Setup_For_Conditional_Move (Switch : Input_Switch_Name; Hit_State : Pin_State) is
   begin
      My_Communications.Runner.Send_Message
        ((Kind                  => Condition_Check_Kind,
          Index                 => <>,
          Conditon_Input_Switch => Switch,
          Skip_If_Hit_State     => (if Hit_State = Low_State then Low else High)));
   end Setup_For_Conditional_Move;

   Last_Enqueued_Command_Index : Command_Index := Command_Index'First with
     Atomic, Volatile;

   Last_Stepper_Position   : Stepper_Position := (others => 0.0);
   Last_Commanded_Position : Stepper_Position := (others => 0.0);

   Step_Delta_Message : aliased Message_From_Server_Content :=
     (Kind            => Regular_Step_Delta_List_Kind,
      Index           => <>,
      Last_Index      => Step_Delta_List_Index'First,
      Fan_Targets     => (others => 0.0),
      Heater_Targets  => (others => Fixed_Point_Celcius'First),
      Safe_Stop_After => False,
      Steps           => (others => (Steps => (others => 0), Dirs => (others => Forward))));

   procedure Enqueue_Command (Command : Queued_Command) is
      procedure Send_Message_And_Reset is
      begin
         My_Communications.Runner.Send_Message (Step_Delta_Message);

         Step_Delta_Message :=
           (Kind            => Regular_Step_Delta_List_Kind,
            Index           => <>,
            Last_Index      => Step_Delta_List_Index'First,
            Fan_Targets     => (others => 0.0),
            Heater_Targets  => (others => Fixed_Point_Celcius'First),
            Safe_Stop_After => False,
            Steps           => (others => (Steps => (others => 0), Dirs => (others => Forward))));
      end Send_Message_And_Reset;
   begin
      if Command.Loop_Until_Hit then
         if Step_Delta_Message.Last_Index /= Step_Delta_List_Index'First then
            Step_Delta_Message.Last_Index := @ - 1;
            Send_Message_And_Reset;
         end if;

         declare
            Total_Offset : constant Stepper_Position :=
              (Command.Pos - Last_Commanded_Position) * Dimensionless (Loop_Move_Multiplier);
         begin
            for S in Stepper_Name loop
               if Total_Offset (S) > 0.0 and Total_Offset (S) < 20.0 then
                  raise Constraint_Error with "Loop move direction vector error potentially greater than 5%.";
               end if;
            end loop;
         end;

         Step_Delta_Message :=
           (Kind            => Looping_Step_Delta_List_Kind,
            Index           => <>,
            Last_Index      => Step_Delta_List_Index'First + Loop_Move_Multiplier - 1,
            Fan_Targets     => (others => 0.0),
            Heater_Targets  => (others => Fixed_Point_Celcius'First),
            Safe_Stop_After => False,
            Steps           => (others => (Steps => (others => 0), Dirs => (others => Forward))));

         Step_Delta_Message.Heater_Targets := (for H in Heater_Name => Fixed_Point_Celcius (Command.Heaters (H)));
         Step_Delta_Message.Fan_Targets    := (for F in Fan_Name => Fixed_Point_PWM_Scale (Command.Fans (F)));

         declare
            Last_Rounded_Offset : Stepper_Position := (others => 0.0);
         begin
            for I in Step_Delta_List_Index range 1 .. Loop_Move_Multiplier loop
               declare
                  Unrounded_Offset : constant Stepper_Position :=
                    (Command.Pos - Last_Commanded_Position) * Dimensionless (I);
                  Delta_Offset     : constant Stepper_Position := Rounding (Unrounded_Offset - Last_Rounded_Offset);
               begin
                  Last_Rounded_Offset := Rounding (Unrounded_Offset);
                  for X of Delta_Offset loop
                     if abs X > Dimensionless (Step_Count'Last) then
                        raise Constraint_Error with "Step rate too high. Delta_Offset = " & Delta_Offset'Image;
                        --  TODO: Add a way to ensure that this will never occur based on the configuration.
                     end if;
                  end loop;

                  Step_Delta_Message.Steps (Step_Delta_List_Index'First + I - 1).Steps :=
                    (for J in Stepper_Name => Step_Count (abs Delta_Offset (J)));
                  Step_Delta_Message.Steps (Step_Delta_List_Index'First + I - 1).Dirs :=
                    (for J in Stepper_Name => (if Delta_Offset (J) >= 0.0 then Forward else Backward));
               end;
            end loop;
         end;

         Send_Message_And_Reset;

         Last_Stepper_Position := Rounding (Command.Pos);
         --  TODO: Take error between stepper position and commanded position in to account. It is unlikely that this
         --  will ever matter in practice, but it would be nice to have.
      else
         declare
            Offset : constant Stepper_Position := Rounding (Command.Pos - Last_Stepper_Position);
         begin
            for X of Offset loop
               if abs X > Dimensionless (Step_Count'Last) then
                  raise Constraint_Error with "Step rate too high. Offset = " & Offset'Image;
                  --  TODO: Add a way to ensure that this will never occur based on the configuration.
               end if;
            end loop;

            Step_Delta_Message.Steps (Step_Delta_Message.Last_Index).Steps :=
              (for I in Stepper_Name => Step_Count (abs Offset (I)));
            Step_Delta_Message.Steps (Step_Delta_Message.Last_Index).Dirs :=
              (for I in Stepper_Name => (if Offset (I) >= 0.0 then Forward else Backward));

            Step_Delta_Message.Heater_Targets := (for H in Heater_Name => Fixed_Point_Celcius (Command.Heaters (H)));
            Step_Delta_Message.Fan_Targets    := (for F in Fan_Name => Fixed_Point_PWM_Scale (Command.Fans (F)));

            if Command.Safe_Stop_After then
               Step_Delta_Message.Safe_Stop_After := True;
               Send_Message_And_Reset;
            elsif Step_Delta_Message.Last_Index = Step_Delta_List_Index'Last then
               Send_Message_And_Reset;
            else
               Step_Delta_Message.Last_Index := @ + 1;
            end if;

            Last_Stepper_Position := @ + Offset;
         end;
      end if;

      Last_Commanded_Position     := Command.Pos;
      Last_Enqueued_Command_Index := Command.Index;
   end Enqueue_Command;

   procedure Reset_Position (Pos : Stepper_Position) is
   begin
      Last_Commanded_Position := Pos;
      Last_Stepper_Position   := Rounding (Pos);
      --  TODO: Take error between stepper position and commanded position in to account. It is unlikely that this will
      --  ever matter in practice, but it would be nice to have.
   end Reset_Position;

   procedure Wait_Until_Idle (Last_Command : Command_Index) is
      Reply : Message_From_Client_Content;
   begin
      loop
         exit when Last_Enqueued_Command_Index >= Last_Command;
      end loop;

      loop
         My_Communications.Runner.Send_Message_And_Wait_For_Reply ((Kind => Check_If_Idle_Kind, Index => <>), Reply);
         exit when Reply.Condition_Met;
      end loop;
   end Wait_Until_Idle;

   procedure TMC_Write (Message : Prunt.TMC_Types.TMC2240.UART_Data_Byte_Array) is
   begin
      My_Communications.Runner.Send_Message
        ((Kind           => TMC_Write_Kind,
          Index          => <>,
          TMC_Write_Data =>
            (for I in Messages.TMC2240_UART_Data_Byte_Array'Range => Messages.TMC2240_UART_Byte (Message (9 - I)))));
   end TMC_Write;

   procedure TMC_Read
     (Message        :     Prunt.TMC_Types.TMC2240.UART_Query_Byte_Array;
      Receive_Failed : out Boolean;
      Reply          : out Prunt.TMC_Types.TMC2240.UART_Data_Byte_Array)
   is
      Client_Reply : Message_From_Client_Content;
   begin
      My_Communications.Runner.Send_Message_And_Wait_For_Reply
        ((Kind          => TMC_Read_Kind,
          Index         => <>,
          TMC_Read_Data =>
           (for I in Messages.TMC2240_UART_Query_Byte_Array'Range => Messages.TMC2240_UART_Byte (Message (5 - I)))),
         Client_Reply);

      if Client_Reply.Kind /= TMC_Read_Reply_Kind then
         raise Constraint_Error with "Received wrong reply type.";
      end if;

      Receive_Failed := Boolean (Client_Reply.TMC_Receive_Failed);
      Reply := (for I in Reply'Range => Prunt.TMC_Types.TMC2240.UART_Byte (Client_Reply.TMC_Data (9 - I)));
   end TMC_Read;

   procedure Autotune_Heater (Heater : Heater_Name; Params : Prunt.Heaters.Heater_Parameters) is
      Reply : Message_From_Client_Content;
   begin
      Reconfigure_Heater (Heater, Params);

      loop
         My_Communications.Runner.Send_Message_And_Wait_For_Reply
           ((Kind => Check_If_Heater_Autotune_Done_Kind, Index => <>, Heater_To_Check => Heater), Reply);
         exit when Reply.Condition_Met;
      end loop;
   end Autotune_Heater;

   Stepper_UART_Address : constant array (Stepper_Name) of Prunt.TMC_Types.TMC2240.UART_Node_Address :=
     (Stepper_1 => 6, Stepper_2 => 4, Stepper_3 => 3, Stepper_4 => 2, Stepper_5 => 5, Stepper_6 => 6);

   package My_Controller is new Prunt.Controller
     (Generic_Types              => My_Controller_Generic_Types,
      Stepper_Hardware           =>
        (for I in Messages.Stepper_Name =>
           (Kind                   => TMC2240_UART_Kind,
            Enable_Stepper         => Enable_Stepper'Access,
            Disable_Stepper        => Disable_Stepper'Access,
            TMC2240_UART_Address   => Messages.Stepper_Name'Pos (I) + 1,
            TMC2240_UART_Write     => TMC_Write'Access,
            TMC2240_UART_Read      => TMC_Read'Access)),
      Interpolation_Time         => 58_490.0 / 1_200_000_000.0 * s,
      Loop_Interpolation_Time    => 58_490.0 / 1_200_000_000.0 * s,
      Setup                      => Setup,
      Reconfigure_Heater         => Reconfigure_Heater,
      Autotune_Heater            => Autotune_Heater,
      Setup_For_Loop_Move        => Setup_For_Loop_Move,
      Setup_For_Conditional_Move => Setup_For_Conditional_Move,
      Enqueue_Command            => Enqueue_Command,
      Reset_Position             => Reset_Position,
      Wait_Until_Idle            => Wait_Until_Idle,
      Config_Path                => "./prunt_board_2.toml",
      Command_Generator_CPU      => 3);

   procedure Report_Error (Occurrence : Ada.Exceptions.Exception_Occurrence) is
   begin
      My_Controller.Report_External_Error (Occurrence);
   end Report_Error;

   procedure Report_Temperature (Thermistor : Messages.Thermistor_Name; Temp : Fixed_Point_Celcius) is
   begin
      My_Controller.Report_Temperature (Thermistor, Temperature (Temp));
   end Report_Temperature;

   procedure Report_Heater_Power (Heater : Messages.Heater_Name; Power : Fixed_Point_PWM_Scale) is
   begin
      My_Controller.Report_Heater_Power (Heater, PWM_Scale (Power));
   end Report_Heater_Power;

   procedure Report_Input_Swtich_State (Switch : Messages.Input_Switch_Name; State : Messages.Input_Switch_State) is
   begin
      My_Controller.Report_Input_Switch_State (Switch, (if State = High then High_State else Low_State));
   end Report_Input_Swtich_State;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      raise Constraint_Error with "Usage: " & Ada.Command_Line.Command_Name & " <serial port path>";
   end if;

   My_Communications.Runner.Init (GNAT.Serial_Communications.Port_Name (Ada.Command_Line.Argument (1)));

   My_Controller.Run;

exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
      GNAT.OS_Lib.OS_Abort;
end Prunt_Board_2_Server;
