with Messages;               use Messages;
with Hardware_Configuration; use Hardware_Configuration;
with Ada.Real_Time;          use Ada.Real_Time;
with Physical_Types;         use Physical_Types;
with Ada.Numerics.Generic_Real_Arrays;
with Ada.Numerics.Generic_Elementary_Functions;
with Init_Checkers;

package Heaters is

   procedure Init;

   procedure Make_Safe;
   --  May be called before Init.

   procedure Setup (Heater : Heater_Name; Parameters : Heater_Parameters);
   --  Must not be called from ISRs.

   procedure Set_Setpoint (Heater : Heater_Name; Setpoint : Temperature);
   --  Must not be called from ISRs.

   function Get_PWM (Heater : Heater_Name) return Fixed_Point_PWM_Scale;

   function Check_If_Autotune_Done (Heater : Heater_Name) return Boolean;

   procedure Update_Reading (Heater : Heater_Name; Current_Temperature : Temperature);

   Heater_Check_Failure : exception;

private

   package Dimensionless_Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (Dimensionless);
   package Dimensionless_Real_Arrays is new Ada.Numerics.Generic_Real_Arrays (Dimensionless);

   Init_Checker : Init_Checkers.Init_Checker;

   protected type Heater_Update_Holder with
     Priority => Thermistor_DMA_Interrupt_Priority
   is
      procedure Set_Update (Temp : Temperature);
      entry Wait_Next_Reading (Temp : out Temperature);
   private
      Reading      : Temperature;
      Update_Ready : Boolean := False;
   end Heater_Update_Holder;

   Heater_Update_Holders : array (Heater_Name) of Heater_Update_Holder;

   protected type Heater_Params_Holder is
      procedure Set (Params : Heater_Parameters);
      function Get return Heater_Parameters;
   private
      Data : Heater_Parameters :=
        (Kind                       => Disabled_Kind,
         Check_Max_Cumulative_Error => 0.0,
         Check_Gain_Time            => 0.0,
         Check_Minimum_Gain         => 0.0,
         Check_Hysteresis           => 0.0);
   end Heater_Params_Holder;

   Heater_Heater_Params : array (Heater_Name) of Heater_Params_Holder;

   protected type Setpoint_Holder is
      procedure Set (Setpoint : Temperature);
      function Get return Temperature;
   private
      Data : Temperature := 0.0 * celsius;
   end Setpoint_Holder;

   Heater_Setpoint_Holders : array (Heater_Name) of Setpoint_Holder;

   task type Heater_Controller (Heater : Heater_Name := Heater_Name'First) with
     Storage_Size => 5 * 1_024
   is
   end Heater_Controller;

   Heater_Controller_1 : Heater_Controller (Heater_1);
   Heater_Controller_2 : Heater_Controller (Heater_2);

   type Safety_Checker_Context is record
      Updated_Since_Last_Reset : Boolean;
      Approaching_Setpoint     : Boolean;
      Starting_Approach        : Boolean;
      Cumulative_Error         : Temperature;
      Last_Setpoint            : Temperature;
      Goal_Temp                : Temperature;
      Goal_Time                : Ada.Real_Time.Time;
   end record;

   type Safety_Checker_Contexts_Array is array (Heater_Name) of Safety_Checker_Context;

   protected Safety_Checker is
      procedure Report_Updated (Updated_Heater : Heater_Name; Current_Temp : Temperature);
   private
      Contexts : Safety_Checker_Contexts_Array :=
        (others =>
           (Updated_Since_Last_Reset => False,
            Approaching_Setpoint     => False,
            Starting_Approach        => False,
            Cumulative_Error         => 0.0 * celsius,
            Last_Setpoint            => 0.0 * celsius,
            Goal_Temp                => 0.0 * celsius,
            Goal_Time                => Clock));
   end Safety_Checker;

   procedure Set_PWM (Heater : Heater_Name; Scale : PWM_Scale; Override_Forced_Zero : Boolean := False) with
     Pre => Scale >= 0.0 and Scale <= 1.0;
   function Get_PWM (Heater : Heater_Name) return Dimensionless with
     Post => Get_PWM'Result >= 0.0 and Get_PWM'Result <= 1.0;

   --  procedure MPC_Loop (Heater : Heater_Name);
   --  procedure MPC_Autotune_Loop (Heater : Heater_Name);
   procedure PID_Loop (Heater : Heater_Name);
   procedure PIDNN_Loop (Heater : Heater_Name);
   procedure PID_Autotune_Loop (Heater : Heater_Name);
   procedure Bang_Bang_Loop (Heater : Heater_Name);
   procedure Disabled_Loop (Heater : Heater_Name);

end Heaters;
