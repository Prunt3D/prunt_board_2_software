with System;

package Messages is

   type Heater_Name is (Heater_1, Heater_2) with
     Size => 8;
   type Fan_Name is (Fan_1, Fan_2, Fan_3, Fan_4) with
     Size => 8;
   type Stepper_Name is (Stepper_1, Stepper_2, Stepper_3, Stepper_4, Stepper_5, Stepper_6) with
     Size => 8;
   type Input_Switch_Name is
     (Endstop_1,
      Endstop_2,
      Endstop_3,
      Endstop_4,
      Stepper_1_Diag_0,
      Stepper_2_Diag_0,
      Stepper_3_Diag_0,
      Stepper_4_Diag_0,
      Stepper_5_Diag_0,
      Stepper_6_Diag_0) with
     Size => 8;
   type Thermistor_Name is (Thermistor_1, Thermistor_2, Thermistor_3, Thermistor_4) with
     Size => 8;

   type Byte_Boolean is new Boolean with
     Size => 8;
   for Byte_Boolean use (False => 0, True => 2#1100_0101#);

   type TMC2240_UART_Byte is mod 2**8 with
     Size => 8;
   type TMC2240_UART_Data_Byte_Array is array (1 .. 8) of TMC2240_UART_Byte with
     Pack;
   type TMC2240_UART_Query_Byte_Array is array (1 .. 4) of TMC2240_UART_Byte with
     Pack;

   type Client_Version is mod 2**32 with
     Size => 32;

   type Client_ID_Part is mod 2**32 with
     Size => 32;

   type Client_ID is array (1 .. 4) of Client_ID_Part with
     Pack, Scalar_Storage_Order => System.Low_Order_First;

   type Input_Switch_State is (Low, High) with
     Size => 8;

   type ADC_Value is mod 2**16 with
     Size => 16;

   type Fixed_Point_PWM_Scale is delta 2.0**(-14) range 0.0 .. 1.0 with
     Size => 16;

   type CRC32 is mod 2**32 with
     Size => 32;

   type Message_Index is mod 2**64 with
     Size => 64;

   type Step_Count is range 0 .. 100 with
     Size => 7;

   type Step_Delta_List_Index is mod 2**11 with
     Size => 16;

   type Step_Delta_Steps is array (Stepper_Name) of Step_Count with
     Scalar_Storage_Order => System.Low_Order_First, Component_Size => 7, Size => 42;

   type Direction is (Forward, Backward) with
     Size => 1;

   type Step_Delta_Dirs is array (Stepper_Name) of Direction with
     Scalar_Storage_Order => System.Low_Order_First, Component_Size => 1, Size => 6;

   type Step_Delta is record
      Dirs  : Step_Delta_Dirs;
      Steps : Step_Delta_Steps;
   end record with
     Scalar_Storage_Order => System.Low_Order_First, Bit_Order => System.Low_Order_First, Size => 48;

   for Step_Delta use record
      Dirs  at 0 range 0 ..  5;
      Steps at 0 range 6 .. 47;
   end record;

   type Step_Delta_List is array (Step_Delta_List_Index) of Step_Delta with
     Pack, Scalar_Storage_Order => System.Low_Order_First;

   type Fan_Target_List is array (Fan_Name) of Fixed_Point_PWM_Scale with
     Pack, Scalar_Storage_Order => System.Low_Order_First;

   type Heater_Thermistor_Map is array (Heater_Name) of Thermistor_Name;

   type Heater_Kind is (Disabled_Kind, PID_Kind, Bang_Bang_Kind, PID_Autotune_Kind) with
     Size => 8;
     --  type Heater_Kind is (Disabled_Kind, PID_Kind, Bang_Bang_Kind, PID_Autotune_Kind, MPC_Kind, MPC_Autotune_Kind) with
     --    Size => 8;

   type Fixed_Point_Celcius is delta 2.0**(-5) range -1_000.0 .. 1_000.0 with
     Size => 16;

   type Fixed_Point_Seconds is delta 2.0**(-5) range 0.0 .. 2_000.0 with
     Size => 16;

   type Fixed_Point_Heat_Capacity is delta 2.0**(-2) range 0.0 .. 16_000.0 with
     Size => 16;

   type Fixed_Point_Thermal_Conductivity is delta 2.0**(-11) range 0.0 .. 31.0 with
     Size => 16;

   type Fixed_Point_Power is delta 2.0**(-6) range 0.0 .. 1_000.0 with
     Size => 16;

   type Fixed_Point_Smoothing_Factor is delta 2.0**(-15) range 0.0 .. 1.0 with
     Size => 16;

     --  TODO: Get some measurements and tune this to suit all heater setups.
   type Fixed_Point_Frequency is delta 2.0**(-5) range 0.0 .. 2_000.0 with
     Size => 16;

   type Fixed_Point_Temperature_Over_Time is delta 2.0**(-9) range 0.0 .. 127.0 with
     Size => 16;

   type Heater_Target_List is array (Heater_Name) of Fixed_Point_Celcius with
     Pack, Scalar_Storage_Order => System.Low_Order_First;

   type Fixed_Point_PID_Parameter is delta 2.0**(-18) range 0.0 .. 8_000.0 with
     Size => 32;

   type PID_Autotune_Cycle_Count is range 2 .. 1_000 with
     Size => 16;

   type Heater_Parameters (Kind : Heater_Kind := Disabled_Kind) is record
      Check_Max_Cumulative_Error : Fixed_Point_Celcius;
      Check_Gain_Time            : Fixed_Point_Seconds;
      Check_Minimum_Gain         : Fixed_Point_Celcius;
      Check_Hysteresis           : Fixed_Point_Celcius;
      case Kind is
         when Disabled_Kind =>
            null;
         when Bang_Bang_Kind =>
            Bang_Bang_Hysteresis : Fixed_Point_Celcius;
         when PID_Kind =>
            Proportional_Scale : Fixed_Point_PID_Parameter;
            Integral_Scale     : Fixed_Point_PID_Parameter;
            Derivative_Scale   : Fixed_Point_PID_Parameter;
         when PID_Autotune_Kind =>
            Max_Cycles                 : PID_Autotune_Cycle_Count;
            PID_Tuning_Temperature     : Fixed_Point_Celcius;
            Proportional_Tuning_Factor : Fixed_Point_PID_Parameter;
            Derivative_Tuning_Factor   : Fixed_Point_PID_Parameter;
            --  when MPC_Kind =>
            --     Block_Heat_Capacity            : Fixed_Point_Heat_Capacity;
            --     Ambient_Transfer               : Fixed_Point_Thermal_Conductivity;
            --     Full_Fan_Ambient_Transfer      : Fixed_Point_Thermal_Conductivity;
            --     Target_Reach_Time              : Fixed_Point_Seconds;
            --     Heater_Power                   : Fixed_Point_Power;
            --     Smoothing                      : Fixed_Point_Smoothing_Factor;
            --     Sensor_Responsiveness          : Fixed_Point_Frequency;
            --     Min_Ambient_Change             : Fixed_Point_Temperature_Over_Time;
            --     Steady_State_Rate              : Fixed_Point_Temperature_Over_Time;
            --     Filament_Heat_Capacity         : Fixed_Point_Lengthwise_Heat_Capacity;
            --     Extruder_Distance_Per_Step     : Fixed_Point_Length;
            --     Extruder_Stepper               : Stepper_Name;
            --     Extruder_Energy_Loss_Direction : Direction;
            --     Cooling_Fan                    : Fan_Name;
      end case;
   end record with
     Scalar_Storage_Order => System.Low_Order_First, Bit_Order => System.Low_Order_First, Size => 192;

     --  TODO: Regenerate this.
   for Heater_Parameters use record
      Kind                       at  0 range 0 ..  7;
      Check_Max_Cumulative_Error at  2 range 0 .. 15;
      Check_Gain_Time            at  4 range 0 .. 15;
      Check_Minimum_Gain         at  6 range 0 .. 15;
      Check_Hysteresis           at  8 range 0 .. 15;
      Bang_Bang_Hysteresis       at 12 range 0 .. 15;
      Proportional_Scale         at 12 range 0 .. 31;
      Integral_Scale             at 16 range 0 .. 31;
      Derivative_Scale           at 20 range 0 .. 31;
      Max_Cycles                 at 12 range 0 .. 15;
      PID_Tuning_Temperature     at 14 range 0 .. 15;
      Proportional_Tuning_Factor at 16 range 0 .. 31;
      Derivative_Tuning_Factor   at 20 range 0 .. 31;
   end record;

   type Thermistor_Point is record
      Temp  : Fixed_Point_Celcius;
      Value : ADC_Value;
   end record with
     Scalar_Storage_Order => System.Low_Order_First, Bit_Order => System.Low_Order_First;

   for Thermistor_Point use record
      Temp  at 0 range 0 .. 31;
      Value at 4 range 0 .. 15;
   end record;

   type Thermistor_Curve_Index is range 1 .. 512;

   type Thermistor_Curve is array (Thermistor_Curve_Index) of Thermistor_Point with
     Pack, Scalar_Storage_Order => System.Low_Order_First;

   type Thermistor_Curves_Array is array (Thermistor_Name) of Thermistor_Curve with
     Pack, Scalar_Storage_Order => System.Low_Order_First;

   type Reported_Temperatures is array (Thermistor_Name) of Fixed_Point_Celcius with
     Pack, Scalar_Storage_Order => System.Low_Order_First;

   type Reported_Heater_PWMs is array (Heater_Name) of Fixed_Point_PWM_Scale with
     Pack, Scalar_Storage_Order => System.Low_Order_First;

   type Reported_Switch_States is array (Input_Switch_Name) of Input_Switch_State with
     Pack, Scalar_Storage_Order => System.Low_Order_First;

   type Tach_Counter is range 0 .. 2**16 - 1 with
     Size => 16;

   type Reported_Tach_Counters is array (Fan_Name) of Tach_Counter with
     Pack, Scalar_Storage_Order => System.Low_Order_First;

   type Message_From_Server_Kind is
     (Setup_Kind,
      Heater_Reconfigure_Kind,
      Loop_Setup_Kind,
      Regular_Step_Delta_List_Kind,
      Looping_Step_Delta_List_Kind,
      Condition_Check_Kind,
      TMC_Write_Kind,
      TMC_Read_Kind,
      Status_Kind,
      Check_If_Idle_Kind,
      Check_If_Heater_Autotune_Done_Kind,
      Enable_Stepper_Kind,
      Disable_Stepper_Kind,
      Enable_High_Power_Switch_Kind,
      Disable_High_Power_Switch_Kind) with
     Size => 8;

   type Message_From_Server_Content (Kind : Message_From_Server_Kind := Setup_Kind) is record
      Index : Message_Index;
      case Kind is
         when Setup_Kind =>
            Heater_Thermistors : Heater_Thermistor_Map;
            Thermistor_Curves  : Thermistor_Curves_Array;
         when Heater_Reconfigure_Kind =>
            Heater        : Heater_Name;
            Heater_Params : Heater_Parameters;
         when Loop_Setup_Kind =>
            Loop_Input_Switch : Input_Switch_Name;
            Loop_Until_State  : Input_Switch_State;
         when Regular_Step_Delta_List_Kind | Looping_Step_Delta_List_Kind =>
            Fan_Targets     : Fan_Target_List;
            Heater_Targets  : Heater_Target_List;
            Last_Index      : Step_Delta_List_Index;
            Safe_Stop_After : Byte_Boolean;
            Steps           : Step_Delta_List;
         when Condition_Check_Kind =>
            --  Skip all Step_Lists until the next time Safe_Stop_After is True.
            Conditon_Input_Switch : Input_Switch_Name;
            Skip_If_Hit_State     : Input_Switch_State;
         when TMC_Write_Kind =>
            TMC_Write_Data : TMC2240_UART_Data_Byte_Array;
         when TMC_Read_Kind =>
            TMC_Read_Data : TMC2240_UART_Query_Byte_Array;
         when Status_Kind =>
            null;
         when Check_If_Idle_Kind =>
            null;
         when Check_If_Heater_Autotune_Done_Kind =>
            Heater_To_Check : Heater_Name;
         when Enable_Stepper_Kind | Disable_Stepper_Kind =>
            Stepper : Stepper_Name;
         when Enable_High_Power_Switch_Kind | Disable_High_Power_Switch_Kind =>
            null;
      end case;
   end record with
     Scalar_Storage_Order => System.Low_Order_First, Bit_Order => System.Low_Order_First, Size => 98_560;

   for Message_From_Server_Content use record
      Kind                  at  0 range 0 ..      7;
      Index                 at  8 range 0 ..     63;
      Heater_Thermistors    at 16 range 0 ..     15;
      Thermistor_Curves     at 18 range 0 .. 98_303;
      Heater                at 16 range 0 ..      7;
      Heater_Params         at 20 range 0 ..    191;
      Loop_Input_Switch     at 16 range 0 ..      7;
      Loop_Until_State      at 17 range 0 ..      7;
      Fan_Targets           at 16 range 0 ..     63;
      Heater_Targets        at 24 range 0 ..     31;
      Last_Index            at 28 range 0 ..     15;
      Safe_Stop_After       at 30 range 0 ..      7;
      Steps                 at 31 range 0 .. 98_303;
      Conditon_Input_Switch at 16 range 0 ..      7;
      Skip_If_Hit_State     at 17 range 0 ..      7;
      TMC_Write_Data        at 16 range 0 ..     63;
      TMC_Read_Data         at 16 range 0 ..     31;
      Heater_To_Check       at 16 range 0 ..      7;
      Stepper               at 16 range 0 ..      7;
   end record;

   type Message_From_Server is record
      Checksum : CRC32;
      Content  : Message_From_Server_Content;
   end record with
     Scalar_Storage_Order => System.Low_Order_First, Bit_Order => System.Low_Order_First, Size => 3_082 * 32;
     --  Size should always be a multiple of 32 to allow for 32-bit CRC inputs on STM32.

   for Message_From_Server use record
      Checksum at 0 range 0 ..     31;
      Content  at 8 range 0 .. 98_559;
   end record;

   type Message_From_Client_Kind is (Hello_Kind, Status_Kind, TMC_Read_Reply_Kind, Check_Reply_Kind) with
     Size => 8;

   type Message_From_Client_Content (Kind : Message_From_Client_Kind := Hello_Kind) is record
      Index        : Message_Index;
      Temperatures : Reported_Temperatures;
      Heaters      : Reported_Heater_PWMs;
      Switches     : Reported_Switch_States;
      Tachs        : Reported_Tach_Counters;
      case Kind is
         when Hello_Kind =>
            Version : Client_Version;
            ID      : Client_ID;
         when Status_Kind =>
            null;
         when TMC_Read_Reply_Kind =>
            TMC_Receive_Failed : Byte_Boolean;
            TMC_Data           : TMC2240_UART_Data_Byte_Array;
         when Check_Reply_Kind =>
            Condition_Met : Byte_Boolean;
      end case;
   end record with
     Scalar_Storage_Order => System.Low_Order_First, Bit_Order => System.Low_Order_First, Size => 576;

   for Message_From_Client_Content use record
      Kind               at  0 range 0 ..   7;
      Index              at  8 range 0 ..  63;
      Temperatures       at 16 range 0 ..  63;
      Heaters            at 24 range 0 ..  31;
      Switches           at 28 range 0 ..  79;
      Tachs              at 38 range 0 ..  63;
      Version            at 48 range 0 ..  31;
      ID                 at 52 range 0 .. 127;
      TMC_Receive_Failed at 48 range 0 ..   7;
      TMC_Data           at 49 range 0 ..  63;
      Condition_Met      at 48 range 0 ..   7;
   end record;

   type Message_From_Client is record
      Checksum : CRC32;
      Content  : Message_From_Client_Content;
   end record with
     Scalar_Storage_Order => System.Low_Order_First, Bit_Order => System.Low_Order_First, Size => 20 * 32;
     --  Size should always be a multiple of 32 to allow for 32-bit CRC inputs on STM32.

   for Message_From_Client use record
      Checksum at 0 range 0 ..  31;
      Content  at 8 range 0 .. 575;
   end record;

end Messages;
