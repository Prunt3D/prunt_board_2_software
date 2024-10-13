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
     Size => 8 * 8, Component_Size => 8, Scalar_Storage_Order => System.Low_Order_First;
   type TMC2240_UART_Query_Byte_Array is array (1 .. 4) of TMC2240_UART_Byte with
     Size => 4 * 8, Component_Size => 8, Scalar_Storage_Order => System.Low_Order_First;

   type Client_Version is mod 2**32 with
     Size => 32;

   type Client_ID_Part is mod 2**32 with
     Size => 32;

   type Client_ID is array (1 .. 4) of Client_ID_Part with
     Size => 32 * 4, Component_Size => 32, Scalar_Storage_Order => System.Low_Order_First;

     --  Change the below client ID if you are porting this code to a new board. The following command may be used to
     --  generate a random ID:
     --  hexdump -vn32 -e '1/4 "16#" "%02X" "#, "' /dev/urandom && echo ""
   DO_NOT_COPY_THIS_CLIENT_ID_AS_IT_IS_MEANT_TO_IDENTIFY_THIS_PARTICULAR_BOARD_MODEL_AND_FIRMWARE :
     constant Client_ID :=
     (16#14BC_80C3#, 16#53B1_4CAC#, 16#DE61_09E7#, 16#6BC8_2ECD#);

   type Input_Switch_State is (Low, High) with
     Size => 8;

   type ADC_Value is mod 2**24 with
     Size => 24;

   type Fixed_Point_PWM_Scale is delta 2.0**(-14) range 0.0 .. 1.0 with
     Size => 16, Small => 2.0**(-14);

   type CRC32 is mod 2**32 with
     Size => 32;

   type Message_Index is mod 2**64 with
     Size => 64;

   type Step_Count is range 0 .. 127 with
     Size => 7;

   type Step_Delta_List_Index is range 1 .. 2_048 with
     Size => 16;

   type Step_Delta_Steps is array (Stepper_Name) of Step_Count with
     Size => 7 * 6, Component_Size => 7, Scalar_Storage_Order => System.Low_Order_First;

   type Direction is (Forward, Backward) with
     Size => 1;

   type Step_Delta_Dirs is array (Stepper_Name) of Direction with
     Size => 6, Component_Size => 1, Scalar_Storage_Order => System.Low_Order_First;

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
     Size => 48 * 2_048, Component_Size => 48, Scalar_Storage_Order => System.Low_Order_First;

   type Fan_Target_List is array (Fan_Name) of Fixed_Point_PWM_Scale with
     Size => 16 * 4, Component_Size => 16, Scalar_Storage_Order => System.Low_Order_First;

   type Heater_Thermistor_Map is array (Heater_Name) of Thermistor_Name with
     Size => 8 * 2, Component_Size => 8, Scalar_Storage_Order => System.Low_Order_First;

   type Heater_Kind is (Disabled_Kind, PID_Kind, Bang_Bang_Kind, PID_Autotune_Kind) with
     Size => 8;

   type Fixed_Point_Celcius is delta 2.0**(-13) range -1_000.0 .. 1_000.0 with
     Size => 24, Small => 2.0**(-13);

   type Fixed_Point_Seconds is delta 2.0**(-5) range 0.0 .. 2_000.0 with
     Size => 16, Small => 2.0**(-5);

   type Heater_Target_List is array (Heater_Name) of Fixed_Point_Celcius with
     Size => 48, Component_Size => 24, Scalar_Storage_Order => System.Low_Order_First;

   type Fixed_Point_PID_Parameter is delta 2.0**(-18) range 0.0 .. 8_000.0 with
     Size => 32, Small => 2.0**(-18);

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
      end case;
   end record with
     Scalar_Storage_Order => System.Low_Order_First, Bit_Order => System.Low_Order_First, Size => 288;

   for Heater_Parameters use record
      Kind                       at  0 range 0 ..  7;
      Check_Max_Cumulative_Error at  4 range 0 .. 31;
      Check_Gain_Time            at  8 range 0 .. 15;
      Check_Minimum_Gain         at 12 range 0 .. 31;
      Check_Hysteresis           at 16 range 0 .. 31;
      Bang_Bang_Hysteresis       at 20 range 0 .. 31;
      Proportional_Scale         at 20 range 0 .. 31;
      Integral_Scale             at 24 range 0 .. 31;
      Derivative_Scale           at 28 range 0 .. 31;
      Max_Cycles                 at 20 range 0 .. 15;
      PID_Tuning_Temperature     at 24 range 0 .. 31;
      Proportional_Tuning_Factor at 28 range 0 .. 31;
      Derivative_Tuning_Factor   at 32 range 0 .. 31;
   end record;

   type Thermistor_Point is record
      Temp  : Fixed_Point_Celcius;
      Value : ADC_Value;
   end record with
     Scalar_Storage_Order => System.Low_Order_First, Bit_Order => System.Low_Order_First, Size => 48;

   for Thermistor_Point use record
      Temp  at 0 range 0 .. 23;
      Value at 3 range 0 .. 23;
   end record;

   type Thermistor_Curve_Index is range 1 .. 512;

   type Thermistor_Curve is array (Thermistor_Curve_Index) of Thermistor_Point with
     Size => 48 * 512, Component_Size => 48, Scalar_Storage_Order => System.Low_Order_First;

   type Thermistor_Curves_Array is array (Thermistor_Name) of Thermistor_Curve with
     Size => 48 * 512 * 4, Component_Size => 48 * 512, Scalar_Storage_Order => System.Low_Order_First;

   type Reported_Temperatures is array (Thermistor_Name) of Fixed_Point_Celcius with
     Size => 24 * 4, Component_Size => 24, Scalar_Storage_Order => System.Low_Order_First;

   type Reported_Heater_PWMs is array (Heater_Name) of Fixed_Point_PWM_Scale with
     Size => 16 * 2, Component_Size => 16, Scalar_Storage_Order => System.Low_Order_First;

   type Reported_Switch_States is array (Input_Switch_Name) of Input_Switch_State with
     Size => 8 * 10, Component_Size => 8, Scalar_Storage_Order => System.Low_Order_First;

   type Tach_Counter is range 0 .. 2**16 - 1 with
     Size => 16;

   type Reported_Tach_Counters is array (Fan_Name) of Tach_Counter with
     Size => 16 * 4, Component_Size => 16, Scalar_Storage_Order => System.Low_Order_First;

   type Firmware_Data_Offset is mod 2**8;

   type Firmware_Byte is mod 2**8 with
     Size => 8;

   type Firmware_Data_Array is array (1 .. 1_024) of Firmware_Byte with
     Component_Size => 8, Size => 8 * 1_024, Scalar_Storage_Order => System.Low_Order_First;

   type Message_From_Server_Kind is
     (Setup_Kind,
      Heater_Reconfigure_Kind,
      Loop_Setup_Kind,
      Regular_Step_Delta_List_Kind,
      Looping_Step_Delta_List_Kind,
      Condition_Check_Kind,
      Status_Kind,
      Check_If_Idle_Kind,
      Check_If_Heater_Autotune_Done_Kind,
      Enable_Stepper_Kind,
      Disable_Stepper_Kind,
      Enable_High_Power_Switch_Kind,
      Disable_High_Power_Switch_Kind,
      Firmware_Update_Start_Kind,
      Firmware_Update_Data_Kind,
      Firmware_Update_Done_Kind) with
     Size => 8;

   for Message_From_Server_Kind use
     (Setup_Kind                         => 129,
      Heater_Reconfigure_Kind            => 130,
      Loop_Setup_Kind                    => 131,
      Regular_Step_Delta_List_Kind       => 132,
      Looping_Step_Delta_List_Kind       => 133,
      Condition_Check_Kind               => 134,
      Status_Kind                        => 135,
      Check_If_Idle_Kind                 => 136,
      Check_If_Heater_Autotune_Done_Kind => 137,
      Enable_Stepper_Kind                => 138,
      Disable_Stepper_Kind               => 139,
      Enable_High_Power_Switch_Kind      => 140,
      Disable_High_Power_Switch_Kind     => 141,
      Firmware_Update_Start_Kind         => 252,
      Firmware_Update_Data_Kind          => 253,
      Firmware_Update_Done_Kind          => 254);

   type Message_From_Server_Content (Kind : Message_From_Server_Kind := Setup_Kind) is record
      Index : Message_Index;
      case Kind is
         when Firmware_Update_Start_Kind =>
            ID : Client_ID;
         when Firmware_Update_Data_Kind =>
            Firmware_Offset : Firmware_Data_Offset;
            Firmware_Data   : Firmware_Data_Array;
         when Firmware_Update_Done_Kind =>
            null;
         when others =>
            TMC_Write_Data : TMC2240_UART_Data_Byte_Array;
            TMC_Read_Data  : TMC2240_UART_Query_Byte_Array;
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
               when Firmware_Update_Start_Kind | Firmware_Update_Data_Kind | Firmware_Update_Done_Kind =>
                  null;
            end case;
      end case;
   end record with
     Scalar_Storage_Order => System.Low_Order_First, Bit_Order => System.Low_Order_First, Size => 3_085 * 32;
     --  Size should always be a multiple of 32 to allow for 32-bit CRC inputs on STM32.

   for Message_From_Server_Content use record
      Kind                  at  0 range 0 ..      7;
      Index                 at  8 range 0 ..     63;
      ID                    at 32 range 0 ..    127;
      Firmware_Offset       at 16 range 0 ..      7;
      Firmware_Data         at 20 range 0 ..  8_191;
      --  Always keep the above fields at the same positions so the server can handle firmware updates.
      TMC_Write_Data        at 16 range 0 ..     63;
      TMC_Read_Data         at 24 range 0 ..     31;
      Heater_Thermistors    at 32 range 0 ..     15;
      Thermistor_Curves     at 34 range 0 .. 98_303;
      Heater                at 32 range 0 ..      7;
      Heater_Params         at 36 range 0 ..    287;
      Loop_Input_Switch     at 32 range 0 ..      7;
      Loop_Until_State      at 33 range 0 ..      7;
      Fan_Targets           at 32 range 0 ..     63;
      Heater_Targets        at 40 range 0 ..     63;
      Last_Index            at 48 range 0 ..     15;
      Safe_Stop_After       at 50 range 0 ..      7;
      Steps                 at 51 range 0 .. 98_303;
      Conditon_Input_Switch at 32 range 0 ..      7;
      Skip_If_Hit_State     at 33 range 0 ..      7;
      Heater_To_Check       at 32 range 0 ..      7;
      Stepper               at 32 range 0 ..      7;
   end record;

   type Message_From_Server is record
      Checksum : CRC32;
      Content  : Message_From_Server_Content;
   end record with
     Scalar_Storage_Order => System.Low_Order_First, Bit_Order => System.Low_Order_First, Size => 3_086 * 32;
     --  Size should always be a multiple of 32 to allow for 32-bit CRC inputs on STM32.

   for Message_From_Server use record
      Checksum at 0 range 0 ..             31;
      Content  at 4 range 0 .. 3_085 * 32 - 1;
   end record;

   type Message_Length is mod 2**32 with
     Size => 32;

   type Message_From_Client_Kind is (Hello_Kind, Firmware_Update_Reply_Kind, Status_Kind, Check_Reply_Kind) with
     Size => 8;

   for Message_From_Client_Kind use
     (Hello_Kind => 1, Firmware_Update_Reply_Kind => 2, Status_Kind => 3, Check_Reply_Kind => 4);

   type Message_From_Client_Content (Kind : Message_From_Client_Kind := Hello_Kind) is record
      Index : Message_Index;
      case Kind is
         when Hello_Kind | Firmware_Update_Reply_Kind =>
            ID                    : Client_ID;
            Version               : Client_Version;
            Client_Message_Length : Message_Length;
            --  Number of nibbles sent in complete message from the client (i.e. the number of raw bytes that the UART
            --  peripheral sees,assuming no log messages are sent at the same time).
            Server_Message_Length : Message_Length;
            --  Number of bytes sent in complete message from the server.
         when others =>
            Temperatures    : Reported_Temperatures;
            Heaters         : Reported_Heater_PWMs;
            Switches        : Reported_Switch_States;
            Tachs           : Reported_Tach_Counters;
            TMC_Data        : TMC2240_UART_Data_Byte_Array;
            case Kind is
               when Hello_Kind | Firmware_Update_Reply_Kind =>
                  null;
               when Status_Kind =>
                  null;
               when Check_Reply_Kind =>
                  Condition_Met : Byte_Boolean;
            end case;
      end case;
   end record with
     Scalar_Storage_Order => System.Low_Order_First, Bit_Order => System.Low_Order_First, Size => 16 * 32;
     --  Size should always be a multiple of 32 to allow for 32-bit CRC inputs on STM32.

   for Message_From_Client_Content use record
      Kind                  at  0 range 0 ..   7;
      Index                 at  8 range 0 ..  63;
      ID                    at 16 range 0 .. 127;
      Version               at 32 range 0 ..  31;
      Client_Message_Length at 36 range 0 ..  31;
      Server_Message_Length at 40 range 0 ..  31;
      --  Always keep the above fields at the same positions so the server can reliably detect an unexpected firmware
      --  version and handle firmware updates.
      Temperatures          at 16 range 0 ..  95;
      Heaters               at 31 range 0 ..  31;
      Switches              at 35 range 0 ..  79;
      Tachs                 at 45 range 0 ..  63;
      TMC_Data              at 53 range 0 ..  63;
      Condition_Met         at 61 range 0 ..   7;
   end record;

   type Message_From_Client is record
      Checksum : CRC32;
      Content  : Message_From_Client_Content;
   end record with
     Scalar_Storage_Order => System.Low_Order_First, Bit_Order => System.Low_Order_First, Size => 17 * 32;
     --  Size should always be a multiple of 32 to allow for 32-bit CRC inputs on STM32.

   for Message_From_Client use record
      Checksum at 0 range 0 ..          31;
      Content  at 4 range 0 .. 16 * 32 - 1;
   end record;

end Messages;
