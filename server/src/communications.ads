with GNAT.Serial_Communications;
with Messages; use Messages;
with Ada.Exceptions;
with System.Multiprocessors;

generic
   with procedure Report_Error (Occurrence : Ada.Exceptions.Exception_Occurrence);
   with procedure Report_Temperature (Thermistor : Thermistor_Name; Temp : Fixed_Point_Celcius);
   with procedure Report_Heater_Power (Heater : Heater_Name; Power : Fixed_Point_PWM_Scale);
   with procedure Report_Input_Switch_State (Switch : Input_Switch_Name; State : Input_Switch_State);
   with procedure Prompt_For_Update;
   with procedure Log (Message : String);
   Runner_CPU : System.Multiprocessors.CPU_Range;
package Communications is

   UART_Timeout_Error : exception;

   protected TMC_IO is
      procedure Read (Message : TMC2240_UART_Query_Byte_Array; Reply : out TMC2240_UART_Data_Byte_Array);
      procedure Write (Message : TMC2240_UART_Data_Byte_Array);
   end TMC_IO;

   task Runner with
     CPU => Runner_CPU
   is
      entry Init (Port_Name : GNAT.Serial_Communications.Port_Name);
      entry Send_Message (Content : Message_From_Server_Content);
      entry Send_Message_And_Wait_For_Reply
        (Content : Message_From_Server_Content; Reply : out Message_From_Client_Content);
   end Runner;

private

   TMC_Query         : TMC2240_UART_Query_Byte_Array with
     Volatile;
   TMC_Query_Waiting : Boolean with
     Volatile, Atomic;

   TMC_Write         : TMC2240_UART_Data_Byte_Array with
     Volatile;
   TMC_Write_Waiting : Boolean with
     Volatile, Atomic;

   TMC_Reply         : TMC2240_UART_Data_Byte_Array with
     Volatile;
   TMC_Reply_Waiting : Boolean with
     Volatile, Atomic;

   --  We avoid using a protected object for the above as the Runner task accesses them and it is crucial that the
   --  Runner task never blocks on anything other than waiting for a message or reply to maximise reliability.

end Communications;
