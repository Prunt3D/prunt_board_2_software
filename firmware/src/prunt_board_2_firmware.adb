with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;
with Server_Communication;
with Step_Generator;
with Steppers;
with High_Power_Switch;
with Input_Switches;
with Thermistors;
with Heaters;
with Fans;
with Ada.Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
with GNAT.Source_Info;
with System.Machine_Reset;
with System;
with Self_Check;
with MCU_Temperature;

with Last_Chance_Handler;
pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when an exception is propagated. We need it in
--  the executable, therefore it must be somewhere in the closure of the context clauses.

procedure Prunt_Board_2_Firmware is
   pragma Priority (System.Priority'First);
   --  DMA uses polling, so this task has to have a low priority. Even without polling it still makes sense to use a
   --  low priority here so communication will time out in the case that the MCU is overloaded.
   --
   --  TODO: Use interrupts instead of polling.
begin
   Enable_Clock (GPIO_A);
   Enable_Clock (GPIO_B);
   Enable_Clock (GPIO_C);
   Enable_Clock (GPIO_D);
   Enable_Clock (GPIO_F);

   Heaters.Make_Safe;

   --  Unused floating pins and BOOT0.
   Configure_IO (Points => (PD2, PF0), Config => (Mode => Mode_In, Resistors => Pull_Down));

   --  Temporary: Fan tachs.
   Configure_IO (Points => (PA7, PB1, PB0, PA0), Config => (Mode => Mode_In, Resistors => Floating));

   --  Temporary: Fan fault indicator.
   Configure_IO (This => PB4, Config => (Mode => Mode_In, Resistors => Floating));

   --  Always start server communication first so exceptions can be reported.
   Server_Communication.Init;

   Server_Communication.Transmit_String_Line ("Startup: Integrity check");
   if not Self_Check.Current_Bank_Is_Valid then
      raise Constraint_Error with "Integrity check failed. Manual flashing is required.";
   end if;

   Server_Communication.Transmit_String_Line ("Startup: Heaters.Make_Safe");
   Heaters.Make_Safe;
   Server_Communication.Transmit_String_Line ("Startup: Fans.Init");
   Fans.Init;
   Server_Communication.Transmit_String_Line ("Startup: Input_Switches.Init");
   Input_Switches.Init;
   Server_Communication.Transmit_String_Line ("Startup: Steppers.Init");
   Steppers.Init;
   Server_Communication.Transmit_String_Line ("Startup: Step_Generator.Init");
   Step_Generator.Init;
   Server_Communication.Transmit_String_Line ("Startup: High_Power_Switch.Init");
   High_Power_Switch.Init;

   Server_Communication.Transmit_String_Line ("Startup: High_Power_Switch.Wait_For_Power_Good");
   High_Power_Switch.Wait_For_Power_Good;

   delay until Clock + Seconds (1); --  Ensure voltages have time to come up before ADC calibration.

   Server_Communication.Transmit_String_Line ("Startup: Thermistors.Init");
   Thermistors.Init;
   Server_Communication.Transmit_String_Line ("Startup: MCU_Temperature.Init");
   MCU_Temperature.Init;

   Server_Communication.Transmit_String_Line ("Startup: Server_Communication.Run");
   Server_Communication.Run;

end Prunt_Board_2_Firmware;
