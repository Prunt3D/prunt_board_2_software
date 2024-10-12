with Messages; use Messages;
with HAL;      use HAL;
with Init_Checkers;

package Steppers is

   procedure Init;
   procedure Enable (Stepper : Stepper_Name);
   procedure Disable (Stepper : Stepper_Name);

   protected UART_IO is
      entry Start_Read (Input : TMC2240_UART_Query_Byte_Array);
      entry Get_Read_Result (Receive_Failed : out Byte_Boolean; Output : out TMC2240_UART_Data_Byte_Array);
      entry Write (Input : TMC2240_UART_Data_Byte_Array);
   private
      Read_Started : Boolean := False;
   end UART_IO;

private

   Init_Checker : Init_Checkers.Init_Checker;

   type RX_Buffer_Type is array (1 .. 12) of UInt8 with
     Pack, Volatile_Components, Volatile;
   RX_Buffer : aliased RX_Buffer_Type := (others => 0);
   --  Extra bytes for transmitted bytes since we are using half-duplex mode. DMA does not work with CCM, so we put
   --  this at the package level rather than on the stack.

end Steppers;
