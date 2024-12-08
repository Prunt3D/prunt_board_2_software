with Messages; use Messages;
with Init_Checkers;
with HAL;

package Server_Communication is

   procedure Init;
   procedure Run;
   procedure Transmit_String (S : String);
   procedure Transmit_String_Line (S : String);
   procedure Transmit_Fatal_Exception_Mark;
   function Is_Init_Done return Boolean;

   DMA_Error     : exception;
   Timeout_Error : exception;

private

   Init_Checker : Init_Checkers.Init_Checker;

   RX_Message : aliased Message_From_Server with
     Alignment => 4, Volatile;

   TX_Message : aliased Message_From_Client with
     Alignment => 4, Volatile;

   procedure Set_TX_Message_Kind (Kind : Message_From_Client_Kind);
   procedure Set_TX_Message_CRC;
   procedure Transmit_TX_Message;

   Kalico_Persistent_Boot_Flag : HAL.UInt32 with
     Volatile => True, Linker_Section => ".data_persistent.kalico_boot_flag";
end Server_Communication;
