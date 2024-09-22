with Messages; use Messages;

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

   Init_Done : Boolean := False with
     Atomic, Volatile;

   RX_Message : aliased Message_From_Server with
     Alignment => 4, Volatile;

   TX_Message : aliased Message_From_Client with
     Alignment => 4, Volatile;

   procedure Set_TX_Message_Kind (Kind : Message_From_Client_Kind);
   procedure Set_TX_Message_CRC;
   procedure Transmit_TX_Message;

end Server_Communication;
