with Ada.Interrupts.Names;

package STM32.CORDIC.Interrupts is

   procedure Calculate_CORDIC_Function
     (This     : in out CORDIC_Coprocessor;
      Argument : UInt32_Array;
      Result   : out UInt32_Array);
   --  Uses the interrupt interface to get the calculated function result.

   procedure Calculate_CORDIC_Function
     (This     : in out CORDIC_Coprocessor;
      Argument : UInt16_Array;
      Result   : out UInt16_Array);
   --  Uses the interrupt interface to get the calculated function result.

private
   type Buffer_Content is array (Integer range <>) of UInt32;

   type Ring_Buffer is record
      Content : Buffer_Content (0 .. 9);
      Head    : Integer := 0;
      Tail    : Integer := 0;
   end record;

   Ring_Buffer_Full : exception;
   --  Raised when the Ring Buffer is full (Head and Tail is the same).

   protected Receiver is
      pragma Interrupt_Priority;
      entry Get_Result (Value : out UInt32);

   private
      Buffer         : Ring_Buffer;
      Data_Available : Boolean := False;

      procedure Interrupt_Handler;

      pragma Attach_Handler
        (Interrupt_Handler,
         Ada.Interrupts.Names.Cordic_Interrupt);

   end Receiver;

end STM32.CORDIC.Interrupts;
