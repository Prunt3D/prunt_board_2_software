with STM32.Device;

package body STM32.CORDIC.Interrupts is

   -------------------------------
   -- Calculate_CORDIC_Function --
   -------------------------------

   procedure Calculate_CORDIC_Function
     (This     : in out CORDIC_Coprocessor;
      Argument : UInt32_Array;
      Result   : out UInt32_Array)
   is
      --  Test if data width is not 32 bit
      pragma Assert (This.CSR.ARGSIZE = True, "Invalid data size");

      Operation : constant CORDIC_Function := Get_CORDIC_Function (This);
   begin
      case Operation is
         when Cosine | Sine | Phase | Modulus =>
            --  Two 32 bit arguments
            This.WDATA := Argument (1);
            This.WDATA := Argument (2);
         when Hyperbolic_Cosine | Hyperbolic_Sine | Arctangent |
              Hyperbolic_Arctangent | Natural_Logarithm | Square_Root =>
            --  One 32 bit argument
            This.WDATA := Argument (1);
      end case;

      --  Get the results from the Ring Buffer
      case Operation is
         when Cosine | Sine | Phase | Modulus |
              Hyperbolic_Cosine | Hyperbolic_Sine =>
            --  Two 32 bit results
            Receiver.Get_Result (Result (1));
            Receiver.Get_Result (Result (2));
         when Arctangent | Hyperbolic_Arctangent | Natural_Logarithm | Square_Root =>
            --  One 32 bit result
            Receiver.Get_Result (Result (1));
      end case;
   end Calculate_CORDIC_Function;

   -------------------------------
   -- Calculate_CORDIC_Function --
   -------------------------------

   procedure Calculate_CORDIC_Function
     (This     : in out CORDIC_Coprocessor;
      Argument : UInt16_Array;
      Result   : out UInt16_Array)
   is
      --  Test if data width is not 16 bit
      pragma Assert (This.CSR.ARGSIZE = False, "Invalid data size");

      Operation : constant CORDIC_Function := Get_CORDIC_Function (This);
      Data : UInt32;
   begin
      case Operation is
         when Cosine | Sine | Phase | Modulus =>
            --  Two 16 bit argument
            Data := UInt32 (Argument (2));
            Data := Shift_Left (Data, 16) or UInt32 (Argument (1));
            This.WDATA := Data;
         when Hyperbolic_Cosine | Hyperbolic_Sine | Arctangent |
              Hyperbolic_Arctangent | Natural_Logarithm | Square_Root =>
            --  One 16 bit argument
            This.WDATA := UInt32 (Argument (1));
      end case;

      --  Get the results from the Ring Buffer
      Receiver.Get_Result (Data);

      case Operation is
         when Cosine | Sine | Phase | Modulus |
              Hyperbolic_Cosine | Hyperbolic_Sine =>
            --  Two 16 bit results
            Result (1) := UInt16 (Data);
            Result (2) := UInt16 (Shift_Right (Data, 16));
         when Arctangent | Hyperbolic_Arctangent | Natural_Logarithm | Square_Root =>
            --  One 16 bit result
            Result (1) := UInt16 (Data);
      end case;
   end Calculate_CORDIC_Function;

   --------------
   -- Receiver --
   --------------

   protected body Receiver is

      ----------------
      -- Get_Result --
      ----------------

      entry Get_Result (Value : out UInt32)
        when Data_Available
      is
         Next : constant Integer :=
           (Buffer.Tail + 1) mod Buffer.Content'Length;
      begin
         --  Remove an item from our ring buffer.
         Value := Buffer.Content (Next);
         Buffer.Tail := Next;

         --  If the buffer is empty, make sure we block subsequent callers
         --  until the buffer has something in it.
         if Buffer.Tail = Buffer.Head then
            Data_Available := False;
         end if;
      end Get_Result;

      -----------------------
      -- Interrupt_Handler --
      -----------------------

      procedure Interrupt_Handler is
         use STM32.Device;
      begin
         if Interrupt_Enabled (CORDIC_Unit) then
            if Status (CORDIC_Unit, Flag => Result_Ready) then

               if (Buffer.Head + 1) mod Buffer.Content'Length = Buffer.Tail
               then
                  --  But our buffer is full.
                  raise Ring_Buffer_Full;
               else
                  --  Add this first 32 bit data to our buffer.
                  Buffer.Head := (Buffer.Head + 1) mod Buffer.Content'Length;
                  Buffer.Content (Buffer.Head) := Get_CORDIC_Data (CORDIC_Unit);

                  --  Test if the function has two 32 bits results
                  if Get_CORDIC_Results_Number (CORDIC_Unit) = Two_32_Bit then
                     if (Buffer.Head + 1) mod Buffer.Content'Length = Buffer.Tail
                     then
                        --  But our buffer is full.
                        raise Ring_Buffer_Full;
                     else
                        --  Add this second 32 bit data to our buffer.
                        Buffer.Head := (Buffer.Head + 1) mod Buffer.Content'Length;
                        Buffer.Content (Buffer.Head) := Get_CORDIC_Data (CORDIC_Unit);
                     end if;
                  end if;

                  Data_Available := True;
               end if;
            end if;
         end if;
      end Interrupt_Handler;

   end Receiver;

end STM32.CORDIC.Interrupts;
