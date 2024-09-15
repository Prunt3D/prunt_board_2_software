with STM32.Device;

package body STM32.LPTimers is

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out LPTimer) is
   begin
      This.CR.ENABLE := True;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out LPTimer) is
   begin
      This.CR.ENABLE := False;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : LPTimer) return Boolean is
   begin
      return This.CR.ENABLE;
   end Enabled;

   -------------------------
   -- Configure_Prescaler --
   -------------------------

   procedure Configure_Prescaler
     (This      : in out LPTimer;
      Prescaler : LPTimer_Prescaler)
   is
   begin
      This.CFGR.PRESC := Prescaler'Enum_Rep;
   end Configure_Prescaler;

   -----------------------
   -- Current_Prescaler --
   -----------------------

   function Current_Prescaler (This : LPTimer) return LPTimer_Prescaler is
   begin
      return LPTimer_Prescaler'Val (This.CFGR.PRESC);
   end Current_Prescaler;

   -----------------------
   -- Set_Compare_Value --
   -----------------------

   procedure Set_Compare_Value
     (This  : in out LPTimer;
      Value : UInt16)
   is
   begin
      This.CMP.CMP := Value;
   end Set_Compare_Value;

   ---------------------------
   -- Current_Compare_Value --
   ---------------------------

   function Current_Compare_Value (This : LPTimer) return UInt16 is
   begin
      return This.CMP.CMP;
   end Current_Compare_Value;

   --------------------------
   -- Set_Autoreload_Value --
   --------------------------

   procedure Set_Autoreload_Value
     (This  : in out LPTimer;
      Value : UInt16)
   is
   begin
      This.ARR.ARR := Value;
   end Set_Autoreload_Value;

   ------------------------
   -- Current_Autoreload --
   ------------------------

   function Current_Autoreload (This : LPTimer) return UInt16 is
   begin
      return This.ARR.ARR;
   end Current_Autoreload;

   ----------------------------------
   -- Compute_Prescaler_and_Period --
   ----------------------------------

   procedure Compute_Prescaler_And_Period
     (This                : LPTimer;
      Requested_Frequency : UInt32;
      Prescaler           : out LPTimer_Prescaler;
      Period              : out UInt32)
   is
      Max_Prescaler      : constant LPTimer_Prescaler := LPTimer_Prescaler'Last;
      Max_Period         : constant := 16#FFFF#; --  UInt16'Last
      Prescaler_Enum     : UInt8; --  Counter for LPTimer_Prescaler'Enum_Rep
      Hardware_Frequency : UInt32;
      CK_CNT             : UInt32;
   begin

      Hardware_Frequency := STM32.Device.Get_Clock_Frequency (This);

      if Requested_Frequency > Hardware_Frequency then
         raise Invalid_Request with "Frequency too high";
      end if;

      --  We use a numeric prescaler value to calculate the Hardware_Frequency
      --  division considering that the clock prescaler is a power of 2 of this
      --  value, as are the LPTimer_Prescaler discrete values.
      Prescaler_Enum := 0;

      loop
         --  Compute the Counter's clock
         CK_CNT := Hardware_Frequency / UInt32 (2**Integer (Prescaler_Enum));
         --  Determine the CK_CNT periods to achieve the requested frequency
         Period := CK_CNT / Requested_Frequency;

         exit when ((Period <= Max_Period) or
                      (Prescaler_Enum > Max_Prescaler'Enum_Rep));

         Prescaler_Enum := Prescaler_Enum + 1;
      end loop;

      if Prescaler_Enum > Max_Prescaler'Enum_Rep then
         raise Invalid_Request with "Frequency too low";
      end if;

      Prescaler := LPTimer_Prescaler'Val (Prescaler_Enum);
   end Compute_Prescaler_And_Period;

   ------------------------------
   -- Set_Counter_Clock_Source --
   ------------------------------

   procedure Set_Counter_Clock_Source
     (This   : in out LPTimer;
      Mode   : LPTimer_Clock_Source)
   is
   begin
      This.CFGR.COUNTMODE := Mode = External;
   end Set_Counter_Clock_Source;

   -----------------------
   -- Set_Counter_Value --
   -----------------------

   procedure Set_Counter_Value
     (This  : in out LPTimer;
      Value : UInt16)
   is
   begin
      This.CNT.CNT := Value;
   end Set_Counter_Value;

   ---------------------
   -- Current_Counter --
   ---------------------

   function Current_Counter (This : LPTimer) return UInt16 is
   begin
      return This.CNT.CNT;
   end Current_Counter;

   ----------------------------
   -- Set_Counter_Reset_Mode --
   ----------------------------

   procedure Set_Counter_Reset_Mode
     (This   : in out LPTimer;
      Mode   : LPTimer_Counter_Reset_Mode;
      Enable : Boolean := True)
   is
   begin
      case Mode is
         when After_Read_Counter =>
            This.CR.RSTARE := Enable;
         when Now =>
            if Enable then
               while This.CR.COUNTRST loop
                  null;
               end loop;
               This.CR.COUNTRST := True;
            end if;
      end case;
   end Set_Counter_Reset_Mode;

   ----------------------
   -- Set_Preload_Mode --
   ----------------------

   procedure Set_Preload_Mode
     (This   : in out LPTimer;
      Mode   : LPTimer_Preload_Mode)
   is
   begin
      This.CFGR.PRELOAD := Mode = End_Current_Period;
   end Set_Preload_Mode;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (This   : in out LPTimer;
      Source : LPTimer_Interrupt)
   is
   begin
      case Source is
         when Compare_Match =>
            This.IER.CMPMIE := True;
         when Autorreload_Match =>
            This.IER.ARRMIE := True;
         when External_Trigger_Valid_Edge =>
            This.IER.EXTTRIGIE := True;
         when Compare_Register_Update_Ok =>
            This.IER.CMPOKIE := True;
         when Autorreload_Register_Update_Ok =>
            This.IER.ARROKIE := True;
         when Direction_Change_To_Up =>
            This.IER.UPIE := True;
         when Direction_Change_To_Down =>
            This.IER.DOWNIE := True;
      end case;
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt
     (This   : in out LPTimer;
      Source : LPTimer_Interrupt)
   is
   begin
      case Source is
         when Compare_Match =>
            This.IER.CMPMIE := False;
         when Autorreload_Match =>
            This.IER.ARRMIE := False;
         when External_Trigger_Valid_Edge =>
            This.IER.EXTTRIGIE := False;
         when Compare_Register_Update_Ok =>
            This.IER.CMPOKIE := False;
         when Autorreload_Register_Update_Ok =>
            This.IER.ARROKIE := False;
         when Direction_Change_To_Up =>
            This.IER.UPIE := False;
         when Direction_Change_To_Down =>
            This.IER.DOWNIE := False;
      end case;
   end Disable_Interrupt;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (This    : in out LPTimer;
      Sources : LPTimer_Interrupt_List)
   is
   begin
      for Source of Sources loop
         case Source is
            when Compare_Match =>
               This.IER.CMPMIE := True;
            when Autorreload_Match =>
               This.IER.ARRMIE := True;
            when External_Trigger_Valid_Edge =>
               This.IER.EXTTRIGIE := True;
            when Compare_Register_Update_Ok =>
               This.IER.CMPOKIE := True;
            when Autorreload_Register_Update_Ok =>
               This.IER.ARROKIE := True;
            when Direction_Change_To_Up =>
               This.IER.UPIE := True;
            when Direction_Change_To_Down =>
               This.IER.DOWNIE := True;
         end case;
      end loop;
   end Enable_Interrupt;

   -----------------------------
   -- Clear_Pending_Interrupt --
   -----------------------------

   procedure Clear_Pending_Interrupt
     (This   : in out LPTimer;
      Source : LPTimer_Interrupt)
   is
   begin
      case Source is
         when Compare_Match =>
            This.ICR.CMPMCF := True;
         when Autorreload_Match =>
            This.ICR.ARRMCF := True;
         when External_Trigger_Valid_Edge =>
            This.ICR.EXTTRIGCF := True;
         when Compare_Register_Update_Ok =>
            This.ICR.CMPOKCF := True;
         when Autorreload_Register_Update_Ok =>
            This.ICR.ARROKCF := True;
         when Direction_Change_To_Up =>
            This.ICR.UPCF := True;
         when Direction_Change_To_Down =>
            This.ICR.DOWNCF := True;
      end case;
   end Clear_Pending_Interrupt;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This   : LPTimer;
      Source : LPTimer_Interrupt)
      return Boolean
   is
   begin
      case Source is
         when Compare_Match =>
            return This.IER.CMPMIE;
         when Autorreload_Match =>
            return This.IER.ARRMIE;
         when External_Trigger_Valid_Edge =>
            return This.IER.EXTTRIGIE;
         when Compare_Register_Update_Ok =>
            return This.IER.CMPOKIE;
         when Autorreload_Register_Update_Ok =>
            return This.IER.ARROKIE;
         when Direction_Change_To_Up =>
            return This.IER.UPIE;
         when Direction_Change_To_Down =>
            return This.IER.DOWNIE;
      end case;
   end Interrupt_Enabled;

   ------------
   -- Status --
   ------------

   function Status (This : LPTimer;
                    Flag : LPTimer_Status_Flag) return Boolean is
   begin
      case Flag is
         when Compare_Match =>
            return This.IER.CMPMIE;
         when Autorreload_Match =>
            return This.IER.ARRMIE;
         when External_Trigger_Valid_Edge =>
            return This.IER.EXTTRIGIE;
         when Compare_Register_Update_Ok =>
            return This.IER.CMPOKIE;
         when Autorreload_Register_Update_Ok =>
            return This.IER.ARROKIE;
         when Direction_Change_To_Up =>
            return This.IER.UPIE;
         when Direction_Change_To_Down =>
            return This.IER.DOWNIE;
      end case;
   end Status;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status (This : in out LPTimer;
                           Flag : LPTimer_Status_Flag) is
   begin
      case Flag is
         when Compare_Match =>
            This.ICR.CMPMCF := True;
         when Autorreload_Match =>
            This.ICR.ARRMCF := True;
         when External_Trigger_Valid_Edge =>
            This.ICR.EXTTRIGCF := True;
         when Compare_Register_Update_Ok =>
            This.ICR.CMPOKCF := True;
         when Autorreload_Register_Update_Ok =>
            This.ICR.ARROKCF := True;
         when Direction_Change_To_Up =>
            This.ICR.UPCF := True;
         when Direction_Change_To_Down =>
            This.ICR.DOWNCF := True;
      end case;
   end Clear_Status;

   -------------------------
   -- Select_Clock_Source --
   -------------------------

   procedure Select_Clock_Source
     (This   : in out LPTimer;
      Source : LPTimer_Clock_Source)
   is
   begin
      This.CFGR.CKSEL := Source = External;
   end Select_Clock_Source;

   ----------------------
   -- Get_Clock_Source --
   ----------------------

   function Get_Clock_Source (This : LPTimer) return LPTimer_Clock_Source is
   begin
      if This.CFGR.CKSEL then
         return External;
      else
         return Internal;
      end if;
   end Get_Clock_Source;

   ------------------------------
   -- Configure_External_Clock --
   ------------------------------

   procedure Configure_External_Clock
     (This     : in out LPTimer;
      Polarity : LPTimer_External_Clock_Polarity;
      Filter   : LPTimer_Digital_Filter)
   is
   begin
      This.CFGR.CKPOL := Polarity'Enum_Rep;
      This.CFGR.CKFLT := Filter'Enum_Rep;
   end Configure_External_Clock;

   ---------------------------
   -- Configure_Input_Clock --
   ---------------------------

   procedure Configure_Input_Clock
     (This  : in out LPTimer;
      Input : LPTimer_Input;
      Clock : LPTimer_Input_Clock)
   is
   begin
      case Input is
         when Input_1 =>
            This.OR_k.IN1_0 := Clock.Internal;
            This.OR_k.IN1_1 := Clock.Value'Enum_Rep;
         when Input_2 =>
            This.OR_k.IN2_0 := Clock.Internal;
            This.OR_k.IN2_1 := Clock.Value'Enum_Rep;
      end case;
   end Configure_Input_Clock;

   ---------------------------
   -- Select_Trigger_Source --
   ---------------------------

   procedure Select_Trigger_Source
     (This   : in out LPTimer;
      Source : LPTimer_Trigger_Source)
   is
   begin
      if Source'Enum_Rep > 7 then
         This.CFGR.TRIGSEL_1 := True;
      else
         This.CFGR.TRIGSEL_1 := False;
      end if;
      This.CFGR.TRIGSEL_0 := UInt3 (Source'Enum_Rep);
   end Select_Trigger_Source;

   ---------------------------
   -- Select_Trigger_Filter --
   ---------------------------

   procedure Select_Trigger_Filter
     (This   : in out LPTimer;
      Filter : LPTimer_Digital_Filter)
   is
   begin
      This.CFGR.TRGFLT := Filter'Enum_Rep;
   end Select_Trigger_Filter;

   ----------------------
   -- Set_Trigger_Timeout --
   ----------------------

   procedure Set_Trigger_Timeout
     (This : in out LPTimer;
      Enable : Boolean)
   is
   begin
      This.CFGR.TIMOUT := not Enable;
   end Set_Trigger_Timeout;

   -----------------------
   -- Configure_Trigger --
   -----------------------

   procedure Configure_Trigger
     (This    : in out LPTimer;
      Source  : LPTimer_Trigger_Source;
      Filter  : LPTimer_Digital_Filter;
      Timeout : Boolean)
   is
   begin
      if Source'Enum_Rep > 7 then
         This.CFGR.TRIGSEL_1 := True;
      else
         This.CFGR.TRIGSEL_1 := False;
      end if;
      This.CFGR.TRIGSEL_0 := UInt3 (Source'Enum_Rep);
      This.CFGR.TRGFLT := Filter'Enum_Rep;
      This.CFGR.TIMOUT := not Timeout;
   end Configure_Trigger;

   -----------------------
   -- Select_Pulse_Mode --
   -----------------------

   procedure Select_Pulse_Mode
     (This : in out LPTimer;
      Mode : LPTimer_Pulse_Mode)
   is
   begin
      case Mode is
         when Repetitive =>
            This.CR.CNTSTRT := True;
            This.CR.SNGSTRT := False;
         when Single =>
            This.CR.CNTSTRT := False;
            This.CR.SNGSTRT := True;
      end case;
   end Select_Pulse_Mode;

   ------------------------
   -- Set_Waveform_Shape --
   ------------------------

   procedure Set_Waveform_Shape
     (This  : in out LPTimer;
      Shape : LPTimer_Waveform_Shape)
   is
   begin
      This.CFGR.WAVE := Shape = Set_Once;
   end Set_Waveform_Shape;

   ---------------------------
   -- Set_Waveform_Polarity --
   ---------------------------

   procedure Set_Waveform_Polarity
     (This     : in out LPTimer;
      Polarity : LPTimer_Waveform_Polarity)
   is
   begin
      This.CFGR.WAVPOL := Polarity = Inverse;
   end Set_Waveform_Polarity;

   ------------------------------
   -- Configure_Waveform_Shape --
   ------------------------------

   procedure Configure_Waveform_Shape
     (This     : in out LPTimer;
      Shape    : LPTimer_Waveform_Shape;
      Polarity : LPTimer_Waveform_Polarity)
   is
   begin
      This.CFGR.WAVE := Shape = Set_Once;
      This.CFGR.WAVPOL := Polarity = Inverse;
   end Configure_Waveform_Shape;

   ----------------------
   -- Set_Encoder_Mode --
   ----------------------

   procedure Set_Encoder_Mode
     (This : in out LPTimer;
      Enable : Boolean)
   is
   begin
      This.CFGR.ENC := Enable;
   end Set_Encoder_Mode;

end STM32.LPTimers;
