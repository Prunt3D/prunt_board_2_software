with Ada.Unchecked_Conversion;
with STM32.Device;

package body STM32.HRTimers is

   ----------------------------------------------------------------------------

   --  HRTimer Master functions -----------------------------------------------

   ----------------------------------------------------------------------------

   ------------
   -- Enable --
   ------------

   procedure Enable (This : HRTimer_Master) is
   begin
      if This'Address = HRTIM_Master_Base then
         HRTIM_Master_Periph.MCR.MCEN := True;
      end if;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : HRTimer_Master) is
   begin
      if This'Address = HRTIM_Master_Base then
         --  Test if HRTimer A to F has no outputs enabled
         if not (HRTimer_Common_Periph.OENR.TA1OEN or
           HRTimer_Common_Periph.OENR.TA2OEN) and
           not (HRTimer_Common_Periph.OENR.TB1OEN or
           HRTimer_Common_Periph.OENR.TB2OEN) and
           not (HRTimer_Common_Periph.OENR.TC1OEN or
           HRTimer_Common_Periph.OENR.TC2OEN) and
           not (HRTimer_Common_Periph.OENR.TD1OEN or
           HRTimer_Common_Periph.OENR.TD2OEN) and
           not (HRTimer_Common_Periph.OENR.TE1OEN or
           HRTimer_Common_Periph.OENR.TE2OEN) and
           not (HRTimer_Common_Periph.OENR.TF1OEN or
           HRTimer_Common_Periph.OENR.TF2OEN)
         then
            HRTIM_Master_Periph.MCR.MCEN := False;
         end if;
      end if;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : HRTimer_Master) return Boolean
   is
   begin
      if This'Address = HRTIM_Master_Base then
         return HRTIM_Master_Periph.MCR.MCEN;
      end if;
      return False;
   end Enabled;

   function UInt32_To_MCR is new Ada.Unchecked_Conversion
     (UInt32, STM32_SVD.HRTIM.MCR_Register);
   function MCR_To_UInt32 is new Ada.Unchecked_Conversion
     (STM32_SVD.HRTIM.MCR_Register, UInt32);

   ------------
   -- Enable --
   ------------

   procedure Enable (Counters : HRTimer_List) is
      Value : UInt32 := 16#00000000#;
      MCR_Value : UInt32 := MCR_To_UInt32 (HRTIM_Master_Periph.MCR);
   begin
      for Counter of Counters loop
         Value := Value or UInt32 (Counter'Enum_Rep);
      end loop;

      MCR_Value := MCR_Value or Shift_Left (Value, 16);
      HRTIM_Master_Periph.MCR := UInt32_To_MCR (MCR_Value);
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (Counters : HRTimer_List) is
      Value : UInt32 := 16#00000000#;
      MCR_Value : UInt32 := MCR_To_UInt32 (HRTIM_Master_Periph.MCR);
   begin
      for Counter of Counters loop
         case Counter is
            when HRTimer_A =>
               --  Test if HRTimer A has no outputs enabled.
               if not (HRTimer_Common_Periph.OENR.TA1OEN or
                         HRTimer_Common_Periph.OENR.TA2OEN)
               then
                  Value := Value or UInt32 (Counter'Enum_Rep);
               end if;

            when HRTimer_B =>
               --  Test if HRTimer B has no outputs enabled.
               if not (HRTimer_Common_Periph.OENR.TB1OEN or
                         HRTimer_Common_Periph.OENR.TB2OEN)
               then
                  Value := Value or UInt32 (Counter'Enum_Rep);
               end if;

            when HRTimer_C =>
               --  Test if HRTimer C has no outputs enabled.
               if not (HRTimer_Common_Periph.OENR.TC1OEN or
                         HRTimer_Common_Periph.OENR.TC2OEN)
               then
                  Value := Value or UInt32 (Counter'Enum_Rep);
               end if;

            when HRTimer_D =>
               --  Test if HRTimer D has no outputs enabled.
               if not (HRTimer_Common_Periph.OENR.TD1OEN or
                         HRTimer_Common_Periph.OENR.TD2OEN)
               then
                  Value := Value or UInt32 (Counter'Enum_Rep);
               end if;

            when HRTimer_E =>
               --  Test if HRTimer E has no outputs enabled.
               if not (HRTimer_Common_Periph.OENR.TE1OEN or
                         HRTimer_Common_Periph.OENR.TE2OEN)
               then
                  Value := Value or UInt32 (Counter'Enum_Rep);
               end if;

            when HRTimer_F =>
               --  Test if HRTimer E has no outputs enabled.
               if not (HRTimer_Common_Periph.OENR.TF1OEN or
                         HRTimer_Common_Periph.OENR.TF2OEN)
               then
                  Value := Value or UInt32 (Counter'Enum_Rep);
               end if;

            when HRTimer_M => --  Master
               --  Test if HRTimer A to F has no outputs enabled
               if not (HRTimer_Common_Periph.OENR.TA1OEN or
                 HRTimer_Common_Periph.OENR.TA2OEN) and
                 not (HRTimer_Common_Periph.OENR.TB1OEN or
                 HRTimer_Common_Periph.OENR.TB2OEN) and
                 not (HRTimer_Common_Periph.OENR.TC1OEN or
                 HRTimer_Common_Periph.OENR.TC2OEN) and
                 not (HRTimer_Common_Periph.OENR.TD1OEN or
                 HRTimer_Common_Periph.OENR.TD2OEN) and
                 not (HRTimer_Common_Periph.OENR.TE1OEN or
                 HRTimer_Common_Periph.OENR.TE2OEN) and
                 not (HRTimer_Common_Periph.OENR.TF1OEN or
                 HRTimer_Common_Periph.OENR.TF2OEN)
               then
                  Value := Value or UInt32 (Counter'Enum_Rep);
               end if;
         end case;
      end loop;

      MCR_Value := MCR_Value and not Shift_Left (Value, 16);
      HRTIM_Master_Periph.MCR := UInt32_To_MCR (MCR_Value);
   end Disable;

   --------------------------
   -- Set_Register_Preload --
   --------------------------

   procedure Set_Register_Preload
     (This    : in out HRTimer_Master;
      Enabled : Boolean)
   is
   begin
      This.MCR.PREEN := Enabled;
   end Set_Register_Preload;

   -------------------------
   -- Configure_Prescaler --
   -------------------------

   procedure Configure_Prescaler
     (This        : in out HRTimer_Master;
      Prescaler   : HRTimer_Prescaler)
   is
   begin
      This.MCR.CKPSC := Prescaler'Enum_Rep;
   end Configure_Prescaler;

   -----------------------
   -- Current_Prescaler --
   -----------------------

   function Current_Prescaler (This : HRTimer_Master) return HRTimer_Prescaler
   is
   begin
      return HRTimer_Prescaler'Val (This.MCR.CKPSC);
   end Current_Prescaler;

   -------------------------------------
   -- Configure_Synchronization_Input --
   -------------------------------------

   procedure Configure_Synchronization_Input
     (This   : in out HRTimer_Master;
      Source : Synchronization_Input_Source;
      Reset  : Boolean;
      Start  : Boolean)
   is
   begin
      This.MCR.SYNCIN := Source'Enum_Rep;
      This.MCR.SYNCRSTM := Reset;
      This.MCR.SYNCSTRTM := Start;
   end Configure_Synchronization_Input;

   --------------------------------------
   -- Configure_Synchronization_Output --
   --------------------------------------

   procedure Configure_Synchronization_Output
     (This   : in out HRTimer_Master;
      Source : Synchronization_Output_Source;
      Event  : Synchronization_Output_Event)
   is
   begin
      This.MCR.SYNCSRC := Source'Enum_Rep;
      This.MCR.SYNCOUT := Event'Enum_Rep;
   end Configure_Synchronization_Output;

   -------------------------------------------
   -- Configure_DAC_Synchronization_Trigger --
   -------------------------------------------

   procedure Configure_DAC_Synchronization_Trigger
     (This    : in out HRTimer_Master;
      Trigger : DAC_Synchronization_Trigger)
   is
   begin
      This.MCR.DACSYNC := Trigger'Enum_Rep;
   end Configure_DAC_Synchronization_Trigger;

   ---------------------------------------
   -- Configure_Register_Preload_Update --
   ---------------------------------------

   procedure Configure_Register_Preload_Update
     (This       : in out HRTimer_Master;
      Repetition : Boolean;
      Burst_DMA  : Burst_DMA_Update_Mode)
   is
   begin
      This.MCR.MREPU := Repetition;
      This.MCR.BRSTDMA := Burst_DMA'Enum_Rep;
   end Configure_Register_Preload_Update;

   -------------------------
   -- Set_HalfPeriod_Mode --
   -------------------------

   procedure Set_HalfPeriod_Mode
     (This : in out HRTimer_Master;
      Mode : Boolean)
   is
   begin
      This.MCR.HALF := Mode;
   end Set_HalfPeriod_Mode;

   ----------------
   -- Set_Period --
   ----------------

   procedure Set_Period (This : in out HRTimer_Master;  Value : UInt16) is
   begin
      This.MPER.MPER := Value;
   end Set_Period;

   --------------------
   -- Current_Period --
   --------------------

   function Current_Period (This : HRTimer_Master) return UInt16 is
   begin
      return This.MPER.MPER;
   end Current_Period;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This      : in out HRTimer_Master;
      Prescaler : HRTimer_Prescaler;
      Period    : UInt16)
   is
   begin
      Configure_Prescaler (This, Prescaler);
      Set_Period (This, Period);
   end Configure;

   ----------------------------------
   -- Compute_Prescaler_And_Period --
   ----------------------------------

   procedure Compute_Prescaler_And_Period
     (This                : HRTimer_Master;
      Requested_Frequency : UInt32;
      Prescaler           : out HRTimer_Prescaler;
      Period              : out UInt32)
   is
      Max_Period         : constant := 16#FFFF#; --  UInt16'Last
      Prescaler_Enum     : HRTimer_Prescaler := HRTimer_Prescaler'First;
      fHRCK              : UInt32; --  High frequency into HRTIM
      Hardware_Frequency : UInt32; --  fHRTIM
      CK_CNT             : UInt32;
   begin

      Hardware_Frequency := STM32.Device.Get_Clock_Frequency (This);

      if Requested_Frequency > Hardware_Frequency then
         raise Invalid_Request with "Frequency too high";
      end if;

      --  fHRCK is the high-resolution equivalent clock into HRTIM and all
      --  subsequent clocks are derived and synchronous with this source.
      --  Considering the fHRTIM clock period division by 32, it is equivalent
      --  to a frequency of fHRCK = 170 x 32 = 5.44 GHz. The HRtimer
      --  resolutions is tHRCK = 1 / fHRCK = 184 ps.
      fHRCK := Hardware_Frequency * 32;

      loop
         --  Compute the Counter's clock
         CK_CNT := fHRCK / UInt32 (2 ** Prescaler_Enum'Enum_Rep);
         --  Determine the CK_CNT periods to achieve the requested frequency
         Period := CK_CNT / Requested_Frequency;

         exit when ((Period <= Max_Period) or
                      (Prescaler_Enum = HRTimer_Prescaler'Last));

         Prescaler_Enum := HRTimer_Prescaler'Succ (Prescaler_Enum);
      end loop;

      if Period > Max_Period then
         raise Invalid_Request with "Frequency too low";
      end if;

      Prescaler := Prescaler_Enum;
   end Compute_Prescaler_And_Period;

   --------------------------------
   -- Set_Counter_Operating_Mode --
   --------------------------------

   procedure Set_Counter_Operating_Mode
     (This : in out HRTimer_Master;
      Mode : Counter_Operating_Mode)
   is
   begin
      case Mode is
         when SingleShot_NonRetriggerable =>
            This.MCR.CONT := False;
            This.MCR.RETRIG := False;
         when SingleShot_Retriggerable =>
            This.MCR.CONT := False;
            This.MCR.RETRIG := True;
         when Continuous =>
            This.MCR.CONT := True;
            This.MCR.RETRIG := False;
      end case;
   end Set_Counter_Operating_Mode;

   -----------------
   -- Set_Counter --
   -----------------

   procedure Set_Counter (This : in out HRTimer_Master; Value : UInt16) is
   begin
      This.MCNTR.MCNT := Value;
   end Set_Counter;

   ---------------------
   -- Current_Counter --
   ---------------------

   function Current_Counter (This : HRTimer_Master) return UInt16 is
   begin
      return This.MCNTR.MCNT;
   end Current_Counter;

   ----------------------------
   -- Set_Repetition_Counter --
   ----------------------------

   procedure Set_Repetition_Counter
     (This        : in out HRTimer_Master;
      Repetitions : UInt8) is
   begin
      This.MREP.MREP := Repetitions;
   end Set_Repetition_Counter;

   --------------------------------
   -- Current_Repetition_Counter --
   --------------------------------

   function Current_Repetition_Counter
     (This : HRTimer_Master) return UInt8 is
   begin
      return This.MREP.MREP;
   end Current_Repetition_Counter;

   ----------------------------------
   -- Configure_Repetition_Counter --
   ----------------------------------

   procedure Configure_Repetition_Counter
     (This        : in out HRTimer_Master;
      Repetitions : UInt8;
      Interrupt   : Boolean;
      DMA_Request : Boolean)
   is
   begin
      Set_Repetition_Counter (This, Repetitions);
      This.MDIER.MREPIE := Interrupt;
      This.MDIER.MREPDE := DMA_Request;
   end Configure_Repetition_Counter;

   -----------------------
   -- Set_Compare_Value --
   -----------------------

   procedure Set_Compare_Value
     (This    : in out HRTimer_Master;
      Compare : HRTimer_Compare_Number;
      Value   : UInt16)
   is
   begin
      case Compare is
         when Compare_1 =>
            HRTIM_Master_Periph.MCMP1R.MCMP1 := Value;
         when Compare_2 =>
            HRTIM_Master_Periph.MCMP2R.MCMP2 := Value;
         when Compare_3 =>
            HRTIM_Master_Periph.MCMP3R.MCMP3 := Value;
         when Compare_4 =>
            HRTIM_Master_Periph.MCMP4R.MCMP4 := Value;
      end case;
   end Set_Compare_Value;

   ------------------------
   -- Read_Compare_Value --
   ------------------------

   function Read_Compare_Value
     (This    : HRTimer_Master;
      Compare : HRTimer_Compare_Number) return UInt16
   is
      pragma Unreferenced (This);
   begin
      case Compare is
         when Compare_1 =>
            return HRTIM_Master_Periph.MCMP1R.MCMP1;
         when Compare_2 =>
            return HRTIM_Master_Periph.MCMP2R.MCMP2;
         when Compare_3 =>
            return HRTIM_Master_Periph.MCMP3R.MCMP3;
         when Compare_4 =>
            return HRTIM_Master_Periph.MCMP4R.MCMP4;
      end case;
   end Read_Compare_Value;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (This   : in out HRTimer_Master;
      Source : HRTimer_Master_Interrupt)
   is
   begin
      case Source is
         when Compare_1_Interrupt =>
            This.MDIER.MCMP1IE := True;
         when Compare_2_Interrupt =>
            This.MDIER.MCMP2IE := True;
         when Compare_3_Interrupt =>
            This.MDIER.MCMP3IE := True;
         when Compare_4_Interrupt =>
            This.MDIER.MCMP4IE := True;
         when Repetition_Interrupt =>
            This.MDIER.MREPIE := True;
         when Sync_Input_Interrupt =>
            This.MDIER.SYNCIE := True;
         when Update_Interrupt =>
            This.MDIER.MUPDIE := True;
      end case;
   end Enable_Interrupt;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (This    : in out HRTimer_Master;
      Sources : HRTimer_Master_Interrupt_List)
   is
   begin
      for Source of Sources loop
         case Source is
            when Compare_1_Interrupt =>
               This.MDIER.MCMP1IE := True;
            when Compare_2_Interrupt =>
               This.MDIER.MCMP2IE := True;
            when Compare_3_Interrupt =>
               This.MDIER.MCMP3IE := True;
            when Compare_4_Interrupt =>
               This.MDIER.MCMP4IE := True;
            when Repetition_Interrupt =>
               This.MDIER.MREPIE := True;
            when Sync_Input_Interrupt =>
               This.MDIER.SYNCIE := True;
            when Update_Interrupt =>
               This.MDIER.MUPDIE := True;
         end case;
      end loop;
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt
     (This   : in out HRTimer_Master;
      Source : HRTimer_Master_Interrupt)
   is
   begin
      case Source is
         when Compare_1_Interrupt =>
            This.MDIER.MCMP1IE := False;
         when Compare_2_Interrupt =>
            This.MDIER.MCMP2IE := False;
         when Compare_3_Interrupt =>
            This.MDIER.MCMP3IE := False;
         when Compare_4_Interrupt =>
            This.MDIER.MCMP4IE := False;
         when Repetition_Interrupt =>
            This.MDIER.MREPIE := False;
         when Sync_Input_Interrupt =>
            This.MDIER.SYNCIE := False;
         when Update_Interrupt =>
            This.MDIER.MUPDIE := False;
      end case;
   end Disable_Interrupt;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This   : HRTimer_Master;
      Source : HRTimer_Master_Interrupt) return Boolean
   is
   begin
      case Source is
         when Compare_1_Interrupt =>
            return This.MDIER.MCMP1IE;
         when Compare_2_Interrupt =>
            return This.MDIER.MCMP2IE;
         when Compare_3_Interrupt =>
            return This.MDIER.MCMP3IE;
         when Compare_4_Interrupt =>
            return This.MDIER.MCMP4IE;
         when Repetition_Interrupt =>
            return This.MDIER.MREPIE;
         when Sync_Input_Interrupt =>
            return This.MDIER.SYNCIE;
         when Update_Interrupt =>
            return This.MDIER.MUPDIE;
      end case;
   end Interrupt_Enabled;

   ----------------------
   -- Interrupt_Status --
   ----------------------

   function Interrupt_Status
     (This   : HRTimer_Master;
      Source : HRTimer_Master_Interrupt) return Boolean
   is
   begin
      case Source is
         when Compare_1_Interrupt =>
            return This.MISR.MCMP.Arr (1);
         when Compare_2_Interrupt =>
            return This.MISR.MCMP.Arr (2);
         when Compare_3_Interrupt =>
            return This.MISR.MCMP.Arr (3);
         when Compare_4_Interrupt =>
            return This.MISR.MCMP.Arr (4);
         when Repetition_Interrupt =>
            return This.MISR.MREP;
         when Sync_Input_Interrupt =>
            return This.MISR.SYNC;
         when Update_Interrupt =>
            return This.MISR.MUPD;
      end case;
   end Interrupt_Status;

   -----------------------------
   -- Clear_Pending_Interrupt --
   -----------------------------

   procedure Clear_Pending_Interrupt
     (This   : in out HRTimer_Master;
      Source : HRTimer_Master_Interrupt)
   is
   begin
      case Source is
         when Compare_1_Interrupt =>
            This.MICR.MCMP1C := True;
         when Compare_2_Interrupt =>
            This.MICR.MCMP2C := True;
         when Compare_3_Interrupt =>
            This.MICR.MCMP3C := True;
         when Compare_4_Interrupt =>
            This.MICR.MCMP4C := True;
         when Repetition_Interrupt =>
            This.MICR.MREPC := True;
         when Sync_Input_Interrupt =>
            This.MICR.SYNCC := True;
         when Update_Interrupt =>
            This.MICR.MUPDC := True;
      end case;
   end Clear_Pending_Interrupt;

   -----------------------
   -- Enable_DMA_source --
   -----------------------

   procedure Enable_DMA_Source
     (This   : in out HRTimer_Master;
      Source : HRTimer_Master_DMA_Request)
   is
   begin
      case Source is
         when Compare_1_DMA =>
            This.MDIER.MCMP1DE := True;
         when Compare_2_DMA =>
            This.MDIER.MCMP2DE := True;
         when Compare_3_DMA =>
            This.MDIER.MCMP3DE := True;
         when Compare_4_DMA =>
            This.MDIER.MCMP4DE := True;
         when Repetition_DMA =>
            This.MDIER.MREPDE := True;
         when Sync_Input_DMA =>
            This.MDIER.SYNCDE := True;
         when Update_DMA =>
            This.MDIER.MUPDDE := True;
      end case;
   end Enable_DMA_Source;

   ------------------------
   -- Disable_DMA_Source --
   ------------------------

   procedure Disable_DMA_Source
     (This   : in out HRTimer_Master;
      Source : HRTimer_Master_DMA_Request)
   is
   begin
      case Source is
         when Compare_1_DMA =>
            This.MDIER.MCMP1DE := False;
         when Compare_2_DMA =>
            This.MDIER.MCMP2DE := False;
         when Compare_3_DMA =>
            This.MDIER.MCMP3DE := False;
         when Compare_4_DMA =>
            This.MDIER.MCMP4DE := False;
         when Repetition_DMA =>
            This.MDIER.MREPDE := False;
         when Sync_Input_DMA =>
            This.MDIER.SYNCDE := False;
         when Update_DMA =>
            This.MDIER.MUPDDE := False;
      end case;
   end Disable_DMA_Source;

   ------------------------
   -- DMA_Source_Enabled --
   ------------------------

   function DMA_Source_Enabled
     (This   : HRTimer_Master;
      Source : HRTimer_Master_DMA_Request) return Boolean
   is
   begin
      case Source is
         when Compare_1_DMA =>
            return This.MDIER.MCMP1DE;
         when Compare_2_DMA =>
            return This.MDIER.MCMP2DE;
         when Compare_3_DMA =>
            return This.MDIER.MCMP3DE;
         when Compare_4_DMA =>
            return This.MDIER.MCMP4DE;
         when Repetition_DMA =>
            return This.MDIER.MREPDE;
         when Sync_Input_DMA =>
            return This.MDIER.SYNCDE;
         when Update_DMA =>
            return This.MDIER.MUPDDE;
      end case;
   end DMA_Source_Enabled;

   ----------------------------------------------------------------------------

   --  HRTimer A to E functions -----------------------------------------------

   ----------------------------------------------------------------------------

   ------------
   -- Enable --
   ------------

   procedure Enable (This : HRTimer_Channel) is
   begin
      if This'Address = HRTIM_TIMA_Base then
         HRTIM_Master_Periph.MCR.TACEN := True;
      elsif This'Address = HRTIM_TIMB_Base then
         HRTIM_Master_Periph.MCR.TBCEN := True;
      elsif This'Address = HRTIM_TIMC_Base then
         HRTIM_Master_Periph.MCR.TCCEN := True;
      elsif This'Address = HRTIM_TIMD_Base then
         HRTIM_Master_Periph.MCR.TDCEN := True;
      elsif This'Address = HRTIM_TIME_Base then
         HRTIM_Master_Periph.MCR.TECEN := True;
      elsif This'Address = HRTIM_TIMF_Base then
         HRTIM_Master_Periph.MCR.TFCEN := True;
      end if;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : HRTimer_Channel) is
   begin
      if This'Address = HRTIM_TIMA_Base then
         --  Test if HRTimer A has no outputs enabled.
         if not (HRTimer_Common_Periph.OENR.TA1OEN or
                 HRTimer_Common_Periph.OENR.TA2OEN)
         then
            HRTIM_Master_Periph.MCR.TACEN := False;
         end if;

      elsif This'Address = HRTIM_TIMB_Base then
         --  Test if HRTimer B has no outputs enabled.
         if not (HRTimer_Common_Periph.OENR.TB1OEN or
                 HRTimer_Common_Periph.OENR.TB2OEN)
         then
            HRTIM_Master_Periph.MCR.TBCEN := False;
         end if;

      elsif This'Address = HRTIM_TIMC_Base then
         --  Test if HRTimer C has no outputs enabled.
         if not (HRTimer_Common_Periph.OENR.TC1OEN or
                 HRTimer_Common_Periph.OENR.TC2OEN)
         then
            HRTIM_Master_Periph.MCR.TCCEN := False;
         end if;

      elsif This'Address = HRTIM_TIMD_Base then
         --  Test if HRTimer D has no outputs enabled.
         if not (HRTimer_Common_Periph.OENR.TD1OEN or
                 HRTimer_Common_Periph.OENR.TD2OEN)
         then
            HRTIM_Master_Periph.MCR.TDCEN := False;
         end if;

      elsif This'Address = HRTIM_TIME_Base then
         --  Test if HRTimer E has no outputs enabled.
         if not (HRTimer_Common_Periph.OENR.TE1OEN or
                 HRTimer_Common_Periph.OENR.TE2OEN)
         then
            HRTIM_Master_Periph.MCR.TECEN := False;
         end if;

      elsif This'Address = HRTIM_TIMF_Base then
         --  Test if HRTimer E has no outputs enabled.
         if not (HRTimer_Common_Periph.OENR.TF1OEN or
                 HRTimer_Common_Periph.OENR.TF2OEN)
         then
            HRTIM_Master_Periph.MCR.TFCEN := False;
         end if;
      end if;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : HRTimer_Channel) return Boolean is
   begin
      if This'Address = HRTIM_TIMA_Base then
         return HRTIM_Master_Periph.MCR.TACEN;
      elsif This'Address = HRTIM_TIMB_Base then
         return HRTIM_Master_Periph.MCR.TBCEN;
      elsif This'Address = HRTIM_TIMC_Base then
         return HRTIM_Master_Periph.MCR.TCCEN;
      elsif This'Address = HRTIM_TIMD_Base then
         return HRTIM_Master_Periph.MCR.TDCEN;
      elsif This'Address = HRTIM_TIME_Base then
         return HRTIM_Master_Periph.MCR.TECEN;
      elsif This'Address = HRTIM_TIMF_Base then
         return HRTIM_Master_Periph.MCR.TFCEN;
      end if;
      return False;
   end Enabled;

   --------------------------
   -- Set_Register_Preload --
   --------------------------

   procedure Set_Register_Preload
     (This    : in out HRTimer_Channel;
      Enabled : Boolean)
   is
   begin
      This.TIMxCR.PREEN := Enabled;
   end Set_Register_Preload;

   -------------------------
   -- Configure_Prescaler --
   -------------------------

   procedure Configure_Prescaler
     (This      : in out HRTimer_Channel;
      Prescaler : HRTimer_Prescaler)
   is
   begin
      This.TIMxCR.CKPSCx := Prescaler'Enum_Rep;
   end Configure_Prescaler;

   -----------------------
   -- Current_Prescaler --
   -----------------------

   function Current_Prescaler (This : HRTimer_Channel) return HRTimer_Prescaler is
   begin
      return HRTimer_Prescaler'Val (This.TIMxCR.CKPSCx);
   end Current_Prescaler;

   -----------------------
   -- Set_PushPull_Mode --
   -----------------------

   procedure Set_PushPull_Mode (This : in out HRTimer_Channel; Mode : Boolean) is
   begin
      This.TIMxCR.PSHPLL := Mode;
   end Set_PushPull_Mode;

   -------------------------------------
   -- Configure_Synchronization_Input --
   -------------------------------------

   procedure Configure_Synchronization_Input
     (This   : in out HRTimer_Channel;
      Reset  : Boolean;
      Start  : Boolean)
   is
   begin
      This.TIMxCR.SYNCRSTx := Reset;
      This.TIMxCR.SYNCSTRTx := Start;
   end Configure_Synchronization_Input;

   -------------------------------------------
   -- Configure_DAC_Synchronization_Trigger --
   -------------------------------------------

   procedure Configure_DAC_Synchronization_Trigger
     (This    : in out HRTimer_Channel;
      Trigger : DAC_Synchronization_Trigger)
   is
   begin
      This.TIMxCR.DACSYNC := Trigger'Enum_Rep;
   end Configure_DAC_Synchronization_Trigger;

   ----------------------------------------
   -- Configure_Dual_Channel_DAC_Trigger --
   ----------------------------------------

   procedure Configure_Dual_Channel_DAC_Trigger
     (This   : in out HRTimer_Channel;
      Enable : Boolean;
      Step   : DAC_Step_Trigger;
      Reset  : DAC_Reset_Trigger)
   is
   begin
      This.TIMxCR2.DCDS := Step = Output_1_Reset_Event;
      This.TIMxCR2.DCDR := Reset = Output_1_Set_Event;
      This.TIMxCR2.DCDE := Enable;
   end Configure_Dual_Channel_DAC_Trigger;

   -------------------------------------------
   -- Configure_Comparator_AutoDelayed_Mode --
   -------------------------------------------

   procedure Configure_Comparator_AutoDelayed_Mode
     (This : in out HRTimer_Channel;
      Mode : CMP_Autodelayed_Mode_Descriptor)
   is
   begin
      case Mode.Selector is
         when CMP2 =>
            This.TIMxCR.DELCMP.Arr (2) := Mode.AutoDelay_1'Enum_Rep;
         when CMP4 =>
            This.TIMxCR.DELCMP.Arr (3) := Mode.AutoDelay_2'Enum_Rep;
      end case;
   end Configure_Comparator_AutoDelayed_Mode;

   -----------------------------------
   -- Configure_Triggered_Half_Mode --
   -----------------------------------

   procedure Configure_Triggered_Half_Mode
     (This : in out HRTimer_Channel;
      Mode : Triggered_Half_Mode)
   is
   begin
      This.TIMxCR2.TRGHLF := Mode = Hardware_Set;
   end Configure_Triggered_Half_Mode;

   ----------------------------
   -- Configure_GTC_PWM_Mode --
   ----------------------------

   procedure Configure_GTC_PWM_Mode
     (This : in out HRTimer_Channel;
      Comp : GTC_Comparator;
      Mode : GTC_PWM_Event)
   is
   begin
      case Comp is
         when CMP1 =>
            This.TIMxCR2.GTCMP.Arr (1) := Mode = Counter_Greater_Then_Compare;
         when CMP3 =>
            This.TIMxCR2.GTCMP.Arr (2) := Mode = Counter_Greater_Then_Compare;
      end case;
   end Configure_GTC_PWM_Mode;

   ---------------------------------------
   -- Configure_Register_Preload_Update --
   ---------------------------------------

   procedure Configure_Register_Preload_Update
     (This    : in out HRTimer_Channel;
      Event   : HRTimer_Update_Event;
      Enabled : Boolean)
   is
   begin
      case Event is
         when Repetition_Counter_Reset =>
            This.TIMxCR.TxREPU := Enabled;
         when Counter_Reset =>
            This.TIMxCR.TxRSTU := Enabled;
         when TimerA_Update =>
            This.TIMxCR.TAU := Enabled;
         when TimerB_Update =>
            This.TIMxCR.TBU := Enabled;
         when TimerC_Update =>
            This.TIMxCR.TCU := Enabled;
         when TimerD_Update =>
            This.TIMxCR.TDU := Enabled;
         when TimerE_Update =>
            This.TIMxCR.TEU := Enabled;
         when TimerF_Update =>
            This.TIMxCR.TFU := Enabled;
         when Master_Update =>
            This.TIMxCR.MSTU := Enabled;
      end case;
   end Configure_Register_Preload_Update;

   ----------------------------------
   -- Set_Update_Gating_Mode --
   ----------------------------------

   procedure Set_Update_Gating_Mode
     (This : in out HRTimer_Channel;
      Mode : Update_Gating_Mode)
   is
   begin
      This.TIMxCR.UPDGAT := Mode'Enum_Rep;
   end Set_Update_Gating_Mode;

   -------------------------
   -- Set_HalfPeriod_Mode --
   -------------------------

   procedure Set_HalfPeriod_Mode
     (This : in out HRTimer_Channel;
      Mode : Boolean)
   is
   begin
      This.TIMxCR.HALF := Mode;
   end Set_HalfPeriod_Mode;

   ----------------
   -- Set_Period --
   ----------------

   procedure Set_Period (This : in out HRTimer_Channel;  Value : UInt16) is
   begin
      This.PERxR.PERx := Value;
   end Set_Period;

   --------------------
   -- Current_Period --
   --------------------

   function Current_Period (This : HRTimer_Channel) return UInt16 is
   begin
      return This.PERxR.PERx;
   end Current_Period;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This      : in out HRTimer_Channel;
      Prescaler : HRTimer_Prescaler;
      Period    : UInt16)
   is
   begin
      Configure_Prescaler (This, Prescaler);
      Set_Period (This, Period);
   end Configure;

   ----------------------------------
   -- Compute_Prescaler_and_Period --
   ----------------------------------

   procedure Compute_Prescaler_And_Period
     (This                : HRTimer_Channel;
      Requested_Frequency : UInt32;
      Prescaler           : out HRTimer_Prescaler;
      Period              : out UInt32)
   is
      Max_Period         : constant := 16#FFFF#; --  UInt16'Last
      Prescaler_Enum     : HRTimer_Prescaler := HRTimer_Prescaler'First;
      fHRCK              : UInt32; --  High frequency into HRTIM
      Hardware_Frequency : UInt32; --  fHRTIM
      CK_CNT             : UInt32;
   begin

      Hardware_Frequency := STM32.Device.Get_Clock_Frequency (This);

      if Requested_Frequency > Hardware_Frequency then
         raise Invalid_Request with "Frequency too high";
      end if;

      --  fHRCK is the high-resolution equivalent clock into HRTIM and all
      --  subsequent clocks are derived and synchronous with this source.
      --  Considering the fHRTIM clock period division by 32, it is equivalent
      --  to a frequency of fHRCK = 170 x 32 = 5.44 GHz. The HRtimer
      --  resolutions is tHRCK = 1 / fHRCK = 184 ps.
      fHRCK := Hardware_Frequency * 32;

      loop
         --  Compute the Counter's clock
         CK_CNT := fHRCK / Prescaler_Enum'Enum_Rep;
         --  Determine the CK_CNT periods to achieve the requested frequency
         Period := CK_CNT / Requested_Frequency;

         exit when ((Period <= Max_Period) or
                      (Prescaler_Enum = HRTimer_Prescaler'Last));

         Prescaler_Enum := HRTimer_Prescaler'Succ (Prescaler_Enum);
      end loop;

      if Period > Max_Period then
         raise Invalid_Request with "Frequency too low";
      end if;

      Prescaler := Prescaler_Enum;
   end Compute_Prescaler_And_Period;

   ----------------------
   -- Set_Counter_Mode --
   ----------------------

   procedure Set_Counter_Mode
     (This : in out HRTimer_Channel;
      Mode : HRTimer_Counter_Mode)
   is
   begin
      This.TIMxCR2.UDM := Mode = Down;
   end Set_Counter_Mode;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This         : in out HRTimer_Channel;
      Prescaler    : HRTimer_Prescaler;
      Period       : UInt16;
      Counter_Mode : HRTimer_Counter_Mode)
   is
   begin
      Configure_Prescaler (This, Prescaler);
      Set_Period (This, Period);
      Set_Counter_Mode (This, Counter_Mode);
   end Configure;

   ------------------------------------
   -- Configure_UpDown_RoolOver_Mode --
   ------------------------------------

   procedure Configure_UpDown_RollOver_Mode
     (This  : in out HRTimer_Channel;
      Mode  : UpDown_RollOver_Mode;
      Event : UpDown_RollOver_Event)
   is
   begin
      case Event is
         when Update_IRQ_DMA =>
            This.TIMxCR2.ROM := Mode'Enum_Rep;
         when Output =>
            This.TIMxCR2.OUTROM := Mode'Enum_Rep;
         when ADC =>
            This.TIMxCR2.ADROM := Mode'Enum_Rep;
         when Burst_Mode =>
            This.TIMxCR2.BMROM := Mode'Enum_Rep;
         when Fault_And_Event =>
            This.TIMxCR2.FEROM := Mode'Enum_Rep;
      end case;
   end Configure_UpDown_RollOver_Mode;

   --------------------------------
   -- Set_Counter_Operating_Mode --
   --------------------------------

   procedure Set_Counter_Operating_Mode
     (This : in out HRTimer_Channel;
      Mode : Counter_Operating_Mode)
   is
   begin
      case Mode is
         when SingleShot_NonRetriggerable =>
            This.TIMxCR.CONT := False;
            This.TIMxCR.RETRIG := False;
         when SingleShot_Retriggerable =>
            This.TIMxCR.CONT := False;
            This.TIMxCR.RETRIG := True;
         when Continuous =>
            This.TIMxCR.CONT := True;
            This.TIMxCR.RETRIG := False;
      end case;
   end Set_Counter_Operating_Mode;

   -----------------------
   -- Set_Counter_Value --
   -----------------------

   procedure Set_Counter_Value
     (This  : in out HRTimer_Channel;
      Value : UInt16)
   is
   begin
      This.CNTxR.CNTx := Value;
   end Set_Counter_Value;

   ---------------------------
   -- Current_Counter_Value --
   ---------------------------

   function Current_Counter_Value (This : HRTimer_Channel) return UInt16 is
   begin
      return This.CNTxR.CNTx;
   end Current_Counter_Value;

   ----------------------------
   -- Set_Repetition_Counter --
   ----------------------------

   procedure Set_Repetition_Counter
     (This        : in out HRTimer_Channel;
      Repetitions : UInt8) is
   begin
      This.REPxR.REPx := Repetitions;
   end Set_Repetition_Counter;

   --------------------------------
   -- Current_Repetition_Counter --
   --------------------------------

   function Current_Repetition_Counter (This : HRTimer_Channel) return UInt8 is
   begin
      return This.REPxR.REPx;
   end Current_Repetition_Counter;

   ----------------------------------
   -- Configure_Repetition_Counter --
   ----------------------------------

   procedure Configure_Repetition_Counter
     (This        : in out HRTimer_Channel;
      Repetitions : UInt8;
      Interrupt   : Boolean;
      DMA_Request : Boolean)
   is
   begin
      Set_Repetition_Counter (This, Repetitions);
      This.TIMxDIER.REPIE := Interrupt;
      This.TIMxDIER.REPDE := DMA_Request;
   end Configure_Repetition_Counter;

   -----------------------------
   -- Set_Counter_Reset_Event --
   -----------------------------

   procedure Set_Counter_Reset_Event
     (This   : in out HRTimer_Channel;
      Event  : Counter_Reset_Event;
      Enable : Boolean)
   is
   begin
      if Enable then
         This.RSTxR := This.RSTxR or 2 ** Event'Enum_Rep;
      else
         This.RSTxR := This.RSTxR and not (2 ** Event'Enum_Rep);
      end if;
   end Set_Counter_Reset_Event;

   -----------------------
   -- Set_Compare_Value --
   -----------------------

   procedure Set_Compare_Value
     (This    : in out HRTimer_Channel;
      Compare : HRTimer_Compare_Number;
      Value   : UInt16)
   is
   begin
      case Compare is
         when Compare_1 =>
            This.CMP1xR.CMP1x := Value;
         when Compare_2 =>
            This.CMP2xR.CMP2x := Value;
         when Compare_3 =>
            This.CMP3xR.CMP3x := Value;
         when Compare_4 =>
            This.CMP4xR.CMP4x := Value;
      end case;
   end Set_Compare_Value;

   ---------------------------
   -- Current_Compare_Value --
   ---------------------------

   function Current_Compare_Value
     (This    : HRTimer_Channel;
      Compare : HRTimer_Compare_Number) return UInt16
   is
   begin
      case Compare is
         when Compare_1 =>
            return This.CMP1xR.CMP1x;
         when Compare_2 =>
            return This.CMP2xR.CMP2x;
         when Compare_3 =>
            return This.CMP3xR.CMP3x;
         when Compare_4 =>
            return This.CMP4xR.CMP4x;
      end case;
   end Current_Compare_Value;

   ---------------------------
   -- Current_Capture_Value --
   ---------------------------

   function Current_Capture_Value
     (This    : HRTimer_Channel;
      Capture : HRTimer_Capture_Number) return UInt16
   is
   begin
      case Capture is
         when Capture_1 =>
            return This.CPT1xR.CPT1x;
         when Capture_2 =>
            return This.CPT2xR.CPT2x;
      end case;
   end Current_Capture_Value;

   -----------------------
   -- Set_Capture_Event --
   -----------------------

   procedure Set_Capture_Event
     (This    : in out HRTimer_Channel;
      Capture : HRTimer_Capture_Number;
      Event   : HRTimer_Capture_Event;
      Enable  : Boolean)
   is
   begin
      case Capture is
         when Capture_1 =>
            if Enable then
               This.CPT1xCR := This.CPT1xCR or 2 ** Event'Enum_Rep;
            else
               This.CPT1xCR := This.CPT1xCR and not (2 ** Event'Enum_Rep);
            end if;
         when Capture_2 =>
            if Enable then
               This.CPT2xCR := This.CPT2xCR or 2 ** Event'Enum_Rep;
            else
               This.CPT2xCR := This.CPT2xCR and not (2 ** Event'Enum_Rep);
            end if;
      end case;
   end Set_Capture_Event;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (This   : in out HRTimer_Channel;
      Source : HRTimer_Channel_Interrupt)
   is
   begin
      case Source is
         when Compare_1_Interrupt =>
            This.TIMxDIER.CMP1IE := True;
         when Compare_2_Interrupt =>
            This.TIMxDIER.CMP2IE := True;
         when Compare_3_Interrupt =>
            This.TIMxDIER.CMP3IE := True;
         when Compare_4_Interrupt =>
            This.TIMxDIER.CMP4IE := True;
         when Repetition_Interrupt =>
            This.TIMxDIER.REPIE := True;
         when Update_Interrupt =>
            This.TIMxDIER.UPDIE := True;
         when Capture_1_Interrupt =>
            This.TIMxDIER.CPT1IE := True;
         when Capture_2_Interrupt =>
            This.TIMxDIER.CPT2IE := True;
         when Output_1_Set_Interrupt =>
            This.TIMxDIER.SET1xIE := True;
         when Output_1_Reset_Interrupt =>
            This.TIMxDIER.RSTx1IE := True;
         when Output_2_Set_Interrupt =>
            This.TIMxDIER.SETx2IE := True;
         when Output_2_Reset_Interrupt =>
            This.TIMxDIER.RSTx2IE := True;
         when Reset_RollOver_Interrupt =>
            This.TIMxDIER.RSTIE := True;
         when Delayed_Protection_Interrupt =>
            This.TIMxDIER.DLYPRTIE := True;
      end case;
   end Enable_Interrupt;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (This    : in out HRTimer_Channel;
      Sources : HRTimer_Channel_Interrupt_List)
   is
   begin
      for Source of Sources loop
         case Source is
            when Compare_1_Interrupt =>
               This.TIMxDIER.CMP1IE := True;
            when Compare_2_Interrupt =>
               This.TIMxDIER.CMP2IE := True;
            when Compare_3_Interrupt =>
               This.TIMxDIER.CMP3IE := True;
            when Compare_4_Interrupt =>
               This.TIMxDIER.CMP4IE := True;
            when Repetition_Interrupt =>
               This.TIMxDIER.REPIE := True;
            when Update_Interrupt =>
               This.TIMxDIER.UPDIE := True;
            when Capture_1_Interrupt =>
               This.TIMxDIER.CPT1IE := True;
            when Capture_2_Interrupt =>
               This.TIMxDIER.CPT2IE := True;
            when Output_1_Set_Interrupt =>
               This.TIMxDIER.SET1xIE := True;
            when Output_1_Reset_Interrupt =>
               This.TIMxDIER.RSTx1IE := True;
            when Output_2_Set_Interrupt =>
               This.TIMxDIER.SETx2IE := True;
            when Output_2_Reset_Interrupt =>
               This.TIMxDIER.RSTx2IE := True;
            when Reset_RollOver_Interrupt =>
               This.TIMxDIER.RSTIE := True;
            when Delayed_Protection_Interrupt =>
               This.TIMxDIER.DLYPRTIE := True;
         end case;
      end loop;
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt
     (This   : in out HRTimer_Channel;
      Source : HRTimer_Channel_Interrupt)
   is
   begin
      case Source is
         when Compare_1_Interrupt =>
            This.TIMxDIER.CMP1IE := False;
         when Compare_2_Interrupt =>
            This.TIMxDIER.CMP2IE := False;
         when Compare_3_Interrupt =>
            This.TIMxDIER.CMP3IE := False;
         when Compare_4_Interrupt =>
            This.TIMxDIER.CMP4IE := False;
         when Repetition_Interrupt =>
            This.TIMxDIER.REPIE := False;
         when Update_Interrupt =>
            This.TIMxDIER.UPDIE := False;
         when Capture_1_Interrupt =>
            This.TIMxDIER.CPT1IE := False;
         when Capture_2_Interrupt =>
            This.TIMxDIER.CPT2IE := False;
         when Output_1_Set_Interrupt =>
            This.TIMxDIER.SET1xIE := False;
         when Output_1_Reset_Interrupt =>
            This.TIMxDIER.RSTx1IE := False;
         when Output_2_Set_Interrupt =>
            This.TIMxDIER.SETx2IE := False;
         when Output_2_Reset_Interrupt =>
            This.TIMxDIER.RSTx2IE := False;
         when Reset_RollOver_Interrupt =>
            This.TIMxDIER.RSTIE := False;
         when Delayed_Protection_Interrupt =>
            This.TIMxDIER.DLYPRTIE := False;
      end case;
   end Disable_Interrupt;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This   : HRTimer_Channel;
      Source : HRTimer_Channel_Interrupt) return Boolean
   is
   begin
      case Source is
         when Compare_1_Interrupt =>
            return This.TIMxDIER.CMP1IE;
         when Compare_2_Interrupt =>
            return This.TIMxDIER.CMP2IE;
         when Compare_3_Interrupt =>
            return This.TIMxDIER.CMP3IE;
         when Compare_4_Interrupt =>
            return This.TIMxDIER.CMP4IE;
         when Repetition_Interrupt =>
            return This.TIMxDIER.REPIE;
         when Update_Interrupt =>
            return This.TIMxDIER.UPDIE;
         when Capture_1_Interrupt =>
            return This.TIMxDIER.CPT1IE;
         when Capture_2_Interrupt =>
            return This.TIMxDIER.CPT2IE;
         when Output_1_Set_Interrupt =>
            return This.TIMxDIER.SET1xIE;
         when Output_1_Reset_Interrupt =>
            return This.TIMxDIER.RSTx1IE;
         when Output_2_Set_Interrupt =>
            return This.TIMxDIER.SETx2IE;
         when Output_2_Reset_Interrupt =>
            return This.TIMxDIER.RSTx2IE;
         when Reset_RollOver_Interrupt =>
            return This.TIMxDIER.RSTIE;
         when Delayed_Protection_Interrupt =>
            return This.TIMxDIER.DLYPRTIE;
      end case;
   end Interrupt_Enabled;

   ----------------------
   -- Interrupt_Status --
   ----------------------

   function Interrupt_Status
     (This   : HRTimer_Channel;
      Source : HRTimer_Channel_Interrupt) return Boolean
   is
   begin
      case Source is
         when Compare_1_Interrupt =>
            return This.TIMxISR.CMP.Arr (1);
         when Compare_2_Interrupt =>
            return This.TIMxISR.CMP.Arr (2);
         when Compare_3_Interrupt =>
            return This.TIMxISR.CMP.Arr (3);
         when Compare_4_Interrupt =>
            return This.TIMxISR.CMP.Arr (4);
         when Repetition_Interrupt =>
            return This.TIMxISR.REP;
         when Update_Interrupt =>
            return This.TIMxISR.UPD;
         when Capture_1_Interrupt =>
            return This.TIMxISR.CPT.Arr (1);
         when Capture_2_Interrupt =>
            return This.TIMxISR.CPT.Arr (2);
         when Output_1_Set_Interrupt =>
            return This.TIMxISR.SETx1;
         when Output_1_Reset_Interrupt =>
            return This.TIMxISR.RSTx1;
         when Output_2_Set_Interrupt =>
            return This.TIMxISR.SETx2;
         when Output_2_Reset_Interrupt =>
            return This.TIMxISR.RSTx2;
         when Reset_RollOver_Interrupt =>
            return This.TIMxISR.RST;
         when Delayed_Protection_Interrupt =>
            return This.TIMxISR.DLYPRT;
      end case;
   end Interrupt_Status;

   -----------------------------
   -- Clear_Pending_Interrupt --
   -----------------------------

   procedure Clear_Pending_Interrupt
     (This   : in out HRTimer_Channel;
      Source : HRTimer_Channel_Interrupt)
   is
   begin
      case Source is
         when Compare_1_Interrupt =>
            This.TIMxICR.CMP1C := True;
         when Compare_2_Interrupt =>
            This.TIMxICR.CMP2C := True;
         when Compare_3_Interrupt =>
            This.TIMxICR.CMP3C := True;
         when Compare_4_Interrupt =>
            This.TIMxICR.CMP4C := True;
         when Repetition_Interrupt =>
            This.TIMxICR.REPC := True;
         when Update_Interrupt =>
            This.TIMxICR.UPDC := True;
         when Capture_1_Interrupt =>
            This.TIMxICR.CPT1C := True;
         when Capture_2_Interrupt =>
            This.TIMxICR.CPT2C := True;
         when Output_1_Set_Interrupt =>
            This.TIMxICR.SET1xC := True;
         when Output_1_Reset_Interrupt =>
            This.TIMxICR.RSTx1C := True;
         when Output_2_Set_Interrupt =>
            This.TIMxICR.SET2xC := True;
         when Output_2_Reset_Interrupt =>
            This.TIMxICR.RSTx2C := True;
         when Reset_RollOver_Interrupt =>
            This.TIMxICR.RSTC := True;
         when Delayed_Protection_Interrupt =>
            This.TIMxICR.DLYPRTC := True;
      end case;
   end Clear_Pending_Interrupt;

   -----------------------
   -- Enable_DMA_Source --
   -----------------------

   procedure Enable_DMA_Source
     (This   : in out HRTimer_Channel;
      Source : HRTimer_Channel_DMA_Request)
   is
   begin
      case Source is
         when Compare_1_DMA =>
            This.TIMxDIER.CMP1DE := True;
         when Compare_2_DMA =>
            This.TIMxDIER.CMP2DE := True;
         when Compare_3_DMA =>
            This.TIMxDIER.CMP3DE := True;
         when Compare_4_DMA =>
            This.TIMxDIER.CMP4DE := True;
         when Repetition_DMA =>
            This.TIMxDIER.REPDE := True;
         when Update_DMA =>
            This.TIMxDIER.UPDDE := True;
         when Capture_1_DMA =>
            This.TIMxDIER.CPT1DE := True;
         when Capture_2_DMA =>
            This.TIMxDIER.CPT2DE := True;
         when Output_1_Set_DMA =>
            This.TIMxDIER.SET1xDE := True;
         when Output_1_Reset_DMA =>
            This.TIMxDIER.RSTx1DE := True;
         when Output_2_Set_DMA =>
            This.TIMxDIER.SETx2DE := True;
         when Output_2_Reset_DMA =>
            This.TIMxDIER.RSTx2DE := True;
         when Reset_RollOver_DMA =>
            This.TIMxDIER.RSTDE := True;
         when Delayed_Protection_DMA =>
            This.TIMxDIER.DLYPRTDE := True;
      end case;
   end Enable_DMA_Source;

   ------------------------
   -- Disable_DMA_Source --
   ------------------------

   procedure Disable_DMA_Source
     (This   : in out HRTimer_Channel;
      Source : HRTimer_Channel_DMA_Request)
   is
   begin
      case Source is
         when Compare_1_DMA =>
            This.TIMxDIER.CMP1DE := False;
         when Compare_2_DMA =>
            This.TIMxDIER.CMP2DE := False;
         when Compare_3_DMA =>
            This.TIMxDIER.CMP3DE := False;
         when Compare_4_DMA =>
            This.TIMxDIER.CMP4DE := False;
         when Repetition_DMA =>
            This.TIMxDIER.REPDE := False;
         when Update_DMA =>
            This.TIMxDIER.UPDDE := False;
         when Capture_1_DMA =>
            This.TIMxDIER.CPT1DE := False;
         when Capture_2_DMA =>
            This.TIMxDIER.CPT2DE := False;
         when Output_1_Set_DMA =>
            This.TIMxDIER.SET1xDE := False;
         when Output_1_Reset_DMA =>
            This.TIMxDIER.RSTx1DE := False;
         when Output_2_Set_DMA =>
            This.TIMxDIER.SETx2DE := False;
         when Output_2_Reset_DMA =>
            This.TIMxDIER.RSTx2DE := False;
         when Reset_RollOver_DMA =>
            This.TIMxDIER.RSTDE := False;
         when Delayed_Protection_DMA =>
            This.TIMxDIER.DLYPRTDE := False;
      end case;
   end Disable_DMA_Source;

   -----------------------
   -- DMA_Source_Enabled --
   -----------------------

   function DMA_Source_Enabled
     (This   : HRTimer_Channel;
      Source : HRTimer_Channel_DMA_Request) return Boolean
   is
   begin
      case Source is
         when Compare_1_DMA =>
            return This.TIMxDIER.CMP1DE;
         when Compare_2_DMA =>
            return This.TIMxDIER.CMP2DE;
         when Compare_3_DMA =>
            return This.TIMxDIER.CMP3DE;
         when Compare_4_DMA =>
            return This.TIMxDIER.CMP4DE;
         when Repetition_DMA =>
            return This.TIMxDIER.REPDE;
         when Update_DMA =>
            return This.TIMxDIER.UPDDE;
         when Capture_1_DMA =>
            return This.TIMxDIER.CPT1DE;
         when Capture_2_DMA =>
            return This.TIMxDIER.CPT2DE;
         when Output_1_Set_DMA =>
            return This.TIMxDIER.SET1xDE;
         when Output_1_Reset_DMA =>
            return This.TIMxDIER.RSTx1DE;
         when Output_2_Set_DMA =>
            return This.TIMxDIER.SETx2DE;
         when Output_2_Reset_DMA =>
            return This.TIMxDIER.RSTx2DE;
         when Reset_RollOver_DMA =>
            return This.TIMxDIER.RSTDE;
         when Delayed_Protection_DMA =>
            return This.TIMxDIER.DLYPRTDE;
      end case;
   end DMA_Source_Enabled;

   -------------------
   --  Set_Deadtime --
   -------------------

   procedure Set_Deadtime (This : in out HRTimer_Channel; Enable : Boolean) is
   begin
      This.OUTxR.DTEN := Enable;
   end Set_Deadtime;

   ----------------------
   -- Enabled_Deadtime --
   ----------------------

   function Enabled_Deadtime (This : HRTimer_Channel) return Boolean is
   begin
      return This.OUTxR.DTEN;
   end Enabled_Deadtime;

   ------------------------
   -- Configure_Deadtime --
   ------------------------

   procedure Configure_Deadtime
     (This          : in out HRTimer_Channel;
      Prescaler     : HRTimer_Deadtime_Prescaler;
      Rising_Value  : UInt9;
      Rising_Sign   : HRTimer_Deadtime_Sign := Positive_Sign;
      Falling_Value : UInt9;
      Falling_Sign  : HRTimer_Deadtime_Sign := Positive_Sign)
   is
   begin
      --  The deadtime (generator) time after the prescaler is defined by
      --  tDTG = tHRTIM / 8 * 2**DTPRSC. For tHRTIM = 1/144 MHz and
      --  DTPRSC = 0, tDTG = 868 ps; DTPRSC = 1, tDTG = 1.736 ns; and so on.
      This.DTxR.DTPRSC := Prescaler'Enum_Rep;

      --  Two deadtimes can be defined in relationship with the rising edge
      --  and the falling edge of the Output 1 reference waveform.
      --  The final deadtime after rising and falling values is defined by
      --  tDTR = DTRx * tDTG, and tDTF = DTFx * tDTG.
      --  See chapter 27.5.26 at pg. 994 in the RM0440 rev. 6.
      This.DTxR.DTRx := Rising_Value;
      This.DTxR.DTFx := Falling_Value;

      --  The sign determines whether the deadtime is positive or negative
      --  (overlaping signals). See pg. 875 in RM0440 rev. 6.
      This.DTxR.SDTRx := Rising_Sign = Negative_Sign;
      This.DTxR.SDTFx := Falling_Sign = Negative_Sign;
   end Configure_Deadtime;

   ------------------------
   -- Configure_Deadtime --
   ------------------------

   procedure Configure_Deadtime
     (This          : in out HRTimer_Channel;
      Rising_Value  : Float;
      Rising_Sign   : HRTimer_Deadtime_Sign := Positive_Sign;
      Falling_Value : Float;
      Falling_Sign  : HRTimer_Deadtime_Sign := Positive_Sign)
   is
      Timer_Frequency : constant UInt32 :=
        STM32.Device.Get_Clock_Frequency (This);
      --  The clock frequency of this timer.

      tHRTIM : constant Float := 1.0 / Float (Timer_Frequency);
      --  Time period for one cycle of the input timer frequency.

      Max_Divisor : Float;
      DTPRSC      : UInt3 := 0;
      DTRx        : UInt9;
      DTFx        : UInt9;
   begin
      --  The deadtime (generator) time after the prescaler is defined by
      --  tDTG = tHRTIM / 8 * 2**DTPRSC. The final deadtime after rising
      --  and falling values is defined by tDTR = DTRx * tDTG, and
      --  tDTF = DTFx * tDTG.
      --  See chapter 27.5.26 at pg. 994 in the RM0440 rev. 6.

      --  Test if the rising or falling deadtime is too large, that is, if
      --  it is greater then the maximum values of prescaler and rising or
      --  falling divisors.
      Max_Divisor := Float'Max (Rising_Value, Falling_Value) / (tHRTIM / 8.0);

      --  DTRx register is type UInt9, DTPRSC register is type UInt3. The value
      --  to be compared is 2 ** Natural (UInt3'Last) * UInt9'Last = 16#FFFF#,
      --  this is UInt16'Last.
      pragma Assert (UInt32 (Max_Divisor) > 16#FFFF#,
                     "This deadtime is too large.");

      --  Find the minimum prescaler value.
      while Max_Divisor > Float (2 ** Natural (DTPRSC) * UInt9'Last) loop
         DTPRSC := DTPRSC + 1;
      end loop;

      --  Save the minimum prescaler value into the prescaler register.
      This.DTxR.DTPRSC := DTPRSC;

      --  Calculates the rising deadtime value and save it into the register.
      DTRx := UInt9 (Rising_Value / (tHRTIM / 8.0) / Float (2 ** Natural (DTPRSC)));
      This.DTxR.DTRx := DTRx;

      --  Calculates the falling deadtime value and save it into the register.
      DTFx := UInt9 (Falling_Value / (tHRTIM / 8.0) / Float (2 ** Natural (DTPRSC)));
      This.DTxR.DTFx := DTFx;

      --  The sign determines whether the deadtime is positive or negative
      --  (overlaping signals). See pg. 875 in RM0440 rev. 6.
      This.DTxR.SDTRx := Rising_Sign = Negative_Sign;
      This.DTxR.SDTFx := Falling_Sign = Negative_Sign;
   end Configure_Deadtime;

   --------------------------
   -- Enable_Deadtime_Lock --
   --------------------------

   procedure Enable_Deadtime_Lock
     (This : in out HRTimer_Channel; Lock : Deadtime_Lock) is
   begin
      if not This.DTxR.DTRLKx then
         This.DTxR.DTRLKx := Lock.Rising_Value;
      end if;
      if not This.DTxR.DTRSLKx then
         This.DTxR.DTRSLKx := Lock.Rising_Sign;
      end if;
      if not This.DTxR.DTFLKx then
         This.DTxR.DTFLKx := Lock.Falling_Value;
      end if;
      if not This.DTxR.DTFSLKx then
         This.DTxR.DTFSLKx := Lock.Falling_Sign;
      end if;
   end Enable_Deadtime_Lock;

   ------------------------
   -- Read_Deadtime_Lock --
   ------------------------

   function Read_Deadtime_Lock (This : HRTimer_Channel) return Deadtime_Lock is
      Lock : Deadtime_Lock;
   begin
      Lock.Rising_Value := This.DTxR.DTRLKx;
      Lock.Rising_Sign := This.DTxR.DTRSLKx;
      Lock.Falling_Value := This.DTxR.DTFLKx;
      Lock.Falling_Sign := This.DTxR.DTFSLKx;
      return Lock;
   end Read_Deadtime_Lock;

   ------------------------------
   -- Set_Channel_Output_State --
   ------------------------------

   procedure Set_Channel_Output_State
     (This  : in out HRTimer_Channel;
      Out_1 : Output_State;
      Out_2 : Output_State)
   is
   begin
      if Out_1 = Low then
         This.RSTx1R := This.RSTx1R or 16#01#;
      else
         This.SETx1R := This.SETx1R or 16#01#;
      end if;
      if Out_2 = Low then
         This.RSTx2R := This.RSTx2R or 16#01#;
      else
         This.SETx2R := This.SETx2R or 16#01#;
      end if;
   end Set_Channel_Output_State;

   ------------------------------------
   -- Configure_Channel_Output_Event --
   ------------------------------------

   procedure Configure_Channel_Output_Event
     (This        : in out HRTimer_Channel;
      Output      : HRTimer_Channel_Output;
      Set_Event   : Output_Event;
      Reset_Event : Output_Event)
   is
   begin
      case Output is
         when Output_1 =>
            This.SETx1R := 2 ** Set_Event'Enum_Rep;
            This.RSTx1R := 2 ** Reset_Event'Enum_Rep;
         when Output_2 =>
            This.SETx2R := 2 ** Set_Event'Enum_Rep;
            This.RSTx2R := 2 ** Reset_Event'Enum_Rep;
      end case;
   end Configure_Channel_Output_Event;

   ------------------------------------
   -- Configure_Channel_Output_Event --
   ------------------------------------

   procedure Configure_Channel_Output_Event
     (This       : in out HRTimer_Channel;
      Output     : HRTimer_Channel_Output;
      Event      : Output_Event;
      Event_Type : Output_Event_Type;
      Enabled    : Boolean)
   is
   begin
      case Output is
         when Output_1 =>
            case Event_Type is
               when Reset_Event =>
                  if Enabled then
                     This.RSTx1R := This.RSTx1R or 2 ** Event'Enum_Rep;
                  else
                     This.RSTx1R := This.RSTx1R and not (2 ** Event'Enum_Rep);
                  end if;
               when Set_Event =>
                  if Enabled then
                     This.SETx1R := This.SETx1R or 2 ** Event'Enum_Rep;
                  else
                     This.SETx1R := This.SETx1R and not (2 ** Event'Enum_Rep);
                  end if;
            end case;
         when Output_2 =>
            case Event_Type is
               when Reset_Event =>
                  if Enabled then
                     This.RSTx2R := This.RSTx2R or 2 ** Event'Enum_Rep;
                  else
                     This.RSTx2R := This.RSTx2R and not (2 ** Event'Enum_Rep);
                  end if;
               when Set_Event =>
                  if Enabled then
                     This.SETx2R := This.SETx2R or 2 ** Event'Enum_Rep;
                  else
                     This.SETx2R := This.SETx2R and not (2 ** Event'Enum_Rep);
                  end if;
            end case;
      end case;
   end Configure_Channel_Output_Event;

   ------------------------------
   -- Configure_External_Event --
   ------------------------------

   procedure Configure_External_Event
     (This         : in out HRTimer_Channel;
      Event_Number : External_Event_Number;
      Event_Latch  : External_Event_Latch;
      Event_Filter : External_Event_Blanking_Filter)
   is
   begin
      case Event_Number is
         when Event_1 =>
            This.EEFxR1.EE1LTCH := Event_Latch = Latch;
            This.EEFxR1.EE1FLTR := Event_Filter'Enum_Rep;
         when Event_2 =>
            This.EEFxR1.EE2LTCH := Event_Latch = Latch;
            This.EEFxR1.EE2FLTR := Event_Filter'Enum_Rep;
         when Event_3 =>
            This.EEFxR1.EE3LTCH := Event_Latch = Latch;
            This.EEFxR1.EE3FLTR := Event_Filter'Enum_Rep;
         when Event_4 =>
            This.EEFxR1.EE4LTCH := Event_Latch = Latch;
            This.EEFxR1.EE4FLTR := Event_Filter'Enum_Rep;
         when Event_5 =>
            This.EEFxR1.EE5LTCH := Event_Latch = Latch;
            This.EEFxR1.EE5FLTR := Event_Filter'Enum_Rep;
         when Event_6 =>
            This.EEFxR2.EE6LTCH := Event_Latch = Latch;
            This.EEFxR2.EE6FLTR := Event_Filter'Enum_Rep;
         when Event_7 =>
            This.EEFxR2.EE7LTCH := Event_Latch = Latch;
            This.EEFxR2.EE7FLTR := Event_Filter'Enum_Rep;
         when Event_8 =>
            This.EEFxR2.EE8LTCH := Event_Latch = Latch;
            This.EEFxR2.EE8FLTR := Event_Filter'Enum_Rep;
         when Event_9 =>
            This.EEFxR2.EE9LTCH := Event_Latch = Latch;
            This.EEFxR2.EE9FLTR := Event_Filter'Enum_Rep;
         when Event_10 =>
            This.EEFxR2.EE10LTCH := Event_Latch = Latch;
            This.EEFxR2.EE10FLTR := Event_Filter'Enum_Rep;
      end case;

   end Configure_External_Event;

   -----------------------------------------
   -- Set_External_Event_A_Counter_Enable --
   -----------------------------------------

   procedure Set_External_Event_A_Counter_Enable
     (This   : in out HRTimer_Channel;
      Enable : Boolean)
   is
   begin
      This.EEFxR3.EEVACE := Enable;
   end Set_External_Event_A_Counter_Enable;

   ----------------------------------------
   -- Set_External_Event_A_Counter_Value --
   ----------------------------------------

   procedure Set_External_Event_A_Counter_Value
     (This  : in out HRTimer_Channel;
      Value : UInt6)
   is
   begin
      This.EEFxR3.EEVACNT := Value;
   end Set_External_Event_A_Counter_Value;

   ----------------------------------------
   -- Set_External_Event_A_Counter_Reset --
   ----------------------------------------

   procedure Set_External_Event_A_Counter_Reset
     (This  : in out HRTimer_Channel;
      Event : External_Event_Counter_Reset)
   is
   begin
      case Event is
         when Imediate =>
            This.EEFxR3.EEVACRES := True;
         when Reset_RollOver =>
            This.EEFxR3.EEVARSTM := False;
         when Reset_RollOver_No_Event =>
            This.EEFxR3.EEVARSTM := True;
      end case;
   end Set_External_Event_A_Counter_Reset;

   ---------------------------------
   -- Set_External_Event_A_Source --
   ---------------------------------

   procedure Set_External_Event_A_Source
     (This   : in out HRTimer_Channel;
      Source : External_Event_Number)
   is
   begin
      This.EEFxR3.EEVASEL := Source'Enum_Rep;
   end Set_External_Event_A_Source;

   --------------------------------
   -- Configure_External_Event_A --
   --------------------------------

   procedure Configure_External_Event_A
     (This   : in out HRTimer_Channel;
      Enable : Boolean;
      Value  : UInt6;
      Event  : External_Event_Counter_Reset;
      Source : External_Event_Number)
   is
   begin
      if This.EEFxR3.EEVACE then
         This.EEFxR3.EEVACE := False;
      end if;
      This.EEFxR3.EEVACNT := Value;
      case Event is
         when Imediate =>
            This.EEFxR3.EEVACRES := True;
         when Reset_RollOver =>
            This.EEFxR3.EEVARSTM := False;
         when Reset_RollOver_No_Event =>
            This.EEFxR3.EEVARSTM := True;
      end case;
      This.EEFxR3.EEVASEL := Source'Enum_Rep;
      This.EEFxR3.EEVACE := Enable;
   end Configure_External_Event_A;

   ----------------------
   -- Set_Chopper_Mode --
   ----------------------

   procedure Set_Chopper_Mode
     (This    : in out HRTimer_Channel;
      Output1 : Boolean;
      Output2 : Boolean)
   is
   begin
      This.OUTxR.CHP1 := Output1;
      This.OUTxR.CHP2 := Output2;
   end Set_Chopper_Mode;

   --------------------------
   -- Enabled_Chopper_Mode --
   --------------------------

   function Enabled_Chopper_Mode
     (This   : HRTimer_Channel;
      Output : HRTimer_Channel_Output) return Boolean is
   begin
      case Output is
         when Output_1 =>
            return This.OUTxR.CHP1;
         when Output_2 =>
            return This.OUTxR.CHP2;
      end case;
   end Enabled_Chopper_Mode;

   ----------------------------
   -- Configure_Chopper_Mode --
   ----------------------------

   procedure Configure_Chopper_Mode
     (This              : in out HRTimer_Channel;
      Output1           : Boolean;
      Output2           : Boolean;
      Carrier_Frequency : Chopper_Carrier_Frequency;
      Duty_Cycle        : Chopper_Duty_Cycle;
      Start_PulseWidth  : Chopper_Start_PulseWidth)
   is
   begin
      Set_Chopper_Mode (This, Output1, Output2);
      This.CHPxR.CHPFRQ := Carrier_Frequency'Enum_Rep;
      This.CHPxR.CHPDTY := Duty_Cycle'Enum_Rep;
      This.CHPxR.STRTPW := Start_PulseWidth'Enum_Rep;
   end Configure_Chopper_Mode;

   ---------------------------
   -- Set_Burst_Mode_Output --
   ---------------------------

   procedure Set_Burst_Mode_Idle_Output
     (This   : in out HRTimer_Channel;
      Output : HRTimer_Channel_Output;
      Mode   : Burst_Mode_Idle_Output)
   is
   begin
      case Output is
         when Output_1 =>
            case Mode is
               when No_Action =>
                  This.OUTxR.IDLEM1 := False;
                  This.OUTxR.IDLES1 := False;
               when Inactive =>
                  This.OUTxR.IDLEM1 := True;
                  This.OUTxR.IDLES1 := False;
               when Active =>
                  This.OUTxR.IDLEM1 := True;
                  This.OUTxR.IDLES1 := True;
            end case;
         when Output_2 =>
            case Mode is
               when No_Action =>
                  This.OUTxR.IDLEM2 := False;
                  This.OUTxR.IDLES2 := False;
               when Inactive =>
                  This.OUTxR.IDLEM2 := True;
                  This.OUTxR.IDLES2 := False;
               when Active =>
                  This.OUTxR.IDLEM2 := True;
                  This.OUTxR.IDLES2 := True;
            end case;
      end case;
   end Set_Burst_Mode_Idle_Output;

   ----------------------------------
   -- Set_Deadtime_Burst_Mode_Idle --
   ----------------------------------

   procedure Set_Deadtime_Burst_Mode_Idle
     (This   : in out HRTimer_Channel;
      Output : HRTimer_Channel_Output;
      Enable : Boolean)
   is
   begin
      case Output is
         when Output_1 =>
            This.OUTxR.DIDL1 := Enable;
         when Output_2 =>
            This.OUTxR.DIDL2 := Enable;
      end case;
   end Set_Deadtime_Burst_Mode_Idle;

   ---------------------------------
   -- Set_Channel_Output_Polarity --
   ---------------------------------

   procedure Set_Channel_Output_Polarity
     (This     : in out HRTimer_Channel;
      Output   : HRTimer_Channel_Output;
      Polarity : Channel_Output_Polarity)
   is
   begin
      case Output is
         when Output_1 =>
            This.OUTxR.POL1 := Polarity = Low;
         when Output_2 =>
            This.OUTxR.POL2 := Polarity = Low;
      end case;
   end Set_Channel_Output_Polarity;

   -------------------------------------
   -- Current_Channel_Output_Polarity --
   -------------------------------------

   function Current_Channel_Output_Polarity
     (This     : HRTimer_Channel;
      Output   : HRTimer_Channel_Output) return Channel_Output_Polarity
   is
   begin
      case Output is
         when Output_1 =>
            return (if This.OUTxR.POL1 then Low else High);
         when Output_2 =>
            return (if This.OUTxR.POL2 then Low else High);
      end case;
   end Current_Channel_Output_Polarity;

   ------------------------------
   -- Configure_Channel_Output --
   ------------------------------

   procedure Configure_Channel_Output
     (This       : in out HRTimer_Channel;
      Mode       : Counter_Operating_Mode;
      State      : HRTimer_State;
      Output     : HRTimer_Channel_Output;
      Polarity   : Channel_Output_Polarity;
      Idle_State : Boolean)
   is
   begin
      --  Disable the timer before configuration
      Disable (This);

      --  Timer counter operating in continuous mode
      Set_Counter_Operating_Mode (This, Mode);

      --  Choose enable/disable the output 1
      Set_Channel_Output (This, Output, not Idle_State);
      --  Choose output polarity
      Set_Channel_Output_Polarity (This, Output, Polarity);

      --  Choose enable timer channel or not
      if State = Enable then
         Enable (This);
      end if;
   end Configure_Channel_Output;

   ------------------------------
   -- Configure_Channel_Output --
   ------------------------------

   procedure Configure_Channel_Output
     (This       : in out HRTimer_Channel;
      Mode       : Counter_Operating_Mode;
      State      : HRTimer_State;
      Compare    : HRTimer_Compare_Number;
      Pulse      : UInt16;
      Output     : HRTimer_Channel_Output;
      Polarity   : Channel_Output_Polarity;
      Idle_State : Boolean)
   is
      Event : Output_Event;
   begin
      case Compare is
         when Compare_1 =>
            Event := Timer_Compare_1;
         when Compare_2 =>
            Event := Timer_Compare_2;
         when Compare_3 =>
            Event := Timer_Compare_3;
         when Compare_4 =>
            Event := Timer_Compare_4;
      end case;

      --  Disable the timer before configuration
      Disable (This);
      --  Choose compare channel and program its value
      Set_Compare_Value (This, Compare, Pulse);
      --  Timer counter operating in continuous mode
      Set_Counter_Operating_Mode (This, Mode);

      --  Output set on timer period and reset on compare event
      Configure_Channel_Output_Event
        (This,
         Output      => Output,
         Set_Event   => Timer_Period,
         Reset_Event => Event);

      --  Choose enable/disable the output 1
      Set_Channel_Output (This, Output, not Idle_State);

      --  Choose output polarity
      Set_Channel_Output_Polarity (This, Output, Polarity);

      --  Choose enable timer channel or not
      if State = Enable then
         Enable (This);
      end if;
   end Configure_Channel_Output;

   ------------------------------
   -- Configure_Channel_Output --
   ------------------------------

   procedure Configure_Channel_Output
     (This                     : in out HRTimer_Channel;
      Mode                     : Counter_Operating_Mode;
      State                    : HRTimer_State;
      Polarity                 : Channel_Output_Polarity;
      Idle_State               : Boolean;
      Complementary_Polarity   : Channel_Output_Polarity;
      Complementary_Idle_State : Boolean)
   is
   begin
      --  Disable the timer before configuration
      Disable (This);

      --  Timer counter operating in continuous mode
      Set_Counter_Operating_Mode (This, Mode);

      --  Choose enable/disable the output 1
      Set_Channel_Output (This, Output_1, not Idle_State);
      --  Choose output 1 polarity
      Set_Channel_Output_Polarity (This, Output_1, Polarity);

      --  Choose enable/disable the output 2
      Set_Channel_Output (This, Output_2, not Complementary_Idle_State);
      --  Choose output 2 polarity
      Set_Channel_Output_Polarity (This, Output_2, Complementary_Polarity);

      --  Choose enable timer channel or not
      if State = Enable then
         Enable (This);
      end if;
   end Configure_Channel_Output;

   ------------------------------
   -- Configure_Channel_Output --
   ------------------------------

   procedure Configure_Channel_Output
     (This                     : in out HRTimer_Channel;
      Mode                     : Counter_Operating_Mode;
      State                    : HRTimer_State;
      Compare                  : HRTimer_Compare_Number;
      Pulse                    : UInt16;
      Polarity                 : Channel_Output_Polarity;
      Idle_State               : Boolean;
      Complementary_Polarity   : Channel_Output_Polarity;
      Complementary_Idle_State : Boolean)
   is
      Event : Output_Event;
   begin
      case Compare is
         when Compare_1 =>
            Event := Timer_Compare_1;
         when Compare_2 =>
            Event := Timer_Compare_2;
         when Compare_3 =>
            Event := Timer_Compare_3;
         when Compare_4 =>
            Event := Timer_Compare_4;
      end case;

      --  Disable the timer before configuration
      Disable (This);
      --  Choose compare channel and program its value
      Set_Compare_Value (This, Compare, Pulse);
      --  Timer counter operating in continuous mode
      Set_Counter_Operating_Mode (This, Mode);

      --  Output 1 set on timer period and reset on compare event
      Configure_Channel_Output_Event
        (This,
         Output => Output_1,
         Set_Event => Timer_Period,
         Reset_Event => Event);
      --  Choose enable/disable the output 1
      Set_Channel_Output (This, Output_1, not Idle_State);
      --  Choose output 1 polarity
      Set_Channel_Output_Polarity (This, Output_1, Polarity);

      --  Output 2 set on timer period and reset on compare event
      Configure_Channel_Output_Event
        (This,
         Output => Output_2,
         Set_Event => Event,
         Reset_Event => Timer_Period);
      --  Choose enable/disable the output 2
      Set_Channel_Output (This, Output_2, not Complementary_Idle_State);
      --  Choose output 2 polarity
      Set_Channel_Output_Polarity (This, Output_2, Complementary_Polarity);

      --  Choose enable timer channel or not
      if State = Enable then
         Enable (This);
      end if;
   end Configure_Channel_Output;

   ---------------------------------
   -- Set_Delayed_Idle_Protection --
   ---------------------------------

   procedure Set_Delayed_Idle_Protection
     (This   : in out HRTimer_Channel;
      Option : Delayed_Idle_Protection)
   is
   begin
      if not Option.Enabled then
         This.OUTxR.DLYPRTEN := False;
      else
         This.OUTxR.DLYPRT := Option.Value'Enum_Rep;
         This.OUTxR.DLYPRTEN := True;
      end if;
   end Set_Delayed_Idle_Protection;

   -------------------------------------
   -- Current_Delayed_Idle_Protection --
   -------------------------------------

   function Current_Delayed_Idle_Protection
     (This : in out HRTimer_Channel) return Delayed_Idle_Protection
   is
      Protection : Delayed_Idle_Protection;
   begin
      Protection.Enabled := This.OUTxR.DLYPRTEN;
      Protection.Value := Delayed_Idle_Protection_Enum'Val (This.OUTxR.DLYPRT);
      return Protection;
   end Current_Delayed_Idle_Protection;

   ---------------------------------
   -- Set_Balanced_Idle_Automatic --
   ---------------------------------

   procedure Set_Balanced_Idle_Automatic
     (This   : in out HRTimer_Channel;
      Enable : Boolean)
   is
   begin
      This.OUTxR.BIAR := Enable;
   end Set_Balanced_Idle_Automatic;

   ------------------------
   -- Set_Fault_Source --
   ------------------------

   procedure Set_Fault_Source
     (This   : in out HRTimer_Channel;
      Source : HRTimer_Fault_Source;
      Enable : Boolean) is
   begin
      case Source is
         when Fault_1 =>
            This.FLTxR.FLT1EN := Enable;
         when Fault_2 =>
            This.FLTxR.FLT2EN := Enable;
         when Fault_3 =>
            This.FLTxR.FLT3EN := Enable;
         when Fault_4 =>
            This.FLTxR.FLT4EN := Enable;
         when Fault_5 =>
            This.FLTxR.FLT5EN := Enable;
         when Fault_6 =>
            This.FLTxR.FLT6EN := Enable;
      end case;
   end Set_Fault_Source;

   --------------------------
   -- Enabled_Fault_Source --
   --------------------------

   function Enabled_Fault_Source
     (This : HRTimer_Channel;
      Source : HRTimer_Fault_Source) return Boolean is
   begin
      case Source is
         when Fault_1 =>
            return This.FLTxR.FLT1EN;
         when Fault_2 =>
            return This.FLTxR.FLT2EN;
         when Fault_3 =>
            return This.FLTxR.FLT3EN;
         when Fault_4 =>
            return This.FLTxR.FLT4EN;
         when Fault_5 =>
            return This.FLTxR.FLT5EN;
         when Fault_6 =>
            return This.FLTxR.FLT6EN;
      end case;
   end Enabled_Fault_Source;

   ------------------------------
   -- Enable_Fault_Source_Lock --
   ------------------------------

   procedure Enable_Fault_Source_Lock (This : in out HRTimer_Channel) is
   begin
      This.FLTxR.FLTLCK := True;
   end Enable_Fault_Source_Lock;

   -------------------------------
   -- Enabled_Fault_Source_Lock --
   -------------------------------

   function Enabled_Fault_Source_Lock (This : HRTimer_Channel) return Boolean is
   begin
      return This.FLTxR.FLTLCK;
   end Enabled_Fault_Source_Lock;

   ----------------------------------------------------------------------------

   --  HRTimer Common functions -----------------------------------------------

   ----------------------------------------------------------------------------

   -------------------------
   -- Set_Register_Update --
   -------------------------

   procedure Set_Register_Update
     (Counter : HRTimer;
      Update  : HRTimer_Register_Update)
   is
   begin
      case Counter is
         when HRTimer_M =>
            if Update = Imediate then
               HRTimer_Common_Periph.CR2.MSWU := True;
            else
               HRTimer_Common_Periph.CR1.MUDIS := Update = Disable;
            end if;
         when HRTimer_A =>
            if Update = Imediate then
               HRTimer_Common_Periph.CR2.TASWU := True;
            else
               HRTimer_Common_Periph.CR1.TAUDIS := Update = Disable;
            end if;
         when HRTimer_B =>
            if Update = Imediate then
               HRTimer_Common_Periph.CR2.TBSWU := True;
            else
               HRTimer_Common_Periph.CR1.TBUDIS := Update = Disable;
            end if;
         when HRTimer_C =>
            if Update = Imediate then
               HRTimer_Common_Periph.CR2.TCSWU := True;
            else
               HRTimer_Common_Periph.CR1.TCUDIS := Update = Disable;
            end if;
         when HRTimer_D =>
            if Update = Imediate then
               HRTimer_Common_Periph.CR2.TDSWU := True;
            else
               HRTimer_Common_Periph.CR1.TDUDIS := Update = Disable;
            end if;
         when HRTimer_E =>
            if Update = Imediate then
               HRTimer_Common_Periph.CR2.TESWU := True;
            else
               HRTimer_Common_Periph.CR1.TEUDIS := Update = Disable;
            end if;
         when HRTimer_F =>
            if Update = Imediate then
               HRTimer_Common_Periph.CR2.TFSWU := True;
            else
               HRTimer_Common_Periph.CR1.TFUDIS := Update = Disable;
            end if;
      end case;
   end Set_Register_Update;

   ---------------------------
   -- Enable_Software_Reset --
   ---------------------------

   procedure Enable_Software_Reset (Counter : HRTimer) is
   begin
      case Counter is
         when HRTimer_M =>
            HRTimer_Common_Periph.CR2.MRST := True;
         when HRTimer_A =>
            HRTimer_Common_Periph.CR2.TARST := True;
         when HRTimer_B =>
            HRTimer_Common_Periph.CR2.TBRST := True;
         when HRTimer_C =>
            HRTimer_Common_Periph.CR2.TCRST := True;
         when HRTimer_D =>
            HRTimer_Common_Periph.CR2.TDRST := True;
         when HRTimer_E =>
            HRTimer_Common_Periph.CR2.TERST := True;
         when HRTimer_F =>
            HRTimer_Common_Periph.CR2.TFRST := True;
      end case;
   end Enable_Software_Reset;

   function UInt32_To_CR1 is new Ada.Unchecked_Conversion
     (UInt32, STM32_SVD.HRTIM.CR1_Register);
   function CR1_To_UInt32 is new Ada.Unchecked_Conversion
     (STM32_SVD.HRTIM.CR1_Register, UInt32);
   function UInt32_To_CR2 is new Ada.Unchecked_Conversion
     (UInt32, STM32_SVD.HRTIM.CR2_Register);
   function CR2_To_UInt32 is new Ada.Unchecked_Conversion
     (STM32_SVD.HRTIM.CR2_Register, UInt32);

   -------------------------
   -- Set_Register_Update --
   -------------------------

   procedure Set_Register_Update
     (Counters : HRTimer_List;
      Update   : HRTimer_Register_Update)
   is
      Value : UInt32 := 16#00000000#;
      CR1_Value : UInt32 := CR1_To_UInt32 (HRTimer_Common_Periph.CR1);
      CR2_Value : UInt32 := CR2_To_UInt32 (HRTimer_Common_Periph.CR2);
   begin
      for Counter of Counters loop
         Value := Value or UInt32 (Counter'Enum_Rep);
      end loop;
      case Update is
         when Imediate =>
            CR2_Value := CR2_Value or Value;
            HRTimer_Common_Periph.CR2 := UInt32_To_CR2 (CR2_Value);
         when Enable =>
            CR1_Value := CR1_Value or Value;
            HRTimer_Common_Periph.CR1 := UInt32_To_CR1 (CR1_Value);
         when Disable =>
            CR1_Value := CR1_Value and not Value;
            HRTimer_Common_Periph.CR1 := UInt32_To_CR1 (CR1_Value);
      end case;
   end Set_Register_Update;

   ---------------------------
   -- Enable_Software_Reset --
   ---------------------------

   procedure Enable_Software_Reset
     (Counters : HRTimer_List)
   is
      Value : UInt32 := 16#00000000#;
      CR2_Value : UInt32 := CR2_To_UInt32 (HRTimer_Common_Periph.CR2);
   begin
      for Counter of Counters loop
         Value := Value or Counter'Enum_Rep;
      end loop;

      CR2_Value := CR2_Value or Shift_Left (Value, 8);
      HRTimer_Common_Periph.CR2 := UInt32_To_CR2 (CR2_Value);
   end Enable_Software_Reset;

   ----------------------
   -- Set_Swap_Outputs --
   ----------------------

   procedure Set_Swap_Outputs
     (This : HRTimer_Channel;
      Swap : HRTimer_State)
   is
   begin
      if This'Address = HRTIM_TIMA_Base then
            HRTimer_Common_Periph.CR2.TARST := Swap = Enable;
      elsif This'Address = HRTIM_TIMB_Base then
            HRTimer_Common_Periph.CR2.TBRST := Swap = Enable;
      elsif This'Address = HRTIM_TIMC_Base then
            HRTimer_Common_Periph.CR2.TCRST := Swap = Enable;
      elsif This'Address = HRTIM_TIMD_Base then
            HRTimer_Common_Periph.CR2.TDRST := Swap = Enable;
      elsif This'Address = HRTIM_TIME_Base then
            HRTimer_Common_Periph.CR2.TERST := Swap = Enable;
      elsif This'Address = HRTIM_TIMF_Base then
            HRTimer_Common_Periph.CR2.TFRST := Swap = Enable;
      end if;
   end Set_Swap_Outputs;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt (Source : HRTimer_Common_Interrupt) is
   begin
      case Source is
         when Fault_1_Interrupt =>
            HRTimer_Common_Periph.IER.FLT1IE := True;
         when Fault_2_Interrupt =>
            HRTimer_Common_Periph.IER.FLT2IE := True;
         when Fault_3_Interrupt =>
            HRTimer_Common_Periph.IER.FLT3IE := True;
         when Fault_4_Interrupt =>
            HRTimer_Common_Periph.IER.FLT4IE := True;
         when Fault_5_Interrupt =>
            HRTimer_Common_Periph.IER.FLT5IE := True;
         when System_Fault_Interrupt =>
            HRTimer_Common_Periph.IER.SYSFLTE := True;
         when DLL_Ready_Interrupt =>
            HRTimer_Common_Periph.IER.DLLRDYIE := True;
         when Burst_Mode_Period_Interrupt =>
            HRTimer_Common_Periph.IER.BMPERIE := True;
      end case;
   end Enable_Interrupt;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (Sources : HRTimer_Common_Interrupt_List)
   is
   begin
      for Source of Sources loop
         case Source is
            when Fault_1_Interrupt =>
               HRTimer_Common_Periph.IER.FLT1IE := True;
            when Fault_2_Interrupt =>
               HRTimer_Common_Periph.IER.FLT2IE := True;
            when Fault_3_Interrupt =>
               HRTimer_Common_Periph.IER.FLT3IE := True;
            when Fault_4_Interrupt =>
               HRTimer_Common_Periph.IER.FLT4IE := True;
            when Fault_5_Interrupt =>
               HRTimer_Common_Periph.IER.FLT5IE := True;
            when System_Fault_Interrupt =>
               HRTimer_Common_Periph.IER.SYSFLTE := True;
            when DLL_Ready_Interrupt =>
               HRTimer_Common_Periph.IER.DLLRDYIE := True;
            when Burst_Mode_Period_Interrupt =>
               HRTimer_Common_Periph.IER.BMPERIE := True;
         end case;
      end loop;
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt
     (Source : HRTimer_Common_Interrupt)
   is
   begin
      case Source is
         when Fault_1_Interrupt =>
            HRTimer_Common_Periph.IER.FLT1IE := False;
         when Fault_2_Interrupt =>
            HRTimer_Common_Periph.IER.FLT2IE := False;
         when Fault_3_Interrupt =>
            HRTimer_Common_Periph.IER.FLT3IE := False;
         when Fault_4_Interrupt =>
            HRTimer_Common_Periph.IER.FLT4IE := False;
         when Fault_5_Interrupt =>
            HRTimer_Common_Periph.IER.FLT5IE := False;
         when System_Fault_Interrupt =>
            HRTimer_Common_Periph.IER.SYSFLTE := False;
         when DLL_Ready_Interrupt =>
            HRTimer_Common_Periph.IER.DLLRDYIE := False;
         when Burst_Mode_Period_Interrupt =>
            HRTimer_Common_Periph.IER.BMPERIE := False;
      end case;
   end Disable_Interrupt;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (Source : HRTimer_Common_Interrupt) return Boolean
   is
   begin
      case Source is
         when Fault_1_Interrupt =>
            return HRTimer_Common_Periph.IER.FLT1IE;
         when Fault_2_Interrupt =>
            return HRTimer_Common_Periph.IER.FLT2IE;
         when Fault_3_Interrupt =>
            return HRTimer_Common_Periph.IER.FLT3IE;
         when Fault_4_Interrupt =>
            return HRTimer_Common_Periph.IER.FLT4IE;
         when Fault_5_Interrupt =>
            return HRTimer_Common_Periph.IER.FLT5IE;
         when System_Fault_Interrupt =>
            return HRTimer_Common_Periph.IER.SYSFLTE;
         when DLL_Ready_Interrupt =>
            return HRTimer_Common_Periph.IER.DLLRDYIE;
         when Burst_Mode_Period_Interrupt =>
            return HRTimer_Common_Periph.IER.BMPERIE;
      end case;
   end Interrupt_Enabled;

   ----------------------
   -- Interrupt_Status --
   ----------------------

   function Interrupt_Status
     (Source : HRTimer_Common_Interrupt) return Boolean
   is
   begin
      case Source is
         when Fault_1_Interrupt =>
            return HRTimer_Common_Periph.ISR.FLT.Arr (1);
         when Fault_2_Interrupt =>
            return HRTimer_Common_Periph.ISR.FLT.Arr (2);
         when Fault_3_Interrupt =>
            return HRTimer_Common_Periph.ISR.FLT.Arr (3);
         when Fault_4_Interrupt =>
            return HRTimer_Common_Periph.ISR.FLT.Arr (4);
         when Fault_5_Interrupt =>
            return HRTimer_Common_Periph.ISR.FLT.Arr (5);
         when System_Fault_Interrupt =>
            return HRTimer_Common_Periph.ISR.SYSFLT;
         when DLL_Ready_Interrupt =>
            return HRTimer_Common_Periph.ISR.DLLRDY;
         when Burst_Mode_Period_Interrupt =>
            return HRTimer_Common_Periph.ISR.BMPER;
      end case;
   end Interrupt_Status;

   -----------------------------
   -- Clear_Pending_Interrupt --
   -----------------------------

   procedure Clear_Pending_Interrupt
     (Source : HRTimer_Common_Interrupt)
   is
   begin
      case Source is
         when Fault_1_Interrupt =>
            HRTimer_Common_Periph.ICR.FLT1C := True;
         when Fault_2_Interrupt =>
            HRTimer_Common_Periph.ICR.FLT2C := True;
         when Fault_3_Interrupt =>
            HRTimer_Common_Periph.ICR.FLT3C := True;
         when Fault_4_Interrupt =>
            HRTimer_Common_Periph.ICR.FLT4C := True;
         when Fault_5_Interrupt =>
            HRTimer_Common_Periph.ICR.FLT5C := True;
         when System_Fault_Interrupt =>
            HRTimer_Common_Periph.ICR.SYSFLTC := True;
         when DLL_Ready_Interrupt =>
            HRTimer_Common_Periph.ICR.DLLRDYC := True;
         when Burst_Mode_Period_Interrupt =>
            HRTimer_Common_Periph.ICR.BMPERC := True;
      end case;
   end Clear_Pending_Interrupt;

   ------------------------
   -- Set_Channel_Output --
   ------------------------

   procedure Set_Channel_Output
     (This   : HRTimer_Channel;
      Output : HRTimer_Channel_Output;
      Enable : Boolean)
   is
   begin
      if This'Address = HRTIM_TIMA_Base then
         case Output is
            when Output_1 =>
               HRTimer_Common_Periph.OENR.TA1OEN := Enable;
            when Output_2 =>
               HRTimer_Common_Periph.OENR.TA2OEN := Enable;
         end case;

      elsif This'Address = HRTIM_TIMB_Base then
         case Output is
            when Output_1 =>
               HRTimer_Common_Periph.OENR.TB1OEN := Enable;
            when Output_2 =>
               HRTimer_Common_Periph.OENR.TB2OEN := Enable;
         end case;

      elsif This'Address = HRTIM_TIMC_Base then
         case Output is
            when Output_1 =>
               HRTimer_Common_Periph.OENR.TC1OEN := Enable;
            when Output_2 =>
               HRTimer_Common_Periph.OENR.TC2OEN := Enable;
         end case;

      elsif This'Address = HRTIM_TIMD_Base then
         case Output is
            when Output_1 =>
               HRTimer_Common_Periph.OENR.TD1OEN := Enable;
            when Output_2 =>
               HRTimer_Common_Periph.OENR.TD2OEN := Enable;
         end case;

      elsif This'Address = HRTIM_TIME_Base then
         case Output is
            when Output_1 =>
               HRTimer_Common_Periph.OENR.TE1OEN := Enable;
            when Output_2 =>
               HRTimer_Common_Periph.OENR.TE2OEN := Enable;
         end case;

      elsif This'Address = HRTIM_TIMF_Base then
         case Output is
            when Output_1 =>
               HRTimer_Common_Periph.OENR.TF1OEN := Enable;
            when Output_2 =>
               HRTimer_Common_Periph.OENR.TF2OEN := Enable;
         end case;
      end if;
   end Set_Channel_Output;

   -------------------------
   -- Set_Channel_Outputs --
   -------------------------

   procedure Set_Channel_Outputs
     (This   : HRTimer_Channel;
      Enable : Boolean)
   is
   begin
      if This'Address = HRTIM_TIMA_Base then
         HRTimer_Common_Periph.OENR.TA1OEN := Enable;
         HRTimer_Common_Periph.OENR.TA2OEN := Enable;

      elsif This'Address = HRTIM_TIMB_Base then
         HRTimer_Common_Periph.OENR.TB1OEN := Enable;
         HRTimer_Common_Periph.OENR.TB2OEN := Enable;

      elsif This'Address = HRTIM_TIMC_Base then
         HRTimer_Common_Periph.OENR.TC1OEN := Enable;
         HRTimer_Common_Periph.OENR.TC2OEN := Enable;

      elsif This'Address = HRTIM_TIMD_Base then
         HRTimer_Common_Periph.OENR.TD1OEN := Enable;
         HRTimer_Common_Periph.OENR.TD2OEN := Enable;

      elsif This'Address = HRTIM_TIME_Base then
         HRTimer_Common_Periph.OENR.TE1OEN := Enable;
         HRTimer_Common_Periph.OENR.TE2OEN := Enable;

      elsif This'Address = HRTIM_TIMF_Base then
         HRTimer_Common_Periph.OENR.TF1OEN := Enable;
         HRTimer_Common_Periph.OENR.TF2OEN := Enable;
      end if;
   end Set_Channel_Outputs;

   -------------------------
   -- Set_Channel_Outputs --
   -------------------------

   procedure Set_Channel_Outputs
     (Channels : HRTimer_List;
      Enable   : Boolean)
   is
   begin
      for Channel of Channels loop
         if Channel = HRTimer_A then
            HRTimer_Common_Periph.OENR.TA1OEN := Enable;
            HRTimer_Common_Periph.OENR.TA2OEN := Enable;

         elsif Channel = HRTimer_B  then
            HRTimer_Common_Periph.OENR.TB1OEN := Enable;
            HRTimer_Common_Periph.OENR.TB2OEN := Enable;

         elsif Channel = HRTimer_C  then
            HRTimer_Common_Periph.OENR.TC1OEN := Enable;
            HRTimer_Common_Periph.OENR.TC2OEN := Enable;

         elsif Channel = HRTimer_D  then
            HRTimer_Common_Periph.OENR.TD1OEN := Enable;
            HRTimer_Common_Periph.OENR.TD2OEN := Enable;

         elsif Channel = HRTimer_E  then
            HRTimer_Common_Periph.OENR.TE1OEN := Enable;
            HRTimer_Common_Periph.OENR.TE2OEN := Enable;

         elsif Channel = HRTimer_F  then
            HRTimer_Common_Periph.OENR.TF1OEN := Enable;
            HRTimer_Common_Periph.OENR.TF2OEN := Enable;
         end if;
      end loop;
   end Set_Channel_Outputs;

   -------------------
   -- Output_Status --
   -------------------

   function Output_Status
     (This : HRTimer_Channel) return Output_Status_List
   is
      Output : Output_Status_List;
   begin
      if This'Address = HRTIM_TIMA_Base then
         if HRTimer_Common_Periph.OENR.TA1OEN then
            Output (1) := Enabled;
         else
            if HRTimer_Common_Periph.ODSR.TA1ODS then
               Output (1) := Fault;
            else
               Output (1) := Idle;
            end if;
         end if;

         if  HRTimer_Common_Periph.OENR.TA2OEN then
            Output (2) := Enabled;
         else
            if HRTimer_Common_Periph.ODSR.TA2ODS then
               Output (2) := Fault;
            else
               Output (2) := Idle;
            end if;
         end if;
      elsif This'Address = HRTIM_TIMB_Base then
         if HRTimer_Common_Periph.OENR.TB1OEN then
            Output (1) := Enabled;
         else
            if HRTimer_Common_Periph.ODSR.TB1ODS then
               Output (1) := Fault;
            else
               Output (1) := Idle;
            end if;
         end if;

         if  HRTimer_Common_Periph.OENR.TB2OEN then
            Output (2) := Enabled;
         else
            if HRTimer_Common_Periph.ODSR.TB2ODS then
               Output (2) := Fault;
            else
               Output (2) := Idle;
            end if;
         end if;
      elsif This'Address = HRTIM_TIMC_Base then
         if HRTimer_Common_Periph.OENR.TC1OEN then
            Output (1) := Enabled;
         else
            if HRTimer_Common_Periph.ODSR.TC1ODS then
               Output (1) := Fault;
            else
               Output (1) := Idle;
            end if;
         end if;

         if  HRTimer_Common_Periph.OENR.TC2OEN then
            Output (2) := Enabled;
         else
            if HRTimer_Common_Periph.ODSR.TC2ODS then
               Output (2) := Fault;
            else
               Output (2) := Idle;
            end if;
         end if;
      elsif This'Address = HRTIM_TIMD_Base then
         if HRTimer_Common_Periph.OENR.TD1OEN then
            Output (1) := Enabled;
         else
            if HRTimer_Common_Periph.ODSR.TD1ODS then
               Output (1) := Fault;
            else
               Output (1) := Idle;
            end if;
         end if;

         if  HRTimer_Common_Periph.OENR.TD2OEN then
            Output (2) := Enabled;
         else
            if HRTimer_Common_Periph.ODSR.TD2ODS then
               Output (2) := Fault;
            else
               Output (2) := Idle;
            end if;
         end if;
      elsif This'Address = HRTIM_TIME_Base then
         if HRTimer_Common_Periph.OENR.TE1OEN then
            Output (1) := Enabled;
         else
            if HRTimer_Common_Periph.ODSR.TE1ODS then
               Output (1) := Fault;
            else
               Output (1) := Idle;
            end if;
         end if;

         if  HRTimer_Common_Periph.OENR.TE2OEN then
            Output (2) := Enabled;
         else
            if HRTimer_Common_Periph.ODSR.TE2ODS then
               Output (2) := Fault;
            else
               Output (2) := Idle;
            end if;
         end if;
      elsif This'Address = HRTIM_TIMF_Base then
         if HRTimer_Common_Periph.OENR.TF1OEN then
            Output (1) := Enabled;
         else
            if HRTimer_Common_Periph.ODSR.TF1ODS then
               Output (1) := Fault;
            else
               Output (1) := Idle;
            end if;
         end if;

         if  HRTimer_Common_Periph.OENR.TF2OEN then
            Output (2) := Enabled;
         else
            if HRTimer_Common_Periph.ODSR.TF2ODS then
               Output (2) := Fault;
            else
               Output (2) := Idle;
            end if;
         end if;
      end if;

      return Output;
   end Output_Status;

   ----------------------------
   -- Channel_Output_Enabled --
   ----------------------------

   function Channel_Output_Enabled
     (This   : HRTimer_Channel;
      Output : HRTimer_Channel_Output) return Boolean
   is
   begin
      if This'Address = HRTIM_TIMA_Base then
         case Output is
            when Output_1 =>
               return HRTimer_Common_Periph.OENR.TA1OEN;
            when Output_2 =>
               return HRTimer_Common_Periph.OENR.TA2OEN;
         end case;
      elsif This'Address = HRTIM_TIMB_Base then
         case Output is
            when Output_1 =>
               return HRTimer_Common_Periph.OENR.TB1OEN;
            when Output_2 =>
               return HRTimer_Common_Periph.OENR.TB2OEN;
         end case;
      elsif This'Address = HRTIM_TIMC_Base then
         case Output is
            when Output_1 =>
               return HRTimer_Common_Periph.OENR.TC1OEN;
            when Output_2 =>
               return HRTimer_Common_Periph.OENR.TC2OEN;
         end case;
      elsif This'Address = HRTIM_TIMD_Base then
         case Output is
            when Output_1 =>
               return HRTimer_Common_Periph.OENR.TD1OEN;
            when Output_2 =>
               return HRTimer_Common_Periph.OENR.TD2OEN;
         end case;
      elsif This'Address = HRTIM_TIME_Base then
         case Output is
            when Output_1 =>
               return HRTimer_Common_Periph.OENR.TE1OEN;
            when Output_2 =>
               return HRTimer_Common_Periph.OENR.TE2OEN;
         end case;
      elsif This'Address = HRTIM_TIMF_Base then
         case Output is
            when Output_1 =>
               return HRTimer_Common_Periph.OENR.TF1OEN;
            when Output_2 =>
               return HRTimer_Common_Periph.OENR.TF2OEN;
         end case;
      else
         return False;
      end if;
   end Channel_Output_Enabled;

   ------------------------
   -- No_Outputs_Enabled --
   ------------------------

   function No_Outputs_Enabled (This : HRTimer_Channel) return Boolean is
   begin
      if This'Address = HRTIM_TIMA_Base then
         if (HRTimer_Common_Periph.OENR.TA1OEN or
           HRTimer_Common_Periph.OENR.TA2OEN)
         then
            return False;
         end if;
      elsif This'Address = HRTIM_TIMB_Base then
         if (HRTimer_Common_Periph.OENR.TB1OEN or
           HRTimer_Common_Periph.OENR.TB2OEN)
         then
            return False;
         end if;
      elsif This'Address = HRTIM_TIMC_Base then
         if (HRTimer_Common_Periph.OENR.TC1OEN or
           HRTimer_Common_Periph.OENR.TC2OEN)
         then
            return False;
         end if;
      elsif This'Address = HRTIM_TIMD_Base then
         if (HRTimer_Common_Periph.OENR.TD1OEN or
           HRTimer_Common_Periph.OENR.TD2OEN)
         then
            return False;
         end if;
      elsif This'Address = HRTIM_TIME_Base then
         if (HRTimer_Common_Periph.OENR.TE1OEN or
           HRTimer_Common_Periph.OENR.TE2OEN)
         then
            return False;
         end if;
      elsif This'Address = HRTIM_TIMF_Base then
         if (HRTimer_Common_Periph.OENR.TF1OEN or
           HRTimer_Common_Periph.OENR.TF2OEN)
         then
            return False;
         end if;
      end if;
      return True;
   end No_Outputs_Enabled;

   -----------------------
   -- Enable_Burst_Mode --
   -----------------------

   procedure Enable_Burst_Mode is
   begin
      HRTimer_Common_Periph.BMCR.BME := True;
   end Enable_Burst_Mode;

   ------------------------
   -- Disable_Burst_Mode --
   ------------------------

   procedure Disable_Burst_Mode is
   begin
      HRTimer_Common_Periph.BMCR.BME := False;
   end Disable_Burst_Mode;

   ------------------------
   -- Burst_Mode_Enabled --
   ------------------------

   function Burst_Mode_Enabled return Boolean is
   begin
      return HRTimer_Common_Periph.BMCR.BME;
   end Burst_Mode_Enabled;

   -----------------------
   -- Burst_Mode_Status --
   -----------------------

   function Burst_Mode_Status return Boolean is
   begin
      return HRTimer_Common_Periph.BMCR.BMSTAT;
   end Burst_Mode_Status;

   --------------------------
   -- Configure_Burst_Mode --
   --------------------------

   procedure Configure_Burst_Mode
     (Operating_Mode : Burst_Mode_Operating_Mode;
      Clock_Source   : Burst_Mode_Clock_Source;
      Prescaler      : Burst_Mode_Prescaler;
      Preload_Enable : Boolean)
   is
   begin
      HRTimer_Common_Periph.BMCR.BMOM := Operating_Mode = Continuous;
      HRTimer_Common_Periph.BMCR.BMCLK := Clock_Source'Enum_Rep;
      HRTimer_Common_Periph.BMCR.BMPRSC := Prescaler'Enum_Rep;
      HRTimer_Common_Periph.BMCR.BMPREN := Preload_Enable;
   end Configure_Burst_Mode;

   ----------------------------------
   -- Configure_HRTimer_Burst_Mode --
   ----------------------------------

   procedure Configure_HRTimer_Burst_Mode
     (Counter : HRTimer;
      Mode    : Burst_Mode_HRTimer_Mode)
   is
   begin
      case Counter is
         when HRTimer_M =>
            HRTimer_Common_Periph.BMCR.MTBM := Mode = Stopped;
         when HRTimer_A =>
            HRTimer_Common_Periph.BMCR.TABM := Mode = Stopped;
         when HRTimer_B =>
            HRTimer_Common_Periph.BMCR.TBBM := Mode = Stopped;
         when HRTimer_C =>
            HRTimer_Common_Periph.BMCR.TCBM := Mode = Stopped;
         when HRTimer_D =>
            HRTimer_Common_Periph.BMCR.TDBM := Mode = Stopped;
         when HRTimer_E =>
            HRTimer_Common_Periph.BMCR.TEBM := Mode = Stopped;
         when HRTimer_F =>
            HRTimer_Common_Periph.BMCR.TFBM := Mode = Stopped;
      end case;
   end Configure_HRTimer_Burst_Mode;

   ----------------------------------
   -- Configure_Burst_Mode_Trigger --
   ----------------------------------

   procedure Configure_Burst_Mode_Trigger
     (Trigger : Burst_Mode_Trigger_Event;
      Enabled : Boolean)
   is
   begin
      if Enabled then
         HRTimer_Common_Periph.BMTRGR :=
           HRTimer_Common_Periph.BMTRGR or (2 ** Trigger'Enum_Rep);
      else
         HRTimer_Common_Periph.BMTRGR :=
           HRTimer_Common_Periph.BMTRGR and not (2 ** Trigger'Enum_Rep);
      end if;
   end Configure_Burst_Mode_Trigger;

   ------------------------------
   --  Set_Burst_Mode_Compare  --
   ------------------------------

   procedure Set_Burst_Mode_Compare (Value : UInt16) is
   begin
      HRTimer_Common_Periph.BMCMPR.BMCMP := Value;
   end  Set_Burst_Mode_Compare;

   -----------------------------
   --  Set_Burst_Mode_Period  --
   -----------------------------

   procedure Set_Burst_Mode_Period (Value : UInt16) is
   begin
      HRTimer_Common_Periph.BMPER.BMPER := Value;
   end  Set_Burst_Mode_Period;

   ------------------------------
   -- Configure_External_Event --
   ------------------------------

   procedure Configure_External_Event
     (Number      : External_Event_Number;
      Source      : External_Event_Source;
      Polarity    : External_Event_Polarity;
      Sensitivity : External_Event_Sensitivity;
      Fast_Mode   : External_Event_Fast_Mode;
      Filter      : External_Event_Frequency_Filter)
   is
   begin
      case Number is
         when Event_1 =>
            HRTimer_Common_Periph.EECR1.EE1SRC := Source'Enum_Rep;
            HRTimer_Common_Periph.EECR1.EE1POL := Polarity = Active_Low;
            HRTimer_Common_Periph.EECR1.EE1SNS := Sensitivity'Enum_Rep;
            HRTimer_Common_Periph.EECR1.EE1FAST := Fast_Mode = Low_Latency;
         when Event_2 =>
            HRTimer_Common_Periph.EECR1.EE2SRC := Source'Enum_Rep;
            HRTimer_Common_Periph.EECR1.EE2POL := Polarity = Active_Low;
            HRTimer_Common_Periph.EECR1.EE2SNS := Sensitivity'Enum_Rep;
            HRTimer_Common_Periph.EECR1.EE2FAST := Fast_Mode = Low_Latency;
         when Event_3 =>
            HRTimer_Common_Periph.EECR1.EE3SRC := Source'Enum_Rep;
            HRTimer_Common_Periph.EECR1.EE3POL := Polarity = Active_Low;
            HRTimer_Common_Periph.EECR1.EE3SNS := Sensitivity'Enum_Rep;
            HRTimer_Common_Periph.EECR1.EE3FAST := Fast_Mode = Low_Latency;
         when Event_4 =>
            HRTimer_Common_Periph.EECR1.EE4SRC := Source'Enum_Rep;
            HRTimer_Common_Periph.EECR1.EE4POL := Polarity = Active_Low;
            HRTimer_Common_Periph.EECR1.EE4SNS := Sensitivity'Enum_Rep;
            HRTimer_Common_Periph.EECR1.EE4FAST := Fast_Mode = Low_Latency;
         when Event_5 =>
            HRTimer_Common_Periph.EECR1.EE5SRC := Source'Enum_Rep;
            HRTimer_Common_Periph.EECR1.EE5POL := Polarity = Active_Low;
            HRTimer_Common_Periph.EECR1.EE5SNS := Sensitivity'Enum_Rep;
            HRTimer_Common_Periph.EECR1.EE5FAST := Fast_Mode = Low_Latency;
         when Event_6 =>
            HRTimer_Common_Periph.EECR2.EE6SRC := Source'Enum_Rep;
            HRTimer_Common_Periph.EECR2.EE6POL := Polarity = Active_Low;
            HRTimer_Common_Periph.EECR2.EE6SNS := Sensitivity'Enum_Rep;
            HRTimer_Common_Periph.EECR3.EE6F := Filter'Enum_Rep;
         when Event_7 =>
            HRTimer_Common_Periph.EECR2.EE7SRC := Source'Enum_Rep;
            HRTimer_Common_Periph.EECR2.EE7POL := Polarity = Active_Low;
            HRTimer_Common_Periph.EECR2.EE7SNS := Sensitivity'Enum_Rep;
            HRTimer_Common_Periph.EECR3.EE7F := Filter'Enum_Rep;
         when Event_8 =>
            HRTimer_Common_Periph.EECR2.EE8SRC := Source'Enum_Rep;
            HRTimer_Common_Periph.EECR2.EE8POL := Polarity = Active_Low;
            HRTimer_Common_Periph.EECR2.EE8SNS := Sensitivity'Enum_Rep;
            HRTimer_Common_Periph.EECR3.EE8F := Filter'Enum_Rep;
         when Event_9 =>
            HRTimer_Common_Periph.EECR2.EE9SRC := Source'Enum_Rep;
            HRTimer_Common_Periph.EECR2.EE9POL := Polarity = Active_Low;
            HRTimer_Common_Periph.EECR2.EE9SNS := Sensitivity'Enum_Rep;
            HRTimer_Common_Periph.EECR3.EE9F := Filter'Enum_Rep;
         when Event_10 =>
            HRTimer_Common_Periph.EECR2.EE10SRC := Source'Enum_Rep;
            HRTimer_Common_Periph.EECR2.EE10POL := Polarity = Active_Low;
            HRTimer_Common_Periph.EECR2.EE10SNS := Sensitivity'Enum_Rep;
            HRTimer_Common_Periph.EECR3.EE10F := Filter'Enum_Rep;
      end case;
   end Configure_External_Event;

   ------------------------------
   -- Set_External_Event_Clock --
   ------------------------------

   procedure Set_External_Event_Clock
     (Clock : External_Event_Sampling_Clock)
   is
   begin
      HRTimer_Common_Periph.EECR3.EEVSD := Clock'Enum_Rep;
   end Set_External_Event_Clock;

   ---------------------------
   -- Configure_ADC_Trigger --
   ---------------------------

   procedure Configure_ADC_Trigger
     (Output  : ADC_Trigger_Output;
      Source  : ADC_Trigger_Source;
      Divisor : PostScaler_Divisor)
   is
   begin
      case Output is
         when ADC_Trigger_1 =>
            HRTimer_Common_Periph.ADC1R := 2 ** Source'Enum_Rep;
            HRTimer_Common_Periph.ADCPS1.ADC1PSC := Divisor'Enum_Rep;
         when ADC_Trigger_2 =>
            HRTimer_Common_Periph.ADC2R := 2 ** Source'Enum_Rep;
            HRTimer_Common_Periph.ADCPS1.ADC2PSC := Divisor'Enum_Rep;
         when ADC_Trigger_3 =>
            HRTimer_Common_Periph.ADC3R := 2 ** Source'Enum_Rep;
            HRTimer_Common_Periph.ADCPS1.ADC3PSC := Divisor'Enum_Rep;
         when ADC_Trigger_4 =>
            HRTimer_Common_Periph.ADC4R := 2 ** Source'Enum_Rep;
            HRTimer_Common_Periph.ADCPS1.ADC4PSC := Divisor'Enum_Rep;
         when ADC_Trigger_5 =>
            HRTimer_Common_Periph.ADCER.ADC5TRG := Source'Enum_Rep;
            HRTimer_Common_Periph.ADCPS1.ADC5PSC := Divisor'Enum_Rep;
         when ADC_Trigger_6 =>
            HRTimer_Common_Periph.ADCER.ADC6TRG := Source'Enum_Rep;
            HRTimer_Common_Periph.ADCPS2.ADC6PSC := Divisor'Enum_Rep;
         when ADC_Trigger_7 =>
            HRTimer_Common_Periph.ADCER.ADC7TRG := Source'Enum_Rep;
            HRTimer_Common_Periph.ADCPS2.ADC7PSC := Divisor'Enum_Rep;
         when ADC_Trigger_8 =>
            HRTimer_Common_Periph.ADCER.ADC8TRG := Source'Enum_Rep;
            HRTimer_Common_Periph.ADCPS2.ADC8PSC := Divisor'Enum_Rep;
         when ADC_Trigger_9 =>
            HRTimer_Common_Periph.ADCER.ADC9TRG := Source'Enum_Rep;
            HRTimer_Common_Periph.ADCPS2.ADC9PSC := Divisor'Enum_Rep;
         when ADC_Trigger_10 =>
            HRTimer_Common_Periph.ADCER.ADC10TRG := Source'Enum_Rep;
            HRTimer_Common_Periph.ADCPS2.ADC10PSC := Divisor'Enum_Rep;
      end case;
   end Configure_ADC_Trigger;

   ---------------------------
   -- Configure_ADC_Trigger --
   ---------------------------

   procedure Configure_ADC_Trigger
     (Output  : ADC_Trigger_Output;
      Source  : ADC_Trigger_Source;
      Enabled : Boolean)
   is
   begin
      case Output is
         when ADC_Trigger_1 =>
            if Enabled then
               HRTimer_Common_Periph.ADC1R :=
                 HRTimer_Common_Periph.ADC1R or 2 ** Source'Enum_Rep;
            else
               HRTimer_Common_Periph.ADC1R :=
                 HRTimer_Common_Periph.ADC1R and not (2 ** Source'Enum_Rep);
            end if;
         when ADC_Trigger_2 =>
            if Enabled then
               HRTimer_Common_Periph.ADC2R :=
                 HRTimer_Common_Periph.ADC2R or 2 ** Source'Enum_Rep;
            else
               HRTimer_Common_Periph.ADC2R :=
                 HRTimer_Common_Periph.ADC2R and not (2 ** Source'Enum_Rep);
            end if;
         when ADC_Trigger_3 =>
            if Enabled then
               HRTimer_Common_Periph.ADC3R :=
                 HRTimer_Common_Periph.ADC3R or 2 ** Source'Enum_Rep;
            else
               HRTimer_Common_Periph.ADC3R :=
                 HRTimer_Common_Periph.ADC3R and not (2 ** Source'Enum_Rep);
            end if;
         when ADC_Trigger_4 =>
            if Enabled then
               HRTimer_Common_Periph.ADC4R :=
                 HRTimer_Common_Periph.ADC4R or 2 ** Source'Enum_Rep;
            else
               HRTimer_Common_Periph.ADC4R :=
                 HRTimer_Common_Periph.ADC4R and not (2 ** Source'Enum_Rep);
            end if;
         when ADC_Trigger_5 =>
            if Enabled then
               HRTimer_Common_Periph.ADCER.ADC5TRG :=
                 HRTimer_Common_Periph.ADCER.ADC5TRG or 2 ** Source'Enum_Rep;
            else
               HRTimer_Common_Periph.ADCER.ADC5TRG :=
                 HRTimer_Common_Periph.ADCER.ADC5TRG and not (2 ** Source'Enum_Rep);
            end if;
         when ADC_Trigger_6 =>
            if Enabled then
               HRTimer_Common_Periph.ADCER.ADC6TRG :=
                 HRTimer_Common_Periph.ADCER.ADC6TRG or 2 ** Source'Enum_Rep;
            else
               HRTimer_Common_Periph.ADCER.ADC6TRG :=
                 HRTimer_Common_Periph.ADCER.ADC6TRG and not (2 ** Source'Enum_Rep);
            end if;
         when ADC_Trigger_7 =>
            if Enabled then
               HRTimer_Common_Periph.ADCER.ADC7TRG :=
                 HRTimer_Common_Periph.ADCER.ADC7TRG or 2 ** Source'Enum_Rep;
            else
               HRTimer_Common_Periph.ADCER.ADC7TRG :=
                 HRTimer_Common_Periph.ADCER.ADC7TRG and not (2 ** Source'Enum_Rep);
            end if;
         when ADC_Trigger_8 =>
            if Enabled then
               HRTimer_Common_Periph.ADCER.ADC8TRG :=
                 HRTimer_Common_Periph.ADCER.ADC8TRG or 2 ** Source'Enum_Rep;
            else
               HRTimer_Common_Periph.ADCER.ADC8TRG :=
                 HRTimer_Common_Periph.ADCER.ADC8TRG and not (2 ** Source'Enum_Rep);
            end if;
         when ADC_Trigger_9 =>
            if Enabled then
               HRTimer_Common_Periph.ADCER.ADC9TRG :=
                 HRTimer_Common_Periph.ADCER.ADC9TRG or 2 ** Source'Enum_Rep;
            else
               HRTimer_Common_Periph.ADCER.ADC9TRG :=
                 HRTimer_Common_Periph.ADCER.ADC9TRG and not (2 ** Source'Enum_Rep);
            end if;
         when ADC_Trigger_10 =>
            if Enabled then
               HRTimer_Common_Periph.ADCER.ADC10TRG :=
                 HRTimer_Common_Periph.ADCER.ADC10TRG or 2 ** Source'Enum_Rep;
            else
               HRTimer_Common_Periph.ADCER.ADC10TRG :=
                 HRTimer_Common_Periph.ADCER.ADC10TRG and not (2 ** Source'Enum_Rep);
            end if;
      end case;
   end Configure_ADC_Trigger;

   ----------------------------------
   -- Configure_ADC_Trigger_Update --
   ----------------------------------

   procedure Configure_ADC_Trigger_Update
     (Output : ADC_Trigger_Output;
      Source : ADC_Trigger_Update_Source)
   is
   begin
      case Output is
         when ADC_Trigger_1 =>
            HRTimer_Common_Periph.CR1.AD1USRC := Source'Enum_Rep;
         when ADC_Trigger_2 =>
            HRTimer_Common_Periph.CR1.AD2USRC := Source'Enum_Rep;
         when ADC_Trigger_3 =>
            HRTimer_Common_Periph.CR1.AD3USRC := Source'Enum_Rep;
         when ADC_Trigger_4 =>
            HRTimer_Common_Periph.CR1.AD4USRC := Source'Enum_Rep;
         when ADC_Trigger_5 =>
            HRTimer_Common_Periph.ADCUR.AD5USRC := Source'Enum_Rep;
         when ADC_Trigger_6 =>
            HRTimer_Common_Periph.ADCUR.AD6USRC := Source'Enum_Rep;
         when ADC_Trigger_7 =>
            HRTimer_Common_Periph.ADCUR.AD7USRC := Source'Enum_Rep;
         when ADC_Trigger_8 =>
            HRTimer_Common_Periph.ADCUR.AD8USRC := Source'Enum_Rep;
         when ADC_Trigger_9 =>
            HRTimer_Common_Periph.ADCUR.AD9USRC := Source'Enum_Rep;
         when ADC_Trigger_10 =>
            HRTimer_Common_Periph.ADCUR.AD10USRC := Source'Enum_Rep;
      end case;
   end Configure_ADC_Trigger_Update;

   -------------------------------
   -- Configure_DLL_Calibration --
   -------------------------------

   procedure Configure_DLL_Calibration
     (Calibration_Start    : Boolean;
      Periodic_Calibration : Boolean;
      Calibration_Rate     : DLL_Calibration) is
   begin
      if Calibration_Start then
         HRTimer_Common_Periph.DLLCR.CALEN := False;
         HRTimer_Common_Periph.DLLCR.CAL := True;

         --  Wait for DLL calibration end
         while not HRTimer_Common_Periph.ISR.DLLRDY loop
            null;
         end loop;

         --  Clear DLL ready flag
         HRTimer_Common_Periph.ISR.DLLRDY := True;
      end if;

      HRTimer_Common_Periph.DLLCR.CALEN := Periodic_Calibration;
      HRTimer_Common_Periph.DLLCR.CALRTE := Calibration_Rate'Enum_Rep;
   end Configure_DLL_Calibration;

   ---------------------
   -- Set_Fault_Input --
   ---------------------

   procedure Set_Fault_Input
     (Input  : HRTimer_Fault_Source;
      Enable : Boolean)
   is
   begin
      case Input is
         when Fault_1 =>
            HRTimer_Common_Periph.FLTINR1.FLT1E := Enable;
         when Fault_2 =>
            HRTimer_Common_Periph.FLTINR1.FLT2E := Enable;
         when Fault_3 =>
            HRTimer_Common_Periph.FLTINR1.FLT3E := Enable;
         when Fault_4 =>
            HRTimer_Common_Periph.FLTINR1.FLT4E := Enable;
         when Fault_5 =>
            HRTimer_Common_Periph.FLTINR2.FLT5E := Enable;
         when Fault_6 =>
            HRTimer_Common_Periph.FLTINR2.FLT6E := Enable;
      end case;
   end Set_Fault_Input;

   -------------------------
   -- Enabled_Fault_Input --
   -------------------------

   function Enabled_Fault_Input
     (Input : HRTimer_Fault_Source) return Boolean
   is
   begin
      case Input is
         when Fault_1 =>
            return HRTimer_Common_Periph.FLTINR1.FLT1E;
         when Fault_2 =>
            return HRTimer_Common_Periph.FLTINR1.FLT2E;
         when Fault_3 =>
            return HRTimer_Common_Periph.FLTINR1.FLT3E;
         when Fault_4 =>
            return HRTimer_Common_Periph.FLTINR1.FLT4E;
         when Fault_5 =>
            return HRTimer_Common_Periph.FLTINR2.FLT5E;
         when Fault_6 =>
            return HRTimer_Common_Periph.FLTINR2.FLT6E;
      end case;
   end Enabled_Fault_Input;

   ---------------------------
   -- Configure_Fault_Input --
   ---------------------------

   procedure Configure_Fault_Input
     (Input    : HRTimer_Fault_Source;
      Enable   : Boolean;
      Polarity : Fault_Input_Polarity;
      Source   : Fault_Input_Source;
      Filter   : Fault_Input_Filter)
   is
   begin
      case Input is
         when Fault_1 =>
            HRTimer_Common_Periph.FLTINR1.FLT1E := Enable;
            HRTimer_Common_Periph.FLTINR1.FLT1P := Polarity = Active_High;
            HRTimer_Common_Periph.FLTINR1.FLT1F := Filter'Enum_Rep;

            HRTimer_Common_Periph.FLTINR1.FLT1SRC := Source = COMPx_Output;
            HRTimer_Common_Periph.FLTINR2.FLT1SRC_1 :=
              (if Source'Enum_Rep > 1 then True else False);
         when Fault_2 =>
            HRTimer_Common_Periph.FLTINR1.FLT2E := Enable;
            HRTimer_Common_Periph.FLTINR1.FLT2P := Polarity = Active_High;
            HRTimer_Common_Periph.FLTINR1.FLT2F := Filter'Enum_Rep;

            HRTimer_Common_Periph.FLTINR1.FLT2SRC := Source = COMPx_Output;
            HRTimer_Common_Periph.FLTINR2.FLT2SRC_1 :=
              (if Source'Enum_Rep > 1 then True else False);
         when Fault_3 =>
            HRTimer_Common_Periph.FLTINR1.FLT3E := Enable;
            HRTimer_Common_Periph.FLTINR1.FLT3P := Polarity = Active_High;
            HRTimer_Common_Periph.FLTINR1.FLT3F := Filter'Enum_Rep;

            HRTimer_Common_Periph.FLTINR1.FLT3SRC := Source = COMPx_Output;
            HRTimer_Common_Periph.FLTINR2.FLT3SRC_1 :=
              (if Source'Enum_Rep > 1 then True else False);
         when Fault_4 =>
            HRTimer_Common_Periph.FLTINR1.FLT4E := Enable;
            HRTimer_Common_Periph.FLTINR1.FLT4P := Polarity = Active_High;
            HRTimer_Common_Periph.FLTINR1.FLT4F := Filter'Enum_Rep;

            HRTimer_Common_Periph.FLTINR1.FLT4SRC := Source = COMPx_Output;
            HRTimer_Common_Periph.FLTINR2.FLT4SRC_1 :=
              (if Source'Enum_Rep > 1 then True else False);
         when Fault_5 =>
            HRTimer_Common_Periph.FLTINR2.FLT5E := Enable;
            HRTimer_Common_Periph.FLTINR2.FLT5P := Polarity = Active_High;
            HRTimer_Common_Periph.FLTINR2.FLT5F := Filter'Enum_Rep;

            HRTimer_Common_Periph.FLTINR2.FLT5SRC := Source = COMPx_Output;
            HRTimer_Common_Periph.FLTINR2.FLT5SRC_1 :=
              (if Source'Enum_Rep > 1 then True else False);
         when Fault_6 =>
            HRTimer_Common_Periph.FLTINR2.FLT6E := Enable;
            HRTimer_Common_Periph.FLTINR2.FLT6P := Polarity = Active_High;
            HRTimer_Common_Periph.FLTINR2.FLT6F := Filter'Enum_Rep;

            HRTimer_Common_Periph.FLTINR2.FLT6SRC_0 := Source = COMPx_Output;
            HRTimer_Common_Periph.FLTINR2.FLT6SRC_1 :=
              (if Source'Enum_Rep > 1 then True else False);
      end case;
   end Configure_Fault_Input;

   ---------------------------------
   -- Configure_Fault_Input_Clock --
   ---------------------------------

   procedure Configure_Fault_Input_Clock (Clock : Fault_Input_Sampling_Clock) is
   begin
      HRTimer_Common_Periph.FLTINR2.FLTSD := Clock'Enum_Rep;
   end Configure_Fault_Input_Clock;

   -----------------------------
   -- Enable_Fault_Input_Lock --
   -----------------------------

   procedure Enable_Fault_Input_Lock (Input : HRTimer_Fault_Source) is
   begin
      case Input is
         when Fault_1 =>
            HRTimer_Common_Periph.FLTINR1.FLT1LCK := True;
         when Fault_2 =>
            HRTimer_Common_Periph.FLTINR1.FLT2LCK := True;
         when Fault_3 =>
            HRTimer_Common_Periph.FLTINR1.FLT3LCK := True;
         when Fault_4 =>
            HRTimer_Common_Periph.FLTINR1.FLT4LCK := True;
         when Fault_5 =>
            HRTimer_Common_Periph.FLTINR2.FLT5LCK := True;
         when Fault_6 =>
            HRTimer_Common_Periph.FLTINR2.FLT6LCK := True;
      end case;
   end Enable_Fault_Input_Lock;

   ------------------------------
   -- Enabled_Fault_Input_Lock --
   ------------------------------

   function Enabled_Fault_Input_Lock
     (Input : HRTimer_Fault_Source) return Boolean
   is
   begin
      case Input is
         when Fault_1 =>
            return HRTimer_Common_Periph.FLTINR1.FLT1LCK;
         when Fault_2 =>
            return HRTimer_Common_Periph.FLTINR1.FLT2LCK;
         when Fault_3 =>
            return HRTimer_Common_Periph.FLTINR1.FLT3LCK;
         when Fault_4 =>
            return HRTimer_Common_Periph.FLTINR1.FLT4LCK;
         when Fault_5 =>
            return HRTimer_Common_Periph.FLTINR2.FLT5LCK;
         when Fault_6 =>
            return HRTimer_Common_Periph.FLTINR2.FLT6LCK;
      end case;
   end Enabled_Fault_Input_Lock;

   ------------------------------
   -- Set_Fault_Input_Blanking --
   ------------------------------

   procedure Set_Fault_Input_Blanking
     (Input  : HRTimer_Fault_Source;
      Enable : Boolean;
      Source : Fault_Input_Blanking_Source)
   is
   begin
      case Input is
         when Fault_1 =>
            HRTimer_Common_Periph.FLTINR3.FLT1BLKE := Enable;
            HRTimer_Common_Periph.FLTINR3.FLT1BLKS := Source = Moving_Window;
         when Fault_2 =>
            HRTimer_Common_Periph.FLTINR3.FLT2BLKE := Enable;
            HRTimer_Common_Periph.FLTINR3.FLT2BLKS := Source = Moving_Window;
         when Fault_3 =>
            HRTimer_Common_Periph.FLTINR3.FLT3BLKE := Enable;
            HRTimer_Common_Periph.FLTINR3.FLT3BLKS := Source = Moving_Window;
         when Fault_4 =>
            HRTimer_Common_Periph.FLTINR3.FLT4BLKE := Enable;
            HRTimer_Common_Periph.FLTINR3.FLT4BLKS := Source = Moving_Window;
         when Fault_5 =>
            HRTimer_Common_Periph.FLTINR4.FLT5BLKE := Enable;
            HRTimer_Common_Periph.FLTINR4.FLT5BLKS := Source = Moving_Window;
         when Fault_6 =>
            HRTimer_Common_Periph.FLTINR4.FLT6BLKE := Enable;
            HRTimer_Common_Periph.FLTINR4.FLT6BLKS := Source = Moving_Window;
      end case;
   end Set_Fault_Input_Blanking;

   -----------------------------
   -- Set_Fault_Input_Counter --
   -----------------------------

   procedure Set_Fault_Input_Counter
     (Input : HRTimer_Fault_Source;
      Count : UInt4)
   is
   begin
      case Input is
         when Fault_1 =>
            HRTimer_Common_Periph.FLTINR3.FLT1CNT := Count;
         when Fault_2 =>
            HRTimer_Common_Periph.FLTINR3.FLT2CNT := Count;
         when Fault_3 =>
            HRTimer_Common_Periph.FLTINR3.FLT3CNT := Count;
         when Fault_4 =>
            HRTimer_Common_Periph.FLTINR3.FLT4CNT := Count;
         when Fault_5 =>
            HRTimer_Common_Periph.FLTINR4.FLT5CNT := Count;
         when Fault_6 =>
            HRTimer_Common_Periph.FLTINR4.FLT6CNT := Count;
      end case;
   end Set_Fault_Input_Counter;

   -------------------------------
   -- Reset_Fault_Input_Counter --
   -------------------------------

   procedure Reset_Fault_Input_Counter (Input : HRTimer_Fault_Source) is
   begin
      case Input is
         when Fault_1 =>
            HRTimer_Common_Periph.FLTINR3.FLT1CRES := True;
         when Fault_2 =>
            HRTimer_Common_Periph.FLTINR3.FLT2CRES := True;
         when Fault_3 =>
            HRTimer_Common_Periph.FLTINR3.FLT3CRES := True;
         when Fault_4 =>
            HRTimer_Common_Periph.FLTINR3.FLT4CRES := True;
         when Fault_5 =>
            HRTimer_Common_Periph.FLTINR4.FLT5CRES := True;
         when Fault_6 =>
            HRTimer_Common_Periph.FLTINR4.FLT6CRES := True;
      end case;
   end Reset_Fault_Input_Counter;

   ----------------------------------------
   -- Set_Fault_Input_Counter_Reset_Mode --
   ----------------------------------------

   procedure Set_Fault_Input_Counter_Reset_Mode
     (Input : HRTimer_Fault_Source;
      Mode  : Fault_Input_Counter_Reset_Mode)
   is
   begin
      case Input is
         when Fault_1 =>
            HRTimer_Common_Periph.FLTINR3.FLT1RSTM :=
              Mode = Reset_RollOver_Event_No_Fault;
         when Fault_2 =>
            HRTimer_Common_Periph.FLTINR3.FLT2RSTM :=
              Mode = Reset_RollOver_Event_No_Fault;
         when Fault_3 =>
            HRTimer_Common_Periph.FLTINR3.FLT3RSTM :=
              Mode = Reset_RollOver_Event_No_Fault;
         when Fault_4 =>
            HRTimer_Common_Periph.FLTINR3.FLT4RSTM :=
              Mode = Reset_RollOver_Event_No_Fault;
         when Fault_5 =>
            HRTimer_Common_Periph.FLTINR4.FLT5RSTM :=
              Mode = Reset_RollOver_Event_No_Fault;
         when Fault_6 =>
            HRTimer_Common_Periph.FLTINR4.FLT6RSTM :=
              Mode = Reset_RollOver_Event_No_Fault;
      end case;
   end Set_Fault_Input_Counter_Reset_Mode;

   --------------------------------
   -- Set_Burst_DMA_Timer_Update --
   --------------------------------

   procedure Set_Burst_DMA_Timer_Update
     (Counter   : HRTimer_Master;
      Registers : Burst_DMA_Master_Update_List;
      Enable    : Boolean)
   is
      pragma Unreferenced (Counter);
   begin
      if Enable then
         for Register of Registers loop
            HRTimer_Common_Periph.BDMUPR :=
              HRTimer_Common_Periph.BDMUPR or (2 ** Register'Enum_Rep);
         end loop;
      else
         for Register of Registers loop
            HRTimer_Common_Periph.BDMUPR :=
              HRTimer_Common_Periph.BDMUPR and not (2 ** Register'Enum_Rep);
         end loop;
      end if;
   end Set_Burst_DMA_Timer_Update;

   --------------------------------
   -- Set_Burst_DMA_Timer_Update --
   --------------------------------

   procedure Set_Burst_DMA_Timer_Update
     (Counter   : HRTimer_Channel;
      Registers : Burst_DMA_Timer_Channel_Update_List;
      Enable    : Boolean)
   is
   begin
      if Counter'Address = HRTIM_TIMA_Base then
         if Enable then
            for Register of Registers loop
               HRTimer_Common_Periph.BDTAUPR :=
                 HRTimer_Common_Periph.BDTAUPR or (2 ** Register'Enum_Rep);
            end loop;
         else
            for Register of Registers loop
               HRTimer_Common_Periph.BDTAUPR :=
                 HRTimer_Common_Periph.BDTAUPR and not (2 ** Register'Enum_Rep);
            end loop;
         end if;

      elsif Counter'Address = HRTIM_TIMB_Base then
         if Enable then
            for Register of Registers loop
               HRTimer_Common_Periph.BDTBUPR :=
                 HRTimer_Common_Periph.BDTBUPR or (2 ** Register'Enum_Rep);
            end loop;
         else
            for Register of Registers loop
               HRTimer_Common_Periph.BDTBUPR :=
                 HRTimer_Common_Periph.BDTBUPR and not (2 ** Register'Enum_Rep);
            end loop;
         end if;

      elsif Counter'Address = HRTIM_TIMC_Base then
         if Enable then
            for Register of Registers loop
               HRTimer_Common_Periph.BDTCUPR :=
                 HRTimer_Common_Periph.BDTCUPR or (2 ** Register'Enum_Rep);
            end loop;
         else
            for Register of Registers loop
               HRTimer_Common_Periph.BDTCUPR :=
                 HRTimer_Common_Periph.BDTCUPR and not (2 ** Register'Enum_Rep);
            end loop;
         end if;

      elsif Counter'Address = HRTIM_TIMD_Base then
         if Enable then
            for Register of Registers loop
               HRTimer_Common_Periph.BDTDUPR :=
                 HRTimer_Common_Periph.BDTDUPR or (2 ** Register'Enum_Rep);
            end loop;
         else
            for Register of Registers loop
               HRTimer_Common_Periph.BDTDUPR :=
                 HRTimer_Common_Periph.BDTDUPR and not (2 ** Register'Enum_Rep);
            end loop;
         end if;

      elsif Counter'Address = HRTIM_TIME_Base then
         if Enable then
            for Register of Registers loop
               HRTimer_Common_Periph.BDTEUPR :=
                 HRTimer_Common_Periph.BDTEUPR or (2 ** Register'Enum_Rep);
            end loop;
         else
            for Register of Registers loop
               HRTimer_Common_Periph.BDTEUPR :=
                 HRTimer_Common_Periph.BDTEUPR and not (2 ** Register'Enum_Rep);
            end loop;
         end if;

      elsif Counter'Address = HRTIM_TIMF_Base then
         if Enable then
            for Register of Registers loop
               HRTimer_Common_Periph.BDTFUPR :=
                 HRTimer_Common_Periph.BDTFUPR or (2 ** Register'Enum_Rep);
            end loop;
         else
            for Register of Registers loop
               HRTimer_Common_Periph.BDTFUPR :=
                 HRTimer_Common_Periph.BDTFUPR and not (2 ** Register'Enum_Rep);
            end loop;
         end if;
      end if;
   end Set_Burst_DMA_Timer_Update;

   ------------------------
   -- Set_Burst_DMA_Data --
   ------------------------

   procedure Set_Burst_DMA_Data (Data : UInt32) is
   begin
      HRTimer_Common_Periph.BDMADR := Data;
   end Set_Burst_DMA_Data;

end STM32.HRTimers;
