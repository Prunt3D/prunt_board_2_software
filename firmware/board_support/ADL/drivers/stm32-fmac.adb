package body STM32.FMAC is

   ----------------
   -- Reset_FMAC --
   ----------------

   procedure Reset_FMAC (This : in out FMAC_Accelerator) is
   begin
      This.CR.RESET := True;
   end Reset_FMAC;

   ------------------------
   -- Set_Buffer_Address --
   ------------------------

   procedure Set_Buffer_Address
     (This         : in out FMAC_Accelerator;
      Buffer       : FMAC_Buffer;
      Base_Address : UInt8)
   is
   begin
      case Buffer is
         when X1 =>
            This.X1BUFCFG.X1_BASE := Base_Address;
         when X2 =>
            This.X2BUFCFG.X2_BASE := Base_Address;
         when Y =>
            This.YBUFCFG.Y_BASE := Base_Address;
      end case;
   end Set_Buffer_Address;

   ---------------------
   -- Set_Buffer_Size --
   ---------------------

   procedure Set_Buffer_Size
     (This         : in out FMAC_Accelerator;
      Buffer       : FMAC_Buffer;
      Size         : UInt8)
   is
   begin
      case Buffer is
         when X1 =>
            This.X1BUFCFG.X1_BUF_SIZE := Size;
         when X2 =>
            This.X2BUFCFG.X2_BUF_SIZE := Size;
         when Y =>
            This.YBUFCFG.Y_BUF_SIZE := Size;
      end case;
   end Set_Buffer_Size;

   --------------------------
   -- Set_Buffer_Watermark --
   --------------------------

   procedure Set_Buffer_Watermark
     (This         : in out FMAC_Accelerator;
      Buffer       : FMAC_Buffer;
      Watermark    : FMAC_Watermark := Threshold_1)
   is
   begin
      case Buffer is
         when X1 =>
            This.X1BUFCFG.FULL_WM := Watermark'Enum_Rep;
         when X2 =>
            null;
         when Y =>
            This.YBUFCFG.EMPTY_WM := Watermark'Enum_Rep;
      end case;
   end Set_Buffer_Watermark;

   ------------------
   -- Set_Clipping --
   ------------------

   procedure Set_Clipping
     (This   : in out FMAC_Accelerator;
      Enable : Boolean)
   is
   begin
      This.CR.CLIPEN := Enable;
   end Set_Clipping;

   -----------------------------
   -- Configure_Memory_Buffer --
   -----------------------------

   procedure Configure_Memory_Buffer
     (This         : in out FMAC_Accelerator;
      Buffer       : FMAC_Buffer;
      Base_Address : UInt8;
      Size         : UInt8;
      Watermark    : FMAC_Watermark := Threshold_1)
   is
   begin
      case Buffer is
         when X1 =>
            Set_Buffer_Address (This, X1, Base_Address);
            Set_Buffer_Size (This, X1, Size);
            Set_Buffer_Watermark (This, X1, Watermark);
         when X2 =>
            Set_Buffer_Address (This, X2, Base_Address);
            Set_Buffer_Size (This, X2, Size);
         when Y =>
            Set_Buffer_Address (This, Y, Base_Address);
            Set_Buffer_Size (This, Y, Size);
            Set_Buffer_Watermark (This, Y, Watermark);
      end case;
   end Configure_Memory_Buffer;

   ------------------------------
   -- Configure_Memory_Buffers --
   ------------------------------

   procedure Configure_Memory_Buffers
     (This   : in out FMAC_Accelerator;
      Config : Memory_Buffer_Configuration)
   is
   begin
      --  Configure X1 buffer for input values
      Set_Buffer_Address (This, X1, Config.Input_Base_Address);
      Set_Buffer_Size (This, X1, Config.Input_Buffer_Size);
      Set_Buffer_Watermark (This, X1, Config.Input_Buffer_Threshold);

      --  Configure X2 buffer for coefficients
      Set_Buffer_Address (This, X2, Config.Coeff_Base_Address);
      Set_Buffer_Size (This, X2, Config.Coeff_Buffer_Size);

      --  Configure Y buffer for output values
      Set_Buffer_Address (This, Y, Config.Output_Base_Address);
      Set_Buffer_Size (This, Y, Config.Output_Buffer_Size);
      Set_Buffer_Watermark (This, Y, Config.Output_Buffer_Threshold);

      Set_Clipping (This, Config.Clipping);
   end Configure_Memory_Buffers;

   --------------------
   -- Set_FMAC_Start --
   --------------------

   procedure Set_FMAC_Start
     (This  : in out FMAC_Accelerator;
      Start : Boolean)
   is
   begin
      This.PARAM.START := Start;
   end Set_FMAC_Start;

   ------------------
   -- FMAC_Started --
   ------------------

   function FMAC_Started (This : FMAC_Accelerator) return Boolean is
   begin
      return This.PARAM.START;
   end FMAC_Started;

   --------------------
   -- Write_Function --
   --------------------

   procedure Write_Function
     (This : in out FMAC_Accelerator;
      Func : FMAC_Function)
   is
   begin
      This.PARAM.FUNC := Func'Enum_Rep;
   end Write_Function;

   -------------------
   -- Read_Function --
   -------------------

   function Read_Function
     (This : FMAC_Accelerator) return FMAC_Function
   is
   begin
      return FMAC_Function'Enum_Val (This.PARAM.FUNC);
   end Read_Function;

   ----------------------
   -- Configure_Buffer --
   ----------------------

   procedure Configure_Buffer
     (This      : in out FMAC_Accelerator;
      Operation : FMAC_Buffer_Function;
      Input_P   : UInt8;
      Input_Q   : UInt8 := 0)
   is
   begin
      This.PARAM.P := Input_P;

      case Operation is
         when Load_X2_Buffer =>
            This.PARAM.Q := Input_Q;
         when others => null;
      end case;

      Write_Function (This, Operation);
   end Configure_Buffer;

   -----------------------
   -- Write_Filter_Gain --
   -----------------------

   procedure Write_Filter_Gain
     (This : in out FMAC_Accelerator;
      Gain : UInt8)
   is
   begin
      This.PARAM.R := Gain;
   end Write_Filter_Gain;

   ----------------------
   -- Read_Filter_Gain --
   ----------------------

   function Read_Filter_Gain
     (This : FMAC_Accelerator) return UInt8
   is
   begin
      return This.PARAM.R;
   end Read_Filter_Gain;

   ----------------------
   -- Configure_Filter --
   ----------------------

   procedure Configure_Filter
     (This      : in out FMAC_Accelerator;
      Operation : FMAC_Filter_Function;
      Input_P   : UInt8;
      Input_Q   : UInt8 := 0;
      Input_R   : UInt8)
   is
   begin
      This.PARAM.P := Input_P;
      This.PARAM.R := Input_R;

      case Operation is
         when IIR_Filter_Direct_Form_1 =>
            This.PARAM.Q := Input_Q;
         when others => null;
      end case;

      Write_Function (This, Operation);
   end Configure_Filter;

   ----------------
   -- Write_Data --
   ----------------

   procedure Write_Data
     (This  : in out FMAC_Accelerator;
      Value : UInt16)
   is
   begin
      This.WDATA.WDATA := Value;
   end Write_Data;

   ---------------
   -- Read_Data --
   ---------------

   function Read_Data
     (This : FMAC_Accelerator) return UInt16
   is
   begin
      return This.RDATA.RDATA;
   end Read_Data;

   ------------------
   -- Write_Buffer --
   ------------------

   procedure Write_Buffer
     (This   : in out FMAC_Accelerator;
      Vector : Block_Q1_15)
   is
   begin
      for N in Vector'Range loop
         Write_Data (This, Q1_15_To_UInt16 (Vector (N)));
      end loop;
   end Write_Buffer;

   ----------------------
   --  Preload_Buffers --
   ----------------------

   procedure Preload_Buffers
     (This           : in out FMAC_Accelerator;
      Filter         : FMAC_Filter_Function;
      Input_Vector   : Block_Q1_15;
      Coeff_Vector_B : Block_Q1_15;
      Coeff_Vector_A : Block_Q1_15;
      Output_Vector  : Block_Q1_15)
   is
   begin
      --  Make shure there is no DMA nor interrupt enabled since no flow
      --  control is required.
      if Interrupt_Enabled (This, Read_Interrupt) then
         Set_Interrupt (This, Read_Interrupt, False);
      end if;
      if Interrupt_Enabled (This, Write_Interrupt) then
         Set_Interrupt (This, Write_Interrupt, False);
      end if;
      if DMA_Enabled (This, Read_DMA) then
         Set_DMA (This, Read_DMA, False);
      end if;
      if DMA_Enabled (This, Write_DMA) then
         Set_DMA (This, Write_DMA, False);
      end if;

      --  Preload input values into X1 buffer
      if Input_Vector'Length /= 0 then
         Configure_Buffer (This,
                           Operation => Load_X1_Buffer,
                           Input_P   => Input_Vector'Length);
         Set_FMAC_Start (This, True);
         Write_Buffer (This, Input_Vector);

         --  Test if START bit is reset
         pragma Assert (FMAC_Started (This), "Preload X1 not done");
      end if;

      --  Preload coefficients into X2 buffer
      if Filter = IIR_Filter_Direct_Form_1 then
         Configure_Buffer (This,
                                Operation => Load_X2_Buffer,
                                Input_P   => Coeff_Vector_B'Length,
                                Input_Q   => Coeff_Vector_A'Length);
      else
         Configure_Buffer (This,
                                Operation => Load_X2_Buffer,
                                Input_P   => Coeff_Vector_B'Length);
      end if;
      Set_FMAC_Start (This, True);
      Write_Buffer (This, Coeff_Vector_B);

      if Filter = IIR_Filter_Direct_Form_1 then
         Write_Buffer (This, Coeff_Vector_A);
      end if;

      --  Test if START bit is reset
      pragma Assert (FMAC_Started (This), "Preload X2 not done");

      --  Preload output values into Y buffer
      if Output_Vector'Length /= 0 then
         Configure_Buffer (This,
                           Operation => Load_Y_Buffer,
                           Input_P   => Output_Vector'Length);
         Set_FMAC_Start (This, True);
         Write_Buffer (This, Output_Vector);

         --  Test if START bit is reset
         pragma Assert (FMAC_Started (This), "Preload Y not done");
      end if;
   end Preload_Buffers;

   ------------
   -- Status --
   ------------

   function Status
     (This : FMAC_Accelerator;
      Flag : FMAC_Status_Flag) return Boolean
   is
   begin
      case Flag is
         when Y_Buffer_Empty =>
            return This.SR.YEMPTY;
         when X1_Buffer_Full =>
            return This.SR.X1FULL;
         when Overflow_Error =>
            return This.SR.OVFL;
         when Underflow_Error =>
            return This.SR.UNFL;
         when Saturation_Error =>
            return This.SR.SAT;
      end case;
   end Status;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status
     (This : in out FMAC_Accelerator;
      Flag : FMAC_Status_Flag)
   is
   begin
      case Flag is
         when Y_Buffer_Empty =>
            This.SR.YEMPTY := False;
         when X1_Buffer_Full =>
            This.SR.X1FULL := False;
         when Overflow_Error =>
            This.SR.OVFL := False;
         when Underflow_Error =>
            This.SR.UNFL := False;
         when Saturation_Error =>
            This.SR.SAT := False;
      end case;
   end Clear_Status;

   -------------------
   -- Set_Interrupt --
   -------------------

   procedure Set_Interrupt
     (This      : in out FMAC_Accelerator;
      Source : FMAC_Interrupts;
      Enable    : Boolean)
   is
   begin
      case Source is
         when Read_Interrupt =>
            This.CR.RIEN := Enable;
         when Write_Interrupt =>
            This.CR.WIEN := Enable;
         when Overflow_Error =>
            This.CR.OVFLIEN := Enable;
         when Underflow_Error =>
            This.CR.UNFLIEN := Enable;
         when Saturation_Error =>
            This.CR.SATIEN := Enable;
      end case;
   end Set_Interrupt;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This   : FMAC_Accelerator;
      Source : FMAC_Interrupts) return Boolean
   is
   begin
      case Source is
         when Read_Interrupt =>
            return This.CR.RIEN;
         when Write_Interrupt =>
            return This.CR.WIEN;
         when Overflow_Error =>
            return This.CR.OVFLIEN;
         when Underflow_Error =>
            return This.CR.UNFLIEN;
         when Saturation_Error =>
            return This.CR.SATIEN;
      end case;
   end Interrupt_Enabled;

   -------------
   -- Set_DMA --
   -------------

   procedure Set_DMA
     (This   : in out FMAC_Accelerator;
      Source : FMAC_DMA;
      Enable : Boolean)
   is
   begin
      case Source is
         when Read_DMA =>
            This.CR.DMAREN := Enable;
         when Write_DMA =>
            This.CR.DMAWEN := Enable;
      end case;
   end Set_DMA;

   -----------------
   -- DMA_Enabled --
   -----------------

   function DMA_Enabled
     (This   : FMAC_Accelerator;
      Source : FMAC_DMA)
      return Boolean
   is
   begin
      case Source is
         when Read_DMA =>
            return This.CR.DMAREN;
         when Write_DMA =>
            return This.CR.DMAWEN;
      end case;
   end DMA_Enabled;

end STM32.FMAC;
