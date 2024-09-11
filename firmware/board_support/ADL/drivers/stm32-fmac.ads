with Ada.Unchecked_Conversion;

private with STM32_SVD.FMAC;

package STM32.FMAC is
   pragma Elaborate_Body;

   type FMAC_Accelerator is limited private;

   procedure Reset_FMAC (This : in out FMAC_Accelerator);
   --  Resets the write and read pointers, the internal control logic,
   --  the FMAC_SR register and the FMAC_PARAM register, including the START
   --  bit if active. Other register settings are not affected. This bit is
   --  reset by hardware.

   type FMAC_Buffer is (X1, X2, Y);
   --  Implementing FIR filters with FMAC (RM0440 rev 6 Chapter 18.3.8):
   --
   --  The FMAC supports FIR filters of length N, where N is the number of taps
   --  or coefficients. The minimum local memory requirement for a FIR filter of
   --  length N is 2N + 1: N coefficients, N input samples and 1 output sample.
   --  Since the local memory size is 256, the maximum value for N is 127.
   --  If maximum throughput is required, it may be necessary to allocate a
   --  small amount of extra space, d1 and d2, to the input and output sample
   --  buffers respectively, to ensure that the filter never stalls waiting for
   --  a new input sample, or waiting for the output sample to be read. In this
   --  case, the local memory requirement is 2N + d1 + d2.
   --
   --  Implementing IIR filters with FMAC (RM0440 rev 6 Chapter 18.3.9):
   --
   --  The FMAC supports IIR filters of length N, where N is the number of
   --  feed-forward taps or coefficients. The number of feedback coefficients,
   --  M, can be any value from 1 to N-1. Only direct form 1 implementations can
   --  be realized, so filters designed for other forms need to be converted.
   --  The minimum memory requirement for an IIR filter with N feed-forward
   --  coefficients and M feed-back coefficients is 2N + 2M: N + M coefficients,
   --  N input samples and M output samples. If M = N-1, then the maximum filter
   --  length that can be implemented is N = 64.
   --  As for the FIR, for maximum throughput a small amount of additional
   --  space, d1 and d2, should be allowed in the input and output buffer size
   --  respectively, making the total memory requirement 2M + 2N + d1 + d2.

   procedure Set_Buffer_Address
     (This         : in out FMAC_Accelerator;
      Buffer       : FMAC_Buffer;
      Base_Address : UInt8);
   --  Define the base address for the X1, X2 and Y buffers.

   procedure Set_Buffer_Size
     (This         : in out FMAC_Accelerator;
      Buffer       : FMAC_Buffer;
      Size         : UInt8)
     with Pre => (if Buffer = X2 then not FMAC_Started (This));
   --  Define the size of buffers in 16-bit words.
   --  For X1 buffer the minimum buffer size is the number of feed-forward taps
   --  in the filter (+ the watermark threshold - 1).
   --  For X2 buffer and FIR filters, the minimum size is the number of filter
   --  coeficients B with fixed length (N + 1) (b0, b1, b2..., bN). For IIR
   --  filter, the minimum size is the number of coeficients B and A concatenated
   --  with fixed length (M + N + 1) (b0, b1, b2..., bN, a1, a2, ..., aM).
   --  For Y buffer and for FIR filters, the minimum buffer size is 1 (+ the
   --  watermark threshold). For IIR filters the minimum buffer size is the
   --  number of feedback taps (+ the watermark threshold).
   --  See RM0440 rev 6, Chapter 18.3.2 and 18.3.5.

   type FMAC_Watermark is
     (Threshold_1,
      Threshold_2,
      Threshold_4,
      Threshold_8);

   procedure Set_Buffer_Watermark
     (This         : in out FMAC_Accelerator;
      Buffer       : FMAC_Buffer;
      Watermark    : FMAC_Watermark := Threshold_1)
     with Pre => (if DMA_Enabled (This, Write_DMA) or
                     DMA_Enabled (This, Read_DMA)
                  then Watermark = Threshold_1);
   --  For X1 buffer defines the threshold for setting the X1 buffer full flag
   --  when operating in circular mode. The flag is set if the number of free
   --  spaces in the buffer is less than 2**FULL_WM. The FULL_WM bitfield of X1
   --  buffer configuration register must be programmed with a value less than
   --  or equal to log2(d1), otherwise the buffer is flagged full before N input
   --  samples have been written, and no more samples are requested.
   --  For Y buffer defines the threshold for setting the Y buffer empty flag
   --  when operating in circular mode. The flag is set if the number of unread
   --  values in the buffer is less than 2**EMPTY_WM. The EMPTY_WM bitfield
   --  of the Y buffer configuration register must be less than or equal to
   --  log2(d2), otherwise the buffer is flagged empty before N output values
   --  have been read.

   procedure Set_Clipping
     (This   : in out FMAC_Accelerator;
      Enable : Boolean);
   --  Values at the output of the accumulator, which exceed the q1.15 range,
   --  wrap (clipping disabled) or are saturated to the maximum positive or
   --  negative value (+1 or -1) according to the sign.

   procedure Configure_Memory_Buffer
     (This         : in out FMAC_Accelerator;
      Buffer       : FMAC_Buffer;
      Base_Address : UInt8;
      Size         : UInt8;
      Watermark    : FMAC_Watermark := Threshold_1);
   --  Configure base addres, size and threshold for input, coefficient or
   --  output buffer.
   --  See RM0440 rev 6, Chapter 18.3.2 "Local memory and buffers" and 18.3.5
   --  "Initialization functions".

   type Memory_Buffer_Configuration is record
      Coeff_Base_Address      : UInt8 := 0; --  X2 buffer base
      Coeff_Buffer_Size       : UInt8; --  N + M
      Input_Base_Address      : UInt8; --  X1 buffer base = Coeff_Buffer_Size
      Input_Buffer_Size       : UInt8; --  N + d1
      Input_Buffer_Threshold  : FMAC_Watermark; --  <= d1
      Output_Base_Address     : UInt8; --  Y buffer base = Coeff_Buffer_Size + Input_Buffer_Size
      Output_Buffer_Size      : UInt8; --  M + d2
      Output_Buffer_Threshold : FMAC_Watermark; --  < d2
      Clipping                : Boolean;
   end record;

   procedure Configure_Memory_Buffers
     (This   : in out FMAC_Accelerator;
      Config : Memory_Buffer_Configuration);
   --  Configure base addres, size and threshold for input, coefficient and
   --  output buffers.

   procedure Set_FMAC_Start
     (This  : in out FMAC_Accelerator;
      Start : Boolean)
     with Post => FMAC_Started (This) = Start;
   --  Triggers the execution of the function selected in the FUNC bitfield.
   --  Resetting it by software stops any ongoing function. For initialization
   --  functions (Load X1, X2 and Y buffers), this bit is reset by hardware.

   function FMAC_Started
     (This : FMAC_Accelerator) return Boolean;

   type FMAC_Function is
     (Load_X1_Buffer,
      Load_X2_Buffer,
      Load_Y_Buffer,
      FIR_Filter_Convolution,
      IIR_Filter_Direct_Form_1)
     with Size => 7;

   for FMAC_Function use
     (Load_X1_Buffer           => 16#01#,
      Load_X2_Buffer           => 16#02#,
      Load_Y_Buffer            => 16#03#,
      FIR_Filter_Convolution   => 16#08#,
      IIR_Filter_Direct_Form_1 => 16#09#);

   subtype FMAC_Buffer_Function is FMAC_Function range
     Load_X1_Buffer .. Load_Y_Buffer;

   subtype FMAC_Filter_Function is FMAC_Function range
     FIR_Filter_Convolution .. IIR_Filter_Direct_Form_1;

   procedure Write_Function
     (This : in out FMAC_Accelerator;
      Func : FMAC_Function)
     with Post => Read_Function (This) = Func;

   function Read_Function
     (This : FMAC_Accelerator) return FMAC_Function;

   procedure Configure_Buffer
     (This      : in out FMAC_Accelerator;
      Operation : FMAC_Buffer_Function;
      Input_P   : UInt8; --  Length N of the coefficient vector B
      Input_Q   : UInt8 := 0) --  Length M of the coefficient vector A
     with Pre => not FMAC_Started (This);
   --  Trigger by writing the appropriate value in the FUNC bitfield of the
   --  FMAC_PARAM register, with the START bit set. The P and Q bitfields
   --  must also contain the appropriate parameter values for each function.
   --  For initialization functions (load X1, X2 or Y buffers), the function
   --  completes when N writes have been performed to the FMAC_WDATA register,
   --  then the START bit is automatically reset by hardware.
   --  See RM0440 rev 6 section 18.3.5 for detailed instructions about each
   --  initialization functions (Load X1, X2 and Y buffers).

   procedure Write_Filter_Gain
     (This : in out FMAC_Accelerator;
      Gain : UInt8)
     with Pre => not FMAC_Started (This);
   --  Write the gain value applied to the accumulator output.

   function Read_Filter_Gain
     (This : FMAC_Accelerator) return UInt8
     with Pre => Read_Function (This) in FMAC_Filter_Function;
   --  Read the gain applied to the accumulator output. It is only valid when a
   --  filter function is programmed in the FUNC bitfield of the FMAC_PARAM
   --  register.

   procedure Configure_Filter
     (This      : in out FMAC_Accelerator;
      Operation : FMAC_Filter_Function;
      Input_P   : UInt8; --  Length N of the coefficient vector B
      Input_Q   : UInt8 := 0; --  Length M of the coefficient vector A
      Input_R   : UInt8) --  Gain applied to the accumulator output
     with Pre => not FMAC_Started (This);
   --  Trigger by writing the appropriate value in the FUNC bitfield of the
   --  FMAC_PARAM register, with the START bit set. The P, Q and R bitfields
   --  must also contain the appropriate parameter values for each function.
   --  For filter functions (FIR or IIR), the filter functions continue to run
   --  until the START bit is reset by software.
   --  See RM0440 rev 6 section 18.3.5 for detailed instructions about filter
   --  functions (FIR and IIR).

   type Block_16 is array (Positive range <>) of UInt16
     with Component_Size => 16;

   --  The FMAC operates in fixed point signed integer format. Input and output
   --  values are q1.15.
   --  In q1.15 format, numbers are represented by one sign bit and 15 fractional
   --  bits (binary decimal places). The numeric range is therefore -1 (0x8000)
   --  to 1 - 2**(-15) (0x7FFF).
   type Q1_15 is delta 2.0**(-15) range -1.0 .. 1.0 - 2.0**(-15)
     with Size => 16, Small => 2.0**(-15);
   type Block_Q1_15 is array (Positive range <>) of Q1_15;

   --  The input (WDATA) and output (RDATA) data of the FMAC uses UInt16
   --  to represent the fixed point values. So we need to convert the type
   --  Q1_15 to UInt16 and vice-versa.
   function Q1_15_To_UInt16 is new Ada.Unchecked_Conversion (Q1_15, UInt16);
   function UInt16_To_Q1_15 is new Ada.Unchecked_Conversion (UInt16, Q1_15);

   procedure Write_Data
     (This  : in out FMAC_Accelerator;
      Value : UInt16);
   --  When a write access to this register occurs, the write data are
   --  transferred to the address offset indicated by the write pointer. The
   --  pointer address is automatically incremented after each write access.

   function Read_Data
     (This : FMAC_Accelerator) return UInt16;
   --  When a read access to this register occurs, the read data are the
   --  contents of the Y output buffer at the address offset indicated by the
   --  READ pointer. The pointer address is automatically incremented after
   --  each read access.

   procedure Write_Buffer
     (This   : in out FMAC_Accelerator;
      Vector : Block_Q1_15);
   --  Write the values of Vector into the X1, X2 or Y buffers pre-selected by
   --  Configure_FMAC_Parameters and Set_FMAC_Start to True. This is useful when
   --  we want to preload the X1, X2 or Y FMAC buffers with initial values. Note
   --  that the length of Vector must coincide with the specified length of each
   --  FMAC buffer.

   procedure Preload_Buffers
     (This           : in out FMAC_Accelerator;
      Filter         : FMAC_Filter_Function;
      Input_Vector   : Block_Q1_15; --  X1 buffer
      Coeff_Vector_B : Block_Q1_15; --  X2 buffer (first part, feed-foward taps)
      Coeff_Vector_A : Block_Q1_15; --  X2 buffer (second part, feedback taps)
      Output_Vector  : Block_Q1_15) --  Y buffer
     with Pre => Coeff_Vector_B'Length /= 0,
          Post => not Interrupt_Enabled (This, Read_Interrupt) and
                  not Interrupt_Enabled (This, Write_Interrupt) and
                  not DMA_Enabled (This, Read_DMA) and
                  not DMA_Enabled (This, Write_DMA);
   --  Preload the X1, X2 and Y FMAC buffers with polling. For FIR, The X2
   --  buffer has only b0 .. bN coefficients from Coeff_Vector_B, so
   --  Coeff_Vector_A don't care; for IIR, X2 has b0 .. bN concatenated with
   --  a1 .. aM from Coeff_Vector_A.
   --  It is optional to preload X1 because when the FMAC start it will only
   --  calculate the first filter output when it has reached X1 buffer size.
   --  If the preload of X1 is not done, this buffer must have zero length.
   --  For IIR, it is recommended to preload the Y buffer with zeros to avoid
   --  unpredictable transient values at the outset.

   type FMAC_Status_Flag is
     (Y_Buffer_Empty,
      X1_Buffer_Full,
      Overflow_Error,
      Underflow_Error,
      Saturation_Error);

   function Status
     (This : FMAC_Accelerator;
      Flag : FMAC_Status_Flag) return Boolean;

   procedure Clear_Status
     (This : in out FMAC_Accelerator;
      Flag : FMAC_Status_Flag);

   type FMAC_Interrupts is
     (Read_Interrupt,
      Write_Interrupt,
      Overflow_Error,
      Underflow_Error,
      Saturation_Error);

   procedure Set_Interrupt
     (This   : in out FMAC_Accelerator;
      Source : FMAC_Interrupts;
      Enable : Boolean)
     with Post => Interrupt_Enabled (This, Source) = Enable;

   function Interrupt_Enabled
     (This   : FMAC_Accelerator;
      Source : FMAC_Interrupts) return Boolean;

   type FMAC_DMA is (Read_DMA, Write_DMA);

   procedure Set_DMA
     (This   : in out FMAC_Accelerator;
      Source : FMAC_DMA;
      Enable : Boolean)
     with Post => DMA_Enabled (This, Source) = Enable;

   function DMA_Enabled
     (This   : FMAC_Accelerator;
      Source : FMAC_DMA) return Boolean;

private

   type FMAC_Accelerator is new STM32_SVD.FMAC.FMAC_Peripheral;

end STM32.FMAC;
