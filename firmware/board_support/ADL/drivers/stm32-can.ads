------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2015-2018, AdaCore                        --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of STMicroelectronics nor the names of its       --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

--  This file provides interfaces for the CAN modules on the
--  STM32G4 (ARM Cortex M4F) microcontrollers from ST Microelectronics.
--  It is based on the stm32g4xx_hal_fdcan.h and stm32g4xx_hal_fdcan.c files.

private with System;

private with STM32_SVD.FDCAN;

with Ada.Real_Time; use Ada.Real_Time;

package STM32.CAN is

   type CAN_Controller is limited private;

   procedure Enter_Init_Mode
     (This : in out CAN_Controller)
    with Post => Is_Init_Mode (This);

   function Is_Init_Mode (This : CAN_Controller) return Boolean;

   procedure Exit_Init_Mode
     (This : in out CAN_Controller)
    with Post => not Is_Init_Mode (This);

   procedure Sleep
     (This : in out CAN_Controller)
     with Post => Is_Sleep_Mode (This);

   function Is_Sleep_Mode (This : CAN_Controller) return Boolean;

   procedure Wakeup
     (This : in out CAN_Controller)
     with Post => not Is_Sleep_Mode (This);

   type Clock_Divider is
     (Div_1,
      Div_2,
      Div_4,
      Div_6,
      Div_8,
      Div_10,
      Div_12,
      Div_14,
      Div_16,
      Div_18,
      Div_20,
      Div_22,
      Div_24,
      Div_26,
      Div_28,
      Div_30)
     with Size => 4;

   procedure Configure_Clock_Divider
     (This    : in out CAN_Controller;
      Divider : Clock_Divider)
     with Pre => Is_Init_Mode (This);
   --  The APB clock could be divided prior to be used by the CAN sub system.
   --  The rate must be computed using the kernel divider output clock. The
   --  clock is common to all FDCAN instances. This parameter is applied only at
   --  initialisation of first FDCAN instance.

   ---------------------------------------------------
   --  Types for Nominal (arbitration) CAN FD phase --
   ---------------------------------------------------

   subtype Resynch_Quanta_N is Positive range 1 .. 128;
   --  These bits define the maximum number of time quanta the CAN hardware is
   --  allowed to lengthen or shorten a bit to perform the resynchronization.
   --  This is the SJW. The preferred value used for CANopen and DeviceNet is 1.
   subtype Segment_1_Quanta_N is Positive range 1 .. 256;
   --  Defines the location of the sample point (number of time quanta).
   --  It includes the PROP_SEG and PHASE_SEG1 of the CAN standard.
   subtype Segment_2_Quanta_N is Positive range 1 .. 128;
   --  Defines the location of the sample point (number of time quanta).
   --  It represents the PHASE_SEG2 of the CAN standard.
   subtype Time_Quanta_Prescaler_N is Positive range 1 .. 512;
   --  These bits define the length of a time quanta.

   type Bit_Timing_Config_N is record
      Resynch_Jump_Width : Resynch_Quanta_N;
      Time_Segment_1     : Segment_1_Quanta_N;
      Time_Segment_2     : Segment_2_Quanta_N;
      Quanta_Prescaler   : Time_Quanta_Prescaler_N;
   end record;
   --  Bit timing for Nominal (arbitration) CAN FD phase.

   subtype Bit_Time_Quanta_N is Positive range 8 .. 81;
   --  This is the number of time quanta in one Bit Time. So for a 1 MHz bit
   --  rate and the minimum Bit_Time_Quanta = 8, the minimum prescaler input
   --  frequency is 8 MHz.
   --  1 Bit time (= 1/bit rate) is defined by four time segments:
   --  SYNC_SEG - 1 time quantum long;
   --  PROP_SEG - 1 to 128 time quanta long;
   --  PHASE_SEG1 - 1 to 128 time quanta long;
   --  PHASE_SEG2 - maximum of PHASE_SEG1 and the Information processing time,
   --  that is less then or equal to 2 Time Quanta long.
   --
   --  The sample point of start frame is taken at 87.5% maximum of
   --  Bit_Time_Quanta'Last, and must not be grater then SYNC_SEG + PROP_SEG +
   --  PHASE_SEG (Segment_Sync_Quanta + Segment_1_Quanta) = 257. So the maximum
   --  value for Bit_Time_Quanta is 257 / 0.875 = 293.7 ~ 294, but the limits
   --  for this FDCAN is in the range of 4 to 81. See RM0440 rev. 6 chapter
   --  44.4.7 FDCAN nominal bit timing.

   subtype Bit_Rate_Range_N is Float range 1.0 .. 1_000.0;
   --  This is the actual bit rate frequency of the CAN arbitration phase in kHz.
   --  The standard frequencies are 10, 20, 50, 83.333, 100, 125, 250, 500, 800
   --  and 1000 kHz.

   ----------------------------------
   --  Types for Data CAN FD phase --
   ----------------------------------

   subtype Resynch_Quanta_D is Positive range 1 .. 16;
   --  These bits define the maximum number of time quanta the CAN hardware is
   --  allowed to lengthen or shorten a bit to perform the resynchronization.
   --  This is the SJW. The preferred value used for CANopen and DeviceNet is 1.
   subtype Segment_1_Quanta_D is Positive range 1 .. 32;
   --  Defines the location of the sample point (number of time quanta).
   --  It includes the PROP_SEG and PHASE_SEG1 of the CAN standard.
   subtype Segment_2_Quanta_D is Positive range 1 .. 16;
   --  Defines the location of the sample point (number of time quanta).
   --  It represents the PHASE_SEG2 of the CAN standard.
   subtype Time_Quanta_Prescaler_D is Positive range 1 .. 32;
   --  These bits define the length of a time quanta.

   type Bit_Timing_Config_D is record
      Resynch_Jump_Width : Resynch_Quanta_D;
      Time_Segment_1     : Segment_1_Quanta_D;
      Time_Segment_2     : Segment_2_Quanta_D;
      Quanta_Prescaler   : Time_Quanta_Prescaler_D;
   end record;
   --  Bit timing for data CAN FD phase.

   subtype Bit_Time_Quanta_D is Positive range 4 .. 38;
   --  This is the number of time quanta in one Bit Time.
   --  1 Bit time (= 1/bit rate) is defined by four time segments:
   --  SYNC_SEG - 1 time quantum long;
   --  PROP_SEG - 1 to 16 time quanta long;
   --  PHASE_SEG1 - 1 to 16 time quanta long;
   --  PHASE_SEG2 - maximum of PHASE_SEG1 and the Information processing time,
   --  that is less then or equal to 2 Time Quanta long.
   --
   --  The sample point of start frame is taken at 87.5% maximum of
   --  Bit_Time_Quanta'Last, and must not be grater then SYNC_SEG + PROP_SEG +
   --  PHASE_SEG (Segment_Sync_Quanta + Segment_1_Quanta) = 33. So the maximum
   --  value for Bit_Time_Quanta is 33 / 0.875 = 37.7 ~ 38.

   subtype Bit_Rate_Range_D is Float range 1.0 .. 8_000.0;
   --  This is the actual bit rate frequency of the CAN data phase in kHz.
   --  There is no restrictions of maximum bit rate but it has to be larger or
   --  equal to that in arbitration phase.

   -----------------------------------
   --  Types for both CAN FD phases --
   -----------------------------------

   Segment_Sync_Quanta : constant Positive := 1;
   --  This is the SYNC_SEG segment, the time quanta for syncronism.

   subtype Sample_Point_Range is Float range 50.0 .. 90.0;
   --  The sample point of the start frame (at the end of PHASE_SEG1) is taken
   --  between 50 to 90% of the Bit Time. The preferred value used by CANopen
   --  and DeviceNet is 87.5% and 75% for ARINC 825.
   --  See http://www.bittiming.can-wiki.info/#bxCAN for this calculation.

   subtype Clock_Tolerance is Float range 0.0 .. 1.58;
   --  Clock tolerance for the bit rate in percent.
   --  The tolerance range df for an oscillator’s frequency fosc around the
   --  nominal frequency fnom with fosc = fnom ± df depends on the proportions
   --  of Phase_Seg1, Phase_Seg2, SJW, and the bit time. The maximum tolerance
   --  df for the Nominal (Arbitration) phase is the defined by two conditions
   --  (both shall be met):
   --
   --               SJWA                     min(Phase_Seg1A, Phase_Seg2A)
   --  1. df ≤ --------------       2. df ≤ -------------------------------
   --           20*Bit_TimeA                  2*(13*Bit_TimeA-Phase_Seg2A)
   --
   --  The combination Prop_Seg = 1 and Phase_Seg1 = Phase_Seg2 = SJW = 4 allows
   --  the largest possible oscillator tolerance of 1.58%.
   --  See chapter 4 Oscillator Tolerance Range from The Configuration of the
   --  CAN Bit Timing, Florian Hartwitch, Armin Bassemir, 6th International CAN
   --  Conference.
   --  For the Data phase, the maximum tolerance df is defined by three
   --  conditions:
   --
   --               SJWD
   --  1. df ≤ --------------
   --           20*Bit_TimeD
   --
   --                         min(Phase_Seg1A, Phase_Seg2A)
   --  2. df ≤ -------------------------------------------------------------
   --                                         BRPrescD
   --           2*[(6*Bit_TimeD-Phase_Seg2D)*----------+7*Bit_TimeA]
   --                                         BRPrescA
   --
   --                          SJWD-max(0;BRPrescA/BRPrescD-1)
   --  3. df ≤ -----------------------------------------------------------------
   --                                         BRPrescA
   --           2*[(2*Bit_TimeA-Phase_Seg2A)*----------+Phase_Seg2D+4*Bit_TimeD]
   --                                         BRPrescD
   --  See chapter 2 in Robustness of a CAN FD Bus System - About Oscillator
   --  Tolerance and Edge Deviations - Dr. Arthur Mutter

   procedure Calculate_Bit_Timing
     (This           : aliased CAN_Controller;
      Speed_N        : Bit_Rate_Range_N;
      Speed_D        : Bit_Rate_Range_D;
      Sample_Point_N : Sample_Point_Range;
      Sample_Point_D : Sample_Point_Range;
      Tolerance      : Clock_Tolerance;
      Bit_Timing_N   : out Bit_Timing_Config_N;
      Bit_Timing_D   : out Bit_Timing_Config_D);
   --  Automatically calculate bit timings based on requested bit rate and
   --  sample ratio.
   --  1 nominal Bit Time is defined by the time length in quanta of four time
   --  segments, each one composed of 1 or more quanta: SINC_SEG, PROP_SEG,
   --  PHASE_SEG1 and PHASE_SEG2.
   --  The Baud Rate (the Bit frequency) is the inverse of 1 nominal Bit Time.
   --  The prescaler is calculated to get the time of one quanta, so we divide
   --  the bus frequency of the CAN peripheral by the baud rate and divide this
   --  value by the number of quanta in one nominal Bit time.
   --  See RM0440 rev. 6 chapter 44.3.1 Bit timing.

   procedure Configure_Bit_Timing
     (This         : in out CAN_Controller;
      Bit_Timing_N : Bit_Timing_Config_N;
      Bit_Timing_D : Bit_Timing_Config_D)
     with Pre => Is_Init_Mode (This);

   type Operating_Mode is
     (Normal,
      Loopback, --  external loopback
      Silent, --  bus monitoring
      Silent_Loopback, --  internal loopback
      Restricted);

   procedure Set_Operating_Mode
     (This : in out CAN_Controller;
      Mode : Operating_Mode)
     with Pre => (if Mode /= Normal then Is_Init_Mode (This));

   function Is_Restricted_Mode
     (This : CAN_Controller) return Boolean;
   --  Check if the FDCAN peripheral entered Restricted Operation Mode.

   type Frame_Format is
     (Classic,
      FD_No_BRS,
      FD_BRS);
   --  Classic CAN or FDCAN with or without BitRate Switching.

   procedure Set_Frame_Format
     (This  : in out CAN_Controller;
      Frame : Frame_Format)
     with Pre => Is_Init_Mode (This);
   --  Choose between classic CAN or FDCAN with or without BitRate Switching.

   procedure Set_Edge_Filtering
     (This    : in out CAN_Controller;
      Enabled : Boolean)
     with Pre => Is_Init_Mode (This);
   --  Enable edge filtering during bus integration. Two consecutive dominant
   --  tq are required to detect an edge for hard synchronization.

   procedure Set_Non_ISO_Mode
     (This    : in out CAN_Controller;
      Enabled : Boolean)
     with Pre => Is_Init_Mode (This);
   --  When disabled ISO 11898-1 protocol mode, CAN FD frame format is according
   --  to Bosch CAN FD specification V1.0.

   procedure Configure
     (This                : in out CAN_Controller;
      Mode                : Operating_Mode;
      Frame               : Frame_Format;
      Auto_Retransmission : Boolean;
      Exception_Handling  : Boolean;
      Edge_Filtering      : Boolean;
      Pause_Transmission  : Boolean;
      Non_ISO_Operation   : Boolean;
      Bit_Timing_N        : Bit_Timing_Config_N;
      Bit_Timing_D        : Bit_Timing_Config_D);

   procedure Configure_Tx_Delay_Compensation
     (This   : in out CAN_Controller;
      Offset : UInt7;
      Window : UInt7)
     with Pre => Is_Init_Mode (This);
   --  Offset value defining the distance between the measured delay from
   --  FDCAN_TX to FDCAN_RX and the secondary sample point. Valid values are
   --  0 to 127 mtq.
   --  Filter window length defines the minimum value for the SSP position,
   --  dominant edges on FDCAN_RX that would result in an earlier SSP position
   --  are ignored for transmitter delay measurements.

   procedure Set_Tx_Delay_Compensation
     (This    : in out CAN_Controller;
      Enabled : Boolean)
     with Pre => Is_Init_Mode (This);
   --  The FDCAN implements a delay compensation mechanism to compensate the CAN
   --  transceiver loop delay, thereby enabling transmission with higher bit
   --  rates during the FDCAN data phase independent of the delay of a specific
   --  CAN transceiver.

   subtype Timestamp_Prescaler is Positive range 1 .. 16;

   procedure Configure_Timestamp_Counter
     (This      : in out CAN_Controller;
      Prescaler : Timestamp_Prescaler)
     with Pre => Is_Init_Mode (This);
   --  Configures the timestamp and timeout counters time unit in multiples of
   --  CAN bit times. In CAN FD mode the internal timestamp counter TCP does
   --  not provide a constant time base due to the different CAN bit times
   --  between arbitration phase and data phase. Thus CAN FD requires an
   --  external counter for timestamp generation (TSS = 10).

   type Timestamp_Select is
     (Disabled, --  Value is allways 0x00
      From_Prescaler, --  Incremented according to TCP
      External_TIM3) --  External counter from TIM3 counter (tim3_cnt[0:15]
     with Size => 2;

   procedure Set_Timestamp_Counter
     (This : in out CAN_Controller;
      Mode : Timestamp_Select);

   procedure Clear_Timestamp_Counter
     (This : in out CAN_Controller);
   --  A write access to timestamp register TSCV[TSC] resets the counter value.

   function Get_Timestamp_Counter
     (This : CAN_Controller) return UInt16;

   type Timeout_Mode is
     (Continuous,
      Tx_FIFO_Event,
      Rx_FIFO_0,
      Rx_FIFO_1)
     with Size => 2;

   procedure Configure_Timeout_Counter
     (This    : in out CAN_Controller;
      Mode    : Timeout_Mode;
      Period  : UInt16)
     with Pre => Is_Init_Mode (This);
   --  Configures the timeout period for the down-counter.

   procedure Set_Timeout_Counter
     (This    : in out CAN_Controller;
      Enabled : Boolean);
   --  Enable/disable the timeout counter.

   procedure Reset_Timeout_Counter (This : in out CAN_Controller);
   --  When operating in Continuous mode, a write to timeout counter value
   --  (TOCV) presets the counter to the value configured by timeout period
   --  TOCC[TOP] and continues down-counting. When the timeout counter is
   --  controlled by one of the FIFOs, an empty FIFO presets the counter to
   --  the value configured by TOCC[TOP]. Down-counting is started when the
   --  first FIFO element is stored.

   function Get_Timeout_Counter
     (This : CAN_Controller) return UInt16;

   procedure Configure_RAM_Watchdog
     (This  : in out CAN_Controller;
      Value : UInt8)
     with Pre => Is_Init_Mode (This);
   --  Start value of the message RAM watchdog counter. With the reset value of
   --  00, the counter is disabled. The RAM Watchdog Counter is clocked by the
   --  fdcan_pclk clock.
   --  The RAM Watchdog monitors the READY output of the Message RAM. A Message
   --  RAM access starts the Message RAM Watchdog Counter with the value
   --  configured by the RWD[WDC] bits.
   --  The counter is reloaded with RWD[WDC] bits when the Message RAM signals
   --  successful completion by activating its READY output. In case there is
   --  no response from the Message RAM until the counter has counted down to 0,
   --  the counter stops and interrupt flag IR[WDI] bit is set.

   function Get_RAM_Watchdog
     (This : CAN_Controller) return UInt8;
   --  Actual message RAM watchdog counter value.

   procedure Set_Transmitter_Delay_Offset
     (This   : in out CAN_Controller;
      Offset : UInt7)
     with Pre => Is_Init_Mode (This);
   --  Offset value defining the distance between the measured delay from
   --  FDCAN_TX to FDCAN_RX and the secondary sample point. Valid values are
   --  0 to 127 mtq.

   procedure Set_Transmitter_Delay_Window
     (This   : in out CAN_Controller;
      Window : UInt7)
     with Pre => Is_Init_Mode (This);
   --  Defines the minimum value for the SSP position, dominant edges on
   --  FDCAN_RX that would result in an earlier SSP position are ignored for
   --  transmitter delay measurements.

   ---------------
   --  Filters  --
   ---------------

   subtype CAN_Standard_Id is HAL.UInt11;
   subtype CAN_Extended_Id is HAL.UInt29;

   subtype Filter_Bank_Std_Size is Natural range 0 .. 28;
   --  Number of standard message ID filter elements.
   subtype Filter_Bank_Ext_Size is Natural range 0 .. 8;
   --  Number of extended message ID filter elements.

   type Fifo_Operating_Mode is (Blocking, Overwrite);

   type NonMatching_Frames_Mode is
     (Rx_FIFO_0,
      Rx_FIFO_1,
      Reject)
     with Size => 2;

   type Remote_Frames_Mode is (Filter, Reject);

   procedure Configure_Global_Filter
     (This         : in out CAN_Controller;
      Remote_Std   : Remote_Frames_Mode;
      Remote_Ext   : Remote_Frames_Mode;
      NonMatch_Std : NonMatching_Frames_Mode;
      NonMatch_Ext : NonMatching_Frames_Mode;
      FIFO_0_Mode  : Fifo_Operating_Mode;
      FIFO_1_Mode  : Fifo_Operating_Mode;
      Filter_Std   : Filter_Bank_Std_Size;
      Filter_Ext   : Filter_Bank_Ext_Size)
     with Pre => Is_Init_Mode (This);

   procedure Configure_Extended_ID_Mask
     (This : in out CAN_Controller;
      Mask : CAN_Extended_Id)
     with Pre => Is_Init_Mode (This);
   --  For acceptance filtering of extended frames the Extended ID AND Mask is
   --  AND-ed with the Message ID of a received frame. Intended for masking of
   --  29-bit IDs in SAE J1939. With the reset value of all bits set to 1 the
   --  mask is not active.

   subtype Filter_Bank_Std_Nr is Filter_Bank_Std_Size
     range 1 .. Filter_Bank_Std_Size'Last;
   --  Filter list standard element number.
   subtype Filter_Bank_Ext_Nr is Filter_Bank_Ext_Size
     range 1 .. Filter_Bank_Ext_Size'Last;
   --  Filter list extended element number.

   type Filter_Type is
     (Range_ID, --  From SFID1 to SFID2 (SFID2 >= SFID1)
      Dual_ID, --  For SFID1 or SFID2
      Classic, --  SFID1 = filter, SFID2 = mask
      Option_1)
     with Size => 2;
   --  Differences between standard and extended filter IDs:
   --  Option    Standard     Extended
   --  1         Disabled     Range_ID_No_EIDM (XIDAM mask not applied)

   type Filter_Configuration is
     (Disable,
      Rx_FIFO_0, --  Store in Rx FIFO 0 if filter matches
      Rx_FIFO_1, --  Store in Rx FIFO 1 if filter matches
      Reject, --  Reject ID if filter matches
      Set_Prio, --  Set priority if filter matches
      Set_Prio_Rx_FIFO_0, --  Set priority and stores in FIFO 0 if filter matches
      Set_Prio_Rx_FIFO_1) --  Set priority and stores in FIFO 1 if filter matches
    with Size => 3;

   procedure Configure_Std_Filter
     (This    : CAN_Controller;
      Bank_Nr : Filter_Bank_Std_Nr;
      ID_Type : Filter_Type;
      Config  : Filter_Configuration;
      SFID_1  : CAN_Standard_Id;
      SFID_2  : CAN_Standard_Id)
     with Pre => Is_Init_Mode (This);
   --  SFID must be a hexadecimal number between 16#000# and 16#7FF#.
   --  See RM0440 rev 6 chapter 44.3.8 FDCAN Standard message ID filter element.

   procedure Set_Std_Filter_Activation
     (This    : CAN_Controller;
      Bank_Nr : Filter_Bank_Std_Nr;
      Config  : Filter_Configuration);
   --  Enable/disable, reject/store standard filter.

   procedure Configure_Ext_Filter
     (This    : CAN_Controller;
      Bank_Nr : Filter_Bank_Ext_Nr;
      ID_Type : Filter_Type;
      Config  : Filter_Configuration;
      EFID_1  : CAN_Extended_Id;
      EFID_2  : CAN_Extended_Id)
     with Pre => Is_Init_Mode (This);
   --  EFID must be a hexadecimal number between 16#00000000# and 16#1FFFFFFF#.
   --  See RM0440 rev 6 chapter 44.3.9 FDCAN Extended message ID filter element.

   procedure Set_Ext_Filter_Activation
     (This    : CAN_Controller;
      Bank_Nr : Filter_Bank_Ext_Nr;
      Config  : Filter_Configuration);
   --  Enable/disable, reject/store extended filter.

   ---------------
   --  Rx FIFO  --
   ---------------

   type Rx_Fifo_Nr is (FIFO_0, FIFO_1);
   subtype Rx_Fifo_Bank_Nr is Positive range 1 .. 3;
   --  Rx FIFO element number

   subtype DLC_Range is Natural range 0 .. 64
     with Static_Predicate => (case DLC_Range is
                                  when 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 12 |
                                       16 | 20 | 24 | 32 | 48 | 64 => True,
                                  when others => False);
   --  Standard and Extended Data Length Code as a Natural number.

   type DLC_Array is array (UInt4) of DLC_Range;
   Data_Length : constant DLC_Array :=
     (0, 1, 2, 3, 4, 5, 6, 7, 8, 12, 16, 20, 24, 32, 48, 64);

   type Message_Data is array (Natural range <>) of UInt8;

   function Get_Rx_Fifo_Fill_Level
     (This : CAN_Controller;
      Fifo : Rx_Fifo_Nr) return UInt2;
   --  Number of elements stored in Rx FIFO, range 0 to 3.

   type Rx_Fifo_Message is record
      ESI       : Boolean; --  Error State Indicator
      XTD       : Boolean; --  Extended Identifier
      RTR       : Boolean; --  Remote Transmission Request
      ID        : CAN_Extended_Id; --  Identifier
      ANMF      : Boolean; --  Accepted Non-Matching Frame
      FIDX      : Natural; --  Filter Index
      FDF       : Boolean; --  FD Format
      BRS       : Boolean; --  Bit Rate Switching
      DLC       : DLC_Range; --  Data Length Code
      RXTS      : Natural; --  Rx TimeStamp
      Data      : Message_Data (0 .. 63) := (others => 0); --  Up to 64 Data Bytes
   end record;

   function Read_Rx_Fifo_Message
     (This : CAN_Controller;
      Fifo : Rx_Fifo_Nr) return Rx_Fifo_Message
   with Pre => Get_Rx_Fifo_Fill_Level (This, Fifo) /= 0;

   Default_Timeout : constant Time_Span := Milliseconds (100);

   procedure Receive_Message
     (This    : CAN_Controller;
      Fifo    : Rx_Fifo_Nr;
      Message : out Rx_Fifo_Message;
      Success : out Boolean;
      Timeout : Time_Span := Default_Timeout);

   function Is_Rx_Fifo_Msg_Lost
     (This : CAN_Controller;
      Fifo : Rx_Fifo_Nr) return Boolean;

   function Is_Rx_Fifo_Full
     (This : CAN_Controller;
      Fifo : Rx_Fifo_Nr) return Boolean;

   function Get_Rx_Fifo_Get_Index
     (This : CAN_Controller;
      Fifo : Rx_Fifo_Nr) return UInt2;
   --  Rx FIFO read (Get) index pointer for the next message to read, range
   --  0 to 2.

   function Get_Rx_Fifo_Put_Index
     (This : CAN_Controller;
      Fifo : Rx_Fifo_Nr) return UInt2;
   --  Rx FIFO 0 write index pointer for the next message to receive, range
   --  0 to 2.

   ---------------------
   --  Tx Event FIFO  --
   ---------------------

   subtype Tx_Event_Bank_Nr is Positive range 1 .. 3;
   --  Tx event FIFO element number.

   function Get_Tx_Event_Fill_Level
     (This : CAN_Controller) return UInt2;
   --  Number of elements stored in Tx event FIFO, range 0 to 3.

   type Tx_Event_Message is record
      ESI       : Boolean; --  Error State Indicator
      XTD       : Boolean; --  Extended Identifier
      RTR       : Boolean; --  Remote Transmission Request
      ID        : CAN_Extended_Id; --  Identifier
      MM        : HAL.UInt8; --  Message Marker
      ET        : HAL.UInt2; --  Event Type
      EDL       : Boolean; --  Extended Data Length (FD Format)
      BRS       : Boolean; --  Bit Rate Switching
      DLC       : DLC_Range; --  Data Length Code
      TXTS      : Natural; --  Tx TimeStamp
   end record;

   function Read_Tx_Event_Message
     (This : CAN_Controller;
      Bank : Tx_Event_Bank_Nr) return Tx_Event_Message
   with Pre => Get_Tx_Event_Fill_Level (This) /= 0;

   type High_Prio_Message_Status is record
      Filter_List     : Boolean;
      Filter_Index    : Natural;
      Message_Storage : UInt2;
      Message_Index   : Natural;
   end record;

   function Get_High_Prio_Message_Status
     (This : CAN_Controller) return High_Prio_Message_Status;
   --  Get high priority message status.

   function Is_Tx_Event_Msg_Lost
     (This : CAN_Controller) return Boolean;
   --  Tx event FIFO element lost, also set after write attempt to Tx event
   --  FIFO of size 0. This bit is a copy of interrupt flag IR[TEFL]. When
   --  IR[TEFL] is reset, this bit is also reset.

   function Is_Tx_Event_Full
     (This : CAN_Controller) return Boolean;

   function Get_Tx_Event_Put_Index
     (This : CAN_Controller) return UInt2;
   --  Tx event FIFO write index pointer, range 0 to 3.

   function Get_Tx_Event_Get_Index
     (This : CAN_Controller) return UInt2;
   --  Tx event FIFO read index pointer, range 0 to 3.

   ------------------------------
   --  Tx Buffer (FIFO/Queue)  --
   ------------------------------

   type Tx_Buffer_Mode is (FIFO, Queue);

   procedure Set_Tx_Buffer_Mode
     (This : in out CAN_Controller;
      Mode : Tx_Buffer_Mode)
     with Pre => Is_Init_Mode (This);
   --  In FIFO mode, messages stored in the Tx FIFO are transmitted starting
   --  with the message referenced by the Get Index TXFQS[TFGI]. In Queue mode,
   --  messages stored in the Tx queue are transmitted starting with the message
   --  with the lowest Message ID (highest priority).

   subtype Tx_Buffer_Bank_Nr is Positive range 1 .. 3;
   --  Tx FIFO/Queue element number.

   function Is_Tx_Buffer_Full
     (This : CAN_Controller) return Boolean;

   type Tx_Buffer_Message is record
      ESI       : Boolean; --  Error State Indicator
      XTD       : Boolean; --  Extended Identifier
      RTR       : Boolean; --  Remote Transmission Request
      ID        : CAN_Extended_Id; --  Identifier
      MM        : HAL.UInt8; --  Message Marker
      EFC       : Boolean; --  Event FIFO Control
      FDF       : Boolean; --  FD Format
      BRS       : Boolean; --  Bit Rate Switching
      DLC       : DLC_Range; --  Data Length Code
      RXTS      : Natural; --  Rx TimeStamp
      Data      : Message_Data (0 .. 63) := (others => 0); --  Up to 64 Data Bytes
   end record;

   procedure Write_Tx_FifoQ_Message
     (This     : in out CAN_Controller;
      Message  : Tx_Buffer_Message;
      Bank     : out Tx_Buffer_Bank_Nr;
      Transmit : Boolean)
   with Pre => not Is_Tx_Buffer_Full (This);
   --  Write Tx message to Tx Buffer FIFO/Queue, save the bank used and
   --  choose to activate transmit request.

   procedure Transmission_Request
     (This : in out CAN_Controller;
      Bank : Tx_Buffer_Bank_Nr);

   function Is_Tx_Request_Pending
     (This : CAN_Controller;
      Bank : Tx_Buffer_Bank_Nr) return Boolean;
   --  The bits are set via register TXBAR. The bits are reset after a
   --  requested transmission has completed or has been canceled via register
   --  TXBCR.

   function Transmission_Successful
     (This : CAN_Controller;
      Bank : Tx_Buffer_Bank_Nr) return Boolean;
   --  The bits are set when the corresponding TXBRP bit is cleared after a
   --  successful transmission. The bits are reset when a new transmission is
   --  requested by writing a 1 to the corresponding bit of register TXBAR.

   procedure Abort_Tx_Buffer_Request
     (This : in out CAN_Controller;
      Bank : Tx_Buffer_Bank_Nr);
   --  When a transmission request for the Tx buffer referenced by the Get Index
   --  is canceled, the Get Index is incremented to the next Tx buffer with
   --  pending transmission request and the Tx FIFO Free Level is recalculated.
   --  When transmission cancellation is applied to any other Tx buffer, the
   --  Get Index and the FIFO Free Level remain unchanged.

   function Abort_Tx_Buffer_Finished
     (This : CAN_Controller;
      Bank : Tx_Buffer_Bank_Nr) return Boolean;
   --  The Cancelation Finished bit are set when the corresponding TXBRP bit is
   --  cleared after a cancellation was requested via TXBCR.

   function Get_Tx_Buffer_Put_Index
     (This : CAN_Controller) return UInt2
     with Inline;
   --  Tx FIFO/queue write index pointer for the next free (empty) bank
   --  to write, range 0 to 3.

   function Get_Tx_Buffer_Get_Index
     (This : CAN_Controller) return UInt2
     with Inline;
   --  Tx FIFO read index pointer for the next message to transmit, range 0 to 3.
   --  Read as 0 when Tx queue operation is configured (TXBC.TFQM = 1).

   function Get_Tx_Fifo_Free_Level
     (This : CAN_Controller) return UInt2;
   --  Number of consecutive free Tx FIFO elements starting from TFGI, range
   --  0 to 3. Read as 0 when Tx queue operation is configured (TXBC[TFQM] = 1).

   procedure Transmit_Message
     (This    : in out CAN_Controller;
      Message : Tx_Buffer_Message;
      Success : out Boolean;
      Timeout : Time_Span := Default_Timeout);

   ----------------
   -- Interrupts --
   ----------------

   procedure Set_Tx_Buffer_Interrupt
     (This    : in out CAN_Controller;
      Bank    : Tx_Buffer_Bank_Nr;
      Enabled : Boolean);
   --  Each Tx Buffer has its own Transmission Completed interrupt to set TC
   --  flag in IR register, but interrupt will only occur if TC is enabled in
   --  IE register.

   procedure Set_Tx_Buffer_Abort_Interrupt
     (This    : in out CAN_Controller;
      Bank    : Tx_Buffer_Bank_Nr;
      Enabled : Boolean);
   --  Each Tx Buffer has its own Transmission Cancellation Finished interrupt
   --  to set TCF flag in IR register, but interrupt will only occur if TCF is
   --  enabled in IE register.

   type CAN_Interrupt is
     (Rx_FIFO_0_Msg_Pending,
      Rx_FIFO_0_Full,
      Rx_FIFO_0_Msg_Lost,
      Rx_FIFO_1_Msg_Pending,
      Rx_FIFO_1_Full,
      Rx_FIFO_1_Msg_Lost,
      High_Prio_Msg_Received,
      Transmission_Completed,
      Transmission_Abort_Finished,
      Tx_FIFO_Empty,
      Tx_Event_Fifo_Pending,
      Tx_Event_Fifo_Full,
      Tx_Event_Fifo_Msg_Lost,
      Timestanp_Wraparound,
      Rx_Msg_RAM_Access_Failure,
      Timeout_Occurred,
      Error_Logging_Overflow,
      Error_Passive,
      Error_Warning,
      Bus_Off,
      Msg_RAM_Watchdog,
      Error_Arbitration_Phase,
      Error_Data_Phase,
      Access_Reserved_Address);

   function Interrupt_Enabled
     (This   : CAN_Controller;
      Source : CAN_Interrupt) return Boolean
     with Inline;

   procedure Enable_Interrupts
     (This   : in out CAN_Controller;
      Source : CAN_Interrupt)
     with Post => Interrupt_Enabled (This, Source),
          Inline;

   procedure Disable_Interrupts
     (This   : in out CAN_Controller;
      Source : CAN_Interrupt)
     with Post => not Interrupt_Enabled (This, Source),
          Inline;

   function Status
     (This   : CAN_Controller;
      Source : CAN_Interrupt) return Boolean
     with Inline;
   --  The flags are set when one of the listed conditions is detected
   --  (edge-sensitive). The flags remain set until the Host clears them. A
   --  flag is cleared by writing a 1 to the corresponding bit position.

   procedure Clear_Pending_Interrupt
     (This   : in out CAN_Controller;
      Source : CAN_Interrupt)
     with Post => not Status (This, Source),
          Inline;

   type Interrupt_Line is (Line_0, Line_1);

   function Interrupt_Line_Enabled
     (This   : CAN_Controller;
      Source : Interrupt_Line) return Boolean
     with Inline;

   procedure Set_Interrupt_Line
     (This    : in out CAN_Controller;
      Source  : Interrupt_Line;
      Enabled : Boolean)
     with Post => Interrupt_Line_Enabled (This, Source) = Enabled,
          Inline;
   --  Enable/disable module interrupt lines 1 or 2.

   type Interrupt_Group is
     (Rx_FIFO_0,
      Rx_FIFO_1,
      Status_Msg,
      Tx_FIFO_Error,
      Time_RAM,
      Passive_Log_Error,
      Protocol_Error);
   --  Group              Interrupts
   --  Rx_FIFO_0          RF0LL, RF0FL, RF0NL
   --  Rx_FIFO_1          RF1LL, RF1FL, RF1NL
   --  Status_Msg         TCFL, TCL, HPML
   --  Tx_FIFO_Error      TEFLL, TEFFL, TEFNL, TFEL
   --  Time_RAM           TOOL, MRAFL, TSWL
   --  Passive_Error_Log  EPL, ELOL
   --  Protocol_Error     ARAL, PEDL, PEAL, WDIL, BOL, EWL

   procedure Set_Group_Interrupt_Line
     (This    : in out CAN_Controller;
      Group   : Interrupt_Group;
      Source  : Interrupt_Line)
     with Inline;
   --  Assigns an interrupt generated by a specific group of interrupt flag
   --  from the Interrupt register to one of the two module interrupt lines.
   --  For interrupt generation the respective interrupt line has to be enabled
   --  via ILE[EINT0] and ILE[EINT1].

   function Get_Group_Interrupt_Line
     (This    : CAN_Controller;
      Group   : Interrupt_Group) return Interrupt_Line
     with Inline;

   type Last_Error_Code_Enum is
     (No_Error,
      Rx_Msg_Stuff,
      Rx_Wrong_Format,
      Tx_No_Ack,
      Tx_Bit_1,
      Tx_Bit_0,
      CRC_Check,
      No_Change_Status)
     with Size => 3;

   type Communication_Activity_Enum is
     (Synchronizing,
      Idle,
      Receiver,
      Transmitter)
     with Size => 2;

   type Protocol_Status is record
      Last_Error_Code        : Last_Error_Code_Enum;
      Communication_Activity : Communication_Activity_Enum;
      Error_Passive          : Boolean;
      Error_Counter_Warning  : Boolean;
      Bus_Off                : Boolean;
      Data_Last_Error_Code   : Last_Error_Code_Enum;
      Rx_ESI_Flag            : Boolean;
      Rx_BRS_Flag            : Boolean;
      Rx_FDF_Flag            : Boolean;
      Protocol_Exception     : Boolean;
      Tx_Delay_Compensation  : UInt7;
   end record;

   procedure Get_Protocol_Status
     (This   : CAN_Controller;
      Status : out Protocol_Status);

   type Error_Counter is record
      Tx_Error_Counter : UInt8;
      Rx_Error_Counter : UInt7;
      Rx_Error_Passive : Boolean;
      Error_Logging    : UInt8;
   end record;

   function Get_Error_Counter
     (This : CAN_Controller) return Error_Counter;

private

   type CAN_Controller is new STM32_SVD.FDCAN.FDCAN_Peripheral;

   procedure Set_Init_Mode
     (This    : in out CAN_Controller;
      Enabled : Boolean);

   procedure Set_Sleep_Mode
     (This    : in out CAN_Controller;
      Enabled : Boolean);

   -------------------------
   --  FDCAN Message RAM  --
   -------------------------

   --  The Message RAM has a width of 32 bits, and the FDCAN module is
   --  configured to allocate up to 212 words in it. When the FDCAN addresses
   --  the Message RAM, it addresses 32-bit words (aligned), not a single byte.
   --  The RAM addresses are 32-bit words, i.e. only bits 15 to 2 are evaluated,
   --  the two least significant bits are ignored. See RM0440 rev 6 chapter
   --  44.3.3 Message RAM figure 669 Message RAM configuration.
   --  The start address of Message SRAM is defined by FDCAN_Base (16#4000A400#).

   subtype Message_RAM_Offset is Natural range 0 .. 2559;
   --  Map each word of the 2560 words (10240 bytes) from Message RAM.
   type Message_Ram_Array is array (Message_RAM_Offset) of UInt32
     with Component_Size => 32, Size => 2560 * 32;
   --  Associate a real address to the Message RAM.
   Message_Ram : Message_Ram_Array
     with Address => STM32_SVD.FDCAN_Base;

   ----------------------------------
   -- FDCAN Message RAM Structures --
   ----------------------------------

   Filter_Std_Elmt_Size : constant Positive := 1 * 4;
   --  Filter standard element size in bytes.
   FLSSA : constant Natural := 0;
   --  Filter list standard start address (relative to FDCAN_Base).

   Filter_Ext_Elmt_Size : constant Positive := 2 * 4;
   --  Filter extended element size in bytes.
   FLESA : constant Natural := FLSSA + Filter_Std_Elmt_Size * Filter_Bank_Std_Nr'Last;
   --  Filter list extended start address.

   Rx_Fifo_Elmt_Size : constant Positive := 18 * 4;
   --  Rx FIFO element size in bytes.
   RF0SA : constant Natural := FLESA + Filter_Ext_Elmt_Size * Filter_Bank_Ext_Nr'Last;
   --  Rx FIFO 0 start address.
   RF1SA : constant Natural := RF0SA + Rx_Fifo_Elmt_Size * Rx_Fifo_Bank_Nr'Last;
   --  Rx FIFO 1 start address.

   Tx_Event_Elmt_Size : constant Positive := 2 * 4;
   --  Tx event FIFO element size in bytes.
   TEFSA : constant Natural := RF1SA + Rx_Fifo_Elmt_Size * Rx_Fifo_Bank_Nr'Last;
   --  Tx event FIFO start address.

   Tx_Buffer_Elmt_Size : constant Positive := 18 * 4;
   --  Tx FIFO/Queue element size in bytes.
   TFQSA : constant Natural := TEFSA + Tx_Event_Elmt_Size * Tx_Event_Bank_Nr'Last;
   --  Tx FIFO/Queue start address.

   Message_Ram_Size : constant Natural := TFQSA + Tx_Buffer_Elmt_Size * Tx_Buffer_Bank_Nr'Last;
   --  This gives the total available SRAM of 212 words.

   type Message_Ram_SA is record
      Std_Filter : Natural;
      Ext_Filter : Natural;
      Rx_FIFO_0  : Natural;
      Rx_FIFO_1  : Natural;
      Tx_Event   : Natural;
      Tx_Buffer  : Natural;
   end record;
   --  Message RAM block start addresses used to calculate the initial addresses
   --  for standard filter, extended filter, Rx FIFO 0, Rx FIFO 1, Tx event FIFO
   --  and Tx FIFO/Queue.

   function Calculate_Ram_Block_Addresses
     (This : CAN_Controller) return Message_Ram_SA;
   --  Calculate RAM block start addresses for each CAN instance.

   type Filter_Std_Element is record
      SFT       : HAL.UInt2;
      SFEC      : HAL.UInt3;
      SFID1     : HAL.UInt11;
      Reserved0 : HAL.UInt5;
      SFID2     : HAL.UInt11;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   --  Representation for the standard message ID filter element. See RM0440
   --  rev 6 chapter 44.3.8 FDCAN Standard message ID filter element.

   for Filter_Std_Element use record
      SFT       at 0 range 30 .. 31;
      SFEC      at 0 range 27 .. 29;
      SFID1     at 0 range 16 .. 26;
      Reserved0 at 0 range 11 .. 15;
      SFID2     at 0 range  0 .. 10;
   end record;

   type Filter_Ext_Element is record
      EFEC      : HAL.UInt3;
      EFID1     : HAL.UInt29;
      EFT       : HAL.UInt2;
      Reserved0 : HAL.Bit;
      EFID2     : HAL.UInt29;
   end record with Size => 64, Bit_Order => System.Low_Order_First;
   --  Representation for the extended message ID filter element. See RM0440
   --  rev 6 chapter 44.3.9 FDCAN Extended message ID filter element.

   for Filter_Ext_Element use record
      EFEC      at 0 range 29 .. 31;
      EFID1     at 0 range  0 .. 28;
      EFT       at 4 range 30 .. 31;
      Reserved0 at 4 range 29 .. 29;
      EFID2     at 4 range  0 .. 28;
   end record;

   --  For storage of CAN FD messages, the element size is configured to set 64
   --  bytes data field. Each four data bytes are packed into a word, so the
   --  message storage contains 16 words.

   --  Type definition for Data Word Array
   type DW_Field_Array is array (0 .. 3) of HAL.UInt8
     with Component_Size => 8, Size => 32;

   --  Type definition for Data Word Field
   type DW_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DW as a value
            DB3 : HAL.UInt8;
            DB2 : HAL.UInt8;
            DB1 : HAL.UInt8;
            DB0 : HAL.UInt8;
         when True =>
            --  DW as an array
            Arr : DW_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Bit_Order => System.Low_Order_First;
   --  Representation for the word containing 4 data bytes.

   for DW_Field use record
      DB3      at 0 range 24 .. 31;
      DB2      at 0 range 16 .. 23;
      DB1      at 0 range  8 .. 15;
      DB0      at 0 range  0 .. 7;
      Arr      at 0 range  0 .. 31;
   end record;

   --  Type definition for Data Array
   type Data_Field_Array is array (0 .. 15) of DW_Field
     with Component_Size => 32, Size => 16 * 32;

   --  Type definition for Data Field
   type Data_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  Data as a value
            DW0  : DW_Field;
            DW1  : DW_Field;
            DW2  : DW_Field;
            DW3  : DW_Field;
            DW4  : DW_Field;
            DW5  : DW_Field;
            DW6  : DW_Field;
            DW7  : DW_Field;
            DW8  : DW_Field;
            DW9  : DW_Field;
            DW10 : DW_Field;
            DW11 : DW_Field;
            DW12 : DW_Field;
            DW13 : DW_Field;
            DW14 : DW_Field;
            DW15 : DW_Field;
         when True =>
            --  Data as an array
            Arr  : Data_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16 * 32, Bit_Order => System.Low_Order_First;
   --  Representation for the data containing 16 data words.

   for Data_Field use record
      DW0       at 16#00# range  0 .. 31;
      DW1       at 16#04# range  0 .. 31;
      DW2       at 16#08# range  0 .. 31;
      DW3       at 16#0C# range  0 .. 31;
      DW4       at 16#10# range  0 .. 31;
      DW5       at 16#14# range  0 .. 31;
      DW6       at 16#18# range  0 .. 31;
      DW7       at 16#1C# range  0 .. 31;
      DW8       at 16#20# range  0 .. 31;
      DW9       at 16#24# range  0 .. 31;
      DW10      at 16#28# range  0 .. 31;
      DW11      at 16#2C# range  0 .. 31;
      DW12      at 16#30# range  0 .. 31;
      DW13      at 16#34# range  0 .. 31;
      DW14      at 16#38# range  0 .. 31;
      DW15      at 16#3C# range  0 .. 31;
      Arr       at 16#00# range  0 .. 16 * 32 - 1;
   end record;

   type Rx_Buffer_Element is record
      ESI       : Boolean; --  Error State Indicator
      XTD       : Boolean; --  Extended Identifier
      RTR       : Boolean; --  Remote Transmission Request
      ID        : CAN_Extended_Id; --  Identifier
      ANMF      : Boolean; --  Accepted Non-Matching Frame
      FIDX      : HAL.UInt7; --  Filter Index
      Reserved0 : HAL.UInt2;
      FDF       : Boolean; --  FD Format
      BRS       : Boolean; --  Bit Rate Switch
      DLC       : HAL.UInt4; --  Data Length Code
      RXTS      : HAL.UInt16; --  Rx Timestamp
      Data      : Data_Field;
   end record with Size => 18 * 32, Bit_Order => System.Low_Order_First;
   --  Representation for the Rx FIFO element. See RM0440 rev 6 chapter 44.3.5
   --  FDCAN Rx FIFO element.

   for Rx_Buffer_Element use record
      ESI       at 16#00# range 31 .. 31;
      XTD       at 16#00# range 30 .. 30;
      RTR       at 16#00# range 29 .. 29;
      ID        at 16#00# range  0 .. 28;
      ANMF      at 16#04# range 31 .. 31;
      FIDX      at 16#04# range 24 .. 30;
      Reserved0 at 16#04# range 22 .. 23;
      FDF       at 16#04# range 21 .. 21;
      BRS       at 16#04# range 20 .. 20;
      DLC       at 16#04# range 16 .. 19;
      RXTS      at 16#04# range  0 .. 15;
      Data      at 16#08# range  0 .. 16 * 32 - 1;
   end record;

   type Tx_Event_Element is record
      ESI       : Boolean; --  Error State Indicator
      XTD       : Boolean; --  Extended Identifier
      RTR       : Boolean; --  Remote Transmission Request
      ID        : CAN_Extended_Id; --  Identifier
      MM        : HAL.UInt8; --  Message Marker
      ET        : HAL.UInt2; --  Event Type
      EDL       : Boolean; --  Extended Data Length
      BRS       : Boolean; --  Bit Rate Switch
      DLC       : HAL.UInt4; --  Data Length Code
      TXTS      : HAL.UInt16; --  Tx Timestamp
   end record with Size => 64, Bit_Order => System.Low_Order_First;
   --  Representation for the Tx event FIFO element. See RM0440 rev 6 chapter
   --  44.3.7 FDCAN Tx event FIFO element.

   for Tx_Event_Element use record
      ESI       at 0 range 31 .. 31;
      XTD       at 0 range 30 .. 30;
      RTR       at 0 range 29 .. 29;
      ID        at 0 range  0 .. 28;
      MM        at 4 range 24 .. 31;
      ET        at 4 range 22 .. 23;
      EDL       at 4 range 21 .. 21;
      BRS       at 4 range 20 .. 20;
      DLC       at 4 range 16 .. 19;
      TXTS      at 4 range  0 .. 15;
   end record;

   type Tx_Buffer_Element is record
      ESI       : Boolean; --  Error State Indicator
      XTD       : Boolean; --  Extended Identifier
      RTR       : Boolean; --  Remote Transmission Request
      ID        : CAN_Extended_Id; --  Identifier
      MM        : HAL.UInt8; --  Message Marker
      EFC       : Boolean; --  Event FIFO Control
      Reserved0 : Boolean;
      FDF       : Boolean; --  FD Format
      BRS       : Boolean; --  Bit Rate Switch
      DLC       : HAL.UInt4; --  Data Length Code
      Reserved1 : HAL.UInt16;
      Data      : Data_Field;
   end record with Size => 18 * 32, Bit_Order => System.Low_Order_First;
   --  Representation for the Tx buffer (FIFO/queue) element. See RM0440 rev 6
   --  chapter 44.3.6 FDCAN Tx buffer element.

   for Tx_Buffer_Element use record
      ESI       at 16#00# range 31 .. 31;
      XTD       at 16#00# range 30 .. 30;
      RTR       at 16#00# range 29 .. 29;
      ID        at 16#00# range  0 .. 28;
      MM        at 16#04# range 24 .. 31;
      EFC       at 16#04# range 23 .. 23;
      Reserved0 at 16#04# range 22 .. 22;
      FDF       at 16#04# range 21 .. 21;
      BRS       at 16#04# range 20 .. 20;
      DLC       at 16#04# range 16 .. 19;
      Reserved1 at 16#04# range  0 .. 15;
      Data      at 16#08# range  0 .. 16 * 32 - 1;
   end record;

end STM32.CAN;
