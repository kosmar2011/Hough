
//------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/ccs_in_wait_v1.v 
//------------------------------------------------------------------------------
// Catapult Synthesis - Sample I/O Port Library
//
// Copyright (c) 2003-2017 Mentor Graphics Corp.
//       All Rights Reserved
//
// This document may be used and distributed without restriction provided that
// this copyright statement is not removed from the file and that any derivative
// work contains this copyright notice.
//
// The design information contained in this file is intended to be an example
// of the functionality which the end user may study in preparation for creating
// their own custom interfaces. This design does not necessarily present a 
// complete implementation of the named protocol or standard.
//
//------------------------------------------------------------------------------


module ccs_in_wait_v1 (idat, rdy, ivld, dat, irdy, vld);

  parameter integer rscid = 1;
  parameter integer width = 8;

  output [width-1:0] idat;
  output             rdy;
  output             ivld;
  input  [width-1:0] dat;
  input              irdy;
  input              vld;

  wire   [width-1:0] idat;
  wire               rdy;
  wire               ivld;

  assign idat = dat;
  assign rdy = irdy;
  assign ivld = vld;

endmodule


//------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/ccs_out_wait_v1.v 
//------------------------------------------------------------------------------
// Catapult Synthesis - Sample I/O Port Library
//
// Copyright (c) 2003-2017 Mentor Graphics Corp.
//       All Rights Reserved
//
// This document may be used and distributed without restriction provided that
// this copyright statement is not removed from the file and that any derivative
// work contains this copyright notice.
//
// The design information contained in this file is intended to be an example
// of the functionality which the end user may study in preparation for creating
// their own custom interfaces. This design does not necessarily present a 
// complete implementation of the named protocol or standard.
//
//------------------------------------------------------------------------------


module ccs_out_wait_v1 (dat, irdy, vld, idat, rdy, ivld);

  parameter integer rscid = 1;
  parameter integer width = 8;

  output [width-1:0] dat;
  output             irdy;
  output             vld;
  input  [width-1:0] idat;
  input              rdy;
  input              ivld;

  wire   [width-1:0] dat;
  wire               irdy;
  wire               vld;

  assign dat = idat;
  assign irdy = rdy;
  assign vld = ivld;

endmodule



//------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/mgc_io_sync_v2.v 
//------------------------------------------------------------------------------
// Catapult Synthesis - Sample I/O Port Library
//
// Copyright (c) 2003-2017 Mentor Graphics Corp.
//       All Rights Reserved
//
// This document may be used and distributed without restriction provided that
// this copyright statement is not removed from the file and that any derivative
// work contains this copyright notice.
//
// The design information contained in this file is intended to be an example
// of the functionality which the end user may study in preparation for creating
// their own custom interfaces. This design does not necessarily present a 
// complete implementation of the named protocol or standard.
//
//------------------------------------------------------------------------------


module mgc_io_sync_v2 (ld, lz);
    parameter valid = 0;

    input  ld;
    output lz;

    wire   lz;

    assign lz = ld;

endmodule


//------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/mgc_shift_r_beh_v5.v 
module mgc_shift_r_v5(a,s,z);
   parameter    width_a = 4;
   parameter    signd_a = 1;
   parameter    width_s = 2;
   parameter    width_z = 8;

   input [width_a-1:0] a;
   input [width_s-1:0] s;
   output [width_z -1:0] z;

   generate
     if (signd_a)
     begin: SGNED
       assign z = fshr_u(a,s,a[width_a-1]);
     end
     else
     begin: UNSGNED
       assign z = fshr_u(a,s,1'b0);
     end
   endgenerate

   //Shift right - unsigned shift argument
   function [width_z-1:0] fshr_u;
      input [width_a-1:0] arg1;
      input [width_s-1:0] arg2;
      input sbit;
      parameter olen = width_z;
      parameter ilen = signd_a ? width_a : width_a+1;
      parameter len = (ilen >= olen) ? ilen : olen;
      reg signed [len-1:0] result;
      reg signed [len-1:0] result_t;
      begin
        result_t = $signed( {(len){sbit}} );
        result_t[width_a-1:0] = arg1;
        result = result_t >>> arg2;
        fshr_u =  result[olen-1:0];
      end
   endfunction // fshl_u

endmodule

//------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/ccs_in_v1.v 
//------------------------------------------------------------------------------
// Catapult Synthesis - Sample I/O Port Library
//
// Copyright (c) 2003-2017 Mentor Graphics Corp.
//       All Rights Reserved
//
// This document may be used and distributed without restriction provided that
// this copyright statement is not removed from the file and that any derivative
// work contains this copyright notice.
//
// The design information contained in this file is intended to be an example
// of the functionality which the end user may study in preparation for creating
// their own custom interfaces. This design does not necessarily present a 
// complete implementation of the named protocol or standard.
//
//------------------------------------------------------------------------------


module ccs_in_v1 (idat, dat);

  parameter integer rscid = 1;
  parameter integer width = 8;

  output [width-1:0] idat;
  input  [width-1:0] dat;

  wire   [width-1:0] idat;

  assign idat = dat;

endmodule


//------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/ram_sync_dualRW_be_generic.v 
//------------------------------------------------------------------------------
// Catapult Synthesis - Sample I/O Port Library
//
// Copyright (c) 2003-2015 Mentor Graphics Corp.
//       All Rights Reserved
//
// This document may be used and distributed without restriction provided that
// this copyright statement is not removed from the file and that any derivative
// work contains this copyright notice.
//
// The design information contained in this file is intended to be an example
// of the functionality which the end user may study in preparation for creating
// their own custom interfaces. This design does not necessarily present a 
// complete implementation of the named protocol or standard.
//
//------------------------------------------------------------------------------

module ram_sync_dualRW_be ( data_in, addr, re, we, data_out, clk, a_rst, s_rst, en);

  parameter ram_id = 1;
  parameter words = 'd16;
  parameter width = 'd16;
  parameter addr_width = 4;
  parameter [0:0] a_reset_active = 1;
  parameter [0:0] s_reset_active = 1;
  parameter [0:0] enable_active = 1;
  parameter [0:0] re_active = 1;
  parameter [0:0] we_active = 1;
  parameter num_byte_enables = 1;
  parameter [0:0] clock_edge = 1;
  parameter no_of_RAM_dualRW_readwrite_port = 2;

  localparam byte_width = width / num_byte_enables;

  input [(width*no_of_RAM_dualRW_readwrite_port)-1:0] data_in;
  input [(addr_width*no_of_RAM_dualRW_readwrite_port)-1:0] addr;
  input [(num_byte_enables*no_of_RAM_dualRW_readwrite_port)-1:0] re;
  input [(num_byte_enables*no_of_RAM_dualRW_readwrite_port)-1:0] we;
  output [(width*no_of_RAM_dualRW_readwrite_port)-1:0] data_out;
  input clk;
  input a_rst;
  input s_rst;
  input en;

  // synopsys translate_off
  reg  [width-1:0] mem [words-1:0];
  
  wire [num_byte_enables-1:0] reA;
  wire [num_byte_enables-1:0] reB;
  wire [num_byte_enables-1:0] weA;
  wire [num_byte_enables-1:0] weB;

  wire [width-1:0] data_inA;
  wire [width-1:0] data_inB;
  reg [width-1:0] data_outA;
  reg [width-1:0] data_outB;
  wire [addr_width-1:0] addrA;
  wire [addr_width-1:0] addrB;

  integer count;
  initial
  begin
    for (count = 0; count < words; count = count + 1) 
      mem[count] = 0;
  end

  integer i;
  generate
    if ( clock_edge == 1'b1 )
    begin: POSEDGE_BLK
      always @(posedge clk)
      begin
        if ( en == enable_active )
        begin
          for (i = 0; i < num_byte_enables; i = i + 1)
          begin
            if ( reA[i] == re_active )
              data_outA[i*byte_width+: byte_width] <= mem[addrA][i*byte_width+: byte_width];
            else
              data_outA[i*byte_width+: byte_width] <= {(byte_width){1'bX}};
            if ( reB[i] == re_active )
              data_outB[i*byte_width+: byte_width] <= mem[addrB][i*byte_width+: byte_width];
            else
              data_outB[i*byte_width+: byte_width] <= {(byte_width){1'bX}};
            if (weA[i] == we_active)
              mem[addrA][i*byte_width+:byte_width] <= data_inA[i*byte_width+:byte_width];
            if (weB[i] == we_active)
              mem[addrB][i*byte_width+:byte_width] <= data_inB[i*byte_width+:byte_width];
          end
        end
      end
    end else
    begin: NEGEDGE_BLK
      always @(negedge clk)
      begin
        if ( en == enable_active )
        begin
          for (i = 0; i < num_byte_enables; i = i + 1)
          begin
            if ( reA[i] == re_active )
              data_outA[i*byte_width+: byte_width] <= mem[addrA][i*byte_width+: byte_width];
            else
              data_outA[i*byte_width+: byte_width] <= {(byte_width){1'bX}};
            if ( reB[i] == re_active )
              data_outB[i*byte_width+: byte_width] <= mem[addrB][i*byte_width+: byte_width];
            else
              data_outB[i*byte_width+: byte_width] <= {(byte_width){1'bX}};
            if (weA[i] == we_active)
              mem[addrA][i*byte_width+:byte_width] <= data_inA[i*byte_width+:byte_width];
            if (weB[i] == we_active)
              mem[addrB][i*byte_width+:byte_width] <= data_inB[i*byte_width+:byte_width];
          end
        end
      end
    end
  endgenerate

  assign reA = re[1*num_byte_enables-1:0*num_byte_enables];
  assign reB = re[2*num_byte_enables-1:1*num_byte_enables];
  assign weA = we[1*num_byte_enables-1:0*num_byte_enables];
  assign weB = we[2*num_byte_enables-1:1*num_byte_enables];

  assign addrA = addr[addr_width-1:0];
  assign addrB = addr[(2*addr_width)-1:addr_width];
  assign data_inA = data_in[width-1:0];
  assign data_inB = data_in[(2*width)-1:width];

  assign data_out = {data_outB,data_outA};

  // synopsys translate_on
endmodule

module ram_sync_dualRW_be_port ( data_in_d, addr_d, re_d, we_d, data_out_d, data_in, addr, re, we, data_out, clk, a_rst, s_rst, en);

  parameter ram_id = 1;
  parameter words = 16;
  parameter width = 16;
  parameter addr_width = 4;
  parameter [0:0] a_reset_active = 1;
  parameter [0:0] s_reset_active = 1;
  parameter [0:0] enable_active = 1;
  parameter [0:0] re_active = 1;
  parameter [0:0] we_active = 1;
  parameter num_byte_enables = 1;
  parameter [0:0] clock_edge = 1;
  parameter no_of_RAM_dualRW_readwrite_port = 2;

  input [(width*no_of_RAM_dualRW_readwrite_port)-1:0] data_in_d;
  input [(addr_width*no_of_RAM_dualRW_readwrite_port)-1:0] addr_d;
  input [(num_byte_enables*no_of_RAM_dualRW_readwrite_port)-1:0] re_d;
  input [(num_byte_enables*no_of_RAM_dualRW_readwrite_port)-1:0] we_d;
  output [(width*no_of_RAM_dualRW_readwrite_port)-1:0] data_out_d;

  output [(width*no_of_RAM_dualRW_readwrite_port)-1:0] data_in;
  output [(addr_width*no_of_RAM_dualRW_readwrite_port)-1:0] addr;
  output [(num_byte_enables*no_of_RAM_dualRW_readwrite_port)-1:0] re;
  output [(num_byte_enables*no_of_RAM_dualRW_readwrite_port)-1:0] we;
  input [(width*no_of_RAM_dualRW_readwrite_port)-1:0] data_out;

  input clk;
  input a_rst;
  input s_rst;
  input en;

  assign data_in    = data_in_d;
  assign addr       = addr_d;
  assign re         = re_d;
  assign we         = we_d;
  assign data_out_d = data_out;

endmodule

//------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/ccs_genreg_v1.v 
//------------------------------------------------------------------------------
// Catapult Synthesis - Sample I/O Port Library
//
// Copyright (c) 2003-2017 Mentor Graphics Corp.
//       All Rights Reserved
//
// This document may be used and distributed without restriction provided that
// this copyright statement is not removed from the file and that any derivative
// work contains this copyright notice.
//
// The design information contained in this file is intended to be an example
// of the functionality which the end user may study in preparation for creating
// their own custom interfaces. This design does not necessarily present a 
// complete implementation of the named protocol or standard.
//
//------------------------------------------------------------------------------

module ccs_genreg_v1 (clk, en, arst, srst, d, z);
    parameter integer width   = 1;
    parameter integer ph_clk  = 1;
    parameter integer ph_en   = 1;
    parameter integer ph_arst = 0;
    parameter integer ph_srst = 1;
    parameter         has_en  = 1'b1;

    input clk;
    input en;
    input arst;
    input srst;
    input      [width-1:0] d;
    output reg [width-1:0] z;

    //  Generate parameters
    //  ph_clk | ph_arst | has_en     Label:
    //    1        1          1       GEN_CLK1_ARST1_EN1
    //    1        1          0       GEN_CLK1_ARST1_EN0
    //    1        0          1       GEN_CLK1_ARST0_EN1
    //    1        0          0       GEN_CLK1_ARST0_EN0
    //    0        1          1       GEN_CLK0_ARST1_EN1
    //    0        1          0       GEN_CLK0_ARST1_EN0
    //    0        0          1       GEN_CLK0_ARST0_EN1
    //    0        0          0       GEN_CLK0_ARST0_EN0
    
    generate 
      // Pos edge clock, pos edge async reset, has enable
      if (ph_clk == 1 & ph_arst == 1 & has_en == 1)
      begin: GEN_CLK1_ARST1_EN1
        always @(posedge clk or posedge arst)
          if (arst == 1'b1)
            z <= {width{1'b0}};
          else if (srst == $unsigned(ph_srst))
            z <= {width{1'b0}};
          else if (en == $unsigned(ph_en))
            z <= d;
      end  //GEN_CLK1_ARST1_EN1

      // Pos edge clock, pos edge async reset, no enable
      else if (ph_clk == 1 & ph_arst == 1 & has_en == 0)
      begin: GEN_CLK1_ARST1_EN0
        always @(posedge clk or posedge arst)
          if (arst == 1'b1)
            z <= {width{1'b0}};
          else if (srst == $unsigned(ph_srst))
            z <= {width{1'b0}};
          else
            z <= d;
      end  //GEN_CLK1_ARST1_EN0

      // Pos edge clock, neg edge async reset, has enable
      else if (ph_clk == 1 & ph_arst == 0 & has_en == 1)
      begin: GEN_CLK1_ARST0_EN1
        always @(posedge clk or negedge arst)
          if (arst == 1'b0)
            z <= {width{1'b0}};
          else if (srst == $unsigned(ph_srst))
            z <= {width{1'b0}};
          else if (en == $unsigned(ph_en))
            z <= d;
      end  //GEN_CLK1_ARST0_EN1

      // Pos edge clock, neg edge async reset, no enable
      else if (ph_clk == 1 & ph_arst == 0 & has_en == 0)
      begin: GEN_CLK1_ARST0_EN0
        always @(posedge clk or negedge arst)
          if (arst == 1'b0)
            z <= {width{1'b0}};
          else if (srst == $unsigned(ph_srst))
            z <= {width{1'b0}};
          else
            z <= d;
      end  //GEN_CLK1_ARST0_EN0


      // Neg edge clock, pos edge async reset, has enable
      if (ph_clk == 0 & ph_arst == 1 & has_en == 1)
      begin: GEN_CLK0_ARST1_EN1
        always @(negedge clk or posedge arst)
          if (arst == 1'b1)
            z <= {width{1'b0}};
          else if (srst == $unsigned(ph_srst))
            z <= {width{1'b0}};
          else if (en == $unsigned(ph_en))
            z <= d;
      end  //GEN_CLK0_ARST1_EN1

      // Neg edge clock, pos edge async reset, no enable
      else if (ph_clk == 0 & ph_arst == 1 & has_en == 0)
      begin: GEN_CLK0_ARST1_EN0
        always @(negedge clk or posedge arst)
          if (arst == 1'b1)
            z <= {width{1'b0}};
          else if (srst == $unsigned(ph_srst))
            z <= {width{1'b0}};
          else
            z <= d;
      end  //GEN_CLK0_ARST1_EN0

      // Neg edge clock, neg edge async reset, has enable
      else if (ph_clk == 0 & ph_arst == 0 & has_en == 1)
      begin: GEN_CLK0_ARST0_EN1
        always @(negedge clk or negedge arst)
          if (arst == 1'b0)
            z <= {width{1'b0}};
          else if (srst == $unsigned(ph_srst))
            z <= {width{1'b0}};
          else if (en == $unsigned(ph_en))
            z <= d;
      end  //GEN_CLK0_ARST0_EN1

      // Neg edge clock, neg edge async reset, no enable
      else if (ph_clk == 0 & ph_arst == 0 & has_en == 0)
      begin: GEN_CLK0_ARST0_EN0
        always @(negedge clk or negedge arst)
          if (arst == 1'b0)
            z <= {width{1'b0}};
          else if (srst == $unsigned(ph_srst))
            z <= {width{1'b0}};
          else
            z <= d;
      end  //GEN_CLK0_ARST0_EN0
    endgenerate
endmodule


//------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/ccs_fifo_wait_core_v5.v 
//------------------------------------------------------------------------------
// Catapult Synthesis - Sample I/O Port Library
//
// Copyright (c) 2003-2017 Mentor Graphics Corp.
//       All Rights Reserved
//
// This document may be used and distributed without restriction provided that
// this copyright statement is not removed from the file and that any derivative
// work contains this copyright notice.
//
// The design information contained in this file is intended to be an example
// of the functionality which the end user may study in preparation for creating
// their own custom interfaces. This design does not necessarily present a 
// complete implementation of the named protocol or standard.
//
//------------------------------------------------------------------------------

/*
 *            _________________________________________________
 * WRITER    |                                                 |   READER
 *           |               ccs_fifo_wait_core                |
 *           |             _____________________               |
 *        --<|  din_rdy --<|  ---------------- <|--- dout_rdy <|---
 *           |             |       FIFO         |              |
 *        ---|> din_vld ---|> ----------------  |>-- dout_vld  |>--
 *        ---|>     din ---|> ----------------  |>-- dout      |>--
 *           |             |____________________|              |
 *           |_________________________________________________|
 *
 *    rdy    - can be considered as a notFULL signal
 *    vld    - can be considered as a notEMPTY signal
 *    is_idle - clk can be safely gated
 *
 * Change History:
 *    2019-01-24 - Add assertion to verify rdy signal behavior under reset.
 *                 Fix bug in that behavior.
 */

module ccs_fifo_wait_core_v5 (clk, en, arst, srst, din_vld, din_rdy, din, dout_vld, dout_rdy, dout, sd, is_idle);

    parameter integer rscid    = 0;     // resource ID
    parameter integer width    = 8;     // fifo width
    parameter integer sz_width = 8;     // size of port for elements in fifo
    parameter integer fifo_sz  = 8;     // fifo depth
    parameter integer ph_clk   = 1;  // clock polarity 1=rising edge, 0=falling edge
    parameter integer ph_en    = 1;  // clock enable polarity
    parameter integer ph_arst  = 1;  // async reset polarity
    parameter integer ph_srst  = 1;  // sync reset polarity
    parameter integer ph_log2  = 3;     // log2(fifo_sz)

    input                 clk;
    input                 en;
    input                 arst;
    input                 srst;
    input                 din_vld;    // writer has valid data 
    output                din_rdy;    // fifo ready for data (not full)
    input  [width-1:0]    din;
    output                dout_vld;   // fifo has valid data (not empty)
    input                 dout_rdy;   // reader ready for data
    output [width-1:0]    dout;
    output [sz_width-1:0] sd; 
    output                is_idle;

    localparam integer fifo_b  = width * fifo_sz;
    localparam integer fifo_mx = (fifo_sz > 0) ? (fifo_sz-1) : 0 ;
    localparam integer fifo_mx_over_8 = fifo_mx / 8 ;

    reg      [fifo_mx:0] stat_pre;
    wire     [fifo_mx:0] stat;
    reg      [( (fifo_b > 0) ? fifo_b : 1)-1:0] buff_pre;
    wire     [( (fifo_b > 0) ? fifo_b : 1)-1:0] buff;
    reg      [fifo_mx:0] en_l;
    reg      [fifo_mx_over_8:0] en_l_s;

    reg      [width-1:0] buff_nxt;

    reg                  stat_nxt;
    reg                  stat_behind;
    reg                  stat_ahead;
    reg                  en_l_var;

    integer              i;
    genvar               eni;

    wire [32:0]          size_t;
    reg  [31:0]          count;
    reg  [31:0]          count_t;
    reg  [32:0]          n_elem;
    // synopsys translate_off
    reg  [31:0]          peak;
    initial
    begin
      count = 32'b0;
      peak  = 32'b0;
    end
    // synopsys translate_on
  wire din_rdy_drv  ;
  wire dout_vld_drv ;
    wire                 active;
    wire                 din_vld_int;
    wire                 hs_init;

    //assign din_rdy  = din_rdy_drv;    // dout_rdy | (~stat[0] & hs_init);   // original
    assign din_rdy = (fifo_sz > 0) ? (~stat[0] | dout_rdy) && hs_init : dout_rdy ;  
    assign dout_vld = dout_vld_drv;
    assign is_idle = (~((din_vld && din_rdy) || (dout_vld && dout_rdy))) && hs_init;

    generate
    if ( fifo_sz > 0 )
    begin: FIFO_REG
    assign din_vld_int = din_vld & hs_init;
    assign active =   (din_vld_int & din_rdy_drv) | (dout_rdy & dout_vld_drv);

      assign din_rdy_drv = dout_rdy | (~stat[0] & hs_init);
      assign dout_vld_drv = din_vld_int | stat[fifo_sz-1];

      assign size_t = (count - {31'b0 , (dout_rdy & stat[fifo_sz-1])}) + { 31'b0, din_vld_int};
      assign sd = size_t[sz_width-1:0];

      assign dout = (stat[fifo_sz-1]) ? buff[fifo_b-1:width*(fifo_sz-1)] : din;

      always @(*)
      begin: FIFOPROC
        n_elem = 33'b0;
        for (i = fifo_sz-1; i >= 0; i = i - 1)
        begin
          stat_behind = (i != 0) ? stat[i-1] : 1'b0;
          stat_ahead  = (i != (fifo_sz-1)) ? stat[i+1] : 1'b1;

          // Determine if this buffer element will have data
          stat_nxt = stat_ahead &                       // valid element ahead of this one (or head)
                       (stat_behind                     // valid element behind this one
                         | (stat[i] & (~dout_rdy))      // valid element and output not ready (in use, no tx)
                         | (stat[i] & din_vld_int)      // valid element and input has data
                         | (din_vld_int  & (~dout_rdy)) // input has data and output not ready
                       );
          stat_pre[i] = stat_nxt;

          if (dout_rdy & stat_behind )
          begin
            // pop n shift
            buff_nxt[0+:width] = buff[width*(i-1)+:width];
            en_l_var = 1'b1;
          end
          else if (din_vld_int & stat_nxt & ~((~dout_rdy) & stat[i]))
          begin
            // update tail with input data
            buff_nxt = din;
            en_l_var = 1'b1;
          end
          else
          begin
            // no-op, disable register
            buff_nxt = din; // Don't care input to disabled flop
            en_l_var = 1'b0;
          end
          buff_pre[width*i+:width] = buff_nxt[0+:width];
             
          if (ph_en != 0)
            en_l[i] = en & en_l_var;
          else
            en_l[i] = en | ~en_l_var;

          if ((stat_ahead == 1'b1) & (stat[i] == 1'b0)) 
            //found tail, update the number of elements for count
            n_elem = ($unsigned(fifo_sz) - 1) - $unsigned(i);
        end //for loop

        // Enable for stat registers (partitioned into banks of eight)
        // Take care of the head first
        if (ph_en != 0)
          en_l_s[(((fifo_sz > 0) ? fifo_sz : 1)-1)/8] = en & active;
        else
          en_l_s[(((fifo_sz > 0) ? fifo_sz : 1)-1)/8] = en | ~active;

        // Now every eight
        for (i = fifo_sz-1; i >= 7; i = i - 1)
        begin
          if (($unsigned(i)%8) == 0)
          begin
            if (ph_en != 0)
              en_l_s[(i/8)-1] = en & (stat[i]) & (active);
            else
              en_l_s[(i/8)-1] = en | ~(stat[i]) | ~(active);
          end
        end
        
        // Update count and peak
        if ( stat[fifo_sz-1] == 1'b0 )
          count_t = 32'b0;
        else if ( stat[0] == 1'b1 )
          count_t = fifo_sz;
        else 
          count_t = n_elem[31:0];
        count = count_t;
        // synopsys translate_off
        if ( peak < count )
          peak = count;
        // synopsys translate_on
      end //FIFOPROC

      // Handshake valid after reset
      ccs_genreg_v1
      #(
        .width   (1),
        .ph_clk  (ph_clk),
        .ph_en   (1),
        .ph_arst (ph_arst),
        .ph_srst (ph_srst),
        .has_en  (1'b0)
      )
      HS_INIT_REG
      (
        .clk     (clk),
        .en      (1'b1),
        .arst    (arst),
        .srst    (srst),
        .d       (1'b1),
        .z       (hs_init)
      );

      // Buffer and status registers
      for (eni = fifo_sz-1; eni >= 0; eni = eni - 1)
      begin: GEN_REGS
        ccs_genreg_v1
        #(
          .width   (1),
          .ph_clk  (ph_clk),
          .ph_en   (ph_en),
          .ph_arst (ph_arst),
          .ph_srst (ph_srst),
          .has_en  (1'b1)
        )
        STATREG
        (
          .clk     (clk),
          .en      (en_l_s[eni/8]),
          .arst    (arst),
          .srst    (srst),
          .d       (stat_pre[eni]),
          .z       (stat[eni])
        );

        ccs_genreg_v1
        #(
          .width   (width),
          .ph_clk  (ph_clk),
          .ph_en   (ph_en),
          .ph_arst (ph_arst),
          .ph_srst (ph_srst),
          .has_en  (1'b1)
        )
        BUFREG
        (
          .clk     (clk),
          .en      (en_l[eni]),
          .arst    (arst),
          .srst    (srst),
          .d       (buff_pre[width*eni+:width]),
          .z       (buff[width*eni+:width])
        );
      end

    end
    else
    begin: FEED_THRU
      assign din_rdy_drv  = dout_rdy;
      assign dout_vld_drv = din_vld;
      assign dout     = din;
      // non-blocking is not II=1 when fifo_sz=0
      assign sd = {{(sz_width-1){1'b0}}, (din_vld & ~dout_rdy)};
    end
    endgenerate

`ifdef RDY_ASRT 
    generate
    if (ph_clk==1) 
    begin: POS_CLK_ASSERT

       property rdyAsrt ;
         @(posedge clk) ((srst==ph_srst) || (arst==ph_arst)) |=> (din_rdy==0);
       endproperty
       a1Pos: assert property(rdyAsrt);

    end else if (ph_clk==0) 
    begin: NEG_CLK_ASSERT

       property rdyAsrt ;
         @(negedge clk) ((srst==ph_srst) || (arst==ph_arst)) |=> (din_rdy==0);
       endproperty
       a1Neg: assert property(rdyAsrt);

    end
    endgenerate

`endif
   
endmodule



//------> /opt/mentor/catapult_10.5a/Mgc_home/pkgs/siflibs/ccs_pipe_v5.v 
//------------------------------------------------------------------------------
// Catapult Synthesis - Sample I/O Port Library
//
// Copyright (c) 2003-2017 Mentor Graphics Corp.
//       All Rights Reserved
//
// This document may be used and distributed without restriction provided that
// this copyright statement is not removed from the file and that any derivative
// work contains this copyright notice.
//
// The design information contained in this file is intended to be an example
// of the functionality which the end user may study in preparation for creating
// their own custom interfaces. This design does not necessarily present a 
// complete implementation of the named protocol or standard.
//
//------------------------------------------------------------------------------
/*
 *
 *            _______________________________________________
 * WRITER    |                                              |          READER
 *           |                 ccs_pipe                     |
 *           |            ______________________            |
 *        --<| din_rdy --<|  ---------------- <|---dout_rdy<|---
 *           |            |       FIFO         |            |
 *        ---|>din_vld ---|> ----------------  |>--dout_vld |>--
 *        ---|>din -------|> ----------------  |> -----dout |>--
 *           |            |____________________|            |
 *           |______________________________________________|
 *
 *    din_rdy     - can be considered as a notFULL signal
 *    dout_vld    - can be considered as a notEMPTY signal
 *    write_stall - an internal debug signal formed from din_vld & !din_rdy
 *    read_stall  - an internal debug signal formed from dout_rdy & !dout_vld
 *    is_idle     - indicates the clock can be safely gated
 */

module ccs_pipe_v5 (clk, en, arst, srst, din_rdy, din_vld, din, dout_rdy, dout_vld, dout, sz, sz_req, is_idle);

    parameter integer rscid    = 0; // resource ID
    parameter integer width    = 8; // fifo width
    parameter integer sz_width = 8; // width of size of elements in fifo
    parameter integer fifo_sz  = 8; // fifo depth
    parameter integer log2_sz  = 3; // log2(fifo_sz)
    parameter integer ph_clk   = 1; // clock polarity 1=rising edge, 0=falling edge
    parameter integer ph_en    = 1; // clock enable polarity
    parameter integer ph_arst  = 1; // async reset polarity
    parameter integer ph_srst  = 1; // sync reset polarity

    // clock 
    input              clk;
    input              en;
    input              arst;
    input              srst;

    // writer
    output             din_rdy;
    input              din_vld;
    input  [width-1:0] din;

    // reader
    input              dout_rdy;
    output             dout_vld;
    output [width-1:0] dout;

    // size
    output [sz_width-1:0] sz;
    input                 sz_req;
    output                is_idle;
   
    // synopsys translate_off
    wire   write_stall;
    wire   read_stall;
    assign write_stall = din_vld & !din_rdy;
    assign read_stall  = dout_rdy & !dout_vld;
    // synopsys translate_on

    ccs_fifo_wait_core_v5
    #(
        .rscid    (rscid),
        .width    (width),
        .sz_width (sz_width),
        .fifo_sz  (fifo_sz),
        .ph_clk   (ph_clk),
        .ph_en    (ph_en),
        .ph_arst  (ph_arst),
        .ph_srst  (ph_srst),
        .ph_log2  (log2_sz)
    )
    FIFO
    (
        .clk      (clk),
        .en       (en),
        .arst     (arst),
        .srst     (srst),
        .din_vld  (din_vld),
        .din_rdy  (din_rdy),
        .din      (din),
        .dout_vld (dout_vld),
        .dout_rdy (dout_rdy),
        .dout     (dout),
        .sd       (sz),
        .is_idle  (is_idle)
    );

endmodule


//------> ./rtl_Hough_Algorithm_HW_1296_864mgc_rom_23_70_32_1_60.v 
// ----------------------------------------------------------------------
//  HLS HDL:        Verilog Netlister
//  HLS Version:    10.5a/871028 Production Release
//  HLS Date:       Tue Apr 14 07:55:32 PDT 2020
// 
//  Generated by:   user2@edatools.ee.duth.gr
//  Generated date: Sun Jul  4 16:59:19 2021
// ----------------------------------------------------------------------

// 
module Hough_Algorithm_HW_1296_864mgc_rom_23_70_32_1_60 (addr, data_out
);
  input [6:0]addr ;
  output [31:0]data_out ;


  // Constants for ROM dimensions
  parameter n_width    = 32;
  parameter n_size     = 70;
  parameter n_numports = 1;
  parameter n_addr_w   = 7;
  parameter n_inreg    = 0;
  parameter n_outreg   = 0;

  // Declare storage for memory elements
  wire [31:0] mem [69:0];

  // Declare output registers
  reg [31:0] data_out_t;

  // Initialize ROM contents
  // pragma attribute mem rom_block TRUE
  assign mem[0] = 32'b10000000000000000000000000000000;
  assign mem[1] = 32'b01001011100100000001010001110110;
  assign mem[2] = 32'b00100111111011001110000101101101;
  assign mem[3] = 32'b00010100010001000100011101010000;
  assign mem[4] = 32'b00001010001011000011010100001100;
  assign mem[5] = 32'b00000101000101110101111110000101;
  assign mem[6] = 32'b00000010100010111101100001111001;
  assign mem[7] = 32'b00000001010001011111000101010100;
  assign mem[8] = 32'b00000000101000101111100101001101;
  assign mem[9] = 32'b00000000010100010111110010111010;
  assign mem[10] = 32'b00000000001010001011111001100000;
  assign mem[11] = 32'b00000000000101000101111100110000;
  assign mem[12] = 32'b00000000000010100010111110011000;
  assign mem[13] = 32'b00000000000001010001011111001100;
  assign mem[14] = 32'b00000000000000101000101111100110;
  assign mem[15] = 32'b00000000000000010100010111110011;
  assign mem[16] = 32'b00000000000000001010001011111001;
  assign mem[17] = 32'b00000000000000000101000101111100;
  assign mem[18] = 32'b00000000000000000010100010111110;
  assign mem[19] = 32'b00000000000000000001010001011111;
  assign mem[20] = 32'b00000000000000000000101000101111;
  assign mem[21] = 32'b00000000000000000000010100010111;
  assign mem[22] = 32'b00000000000000000000001010001011;
  assign mem[23] = 32'b00000000000000000000000101000101;
  assign mem[24] = 32'b00000000000000000000000010100010;
  assign mem[25] = 32'b00000000000000000000000001010001;
  assign mem[26] = 32'b00000000000000000000000000101000;
  assign mem[27] = 32'b00000000000000000000000000010100;
  assign mem[28] = 32'b00000000000000000000000000001010;
  assign mem[29] = 32'b00000000000000000000000000000101;
  assign mem[30] = 32'b00000000000000000000000000000010;
  assign mem[31] = 32'b00000000000000000000000000000001;
  assign mem[32] = 32'b00000000000000000000000000000000;
  assign mem[33] = 32'b00000000000000000000000000000000;
  assign mem[34] = 32'b00000000000000000000000000000000;
  assign mem[35] = 32'b00000000000000000000000000000000;
  assign mem[36] = 32'b00000000000000000000000000000000;
  assign mem[37] = 32'b00000000000000000000000000000000;
  assign mem[38] = 32'b00000000000000000000000000000000;
  assign mem[39] = 32'b00000000000000000000000000000000;
  assign mem[40] = 32'b00000000000000000000000000000000;
  assign mem[41] = 32'b00000000000000000000000000000000;
  assign mem[42] = 32'b00000000000000000000000000000000;
  assign mem[43] = 32'b00000000000000000000000000000000;
  assign mem[44] = 32'b00000000000000000000000000000000;
  assign mem[45] = 32'b00000000000000000000000000000000;
  assign mem[46] = 32'b00000000000000000000000000000000;
  assign mem[47] = 32'b00000000000000000000000000000000;
  assign mem[48] = 32'b00000000000000000000000000000000;
  assign mem[49] = 32'b00000000000000000000000000000000;
  assign mem[50] = 32'b00000000000000000000000000000000;
  assign mem[51] = 32'b00000000000000000000000000000000;
  assign mem[52] = 32'b00000000000000000000000000000000;
  assign mem[53] = 32'b00000000000000000000000000000000;
  assign mem[54] = 32'b00000000000000000000000000000000;
  assign mem[55] = 32'b00000000000000000000000000000000;
  assign mem[56] = 32'b00000000000000000000000000000000;
  assign mem[57] = 32'b00000000000000000000000000000000;
  assign mem[58] = 32'b00000000000000000000000000000000;
  assign mem[59] = 32'b00000000000000000000000000000000;
  assign mem[60] = 32'b00000000000000000000000000000000;
  assign mem[61] = 32'b00000000000000000000000000000000;
  assign mem[62] = 32'b00000000000000000000000000000000;
  assign mem[63] = 32'b00000000000000000000000000000000;
  assign mem[64] = 32'b00000000000000000000000000000000;
  assign mem[65] = 32'b00000000000000000000000000000000;
  assign mem[66] = 32'b00000000000000000000000000000000;
  assign mem[67] = 32'b00000000000000000000000000000000;
  assign mem[68] = 32'b00000000000000000000000000000000;
  assign mem[69] = 32'b00000000000000000000000000000000;


  // Combinational ROM read block
  always@(*)
  begin
    if ( addr >= 'd0 && addr < 'd70)
    begin
      data_out_t <= mem[ addr ];
    end
    else
    begin
      data_out_t <= mem[ {1'b0, addr[5:0]} ];
    end
  end

  // Output register assignment
  assign data_out = data_out_t;

endmodule



//------> ./rtl_Hough_Algorithm_HW_1296_864mgc_rom_22_70_32_1_60.v 
// ----------------------------------------------------------------------
//  HLS HDL:        Verilog Netlister
//  HLS Version:    10.5a/871028 Production Release
//  HLS Date:       Tue Apr 14 07:55:32 PDT 2020
// 
//  Generated by:   user2@edatools.ee.duth.gr
//  Generated date: Sun Jul  4 16:59:19 2021
// ----------------------------------------------------------------------

// 
module Hough_Algorithm_HW_1296_864mgc_rom_22_70_32_1_60 (addr, data_out
);
  input [6:0]addr ;
  output [31:0]data_out ;


  // Constants for ROM dimensions
  parameter n_width    = 32;
  parameter n_size     = 70;
  parameter n_numports = 1;
  parameter n_addr_w   = 7;
  parameter n_inreg    = 0;
  parameter n_outreg   = 0;

  // Declare storage for memory elements
  wire [31:0] mem [69:0];

  // Declare output registers
  reg [31:0] data_out_t;

  // Initialize ROM contents
  // pragma attribute mem rom_block TRUE
  assign mem[0] = 32'b10000000000000000000000000000000;
  assign mem[1] = 32'b01001011100100000001010001110110;
  assign mem[2] = 32'b00100111111011001110000101101101;
  assign mem[3] = 32'b00010100010001000100011101010000;
  assign mem[4] = 32'b00001010001011000011010100001100;
  assign mem[5] = 32'b00000101000101110101111110000101;
  assign mem[6] = 32'b00000010100010111101100001111001;
  assign mem[7] = 32'b00000001010001011111000101010100;
  assign mem[8] = 32'b00000000101000101111100101001101;
  assign mem[9] = 32'b00000000010100010111110010111010;
  assign mem[10] = 32'b00000000001010001011111001100000;
  assign mem[11] = 32'b00000000000101000101111100110000;
  assign mem[12] = 32'b00000000000010100010111110011000;
  assign mem[13] = 32'b00000000000001010001011111001100;
  assign mem[14] = 32'b00000000000000101000101111100110;
  assign mem[15] = 32'b00000000000000010100010111110011;
  assign mem[16] = 32'b00000000000000001010001011111001;
  assign mem[17] = 32'b00000000000000000101000101111100;
  assign mem[18] = 32'b00000000000000000010100010111110;
  assign mem[19] = 32'b00000000000000000001010001011111;
  assign mem[20] = 32'b00000000000000000000101000101111;
  assign mem[21] = 32'b00000000000000000000010100010111;
  assign mem[22] = 32'b00000000000000000000001010001011;
  assign mem[23] = 32'b00000000000000000000000101000101;
  assign mem[24] = 32'b00000000000000000000000010100010;
  assign mem[25] = 32'b00000000000000000000000001010001;
  assign mem[26] = 32'b00000000000000000000000000101000;
  assign mem[27] = 32'b00000000000000000000000000010100;
  assign mem[28] = 32'b00000000000000000000000000001010;
  assign mem[29] = 32'b00000000000000000000000000000101;
  assign mem[30] = 32'b00000000000000000000000000000010;
  assign mem[31] = 32'b00000000000000000000000000000001;
  assign mem[32] = 32'b00000000000000000000000000000000;
  assign mem[33] = 32'b00000000000000000000000000000000;
  assign mem[34] = 32'b00000000000000000000000000000000;
  assign mem[35] = 32'b00000000000000000000000000000000;
  assign mem[36] = 32'b00000000000000000000000000000000;
  assign mem[37] = 32'b00000000000000000000000000000000;
  assign mem[38] = 32'b00000000000000000000000000000000;
  assign mem[39] = 32'b00000000000000000000000000000000;
  assign mem[40] = 32'b00000000000000000000000000000000;
  assign mem[41] = 32'b00000000000000000000000000000000;
  assign mem[42] = 32'b00000000000000000000000000000000;
  assign mem[43] = 32'b00000000000000000000000000000000;
  assign mem[44] = 32'b00000000000000000000000000000000;
  assign mem[45] = 32'b00000000000000000000000000000000;
  assign mem[46] = 32'b00000000000000000000000000000000;
  assign mem[47] = 32'b00000000000000000000000000000000;
  assign mem[48] = 32'b00000000000000000000000000000000;
  assign mem[49] = 32'b00000000000000000000000000000000;
  assign mem[50] = 32'b00000000000000000000000000000000;
  assign mem[51] = 32'b00000000000000000000000000000000;
  assign mem[52] = 32'b00000000000000000000000000000000;
  assign mem[53] = 32'b00000000000000000000000000000000;
  assign mem[54] = 32'b00000000000000000000000000000000;
  assign mem[55] = 32'b00000000000000000000000000000000;
  assign mem[56] = 32'b00000000000000000000000000000000;
  assign mem[57] = 32'b00000000000000000000000000000000;
  assign mem[58] = 32'b00000000000000000000000000000000;
  assign mem[59] = 32'b00000000000000000000000000000000;
  assign mem[60] = 32'b00000000000000000000000000000000;
  assign mem[61] = 32'b00000000000000000000000000000000;
  assign mem[62] = 32'b00000000000000000000000000000000;
  assign mem[63] = 32'b00000000000000000000000000000000;
  assign mem[64] = 32'b00000000000000000000000000000000;
  assign mem[65] = 32'b00000000000000000000000000000000;
  assign mem[66] = 32'b00000000000000000000000000000000;
  assign mem[67] = 32'b00000000000000000000000000000000;
  assign mem[68] = 32'b00000000000000000000000000000000;
  assign mem[69] = 32'b00000000000000000000000000000000;


  // Combinational ROM read block
  always@(*)
  begin
    if ( addr >= 'd0 && addr < 'd70)
    begin
      data_out_t <= mem[ addr ];
    end
    else
    begin
      data_out_t <= mem[ {1'b0, addr[5:0]} ];
    end
  end

  // Output register assignment
  assign data_out = data_out_t;

endmodule



//------> ./rtl.v 
// ----------------------------------------------------------------------
//  HLS HDL:        Verilog Netlister
//  HLS Version:    10.5a/871028 Production Release
//  HLS Date:       Tue Apr 14 07:55:32 PDT 2020
// 
//  Generated by:   user2@edatools.ee.duth.gr
//  Generated date: Sun Jul  4 16:59:19 2021
// ----------------------------------------------------------------------

// 
// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_core_fsm
//  FSM Module
// ------------------------------------------------------------------


module getMaxLine_core_core_fsm (
  clk, rst, core_wen, fsm_output, T_LINE_C_4_tr0, ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0,
      T_LINE_C_10_tr0, T_LINE_C_10_tr1, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0,
      T_LINE_C_14_tr0, T_LINE_C_14_tr1, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0,
      T_LINE_C_18_tr0, R_LINE_C_0_tr0
);
  input clk;
  input rst;
  input core_wen;
  output [31:0] fsm_output;
  reg [31:0] fsm_output;
  input T_LINE_C_4_tr0;
  input ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0;
  input T_LINE_C_10_tr0;
  input T_LINE_C_10_tr1;
  input ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0;
  input ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0;
  input T_LINE_C_14_tr0;
  input T_LINE_C_14_tr1;
  input ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0;
  input ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0;
  input T_LINE_C_18_tr0;
  input R_LINE_C_0_tr0;


  // FSM State Type Declaration for getMaxLine_core_core_fsm_1
  parameter
    main_C_0 = 5'd0,
    T_LINE_C_0 = 5'd1,
    T_LINE_C_1 = 5'd2,
    T_LINE_C_2 = 5'd3,
    T_LINE_C_3 = 5'd4,
    T_LINE_C_4 = 5'd5,
    ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0 = 5'd6,
    ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1 = 5'd7,
    ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_2 = 5'd8,
    ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_3 = 5'd9,
    ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_4 = 5'd10,
    ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5 = 5'd11,
    T_LINE_C_5 = 5'd12,
    T_LINE_C_6 = 5'd13,
    T_LINE_C_7 = 5'd14,
    T_LINE_C_8 = 5'd15,
    T_LINE_C_9 = 5'd16,
    T_LINE_C_10 = 5'd17,
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0
        = 5'd18,
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0
        = 5'd19,
    T_LINE_C_11 = 5'd20,
    T_LINE_C_12 = 5'd21,
    T_LINE_C_13 = 5'd22,
    T_LINE_C_14 = 5'd23,
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0
        = 5'd24,
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0
        = 5'd25,
    T_LINE_C_15 = 5'd26,
    T_LINE_C_16 = 5'd27,
    T_LINE_C_17 = 5'd28,
    T_LINE_C_18 = 5'd29,
    R_LINE_C_0 = 5'd30,
    main_C_1 = 5'd31;

  reg [4:0] state_var;
  reg [4:0] state_var_NS;


  // Interconnect Declarations for Component Instantiations 
  always @(*)
  begin : getMaxLine_core_core_fsm_1
    case (state_var)
      T_LINE_C_0 : begin
        fsm_output = 32'b00000000000000000000000000000010;
        state_var_NS = T_LINE_C_1;
      end
      T_LINE_C_1 : begin
        fsm_output = 32'b00000000000000000000000000000100;
        state_var_NS = T_LINE_C_2;
      end
      T_LINE_C_2 : begin
        fsm_output = 32'b00000000000000000000000000001000;
        state_var_NS = T_LINE_C_3;
      end
      T_LINE_C_3 : begin
        fsm_output = 32'b00000000000000000000000000010000;
        state_var_NS = T_LINE_C_4;
      end
      T_LINE_C_4 : begin
        fsm_output = 32'b00000000000000000000000000100000;
        if ( T_LINE_C_4_tr0 ) begin
          state_var_NS = T_LINE_C_5;
        end
        else begin
          state_var_NS = ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0;
        end
      end
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0 :
          begin
        fsm_output = 32'b00000000000000000000000001000000;
        state_var_NS = ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1;
      end
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1 :
          begin
        fsm_output = 32'b00000000000000000000000010000000;
        state_var_NS = ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_2;
      end
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_2 :
          begin
        fsm_output = 32'b00000000000000000000000100000000;
        state_var_NS = ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_3;
      end
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_3 :
          begin
        fsm_output = 32'b00000000000000000000001000000000;
        state_var_NS = ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_4;
      end
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_4 :
          begin
        fsm_output = 32'b00000000000000000000010000000000;
        state_var_NS = ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5;
      end
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5 :
          begin
        fsm_output = 32'b00000000000000000000100000000000;
        if ( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0
            ) begin
          state_var_NS = T_LINE_C_5;
        end
        else begin
          state_var_NS = ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0;
        end
      end
      T_LINE_C_5 : begin
        fsm_output = 32'b00000000000000000001000000000000;
        state_var_NS = T_LINE_C_6;
      end
      T_LINE_C_6 : begin
        fsm_output = 32'b00000000000000000010000000000000;
        state_var_NS = T_LINE_C_7;
      end
      T_LINE_C_7 : begin
        fsm_output = 32'b00000000000000000100000000000000;
        state_var_NS = T_LINE_C_8;
      end
      T_LINE_C_8 : begin
        fsm_output = 32'b00000000000000001000000000000000;
        state_var_NS = T_LINE_C_9;
      end
      T_LINE_C_9 : begin
        fsm_output = 32'b00000000000000010000000000000000;
        state_var_NS = T_LINE_C_10;
      end
      T_LINE_C_10 : begin
        fsm_output = 32'b00000000000000100000000000000000;
        if ( T_LINE_C_10_tr0 ) begin
          state_var_NS = T_LINE_C_11;
        end
        else if ( T_LINE_C_10_tr1 ) begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0;
        end
        else begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0;
        end
      end
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0
          : begin
        fsm_output = 32'b00000000000001000000000000000000;
        if ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0
            ) begin
          state_var_NS = T_LINE_C_11;
        end
        else begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0;
        end
      end
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0
          : begin
        fsm_output = 32'b00000000000010000000000000000000;
        if ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
            ) begin
          state_var_NS = T_LINE_C_11;
        end
        else begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0;
        end
      end
      T_LINE_C_11 : begin
        fsm_output = 32'b00000000000100000000000000000000;
        state_var_NS = T_LINE_C_12;
      end
      T_LINE_C_12 : begin
        fsm_output = 32'b00000000001000000000000000000000;
        state_var_NS = T_LINE_C_13;
      end
      T_LINE_C_13 : begin
        fsm_output = 32'b00000000010000000000000000000000;
        state_var_NS = T_LINE_C_14;
      end
      T_LINE_C_14 : begin
        fsm_output = 32'b00000000100000000000000000000000;
        if ( T_LINE_C_14_tr0 ) begin
          state_var_NS = T_LINE_C_15;
        end
        else if ( T_LINE_C_14_tr1 ) begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0;
        end
        else begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0;
        end
      end
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0
          : begin
        fsm_output = 32'b00000001000000000000000000000000;
        if ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0
            ) begin
          state_var_NS = T_LINE_C_15;
        end
        else begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0;
        end
      end
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0
          : begin
        fsm_output = 32'b00000010000000000000000000000000;
        if ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
            ) begin
          state_var_NS = T_LINE_C_15;
        end
        else begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0;
        end
      end
      T_LINE_C_15 : begin
        fsm_output = 32'b00000100000000000000000000000000;
        state_var_NS = T_LINE_C_16;
      end
      T_LINE_C_16 : begin
        fsm_output = 32'b00001000000000000000000000000000;
        state_var_NS = T_LINE_C_17;
      end
      T_LINE_C_17 : begin
        fsm_output = 32'b00010000000000000000000000000000;
        state_var_NS = T_LINE_C_18;
      end
      T_LINE_C_18 : begin
        fsm_output = 32'b00100000000000000000000000000000;
        if ( T_LINE_C_18_tr0 ) begin
          state_var_NS = R_LINE_C_0;
        end
        else begin
          state_var_NS = T_LINE_C_0;
        end
      end
      R_LINE_C_0 : begin
        fsm_output = 32'b01000000000000000000000000000000;
        if ( R_LINE_C_0_tr0 ) begin
          state_var_NS = main_C_1;
        end
        else begin
          state_var_NS = T_LINE_C_0;
        end
      end
      main_C_1 : begin
        fsm_output = 32'b10000000000000000000000000000000;
        state_var_NS = main_C_0;
      end
      // main_C_0
      default : begin
        fsm_output = 32'b00000000000000000000000000000001;
        state_var_NS = T_LINE_C_0;
      end
    endcase
  end

  always @(posedge clk) begin
    if ( rst ) begin
      state_var <= main_C_0;
    end
    else if ( core_wen ) begin
      state_var <= state_var_NS;
    end
  end

endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_staller
// ------------------------------------------------------------------


module getMaxLine_core_staller (
  core_wen, x1_rsci_wen_comp, y1_rsci_wen_comp, x2_rsci_wen_comp, y2_rsci_wen_comp,
      acc_rsci_wen_comp
);
  output core_wen;
  input x1_rsci_wen_comp;
  input y1_rsci_wen_comp;
  input x2_rsci_wen_comp;
  input y2_rsci_wen_comp;
  input acc_rsci_wen_comp;



  // Interconnect Declarations for Component Instantiations 
  assign core_wen = x1_rsci_wen_comp & y1_rsci_wen_comp & x2_rsci_wen_comp & y2_rsci_wen_comp
      & acc_rsci_wen_comp;
endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_wait_dp
// ------------------------------------------------------------------


module getMaxLine_core_wait_dp (
  clk, rst, T_LINE_if_if_dividend1_mul_cmp_z, core_wen, T_LINE_if_if_dividend1_mul_cmp_z_oreg
);
  input clk;
  input rst;
  input [43:0] T_LINE_if_if_dividend1_mul_cmp_z;
  input core_wen;
  output [43:0] T_LINE_if_if_dividend1_mul_cmp_z_oreg;
  reg [43:0] T_LINE_if_if_dividend1_mul_cmp_z_oreg;



  // Interconnect Declarations for Component Instantiations 
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_if_dividend1_mul_cmp_z_oreg <= 44'b00000000000000000000000000000000000000000000;
    end
    else if ( core_wen ) begin
      T_LINE_if_if_dividend1_mul_cmp_z_oreg <= T_LINE_if_if_dividend1_mul_cmp_z;
    end
  end
endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_acc_rsci_acc_wait_dp
// ------------------------------------------------------------------


module getMaxLine_core_acc_rsci_acc_wait_dp (
  clk, rst, acc_rsci_oswt, acc_rsci_wen_comp, acc_rsci_idat_mxwt, acc_rsci_biwt,
      acc_rsci_bdwt, acc_rsci_bcwt, acc_rsci_idat
);
  input clk;
  input rst;
  input acc_rsci_oswt;
  output acc_rsci_wen_comp;
  output [15:0] acc_rsci_idat_mxwt;
  input acc_rsci_biwt;
  input acc_rsci_bdwt;
  output acc_rsci_bcwt;
  reg acc_rsci_bcwt;
  input [15:0] acc_rsci_idat;


  // Interconnect Declarations
  reg [15:0] acc_rsci_idat_bfwt;


  // Interconnect Declarations for Component Instantiations 
  assign acc_rsci_wen_comp = (~ acc_rsci_oswt) | acc_rsci_biwt | acc_rsci_bcwt;
  assign acc_rsci_idat_mxwt = MUX_v_16_2_2(acc_rsci_idat, acc_rsci_idat_bfwt, acc_rsci_bcwt);
  always @(posedge clk) begin
    if ( rst ) begin
      acc_rsci_bcwt <= 1'b0;
    end
    else begin
      acc_rsci_bcwt <= ~((~(acc_rsci_bcwt | acc_rsci_biwt)) | acc_rsci_bdwt);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      acc_rsci_idat_bfwt <= 16'b0000000000000000;
    end
    else if ( acc_rsci_biwt ) begin
      acc_rsci_idat_bfwt <= acc_rsci_idat;
    end
  end

  function automatic [15:0] MUX_v_16_2_2;
    input [15:0] input_0;
    input [15:0] input_1;
    input [0:0] sel;
    reg [15:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_16_2_2 = result;
  end
  endfunction

endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_acc_rsci_acc_wait_ctrl
// ------------------------------------------------------------------


module getMaxLine_core_acc_rsci_acc_wait_ctrl (
  core_wen, acc_rsci_oswt, acc_rsci_biwt, acc_rsci_bdwt, acc_rsci_bcwt, acc_rsci_irdy_core_sct,
      acc_rsci_ivld
);
  input core_wen;
  input acc_rsci_oswt;
  output acc_rsci_biwt;
  output acc_rsci_bdwt;
  input acc_rsci_bcwt;
  output acc_rsci_irdy_core_sct;
  input acc_rsci_ivld;


  // Interconnect Declarations
  wire acc_rsci_ogwt;


  // Interconnect Declarations for Component Instantiations 
  assign acc_rsci_bdwt = acc_rsci_oswt & core_wen;
  assign acc_rsci_biwt = acc_rsci_ogwt & acc_rsci_ivld;
  assign acc_rsci_ogwt = acc_rsci_oswt & (~ acc_rsci_bcwt);
  assign acc_rsci_irdy_core_sct = acc_rsci_ogwt;
endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_y2_rsci_y2_wait_dp
// ------------------------------------------------------------------


module getMaxLine_core_y2_rsci_y2_wait_dp (
  clk, rst, y2_rsci_oswt, y2_rsci_wen_comp, y2_rsci_biwt, y2_rsci_bdwt, y2_rsci_bcwt
);
  input clk;
  input rst;
  input y2_rsci_oswt;
  output y2_rsci_wen_comp;
  input y2_rsci_biwt;
  input y2_rsci_bdwt;
  output y2_rsci_bcwt;
  reg y2_rsci_bcwt;



  // Interconnect Declarations for Component Instantiations 
  assign y2_rsci_wen_comp = (~ y2_rsci_oswt) | y2_rsci_biwt | y2_rsci_bcwt;
  always @(posedge clk) begin
    if ( rst ) begin
      y2_rsci_bcwt <= 1'b0;
    end
    else begin
      y2_rsci_bcwt <= ~((~(y2_rsci_bcwt | y2_rsci_biwt)) | y2_rsci_bdwt);
    end
  end
endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_y2_rsci_y2_wait_ctrl
// ------------------------------------------------------------------


module getMaxLine_core_y2_rsci_y2_wait_ctrl (
  core_wen, y2_rsci_oswt, y2_rsci_irdy, y2_rsci_biwt, y2_rsci_bdwt, y2_rsci_bcwt,
      y2_rsci_ivld_core_sct
);
  input core_wen;
  input y2_rsci_oswt;
  input y2_rsci_irdy;
  output y2_rsci_biwt;
  output y2_rsci_bdwt;
  input y2_rsci_bcwt;
  output y2_rsci_ivld_core_sct;


  // Interconnect Declarations
  wire y2_rsci_ogwt;


  // Interconnect Declarations for Component Instantiations 
  assign y2_rsci_bdwt = y2_rsci_oswt & core_wen;
  assign y2_rsci_biwt = y2_rsci_ogwt & y2_rsci_irdy;
  assign y2_rsci_ogwt = y2_rsci_oswt & (~ y2_rsci_bcwt);
  assign y2_rsci_ivld_core_sct = y2_rsci_ogwt;
endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_x2_rsci_x2_wait_dp
// ------------------------------------------------------------------


module getMaxLine_core_x2_rsci_x2_wait_dp (
  clk, rst, x2_rsci_oswt, x2_rsci_wen_comp, x2_rsci_biwt, x2_rsci_bdwt, x2_rsci_bcwt
);
  input clk;
  input rst;
  input x2_rsci_oswt;
  output x2_rsci_wen_comp;
  input x2_rsci_biwt;
  input x2_rsci_bdwt;
  output x2_rsci_bcwt;
  reg x2_rsci_bcwt;



  // Interconnect Declarations for Component Instantiations 
  assign x2_rsci_wen_comp = (~ x2_rsci_oswt) | x2_rsci_biwt | x2_rsci_bcwt;
  always @(posedge clk) begin
    if ( rst ) begin
      x2_rsci_bcwt <= 1'b0;
    end
    else begin
      x2_rsci_bcwt <= ~((~(x2_rsci_bcwt | x2_rsci_biwt)) | x2_rsci_bdwt);
    end
  end
endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_x2_rsci_x2_wait_ctrl
// ------------------------------------------------------------------


module getMaxLine_core_x2_rsci_x2_wait_ctrl (
  core_wen, x2_rsci_oswt, x2_rsci_irdy, x2_rsci_biwt, x2_rsci_bdwt, x2_rsci_bcwt,
      x2_rsci_ivld_core_sct
);
  input core_wen;
  input x2_rsci_oswt;
  input x2_rsci_irdy;
  output x2_rsci_biwt;
  output x2_rsci_bdwt;
  input x2_rsci_bcwt;
  output x2_rsci_ivld_core_sct;


  // Interconnect Declarations
  wire x2_rsci_ogwt;


  // Interconnect Declarations for Component Instantiations 
  assign x2_rsci_bdwt = x2_rsci_oswt & core_wen;
  assign x2_rsci_biwt = x2_rsci_ogwt & x2_rsci_irdy;
  assign x2_rsci_ogwt = x2_rsci_oswt & (~ x2_rsci_bcwt);
  assign x2_rsci_ivld_core_sct = x2_rsci_ogwt;
endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_y1_rsci_y1_wait_dp
// ------------------------------------------------------------------


module getMaxLine_core_y1_rsci_y1_wait_dp (
  clk, rst, y1_rsci_oswt, y1_rsci_wen_comp, y1_rsci_biwt, y1_rsci_bdwt, y1_rsci_bcwt
);
  input clk;
  input rst;
  input y1_rsci_oswt;
  output y1_rsci_wen_comp;
  input y1_rsci_biwt;
  input y1_rsci_bdwt;
  output y1_rsci_bcwt;
  reg y1_rsci_bcwt;



  // Interconnect Declarations for Component Instantiations 
  assign y1_rsci_wen_comp = (~ y1_rsci_oswt) | y1_rsci_biwt | y1_rsci_bcwt;
  always @(posedge clk) begin
    if ( rst ) begin
      y1_rsci_bcwt <= 1'b0;
    end
    else begin
      y1_rsci_bcwt <= ~((~(y1_rsci_bcwt | y1_rsci_biwt)) | y1_rsci_bdwt);
    end
  end
endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_y1_rsci_y1_wait_ctrl
// ------------------------------------------------------------------


module getMaxLine_core_y1_rsci_y1_wait_ctrl (
  core_wen, y1_rsci_oswt, y1_rsci_irdy, y1_rsci_biwt, y1_rsci_bdwt, y1_rsci_bcwt,
      y1_rsci_ivld_core_sct
);
  input core_wen;
  input y1_rsci_oswt;
  input y1_rsci_irdy;
  output y1_rsci_biwt;
  output y1_rsci_bdwt;
  input y1_rsci_bcwt;
  output y1_rsci_ivld_core_sct;


  // Interconnect Declarations
  wire y1_rsci_ogwt;


  // Interconnect Declarations for Component Instantiations 
  assign y1_rsci_bdwt = y1_rsci_oswt & core_wen;
  assign y1_rsci_biwt = y1_rsci_ogwt & y1_rsci_irdy;
  assign y1_rsci_ogwt = y1_rsci_oswt & (~ y1_rsci_bcwt);
  assign y1_rsci_ivld_core_sct = y1_rsci_ogwt;
endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_x1_rsci_x1_wait_dp
// ------------------------------------------------------------------


module getMaxLine_core_x1_rsci_x1_wait_dp (
  clk, rst, x1_rsci_oswt, x1_rsci_wen_comp, x1_rsci_biwt, x1_rsci_bdwt, x1_rsci_bcwt
);
  input clk;
  input rst;
  input x1_rsci_oswt;
  output x1_rsci_wen_comp;
  input x1_rsci_biwt;
  input x1_rsci_bdwt;
  output x1_rsci_bcwt;
  reg x1_rsci_bcwt;



  // Interconnect Declarations for Component Instantiations 
  assign x1_rsci_wen_comp = (~ x1_rsci_oswt) | x1_rsci_biwt | x1_rsci_bcwt;
  always @(posedge clk) begin
    if ( rst ) begin
      x1_rsci_bcwt <= 1'b0;
    end
    else begin
      x1_rsci_bcwt <= ~((~(x1_rsci_bcwt | x1_rsci_biwt)) | x1_rsci_bdwt);
    end
  end
endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_x1_rsci_x1_wait_ctrl
// ------------------------------------------------------------------


module getMaxLine_core_x1_rsci_x1_wait_ctrl (
  core_wen, x1_rsci_oswt, x1_rsci_irdy, x1_rsci_biwt, x1_rsci_bdwt, x1_rsci_bcwt,
      x1_rsci_ivld_core_sct
);
  input core_wen;
  input x1_rsci_oswt;
  input x1_rsci_irdy;
  output x1_rsci_biwt;
  output x1_rsci_bdwt;
  input x1_rsci_bcwt;
  output x1_rsci_ivld_core_sct;


  // Interconnect Declarations
  wire x1_rsci_ogwt;


  // Interconnect Declarations for Component Instantiations 
  assign x1_rsci_bdwt = x1_rsci_oswt & core_wen;
  assign x1_rsci_biwt = x1_rsci_ogwt & x1_rsci_irdy;
  assign x1_rsci_ogwt = x1_rsci_oswt & (~ x1_rsci_bcwt);
  assign x1_rsci_ivld_core_sct = x1_rsci_ogwt;
endmodule

// ------------------------------------------------------------------
//  Design Unit:    houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_13_400000_16_19_0_1_0_0_0_1_1_16_400000_2_gen
// ------------------------------------------------------------------


module houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_13_400000_16_19_0_1_0_0_0_1_1_16_400000_2_gen
    (
  en, data_out, we, re, addr, data_in, data_in_d, addr_d, re_d, we_d, data_out_d,
      en_d
);
  output en;
  input [31:0] data_out;
  output [1:0] we;
  output [1:0] re;
  output [37:0] addr;
  output [31:0] data_in;
  input [31:0] data_in_d;
  input [37:0] addr_d;
  input [1:0] re_d;
  input [1:0] we_d;
  output [31:0] data_out_d;
  input en_d;



  // Interconnect Declarations for Component Instantiations 
  assign en = en_d;
  assign data_out_d = data_out;
  assign we = we_d;
  assign re = re_d;
  assign addr = addr_d;
  assign data_in = data_in_d;
endmodule

// ------------------------------------------------------------------
//  Design Unit:    houghTransform_core_core_fsm
//  FSM Module
// ------------------------------------------------------------------


module houghTransform_core_core_fsm (
  clk, rst, core_wen, fsm_output, acc_tmp_vinit_C_0_tr0, HCOL_C_0_tr0, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0,
      HACC_C_15_tr0, HCOL_C_1_tr0, HROW_C_0_tr0, for_1_C_2_tr0
);
  input clk;
  input rst;
  input core_wen;
  output [31:0] fsm_output;
  reg [31:0] fsm_output;
  input acc_tmp_vinit_C_0_tr0;
  input HCOL_C_0_tr0;
  input ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0;
  input HACC_C_15_tr0;
  input HCOL_C_1_tr0;
  input HROW_C_0_tr0;
  input for_1_C_2_tr0;


  // FSM State Type Declaration for houghTransform_core_core_fsm_1
  parameter
    core_rlp_C_0 = 5'd0,
    main_C_0 = 5'd1,
    acc_tmp_vinit_C_0 = 5'd2,
    HCOL_C_0 = 5'd3,
    HACC_C_0 = 5'd4,
    HACC_C_1 = 5'd5,
    HACC_C_2 = 5'd6,
    HACC_C_3 = 5'd7,
    ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0 = 5'd8,
    ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1 = 5'd9,
    ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_2 = 5'd10,
    ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_3 = 5'd11,
    ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_4 = 5'd12,
    ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5 = 5'd13,
    HACC_C_4 = 5'd14,
    HACC_C_5 = 5'd15,
    HACC_C_6 = 5'd16,
    HACC_C_7 = 5'd17,
    HACC_C_8 = 5'd18,
    HACC_C_9 = 5'd19,
    HACC_C_10 = 5'd20,
    HACC_C_11 = 5'd21,
    HACC_C_12 = 5'd22,
    HACC_C_13 = 5'd23,
    HACC_C_14 = 5'd24,
    HACC_C_15 = 5'd25,
    HCOL_C_1 = 5'd26,
    HROW_C_0 = 5'd27,
    for_1_C_0 = 5'd28,
    for_1_C_1 = 5'd29,
    for_1_C_2 = 5'd30,
    main_C_1 = 5'd31;

  reg [4:0] state_var;
  reg [4:0] state_var_NS;


  // Interconnect Declarations for Component Instantiations 
  always @(*)
  begin : houghTransform_core_core_fsm_1
    case (state_var)
      main_C_0 : begin
        fsm_output = 32'b00000000000000000000000000000010;
        state_var_NS = acc_tmp_vinit_C_0;
      end
      acc_tmp_vinit_C_0 : begin
        fsm_output = 32'b00000000000000000000000000000100;
        if ( acc_tmp_vinit_C_0_tr0 ) begin
          state_var_NS = HCOL_C_0;
        end
        else begin
          state_var_NS = acc_tmp_vinit_C_0;
        end
      end
      HCOL_C_0 : begin
        fsm_output = 32'b00000000000000000000000000001000;
        if ( HCOL_C_0_tr0 ) begin
          state_var_NS = HCOL_C_1;
        end
        else begin
          state_var_NS = HACC_C_0;
        end
      end
      HACC_C_0 : begin
        fsm_output = 32'b00000000000000000000000000010000;
        state_var_NS = HACC_C_1;
      end
      HACC_C_1 : begin
        fsm_output = 32'b00000000000000000000000000100000;
        state_var_NS = HACC_C_2;
      end
      HACC_C_2 : begin
        fsm_output = 32'b00000000000000000000000001000000;
        state_var_NS = HACC_C_3;
      end
      HACC_C_3 : begin
        fsm_output = 32'b00000000000000000000000010000000;
        state_var_NS = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0;
      end
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0 :
          begin
        fsm_output = 32'b00000000000000000000000100000000;
        state_var_NS = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1;
      end
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1 :
          begin
        fsm_output = 32'b00000000000000000000001000000000;
        state_var_NS = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_2;
      end
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_2 :
          begin
        fsm_output = 32'b00000000000000000000010000000000;
        state_var_NS = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_3;
      end
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_3 :
          begin
        fsm_output = 32'b00000000000000000000100000000000;
        state_var_NS = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_4;
      end
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_4 :
          begin
        fsm_output = 32'b00000000000000000001000000000000;
        state_var_NS = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5;
      end
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5 :
          begin
        fsm_output = 32'b00000000000000000010000000000000;
        if ( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0
            ) begin
          state_var_NS = HACC_C_4;
        end
        else begin
          state_var_NS = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0;
        end
      end
      HACC_C_4 : begin
        fsm_output = 32'b00000000000000000100000000000000;
        state_var_NS = HACC_C_5;
      end
      HACC_C_5 : begin
        fsm_output = 32'b00000000000000001000000000000000;
        state_var_NS = HACC_C_6;
      end
      HACC_C_6 : begin
        fsm_output = 32'b00000000000000010000000000000000;
        state_var_NS = HACC_C_7;
      end
      HACC_C_7 : begin
        fsm_output = 32'b00000000000000100000000000000000;
        state_var_NS = HACC_C_8;
      end
      HACC_C_8 : begin
        fsm_output = 32'b00000000000001000000000000000000;
        state_var_NS = HACC_C_9;
      end
      HACC_C_9 : begin
        fsm_output = 32'b00000000000010000000000000000000;
        state_var_NS = HACC_C_10;
      end
      HACC_C_10 : begin
        fsm_output = 32'b00000000000100000000000000000000;
        state_var_NS = HACC_C_11;
      end
      HACC_C_11 : begin
        fsm_output = 32'b00000000001000000000000000000000;
        state_var_NS = HACC_C_12;
      end
      HACC_C_12 : begin
        fsm_output = 32'b00000000010000000000000000000000;
        state_var_NS = HACC_C_13;
      end
      HACC_C_13 : begin
        fsm_output = 32'b00000000100000000000000000000000;
        state_var_NS = HACC_C_14;
      end
      HACC_C_14 : begin
        fsm_output = 32'b00000001000000000000000000000000;
        state_var_NS = HACC_C_15;
      end
      HACC_C_15 : begin
        fsm_output = 32'b00000010000000000000000000000000;
        if ( HACC_C_15_tr0 ) begin
          state_var_NS = HCOL_C_1;
        end
        else begin
          state_var_NS = HACC_C_0;
        end
      end
      HCOL_C_1 : begin
        fsm_output = 32'b00000100000000000000000000000000;
        if ( HCOL_C_1_tr0 ) begin
          state_var_NS = HROW_C_0;
        end
        else begin
          state_var_NS = HCOL_C_0;
        end
      end
      HROW_C_0 : begin
        fsm_output = 32'b00001000000000000000000000000000;
        if ( HROW_C_0_tr0 ) begin
          state_var_NS = for_1_C_0;
        end
        else begin
          state_var_NS = HCOL_C_0;
        end
      end
      for_1_C_0 : begin
        fsm_output = 32'b00010000000000000000000000000000;
        state_var_NS = for_1_C_1;
      end
      for_1_C_1 : begin
        fsm_output = 32'b00100000000000000000000000000000;
        state_var_NS = for_1_C_2;
      end
      for_1_C_2 : begin
        fsm_output = 32'b01000000000000000000000000000000;
        if ( for_1_C_2_tr0 ) begin
          state_var_NS = main_C_1;
        end
        else begin
          state_var_NS = for_1_C_0;
        end
      end
      main_C_1 : begin
        fsm_output = 32'b10000000000000000000000000000000;
        state_var_NS = main_C_0;
      end
      // core_rlp_C_0
      default : begin
        fsm_output = 32'b00000000000000000000000000000001;
        state_var_NS = main_C_0;
      end
    endcase
  end

  always @(posedge clk) begin
    if ( rst ) begin
      state_var <= core_rlp_C_0;
    end
    else if ( core_wen ) begin
      state_var <= state_var_NS;
    end
  end

endmodule

// ------------------------------------------------------------------
//  Design Unit:    houghTransform_core_staller
// ------------------------------------------------------------------


module houghTransform_core_staller (
  clk, rst, core_wen, core_wten, data_in_rsci_wen_comp, acc_rsci_wen_comp
);
  input clk;
  input rst;
  output core_wen;
  output core_wten;
  reg core_wten;
  input data_in_rsci_wen_comp;
  input acc_rsci_wen_comp;



  // Interconnect Declarations for Component Instantiations 
  assign core_wen = data_in_rsci_wen_comp & acc_rsci_wen_comp;
  always @(posedge clk) begin
    if ( rst ) begin
      core_wten <= 1'b0;
    end
    else begin
      core_wten <= ~ core_wen;
    end
  end
endmodule

// ------------------------------------------------------------------
//  Design Unit:    houghTransform_core_heightIn_rsc_triosy_obj_heightIn_rsc_triosy_wait_ctrl
// ------------------------------------------------------------------


module houghTransform_core_heightIn_rsc_triosy_obj_heightIn_rsc_triosy_wait_ctrl
    (
  core_wten, heightIn_rsc_triosy_obj_iswt0, heightIn_rsc_triosy_obj_ld_core_sct
);
  input core_wten;
  input heightIn_rsc_triosy_obj_iswt0;
  output heightIn_rsc_triosy_obj_ld_core_sct;



  // Interconnect Declarations for Component Instantiations 
  assign heightIn_rsc_triosy_obj_ld_core_sct = heightIn_rsc_triosy_obj_iswt0 & (~
      core_wten);
endmodule

// ------------------------------------------------------------------
//  Design Unit:    houghTransform_core_widthIn_rsc_triosy_obj_widthIn_rsc_triosy_wait_ctrl
// ------------------------------------------------------------------


module houghTransform_core_widthIn_rsc_triosy_obj_widthIn_rsc_triosy_wait_ctrl (
  core_wten, widthIn_rsc_triosy_obj_iswt0, widthIn_rsc_triosy_obj_ld_core_sct
);
  input core_wten;
  input widthIn_rsc_triosy_obj_iswt0;
  output widthIn_rsc_triosy_obj_ld_core_sct;



  // Interconnect Declarations for Component Instantiations 
  assign widthIn_rsc_triosy_obj_ld_core_sct = widthIn_rsc_triosy_obj_iswt0 & (~ core_wten);
endmodule

// ------------------------------------------------------------------
//  Design Unit:    houghTransform_core_wait_dp
// ------------------------------------------------------------------


module houghTransform_core_wait_dp (
  clk, rst, acc_tmp_rsc_cgo_iro, acc_tmp_rsci_data_out_d, acc_tmp_rsci_en_d, core_wen,
      acc_tmp_rsc_cgo, acc_tmp_rsci_data_out_d_oreg
);
  input clk;
  input rst;
  input acc_tmp_rsc_cgo_iro;
  input [31:0] acc_tmp_rsci_data_out_d;
  output acc_tmp_rsci_en_d;
  input core_wen;
  input acc_tmp_rsc_cgo;
  output [15:0] acc_tmp_rsci_data_out_d_oreg;


  // Interconnect Declarations
  reg [15:0] acc_tmp_rsci_data_out_d_oreg_pconst_15_0;


  // Interconnect Declarations for Component Instantiations 
  assign acc_tmp_rsci_en_d = ~(core_wen & (acc_tmp_rsc_cgo | acc_tmp_rsc_cgo_iro));
  assign acc_tmp_rsci_data_out_d_oreg = acc_tmp_rsci_data_out_d_oreg_pconst_15_0;
  always @(posedge clk) begin
    if ( rst ) begin
      acc_tmp_rsci_data_out_d_oreg_pconst_15_0 <= 16'b0000000000000000;
    end
    else if ( ~ acc_tmp_rsci_en_d ) begin
      acc_tmp_rsci_data_out_d_oreg_pconst_15_0 <= acc_tmp_rsci_data_out_d[15:0];
    end
  end
endmodule

// ------------------------------------------------------------------
//  Design Unit:    houghTransform_core_acc_rsci_acc_wait_dp
// ------------------------------------------------------------------


module houghTransform_core_acc_rsci_acc_wait_dp (
  clk, rst, acc_rsci_oswt, acc_rsci_wen_comp, acc_rsci_biwt, acc_rsci_bdwt, acc_rsci_bcwt
);
  input clk;
  input rst;
  input acc_rsci_oswt;
  output acc_rsci_wen_comp;
  input acc_rsci_biwt;
  input acc_rsci_bdwt;
  output acc_rsci_bcwt;
  reg acc_rsci_bcwt;



  // Interconnect Declarations for Component Instantiations 
  assign acc_rsci_wen_comp = (~ acc_rsci_oswt) | acc_rsci_biwt | acc_rsci_bcwt;
  always @(posedge clk) begin
    if ( rst ) begin
      acc_rsci_bcwt <= 1'b0;
    end
    else begin
      acc_rsci_bcwt <= ~((~(acc_rsci_bcwt | acc_rsci_biwt)) | acc_rsci_bdwt);
    end
  end
endmodule

// ------------------------------------------------------------------
//  Design Unit:    houghTransform_core_acc_rsci_acc_wait_ctrl
// ------------------------------------------------------------------


module houghTransform_core_acc_rsci_acc_wait_ctrl (
  core_wen, acc_rsci_oswt, acc_rsci_irdy, acc_rsci_biwt, acc_rsci_bdwt, acc_rsci_bcwt,
      acc_rsci_ivld_core_sct
);
  input core_wen;
  input acc_rsci_oswt;
  input acc_rsci_irdy;
  output acc_rsci_biwt;
  output acc_rsci_bdwt;
  input acc_rsci_bcwt;
  output acc_rsci_ivld_core_sct;


  // Interconnect Declarations
  wire acc_rsci_ogwt;


  // Interconnect Declarations for Component Instantiations 
  assign acc_rsci_bdwt = acc_rsci_oswt & core_wen;
  assign acc_rsci_biwt = acc_rsci_ogwt & acc_rsci_irdy;
  assign acc_rsci_ogwt = acc_rsci_oswt & (~ acc_rsci_bcwt);
  assign acc_rsci_ivld_core_sct = acc_rsci_ogwt;
endmodule

// ------------------------------------------------------------------
//  Design Unit:    houghTransform_core_data_in_rsci_data_in_wait_dp
// ------------------------------------------------------------------


module houghTransform_core_data_in_rsci_data_in_wait_dp (
  clk, rst, data_in_rsci_oswt, data_in_rsci_wen_comp, data_in_rsci_idat_mxwt, data_in_rsci_biwt,
      data_in_rsci_bdwt, data_in_rsci_bcwt, data_in_rsci_idat
);
  input clk;
  input rst;
  input data_in_rsci_oswt;
  output data_in_rsci_wen_comp;
  output [7:0] data_in_rsci_idat_mxwt;
  input data_in_rsci_biwt;
  input data_in_rsci_bdwt;
  output data_in_rsci_bcwt;
  reg data_in_rsci_bcwt;
  input [7:0] data_in_rsci_idat;


  // Interconnect Declarations
  reg [7:0] data_in_rsci_idat_bfwt;


  // Interconnect Declarations for Component Instantiations 
  assign data_in_rsci_wen_comp = (~ data_in_rsci_oswt) | data_in_rsci_biwt | data_in_rsci_bcwt;
  assign data_in_rsci_idat_mxwt = MUX_v_8_2_2(data_in_rsci_idat, data_in_rsci_idat_bfwt,
      data_in_rsci_bcwt);
  always @(posedge clk) begin
    if ( rst ) begin
      data_in_rsci_bcwt <= 1'b0;
    end
    else begin
      data_in_rsci_bcwt <= ~((~(data_in_rsci_bcwt | data_in_rsci_biwt)) | data_in_rsci_bdwt);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      data_in_rsci_idat_bfwt <= 8'b00000000;
    end
    else if ( data_in_rsci_biwt ) begin
      data_in_rsci_idat_bfwt <= data_in_rsci_idat;
    end
  end

  function automatic [7:0] MUX_v_8_2_2;
    input [7:0] input_0;
    input [7:0] input_1;
    input [0:0] sel;
    reg [7:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_8_2_2 = result;
  end
  endfunction

endmodule

// ------------------------------------------------------------------
//  Design Unit:    houghTransform_core_data_in_rsci_data_in_wait_ctrl
// ------------------------------------------------------------------


module houghTransform_core_data_in_rsci_data_in_wait_ctrl (
  core_wen, data_in_rsci_oswt, data_in_rsci_biwt, data_in_rsci_bdwt, data_in_rsci_bcwt,
      data_in_rsci_irdy_core_sct, data_in_rsci_ivld
);
  input core_wen;
  input data_in_rsci_oswt;
  output data_in_rsci_biwt;
  output data_in_rsci_bdwt;
  input data_in_rsci_bcwt;
  output data_in_rsci_irdy_core_sct;
  input data_in_rsci_ivld;


  // Interconnect Declarations
  wire data_in_rsci_ogwt;


  // Interconnect Declarations for Component Instantiations 
  assign data_in_rsci_bdwt = data_in_rsci_oswt & core_wen;
  assign data_in_rsci_biwt = data_in_rsci_ogwt & data_in_rsci_ivld;
  assign data_in_rsci_ogwt = data_in_rsci_oswt & (~ data_in_rsci_bcwt);
  assign data_in_rsci_irdy_core_sct = data_in_rsci_ogwt;
endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_acc_rsci
// ------------------------------------------------------------------


module getMaxLine_core_acc_rsci (
  clk, rst, acc_rsc_dat, acc_rsc_vld, acc_rsc_rdy, core_wen, acc_rsci_oswt, acc_rsci_wen_comp,
      acc_rsci_idat_mxwt
);
  input clk;
  input rst;
  input [15:0] acc_rsc_dat;
  input acc_rsc_vld;
  output acc_rsc_rdy;
  input core_wen;
  input acc_rsci_oswt;
  output acc_rsci_wen_comp;
  output [15:0] acc_rsci_idat_mxwt;


  // Interconnect Declarations
  wire acc_rsci_biwt;
  wire acc_rsci_bdwt;
  wire acc_rsci_bcwt;
  wire acc_rsci_irdy_core_sct;
  wire acc_rsci_ivld;
  wire [15:0] acc_rsci_idat;


  // Interconnect Declarations for Component Instantiations 
  ccs_in_wait_v1 #(.rscid(32'sd19),
  .width(32'sd16)) acc_rsci (
      .rdy(acc_rsc_rdy),
      .vld(acc_rsc_vld),
      .dat(acc_rsc_dat),
      .irdy(acc_rsci_irdy_core_sct),
      .ivld(acc_rsci_ivld),
      .idat(acc_rsci_idat)
    );
  getMaxLine_core_acc_rsci_acc_wait_ctrl getMaxLine_core_acc_rsci_acc_wait_ctrl_inst
      (
      .core_wen(core_wen),
      .acc_rsci_oswt(acc_rsci_oswt),
      .acc_rsci_biwt(acc_rsci_biwt),
      .acc_rsci_bdwt(acc_rsci_bdwt),
      .acc_rsci_bcwt(acc_rsci_bcwt),
      .acc_rsci_irdy_core_sct(acc_rsci_irdy_core_sct),
      .acc_rsci_ivld(acc_rsci_ivld)
    );
  getMaxLine_core_acc_rsci_acc_wait_dp getMaxLine_core_acc_rsci_acc_wait_dp_inst
      (
      .clk(clk),
      .rst(rst),
      .acc_rsci_oswt(acc_rsci_oswt),
      .acc_rsci_wen_comp(acc_rsci_wen_comp),
      .acc_rsci_idat_mxwt(acc_rsci_idat_mxwt),
      .acc_rsci_biwt(acc_rsci_biwt),
      .acc_rsci_bdwt(acc_rsci_bdwt),
      .acc_rsci_bcwt(acc_rsci_bcwt),
      .acc_rsci_idat(acc_rsci_idat)
    );
endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_y2_rsci
// ------------------------------------------------------------------


module getMaxLine_core_y2_rsci (
  clk, rst, y2_rsc_dat, y2_rsc_vld, y2_rsc_rdy, core_wen, y2_rsci_oswt, y2_rsci_wen_comp,
      y2_rsci_idat
);
  input clk;
  input rst;
  output [9:0] y2_rsc_dat;
  output y2_rsc_vld;
  input y2_rsc_rdy;
  input core_wen;
  input y2_rsci_oswt;
  output y2_rsci_wen_comp;
  input [9:0] y2_rsci_idat;


  // Interconnect Declarations
  wire y2_rsci_irdy;
  wire y2_rsci_biwt;
  wire y2_rsci_bdwt;
  wire y2_rsci_bcwt;
  wire y2_rsci_ivld_core_sct;


  // Interconnect Declarations for Component Instantiations 
  ccs_out_wait_v1 #(.rscid(32'sd18),
  .width(32'sd10)) y2_rsci (
      .irdy(y2_rsci_irdy),
      .ivld(y2_rsci_ivld_core_sct),
      .idat(y2_rsci_idat),
      .rdy(y2_rsc_rdy),
      .vld(y2_rsc_vld),
      .dat(y2_rsc_dat)
    );
  getMaxLine_core_y2_rsci_y2_wait_ctrl getMaxLine_core_y2_rsci_y2_wait_ctrl_inst
      (
      .core_wen(core_wen),
      .y2_rsci_oswt(y2_rsci_oswt),
      .y2_rsci_irdy(y2_rsci_irdy),
      .y2_rsci_biwt(y2_rsci_biwt),
      .y2_rsci_bdwt(y2_rsci_bdwt),
      .y2_rsci_bcwt(y2_rsci_bcwt),
      .y2_rsci_ivld_core_sct(y2_rsci_ivld_core_sct)
    );
  getMaxLine_core_y2_rsci_y2_wait_dp getMaxLine_core_y2_rsci_y2_wait_dp_inst (
      .clk(clk),
      .rst(rst),
      .y2_rsci_oswt(y2_rsci_oswt),
      .y2_rsci_wen_comp(y2_rsci_wen_comp),
      .y2_rsci_biwt(y2_rsci_biwt),
      .y2_rsci_bdwt(y2_rsci_bdwt),
      .y2_rsci_bcwt(y2_rsci_bcwt)
    );
endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_x2_rsci
// ------------------------------------------------------------------


module getMaxLine_core_x2_rsci (
  clk, rst, x2_rsc_dat, x2_rsc_vld, x2_rsc_rdy, core_wen, x2_rsci_oswt, x2_rsci_wen_comp,
      x2_rsci_idat
);
  input clk;
  input rst;
  output [10:0] x2_rsc_dat;
  output x2_rsc_vld;
  input x2_rsc_rdy;
  input core_wen;
  input x2_rsci_oswt;
  output x2_rsci_wen_comp;
  input [10:0] x2_rsci_idat;


  // Interconnect Declarations
  wire x2_rsci_irdy;
  wire x2_rsci_biwt;
  wire x2_rsci_bdwt;
  wire x2_rsci_bcwt;
  wire x2_rsci_ivld_core_sct;


  // Interconnect Declarations for Component Instantiations 
  ccs_out_wait_v1 #(.rscid(32'sd17),
  .width(32'sd11)) x2_rsci (
      .irdy(x2_rsci_irdy),
      .ivld(x2_rsci_ivld_core_sct),
      .idat(x2_rsci_idat),
      .rdy(x2_rsc_rdy),
      .vld(x2_rsc_vld),
      .dat(x2_rsc_dat)
    );
  getMaxLine_core_x2_rsci_x2_wait_ctrl getMaxLine_core_x2_rsci_x2_wait_ctrl_inst
      (
      .core_wen(core_wen),
      .x2_rsci_oswt(x2_rsci_oswt),
      .x2_rsci_irdy(x2_rsci_irdy),
      .x2_rsci_biwt(x2_rsci_biwt),
      .x2_rsci_bdwt(x2_rsci_bdwt),
      .x2_rsci_bcwt(x2_rsci_bcwt),
      .x2_rsci_ivld_core_sct(x2_rsci_ivld_core_sct)
    );
  getMaxLine_core_x2_rsci_x2_wait_dp getMaxLine_core_x2_rsci_x2_wait_dp_inst (
      .clk(clk),
      .rst(rst),
      .x2_rsci_oswt(x2_rsci_oswt),
      .x2_rsci_wen_comp(x2_rsci_wen_comp),
      .x2_rsci_biwt(x2_rsci_biwt),
      .x2_rsci_bdwt(x2_rsci_bdwt),
      .x2_rsci_bcwt(x2_rsci_bcwt)
    );
endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_y1_rsci
// ------------------------------------------------------------------


module getMaxLine_core_y1_rsci (
  clk, rst, y1_rsc_dat, y1_rsc_vld, y1_rsc_rdy, core_wen, y1_rsci_oswt, y1_rsci_wen_comp,
      y1_rsci_idat
);
  input clk;
  input rst;
  output [9:0] y1_rsc_dat;
  output y1_rsc_vld;
  input y1_rsc_rdy;
  input core_wen;
  input y1_rsci_oswt;
  output y1_rsci_wen_comp;
  input [9:0] y1_rsci_idat;


  // Interconnect Declarations
  wire y1_rsci_irdy;
  wire y1_rsci_biwt;
  wire y1_rsci_bdwt;
  wire y1_rsci_bcwt;
  wire y1_rsci_ivld_core_sct;


  // Interconnect Declarations for Component Instantiations 
  ccs_out_wait_v1 #(.rscid(32'sd16),
  .width(32'sd10)) y1_rsci (
      .irdy(y1_rsci_irdy),
      .ivld(y1_rsci_ivld_core_sct),
      .idat(y1_rsci_idat),
      .rdy(y1_rsc_rdy),
      .vld(y1_rsc_vld),
      .dat(y1_rsc_dat)
    );
  getMaxLine_core_y1_rsci_y1_wait_ctrl getMaxLine_core_y1_rsci_y1_wait_ctrl_inst
      (
      .core_wen(core_wen),
      .y1_rsci_oswt(y1_rsci_oswt),
      .y1_rsci_irdy(y1_rsci_irdy),
      .y1_rsci_biwt(y1_rsci_biwt),
      .y1_rsci_bdwt(y1_rsci_bdwt),
      .y1_rsci_bcwt(y1_rsci_bcwt),
      .y1_rsci_ivld_core_sct(y1_rsci_ivld_core_sct)
    );
  getMaxLine_core_y1_rsci_y1_wait_dp getMaxLine_core_y1_rsci_y1_wait_dp_inst (
      .clk(clk),
      .rst(rst),
      .y1_rsci_oswt(y1_rsci_oswt),
      .y1_rsci_wen_comp(y1_rsci_wen_comp),
      .y1_rsci_biwt(y1_rsci_biwt),
      .y1_rsci_bdwt(y1_rsci_bdwt),
      .y1_rsci_bcwt(y1_rsci_bcwt)
    );
endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_x1_rsci
// ------------------------------------------------------------------


module getMaxLine_core_x1_rsci (
  clk, rst, x1_rsc_dat, x1_rsc_vld, x1_rsc_rdy, core_wen, x1_rsci_oswt, x1_rsci_wen_comp,
      x1_rsci_idat
);
  input clk;
  input rst;
  output [10:0] x1_rsc_dat;
  output x1_rsc_vld;
  input x1_rsc_rdy;
  input core_wen;
  input x1_rsci_oswt;
  output x1_rsci_wen_comp;
  input [10:0] x1_rsci_idat;


  // Interconnect Declarations
  wire x1_rsci_irdy;
  wire x1_rsci_biwt;
  wire x1_rsci_bdwt;
  wire x1_rsci_bcwt;
  wire x1_rsci_ivld_core_sct;


  // Interconnect Declarations for Component Instantiations 
  ccs_out_wait_v1 #(.rscid(32'sd15),
  .width(32'sd11)) x1_rsci (
      .irdy(x1_rsci_irdy),
      .ivld(x1_rsci_ivld_core_sct),
      .idat(x1_rsci_idat),
      .rdy(x1_rsc_rdy),
      .vld(x1_rsc_vld),
      .dat(x1_rsc_dat)
    );
  getMaxLine_core_x1_rsci_x1_wait_ctrl getMaxLine_core_x1_rsci_x1_wait_ctrl_inst
      (
      .core_wen(core_wen),
      .x1_rsci_oswt(x1_rsci_oswt),
      .x1_rsci_irdy(x1_rsci_irdy),
      .x1_rsci_biwt(x1_rsci_biwt),
      .x1_rsci_bdwt(x1_rsci_bdwt),
      .x1_rsci_bcwt(x1_rsci_bcwt),
      .x1_rsci_ivld_core_sct(x1_rsci_ivld_core_sct)
    );
  getMaxLine_core_x1_rsci_x1_wait_dp getMaxLine_core_x1_rsci_x1_wait_dp_inst (
      .clk(clk),
      .rst(rst),
      .x1_rsci_oswt(x1_rsci_oswt),
      .x1_rsci_wen_comp(x1_rsci_wen_comp),
      .x1_rsci_biwt(x1_rsci_biwt),
      .x1_rsci_bdwt(x1_rsci_bdwt),
      .x1_rsci_bcwt(x1_rsci_bcwt)
    );
endmodule

// ------------------------------------------------------------------
//  Design Unit:    houghTransform_core_heightIn_rsc_triosy_obj
// ------------------------------------------------------------------


module houghTransform_core_heightIn_rsc_triosy_obj (
  heightIn_rsc_triosy_lz, core_wten, heightIn_rsc_triosy_obj_iswt0
);
  output heightIn_rsc_triosy_lz;
  input core_wten;
  input heightIn_rsc_triosy_obj_iswt0;


  // Interconnect Declarations
  wire heightIn_rsc_triosy_obj_ld_core_sct;


  // Interconnect Declarations for Component Instantiations 
  mgc_io_sync_v2 #(.valid(32'sd0)) heightIn_rsc_triosy_obj (
      .ld(heightIn_rsc_triosy_obj_ld_core_sct),
      .lz(heightIn_rsc_triosy_lz)
    );
  houghTransform_core_heightIn_rsc_triosy_obj_heightIn_rsc_triosy_wait_ctrl houghTransform_core_heightIn_rsc_triosy_obj_heightIn_rsc_triosy_wait_ctrl_inst
      (
      .core_wten(core_wten),
      .heightIn_rsc_triosy_obj_iswt0(heightIn_rsc_triosy_obj_iswt0),
      .heightIn_rsc_triosy_obj_ld_core_sct(heightIn_rsc_triosy_obj_ld_core_sct)
    );
endmodule

// ------------------------------------------------------------------
//  Design Unit:    houghTransform_core_widthIn_rsc_triosy_obj
// ------------------------------------------------------------------


module houghTransform_core_widthIn_rsc_triosy_obj (
  widthIn_rsc_triosy_lz, core_wten, widthIn_rsc_triosy_obj_iswt0
);
  output widthIn_rsc_triosy_lz;
  input core_wten;
  input widthIn_rsc_triosy_obj_iswt0;


  // Interconnect Declarations
  wire widthIn_rsc_triosy_obj_ld_core_sct;


  // Interconnect Declarations for Component Instantiations 
  mgc_io_sync_v2 #(.valid(32'sd0)) widthIn_rsc_triosy_obj (
      .ld(widthIn_rsc_triosy_obj_ld_core_sct),
      .lz(widthIn_rsc_triosy_lz)
    );
  houghTransform_core_widthIn_rsc_triosy_obj_widthIn_rsc_triosy_wait_ctrl houghTransform_core_widthIn_rsc_triosy_obj_widthIn_rsc_triosy_wait_ctrl_inst
      (
      .core_wten(core_wten),
      .widthIn_rsc_triosy_obj_iswt0(widthIn_rsc_triosy_obj_iswt0),
      .widthIn_rsc_triosy_obj_ld_core_sct(widthIn_rsc_triosy_obj_ld_core_sct)
    );
endmodule

// ------------------------------------------------------------------
//  Design Unit:    houghTransform_core_acc_rsci
// ------------------------------------------------------------------


module houghTransform_core_acc_rsci (
  clk, rst, acc_rsc_dat, acc_rsc_vld, acc_rsc_rdy, core_wen, acc_rsci_oswt, acc_rsci_wen_comp,
      acc_rsci_idat
);
  input clk;
  input rst;
  output [15:0] acc_rsc_dat;
  output acc_rsc_vld;
  input acc_rsc_rdy;
  input core_wen;
  input acc_rsci_oswt;
  output acc_rsci_wen_comp;
  input [15:0] acc_rsci_idat;


  // Interconnect Declarations
  wire acc_rsci_irdy;
  wire acc_rsci_biwt;
  wire acc_rsci_bdwt;
  wire acc_rsci_bcwt;
  wire acc_rsci_ivld_core_sct;


  // Interconnect Declarations for Component Instantiations 
  ccs_out_wait_v1 #(.rscid(32'sd12),
  .width(32'sd16)) acc_rsci (
      .irdy(acc_rsci_irdy),
      .ivld(acc_rsci_ivld_core_sct),
      .idat(acc_rsci_idat),
      .rdy(acc_rsc_rdy),
      .vld(acc_rsc_vld),
      .dat(acc_rsc_dat)
    );
  houghTransform_core_acc_rsci_acc_wait_ctrl houghTransform_core_acc_rsci_acc_wait_ctrl_inst
      (
      .core_wen(core_wen),
      .acc_rsci_oswt(acc_rsci_oswt),
      .acc_rsci_irdy(acc_rsci_irdy),
      .acc_rsci_biwt(acc_rsci_biwt),
      .acc_rsci_bdwt(acc_rsci_bdwt),
      .acc_rsci_bcwt(acc_rsci_bcwt),
      .acc_rsci_ivld_core_sct(acc_rsci_ivld_core_sct)
    );
  houghTransform_core_acc_rsci_acc_wait_dp houghTransform_core_acc_rsci_acc_wait_dp_inst
      (
      .clk(clk),
      .rst(rst),
      .acc_rsci_oswt(acc_rsci_oswt),
      .acc_rsci_wen_comp(acc_rsci_wen_comp),
      .acc_rsci_biwt(acc_rsci_biwt),
      .acc_rsci_bdwt(acc_rsci_bdwt),
      .acc_rsci_bcwt(acc_rsci_bcwt)
    );
endmodule

// ------------------------------------------------------------------
//  Design Unit:    houghTransform_core_data_in_rsci
// ------------------------------------------------------------------


module houghTransform_core_data_in_rsci (
  clk, rst, data_in_rsc_dat, data_in_rsc_vld, data_in_rsc_rdy, core_wen, data_in_rsci_oswt,
      data_in_rsci_wen_comp, data_in_rsci_idat_mxwt
);
  input clk;
  input rst;
  input [7:0] data_in_rsc_dat;
  input data_in_rsc_vld;
  output data_in_rsc_rdy;
  input core_wen;
  input data_in_rsci_oswt;
  output data_in_rsci_wen_comp;
  output [7:0] data_in_rsci_idat_mxwt;


  // Interconnect Declarations
  wire data_in_rsci_biwt;
  wire data_in_rsci_bdwt;
  wire data_in_rsci_bcwt;
  wire data_in_rsci_irdy_core_sct;
  wire data_in_rsci_ivld;
  wire [7:0] data_in_rsci_idat;


  // Interconnect Declarations for Component Instantiations 
  ccs_in_wait_v1 #(.rscid(32'sd9),
  .width(32'sd8)) data_in_rsci (
      .rdy(data_in_rsc_rdy),
      .vld(data_in_rsc_vld),
      .dat(data_in_rsc_dat),
      .irdy(data_in_rsci_irdy_core_sct),
      .ivld(data_in_rsci_ivld),
      .idat(data_in_rsci_idat)
    );
  houghTransform_core_data_in_rsci_data_in_wait_ctrl houghTransform_core_data_in_rsci_data_in_wait_ctrl_inst
      (
      .core_wen(core_wen),
      .data_in_rsci_oswt(data_in_rsci_oswt),
      .data_in_rsci_biwt(data_in_rsci_biwt),
      .data_in_rsci_bdwt(data_in_rsci_bdwt),
      .data_in_rsci_bcwt(data_in_rsci_bcwt),
      .data_in_rsci_irdy_core_sct(data_in_rsci_irdy_core_sct),
      .data_in_rsci_ivld(data_in_rsci_ivld)
    );
  houghTransform_core_data_in_rsci_data_in_wait_dp houghTransform_core_data_in_rsci_data_in_wait_dp_inst
      (
      .clk(clk),
      .rst(rst),
      .data_in_rsci_oswt(data_in_rsci_oswt),
      .data_in_rsci_wen_comp(data_in_rsci_wen_comp),
      .data_in_rsci_idat_mxwt(data_in_rsci_idat_mxwt),
      .data_in_rsci_biwt(data_in_rsci_biwt),
      .data_in_rsci_bdwt(data_in_rsci_bdwt),
      .data_in_rsci_bcwt(data_in_rsci_bcwt),
      .data_in_rsci_idat(data_in_rsci_idat)
    );
endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core
// ------------------------------------------------------------------


module getMaxLine_core (
  clk, rst, x1_rsc_dat, x1_rsc_vld, x1_rsc_rdy, y1_rsc_dat, y1_rsc_vld, y1_rsc_rdy,
      x2_rsc_dat, x2_rsc_vld, x2_rsc_rdy, y2_rsc_dat, y2_rsc_vld, y2_rsc_rdy, acc_rsc_dat,
      acc_rsc_vld, acc_rsc_rdy, T_LINE_if_if_dividend1_mul_cmp_a, T_LINE_if_if_dividend1_mul_cmp_b,
      T_LINE_if_if_dividend1_mul_cmp_z
);
  input clk;
  input rst;
  output [10:0] x1_rsc_dat;
  output x1_rsc_vld;
  input x1_rsc_rdy;
  output [9:0] y1_rsc_dat;
  output y1_rsc_vld;
  input y1_rsc_rdy;
  output [10:0] x2_rsc_dat;
  output x2_rsc_vld;
  input x2_rsc_rdy;
  output [9:0] y2_rsc_dat;
  output y2_rsc_vld;
  input y2_rsc_rdy;
  input [15:0] acc_rsc_dat;
  input acc_rsc_vld;
  output acc_rsc_rdy;
  output [19:0] T_LINE_if_if_dividend1_mul_cmp_a;
  output [26:0] T_LINE_if_if_dividend1_mul_cmp_b;
  reg [26:0] T_LINE_if_if_dividend1_mul_cmp_b;
  input [43:0] T_LINE_if_if_dividend1_mul_cmp_z;


  // Interconnect Declarations
  wire core_wen;
  wire x1_rsci_wen_comp;
  wire y1_rsci_wen_comp;
  wire x2_rsci_wen_comp;
  wire y2_rsci_wen_comp;
  wire acc_rsci_wen_comp;
  wire [15:0] acc_rsci_idat_mxwt;
  wire [43:0] T_LINE_if_if_dividend1_mul_cmp_z_oreg;
  reg [7:0] x1_rsci_idat_10_3;
  reg x1_rsci_idat_2;
  reg x1_rsci_idat_1;
  reg x1_rsci_idat_0;
  reg [5:0] y1_rsci_idat_9_4;
  reg y1_rsci_idat_3;
  reg y1_rsci_idat_2;
  reg y1_rsci_idat_1;
  reg y1_rsci_idat_0;
  reg [7:0] x2_rsci_idat_10_3;
  reg x2_rsci_idat_2;
  reg x2_rsci_idat_1;
  reg x2_rsci_idat_0;
  reg [5:0] y2_rsci_idat_9_4;
  reg y2_rsci_idat_3;
  reg y2_rsci_idat_2;
  reg y2_rsci_idat_1;
  reg y2_rsci_idat_0;
  wire [31:0] fsm_output;
  wire or_dcpl_4;
  wire or_dcpl_13;
  wire or_dcpl_69;
  wire or_dcpl_72;
  wire or_dcpl_75;
  wire or_dcpl_77;
  wire or_dcpl_81;
  wire or_dcpl_82;
  wire or_dcpl_83;
  wire or_dcpl_94;
  wire and_dcpl_93;
  wire not_tmp_80;
  wire not_tmp_82;
  wire or_dcpl_103;
  wire or_dcpl_109;
  wire or_dcpl_110;
  wire or_dcpl_131;
  wire or_dcpl_141;
  wire and_dcpl_103;
  wire and_dcpl_105;
  wire or_dcpl_149;
  wire or_tmp_79;
  wire or_tmp_209;
  wire or_tmp_581;
  wire or_tmp_695;
  wire and_444_cse;
  reg operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm;
  reg T_LINE_slc_T_LINE_acc_6_itm;
  reg T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
  reg [32:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva;
  reg operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs;
  reg operator_27_3_true_AC_TRN_AC_WRAP_return_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs;
  reg [32:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4;
  reg [19:0] T_LINE_if_if_dividend1_sva;
  reg [19:0] T_LINE_if_else_dividend1_sva;
  reg [19:0] T_LINE_if_if_dividend2_sva;
  reg [19:0] T_LINE_if_else_dividend2_sva;
  reg [2:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_3_dfm_1_34_32;
  reg [2:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_3_dfm_1_34_32;
  reg [8:0] reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11;
  reg reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_ac_fixed_cctor_20_1_ftd_11;
  reg [10:0] reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_2;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_3;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_4;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_5;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_6;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_7;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_8;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_9;
  wire x2_t_and_1_cse;
  wire Hough_Algorithm_HW_1296_864_getMaxLine_and_cse;
  reg reg_acc_rsci_irdy_core_psct_cse;
  reg reg_y2_rsci_ivld_core_psct_cse;
  wire [1:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_mux_cse;
  wire T_LINE_asn_48;
  wire T_LINE_asn_46;
  reg [10:0] R_LINE_r_10_0_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_19_sva;
  reg [26:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva;
  reg [2:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_32_30;
  reg [2:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_32_30;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_29;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_29;
  reg [3:0] ac_math_ac_cos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_17_0_sva_7_4;
  reg [1:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_28_27;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_28;
  reg [3:0] ac_math_ac_cos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_17_0_sva_3_0;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_26;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_27;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_25;
  reg [1:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_26_25;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_24;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_24;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_23;
  reg [1:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_23_22;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_22;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_21;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_21;
  reg [2:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_20_18;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_20;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_17;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_19;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_16;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_18;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_15;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_17;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_14;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_16;
  reg [2:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_13_11;
  reg [1:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_15_14;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_10;
  reg [2:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11;
  reg [1:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_9_8;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_10;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_7;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_9;
  reg [1:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_6_5;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_8;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_4;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_7;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_3;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_6;
  reg [1:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_2_1;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_5;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_0;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_4;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_3;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_2;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_1;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_0;
  wire mux_19_itm;
  wire mux_20_itm;
  wire [19:0] z_out;
  wire [20:0] nl_z_out;
  wire [10:0] z_out_1;
  wire [11:0] nl_z_out_1;
  wire [32:0] z_out_2;
  wire [26:0] z_out_3;
  wire [27:0] nl_z_out_3;
  wire or_tmp_777;
  wire or_tmp_778;
  wire or_tmp_779;
  wire or_tmp_780;
  wire [35:0] z_out_4;
  wire or_tmp_789;
  wire or_tmp_790;
  wire [34:0] z_out_5;
  wire or_tmp_792;
  wire or_tmp_793;
  wire [43:0] z_out_6;
  wire [31:0] data_out_out;
  wire [4:0] z_out_7;
  wire [5:0] nl_z_out_7;
  wire or_tmp_800;
  wire or_tmp_802;
  wire [16:0] z_out_8;
  reg [15:0] threshold_23_8_lpi_3;
  reg x1_t_26_lpi_3;
  reg x1_t_27_lpi_3;
  reg x1_t_25_lpi_3;
  reg x1_t_28_lpi_3;
  reg x1_t_24_lpi_3;
  reg x1_t_29_lpi_3;
  reg x1_t_23_lpi_3;
  reg x1_t_30_lpi_3;
  reg [11:0] x1_t_42_31_lpi_3;
  reg y1_t_27_lpi_3;
  reg y1_t_26_lpi_3;
  reg y1_t_28_lpi_3;
  reg y1_t_25_lpi_3;
  reg y1_t_29_lpi_3;
  reg y1_t_24_lpi_3;
  reg y1_t_30_lpi_3;
  reg y1_t_23_lpi_3;
  reg y1_t_31_lpi_3;
  reg [10:0] y1_t_42_32_lpi_3;
  reg x2_t_26_lpi_3;
  reg x2_t_27_lpi_3;
  reg x2_t_25_lpi_3;
  reg x2_t_28_lpi_3;
  reg x2_t_24_lpi_3;
  reg x2_t_29_lpi_3;
  reg x2_t_23_lpi_3;
  reg x2_t_30_lpi_3;
  reg [11:0] x2_t_42_31_lpi_3;
  reg y2_t_27_lpi_3;
  reg y2_t_26_lpi_3;
  reg y2_t_28_lpi_3;
  reg y2_t_25_lpi_3;
  reg y2_t_29_lpi_3;
  reg y2_t_24_lpi_3;
  reg y2_t_30_lpi_3;
  reg y2_t_23_lpi_3;
  reg y2_t_31_lpi_3;
  reg [10:0] y2_t_42_32_lpi_3;
  reg [7:0] T_LINE_t_7_0_sva;
  reg [4:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva;
  reg [4:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva;
  reg [31:0] ac_math_atan_pi_2mi_return_2_69_38_sva;
  reg [4:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1;
  reg [4:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_9_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_10_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_8_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_11_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_7_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_12_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_6_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_13_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_5_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_14_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_4_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_15_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_3_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_16_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_2_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_17_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_1_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_18_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_15_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_16_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_17_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_18_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_8_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_7_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_6_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_5_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_4_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_3_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_2_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_1_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_0_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_7_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_6_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_5_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_4_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_3_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_2_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_1_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_0_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_7_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_6_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_5_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_4_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_3_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_2_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_1_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_0_lpi_3_dfm;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_itm;
  reg [32:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm;
  reg [34:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm;
  reg [2:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_32_30;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_29;
  reg [1:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_26_25;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_24;
  reg [1:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_23_22;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_21;
  reg [2:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_20_18;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_17;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_16;
  reg [1:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14;
  reg [2:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_13_11;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_10;
  reg [1:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_9_8;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_7;
  reg [1:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_6_5;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_4;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_3;
  reg [1:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_2_1;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_0;
  reg [2:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_32_30;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_29;
  reg [1:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_28_27;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_24;
  reg [1:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_15_14;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_7;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_4;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_3;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_28;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_27;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_26;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_25;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_23;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_22;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_9;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_8;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_6;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_5;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0;
  wire T_LINE_if_if_dividend1_mul_cmp_a_mx0c0;
  wire T_LINE_if_if_dividend1_mul_cmp_a_mx0c1;
  wire T_LINE_if_if_dividend1_mul_cmp_a_mx0c2;
  wire T_LINE_if_if_dividend1_mul_cmp_a_mx0c3;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_mx1w0;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_mx1w0;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_mx1w0;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_mx1w0;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_mx1w0;
  wire [9:0] T_LINE_if_acc_5_psp_1;
  wire [10:0] nl_T_LINE_if_acc_5_psp_1;
  wire ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_ncse_2_sva_2;
  wire [2:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1;
  wire [1:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1;
  wire [19:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0;
  wire [19:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0;
  wire [27:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_lpi_3_dfm_mx0;
  wire [19:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1;
  wire [20:0] nl_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_16_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_17_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_18_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_19_lpi_3_dfm_1;
  wire [19:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0;
  wire [19:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0;
  wire [26:0] exs_tmp_16_26_0;
  reg reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd;
  reg [25:0] reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_37_m1c;
  wire and_438_rgt;
  wire and_499_rgt;
  wire and_501_rgt;
  wire and_503_rgt;
  wire ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt;
  wire ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_28_rgt;
  wire ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_29_rgt;
  wire ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt;
  wire ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt;
  wire ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt;
  wire ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt;
  wire ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_1_rgt;
  wire ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_rgt;
  wire ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_1_rgt;
  wire T_LINE_if_or_ssc;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_or_cse;
  wire nor_cse;
  wire ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_69_cse;
  wire ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_68_cse;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27;
  reg [26:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0;
  wire x1_t_and_3_cse;
  wire x2_t_and_4_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_and_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_and_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_and_cse;
  wire x2_t_and_3_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_and_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_and_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_and_cse;
  wire T_LINE_if_and_cse;
  wire ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_70_cse;
  wire ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_and_60_cse;
  wire ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_and_2_cse;
  wire ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_and_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse;
  wire [12:0] T_LINE_if_mux1h_4_rgt;
  wire [12:0] T_LINE_if_mux1h_8_rgt;
  wire [32:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_mux_rgt;
  wire [34:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_rgt;
  reg [2:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_34_32;
  reg [31:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_31_0;
  reg ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_32;
  reg [31:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0;
  reg [2:0] T_LINE_if_acc_2_itm_16_14;
  reg T_LINE_if_acc_2_itm_13;
  reg [8:0] T_LINE_if_acc_2_itm_12_4;
  reg [8:0] T_LINE_if_acc_1_itm_12_4;
  reg [3:0] T_LINE_if_acc_1_itm_3_0;
  wire and_1762_cse;
  wire nor_66_cse;
  wire and_1771_itm;
  wire T_LINE_if_if_dividend1_or_itm;
  wire T_LINE_if_if_dividend1_or_1_itm;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_6_cse;
  wire nor_55_cse;
  wire or_948_cse_1;

  wire[11:0] T_LINE_if_T_LINE_if_and_34_nl;
  wire[0:0] T_LINE_if_aelse_not_57_nl;
  wire[0:0] mux_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_16_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_16_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_17_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_17_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_18_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_18_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_19_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_19_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_16_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_16_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_17_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_17_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_18_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_18_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_19_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_19_nl;
  wire[10:0] T_LINE_if_T_LINE_if_and_35_nl;
  wire[0:0] mux_22_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_11_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_11_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_16_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_16_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_17_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_17_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_18_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_18_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_19_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_19_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_11_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_11_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_16_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_16_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_17_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_17_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_18_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_18_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_19_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_19_nl;
  wire[8:0] T_LINE_if_acc_nl;
  wire[9:0] nl_T_LINE_if_acc_nl;
  wire[0:0] T_LINE_if_if_T_LINE_if_if_T_LINE_if_if_nor_nl;
  wire[5:0] T_LINE_if_aelse_acc_nl;
  wire[6:0] nl_T_LINE_if_aelse_acc_nl;
  wire[5:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_and_nl;
  wire[0:0] or_365_nl;
  wire[8:0] operator_60_32_true_AC_TRN_AC_WRAP_acc_2_nl;
  wire[9:0] nl_operator_60_32_true_AC_TRN_AC_WRAP_acc_2_nl;
  wire[0:0] or_367_nl;
  wire[0:0] or_368_nl;
  wire[0:0] mux_23_nl;
  wire[0:0] or_nl;
  wire[0:0] or_1032_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_11_nl;
  wire[0:0] Hough_Algorithm_HW_1296_864_getMaxLine_not_24_nl;
  wire[0:0] Hough_Algorithm_HW_1296_864_getMaxLine_not_23_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_10_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_9_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_8_nl;
  wire[0:0] Hough_Algorithm_HW_1296_864_getMaxLine_not_19_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_7_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_6_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_5_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_4_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_3_nl;
  wire[0:0] Hough_Algorithm_HW_1296_864_getMaxLine_not_13_nl;
  wire[0:0] Hough_Algorithm_HW_1296_864_getMaxLine_not_12_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_2_nl;
  wire[0:0] Hough_Algorithm_HW_1296_864_getMaxLine_not_10_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_1_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_nl;
  wire[0:0] Hough_Algorithm_HW_1296_864_getMaxLine_not_7_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_nl;
  wire[0:0] Hough_Algorithm_HW_1296_864_getMaxLine_not_5_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_104_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_105_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_106_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_107_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_1_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_108_nl;
  wire[0:0] or_709_nl;
  wire[31:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_not_4_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_nand_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_T000000;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_nor_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_or_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_1_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_2_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_18_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_17_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_16_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_11_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_10_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_9_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_8_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_7_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_6_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_5_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_4_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_3_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_2_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_1_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_nl;
  wire[0:0] not_586_nl;
  wire[0:0] mux_24_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_mux1h_79_nl;
  wire[0:0] T_LINE_if_aelse_not_74_nl;
  wire[27:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_if_1_acc_nl;
  wire[28:0] nl_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_if_1_acc_nl;
  wire[0:0] or_170_nl;
  wire[0:0] or_172_nl;
  wire[0:0] T_LINE_if_T_LINE_if_T_LINE_if_nor_2_nl;
  wire[18:0] T_LINE_if_mux1h_25_nl;
  wire[0:0] T_LINE_if_T_LINE_if_or_1_nl;
  wire[7:0] T_LINE_if_T_LINE_if_T_LINE_if_nor_3_nl;
  wire[0:0] T_LINE_if_or_2_nl;
  wire[10:0] T_LINE_mux1h_4_nl;
  wire[25:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux1h_11_nl;
  wire[0:0] or_1063_nl;
  wire[0:0] or_1064_nl;
  wire[36:0] acc_3_nl;
  wire[37:0] nl_acc_3_nl;
  wire[34:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_3_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_5_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_6_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_7_nl;
  wire[17:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_4_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_and_3_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_8_nl;
  wire[15:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_1_nl;
  wire[15:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_5_nl;
  wire[35:0] acc_4_nl;
  wire[36:0] nl_acc_4_nl;
  wire[34:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux_1_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_4_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_5_nl;
  wire[32:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_4_nl;
  wire[44:0] acc_5_nl;
  wire[45:0] nl_acc_5_nl;
  wire[43:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_5_nl;
  wire[0:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_6_nl;
  wire[39:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_6_nl;
  wire[4:0] ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_23_nl;
  wire[17:0] acc_7_nl;
  wire[18:0] nl_acc_7_nl;
  wire[16:0] operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_2_nl;
  wire[0:0] operator_20_15_true_AC_TRN_AC_WRAP_2_or_5_nl;
  wire[0:0] operator_20_15_true_AC_TRN_AC_WRAP_2_or_6_nl;
  wire[0:0] operator_20_15_true_AC_TRN_AC_WRAP_2_or_7_nl;
  wire[15:0] operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_3_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_20_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_21_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_22_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_23_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_24_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_25_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_26_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_27_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_28_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_29_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_30_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_31_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_20_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_21_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_22_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_23_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_24_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_25_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_26_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_27_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_28_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_29_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_30_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_31_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_20_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_21_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_22_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_23_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_24_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_25_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_26_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_27_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_28_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_29_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_30_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_20_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_21_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_22_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_23_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_24_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_25_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_26_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_27_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_28_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_29_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_30_nl;

  // Interconnect Declarations for Component Instantiations 
  wire[2:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_2_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_3_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_4_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_5_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_6_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_7_nl;
  wire[2:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_8_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_9_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_10_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_11_nl;
  wire[2:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_12_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_13_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_14_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_15_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_16_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_17_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_18_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_19_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_20_nl;
  wire [32:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a;
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_nl = MUX1HOT_v_3_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_32_30,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_32_30,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_32_30,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_32_30,
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_2_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_29,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_29,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_29,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_29,
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_3_nl = MUX1HOT_v_2_4_2(({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_28
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_27}),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_28_27,
      ({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_28
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_27}),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_28_27,
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_4_nl = MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_26_25,
      ({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_26
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_25}),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_26_25,
      ({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_26
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_25}),
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_5_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_24,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_24,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_24,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_24,
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_6_nl = MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_23_22,
      ({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_23
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_22}),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_23_22,
      ({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_23
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_22}),
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_7_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_21,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_21,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_21,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm,
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_8_nl = MUX1HOT_v_3_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_20_18,
      ({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_20
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_19
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_18}),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_20_18,
      ({ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm}),
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_9_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_17,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_17,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_17,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm,
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_10_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_16,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_16,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_16,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm,
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_11_nl = MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_15_14,
      ({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_15
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_14}),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_15_14,
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_12_nl = MUX1HOT_v_3_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_13_11,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_13_11,
      ({ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm}),
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_13_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_10,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_10,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_10,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm,
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_14_nl = MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_9_8,
      ({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_9
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_8}),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_9_8,
      ({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_9
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_8}),
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_15_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_7,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_7,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_7,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_7,
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_16_nl = MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_6_5,
      ({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_6
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_5}),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_6_5,
      ({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_6
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_5}),
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_17_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_4,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_4,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_4,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_4,
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_18_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_3,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_3,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_3,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_3,
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_19_nl = MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_2_1,
      ({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_2
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_1}),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_2_1,
      ({ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm}),
      {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6]) , (fsm_output[9])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_20_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_0,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_0,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_0,
      T_LINE_slc_T_LINE_acc_6_itm, {(fsm_output[7]) , (fsm_output[8]) , (fsm_output[6])
      , (fsm_output[9])});
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a = {operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_2_nl , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_3_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_4_nl , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_5_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_6_nl , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_7_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_8_nl , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_9_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_10_nl , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_11_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_12_nl , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_13_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_14_nl , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_15_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_16_nl , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_17_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_18_nl , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_19_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_1_mux1h_20_nl};
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_1_or_1_nl;
  wire [4:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s;
  assign operator_33_3_true_AC_TRN_AC_WRAP_1_or_1_nl = (fsm_output[6]) | (fsm_output[9]);
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s = MUX_v_5_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva,
      operator_33_3_true_AC_TRN_AC_WRAP_1_or_1_nl);
  wire[4:0] ac_math_atan_pi_2mi_mux_nl;
  wire [6:0] nl_ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr;
  assign ac_math_atan_pi_2mi_mux_nl = MUX_v_5_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva,
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva,
      fsm_output[7]);
  assign nl_ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr
      = {2'b0, ac_math_atan_pi_2mi_mux_nl};
  wire [10:0] nl_getMaxLine_core_x1_rsci_inst_x1_rsci_idat;
  assign nl_getMaxLine_core_x1_rsci_inst_x1_rsci_idat = {x1_rsci_idat_10_3 , x1_rsci_idat_2
      , x1_rsci_idat_1 , x1_rsci_idat_0};
  wire [9:0] nl_getMaxLine_core_y1_rsci_inst_y1_rsci_idat;
  assign nl_getMaxLine_core_y1_rsci_inst_y1_rsci_idat = {y1_rsci_idat_9_4 , y1_rsci_idat_3
      , y1_rsci_idat_2 , y1_rsci_idat_1 , y1_rsci_idat_0};
  wire [10:0] nl_getMaxLine_core_x2_rsci_inst_x2_rsci_idat;
  assign nl_getMaxLine_core_x2_rsci_inst_x2_rsci_idat = {x2_rsci_idat_10_3 , x2_rsci_idat_2
      , x2_rsci_idat_1 , x2_rsci_idat_0};
  wire [9:0] nl_getMaxLine_core_y2_rsci_inst_y2_rsci_idat;
  assign nl_getMaxLine_core_y2_rsci_inst_y2_rsci_idat = {y2_rsci_idat_9_4 , y2_rsci_idat_3
      , y2_rsci_idat_2 , y2_rsci_idat_1 , y2_rsci_idat_0};
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_T_LINE_C_4_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_T_LINE_C_4_tr0 = operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs;
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_T_LINE_C_10_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_T_LINE_C_10_tr0 = operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs;
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0
      = ~ (z_out_6[4]);
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
      = ~ (z_out_6[4]);
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_T_LINE_C_14_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_T_LINE_C_14_tr0 = operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs;
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0
      = ~ (z_out_6[4]);
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
      = ~ (z_out_6[4]);
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_T_LINE_C_18_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_T_LINE_C_18_tr0 = ~ T_LINE_slc_T_LINE_acc_6_itm;
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_R_LINE_C_0_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_R_LINE_C_0_tr0 = ~ (z_out_8[8]);
  mgc_shift_r_v5 #(.width_a(32'sd33),
  .signd_a(32'sd1),
  .width_s(32'sd5),
  .width_z(32'sd33)) operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg (
      .a(nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a[32:0]),
      .s(nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s[4:0]),
      .z(z_out_2)
    );
  Hough_Algorithm_HW_1296_864mgc_rom_23_70_32_1_60  ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg
      (
      .addr(nl_ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr[6:0]),
      .data_out(data_out_out)
    );
  getMaxLine_core_x1_rsci getMaxLine_core_x1_rsci_inst (
      .clk(clk),
      .rst(rst),
      .x1_rsc_dat(x1_rsc_dat),
      .x1_rsc_vld(x1_rsc_vld),
      .x1_rsc_rdy(x1_rsc_rdy),
      .core_wen(core_wen),
      .x1_rsci_oswt(reg_y2_rsci_ivld_core_psct_cse),
      .x1_rsci_wen_comp(x1_rsci_wen_comp),
      .x1_rsci_idat(nl_getMaxLine_core_x1_rsci_inst_x1_rsci_idat[10:0])
    );
  getMaxLine_core_y1_rsci getMaxLine_core_y1_rsci_inst (
      .clk(clk),
      .rst(rst),
      .y1_rsc_dat(y1_rsc_dat),
      .y1_rsc_vld(y1_rsc_vld),
      .y1_rsc_rdy(y1_rsc_rdy),
      .core_wen(core_wen),
      .y1_rsci_oswt(reg_y2_rsci_ivld_core_psct_cse),
      .y1_rsci_wen_comp(y1_rsci_wen_comp),
      .y1_rsci_idat(nl_getMaxLine_core_y1_rsci_inst_y1_rsci_idat[9:0])
    );
  getMaxLine_core_x2_rsci getMaxLine_core_x2_rsci_inst (
      .clk(clk),
      .rst(rst),
      .x2_rsc_dat(x2_rsc_dat),
      .x2_rsc_vld(x2_rsc_vld),
      .x2_rsc_rdy(x2_rsc_rdy),
      .core_wen(core_wen),
      .x2_rsci_oswt(reg_y2_rsci_ivld_core_psct_cse),
      .x2_rsci_wen_comp(x2_rsci_wen_comp),
      .x2_rsci_idat(nl_getMaxLine_core_x2_rsci_inst_x2_rsci_idat[10:0])
    );
  getMaxLine_core_y2_rsci getMaxLine_core_y2_rsci_inst (
      .clk(clk),
      .rst(rst),
      .y2_rsc_dat(y2_rsc_dat),
      .y2_rsc_vld(y2_rsc_vld),
      .y2_rsc_rdy(y2_rsc_rdy),
      .core_wen(core_wen),
      .y2_rsci_oswt(reg_y2_rsci_ivld_core_psct_cse),
      .y2_rsci_wen_comp(y2_rsci_wen_comp),
      .y2_rsci_idat(nl_getMaxLine_core_y2_rsci_inst_y2_rsci_idat[9:0])
    );
  getMaxLine_core_acc_rsci getMaxLine_core_acc_rsci_inst (
      .clk(clk),
      .rst(rst),
      .acc_rsc_dat(acc_rsc_dat),
      .acc_rsc_vld(acc_rsc_vld),
      .acc_rsc_rdy(acc_rsc_rdy),
      .core_wen(core_wen),
      .acc_rsci_oswt(reg_acc_rsci_irdy_core_psct_cse),
      .acc_rsci_wen_comp(acc_rsci_wen_comp),
      .acc_rsci_idat_mxwt(acc_rsci_idat_mxwt)
    );
  getMaxLine_core_wait_dp getMaxLine_core_wait_dp_inst (
      .clk(clk),
      .rst(rst),
      .T_LINE_if_if_dividend1_mul_cmp_z(T_LINE_if_if_dividend1_mul_cmp_z),
      .core_wen(core_wen),
      .T_LINE_if_if_dividend1_mul_cmp_z_oreg(T_LINE_if_if_dividend1_mul_cmp_z_oreg)
    );
  getMaxLine_core_staller getMaxLine_core_staller_inst (
      .core_wen(core_wen),
      .x1_rsci_wen_comp(x1_rsci_wen_comp),
      .y1_rsci_wen_comp(y1_rsci_wen_comp),
      .x2_rsci_wen_comp(x2_rsci_wen_comp),
      .y2_rsci_wen_comp(y2_rsci_wen_comp),
      .acc_rsci_wen_comp(acc_rsci_wen_comp)
    );
  getMaxLine_core_core_fsm getMaxLine_core_core_fsm_inst (
      .clk(clk),
      .rst(rst),
      .core_wen(core_wen),
      .fsm_output(fsm_output),
      .T_LINE_C_4_tr0(nl_getMaxLine_core_core_fsm_inst_T_LINE_C_4_tr0[0:0]),
      .ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm),
      .T_LINE_C_10_tr0(nl_getMaxLine_core_core_fsm_inst_T_LINE_C_10_tr0[0:0]),
      .T_LINE_C_10_tr1(T_LINE_asn_46),
      .ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0(nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0[0:0]),
      .ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0(nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0[0:0]),
      .T_LINE_C_14_tr0(nl_getMaxLine_core_core_fsm_inst_T_LINE_C_14_tr0[0:0]),
      .T_LINE_C_14_tr1(T_LINE_asn_46),
      .ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0(nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0[0:0]),
      .ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0(nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0[0:0]),
      .T_LINE_C_18_tr0(nl_getMaxLine_core_core_fsm_inst_T_LINE_C_18_tr0[0:0]),
      .R_LINE_C_0_tr0(nl_getMaxLine_core_core_fsm_inst_R_LINE_C_0_tr0[0:0])
    );
  assign nor_cse = ~((fsm_output[30]) | (fsm_output[0]));
  assign x1_t_and_3_cse = core_wen & or_tmp_79;
  assign x2_t_and_1_cse = T_LINE_asn_48 & (fsm_output[29]);
  assign x2_t_and_3_cse = core_wen & (T_LINE_asn_46 | T_LINE_asn_48) & (fsm_output[29]);
  assign x2_t_and_4_cse = core_wen & (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      & (fsm_output[29]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_and_cse
      = core_wen & (fsm_output[19]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_and_cse
      = core_wen & (~ or_dcpl_4) & (fsm_output[22]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_and_cse
      = core_wen & (~ or_dcpl_4) & (fsm_output[28]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_and_cse
      = core_wen & (fsm_output[18]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_and_cse
      = core_wen & (~ or_dcpl_13) & (fsm_output[22]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_and_cse
      = core_wen & (~ or_dcpl_13) & (fsm_output[28]);
  assign Hough_Algorithm_HW_1296_864_getMaxLine_and_cse = core_wen & (~((~ (fsm_output[30]))
      | (z_out_8[8])));
  assign and_438_rgt = (~ (z_out_8[16])) & (fsm_output[1]);
  assign T_LINE_if_if_dividend1_mul_cmp_a = {reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd
      , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1 , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_2
      , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_3 , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_4
      , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_5 , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_6
      , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_7 , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_8
      , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_9};
  assign and_499_rgt = (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      & (fsm_output[4]);
  assign and_501_rgt = (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      & (fsm_output[12]);
  assign and_503_rgt = (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      & (fsm_output[13]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_and_nl
      = MUX_v_6_2_2(6'b000000, (z_out_1[5:0]), or_tmp_695);
  assign or_365_nl = or_dcpl_72 | or_dcpl_69 | (fsm_output[19]);
  assign T_LINE_if_mux1h_4_rgt = MUX1HOT_v_13_3_2((z_out[12:0]), ({3'b000 , (T_LINE_if_acc_5_psp_1[9:8])
      , 8'b00000000}), ({7'b0000000 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_and_nl}),
      {(fsm_output[2]) , (fsm_output[4]) , or_365_nl});
  assign nl_operator_60_32_true_AC_TRN_AC_WRAP_acc_2_nl = (R_LINE_r_10_0_sva[10:2])
      + 9'b100011011;
  assign operator_60_32_true_AC_TRN_AC_WRAP_acc_2_nl = nl_operator_60_32_true_AC_TRN_AC_WRAP_acc_2_nl[8:0];
  assign or_367_nl = (fsm_output[11]) | (fsm_output[4]) | or_dcpl_77 | or_dcpl_75
      | (fsm_output[5]) | (fsm_output[7]);
  assign or_368_nl = (fsm_output[16]) | (fsm_output[20]) | or_dcpl_83 | or_dcpl_82;
  assign T_LINE_if_mux1h_8_rgt = MUX1HOT_v_13_3_2((z_out_8[12:0]), ({3'b000 , T_LINE_if_acc_5_psp_1}),
      ({4'b0000 , operator_60_32_true_AC_TRN_AC_WRAP_acc_2_nl}), {(fsm_output[3])
      , or_367_nl , or_368_nl});
  assign T_LINE_if_and_cse = core_wen & (fsm_output[11:5]==7'b0000000);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_70_cse
      = core_wen & (~ or_tmp_209);
  assign and_1762_cse = ((fsm_output[11]) | (fsm_output[5])) & core_wen;
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_and_60_cse
      = core_wen & (~((fsm_output[9]) | (fsm_output[6]) | or_dcpl_94));
  assign nor_55_cse = ~((fsm_output[7:6]!=2'b00));
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_69_cse
      = ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs
      & (fsm_output[11]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_68_cse
      = (~ ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs)
      & (fsm_output[11]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt
      = ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_68_cse
      | ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_69_cse;
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_and_2_cse
      = core_wen & (~(or_dcpl_77 | (fsm_output[6]) | or_dcpl_94));
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_28_rgt
      = ((~ mux_19_itm) & (fsm_output[22])) | ((~ mux_20_itm) & (fsm_output[28]));
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_29_rgt
      = (mux_19_itm & (fsm_output[22])) | (mux_20_itm & (fsm_output[28]));
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
      = (T_LINE_if_if_slc_T_LINE_if_acc_8_svs & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs
      & (fsm_output[22])) | (T_LINE_if_if_slc_T_LINE_if_acc_8_svs & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs
      & (fsm_output[28]));
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
      = (T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs)
      & (fsm_output[22])) | (T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs)
      & (fsm_output[28]));
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
      = (not_tmp_80 & (fsm_output[22])) | (not_tmp_82 & (fsm_output[28]));
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt
      = ((~(T_LINE_if_if_slc_T_LINE_if_acc_8_svs | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs))
      & (fsm_output[22])) | ((~(T_LINE_if_if_slc_T_LINE_if_acc_8_svs | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs))
      & (fsm_output[28]));
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_not_4_nl
      = ~ and_dcpl_93;
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_nl
      = MUX_v_32_2_2(32'b00000000000000000000000000000000, (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm[31:0]),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_not_4_nl);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_mux_rgt
      = MUX_v_33_2_2(({1'b0 , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_nl}),
      z_out_2, fsm_output[8]);
  assign nor_66_cse = ~((fsm_output[10:9]!=2'b00));
  assign and_1771_itm = nor_66_cse & core_wen;
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_nand_nl
      = ~(and_dcpl_93 & (~ (fsm_output[7])));
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_rgt
      = MUX_v_35_2_2(({3'b000 , (signext_32_1(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_nand_nl))}),
      (z_out_4[34:0]), fsm_output[8]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_and_cse
      = core_wen & (~(or_dcpl_77 | or_dcpl_94));
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_1_rgt
      = (T_LINE_if_if_slc_T_LINE_if_acc_8_svs & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c)
      | ((~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs) & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_37_m1c);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_rgt
      = (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp)
      & (fsm_output[9]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_1_rgt
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp
      & (fsm_output[9]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_or_cse
      = ((~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs) & (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[32]))
      | (T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[32]));
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_6_cse
      = core_wen & (~(or_dcpl_149 | (fsm_output[16]) | (fsm_output[23]) | (fsm_output[24])
      | (fsm_output[15]) | (fsm_output[17]) | (fsm_output[18]) | or_dcpl_81));
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c
      = (fsm_output[18]) | (fsm_output[24]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_37_m1c
      = (fsm_output[19]) | (fsm_output[25]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse
      = ((~(T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (z_out_6[4]))) & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c)
      | ((T_LINE_if_if_slc_T_LINE_if_acc_8_svs | (~ (z_out_6[4]))) & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_37_m1c)
      | or_dcpl_131 | (fsm_output[27:26]!=2'b00);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      = (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs) & (fsm_output[17]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      = T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (fsm_output[17]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      = (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs) & (fsm_output[23]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      = T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (fsm_output[23]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_mx1w0
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_mx1w0
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_mx1w0
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_mx1w0
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_mx1w0
      = ~(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign nl_T_LINE_if_acc_5_psp_1 = conv_s2u_9_10({T_LINE_if_acc_2_itm_16_14 , T_LINE_if_acc_2_itm_13
      , (T_LINE_if_acc_2_itm_12_4[8:4])}) + ({(T_LINE_t_7_0_sva[6:0]) , 3'b111});
  assign T_LINE_if_acc_5_psp_1 = nl_T_LINE_if_acc_5_psp_1[9:0];
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_ncse_2_sva_2
      = (T_LINE_if_acc_1_itm_12_4[4]) ^ (T_LINE_if_acc_1_itm_12_4[5]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_mux_cse
      = MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_ncse_2_sva_2}},
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_ncse_2_sva_2}),
      2'b01, T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1
      = ~(MUX_v_3_2_2(({{2{T_LINE_if_if_slc_T_LINE_if_acc_8_svs}}, T_LINE_if_if_slc_T_LINE_if_acc_8_svs}),
      3'b111, ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_ncse_2_sva_2));
  assign T_LINE_if_aelse_not_74_nl = ~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1
      = MUX_v_2_2_2(2'b00, (signext_2_1(T_LINE_if_aelse_not_74_nl)), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_ncse_2_sva_2);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0
      = MUX_v_20_2_2(({1'b0 , (T_LINE_if_if_dividend1_sva[18:0])}), z_out, T_LINE_if_if_dividend1_sva[19]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0
      = MUX_v_20_2_2(({1'b0 , (T_LINE_if_else_dividend1_sva[18:0])}), z_out, T_LINE_if_else_dividend1_sva[19]);
  assign nl_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_if_1_acc_nl
      = ({1'b1 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0})
      + conv_u2s_27_28({reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
      , reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1});
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_if_1_acc_nl
      = nl_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_if_1_acc_nl[27:0];
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_lpi_3_dfm_mx0
      = MUX_v_28_2_2(({1'b0 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0}),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_if_1_acc_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27);
  assign nl_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1
      = conv_u2s_1_20(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm)
      + ({(~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_19_lpi_3_dfm_1)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_18_lpi_3_dfm_1)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_17_lpi_3_dfm_1)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_16_lpi_3_dfm_1)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_mx1w0)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_mx1w0)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_mx1w0)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_mx1w0)
      , (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_mx1w0)});
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1
      = nl_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[19:0];
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_16_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_15_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_17_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_16_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_18_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_17_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_19_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_18_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0
      = MUX_v_20_2_2(({1'b0 , (T_LINE_if_if_dividend2_sva[18:0])}), z_out, T_LINE_if_if_dividend2_sva[19]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0
      = MUX_v_20_2_2(({1'b0 , (T_LINE_if_else_dividend2_sva[18:0])}), z_out, T_LINE_if_else_dividend2_sva[19]);
  assign T_LINE_asn_46 = T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs);
  assign T_LINE_asn_48 = ~(T_LINE_if_if_slc_T_LINE_if_acc_8_svs | operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs);
  assign or_dcpl_4 = operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
      | T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
  assign or_dcpl_13 = operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
      | (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  assign or_dcpl_69 = (fsm_output[18:17]!=2'b00);
  assign or_dcpl_72 = (fsm_output[25:23]!=3'b000);
  assign or_dcpl_75 = (fsm_output[6]) | (fsm_output[8]);
  assign or_dcpl_77 = (fsm_output[10:9]!=2'b00);
  assign or_dcpl_81 = (fsm_output[19]) | (fsm_output[21]);
  assign or_dcpl_82 = or_dcpl_69 | or_dcpl_81;
  assign or_dcpl_83 = (fsm_output[15:14]!=2'b00);
  assign or_dcpl_94 = (fsm_output[8:7]!=2'b00);
  assign and_dcpl_93 = ~((fsm_output[11]) | (fsm_output[6]));
  assign not_tmp_80 = ~((~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs)
      | T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  assign or_170_nl = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs
      | T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
  assign mux_19_itm = MUX_s_1_2_2(not_tmp_80, or_170_nl, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign not_tmp_82 = ~((~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs)
      | T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  assign or_172_nl = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs
      | T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
  assign mux_20_itm = MUX_s_1_2_2(not_tmp_82, or_172_nl, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign or_dcpl_103 = (fsm_output[12]) | (fsm_output[8]);
  assign or_dcpl_109 = (fsm_output[16]) | (fsm_output[23]);
  assign or_dcpl_110 = (fsm_output[11]) | (fsm_output[22]);
  assign or_dcpl_131 = (fsm_output[21:20]!=2'b00);
  assign or_dcpl_141 = (fsm_output[19:18]!=2'b00);
  assign and_dcpl_103 = ~(T_LINE_if_if_slc_T_LINE_if_acc_8_svs | (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[32]));
  assign and_dcpl_105 = T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (~ (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[32]));
  assign or_dcpl_149 = (fsm_output[22]) | (fsm_output[25]);
  assign or_tmp_79 = (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      & (fsm_output[26]);
  assign and_444_cse = (~ (z_out_8[8])) & (fsm_output[30]);
  assign or_tmp_209 = or_dcpl_75 | (fsm_output[7]);
  assign or_tmp_581 = (fsm_output[23]) | (fsm_output[17]);
  assign or_tmp_695 = (fsm_output[25:24]!=2'b00) | or_dcpl_141;
  assign T_LINE_if_if_dividend1_mul_cmp_a_mx0c0 = T_LINE_if_if_slc_T_LINE_if_acc_8_svs
      & (fsm_output[14]);
  assign T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 = (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs)
      & (fsm_output[14]);
  assign T_LINE_if_if_dividend1_mul_cmp_a_mx0c2 = T_LINE_if_if_slc_T_LINE_if_acc_8_svs
      & (fsm_output[20]);
  assign T_LINE_if_if_dividend1_mul_cmp_a_mx0c3 = (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs)
      & (fsm_output[20]);
  assign T_LINE_if_if_dividend1_or_itm = T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 |
      T_LINE_if_if_dividend1_mul_cmp_a_mx0c3;
  assign T_LINE_if_if_dividend1_or_1_itm = T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
      | T_LINE_if_if_dividend1_mul_cmp_a_mx0c2;
  assign or_tmp_777 = (~ ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs)
      & (fsm_output[10]);
  assign or_tmp_778 = ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs
      & (fsm_output[10]);
  assign or_tmp_779 = (~ ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs)
      & (fsm_output[8]);
  assign or_tmp_780 = ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs
      & (fsm_output[8]);
  assign or_948_cse_1 = (fsm_output[25]) | (fsm_output[24]) | (fsm_output[19]) |
      (fsm_output[18]);
  assign exs_tmp_16_26_0 = MUX_v_27_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva,
      ({reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
      , reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1}),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27);
  assign or_tmp_789 = (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp)
      & (fsm_output[7]);
  assign or_tmp_790 = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp
      & (fsm_output[7]);
  assign or_tmp_792 = (fsm_output[21]) | (fsm_output[15]);
  assign or_tmp_793 = (fsm_output[22]) | (fsm_output[16]);
  assign or_tmp_800 = (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs) & (fsm_output[29]);
  assign or_tmp_802 = T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (fsm_output[29]);
  assign T_LINE_if_or_ssc = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse;
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_t_7_0_sva <= 8'b00000000;
    end
    else if ( ((fsm_output[26]) | (fsm_output[31]) | (fsm_output[0]) | (fsm_output[30]))
        & core_wen ) begin
      T_LINE_t_7_0_sva <= MUX_v_8_2_2(8'b00000000, (z_out_1[7:0]), nor_cse);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      x1_t_42_31_lpi_3 <= 12'b000000000000;
    end
    else if ( mux_nl & (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
        & core_wen ) begin
      x1_t_42_31_lpi_3 <= MUX_v_12_2_2((z_out_8[11:0]), T_LINE_if_T_LINE_if_and_34_nl,
          or_tmp_79);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      x1_t_30_lpi_3 <= 1'b0;
      x1_t_29_lpi_3 <= 1'b0;
      x1_t_28_lpi_3 <= 1'b0;
      x1_t_27_lpi_3 <= 1'b0;
      x1_t_26_lpi_3 <= 1'b0;
      x1_t_25_lpi_3 <= 1'b0;
      x1_t_24_lpi_3 <= 1'b0;
      x1_t_23_lpi_3 <= 1'b0;
      y1_t_31_lpi_3 <= 1'b0;
      y1_t_30_lpi_3 <= 1'b0;
      y1_t_29_lpi_3 <= 1'b0;
      y1_t_28_lpi_3 <= 1'b0;
      y1_t_27_lpi_3 <= 1'b0;
      y1_t_26_lpi_3 <= 1'b0;
      y1_t_25_lpi_3 <= 1'b0;
      y1_t_24_lpi_3 <= 1'b0;
      y1_t_23_lpi_3 <= 1'b0;
    end
    else if ( x1_t_and_3_cse ) begin
      x1_t_30_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_12_nl
          & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      x1_t_29_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_13_nl
          & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      x1_t_28_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_14_nl
          & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      x1_t_27_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_15_nl
          & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      x1_t_26_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_16_nl
          & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      x1_t_25_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_17_nl
          & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      x1_t_24_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_18_nl
          & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      x1_t_23_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_19_nl
          & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      y1_t_31_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_11_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      y1_t_30_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_12_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      y1_t_29_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_13_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      y1_t_28_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_14_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      y1_t_27_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_15_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      y1_t_26_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_16_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      y1_t_25_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_17_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      y1_t_24_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_18_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      y1_t_23_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_19_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_else_dividend1_sva <= 20'b00000000000000000000;
    end
    else if ( core_wen & (~ or_dcpl_4) & (fsm_output[16]) ) begin
      T_LINE_if_else_dividend1_sva <= z_out_6[43:24];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      x2_t_42_31_lpi_3 <= 12'b000000000000;
      y2_t_42_32_lpi_3 <= 11'b00000000000;
    end
    else if ( x2_t_and_3_cse ) begin
      x2_t_42_31_lpi_3 <= MUX_v_12_2_2(12'b000010100010, (z_out_8[11:0]), x2_t_and_1_cse);
      y2_t_42_32_lpi_3 <= MUX_v_11_2_2((z_out_8[10:0]), 11'b00000110110, x2_t_and_1_cse);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      x2_t_30_lpi_3 <= 1'b0;
      x2_t_29_lpi_3 <= 1'b0;
      x2_t_28_lpi_3 <= 1'b0;
      x2_t_27_lpi_3 <= 1'b0;
      x2_t_26_lpi_3 <= 1'b0;
      x2_t_25_lpi_3 <= 1'b0;
      x2_t_24_lpi_3 <= 1'b0;
      x2_t_23_lpi_3 <= 1'b0;
      y2_t_31_lpi_3 <= 1'b0;
      y2_t_30_lpi_3 <= 1'b0;
      y2_t_29_lpi_3 <= 1'b0;
      y2_t_28_lpi_3 <= 1'b0;
      y2_t_27_lpi_3 <= 1'b0;
      y2_t_26_lpi_3 <= 1'b0;
      y2_t_25_lpi_3 <= 1'b0;
      y2_t_24_lpi_3 <= 1'b0;
      y2_t_23_lpi_3 <= 1'b0;
    end
    else if ( x2_t_and_4_cse ) begin
      x2_t_30_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_12_nl
          & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      x2_t_29_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_13_nl
          & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      x2_t_28_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_14_nl
          & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      x2_t_27_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_15_nl
          & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      x2_t_26_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_16_nl
          & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      x2_t_25_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_17_nl
          & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      x2_t_24_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_18_nl
          & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      x2_t_23_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_19_nl
          & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      y2_t_31_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_11_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      y2_t_30_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_12_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      y2_t_29_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_13_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      y2_t_28_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_14_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      y2_t_27_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_15_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      y2_t_26_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_16_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      y2_t_25_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_17_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      y2_t_24_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_18_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
      y2_t_23_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_19_nl
          & T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs
          <= 1'b0;
      operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva <= 1'b0;
    end
    else if ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_and_cse
        ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs
          <= ~((T_LINE_if_else_dividend1_sva[19]) ^ (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[32]));
      operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva <= ~((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[32:6]!=27'b000000000000000000000000000));
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_7_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_6_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_5_lpi_3_dfm
          <= 1'b0;
      T_LINE_if_else_dividend2_sva <= 20'b00000000000000000000;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_4_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_3_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_2_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_1_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_0_lpi_3_dfm
          <= 1'b0;
    end
    else if ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_and_cse
        ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_7_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_6_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_5_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0;
      T_LINE_if_else_dividend2_sva <= z_out_6[43:24];
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_4_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_3_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_2_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_1_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_0_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_mx1w0;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs
          <= 1'b0;
    end
    else if ( core_wen & (fsm_output[25]) ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs
          <= ~((T_LINE_if_else_dividend2_sva[19]) ^ (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[32]));
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_7_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_6_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_5_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_4_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_3_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_2_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_1_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_0_lpi_3_dfm
          <= 1'b0;
    end
    else if ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_and_cse
        ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_7_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_6_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_5_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_4_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_3_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_2_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_1_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_0_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_mx1w0;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      y1_t_42_32_lpi_3 <= 11'b00000000000;
    end
    else if ( mux_22_nl & (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
        & core_wen ) begin
      y1_t_42_32_lpi_3 <= MUX_v_11_2_2((z_out_8[10:0]), T_LINE_if_T_LINE_if_and_35_nl,
          or_tmp_79);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_if_dividend1_sva <= 20'b00000000000000000000;
    end
    else if ( core_wen & (~ or_dcpl_13) & (fsm_output[16]) ) begin
      T_LINE_if_if_dividend1_sva <= z_out_6[43:24];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs
          <= 1'b0;
      operator_27_3_true_AC_TRN_AC_WRAP_return_sva <= 1'b0;
    end
    else if ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_and_cse
        ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs
          <= ~((T_LINE_if_if_dividend1_sva[19]) ^ (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[32]));
      operator_27_3_true_AC_TRN_AC_WRAP_return_sva <= ~((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[32:6]!=27'b000000000000000000000000000));
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm
          <= 1'b0;
      T_LINE_if_if_dividend2_sva <= 20'b00000000000000000000;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm
          <= 1'b0;
    end
    else if ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_and_cse
        ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0;
      T_LINE_if_if_dividend2_sva <= z_out_6[43:24];
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_mx1w0;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs
          <= 1'b0;
    end
    else if ( core_wen & (fsm_output[24]) ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs
          <= ~((T_LINE_if_if_dividend2_sva[19]) ^ (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[32]));
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_8_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_7_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_6_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_5_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_4_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_3_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_2_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_1_lpi_3_dfm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_0_lpi_3_dfm
          <= 1'b0;
    end
    else if ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_and_cse
        ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_8_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_7_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_6_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_5_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_4_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_3_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_2_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_1_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_0_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_mx1w0;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      R_LINE_r_10_0_sva <= 11'b00000000000;
    end
    else if ( core_wen & ((fsm_output[0]) | (fsm_output[30])) ) begin
      R_LINE_r_10_0_sva <= MUX_v_11_2_2(11'b00000000000, z_out_1, (fsm_output[30]));
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      y2_rsci_idat_0 <= 1'b0;
      y2_rsci_idat_1 <= 1'b0;
      y2_rsci_idat_2 <= 1'b0;
      y2_rsci_idat_3 <= 1'b0;
      y2_rsci_idat_9_4 <= 6'b000000;
      x2_rsci_idat_0 <= 1'b0;
      x2_rsci_idat_1 <= 1'b0;
      x2_rsci_idat_2 <= 1'b0;
      x2_rsci_idat_10_3 <= 8'b00000000;
      y1_rsci_idat_0 <= 1'b0;
      y1_rsci_idat_1 <= 1'b0;
      y1_rsci_idat_2 <= 1'b0;
      y1_rsci_idat_3 <= 1'b0;
      y1_rsci_idat_9_4 <= 6'b000000;
      x1_rsci_idat_0 <= 1'b0;
      x1_rsci_idat_1 <= 1'b0;
      x1_rsci_idat_2 <= 1'b0;
      x1_rsci_idat_10_3 <= 8'b00000000;
    end
    else if ( Hough_Algorithm_HW_1296_864_getMaxLine_and_cse ) begin
      y2_rsci_idat_0 <= y2_t_28_lpi_3;
      y2_rsci_idat_1 <= y2_t_29_lpi_3;
      y2_rsci_idat_2 <= y2_t_30_lpi_3;
      y2_rsci_idat_3 <= y2_t_31_lpi_3;
      y2_rsci_idat_9_4 <= y2_t_42_32_lpi_3[5:0];
      x2_rsci_idat_0 <= x2_t_28_lpi_3;
      x2_rsci_idat_1 <= x2_t_29_lpi_3;
      x2_rsci_idat_2 <= x2_t_30_lpi_3;
      x2_rsci_idat_10_3 <= x2_t_42_31_lpi_3[7:0];
      y1_rsci_idat_0 <= y1_t_28_lpi_3;
      y1_rsci_idat_1 <= y1_t_29_lpi_3;
      y1_rsci_idat_2 <= y1_t_30_lpi_3;
      y1_rsci_idat_3 <= y1_t_31_lpi_3;
      y1_rsci_idat_9_4 <= y1_t_42_32_lpi_3[5:0];
      x1_rsci_idat_0 <= x1_t_28_lpi_3;
      x1_rsci_idat_1 <= x1_t_29_lpi_3;
      x1_rsci_idat_2 <= x1_t_30_lpi_3;
      x1_rsci_idat_10_3 <= x1_t_42_31_lpi_3[7:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      threshold_23_8_lpi_3 <= 16'b0000000000000000;
    end
    else if ( core_wen & ((fsm_output[0]) | (fsm_output[31]) | and_438_rgt) ) begin
      threshold_23_8_lpi_3 <= MUX_v_16_2_2(16'b0000000101000100, acc_rsci_idat_mxwt,
          and_438_rgt);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      reg_acc_rsci_irdy_core_psct_cse <= 1'b0;
      reg_y2_rsci_ivld_core_psct_cse <= 1'b0;
      T_LINE_if_if_dividend1_mul_cmp_b <= 27'b000000000000000000000000000;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_0
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_24
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_21
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_17
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_10
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_7
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_4
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_2
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_0
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_24
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_7
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_4
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_0
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm
          <= 1'b0;
      T_LINE_slc_T_LINE_acc_6_itm <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_15_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_16_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_17_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_18_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_19_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_18_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_17_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_16_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_15_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_14_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_13_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_12_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_11_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_10_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_9_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_8_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_7_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_6_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_5_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_4_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_3_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_2_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_1_sva
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva
          <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd <= 11'b00000000000;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1 <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_2 <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_3 <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_4 <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_5 <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_6 <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_7 <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_8 <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_9 <= 1'b0;
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_ac_fixed_cctor_20_1_ftd_11
          <= 1'b0;
    end
    else if ( core_wen ) begin
      reg_acc_rsci_irdy_core_psct_cse <= ~((nor_cse & (~ (fsm_output[29]))) | ((~
          T_LINE_slc_T_LINE_acc_6_itm) & (fsm_output[29])) | and_444_cse);
      reg_y2_rsci_ivld_core_psct_cse <= and_444_cse;
      T_LINE_if_if_dividend1_mul_cmp_b <= MUX_v_27_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[32:6]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[32:6]),
          T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_0
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_11_nl
          & (~ (fsm_output[5]));
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_24
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_10_nl
          & (~ (fsm_output[5]));
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_21
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_9_nl
          & (~ (fsm_output[5]));
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_17
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_8_nl
          & (~ (fsm_output[5]));
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_10
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_7_nl
          & (~ (fsm_output[5]));
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_7
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_6_nl
          & (~ (fsm_output[5]));
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_4
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_5_nl
          & (~ (fsm_output[5]));
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_2
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_4_nl
          & (~ (fsm_output[5]));
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_0
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_3_nl
          & (~ (fsm_output[5]));
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_24
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_2_nl
          & (~ (fsm_output[5]));
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_7
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_1_nl
          & (~ (fsm_output[5]));
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_4
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_nl
          & (~ (fsm_output[5]));
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_0
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_nl
          & (~ (fsm_output[5]));
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_104_nl
          & (~ (fsm_output[5]));
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_105_nl
          & (~ (fsm_output[5]));
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_106_nl
          & (~ (fsm_output[5]));
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_107_nl
          & (~ (fsm_output[5]));
      T_LINE_slc_T_LINE_acc_6_itm <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_108_nl
          & (~ (fsm_output[5]));
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_itm
          <= z_out_8[3];
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_18_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_17_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_16_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_15_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_14_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_13_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_12_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_11_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_10_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_9_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_8_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_7_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_6_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_5_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_4_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_15_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_3_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_16_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_2_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_17_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_1_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_18_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_nl
          & (~ or_tmp_581);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_19_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[19]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[19]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_18_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[19]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[19]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_18_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[18]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[18]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_17_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[18]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[18]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_17_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[17]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[17]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_16_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[17]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[17]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_16_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[16]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[16]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_15_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[16]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[16]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_15_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[15]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[15]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_14_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[15]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[15]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_14_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[14]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[14]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_13_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[14]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[14]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_13_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[13]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[13]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_12_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[13]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[13]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_12_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[12]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[12]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_11_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[12]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[12]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_11_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[11]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[11]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_10_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[11]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[11]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_10_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[10]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[10]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_9_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[10]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[10]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_9_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[9]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[9]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_8_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[9]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[9]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_8_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[8]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[8]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_7_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[8]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[8]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_7_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[7]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[7]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_6_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[7]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[7]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_6_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[6]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[6]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_5_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[6]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[6]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_5_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[5]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[5]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_4_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[5]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[5]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_4_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[4]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[4]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_3_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[4]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[4]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_3_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[3]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[3]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_2_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[3]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[3]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_2_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[2]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[2]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_1_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[2]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[2]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_1_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[1]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[1]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[1]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[1]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_695 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_mux1h_79_nl
          & (~ or_tmp_695);
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd <= MUX_v_11_2_2((z_out_8[11:1]), (z_out_8[10:0]),
          T_LINE_if_if_dividend1_or_itm);
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1 <= MUX1HOT_s_1_3_2((z_out_8[0]),
          y1_t_31_lpi_3, y2_t_31_lpi_3, {T_LINE_if_if_dividend1_or_1_itm , T_LINE_if_if_dividend1_mul_cmp_a_mx0c1
          , T_LINE_if_if_dividend1_mul_cmp_a_mx0c3});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_2 <= MUX1HOT_s_1_4_2(x1_t_30_lpi_3,
          y1_t_30_lpi_3, x2_t_30_lpi_3, y2_t_30_lpi_3, {T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
          , T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 , T_LINE_if_if_dividend1_mul_cmp_a_mx0c2
          , T_LINE_if_if_dividend1_mul_cmp_a_mx0c3});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_3 <= MUX1HOT_s_1_4_2(x1_t_29_lpi_3,
          y1_t_29_lpi_3, x2_t_29_lpi_3, y2_t_29_lpi_3, {T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
          , T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 , T_LINE_if_if_dividend1_mul_cmp_a_mx0c2
          , T_LINE_if_if_dividend1_mul_cmp_a_mx0c3});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_4 <= MUX1HOT_s_1_4_2(x1_t_28_lpi_3,
          y1_t_28_lpi_3, x2_t_28_lpi_3, y2_t_28_lpi_3, {T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
          , T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 , T_LINE_if_if_dividend1_mul_cmp_a_mx0c2
          , T_LINE_if_if_dividend1_mul_cmp_a_mx0c3});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_5 <= MUX1HOT_s_1_4_2(x1_t_27_lpi_3,
          y1_t_27_lpi_3, x2_t_27_lpi_3, y2_t_27_lpi_3, {T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
          , T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 , T_LINE_if_if_dividend1_mul_cmp_a_mx0c2
          , T_LINE_if_if_dividend1_mul_cmp_a_mx0c3});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_6 <= MUX1HOT_s_1_4_2(x1_t_26_lpi_3,
          y1_t_26_lpi_3, x2_t_26_lpi_3, y2_t_26_lpi_3, {T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
          , T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 , T_LINE_if_if_dividend1_mul_cmp_a_mx0c2
          , T_LINE_if_if_dividend1_mul_cmp_a_mx0c3});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_7 <= MUX1HOT_s_1_4_2(x1_t_25_lpi_3,
          y1_t_25_lpi_3, x2_t_25_lpi_3, y2_t_25_lpi_3, {T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
          , T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 , T_LINE_if_if_dividend1_mul_cmp_a_mx0c2
          , T_LINE_if_if_dividend1_mul_cmp_a_mx0c3});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_8 <= MUX1HOT_s_1_4_2(x1_t_24_lpi_3,
          y1_t_24_lpi_3, x2_t_24_lpi_3, y2_t_24_lpi_3, {T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
          , T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 , T_LINE_if_if_dividend1_mul_cmp_a_mx0c2
          , T_LINE_if_if_dividend1_mul_cmp_a_mx0c3});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_9 <= MUX1HOT_s_1_4_2(x1_t_23_lpi_3,
          y1_t_23_lpi_3, x2_t_23_lpi_3, y2_t_23_lpi_3, {T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
          , T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 , T_LINE_if_if_dividend1_mul_cmp_a_mx0c2
          , T_LINE_if_if_dividend1_mul_cmp_a_mx0c3});
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_ac_fixed_cctor_20_1_ftd_11
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[8];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_if_slc_T_LINE_if_acc_8_svs <= 1'b0;
    end
    else if ( core_wen & (and_499_rgt | and_501_rgt | and_503_rgt) ) begin
      T_LINE_if_if_slc_T_LINE_if_acc_8_svs <= MUX1HOT_s_1_3_2((z_out[18]), (readslicef_9_1_8(T_LINE_if_acc_nl)),
          T_LINE_if_if_T_LINE_if_if_T_LINE_if_if_nor_nl, {and_499_rgt , and_501_rgt
          , and_503_rgt});
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
          <= 1'b0;
    end
    else if ( core_wen & (fsm_output[1]) ) begin
      operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
          <= z_out_8[16];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_acc_1_itm_12_4 <= 9'b000000000;
    end
    else if ( ((fsm_output[18]) | (fsm_output[19]) | (fsm_output[24]) | (fsm_output[25])
        | (fsm_output[4]) | (fsm_output[17]) | (fsm_output[23]) | (fsm_output[2])
        | (fsm_output[3])) & core_wen ) begin
      T_LINE_if_acc_1_itm_12_4 <= T_LINE_if_mux1h_4_rgt[12:4];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_acc_1_itm_3_0 <= 4'b0000;
    end
    else if ( ((fsm_output[18]) | (fsm_output[19]) | (fsm_output[24]) | (fsm_output[25])
        | (fsm_output[4]) | (fsm_output[17]) | (fsm_output[23]) | (fsm_output[2]))
        & core_wen ) begin
      T_LINE_if_acc_1_itm_3_0 <= T_LINE_if_mux1h_4_rgt[3:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_acc_2_itm_16_14 <= 3'b000;
    end
    else if ( ((fsm_output[19]) | (fsm_output[21]) | (fsm_output[17]) | (fsm_output[18])
        | (fsm_output[15]) | (fsm_output[16]) | (fsm_output[20]) | (fsm_output[14])
        | (fsm_output[3]) | (fsm_output[4]) | (fsm_output[7]) | (fsm_output[5]) |
        (fsm_output[8]) | (fsm_output[10]) | (fsm_output[11]) | (fsm_output[6]) |
        (fsm_output[9])) & core_wen ) begin
      T_LINE_if_acc_2_itm_16_14 <= T_LINE_if_mux1h_8_rgt[12:10];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_acc_2_itm_13 <= 1'b0;
    end
    else if ( ((fsm_output[4]) | (fsm_output[19]) | (fsm_output[21]) | (fsm_output[17])
        | (fsm_output[18]) | (fsm_output[15]) | (fsm_output[16]) | (fsm_output[20])
        | (fsm_output[14]) | (fsm_output[3])) & core_wen ) begin
      T_LINE_if_acc_2_itm_13 <= T_LINE_if_mux1h_8_rgt[9];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_acc_2_itm_12_4 <= 9'b000000000;
    end
    else if ( (mux_23_nl | (fsm_output[3])) & core_wen ) begin
      T_LINE_if_acc_2_itm_12_4 <= T_LINE_if_mux1h_8_rgt[8:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_cos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_17_0_sva_3_0
          <= 4'b0000;
      ac_math_ac_cos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_17_0_sva_7_4
          <= 4'b0000;
    end
    else if ( T_LINE_if_and_cse ) begin
      ac_math_ac_cos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_17_0_sva_3_0
          <= T_LINE_if_acc_1_itm_3_0;
      ac_math_ac_cos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_17_0_sva_7_4
          <= T_LINE_if_acc_2_itm_12_4[3:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_32_30
          <= 3'b000;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_29
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_28
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_27
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_26_25
          <= 2'b00;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_24
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_23_22
          <= 2'b00;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_21
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_20_18
          <= 3'b000;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_17
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_16
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_15
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_14
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_13_11
          <= 3'b000;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_10
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_9_8
          <= 2'b00;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_7
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_6_5
          <= 2'b00;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_4
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_3
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_2_1
          <= 2'b00;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_29
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_26
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_25
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_23
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_22
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_9
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_8
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_6
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_5
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_3
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm
          <= 1'b0;
    end
    else if ( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_70_cse
        ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_32_30
          <= MUX_v_3_2_2((signext_3_1(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1])),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[32:30]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_29
          <= MUX_s_1_2_2(T_LINE_if_if_slc_T_LINE_if_acc_8_svs, (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[29]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_28
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[28]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_27
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[27]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_26_25
          <= MUX_v_2_2_2(({{1{T_LINE_if_if_slc_T_LINE_if_acc_8_svs}}, T_LINE_if_if_slc_T_LINE_if_acc_8_svs}),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[26:25]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_24
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[24]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_23_22
          <= MUX_v_2_2_2(({{1{T_LINE_if_if_slc_T_LINE_if_acc_8_svs}}, T_LINE_if_if_slc_T_LINE_if_acc_8_svs}),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[23:22]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_21
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[21]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_20_18
          <= MUX_v_3_2_2(({{2{T_LINE_if_if_slc_T_LINE_if_acc_8_svs}}, T_LINE_if_if_slc_T_LINE_if_acc_8_svs}),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[20:18]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_17
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[17]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_16
          <= MUX_s_1_2_2(T_LINE_if_if_slc_T_LINE_if_acc_8_svs, (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[16]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_15
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[15]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_14
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[14]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_13_11
          <= MUX_v_3_2_2(({{2{T_LINE_if_if_slc_T_LINE_if_acc_8_svs}}, T_LINE_if_if_slc_T_LINE_if_acc_8_svs}),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[13:11]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_10
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[10]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_9_8
          <= MUX_v_2_2_2(({{1{T_LINE_if_if_slc_T_LINE_if_acc_8_svs}}, T_LINE_if_if_slc_T_LINE_if_acc_8_svs}),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[9:8]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_7
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[7]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_6_5
          <= MUX_v_2_2_2(({{1{T_LINE_if_if_slc_T_LINE_if_acc_8_svs}}, T_LINE_if_if_slc_T_LINE_if_acc_8_svs}),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[6:5]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_4
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[4]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_3
          <= MUX_s_1_2_2(T_LINE_if_if_slc_T_LINE_if_acc_8_svs, (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[3]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_2_1
          <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_mux_cse,
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[2:1]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_29
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[29]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_26
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[26]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_25
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[25]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_23
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[23]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_22
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[22]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_9
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[9]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_8
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[8]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_6
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[6]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_5
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[5]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_3
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[3]),
          fsm_output[11]);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm
          <= MUX1HOT_s_1_4_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[1]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[18]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_18_lpi_3_dfm_1,
          {(fsm_output[5]) , (fsm_output[11]) , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_28_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_29_rgt});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm
          <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[11]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_17_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[17]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[8]),
          {(fsm_output[5]) , (fsm_output[11]) , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm
          <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[12]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_16_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[16]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_17_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[17]),
          {(fsm_output[5]) , (fsm_output[11]) , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm
          <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[13]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[15]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_16_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[16]),
          {(fsm_output[5]) , (fsm_output[11]) , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm
          <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[16]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[14]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[15]),
          {(fsm_output[5]) , (fsm_output[11]) , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm
          <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[18]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[12]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[13]),
          {(fsm_output[5]) , (fsm_output[11]) , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm
          <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[19]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[11]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[12]),
          {(fsm_output[5]) , (fsm_output[11]) , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm
          <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[20]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_19_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[19]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[10]),
          {(fsm_output[5]) , (fsm_output[11]) , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt});
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_32_30
          <= 3'b000;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_28_27
          <= 2'b00;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_15_14
          <= 2'b00;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_32_30
          <= 3'b000;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_28_27
          <= 2'b00;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_15_14
          <= 2'b00;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva
          <= 5'b00000;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva
          <= 5'b00000;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_3_dfm_1_34_32
          <= 3'b000;
    end
    else if ( and_1762_cse ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_32_30
          <= MUX_v_3_2_2(3'b000, (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[32:30]),
          Hough_Algorithm_HW_1296_864_getMaxLine_not_24_nl);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_28_27
          <= MUX_v_2_2_2(2'b00, (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[28:27]),
          Hough_Algorithm_HW_1296_864_getMaxLine_not_23_nl);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_15_14
          <= MUX_v_2_2_2(2'b00, (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[15:14]),
          Hough_Algorithm_HW_1296_864_getMaxLine_not_19_nl);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_32_30
          <= MUX_v_3_2_2(3'b000, (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[32:30]),
          Hough_Algorithm_HW_1296_864_getMaxLine_not_13_nl);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_28_27
          <= MUX_v_2_2_2(2'b00, (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[28:27]),
          Hough_Algorithm_HW_1296_864_getMaxLine_not_12_nl);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_15_14
          <= MUX_v_2_2_2(2'b00, (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[15:14]),
          Hough_Algorithm_HW_1296_864_getMaxLine_not_10_nl);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva
          <= MUX_v_5_2_2(5'b00000, ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1,
          Hough_Algorithm_HW_1296_864_getMaxLine_not_7_nl);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva
          <= MUX_v_5_2_2(5'b00000, ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1,
          Hough_Algorithm_HW_1296_864_getMaxLine_not_5_nl);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_3_dfm_1_34_32
          <= MUX_v_3_2_2(({{1{ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_mux_cse[1]}},
          ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_mux_cse}),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm[34:32]),
          fsm_output[11]);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_29
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_26
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_25
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_23
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_22
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_20
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_19
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_18
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_16
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11
          <= 3'b000;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_9
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_8
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_6
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_5
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_3
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_1
          <= 1'b0;
    end
    else if ( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_and_60_cse
        ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_29
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[29]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_26
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[26]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_25
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[25]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_23
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[23]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_22
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[22]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_20
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[20]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_19
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[19]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_18
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[18]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_16
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[16]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11
          <= MUX_v_3_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1,
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[13:11]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_9
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[9]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_8
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[8]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_6
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[6]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_5
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[5]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_3
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[3]),
          fsm_output[11]);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_1
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11_1[2]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[1]),
          fsm_output[11]);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_3_dfm_1_34_32
          <= 3'b000;
    end
    else if ( core_wen & nor_55_cse ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_3_dfm_1_34_32
          <= MUX_v_3_2_2(({{1{ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_mux_cse[1]}},
          ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_mux_cse}),
          ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_34_32,
          fsm_output[11]);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_32_30
          <= 3'b000;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_29
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_28
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_27
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_26_25
          <= 2'b00;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_24
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_23_22
          <= 2'b00;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_21
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_20_18
          <= 3'b000;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_17
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_16
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14
          <= 2'b00;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_13_11
          <= 3'b000;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_10
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_9_8
          <= 2'b00;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_7
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_6_5
          <= 2'b00;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_4
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_3
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_2_1
          <= 2'b00;
    end
    else if ( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_and_2_cse
        ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_32_30
          <= MUX_v_3_2_2((signext_3_1(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1])),
          (z_out_4[32:30]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_29
          <= MUX_s_1_2_2(T_LINE_if_if_slc_T_LINE_if_acc_8_svs, (z_out_4[29]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_28
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (z_out_4[28]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_27
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (z_out_4[27]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_26_25
          <= MUX_v_2_2_2(({{1{T_LINE_if_if_slc_T_LINE_if_acc_8_svs}}, T_LINE_if_if_slc_T_LINE_if_acc_8_svs}),
          (z_out_4[26:25]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_24
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (z_out_4[24]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_23_22
          <= MUX_v_2_2_2(({{1{T_LINE_if_if_slc_T_LINE_if_acc_8_svs}}, T_LINE_if_if_slc_T_LINE_if_acc_8_svs}),
          (z_out_4[23:22]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_21
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (z_out_4[21]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_20_18
          <= MUX_v_3_2_2(({{2{T_LINE_if_if_slc_T_LINE_if_acc_8_svs}}, T_LINE_if_if_slc_T_LINE_if_acc_8_svs}),
          (z_out_4[20:18]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_17
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (z_out_4[17]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_16
          <= MUX_s_1_2_2(T_LINE_if_if_slc_T_LINE_if_acc_8_svs, (z_out_4[16]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14
          <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1,
          (z_out_4[15:14]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_13_11
          <= MUX_v_3_2_2(({{2{T_LINE_if_if_slc_T_LINE_if_acc_8_svs}}, T_LINE_if_if_slc_T_LINE_if_acc_8_svs}),
          (z_out_4[13:11]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_10
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (z_out_4[10]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_9_8
          <= MUX_v_2_2_2(({{1{T_LINE_if_if_slc_T_LINE_if_acc_8_svs}}, T_LINE_if_if_slc_T_LINE_if_acc_8_svs}),
          (z_out_4[9:8]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_7
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (z_out_4[7]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_6_5
          <= MUX_v_2_2_2(({{1{T_LINE_if_if_slc_T_LINE_if_acc_8_svs}}, T_LINE_if_if_slc_T_LINE_if_acc_8_svs}),
          (z_out_4[6:5]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_4
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14_1[1]),
          (z_out_4[4]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_3
          <= MUX_s_1_2_2(T_LINE_if_if_slc_T_LINE_if_acc_8_svs, (z_out_4[3]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_2_1
          <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_mux_cse,
          (z_out_4[2:1]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4
          <= 33'b000000000000000000000000000000000;
    end
    else if ( core_wen & (~(or_dcpl_110 | or_dcpl_109 | (fsm_output[24]) | (fsm_output[20])
        | or_dcpl_83 | or_dcpl_82 | (fsm_output[10]) | (fsm_output[13]) | or_dcpl_103))
        ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4
          <= MUX_v_33_2_2((z_out_6[32:0]), (z_out_5[32:0]), fsm_output[9]);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva
          <= 33'b000000000000000000000000000000000;
    end
    else if ( core_wen & (~(or_dcpl_110 | (fsm_output[25]) | (fsm_output[16]) | (fsm_output[23])
        | (fsm_output[20]) | or_dcpl_83 | or_dcpl_82 | (fsm_output[9]) | (fsm_output[13])
        | or_dcpl_103)) ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva
          <= MUX_v_33_2_2(z_out_2, (z_out_4[32:0]), fsm_output[10]);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_32
          <= 1'b0;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_34_32
          <= 3'b000;
    end
    else if ( and_1771_itm ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_32
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_mux_rgt[32];
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_34_32
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_rgt[34:32];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0
          <= 32'b00000000000000000000000000000000;
    end
    else if ( (~((~ nor_66_cse) | (fsm_output[6]))) & core_wen ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_mux_rgt[31:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_31_0
          <= 32'b00000000000000000000000000000000;
    end
    else if ( (~((fsm_output[6]) | (fsm_output[11]) | (fsm_output[7]))) & nor_66_cse
        & core_wen ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_31_0
          <= ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_rgt[31:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1
          <= 5'b00000;
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1
          <= 5'b00000;
    end
    else if ( ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_and_cse
        ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1
          <= z_out_3[4:0];
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1
          <= z_out_7;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_atan_pi_2mi_return_2_69_38_sva <= 32'b00000000000000000000000000000000;
    end
    else if ( core_wen & (~ (fsm_output[7])) ) begin
      ac_math_atan_pi_2mi_return_2_69_38_sva <= data_out_out;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp
          <= 1'b0;
    end
    else if ( core_wen & (~(((~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs) & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c)
        | (T_LINE_if_if_slc_T_LINE_if_acc_8_svs & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_37_m1c)
        | or_dcpl_131 | (fsm_output[27]) | (fsm_output[26]) | (fsm_output[8]) | (fsm_output[7])))
        ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp
          <= MUX_s_1_2_2((z_out_4[35]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_T000000,
          ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_1_rgt);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm
          <= 33'b000000000000000000000000000000000;
    end
    else if ( core_wen & (~((fsm_output[10]) | (fsm_output[8]) | (fsm_output[7])))
        ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm
          <= MUX1HOT_v_33_3_2(z_out_2, ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4,
          (z_out_4[32:0]), {(fsm_output[6]) , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_rgt
          , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_1_rgt});
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs
          <= 1'b0;
    end
    else if ( core_wen & (fsm_output[7]) ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_35_svs
          <= z_out_4[35];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm
          <= 35'b00000000000000000000000000000000000;
    end
    else if ( core_wen & (~(or_dcpl_77 | (fsm_output[8]))) ) begin
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm
          <= z_out_5;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm
          <= 1'b0;
    end
    else if ( core_wen & (~((fsm_output[21]) | (fsm_output[9]) | (fsm_output[10])
        | (fsm_output[27]) | (fsm_output[8]))) ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_nor_nl,
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_nl,
          ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_or_nl);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
          <= 1'b0;
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1
          <= 26'b00000000000000000000000000;
    end
    else if ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_6_cse
        ) begin
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
          <= (z_out_3[26]) & (~(and_dcpl_103 | and_dcpl_105));
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1
          <= MUX1HOT_v_26_3_2((z_out_3[25:0]), (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[31:6]),
          (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[31:6]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_nl
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_1_nl
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_2_nl});
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva
          <= 27'b000000000000000000000000000;
    end
    else if ( core_wen & (~(or_dcpl_149 | or_dcpl_109 | (fsm_output[24]) | (fsm_output[17])
        | or_dcpl_141)) ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva
          <= z_out_6[26:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0
          <= 27'b000000000000000000000000000;
    end
    else if ( (mux_24_nl | (fsm_output[17]) | (fsm_output[23])) & core_wen ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0
          <= MUX_v_27_2_2(27'b000000000000000000000000000, (z_out_4[26:0]), not_586_nl);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11
          <= 9'b000000000;
    end
    else if ( core_wen & (~ or_dcpl_72) ) begin
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[8:0];
    end
  end
  assign T_LINE_if_aelse_not_57_nl = ~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
  assign T_LINE_if_T_LINE_if_and_34_nl = MUX_v_12_2_2(12'b000000000000, x1_t_42_31_lpi_3,
      T_LINE_if_aelse_not_57_nl);
  assign mux_nl = MUX_s_1_2_2((fsm_output[23]), T_LINE_if_if_slc_T_LINE_if_acc_8_svs,
      fsm_output[26]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_12_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[7]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_7_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_12_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_12_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_13_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[6]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_6_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_13_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_13_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_14_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[5]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_5_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_14_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_14_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_15_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[4]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_4_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_15_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_15_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_16_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[3]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_3_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_16_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_16_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_17_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[2]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_2_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_17_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_17_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_18_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[1]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_1_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_18_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_18_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_19_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[0]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_0_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_19_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_19_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_11_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[8]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_11_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_11_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_12_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[7]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_12_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_12_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_13_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[6]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_13_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_13_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_14_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[5]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_14_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_14_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_15_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[4]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_15_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_15_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_16_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[3]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_16_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_16_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_17_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[2]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_17_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_17_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_18_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[1]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_18_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_18_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_19_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[0]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_19_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_19_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_12_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[7]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_7_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_12_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_12_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_13_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[6]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_6_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_13_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_13_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_14_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[5]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_5_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_14_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_14_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_15_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[4]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_4_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_15_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_15_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_16_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[3]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_3_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_16_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_16_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_17_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[2]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_2_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_17_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_17_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_18_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[1]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_1_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_18_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_18_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_19_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[0]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_0_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_19_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_19_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_11_nl
      = MUX_s_1_2_2(reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_ac_fixed_cctor_20_1_ftd_11,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_8_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_11_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_11_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_12_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[7]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_7_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_12_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_12_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_13_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[6]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_6_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_13_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_13_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_14_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[5]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_5_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_14_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_14_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_15_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[4]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_4_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_15_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_15_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_16_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[3]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_3_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_16_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_16_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_17_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[2]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_2_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_17_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_17_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_18_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[1]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_1_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_18_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_18_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_19_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11[0]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_0_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_19_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_19_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign T_LINE_if_T_LINE_if_and_35_nl = MUX_v_11_2_2(11'b00000000000, y1_t_42_32_lpi_3,
      T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  assign mux_22_nl = MUX_s_1_2_2((fsm_output[23]), (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs),
      fsm_output[26]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_11_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_0,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[0]),
      fsm_output[11]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_10_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_24,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[24]),
      fsm_output[11]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_9_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_21,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[21]),
      fsm_output[11]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_8_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_17,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[17]),
      fsm_output[11]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_7_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_10,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[10]),
      fsm_output[11]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_6_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_7,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[7]),
      fsm_output[11]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_5_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_4,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[4]),
      fsm_output[11]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_4_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_2,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[2]),
      fsm_output[11]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_3_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_0,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[0]),
      fsm_output[11]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_2_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_24,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[24]),
      fsm_output[11]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_1_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_7,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[7]),
      fsm_output[11]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_4,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[4]),
      fsm_output[11]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_0,
      (z_out_4[0]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_104_nl
      = MUX1HOT_s_1_4_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[10]),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[9]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1,
      {or_tmp_209 , (fsm_output[11]) , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_28_rgt
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_29_rgt});
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_105_nl
      = MUX1HOT_s_1_6_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[17]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[13]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[14]),
      {or_tmp_209 , (fsm_output[11]) , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt});
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_106_nl
      = MUX1HOT_s_1_6_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[2]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[10]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[11]),
      {or_tmp_209 , (fsm_output[11]) , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_24_rgt
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_25_rgt
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_26_rgt
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_or_27_rgt});
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_nl
      = ((~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs)
      & (fsm_output[22])) | ((~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs)
      & (fsm_output[28]));
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_1_nl
      = (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs
      & (fsm_output[22])) | (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs
      & (fsm_output[28]));
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_107_nl
      = MUX1HOT_s_1_4_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm,
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[21]),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[19]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_19_lpi_3_dfm_1,
      {or_tmp_209 , (fsm_output[11]) , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_nl
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_1_nl});
  assign or_709_nl = (fsm_output[28]) | (fsm_output[27]) | (fsm_output[6]) | or_dcpl_94;
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_mux1h_108_nl
      = MUX1HOT_s_1_3_2(T_LINE_slc_T_LINE_acc_6_itm, (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[0]),
      (z_out_8[6]), {or_709_nl , (fsm_output[11]) , (fsm_output[26])});
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_18_nl
      = MUX_s_1_2_2((~ (z_out_4[27])), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_17_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_16_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_15_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_14_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_13_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_12_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_11_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_10_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_9_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_8_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_7_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_6_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_5_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_4_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_3_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_15_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_2_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_15_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_16_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_1_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_16_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_17_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_17_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_18_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27,
      (z_out_4[27]), ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_1_rgt);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_mux1h_79_nl
      = MUX1HOT_s_1_4_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[0]),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[0]),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[0]),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[0]),
      {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
  assign nl_T_LINE_if_acc_nl = conv_u2s_8_9(T_LINE_t_7_0_sva) + 9'b111010011;
  assign T_LINE_if_acc_nl = nl_T_LINE_if_acc_nl[8:0];
  assign nl_T_LINE_if_aelse_acc_nl = ({1'b1 , (~ (T_LINE_t_7_0_sva[7:3]))}) + 6'b010001;
  assign T_LINE_if_aelse_acc_nl = nl_T_LINE_if_aelse_acc_nl[5:0];
  assign T_LINE_if_if_T_LINE_if_if_T_LINE_if_if_nor_nl = ~((readslicef_6_1_5(T_LINE_if_aelse_acc_nl))
      | T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  assign or_nl = (fsm_output[4]) | (fsm_output[14]);
  assign or_1032_nl = (fsm_output[11:5]!=7'b0000000);
  assign mux_23_nl = MUX_s_1_2_2(or_nl, (fsm_output[14]), or_1032_nl);
  assign Hough_Algorithm_HW_1296_864_getMaxLine_not_24_nl = ~ (fsm_output[5]);
  assign Hough_Algorithm_HW_1296_864_getMaxLine_not_23_nl = ~ (fsm_output[5]);
  assign Hough_Algorithm_HW_1296_864_getMaxLine_not_19_nl = ~ (fsm_output[5]);
  assign Hough_Algorithm_HW_1296_864_getMaxLine_not_13_nl = ~ (fsm_output[5]);
  assign Hough_Algorithm_HW_1296_864_getMaxLine_not_12_nl = ~ (fsm_output[5]);
  assign Hough_Algorithm_HW_1296_864_getMaxLine_not_10_nl = ~ (fsm_output[5]);
  assign Hough_Algorithm_HW_1296_864_getMaxLine_not_7_nl = ~ (fsm_output[5]);
  assign Hough_Algorithm_HW_1296_864_getMaxLine_not_5_nl = ~ (fsm_output[5]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_T000000
      = reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
      | (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1!=26'b00000000000000000000000000);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_nor_nl
      = ~(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_itm
      | (z_out_7[3]));
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_nl
      = ~((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_lpi_3_dfm_mx0!=28'b0000000000000000000000000000));
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_or_nl
      = (fsm_output[20]) | (fsm_output[26]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_nl
      = (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_or_cse
      & (fsm_output[14])) | (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_or_cse
      & (fsm_output[20]));
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_1_nl
      = (and_dcpl_103 & (fsm_output[14])) | (and_dcpl_103 & (fsm_output[20]));
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_2_nl
      = (and_dcpl_105 & (fsm_output[14])) | (and_dcpl_105 & (fsm_output[20]));
  assign not_586_nl = ~ or_tmp_581;
  assign mux_24_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_37_m1c,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c,
      T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  assign T_LINE_if_T_LINE_if_T_LINE_if_nor_2_nl = ~((T_LINE_if_acc_5_psp_1[9]) |
      (fsm_output[2]) | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse);
  assign T_LINE_if_mux1h_25_nl = MUX1HOT_v_19_6_2(({11'b00000001000 , (~ T_LINE_t_7_0_sva)}),
      (signext_19_18({(~ T_LINE_if_acc_5_psp_1) , (~ (T_LINE_if_acc_2_itm_12_4[3:0]))
      , (~ T_LINE_if_acc_1_itm_3_0)})), (~ (T_LINE_if_if_dividend1_sva[18:0])), (~
      (T_LINE_if_else_dividend1_sva[18:0])), (~ (T_LINE_if_if_dividend2_sva[18:0])),
      (~ (T_LINE_if_else_dividend2_sva[18:0])), {(fsm_output[2]) , (fsm_output[4])
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse});
  assign T_LINE_if_T_LINE_if_or_1_nl = (~((fsm_output[2]) | T_LINE_if_or_ssc)) |
      (fsm_output[4]);
  assign T_LINE_if_or_2_nl = (fsm_output[4]) | T_LINE_if_or_ssc;
  assign T_LINE_if_T_LINE_if_T_LINE_if_nor_3_nl = ~(MUX_v_8_2_2(T_LINE_t_7_0_sva,
      8'b11111111, T_LINE_if_or_2_nl));
  assign nl_z_out = ({T_LINE_if_T_LINE_if_T_LINE_if_nor_2_nl , T_LINE_if_mux1h_25_nl})
      + conv_u2u_17_20({T_LINE_if_T_LINE_if_or_1_nl , 4'b0000 , T_LINE_if_T_LINE_if_T_LINE_if_nor_3_nl
      , 4'b0001});
  assign z_out = nl_z_out[19:0];
  assign T_LINE_mux1h_4_nl = MUX1HOT_v_11_3_2(({{3{T_LINE_t_7_0_sva[7]}}, T_LINE_t_7_0_sva}),
      R_LINE_r_10_0_sva, (signext_11_6({(T_LINE_if_acc_1_itm_12_4[1:0]) , T_LINE_if_acc_1_itm_3_0})),
      {(fsm_output[26]) , (fsm_output[30]) , or_948_cse_1});
  assign nl_z_out_1 = T_LINE_mux1h_4_nl + 11'b00000000001;
  assign z_out_1 = nl_z_out_1[10:0];
  assign or_1063_nl = ((~((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[32])
      & T_LINE_if_if_slc_T_LINE_if_acc_8_svs)) & (fsm_output[14])) | ((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[32])
      & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs) & (fsm_output[20]));
  assign or_1064_nl = ((ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[32])
      & T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (fsm_output[14])) | (((~ (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[32]))
      | T_LINE_if_if_slc_T_LINE_if_acc_8_svs) & (fsm_output[20]));
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux1h_11_nl
      = MUX1HOT_v_26_3_2(({{21{ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva[4]}},
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva}),
      (~ (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[31:6])),
      (~ (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[31:6])),
      {(fsm_output[6]) , or_1063_nl , or_1064_nl});
  assign nl_z_out_3 = conv_u2u_26_27(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux1h_11_nl)
      + 27'b000000000000000000000000001;
  assign z_out_3 = nl_z_out_3[26:0];
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_5_nl
      = or_tmp_777 | or_tmp_778;
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_6_nl
      = or_tmp_779 | or_tmp_780;
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_3_nl
      = MUX1HOT_v_35_7_2((signext_35_33({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_32_30
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_29
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_28_27
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_26
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_25
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_24
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_23
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_22
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_21
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_20
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_19
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_18
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_17
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_16
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_15_14
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_13_11
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_10
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_9
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_8
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_7
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_6
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_5
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_4
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_3
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_2
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_1
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_3_dfm_1_0})),
      ({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_3_dfm_1_34_32
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_31_0}),
      ({(~ ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_3_dfm_1_34_32)
      , (~ ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0)}),
      (signext_35_33({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_32_30
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_29
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_28_27
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_26
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_25
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_24
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_23
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_22
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_15_14
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_9
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_8
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_7
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_6
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_5
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_4
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_3
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm
      , T_LINE_slc_T_LINE_acc_6_itm})), ({(~ ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_3_dfm_1_34_32)
      , (~ ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm_31_0)}),
      (signext_35_33({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_32_30
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_29
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_28
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_27
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_26_25
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_24
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_23_22
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_21
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_20_18
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_17
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_16
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_15_14
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_13_11
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_10
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_9_8
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_7
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_6_5
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_4
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_3
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_2_1
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_3_dfm_1_0})),
      (signext_35_28({ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_19_sva})),
      {ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_5_nl
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_6_nl
      , (fsm_output[6]) , (fsm_output[9]) , (fsm_output[7]) , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_31_rgt
      , or_948_cse_1});
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_7_nl
      = (~(or_tmp_778 | or_tmp_779 | (fsm_output[9]) | ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_68_cse
      | or_948_cse_1)) | or_tmp_777 | or_tmp_780 | (fsm_output[7:6]!=2'b00) | ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_69_cse;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_and_3_nl
      = (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva[26])
      & (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_8_nl
      = (fsm_output[7:6]!=2'b00);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_4_nl
      = MUX1HOT_v_18_9_2((signext_18_17(~ (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[32:16]))),
      (signext_18_17(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[32:16])),
      ({2'b00 , (ac_math_atan_pi_2mi_return_2_69_38_sva[31:16])}), ({2'b11 , (~ (ac_math_atan_pi_2mi_return_2_69_38_sva[31:16]))}),
      ({T_LINE_if_acc_2_itm_13 , T_LINE_if_acc_2_itm_12_4 , ac_math_ac_cos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_17_0_sva_7_4
      , ac_math_ac_cos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_17_0_sva_3_0}),
      (signext_18_17(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[32:16])),
      (signext_18_17({(~ ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_32)
      , (~ (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0[31:16]))})),
      (signext_18_17({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_32
      , (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0[31:16])})),
      (signext_18_12({ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_and_3_nl
      , (exs_tmp_16_26_0[26:16])})), {or_tmp_777 , or_tmp_778 , or_tmp_779 , or_tmp_780
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_8_nl
      , (fsm_output[9]) , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_69_cse
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_68_cse
      , or_948_cse_1});
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_5_nl
      = MUX1HOT_v_16_8_2((~ (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[15:0])),
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[15:0]),
      (ac_math_atan_pi_2mi_return_2_69_38_sva[15:0]), (~ (ac_math_atan_pi_2mi_return_2_69_38_sva[15:0])),
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm[15:0]),
      (~ (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0[15:0])),
      (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0[15:0]),
      (exs_tmp_16_26_0[15:0]), {or_tmp_777 , or_tmp_778 , or_tmp_779 , or_tmp_780
      , (fsm_output[9]) , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_69_cse
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_68_cse
      , or_948_cse_1});
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_1_nl
      = MUX_v_16_2_2(16'b0000000000000000, ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_5_nl,
      nor_55_cse);
  assign nl_acc_3_nl = conv_s2u_36_37({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_3_nl
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_7_nl})
      + conv_s2u_35_37({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_4_nl
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_1_nl
      , 1'b1});
  assign acc_3_nl = nl_acc_3_nl[36:0];
  assign z_out_4 = readslicef_37_36_1(acc_3_nl);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_4_nl
      = or_tmp_789 | or_tmp_790;
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux_1_nl
      = MUX_v_35_2_2((signext_35_33({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_32_30
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_29
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_28
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_27
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_26_25
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_24
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_23_22
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_21
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_20_18
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_17
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_16
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_15
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_14
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_13_11
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_10
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_9_8
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_7
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_6_5
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_4
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_3
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_2_1
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_3_dfm_1_0})),
      ({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_3_dfm_1_34_32
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_31_0}),
      ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_4_nl);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_5_nl
      = (~(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_rgt
      | or_tmp_789)) | ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_1_rgt
      | or_tmp_790;
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_4_nl
      = MUX1HOT_v_33_4_2(z_out_2, (~ z_out_2), ({1'b0 , data_out_out}), ({1'b1 ,
      (~ data_out_out)}), {ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_rgt
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_and_1_rgt
      , or_tmp_789 , or_tmp_790});
  assign nl_acc_4_nl = ({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux_1_nl
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_5_nl})
      + conv_s2u_34_36({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_4_nl
      , 1'b1});
  assign acc_4_nl = nl_acc_4_nl[35:0];
  assign z_out_5 = readslicef_36_35_1(acc_4_nl);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_5_nl
      = MUX1HOT_v_44_4_2((signext_44_33({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_32_30
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_29
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_28_27
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_26
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_25
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_24
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_23
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_22
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_15_14
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_9
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_8
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_7
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_6
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_5
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_4
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_3_dfm_1_3
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm
      , T_LINE_slc_T_LINE_acc_6_itm})), 44'b00000000000000000000000000000000000000000001,
      (~ T_LINE_if_if_dividend1_mul_cmp_z_oreg), 44'b11111111111111111111111111111111111111110101,
      {(fsm_output[7]) , or_tmp_792 , or_tmp_793 , or_948_cse_1});
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_6_nl
      = (~(or_tmp_792 | or_tmp_793 | or_948_cse_1)) | (fsm_output[7]);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_6_nl
      = MUX1HOT_v_40_4_2((signext_40_33(~ ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_itm)),
      ({13'b0000000000000 , (~ reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd)
      , (~ reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1)}),
      ({T_LINE_if_acc_2_itm_12_4 , (R_LINE_r_10_0_sva[1:0]) , 29'b00000000000000000000000000001}),
      ({36'b000000000000000000000000000000000000 , (z_out_1[5:2])}), {(fsm_output[7])
      , or_tmp_792 , or_tmp_793 , or_948_cse_1});
  assign nl_acc_5_nl = ({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_5_nl
      , ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_or_6_nl})
      + conv_s2u_41_45({ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_else_mux1h_6_nl
      , 1'b1});
  assign acc_5_nl = nl_acc_5_nl[44:0];
  assign z_out_6 = readslicef_45_44_1(acc_5_nl);
  assign ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_23_nl
      = MUX_v_5_2_2(ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva,
      ({2'b11 , (ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1[4:2])}),
      fsm_output[7]);
  assign nl_z_out_7 = ac_math_ac_sincos_cordic_27_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_23_nl
      + 5'b00001;
  assign z_out_7 = nl_z_out_7[4:0];
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_or_5_nl = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      | or_tmp_800;
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_or_6_nl = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      | or_tmp_802;
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_2_nl = MUX1HOT_v_17_9_2(17'b00000000001010001,
      17'b00000000000011011, ({8'b11111111 , T_LINE_if_acc_1_itm_12_4}), 17'b11111111111010011,
      17'b00000000000000001, 17'b11111111110101111, ({1'b1 , acc_rsci_idat_mxwt}),
      17'b11111111100011011, 17'b11111111111100101, {operator_20_15_true_AC_TRN_AC_WRAP_2_or_5_nl
      , operator_20_15_true_AC_TRN_AC_WRAP_2_or_6_nl , (fsm_output[3]) , (fsm_output[26])
      , (fsm_output[6]) , T_LINE_if_if_dividend1_or_1_itm , (fsm_output[1]) , (fsm_output[30])
      , T_LINE_if_if_dividend1_or_itm});
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_or_7_nl = (~(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      | or_tmp_800 | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      | or_tmp_802 | (fsm_output[3]) | (fsm_output[26]) | (fsm_output[6]) | T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
      | T_LINE_if_if_dividend1_mul_cmp_a_mx0c2 | (fsm_output[30]) | T_LINE_if_if_dividend1_mul_cmp_a_mx0c1
      | T_LINE_if_if_dividend1_mul_cmp_a_mx0c3)) | (fsm_output[1]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_20_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm,
      (T_LINE_if_else_dividend1_sva[19]), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_21_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_22_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_23_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_24_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_25_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_26_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_27_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_28_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_29_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_30_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_31_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_20_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm,
      (T_LINE_if_else_dividend2_sva[19]), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_21_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_22_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_23_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_24_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_25_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_26_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_27_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_28_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_29_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_30_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_31_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_20_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm,
      (T_LINE_if_if_dividend1_sva[19]), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_21_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_22_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_23_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_24_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_25_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_26_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_27_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_28_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_29_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_30_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_20_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm,
      (T_LINE_if_if_dividend2_sva[19]), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_21_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_22_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_23_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_24_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_25_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_26_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_27_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_28_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_29_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_30_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_3_nl = MUX1HOT_v_16_13_2((signext_16_12({ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_20_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_21_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_22_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_23_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_24_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_25_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_26_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_27_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_28_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_29_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_30_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_31_nl})),
      (signext_16_12({ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_20_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_21_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_22_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_23_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_24_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_25_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_26_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_27_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_28_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_29_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_30_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_31_nl})),
      (signext_16_11({ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_20_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_21_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_22_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_23_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_24_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_25_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_26_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_27_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_28_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_29_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_30_nl})),
      (signext_16_11({ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_20_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_21_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_22_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_23_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_24_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_25_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_26_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_27_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_28_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_29_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_30_nl})),
      ({4'b0000 , T_LINE_t_7_0_sva , 4'b0001}), ({10'b0000000000 , (z_out_1[7:2])}),
      ({13'b1111111111111 , (z_out_7[4:2])}), ({{4{x1_t_42_31_lpi_3[11]}}, x1_t_42_31_lpi_3}),
      ({{4{x2_t_42_31_lpi_3[11]}}, x2_t_42_31_lpi_3}), (~ threshold_23_8_lpi_3),
      ({8'b00000000 , (z_out_1[10:3])}), ({{5{y1_t_42_32_lpi_3[10]}}, y1_t_42_32_lpi_3}),
      ({{5{y2_t_42_32_lpi_3[10]}}, y2_t_42_32_lpi_3}), {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      , or_tmp_800 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      , or_tmp_802 , (fsm_output[3]) , (fsm_output[26]) , (fsm_output[6]) , T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
      , T_LINE_if_if_dividend1_mul_cmp_a_mx0c2 , (fsm_output[1]) , (fsm_output[30])
      , T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 , T_LINE_if_if_dividend1_mul_cmp_a_mx0c3});
  assign nl_acc_7_nl = ({operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_2_nl , operator_20_15_true_AC_TRN_AC_WRAP_2_or_7_nl})
      + conv_u2u_17_18({operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_3_nl , 1'b1});
  assign acc_7_nl = nl_acc_7_nl[17:0];
  assign z_out_8 = readslicef_18_17_1(acc_7_nl);

  function automatic [0:0] MUX1HOT_s_1_3_2;
    input [0:0] input_2;
    input [0:0] input_1;
    input [0:0] input_0;
    input [2:0] sel;
    reg [0:0] result;
  begin
    result = input_0 & {1{sel[0]}};
    result = result | ( input_1 & {1{sel[1]}});
    result = result | ( input_2 & {1{sel[2]}});
    MUX1HOT_s_1_3_2 = result;
  end
  endfunction


  function automatic [0:0] MUX1HOT_s_1_4_2;
    input [0:0] input_3;
    input [0:0] input_2;
    input [0:0] input_1;
    input [0:0] input_0;
    input [3:0] sel;
    reg [0:0] result;
  begin
    result = input_0 & {1{sel[0]}};
    result = result | ( input_1 & {1{sel[1]}});
    result = result | ( input_2 & {1{sel[2]}});
    result = result | ( input_3 & {1{sel[3]}});
    MUX1HOT_s_1_4_2 = result;
  end
  endfunction


  function automatic [0:0] MUX1HOT_s_1_5_2;
    input [0:0] input_4;
    input [0:0] input_3;
    input [0:0] input_2;
    input [0:0] input_1;
    input [0:0] input_0;
    input [4:0] sel;
    reg [0:0] result;
  begin
    result = input_0 & {1{sel[0]}};
    result = result | ( input_1 & {1{sel[1]}});
    result = result | ( input_2 & {1{sel[2]}});
    result = result | ( input_3 & {1{sel[3]}});
    result = result | ( input_4 & {1{sel[4]}});
    MUX1HOT_s_1_5_2 = result;
  end
  endfunction


  function automatic [0:0] MUX1HOT_s_1_6_2;
    input [0:0] input_5;
    input [0:0] input_4;
    input [0:0] input_3;
    input [0:0] input_2;
    input [0:0] input_1;
    input [0:0] input_0;
    input [5:0] sel;
    reg [0:0] result;
  begin
    result = input_0 & {1{sel[0]}};
    result = result | ( input_1 & {1{sel[1]}});
    result = result | ( input_2 & {1{sel[2]}});
    result = result | ( input_3 & {1{sel[3]}});
    result = result | ( input_4 & {1{sel[4]}});
    result = result | ( input_5 & {1{sel[5]}});
    MUX1HOT_s_1_6_2 = result;
  end
  endfunction


  function automatic [10:0] MUX1HOT_v_11_3_2;
    input [10:0] input_2;
    input [10:0] input_1;
    input [10:0] input_0;
    input [2:0] sel;
    reg [10:0] result;
  begin
    result = input_0 & {11{sel[0]}};
    result = result | ( input_1 & {11{sel[1]}});
    result = result | ( input_2 & {11{sel[2]}});
    MUX1HOT_v_11_3_2 = result;
  end
  endfunction


  function automatic [12:0] MUX1HOT_v_13_3_2;
    input [12:0] input_2;
    input [12:0] input_1;
    input [12:0] input_0;
    input [2:0] sel;
    reg [12:0] result;
  begin
    result = input_0 & {13{sel[0]}};
    result = result | ( input_1 & {13{sel[1]}});
    result = result | ( input_2 & {13{sel[2]}});
    MUX1HOT_v_13_3_2 = result;
  end
  endfunction


  function automatic [15:0] MUX1HOT_v_16_13_2;
    input [15:0] input_12;
    input [15:0] input_11;
    input [15:0] input_10;
    input [15:0] input_9;
    input [15:0] input_8;
    input [15:0] input_7;
    input [15:0] input_6;
    input [15:0] input_5;
    input [15:0] input_4;
    input [15:0] input_3;
    input [15:0] input_2;
    input [15:0] input_1;
    input [15:0] input_0;
    input [12:0] sel;
    reg [15:0] result;
  begin
    result = input_0 & {16{sel[0]}};
    result = result | ( input_1 & {16{sel[1]}});
    result = result | ( input_2 & {16{sel[2]}});
    result = result | ( input_3 & {16{sel[3]}});
    result = result | ( input_4 & {16{sel[4]}});
    result = result | ( input_5 & {16{sel[5]}});
    result = result | ( input_6 & {16{sel[6]}});
    result = result | ( input_7 & {16{sel[7]}});
    result = result | ( input_8 & {16{sel[8]}});
    result = result | ( input_9 & {16{sel[9]}});
    result = result | ( input_10 & {16{sel[10]}});
    result = result | ( input_11 & {16{sel[11]}});
    result = result | ( input_12 & {16{sel[12]}});
    MUX1HOT_v_16_13_2 = result;
  end
  endfunction


  function automatic [15:0] MUX1HOT_v_16_8_2;
    input [15:0] input_7;
    input [15:0] input_6;
    input [15:0] input_5;
    input [15:0] input_4;
    input [15:0] input_3;
    input [15:0] input_2;
    input [15:0] input_1;
    input [15:0] input_0;
    input [7:0] sel;
    reg [15:0] result;
  begin
    result = input_0 & {16{sel[0]}};
    result = result | ( input_1 & {16{sel[1]}});
    result = result | ( input_2 & {16{sel[2]}});
    result = result | ( input_3 & {16{sel[3]}});
    result = result | ( input_4 & {16{sel[4]}});
    result = result | ( input_5 & {16{sel[5]}});
    result = result | ( input_6 & {16{sel[6]}});
    result = result | ( input_7 & {16{sel[7]}});
    MUX1HOT_v_16_8_2 = result;
  end
  endfunction


  function automatic [16:0] MUX1HOT_v_17_9_2;
    input [16:0] input_8;
    input [16:0] input_7;
    input [16:0] input_6;
    input [16:0] input_5;
    input [16:0] input_4;
    input [16:0] input_3;
    input [16:0] input_2;
    input [16:0] input_1;
    input [16:0] input_0;
    input [8:0] sel;
    reg [16:0] result;
  begin
    result = input_0 & {17{sel[0]}};
    result = result | ( input_1 & {17{sel[1]}});
    result = result | ( input_2 & {17{sel[2]}});
    result = result | ( input_3 & {17{sel[3]}});
    result = result | ( input_4 & {17{sel[4]}});
    result = result | ( input_5 & {17{sel[5]}});
    result = result | ( input_6 & {17{sel[6]}});
    result = result | ( input_7 & {17{sel[7]}});
    result = result | ( input_8 & {17{sel[8]}});
    MUX1HOT_v_17_9_2 = result;
  end
  endfunction


  function automatic [17:0] MUX1HOT_v_18_9_2;
    input [17:0] input_8;
    input [17:0] input_7;
    input [17:0] input_6;
    input [17:0] input_5;
    input [17:0] input_4;
    input [17:0] input_3;
    input [17:0] input_2;
    input [17:0] input_1;
    input [17:0] input_0;
    input [8:0] sel;
    reg [17:0] result;
  begin
    result = input_0 & {18{sel[0]}};
    result = result | ( input_1 & {18{sel[1]}});
    result = result | ( input_2 & {18{sel[2]}});
    result = result | ( input_3 & {18{sel[3]}});
    result = result | ( input_4 & {18{sel[4]}});
    result = result | ( input_5 & {18{sel[5]}});
    result = result | ( input_6 & {18{sel[6]}});
    result = result | ( input_7 & {18{sel[7]}});
    result = result | ( input_8 & {18{sel[8]}});
    MUX1HOT_v_18_9_2 = result;
  end
  endfunction


  function automatic [18:0] MUX1HOT_v_19_6_2;
    input [18:0] input_5;
    input [18:0] input_4;
    input [18:0] input_3;
    input [18:0] input_2;
    input [18:0] input_1;
    input [18:0] input_0;
    input [5:0] sel;
    reg [18:0] result;
  begin
    result = input_0 & {19{sel[0]}};
    result = result | ( input_1 & {19{sel[1]}});
    result = result | ( input_2 & {19{sel[2]}});
    result = result | ( input_3 & {19{sel[3]}});
    result = result | ( input_4 & {19{sel[4]}});
    result = result | ( input_5 & {19{sel[5]}});
    MUX1HOT_v_19_6_2 = result;
  end
  endfunction


  function automatic [25:0] MUX1HOT_v_26_3_2;
    input [25:0] input_2;
    input [25:0] input_1;
    input [25:0] input_0;
    input [2:0] sel;
    reg [25:0] result;
  begin
    result = input_0 & {26{sel[0]}};
    result = result | ( input_1 & {26{sel[1]}});
    result = result | ( input_2 & {26{sel[2]}});
    MUX1HOT_v_26_3_2 = result;
  end
  endfunction


  function automatic [1:0] MUX1HOT_v_2_4_2;
    input [1:0] input_3;
    input [1:0] input_2;
    input [1:0] input_1;
    input [1:0] input_0;
    input [3:0] sel;
    reg [1:0] result;
  begin
    result = input_0 & {2{sel[0]}};
    result = result | ( input_1 & {2{sel[1]}});
    result = result | ( input_2 & {2{sel[2]}});
    result = result | ( input_3 & {2{sel[3]}});
    MUX1HOT_v_2_4_2 = result;
  end
  endfunction


  function automatic [32:0] MUX1HOT_v_33_3_2;
    input [32:0] input_2;
    input [32:0] input_1;
    input [32:0] input_0;
    input [2:0] sel;
    reg [32:0] result;
  begin
    result = input_0 & {33{sel[0]}};
    result = result | ( input_1 & {33{sel[1]}});
    result = result | ( input_2 & {33{sel[2]}});
    MUX1HOT_v_33_3_2 = result;
  end
  endfunction


  function automatic [32:0] MUX1HOT_v_33_4_2;
    input [32:0] input_3;
    input [32:0] input_2;
    input [32:0] input_1;
    input [32:0] input_0;
    input [3:0] sel;
    reg [32:0] result;
  begin
    result = input_0 & {33{sel[0]}};
    result = result | ( input_1 & {33{sel[1]}});
    result = result | ( input_2 & {33{sel[2]}});
    result = result | ( input_3 & {33{sel[3]}});
    MUX1HOT_v_33_4_2 = result;
  end
  endfunction


  function automatic [34:0] MUX1HOT_v_35_7_2;
    input [34:0] input_6;
    input [34:0] input_5;
    input [34:0] input_4;
    input [34:0] input_3;
    input [34:0] input_2;
    input [34:0] input_1;
    input [34:0] input_0;
    input [6:0] sel;
    reg [34:0] result;
  begin
    result = input_0 & {35{sel[0]}};
    result = result | ( input_1 & {35{sel[1]}});
    result = result | ( input_2 & {35{sel[2]}});
    result = result | ( input_3 & {35{sel[3]}});
    result = result | ( input_4 & {35{sel[4]}});
    result = result | ( input_5 & {35{sel[5]}});
    result = result | ( input_6 & {35{sel[6]}});
    MUX1HOT_v_35_7_2 = result;
  end
  endfunction


  function automatic [2:0] MUX1HOT_v_3_4_2;
    input [2:0] input_3;
    input [2:0] input_2;
    input [2:0] input_1;
    input [2:0] input_0;
    input [3:0] sel;
    reg [2:0] result;
  begin
    result = input_0 & {3{sel[0]}};
    result = result | ( input_1 & {3{sel[1]}});
    result = result | ( input_2 & {3{sel[2]}});
    result = result | ( input_3 & {3{sel[3]}});
    MUX1HOT_v_3_4_2 = result;
  end
  endfunction


  function automatic [39:0] MUX1HOT_v_40_4_2;
    input [39:0] input_3;
    input [39:0] input_2;
    input [39:0] input_1;
    input [39:0] input_0;
    input [3:0] sel;
    reg [39:0] result;
  begin
    result = input_0 & {40{sel[0]}};
    result = result | ( input_1 & {40{sel[1]}});
    result = result | ( input_2 & {40{sel[2]}});
    result = result | ( input_3 & {40{sel[3]}});
    MUX1HOT_v_40_4_2 = result;
  end
  endfunction


  function automatic [43:0] MUX1HOT_v_44_4_2;
    input [43:0] input_3;
    input [43:0] input_2;
    input [43:0] input_1;
    input [43:0] input_0;
    input [3:0] sel;
    reg [43:0] result;
  begin
    result = input_0 & {44{sel[0]}};
    result = result | ( input_1 & {44{sel[1]}});
    result = result | ( input_2 & {44{sel[2]}});
    result = result | ( input_3 & {44{sel[3]}});
    MUX1HOT_v_44_4_2 = result;
  end
  endfunction


  function automatic [0:0] MUX_s_1_2_2;
    input [0:0] input_0;
    input [0:0] input_1;
    input [0:0] sel;
    reg [0:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_s_1_2_2 = result;
  end
  endfunction


  function automatic [10:0] MUX_v_11_2_2;
    input [10:0] input_0;
    input [10:0] input_1;
    input [0:0] sel;
    reg [10:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_11_2_2 = result;
  end
  endfunction


  function automatic [11:0] MUX_v_12_2_2;
    input [11:0] input_0;
    input [11:0] input_1;
    input [0:0] sel;
    reg [11:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_12_2_2 = result;
  end
  endfunction


  function automatic [15:0] MUX_v_16_2_2;
    input [15:0] input_0;
    input [15:0] input_1;
    input [0:0] sel;
    reg [15:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_16_2_2 = result;
  end
  endfunction


  function automatic [19:0] MUX_v_20_2_2;
    input [19:0] input_0;
    input [19:0] input_1;
    input [0:0] sel;
    reg [19:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_20_2_2 = result;
  end
  endfunction


  function automatic [26:0] MUX_v_27_2_2;
    input [26:0] input_0;
    input [26:0] input_1;
    input [0:0] sel;
    reg [26:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_27_2_2 = result;
  end
  endfunction


  function automatic [27:0] MUX_v_28_2_2;
    input [27:0] input_0;
    input [27:0] input_1;
    input [0:0] sel;
    reg [27:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_28_2_2 = result;
  end
  endfunction


  function automatic [1:0] MUX_v_2_2_2;
    input [1:0] input_0;
    input [1:0] input_1;
    input [0:0] sel;
    reg [1:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_2_2_2 = result;
  end
  endfunction


  function automatic [31:0] MUX_v_32_2_2;
    input [31:0] input_0;
    input [31:0] input_1;
    input [0:0] sel;
    reg [31:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_32_2_2 = result;
  end
  endfunction


  function automatic [32:0] MUX_v_33_2_2;
    input [32:0] input_0;
    input [32:0] input_1;
    input [0:0] sel;
    reg [32:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_33_2_2 = result;
  end
  endfunction


  function automatic [34:0] MUX_v_35_2_2;
    input [34:0] input_0;
    input [34:0] input_1;
    input [0:0] sel;
    reg [34:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_35_2_2 = result;
  end
  endfunction


  function automatic [2:0] MUX_v_3_2_2;
    input [2:0] input_0;
    input [2:0] input_1;
    input [0:0] sel;
    reg [2:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_3_2_2 = result;
  end
  endfunction


  function automatic [4:0] MUX_v_5_2_2;
    input [4:0] input_0;
    input [4:0] input_1;
    input [0:0] sel;
    reg [4:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_5_2_2 = result;
  end
  endfunction


  function automatic [5:0] MUX_v_6_2_2;
    input [5:0] input_0;
    input [5:0] input_1;
    input [0:0] sel;
    reg [5:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_6_2_2 = result;
  end
  endfunction


  function automatic [7:0] MUX_v_8_2_2;
    input [7:0] input_0;
    input [7:0] input_1;
    input [0:0] sel;
    reg [7:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_8_2_2 = result;
  end
  endfunction


  function automatic [16:0] readslicef_18_17_1;
    input [17:0] vector;
    reg [17:0] tmp;
  begin
    tmp = vector >> 1;
    readslicef_18_17_1 = tmp[16:0];
  end
  endfunction


  function automatic [34:0] readslicef_36_35_1;
    input [35:0] vector;
    reg [35:0] tmp;
  begin
    tmp = vector >> 1;
    readslicef_36_35_1 = tmp[34:0];
  end
  endfunction


  function automatic [35:0] readslicef_37_36_1;
    input [36:0] vector;
    reg [36:0] tmp;
  begin
    tmp = vector >> 1;
    readslicef_37_36_1 = tmp[35:0];
  end
  endfunction


  function automatic [43:0] readslicef_45_44_1;
    input [44:0] vector;
    reg [44:0] tmp;
  begin
    tmp = vector >> 1;
    readslicef_45_44_1 = tmp[43:0];
  end
  endfunction


  function automatic [0:0] readslicef_6_1_5;
    input [5:0] vector;
    reg [5:0] tmp;
  begin
    tmp = vector >> 5;
    readslicef_6_1_5 = tmp[0:0];
  end
  endfunction


  function automatic [0:0] readslicef_9_1_8;
    input [8:0] vector;
    reg [8:0] tmp;
  begin
    tmp = vector >> 8;
    readslicef_9_1_8 = tmp[0:0];
  end
  endfunction


  function automatic [10:0] signext_11_6;
    input [5:0] vector;
  begin
    signext_11_6= {{5{vector[5]}}, vector};
  end
  endfunction


  function automatic [15:0] signext_16_11;
    input [10:0] vector;
  begin
    signext_16_11= {{5{vector[10]}}, vector};
  end
  endfunction


  function automatic [15:0] signext_16_12;
    input [11:0] vector;
  begin
    signext_16_12= {{4{vector[11]}}, vector};
  end
  endfunction


  function automatic [17:0] signext_18_12;
    input [11:0] vector;
  begin
    signext_18_12= {{6{vector[11]}}, vector};
  end
  endfunction


  function automatic [17:0] signext_18_17;
    input [16:0] vector;
  begin
    signext_18_17= {{1{vector[16]}}, vector};
  end
  endfunction


  function automatic [18:0] signext_19_18;
    input [17:0] vector;
  begin
    signext_19_18= {{1{vector[17]}}, vector};
  end
  endfunction


  function automatic [1:0] signext_2_1;
    input [0:0] vector;
  begin
    signext_2_1= {{1{vector[0]}}, vector};
  end
  endfunction


  function automatic [31:0] signext_32_1;
    input [0:0] vector;
  begin
    signext_32_1= {{31{vector[0]}}, vector};
  end
  endfunction


  function automatic [34:0] signext_35_28;
    input [27:0] vector;
  begin
    signext_35_28= {{7{vector[27]}}, vector};
  end
  endfunction


  function automatic [34:0] signext_35_33;
    input [32:0] vector;
  begin
    signext_35_33= {{2{vector[32]}}, vector};
  end
  endfunction


  function automatic [2:0] signext_3_1;
    input [0:0] vector;
  begin
    signext_3_1= {{2{vector[0]}}, vector};
  end
  endfunction


  function automatic [39:0] signext_40_33;
    input [32:0] vector;
  begin
    signext_40_33= {{7{vector[32]}}, vector};
  end
  endfunction


  function automatic [43:0] signext_44_33;
    input [32:0] vector;
  begin
    signext_44_33= {{11{vector[32]}}, vector};
  end
  endfunction


  function automatic [9:0] conv_s2u_9_10 ;
    input [8:0]  vector ;
  begin
    conv_s2u_9_10 = {vector[8], vector};
  end
  endfunction


  function automatic [35:0] conv_s2u_34_36 ;
    input [33:0]  vector ;
  begin
    conv_s2u_34_36 = {{2{vector[33]}}, vector};
  end
  endfunction


  function automatic [36:0] conv_s2u_35_37 ;
    input [34:0]  vector ;
  begin
    conv_s2u_35_37 = {{2{vector[34]}}, vector};
  end
  endfunction


  function automatic [36:0] conv_s2u_36_37 ;
    input [35:0]  vector ;
  begin
    conv_s2u_36_37 = {vector[35], vector};
  end
  endfunction


  function automatic [44:0] conv_s2u_41_45 ;
    input [40:0]  vector ;
  begin
    conv_s2u_41_45 = {{4{vector[40]}}, vector};
  end
  endfunction


  function automatic [19:0] conv_u2s_1_20 ;
    input [0:0]  vector ;
  begin
    conv_u2s_1_20 = {{19{1'b0}}, vector};
  end
  endfunction


  function automatic [8:0] conv_u2s_8_9 ;
    input [7:0]  vector ;
  begin
    conv_u2s_8_9 =  {1'b0, vector};
  end
  endfunction


  function automatic [27:0] conv_u2s_27_28 ;
    input [26:0]  vector ;
  begin
    conv_u2s_27_28 =  {1'b0, vector};
  end
  endfunction


  function automatic [17:0] conv_u2u_17_18 ;
    input [16:0]  vector ;
  begin
    conv_u2u_17_18 = {1'b0, vector};
  end
  endfunction


  function automatic [19:0] conv_u2u_17_20 ;
    input [16:0]  vector ;
  begin
    conv_u2u_17_20 = {{3{1'b0}}, vector};
  end
  endfunction


  function automatic [26:0] conv_u2u_26_27 ;
    input [25:0]  vector ;
  begin
    conv_u2u_26_27 = {1'b0, vector};
  end
  endfunction

endmodule

// ------------------------------------------------------------------
//  Design Unit:    houghTransform_core
// ------------------------------------------------------------------


module houghTransform_core (
  clk, rst, data_in_rsc_dat, data_in_rsc_vld, data_in_rsc_rdy, widthIn_rsc_triosy_lz,
      heightIn_rsc_triosy_lz, acc_rsc_dat, acc_rsc_vld, acc_rsc_rdy, widthIn_rsci_idat,
      heightIn_rsci_idat, acc_tmp_rsci_data_in_d, acc_tmp_rsci_addr_d, acc_tmp_rsci_re_d,
      acc_tmp_rsci_we_d, acc_tmp_rsci_data_out_d, acc_tmp_rsci_en_d
);
  input clk;
  input rst;
  input [7:0] data_in_rsc_dat;
  input data_in_rsc_vld;
  output data_in_rsc_rdy;
  output widthIn_rsc_triosy_lz;
  output heightIn_rsc_triosy_lz;
  output [15:0] acc_rsc_dat;
  output acc_rsc_vld;
  input acc_rsc_rdy;
  input [10:0] widthIn_rsci_idat;
  input [9:0] heightIn_rsci_idat;
  output [15:0] acc_tmp_rsci_data_in_d;
  output [18:0] acc_tmp_rsci_addr_d;
  output [1:0] acc_tmp_rsci_re_d;
  output [1:0] acc_tmp_rsci_we_d;
  input [31:0] acc_tmp_rsci_data_out_d;
  output acc_tmp_rsci_en_d;


  // Interconnect Declarations
  wire core_wen;
  wire core_wten;
  wire data_in_rsci_wen_comp;
  wire [7:0] data_in_rsci_idat_mxwt;
  wire acc_rsci_wen_comp;
  reg [15:0] acc_rsci_idat;
  wire [15:0] acc_tmp_rsci_data_out_d_oreg;
  wire [31:0] fsm_output;
  wire HROW_equal_tmp;
  wire HCOL_equal_tmp;
  wire for_for_nor_tmp;
  wire and_dcpl_13;
  wire or_dcpl_22;
  wire or_dcpl_33;
  wire or_dcpl_34;
  wire or_dcpl_35;
  wire or_dcpl_36;
  wire or_dcpl_54;
  wire or_dcpl_58;
  wire or_dcpl_60;
  wire or_dcpl_61;
  wire and_dcpl_32;
  wire or_dcpl_73;
  wire or_tmp_57;
  wire or_tmp_77;
  wire or_tmp_167;
  wire or_tmp_302;
  wire or_tmp_306;
  wire or_tmp_404;
  wire or_tmp_419;
  wire or_tmp_420;
  wire and_137_cse;
  wire and_186_cse;
  wire and_198_cse;
  wire and_195_cse;
  wire and_435_cse;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_nor_itm;
  reg HACC_slc_HACC_acc_6_itm;
  reg HACC_idx_HACC_idx_acc_conv_2f_and_itm;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs;
  reg [2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_4_dfm_1_34_32;
  reg [2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_4_dfm_1_34_32;
  reg [34:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm;
  reg [38:0] HACC_mul_2_itm;
  reg [15:0] reg_HACC_slc_HACC_acc_7_39_14_psp_ftd;
  wire or_131_ssc;
  wire or_134_ssc;
  reg reg_heightIn_rsc_triosy_obj_ld_core_psct_cse;
  reg reg_acc_rsci_ivld_core_psct_cse;
  reg reg_data_in_rsci_irdy_core_psct_cse;
  reg reg_acc_tmp_rsc_cgo_cse;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_cse;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_cse;
  wire and_143_cse;
  reg [14:0] HACC_idx_acc_8_psp;
  reg [12:0] HACC_acc_10_psp;
  wire or_cse;
  wire or_125_rmff;
  reg [7:0] HACC_t_7_0_sva;
  wire [7:0] z_out_1;
  wire [8:0] nl_z_out_1;
  wire [15:0] z_out_2;
  wire [18:0] z_out_3;
  wire [19:0] nl_z_out_3;
  wire [32:0] z_out_4;
  wire or_tmp_463;
  wire or_tmp_464;
  wire [39:0] z_out_5;
  wire or_tmp_475;
  wire or_tmp_476;
  wire [34:0] z_out_6;
  wire [32:0] z_out_7;
  wire [33:0] nl_z_out_7;
  wire [32:0] z_out_8;
  wire [38:0] z_out_9;
  wire [31:0] data_out_out;
  reg [10:0] operator_11_false_io_read_widthIn_rsc_cse_sva;
  reg [9:0] operator_10_false_io_read_heightIn_rsc_cse_sva;
  reg [18:0] acc_tmp_vinit_ndx_sva;
  reg [9:0] HROW_y_sva;
  reg [4:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva;
  reg [4:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva;
  reg [31:0] ac_math_atan_pi_2mi_return_69_38_sva;
  reg [32:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva;
  reg [4:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1;
  reg [32:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4;
  reg [4:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1;
  reg [7:0] HACC_t_7_0_sva_1;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_itm;
  reg [34:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm;
  reg [37:0] HACC_mul_3_itm;
  reg [2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_32_30;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_29;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_28_27;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_26_25;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_24;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_23_22;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_21;
  reg [2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_20_18;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_17;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_16;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_15_14;
  reg [2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_13_11;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_10;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_9_8;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_7;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_6_5;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_4;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_3;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_2;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_1;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_0;
  reg [2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_32_30;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_29;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_28_27;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_26;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_25;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_24;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_23;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_22;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_21;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_20;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_19;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_18;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_17;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_16;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_15_14;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_13;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_12;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_11;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_10;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_9;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_8;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_7;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_6;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_5;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_4;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_3;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_0;
  reg [2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_32_30;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_29;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_28_27;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_26_25;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_24;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_23_22;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_21;
  reg [2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_20_18;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_17;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_16;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_15_14;
  reg [2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_13_11;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_10;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_9_8;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_7;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_6_5;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_4;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_3;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_2;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_1;
  reg [2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_32_30;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_29;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_28_27;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_26;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_25;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_24;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_23;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_22;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_21;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_20;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_19;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_18;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_17;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_16;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_15_14;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_13;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_12;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_11;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_10;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_9;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_8;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_7;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_6;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_5;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_4;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_3;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_2_1;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_0;
  wire HACC_idx_acc_3_psp_sva_mx0c0;
  wire HACC_idx_acc_3_psp_sva_mx0c3;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0;
  wire [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx1;
  wire [18:0] for_conc_3_itm_18_0;
  wire [18:0] for_conc_4_itm_18_0;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse;
  wire HACC_t_or_cse;
  wire and_954_cse;
  wire and_956_cse;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_33_cse;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_32_cse;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_and_cse;
  wire or_164_ssc;
  wire HACC_idx_and_ssc;
  wire HACC_idx_and_ssc_2;
  reg reg_HACC_idx_acc_3_psp_ftd;
  reg [1:0] reg_HACC_idx_acc_3_psp_ftd_1;
  reg [2:0] reg_HACC_idx_acc_3_psp_ftd_2;
  reg [10:0] reg_HACC_idx_acc_3_psp_ftd_3;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_or_itm;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_or_itm;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm;
  wire for_1_nor_2_seb;
  wire or_379_cse;
  wire [4:0] z_out_6_2;

  wire[18:0] for_and_nl;
  wire[18:0] for_mux1h_6_nl;
  wire[10:0] HCOL_x_HCOL_x_and_nl;
  wire[10:0] HCOL_x_mux_nl;
  wire[0:0] HCOL_x_nor_nl;
  wire[0:0] or_140_nl;
  wire[0:0] or_143_nl;
  wire[0:0] for_nor_nl;
  wire[0:0] or_568_nl;
  wire[11:0] HACC_mux1h_8_nl;
  wire[11:0] HACC_acc_5_nl;
  wire[12:0] nl_HACC_acc_5_nl;
  wire[0:0] or_516_nl;
  wire[0:0] or_163_nl;
  wire[9:0] HACC_mux_1_nl;
  wire[9:0] HACC_acc_12_nl;
  wire[10:0] nl_HACC_acc_12_nl;
  wire[0:0] or_165_nl;
  wire[0:0] or_462_nl;
  wire[0:0] T_LINE_if_aelse_mux_nl;
  wire[4:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_mux_1_nl;
  wire[0:0] not_224_nl;
  wire[0:0] T_LINE_if_aelse_mux1h_17_nl;
  wire[32:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_mux_nl;
  wire[31:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_1_nl;
  wire[31:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_1_nl;
  wire[0:0] or_484_nl;
  wire[31:0] and_nl;
  wire[0:0] not_226_nl;
  wire[0:0] or_486_nl;
  wire[3:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl;
  wire[4:0] nl_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl;
  wire[0:0] HACC_idx_HACC_idx_acc_conv_2f_or_nl;
  wire[0:0] HACC_idx_HACC_idx_acc_conv_2f_and_nl;
  wire[32:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_y_2mi_mux1h_nl;
  wire[0:0] or_497_nl;
  wire[0:0] or_506_nl;
  wire[3:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_acc_nl;
  wire[4:0] nl_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_acc_nl;
  wire[0:0] for_nor_1_nl;
  wire[0:0] for_nor_2_nl;
  wire[16:0] for_mux1h_2_nl;
  wire[1:0] for_mux1h_7_nl;
  wire[0:0] for_or_nl;
  wire[1:0] HACC_mux_nl;
  wire[0:0] HACC_idx_or_1_nl;
  wire[0:0] HACC_idx_or_2_nl;
  wire[0:0] HACC_idx_or_3_nl;
  wire[6:0] HACC_acc_nl;
  wire[7:0] nl_HACC_acc_nl;
  wire[6:0] HACC_mux_7_nl;
  wire[5:0] HACC_mux_8_nl;
  wire[7:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_3_nl;
  wire[16:0] acc_2_nl;
  wire[17:0] nl_acc_2_nl;
  wire[3:0] HCOL_and_2_nl;
  wire[3:0] HCOL_mux1h_4_nl;
  wire[0:0] HCOL_nor_3_nl;
  wire[0:0] HCOL_or_4_nl;
  wire[0:0] HCOL_mux1h_5_nl;
  wire[10:0] HCOL_mux1h_6_nl;
  wire[0:0] HCOL_or_5_nl;
  wire[14:0] HCOL_HCOL_nand_1_nl;
  wire[14:0] HCOL_mux1h_7_nl;
  wire[0:0] HCOL_or_6_nl;
  wire[0:0] HCOL_not_6_nl;
  wire[17:0] for_1_mux1h_5_nl;
  wire[0:0] for_1_for_1_or_2_nl;
  wire[0:0] for_1_for_1_and_2_nl;
  wire[1:0] for_1_for_1_and_3_nl;
  wire[0:0] for_1_for_1_or_3_nl;
  wire[33:0] acc_4_nl;
  wire[34:0] nl_acc_4_nl;
  wire[2:0] HROW_HROW_and_13_nl;
  wire[0:0] HROW_HROW_and_14_nl;
  wire[1:0] HROW_HROW_and_15_nl;
  wire[1:0] HROW_HROW_and_16_nl;
  wire[0:0] HROW_HROW_and_17_nl;
  wire[1:0] HROW_HROW_and_18_nl;
  wire[0:0] HROW_HROW_and_19_nl;
  wire[2:0] HROW_HROW_and_20_nl;
  wire[0:0] HROW_HROW_and_21_nl;
  wire[0:0] HROW_HROW_and_22_nl;
  wire[1:0] HROW_HROW_and_23_nl;
  wire[2:0] HROW_HROW_and_24_nl;
  wire[0:0] HROW_HROW_and_25_nl;
  wire[1:0] HROW_mux_9_nl;
  wire[0:0] HROW_mux_10_nl;
  wire[1:0] HROW_mux_11_nl;
  wire[0:0] HROW_mux_12_nl;
  wire[0:0] HROW_mux_13_nl;
  wire[0:0] HROW_mux_14_nl;
  wire[0:0] HROW_mux_15_nl;
  wire[0:0] HROW_mux_16_nl;
  wire[0:0] HROW_or_1_nl;
  wire[32:0] HROW_mux_17_nl;
  wire[40:0] acc_5_nl;
  wire[41:0] nl_acc_5_nl;
  wire[38:0] for_1_mux1h_6_nl;
  wire[0:0] for_1_or_8_nl;
  wire[0:0] for_1_or_9_nl;
  wire[0:0] for_1_or_10_nl;
  wire[21:0] for_1_or_11_nl;
  wire[21:0] for_1_and_4_nl;
  wire[21:0] for_1_mux1h_7_nl;
  wire[0:0] for_1_not_6_nl;
  wire[15:0] for_1_or_12_nl;
  wire[15:0] for_1_and_5_nl;
  wire[15:0] for_1_mux1h_8_nl;
  wire[0:0] for_1_nor_6_nl;
  wire[35:0] acc_6_nl;
  wire[36:0] nl_acc_6_nl;
  wire[2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_15_nl;
  wire[2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_15_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_29_nl;
  wire[1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_16_nl;
  wire[1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_16_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_30_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_17_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_17_nl;
  wire[1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_18_nl;
  wire[1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_18_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_32_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_19_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_19_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_20_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_20_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_21_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_21_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_22_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_22_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_23_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_23_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_24_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_24_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_25_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_25_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_26_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_26_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_27_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_27_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_28_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_28_nl;
  wire[15:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux1h_18_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_29_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_29_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_33_nl;
  wire[32:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux1h_19_nl;
  wire[16:0] HACC_idx_acc_9_nl;
  wire[17:0] nl_HACC_idx_acc_9_nl;
  wire[2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_13_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_45_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_14_nl;
  wire[1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_15_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_47_nl;
  wire[1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_16_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_48_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_17_nl;
  wire[1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_18_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_50_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_19_nl;
  wire[2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_20_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_52_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_21_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_22_nl;
  wire[1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_23_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_55_nl;
  wire[2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_24_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_56_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_25_nl;
  wire[1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_9_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_10_nl;
  wire[1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_11_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_12_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_13_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_14_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_15_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_16_nl;
  wire[32:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_1_nl;
  wire[11:0] HACC_mux_9_nl;
  wire[26:0] HACC_mux_10_nl;

  // Interconnect Declarations for Component Instantiations 
  wire[2:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_2_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_3_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_4_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_5_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_6_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_7_nl;
  wire[2:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_8_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_9_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_10_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_11_nl;
  wire[2:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_12_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_13_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_14_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_15_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_16_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_17_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_18_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_19_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux1h_20_nl;
  wire [32:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a;
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_nl = MUX1HOT_v_3_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_32_30,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_32_30,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_32_30,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_32_30,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_2_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_29,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_29,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_29,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_29,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_3_nl = MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_28_27,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_28_27,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_28_27,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_28_27,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_4_nl = MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_26_25,
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_26
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_25}),
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_26
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_25}),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_26_25,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_5_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_24,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_24,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_24,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_24,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_6_nl = MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_23_22,
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_23
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_22}),
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_23
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_22}),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_23_22,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_7_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_21,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_21,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_21,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_21,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_8_nl = MUX1HOT_v_3_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_20_18,
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_20
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_19
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_18}),
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_20
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_19
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_18}),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_20_18,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_9_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_17,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_17,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_17,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_17,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_10_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_16,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_16,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_16,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_16,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_11_nl = MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_15_14,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_15_14,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_15_14,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_15_14,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_12_nl = MUX1HOT_v_3_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_13_11,
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_13
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_12
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_11}),
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_13
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_12
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_11}),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_13_11,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_13_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_10,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_10,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_10,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_10,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_14_nl = MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_9_8,
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_9
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_8}),
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_9
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_8}),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_9_8,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_15_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_7,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_7,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_7,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_7,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_16_nl = MUX1HOT_v_2_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_6_5,
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_6
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_5}),
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_6
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_5}),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_6_5,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_17_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_4,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_4,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_4,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_4,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_18_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_3,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_3,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_3,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_3,
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_19_nl = MUX1HOT_v_2_4_2(({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_2
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_1}),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_2_1,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1,
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_2
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_1}),
      {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9]) , (fsm_output[11])});
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux1h_20_nl = MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_0,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_0,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_0,
      HACC_slc_HACC_acc_6_itm, {(fsm_output[10]) , (fsm_output[8]) , (fsm_output[9])
      , (fsm_output[11])});
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a = {operator_33_3_true_AC_TRN_AC_WRAP_mux1h_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_2_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_3_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_4_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_5_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_6_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_7_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_8_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_9_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_10_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_11_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_12_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_13_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_14_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_15_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_16_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_17_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_18_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_19_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux1h_20_nl};
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_or_1_nl;
  wire [4:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s;
  assign operator_33_3_true_AC_TRN_AC_WRAP_or_1_nl = (fsm_output[8]) | (fsm_output[11]);
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s = MUX_v_5_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva,
      operator_33_3_true_AC_TRN_AC_WRAP_or_1_nl);
  wire[4:0] ac_math_atan_pi_2mi_mux_nl;
  wire [6:0] nl_ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr;
  assign ac_math_atan_pi_2mi_mux_nl = MUX_v_5_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva,
      fsm_output[9]);
  assign nl_ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr
      = {2'b0, ac_math_atan_pi_2mi_mux_nl};
  wire [0:0] nl_houghTransform_core_core_fsm_inst_acc_tmp_vinit_C_0_tr0;
  assign nl_houghTransform_core_core_fsm_inst_acc_tmp_vinit_C_0_tr0 = for_for_nor_tmp;
  wire [0:0] nl_houghTransform_core_core_fsm_inst_HCOL_C_0_tr0;
  assign nl_houghTransform_core_core_fsm_inst_HCOL_C_0_tr0 = ~ (z_out_3[8]);
  wire [0:0] nl_houghTransform_core_core_fsm_inst_HACC_C_15_tr0;
  assign nl_houghTransform_core_core_fsm_inst_HACC_C_15_tr0 = ~ HACC_slc_HACC_acc_6_itm;
  wire [0:0] nl_houghTransform_core_core_fsm_inst_for_1_C_2_tr0;
  assign nl_houghTransform_core_core_fsm_inst_for_1_C_2_tr0 = ~ HACC_slc_HACC_acc_6_itm;
  mgc_shift_r_v5 #(.width_a(32'sd33),
  .signd_a(32'sd1),
  .width_s(32'sd5),
  .width_z(32'sd33)) operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg (
      .a(nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a[32:0]),
      .s(nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s[4:0]),
      .z(z_out_8)
    );
  Hough_Algorithm_HW_1296_864mgc_rom_22_70_32_1_60  ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg
      (
      .addr(nl_ac_math_atan_pi_2mi_1_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr[6:0]),
      .data_out(data_out_out)
    );
  houghTransform_core_data_in_rsci houghTransform_core_data_in_rsci_inst (
      .clk(clk),
      .rst(rst),
      .data_in_rsc_dat(data_in_rsc_dat),
      .data_in_rsc_vld(data_in_rsc_vld),
      .data_in_rsc_rdy(data_in_rsc_rdy),
      .core_wen(core_wen),
      .data_in_rsci_oswt(reg_data_in_rsci_irdy_core_psct_cse),
      .data_in_rsci_wen_comp(data_in_rsci_wen_comp),
      .data_in_rsci_idat_mxwt(data_in_rsci_idat_mxwt)
    );
  houghTransform_core_acc_rsci houghTransform_core_acc_rsci_inst (
      .clk(clk),
      .rst(rst),
      .acc_rsc_dat(acc_rsc_dat),
      .acc_rsc_vld(acc_rsc_vld),
      .acc_rsc_rdy(acc_rsc_rdy),
      .core_wen(core_wen),
      .acc_rsci_oswt(reg_acc_rsci_ivld_core_psct_cse),
      .acc_rsci_wen_comp(acc_rsci_wen_comp),
      .acc_rsci_idat(acc_rsci_idat)
    );
  houghTransform_core_wait_dp houghTransform_core_wait_dp_inst (
      .clk(clk),
      .rst(rst),
      .acc_tmp_rsc_cgo_iro(or_125_rmff),
      .acc_tmp_rsci_data_out_d(acc_tmp_rsci_data_out_d),
      .acc_tmp_rsci_en_d(acc_tmp_rsci_en_d),
      .core_wen(core_wen),
      .acc_tmp_rsc_cgo(reg_acc_tmp_rsc_cgo_cse),
      .acc_tmp_rsci_data_out_d_oreg(acc_tmp_rsci_data_out_d_oreg)
    );
  houghTransform_core_widthIn_rsc_triosy_obj houghTransform_core_widthIn_rsc_triosy_obj_inst
      (
      .widthIn_rsc_triosy_lz(widthIn_rsc_triosy_lz),
      .core_wten(core_wten),
      .widthIn_rsc_triosy_obj_iswt0(reg_heightIn_rsc_triosy_obj_ld_core_psct_cse)
    );
  houghTransform_core_heightIn_rsc_triosy_obj houghTransform_core_heightIn_rsc_triosy_obj_inst
      (
      .heightIn_rsc_triosy_lz(heightIn_rsc_triosy_lz),
      .core_wten(core_wten),
      .heightIn_rsc_triosy_obj_iswt0(reg_heightIn_rsc_triosy_obj_ld_core_psct_cse)
    );
  houghTransform_core_staller houghTransform_core_staller_inst (
      .clk(clk),
      .rst(rst),
      .core_wen(core_wen),
      .core_wten(core_wten),
      .data_in_rsci_wen_comp(data_in_rsci_wen_comp),
      .acc_rsci_wen_comp(acc_rsci_wen_comp)
    );
  houghTransform_core_core_fsm houghTransform_core_core_fsm_inst (
      .clk(clk),
      .rst(rst),
      .core_wen(core_wen),
      .fsm_output(fsm_output),
      .acc_tmp_vinit_C_0_tr0(nl_houghTransform_core_core_fsm_inst_acc_tmp_vinit_C_0_tr0[0:0]),
      .HCOL_C_0_tr0(nl_houghTransform_core_core_fsm_inst_HCOL_C_0_tr0[0:0]),
      .ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_5_tr0(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_nor_itm),
      .HACC_C_15_tr0(nl_houghTransform_core_core_fsm_inst_HACC_C_15_tr0[0:0]),
      .HCOL_C_1_tr0(HCOL_equal_tmp),
      .HROW_C_0_tr0(HROW_equal_tmp),
      .for_1_C_2_tr0(nl_houghTransform_core_core_fsm_inst_for_1_C_2_tr0[0:0])
    );
  assign or_125_rmff = (fsm_output[23]) | (fsm_output[24]) | (fsm_output[22]) | (fsm_output[28])
      | (fsm_output[1]) | (fsm_output[25]) | (fsm_output[2]) | and_186_cse | and_143_cse;
  assign and_143_cse = HACC_slc_HACC_acc_6_itm & (fsm_output[30]);
  assign HACC_t_or_cse = (fsm_output[25]) | (fsm_output[3]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_cse
      = core_wen & ((fsm_output[7]) | (fsm_output[13]));
  assign or_379_cse = (fsm_output[9:8]!=2'b00);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_33_cse
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs
      & (fsm_output[13]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_32_cse
      = (~ ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs)
      & (fsm_output[13]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_32_cse
      | ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_33_cse;
  assign or_cse = (fsm_output[8]) | (fsm_output[13]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_and_cse
      = core_wen & (~(or_dcpl_35 | or_dcpl_33));
  assign and_954_cse = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs
      & (fsm_output[12]);
  assign and_956_cse = (~ ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs)
      & (fsm_output[12]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_or_itm
      = and_954_cse | and_956_cse;
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_cse
      = core_wen & (fsm_output[9]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_or_itm
      = or_tmp_419 | or_tmp_420;
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0
      = (z_out_6_2[0]) & (~ (z_out_3[18]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0
      = ~((z_out_6_2[0]) | (z_out_3[18]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx1
      = MUX_v_2_2_2((signext_2_1(z_out_6_2[0])), 2'b01, z_out_3[18]);
  assign HROW_equal_tmp = HROW_y_sva == (z_out_7[9:0]);
  assign HCOL_equal_tmp = (acc_tmp_vinit_ndx_sva[10:0]) == (z_out_2[10:0]);
  assign for_for_nor_tmp = ~((acc_tmp_vinit_ndx_sva!=19'b0000000000000000000));
  assign and_137_cse = (~ HROW_equal_tmp) & (fsm_output[27]);
  assign and_dcpl_13 = ~((fsm_output[0]) | (fsm_output[31]));
  assign or_dcpl_22 = (fsm_output[0]) | (fsm_output[31]);
  assign or_dcpl_33 = (fsm_output[11:10]!=2'b00);
  assign or_dcpl_34 = or_dcpl_33 | or_cse;
  assign or_dcpl_35 = (fsm_output[9]) | (fsm_output[12]);
  assign or_dcpl_36 = (fsm_output[7:6]!=2'b00);
  assign or_dcpl_54 = (fsm_output[21:20]!=2'b00);
  assign or_dcpl_58 = (fsm_output[10]) | (fsm_output[8]);
  assign or_dcpl_60 = (fsm_output[12:11]!=2'b00);
  assign or_dcpl_61 = (fsm_output[7]) | (fsm_output[9]);
  assign and_dcpl_32 = ~((fsm_output[10]) | (fsm_output[8]) | (fsm_output[13]));
  assign or_dcpl_73 = (fsm_output[14:13]!=2'b00);
  assign and_186_cse = HROW_equal_tmp & (fsm_output[27]);
  assign and_195_cse = for_for_nor_tmp & (fsm_output[2]);
  assign and_198_cse = (~ for_for_nor_tmp) & (fsm_output[2]);
  assign or_tmp_57 = and_dcpl_13 & (~ (fsm_output[28])) & (~((fsm_output[29]) | (fsm_output[1])))
      & (~ (fsm_output[30]));
  assign or_tmp_77 = or_dcpl_36 | (fsm_output[5]) | or_dcpl_35 | or_dcpl_34;
  assign and_435_cse = (z_out_3[18]) & (fsm_output[7]);
  assign or_tmp_167 = (~ (z_out_3[18])) & (fsm_output[7]);
  assign or_tmp_302 = ~((fsm_output[9]) | (fsm_output[8]) | (fsm_output[13]));
  assign or_tmp_306 = or_dcpl_35 | (fsm_output[11]) | or_dcpl_58;
  assign or_tmp_404 = or_dcpl_60 | (fsm_output[10]);
  assign or_tmp_419 = HACC_idx_HACC_idx_acc_conv_2f_and_itm & (fsm_output[11]);
  assign or_tmp_420 = (~ HACC_idx_HACC_idx_acc_conv_2f_and_itm) & (fsm_output[11]);
  assign HACC_idx_acc_3_psp_sva_mx0c0 = (fsm_output[3]) | (fsm_output[25]) | (fsm_output[14]);
  assign HACC_idx_acc_3_psp_sva_mx0c3 = (fsm_output[21:19]!=3'b000);
  assign for_conc_3_itm_18_0 = MUX_v_19_2_2(19'b1100001101001111111, (z_out_5[18:0]),
      fsm_output[2]);
  assign for_conc_4_itm_18_0 = MUX_v_19_2_2(19'b0000000000000000000, acc_tmp_vinit_ndx_sva,
      (fsm_output[30]));
  assign or_131_ssc = (fsm_output[2:1]!=2'b00);
  assign or_134_ssc = (fsm_output[30]) | (fsm_output[27]);
  assign for_nor_1_nl = ~(and_186_cse | and_143_cse | (fsm_output[22]));
  assign acc_tmp_rsci_re_d = {1'b1 , for_nor_1_nl};
  assign for_nor_2_nl = ~((fsm_output[24]) | (fsm_output[1]) | and_198_cse);
  assign acc_tmp_rsci_we_d = {1'b1 , for_nor_2_nl};
  assign acc_tmp_rsci_data_in_d = MUX_v_16_2_2(16'b0000000000000000, z_out_2, (fsm_output[24]));
  assign for_mux1h_2_nl = MUX1HOT_v_17_4_2((for_conc_3_itm_18_0[18:2]), (z_out_6[16:0]),
      ({reg_HACC_idx_acc_3_psp_ftd , reg_HACC_idx_acc_3_psp_ftd_1 , reg_HACC_idx_acc_3_psp_ftd_2
      , reg_HACC_idx_acc_3_psp_ftd_3}), (for_conc_4_itm_18_0[18:2]), {or_131_ssc
      , (fsm_output[22]) , (fsm_output[24]) , or_134_ssc});
  assign for_or_nl = (fsm_output[22]) | (fsm_output[24]);
  assign for_mux1h_7_nl = MUX1HOT_v_2_3_2((for_conc_3_itm_18_0[1:0]), (HACC_t_7_0_sva[1:0]),
      (for_conc_4_itm_18_0[1:0]), {or_131_ssc , for_or_nl , or_134_ssc});
  assign acc_tmp_rsci_addr_d = {for_mux1h_2_nl , for_mux1h_7_nl};
  assign or_tmp_463 = (~ ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs)
      & (fsm_output[10]);
  assign or_tmp_464 = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs
      & (fsm_output[10]);
  assign or_tmp_475 = (~ HACC_idx_HACC_idx_acc_conv_2f_and_itm) & (fsm_output[9]);
  assign or_tmp_476 = HACC_idx_HACC_idx_acc_conv_2f_and_itm & (fsm_output[9]);
  assign or_164_ssc = or_dcpl_36 | or_dcpl_35 | or_dcpl_34;
  assign HACC_idx_and_ssc = (~ or_dcpl_54) & HACC_idx_acc_3_psp_sva_mx0c3;
  assign HACC_idx_and_ssc_2 = core_wen & (HACC_idx_acc_3_psp_sva_mx0c0 | or_tmp_77
      | (fsm_output[18]) | HACC_idx_acc_3_psp_sva_mx0c3 | (fsm_output[22]));
  assign for_1_nor_2_seb = ~((fsm_output[8:7]!=2'b00));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm
      = or_tmp_475 | or_tmp_476;
  always @(posedge clk) begin
    if ( rst ) begin
      reg_heightIn_rsc_triosy_obj_ld_core_psct_cse <= 1'b0;
      reg_acc_tmp_rsc_cgo_cse <= 1'b0;
      reg_acc_rsci_ivld_core_psct_cse <= 1'b0;
      reg_data_in_rsci_irdy_core_psct_cse <= 1'b0;
      operator_10_false_io_read_heightIn_rsc_cse_sva <= 10'b0000000000;
      operator_11_false_io_read_widthIn_rsc_cse_sva <= 11'b00000000000;
      acc_tmp_vinit_ndx_sva <= 19'b0000000000000000000;
      HACC_acc_10_psp <= 13'b0000000000000;
      HACC_idx_acc_8_psp <= 15'b000000000000000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_4_dfm_1_34_32
          <= 3'b000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_32_30
          <= 3'b000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_29
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_28_27
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_26
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_25
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_24
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_23
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_22
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_21
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_20
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_19
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_18
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_17
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_16
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_15_14
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_13
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_12
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_11
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_10
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_9
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_8
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_7
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_6
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_5
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_4
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_3
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_0
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva
          <= 5'b00000;
      HACC_mul_2_itm <= 39'b000000000000000000000000000000000000000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm
          <= 35'b00000000000000000000000000000000000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_itm
          <= 1'b0;
      ac_math_atan_pi_2mi_return_69_38_sva <= 32'b00000000000000000000000000000000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva
          <= 33'b000000000000000000000000000000000;
    end
    else if ( core_wen ) begin
      reg_heightIn_rsc_triosy_obj_ld_core_psct_cse <= (~ HACC_slc_HACC_acc_6_itm)
          & (fsm_output[30]);
      reg_acc_tmp_rsc_cgo_cse <= or_125_rmff;
      reg_acc_rsci_ivld_core_psct_cse <= fsm_output[29];
      reg_data_in_rsci_irdy_core_psct_cse <= and_137_cse | ((~ HCOL_equal_tmp) &
          (fsm_output[26])) | and_195_cse;
      operator_10_false_io_read_heightIn_rsc_cse_sva <= MUX_v_10_2_2(heightIn_rsci_idat,
          operator_10_false_io_read_heightIn_rsc_cse_sva, or_tmp_57);
      operator_11_false_io_read_widthIn_rsc_cse_sva <= MUX_v_11_2_2(widthIn_rsci_idat,
          operator_11_false_io_read_widthIn_rsc_cse_sva, or_tmp_57);
      acc_tmp_vinit_ndx_sva <= MUX_v_19_2_2(for_and_nl, (z_out_5[18:0]), or_568_nl);
      HACC_acc_10_psp <= MUX1HOT_v_13_3_2((z_out_2[12:0]), HACC_acc_10_psp, ({1'b0
          , HACC_mux1h_8_nl}), {(fsm_output[4]) , or_tmp_77 , or_163_nl});
      HACC_idx_acc_8_psp <= MUX_v_15_2_2(({5'b00000 , HACC_mux_1_nl}), (z_out_2[14:0]),
          fsm_output[21]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_4_dfm_1_34_32
          <= MUX1HOT_v_3_3_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx1[1]}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx1}),
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_4_dfm_1_34_32,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm[34:32]),
          {(fsm_output[7]) , or_379_cse , (fsm_output[13])});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_32_30
          <= MUX1HOT_v_3_3_2(({{2{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}),
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_32_30,
          (z_out_5[32:30]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_29
          <= MUX1HOT_s_1_3_2((z_out_3[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_29,
          (z_out_5[29]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_28_27
          <= MUX1HOT_v_2_3_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}),
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_28_27,
          (z_out_5[28:27]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_26
          <= MUX1HOT_s_1_3_2((z_out_3[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_26,
          (z_out_5[26]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_25
          <= MUX1HOT_s_1_3_2((z_out_3[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_25,
          (z_out_5[25]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_24
          <= MUX1HOT_s_1_3_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_24,
          (z_out_5[24]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_23
          <= MUX1HOT_s_1_3_2((z_out_3[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_23,
          (z_out_5[23]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_22
          <= MUX1HOT_s_1_3_2((z_out_3[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_22,
          (z_out_5[22]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_21
          <= MUX1HOT_s_1_3_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_21,
          (z_out_5[21]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_20
          <= MUX1HOT_s_1_3_2((z_out_3[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_20,
          (z_out_5[20]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_19
          <= MUX1HOT_s_1_3_2((z_out_3[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_19,
          (z_out_5[19]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_18
          <= MUX1HOT_s_1_3_2((z_out_3[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_18,
          (z_out_5[18]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_17
          <= MUX1HOT_s_1_3_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_17,
          (z_out_5[17]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_16
          <= MUX1HOT_s_1_3_2((z_out_3[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_16,
          (z_out_5[16]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_15_14
          <= MUX1HOT_v_2_3_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}),
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_15_14,
          (z_out_5[15:14]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_13
          <= MUX1HOT_s_1_3_2((z_out_3[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_13,
          (z_out_5[13]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_12
          <= MUX1HOT_s_1_3_2((z_out_3[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_12,
          (z_out_5[12]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_11
          <= MUX1HOT_s_1_3_2((z_out_3[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_11,
          (z_out_5[11]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_10
          <= MUX1HOT_s_1_3_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_10,
          (z_out_5[10]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_9
          <= MUX1HOT_s_1_3_2((z_out_3[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_9,
          (z_out_5[9]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_8
          <= MUX1HOT_s_1_3_2((z_out_3[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_8,
          (z_out_5[8]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_7
          <= MUX1HOT_s_1_3_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_7,
          (z_out_5[7]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_6
          <= MUX1HOT_s_1_3_2((z_out_3[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_6,
          (z_out_5[6]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_5
          <= MUX1HOT_s_1_3_2((z_out_3[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_5,
          (z_out_5[5]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_4
          <= MUX1HOT_s_1_3_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_4,
          (z_out_5[4]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_3
          <= MUX1HOT_s_1_3_2((z_out_3[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_3,
          (z_out_5[3]), {(fsm_output[7]) , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1
          <= MUX1HOT_v_2_4_2(2'b01, (signext_2_1(z_out_6_2[0])), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1,
          (z_out_5[2:1]), {or_462_nl , or_tmp_167 , or_tmp_306 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_0
          <= T_LINE_if_aelse_mux_nl & (~((~((fsm_output[9]) | (fsm_output[12]) |
          (fsm_output[11]))) & and_dcpl_32));
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva
          <= MUX_v_5_2_2(5'b00000, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_mux_1_nl,
          not_224_nl);
      HACC_mul_2_itm <= MUX1HOT_v_39_3_2(({6'b000000 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_mux_nl}),
          ({7'b0000000 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_1_nl}),
          z_out_9, {or_tmp_404 , or_484_nl , (fsm_output[16])});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm
          <= MUX1HOT_v_35_3_2(({3'b000 , and_nl}), (z_out_5[34:0]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm,
          {or_486_nl , (fsm_output[10]) , or_dcpl_60});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_itm
          <= readslicef_4_1_3(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl);
      ac_math_atan_pi_2mi_return_69_38_sva <= MUX_v_32_2_2(data_out_out, ac_math_atan_pi_2mi_return_69_38_sva,
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva
          <= MUX1HOT_v_33_3_2(z_out_8, (z_out_5[32:0]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva,
          {(fsm_output[9]) , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_or_itm
          , or_506_nl});
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      acc_rsci_idat <= 16'b0000000000000000;
    end
    else if ( core_wen & (fsm_output[29]) ) begin
      acc_rsci_idat <= acc_tmp_rsci_data_out_d_oreg;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HROW_y_sva <= 10'b0000000000;
    end
    else if ( core_wen & ((fsm_output[2]) | (fsm_output[27])) ) begin
      HROW_y_sva <= MUX_v_10_2_2(10'b0000000000, (z_out_4[9:0]), (fsm_output[27]));
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HACC_t_7_0_sva <= 8'b00000000;
    end
    else if ( core_wen & HACC_t_or_cse ) begin
      HACC_t_7_0_sva <= MUX_v_8_2_2(8'b00000000, HACC_t_7_0_sva_1, (fsm_output[25]));
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_32_30
          <= 3'b000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_29
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_28_27
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_26
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_25
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_24
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_23
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_22
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_21
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_20
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_19
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_18
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_17
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_16
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_15_14
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_13
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_12
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_11
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_10
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_9
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_8
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_7
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_6
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_5
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_4
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_3
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_0
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_32_30
          <= 3'b000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_29
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_28_27
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_26_25
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_24
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_23_22
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_21
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_20_18
          <= 3'b000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_17
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_16
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_15_14
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_13_11
          <= 3'b000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_10
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_9_8
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_7
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_6_5
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_4
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_3
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_2
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_1
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_0
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_32_30
          <= 3'b000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_29
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_28_27
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_26_25
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_24
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_23_22
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_21
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_20_18
          <= 3'b000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_17
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_16
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_15_14
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_13_11
          <= 3'b000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_10
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_9_8
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_7
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_6_5
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_4
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_3
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_2
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_1
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva
          <= 5'b00000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_4_dfm_1_34_32
          <= 3'b000;
    end
    else if ( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_cse
        ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_32_30
          <= MUX_v_3_2_2(({{2{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}),
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[32:30]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_29
          <= MUX_s_1_2_2((z_out_3[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[29]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_28_27
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}),
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[28:27]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_26
          <= MUX_s_1_2_2((z_out_3[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[26]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_25
          <= MUX_s_1_2_2((z_out_3[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[25]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_24
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[24]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_23
          <= MUX_s_1_2_2((z_out_3[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[23]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_22
          <= MUX_s_1_2_2((z_out_3[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[22]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_21
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[21]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_20
          <= MUX_s_1_2_2((z_out_3[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[20]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_19
          <= MUX_s_1_2_2((z_out_3[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[19]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_18
          <= MUX_s_1_2_2((z_out_3[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[18]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_17
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[17]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_16
          <= MUX_s_1_2_2((z_out_3[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[16]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_15_14
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}),
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[15:14]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_13
          <= MUX_s_1_2_2((z_out_3[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[13]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_12
          <= MUX_s_1_2_2((z_out_3[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[12]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_11
          <= MUX_s_1_2_2((z_out_3[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[11]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_10
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[10]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_9
          <= MUX_s_1_2_2((z_out_3[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[9]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_8
          <= MUX_s_1_2_2((z_out_3[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[8]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_7
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[7]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_6
          <= MUX_s_1_2_2((z_out_3[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[6]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_5
          <= MUX_s_1_2_2((z_out_3[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[5]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_4
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[4]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_3
          <= MUX_s_1_2_2((z_out_3[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[3]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_0
          <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[0])
          & (fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_32_30
          <= MUX_v_3_2_2(3'b000, (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[32:30]),
          (fsm_output[13]));
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_29
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[29]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_28_27
          <= MUX_v_2_2_2(2'b00, (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[28:27]),
          (fsm_output[13]));
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_26_25
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[26:25]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_24
          <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[24])
          & (fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_23_22
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[23:22]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_21
          <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[21])
          & (fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_20_18
          <= MUX_v_3_2_2(({{2{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[20:18]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_17
          <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[17])
          & (fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_16
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[16]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_15_14
          <= MUX_v_2_2_2(2'b00, (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[15:14]),
          (fsm_output[13]));
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_13_11
          <= MUX_v_3_2_2(({{2{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[13:11]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_10
          <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[10])
          & (fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_9_8
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[9:8]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_7
          <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[7])
          & (fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_6_5
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[6:5]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_4
          <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[4])
          & (fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_3
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[3]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_2
          <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[2])
          & (fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_1
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[1]),
          fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_0
          <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[0])
          & (fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_32_30
          <= MUX_v_3_2_2(3'b000, (HACC_mul_3_itm[32:30]), (fsm_output[13]));
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_29
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
          (HACC_mul_3_itm[29]), fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_28_27
          <= MUX_v_2_2_2(2'b00, (HACC_mul_3_itm[28:27]), (fsm_output[13]));
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_26_25
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (HACC_mul_3_itm[26:25]), fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_24
          <= (HACC_mul_3_itm[24]) & (fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_23_22
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (HACC_mul_3_itm[23:22]), fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_21
          <= (HACC_mul_3_itm[21]) & (fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_20_18
          <= MUX_v_3_2_2(({{2{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (HACC_mul_3_itm[20:18]), fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_17
          <= (HACC_mul_3_itm[17]) & (fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_16
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
          (HACC_mul_3_itm[16]), fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_15_14
          <= MUX_v_2_2_2(2'b00, (HACC_mul_3_itm[15:14]), (fsm_output[13]));
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_13_11
          <= MUX_v_3_2_2(({{2{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (HACC_mul_3_itm[13:11]), fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_10
          <= (HACC_mul_3_itm[10]) & (fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_9_8
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (HACC_mul_3_itm[9:8]), fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_7
          <= (HACC_mul_3_itm[7]) & (fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_6_5
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (HACC_mul_3_itm[6:5]), fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_4
          <= (HACC_mul_3_itm[4]) & (fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_3
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
          (HACC_mul_3_itm[3]), fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_2
          <= (HACC_mul_3_itm[2]) & (fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_1
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
          (HACC_mul_3_itm[1]), fsm_output[13]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva
          <= MUX_v_5_2_2(5'b00000, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1,
          (fsm_output[13]));
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_4_dfm_1_34_32
          <= MUX_v_3_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx1[1]}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx1}),
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm[34:32]),
          fsm_output[13]);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_2_1
          <= 2'b00;
    end
    else if ( core_wen & (and_435_cse | or_tmp_167 | (fsm_output[13])) ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_2_1
          <= MUX1HOT_v_2_3_2(2'b01, (signext_2_1(z_out_6_2[0])), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[2:1]),
          {and_435_cse , or_tmp_167 , (fsm_output[13])});
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HACC_slc_HACC_acc_6_itm <= 1'b0;
    end
    else if ( core_wen & ((fsm_output[7]) | (fsm_output[29]) | (fsm_output[15]) |
        (fsm_output[13])) ) begin
      HACC_slc_HACC_acc_6_itm <= T_LINE_if_aelse_mux1h_17_nl & (~ (fsm_output[7]));
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1
          <= 5'b00000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1
          <= 5'b00000;
    end
    else if ( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_and_cse
        ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1
          <= z_out_1[4:0];
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1
          <= z_out_3[4:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HACC_idx_HACC_idx_acc_conv_2f_and_itm <= 1'b0;
    end
    else if ( core_wen & ((fsm_output[18]) | (fsm_output[17]) | (fsm_output[8]))
        ) begin
      HACC_idx_HACC_idx_acc_conv_2f_and_itm <= MUX1HOT_s_1_3_2((z_out_5[35]), HACC_idx_HACC_idx_acc_conv_2f_or_nl,
          HACC_idx_HACC_idx_acc_conv_2f_and_nl, {(fsm_output[8]) , (fsm_output[17])
          , (fsm_output[18])});
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HACC_mul_3_itm <= 38'b00000000000000000000000000000000000000;
    end
    else if ( core_wen & ((fsm_output[15]) | or_tmp_306) ) begin
      HACC_mul_3_itm <= MUX_v_38_2_2(({5'b00000 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_y_2mi_mux1h_nl}),
          (z_out_9[37:0]), fsm_output[15]);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_nor_itm
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs
          <= 1'b0;
    end
    else if ( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_cse
        ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_nor_itm
          <= ~(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_3_itm
          | (readslicef_4_1_3(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_acc_nl)));
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_35_svs
          <= z_out_5[35];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4
          <= 33'b000000000000000000000000000000000;
    end
    else if ( core_wen & (~((fsm_output[12]) | (fsm_output[10]) | or_dcpl_73)) )
        begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4
          <= MUX_v_33_2_2(z_out_4, (z_out_6[32:0]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_or_itm);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm
          <= 35'b00000000000000000000000000000000000;
    end
    else if ( core_wen & (~ or_tmp_404) ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm
          <= z_out_6;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HACC_t_7_0_sva_1 <= 8'b00000000;
    end
    else if ( core_wen & (fsm_output[14]) ) begin
      HACC_t_7_0_sva_1 <= z_out_1;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      reg_HACC_slc_HACC_acc_7_39_14_psp_ftd <= 16'b0000000000000000;
    end
    else if ( core_wen & (fsm_output[17]) ) begin
      reg_HACC_slc_HACC_acc_7_39_14_psp_ftd <= z_out_5[39:24];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      reg_HACC_idx_acc_3_psp_ftd <= 1'b0;
      reg_HACC_idx_acc_3_psp_ftd_1 <= 2'b00;
      reg_HACC_idx_acc_3_psp_ftd_2 <= 3'b000;
      reg_HACC_idx_acc_3_psp_ftd_3 <= 11'b00000000000;
    end
    else if ( HACC_idx_and_ssc_2 ) begin
      reg_HACC_idx_acc_3_psp_ftd <= z_out_6[16];
      reg_HACC_idx_acc_3_psp_ftd_1 <= MUX1HOT_v_2_3_2((z_out_2[15:14]), reg_HACC_idx_acc_3_psp_ftd_1,
          (z_out_6[15:14]), {HACC_idx_and_ssc , or_dcpl_54 , (fsm_output[22])});
      reg_HACC_idx_acc_3_psp_ftd_2 <= MUX1HOT_v_3_4_2(({1'b0 , HACC_mux_nl}), (z_out_2[13:11]),
          reg_HACC_idx_acc_3_psp_ftd_2, (z_out_6[13:11]), {or_tmp_77 , HACC_idx_or_1_nl
          , or_dcpl_54 , (fsm_output[22])});
      reg_HACC_idx_acc_3_psp_ftd_3 <= MUX1HOT_v_11_3_2((z_out_2[10:0]), reg_HACC_idx_acc_3_psp_ftd_3,
          (z_out_6[10:0]), {HACC_idx_or_2_nl , HACC_idx_or_3_nl , (fsm_output[22])});
    end
  end
  assign HCOL_x_mux_nl = MUX_v_11_2_2((acc_tmp_vinit_ndx_sva[10:0]), reg_HACC_idx_acc_3_psp_ftd_3,
      fsm_output[26]);
  assign HCOL_x_nor_nl = ~(or_dcpl_22 | (fsm_output[29]) | (fsm_output[30]) | (fsm_output[2])
      | (fsm_output[27]));
  assign HCOL_x_HCOL_x_and_nl = MUX_v_11_2_2(11'b00000000000, HCOL_x_mux_nl, HCOL_x_nor_nl);
  assign or_140_nl = (~((~ and_dcpl_13) | (fsm_output[28]) | (fsm_output[29]) | (fsm_output[1])
      | (fsm_output[30]) | (fsm_output[2]) | (fsm_output[27]))) | and_137_cse | and_195_cse;
  assign or_143_nl = (fsm_output[30:29]!=2'b00);
  assign for_mux1h_6_nl = MUX1HOT_v_19_3_2(19'b1100001101001111111, ({8'b00000000
      , HCOL_x_HCOL_x_and_nl}), acc_tmp_vinit_ndx_sva, {(fsm_output[1]) , or_140_nl
      , or_143_nl});
  assign for_nor_nl = ~(or_dcpl_22 | and_186_cse);
  assign for_and_nl = MUX_v_19_2_2(19'b0000000000000000000, for_mux1h_6_nl, for_nor_nl);
  assign or_568_nl = and_198_cse | (fsm_output[28]);
  assign nl_HACC_acc_5_nl = conv_u2s_11_12(acc_tmp_vinit_ndx_sva[10:0]) + conv_s2s_11_12({1'b1
      , (~ (operator_11_false_io_read_widthIn_rsc_cse_sva[10:1]))}) + 12'b000000000001;
  assign HACC_acc_5_nl = nl_HACC_acc_5_nl[11:0];
  assign or_516_nl = (fsm_output[21]) | (fsm_output[15]);
  assign HACC_mux1h_8_nl = MUX1HOT_v_12_3_2(HACC_acc_5_nl, (HACC_acc_10_psp[11:0]),
      (z_out_2[11:0]), {(fsm_output[14]) , or_516_nl , (fsm_output[20])});
  assign or_163_nl = or_dcpl_54 | (fsm_output[15:14]!=2'b00);
  assign nl_HACC_acc_12_nl = conv_s2u_9_10({(reg_HACC_idx_acc_3_psp_ftd_2[1:0]) ,
      (reg_HACC_idx_acc_3_psp_ftd_3[10:4])}) + ({(HACC_t_7_0_sva[6:0]) , 3'b111});
  assign HACC_acc_12_nl = nl_HACC_acc_12_nl[9:0];
  assign or_165_nl = or_dcpl_61 | or_dcpl_60 | or_dcpl_58 | (fsm_output[13]);
  assign HACC_mux_1_nl = MUX_v_10_2_2(HACC_acc_12_nl, (HACC_idx_acc_8_psp[9:0]),
      or_165_nl);
  assign or_462_nl = (~((fsm_output[7]) | (fsm_output[9]) | (fsm_output[12]) | (fsm_output[11])
      | (~ and_dcpl_32))) | and_435_cse;
  assign T_LINE_if_aelse_mux_nl = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_0,
      (z_out_5[0]), fsm_output[13]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_mux_1_nl
      = MUX_v_5_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1,
      fsm_output[13]);
  assign not_224_nl = ~ or_tmp_302;
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_mux_nl
      = MUX_v_33_2_2(z_out_8, (HACC_mul_2_itm[32:0]), or_dcpl_60);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_1_nl
      = MUX_v_32_2_2((HACC_mul_2_itm[31:0]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_2_itm[31:0]),
      fsm_output[13]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_1_nl
      = MUX_v_32_2_2(32'b00000000000000000000000000000000, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_1_nl,
      or_cse);
  assign or_484_nl = (fsm_output[7]) | (fsm_output[8]) | (fsm_output[13]);
  assign not_226_nl = ~ or_tmp_302;
  assign and_nl = MUX_v_32_2_2(32'b00000000000000000000000000000000, (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm[31:0]),
      not_226_nl);
  assign or_486_nl = or_dcpl_61 | or_cse;
  assign nl_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl
      = ({1'b1 , (z_out_3[4:2])}) + 4'b0001;
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl
      = nl_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl[3:0];
  assign or_506_nl = or_dcpl_33 | (fsm_output[15]) | or_dcpl_73;
  assign T_LINE_if_aelse_mux1h_17_nl = MUX1HOT_s_1_3_2((HACC_mul_3_itm[0]), (z_out_6_2[4]),
      (z_out_3[12]), {(fsm_output[13]) , (fsm_output[15]) , (fsm_output[29])});
  assign HACC_idx_HACC_idx_acc_conv_2f_or_nl = (z_out_5[23:14]!=10'b0000000000);
  assign HACC_idx_HACC_idx_acc_conv_2f_and_nl = HACC_idx_HACC_idx_acc_conv_2f_and_itm
      & (z_out_2[13]);
  assign or_497_nl = or_dcpl_35 | (fsm_output[10]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_y_2mi_mux1h_nl
      = MUX1HOT_v_33_4_2(z_out_8, (HACC_mul_3_itm[32:0]), z_out_7, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4,
      {(fsm_output[8]) , or_497_nl , or_tmp_419 , or_tmp_420});
  assign nl_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_acc_nl
      = ({1'b1 , (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva_1[4:2])})
      + 4'b0001;
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_acc_nl
      = nl_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_acc_nl[3:0];
  assign HACC_mux_nl = MUX_v_2_2_2((z_out_2[12:11]), (reg_HACC_idx_acc_3_psp_ftd_2[1:0]),
      or_164_ssc);
  assign HACC_idx_or_1_nl = (fsm_output[18]) | HACC_idx_and_ssc;
  assign HACC_idx_or_2_nl = HACC_idx_acc_3_psp_sva_mx0c0 | (fsm_output[18]) | ((~
      or_164_ssc) & or_tmp_77) | HACC_idx_and_ssc;
  assign HACC_idx_or_3_nl = or_164_ssc | or_dcpl_54;
  assign HACC_mux_7_nl = MUX_v_7_2_2(7'b1010011, (signext_7_2(HACC_idx_acc_8_psp[9:8])),
      fsm_output[7]);
  assign HACC_mux_8_nl = MUX_v_6_2_2((HACC_t_7_0_sva_1[7:2]), 6'b000001, fsm_output[7]);
  assign nl_HACC_acc_nl = HACC_mux_7_nl + conv_u2u_6_7(HACC_mux_8_nl);
  assign HACC_acc_nl = nl_HACC_acc_nl[6:0];
  assign z_out_6_2 = readslicef_7_5_2(HACC_acc_nl);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_3_nl
      = MUX_v_8_2_2(({{3{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva[4]}},
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_i_4_0_sva}),
      HACC_t_7_0_sva, fsm_output[14]);
  assign nl_z_out_1 = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_mux_3_nl
      + 8'b00000001;
  assign z_out_1 = nl_z_out_1[7:0];
  assign HCOL_mux1h_4_nl = MUX1HOT_v_4_4_2(({reg_HACC_idx_acc_3_psp_ftd_2 , (reg_HACC_idx_acc_3_psp_ftd_3[10])}),
      (signext_4_3({(~ (reg_HACC_idx_acc_3_psp_ftd_1[0])) , (~ (reg_HACC_idx_acc_3_psp_ftd_2[2:1]))})),
      ({2'b00 , (reg_HACC_slc_HACC_acc_7_39_14_psp_ftd[15:14])}), (acc_tmp_rsci_data_out_d_oreg[15:12]),
      {(fsm_output[19]) , (fsm_output[21]) , (fsm_output[18]) , (fsm_output[24])});
  assign HCOL_nor_3_nl = ~(HACC_t_or_cse | (fsm_output[14]) | (fsm_output[5]) | (fsm_output[4])
      | (fsm_output[20]) | (fsm_output[26]));
  assign HCOL_and_2_nl = MUX_v_4_2_2(4'b0000, HCOL_mux1h_4_nl, HCOL_nor_3_nl);
  assign HCOL_mux1h_5_nl = MUX1HOT_s_1_6_2((HACC_t_7_0_sva[7]), (reg_HACC_idx_acc_3_psp_ftd_3[9]),
      (reg_HACC_idx_acc_3_psp_ftd_1[1]), (~ (reg_HACC_idx_acc_3_psp_ftd_2[0])), (reg_HACC_slc_HACC_acc_7_39_14_psp_ftd[13]),
      (acc_tmp_rsci_data_out_d_oreg[11]), {(fsm_output[5]) , (fsm_output[19]) , (fsm_output[20])
      , (fsm_output[21]) , (fsm_output[18]) , (fsm_output[24])});
  assign HCOL_or_4_nl = (HCOL_mux1h_5_nl & (~(HACC_t_or_cse | (fsm_output[14]) |
      (fsm_output[26])))) | (fsm_output[4]);
  assign HCOL_mux1h_6_nl = MUX1HOT_v_11_10_2((acc_tmp_vinit_ndx_sva[10:0]), ({1'b0
      , HROW_y_sva}), ({(HACC_t_7_0_sva[6:0]) , 4'b0001}), ({(reg_HACC_idx_acc_3_psp_ftd_3[8:0])
      , (reg_HACC_slc_HACC_acc_7_39_14_psp_ftd[1:0])}), ({3'b000 , (~ HACC_t_7_0_sva)}),
      ({(reg_HACC_idx_acc_3_psp_ftd_1[0]) , reg_HACC_idx_acc_3_psp_ftd_2 , (reg_HACC_idx_acc_3_psp_ftd_3[10:4])}),
      (~ reg_HACC_idx_acc_3_psp_ftd_3), (reg_HACC_slc_HACC_acc_7_39_14_psp_ftd[12:2]),
      operator_11_false_io_read_widthIn_rsc_cse_sva, (acc_tmp_rsci_data_out_d_oreg[10:0]),
      {HACC_t_or_cse , (fsm_output[14]) , (fsm_output[5]) , (fsm_output[19]) , (fsm_output[4])
      , (fsm_output[20]) , (fsm_output[21]) , (fsm_output[18]) , (fsm_output[26])
      , (fsm_output[24])});
  assign HCOL_or_5_nl = (~(HACC_t_or_cse | (fsm_output[5]) | (fsm_output[19]) | (fsm_output[4])
      | (fsm_output[20]) | (fsm_output[21]) | (fsm_output[18]) | (fsm_output[26])
      | (fsm_output[24]))) | (fsm_output[14]);
  assign HCOL_or_6_nl = HACC_t_or_cse | (fsm_output[20]) | (fsm_output[24]);
  assign HCOL_mux1h_7_nl = MUX1HOT_v_15_7_2(15'b111111111111110, ({6'b000000 , (operator_10_false_io_read_heightIn_rsc_cse_sva[9:1])}),
      ({6'b000000 , (~ (HACC_acc_10_psp[12:4]))}), ({14'b11111111111111 , (~ HACC_idx_HACC_idx_acc_conv_2f_and_itm)}),
      ({3'b111 , HACC_t_7_0_sva , 4'b1110}), ({(reg_HACC_idx_acc_3_psp_ftd_2[1:0])
      , reg_HACC_idx_acc_3_psp_ftd_3 , 2'b10}), 15'b111111100011010, {HCOL_or_6_nl
      , (fsm_output[14]) , (fsm_output[5]) , (fsm_output[19]) , (fsm_output[4]) ,
      (fsm_output[21]) , (fsm_output[18])});
  assign HCOL_not_6_nl = ~ (fsm_output[26]);
  assign HCOL_HCOL_nand_1_nl = ~(MUX_v_15_2_2(15'b000000000000000, HCOL_mux1h_7_nl,
      HCOL_not_6_nl));
  assign nl_acc_2_nl = ({HCOL_and_2_nl , HCOL_or_4_nl , HCOL_mux1h_6_nl , HCOL_or_5_nl})
      + conv_s2u_16_17({HCOL_HCOL_nand_1_nl , 1'b1});
  assign acc_2_nl = nl_acc_2_nl[16:0];
  assign z_out_2 = readslicef_17_16_1(acc_2_nl);
  assign for_1_mux1h_5_nl = MUX1HOT_v_18_4_2(({6'b000000 , (acc_tmp_vinit_ndx_sva[18:7])}),
      ({(~ (HACC_idx_acc_8_psp[9:0])) , (~ (reg_HACC_idx_acc_3_psp_ftd_3[3:0])) ,
      (~ (HACC_acc_10_psp[3:0]))}), ({{13{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva[4]}},
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva}),
      ({10'b0000000001 , (~ data_in_rsci_idat_mxwt)}), {(fsm_output[29]) , (fsm_output[7])
      , (fsm_output[8]) , (fsm_output[3])});
  assign for_1_for_1_or_2_nl = (~((fsm_output[8]) | (fsm_output[3]))) | (fsm_output[29]);
  assign for_1_for_1_and_2_nl = (~ (fsm_output[3])) & for_1_nor_2_seb;
  assign for_1_for_1_and_3_nl = MUX_v_2_2_2(2'b00, ({{1{for_1_nor_2_seb}}, for_1_nor_2_seb}),
      (fsm_output[3]));
  assign for_1_for_1_or_3_nl = (fsm_output[29]) | (fsm_output[3]);
  assign nl_z_out_3 = conv_s2u_18_19(for_1_mux1h_5_nl) + conv_u2u_17_19({for_1_for_1_or_2_nl
      , (signext_4_1(fsm_output[29])) , 2'b00 , (signext_7_6({for_1_for_1_and_2_nl
      , ({{1{for_1_nor_2_seb}}, for_1_nor_2_seb}) , for_1_for_1_and_3_nl , for_1_nor_2_seb}))
      , 1'b0 , for_1_for_1_or_3_nl , 1'b1});
  assign z_out_3 = nl_z_out_3[18:0];
  assign HROW_HROW_and_13_nl = MUX_v_3_2_2(3'b000, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_32_30,
      (fsm_output[9]));
  assign HROW_HROW_and_14_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_29
      & (fsm_output[9]);
  assign HROW_HROW_and_15_nl = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_28_27,
      (fsm_output[9]));
  assign HROW_HROW_and_16_nl = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_26_25,
      (fsm_output[9]));
  assign HROW_HROW_and_17_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_24
      & (fsm_output[9]);
  assign HROW_HROW_and_18_nl = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_23_22,
      (fsm_output[9]));
  assign HROW_HROW_and_19_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_21
      & (fsm_output[9]);
  assign HROW_HROW_and_20_nl = MUX_v_3_2_2(3'b000, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_20_18,
      (fsm_output[9]));
  assign HROW_HROW_and_21_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_17
      & (fsm_output[9]);
  assign HROW_HROW_and_22_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_16
      & (fsm_output[9]);
  assign HROW_HROW_and_23_nl = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_15_14,
      (fsm_output[9]));
  assign HROW_HROW_and_24_nl = MUX_v_3_2_2(3'b000, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_13_11,
      (fsm_output[9]));
  assign HROW_HROW_and_25_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_10
      & (fsm_output[9]);
  assign HROW_mux_9_nl = MUX_v_2_2_2((HROW_y_sva[9:8]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_9_8,
      fsm_output[9]);
  assign HROW_mux_10_nl = MUX_s_1_2_2((HROW_y_sva[7]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_7,
      fsm_output[9]);
  assign HROW_mux_11_nl = MUX_v_2_2_2((HROW_y_sva[6:5]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_6_5,
      fsm_output[9]);
  assign HROW_mux_12_nl = MUX_s_1_2_2((HROW_y_sva[4]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_4,
      fsm_output[9]);
  assign HROW_mux_13_nl = MUX_s_1_2_2((HROW_y_sva[3]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_3,
      fsm_output[9]);
  assign HROW_mux_14_nl = MUX_s_1_2_2((HROW_y_sva[2]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_2,
      fsm_output[9]);
  assign HROW_mux_15_nl = MUX_s_1_2_2((HROW_y_sva[1]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_1,
      fsm_output[9]);
  assign HROW_mux_16_nl = MUX_s_1_2_2((HROW_y_sva[0]), HACC_slc_HACC_acc_6_itm, fsm_output[9]);
  assign HROW_or_1_nl = (~ (fsm_output[27])) | (fsm_output[9]);
  assign HROW_mux_17_nl = MUX_v_33_2_2(33'b000000000000000000000000000000001, (~
      (HACC_mul_3_itm[32:0])), fsm_output[9]);
  assign nl_acc_4_nl = ({HROW_HROW_and_13_nl , HROW_HROW_and_14_nl , HROW_HROW_and_15_nl
      , HROW_HROW_and_16_nl , HROW_HROW_and_17_nl , HROW_HROW_and_18_nl , HROW_HROW_and_19_nl
      , HROW_HROW_and_20_nl , HROW_HROW_and_21_nl , HROW_HROW_and_22_nl , HROW_HROW_and_23_nl
      , HROW_HROW_and_24_nl , HROW_HROW_and_25_nl , HROW_mux_9_nl , HROW_mux_10_nl
      , HROW_mux_11_nl , HROW_mux_12_nl , HROW_mux_13_nl , HROW_mux_14_nl , HROW_mux_15_nl
      , HROW_mux_16_nl , HROW_or_1_nl}) + ({HROW_mux_17_nl , 1'b1});
  assign acc_4_nl = nl_acc_4_nl[33:0];
  assign z_out_4 = readslicef_34_33_1(acc_4_nl);
  assign for_1_or_8_nl = (fsm_output[28]) | (fsm_output[2]);
  assign for_1_or_9_nl = or_tmp_463 | or_tmp_464;
  assign for_1_mux1h_6_nl = MUX1HOT_v_39_7_2(({20'b00000000000000000000 , acc_tmp_vinit_ndx_sva}),
      (signext_39_35({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_4_dfm_1_34_32
      , (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm[31:0])})),
      (signext_39_35({(~ ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_4_dfm_1_34_32)
      , (~ (HACC_mul_2_itm[31:0]))})), (signext_39_33({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_32_30
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_29
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_28_27
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_26_25
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_24
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_23_22
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_21
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_20_18
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_17
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_16
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_15_14
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_13_11
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_10
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_9_8
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_7
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_6_5
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_4
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_3
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_2
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_1
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_0})),
      (signext_39_35({(~ ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_4_dfm_1_34_32)
      , (~ (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_itm[31:0]))})),
      (signext_39_33({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_32_30
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_29
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_28_27
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_26
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_25
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_24
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_23
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_22
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_21
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_20
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_19
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_18
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_17
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_16
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_15_14
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_13
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_12
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_11
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_10
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_9
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_8
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_7
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_6
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_5
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_4
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_3
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_0})),
      HACC_mul_2_itm, {for_1_or_8_nl , for_1_or_9_nl , (fsm_output[8]) , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_or_itm
      , (fsm_output[9]) , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_or_30_cse
      , (fsm_output[17])});
  assign for_1_or_10_nl = (~((fsm_output[28]) | or_tmp_463 | and_954_cse | (fsm_output[2])
      | ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_32_cse
      | (fsm_output[17]))) | or_tmp_464 | (fsm_output[8]) | and_956_cse | (fsm_output[9])
      | ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_33_cse;
  assign for_1_mux1h_7_nl = MUX1HOT_v_22_8_2(({6'b000000 , (ac_math_atan_pi_2mi_return_69_38_sva[31:16])}),
      ({6'b111111 , (~ (ac_math_atan_pi_2mi_return_69_38_sva[31:16]))}), (signext_22_18({(HACC_idx_acc_8_psp[9:0])
      , (reg_HACC_idx_acc_3_psp_ftd_3[3:0]) , (HACC_acc_10_psp[3:0])})), (signext_22_17(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[32:16])),
      (signext_22_17(~ (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[32:16]))),
      (signext_22_17(~ (HACC_mul_2_itm[32:16]))), (signext_22_17(HACC_mul_2_itm[32:16])),
      (HACC_mul_3_itm[37:16]), {or_tmp_463 , or_tmp_464 , or_379_cse , and_954_cse
      , and_956_cse , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_33_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_32_cse
      , (fsm_output[17])});
  assign for_1_not_6_nl = ~ (fsm_output[28]);
  assign for_1_and_4_nl = MUX_v_22_2_2(22'b0000000000000000000000, for_1_mux1h_7_nl,
      for_1_not_6_nl);
  assign for_1_or_11_nl = MUX_v_22_2_2(for_1_and_4_nl, 22'b1111111111111111111111,
      (fsm_output[2]));
  assign for_1_mux1h_8_nl = MUX1HOT_v_16_8_2(16'b0000000000000001, (ac_math_atan_pi_2mi_return_69_38_sva[15:0]),
      (~ (ac_math_atan_pi_2mi_return_69_38_sva[15:0])), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[15:0]),
      (~ (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[15:0])),
      (~ (HACC_mul_2_itm[15:0])), (HACC_mul_2_itm[15:0]), (HACC_mul_3_itm[15:0]),
      {(fsm_output[28]) , or_tmp_463 , or_tmp_464 , and_954_cse , and_956_cse , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_33_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_and_32_cse
      , (fsm_output[17])});
  assign for_1_nor_6_nl = ~((fsm_output[9:8]!=2'b00));
  assign for_1_and_5_nl = MUX_v_16_2_2(16'b0000000000000000, for_1_mux1h_8_nl, for_1_nor_6_nl);
  assign for_1_or_12_nl = MUX_v_16_2_2(for_1_and_5_nl, 16'b1111111111111111, (fsm_output[2]));
  assign nl_acc_5_nl = conv_s2u_40_41({for_1_mux1h_6_nl , for_1_or_10_nl}) + conv_s2u_39_41({for_1_or_11_nl
      , for_1_or_12_nl , 1'b1});
  assign acc_5_nl = nl_acc_5_nl[40:0];
  assign z_out_5 = readslicef_41_40_1(acc_5_nl);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_15_nl
      = MUX_v_3_2_2((signext_3_1(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_32_30[2])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_acc_a_lpi_4_dfm_1_34_32,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_29_nl
      = ~ (fsm_output[22]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_15_nl
      = MUX_v_3_2_2(3'b000, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_15_nl,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_29_nl);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_16_nl
      = MUX_v_2_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_32_30[1:0]),
      (HACC_mul_2_itm[31:30]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_30_nl
      = ~ (fsm_output[22]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_16_nl
      = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_16_nl,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_30_nl);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_17_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_29,
      (HACC_mul_2_itm[29]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_17_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_17_nl
      & (~ (fsm_output[22]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_18_nl
      = MUX_v_2_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_28_27,
      (HACC_mul_2_itm[28:27]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_32_nl
      = ~ (fsm_output[22]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_18_nl
      = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_18_nl,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_32_nl);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_19_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_26,
      (HACC_mul_2_itm[26]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_19_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_19_nl
      & (~ (fsm_output[22]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_20_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_25,
      (HACC_mul_2_itm[25]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_20_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_20_nl
      & (~ (fsm_output[22]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_21_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_24,
      (HACC_mul_2_itm[24]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_21_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_21_nl
      & (~ (fsm_output[22]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_22_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_23,
      (HACC_mul_2_itm[23]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_22_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_22_nl
      & (~ (fsm_output[22]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_23_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_22,
      (HACC_mul_2_itm[22]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_23_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_23_nl
      & (~ (fsm_output[22]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_24_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_21,
      (HACC_mul_2_itm[21]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_24_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_24_nl
      & (~ (fsm_output[22]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_25_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_20,
      (HACC_mul_2_itm[20]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_25_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_25_nl
      & (~ (fsm_output[22]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_26_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_19,
      (HACC_mul_2_itm[19]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_26_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_26_nl
      & (~ (fsm_output[22]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_27_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_18,
      (HACC_mul_2_itm[18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_27_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_27_nl
      & (~ (fsm_output[22]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_28_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_17,
      (HACC_mul_2_itm[17]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_28_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_28_nl
      & (~ (fsm_output[22]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux1h_18_nl
      = MUX1HOT_v_16_3_2(({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_16
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_15_14
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_13
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_12
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_11
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_10
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_9
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_8
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_7
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_6
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_5
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_4
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_3
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_2_1}),
      (HACC_mul_2_itm[16:1]), ({HACC_idx_acc_8_psp , 1'b0}), {ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_or_itm
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm
      , (fsm_output[22])});
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_29_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_y_lpi_4_dfm_1_0,
      (HACC_mul_2_itm[0]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_2_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_29_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_29_nl
      & (~ (fsm_output[22]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_33_nl
      = (~(or_tmp_420 | or_tmp_475 | (fsm_output[22]))) | or_tmp_419 | or_tmp_476;
  assign nl_HACC_idx_acc_9_nl = ({reg_HACC_idx_acc_3_psp_ftd_3 , (HACC_t_7_0_sva[7:2])})
      + conv_s2s_16_17({(HACC_acc_10_psp[11:0]) , (reg_HACC_idx_acc_3_psp_ftd_3[3:0])});
  assign HACC_idx_acc_9_nl = nl_HACC_idx_acc_9_nl[16:0];
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux1h_19_nl
      = MUX1HOT_v_33_5_2((~ z_out_8), z_out_8, ({1'b0 , data_out_out}), ({1'b1 ,
      (~ data_out_out)}), (signext_33_17(HACC_idx_acc_9_nl)), {or_tmp_419 , or_tmp_420
      , or_tmp_475 , or_tmp_476 , (fsm_output[22])});
  assign nl_acc_6_nl = ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_15_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_16_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_17_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_18_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_19_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_20_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_21_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_22_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_23_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_24_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_25_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_26_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_27_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_28_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux1h_18_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_29_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_33_nl})
      + conv_s2u_34_36({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux1h_19_nl
      , 1'b1});
  assign acc_6_nl = nl_acc_6_nl[35:0];
  assign z_out_6 = readslicef_36_35_1(acc_6_nl);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_45_nl
      = ~ (fsm_output[27]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_13_nl
      = MUX_v_3_2_2(3'b000, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_32_30,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_45_nl);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_14_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_29
      & (~ (fsm_output[27]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_47_nl
      = ~ (fsm_output[27]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_15_nl
      = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_28_27,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_47_nl);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_48_nl
      = ~ (fsm_output[27]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_16_nl
      = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_26_25,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_48_nl);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_17_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_24
      & (~ (fsm_output[27]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_50_nl
      = ~ (fsm_output[27]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_18_nl
      = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_23_22,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_50_nl);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_19_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_21
      & (~ (fsm_output[27]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_52_nl
      = ~ (fsm_output[27]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_20_nl
      = MUX_v_3_2_2(3'b000, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_20_18,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_52_nl);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_21_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_17
      & (~ (fsm_output[27]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_22_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_16
      & (~ (fsm_output[27]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_55_nl
      = ~ (fsm_output[27]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_23_nl
      = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_15_14,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_55_nl);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_56_nl
      = ~ (fsm_output[27]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_24_nl
      = MUX_v_3_2_2(3'b000, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_13_11,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_not_56_nl);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_25_nl
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_10
      & (~ (fsm_output[27]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_9_nl
      = MUX_v_2_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_9_8,
      (operator_10_false_io_read_heightIn_rsc_cse_sva[9:8]), fsm_output[27]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_10_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_7,
      (operator_10_false_io_read_heightIn_rsc_cse_sva[7]), fsm_output[27]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_11_nl
      = MUX_v_2_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_6_5,
      (operator_10_false_io_read_heightIn_rsc_cse_sva[6:5]), fsm_output[27]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_12_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_4,
      (operator_10_false_io_read_heightIn_rsc_cse_sva[4]), fsm_output[27]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_13_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_3,
      (operator_10_false_io_read_heightIn_rsc_cse_sva[3]), fsm_output[27]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_14_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_2,
      (operator_10_false_io_read_heightIn_rsc_cse_sva[2]), fsm_output[27]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_15_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_lpi_4_dfm_1_1,
      (operator_10_false_io_read_heightIn_rsc_cse_sva[1]), fsm_output[27]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_16_nl
      = MUX_s_1_2_2(HACC_slc_HACC_acc_6_itm, (operator_10_false_io_read_heightIn_rsc_cse_sva[0]),
      fsm_output[27]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_1_nl
      = MUX_v_33_2_2((HACC_mul_3_itm[32:0]), 33'b111111111111111111111111111111111,
      (fsm_output[27]));
  assign nl_z_out_7 = ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_13_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_14_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_15_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_16_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_17_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_18_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_19_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_20_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_21_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_22_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_23_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_24_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_and_25_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_9_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_10_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_11_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_12_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_13_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_14_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_15_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_mux_16_nl})
      + ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_for_if_or_1_nl;
  assign z_out_7 = nl_z_out_7[32:0];
  assign HACC_mux_9_nl = MUX_v_12_2_2((HACC_acc_10_psp[11:0]), ({{1{reg_HACC_idx_acc_3_psp_ftd_3[10]}},
      reg_HACC_idx_acc_3_psp_ftd_3}), fsm_output[15]);
  assign HACC_mux_10_nl = MUX_v_27_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva[32:6]),
      (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_1_x_sva_4[32:6]),
      fsm_output[15]);
  assign z_out_9 = conv_u2u_39_39($signed(HACC_mux_9_nl) * $signed(HACC_mux_10_nl));

  function automatic [0:0] MUX1HOT_s_1_3_2;
    input [0:0] input_2;
    input [0:0] input_1;
    input [0:0] input_0;
    input [2:0] sel;
    reg [0:0] result;
  begin
    result = input_0 & {1{sel[0]}};
    result = result | ( input_1 & {1{sel[1]}});
    result = result | ( input_2 & {1{sel[2]}});
    MUX1HOT_s_1_3_2 = result;
  end
  endfunction


  function automatic [0:0] MUX1HOT_s_1_4_2;
    input [0:0] input_3;
    input [0:0] input_2;
    input [0:0] input_1;
    input [0:0] input_0;
    input [3:0] sel;
    reg [0:0] result;
  begin
    result = input_0 & {1{sel[0]}};
    result = result | ( input_1 & {1{sel[1]}});
    result = result | ( input_2 & {1{sel[2]}});
    result = result | ( input_3 & {1{sel[3]}});
    MUX1HOT_s_1_4_2 = result;
  end
  endfunction


  function automatic [0:0] MUX1HOT_s_1_6_2;
    input [0:0] input_5;
    input [0:0] input_4;
    input [0:0] input_3;
    input [0:0] input_2;
    input [0:0] input_1;
    input [0:0] input_0;
    input [5:0] sel;
    reg [0:0] result;
  begin
    result = input_0 & {1{sel[0]}};
    result = result | ( input_1 & {1{sel[1]}});
    result = result | ( input_2 & {1{sel[2]}});
    result = result | ( input_3 & {1{sel[3]}});
    result = result | ( input_4 & {1{sel[4]}});
    result = result | ( input_5 & {1{sel[5]}});
    MUX1HOT_s_1_6_2 = result;
  end
  endfunction


  function automatic [10:0] MUX1HOT_v_11_10_2;
    input [10:0] input_9;
    input [10:0] input_8;
    input [10:0] input_7;
    input [10:0] input_6;
    input [10:0] input_5;
    input [10:0] input_4;
    input [10:0] input_3;
    input [10:0] input_2;
    input [10:0] input_1;
    input [10:0] input_0;
    input [9:0] sel;
    reg [10:0] result;
  begin
    result = input_0 & {11{sel[0]}};
    result = result | ( input_1 & {11{sel[1]}});
    result = result | ( input_2 & {11{sel[2]}});
    result = result | ( input_3 & {11{sel[3]}});
    result = result | ( input_4 & {11{sel[4]}});
    result = result | ( input_5 & {11{sel[5]}});
    result = result | ( input_6 & {11{sel[6]}});
    result = result | ( input_7 & {11{sel[7]}});
    result = result | ( input_8 & {11{sel[8]}});
    result = result | ( input_9 & {11{sel[9]}});
    MUX1HOT_v_11_10_2 = result;
  end
  endfunction


  function automatic [10:0] MUX1HOT_v_11_3_2;
    input [10:0] input_2;
    input [10:0] input_1;
    input [10:0] input_0;
    input [2:0] sel;
    reg [10:0] result;
  begin
    result = input_0 & {11{sel[0]}};
    result = result | ( input_1 & {11{sel[1]}});
    result = result | ( input_2 & {11{sel[2]}});
    MUX1HOT_v_11_3_2 = result;
  end
  endfunction


  function automatic [11:0] MUX1HOT_v_12_3_2;
    input [11:0] input_2;
    input [11:0] input_1;
    input [11:0] input_0;
    input [2:0] sel;
    reg [11:0] result;
  begin
    result = input_0 & {12{sel[0]}};
    result = result | ( input_1 & {12{sel[1]}});
    result = result | ( input_2 & {12{sel[2]}});
    MUX1HOT_v_12_3_2 = result;
  end
  endfunction


  function automatic [12:0] MUX1HOT_v_13_3_2;
    input [12:0] input_2;
    input [12:0] input_1;
    input [12:0] input_0;
    input [2:0] sel;
    reg [12:0] result;
  begin
    result = input_0 & {13{sel[0]}};
    result = result | ( input_1 & {13{sel[1]}});
    result = result | ( input_2 & {13{sel[2]}});
    MUX1HOT_v_13_3_2 = result;
  end
  endfunction


  function automatic [14:0] MUX1HOT_v_15_7_2;
    input [14:0] input_6;
    input [14:0] input_5;
    input [14:0] input_4;
    input [14:0] input_3;
    input [14:0] input_2;
    input [14:0] input_1;
    input [14:0] input_0;
    input [6:0] sel;
    reg [14:0] result;
  begin
    result = input_0 & {15{sel[0]}};
    result = result | ( input_1 & {15{sel[1]}});
    result = result | ( input_2 & {15{sel[2]}});
    result = result | ( input_3 & {15{sel[3]}});
    result = result | ( input_4 & {15{sel[4]}});
    result = result | ( input_5 & {15{sel[5]}});
    result = result | ( input_6 & {15{sel[6]}});
    MUX1HOT_v_15_7_2 = result;
  end
  endfunction


  function automatic [15:0] MUX1HOT_v_16_3_2;
    input [15:0] input_2;
    input [15:0] input_1;
    input [15:0] input_0;
    input [2:0] sel;
    reg [15:0] result;
  begin
    result = input_0 & {16{sel[0]}};
    result = result | ( input_1 & {16{sel[1]}});
    result = result | ( input_2 & {16{sel[2]}});
    MUX1HOT_v_16_3_2 = result;
  end
  endfunction


  function automatic [15:0] MUX1HOT_v_16_8_2;
    input [15:0] input_7;
    input [15:0] input_6;
    input [15:0] input_5;
    input [15:0] input_4;
    input [15:0] input_3;
    input [15:0] input_2;
    input [15:0] input_1;
    input [15:0] input_0;
    input [7:0] sel;
    reg [15:0] result;
  begin
    result = input_0 & {16{sel[0]}};
    result = result | ( input_1 & {16{sel[1]}});
    result = result | ( input_2 & {16{sel[2]}});
    result = result | ( input_3 & {16{sel[3]}});
    result = result | ( input_4 & {16{sel[4]}});
    result = result | ( input_5 & {16{sel[5]}});
    result = result | ( input_6 & {16{sel[6]}});
    result = result | ( input_7 & {16{sel[7]}});
    MUX1HOT_v_16_8_2 = result;
  end
  endfunction


  function automatic [16:0] MUX1HOT_v_17_4_2;
    input [16:0] input_3;
    input [16:0] input_2;
    input [16:0] input_1;
    input [16:0] input_0;
    input [3:0] sel;
    reg [16:0] result;
  begin
    result = input_0 & {17{sel[0]}};
    result = result | ( input_1 & {17{sel[1]}});
    result = result | ( input_2 & {17{sel[2]}});
    result = result | ( input_3 & {17{sel[3]}});
    MUX1HOT_v_17_4_2 = result;
  end
  endfunction


  function automatic [17:0] MUX1HOT_v_18_4_2;
    input [17:0] input_3;
    input [17:0] input_2;
    input [17:0] input_1;
    input [17:0] input_0;
    input [3:0] sel;
    reg [17:0] result;
  begin
    result = input_0 & {18{sel[0]}};
    result = result | ( input_1 & {18{sel[1]}});
    result = result | ( input_2 & {18{sel[2]}});
    result = result | ( input_3 & {18{sel[3]}});
    MUX1HOT_v_18_4_2 = result;
  end
  endfunction


  function automatic [18:0] MUX1HOT_v_19_3_2;
    input [18:0] input_2;
    input [18:0] input_1;
    input [18:0] input_0;
    input [2:0] sel;
    reg [18:0] result;
  begin
    result = input_0 & {19{sel[0]}};
    result = result | ( input_1 & {19{sel[1]}});
    result = result | ( input_2 & {19{sel[2]}});
    MUX1HOT_v_19_3_2 = result;
  end
  endfunction


  function automatic [21:0] MUX1HOT_v_22_8_2;
    input [21:0] input_7;
    input [21:0] input_6;
    input [21:0] input_5;
    input [21:0] input_4;
    input [21:0] input_3;
    input [21:0] input_2;
    input [21:0] input_1;
    input [21:0] input_0;
    input [7:0] sel;
    reg [21:0] result;
  begin
    result = input_0 & {22{sel[0]}};
    result = result | ( input_1 & {22{sel[1]}});
    result = result | ( input_2 & {22{sel[2]}});
    result = result | ( input_3 & {22{sel[3]}});
    result = result | ( input_4 & {22{sel[4]}});
    result = result | ( input_5 & {22{sel[5]}});
    result = result | ( input_6 & {22{sel[6]}});
    result = result | ( input_7 & {22{sel[7]}});
    MUX1HOT_v_22_8_2 = result;
  end
  endfunction


  function automatic [1:0] MUX1HOT_v_2_3_2;
    input [1:0] input_2;
    input [1:0] input_1;
    input [1:0] input_0;
    input [2:0] sel;
    reg [1:0] result;
  begin
    result = input_0 & {2{sel[0]}};
    result = result | ( input_1 & {2{sel[1]}});
    result = result | ( input_2 & {2{sel[2]}});
    MUX1HOT_v_2_3_2 = result;
  end
  endfunction


  function automatic [1:0] MUX1HOT_v_2_4_2;
    input [1:0] input_3;
    input [1:0] input_2;
    input [1:0] input_1;
    input [1:0] input_0;
    input [3:0] sel;
    reg [1:0] result;
  begin
    result = input_0 & {2{sel[0]}};
    result = result | ( input_1 & {2{sel[1]}});
    result = result | ( input_2 & {2{sel[2]}});
    result = result | ( input_3 & {2{sel[3]}});
    MUX1HOT_v_2_4_2 = result;
  end
  endfunction


  function automatic [32:0] MUX1HOT_v_33_3_2;
    input [32:0] input_2;
    input [32:0] input_1;
    input [32:0] input_0;
    input [2:0] sel;
    reg [32:0] result;
  begin
    result = input_0 & {33{sel[0]}};
    result = result | ( input_1 & {33{sel[1]}});
    result = result | ( input_2 & {33{sel[2]}});
    MUX1HOT_v_33_3_2 = result;
  end
  endfunction


  function automatic [32:0] MUX1HOT_v_33_4_2;
    input [32:0] input_3;
    input [32:0] input_2;
    input [32:0] input_1;
    input [32:0] input_0;
    input [3:0] sel;
    reg [32:0] result;
  begin
    result = input_0 & {33{sel[0]}};
    result = result | ( input_1 & {33{sel[1]}});
    result = result | ( input_2 & {33{sel[2]}});
    result = result | ( input_3 & {33{sel[3]}});
    MUX1HOT_v_33_4_2 = result;
  end
  endfunction


  function automatic [32:0] MUX1HOT_v_33_5_2;
    input [32:0] input_4;
    input [32:0] input_3;
    input [32:0] input_2;
    input [32:0] input_1;
    input [32:0] input_0;
    input [4:0] sel;
    reg [32:0] result;
  begin
    result = input_0 & {33{sel[0]}};
    result = result | ( input_1 & {33{sel[1]}});
    result = result | ( input_2 & {33{sel[2]}});
    result = result | ( input_3 & {33{sel[3]}});
    result = result | ( input_4 & {33{sel[4]}});
    MUX1HOT_v_33_5_2 = result;
  end
  endfunction


  function automatic [34:0] MUX1HOT_v_35_3_2;
    input [34:0] input_2;
    input [34:0] input_1;
    input [34:0] input_0;
    input [2:0] sel;
    reg [34:0] result;
  begin
    result = input_0 & {35{sel[0]}};
    result = result | ( input_1 & {35{sel[1]}});
    result = result | ( input_2 & {35{sel[2]}});
    MUX1HOT_v_35_3_2 = result;
  end
  endfunction


  function automatic [38:0] MUX1HOT_v_39_3_2;
    input [38:0] input_2;
    input [38:0] input_1;
    input [38:0] input_0;
    input [2:0] sel;
    reg [38:0] result;
  begin
    result = input_0 & {39{sel[0]}};
    result = result | ( input_1 & {39{sel[1]}});
    result = result | ( input_2 & {39{sel[2]}});
    MUX1HOT_v_39_3_2 = result;
  end
  endfunction


  function automatic [38:0] MUX1HOT_v_39_7_2;
    input [38:0] input_6;
    input [38:0] input_5;
    input [38:0] input_4;
    input [38:0] input_3;
    input [38:0] input_2;
    input [38:0] input_1;
    input [38:0] input_0;
    input [6:0] sel;
    reg [38:0] result;
  begin
    result = input_0 & {39{sel[0]}};
    result = result | ( input_1 & {39{sel[1]}});
    result = result | ( input_2 & {39{sel[2]}});
    result = result | ( input_3 & {39{sel[3]}});
    result = result | ( input_4 & {39{sel[4]}});
    result = result | ( input_5 & {39{sel[5]}});
    result = result | ( input_6 & {39{sel[6]}});
    MUX1HOT_v_39_7_2 = result;
  end
  endfunction


  function automatic [2:0] MUX1HOT_v_3_3_2;
    input [2:0] input_2;
    input [2:0] input_1;
    input [2:0] input_0;
    input [2:0] sel;
    reg [2:0] result;
  begin
    result = input_0 & {3{sel[0]}};
    result = result | ( input_1 & {3{sel[1]}});
    result = result | ( input_2 & {3{sel[2]}});
    MUX1HOT_v_3_3_2 = result;
  end
  endfunction


  function automatic [2:0] MUX1HOT_v_3_4_2;
    input [2:0] input_3;
    input [2:0] input_2;
    input [2:0] input_1;
    input [2:0] input_0;
    input [3:0] sel;
    reg [2:0] result;
  begin
    result = input_0 & {3{sel[0]}};
    result = result | ( input_1 & {3{sel[1]}});
    result = result | ( input_2 & {3{sel[2]}});
    result = result | ( input_3 & {3{sel[3]}});
    MUX1HOT_v_3_4_2 = result;
  end
  endfunction


  function automatic [3:0] MUX1HOT_v_4_4_2;
    input [3:0] input_3;
    input [3:0] input_2;
    input [3:0] input_1;
    input [3:0] input_0;
    input [3:0] sel;
    reg [3:0] result;
  begin
    result = input_0 & {4{sel[0]}};
    result = result | ( input_1 & {4{sel[1]}});
    result = result | ( input_2 & {4{sel[2]}});
    result = result | ( input_3 & {4{sel[3]}});
    MUX1HOT_v_4_4_2 = result;
  end
  endfunction


  function automatic [0:0] MUX_s_1_2_2;
    input [0:0] input_0;
    input [0:0] input_1;
    input [0:0] sel;
    reg [0:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_s_1_2_2 = result;
  end
  endfunction


  function automatic [9:0] MUX_v_10_2_2;
    input [9:0] input_0;
    input [9:0] input_1;
    input [0:0] sel;
    reg [9:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_10_2_2 = result;
  end
  endfunction


  function automatic [10:0] MUX_v_11_2_2;
    input [10:0] input_0;
    input [10:0] input_1;
    input [0:0] sel;
    reg [10:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_11_2_2 = result;
  end
  endfunction


  function automatic [11:0] MUX_v_12_2_2;
    input [11:0] input_0;
    input [11:0] input_1;
    input [0:0] sel;
    reg [11:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_12_2_2 = result;
  end
  endfunction


  function automatic [14:0] MUX_v_15_2_2;
    input [14:0] input_0;
    input [14:0] input_1;
    input [0:0] sel;
    reg [14:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_15_2_2 = result;
  end
  endfunction


  function automatic [15:0] MUX_v_16_2_2;
    input [15:0] input_0;
    input [15:0] input_1;
    input [0:0] sel;
    reg [15:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_16_2_2 = result;
  end
  endfunction


  function automatic [18:0] MUX_v_19_2_2;
    input [18:0] input_0;
    input [18:0] input_1;
    input [0:0] sel;
    reg [18:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_19_2_2 = result;
  end
  endfunction


  function automatic [21:0] MUX_v_22_2_2;
    input [21:0] input_0;
    input [21:0] input_1;
    input [0:0] sel;
    reg [21:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_22_2_2 = result;
  end
  endfunction


  function automatic [26:0] MUX_v_27_2_2;
    input [26:0] input_0;
    input [26:0] input_1;
    input [0:0] sel;
    reg [26:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_27_2_2 = result;
  end
  endfunction


  function automatic [1:0] MUX_v_2_2_2;
    input [1:0] input_0;
    input [1:0] input_1;
    input [0:0] sel;
    reg [1:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_2_2_2 = result;
  end
  endfunction


  function automatic [31:0] MUX_v_32_2_2;
    input [31:0] input_0;
    input [31:0] input_1;
    input [0:0] sel;
    reg [31:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_32_2_2 = result;
  end
  endfunction


  function automatic [32:0] MUX_v_33_2_2;
    input [32:0] input_0;
    input [32:0] input_1;
    input [0:0] sel;
    reg [32:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_33_2_2 = result;
  end
  endfunction


  function automatic [37:0] MUX_v_38_2_2;
    input [37:0] input_0;
    input [37:0] input_1;
    input [0:0] sel;
    reg [37:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_38_2_2 = result;
  end
  endfunction


  function automatic [2:0] MUX_v_3_2_2;
    input [2:0] input_0;
    input [2:0] input_1;
    input [0:0] sel;
    reg [2:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_3_2_2 = result;
  end
  endfunction


  function automatic [3:0] MUX_v_4_2_2;
    input [3:0] input_0;
    input [3:0] input_1;
    input [0:0] sel;
    reg [3:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_4_2_2 = result;
  end
  endfunction


  function automatic [4:0] MUX_v_5_2_2;
    input [4:0] input_0;
    input [4:0] input_1;
    input [0:0] sel;
    reg [4:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_5_2_2 = result;
  end
  endfunction


  function automatic [5:0] MUX_v_6_2_2;
    input [5:0] input_0;
    input [5:0] input_1;
    input [0:0] sel;
    reg [5:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_6_2_2 = result;
  end
  endfunction


  function automatic [6:0] MUX_v_7_2_2;
    input [6:0] input_0;
    input [6:0] input_1;
    input [0:0] sel;
    reg [6:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_7_2_2 = result;
  end
  endfunction


  function automatic [7:0] MUX_v_8_2_2;
    input [7:0] input_0;
    input [7:0] input_1;
    input [0:0] sel;
    reg [7:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_8_2_2 = result;
  end
  endfunction


  function automatic [15:0] readslicef_17_16_1;
    input [16:0] vector;
    reg [16:0] tmp;
  begin
    tmp = vector >> 1;
    readslicef_17_16_1 = tmp[15:0];
  end
  endfunction


  function automatic [32:0] readslicef_34_33_1;
    input [33:0] vector;
    reg [33:0] tmp;
  begin
    tmp = vector >> 1;
    readslicef_34_33_1 = tmp[32:0];
  end
  endfunction


  function automatic [34:0] readslicef_36_35_1;
    input [35:0] vector;
    reg [35:0] tmp;
  begin
    tmp = vector >> 1;
    readslicef_36_35_1 = tmp[34:0];
  end
  endfunction


  function automatic [39:0] readslicef_41_40_1;
    input [40:0] vector;
    reg [40:0] tmp;
  begin
    tmp = vector >> 1;
    readslicef_41_40_1 = tmp[39:0];
  end
  endfunction


  function automatic [0:0] readslicef_4_1_3;
    input [3:0] vector;
    reg [3:0] tmp;
  begin
    tmp = vector >> 3;
    readslicef_4_1_3 = tmp[0:0];
  end
  endfunction


  function automatic [4:0] readslicef_7_5_2;
    input [6:0] vector;
    reg [6:0] tmp;
  begin
    tmp = vector >> 2;
    readslicef_7_5_2 = tmp[4:0];
  end
  endfunction


  function automatic [21:0] signext_22_17;
    input [16:0] vector;
  begin
    signext_22_17= {{5{vector[16]}}, vector};
  end
  endfunction


  function automatic [21:0] signext_22_18;
    input [17:0] vector;
  begin
    signext_22_18= {{4{vector[17]}}, vector};
  end
  endfunction


  function automatic [1:0] signext_2_1;
    input [0:0] vector;
  begin
    signext_2_1= {{1{vector[0]}}, vector};
  end
  endfunction


  function automatic [32:0] signext_33_17;
    input [16:0] vector;
  begin
    signext_33_17= {{16{vector[16]}}, vector};
  end
  endfunction


  function automatic [38:0] signext_39_33;
    input [32:0] vector;
  begin
    signext_39_33= {{6{vector[32]}}, vector};
  end
  endfunction


  function automatic [38:0] signext_39_35;
    input [34:0] vector;
  begin
    signext_39_35= {{4{vector[34]}}, vector};
  end
  endfunction


  function automatic [2:0] signext_3_1;
    input [0:0] vector;
  begin
    signext_3_1= {{2{vector[0]}}, vector};
  end
  endfunction


  function automatic [3:0] signext_4_1;
    input [0:0] vector;
  begin
    signext_4_1= {{3{vector[0]}}, vector};
  end
  endfunction


  function automatic [3:0] signext_4_3;
    input [2:0] vector;
  begin
    signext_4_3= {{1{vector[2]}}, vector};
  end
  endfunction


  function automatic [6:0] signext_7_2;
    input [1:0] vector;
  begin
    signext_7_2= {{5{vector[1]}}, vector};
  end
  endfunction


  function automatic [6:0] signext_7_6;
    input [5:0] vector;
  begin
    signext_7_6= {{1{vector[5]}}, vector};
  end
  endfunction


  function automatic [11:0] conv_s2s_11_12 ;
    input [10:0]  vector ;
  begin
    conv_s2s_11_12 = {vector[10], vector};
  end
  endfunction


  function automatic [16:0] conv_s2s_16_17 ;
    input [15:0]  vector ;
  begin
    conv_s2s_16_17 = {vector[15], vector};
  end
  endfunction


  function automatic [9:0] conv_s2u_9_10 ;
    input [8:0]  vector ;
  begin
    conv_s2u_9_10 = {vector[8], vector};
  end
  endfunction


  function automatic [16:0] conv_s2u_16_17 ;
    input [15:0]  vector ;
  begin
    conv_s2u_16_17 = {vector[15], vector};
  end
  endfunction


  function automatic [18:0] conv_s2u_18_19 ;
    input [17:0]  vector ;
  begin
    conv_s2u_18_19 = {vector[17], vector};
  end
  endfunction


  function automatic [35:0] conv_s2u_34_36 ;
    input [33:0]  vector ;
  begin
    conv_s2u_34_36 = {{2{vector[33]}}, vector};
  end
  endfunction


  function automatic [40:0] conv_s2u_39_41 ;
    input [38:0]  vector ;
  begin
    conv_s2u_39_41 = {{2{vector[38]}}, vector};
  end
  endfunction


  function automatic [40:0] conv_s2u_40_41 ;
    input [39:0]  vector ;
  begin
    conv_s2u_40_41 = {vector[39], vector};
  end
  endfunction


  function automatic [11:0] conv_u2s_11_12 ;
    input [10:0]  vector ;
  begin
    conv_u2s_11_12 =  {1'b0, vector};
  end
  endfunction


  function automatic [6:0] conv_u2u_6_7 ;
    input [5:0]  vector ;
  begin
    conv_u2u_6_7 = {1'b0, vector};
  end
  endfunction


  function automatic [18:0] conv_u2u_17_19 ;
    input [16:0]  vector ;
  begin
    conv_u2u_17_19 = {{2{1'b0}}, vector};
  end
  endfunction


  function automatic [38:0] conv_u2u_39_39 ;
    input [38:0]  vector ;
  begin
    conv_u2u_39_39 = vector;
  end
  endfunction

endmodule

// ------------------------------------------------------------------
//  Design Unit:    getMaxLine
// ------------------------------------------------------------------


module getMaxLine (
  clk, rst, x1_rsc_dat, x1_rsc_vld, x1_rsc_rdy, y1_rsc_dat, y1_rsc_vld, y1_rsc_rdy,
      x2_rsc_dat, x2_rsc_vld, x2_rsc_rdy, y2_rsc_dat, y2_rsc_vld, y2_rsc_rdy, acc_rsc_dat,
      acc_rsc_vld, acc_rsc_rdy
);
  input clk;
  input rst;
  output [10:0] x1_rsc_dat;
  output x1_rsc_vld;
  input x1_rsc_rdy;
  output [9:0] y1_rsc_dat;
  output y1_rsc_vld;
  input y1_rsc_rdy;
  output [10:0] x2_rsc_dat;
  output x2_rsc_vld;
  input x2_rsc_rdy;
  output [9:0] y2_rsc_dat;
  output y2_rsc_vld;
  input y2_rsc_rdy;
  input [15:0] acc_rsc_dat;
  input acc_rsc_vld;
  output acc_rsc_rdy;


  // Interconnect Declarations
  wire [19:0] T_LINE_if_if_dividend1_mul_cmp_a;
  wire [26:0] T_LINE_if_if_dividend1_mul_cmp_b;


  // Interconnect Declarations for Component Instantiations 
  wire [43:0] nl_getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_z;
  assign nl_getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_z = conv_u2u_47_44($signed(T_LINE_if_if_dividend1_mul_cmp_a)
      * $signed(T_LINE_if_if_dividend1_mul_cmp_b));
  getMaxLine_core getMaxLine_core_inst (
      .clk(clk),
      .rst(rst),
      .x1_rsc_dat(x1_rsc_dat),
      .x1_rsc_vld(x1_rsc_vld),
      .x1_rsc_rdy(x1_rsc_rdy),
      .y1_rsc_dat(y1_rsc_dat),
      .y1_rsc_vld(y1_rsc_vld),
      .y1_rsc_rdy(y1_rsc_rdy),
      .x2_rsc_dat(x2_rsc_dat),
      .x2_rsc_vld(x2_rsc_vld),
      .x2_rsc_rdy(x2_rsc_rdy),
      .y2_rsc_dat(y2_rsc_dat),
      .y2_rsc_vld(y2_rsc_vld),
      .y2_rsc_rdy(y2_rsc_rdy),
      .acc_rsc_dat(acc_rsc_dat),
      .acc_rsc_vld(acc_rsc_vld),
      .acc_rsc_rdy(acc_rsc_rdy),
      .T_LINE_if_if_dividend1_mul_cmp_a(T_LINE_if_if_dividend1_mul_cmp_a),
      .T_LINE_if_if_dividend1_mul_cmp_b(T_LINE_if_if_dividend1_mul_cmp_b),
      .T_LINE_if_if_dividend1_mul_cmp_z(nl_getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_z[43:0])
    );

  function automatic [43:0] conv_u2u_47_44 ;
    input [46:0]  vector ;
  begin
    conv_u2u_47_44 = vector[43:0];
  end
  endfunction

endmodule

// ------------------------------------------------------------------
//  Design Unit:    houghTransform
// ------------------------------------------------------------------


module houghTransform (
  clk, rst, data_in_rsc_dat, data_in_rsc_vld, data_in_rsc_rdy, widthIn_rsc_dat, widthIn_rsc_triosy_lz,
      heightIn_rsc_dat, heightIn_rsc_triosy_lz, acc_rsc_dat, acc_rsc_vld, acc_rsc_rdy
);
  input clk;
  input rst;
  input [7:0] data_in_rsc_dat;
  input data_in_rsc_vld;
  output data_in_rsc_rdy;
  input [10:0] widthIn_rsc_dat;
  output widthIn_rsc_triosy_lz;
  input [9:0] heightIn_rsc_dat;
  output heightIn_rsc_triosy_lz;
  output [15:0] acc_rsc_dat;
  output acc_rsc_vld;
  input acc_rsc_rdy;


  // Interconnect Declarations
  wire [10:0] widthIn_rsci_idat;
  wire [9:0] heightIn_rsci_idat;
  wire [15:0] acc_tmp_rsci_data_in_d;
  wire [18:0] acc_tmp_rsci_addr_d;
  wire [1:0] acc_tmp_rsci_re_d;
  wire [1:0] acc_tmp_rsci_we_d;
  wire [31:0] acc_tmp_rsci_data_out_d;
  wire acc_tmp_rsci_en_d;
  wire acc_tmp_rsc_en;
  wire [31:0] acc_tmp_rsc_data_out;
  wire [1:0] acc_tmp_rsc_we;
  wire [1:0] acc_tmp_rsc_re;
  wire [37:0] acc_tmp_rsc_addr;
  wire [31:0] acc_tmp_rsc_data_in;


  // Interconnect Declarations for Component Instantiations 
  wire [31:0] nl_acc_tmp_rsci_data_in_d;
  assign nl_acc_tmp_rsci_data_in_d = {16'b0000000000000000 , acc_tmp_rsci_data_in_d};
  wire [37:0] nl_acc_tmp_rsci_addr_d;
  assign nl_acc_tmp_rsci_addr_d = {19'b0000000000000000000 , acc_tmp_rsci_addr_d};
  ccs_in_v1 #(.rscid(32'sd10),
  .width(32'sd11)) widthIn_rsci (
      .dat(widthIn_rsc_dat),
      .idat(widthIn_rsci_idat)
    );
  ccs_in_v1 #(.rscid(32'sd11),
  .width(32'sd10)) heightIn_rsci (
      .dat(heightIn_rsc_dat),
      .idat(heightIn_rsci_idat)
    );
  ram_sync_dualRW_be #(.ram_id(32'sd13),
  .words(32'sd400000),
  .width(32'sd16),
  .addr_width(32'sd19),
  .a_reset_active(32'sd0),
  .s_reset_active(32'sd1),
  .enable_active(32'sd0),
  .re_active(32'sd0),
  .we_active(32'sd0),
  .num_byte_enables(32'sd1),
  .clock_edge(32'sd1),
  .no_of_RAM_dualRW_readwrite_port(32'sd2)) acc_tmp_rsc_comp (
      .data_in(acc_tmp_rsc_data_in),
      .addr(acc_tmp_rsc_addr),
      .re(acc_tmp_rsc_re),
      .we(acc_tmp_rsc_we),
      .data_out(acc_tmp_rsc_data_out),
      .clk(clk),
      .a_rst(1'b1),
      .s_rst(rst),
      .en(acc_tmp_rsc_en)
    );
  houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_13_400000_16_19_0_1_0_0_0_1_1_16_400000_2_gen
      acc_tmp_rsci (
      .en(acc_tmp_rsc_en),
      .data_out(acc_tmp_rsc_data_out),
      .we(acc_tmp_rsc_we),
      .re(acc_tmp_rsc_re),
      .addr(acc_tmp_rsc_addr),
      .data_in(acc_tmp_rsc_data_in),
      .data_in_d(nl_acc_tmp_rsci_data_in_d[31:0]),
      .addr_d(nl_acc_tmp_rsci_addr_d[37:0]),
      .re_d(acc_tmp_rsci_re_d),
      .we_d(acc_tmp_rsci_we_d),
      .data_out_d(acc_tmp_rsci_data_out_d),
      .en_d(acc_tmp_rsci_en_d)
    );
  houghTransform_core houghTransform_core_inst (
      .clk(clk),
      .rst(rst),
      .data_in_rsc_dat(data_in_rsc_dat),
      .data_in_rsc_vld(data_in_rsc_vld),
      .data_in_rsc_rdy(data_in_rsc_rdy),
      .widthIn_rsc_triosy_lz(widthIn_rsc_triosy_lz),
      .heightIn_rsc_triosy_lz(heightIn_rsc_triosy_lz),
      .acc_rsc_dat(acc_rsc_dat),
      .acc_rsc_vld(acc_rsc_vld),
      .acc_rsc_rdy(acc_rsc_rdy),
      .widthIn_rsci_idat(widthIn_rsci_idat),
      .heightIn_rsci_idat(heightIn_rsci_idat),
      .acc_tmp_rsci_data_in_d(acc_tmp_rsci_data_in_d),
      .acc_tmp_rsci_addr_d(acc_tmp_rsci_addr_d),
      .acc_tmp_rsci_re_d(acc_tmp_rsci_re_d),
      .acc_tmp_rsci_we_d(acc_tmp_rsci_we_d),
      .acc_tmp_rsci_data_out_d(acc_tmp_rsci_data_out_d),
      .acc_tmp_rsci_en_d(acc_tmp_rsci_en_d)
    );
endmodule

// ------------------------------------------------------------------
//  Design Unit:    Hough_Algorithm_HW_1296_864
// ------------------------------------------------------------------


module Hough_Algorithm_HW_1296_864 (
  clk, rst, data_in_rsc_dat, data_in_rsc_vld, data_in_rsc_rdy, widthIn_rsc_dat, widthIn_rsc_triosy_lz,
      heightIn_rsc_dat, heightIn_rsc_triosy_lz, x1_rsc_dat, x1_rsc_vld, x1_rsc_rdy,
      y1_rsc_dat, y1_rsc_vld, y1_rsc_rdy, x2_rsc_dat, x2_rsc_vld, x2_rsc_rdy, y2_rsc_dat,
      y2_rsc_vld, y2_rsc_rdy
);
  input clk;
  input rst;
  input [7:0] data_in_rsc_dat;
  input data_in_rsc_vld;
  output data_in_rsc_rdy;
  input [10:0] widthIn_rsc_dat;
  output widthIn_rsc_triosy_lz;
  input [9:0] heightIn_rsc_dat;
  output heightIn_rsc_triosy_lz;
  output [10:0] x1_rsc_dat;
  output x1_rsc_vld;
  input x1_rsc_rdy;
  output [9:0] y1_rsc_dat;
  output y1_rsc_vld;
  input y1_rsc_rdy;
  output [10:0] x2_rsc_dat;
  output x2_rsc_vld;
  input x2_rsc_rdy;
  output [9:0] y2_rsc_dat;
  output y2_rsc_vld;
  input y2_rsc_rdy;


  // Interconnect Declarations
  wire [15:0] acc_rsc_dat_nhoughTransform_inst;
  wire acc_rsc_rdy_nhoughTransform_inst;
  wire [10:0] x1_rsc_dat_ngetMaxLine_inst;
  wire [9:0] y1_rsc_dat_ngetMaxLine_inst;
  wire [10:0] x2_rsc_dat_ngetMaxLine_inst;
  wire [9:0] y2_rsc_dat_ngetMaxLine_inst;
  wire [15:0] acc_rsc_dat_ngetMaxLine_inst;
  wire acc_rsc_vld_ngetMaxLine_inst;
  wire data_in_rsc_rdy_nhoughTransform_inst_bud;
  wire widthIn_rsc_triosy_lz_nhoughTransform_inst_bud;
  wire heightIn_rsc_triosy_lz_nhoughTransform_inst_bud;
  wire acc_rsc_vld_nhoughTransform_inst_bud;
  wire acc_rsc_rdy_ngetMaxLine_inst_bud;
  wire x1_rsc_vld_ngetMaxLine_inst_bud;
  wire y1_rsc_vld_ngetMaxLine_inst_bud;
  wire x2_rsc_vld_ngetMaxLine_inst_bud;
  wire y2_rsc_vld_ngetMaxLine_inst_bud;
  wire acc_unc_2;
  wire acc_idle;


  // Interconnect Declarations for Component Instantiations 
  ccs_pipe_v5 #(.rscid(32'sd21),
  .width(32'sd16),
  .sz_width(32'sd1),
  .fifo_sz(32'sd1),
  .log2_sz(32'sd0),
  .ph_clk(32'sd1),
  .ph_en(32'sd0),
  .ph_arst(32'sd0),
  .ph_srst(32'sd1)) acc_cns_pipe (
      .clk(clk),
      .en(1'b0),
      .arst(1'b1),
      .srst(rst),
      .din_rdy(acc_rsc_rdy_nhoughTransform_inst),
      .din_vld(acc_rsc_vld_nhoughTransform_inst_bud),
      .din(acc_rsc_dat_nhoughTransform_inst),
      .dout_rdy(acc_rsc_rdy_ngetMaxLine_inst_bud),
      .dout_vld(acc_rsc_vld_ngetMaxLine_inst),
      .dout(acc_rsc_dat_ngetMaxLine_inst),
      .sz(acc_unc_2),
      .sz_req(1'b0),
      .is_idle(acc_idle)
    );
  houghTransform houghTransform_inst (
      .clk(clk),
      .rst(rst),
      .data_in_rsc_dat(data_in_rsc_dat),
      .data_in_rsc_vld(data_in_rsc_vld),
      .data_in_rsc_rdy(data_in_rsc_rdy_nhoughTransform_inst_bud),
      .widthIn_rsc_dat(widthIn_rsc_dat),
      .widthIn_rsc_triosy_lz(widthIn_rsc_triosy_lz_nhoughTransform_inst_bud),
      .heightIn_rsc_dat(heightIn_rsc_dat),
      .heightIn_rsc_triosy_lz(heightIn_rsc_triosy_lz_nhoughTransform_inst_bud),
      .acc_rsc_dat(acc_rsc_dat_nhoughTransform_inst),
      .acc_rsc_vld(acc_rsc_vld_nhoughTransform_inst_bud),
      .acc_rsc_rdy(acc_rsc_rdy_nhoughTransform_inst)
    );
  getMaxLine getMaxLine_inst (
      .clk(clk),
      .rst(rst),
      .x1_rsc_dat(x1_rsc_dat_ngetMaxLine_inst),
      .x1_rsc_vld(x1_rsc_vld_ngetMaxLine_inst_bud),
      .x1_rsc_rdy(x1_rsc_rdy),
      .y1_rsc_dat(y1_rsc_dat_ngetMaxLine_inst),
      .y1_rsc_vld(y1_rsc_vld_ngetMaxLine_inst_bud),
      .y1_rsc_rdy(y1_rsc_rdy),
      .x2_rsc_dat(x2_rsc_dat_ngetMaxLine_inst),
      .x2_rsc_vld(x2_rsc_vld_ngetMaxLine_inst_bud),
      .x2_rsc_rdy(x2_rsc_rdy),
      .y2_rsc_dat(y2_rsc_dat_ngetMaxLine_inst),
      .y2_rsc_vld(y2_rsc_vld_ngetMaxLine_inst_bud),
      .y2_rsc_rdy(y2_rsc_rdy),
      .acc_rsc_dat(acc_rsc_dat_ngetMaxLine_inst),
      .acc_rsc_vld(acc_rsc_vld_ngetMaxLine_inst),
      .acc_rsc_rdy(acc_rsc_rdy_ngetMaxLine_inst_bud)
    );
  assign data_in_rsc_rdy = data_in_rsc_rdy_nhoughTransform_inst_bud;
  assign widthIn_rsc_triosy_lz = widthIn_rsc_triosy_lz_nhoughTransform_inst_bud;
  assign heightIn_rsc_triosy_lz = heightIn_rsc_triosy_lz_nhoughTransform_inst_bud;
  assign x1_rsc_vld = x1_rsc_vld_ngetMaxLine_inst_bud;
  assign x1_rsc_dat = x1_rsc_dat_ngetMaxLine_inst;
  assign y1_rsc_vld = y1_rsc_vld_ngetMaxLine_inst_bud;
  assign y1_rsc_dat = y1_rsc_dat_ngetMaxLine_inst;
  assign x2_rsc_vld = x2_rsc_vld_ngetMaxLine_inst_bud;
  assign x2_rsc_dat = x2_rsc_dat_ngetMaxLine_inst;
  assign y2_rsc_vld = y2_rsc_vld_ngetMaxLine_inst_bud;
  assign y2_rsc_dat = y2_rsc_dat_ngetMaxLine_inst;
endmodule



