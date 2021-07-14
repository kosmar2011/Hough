
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


//------> ./rtl_Hough_Algorithm_HW_1296_864mgc_rom_27_70_32_1_60.v 
// ----------------------------------------------------------------------
//  HLS HDL:        Verilog Netlister
//  HLS Version:    10.5a/871028 Production Release
//  HLS Date:       Tue Apr 14 07:55:32 PDT 2020
// 
//  Generated by:   user2@edatools.ee.duth.gr
//  Generated date: Wed Jul 14 10:45:43 2021
// ----------------------------------------------------------------------

// 
module Hough_Algorithm_HW_1296_864mgc_rom_27_70_32_1_60 (addr, data_out
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



//------> ./rtl_Hough_Algorithm_HW_1296_864mgc_rom_26_70_32_1_60.v 
// ----------------------------------------------------------------------
//  HLS HDL:        Verilog Netlister
//  HLS Version:    10.5a/871028 Production Release
//  HLS Date:       Tue Apr 14 07:55:32 PDT 2020
// 
//  Generated by:   user2@edatools.ee.duth.gr
//  Generated date: Wed Jul 14 10:45:43 2021
// ----------------------------------------------------------------------

// 
module Hough_Algorithm_HW_1296_864mgc_rom_26_70_32_1_60 (addr, data_out
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
//  Generated date: Wed Jul 14 10:45:42 2021
// ----------------------------------------------------------------------

// 
// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_23_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
// ------------------------------------------------------------------


module getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_23_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    (
  en, data_out, we, re, addr, data_in, data_in_d, addr_d, re_d, we_d, data_out_d,
      en_d
);
  output en;
  input [53:0] data_out;
  output [1:0] we;
  output [1:0] re;
  output [15:0] addr;
  output [53:0] data_in;
  input [53:0] data_in_d;
  input [15:0] addr_d;
  input [1:0] re_d;
  input [1:0] we_d;
  output [53:0] data_out_d;
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
//  Design Unit:    getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_22_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
// ------------------------------------------------------------------


module getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_22_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    (
  en, data_out, we, re, addr, data_in, data_in_d, addr_d, re_d, we_d, data_out_d,
      en_d
);
  output en;
  input [53:0] data_out;
  output [1:0] we;
  output [1:0] re;
  output [15:0] addr;
  output [53:0] data_in;
  input [53:0] data_in_d;
  input [15:0] addr_d;
  input [1:0] re_d;
  input [1:0] we_d;
  output [53:0] data_out_d;
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
//  Design Unit:    getMaxLine_core_core_fsm
//  FSM Module
// ------------------------------------------------------------------


module getMaxLine_core_core_fsm (
  clk, rst, core_wen, fsm_output, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0_tr0,
      for_C_4_tr0, T_LINE_C_5_tr0, T_LINE_C_5_tr1, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0,
      T_LINE_C_9_tr0, T_LINE_C_9_tr1, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0,
      T_LINE_C_13_tr0, R_LINE_C_0_tr0
);
  input clk;
  input rst;
  input core_wen;
  output [26:0] fsm_output;
  reg [26:0] fsm_output;
  input ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0_tr0;
  input for_C_4_tr0;
  input T_LINE_C_5_tr0;
  input T_LINE_C_5_tr1;
  input ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0;
  input ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0;
  input T_LINE_C_9_tr0;
  input T_LINE_C_9_tr1;
  input ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0;
  input ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0;
  input T_LINE_C_13_tr0;
  input R_LINE_C_0_tr0;


  // FSM State Type Declaration for getMaxLine_core_core_fsm_1
  parameter
    main_C_0 = 5'd0,
    for_C_0 = 5'd1,
    for_C_1 = 5'd2,
    for_C_2 = 5'd3,
    ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0 = 5'd4,
    for_C_3 = 5'd5,
    for_C_4 = 5'd6,
    T_LINE_C_0 = 5'd7,
    T_LINE_C_1 = 5'd8,
    T_LINE_C_2 = 5'd9,
    T_LINE_C_3 = 5'd10,
    T_LINE_C_4 = 5'd11,
    T_LINE_C_5 = 5'd12,
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0
        = 5'd13,
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0
        = 5'd14,
    T_LINE_C_6 = 5'd15,
    T_LINE_C_7 = 5'd16,
    T_LINE_C_8 = 5'd17,
    T_LINE_C_9 = 5'd18,
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0
        = 5'd19,
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0
        = 5'd20,
    T_LINE_C_10 = 5'd21,
    T_LINE_C_11 = 5'd22,
    T_LINE_C_12 = 5'd23,
    T_LINE_C_13 = 5'd24,
    R_LINE_C_0 = 5'd25,
    main_C_1 = 5'd26;

  reg [4:0] state_var;
  reg [4:0] state_var_NS;


  // Interconnect Declarations for Component Instantiations 
  always @(*)
  begin : getMaxLine_core_core_fsm_1
    case (state_var)
      for_C_0 : begin
        fsm_output = 27'b000000000000000000000000010;
        state_var_NS = for_C_1;
      end
      for_C_1 : begin
        fsm_output = 27'b000000000000000000000000100;
        state_var_NS = for_C_2;
      end
      for_C_2 : begin
        fsm_output = 27'b000000000000000000000001000;
        state_var_NS = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0;
      end
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0 :
          begin
        fsm_output = 27'b000000000000000000000010000;
        if ( ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0_tr0
            ) begin
          state_var_NS = for_C_3;
        end
        else begin
          state_var_NS = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0;
        end
      end
      for_C_3 : begin
        fsm_output = 27'b000000000000000000000100000;
        state_var_NS = for_C_4;
      end
      for_C_4 : begin
        fsm_output = 27'b000000000000000000001000000;
        if ( for_C_4_tr0 ) begin
          state_var_NS = T_LINE_C_0;
        end
        else begin
          state_var_NS = for_C_0;
        end
      end
      T_LINE_C_0 : begin
        fsm_output = 27'b000000000000000000010000000;
        state_var_NS = T_LINE_C_1;
      end
      T_LINE_C_1 : begin
        fsm_output = 27'b000000000000000000100000000;
        state_var_NS = T_LINE_C_2;
      end
      T_LINE_C_2 : begin
        fsm_output = 27'b000000000000000001000000000;
        state_var_NS = T_LINE_C_3;
      end
      T_LINE_C_3 : begin
        fsm_output = 27'b000000000000000010000000000;
        state_var_NS = T_LINE_C_4;
      end
      T_LINE_C_4 : begin
        fsm_output = 27'b000000000000000100000000000;
        state_var_NS = T_LINE_C_5;
      end
      T_LINE_C_5 : begin
        fsm_output = 27'b000000000000001000000000000;
        if ( T_LINE_C_5_tr0 ) begin
          state_var_NS = T_LINE_C_6;
        end
        else if ( T_LINE_C_5_tr1 ) begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0;
        end
        else begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0;
        end
      end
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0
          : begin
        fsm_output = 27'b000000000000010000000000000;
        if ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0
            ) begin
          state_var_NS = T_LINE_C_6;
        end
        else begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0;
        end
      end
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0
          : begin
        fsm_output = 27'b000000000000100000000000000;
        if ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
            ) begin
          state_var_NS = T_LINE_C_6;
        end
        else begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0;
        end
      end
      T_LINE_C_6 : begin
        fsm_output = 27'b000000000001000000000000000;
        state_var_NS = T_LINE_C_7;
      end
      T_LINE_C_7 : begin
        fsm_output = 27'b000000000010000000000000000;
        state_var_NS = T_LINE_C_8;
      end
      T_LINE_C_8 : begin
        fsm_output = 27'b000000000100000000000000000;
        state_var_NS = T_LINE_C_9;
      end
      T_LINE_C_9 : begin
        fsm_output = 27'b000000001000000000000000000;
        if ( T_LINE_C_9_tr0 ) begin
          state_var_NS = T_LINE_C_10;
        end
        else if ( T_LINE_C_9_tr1 ) begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0;
        end
        else begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0;
        end
      end
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0
          : begin
        fsm_output = 27'b000000010000000000000000000;
        if ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0
            ) begin
          state_var_NS = T_LINE_C_10;
        end
        else begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0;
        end
      end
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0
          : begin
        fsm_output = 27'b000000100000000000000000000;
        if ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
            ) begin
          state_var_NS = T_LINE_C_10;
        end
        else begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0;
        end
      end
      T_LINE_C_10 : begin
        fsm_output = 27'b000001000000000000000000000;
        state_var_NS = T_LINE_C_11;
      end
      T_LINE_C_11 : begin
        fsm_output = 27'b000010000000000000000000000;
        state_var_NS = T_LINE_C_12;
      end
      T_LINE_C_12 : begin
        fsm_output = 27'b000100000000000000000000000;
        state_var_NS = T_LINE_C_13;
      end
      T_LINE_C_13 : begin
        fsm_output = 27'b001000000000000000000000000;
        if ( T_LINE_C_13_tr0 ) begin
          state_var_NS = R_LINE_C_0;
        end
        else begin
          state_var_NS = T_LINE_C_0;
        end
      end
      R_LINE_C_0 : begin
        fsm_output = 27'b010000000000000000000000000;
        if ( R_LINE_C_0_tr0 ) begin
          state_var_NS = main_C_1;
        end
        else begin
          state_var_NS = T_LINE_C_0;
        end
      end
      main_C_1 : begin
        fsm_output = 27'b100000000000000000000000000;
        state_var_NS = main_C_0;
      end
      // main_C_0
      default : begin
        fsm_output = 27'b000000000000000000000000001;
        state_var_NS = for_C_0;
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
  clk, rst, cos_out_1_rsc_cgo_iro, cos_out_1_rsci_data_out_d, cos_out_1_rsci_en_d,
      sin_out_1_rsci_data_out_d, T_LINE_if_if_dividend1_mul_cmp_z, core_wen, cos_out_1_rsc_cgo,
      cos_out_1_rsci_data_out_d_oreg, sin_out_1_rsci_data_out_d_oreg, T_LINE_if_if_dividend1_mul_cmp_z_oreg
);
  input clk;
  input rst;
  input cos_out_1_rsc_cgo_iro;
  input [53:0] cos_out_1_rsci_data_out_d;
  output cos_out_1_rsci_en_d;
  input [53:0] sin_out_1_rsci_data_out_d;
  input [43:0] T_LINE_if_if_dividend1_mul_cmp_z;
  input core_wen;
  input cos_out_1_rsc_cgo;
  output [26:0] cos_out_1_rsci_data_out_d_oreg;
  output [26:0] sin_out_1_rsci_data_out_d_oreg;
  output [43:0] T_LINE_if_if_dividend1_mul_cmp_z_oreg;
  reg [43:0] T_LINE_if_if_dividend1_mul_cmp_z_oreg;


  // Interconnect Declarations
  reg [26:0] cos_out_1_rsci_data_out_d_oreg_pconst_26_0;
  reg [26:0] sin_out_1_rsci_data_out_d_oreg_pconst_26_0;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_1_cse;


  // Interconnect Declarations for Component Instantiations 
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_1_cse
      = ~(core_wen & (cos_out_1_rsc_cgo | cos_out_1_rsc_cgo_iro));
  assign cos_out_1_rsci_en_d = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_1_cse;
  assign cos_out_1_rsci_data_out_d_oreg = cos_out_1_rsci_data_out_d_oreg_pconst_26_0;
  assign sin_out_1_rsci_data_out_d_oreg = sin_out_1_rsci_data_out_d_oreg_pconst_26_0;
  always @(posedge clk) begin
    if ( rst ) begin
      cos_out_1_rsci_data_out_d_oreg_pconst_26_0 <= 27'b000000000000000000000000000;
    end
    else if ( ~ cos_out_1_rsci_en_d ) begin
      cos_out_1_rsci_data_out_d_oreg_pconst_26_0 <= cos_out_1_rsci_data_out_d[26:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      sin_out_1_rsci_data_out_d_oreg_pconst_26_0 <= 27'b000000000000000000000000000;
    end
    else if ( ~ ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_1_cse
        ) begin
      sin_out_1_rsci_data_out_d_oreg_pconst_26_0 <= sin_out_1_rsci_data_out_d[26:0];
    end
  end
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
//  Design Unit:    houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_15_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
// ------------------------------------------------------------------


module houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_15_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    (
  en, data_out, we, re, addr, data_in, data_in_d, addr_d, re_d, we_d, data_out_d,
      en_d
);
  output en;
  input [53:0] data_out;
  output [1:0] we;
  output [1:0] re;
  output [15:0] addr;
  output [53:0] data_in;
  input [53:0] data_in_d;
  input [15:0] addr_d;
  input [1:0] re_d;
  input [1:0] we_d;
  output [53:0] data_out_d;
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
//  Design Unit:    houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_14_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
// ------------------------------------------------------------------


module houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_14_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
    (
  en, data_out, we, re, addr, data_in, data_in_d, addr_d, re_d, we_d, data_out_d,
      en_d
);
  output en;
  input [53:0] data_out;
  output [1:0] we;
  output [1:0] re;
  output [15:0] addr;
  output [53:0] data_in;
  input [53:0] data_in_d;
  input [15:0] addr_d;
  input [1:0] re_d;
  input [1:0] we_d;
  output [53:0] data_out_d;
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
  clk, rst, core_wen, fsm_output, acc_tmp_vinit_C_0_tr0, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0_tr0,
      for_1_C_4_tr0, HCOL_C_0_tr0, HACC_C_0_tr0, HCOL_C_1_tr0, HROW_C_0_tr0, WRITE_C_0_tr0
);
  input clk;
  input rst;
  input core_wen;
  output [14:0] fsm_output;
  reg [14:0] fsm_output;
  input acc_tmp_vinit_C_0_tr0;
  input ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0_tr0;
  input for_1_C_4_tr0;
  input HCOL_C_0_tr0;
  input HACC_C_0_tr0;
  input HCOL_C_1_tr0;
  input HROW_C_0_tr0;
  input WRITE_C_0_tr0;


  // FSM State Type Declaration for houghTransform_core_core_fsm_1
  parameter
    core_rlp_C_0 = 4'd0,
    main_C_0 = 4'd1,
    acc_tmp_vinit_C_0 = 4'd2,
    for_1_C_0 = 4'd3,
    for_1_C_1 = 4'd4,
    for_1_C_2 = 4'd5,
    ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0 = 4'd6,
    for_1_C_3 = 4'd7,
    for_1_C_4 = 4'd8,
    HCOL_C_0 = 4'd9,
    HACC_C_0 = 4'd10,
    HCOL_C_1 = 4'd11,
    HROW_C_0 = 4'd12,
    WRITE_C_0 = 4'd13,
    main_C_1 = 4'd14;

  reg [3:0] state_var;
  reg [3:0] state_var_NS;


  // Interconnect Declarations for Component Instantiations 
  always @(*)
  begin : houghTransform_core_core_fsm_1
    case (state_var)
      main_C_0 : begin
        fsm_output = 15'b000000000000010;
        state_var_NS = acc_tmp_vinit_C_0;
      end
      acc_tmp_vinit_C_0 : begin
        fsm_output = 15'b000000000000100;
        if ( acc_tmp_vinit_C_0_tr0 ) begin
          state_var_NS = for_1_C_0;
        end
        else begin
          state_var_NS = acc_tmp_vinit_C_0;
        end
      end
      for_1_C_0 : begin
        fsm_output = 15'b000000000001000;
        state_var_NS = for_1_C_1;
      end
      for_1_C_1 : begin
        fsm_output = 15'b000000000010000;
        state_var_NS = for_1_C_2;
      end
      for_1_C_2 : begin
        fsm_output = 15'b000000000100000;
        state_var_NS = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0;
      end
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0 :
          begin
        fsm_output = 15'b000000001000000;
        if ( ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0_tr0
            ) begin
          state_var_NS = for_1_C_3;
        end
        else begin
          state_var_NS = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0;
        end
      end
      for_1_C_3 : begin
        fsm_output = 15'b000000010000000;
        state_var_NS = for_1_C_4;
      end
      for_1_C_4 : begin
        fsm_output = 15'b000000100000000;
        if ( for_1_C_4_tr0 ) begin
          state_var_NS = HCOL_C_0;
        end
        else begin
          state_var_NS = for_1_C_0;
        end
      end
      HCOL_C_0 : begin
        fsm_output = 15'b000001000000000;
        if ( HCOL_C_0_tr0 ) begin
          state_var_NS = HCOL_C_1;
        end
        else begin
          state_var_NS = HACC_C_0;
        end
      end
      HACC_C_0 : begin
        fsm_output = 15'b000010000000000;
        if ( HACC_C_0_tr0 ) begin
          state_var_NS = HCOL_C_1;
        end
        else begin
          state_var_NS = HACC_C_0;
        end
      end
      HCOL_C_1 : begin
        fsm_output = 15'b000100000000000;
        if ( HCOL_C_1_tr0 ) begin
          state_var_NS = HROW_C_0;
        end
        else begin
          state_var_NS = HCOL_C_0;
        end
      end
      HROW_C_0 : begin
        fsm_output = 15'b001000000000000;
        if ( HROW_C_0_tr0 ) begin
          state_var_NS = WRITE_C_0;
        end
        else begin
          state_var_NS = HCOL_C_0;
        end
      end
      WRITE_C_0 : begin
        fsm_output = 15'b010000000000000;
        if ( WRITE_C_0_tr0 ) begin
          state_var_NS = main_C_1;
        end
        else begin
          state_var_NS = WRITE_C_0;
        end
      end
      main_C_1 : begin
        fsm_output = 15'b100000000000000;
        state_var_NS = main_C_0;
      end
      // core_rlp_C_0
      default : begin
        fsm_output = 15'b000000000000001;
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
  clk, rst, acc_tmp_rsc_cgo_iro, acc_tmp_rsci_data_out_d, acc_tmp_rsci_en_d, cos_out_rsc_cgo_iro,
      cos_out_rsci_data_out_d, cos_out_rsci_en_d, sin_out_rsci_data_out_d, core_wen,
      acc_tmp_rsc_cgo, acc_tmp_rsci_data_out_d_oreg, cos_out_rsc_cgo, cos_out_rsci_data_out_d_oreg,
      sin_out_rsci_data_out_d_oreg
);
  input clk;
  input rst;
  input acc_tmp_rsc_cgo_iro;
  input [31:0] acc_tmp_rsci_data_out_d;
  output acc_tmp_rsci_en_d;
  input cos_out_rsc_cgo_iro;
  input [53:0] cos_out_rsci_data_out_d;
  output cos_out_rsci_en_d;
  input [53:0] sin_out_rsci_data_out_d;
  input core_wen;
  input acc_tmp_rsc_cgo;
  output [31:0] acc_tmp_rsci_data_out_d_oreg;
  reg [31:0] acc_tmp_rsci_data_out_d_oreg;
  input cos_out_rsc_cgo;
  output [26:0] cos_out_rsci_data_out_d_oreg;
  output [26:0] sin_out_rsci_data_out_d_oreg;


  // Interconnect Declarations
  reg [26:0] cos_out_rsci_data_out_d_oreg_pconst_26_0;
  reg [26:0] sin_out_rsci_data_out_d_oreg_pconst_26_0;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_1_cse;


  // Interconnect Declarations for Component Instantiations 
  assign acc_tmp_rsci_en_d = ~(core_wen & (acc_tmp_rsc_cgo | acc_tmp_rsc_cgo_iro));
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_1_cse
      = ~(core_wen & (cos_out_rsc_cgo | cos_out_rsc_cgo_iro));
  assign cos_out_rsci_en_d = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_1_cse;
  assign cos_out_rsci_data_out_d_oreg = cos_out_rsci_data_out_d_oreg_pconst_26_0;
  assign sin_out_rsci_data_out_d_oreg = sin_out_rsci_data_out_d_oreg_pconst_26_0;
  always @(posedge clk) begin
    if ( rst ) begin
      acc_tmp_rsci_data_out_d_oreg <= 32'b00000000000000000000000000000000;
    end
    else if ( ~ acc_tmp_rsci_en_d ) begin
      acc_tmp_rsci_data_out_d_oreg <= acc_tmp_rsci_data_out_d;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      cos_out_rsci_data_out_d_oreg_pconst_26_0 <= 27'b000000000000000000000000000;
    end
    else if ( ~ cos_out_rsci_en_d ) begin
      cos_out_rsci_data_out_d_oreg_pconst_26_0 <= cos_out_rsci_data_out_d[26:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      sin_out_rsci_data_out_d_oreg_pconst_26_0 <= 27'b000000000000000000000000000;
    end
    else if ( ~ ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nand_1_cse
        ) begin
      sin_out_rsci_data_out_d_oreg_pconst_26_0 <= sin_out_rsci_data_out_d[26:0];
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
  ccs_in_wait_v1 #(.rscid(32'sd21),
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
  ccs_out_wait_v1 #(.rscid(32'sd20),
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
  ccs_out_wait_v1 #(.rscid(32'sd19),
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
  ccs_out_wait_v1 #(.rscid(32'sd18),
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
  ccs_out_wait_v1 #(.rscid(32'sd17),
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
      acc_rsc_vld, acc_rsc_rdy, cos_out_1_rsci_data_in_d, cos_out_1_rsci_addr_d,
      cos_out_1_rsci_re_d, cos_out_1_rsci_we_d, cos_out_1_rsci_data_out_d, cos_out_1_rsci_en_d,
      sin_out_1_rsci_data_in_d, sin_out_1_rsci_re_d, sin_out_1_rsci_we_d, sin_out_1_rsci_data_out_d,
      T_LINE_if_if_dividend1_mul_cmp_a, T_LINE_if_if_dividend1_mul_cmp_b, T_LINE_if_if_dividend1_mul_cmp_z
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
  output [26:0] cos_out_1_rsci_data_in_d;
  output [7:0] cos_out_1_rsci_addr_d;
  output [1:0] cos_out_1_rsci_re_d;
  output [1:0] cos_out_1_rsci_we_d;
  input [53:0] cos_out_1_rsci_data_out_d;
  output cos_out_1_rsci_en_d;
  output [26:0] sin_out_1_rsci_data_in_d;
  output [1:0] sin_out_1_rsci_re_d;
  output [1:0] sin_out_1_rsci_we_d;
  input [53:0] sin_out_1_rsci_data_out_d;
  output [17:0] T_LINE_if_if_dividend1_mul_cmp_a;
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
  wire [26:0] cos_out_1_rsci_data_out_d_oreg;
  wire [26:0] sin_out_1_rsci_data_out_d_oreg;
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
  wire [26:0] fsm_output;
  wire or_dcpl_8;
  wire or_dcpl_11;
  wire and_dcpl_54;
  wire or_dcpl_66;
  wire or_dcpl_74;
  wire or_dcpl_75;
  wire or_dcpl_80;
  wire or_dcpl_95;
  wire or_dcpl_107;
  wire or_dcpl_109;
  wire or_dcpl_110;
  wire or_tmp_124;
  wire or_tmp_125;
  wire or_tmp_126;
  wire or_tmp_127;
  wire or_tmp_138;
  wire or_tmp_191;
  wire or_tmp_250;
  wire or_tmp_379;
  wire and_281_cse;
  wire and_359_cse;
  wire and_357_cse;
  reg operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs;
  reg T_LINE_slc_T_LINE_acc_6_itm;
  reg T_LINE_if_land_lpi_3_dfm;
  reg operator_27_3_true_AC_TRN_AC_WRAP_return_sva;
  reg operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs;
  reg [26:0] T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva;
  reg [26:0] T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva;
  reg [19:0] T_LINE_if_if_dividend1_sva;
  reg [19:0] T_LINE_if_else_dividend1_sva;
  reg [19:0] T_LINE_if_if_dividend2_sva;
  reg [19:0] T_LINE_if_else_dividend2_sva;
  reg [2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_1_34_32;
  reg [31:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_1_31_0;
  wire asn_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_nor_1_ssc;
  wire and_214_ssc;
  wire and_216_ssc;
  wire asn_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_nor_ssc;
  reg [8:0] reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_2;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_3;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_4;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_5;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_6;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_7;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_8;
  reg reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_9;
  wire Hough_Algorithm_HW_1296_864_getMaxLine_and_cse;
  reg reg_sin_out_1_rsc_cgo_cse;
  reg reg_acc_rsci_irdy_core_psct_cse;
  reg reg_y2_rsci_ivld_core_psct_cse;
  wire or_147_cse;
  wire and_58_cse;
  wire T_LINE_and_4;
  wire T_LINE_T_LINE_nor_3;
  reg [8:0] x1_t_26_18_lpi_3;
  reg [12:0] for_acc_7_psp_12_0;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_19_sva;
  reg [26:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva;
  wire or_234_rmff;
  wire [32:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0;
  reg [7:0] T_LINE_t_7_0_sva;
  wire [15:0] z_out;
  wire [16:0] nl_z_out;
  wire [8:0] z_out_2;
  wire [9:0] nl_z_out_2;
  wire [9:0] z_out_3;
  wire [10:0] nl_z_out_3;
  wire [10:0] z_out_4;
  wire [11:0] nl_z_out_4;
  wire or_tmp_457;
  wire [27:0] z_out_5;
  wire or_tmp_459;
  wire or_tmp_461;
  wire [43:0] z_out_6;
  wire [44:0] nl_z_out_6;
  wire [7:0] z_out_7;
  wire [8:0] nl_z_out_7;
  wire [26:0] z_out_8;
  wire [27:0] nl_z_out_8;
  wire or_tmp_476;
  wire [23:0] z_out_9;
  wire [24:0] nl_z_out_9;
  wire [32:0] z_out_10;
  wire [34:0] z_out_11;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs;
  reg [4:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva;
  reg [15:0] threshold_23_8_lpi_3;
  reg x1_t_13_lpi_3;
  reg x1_t_14_lpi_3;
  reg x1_t_12_lpi_3;
  reg x1_t_15_lpi_3;
  reg x1_t_11_lpi_3;
  reg x1_t_16_lpi_3;
  reg x1_t_10_lpi_3;
  reg x1_t_17_lpi_3;
  reg y1_t_14_lpi_3;
  reg y1_t_13_lpi_3;
  reg y1_t_15_lpi_3;
  reg y1_t_12_lpi_3;
  reg y1_t_16_lpi_3;
  reg y1_t_11_lpi_3;
  reg y1_t_17_lpi_3;
  reg y1_t_10_lpi_3;
  reg y1_t_18_lpi_3;
  reg [6:0] y1_t_25_19_lpi_3;
  reg x2_t_13_lpi_3;
  reg x2_t_14_lpi_3;
  reg x2_t_12_lpi_3;
  reg x2_t_15_lpi_3;
  reg x2_t_11_lpi_3;
  reg x2_t_16_lpi_3;
  reg x2_t_10_lpi_3;
  reg x2_t_17_lpi_3;
  reg y2_t_14_lpi_3;
  reg y2_t_13_lpi_3;
  reg y2_t_15_lpi_3;
  reg y2_t_12_lpi_3;
  reg y2_t_16_lpi_3;
  reg y2_t_11_lpi_3;
  reg y2_t_17_lpi_3;
  reg y2_t_10_lpi_3;
  reg y2_t_18_lpi_3;
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
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_9_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_7_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_6_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_5_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_4_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_3_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_2_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_1_lpi_3_dfm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_0_lpi_3_dfm;
  reg [9:0] operator_27_12_true_AC_TRN_AC_WRAP_acc_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_1_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_2_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_3_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_4_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_5_itm;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_6_itm;
  reg [2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_32_30;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_29;
  reg [1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_28_27;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_24;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_21;
  reg [1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_15_14;
  reg [2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_7;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_4;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_3;
  reg [2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_32_30;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_29;
  reg [1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_26_25;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_24;
  reg [1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_23_22;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_21;
  reg [2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_20_18;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_17;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_16;
  reg [1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14;
  reg [2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_13_11;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_10;
  reg [1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_9_8;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_7;
  reg [1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_6_5;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_4;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_3;
  reg [1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_2_1;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_0;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_26;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_25;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_23;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_22;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_20;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_9;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_8;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_6;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_5;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_28;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_27;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1;
  wire [1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14_mx0w0;
  wire [2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0;
  wire [4:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_2;
  wire [5:0] nl_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_2;
  wire [32:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_1_sva_1;
  wire [32:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_sva_1;
  wire [31:0] ac_math_atan_pi_2mi_return_1_69_38_sva_1;
  wire [19:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0;
  wire [19:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0;
  wire [27:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_lpi_3_dfm_mx0;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_10_lpi_3_dfm_1;
  wire [19:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0;
  wire [19:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0;
  wire or_395_tmp;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_m1c_6;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_m1c_7;
  wire or_312_ssc;
  reg reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd;
  reg [25:0] reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1;
  reg [6:0] reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_ftd_1;
  reg [8:0] reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_ftd_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_28_m1c;
  wire or_126_tmp;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_1_m1c;
  wire and_1073_ssc;
  wire and_1075_ssc;
  wire and_1076_ssc;
  wire and_1078_ssc;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_and_2_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_or_cse;
  wire or_569_cse;
  wire or_cse;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_and_1_cse;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_and_cse;
  wire or_309_tmp;
  wire y2_t_and_rgt;
  wire y2_t_and_1_rgt;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27;
  reg [26:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0;
  reg [1:0] reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_ftd_1_0;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_and_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_and_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_and_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_and_cse;
  wire x1_t_and_2_cse;
  wire x2_t_and_2_cse;
  wire x1_t_and_cse;
  wire x1_t_and_1_cse;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_30_cse;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_31_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_43_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_46_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_6_cse;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_7_cse;
  wire or_621_tmp;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_or_itm;
  wire for_nor_3_itm;
  wire operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_itm_16_1;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1;
  wire [5:0] z_out_1_8_3;
  wire [1:0] operator_26_11_true_AC_TRN_AC_WRAP_operator_26_11_true_AC_TRN_AC_WRAP_operator_26_11_true_AC_TRN_AC_WRAP_nor_2_cse;

  wire[0:0] or_248_nl;
  wire[0:0] or_250_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_16_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_16_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_11_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_11_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_10_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_10_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_9_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_9_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_16_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_16_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_11_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_11_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_10_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_10_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_9_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_9_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_11_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_11_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_10_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_10_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_9_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_9_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_8_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_8_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_7_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_7_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_11_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_11_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_10_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_9_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_8_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_7_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_nl;
  wire[7:0] for_t_mux_nl;
  wire[0:0] or_286_nl;
  wire[0:0] nor_nl;
  wire[0:0] T_LINE_if_aelse_T_LINE_if_aelse_and_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_and_3_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_and_1_nl;
  wire[0:0] and_486_nl;
  wire[0:0] and_1106_nl;
  wire[6:0] T_LINE_if_T_LINE_if_and_17_nl;
  wire[0:0] mux_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_or_1_nl;
  wire[0:0] x2_t_and_nl;
  wire[0:0] x2_t_and_1_nl;
  wire[10:0] R_LINE_r_R_LINE_r_and_nl;
  wire[10:0] R_LINE_r_mux_nl;
  wire[0:0] nor_42_nl;
  wire[0:0] nor_41_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_and_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_nor_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_1_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_4_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_5_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_6_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_7_nl;
  wire[0:0] or_333_nl;
  wire[0:0] and_568_nl;
  wire[1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux1h_1_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux1h_2_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux1h_3_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_10_nl;
  wire[6:0] T_LINE_acc_nl;
  wire[7:0] nl_T_LINE_acc_nl;
  wire[5:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_and_nl;
  wire[0:0] operator_27_12_true_AC_TRN_AC_WRAP_operator_27_12_true_AC_TRN_AC_WRAP_nor_nl;
  wire[0:0] operator_27_12_true_AC_TRN_AC_WRAP_and_1_nl;
  wire[0:0] or_398_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_T000000;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_11_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_10_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_18_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_24_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_9_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_8_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_7_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_6_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_5_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_4_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_66_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_3_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_2_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_1_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_nl;
  wire[26:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_1_nl;
  wire[0:0] not_387_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_mux1h_79_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_not_nl;
  wire[32:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_1_nl;
  wire[33:0] nl_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_1_nl;
  wire[16:0] operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_nl;
  wire[17:0] nl_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_nl;
  wire[35:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_nl;
  wire[36:0] nl_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_nl;
  wire[0:0] operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_7_nl;
  wire[0:0] operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_8_nl;
  wire[0:0] operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_9_nl;
  wire[0:0] operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_10_nl;
  wire[0:0] operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_11_nl;
  wire[0:0] operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_12_nl;
  wire[0:0] operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_13_nl;
  wire[0:0] operator_28_13_true_AC_TRN_AC_WRAP_mux_10_nl;
  wire[0:0] operator_28_13_true_AC_TRN_AC_WRAP_mux_11_nl;
  wire[0:0] operator_28_13_true_AC_TRN_AC_WRAP_mux_12_nl;
  wire[0:0] operator_28_13_true_AC_TRN_AC_WRAP_mux_13_nl;
  wire[0:0] operator_28_13_true_AC_TRN_AC_WRAP_mux_14_nl;
  wire[0:0] operator_28_13_true_AC_TRN_AC_WRAP_mux_15_nl;
  wire[0:0] operator_28_13_true_AC_TRN_AC_WRAP_mux_16_nl;
  wire[0:0] operator_28_13_true_AC_TRN_AC_WRAP_mux_17_nl;
  wire[0:0] operator_28_13_true_AC_TRN_AC_WRAP_mux_18_nl;
  wire[0:0] operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_or_1_nl;
  wire[8:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl;
  wire[9:0] nl_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_or_2_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_or_3_nl;
  wire[7:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux1h_2_nl;
  wire[0:0] operator_20_15_true_AC_TRN_AC_WRAP_2_mux_9_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_17_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_42_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_17_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_43_nl;
  wire[0:0] operator_20_15_true_AC_TRN_AC_WRAP_2_mux_10_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_18_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_27_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_18_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_28_nl;
  wire[0:0] operator_20_15_true_AC_TRN_AC_WRAP_2_mux_11_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_19_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_44_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_19_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_45_nl;
  wire[0:0] operator_20_15_true_AC_TRN_AC_WRAP_2_mux_12_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_20_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_46_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_20_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_47_nl;
  wire[0:0] operator_20_15_true_AC_TRN_AC_WRAP_2_mux_13_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_21_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_48_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_21_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_49_nl;
  wire[0:0] operator_20_15_true_AC_TRN_AC_WRAP_2_mux_14_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_22_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_50_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_22_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_51_nl;
  wire[0:0] operator_20_15_true_AC_TRN_AC_WRAP_2_mux_15_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_23_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_52_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_23_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_53_nl;
  wire[0:0] operator_20_15_true_AC_TRN_AC_WRAP_2_mux_16_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_24_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_54_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_24_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_55_nl;
  wire[0:0] operator_20_15_true_AC_TRN_AC_WRAP_2_mux_17_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_25_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_56_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_25_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_57_nl;
  wire[8:0] operator_27_12_true_AC_TRN_AC_WRAP_mux1h_4_nl;
  wire[10:0] for_mux_25_nl;
  wire[28:0] acc_5_nl;
  wire[29:0] nl_acc_5_nl;
  wire[18:0] for_for_or_19_nl;
  wire[0:0] not_401_nl;
  wire[7:0] for_mux_26_nl;
  wire[0:0] for_or_2_nl;
  wire[0:0] for_for_and_7_nl;
  wire[12:0] for_for_and_8_nl;
  wire[7:0] for_mux_27_nl;
  wire[3:0] for_for_or_20_nl;
  wire[0:0] not_403_nl;
  wire[0:0] for_mux_28_nl;
  wire[43:0] for_mux1h_6_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_and_3_nl;
  wire[26:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_mux_4_nl;
  wire[39:0] for_mux1h_7_nl;
  wire[1:0] operator_26_11_true_AC_TRN_AC_WRAP_mux_nl;
  wire[6:0] operator_26_11_true_AC_TRN_AC_WRAP_mux1h_2_nl;
  wire[6:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_nor_1_nl;
  wire[6:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_mux1h_2_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_or_1_nl;
  wire[18:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_mux1h_3_nl;
  wire[7:0] for_for_and_9_nl;
  wire[7:0] for_mux_29_nl;
  wire[0:0] for_nor_7_nl;
  wire[0:0] for_for_and_10_nl;
  wire[7:0] for_and_14_nl;
  wire[7:0] for_mux1h_8_nl;
  wire[0:0] not_404_nl;
  wire[0:0] for_for_and_11_nl;
  wire[0:0] for_mux_30_nl;
  wire[0:0] for_or_3_nl;
  wire[0:0] for_mux1h_9_nl;
  wire[0:0] for_for_or_21_nl;
  wire[2:0] for_for_or_22_nl;
  wire[2:0] for_and_15_nl;
  wire[0:0] for_for_or_23_nl;
  wire[0:0] for_mux_31_nl;
  wire[0:0] for_for_or_24_nl;
  wire[0:0] for_mux_32_nl;
  wire[0:0] for_for_or_25_nl;
  wire[0:0] for_mux_33_nl;
  wire[0:0] for_for_or_26_nl;
  wire[0:0] for_mux_34_nl;
  wire[0:0] for_for_or_27_nl;
  wire[0:0] for_mux_35_nl;
  wire[0:0] for_for_or_28_nl;
  wire[0:0] for_mux_36_nl;
  wire[0:0] for_for_or_29_nl;
  wire[0:0] for_mux_37_nl;
  wire[0:0] for_for_or_30_nl;
  wire[0:0] for_mux_38_nl;
  wire[0:0] for_for_or_31_nl;
  wire[0:0] for_mux_39_nl;
  wire[0:0] for_for_or_32_nl;
  wire[0:0] for_mux_40_nl;
  wire[0:0] for_for_or_33_nl;
  wire[0:0] for_mux_41_nl;
  wire[0:0] for_for_for_nor_2_nl;
  wire[0:0] for_mux_42_nl;
  wire[0:0] for_for_or_34_nl;
  wire[0:0] for_mux_43_nl;
  wire[0:0] for_for_or_35_nl;
  wire[0:0] for_mux_44_nl;
  wire[0:0] for_for_for_nor_3_nl;
  wire[0:0] for_mux_45_nl;
  wire[0:0] for_for_or_36_nl;
  wire[0:0] for_mux_46_nl;
  wire[0:0] for_for_or_37_nl;
  wire[0:0] for_mux_47_nl;
  wire[33:0] acc_10_nl;
  wire[34:0] nl_acc_10_nl;
  wire[2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_23_nl;
  wire[0:0] not_415_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_24_nl;
  wire[1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_25_nl;
  wire[0:0] not_417_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_26_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_27_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_28_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_29_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_30_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_31_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_32_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_33_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_34_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_35_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_36_nl;
  wire[1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_37_nl;
  wire[0:0] not_429_nl;
  wire[2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_38_nl;
  wire[0:0] not_430_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_39_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_40_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_41_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_42_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_43_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_44_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_45_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_4_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_5_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_6_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_7_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_5_nl;
  wire[32:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux1h_5_nl;
  wire[35:0] acc_11_nl;
  wire[36:0] nl_acc_11_nl;
  wire[2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_and_1_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_not_2_nl;
  wire[31:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_mux_1_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_or_2_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_or_1_nl;
  wire[31:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_mux1h_2_nl;

  // Interconnect Declarations for Component Instantiations 
  wire [32:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_a;
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_a = {ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_32_30
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_29
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_28_27
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_26
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_25
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_24
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_23
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_22
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_21
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_20
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_6_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_5_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_4_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_3_itm
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_15_14
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_2_itm
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_9
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_8
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_7
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_6
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_5
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_4
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_3
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_1_itm
      , T_LINE_slc_T_LINE_acc_6_itm};
  wire [4:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_s;
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_s = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva;
  wire [32:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a;
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a = {ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_32_30
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_29
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_28
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_27
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_26_25
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_24
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_23_22
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_21
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_20_18
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_17
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_16
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_13_11
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_10
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_9_8
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_7
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_6_5
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_4
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_3
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_2_1
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_0};
  wire [4:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s;
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva;
  wire [6:0] nl_ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr;
  assign nl_ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr
      = {2'b0, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva};
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
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0_tr0
      = ~ (z_out_1_8_3[0]);
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_for_C_4_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_for_C_4_tr0 = ~ (z_out_7[6]);
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_T_LINE_C_5_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_T_LINE_C_5_tr0 = operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs;
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0
      = ~ (z_out_10[4]);
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
      = ~ (z_out_10[4]);
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_T_LINE_C_9_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_T_LINE_C_9_tr0 = operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs;
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0
      = ~ (z_out_10[4]);
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
      = ~ (z_out_10[4]);
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_T_LINE_C_13_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_T_LINE_C_13_tr0 = ~ T_LINE_slc_T_LINE_acc_6_itm;
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_R_LINE_C_0_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_R_LINE_C_0_tr0 = ~ (z_out_1_8_3[5]);
  mgc_shift_r_v5 #(.width_a(32'sd33),
  .signd_a(32'sd1),
  .width_s(32'sd5),
  .width_z(32'sd33)) operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg (
      .a(nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_a[32:0]),
      .s(nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_s[4:0]),
      .z(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_1_sva_1)
    );
  mgc_shift_r_v5 #(.width_a(32'sd33),
  .signd_a(32'sd1),
  .width_s(32'sd5),
  .width_z(32'sd33)) operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg (
      .a(nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a[32:0]),
      .s(nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s[4:0]),
      .z(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_sva_1)
    );
  Hough_Algorithm_HW_1296_864mgc_rom_27_70_32_1_60  ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg
      (
      .addr(nl_ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr[6:0]),
      .data_out(ac_math_atan_pi_2mi_return_1_69_38_sva_1)
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
      .cos_out_1_rsc_cgo_iro(or_234_rmff),
      .cos_out_1_rsci_data_out_d(cos_out_1_rsci_data_out_d),
      .cos_out_1_rsci_en_d(cos_out_1_rsci_en_d),
      .sin_out_1_rsci_data_out_d(sin_out_1_rsci_data_out_d),
      .T_LINE_if_if_dividend1_mul_cmp_z(T_LINE_if_if_dividend1_mul_cmp_z),
      .core_wen(core_wen),
      .cos_out_1_rsc_cgo(reg_sin_out_1_rsc_cgo_cse),
      .cos_out_1_rsci_data_out_d_oreg(cos_out_1_rsci_data_out_d_oreg),
      .sin_out_1_rsci_data_out_d_oreg(sin_out_1_rsci_data_out_d_oreg),
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
      .ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0_tr0(nl_getMaxLine_core_core_fsm_inst_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0_tr0[0:0]),
      .for_C_4_tr0(nl_getMaxLine_core_core_fsm_inst_for_C_4_tr0[0:0]),
      .T_LINE_C_5_tr0(nl_getMaxLine_core_core_fsm_inst_T_LINE_C_5_tr0[0:0]),
      .T_LINE_C_5_tr1(T_LINE_and_4),
      .ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0(nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0[0:0]),
      .ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0(nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0[0:0]),
      .T_LINE_C_9_tr0(nl_getMaxLine_core_core_fsm_inst_T_LINE_C_9_tr0[0:0]),
      .T_LINE_C_9_tr1(T_LINE_and_4),
      .ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0(nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0[0:0]),
      .ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0(nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0[0:0]),
      .T_LINE_C_13_tr0(nl_getMaxLine_core_core_fsm_inst_T_LINE_C_13_tr0[0:0]),
      .R_LINE_C_0_tr0(nl_getMaxLine_core_core_fsm_inst_R_LINE_C_0_tr0[0:0])
    );
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_and_cse
      = core_wen & (fsm_output[13]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_and_cse
      = core_wen & (fsm_output[14]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_and_cse
      = core_wen & (~ or_dcpl_8) & (fsm_output[17]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_and_cse
      = core_wen & (~ or_dcpl_11) & (fsm_output[17]);
  assign Hough_Algorithm_HW_1296_864_getMaxLine_and_cse = core_wen & (~((~ (fsm_output[25]))
      | (z_out_1_8_3[5])));
  assign or_234_rmff = and_357_cse | (fsm_output[5]) | and_359_cse | and_281_cse;
  assign T_LINE_if_if_dividend1_mul_cmp_a = {reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd
      , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1 , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_2
      , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_3 , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_4
      , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_5 , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_6
      , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_7 , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_8
      , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_9};
  assign x1_t_and_2_cse = core_wen & or_tmp_138;
  assign x2_t_and_2_cse = core_wen & (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      & (fsm_output[23]);
  assign or_126_tmp = (~ (z_out_10[4])) | operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
      | T_LINE_if_land_lpi_3_dfm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_1_m1c
      = (fsm_output[14]) | (fsm_output[20]);
  assign x1_t_and_cse = operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
      & (fsm_output[17]);
  assign x1_t_and_1_cse = (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      & (fsm_output[17]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_and_2_cse
      = (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      & or_tmp_191;
  assign y2_t_and_rgt = T_LINE_and_4 & (fsm_output[24]);
  assign y2_t_and_1_rgt = T_LINE_T_LINE_nor_3 & (fsm_output[24]);
  assign and_58_cse = (~ (z_out_7[6])) & (fsm_output[6]);
  assign or_312_ssc = or_dcpl_80 | or_dcpl_75 | (fsm_output[19]) | or_dcpl_74;
  assign or_309_tmp = (fsm_output[4:3]!=2'b00);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_and_1_cse
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1
      & (fsm_output[4]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_and_cse
      = (~ ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1)
      & (fsm_output[4]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_or_itm
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_and_cse
      | ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_and_1_cse;
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_m1c_6
      = (~ operator_27_3_true_AC_TRN_AC_WRAP_return_sva) & (fsm_output[17]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_m1c_7
      = (~ operator_27_3_true_AC_TRN_AC_WRAP_return_sva) & (fsm_output[23]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_30_cse
      = operator_27_3_true_AC_TRN_AC_WRAP_return_sva & (fsm_output[17]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_31_cse
      = operator_27_3_true_AC_TRN_AC_WRAP_return_sva & (fsm_output[23]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_6_cse
      = ((~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs)
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_m1c_6)
      | ((~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs)
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_m1c_7);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_7_cse
      = (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_m1c_6)
      | (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs
      & ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_m1c_7);
  assign or_147_cse = (z_out_9[8]) | (z_out_1_8_3[2]);
  assign or_395_tmp = or_dcpl_66 | (fsm_output[14]) | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_28_m1c
      | (fsm_output[20]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_or_cse
      = (T_LINE_if_land_lpi_3_dfm & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_28_m1c)
      | ((~ T_LINE_if_land_lpi_3_dfm) & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_1_m1c);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_28_m1c
      = (fsm_output[13]) | (fsm_output[19]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse
      = (or_dcpl_109 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_28_m1c)
      | (or_dcpl_110 & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_1_m1c)
      | or_dcpl_107 | (fsm_output[15]) | (fsm_output[21]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_43_cse
      = (~ or_dcpl_109) & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_28_m1c;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_46_cse
      = (~ or_dcpl_110) & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_1_m1c;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      = (~ T_LINE_if_land_lpi_3_dfm) & (fsm_output[12]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      = T_LINE_if_land_lpi_3_dfm & (fsm_output[12]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      = (~ T_LINE_if_land_lpi_3_dfm) & (fsm_output[18]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      = T_LINE_if_land_lpi_3_dfm & (fsm_output[18]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx1
      = MUX_s_1_2_2((z_out_7[2]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs,
      z_out_11[26]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1
      = ~(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_not_nl
      = ~ (z_out_11[26]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14_mx0w0
      = MUX_v_2_2_2(2'b00, (signext_2_1(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_not_nl)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx1);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0
      = ~(MUX_v_3_2_2((signext_3_1(z_out_11[26])), 3'b111, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx1));
  assign nl_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_2
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva
      + 5'b00001;
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_2
      = nl_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_2[4:0];
  assign nl_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_1_nl
      = ({ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_32_30
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_29
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_28
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_27
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_26_25
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_24
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_23_22
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_21
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_20_18
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_17
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_16
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_13_11
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_10
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_9_8
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_7
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_6_5
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_4
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_3
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_2_1
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_0})
      - ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_1_sva_1;
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_1_nl
      = nl_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_1_nl[32:0];
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0
      = MUX_v_33_2_2((z_out_6[32:0]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_1_nl,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1);
  assign nl_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_nl = ({1'b1 , acc_rsci_idat_mxwt})
      + conv_u2u_16_17(~ threshold_23_8_lpi_3) + 17'b00000000000000001;
  assign operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_nl = nl_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_nl[16:0];
  assign operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_itm_16_1 = readslicef_17_1_16(operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_nl);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0
      = MUX_v_20_2_2(({1'b0 , (T_LINE_if_if_dividend1_sva[18:0])}), (z_out_8[19:0]),
      T_LINE_if_if_dividend1_sva[19]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0
      = MUX_v_20_2_2(({1'b0 , (T_LINE_if_else_dividend1_sva[18:0])}), (z_out_8[19:0]),
      T_LINE_if_else_dividend1_sva[19]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_lpi_3_dfm_mx0
      = MUX_v_28_2_2(({1'b0 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0}),
      z_out_5, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_10_lpi_3_dfm_1
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_9_sva
      | (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0
      = MUX_v_20_2_2(({1'b0 , (T_LINE_if_if_dividend2_sva[18:0])}), (z_out_8[19:0]),
      T_LINE_if_if_dividend2_sva[19]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0
      = MUX_v_20_2_2(({1'b0 , (T_LINE_if_else_dividend2_sva[18:0])}), (z_out_8[19:0]),
      T_LINE_if_else_dividend2_sva[19]);
  assign T_LINE_T_LINE_nor_3 = ~(T_LINE_if_land_lpi_3_dfm | operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs);
  assign T_LINE_and_4 = T_LINE_if_land_lpi_3_dfm & (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs);
  assign or_dcpl_8 = operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
      | (~ T_LINE_if_land_lpi_3_dfm);
  assign or_dcpl_11 = operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
      | T_LINE_if_land_lpi_3_dfm;
  assign and_dcpl_54 = ~((fsm_output[26]) | (fsm_output[0]));
  assign or_dcpl_66 = (fsm_output[12]) | (fsm_output[18]);
  assign or_dcpl_74 = (fsm_output[20]) | (fsm_output[17]);
  assign or_dcpl_75 = (fsm_output[14:13]!=2'b00);
  assign or_dcpl_80 = (fsm_output[11]) | (fsm_output[10]) | (fsm_output[16]) | or_dcpl_66;
  assign nl_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_nl
      = conv_s2u_35_36({(~ ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_1_34_32)
      , (~ ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_1_31_0)})
      + conv_s2u_34_36({reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1
      , 8'b00000000}) + 36'b000000000000000000000000000000000001;
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_nl
      = nl_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_nl[35:0];
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_itm_35_1
      = readslicef_36_1_35(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_acc_3_nl);
  assign or_dcpl_95 = or_dcpl_75 | (fsm_output[15]);
  assign or_dcpl_107 = (fsm_output[16]) | (fsm_output[22]);
  assign or_dcpl_109 = ~((z_out_10[4]) & T_LINE_if_land_lpi_3_dfm);
  assign or_dcpl_110 = (~ (z_out_10[4])) | T_LINE_if_land_lpi_3_dfm;
  assign and_281_cse = (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      & (fsm_output[8]);
  assign and_359_cse = (~ (z_out_1_8_3[0])) & (fsm_output[4]);
  assign and_357_cse = (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_itm_16_1)
      & (fsm_output[7]);
  assign or_tmp_124 = T_LINE_if_land_lpi_3_dfm & (fsm_output[9]);
  assign or_tmp_125 = (~ T_LINE_if_land_lpi_3_dfm) & (fsm_output[9]);
  assign or_tmp_126 = T_LINE_if_land_lpi_3_dfm & (fsm_output[15]);
  assign or_tmp_127 = (~ T_LINE_if_land_lpi_3_dfm) & (fsm_output[15]);
  assign or_tmp_138 = (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      & (fsm_output[21]);
  assign or_tmp_191 = (fsm_output[20:18]!=3'b000);
  assign or_tmp_250 = ~((fsm_output[4]) | (fsm_output[23]) | (fsm_output[17]));
  assign or_tmp_379 = or_dcpl_75 | (fsm_output[20:19]!=2'b00);
  assign asn_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_nor_1_ssc
      = ~(T_LINE_if_land_lpi_3_dfm | (cos_out_1_rsci_data_out_d_oreg[26]));
  assign and_214_ssc = T_LINE_if_land_lpi_3_dfm & (~ (sin_out_1_rsci_data_out_d_oreg[26]));
  assign and_216_ssc = T_LINE_if_land_lpi_3_dfm & (~ (T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva[26]));
  assign asn_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_nor_ssc
      = ~(T_LINE_if_land_lpi_3_dfm | (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva[26]));
  assign cos_out_1_rsci_re_d = {1'b1 , (~ and_357_cse)};
  assign cos_out_1_rsci_we_d = {1'b1 , (~ and_359_cse)};
  assign sin_out_1_rsci_re_d = {1'b1 , (~ and_357_cse)};
  assign sin_out_1_rsci_we_d = {1'b1 , (~ and_359_cse)};
  assign cos_out_1_rsci_data_in_d = z_out_10[32:6];
  assign cos_out_1_rsci_addr_d = T_LINE_t_7_0_sva;
  assign sin_out_1_rsci_data_in_d = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[32:6];
  assign or_cse = (fsm_output[23]) | (fsm_output[17]);
  assign or_569_cse = (fsm_output[20]) | (fsm_output[19]) | (fsm_output[14]) | (fsm_output[13]);
  assign or_tmp_457 = (fsm_output[21]) | (fsm_output[15]);
  assign or_tmp_459 = (fsm_output[16]) | (fsm_output[10]);
  assign or_tmp_461 = (fsm_output[17]) | (fsm_output[11]);
  assign or_tmp_476 = (fsm_output[18]) | (fsm_output[24]);
  assign and_1073_ssc = (~(T_LINE_if_land_lpi_3_dfm & (sin_out_1_rsci_data_out_d_oreg[26])))
      & (fsm_output[9]);
  assign and_1075_ssc = T_LINE_if_land_lpi_3_dfm & (sin_out_1_rsci_data_out_d_oreg[26])
      & (fsm_output[9]);
  assign and_1076_ssc = (T_LINE_if_land_lpi_3_dfm | (~ (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva[26])))
      & (fsm_output[15]);
  assign and_1078_ssc = (~ T_LINE_if_land_lpi_3_dfm) & (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva[26])
      & (fsm_output[15]);
  assign or_621_tmp = (~((fsm_output[9]) | (fsm_output[21]) | (fsm_output[17])))
      | (operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
      & (~ (fsm_output[17]))) | (or_tmp_138 & (~ T_LINE_if_land_lpi_3_dfm)) | x1_t_and_cse;
  assign for_nor_3_itm = ~(or_tmp_476 | or_cse);
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs
          <= 1'b0;
    end
    else if ( core_wen & (~((~ (fsm_output[3])) | (z_out_11[26]))) ) begin
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs
          <= z_out_7[2];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_if_dividend1_sva <= 20'b00000000000000000000;
    end
    else if ( core_wen & (~ or_dcpl_8) & (fsm_output[11]) ) begin
      T_LINE_if_if_dividend1_sva <= z_out_6[43:24];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_else_dividend1_sva <= 20'b00000000000000000000;
    end
    else if ( core_wen & (~ or_dcpl_11) & (fsm_output[11]) ) begin
      T_LINE_if_else_dividend1_sva <= z_out_6[43:24];
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
          <= ~((T_LINE_if_if_dividend1_sva[19]) ^ (T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva[26]));
      operator_27_3_true_AC_TRN_AC_WRAP_return_sva <= ~((T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva!=27'b000000000000000000000000000));
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
          <= ~((T_LINE_if_else_dividend1_sva[19]) ^ (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva[26]));
      operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva <= ~((T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva!=27'b000000000000000000000000000));
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
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_1;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_1;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_1;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_1;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1;
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
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_6_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_5_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0;
      T_LINE_if_else_dividend2_sva <= z_out_6[43:24];
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_4_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_3_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_1;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_2_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_1;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_1_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_1;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_0_lpi_3_dfm
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs
          <= 1'b0;
    end
    else if ( core_wen & (fsm_output[19]) ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs
          <= ~((T_LINE_if_if_dividend2_sva[19]) ^ (T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva[26]));
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs
          <= 1'b0;
    end
    else if ( core_wen & (fsm_output[20]) ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs
          <= ~((T_LINE_if_else_dividend2_sva[19]) ^ (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva[26]));
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_land_lpi_3_dfm <= 1'b0;
    end
    else if ( core_wen & and_281_cse ) begin
      T_LINE_if_land_lpi_3_dfm <= ~((z_out_1_8_3[2]) | (z_out_9[8]));
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
      y2_rsci_idat_0 <= y2_t_15_lpi_3;
      y2_rsci_idat_1 <= y2_t_16_lpi_3;
      y2_rsci_idat_2 <= y2_t_17_lpi_3;
      y2_rsci_idat_3 <= y2_t_18_lpi_3;
      y2_rsci_idat_9_4 <= reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_ftd_1[5:0];
      x2_rsci_idat_0 <= x2_t_15_lpi_3;
      x2_rsci_idat_1 <= x2_t_16_lpi_3;
      x2_rsci_idat_2 <= x2_t_17_lpi_3;
      x2_rsci_idat_10_3 <= reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_ftd_1[7:0];
      y1_rsci_idat_0 <= y1_t_15_lpi_3;
      y1_rsci_idat_1 <= y1_t_16_lpi_3;
      y1_rsci_idat_2 <= y1_t_17_lpi_3;
      y1_rsci_idat_3 <= y1_t_18_lpi_3;
      y1_rsci_idat_9_4 <= y1_t_25_19_lpi_3[5:0];
      x1_rsci_idat_0 <= x1_t_15_lpi_3;
      x1_rsci_idat_1 <= x1_t_16_lpi_3;
      x1_rsci_idat_2 <= x1_t_17_lpi_3;
      x1_rsci_idat_10_3 <= x1_t_26_18_lpi_3[7:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      reg_sin_out_1_rsc_cgo_cse <= 1'b0;
      reg_acc_rsci_irdy_core_psct_cse <= 1'b0;
      reg_y2_rsci_ivld_core_psct_cse <= 1'b0;
      T_LINE_if_if_dividend1_mul_cmp_b <= 27'b000000000000000000000000000;
      threshold_23_8_lpi_3 <= 16'b0000000000000000;
      T_LINE_t_7_0_sva <= 8'b00000000;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_9_sva
          <= 1'b0;
      x1_t_26_18_lpi_3 <= 9'b000000000;
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_ftd_1
          <= 9'b000000000;
      for_acc_7_psp_12_0 <= 13'b0000000000000;
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
          <= 1'b0;
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1
          <= 26'b00000000000000000000000000;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_32_30
          <= 3'b000;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_29
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_28
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_27
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_26_25
          <= 2'b00;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_24
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_23_22
          <= 2'b00;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_21
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_20_18
          <= 3'b000;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_17
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_16
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14
          <= 2'b00;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_13_11
          <= 3'b000;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_10
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_9_8
          <= 2'b00;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_7
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_6_5
          <= 2'b00;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_4
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_3
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_2_1
          <= 2'b00;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_0
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_32_30
          <= 3'b000;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_29
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_28_27
          <= 2'b00;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_26
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_25
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_24
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_23
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_22
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_21
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_20
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_15_14
          <= 2'b00;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11
          <= 3'b000;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_9
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_8
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_7
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_6
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_5
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_4
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_3
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva
          <= 5'b00000;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_1_34_32
          <= 3'b000;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_1_31_0
          <= 32'b00000000000000000000000000000000;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_1_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_2_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_3_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_4_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_5_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_6_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_itm
          <= 1'b0;
      operator_27_12_true_AC_TRN_AC_WRAP_acc_itm <= 10'b0000000000;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva
          <= 27'b000000000000000000000000000;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp
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
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0
          <= 27'b000000000000000000000000000;
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
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm
          <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd <= 9'b000000000;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1 <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_2 <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_3 <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_4 <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_5 <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_6 <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_7 <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_8 <= 1'b0;
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_9 <= 1'b0;
    end
    else if ( core_wen ) begin
      reg_sin_out_1_rsc_cgo_cse <= or_234_rmff;
      reg_acc_rsci_irdy_core_psct_cse <= ((z_out_1_8_3[5]) & (fsm_output[25])) |
          and_58_cse | (T_LINE_slc_T_LINE_acc_6_itm & (fsm_output[24]));
      reg_y2_rsci_ivld_core_psct_cse <= (~ (z_out_1_8_3[5])) & (fsm_output[25]);
      T_LINE_if_if_dividend1_mul_cmp_b <= MUX1HOT_v_27_4_2(cos_out_1_rsci_data_out_d_oreg,
          sin_out_1_rsci_data_out_d_oreg, T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva,
          T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva, {or_tmp_124 , or_tmp_125 ,
          or_tmp_126 , or_tmp_127});
      threshold_23_8_lpi_3 <= MUX1HOT_v_16_3_2(16'b0000000101000100, acc_rsci_idat_mxwt,
          threshold_23_8_lpi_3, {or_248_nl , and_357_cse , or_250_nl});
      T_LINE_t_7_0_sva <= MUX_v_8_2_2(8'b00000000, for_t_mux_nl, nor_nl);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_9_sva
          <= MUX1HOT_s_1_4_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_9_sva,
          T_LINE_if_aelse_T_LINE_if_aelse_and_nl, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva,
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1,
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_nl
          , or_dcpl_66 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_and_3_nl
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_and_1_nl});
      x1_t_26_18_lpi_3 <= MUX1HOT_v_9_4_2((z_out[8:0]), z_out_2, (signext_9_1(~ T_LINE_if_land_lpi_3_dfm)),
          x1_t_26_18_lpi_3, {and_486_nl , x1_t_and_1_cse , and_1106_nl , or_621_tmp});
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_ftd_1
          <= MUX1HOT_v_9_4_2(reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_ftd_1,
          9'b010100010, z_out_2, (z_out_9[8:0]), {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_or_1_nl
          , x2_t_and_nl , x2_t_and_1_nl , x1_t_and_1_cse});
      for_acc_7_psp_12_0 <= MUX_v_13_2_2((z_out_5[12:0]), ({2'b00 , R_LINE_r_R_LINE_r_and_nl}),
          nor_41_nl);
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
          <= MUX1HOT_s_1_3_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_and_nl,
          reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd,
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_nl,
          {(fsm_output[9]) , or_312_ssc , (fsm_output[15])});
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1
          <= MUX1HOT_v_26_7_2((z_out_6[25:0]), reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1,
          (z_out_8[25:0]), (cos_out_1_rsci_data_out_d_oreg[25:0]), (sin_out_1_rsci_data_out_d_oreg[25:0]),
          (T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva[25:0]), (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva[25:0]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_nor_nl
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_nl
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_1_nl
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_4_nl
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_5_nl
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_6_nl
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_7_nl});
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_32_30
          <= MUX_v_3_2_2((signext_3_1(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14_mx0w0[1])),
          (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[32:30]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_29
          <= MUX_s_1_2_2((z_out_11[26]), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[29]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_28
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14_mx0w0[1]),
          (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[28]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_27
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14_mx0w0[1]),
          (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[27]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_26_25
          <= MUX_v_2_2_2((signext_2_1(z_out_11[26])), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[26:25]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_24
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14_mx0w0[1]),
          (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[24]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_23_22
          <= MUX_v_2_2_2((signext_2_1(z_out_11[26])), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[23:22]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_21
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14_mx0w0[1]),
          (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[21]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_20_18
          <= MUX_v_3_2_2((signext_3_1(z_out_11[26])), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[20:18]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_17
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14_mx0w0[1]),
          (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[17]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_16
          <= MUX_s_1_2_2((z_out_11[26]), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[16]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14
          <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14_mx0w0,
          (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[15:14]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_13_11
          <= MUX_v_3_2_2((signext_3_1(z_out_11[26])), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[13:11]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_10
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14_mx0w0[1]),
          (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[10]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_9_8
          <= MUX_v_2_2_2((signext_2_1(z_out_11[26])), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[9:8]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_7
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14_mx0w0[1]),
          (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[7]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_6_5
          <= MUX_v_2_2_2((signext_2_1(z_out_11[26])), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[6:5]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_4
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14_mx0w0[1]),
          (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[4]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_3
          <= MUX_s_1_2_2((z_out_11[26]), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[3]),
          fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_2_1
          <= MUX1HOT_v_2_3_2(2'b01, (signext_2_1(z_out_7[2])), (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[2:1]),
          {or_333_nl , and_568_nl , (fsm_output[4])});
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_0
          <= (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_dfm_mx0[0])
          & (fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_32_30
          <= MUX_v_3_2_2(3'b000, (z_out_10[32:30]), (fsm_output[4]));
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_29
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_10[29]), fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_28_27
          <= MUX_v_2_2_2(2'b00, (z_out_10[28:27]), (fsm_output[4]));
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_26
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_10[26]), fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_25
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_10[25]), fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_24
          <= (z_out_10[24]) & (fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_23
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_10[23]), fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_22
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_10[22]), fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_21
          <= (z_out_10[21]) & (fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_20
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_10[20]), fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_15_14
          <= MUX_v_2_2_2(2'b00, (z_out_10[15:14]), (fsm_output[4]));
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11
          <= MUX_v_3_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0,
          (z_out_10[13:11]), fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_9
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_10[9]), fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_8
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_10[8]), fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_7
          <= (z_out_10[7]) & (fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_6
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_10[6]), fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_5
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_10[5]), fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_4
          <= (z_out_10[4]) & (fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_3
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_10[3]), fsm_output[4]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva
          <= MUX_v_5_2_2(5'b00000, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_2,
          (fsm_output[4]));
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_1_34_32
          <= MUX_v_3_2_2((signext_3_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_nl)),
          (z_out_11[34:32]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_or_itm);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_1_31_0
          <= MUX_v_32_2_2(32'b00000000000000000000000000000000, (z_out_11[31:0]),
          (fsm_output[4]));
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_1_itm
          <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_10[1]), (z_out[14]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1,
          (~ (T_LINE_if_if_dividend1_sva[19])), (~ (T_LINE_if_if_dividend2_sva[19])),
          {(fsm_output[3]) , (fsm_output[4]) , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_6_cse
          , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_7_cse
          , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_30_cse
          , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_31_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_2_itm
          <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux1h_1_nl
          & (~ or_tmp_250);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_3_itm
          <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_10[16]), (z_out[12]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1,
          (~ (T_LINE_if_if_dividend1_sva[19])), (~ (T_LINE_if_if_dividend2_sva[19])),
          {(fsm_output[3]) , (fsm_output[4]) , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_6_cse
          , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_7_cse
          , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_30_cse
          , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_31_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_4_itm
          <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux1h_2_nl
          & (~ or_tmp_250);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_5_itm
          <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_10[18]), (z_out[10]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1,
          (~ (T_LINE_if_if_dividend1_sva[19])), (~ (T_LINE_if_if_dividend2_sva[19])),
          {(fsm_output[3]) , (fsm_output[4]) , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_6_cse
          , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_7_cse
          , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_30_cse
          , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_31_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_6_itm
          <= MUX1HOT_s_1_6_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_10[19]), (z_out[9]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1,
          (~ (T_LINE_if_if_dividend1_sva[19])), (~ (T_LINE_if_if_dividend2_sva[19])),
          {(fsm_output[3]) , (fsm_output[4]) , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_6_cse
          , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_7_cse
          , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_30_cse
          , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_31_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_itm
          <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux1h_3_nl
          & (~ or_tmp_250);
      operator_27_12_true_AC_TRN_AC_WRAP_acc_itm <= MUX1HOT_v_10_3_2(z_out_3, ({2'b00
          , z_out_7}), ({4'b0000 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_and_nl}),
          {operator_27_12_true_AC_TRN_AC_WRAP_operator_27_12_true_AC_TRN_AC_WRAP_nor_nl
          , operator_27_12_true_AC_TRN_AC_WRAP_and_1_nl , or_395_tmp});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva
          <= MUX_v_27_2_2((z_out_6[26:0]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva,
          or_398_nl);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp,
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_T000000,
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_or_cse);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_11_nl
          & (~ or_dcpl_66);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_10_nl
          & (~ or_dcpl_66);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_18_nl
          & (~ or_dcpl_66);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_24_nl
          & (~ or_dcpl_66);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_9_nl
          & (~ or_dcpl_66);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_8_nl
          & (~ or_dcpl_66);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_7_nl
          & (~ or_dcpl_66);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_6_nl
          & (~ or_dcpl_66);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_5_nl
          & (~ or_dcpl_66);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_4_nl
          & (~ or_dcpl_66);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_66_nl
          & (~ or_dcpl_66);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_3_nl
          & (~ or_dcpl_66);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_2_nl
          & (~ or_dcpl_66);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_1_nl
          & (~ or_dcpl_66);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_nl
          & (~ or_dcpl_66);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_nl
          & (~ or_dcpl_66);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0
          <= MUX_v_27_2_2(27'b000000000000000000000000000, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_1_nl,
          not_387_nl);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_19_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[19]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[19]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_18_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[19]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[19]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_18_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[18]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[18]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_17_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[18]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[18]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_17_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[17]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[17]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_16_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[17]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[17]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_16_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[16]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[16]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_15_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[16]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[16]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_15_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[15]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[15]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_14_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[15]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[15]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_14_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[14]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[14]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_13_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[14]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[14]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_13_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[13]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[13]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_12_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[13]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[13]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_12_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[12]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[12]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_11_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[12]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[12]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_11_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[11]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[11]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_10_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[11]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[11]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_10_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[10]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[10]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_9_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[10]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[10]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_9_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[9]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[9]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_8_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[9]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[9]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_8_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[8]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[8]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_7_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[8]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[8]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_7_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[7]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[7]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_6_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[7]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[7]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_6_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[6]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[6]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_5_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[6]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[6]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_5_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[5]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[5]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_4_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[5]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[5]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_4_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[4]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[4]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_3_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[4]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[4]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_3_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[3]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[3]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_2_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[3]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[3]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_2_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[2]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[2]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_1_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[2]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[2]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_1_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[1]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[1]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[1]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[1]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_tmp_379 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_mux1h_79_nl
          & (~ or_tmp_379);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_nl,
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm,
          or_dcpl_107);
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd <= MUX1HOT_v_9_4_2((operator_27_12_true_AC_TRN_AC_WRAP_acc_itm[9:1]),
          (signext_9_8(operator_27_12_true_AC_TRN_AC_WRAP_acc_itm[7:0])), (z_out_3[9:1]),
          ({{1{z_out_7[7]}}, z_out_7}), {or_tmp_124 , or_tmp_125 , or_tmp_126 , or_tmp_127});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1 <= MUX1HOT_s_1_4_2((operator_27_12_true_AC_TRN_AC_WRAP_acc_itm[0]),
          y1_t_18_lpi_3, (z_out_3[0]), y2_t_18_lpi_3, {or_tmp_124 , or_tmp_125 ,
          or_tmp_126 , or_tmp_127});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_2 <= MUX1HOT_s_1_4_2(x1_t_17_lpi_3,
          y1_t_17_lpi_3, x2_t_17_lpi_3, y2_t_17_lpi_3, {or_tmp_124 , or_tmp_125 ,
          or_tmp_126 , or_tmp_127});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_3 <= MUX1HOT_s_1_4_2(x1_t_16_lpi_3,
          y1_t_16_lpi_3, x2_t_16_lpi_3, y2_t_16_lpi_3, {or_tmp_124 , or_tmp_125 ,
          or_tmp_126 , or_tmp_127});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_4 <= MUX1HOT_s_1_4_2(x1_t_15_lpi_3,
          y1_t_15_lpi_3, x2_t_15_lpi_3, y2_t_15_lpi_3, {or_tmp_124 , or_tmp_125 ,
          or_tmp_126 , or_tmp_127});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_5 <= MUX1HOT_s_1_4_2(x1_t_14_lpi_3,
          y1_t_14_lpi_3, x2_t_14_lpi_3, y2_t_14_lpi_3, {or_tmp_124 , or_tmp_125 ,
          or_tmp_126 , or_tmp_127});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_6 <= MUX1HOT_s_1_4_2(x1_t_13_lpi_3,
          y1_t_13_lpi_3, x2_t_13_lpi_3, y2_t_13_lpi_3, {or_tmp_124 , or_tmp_125 ,
          or_tmp_126 , or_tmp_127});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_7 <= MUX1HOT_s_1_4_2(x1_t_12_lpi_3,
          y1_t_12_lpi_3, x2_t_12_lpi_3, y2_t_12_lpi_3, {or_tmp_124 , or_tmp_125 ,
          or_tmp_126 , or_tmp_127});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_8 <= MUX1HOT_s_1_4_2(x1_t_11_lpi_3,
          y1_t_11_lpi_3, x2_t_11_lpi_3, y2_t_11_lpi_3, {or_tmp_124 , or_tmp_125 ,
          or_tmp_126 , or_tmp_127});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_9 <= MUX1HOT_s_1_4_2(x1_t_10_lpi_3,
          y1_t_10_lpi_3, x2_t_10_lpi_3, y2_t_10_lpi_3, {or_tmp_124 , or_tmp_125 ,
          or_tmp_126 , or_tmp_127});
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      x1_t_10_lpi_3 <= 1'b0;
      x1_t_11_lpi_3 <= 1'b0;
      x1_t_12_lpi_3 <= 1'b0;
      x1_t_13_lpi_3 <= 1'b0;
      x1_t_14_lpi_3 <= 1'b0;
      x1_t_15_lpi_3 <= 1'b0;
      x1_t_16_lpi_3 <= 1'b0;
      x1_t_17_lpi_3 <= 1'b0;
      y1_t_10_lpi_3 <= 1'b0;
      y1_t_11_lpi_3 <= 1'b0;
      y1_t_12_lpi_3 <= 1'b0;
      y1_t_13_lpi_3 <= 1'b0;
      y1_t_14_lpi_3 <= 1'b0;
      y1_t_15_lpi_3 <= 1'b0;
      y1_t_16_lpi_3 <= 1'b0;
      y1_t_17_lpi_3 <= 1'b0;
      y1_t_18_lpi_3 <= 1'b0;
    end
    else if ( x1_t_and_2_cse ) begin
      x1_t_10_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_16_nl
          & (~ T_LINE_if_land_lpi_3_dfm);
      x1_t_11_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_15_nl
          & (~ T_LINE_if_land_lpi_3_dfm);
      x1_t_12_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_14_nl
          & (~ T_LINE_if_land_lpi_3_dfm);
      x1_t_13_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_13_nl
          & (~ T_LINE_if_land_lpi_3_dfm);
      x1_t_14_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_12_nl
          & (~ T_LINE_if_land_lpi_3_dfm);
      x1_t_15_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_11_nl
          & (~ T_LINE_if_land_lpi_3_dfm);
      x1_t_16_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_10_nl
          & (~ T_LINE_if_land_lpi_3_dfm);
      x1_t_17_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_9_nl
          & (~ T_LINE_if_land_lpi_3_dfm);
      y1_t_10_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_15_nl
          & T_LINE_if_land_lpi_3_dfm;
      y1_t_11_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_14_nl
          & T_LINE_if_land_lpi_3_dfm;
      y1_t_12_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_13_nl
          & T_LINE_if_land_lpi_3_dfm;
      y1_t_13_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_12_nl
          & T_LINE_if_land_lpi_3_dfm;
      y1_t_14_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_11_nl
          & T_LINE_if_land_lpi_3_dfm;
      y1_t_15_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_10_nl
          & T_LINE_if_land_lpi_3_dfm;
      y1_t_16_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_9_nl
          & T_LINE_if_land_lpi_3_dfm;
      y1_t_17_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_8_nl
          & T_LINE_if_land_lpi_3_dfm;
      y1_t_18_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_7_nl
          & T_LINE_if_land_lpi_3_dfm;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      x2_t_10_lpi_3 <= 1'b0;
      x2_t_11_lpi_3 <= 1'b0;
      x2_t_12_lpi_3 <= 1'b0;
      x2_t_13_lpi_3 <= 1'b0;
      x2_t_14_lpi_3 <= 1'b0;
      x2_t_15_lpi_3 <= 1'b0;
      x2_t_16_lpi_3 <= 1'b0;
      x2_t_17_lpi_3 <= 1'b0;
      y2_t_10_lpi_3 <= 1'b0;
      y2_t_11_lpi_3 <= 1'b0;
      y2_t_12_lpi_3 <= 1'b0;
      y2_t_13_lpi_3 <= 1'b0;
      y2_t_14_lpi_3 <= 1'b0;
      y2_t_15_lpi_3 <= 1'b0;
      y2_t_16_lpi_3 <= 1'b0;
      y2_t_17_lpi_3 <= 1'b0;
      y2_t_18_lpi_3 <= 1'b0;
    end
    else if ( x2_t_and_2_cse ) begin
      x2_t_10_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_16_nl
          & (~ T_LINE_if_land_lpi_3_dfm);
      x2_t_11_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_15_nl
          & (~ T_LINE_if_land_lpi_3_dfm);
      x2_t_12_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_14_nl
          & (~ T_LINE_if_land_lpi_3_dfm);
      x2_t_13_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_13_nl
          & (~ T_LINE_if_land_lpi_3_dfm);
      x2_t_14_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_12_nl
          & (~ T_LINE_if_land_lpi_3_dfm);
      x2_t_15_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_11_nl
          & (~ T_LINE_if_land_lpi_3_dfm);
      x2_t_16_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_10_nl
          & (~ T_LINE_if_land_lpi_3_dfm);
      x2_t_17_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_9_nl
          & (~ T_LINE_if_land_lpi_3_dfm);
      y2_t_10_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_15_nl
          & T_LINE_if_land_lpi_3_dfm;
      y2_t_11_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_14_nl
          & T_LINE_if_land_lpi_3_dfm;
      y2_t_12_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_13_nl
          & T_LINE_if_land_lpi_3_dfm;
      y2_t_13_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_12_nl
          & T_LINE_if_land_lpi_3_dfm;
      y2_t_14_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_11_nl
          & T_LINE_if_land_lpi_3_dfm;
      y2_t_15_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_10_nl
          & T_LINE_if_land_lpi_3_dfm;
      y2_t_16_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_9_nl
          & T_LINE_if_land_lpi_3_dfm;
      y2_t_17_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_8_nl
          & T_LINE_if_land_lpi_3_dfm;
      y2_t_18_lpi_3 <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_7_nl
          & T_LINE_if_land_lpi_3_dfm;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      y1_t_25_19_lpi_3 <= 7'b0000000;
    end
    else if ( mux_nl & (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
        & core_wen ) begin
      y1_t_25_19_lpi_3 <= MUX_v_7_2_2((z_out_9[6:0]), T_LINE_if_T_LINE_if_and_17_nl,
          or_tmp_138);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_ftd_1_0
          <= 2'b00;
    end
    else if ( core_wen & (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_and_2_cse)
        ) begin
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_ftd_1_0
          <= z_out[8:7];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_ftd_1
          <= 7'b0000000;
    end
    else if ( core_wen & (y2_t_and_rgt | y2_t_and_1_rgt | x1_t_and_1_cse) ) begin
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_ftd_1
          <= MUX1HOT_v_7_3_2((z_out_9[6:0]), 7'b0110110, (z_out[6:0]), {y2_t_and_rgt
          , y2_t_and_1_rgt , x1_t_and_1_cse});
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_slc_T_LINE_acc_6_itm <= 1'b0;
    end
    else if ( core_wen & ((fsm_output[3]) | (fsm_output[4]) | (fsm_output[21])) )
        begin
      T_LINE_slc_T_LINE_acc_6_itm <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_10_nl
          & (~ (fsm_output[3]));
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
          <= 1'b0;
    end
    else if ( core_wen & (fsm_output[7]) ) begin
      operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
          <= operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_itm_16_1;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva <= 27'b000000000000000000000000000;
    end
    else if ( core_wen & (~(or_dcpl_80 | or_dcpl_95 | (fsm_output[19]) | (fsm_output[17])))
        ) begin
      T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva <= sin_out_1_rsci_data_out_d_oreg;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva <= 27'b000000000000000000000000000;
    end
    else if ( core_wen & (~(or_dcpl_80 | or_dcpl_95 | or_dcpl_74)) ) begin
      T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva <= cos_out_1_rsci_data_out_d_oreg;
    end
  end
  assign or_248_nl = (fsm_output[26]) | (fsm_output[0]);
  assign or_250_nl = (and_dcpl_54 & (~ (fsm_output[7]))) | (operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_1_itm_16_1
      & (fsm_output[7]));
  assign or_286_nl = (~((~ and_dcpl_54) | (fsm_output[5]) | (fsm_output[6]) | (fsm_output[25])
      | (fsm_output[21]))) | ((z_out_7[6]) & (fsm_output[6]));
  assign for_t_mux_nl = MUX_v_8_2_2((z_out_4[7:0]), T_LINE_t_7_0_sva, or_286_nl);
  assign nor_nl = ~((fsm_output[0]) | (fsm_output[25]) | and_58_cse);
  assign T_LINE_if_aelse_T_LINE_if_aelse_and_nl = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_9_sva
      & or_dcpl_11;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_nl
      = (~((fsm_output[12]) | (fsm_output[18]) | (fsm_output[14]) | (fsm_output[20])
      | (fsm_output[23]))) | (or_dcpl_11 & (fsm_output[23])) | (or_126_tmp & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_1_m1c);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_and_3_nl
      = (~ or_126_tmp) & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_or_1_m1c;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_and_1_nl
      = (~ or_dcpl_11) & (fsm_output[23]);
  assign and_486_nl = (fsm_output[9]) & (~ or_621_tmp);
  assign and_1106_nl = or_tmp_138 & (~ or_621_tmp);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_or_1_nl
      = (((~ (fsm_output[23])) | operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      & ((~((fsm_output[20:17]!=4'b0000))) | x1_t_and_cse | (operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
      & or_tmp_191))) | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_and_2_cse;
  assign x2_t_and_nl = T_LINE_and_4 & (fsm_output[23]);
  assign x2_t_and_1_nl = T_LINE_T_LINE_nor_3 & (fsm_output[23]);
  assign R_LINE_r_mux_nl = MUX_v_11_2_2((for_acc_7_psp_12_0[10:0]), z_out_4, fsm_output[25]);
  assign nor_42_nl = ~((fsm_output[26]) | (fsm_output[1]) | (fsm_output[0]) | (fsm_output[2])
      | (fsm_output[3]) | (fsm_output[5]) | (fsm_output[6]) | (fsm_output[4]));
  assign R_LINE_r_R_LINE_r_and_nl = MUX_v_11_2_2(11'b00000000000, R_LINE_r_mux_nl,
      nor_42_nl);
  assign nor_41_nl = ~((fsm_output[26]) | (fsm_output[1]) | (fsm_output[0]) | (fsm_output[2])
      | (fsm_output[3]) | (fsm_output[5]) | (fsm_output[4]));
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_and_nl
      = (z_out_8[26]) & (~(asn_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_nor_1_ssc
      | and_214_ssc));
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_nl
      = (z_out_8[26]) & (~(and_216_ssc | asn_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_nor_ssc));
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_nor_nl
      = ~(or_309_tmp | (~((fsm_output[4:2]!=3'b000))));
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_nl
      = or_309_tmp | or_312_ssc;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_1_nl
      = ((((~ T_LINE_if_land_lpi_3_dfm) & (cos_out_1_rsci_data_out_d_oreg[26])) |
      (T_LINE_if_land_lpi_3_dfm & (sin_out_1_rsci_data_out_d_oreg[26]))) & (fsm_output[9]))
      | (((T_LINE_if_land_lpi_3_dfm & (T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva[26]))
      | ((~ T_LINE_if_land_lpi_3_dfm) & (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva[26])))
      & (fsm_output[15]));
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_4_nl
      = asn_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_nor_1_ssc
      & (fsm_output[9]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_5_nl
      = and_214_ssc & (fsm_output[9]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_6_nl
      = and_216_ssc & (fsm_output[15]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_7_nl
      = asn_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_nor_ssc
      & (fsm_output[15]);
  assign or_333_nl = (~((fsm_output[4:3]!=2'b00))) | ((z_out_11[26]) & (fsm_output[3]));
  assign and_568_nl = (~ (z_out_11[26])) & (fsm_output[3]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_nl
      = MUX_v_2_2_2((signext_2_1(z_out_7[2])), 2'b01, z_out_11[26]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux1h_1_nl
      = MUX1HOT_s_1_5_2((z_out_10[10]), (z_out[13]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1,
      (~ (T_LINE_if_if_dividend1_sva[19])), (~ (T_LINE_if_if_dividend2_sva[19])),
      {(fsm_output[4]) , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_6_cse
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_7_cse
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_30_cse
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_31_cse});
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux1h_2_nl
      = MUX1HOT_s_1_5_2((z_out_10[17]), (z_out[11]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1,
      (~ (T_LINE_if_if_dividend1_sva[19])), (~ (T_LINE_if_if_dividend2_sva[19])),
      {(fsm_output[4]) , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_6_cse
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_7_cse
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_30_cse
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_31_cse});
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux1h_3_nl
      = MUX1HOT_s_1_5_2((z_out_10[2]), (z_out[15]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1,
      (~ (T_LINE_if_if_dividend1_sva[19])), (~ (T_LINE_if_if_dividend2_sva[19])),
      {(fsm_output[4]) , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_6_cse
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_or_7_cse
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_30_cse
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_and_31_cse});
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_and_nl
      = MUX_v_6_2_2(6'b000000, (z_out_3[5:0]), or_tmp_379);
  assign operator_27_12_true_AC_TRN_AC_WRAP_operator_27_12_true_AC_TRN_AC_WRAP_nor_nl
      = ~(or_147_cse | or_395_tmp);
  assign operator_27_12_true_AC_TRN_AC_WRAP_and_1_nl = or_147_cse & (~ or_395_tmp);
  assign or_398_nl = (fsm_output[11]) | (fsm_output[12]) | (fsm_output[18]) | (fsm_output[14])
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_28_m1c
      | or_dcpl_74;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_T000000
      = reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
      | (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1!=26'b00000000000000000000000000);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_11_nl
      = MUX_s_1_2_2((~ (z_out_6[27])), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_10_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_18_nl
      = MUX1HOT_s_1_3_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva,
      {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_43_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_46_cse});
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_24_nl
      = MUX1HOT_s_1_3_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva,
      {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_43_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_46_cse});
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_9_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_8_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_7_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_6_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_5_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_4_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux1h_66_nl
      = MUX1HOT_s_1_3_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_Q_9_sva,
      {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_43_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_and_46_cse});
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_3_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_2_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_1_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_44_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27,
      (z_out_6[27]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_or_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_1_nl
      = MUX_v_27_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0,
      (z_out_6[26:0]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_or_cse);
  assign not_387_nl = ~ or_dcpl_66;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_mux1h_79_nl
      = MUX1HOT_s_1_4_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[0]),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[0]),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[0]),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[0]),
      {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_nl
      = ~((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_lpi_3_dfm_mx0!=28'b0000000000000000000000000000));
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_16_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_ftd_1[0]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_0_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_16_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_16_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_15_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_ftd_1[1]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_1_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_15_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_15_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_14_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_ftd_1[2]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_2_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_14_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_14_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_13_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_ftd_1[3]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_3_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_13_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_13_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_12_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_ftd_1[4]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_4_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_12_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_12_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_11_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_ftd_1[5]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_5_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_11_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_11_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_10_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_ftd_1[6]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_6_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_10_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_10_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_9_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_ftd_1[7]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_7_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_9_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_9_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_15_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_ftd_1[0]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_15_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_15_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_14_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_ftd_1[1]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_14_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_14_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_13_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_ftd_1[2]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_13_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_13_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_12_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_ftd_1[3]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_12_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_12_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_11_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_ftd_1[4]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_11_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_11_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_10_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_ftd_1[5]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_10_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_10_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_9_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_ftd_1[6]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_9_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_9_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_8_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_ftd_1_0[0]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_8_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_8_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_7_nl
      = MUX_s_1_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_ftd_1_0[1]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_mux_7_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_7_nl,
      (~ (T_LINE_if_if_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_16_nl
      = MUX_s_1_2_2((z_out_9[0]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_16_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_16_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_15_nl
      = MUX_s_1_2_2((z_out_9[1]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_15_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_15_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_14_nl
      = MUX_s_1_2_2((z_out_9[2]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_14_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_14_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_13_nl
      = MUX_s_1_2_2((z_out_9[3]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_13_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_13_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_12_nl
      = MUX_s_1_2_2((z_out_9[4]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_12_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_12_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_11_nl
      = MUX_s_1_2_2((z_out_9[5]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_11_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_11_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_10_nl
      = MUX_s_1_2_2((z_out_9[6]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_10_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_10_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_9_nl
      = MUX_s_1_2_2((z_out_9[7]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_9_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_mux_9_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_15_nl
      = MUX_s_1_2_2((z_out[0]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_15_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_15_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_14_nl
      = MUX_s_1_2_2((z_out[1]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_14_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_14_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_13_nl
      = MUX_s_1_2_2((z_out[2]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_13_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_13_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_12_nl
      = MUX_s_1_2_2((z_out[3]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_12_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_12_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_11_nl
      = MUX_s_1_2_2((z_out[4]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_11_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_11_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_nl
      = MUX_s_1_2_2((z_out[5]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_10_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_nl
      = MUX_s_1_2_2((z_out[6]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_9_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_nl
      = MUX_s_1_2_2((z_out[7]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_8_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_nl
      = MUX_s_1_2_2((z_out[8]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_7_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_nl,
      (~ (T_LINE_if_if_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_sva);
  assign T_LINE_if_T_LINE_if_and_17_nl = MUX_v_7_2_2(7'b0000000, y1_t_25_19_lpi_3,
      T_LINE_if_land_lpi_3_dfm);
  assign mux_nl = MUX_s_1_2_2((fsm_output[18]), (~ T_LINE_if_land_lpi_3_dfm), fsm_output[21]);
  assign nl_T_LINE_acc_nl = conv_u2s_6_7(z_out_4[7:2]) + 7'b1010011;
  assign T_LINE_acc_nl = nl_T_LINE_acc_nl[6:0];
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_10_nl
      = MUX_s_1_2_2((z_out_10[0]), (readslicef_7_1_6(T_LINE_acc_nl)), fsm_output[21]);
  assign operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_7_nl
      = (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1)
      & or_cse;
  assign operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_8_nl
      = (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1)
      & or_cse;
  assign operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_9_nl
      = (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1)
      & or_cse;
  assign operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_10_nl
      = (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1)
      & or_cse;
  assign operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_11_nl
      = (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1)
      & or_cse;
  assign operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_12_nl
      = (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1)
      & or_cse;
  assign operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_13_nl
      = (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1)
      & or_cse;
  assign operator_28_13_true_AC_TRN_AC_WRAP_mux_10_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[10]),
      (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0),
      or_cse);
  assign operator_28_13_true_AC_TRN_AC_WRAP_mux_11_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[9]),
      (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0),
      or_cse);
  assign operator_28_13_true_AC_TRN_AC_WRAP_mux_12_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[8]),
      (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0),
      or_cse);
  assign operator_28_13_true_AC_TRN_AC_WRAP_mux_13_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[7]),
      (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0),
      or_cse);
  assign operator_28_13_true_AC_TRN_AC_WRAP_mux_14_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[6]),
      (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_1),
      or_cse);
  assign operator_28_13_true_AC_TRN_AC_WRAP_mux_15_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[5]),
      (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_1),
      or_cse);
  assign operator_28_13_true_AC_TRN_AC_WRAP_mux_16_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[4]),
      (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_1),
      or_cse);
  assign operator_28_13_true_AC_TRN_AC_WRAP_mux_17_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[3]),
      (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_1),
      or_cse);
  assign operator_28_13_true_AC_TRN_AC_WRAP_mux_18_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[2]),
      (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1),
      or_cse);
  assign operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_or_1_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm
      | (~ or_cse);
  assign nl_z_out = ({operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_7_nl
      , operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_8_nl
      , operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_9_nl
      , operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_10_nl
      , operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_11_nl
      , operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_12_nl
      , operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_and_13_nl
      , operator_28_13_true_AC_TRN_AC_WRAP_mux_10_nl , operator_28_13_true_AC_TRN_AC_WRAP_mux_11_nl
      , operator_28_13_true_AC_TRN_AC_WRAP_mux_12_nl , operator_28_13_true_AC_TRN_AC_WRAP_mux_13_nl
      , operator_28_13_true_AC_TRN_AC_WRAP_mux_14_nl , operator_28_13_true_AC_TRN_AC_WRAP_mux_15_nl
      , operator_28_13_true_AC_TRN_AC_WRAP_mux_16_nl , operator_28_13_true_AC_TRN_AC_WRAP_mux_17_nl
      , operator_28_13_true_AC_TRN_AC_WRAP_mux_18_nl}) + conv_u2u_9_16({(~ or_cse)
      , 3'b000 , (~ or_cse) , (~ or_cse) , 1'b0 , (~ or_cse) , operator_28_13_true_AC_TRN_AC_WRAP_operator_28_13_true_AC_TRN_AC_WRAP_or_1_nl});
  assign z_out = nl_z_out[15:0];
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_or_2_nl
      = (~((fsm_output[4]) | (fsm_output[8]))) | (fsm_output[25]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_or_3_nl
      = (~ (fsm_output[4])) | (fsm_output[25]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux1h_2_nl
      = MUX1HOT_v_8_3_2(({5'b11111 , (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_2[4:2])}),
      (z_out_4[10:3]), ({3'b111 , (~ (T_LINE_t_7_0_sva[7:3]))}), {(fsm_output[4])
      , (fsm_output[25]) , (fsm_output[8])});
  assign nl_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl
      = ({ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_or_2_nl
      , 3'b000 , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_or_3_nl
      , (fsm_output[25]) , 1'b0 , (fsm_output[25]) , 1'b1}) + conv_u2u_8_9(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux1h_2_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl
      = nl_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl[8:0];
  assign z_out_1_8_3 = readslicef_9_6_3(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_acc_nl);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_42_nl
      = MUX_s_1_2_2((z_out_9[16]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_17_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_42_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_43_nl
      = MUX_s_1_2_2((z_out_9[16]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_17_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_43_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_mux_9_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_17_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_17_nl,
      fsm_output[23]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_27_nl
      = MUX_s_1_2_2((z_out_9[15]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_18_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_27_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_28_nl
      = MUX_s_1_2_2((z_out_9[15]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_18_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_mux_28_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_mux_10_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_18_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_18_nl,
      fsm_output[23]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_44_nl
      = MUX_s_1_2_2((z_out_9[14]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_19_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_44_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_45_nl
      = MUX_s_1_2_2((z_out_9[14]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_19_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_45_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_mux_11_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_19_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_19_nl,
      fsm_output[23]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_46_nl
      = MUX_s_1_2_2((z_out_9[13]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_20_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_46_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_47_nl
      = MUX_s_1_2_2((z_out_9[13]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_20_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_47_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_mux_12_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_20_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_20_nl,
      fsm_output[23]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_48_nl
      = MUX_s_1_2_2((z_out_9[12]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_21_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_48_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_49_nl
      = MUX_s_1_2_2((z_out_9[12]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_21_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_49_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_mux_13_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_21_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_21_nl,
      fsm_output[23]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_50_nl
      = MUX_s_1_2_2((z_out_9[11]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_22_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_50_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_51_nl
      = MUX_s_1_2_2((z_out_9[11]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_22_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_51_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_mux_14_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_22_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_22_nl,
      fsm_output[23]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_52_nl
      = MUX_s_1_2_2((z_out_9[10]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_10_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_23_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_52_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_53_nl
      = MUX_s_1_2_2((z_out_9[10]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_10_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_23_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_53_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_mux_15_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_23_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_23_nl,
      fsm_output[23]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_54_nl
      = MUX_s_1_2_2((z_out_9[9]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_24_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_54_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_55_nl
      = MUX_s_1_2_2((z_out_9[9]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_24_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_55_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_mux_16_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_24_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_24_nl,
      fsm_output[23]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_56_nl
      = MUX_s_1_2_2((z_out_9[8]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_25_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_56_nl,
      (~ (T_LINE_if_else_dividend1_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_57_nl
      = MUX_s_1_2_2((z_out_9[8]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_25_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_57_nl,
      (~ (T_LINE_if_else_dividend2_sva[19])), operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva);
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_mux_17_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_mux_25_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_mux_25_nl,
      fsm_output[23]);
  assign nl_z_out_2 = ({operator_20_15_true_AC_TRN_AC_WRAP_2_mux_9_nl , operator_20_15_true_AC_TRN_AC_WRAP_2_mux_10_nl
      , operator_20_15_true_AC_TRN_AC_WRAP_2_mux_11_nl , operator_20_15_true_AC_TRN_AC_WRAP_2_mux_12_nl
      , operator_20_15_true_AC_TRN_AC_WRAP_2_mux_13_nl , operator_20_15_true_AC_TRN_AC_WRAP_2_mux_14_nl
      , operator_20_15_true_AC_TRN_AC_WRAP_2_mux_15_nl , operator_20_15_true_AC_TRN_AC_WRAP_2_mux_16_nl
      , operator_20_15_true_AC_TRN_AC_WRAP_2_mux_17_nl}) + 9'b001010001;
  assign z_out_2 = nl_z_out_2[8:0];
  assign operator_27_12_true_AC_TRN_AC_WRAP_mux1h_4_nl = MUX1HOT_v_9_3_2(x1_t_26_18_lpi_3,
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_else_ac_fixed_cctor_17_1_ftd_1,
      (signext_9_6(operator_27_12_true_AC_TRN_AC_WRAP_acc_itm[5:0])), {(fsm_output[8])
      , (fsm_output[15]) , or_569_cse});
  assign nl_z_out_3 = conv_s2u_9_10(operator_27_12_true_AC_TRN_AC_WRAP_mux1h_4_nl)
      + conv_s2u_8_10({(~ or_569_cse) , 1'b0 , (~ or_569_cse) , 1'b0 , (signext_3_1(~
      or_569_cse)) , 1'b1});
  assign z_out_3 = nl_z_out_3[9:0];
  assign for_mux_25_nl = MUX_v_11_2_2(({{3{T_LINE_t_7_0_sva[7]}}, T_LINE_t_7_0_sva}),
      (for_acc_7_psp_12_0[10:0]), fsm_output[25]);
  assign nl_z_out_4 = for_mux_25_nl + 11'b00000000001;
  assign z_out_4 = nl_z_out_4[10:0];
  assign not_401_nl = ~ or_tmp_457;
  assign for_for_or_19_nl = MUX_v_19_2_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0[26:8]),
      19'b1111111111111111111, not_401_nl);
  assign for_mux_26_nl = MUX_v_8_2_2((~ T_LINE_t_7_0_sva), (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0[7:0]),
      or_tmp_457);
  assign for_or_2_nl = (~ or_tmp_457) | (fsm_output[1]);
  assign for_for_and_7_nl = reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
      & or_tmp_457;
  assign for_for_and_8_nl = MUX_v_13_2_2(13'b0000000000000, (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[25:13]),
      or_tmp_457);
  assign for_mux_27_nl = MUX_v_8_2_2((~ T_LINE_t_7_0_sva), (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[12:5]),
      or_tmp_457);
  assign not_403_nl = ~ or_tmp_457;
  assign for_for_or_20_nl = MUX_v_4_2_2((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[4:1]),
      4'b1111, not_403_nl);
  assign for_mux_28_nl = MUX_s_1_2_2((~ (T_LINE_t_7_0_sva[7])), (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[0]),
      or_tmp_457);
  assign nl_acc_5_nl = ({1'b1 , for_for_or_19_nl , for_mux_26_nl , for_or_2_nl})
      + conv_u2u_28_29({for_for_and_7_nl , for_for_and_8_nl , for_mux_27_nl , for_for_or_20_nl
      , for_mux_28_nl , 1'b1});
  assign acc_5_nl = nl_acc_5_nl[28:0];
  assign z_out_5 = readslicef_29_28_1(acc_5_nl);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_and_3_nl
      = (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva[26])
      & (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_mux_4_nl
      = MUX_v_27_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva,
      ({reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
      , reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1}),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27);
  assign for_mux1h_6_nl = MUX1HOT_v_44_5_2(({18'b000000000000000000 , (T_LINE_t_7_0_sva[6:0])
      , 6'b000000 , T_LINE_t_7_0_sva , 5'b00000}), 44'b00000000000000000000000000000000000000000001,
      ({{11{ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_1_sva_1[32]}},
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_1_sva_1}),
      (~ T_LINE_if_if_dividend1_mul_cmp_z_oreg), (signext_44_28({ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_and_3_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_mux_4_nl})),
      {(fsm_output[2]) , or_tmp_459 , (fsm_output[4]) , or_tmp_461 , or_569_cse});
  assign for_mux1h_7_nl = MUX1HOT_v_40_5_2(({16'b0000000000000000 , z_out_9}), ({13'b0000000000000
      , (~ reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd)
      , (~ reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1)}),
      (signext_40_33({ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_32_30
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_29
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_28
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_27
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_26_25
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_24
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_23_22
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_21
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_20_18
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_17
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_16
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_15_14
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_13_11
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_10
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_9_8
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_7
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_6_5
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_4
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_3
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_2_1
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_2_dfm_1_0})),
      ({x1_t_26_18_lpi_3 , (for_acc_7_psp_12_0[1:0]) , 29'b00000000000000000000000000001}),
      (signext_40_28({ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_19_sva})),
      {(fsm_output[2]) , or_tmp_459 , (fsm_output[4]) , or_tmp_461 , or_569_cse});
  assign nl_z_out_6 = for_mux1h_6_nl + conv_s2u_40_44(for_mux1h_7_nl);
  assign z_out_6 = nl_z_out_6[43:0];
  assign operator_26_11_true_AC_TRN_AC_WRAP_mux_nl = MUX_v_2_2_2(2'b01, 2'b10, fsm_output[6]);
  assign operator_26_11_true_AC_TRN_AC_WRAP_operator_26_11_true_AC_TRN_AC_WRAP_operator_26_11_true_AC_TRN_AC_WRAP_nor_2_cse
      = ~(MUX_v_2_2_2(operator_26_11_true_AC_TRN_AC_WRAP_mux_nl, 2'b11, (fsm_output[3])));
  assign operator_26_11_true_AC_TRN_AC_WRAP_mux1h_2_nl = MUX1HOT_v_7_4_2(y1_t_25_19_lpi_3,
      (signext_7_2(reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[25:24])),
      ({1'b0 , (T_LINE_t_7_0_sva[7:2])}), reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_16_1_ftd_1,
      {(fsm_output[8]) , (fsm_output[3]) , (fsm_output[6]) , (fsm_output[15])});
  assign nl_z_out_7 = conv_s2u_7_8(operator_26_11_true_AC_TRN_AC_WRAP_mux1h_2_nl)
      + conv_s2u_7_8({(~ (fsm_output[3])) , operator_26_11_true_AC_TRN_AC_WRAP_operator_26_11_true_AC_TRN_AC_WRAP_operator_26_11_true_AC_TRN_AC_WRAP_nor_2_cse
      , 1'b0 , operator_26_11_true_AC_TRN_AC_WRAP_operator_26_11_true_AC_TRN_AC_WRAP_operator_26_11_true_AC_TRN_AC_WRAP_nor_2_cse
      , 1'b1});
  assign z_out_7 = nl_z_out_7[7:0];
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_mux1h_2_nl
      = MUX1HOT_v_7_4_2((cos_out_1_rsci_data_out_d_oreg[25:19]), (sin_out_1_rsci_data_out_d_oreg[25:19]),
      (T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva[25:19]), (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva[25:19]),
      {and_1073_ssc , and_1075_ssc , and_1076_ssc , and_1078_ssc});
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_or_1_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_nor_1_nl
      = ~(MUX_v_7_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_mux1h_2_nl,
      7'b1111111, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_or_1_nl));
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_mux1h_3_nl
      = MUX1HOT_v_19_8_2((~ (T_LINE_if_if_dividend1_sva[18:0])), (~ (T_LINE_if_else_dividend1_sva[18:0])),
      (~ (T_LINE_if_if_dividend2_sva[18:0])), (~ (T_LINE_if_else_dividend2_sva[18:0])),
      (~ (cos_out_1_rsci_data_out_d_oreg[18:0])), (~ (sin_out_1_rsci_data_out_d_oreg[18:0])),
      (~ (T_LINE_if_if_slc_sin_out_1_27_26_0_ncse_sva[18:0])), (~ (T_LINE_if_if_dividend1_slc_cos_out_1_27_26_0_ncse_sva[18:0])),
      {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      , and_1073_ssc , and_1075_ssc , and_1076_ssc , and_1078_ssc});
  assign nl_z_out_8 = conv_u2u_26_27({ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_nor_1_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qif_mux1h_3_nl})
      + 27'b000000000000000000000000001;
  assign z_out_8 = nl_z_out_8[26:0];
  assign for_mux_29_nl = MUX_v_8_2_2(T_LINE_t_7_0_sva, ({{7{ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_itm}},
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_itm}),
      or_tmp_476);
  assign for_nor_7_nl = ~(or_cse | (fsm_output[8]));
  assign for_for_and_9_nl = MUX_v_8_2_2(8'b00000000, for_mux_29_nl, for_nor_7_nl);
  assign for_for_and_10_nl = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_itm
      & (~((fsm_output[2]) | or_cse | (fsm_output[8])));
  assign for_mux1h_8_nl = MUX1HOT_v_8_3_2(T_LINE_t_7_0_sva, (signext_8_5({ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_1_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_2_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_3_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_4_itm})),
      ({2'b00 , (T_LINE_t_7_0_sva[7:2])}), {(fsm_output[2]) , or_tmp_476 , (fsm_output[8])});
  assign not_404_nl = ~ or_cse;
  assign for_and_14_nl = MUX_v_8_2_2(8'b00000000, for_mux1h_8_nl, not_404_nl);
  assign for_mux_30_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_5_itm,
      (T_LINE_t_7_0_sva[1]), fsm_output[8]);
  assign for_for_and_11_nl = for_mux_30_nl & (~((fsm_output[2]) | or_cse));
  assign for_mux1h_9_nl = MUX1HOT_s_1_3_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_6_itm,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm,
      (T_LINE_t_7_0_sva[0]), {or_tmp_476 , or_cse , (fsm_output[8])});
  assign for_or_3_nl = for_mux1h_9_nl | (fsm_output[2]);
  assign for_for_or_21_nl = for_nor_3_itm | (fsm_output[2]) | (fsm_output[8]);
  assign for_and_15_nl = MUX_v_3_2_2(3'b000, (for_acc_7_psp_12_0[12:10]), for_nor_3_itm);
  assign for_for_or_22_nl = MUX_v_3_2_2(for_and_15_nl, 3'b111, (fsm_output[8]));
  assign for_mux_31_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[9]), (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_3_lpi_3_dfm_1),
      or_cse);
  assign for_for_or_23_nl = (for_mux_31_nl & (~ or_tmp_476)) | (fsm_output[8]);
  assign for_mux_32_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[8]), (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1),
      or_cse);
  assign for_for_or_24_nl = (for_mux_32_nl & (~ or_tmp_476)) | (fsm_output[8]);
  assign for_mux_33_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[7]), (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1),
      or_cse);
  assign for_for_or_25_nl = (for_mux_33_nl & (~ or_tmp_476)) | (fsm_output[8]);
  assign for_mux_34_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[6]), (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1),
      or_cse);
  assign for_for_or_26_nl = (for_mux_34_nl & (~ or_tmp_476)) | (fsm_output[8]);
  assign for_mux_35_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[5]), (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1),
      or_cse);
  assign for_for_or_27_nl = (for_mux_35_nl & (~ or_tmp_476)) | (fsm_output[8]);
  assign for_mux_36_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[4]), (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1),
      or_cse);
  assign for_for_or_28_nl = (for_mux_36_nl & (~ or_tmp_476)) | (fsm_output[8]);
  assign for_mux_37_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[3]), (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_10_lpi_3_dfm_1),
      or_cse);
  assign for_for_or_29_nl = (for_mux_37_nl & (~ or_tmp_476)) | (fsm_output[8]);
  assign for_mux_38_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[2]), (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1),
      or_cse);
  assign for_for_or_30_nl = (for_mux_38_nl & (~ or_tmp_476)) | (fsm_output[8]);
  assign for_mux_39_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[1]), (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1),
      or_cse);
  assign for_for_or_31_nl = (for_mux_39_nl & (~ or_tmp_476)) | (fsm_output[8]);
  assign for_mux_40_nl = MUX_s_1_2_2((for_acc_7_psp_12_0[0]), (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0),
      or_cse);
  assign for_for_or_32_nl = (for_mux_40_nl & (~ or_tmp_476)) | (fsm_output[8]);
  assign for_mux_41_nl = MUX_s_1_2_2((T_LINE_t_7_0_sva[6]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_7_lpi_3_dfm_mx1w0,
      or_cse);
  assign for_for_or_33_nl = (~(for_mux_41_nl | or_tmp_476)) | (fsm_output[8]);
  assign for_mux_42_nl = MUX_s_1_2_2((T_LINE_t_7_0_sva[5]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_6_lpi_3_dfm_mx1w0,
      or_cse);
  assign for_for_for_nor_2_nl = ~(for_mux_42_nl | or_tmp_476 | (fsm_output[8]));
  assign for_mux_43_nl = MUX_s_1_2_2((~ (T_LINE_t_7_0_sva[4])), (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_5_lpi_3_dfm_mx1w0),
      or_cse);
  assign for_for_or_34_nl = for_mux_43_nl | or_tmp_476 | (fsm_output[8]);
  assign for_mux_44_nl = MUX_s_1_2_2((T_LINE_t_7_0_sva[3]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_4_lpi_3_dfm_1,
      or_cse);
  assign for_for_or_35_nl = (~(for_mux_44_nl | (fsm_output[8]))) | or_tmp_476;
  assign for_mux_45_nl = MUX_s_1_2_2((T_LINE_t_7_0_sva[2]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_2_lpi_3_dfm_1,
      or_cse);
  assign for_for_for_nor_3_nl = ~(for_mux_45_nl | or_tmp_476 | (fsm_output[8]));
  assign for_mux_46_nl = MUX_s_1_2_2((~ (T_LINE_t_7_0_sva[1])), (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_1_lpi_3_dfm_1),
      or_cse);
  assign for_for_or_36_nl = for_mux_46_nl | or_tmp_476 | (fsm_output[8]);
  assign for_mux_47_nl = MUX_s_1_2_2((~ (T_LINE_t_7_0_sva[0])), (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_0_lpi_3_dfm_1),
      or_cse);
  assign for_for_or_37_nl = for_mux_47_nl | or_tmp_476 | (fsm_output[8]);
  assign nl_z_out_9 = ({for_for_and_9_nl , (signext_6_1(for_for_and_10_nl)) , for_and_14_nl
      , for_for_and_11_nl , for_or_3_nl}) + conv_s2u_21_24({for_for_or_21_nl , for_for_or_22_nl
      , for_for_or_23_nl , for_for_or_24_nl , for_for_or_25_nl , for_for_or_26_nl
      , for_for_or_27_nl , for_for_or_28_nl , for_for_or_29_nl , for_for_or_30_nl
      , for_for_or_31_nl , for_for_or_32_nl , for_for_or_33_nl , for_for_for_nor_2_nl
      , for_for_or_34_nl , for_for_or_35_nl , for_for_for_nor_3_nl , for_for_or_36_nl
      , for_for_or_37_nl});
  assign z_out_9 = nl_z_out_9[23:0];
  assign not_415_nl = ~ or_569_cse;
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_23_nl
      = MUX_v_3_2_2(3'b000, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_32_30,
      not_415_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_24_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_29
      & (~ or_569_cse);
  assign not_417_nl = ~ or_569_cse;
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_25_nl
      = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_28_27,
      not_417_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_26_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_26
      & (~ or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_27_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_25
      & (~ or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_28_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_24
      & (~ or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_29_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_23
      & (~ or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_30_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_22
      & (~ or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_31_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_21
      & (~ or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_32_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_20
      & (~ or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_33_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_6_itm
      & (~ or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_34_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_5_itm
      & (~ or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_35_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_4_itm
      & (~ or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_36_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_3_itm
      & (~ or_569_cse);
  assign not_429_nl = ~ or_569_cse;
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_37_nl
      = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_15_14,
      not_429_nl);
  assign not_430_nl = ~ or_569_cse;
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_38_nl
      = MUX_v_3_2_2(3'b000, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_13_11,
      not_430_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_39_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_2_itm
      & (~ or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_40_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_9
      & (~ or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_41_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_8
      & (~ or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_42_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_7
      & (~ or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_43_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_6
      & (~ or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_44_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_5
      & (~ or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_45_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_4
      & (~ or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_4_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_2_dfm_1_3,
      (z_out_3[5]), or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_5_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_itm,
      (z_out_3[4]), or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_6_nl
      = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_mux_1_itm,
      (z_out_3[3]), or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_7_nl
      = MUX_s_1_2_2(T_LINE_slc_T_LINE_acc_6_itm, (z_out_3[2]), or_569_cse);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_5_nl
      = (~(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_and_1_cse
      | or_569_cse)) | ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_and_cse;
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux1h_5_nl
      = MUX1HOT_v_33_3_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_sva_1,
      (~ ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_sva_1),
      33'b111111111111111111111111111110101, {ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_and_1_cse
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_and_cse
      , or_569_cse});
  assign nl_acc_10_nl = ({ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_23_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_24_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_25_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_26_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_27_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_28_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_29_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_30_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_31_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_32_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_33_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_34_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_35_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_36_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_37_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_38_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_39_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_40_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_41_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_42_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_43_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_44_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_and_45_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_4_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_5_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_6_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_7_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_5_nl})
      + ({ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux1h_5_nl
      , 1'b1});
  assign acc_10_nl = nl_acc_10_nl[33:0];
  assign z_out_10 = readslicef_34_33_1(acc_10_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_not_2_nl
      = ~ (fsm_output[3]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_and_1_nl
      = MUX_v_3_2_2(3'b000, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_1_34_32,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_not_2_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_mux_1_nl
      = MUX_v_32_2_2(32'b00000001000000000000000000000001, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_lpi_2_dfm_1_31_0,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_or_itm);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_or_2_nl
      = (~((fsm_output[3]) | ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_and_cse))
      | ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_and_1_cse;
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_or_1_nl
      = (~((reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[25])
      | ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_and_cse))
      | ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_and_1_cse;
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_mux1h_2_nl
      = MUX1HOT_v_32_3_2((signext_32_26(~ reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1)),
      (~ ac_math_atan_pi_2mi_return_1_69_38_sva_1), ac_math_atan_pi_2mi_return_1_69_38_sva_1,
      {(fsm_output[3]) , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_and_1_cse
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_and_cse});
  assign nl_acc_11_nl = ({ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_and_1_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_mux_1_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_or_2_nl})
      + conv_s2u_34_36({ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_or_1_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_mux1h_2_nl
      , 1'b1});
  assign acc_11_nl = nl_acc_11_nl[35:0];
  assign z_out_11 = readslicef_36_35_1(acc_11_nl);

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


  function automatic [9:0] MUX1HOT_v_10_3_2;
    input [9:0] input_2;
    input [9:0] input_1;
    input [9:0] input_0;
    input [2:0] sel;
    reg [9:0] result;
  begin
    result = input_0 & {10{sel[0]}};
    result = result | ( input_1 & {10{sel[1]}});
    result = result | ( input_2 & {10{sel[2]}});
    MUX1HOT_v_10_3_2 = result;
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


  function automatic [18:0] MUX1HOT_v_19_8_2;
    input [18:0] input_7;
    input [18:0] input_6;
    input [18:0] input_5;
    input [18:0] input_4;
    input [18:0] input_3;
    input [18:0] input_2;
    input [18:0] input_1;
    input [18:0] input_0;
    input [7:0] sel;
    reg [18:0] result;
  begin
    result = input_0 & {19{sel[0]}};
    result = result | ( input_1 & {19{sel[1]}});
    result = result | ( input_2 & {19{sel[2]}});
    result = result | ( input_3 & {19{sel[3]}});
    result = result | ( input_4 & {19{sel[4]}});
    result = result | ( input_5 & {19{sel[5]}});
    result = result | ( input_6 & {19{sel[6]}});
    result = result | ( input_7 & {19{sel[7]}});
    MUX1HOT_v_19_8_2 = result;
  end
  endfunction


  function automatic [25:0] MUX1HOT_v_26_7_2;
    input [25:0] input_6;
    input [25:0] input_5;
    input [25:0] input_4;
    input [25:0] input_3;
    input [25:0] input_2;
    input [25:0] input_1;
    input [25:0] input_0;
    input [6:0] sel;
    reg [25:0] result;
  begin
    result = input_0 & {26{sel[0]}};
    result = result | ( input_1 & {26{sel[1]}});
    result = result | ( input_2 & {26{sel[2]}});
    result = result | ( input_3 & {26{sel[3]}});
    result = result | ( input_4 & {26{sel[4]}});
    result = result | ( input_5 & {26{sel[5]}});
    result = result | ( input_6 & {26{sel[6]}});
    MUX1HOT_v_26_7_2 = result;
  end
  endfunction


  function automatic [26:0] MUX1HOT_v_27_4_2;
    input [26:0] input_3;
    input [26:0] input_2;
    input [26:0] input_1;
    input [26:0] input_0;
    input [3:0] sel;
    reg [26:0] result;
  begin
    result = input_0 & {27{sel[0]}};
    result = result | ( input_1 & {27{sel[1]}});
    result = result | ( input_2 & {27{sel[2]}});
    result = result | ( input_3 & {27{sel[3]}});
    MUX1HOT_v_27_4_2 = result;
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


  function automatic [31:0] MUX1HOT_v_32_3_2;
    input [31:0] input_2;
    input [31:0] input_1;
    input [31:0] input_0;
    input [2:0] sel;
    reg [31:0] result;
  begin
    result = input_0 & {32{sel[0]}};
    result = result | ( input_1 & {32{sel[1]}});
    result = result | ( input_2 & {32{sel[2]}});
    MUX1HOT_v_32_3_2 = result;
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


  function automatic [39:0] MUX1HOT_v_40_5_2;
    input [39:0] input_4;
    input [39:0] input_3;
    input [39:0] input_2;
    input [39:0] input_1;
    input [39:0] input_0;
    input [4:0] sel;
    reg [39:0] result;
  begin
    result = input_0 & {40{sel[0]}};
    result = result | ( input_1 & {40{sel[1]}});
    result = result | ( input_2 & {40{sel[2]}});
    result = result | ( input_3 & {40{sel[3]}});
    result = result | ( input_4 & {40{sel[4]}});
    MUX1HOT_v_40_5_2 = result;
  end
  endfunction


  function automatic [43:0] MUX1HOT_v_44_5_2;
    input [43:0] input_4;
    input [43:0] input_3;
    input [43:0] input_2;
    input [43:0] input_1;
    input [43:0] input_0;
    input [4:0] sel;
    reg [43:0] result;
  begin
    result = input_0 & {44{sel[0]}};
    result = result | ( input_1 & {44{sel[1]}});
    result = result | ( input_2 & {44{sel[2]}});
    result = result | ( input_3 & {44{sel[3]}});
    result = result | ( input_4 & {44{sel[4]}});
    MUX1HOT_v_44_5_2 = result;
  end
  endfunction


  function automatic [6:0] MUX1HOT_v_7_3_2;
    input [6:0] input_2;
    input [6:0] input_1;
    input [6:0] input_0;
    input [2:0] sel;
    reg [6:0] result;
  begin
    result = input_0 & {7{sel[0]}};
    result = result | ( input_1 & {7{sel[1]}});
    result = result | ( input_2 & {7{sel[2]}});
    MUX1HOT_v_7_3_2 = result;
  end
  endfunction


  function automatic [6:0] MUX1HOT_v_7_4_2;
    input [6:0] input_3;
    input [6:0] input_2;
    input [6:0] input_1;
    input [6:0] input_0;
    input [3:0] sel;
    reg [6:0] result;
  begin
    result = input_0 & {7{sel[0]}};
    result = result | ( input_1 & {7{sel[1]}});
    result = result | ( input_2 & {7{sel[2]}});
    result = result | ( input_3 & {7{sel[3]}});
    MUX1HOT_v_7_4_2 = result;
  end
  endfunction


  function automatic [7:0] MUX1HOT_v_8_3_2;
    input [7:0] input_2;
    input [7:0] input_1;
    input [7:0] input_0;
    input [2:0] sel;
    reg [7:0] result;
  begin
    result = input_0 & {8{sel[0]}};
    result = result | ( input_1 & {8{sel[1]}});
    result = result | ( input_2 & {8{sel[2]}});
    MUX1HOT_v_8_3_2 = result;
  end
  endfunction


  function automatic [8:0] MUX1HOT_v_9_3_2;
    input [8:0] input_2;
    input [8:0] input_1;
    input [8:0] input_0;
    input [2:0] sel;
    reg [8:0] result;
  begin
    result = input_0 & {9{sel[0]}};
    result = result | ( input_1 & {9{sel[1]}});
    result = result | ( input_2 & {9{sel[2]}});
    MUX1HOT_v_9_3_2 = result;
  end
  endfunction


  function automatic [8:0] MUX1HOT_v_9_4_2;
    input [8:0] input_3;
    input [8:0] input_2;
    input [8:0] input_1;
    input [8:0] input_0;
    input [3:0] sel;
    reg [8:0] result;
  begin
    result = input_0 & {9{sel[0]}};
    result = result | ( input_1 & {9{sel[1]}});
    result = result | ( input_2 & {9{sel[2]}});
    result = result | ( input_3 & {9{sel[3]}});
    MUX1HOT_v_9_4_2 = result;
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


  function automatic [12:0] MUX_v_13_2_2;
    input [12:0] input_0;
    input [12:0] input_1;
    input [0:0] sel;
    reg [12:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_13_2_2 = result;
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


  function automatic [0:0] readslicef_17_1_16;
    input [16:0] vector;
    reg [16:0] tmp;
  begin
    tmp = vector >> 16;
    readslicef_17_1_16 = tmp[0:0];
  end
  endfunction


  function automatic [27:0] readslicef_29_28_1;
    input [28:0] vector;
    reg [28:0] tmp;
  begin
    tmp = vector >> 1;
    readslicef_29_28_1 = tmp[27:0];
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


  function automatic [0:0] readslicef_36_1_35;
    input [35:0] vector;
    reg [35:0] tmp;
  begin
    tmp = vector >> 35;
    readslicef_36_1_35 = tmp[0:0];
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


  function automatic [0:0] readslicef_7_1_6;
    input [6:0] vector;
    reg [6:0] tmp;
  begin
    tmp = vector >> 6;
    readslicef_7_1_6 = tmp[0:0];
  end
  endfunction


  function automatic [5:0] readslicef_9_6_3;
    input [8:0] vector;
    reg [8:0] tmp;
  begin
    tmp = vector >> 3;
    readslicef_9_6_3 = tmp[5:0];
  end
  endfunction


  function automatic [1:0] signext_2_1;
    input [0:0] vector;
  begin
    signext_2_1= {{1{vector[0]}}, vector};
  end
  endfunction


  function automatic [31:0] signext_32_26;
    input [25:0] vector;
  begin
    signext_32_26= {{6{vector[25]}}, vector};
  end
  endfunction


  function automatic [2:0] signext_3_1;
    input [0:0] vector;
  begin
    signext_3_1= {{2{vector[0]}}, vector};
  end
  endfunction


  function automatic [2:0] signext_3_2;
    input [1:0] vector;
  begin
    signext_3_2= {{1{vector[1]}}, vector};
  end
  endfunction


  function automatic [39:0] signext_40_28;
    input [27:0] vector;
  begin
    signext_40_28= {{12{vector[27]}}, vector};
  end
  endfunction


  function automatic [39:0] signext_40_33;
    input [32:0] vector;
  begin
    signext_40_33= {{7{vector[32]}}, vector};
  end
  endfunction


  function automatic [43:0] signext_44_28;
    input [27:0] vector;
  begin
    signext_44_28= {{16{vector[27]}}, vector};
  end
  endfunction


  function automatic [5:0] signext_6_1;
    input [0:0] vector;
  begin
    signext_6_1= {{5{vector[0]}}, vector};
  end
  endfunction


  function automatic [6:0] signext_7_2;
    input [1:0] vector;
  begin
    signext_7_2= {{5{vector[1]}}, vector};
  end
  endfunction


  function automatic [7:0] signext_8_5;
    input [4:0] vector;
  begin
    signext_8_5= {{3{vector[4]}}, vector};
  end
  endfunction


  function automatic [8:0] signext_9_1;
    input [0:0] vector;
  begin
    signext_9_1= {{8{vector[0]}}, vector};
  end
  endfunction


  function automatic [8:0] signext_9_6;
    input [5:0] vector;
  begin
    signext_9_6= {{3{vector[5]}}, vector};
  end
  endfunction


  function automatic [8:0] signext_9_8;
    input [7:0] vector;
  begin
    signext_9_8= {{1{vector[7]}}, vector};
  end
  endfunction


  function automatic [7:0] conv_s2u_7_8 ;
    input [6:0]  vector ;
  begin
    conv_s2u_7_8 = {vector[6], vector};
  end
  endfunction


  function automatic [9:0] conv_s2u_8_10 ;
    input [7:0]  vector ;
  begin
    conv_s2u_8_10 = {{2{vector[7]}}, vector};
  end
  endfunction


  function automatic [9:0] conv_s2u_9_10 ;
    input [8:0]  vector ;
  begin
    conv_s2u_9_10 = {vector[8], vector};
  end
  endfunction


  function automatic [23:0] conv_s2u_21_24 ;
    input [20:0]  vector ;
  begin
    conv_s2u_21_24 = {{3{vector[20]}}, vector};
  end
  endfunction


  function automatic [35:0] conv_s2u_34_36 ;
    input [33:0]  vector ;
  begin
    conv_s2u_34_36 = {{2{vector[33]}}, vector};
  end
  endfunction


  function automatic [35:0] conv_s2u_35_36 ;
    input [34:0]  vector ;
  begin
    conv_s2u_35_36 = {vector[34], vector};
  end
  endfunction


  function automatic [43:0] conv_s2u_40_44 ;
    input [39:0]  vector ;
  begin
    conv_s2u_40_44 = {{4{vector[39]}}, vector};
  end
  endfunction


  function automatic [6:0] conv_u2s_6_7 ;
    input [5:0]  vector ;
  begin
    conv_u2s_6_7 =  {1'b0, vector};
  end
  endfunction


  function automatic [8:0] conv_u2u_8_9 ;
    input [7:0]  vector ;
  begin
    conv_u2u_8_9 = {1'b0, vector};
  end
  endfunction


  function automatic [15:0] conv_u2u_9_16 ;
    input [8:0]  vector ;
  begin
    conv_u2u_9_16 = {{7{1'b0}}, vector};
  end
  endfunction


  function automatic [16:0] conv_u2u_16_17 ;
    input [15:0]  vector ;
  begin
    conv_u2u_16_17 = {1'b0, vector};
  end
  endfunction


  function automatic [26:0] conv_u2u_26_27 ;
    input [25:0]  vector ;
  begin
    conv_u2u_26_27 = {1'b0, vector};
  end
  endfunction


  function automatic [28:0] conv_u2u_28_29 ;
    input [27:0]  vector ;
  begin
    conv_u2u_28_29 = {1'b0, vector};
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
      acc_tmp_rsci_we_d, acc_tmp_rsci_data_out_d, acc_tmp_rsci_en_d, cos_out_rsci_data_in_d,
      cos_out_rsci_addr_d, cos_out_rsci_re_d, cos_out_rsci_we_d, cos_out_rsci_data_out_d,
      cos_out_rsci_en_d, sin_out_rsci_data_in_d, sin_out_rsci_re_d, sin_out_rsci_we_d,
      sin_out_rsci_data_out_d
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
  output [37:0] acc_tmp_rsci_addr_d;
  output [1:0] acc_tmp_rsci_re_d;
  output [1:0] acc_tmp_rsci_we_d;
  input [31:0] acc_tmp_rsci_data_out_d;
  output acc_tmp_rsci_en_d;
  output [26:0] cos_out_rsci_data_in_d;
  output [7:0] cos_out_rsci_addr_d;
  output [1:0] cos_out_rsci_re_d;
  output [1:0] cos_out_rsci_we_d;
  input [53:0] cos_out_rsci_data_out_d;
  output cos_out_rsci_en_d;
  output [26:0] sin_out_rsci_data_in_d;
  output [1:0] sin_out_rsci_re_d;
  output [1:0] sin_out_rsci_we_d;
  input [53:0] sin_out_rsci_data_out_d;


  // Interconnect Declarations
  wire core_wen;
  wire core_wten;
  wire data_in_rsci_wen_comp;
  wire [7:0] data_in_rsci_idat_mxwt;
  wire acc_rsci_wen_comp;
  reg [15:0] acc_rsci_idat;
  wire [31:0] acc_tmp_rsci_data_out_d_oreg;
  wire [26:0] cos_out_rsci_data_out_d_oreg;
  wire [26:0] sin_out_rsci_data_out_d_oreg;
  wire [14:0] fsm_output;
  wire HROW_equal_tmp;
  wire HCOL_equal_tmp;
  wire for_for_nor_tmp;
  wire and_dcpl_18;
  wire or_dcpl_28;
  wire or_dcpl_36;
  wire or_tmp_41;
  wire or_tmp_88;
  wire or_tmp_97;
  wire and_32_cse;
  wire and_63_cse;
  wire and_69_cse;
  wire and_67_cse;
  reg HACC_stage_0;
  reg HACC_stage_0_3;
  reg HACC_stage_0_2;
  reg HACC_stage_0_5;
  reg HACC_stage_0_4;
  reg HACC_stage_0_6;
  reg HACC_stage_0_7;
  reg HACC_stage_0_8;
  reg reg_heightIn_rsc_triosy_obj_ld_core_psct_cse;
  reg reg_sin_out_rsc_cgo_cse;
  reg reg_acc_rsci_ivld_core_psct_cse;
  reg reg_data_in_rsci_irdy_core_psct_cse;
  reg reg_acc_tmp_rsc_cgo_cse;
  wire or_54_rmff;
  wire or_53_rmff;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_or_1_rmff;
  reg [35:0] HACC_mul_itm_1;
  reg [7:0] for_1_t_7_0_sva;
  wire [12:0] z_out;
  wire [23:0] z_out_1;
  wire [24:0] nl_z_out_1;
  wire or_tmp_138;
  wire [18:0] z_out_2;
  wire [32:0] z_out_3;
  wire [34:0] z_out_4;
  wire [32:0] z_out_5;
  wire [35:0] z_out_6;
  wire [10:0] z_out_7;
  wire [11:0] nl_z_out_7;
  wire [10:0] z_out_8;
  wire [32:0] z_out_9;
  wire [33:0] nl_z_out_9;
  wire [7:0] z_out_11;
  wire [8:0] nl_z_out_11;
  reg [10:0] operator_11_false_io_read_widthIn_rsc_cse_sva;
  reg [9:0] operator_10_false_io_read_heightIn_rsc_cse_sva;
  reg [25:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_sva;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs;
  reg [4:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva;
  reg [9:0] HROW_y_sva;
  reg [18:0] WRITE_i_18_0_sva;
  reg [10:0] HACC_round_r_HACC_round_r_HACC_round_r_conc_trnz_26_16_sva_1;
  wire [11:0] nl_HACC_round_r_HACC_round_r_HACC_round_r_conc_trnz_26_16_sva_1;
  reg [12:0] HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1;
  reg [16:0] HACC_idx_18_2_sva_1;
  reg [16:0] HACC_idx_18_2_sva_2;
  reg [11:0] HACC_acc_4_itm_1;
  reg [10:0] HACC_acc_5_itm_1;
  reg HACC_round_r_HACC_round_r_acc_conv_2f_or_itm_1;
  reg [5:0] HACC_idx_slc_HACC_t_7_2_itm_1;
  reg [5:0] HACC_idx_slc_HACC_t_7_2_itm_2;
  reg [5:0] HACC_idx_slc_HACC_t_7_2_itm_3;
  reg [5:0] HACC_idx_slc_HACC_t_7_2_itm_4;
  reg [1:0] HACC_idx_slc_HACC_t_1_0_1_itm_1;
  reg [1:0] HACC_idx_slc_HACC_t_1_0_1_itm_2;
  reg [1:0] HACC_idx_slc_HACC_t_1_0_1_itm_3;
  reg [1:0] HACC_idx_slc_HACC_t_1_0_1_itm_4;
  reg [1:0] HACC_idx_slc_HACC_t_1_0_itm_5;
  reg [1:0] HACC_idx_slc_HACC_t_1_0_itm_6;
  reg [2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_32_30;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_29;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_24;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_21;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_17;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_16;
  reg [2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_10;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_7;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_4;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_3;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_2;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_1;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_0;
  reg [2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_1_34_32;
  reg [1:0] HACC_ac_fixed_cctor_sva_1_15_14;
  reg [12:0] HACC_idx_acc_2_itm_1_14_2;
  reg [1:0] HACC_idx_acc_2_itm_1_1_0;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_26;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_25;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_23;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_22;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_20;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_19;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_18;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_9;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_8;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_6;
  reg ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_5;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx1;
  wire [1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_15_14_1;
  wire [2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0;
  wire [32:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva_1;
  wire [31:0] ac_math_atan_pi_2mi_return_69_38_sva_1;
  wire [32:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_1;
  wire [12:0] HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1_1;
  wire [13:0] nl_HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1_1;
  wire [2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_32_30;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_29;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_28;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_27;
  wire [1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_26_25;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_24;
  wire [1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_23_22;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_21;
  wire [2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_20_18;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_17;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_16;
  wire [1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_15_14;
  wire [2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_13_11;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_10;
  wire [1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_9_8;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_7;
  wire [1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_6_5;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_4;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_3;
  wire [1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_2_1;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_0;
  reg reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_29_cse;
  wire HACC_idx_and_1_cse;
  wire HACC_round_r_HACC_round_r_acc_conv_2f_and_1_cse;
  wire and_64_cse;
  wire HACC_and_cse;
  reg reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_28_cse;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_and_cse;
  wire ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_and_1_cse;
  wire [35:0] HACC_mul_1_sdt;
  wire signed [37:0] nl_HACC_mul_1_sdt;
  reg [2:0] reg_HACC_mul_1_itm_1_ftd;
  reg reg_HACC_mul_1_itm_1_ftd_1;
  reg [31:0] reg_HACC_mul_1_itm_1_ftd_2;
  wire WRITE_acc_itm_12_1;
  wire [27:0] z_out_10_35_8;
  wire for_1_or_9_cse;

  wire[18:0] WRITE_i_mux1h_3_nl;
  wire[10:0] HCOL_x_HCOL_x_and_nl;
  wire[10:0] HCOL_x_mux_nl;
  wire[0:0] HCOL_x_or_nl;
  wire[0:0] or_89_nl;
  wire[0:0] nor_13_nl;
  wire[7:0] for_1_t_mux1h_4_nl;
  wire[0:0] or_95_nl;
  wire[0:0] for_1_t_nor_nl;
  wire[12:0] HACC_idx_acc_6_nl;
  wire[13:0] nl_HACC_idx_acc_6_nl;
  wire[8:0] HACC_idx_acc_8_nl;
  wire[9:0] nl_HACC_idx_acc_8_nl;
  wire[0:0] or_99_nl;
  wire[1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_or_nl;
  wire[0:0] HACC_mux1h_4_nl;
  wire[0:0] T_LINE_if_aelse_T_LINE_if_aelse_and_nl;
  wire[0:0] and_6_nl;
  wire[0:0] and_11_nl;
  wire[0:0] HACC_mux_11_nl;
  wire[1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_11_nl;
  wire[0:0] HACC_not_nl;
  wire[1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_12_nl;
  wire[0:0] HACC_idx_not_2_nl;
  wire[0:0] or_149_nl;
  wire[0:0] and_272_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_13_nl;
  wire[0:0] HACC_round_r_HACC_round_r_acc_conv_2f_or_nl;
  wire[31:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_nl;
  wire[0:0] HACC_or_2_nl;
  wire[0:0] HACC_and_4_nl;
  wire[32:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_mux_nl;
  wire[35:0] HACC_mul_nl;
  wire signed [38:0] nl_HACC_mul_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_not_nl;
  wire[0:0] HACC_round_r_HACC_round_r_acc_conv_2f_and_nl;
  wire[12:0] WRITE_acc_nl;
  wire[13:0] nl_WRITE_acc_nl;
  wire[18:0] for_mux1h_6_nl;
  wire[18:0] WRITE_i_WRITE_i_and_nl;
  wire[0:0] for_and_nl;
  wire[0:0] for_or_1_nl;
  wire[1:0] for_for_mux_1_nl;
  wire[0:0] or_73_nl;
  wire[0:0] or_71_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_or_2_nl;
  wire[7:0] HACC_t_HACC_t_and_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_or_nl;
  wire[13:0] acc_nl;
  wire[14:0] nl_acc_nl;
  wire[7:0] for_1_for_1_for_1_nand_2_nl;
  wire[0:0] for_1_not_11_nl;
  wire[1:0] for_1_for_1_or_6_nl;
  wire[0:0] for_1_not_12_nl;
  wire[0:0] for_1_mux_24_nl;
  wire[0:0] for_1_or_5_nl;
  wire[7:0] for_1_mux_25_nl;
  wire[7:0] for_1_for_1_and_8_nl;
  wire[7:0] for_1_mux_26_nl;
  wire[0:0] for_1_nor_6_nl;
  wire[5:0] for_1_for_1_and_9_nl;
  wire[5:0] for_1_mux_27_nl;
  wire[0:0] for_1_nor_7_nl;
  wire[9:0] for_1_mux1h_4_nl;
  wire[0:0] for_1_for_1_or_7_nl;
  wire[12:0] for_1_for_1_or_8_nl;
  wire[12:0] for_1_and_1_nl;
  wire[6:0] for_1_for_1_for_1_nand_3_nl;
  wire[6:0] for_1_mux_28_nl;
  wire[0:0] for_1_or_6_nl;
  wire[0:0] for_1_not_15_nl;
  wire[19:0] acc_2_nl;
  wire[20:0] nl_acc_2_nl;
  wire[7:0] HACC_HACC_and_9_nl;
  wire[0:0] HACC_nor_2_nl;
  wire[0:0] HACC_or_3_nl;
  wire[0:0] HACC_HACC_or_3_nl;
  wire[9:0] HACC_HACC_HACC_nand_1_nl;
  wire[9:0] HACC_mux_13_nl;
  wire[0:0] HACC_not_4_nl;
  wire[33:0] acc_3_nl;
  wire[34:0] nl_acc_3_nl;
  wire[2:0] for_1_for_1_and_10_nl;
  wire[0:0] for_1_for_1_and_11_nl;
  wire[0:0] for_1_for_1_and_12_nl;
  wire[0:0] for_1_for_1_and_13_nl;
  wire[1:0] for_1_for_1_and_14_nl;
  wire[0:0] for_1_for_1_and_15_nl;
  wire[1:0] for_1_mux_29_nl;
  wire[0:0] for_1_mux_30_nl;
  wire[2:0] for_1_mux_31_nl;
  wire[0:0] for_1_mux_32_nl;
  wire[0:0] for_1_mux_33_nl;
  wire[1:0] for_1_mux_34_nl;
  wire[2:0] for_1_mux_35_nl;
  wire[0:0] for_1_mux_36_nl;
  wire[1:0] for_1_mux_37_nl;
  wire[0:0] for_1_mux_38_nl;
  wire[1:0] for_1_mux_39_nl;
  wire[0:0] for_1_mux_40_nl;
  wire[0:0] for_1_mux_41_nl;
  wire[1:0] for_1_mux_42_nl;
  wire[0:0] for_1_mux_43_nl;
  wire[0:0] for_1_or_7_nl;
  wire[32:0] for_1_mux_44_nl;
  wire[35:0] acc_4_nl;
  wire[36:0] nl_acc_4_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_nand_1_nl;
  wire[31:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_3_nl;
  wire[33:0] acc_5_nl;
  wire[34:0] nl_acc_5_nl;
  wire[2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_27_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_28_nl;
  wire[1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_29_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_30_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_31_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_32_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_33_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_34_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_35_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_36_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_37_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_38_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_39_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_40_nl;
  wire[1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_41_nl;
  wire[2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_42_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_43_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_44_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_45_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_46_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_47_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_48_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_49_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_50_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_51_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_52_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_53_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_28_nl;
  wire[32:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_28_nl;
  wire[36:0] acc_6_nl;
  wire[37:0] nl_acc_6_nl;
  wire[34:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_mux1h_2_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_or_1_nl;
  wire[25:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_and_1_nl;
  wire[25:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_mux1h_3_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_not_3_nl;
  wire[5:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_and_2_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_nor_3_nl;
  wire[1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_and_3_nl;
  wire[1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_mux_1_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_not_4_nl;
  wire[10:0] operator_11_false_mux_2_nl;
  wire[11:0] acc_8_nl;
  wire[12:0] nl_acc_8_nl;
  wire[9:0] for_1_mux_45_nl;
  wire[0:0] for_1_or_8_nl;
  wire[8:0] for_1_for_1_mux_1_nl;
  wire[2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_17_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_17_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_18_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_19_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_20_nl;
  wire[1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_21_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_21_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_22_nl;
  wire[1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_23_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_23_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_24_nl;
  wire[2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_25_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_25_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_26_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_27_nl;
  wire[1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_28_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_28_nl;
  wire[2:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_29_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_29_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_30_nl;
  wire[1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_31_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_31_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_32_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_33_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_6_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_7_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_8_nl;
  wire[1:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_9_nl;
  wire[0:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_10_nl;
  wire[32:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_11_nl;
  wire[35:0] HACC_acc_nl;
  wire[36:0] nl_HACC_acc_nl;
  wire[35:0] HACC_mux_14_nl;
  wire[2:0] HACC_HACC_and_10_nl;
  wire[0:0] HACC_not_5_nl;
  wire[0:0] HACC_HACC_and_11_nl;
  wire[31:0] HACC_mux_15_nl;
  wire[7:0] ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_2_nl;

  // Interconnect Declarations for Component Instantiations 
  wire [32:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a;
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a = {ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_32_30
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_29
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_28
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_27
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_26_25
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_24
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_23_22
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_21
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_20_18
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_17
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_16
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_15_14
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_13_11
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_10
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_9_8
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_7
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_6_5
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_4
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_3
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_2_1
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_0};
  wire [4:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s;
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva;
  wire [6:0] nl_ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr;
  assign nl_ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr
      = {2'b0, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva};
  wire [32:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_a;
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_a = {ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_32_30
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_29
      , HACC_idx_acc_2_itm_1_1_0 , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_26
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_25
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_24
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_23
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_22
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_21
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_20
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_19
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_18
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_17
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_16
      , HACC_ac_fixed_cctor_sva_1_15_14 , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_10
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_9
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_8
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_7
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_6
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_5
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_4
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_3
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_2
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_1
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_0};
  wire [4:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_s;
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_s = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva;
  wire [0:0] nl_houghTransform_core_core_fsm_inst_acc_tmp_vinit_C_0_tr0;
  assign nl_houghTransform_core_core_fsm_inst_acc_tmp_vinit_C_0_tr0 = for_for_nor_tmp;
  wire [0:0] nl_houghTransform_core_core_fsm_inst_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0_tr0;
  assign nl_houghTransform_core_core_fsm_inst_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0_tr0
      = ~ HACC_stage_0;
  wire [0:0] nl_houghTransform_core_core_fsm_inst_for_1_C_4_tr0;
  assign nl_houghTransform_core_core_fsm_inst_for_1_C_4_tr0 = ~ (z_out_9[6]);
  wire [0:0] nl_houghTransform_core_core_fsm_inst_HCOL_C_0_tr0;
  assign nl_houghTransform_core_core_fsm_inst_HCOL_C_0_tr0 = ~ (z_out_10_35_8[0]);
  wire [0:0] nl_houghTransform_core_core_fsm_inst_HACC_C_0_tr0;
  assign nl_houghTransform_core_core_fsm_inst_HACC_C_0_tr0 = (~(HACC_stage_0_7 |
      HACC_stage_0_6)) & (~(HACC_stage_0_5 | HACC_stage_0_4)) & (~(HACC_stage_0_3
      | HACC_stage_0 | HACC_stage_0_2));
  mgc_shift_r_v5 #(.width_a(32'sd33),
  .signd_a(32'sd1),
  .width_s(32'sd5),
  .width_z(32'sd33)) operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg (
      .a(nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a[32:0]),
      .s(nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s[4:0]),
      .z(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva_1)
    );
  Hough_Algorithm_HW_1296_864mgc_rom_26_70_32_1_60  ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg
      (
      .addr(nl_ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr[6:0]),
      .data_out(ac_math_atan_pi_2mi_return_69_38_sva_1)
    );
  mgc_shift_r_v5 #(.width_a(32'sd33),
  .signd_a(32'sd1),
  .width_s(32'sd5),
  .width_z(32'sd33)) operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg (
      .a(nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_a[32:0]),
      .s(nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_s[4:0]),
      .z(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_1)
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
      .acc_tmp_rsc_cgo_iro(or_54_rmff),
      .acc_tmp_rsci_data_out_d(acc_tmp_rsci_data_out_d),
      .acc_tmp_rsci_en_d(acc_tmp_rsci_en_d),
      .cos_out_rsc_cgo_iro(or_53_rmff),
      .cos_out_rsci_data_out_d(cos_out_rsci_data_out_d),
      .cos_out_rsci_en_d(cos_out_rsci_en_d),
      .sin_out_rsci_data_out_d(sin_out_rsci_data_out_d),
      .core_wen(core_wen),
      .acc_tmp_rsc_cgo(reg_acc_tmp_rsc_cgo_cse),
      .acc_tmp_rsci_data_out_d_oreg(acc_tmp_rsci_data_out_d_oreg),
      .cos_out_rsc_cgo(reg_sin_out_rsc_cgo_cse),
      .cos_out_rsci_data_out_d_oreg(cos_out_rsci_data_out_d_oreg),
      .sin_out_rsci_data_out_d_oreg(sin_out_rsci_data_out_d_oreg)
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
      .ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0_tr0(nl_houghTransform_core_core_fsm_inst_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0_tr0[0:0]),
      .for_1_C_4_tr0(nl_houghTransform_core_core_fsm_inst_for_1_C_4_tr0[0:0]),
      .HCOL_C_0_tr0(nl_houghTransform_core_core_fsm_inst_HCOL_C_0_tr0[0:0]),
      .HACC_C_0_tr0(nl_houghTransform_core_core_fsm_inst_HACC_C_0_tr0[0:0]),
      .HCOL_C_1_tr0(HCOL_equal_tmp),
      .HROW_C_0_tr0(HROW_equal_tmp),
      .WRITE_C_0_tr0(and_dcpl_18)
    );
  assign and_64_cse = HACC_stage_0 & (fsm_output[10]);
  assign or_53_rmff = (fsm_output[7]) | ((z_out_10_35_8[0]) & (fsm_output[9])) |
      and_63_cse | and_64_cse;
  assign or_54_rmff = (HACC_stage_0 & (fsm_output[13])) | and_67_cse | ((HACC_stage_0_7
      | HACC_stage_0_6 | HACC_stage_0_5 | HACC_stage_0_8) & (fsm_output[10])) | and_69_cse;
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_and_cse
      = (~ (z_out_6[35])) & (fsm_output[6]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_and_1_cse
      = (z_out_6[35]) & (fsm_output[6]);
  assign nl_HACC_mul_1_sdt = $signed((sin_out_rsci_data_out_d_oreg)) * $signed(HACC_acc_5_itm_1);
  assign HACC_mul_1_sdt = nl_HACC_mul_1_sdt[35:0];
  assign HACC_idx_and_1_cse = core_wen & HACC_stage_0_4;
  assign HACC_round_r_HACC_round_r_acc_conv_2f_and_1_cse = core_wen & HACC_stage_0_3;
  assign HACC_and_cse = core_wen & HACC_stage_0;
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx1
      = MUX_s_1_2_2((z_out_11[2]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs,
      z_out_6[26]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_not_nl
      = ~ (z_out_6[26]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_15_14_1
      = MUX_v_2_2_2(2'b00, (signext_2_1(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_not_nl)),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx1);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0
      = ~(MUX_v_3_2_2((signext_3_1(z_out_6[26])), 3'b111, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_mx1));
  assign HACC_round_r_HACC_round_r_acc_conv_2f_and_nl = HACC_round_r_HACC_round_r_acc_conv_2f_or_itm_1
      & (HACC_round_r_HACC_round_r_HACC_round_r_conc_trnz_26_16_sva_1[10]);
  assign nl_HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1_1 = ({HACC_round_r_HACC_round_r_HACC_round_r_conc_trnz_26_16_sva_1
      , HACC_ac_fixed_cctor_sva_1_15_14}) + conv_u2s_1_13(HACC_round_r_HACC_round_r_acc_conv_2f_and_nl);
  assign HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1_1 = nl_HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1_1[12:0];
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_32_30
      = MUX_v_3_2_2((HACC_mul_itm_1[32:30]), ({{2{reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_28_cse}},
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_28_cse}),
      or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_29
      = MUX_s_1_2_2((HACC_mul_itm_1[29]), reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_29_cse,
      or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_28
      = MUX_s_1_2_2((HACC_mul_itm_1[28]), reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_28_cse,
      or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_27
      = MUX_s_1_2_2((HACC_mul_itm_1[27]), reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_28_cse,
      or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_26_25
      = MUX_v_2_2_2((HACC_mul_itm_1[26:25]), HACC_idx_slc_HACC_t_1_0_1_itm_3, or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_24
      = MUX_s_1_2_2((HACC_mul_itm_1[24]), reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_28_cse,
      or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_23_22
      = MUX_v_2_2_2((HACC_mul_itm_1[23:22]), HACC_idx_slc_HACC_t_1_0_1_itm_2, or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_21
      = MUX_s_1_2_2((HACC_mul_itm_1[21]), reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_28_cse,
      or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_20_18
      = MUX_v_3_2_2((HACC_mul_itm_1[20:18]), ({{2{reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_29_cse}},
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_29_cse}),
      or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_17
      = MUX_s_1_2_2((HACC_mul_itm_1[17]), reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_28_cse,
      or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_16
      = MUX_s_1_2_2((HACC_mul_itm_1[16]), reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_29_cse,
      or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_15_14
      = MUX_v_2_2_2((HACC_mul_itm_1[15:14]), HACC_idx_slc_HACC_t_1_0_1_itm_1, or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_13_11
      = MUX_v_3_2_2((HACC_mul_itm_1[13:11]), ({{2{reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_29_cse}},
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_29_cse}),
      or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_10
      = MUX_s_1_2_2((HACC_mul_itm_1[10]), reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_28_cse,
      or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_9_8
      = MUX_v_2_2_2((HACC_mul_itm_1[9:8]), HACC_idx_slc_HACC_t_1_0_itm_6, or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_7
      = MUX_s_1_2_2((HACC_mul_itm_1[7]), reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_28_cse,
      or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_6_5
      = MUX_v_2_2_2((HACC_mul_itm_1[6:5]), HACC_idx_slc_HACC_t_1_0_itm_5, or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_4
      = MUX_s_1_2_2((HACC_mul_itm_1[4]), reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_28_cse,
      or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_3
      = MUX_s_1_2_2((HACC_mul_itm_1[3]), reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_29_cse,
      or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_2_1
      = MUX_v_2_2_2((HACC_mul_itm_1[2:1]), HACC_idx_slc_HACC_t_1_0_1_itm_4, or_dcpl_36);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_0
      = MUX_s_1_2_2((HACC_mul_itm_1[0]), HACC_round_r_HACC_round_r_acc_conv_2f_or_itm_1,
      or_dcpl_36);
  assign nl_WRITE_acc_nl = conv_u2s_12_13(z_out_6[18:7]) + 13'b1001111001011;
  assign WRITE_acc_nl = nl_WRITE_acc_nl[12:0];
  assign WRITE_acc_itm_12_1 = readslicef_13_1_12(WRITE_acc_nl);
  assign HROW_equal_tmp = HROW_y_sva == (z_out_1[9:0]);
  assign HCOL_equal_tmp = (WRITE_i_18_0_sva[10:0]) == z_out_7;
  assign for_for_nor_tmp = ~((WRITE_i_18_0_sva!=19'b0000000000000000000));
  assign and_32_cse = (~ HROW_equal_tmp) & (fsm_output[12]);
  assign and_dcpl_18 = ~(HACC_stage_0 | HACC_stage_0_2);
  assign or_dcpl_28 = (fsm_output[4:3]!=2'b00);
  assign or_dcpl_36 = HACC_stage_0_3 | (~ HACC_stage_0_2);
  assign and_63_cse = (~ HACC_stage_0) & (fsm_output[6]);
  assign and_69_cse = HROW_equal_tmp & (fsm_output[12]);
  assign and_67_cse = (fsm_output[2:1]!=2'b00);
  assign or_tmp_41 = ~((fsm_output[14]) | (fsm_output[0]) | (fsm_output[13]) | (fsm_output[1]));
  assign or_tmp_88 = (fsm_output[9]) | (fsm_output[5]) | (fsm_output[12]);
  assign or_tmp_97 = ~((fsm_output[6]) | (fsm_output[10]));
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_or_1_rmff
      = (~((fsm_output[10:9]!=2'b00))) | ((~ (z_out_10_35_8[0])) & (fsm_output[9]))
      | ((~(HACC_stage_0 & (z_out_7[6]))) & (fsm_output[10]));
  assign WRITE_i_WRITE_i_and_nl = MUX_v_19_2_2(19'b0000000000000000000, (z_out_6[18:0]),
      (fsm_output[13]));
  assign for_and_nl = (~ (fsm_output[2])) & and_67_cse;
  assign for_or_1_nl = (fsm_output[13:12]!=2'b00);
  assign for_mux1h_6_nl = MUX1HOT_v_19_4_2(19'b1100001101001111111, z_out_2, ({HACC_idx_18_2_sva_2
      , HACC_idx_slc_HACC_t_1_0_itm_6}), WRITE_i_WRITE_i_and_nl, {for_and_nl , (fsm_output[2])
      , (fsm_output[10]) , for_or_1_nl});
  assign acc_tmp_rsci_addr_d = {(z_out_5[16:0]) , HACC_idx_slc_HACC_t_1_0_1_itm_4
      , for_mux1h_6_nl};
  assign or_73_nl = and_69_cse | (HACC_stage_0 & WRITE_acc_itm_12_1 & (fsm_output[13]));
  assign for_for_mux_1_nl = MUX_v_2_2_2(2'b01, 2'b10, or_73_nl);
  assign or_71_nl = (~((fsm_output[13]) | (fsm_output[10]) | (fsm_output[12]))) |
      ((~(HACC_stage_0 & WRITE_acc_itm_12_1)) & (fsm_output[13])) | ((~ HACC_stage_0_5)
      & (fsm_output[10])) | and_32_cse;
  assign acc_tmp_rsci_re_d = MUX_v_2_2_2(for_for_mux_1_nl, 2'b11, or_71_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_or_2_nl
      = (~((fsm_output[1]) | (fsm_output[2]) | (fsm_output[10]))) | ((~ HACC_stage_0_7)
      & (fsm_output[10])) | (for_for_nor_tmp & (fsm_output[2]));
  assign acc_tmp_rsci_we_d = {1'b1 , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_or_2_nl};
  assign cos_out_rsci_re_d = {1'b1 , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_or_1_rmff};
  assign cos_out_rsci_we_d = {1'b1 , (~ and_63_cse)};
  assign sin_out_rsci_re_d = {1'b1 , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_or_1_rmff};
  assign sin_out_rsci_we_d = {1'b1 , (~ and_63_cse)};
  assign acc_tmp_rsci_data_in_d = MUX_v_16_2_2(16'b0000000000000000, (z_out_1[15:0]),
      (fsm_output[10]));
  assign cos_out_rsci_data_in_d = {reg_HACC_mul_1_itm_1_ftd_1 , (reg_HACC_mul_1_itm_1_ftd_2[31:6])};
  assign HACC_t_HACC_t_and_nl = MUX_v_8_2_2(8'b00000000, z_out_11, (fsm_output[10]));
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_or_nl
      = (fsm_output[10:9]!=2'b00);
  assign cos_out_rsci_addr_d = MUX_v_8_2_2(for_1_t_7_0_sva, HACC_t_HACC_t_and_nl,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_or_nl);
  assign sin_out_rsci_data_in_d = HACC_mul_itm_1[32:6];
  assign or_tmp_138 = (fsm_output[9]) | ((~ HACC_stage_0) & (fsm_output[10]));
  always @(posedge clk) begin
    if ( rst ) begin
      reg_heightIn_rsc_triosy_obj_ld_core_psct_cse <= 1'b0;
      reg_sin_out_rsc_cgo_cse <= 1'b0;
      reg_acc_tmp_rsc_cgo_cse <= 1'b0;
      reg_acc_rsci_ivld_core_psct_cse <= 1'b0;
      reg_data_in_rsci_irdy_core_psct_cse <= 1'b0;
      operator_10_false_io_read_heightIn_rsc_cse_sva <= 10'b0000000000;
      operator_11_false_io_read_widthIn_rsc_cse_sva <= 11'b00000000000;
      WRITE_i_18_0_sva <= 19'b0000000000000000000;
      for_1_t_7_0_sva <= 8'b00000000;
      HACC_idx_acc_2_itm_1_14_2 <= 13'b0000000000000;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_sva
          <= 26'b00000000000000000000000000;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva
          <= 5'b00000;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_1_34_32
          <= 3'b000;
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_29_cse
          <= 1'b0;
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_28_cse
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_32_30
          <= 3'b000;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_29
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_26
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_25
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_24
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_23
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_22
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_21
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_20
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_19
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_18
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_17
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_16
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11
          <= 3'b000;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_10
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_9
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_8
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_7
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_6
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_5
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_4
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_3
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_2
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_1
          <= 1'b0;
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_0
          <= 1'b0;
      HACC_stage_0 <= 1'b0;
      HACC_stage_0_2 <= 1'b0;
      HACC_stage_0_3 <= 1'b0;
      HACC_ac_fixed_cctor_sva_1_15_14 <= 2'b00;
      HACC_idx_acc_2_itm_1_1_0 <= 2'b00;
      HACC_idx_slc_HACC_t_1_0_1_itm_1 <= 2'b00;
      HACC_idx_slc_HACC_t_1_0_1_itm_2 <= 2'b00;
      HACC_idx_slc_HACC_t_1_0_1_itm_3 <= 2'b00;
      HACC_idx_slc_HACC_t_1_0_1_itm_4 <= 2'b00;
      HACC_idx_slc_HACC_t_1_0_itm_5 <= 2'b00;
      HACC_idx_slc_HACC_t_1_0_itm_6 <= 2'b00;
      HACC_round_r_HACC_round_r_acc_conv_2f_or_itm_1 <= 1'b0;
      reg_HACC_mul_1_itm_1_ftd <= 3'b000;
      reg_HACC_mul_1_itm_1_ftd_1 <= 1'b0;
      reg_HACC_mul_1_itm_1_ftd_2 <= 32'b00000000000000000000000000000000;
      HACC_mul_itm_1 <= 36'b000000000000000000000000000000000000;
      HACC_stage_0_4 <= 1'b0;
      HACC_stage_0_5 <= 1'b0;
      HACC_stage_0_6 <= 1'b0;
      HACC_stage_0_7 <= 1'b0;
      HACC_stage_0_8 <= 1'b0;
      HACC_acc_5_itm_1 <= 11'b00000000000;
    end
    else if ( core_wen ) begin
      reg_heightIn_rsc_triosy_obj_ld_core_psct_cse <= and_dcpl_18 & (fsm_output[13]);
      reg_sin_out_rsc_cgo_cse <= or_53_rmff;
      reg_acc_tmp_rsc_cgo_cse <= or_54_rmff;
      reg_acc_rsci_ivld_core_psct_cse <= HACC_stage_0_2 & (fsm_output[13]);
      reg_data_in_rsci_irdy_core_psct_cse <= ((~ HCOL_equal_tmp) & (fsm_output[11]))
          | and_32_cse | ((~ (z_out_9[6])) & (fsm_output[8]));
      operator_10_false_io_read_heightIn_rsc_cse_sva <= MUX_v_10_2_2(heightIn_rsci_idat,
          operator_10_false_io_read_heightIn_rsc_cse_sva, or_tmp_41);
      operator_11_false_io_read_widthIn_rsc_cse_sva <= MUX_v_11_2_2(widthIn_rsci_idat,
          operator_11_false_io_read_widthIn_rsc_cse_sva, or_tmp_41);
      WRITE_i_18_0_sva <= MUX_v_19_2_2(19'b0000000000000000000, WRITE_i_mux1h_3_nl,
          nor_13_nl);
      for_1_t_7_0_sva <= MUX_v_8_2_2(8'b00000000, for_1_t_mux1h_4_nl, for_1_t_nor_nl);
      HACC_idx_acc_2_itm_1_14_2 <= MUX_v_13_2_2(z_out, HACC_idx_acc_6_nl, fsm_output[10]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_sva
          <= MUX_v_26_2_2((z_out_3[25:0]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_sva,
          or_99_nl);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva
          <= MUX_v_5_2_2(5'b00000, (z_out_1[4:0]), (fsm_output[6]));
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_1_34_32
          <= MUX_v_3_2_2((signext_3_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_nl)),
          (z_out_4[34:32]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_or_nl);
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_29_cse
          <= z_out_6[26];
      reg_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_28_cse
          <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_15_14_1[1];
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_32_30
          <= MUX_v_3_2_2(3'b000, (z_out_5[32:30]), (fsm_output[6]));
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_29
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_5[29]), fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_26
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_5[26]), fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_25
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_5[25]), fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_24
          <= (z_out_5[24]) & (fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_23
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_5[23]), fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_22
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_5[22]), fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_21
          <= (z_out_5[21]) & (fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_20
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_5[20]), fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_19
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_5[19]), fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_18
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_5[18]), fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_17
          <= (z_out_5[17]) & (fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_16
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_5[16]), fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11
          <= MUX_v_3_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0,
          (z_out_5[13:11]), fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_10
          <= (z_out_5[10]) & (fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_9
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_5[9]), fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_8
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_5[8]), fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_7
          <= (z_out_5[7]) & (fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_6
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_5[6]), fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_5
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_5[5]), fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_4
          <= (z_out_5[4]) & (fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_3
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_5[3]), fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_2
          <= (z_out_5[2]) & (fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_1
          <= MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11_mx0w0[2]),
          (z_out_5[1]), fsm_output[6]);
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_0
          <= (z_out_5[0]) & (fsm_output[6]);
      HACC_stage_0 <= HACC_mux1h_4_nl | or_tmp_88;
      HACC_stage_0_2 <= (HACC_stage_0 & (~ or_tmp_88)) | (fsm_output[6]);
      HACC_stage_0_3 <= HACC_mux_11_nl & (~ (fsm_output[9]));
      HACC_ac_fixed_cctor_sva_1_15_14 <= MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_11_nl,
          HACC_not_nl);
      HACC_idx_acc_2_itm_1_1_0 <= MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_12_nl,
          HACC_idx_not_2_nl);
      HACC_idx_slc_HACC_t_1_0_1_itm_1 <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_2_dfm_1_15_14_1,
          (for_1_t_7_0_sva[1:0]), fsm_output[10]);
      HACC_idx_slc_HACC_t_1_0_1_itm_2 <= MUX_v_2_2_2((signext_2_1(z_out_6[26])),
          HACC_idx_slc_HACC_t_1_0_1_itm_1, fsm_output[10]);
      HACC_idx_slc_HACC_t_1_0_1_itm_3 <= MUX_v_2_2_2((signext_2_1(z_out_6[26])),
          HACC_idx_slc_HACC_t_1_0_1_itm_2, fsm_output[10]);
      HACC_idx_slc_HACC_t_1_0_1_itm_4 <= MUX1HOT_v_2_3_2(2'b01, (signext_2_1(z_out_11[2])),
          HACC_idx_slc_HACC_t_1_0_1_itm_3, {or_149_nl , and_272_nl , (fsm_output[10])});
      HACC_idx_slc_HACC_t_1_0_itm_5 <= MUX_v_2_2_2((signext_2_1(z_out_6[26])), HACC_idx_slc_HACC_t_1_0_1_itm_4,
          fsm_output[10]);
      HACC_idx_slc_HACC_t_1_0_itm_6 <= MUX_v_2_2_2((signext_2_1(z_out_6[26])), HACC_idx_slc_HACC_t_1_0_itm_5,
          fsm_output[10]);
      HACC_round_r_HACC_round_r_acc_conv_2f_or_itm_1 <= ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_13_nl
          & (~ or_tmp_97);
      reg_HACC_mul_1_itm_1_ftd <= HACC_mul_1_sdt[35:33];
      reg_HACC_mul_1_itm_1_ftd_1 <= MUX_s_1_2_2((z_out_5[32]), (HACC_mul_1_sdt[32]),
          fsm_output[10]);
      reg_HACC_mul_1_itm_1_ftd_2 <= MUX1HOT_v_32_3_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_nl,
          (z_out_5[31:0]), (HACC_mul_1_sdt[31:0]), {HACC_or_2_nl , HACC_and_4_nl
          , (fsm_output[10])});
      HACC_mul_itm_1 <= MUX_v_36_2_2(({3'b000 , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_mux_nl}),
          HACC_mul_nl, fsm_output[10]);
      HACC_stage_0_4 <= HACC_stage_0_3 & (fsm_output[10]);
      HACC_stage_0_5 <= HACC_stage_0_4 & (fsm_output[10]);
      HACC_stage_0_6 <= HACC_stage_0_5 & (fsm_output[10]);
      HACC_stage_0_7 <= HACC_stage_0_6 & (fsm_output[10]);
      HACC_stage_0_8 <= HACC_stage_0_7 & (fsm_output[10]);
      HACC_acc_5_itm_1 <= MUX_v_11_2_2((z_out_2[10:0]), z_out_8, and_64_cse);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      acc_rsci_idat <= 16'b0000000000000000;
    end
    else if ( core_wen & (fsm_output[13]) & HACC_stage_0_2 ) begin
      acc_rsci_idat <= acc_tmp_rsci_data_out_d_oreg[15:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs
          <= 1'b0;
    end
    else if ( core_wen & (~((~ (fsm_output[5])) | (z_out_6[26]))) ) begin
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs
          <= z_out_11[2];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HROW_y_sva <= 10'b0000000000;
    end
    else if ( core_wen & ((fsm_output[12]) | (fsm_output[8])) ) begin
      HROW_y_sva <= MUX_v_10_2_2(10'b0000000000, (z_out_8[9:0]), (fsm_output[12]));
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HACC_idx_18_2_sva_2 <= 17'b00000000000000000;
    end
    else if ( core_wen & HACC_stage_0_6 ) begin
      HACC_idx_18_2_sva_2 <= HACC_idx_18_2_sva_1;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1 <= 13'b0000000000000;
      HACC_idx_slc_HACC_t_7_2_itm_4 <= 6'b000000;
    end
    else if ( HACC_idx_and_1_cse ) begin
      HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1 <= HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1_1;
      HACC_idx_slc_HACC_t_7_2_itm_4 <= HACC_idx_slc_HACC_t_7_2_itm_3;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HACC_round_r_HACC_round_r_HACC_round_r_conc_trnz_26_16_sva_1 <= 11'b00000000000;
      HACC_idx_slc_HACC_t_7_2_itm_3 <= 6'b000000;
    end
    else if ( HACC_round_r_HACC_round_r_acc_conv_2f_and_1_cse ) begin
      HACC_round_r_HACC_round_r_HACC_round_r_conc_trnz_26_16_sva_1 <= nl_HACC_round_r_HACC_round_r_HACC_round_r_conc_trnz_26_16_sva_1[10:0];
      HACC_idx_slc_HACC_t_7_2_itm_3 <= HACC_idx_slc_HACC_t_7_2_itm_2;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HACC_idx_18_2_sva_1 <= 17'b00000000000000000;
    end
    else if ( core_wen & HACC_stage_0_5 ) begin
      HACC_idx_18_2_sva_1 <= z_out_5[16:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HACC_idx_slc_HACC_t_7_2_itm_2 <= 6'b000000;
    end
    else if ( core_wen & HACC_stage_0_2 ) begin
      HACC_idx_slc_HACC_t_7_2_itm_2 <= HACC_idx_slc_HACC_t_7_2_itm_1;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HACC_acc_4_itm_1 <= 12'b000000000000;
      HACC_idx_slc_HACC_t_7_2_itm_1 <= 6'b000000;
    end
    else if ( HACC_and_cse ) begin
      HACC_acc_4_itm_1 <= z_out_2[11:0];
      HACC_idx_slc_HACC_t_7_2_itm_1 <= for_1_t_7_0_sva[7:2];
    end
  end
  assign HCOL_x_mux_nl = MUX_v_11_2_2((WRITE_i_18_0_sva[10:0]), HACC_acc_5_itm_1,
      fsm_output[11]);
  assign HCOL_x_or_nl = (fsm_output[11:9]!=3'b000);
  assign HCOL_x_HCOL_x_and_nl = MUX_v_11_2_2(11'b00000000000, HCOL_x_mux_nl, HCOL_x_or_nl);
  assign or_89_nl = (fsm_output[11:8]!=4'b0000) | and_32_cse;
  assign WRITE_i_mux1h_3_nl = MUX1HOT_v_19_4_2(19'b1100001101001111111, z_out_2,
      ({8'b00000000 , HCOL_x_HCOL_x_and_nl}), (z_out_6[18:0]), {(fsm_output[1]) ,
      (fsm_output[2]) , or_89_nl , (fsm_output[13])});
  assign nor_13_nl = ~((fsm_output[14]) | (fsm_output[0]) | or_dcpl_28 | (fsm_output[7:5]!=3'b000)
      | and_69_cse);
  assign or_95_nl = or_dcpl_28 | (fsm_output[6]) | (fsm_output[8]) | (fsm_output[5]);
  assign for_1_t_mux1h_4_nl = MUX1HOT_v_8_3_2((z_out_8[7:0]), for_1_t_7_0_sva, z_out_11,
      {(fsm_output[7]) , or_95_nl , (fsm_output[10])});
  assign for_1_t_nor_nl = ~((fsm_output[2]) | (fsm_output[9]));
  assign nl_HACC_idx_acc_8_nl = (HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1_1[12:4])
      + 9'b000000001;
  assign HACC_idx_acc_8_nl = nl_HACC_idx_acc_8_nl[8:0];
  assign nl_HACC_idx_acc_6_nl = conv_s2u_11_13({HACC_idx_acc_8_nl , (HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1_1[3:2])})
      + (~ HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1_1);
  assign HACC_idx_acc_6_nl = nl_HACC_idx_acc_6_nl[12:0];
  assign or_99_nl = (fsm_output[6:5]!=2'b00);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_nl
      = MUX_v_2_2_2((signext_2_1(z_out_11[2])), 2'b01, z_out_6[26]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_or_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_and_cse
      | ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_and_1_cse;
  assign T_LINE_if_aelse_T_LINE_if_aelse_and_nl = HACC_stage_0 & (z_out[3]);
  assign and_6_nl = HACC_stage_0 & (z_out_7[6]);
  assign and_11_nl = HACC_stage_0 & WRITE_acc_itm_12_1;
  assign HACC_mux1h_4_nl = MUX1HOT_s_1_3_2(T_LINE_if_aelse_T_LINE_if_aelse_and_nl,
      and_6_nl, and_11_nl, {(fsm_output[6]) , (fsm_output[10]) , (fsm_output[13])});
  assign HACC_mux_11_nl = MUX_s_1_2_2((~ (z_out[3])), HACC_stage_0_2, fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_11_nl
      = MUX_v_2_2_2((z_out_5[15:14]), (z_out_10_35_8[17:16]), fsm_output[10]);
  assign HACC_not_nl = ~ or_tmp_97;
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_12_nl
      = MUX_v_2_2_2((z_out_5[28:27]), (HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1_1[1:0]),
      fsm_output[10]);
  assign HACC_idx_not_2_nl = ~ or_tmp_97;
  assign or_149_nl = (~((fsm_output[5]) | (fsm_output[10]))) | ((z_out_6[26]) & (fsm_output[5]));
  assign and_272_nl = (~ (z_out_6[26])) & (fsm_output[5]);
  assign HACC_round_r_HACC_round_r_acc_conv_2f_or_nl = (z_out_10_35_8[15:2]!=14'b00000000000000);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_mux_13_nl
      = MUX_s_1_2_2((z_out_6[35]), HACC_round_r_HACC_round_r_acc_conv_2f_or_nl, fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_nl
      = MUX_v_32_2_2(32'b00000000000000000000000000000000, (z_out_4[31:0]), (fsm_output[6]));
  assign HACC_or_2_nl = (fsm_output[5]) | ((z_out[3]) & (fsm_output[6]));
  assign HACC_and_4_nl = (~ (z_out[3])) & (fsm_output[6]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_mux_nl
      = MUX_v_33_2_2(z_out_9, z_out_3, z_out_6[35]);
  assign nl_HACC_mul_nl = $signed((cos_out_rsci_data_out_d_oreg)) * $signed(HACC_acc_4_itm_1);
  assign HACC_mul_nl = nl_HACC_mul_nl[35:0];
  assign nl_HACC_round_r_HACC_round_r_HACC_round_r_conc_trnz_26_16_sva_1  = conv_s2u_10_11(z_out_10_35_8[27:18])
      + 11'b00011100101;
  assign for_1_not_11_nl = ~ (fsm_output[6]);
  assign for_1_for_1_for_1_nand_2_nl = ~(MUX_v_8_2_2(8'b00000000, for_1_t_7_0_sva,
      for_1_not_11_nl));
  assign for_1_not_12_nl = ~ (fsm_output[6]);
  assign for_1_for_1_or_6_nl = MUX_v_2_2_2((z_out_1[4:3]), 2'b11, for_1_not_12_nl);
  assign for_1_mux_24_nl = MUX_s_1_2_2((~ (for_1_t_7_0_sva[7])), (z_out_1[2]), fsm_output[6]);
  assign for_1_or_5_nl = (~ (fsm_output[6])) | (fsm_output[3]);
  assign for_1_mux_25_nl = MUX_v_8_2_2((~ for_1_t_7_0_sva), 8'b00000001, fsm_output[6]);
  assign nl_acc_nl = ({for_1_for_1_for_1_nand_2_nl , 2'b11 , for_1_for_1_or_6_nl
      , for_1_mux_24_nl , for_1_or_5_nl}) + conv_s2u_10_14({(~ (fsm_output[6])) ,
      for_1_mux_25_nl , 1'b1});
  assign acc_nl = nl_acc_nl[13:0];
  assign z_out = readslicef_14_13_1(acc_nl);
  assign for_1_mux_26_nl = MUX_v_8_2_2(for_1_t_7_0_sva, (signext_8_1(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva[4])),
      fsm_output[6]);
  assign for_1_nor_6_nl = ~((fsm_output[12]) | (fsm_output[10]));
  assign for_1_for_1_and_8_nl = MUX_v_8_2_2(8'b00000000, for_1_mux_26_nl, for_1_nor_6_nl);
  assign for_1_mux_27_nl = MUX_v_6_2_2((signext_6_1(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva[4])),
      (acc_tmp_rsci_data_out_d_oreg[31:26]), fsm_output[10]);
  assign for_1_nor_7_nl = ~((fsm_output[4]) | (fsm_output[12]));
  assign for_1_for_1_and_9_nl = MUX_v_6_2_2(6'b000000, for_1_mux_27_nl, for_1_nor_7_nl);
  assign for_1_mux1h_4_nl = MUX1HOT_v_10_4_2(({for_1_t_7_0_sva , 2'b01}), ({{5{ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva[4]}},
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva}),
      operator_10_false_io_read_heightIn_rsc_cse_sva, (acc_tmp_rsci_data_out_d_oreg[25:16]),
      {(fsm_output[4]) , (fsm_output[6]) , (fsm_output[12]) , (fsm_output[10])});
  assign for_1_for_1_or_7_nl = or_tmp_97 | (fsm_output[4]) | (fsm_output[12]);
  assign for_1_and_1_nl = MUX_v_13_2_2(13'b0000000000000, HACC_idx_acc_2_itm_1_14_2,
      or_tmp_97);
  assign for_1_for_1_or_8_nl = MUX_v_13_2_2(for_1_and_1_nl, 13'b1111111111111, (fsm_output[12]));
  assign for_1_or_6_nl = (fsm_output[6]) | (fsm_output[10]);
  assign for_1_mux_28_nl = MUX_v_7_2_2((for_1_t_7_0_sva[6:0]), 7'b1111110, for_1_or_6_nl);
  assign for_1_not_15_nl = ~ (fsm_output[12]);
  assign for_1_for_1_for_1_nand_3_nl = ~(MUX_v_7_2_2(7'b0000000, for_1_mux_28_nl,
      for_1_not_15_nl));
  assign nl_z_out_1 = ({for_1_for_1_and_8_nl , for_1_for_1_and_9_nl , for_1_mux1h_4_nl})
      + conv_s2u_21_24({for_1_for_1_or_7_nl , for_1_for_1_or_8_nl , for_1_for_1_for_1_nand_3_nl});
  assign z_out_1 = nl_z_out_1[23:0];
  assign HACC_nor_2_nl = ~(and_64_cse | or_tmp_138);
  assign HACC_HACC_and_9_nl = MUX_v_8_2_2(8'b00000000, (WRITE_i_18_0_sva[18:11]),
      HACC_nor_2_nl);
  assign HACC_or_3_nl = (~((fsm_output[2]) | or_tmp_138)) | and_64_cse;
  assign HACC_HACC_or_3_nl = (~ or_tmp_138) | and_64_cse | (fsm_output[2]);
  assign HACC_mux_13_nl = MUX_v_10_2_2((operator_11_false_io_read_widthIn_rsc_cse_sva[10:1]),
      10'b1111111110, or_tmp_138);
  assign HACC_not_4_nl = ~ (fsm_output[2]);
  assign HACC_HACC_HACC_nand_1_nl = ~(MUX_v_10_2_2(10'b0000000000, HACC_mux_13_nl,
      HACC_not_4_nl));
  assign nl_acc_2_nl = ({HACC_HACC_and_9_nl , (WRITE_i_18_0_sva[10:0]) , HACC_or_3_nl})
      + conv_s2u_12_20({HACC_HACC_or_3_nl , HACC_HACC_HACC_nand_1_nl , 1'b1});
  assign acc_2_nl = nl_acc_2_nl[19:0];
  assign z_out_2 = readslicef_20_19_1(acc_2_nl);
  assign for_1_for_1_and_10_nl = MUX_v_3_2_2(3'b000, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_32_30,
      (fsm_output[6]));
  assign for_1_for_1_and_11_nl = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_29
      & (fsm_output[6]);
  assign for_1_for_1_and_12_nl = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_28
      & (fsm_output[6]);
  assign for_1_for_1_and_13_nl = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_27
      & (fsm_output[6]);
  assign for_1_for_1_and_14_nl = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_26_25,
      (fsm_output[6]));
  assign for_1_for_1_and_15_nl = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_24
      & (fsm_output[6]);
  assign for_1_mux_29_nl = MUX_v_2_2_2((z_out_1[23:22]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_23_22,
      fsm_output[6]);
  assign for_1_mux_30_nl = MUX_s_1_2_2((z_out_1[21]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_21,
      fsm_output[6]);
  assign for_1_mux_31_nl = MUX_v_3_2_2((z_out_1[20:18]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_20_18,
      fsm_output[6]);
  assign for_1_mux_32_nl = MUX_s_1_2_2((z_out_1[17]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_17,
      fsm_output[6]);
  assign for_1_mux_33_nl = MUX_s_1_2_2((z_out_1[16]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_16,
      fsm_output[6]);
  assign for_1_mux_34_nl = MUX_v_2_2_2((z_out_1[15:14]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_15_14,
      fsm_output[6]);
  assign for_1_mux_35_nl = MUX_v_3_2_2((z_out_1[13:11]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_13_11,
      fsm_output[6]);
  assign for_1_mux_36_nl = MUX_s_1_2_2((z_out_1[10]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_10,
      fsm_output[6]);
  assign for_1_mux_37_nl = MUX_v_2_2_2((z_out_1[9:8]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_9_8,
      fsm_output[6]);
  assign for_1_mux_38_nl = MUX_s_1_2_2((z_out_1[7]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_7,
      fsm_output[6]);
  assign for_1_mux_39_nl = MUX_v_2_2_2((z_out_1[6:5]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_6_5,
      fsm_output[6]);
  assign for_1_mux_40_nl = MUX_s_1_2_2((z_out_1[4]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_4,
      fsm_output[6]);
  assign for_1_mux_41_nl = MUX_s_1_2_2((z_out_1[3]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_3,
      fsm_output[6]);
  assign for_1_mux_42_nl = MUX_v_2_2_2((z_out_1[2:1]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_2_1,
      fsm_output[6]);
  assign for_1_mux_43_nl = MUX_s_1_2_2((z_out_1[0]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_0,
      fsm_output[6]);
  assign for_1_or_7_nl = (~ (fsm_output[4])) | (fsm_output[6]);
  assign for_1_mux_44_nl = MUX_v_33_2_2(({7'b0000000 , (for_1_t_7_0_sva[6:0]) , 6'b000000
      , for_1_t_7_0_sva , 5'b00000}), (~ ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_1),
      fsm_output[6]);
  assign nl_acc_3_nl = ({for_1_for_1_and_10_nl , for_1_for_1_and_11_nl , for_1_for_1_and_12_nl
      , for_1_for_1_and_13_nl , for_1_for_1_and_14_nl , for_1_for_1_and_15_nl , for_1_mux_29_nl
      , for_1_mux_30_nl , for_1_mux_31_nl , for_1_mux_32_nl , for_1_mux_33_nl , for_1_mux_34_nl
      , for_1_mux_35_nl , for_1_mux_36_nl , for_1_mux_37_nl , for_1_mux_38_nl , for_1_mux_39_nl
      , for_1_mux_40_nl , for_1_mux_41_nl , for_1_mux_42_nl , for_1_mux_43_nl , for_1_or_7_nl})
      + ({for_1_mux_44_nl , 1'b1});
  assign acc_3_nl = nl_acc_3_nl[33:0];
  assign z_out_3 = readslicef_34_33_1(acc_3_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_nand_1_nl
      = ~(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_and_cse
      & (~((z_out_6[35]) & (fsm_output[6]))));
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_3_nl
      = MUX_v_32_2_2((~ ac_math_atan_pi_2mi_return_69_38_sva_1), ac_math_atan_pi_2mi_return_69_38_sva_1,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_and_cse);
  assign nl_acc_4_nl = ({ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_1_34_32
      , reg_HACC_mul_1_itm_1_ftd_2 , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_nand_1_nl})
      + conv_s2u_34_36({(~ ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_and_cse)
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_3_nl
      , 1'b1});
  assign acc_4_nl = nl_acc_4_nl[35:0];
  assign z_out_4 = readslicef_36_35_1(acc_4_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_27_nl
      = MUX_v_3_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_32_30,
      (signext_3_1(z_out_6[16])), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_28_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_29,
      (z_out_6[16]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_29_nl
      = MUX_v_2_2_2(HACC_idx_acc_2_itm_1_1_0, (signext_2_1(z_out_6[16])), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_30_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_26,
      (z_out_6[16]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_31_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_25,
      (z_out_6[16]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_32_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_24,
      (z_out_6[16]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_33_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_23,
      (z_out_6[16]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_34_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_22,
      (z_out_6[16]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_35_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_21,
      (z_out_6[16]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_36_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_20,
      (z_out_6[16]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_37_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_19,
      (z_out_6[16]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_38_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_18,
      (z_out_6[16]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_39_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_17,
      (z_out_6[16]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_40_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_16,
      (z_out_6[16]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_41_nl
      = MUX_v_2_2_2(HACC_ac_fixed_cctor_sva_1_15_14, (z_out_6[15:14]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_42_nl
      = MUX_v_3_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_13_11,
      (z_out_6[13:11]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_43_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_10,
      (z_out_6[10]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_44_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_9,
      (z_out_6[9]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_45_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_8,
      (z_out_6[8]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_46_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_7,
      (z_out_6[7]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_47_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_6,
      (z_out_6[6]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_48_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_5,
      (z_out_6[5]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_49_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_4,
      (z_out_6[4]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_50_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_3,
      (z_out_6[3]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_51_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_2,
      (z_out_6[2]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_52_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_1,
      (z_out_6[1]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_53_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_2_dfm_1_0,
      (z_out_6[0]), fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_28_nl
      = (~(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_and_1_cse
      | (fsm_output[10]))) | ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_and_cse;
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_28_nl
      = MUX1HOT_v_33_3_2((~ ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva_1),
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva_1,
      ({16'b0000000000000000 , (~ HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1)
      , 4'b0100}), {ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_and_cse
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_and_1_cse
      , (fsm_output[10])});
  assign nl_acc_5_nl = ({ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_27_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_28_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_29_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_30_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_31_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_32_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_33_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_34_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_35_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_36_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_37_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_38_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_39_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_40_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_41_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_42_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_43_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_44_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_45_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_46_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_47_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_48_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_49_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_50_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_51_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_52_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_53_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_28_nl})
      + ({ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_28_nl
      , 1'b1});
  assign acc_5_nl = nl_acc_5_nl[33:0];
  assign z_out_5 = readslicef_34_33_1(acc_5_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_mux1h_2_nl
      = MUX1HOT_v_35_4_2((signext_35_26(~ ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_sva)),
      ({(~ ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_lpi_2_dfm_1_34_32)
      , (~ reg_HACC_mul_1_itm_1_ftd_2)}), ({18'b000000000000000000 , (HACC_round_r_HACC_round_r_acc_conv_2f_acc_atp_sva_1[10:0])
      , HACC_idx_slc_HACC_t_7_2_itm_4}), ({{16{WRITE_i_18_0_sva[18]}}, WRITE_i_18_0_sva}),
      {(fsm_output[5]) , (fsm_output[6]) , (fsm_output[10]) , (fsm_output[13])});
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_or_1_nl
      = (~((fsm_output[5]) | (fsm_output[10]) | (fsm_output[13]))) | (fsm_output[6]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_mux1h_3_nl
      = MUX1HOT_v_26_3_2(26'b00000000010000000000000000, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_sva,
      (signext_26_7(HACC_idx_acc_2_itm_1_14_2[12:6])), {(fsm_output[5]) , (fsm_output[6])
      , (fsm_output[10])});
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_not_3_nl
      = ~ (fsm_output[13]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_and_1_nl
      = MUX_v_26_2_2(26'b00000000000000000000000000, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_mux1h_3_nl,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_not_3_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_nor_3_nl
      = ~((fsm_output[5]) | (fsm_output[6]) | (fsm_output[13]));
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_and_2_nl
      = MUX_v_6_2_2(6'b000000, (HACC_idx_acc_2_itm_1_14_2[5:0]), ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_nor_3_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_mux_1_nl
      = MUX_v_2_2_2(2'b01, HACC_idx_acc_2_itm_1_1_0, fsm_output[10]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_not_4_nl
      = ~ (fsm_output[6]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_and_3_nl
      = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_mux_1_nl,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_not_4_nl);
  assign nl_acc_6_nl = conv_s2u_36_37({ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_mux1h_2_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_or_1_nl})
      + conv_s2u_35_37({ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_and_1_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_and_2_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_if_and_3_nl
      , 1'b1});
  assign acc_6_nl = nl_acc_6_nl[36:0];
  assign z_out_6 = readslicef_37_36_1(acc_6_nl);
  assign operator_11_false_mux_2_nl = MUX_v_11_2_2(operator_11_false_io_read_widthIn_rsc_cse_sva,
      ({5'b00000 , (z_out_11[7:2])}), fsm_output[10]);
  assign nl_z_out_7 = operator_11_false_mux_2_nl + conv_s2u_7_11({1'b1 , (~ (fsm_output[10]))
      , 1'b1 , (signext_2_1(~ (fsm_output[10]))) , 2'b11});
  assign z_out_7 = nl_z_out_7[10:0];
  assign for_1_or_9_cse = (~((fsm_output[7]) | (fsm_output[12]))) | (fsm_output[10]);
  assign for_1_or_8_nl = (fsm_output[12]) | (fsm_output[10]);
  assign for_1_mux_45_nl = MUX_v_10_2_2(({{2{for_1_t_7_0_sva[7]}}, for_1_t_7_0_sva}),
      HROW_y_sva, for_1_or_8_nl);
  assign for_1_for_1_mux_1_nl = MUX_v_9_2_2(9'b000000001, (~ (operator_10_false_io_read_heightIn_rsc_cse_sva[9:1])),
      fsm_output[10]);
  assign nl_acc_8_nl = conv_u2u_11_12({for_1_mux_45_nl , for_1_or_9_cse}) + conv_s2u_11_12({for_1_or_9_cse
      , for_1_for_1_mux_1_nl , 1'b1});
  assign acc_8_nl = nl_acc_8_nl[11:0];
  assign z_out_8 = readslicef_12_11_1(acc_8_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_17_nl
      = ~ (fsm_output[8]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_17_nl
      = MUX_v_3_2_2(3'b000, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_32_30,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_17_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_18_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_29
      & (~ (fsm_output[8]));
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_19_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_28
      & (~ (fsm_output[8]));
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_20_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_27
      & (~ (fsm_output[8]));
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_21_nl
      = ~ (fsm_output[8]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_21_nl
      = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_26_25,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_21_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_22_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_24
      & (~ (fsm_output[8]));
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_23_nl
      = ~ (fsm_output[8]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_23_nl
      = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_23_22,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_23_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_24_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_21
      & (~ (fsm_output[8]));
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_25_nl
      = ~ (fsm_output[8]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_25_nl
      = MUX_v_3_2_2(3'b000, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_20_18,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_25_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_26_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_17
      & (~ (fsm_output[8]));
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_27_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_16
      & (~ (fsm_output[8]));
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_28_nl
      = ~ (fsm_output[8]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_28_nl
      = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_15_14,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_28_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_29_nl
      = ~ (fsm_output[8]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_29_nl
      = MUX_v_3_2_2(3'b000, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_13_11,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_29_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_30_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_10
      & (~ (fsm_output[8]));
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_31_nl
      = ~ (fsm_output[8]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_31_nl
      = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_9_8,
      ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_not_31_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_32_nl
      = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_7
      & (~ (fsm_output[8]));
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_33_nl
      = (ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_6_5[1])
      & (~ (fsm_output[8]));
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_6_nl
      = MUX_s_1_2_2((ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_6_5[0]),
      (for_1_t_7_0_sva[7]), fsm_output[8]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_7_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_4,
      (for_1_t_7_0_sva[6]), fsm_output[8]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_8_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_3,
      (for_1_t_7_0_sva[5]), fsm_output[8]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_9_nl
      = MUX_v_2_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_2_1,
      (for_1_t_7_0_sva[4:3]), fsm_output[8]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_10_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_conc_28_0,
      (for_1_t_7_0_sva[2]), fsm_output[8]);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_11_nl
      = MUX_v_33_2_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva_1,
      33'b111111111111111111111111111010011, fsm_output[8]);
  assign nl_z_out_9 = ({ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_17_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_18_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_19_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_20_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_21_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_22_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_23_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_24_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_25_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_26_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_27_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_28_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_29_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_30_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_31_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_32_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_33_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_6_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_7_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_8_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_9_nl
      , ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_10_nl})
      + ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_11_nl;
  assign z_out_9 = nl_z_out_9[32:0];
  assign HACC_mux_14_nl = MUX_v_36_2_2(HACC_mul_itm_1, ({28'b0000000000000000000000000001
      , (~ data_in_rsci_idat_mxwt)}), fsm_output[9]);
  assign HACC_not_5_nl = ~ (fsm_output[9]);
  assign HACC_HACC_and_10_nl = MUX_v_3_2_2(3'b000, reg_HACC_mul_1_itm_1_ftd, HACC_not_5_nl);
  assign HACC_HACC_and_11_nl = reg_HACC_mul_1_itm_1_ftd_1 & (~ (fsm_output[9]));
  assign HACC_mux_15_nl = MUX_v_32_2_2(reg_HACC_mul_1_itm_1_ftd_2, 32'b00000000000000000000000011111011,
      fsm_output[9]);
  assign nl_HACC_acc_nl = HACC_mux_14_nl + ({HACC_HACC_and_10_nl , HACC_HACC_and_11_nl
      , HACC_mux_15_nl});
  assign HACC_acc_nl = nl_HACC_acc_nl[35:0];
  assign z_out_10_35_8 = readslicef_36_28_8(HACC_acc_nl);
  assign ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_2_nl
      = MUX_v_8_2_2((signext_8_2(ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_42_17_sva[25:24])),
      for_1_t_7_0_sva, fsm_output[10]);
  assign nl_z_out_11 = ac_math_ac_sincos_cordic_52_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_mux_2_nl
      + 8'b00000001;
  assign z_out_11 = nl_z_out_11[7:0];

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


  function automatic [9:0] MUX1HOT_v_10_4_2;
    input [9:0] input_3;
    input [9:0] input_2;
    input [9:0] input_1;
    input [9:0] input_0;
    input [3:0] sel;
    reg [9:0] result;
  begin
    result = input_0 & {10{sel[0]}};
    result = result | ( input_1 & {10{sel[1]}});
    result = result | ( input_2 & {10{sel[2]}});
    result = result | ( input_3 & {10{sel[3]}});
    MUX1HOT_v_10_4_2 = result;
  end
  endfunction


  function automatic [18:0] MUX1HOT_v_19_4_2;
    input [18:0] input_3;
    input [18:0] input_2;
    input [18:0] input_1;
    input [18:0] input_0;
    input [3:0] sel;
    reg [18:0] result;
  begin
    result = input_0 & {19{sel[0]}};
    result = result | ( input_1 & {19{sel[1]}});
    result = result | ( input_2 & {19{sel[2]}});
    result = result | ( input_3 & {19{sel[3]}});
    MUX1HOT_v_19_4_2 = result;
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


  function automatic [31:0] MUX1HOT_v_32_3_2;
    input [31:0] input_2;
    input [31:0] input_1;
    input [31:0] input_0;
    input [2:0] sel;
    reg [31:0] result;
  begin
    result = input_0 & {32{sel[0]}};
    result = result | ( input_1 & {32{sel[1]}});
    result = result | ( input_2 & {32{sel[2]}});
    MUX1HOT_v_32_3_2 = result;
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


  function automatic [34:0] MUX1HOT_v_35_4_2;
    input [34:0] input_3;
    input [34:0] input_2;
    input [34:0] input_1;
    input [34:0] input_0;
    input [3:0] sel;
    reg [34:0] result;
  begin
    result = input_0 & {35{sel[0]}};
    result = result | ( input_1 & {35{sel[1]}});
    result = result | ( input_2 & {35{sel[2]}});
    result = result | ( input_3 & {35{sel[3]}});
    MUX1HOT_v_35_4_2 = result;
  end
  endfunction


  function automatic [7:0] MUX1HOT_v_8_3_2;
    input [7:0] input_2;
    input [7:0] input_1;
    input [7:0] input_0;
    input [2:0] sel;
    reg [7:0] result;
  begin
    result = input_0 & {8{sel[0]}};
    result = result | ( input_1 & {8{sel[1]}});
    result = result | ( input_2 & {8{sel[2]}});
    MUX1HOT_v_8_3_2 = result;
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


  function automatic [12:0] MUX_v_13_2_2;
    input [12:0] input_0;
    input [12:0] input_1;
    input [0:0] sel;
    reg [12:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_13_2_2 = result;
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


  function automatic [25:0] MUX_v_26_2_2;
    input [25:0] input_0;
    input [25:0] input_1;
    input [0:0] sel;
    reg [25:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_26_2_2 = result;
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


  function automatic [35:0] MUX_v_36_2_2;
    input [35:0] input_0;
    input [35:0] input_1;
    input [0:0] sel;
    reg [35:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_36_2_2 = result;
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


  function automatic [8:0] MUX_v_9_2_2;
    input [8:0] input_0;
    input [8:0] input_1;
    input [0:0] sel;
    reg [8:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_9_2_2 = result;
  end
  endfunction


  function automatic [10:0] readslicef_12_11_1;
    input [11:0] vector;
    reg [11:0] tmp;
  begin
    tmp = vector >> 1;
    readslicef_12_11_1 = tmp[10:0];
  end
  endfunction


  function automatic [0:0] readslicef_13_1_12;
    input [12:0] vector;
    reg [12:0] tmp;
  begin
    tmp = vector >> 12;
    readslicef_13_1_12 = tmp[0:0];
  end
  endfunction


  function automatic [12:0] readslicef_14_13_1;
    input [13:0] vector;
    reg [13:0] tmp;
  begin
    tmp = vector >> 1;
    readslicef_14_13_1 = tmp[12:0];
  end
  endfunction


  function automatic [18:0] readslicef_20_19_1;
    input [19:0] vector;
    reg [19:0] tmp;
  begin
    tmp = vector >> 1;
    readslicef_20_19_1 = tmp[18:0];
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


  function automatic [27:0] readslicef_36_28_8;
    input [35:0] vector;
    reg [35:0] tmp;
  begin
    tmp = vector >> 8;
    readslicef_36_28_8 = tmp[27:0];
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


  function automatic [25:0] signext_26_7;
    input [6:0] vector;
  begin
    signext_26_7= {{19{vector[6]}}, vector};
  end
  endfunction


  function automatic [1:0] signext_2_1;
    input [0:0] vector;
  begin
    signext_2_1= {{1{vector[0]}}, vector};
  end
  endfunction


  function automatic [34:0] signext_35_26;
    input [25:0] vector;
  begin
    signext_35_26= {{9{vector[25]}}, vector};
  end
  endfunction


  function automatic [2:0] signext_3_1;
    input [0:0] vector;
  begin
    signext_3_1= {{2{vector[0]}}, vector};
  end
  endfunction


  function automatic [2:0] signext_3_2;
    input [1:0] vector;
  begin
    signext_3_2= {{1{vector[1]}}, vector};
  end
  endfunction


  function automatic [5:0] signext_6_1;
    input [0:0] vector;
  begin
    signext_6_1= {{5{vector[0]}}, vector};
  end
  endfunction


  function automatic [7:0] signext_8_1;
    input [0:0] vector;
  begin
    signext_8_1= {{7{vector[0]}}, vector};
  end
  endfunction


  function automatic [7:0] signext_8_2;
    input [1:0] vector;
  begin
    signext_8_2= {{6{vector[1]}}, vector};
  end
  endfunction


  function automatic [10:0] conv_s2u_7_11 ;
    input [6:0]  vector ;
  begin
    conv_s2u_7_11 = {{4{vector[6]}}, vector};
  end
  endfunction


  function automatic [10:0] conv_s2u_10_11 ;
    input [9:0]  vector ;
  begin
    conv_s2u_10_11 = {vector[9], vector};
  end
  endfunction


  function automatic [13:0] conv_s2u_10_14 ;
    input [9:0]  vector ;
  begin
    conv_s2u_10_14 = {{4{vector[9]}}, vector};
  end
  endfunction


  function automatic [11:0] conv_s2u_11_12 ;
    input [10:0]  vector ;
  begin
    conv_s2u_11_12 = {vector[10], vector};
  end
  endfunction


  function automatic [12:0] conv_s2u_11_13 ;
    input [10:0]  vector ;
  begin
    conv_s2u_11_13 = {{2{vector[10]}}, vector};
  end
  endfunction


  function automatic [19:0] conv_s2u_12_20 ;
    input [11:0]  vector ;
  begin
    conv_s2u_12_20 = {{8{vector[11]}}, vector};
  end
  endfunction


  function automatic [23:0] conv_s2u_21_24 ;
    input [20:0]  vector ;
  begin
    conv_s2u_21_24 = {{3{vector[20]}}, vector};
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


  function automatic [12:0] conv_u2s_1_13 ;
    input [0:0]  vector ;
  begin
    conv_u2s_1_13 = {{12{1'b0}}, vector};
  end
  endfunction


  function automatic [12:0] conv_u2s_12_13 ;
    input [11:0]  vector ;
  begin
    conv_u2s_12_13 =  {1'b0, vector};
  end
  endfunction


  function automatic [11:0] conv_u2u_11_12 ;
    input [10:0]  vector ;
  begin
    conv_u2u_11_12 = {1'b0, vector};
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
  wire [26:0] cos_out_1_rsci_data_in_d;
  wire [7:0] cos_out_1_rsci_addr_d;
  wire [1:0] cos_out_1_rsci_re_d;
  wire [1:0] cos_out_1_rsci_we_d;
  wire [53:0] cos_out_1_rsci_data_out_d;
  wire cos_out_1_rsci_en_d;
  wire [26:0] sin_out_1_rsci_data_in_d;
  wire [1:0] sin_out_1_rsci_re_d;
  wire [1:0] sin_out_1_rsci_we_d;
  wire [53:0] sin_out_1_rsci_data_out_d;
  wire [17:0] T_LINE_if_if_dividend1_mul_cmp_a;
  wire [26:0] T_LINE_if_if_dividend1_mul_cmp_b;
  wire cos_out_1_rsc_en;
  wire [53:0] cos_out_1_rsc_data_out;
  wire [1:0] cos_out_1_rsc_we;
  wire [1:0] cos_out_1_rsc_re;
  wire [15:0] cos_out_1_rsc_addr;
  wire [53:0] cos_out_1_rsc_data_in;
  wire sin_out_1_rsc_en;
  wire [53:0] sin_out_1_rsc_data_out;
  wire [1:0] sin_out_1_rsc_we;
  wire [1:0] sin_out_1_rsc_re;
  wire [15:0] sin_out_1_rsc_addr;
  wire [53:0] sin_out_1_rsc_data_in;


  // Interconnect Declarations for Component Instantiations 
  wire [53:0] nl_cos_out_1_rsci_data_in_d;
  assign nl_cos_out_1_rsci_data_in_d = {27'b000000000000000000000000000 , cos_out_1_rsci_data_in_d};
  wire [15:0] nl_cos_out_1_rsci_addr_d;
  assign nl_cos_out_1_rsci_addr_d = {8'b00000000 , cos_out_1_rsci_addr_d};
  wire [53:0] nl_sin_out_1_rsci_data_in_d;
  assign nl_sin_out_1_rsci_data_in_d = {27'b000000000000000000000000000 , sin_out_1_rsci_data_in_d};
  wire [15:0] nl_sin_out_1_rsci_addr_d;
  assign nl_sin_out_1_rsci_addr_d = {8'b00000000 , cos_out_1_rsci_addr_d};
  wire [43:0] nl_getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_z;
  assign nl_getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_z = conv_u2u_45_44($signed(T_LINE_if_if_dividend1_mul_cmp_a)
      * $signed(T_LINE_if_if_dividend1_mul_cmp_b));
  ram_sync_dualRW_be #(.ram_id(32'sd22),
  .words(32'sd180),
  .width(32'sd27),
  .addr_width(32'sd8),
  .a_reset_active(32'sd0),
  .s_reset_active(32'sd1),
  .enable_active(32'sd0),
  .re_active(32'sd0),
  .we_active(32'sd0),
  .num_byte_enables(32'sd1),
  .clock_edge(32'sd1),
  .no_of_RAM_dualRW_readwrite_port(32'sd2)) cos_out_1_rsc_comp (
      .data_in(cos_out_1_rsc_data_in),
      .addr(cos_out_1_rsc_addr),
      .re(cos_out_1_rsc_re),
      .we(cos_out_1_rsc_we),
      .data_out(cos_out_1_rsc_data_out),
      .clk(clk),
      .a_rst(1'b1),
      .s_rst(rst),
      .en(cos_out_1_rsc_en)
    );
  ram_sync_dualRW_be #(.ram_id(32'sd23),
  .words(32'sd180),
  .width(32'sd27),
  .addr_width(32'sd8),
  .a_reset_active(32'sd0),
  .s_reset_active(32'sd1),
  .enable_active(32'sd0),
  .re_active(32'sd0),
  .we_active(32'sd0),
  .num_byte_enables(32'sd1),
  .clock_edge(32'sd1),
  .no_of_RAM_dualRW_readwrite_port(32'sd2)) sin_out_1_rsc_comp (
      .data_in(sin_out_1_rsc_data_in),
      .addr(sin_out_1_rsc_addr),
      .re(sin_out_1_rsc_re),
      .we(sin_out_1_rsc_we),
      .data_out(sin_out_1_rsc_data_out),
      .clk(clk),
      .a_rst(1'b1),
      .s_rst(rst),
      .en(sin_out_1_rsc_en)
    );
  getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_22_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
      cos_out_1_rsci (
      .en(cos_out_1_rsc_en),
      .data_out(cos_out_1_rsc_data_out),
      .we(cos_out_1_rsc_we),
      .re(cos_out_1_rsc_re),
      .addr(cos_out_1_rsc_addr),
      .data_in(cos_out_1_rsc_data_in),
      .data_in_d(nl_cos_out_1_rsci_data_in_d[53:0]),
      .addr_d(nl_cos_out_1_rsci_addr_d[15:0]),
      .re_d(cos_out_1_rsci_re_d),
      .we_d(cos_out_1_rsci_we_d),
      .data_out_d(cos_out_1_rsci_data_out_d),
      .en_d(cos_out_1_rsci_en_d)
    );
  getMaxLine_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_23_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
      sin_out_1_rsci (
      .en(sin_out_1_rsc_en),
      .data_out(sin_out_1_rsc_data_out),
      .we(sin_out_1_rsc_we),
      .re(sin_out_1_rsc_re),
      .addr(sin_out_1_rsc_addr),
      .data_in(sin_out_1_rsc_data_in),
      .data_in_d(nl_sin_out_1_rsci_data_in_d[53:0]),
      .addr_d(nl_sin_out_1_rsci_addr_d[15:0]),
      .re_d(sin_out_1_rsci_re_d),
      .we_d(sin_out_1_rsci_we_d),
      .data_out_d(sin_out_1_rsci_data_out_d),
      .en_d(cos_out_1_rsci_en_d)
    );
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
      .cos_out_1_rsci_data_in_d(cos_out_1_rsci_data_in_d),
      .cos_out_1_rsci_addr_d(cos_out_1_rsci_addr_d),
      .cos_out_1_rsci_re_d(cos_out_1_rsci_re_d),
      .cos_out_1_rsci_we_d(cos_out_1_rsci_we_d),
      .cos_out_1_rsci_data_out_d(cos_out_1_rsci_data_out_d),
      .cos_out_1_rsci_en_d(cos_out_1_rsci_en_d),
      .sin_out_1_rsci_data_in_d(sin_out_1_rsci_data_in_d),
      .sin_out_1_rsci_re_d(sin_out_1_rsci_re_d),
      .sin_out_1_rsci_we_d(sin_out_1_rsci_we_d),
      .sin_out_1_rsci_data_out_d(sin_out_1_rsci_data_out_d),
      .T_LINE_if_if_dividend1_mul_cmp_a(T_LINE_if_if_dividend1_mul_cmp_a),
      .T_LINE_if_if_dividend1_mul_cmp_b(T_LINE_if_if_dividend1_mul_cmp_b),
      .T_LINE_if_if_dividend1_mul_cmp_z(nl_getMaxLine_core_inst_T_LINE_if_if_dividend1_mul_cmp_z[43:0])
    );

  function automatic [43:0] conv_u2u_45_44 ;
    input [44:0]  vector ;
  begin
    conv_u2u_45_44 = vector[43:0];
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
  wire [37:0] acc_tmp_rsci_addr_d;
  wire [1:0] acc_tmp_rsci_re_d;
  wire [1:0] acc_tmp_rsci_we_d;
  wire [31:0] acc_tmp_rsci_data_out_d;
  wire acc_tmp_rsci_en_d;
  wire [26:0] cos_out_rsci_data_in_d;
  wire [7:0] cos_out_rsci_addr_d;
  wire [1:0] cos_out_rsci_re_d;
  wire [1:0] cos_out_rsci_we_d;
  wire [53:0] cos_out_rsci_data_out_d;
  wire cos_out_rsci_en_d;
  wire [26:0] sin_out_rsci_data_in_d;
  wire [1:0] sin_out_rsci_re_d;
  wire [1:0] sin_out_rsci_we_d;
  wire [53:0] sin_out_rsci_data_out_d;
  wire acc_tmp_rsc_en;
  wire [31:0] acc_tmp_rsc_data_out;
  wire [1:0] acc_tmp_rsc_we;
  wire [1:0] acc_tmp_rsc_re;
  wire [37:0] acc_tmp_rsc_addr;
  wire [31:0] acc_tmp_rsc_data_in;
  wire cos_out_rsc_en;
  wire [53:0] cos_out_rsc_data_out;
  wire [1:0] cos_out_rsc_we;
  wire [1:0] cos_out_rsc_re;
  wire [15:0] cos_out_rsc_addr;
  wire [53:0] cos_out_rsc_data_in;
  wire sin_out_rsc_en;
  wire [53:0] sin_out_rsc_data_out;
  wire [1:0] sin_out_rsc_we;
  wire [1:0] sin_out_rsc_re;
  wire [15:0] sin_out_rsc_addr;
  wire [53:0] sin_out_rsc_data_in;


  // Interconnect Declarations for Component Instantiations 
  wire [31:0] nl_acc_tmp_rsci_data_in_d;
  assign nl_acc_tmp_rsci_data_in_d = {16'b0000000000000000 , acc_tmp_rsci_data_in_d};
  wire [53:0] nl_cos_out_rsci_data_in_d;
  assign nl_cos_out_rsci_data_in_d = {27'b000000000000000000000000000 , cos_out_rsci_data_in_d};
  wire [15:0] nl_cos_out_rsci_addr_d;
  assign nl_cos_out_rsci_addr_d = {8'b00000000 , cos_out_rsci_addr_d};
  wire [53:0] nl_sin_out_rsci_data_in_d;
  assign nl_sin_out_rsci_data_in_d = {27'b000000000000000000000000000 , sin_out_rsci_data_in_d};
  wire [15:0] nl_sin_out_rsci_addr_d;
  assign nl_sin_out_rsci_addr_d = {8'b00000000 , cos_out_rsci_addr_d};
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
  ram_sync_dualRW_be #(.ram_id(32'sd14),
  .words(32'sd180),
  .width(32'sd27),
  .addr_width(32'sd8),
  .a_reset_active(32'sd0),
  .s_reset_active(32'sd1),
  .enable_active(32'sd0),
  .re_active(32'sd0),
  .we_active(32'sd0),
  .num_byte_enables(32'sd1),
  .clock_edge(32'sd1),
  .no_of_RAM_dualRW_readwrite_port(32'sd2)) cos_out_rsc_comp (
      .data_in(cos_out_rsc_data_in),
      .addr(cos_out_rsc_addr),
      .re(cos_out_rsc_re),
      .we(cos_out_rsc_we),
      .data_out(cos_out_rsc_data_out),
      .clk(clk),
      .a_rst(1'b1),
      .s_rst(rst),
      .en(cos_out_rsc_en)
    );
  ram_sync_dualRW_be #(.ram_id(32'sd15),
  .words(32'sd180),
  .width(32'sd27),
  .addr_width(32'sd8),
  .a_reset_active(32'sd0),
  .s_reset_active(32'sd1),
  .enable_active(32'sd0),
  .re_active(32'sd0),
  .we_active(32'sd0),
  .num_byte_enables(32'sd1),
  .clock_edge(32'sd1),
  .no_of_RAM_dualRW_readwrite_port(32'sd2)) sin_out_rsc_comp (
      .data_in(sin_out_rsc_data_in),
      .addr(sin_out_rsc_addr),
      .re(sin_out_rsc_re),
      .we(sin_out_rsc_we),
      .data_out(sin_out_rsc_data_out),
      .clk(clk),
      .a_rst(1'b1),
      .s_rst(rst),
      .en(sin_out_rsc_en)
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
      .addr_d(acc_tmp_rsci_addr_d),
      .re_d(acc_tmp_rsci_re_d),
      .we_d(acc_tmp_rsci_we_d),
      .data_out_d(acc_tmp_rsci_data_out_d),
      .en_d(acc_tmp_rsci_en_d)
    );
  houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_14_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
      cos_out_rsci (
      .en(cos_out_rsc_en),
      .data_out(cos_out_rsc_data_out),
      .we(cos_out_rsc_we),
      .re(cos_out_rsc_re),
      .addr(cos_out_rsc_addr),
      .data_in(cos_out_rsc_data_in),
      .data_in_d(nl_cos_out_rsci_data_in_d[53:0]),
      .addr_d(nl_cos_out_rsci_addr_d[15:0]),
      .re_d(cos_out_rsci_re_d),
      .we_d(cos_out_rsci_we_d),
      .data_out_d(cos_out_rsci_data_out_d),
      .en_d(cos_out_rsci_en_d)
    );
  houghTransform_ram_nangate_45nm_dualport_beh_RAM_dualRW_rwport_en_15_180_27_8_0_1_0_0_0_1_1_27_180_2_gen
      sin_out_rsci (
      .en(sin_out_rsc_en),
      .data_out(sin_out_rsc_data_out),
      .we(sin_out_rsc_we),
      .re(sin_out_rsc_re),
      .addr(sin_out_rsc_addr),
      .data_in(sin_out_rsc_data_in),
      .data_in_d(nl_sin_out_rsci_data_in_d[53:0]),
      .addr_d(nl_sin_out_rsci_addr_d[15:0]),
      .re_d(sin_out_rsci_re_d),
      .we_d(sin_out_rsci_we_d),
      .data_out_d(sin_out_rsci_data_out_d),
      .en_d(cos_out_rsci_en_d)
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
      .acc_tmp_rsci_en_d(acc_tmp_rsci_en_d),
      .cos_out_rsci_data_in_d(cos_out_rsci_data_in_d),
      .cos_out_rsci_addr_d(cos_out_rsci_addr_d),
      .cos_out_rsci_re_d(cos_out_rsci_re_d),
      .cos_out_rsci_we_d(cos_out_rsci_we_d),
      .cos_out_rsci_data_out_d(cos_out_rsci_data_out_d),
      .cos_out_rsci_en_d(cos_out_rsci_en_d),
      .sin_out_rsci_data_in_d(sin_out_rsci_data_in_d),
      .sin_out_rsci_re_d(sin_out_rsci_re_d),
      .sin_out_rsci_we_d(sin_out_rsci_we_d),
      .sin_out_rsci_data_out_d(sin_out_rsci_data_out_d)
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
  ccs_pipe_v5 #(.rscid(32'sd25),
  .width(32'sd16),
  .sz_width(32'sd1),
  .fifo_sz(32'sd8),
  .log2_sz(32'sd3),
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



