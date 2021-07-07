
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
//  Generated date: Wed Jul  7 19:07:17 2021
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
//  Generated date: Wed Jul  7 19:07:17 2021
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
//  Generated date: Wed Jul  7 19:07:17 2021
// ----------------------------------------------------------------------

// 
// ------------------------------------------------------------------
//  Design Unit:    getMaxLine_core_core_fsm
//  FSM Module
// ------------------------------------------------------------------


module getMaxLine_core_core_fsm (
  clk, rst, core_wen, fsm_output, T_LINE_C_4_tr0, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1_tr0,
      T_LINE_C_10_tr0, T_LINE_C_10_tr1, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0,
      T_LINE_C_14_tr0, T_LINE_C_14_tr1, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0,
      T_LINE_C_18_tr0, R_LINE_C_0_tr0
);
  input clk;
  input rst;
  input core_wen;
  output [27:0] fsm_output;
  reg [27:0] fsm_output;
  input T_LINE_C_4_tr0;
  input ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1_tr0;
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
    ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0 = 5'd6,
    ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1 = 5'd7,
    T_LINE_C_5 = 5'd8,
    T_LINE_C_6 = 5'd9,
    T_LINE_C_7 = 5'd10,
    T_LINE_C_8 = 5'd11,
    T_LINE_C_9 = 5'd12,
    T_LINE_C_10 = 5'd13,
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0
        = 5'd14,
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0
        = 5'd15,
    T_LINE_C_11 = 5'd16,
    T_LINE_C_12 = 5'd17,
    T_LINE_C_13 = 5'd18,
    T_LINE_C_14 = 5'd19,
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0
        = 5'd20,
    ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0
        = 5'd21,
    T_LINE_C_15 = 5'd22,
    T_LINE_C_16 = 5'd23,
    T_LINE_C_17 = 5'd24,
    T_LINE_C_18 = 5'd25,
    R_LINE_C_0 = 5'd26,
    main_C_1 = 5'd27;

  reg [4:0] state_var;
  reg [4:0] state_var_NS;


  // Interconnect Declarations for Component Instantiations 
  always @(*)
  begin : getMaxLine_core_core_fsm_1
    case (state_var)
      T_LINE_C_0 : begin
        fsm_output = 28'b0000000000000000000000000010;
        state_var_NS = T_LINE_C_1;
      end
      T_LINE_C_1 : begin
        fsm_output = 28'b0000000000000000000000000100;
        state_var_NS = T_LINE_C_2;
      end
      T_LINE_C_2 : begin
        fsm_output = 28'b0000000000000000000000001000;
        state_var_NS = T_LINE_C_3;
      end
      T_LINE_C_3 : begin
        fsm_output = 28'b0000000000000000000000010000;
        state_var_NS = T_LINE_C_4;
      end
      T_LINE_C_4 : begin
        fsm_output = 28'b0000000000000000000000100000;
        if ( T_LINE_C_4_tr0 ) begin
          state_var_NS = T_LINE_C_5;
        end
        else begin
          state_var_NS = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0;
        end
      end
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0 :
          begin
        fsm_output = 28'b0000000000000000000001000000;
        state_var_NS = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1;
      end
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1 :
          begin
        fsm_output = 28'b0000000000000000000010000000;
        if ( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1_tr0
            ) begin
          state_var_NS = T_LINE_C_5;
        end
        else begin
          state_var_NS = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0;
        end
      end
      T_LINE_C_5 : begin
        fsm_output = 28'b0000000000000000000100000000;
        state_var_NS = T_LINE_C_6;
      end
      T_LINE_C_6 : begin
        fsm_output = 28'b0000000000000000001000000000;
        state_var_NS = T_LINE_C_7;
      end
      T_LINE_C_7 : begin
        fsm_output = 28'b0000000000000000010000000000;
        state_var_NS = T_LINE_C_8;
      end
      T_LINE_C_8 : begin
        fsm_output = 28'b0000000000000000100000000000;
        state_var_NS = T_LINE_C_9;
      end
      T_LINE_C_9 : begin
        fsm_output = 28'b0000000000000001000000000000;
        state_var_NS = T_LINE_C_10;
      end
      T_LINE_C_10 : begin
        fsm_output = 28'b0000000000000010000000000000;
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
        fsm_output = 28'b0000000000000100000000000000;
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
        fsm_output = 28'b0000000000001000000000000000;
        if ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
            ) begin
          state_var_NS = T_LINE_C_11;
        end
        else begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0;
        end
      end
      T_LINE_C_11 : begin
        fsm_output = 28'b0000000000010000000000000000;
        state_var_NS = T_LINE_C_12;
      end
      T_LINE_C_12 : begin
        fsm_output = 28'b0000000000100000000000000000;
        state_var_NS = T_LINE_C_13;
      end
      T_LINE_C_13 : begin
        fsm_output = 28'b0000000001000000000000000000;
        state_var_NS = T_LINE_C_14;
      end
      T_LINE_C_14 : begin
        fsm_output = 28'b0000000010000000000000000000;
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
        fsm_output = 28'b0000000100000000000000000000;
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
        fsm_output = 28'b0000001000000000000000000000;
        if ( ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
            ) begin
          state_var_NS = T_LINE_C_15;
        end
        else begin
          state_var_NS = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0;
        end
      end
      T_LINE_C_15 : begin
        fsm_output = 28'b0000010000000000000000000000;
        state_var_NS = T_LINE_C_16;
      end
      T_LINE_C_16 : begin
        fsm_output = 28'b0000100000000000000000000000;
        state_var_NS = T_LINE_C_17;
      end
      T_LINE_C_17 : begin
        fsm_output = 28'b0001000000000000000000000000;
        state_var_NS = T_LINE_C_18;
      end
      T_LINE_C_18 : begin
        fsm_output = 28'b0010000000000000000000000000;
        if ( T_LINE_C_18_tr0 ) begin
          state_var_NS = R_LINE_C_0;
        end
        else begin
          state_var_NS = T_LINE_C_0;
        end
      end
      R_LINE_C_0 : begin
        fsm_output = 28'b0100000000000000000000000000;
        if ( R_LINE_C_0_tr0 ) begin
          state_var_NS = main_C_1;
        end
        else begin
          state_var_NS = T_LINE_C_0;
        end
      end
      main_C_1 : begin
        fsm_output = 28'b1000000000000000000000000000;
        state_var_NS = main_C_0;
      end
      // main_C_0
      default : begin
        fsm_output = 28'b0000000000000000000000000001;
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
  clk, rst, core_wen, fsm_output, acc_tmp_vinit_C_0_tr0, HCOL_C_0_tr0, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1_tr0,
      HACC_C_15_tr0, HCOL_C_1_tr0, HROW_C_0_tr0, WRITE_C_2_tr0
);
  input clk;
  input rst;
  input core_wen;
  output [27:0] fsm_output;
  reg [27:0] fsm_output;
  input acc_tmp_vinit_C_0_tr0;
  input HCOL_C_0_tr0;
  input ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1_tr0;
  input HACC_C_15_tr0;
  input HCOL_C_1_tr0;
  input HROW_C_0_tr0;
  input WRITE_C_2_tr0;


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
    HACC_C_4 = 5'd10,
    HACC_C_5 = 5'd11,
    HACC_C_6 = 5'd12,
    HACC_C_7 = 5'd13,
    HACC_C_8 = 5'd14,
    HACC_C_9 = 5'd15,
    HACC_C_10 = 5'd16,
    HACC_C_11 = 5'd17,
    HACC_C_12 = 5'd18,
    HACC_C_13 = 5'd19,
    HACC_C_14 = 5'd20,
    HACC_C_15 = 5'd21,
    HCOL_C_1 = 5'd22,
    HROW_C_0 = 5'd23,
    WRITE_C_0 = 5'd24,
    WRITE_C_1 = 5'd25,
    WRITE_C_2 = 5'd26,
    main_C_1 = 5'd27;

  reg [4:0] state_var;
  reg [4:0] state_var_NS;


  // Interconnect Declarations for Component Instantiations 
  always @(*)
  begin : houghTransform_core_core_fsm_1
    case (state_var)
      main_C_0 : begin
        fsm_output = 28'b0000000000000000000000000010;
        state_var_NS = acc_tmp_vinit_C_0;
      end
      acc_tmp_vinit_C_0 : begin
        fsm_output = 28'b0000000000000000000000000100;
        if ( acc_tmp_vinit_C_0_tr0 ) begin
          state_var_NS = HCOL_C_0;
        end
        else begin
          state_var_NS = acc_tmp_vinit_C_0;
        end
      end
      HCOL_C_0 : begin
        fsm_output = 28'b0000000000000000000000001000;
        if ( HCOL_C_0_tr0 ) begin
          state_var_NS = HCOL_C_1;
        end
        else begin
          state_var_NS = HACC_C_0;
        end
      end
      HACC_C_0 : begin
        fsm_output = 28'b0000000000000000000000010000;
        state_var_NS = HACC_C_1;
      end
      HACC_C_1 : begin
        fsm_output = 28'b0000000000000000000000100000;
        state_var_NS = HACC_C_2;
      end
      HACC_C_2 : begin
        fsm_output = 28'b0000000000000000000001000000;
        state_var_NS = HACC_C_3;
      end
      HACC_C_3 : begin
        fsm_output = 28'b0000000000000000000010000000;
        state_var_NS = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0;
      end
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0 :
          begin
        fsm_output = 28'b0000000000000000000100000000;
        state_var_NS = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1;
      end
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1 :
          begin
        fsm_output = 28'b0000000000000000001000000000;
        if ( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1_tr0
            ) begin
          state_var_NS = HACC_C_4;
        end
        else begin
          state_var_NS = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_0;
        end
      end
      HACC_C_4 : begin
        fsm_output = 28'b0000000000000000010000000000;
        state_var_NS = HACC_C_5;
      end
      HACC_C_5 : begin
        fsm_output = 28'b0000000000000000100000000000;
        state_var_NS = HACC_C_6;
      end
      HACC_C_6 : begin
        fsm_output = 28'b0000000000000001000000000000;
        state_var_NS = HACC_C_7;
      end
      HACC_C_7 : begin
        fsm_output = 28'b0000000000000010000000000000;
        state_var_NS = HACC_C_8;
      end
      HACC_C_8 : begin
        fsm_output = 28'b0000000000000100000000000000;
        state_var_NS = HACC_C_9;
      end
      HACC_C_9 : begin
        fsm_output = 28'b0000000000001000000000000000;
        state_var_NS = HACC_C_10;
      end
      HACC_C_10 : begin
        fsm_output = 28'b0000000000010000000000000000;
        state_var_NS = HACC_C_11;
      end
      HACC_C_11 : begin
        fsm_output = 28'b0000000000100000000000000000;
        state_var_NS = HACC_C_12;
      end
      HACC_C_12 : begin
        fsm_output = 28'b0000000001000000000000000000;
        state_var_NS = HACC_C_13;
      end
      HACC_C_13 : begin
        fsm_output = 28'b0000000010000000000000000000;
        state_var_NS = HACC_C_14;
      end
      HACC_C_14 : begin
        fsm_output = 28'b0000000100000000000000000000;
        state_var_NS = HACC_C_15;
      end
      HACC_C_15 : begin
        fsm_output = 28'b0000001000000000000000000000;
        if ( HACC_C_15_tr0 ) begin
          state_var_NS = HCOL_C_1;
        end
        else begin
          state_var_NS = HACC_C_0;
        end
      end
      HCOL_C_1 : begin
        fsm_output = 28'b0000010000000000000000000000;
        if ( HCOL_C_1_tr0 ) begin
          state_var_NS = HROW_C_0;
        end
        else begin
          state_var_NS = HCOL_C_0;
        end
      end
      HROW_C_0 : begin
        fsm_output = 28'b0000100000000000000000000000;
        if ( HROW_C_0_tr0 ) begin
          state_var_NS = WRITE_C_0;
        end
        else begin
          state_var_NS = HCOL_C_0;
        end
      end
      WRITE_C_0 : begin
        fsm_output = 28'b0001000000000000000000000000;
        state_var_NS = WRITE_C_1;
      end
      WRITE_C_1 : begin
        fsm_output = 28'b0010000000000000000000000000;
        state_var_NS = WRITE_C_2;
      end
      WRITE_C_2 : begin
        fsm_output = 28'b0100000000000000000000000000;
        if ( WRITE_C_2_tr0 ) begin
          state_var_NS = main_C_1;
        end
        else begin
          state_var_NS = WRITE_C_0;
        end
      end
      main_C_1 : begin
        fsm_output = 28'b1000000000000000000000000000;
        state_var_NS = main_C_0;
      end
      // core_rlp_C_0
      default : begin
        fsm_output = 28'b0000000000000000000000000001;
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
  wire [27:0] fsm_output;
  wire or_dcpl_4;
  wire or_dcpl_13;
  wire or_dcpl_73;
  wire or_dcpl_77;
  wire or_dcpl_79;
  wire or_dcpl_83;
  wire or_dcpl_87;
  wire or_dcpl_88;
  wire or_dcpl_89;
  wire or_dcpl_92;
  wire or_dcpl_98;
  wire not_tmp_63;
  wire not_tmp_65;
  wire and_dcpl_72;
  wire and_dcpl_74;
  wire or_dcpl_117;
  wire or_tmp_72;
  wire and_395_cse;
  reg operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs;
  reg T_LINE_slc_T_LINE_acc_6_itm;
  reg T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
  reg operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs;
  reg operator_27_3_true_AC_TRN_AC_WRAP_return_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva;
  reg [26:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_32_6;
  reg [19:0] T_LINE_if_if_dividend1_sva;
  reg [19:0] T_LINE_if_else_dividend1_sva;
  reg [19:0] T_LINE_if_if_dividend2_sva;
  reg [19:0] T_LINE_if_else_dividend2_sva;
  reg [8:0] reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11;
  reg reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_ac_fixed_cctor_20_1_ftd_11;
  reg [26:0] reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd;
  reg [5:0] reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd_1;
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
  wire T_LINE_asn_48;
  wire T_LINE_asn_46;
  reg [10:0] R_LINE_r_10_0_sva;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_19_sva;
  reg [26:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva;
  reg [2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_32_30;
  reg [2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_32_30;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_29;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_28_27;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_28_27;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_26;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_26_25;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_25;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_24;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_23_22;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_23;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_22;
  reg [2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_20_18;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_21;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_20;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_19;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_15_14;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_18;
  reg [2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_13_11;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_17;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_16;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_9_8;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_15_14;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_13;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_6_5;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_12;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_11;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_10;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_9;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_8;
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_7;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_6;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_5;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_4;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_3;
  reg [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_2_1;
  reg ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_0;
  wire mux_19_itm;
  wire mux_20_itm;
  wire or_tmp_581;
  wire or_tmp_583;
  wire [16:0] z_out;
  wire [10:0] z_out_1;
  wire [11:0] nl_z_out_1;
  wire or_tmp_599;
  wire or_tmp_600;
  wire [34:0] z_out_2;
  wire or_tmp_602;
  wire or_tmp_603;
  wire [34:0] z_out_3;
  wire [35:0] nl_z_out_3;
  wire [35:0] z_out_4;
  wire or_tmp_611;
  wire or_tmp_613;
  wire [43:0] z_out_5;
  wire [44:0] nl_z_out_5;
  wire [32:0] z_out_6;
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
  reg [4:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva;
  reg [4:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_1;
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
  reg ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm;
  reg [3:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_35_18_sva_7_4;
  reg [3:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_35_18_sva_3_0;
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
  wire [32:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_1;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_1;
  wire [1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_2_1_mx0;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1;
  wire [4:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_1_mx0w1;
  wire [5:0] nl_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_1_mx0w1;
  wire [31:0] ac_math_atan_pi_2mi_return_1_69_38_sva_1;
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
  reg reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd;
  reg [25:0] reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_37_m1c;
  wire and_389_rgt;
  wire and_452_rgt;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_or_rgt;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_or_cse;
  wire nor_cse;
  wire or_731_cse;
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
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_and_64_cse;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_26_cse;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_27_cse;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_22_cse;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_23_cse;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_24_cse;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_25_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse;
  wire [12:0] T_LINE_if_mux1h_4_rgt;
  wire [12:0] T_LINE_if_mux1h_8_rgt;
  wire [34:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_mux1h_1_rgt;
  reg [2:0] T_LINE_if_acc_2_itm_16_14;
  reg T_LINE_if_acc_2_itm_13;
  reg [8:0] T_LINE_if_acc_2_itm_12_4;
  reg [2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_34_32;
  reg [31:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0;
  reg [8:0] T_LINE_if_acc_1_itm_12_4;
  reg [3:0] T_LINE_if_acc_1_itm_3_0;
  wire and_1330_cse;
  wire or_795_cse;
  wire or_791_cse;
  wire T_LINE_if_if_dividend1_or_itm;
  wire T_LINE_if_if_dividend1_or_1_itm;
  wire T_LINE_if_or_3_itm;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_nor_itm;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_itm;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  wire ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_6_cse;
  wire T_LINE_if_nor_1_cse;

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
  wire[0:0] mux_21_nl;
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
  wire[0:0] or_316_nl;
  wire[8:0] operator_60_32_true_AC_TRN_AC_WRAP_acc_2_nl;
  wire[9:0] nl_operator_60_32_true_AC_TRN_AC_WRAP_acc_2_nl;
  wire[0:0] or_318_nl;
  wire[0:0] or_319_nl;
  wire[0:0] mux_22_nl;
  wire[0:0] or_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_mux1h_4_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux_2_nl;
  wire[0:0] Hough_Algorithm_HW_1296_864_getMaxLine_not_11_nl;
  wire[0:0] Hough_Algorithm_HW_1296_864_getMaxLine_not_10_nl;
  wire[0:0] Hough_Algorithm_HW_1296_864_getMaxLine_not_9_nl;
  wire[0:0] Hough_Algorithm_HW_1296_864_getMaxLine_not_8_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_92_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_93_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_94_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_95_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_96_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_97_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_98_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_99_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_1_nl;
  wire[31:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_nl;
  wire[0:0] and_848_nl;
  wire[0:0] and_850_nl;
  wire[2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_mux_nl;
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
  wire[0:0] not_369_nl;
  wire[0:0] mux_23_nl;
  wire[27:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_if_1_acc_nl;
  wire[28:0] nl_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_if_1_acc_nl;
  wire[0:0] or_163_nl;
  wire[0:0] or_165_nl;
  wire[17:0] acc_nl;
  wire[18:0] nl_acc_nl;
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
  wire[10:0] T_LINE_mux1h_1_nl;
  wire[35:0] acc_2_nl;
  wire[36:0] nl_acc_2_nl;
  wire[4:0] T_LINE_if_and_22_nl;
  wire[4:0] T_LINE_if_T_LINE_if_mux_13_nl;
  wire[0:0] T_LINE_if_and_23_nl;
  wire[0:0] T_LINE_if_T_LINE_if_mux_14_nl;
  wire[1:0] T_LINE_if_and_24_nl;
  wire[1:0] T_LINE_if_T_LINE_if_mux_15_nl;
  wire[1:0] T_LINE_if_and_25_nl;
  wire[1:0] T_LINE_if_T_LINE_if_mux_16_nl;
  wire[0:0] T_LINE_if_and_26_nl;
  wire[0:0] T_LINE_if_T_LINE_if_mux_17_nl;
  wire[1:0] T_LINE_if_and_27_nl;
  wire[1:0] T_LINE_if_T_LINE_if_mux_18_nl;
  wire[0:0] T_LINE_if_and_28_nl;
  wire[0:0] T_LINE_if_T_LINE_if_mux_19_nl;
  wire[2:0] T_LINE_if_and_29_nl;
  wire[2:0] T_LINE_if_T_LINE_if_mux_20_nl;
  wire[0:0] T_LINE_if_and_30_nl;
  wire[0:0] T_LINE_if_T_LINE_if_mux_21_nl;
  wire[0:0] T_LINE_if_or_24_nl;
  wire[0:0] T_LINE_if_T_LINE_if_mux_22_nl;
  wire[1:0] T_LINE_if_and_32_nl;
  wire[1:0] T_LINE_if_T_LINE_if_mux_23_nl;
  wire[2:0] T_LINE_if_and_33_nl;
  wire[2:0] T_LINE_if_mux1h_45_nl;
  wire[0:0] T_LINE_if_not_9_nl;
  wire[0:0] T_LINE_if_and_34_nl;
  wire[0:0] T_LINE_if_T_LINE_if_mux_24_nl;
  wire[1:0] T_LINE_if_and_35_nl;
  wire[1:0] T_LINE_if_T_LINE_if_mux_25_nl;
  wire[0:0] T_LINE_if_and_36_nl;
  wire[0:0] T_LINE_if_mux1h_46_nl;
  wire[1:0] T_LINE_if_and_37_nl;
  wire[1:0] T_LINE_if_mux1h_47_nl;
  wire[0:0] T_LINE_if_not_11_nl;
  wire[0:0] T_LINE_if_and_38_nl;
  wire[0:0] T_LINE_if_mux1h_48_nl;
  wire[0:0] T_LINE_if_and_39_nl;
  wire[0:0] T_LINE_if_mux1h_49_nl;
  wire[0:0] T_LINE_if_and_40_nl;
  wire[0:0] T_LINE_if_mux1h_50_nl;
  wire[0:0] T_LINE_if_and_41_nl;
  wire[0:0] T_LINE_if_mux1h_51_nl;
  wire[0:0] T_LINE_if_or_25_nl;
  wire[0:0] T_LINE_if_mux1h_52_nl;
  wire[0:0] T_LINE_if_or_26_nl;
  wire[32:0] T_LINE_if_mux1h_53_nl;
  wire[2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_2_nl;
  wire[2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_2_nl;
  wire[5:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_3_nl;
  wire[5:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_3_nl;
  wire[6:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_1_nl;
  wire[6:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_3_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_nor_3_nl;
  wire[18:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_4_nl;
  wire[31:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_5_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_and_2_nl;
  wire[26:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_mux_3_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_1_nl;
  wire[36:0] acc_4_nl;
  wire[37:0] nl_acc_4_nl;
  wire[34:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_2_nl;
  wire[33:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_3_nl;
  wire[43:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux1h_5_nl;
  wire[8:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_26_nl;
  wire[8:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_25_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_27_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_26_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_28_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_27_nl;
  wire[1:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_29_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_26_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_30_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_28_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_31_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_29_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_32_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_30_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_33_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_31_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_34_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_32_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_35_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_33_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_36_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_34_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_37_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_35_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_38_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_36_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_39_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_37_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_40_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_38_nl;
  wire[1:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_41_nl;
  wire[1:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_39_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_42_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_40_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_43_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_41_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_44_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_42_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_45_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_43_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_46_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_44_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_47_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_45_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_48_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_46_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_49_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_47_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_50_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_48_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_51_nl;
  wire[0:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_49_nl;
  wire[3:0] ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux1h_6_nl;

  // Interconnect Declarations for Component Instantiations 
  wire [6:0] nl_ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr;
  assign nl_ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr
      = {2'b0, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva};
  wire[2:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_2_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_3_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_4_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_5_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_6_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_7_nl;
  wire[2:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_8_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_9_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_10_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_11_nl;
  wire[2:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_12_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_13_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_14_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_15_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_16_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_17_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_18_nl;
  wire[1:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_19_nl;
  wire[0:0] operator_33_3_true_AC_TRN_AC_WRAP_mux_20_nl;
  wire [32:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a;
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_nl = MUX_v_3_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_32_30,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_32_30,
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_2_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_29,
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_3_nl = MUX_v_2_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_28_27,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_28_27,
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_4_nl = MUX_v_2_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_26_25,
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_26
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_25}),
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_5_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_24,
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_6_nl = MUX_v_2_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_23_22,
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_23
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_22}),
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_7_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_21,
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_8_nl = MUX_v_3_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_20_18,
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_20
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_19
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_18}),
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_9_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_17,
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_10_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_16,
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_11_nl = MUX_v_2_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_15_14,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_15_14,
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_12_nl = MUX_v_3_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_13_11,
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_13
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_12
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_11}),
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_13_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_10,
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_14_nl = MUX_v_2_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_9_8,
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_9
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_8}),
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_15_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_7,
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_16_nl = MUX_v_2_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_6_5,
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_6
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_5}),
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_17_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_4,
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_18_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_3,
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_19_nl = MUX_v_2_2_2(({ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm}),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_2_1,
      fsm_output[6]);
  assign operator_33_3_true_AC_TRN_AC_WRAP_mux_20_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_0,
      fsm_output[6]);
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a = {operator_33_3_true_AC_TRN_AC_WRAP_mux_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux_2_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux_3_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux_4_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux_5_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux_6_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux_7_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux_8_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux_9_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux_10_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux_11_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux_12_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux_13_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux_14_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux_15_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux_16_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux_17_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux_18_nl , operator_33_3_true_AC_TRN_AC_WRAP_mux_19_nl
      , operator_33_3_true_AC_TRN_AC_WRAP_mux_20_nl};
  wire [4:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s;
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva;
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
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1_tr0
      = ~ T_LINE_slc_T_LINE_acc_6_itm;
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_T_LINE_C_10_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_T_LINE_C_10_tr0 = operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs;
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_C_0_tr0
      = ~ (z_out_5[4]);
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_for_C_0_tr0
      = ~ (z_out_5[4]);
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_T_LINE_C_14_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_T_LINE_C_14_tr0 = operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs;
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_for_C_0_tr0
      = ~ (z_out_5[4]);
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_for_C_0_tr0
      = ~ (z_out_5[4]);
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_T_LINE_C_18_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_T_LINE_C_18_tr0 = ~ T_LINE_slc_T_LINE_acc_6_itm;
  wire [0:0] nl_getMaxLine_core_core_fsm_inst_R_LINE_C_0_tr0;
  assign nl_getMaxLine_core_core_fsm_inst_R_LINE_C_0_tr0 = ~ (z_out[8]);
  Hough_Algorithm_HW_1296_864mgc_rom_23_70_32_1_60  ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg
      (
      .addr(nl_ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr[6:0]),
      .data_out(ac_math_atan_pi_2mi_return_1_69_38_sva_1)
    );
  mgc_shift_r_v5 #(.width_a(32'sd33),
  .signd_a(32'sd1),
  .width_s(32'sd5),
  .width_z(32'sd33)) operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg (
      .a(nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a[32:0]),
      .s(nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s[4:0]),
      .z(z_out_6)
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
      .ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1_tr0(nl_getMaxLine_core_core_fsm_inst_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1_tr0[0:0]),
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
  assign nor_cse = ~((fsm_output[0]) | (fsm_output[26]));
  assign x1_t_and_3_cse = core_wen & or_tmp_72;
  assign x2_t_and_1_cse = T_LINE_asn_48 & (fsm_output[25]);
  assign x2_t_and_3_cse = core_wen & (T_LINE_asn_46 | T_LINE_asn_48) & (fsm_output[25]);
  assign x2_t_and_4_cse = core_wen & (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      & (fsm_output[25]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_and_cse
      = core_wen & (fsm_output[15]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_5_quotient_temp_and_cse
      = core_wen & (~ or_dcpl_4) & (fsm_output[18]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_7_quotient_temp_and_cse
      = core_wen & (~ or_dcpl_4) & (fsm_output[24]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_and_cse
      = core_wen & (fsm_output[14]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_and_cse
      = core_wen & (~ or_dcpl_13) & (fsm_output[18]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_3_quotient_temp_and_cse
      = core_wen & (~ or_dcpl_13) & (fsm_output[24]);
  assign Hough_Algorithm_HW_1296_864_getMaxLine_and_cse = core_wen & (~((~ (fsm_output[26]))
      | (z_out[8])));
  assign and_389_rgt = (~ (z_out[16])) & (fsm_output[1]);
  assign T_LINE_if_if_dividend1_mul_cmp_a = {reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd
      , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1 , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_2
      , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_3 , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_4
      , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_5 , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_6
      , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_7 , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_8
      , reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_9};
  assign and_452_rgt = (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      & (fsm_output[9]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_and_nl
      = MUX_v_6_2_2(6'b000000, (z_out_1[5:0]), or_dcpl_88);
  assign or_316_nl = or_dcpl_73 | (fsm_output[15]) | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c
      | (fsm_output[21]);
  assign T_LINE_if_mux1h_4_rgt = MUX1HOT_v_13_3_2((z_out_2[12:0]), ({3'b000 , (T_LINE_if_acc_5_psp_1[9:8])
      , 8'b00000000}), ({7'b0000000 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_i_and_nl}),
      {(fsm_output[2]) , (fsm_output[4]) , or_316_nl});
  assign nl_operator_60_32_true_AC_TRN_AC_WRAP_acc_2_nl = (R_LINE_r_10_0_sva[10:2])
      + 9'b100011011;
  assign operator_60_32_true_AC_TRN_AC_WRAP_acc_2_nl = nl_operator_60_32_true_AC_TRN_AC_WRAP_acc_2_nl[8:0];
  assign or_318_nl = or_dcpl_77 | (fsm_output[5:4]!=2'b00);
  assign or_319_nl = or_dcpl_83 | (fsm_output[17]) | (fsm_output[10]) | (fsm_output[16])
      | (fsm_output[13]) | or_dcpl_79;
  assign T_LINE_if_mux1h_8_rgt = MUX1HOT_v_13_3_2((z_out[12:0]), ({3'b000 , T_LINE_if_acc_5_psp_1}),
      ({4'b0000 , operator_60_32_true_AC_TRN_AC_WRAP_acc_2_nl}), {(fsm_output[3])
      , or_318_nl , or_319_nl});
  assign or_791_cse = (fsm_output[7:5]!=3'b000);
  assign T_LINE_if_and_cse = core_wen & (~(or_dcpl_77 | (fsm_output[5])));
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      = (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs) & (fsm_output[13]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      = T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (fsm_output[13]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      = (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs) & (fsm_output[19]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      = T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (fsm_output[19]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_and_64_cse
      = core_wen & (~ (fsm_output[6]));
  assign or_795_cse = (fsm_output[7]) | (fsm_output[5]);
  assign and_1330_cse = or_795_cse & core_wen;
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_26_cse
      = ((~ mux_19_itm) & (fsm_output[18])) | ((~ mux_20_itm) & (fsm_output[24]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_27_cse
      = (mux_19_itm & (fsm_output[18])) | (mux_20_itm & (fsm_output[24]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_22_cse
      = (T_LINE_if_if_slc_T_LINE_if_acc_8_svs & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs
      & (fsm_output[18])) | (T_LINE_if_if_slc_T_LINE_if_acc_8_svs & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs
      & (fsm_output[24]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_23_cse
      = (T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs)
      & (fsm_output[18])) | (T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs)
      & (fsm_output[24]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_24_cse
      = (not_tmp_63 & (fsm_output[18])) | (not_tmp_65 & (fsm_output[24]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_25_cse
      = ((~(T_LINE_if_if_slc_T_LINE_if_acc_8_svs | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs))
      & (fsm_output[18])) | ((~(T_LINE_if_if_slc_T_LINE_if_acc_8_svs | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs))
      & (fsm_output[24]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_nl
      = MUX_v_32_2_2(32'b00000000000000000000000000000000, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0,
      (fsm_output[7]));
  assign and_848_nl = (z_out_4[35]) & (fsm_output[6]);
  assign and_850_nl = (~ (z_out_4[35])) & (fsm_output[6]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_mux1h_1_rgt
      = MUX1HOT_v_35_3_2(({3'b000 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_nl}),
      z_out_2, z_out_3, {or_795_cse , and_848_nl , and_850_nl});
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_or_cse
      = ((~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs) & (reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd[26]))
      | (T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_32_6[26]));
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_and_6_cse
      = core_wen & (~(or_dcpl_83 | (fsm_output[19:17]!=3'b000) | or_dcpl_92 | or_dcpl_87));
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_or_rgt
      = (T_LINE_if_if_slc_T_LINE_if_acc_8_svs & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c)
      | ((~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs) & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_37_m1c);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c
      = (fsm_output[14]) | (fsm_output[20]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_37_m1c
      = (fsm_output[15]) | (fsm_output[21]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_56_cse
      = ((~(T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (z_out_5[4]))) & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c)
      | ((T_LINE_if_if_slc_T_LINE_if_acc_8_svs | (~ (z_out_5[4]))) & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_37_m1c)
      | or_dcpl_117 | (fsm_output[16]) | (fsm_output[22]);
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
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1
      = MUX_v_33_2_2((z_out_5[32:0]), (z_out_4[32:0]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_1
      = (T_LINE_if_acc_1_itm_12_4[4]) ^ (T_LINE_if_acc_1_itm_12_4[5]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_1
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_1
      & (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_2_1_mx0
      = MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_1}},
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_1}),
      2'b01, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1
      = ~(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_slc_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_if_acc_1_2_svs_1
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva);
  assign nl_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_1_mx0w1
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva
      + 5'b00001;
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_1_mx0w1
      = nl_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_1_mx0w1[4:0];
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0
      = MUX_v_20_2_2(({1'b0 , (T_LINE_if_if_dividend1_sva[18:0])}), (z_out_3[19:0]),
      T_LINE_if_if_dividend1_sva[19]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0
      = MUX_v_20_2_2(({1'b0 , (T_LINE_if_else_dividend1_sva[18:0])}), (z_out_3[19:0]),
      T_LINE_if_else_dividend1_sva[19]);
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
      = MUX_v_20_2_2(({1'b0 , (T_LINE_if_if_dividend2_sva[18:0])}), (z_out_3[19:0]),
      T_LINE_if_if_dividend2_sva[19]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0
      = MUX_v_20_2_2(({1'b0 , (T_LINE_if_else_dividend2_sva[18:0])}), (z_out_3[19:0]),
      T_LINE_if_else_dividend2_sva[19]);
  assign T_LINE_asn_46 = T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs);
  assign T_LINE_asn_48 = ~(T_LINE_if_if_slc_T_LINE_if_acc_8_svs | operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs);
  assign or_dcpl_4 = operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
      | T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
  assign or_dcpl_13 = operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
      | (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  assign or_dcpl_73 = (fsm_output[19]) | (fsm_output[13]);
  assign or_dcpl_77 = (fsm_output[7:6]!=2'b00);
  assign or_dcpl_79 = (fsm_output[15:14]!=2'b00);
  assign or_dcpl_83 = (fsm_output[12:11]!=2'b00);
  assign or_dcpl_87 = (fsm_output[21:20]!=2'b00);
  assign or_dcpl_88 = or_dcpl_79 | or_dcpl_87;
  assign or_dcpl_89 = (fsm_output[9:8]!=2'b00);
  assign or_dcpl_92 = (fsm_output[15:13]!=3'b000);
  assign or_dcpl_98 = or_dcpl_83 | (fsm_output[17]) | (fsm_output[18]) | (fsm_output[19])
      | (fsm_output[10]) | (fsm_output[16]);
  assign not_tmp_63 = ~((~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs)
      | T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  assign or_163_nl = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs
      | T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
  assign mux_19_itm = MUX_s_1_2_2(not_tmp_63, or_163_nl, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_if_xnor_svs);
  assign not_tmp_65 = ~((~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs)
      | T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  assign or_165_nl = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs
      | T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
  assign mux_20_itm = MUX_s_1_2_2(not_tmp_65, or_165_nl, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs);
  assign and_dcpl_72 = ~(T_LINE_if_if_slc_T_LINE_if_acc_8_svs | (reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd[26]));
  assign and_dcpl_74 = T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (~ (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_32_6[26]));
  assign or_dcpl_117 = (fsm_output[17]) | (fsm_output[23]);
  assign or_tmp_72 = (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
      & (fsm_output[22]);
  assign and_395_cse = (~ (z_out[8])) & (fsm_output[26]);
  assign T_LINE_if_if_dividend1_mul_cmp_a_mx0c0 = T_LINE_if_if_slc_T_LINE_if_acc_8_svs
      & (fsm_output[10]);
  assign T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 = (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs)
      & (fsm_output[10]);
  assign T_LINE_if_if_dividend1_mul_cmp_a_mx0c2 = T_LINE_if_if_slc_T_LINE_if_acc_8_svs
      & (fsm_output[16]);
  assign T_LINE_if_if_dividend1_mul_cmp_a_mx0c3 = (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs)
      & (fsm_output[16]);
  assign T_LINE_if_if_dividend1_or_itm = T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 |
      T_LINE_if_if_dividend1_mul_cmp_a_mx0c3;
  assign T_LINE_if_if_dividend1_or_1_itm = T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
      | T_LINE_if_if_dividend1_mul_cmp_a_mx0c2;
  assign or_tmp_581 = (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs) & (fsm_output[25]);
  assign or_tmp_583 = T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (fsm_output[25]);
  assign or_tmp_599 = (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva)
      & (fsm_output[7]);
  assign or_tmp_600 = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva
      & (fsm_output[7]);
  assign or_tmp_602 = ((~((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_32_6[26])
      & T_LINE_if_if_slc_T_LINE_if_acc_8_svs)) & (fsm_output[10])) | ((reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd[26])
      & (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs) & (fsm_output[16]));
  assign or_tmp_603 = ((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_32_6[26])
      & T_LINE_if_if_slc_T_LINE_if_acc_8_svs & (fsm_output[10])) | (((~ (reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd[26]))
      | T_LINE_if_if_slc_T_LINE_if_acc_8_svs) & (fsm_output[16]));
  assign or_731_cse = (fsm_output[21]) | (fsm_output[20]) | (fsm_output[15]) | (fsm_output[14]);
  assign or_tmp_611 = (fsm_output[17]) | (fsm_output[11]);
  assign or_tmp_613 = (fsm_output[18]) | (fsm_output[12]);
  assign T_LINE_if_or_3_itm = or_tmp_599 | or_tmp_600;
  assign T_LINE_if_nor_1_cse = ~((fsm_output[2]) | (fsm_output[4]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_nor_itm
      = ~(or_tmp_602 | or_tmp_603 | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_itm
      = ~(or_tmp_611 | or_731_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm
      = ~(or_tmp_613 | or_731_cse);
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_t_7_0_sva <= 8'b00000000;
    end
    else if ( ((fsm_output[27]) | (fsm_output[22]) | (fsm_output[26]) | (fsm_output[0]))
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
      x1_t_42_31_lpi_3 <= MUX_v_12_2_2((z_out[11:0]), T_LINE_if_T_LINE_if_and_34_nl,
          or_tmp_72);
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
    else if ( core_wen & (~ or_dcpl_4) & (fsm_output[12]) ) begin
      T_LINE_if_else_dividend1_sva <= z_out_5[43:24];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      x2_t_42_31_lpi_3 <= 12'b000000000000;
      y2_t_42_32_lpi_3 <= 11'b00000000000;
    end
    else if ( x2_t_and_3_cse ) begin
      x2_t_42_31_lpi_3 <= MUX_v_12_2_2(12'b000010100010, (z_out[11:0]), x2_t_and_1_cse);
      y2_t_42_32_lpi_3 <= MUX_v_11_2_2((z_out[10:0]), 11'b00000110110, x2_t_and_1_cse);
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
          <= ~((T_LINE_if_else_dividend1_sva[19]) ^ (reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd[26]));
      operator_27_3_true_AC_TRN_AC_WRAP_return_2_sva <= ~((reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd!=27'b000000000000000000000000000));
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
      T_LINE_if_else_dividend2_sva <= z_out_5[43:24];
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
    else if ( core_wen & (fsm_output[21]) ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs
          <= ~((T_LINE_if_else_dividend2_sva[19]) ^ (reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd[26]));
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
    else if ( mux_21_nl & (~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
        & core_wen ) begin
      y1_t_42_32_lpi_3 <= MUX_v_11_2_2((z_out[10:0]), T_LINE_if_T_LINE_if_and_35_nl,
          or_tmp_72);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_if_dividend1_sva <= 20'b00000000000000000000;
    end
    else if ( core_wen & (~ or_dcpl_13) & (fsm_output[12]) ) begin
      T_LINE_if_if_dividend1_sva <= z_out_5[43:24];
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
          <= ~((T_LINE_if_if_dividend1_sva[19]) ^ (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_32_6[26]));
      operator_27_3_true_AC_TRN_AC_WRAP_return_sva <= ~((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_32_6!=27'b000000000000000000000000000));
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
      T_LINE_if_if_dividend2_sva <= z_out_5[43:24];
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
    else if ( core_wen & (fsm_output[20]) ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_if_xnor_svs
          <= ~((T_LINE_if_if_dividend2_sva[19]) ^ (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_32_6[26]));
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
    else if ( core_wen & ((fsm_output[0]) | (fsm_output[26])) ) begin
      R_LINE_r_10_0_sva <= MUX_v_11_2_2(11'b00000000000, z_out_1, (fsm_output[26]));
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
    else if ( core_wen & ((fsm_output[0]) | (fsm_output[27]) | and_389_rgt) ) begin
      threshold_23_8_lpi_3 <= MUX_v_16_2_2(16'b0000000101000100, acc_rsci_idat_mxwt,
          and_389_rgt);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      reg_acc_rsci_irdy_core_psct_cse <= 1'b0;
      reg_y2_rsci_ivld_core_psct_cse <= 1'b0;
      T_LINE_if_if_dividend1_mul_cmp_b <= 27'b000000000000000000000000000;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_0
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_1
          <= 5'b00000;
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
      reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd_1
          <= 6'b000000;
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_ac_fixed_cctor_20_1_ftd_11
          <= 1'b0;
    end
    else if ( core_wen ) begin
      reg_acc_rsci_irdy_core_psct_cse <= ~((nor_cse & (~ (fsm_output[25]))) | ((~
          T_LINE_slc_T_LINE_acc_6_itm) & (fsm_output[25])) | and_395_cse);
      reg_y2_rsci_ivld_core_psct_cse <= and_395_cse;
      T_LINE_if_if_dividend1_mul_cmp_b <= MUX_v_27_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_32_6,
          reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd,
          T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_mux1h_4_nl
          & (~ or_dcpl_88);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_0
          <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux_2_nl
          & (~ (fsm_output[5]));
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm
          <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_92_nl
          & (~ (fsm_output[5]));
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm
          <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_93_nl
          & (~ (fsm_output[5]));
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm
          <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_94_nl
          & (~ (fsm_output[5]));
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm
          <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_95_nl
          & (~ (fsm_output[5]));
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm
          <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_96_nl
          & (~ (fsm_output[5]));
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm
          <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_97_nl
          & (~ (fsm_output[5]));
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm
          <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_98_nl
          & (~ (fsm_output[5]));
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm
          <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_99_nl
          & (~ (fsm_output[5]));
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_1
          <= MUX_v_5_2_2(({2'b00 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_mux_nl}),
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_1_mx0w1,
          fsm_output[6]);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_18_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_1_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_17_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_2_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_16_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_3_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_15_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_4_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_14_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_5_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_13_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_6_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_12_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_7_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_11_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_8_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_10_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_9_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_9_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_10_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_8_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_11_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_7_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_12_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_6_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_13_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_5_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_14_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_4_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_15_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_3_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_16_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_2_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_17_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_1_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_18_sva
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_mux_nl
          & (~ or_dcpl_73);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_19_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[19]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[19]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_18_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[19]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[19]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_18_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[18]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[18]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_17_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[18]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[18]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_17_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[17]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[17]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_16_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[17]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[17]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_16_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[16]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[16]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_15_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[16]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[16]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_15_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[15]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[15]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_14_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[15]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[15]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_14_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[14]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[14]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_13_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[14]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[14]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_13_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[13]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[13]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_12_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[13]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[13]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_12_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[12]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[12]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_11_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[12]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[12]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_11_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[11]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[11]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_10_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[11]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[11]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_10_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[10]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[10]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_9_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[10]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[10]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_9_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[9]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[9]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_8_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[9]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[9]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_8_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[8]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[8]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_7_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[8]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[8]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_7_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[7]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[7]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_6_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[7]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[7]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_6_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[6]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[6]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_5_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[6]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[6]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_5_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[5]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[5]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_4_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[5]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[5]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_4_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[4]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[4]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_3_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[4]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[4]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_3_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[3]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[3]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_2_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[3]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[3]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_2_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[2]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[2]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_1_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[2]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[2]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_1_sva
          <= MUX1HOT_s_1_5_2((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[1]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[1]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[1]),
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[1]),
          {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
          , or_dcpl_88 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
          , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd <= MUX_v_11_2_2((z_out[11:1]), (z_out[10:0]),
          T_LINE_if_if_dividend1_or_itm);
      reg_T_LINE_if_if_dividend1_mul_cmp_a_ftd_1 <= MUX1HOT_s_1_3_2((z_out[0]), y1_t_31_lpi_3,
          y2_t_31_lpi_3, {T_LINE_if_if_dividend1_or_1_itm , T_LINE_if_if_dividend1_mul_cmp_a_mx0c1
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
      reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd_1
          <= z_out_6[5:0];
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_ac_fixed_cctor_20_1_ftd_11
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[8];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_if_slc_T_LINE_if_acc_8_svs <= 1'b0;
    end
    else if ( core_wen & (((~ operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs)
        & (fsm_output[8])) | and_452_rgt) ) begin
      T_LINE_if_if_slc_T_LINE_if_acc_8_svs <= MUX_s_1_2_2((readslicef_9_1_8(T_LINE_if_acc_nl)),
          T_LINE_if_if_T_LINE_if_if_T_LINE_if_if_nor_nl, and_452_rgt);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
          <= 1'b0;
    end
    else if ( core_wen & (fsm_output[1]) ) begin
      operator_24_16_false_AC_TRN_AC_WRAP_16_false_slc_operator_24_16_false_AC_TRN_AC_WRAP_16_false_acc_16_svs
          <= z_out[16];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_acc_1_itm_12_4 <= 9'b000000000;
    end
    else if ( ((fsm_output[20]) | (fsm_output[21]) | (fsm_output[15]) | (fsm_output[14])
        | (fsm_output[13]) | (fsm_output[4]) | (fsm_output[19]) | (fsm_output[2])
        | (fsm_output[3])) & core_wen ) begin
      T_LINE_if_acc_1_itm_12_4 <= T_LINE_if_mux1h_4_rgt[12:4];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_acc_1_itm_3_0 <= 4'b0000;
    end
    else if ( ((fsm_output[20]) | (fsm_output[21]) | (fsm_output[15]) | (fsm_output[14])
        | (fsm_output[13]) | (fsm_output[4]) | (fsm_output[19]) | (fsm_output[2]))
        & core_wen ) begin
      T_LINE_if_acc_1_itm_3_0 <= T_LINE_if_mux1h_4_rgt[3:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_acc_2_itm_16_14 <= 3'b000;
    end
    else if ( ((fsm_output[15]) | (fsm_output[14]) | (fsm_output[13]) | (fsm_output[17])
        | (fsm_output[12]) | (fsm_output[11]) | (fsm_output[16]) | (fsm_output[10])
        | (fsm_output[3]) | (fsm_output[4]) | (fsm_output[7]) | (fsm_output[6]) |
        (fsm_output[5])) & core_wen ) begin
      T_LINE_if_acc_2_itm_16_14 <= T_LINE_if_mux1h_8_rgt[12:10];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_acc_2_itm_13 <= 1'b0;
    end
    else if ( ((fsm_output[4]) | (fsm_output[15]) | (fsm_output[14]) | (fsm_output[13])
        | (fsm_output[17]) | (fsm_output[12]) | (fsm_output[11]) | (fsm_output[16])
        | (fsm_output[10]) | (fsm_output[3])) & core_wen ) begin
      T_LINE_if_acc_2_itm_13 <= T_LINE_if_mux1h_8_rgt[9];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_if_acc_2_itm_12_4 <= 9'b000000000;
    end
    else if ( (mux_22_nl | (fsm_output[3])) & core_wen ) begin
      T_LINE_if_acc_2_itm_12_4 <= T_LINE_if_mux1h_8_rgt[8:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_35_18_sva_3_0
          <= 4'b0000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_35_18_sva_7_4
          <= 4'b0000;
    end
    else if ( T_LINE_if_and_cse ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_35_18_sva_3_0
          <= T_LINE_if_acc_1_itm_3_0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_35_18_sva_7_4
          <= T_LINE_if_acc_2_itm_12_4[3:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_32_6
          <= 27'b000000000000000000000000000;
    end
    else if ( core_wen & (~(or_dcpl_98 | or_dcpl_92 | or_dcpl_89 | (fsm_output[20])))
        ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_32_6
          <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[32:6];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_32_30
          <= 3'b000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_29
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_28_27
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_26
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_25
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_24
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_23
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_22
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_21
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_20
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_19
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_18
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_17
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_16
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_15_14
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_13
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_12
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_11
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_10
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_9
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_8
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_7
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_6
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_5
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_4
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_3
          <= 1'b0;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_2_1
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_26_25
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_23_22
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_20_18
          <= 3'b000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_13_11
          <= 3'b000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_9_8
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_6_5
          <= 2'b00;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm
          <= 1'b0;
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm
          <= 1'b0;
    end
    else if ( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_and_64_cse
        ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_32_30
          <= MUX_v_3_2_2(({{2{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_1}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_1}),
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[32:30]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_29
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[29]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_28_27
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_1}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_1}),
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[28:27]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_26
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[26]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_25
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[25]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_24
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_1,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[24]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_23
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[23]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_22
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[22]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_21
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_1,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[21]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_20
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[20]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_19
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[19]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_18
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[18]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_17
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_1,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[17]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_16
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[16]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_15_14
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_1}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_1}),
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[15:14]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_13
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[13]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_12
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[12]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_11
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[11]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_10
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_1,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[10]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_9
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[9]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_8
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[8]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_7
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_1,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[7]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_6
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[6]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_5
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[5]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_4
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_1,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[4]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_3
          <= MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_0_sva,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[3]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_2_1
          <= MUX_v_2_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_2_1_mx0,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[2:1]),
          fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_26_25
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1}),
          (z_out_2[26:25]), fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_23_22
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1}),
          (z_out_2[23:22]), fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_20_18
          <= MUX_v_3_2_2(({{2{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1}),
          (z_out_2[20:18]), fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_13_11
          <= MUX_v_3_2_2(({{2{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1}),
          (z_out_2[13:11]), fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_9_8
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1}),
          (z_out_2[9:8]), fsm_output[7]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_6_5
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1}),
          (z_out_2[6:5]), fsm_output[7]);
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm
          <= MUX1HOT_s_1_4_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1,
          (z_out_2[1]), (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[9]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_9_lpi_3_dfm_1,
          {(fsm_output[5]) , (fsm_output[7]) , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_26_cse
          , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_27_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm
          <= MUX1HOT_s_1_6_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1,
          (z_out_2[16]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_16_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[16]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_17_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[17]),
          {(fsm_output[5]) , (fsm_output[7]) , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_22_cse
          , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_23_cse
          , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_24_cse
          , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_25_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm
          <= MUX1HOT_s_1_6_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1,
          (z_out_2[29]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[11]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[12]),
          {(fsm_output[5]) , (fsm_output[7]) , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_22_cse
          , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_23_cse
          , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_24_cse
          , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_25_cse});
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm
          <= MUX1HOT_s_1_6_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_1,
          (z_out_2[3]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[10]),
          ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_11_lpi_3_dfm_1,
          (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[11]),
          {(fsm_output[5]) , (fsm_output[7]) , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_22_cse
          , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_23_cse
          , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_24_cse
          , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_25_cse});
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_32_30
          <= 3'b000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_28_27
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_15_14
          <= 2'b00;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva
          <= 5'b00000;
    end
    else if ( and_1330_cse ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_32_30
          <= MUX_v_3_2_2(3'b000, (z_out_2[32:30]), Hough_Algorithm_HW_1296_864_getMaxLine_not_11_nl);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_28_27
          <= MUX_v_2_2_2(2'b00, (z_out_2[28:27]), Hough_Algorithm_HW_1296_864_getMaxLine_not_10_nl);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_15_14
          <= MUX_v_2_2_2(2'b00, (z_out_2[15:14]), Hough_Algorithm_HW_1296_864_getMaxLine_not_9_nl);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva
          <= MUX_v_5_2_2(5'b00000, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_1,
          Hough_Algorithm_HW_1296_864_getMaxLine_not_8_nl);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_34_32
          <= 3'b000;
    end
    else if ( or_791_cse & core_wen ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_34_32
          <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_mux1h_1_rgt[34:32];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0
          <= 32'b00000000000000000000000000000000;
    end
    else if ( ((fsm_output[6:5]!=2'b00)) & core_wen ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0
          <= ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_mux1h_1_rgt[31:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      T_LINE_slc_T_LINE_acc_6_itm <= 1'b0;
    end
    else if ( core_wen & ((fsm_output[6]) | (fsm_output[22])) ) begin
      T_LINE_slc_T_LINE_acc_6_itm <= MUX_s_1_2_2((z_out[3]), (z_out[6]), fsm_output[22]);
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
          <= (z_out_3[26]) & (~(and_dcpl_72 | and_dcpl_74));
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1
          <= MUX1HOT_v_26_3_2((z_out_3[25:0]), (reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd[25:0]),
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_32_6[25:0]),
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
    else if ( core_wen & (~((fsm_output[12]) | (fsm_output[18]) | or_dcpl_73 | or_dcpl_88))
        ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva
          <= z_out_5[26:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp
          <= 1'b0;
    end
    else if ( core_wen & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_or_rgt
        ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_unequal_tmp
          <= reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
          | (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1!=26'b00000000000000000000000000);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0
          <= 27'b000000000000000000000000000;
    end
    else if ( (mux_23_nl | (fsm_output[13]) | (fsm_output[19])) & core_wen ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0
          <= MUX_v_27_2_2(27'b000000000000000000000000000, (z_out_3[26:0]), not_369_nl);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm
          <= 1'b0;
    end
    else if ( core_wen & (~ or_dcpl_117) ) begin
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_else_lsb_operator_28_true_1_nor_itm
          <= ~((ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_lpi_3_dfm_mx0!=28'b0000000000000000000000000000));
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd
          <= 27'b000000000000000000000000000;
    end
    else if ( core_wen & (~(or_dcpl_98 | or_dcpl_92 | or_dcpl_89 | (fsm_output[21])))
        ) begin
      reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd
          <= MUX_v_27_2_2((z_out_6[32:6]), (z_out_2[32:6]), fsm_output[7]);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11
          <= 9'b000000000;
    end
    else if ( core_wen & (fsm_output[21:19]==3'b000) ) begin
      reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_else_ac_fixed_cctor_20_1_ftd_11
          <= ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[8:0];
    end
  end
  assign T_LINE_if_aelse_not_57_nl = ~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs;
  assign T_LINE_if_T_LINE_if_and_34_nl = MUX_v_12_2_2(12'b000000000000, x1_t_42_31_lpi_3,
      T_LINE_if_aelse_not_57_nl);
  assign mux_nl = MUX_s_1_2_2((fsm_output[19]), T_LINE_if_if_slc_T_LINE_if_acc_8_svs,
      fsm_output[22]);
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
  assign mux_21_nl = MUX_s_1_2_2((fsm_output[19]), (~ T_LINE_if_if_slc_T_LINE_if_acc_8_svs),
      fsm_output[22]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_mux1h_4_nl
      = MUX1HOT_s_1_6_2((z_out_2[18]), (z_out_4[35]), (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uN_qr_lpi_3_dfm_mx0[0]),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_uN_qr_lpi_3_dfm_mx0[0]),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_uN_qr_lpi_3_dfm_mx0[0]),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uN_qr_lpi_3_dfm_mx0[0]),
      {(fsm_output[4]) , (fsm_output[6]) , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse});
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux_2_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_0,
      (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_1[0]),
      fsm_output[7]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_92_nl
      = MUX1HOT_s_1_4_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm,
      (z_out_2[0]), (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[18]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_18_lpi_3_dfm_1,
      {(fsm_output[6]) , (fsm_output[7]) , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_26_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_27_cse});
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_93_nl
      = MUX1HOT_s_1_6_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm,
      (z_out_2[10]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_17_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[17]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_8_lpi_3_dfm_mx1w0,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[8]),
      {(fsm_output[6]) , (fsm_output[7]) , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_22_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_23_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_24_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_25_cse});
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_94_nl
      = MUX1HOT_s_1_6_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm,
      (z_out_2[17]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[15]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_16_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[16]),
      {(fsm_output[6]) , (fsm_output[7]) , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_22_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_23_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_24_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_25_cse});
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_95_nl
      = MUX1HOT_s_1_6_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm,
      (z_out_2[2]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[14]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_15_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[15]),
      {(fsm_output[6]) , (fsm_output[7]) , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_22_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_23_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_24_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_25_cse});
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_96_nl
      = MUX1HOT_s_1_6_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm,
      (z_out_2[21]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[13]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_14_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[14]),
      {(fsm_output[6]) , (fsm_output[7]) , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_22_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_23_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_24_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_25_cse});
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_97_nl
      = MUX1HOT_s_1_6_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm,
      (z_out_2[24]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_12_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[12]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_13_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[13]),
      {(fsm_output[6]) , (fsm_output[7]) , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_22_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_23_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_24_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_25_cse});
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_98_nl
      = MUX1HOT_s_1_6_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm,
      (z_out_2[4]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_19_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[19]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_10_lpi_3_dfm_1,
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[10]),
      {(fsm_output[6]) , (fsm_output[7]) , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_22_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_23_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_24_cse
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_25_cse});
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_nl
      = ((~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs)
      & (fsm_output[18])) | ((~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs)
      & (fsm_output[24]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_1_nl
      = (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_if_xnor_svs
      & (fsm_output[18])) | (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_6_else_if_xnor_svs
      & (fsm_output[24]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_mux1h_99_nl
      = MUX1HOT_s_1_4_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm,
      (z_out_2[7]), (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_else_else_ac_fixed_cctor_20_1_sva_1[19]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_quotient_temp_19_lpi_3_dfm_1,
      {(fsm_output[6]) , (fsm_output[7]) , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_or_1_nl});
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_mux_nl
      = MUX_v_3_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_2_1_mx0[1]}},
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_2_1_mx0}),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_34_32,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_mux_18_nl
      = MUX_s_1_2_2((~ (z_out_3[27])), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_0_sva,
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
      (z_out_3[27]), ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_or_rgt);
  assign nl_T_LINE_if_acc_nl = conv_u2s_8_9(T_LINE_t_7_0_sva) + 9'b111010011;
  assign T_LINE_if_acc_nl = nl_T_LINE_if_acc_nl[8:0];
  assign nl_T_LINE_if_aelse_acc_nl = ({1'b1 , (~ (T_LINE_t_7_0_sva[7:3]))}) + 6'b010001;
  assign T_LINE_if_aelse_acc_nl = nl_T_LINE_if_aelse_acc_nl[5:0];
  assign T_LINE_if_if_T_LINE_if_if_T_LINE_if_if_nor_nl = ~((readslicef_6_1_5(T_LINE_if_aelse_acc_nl))
      | T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  assign or_nl = (fsm_output[4]) | (fsm_output[10]);
  assign mux_22_nl = MUX_s_1_2_2(or_nl, (fsm_output[10]), or_791_cse);
  assign Hough_Algorithm_HW_1296_864_getMaxLine_not_11_nl = ~ (fsm_output[5]);
  assign Hough_Algorithm_HW_1296_864_getMaxLine_not_10_nl = ~ (fsm_output[5]);
  assign Hough_Algorithm_HW_1296_864_getMaxLine_not_9_nl = ~ (fsm_output[5]);
  assign Hough_Algorithm_HW_1296_864_getMaxLine_not_8_nl = ~ (fsm_output[5]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_nl
      = (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_or_cse
      & (fsm_output[10])) | (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_uD_qelse_or_cse
      & (fsm_output[16]));
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_1_nl
      = (and_dcpl_72 & (fsm_output[10])) | (and_dcpl_72 & (fsm_output[16]));
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qelse_or_2_nl
      = (and_dcpl_74 & (fsm_output[10])) | (and_dcpl_74 & (fsm_output[16]));
  assign not_369_nl = ~ or_dcpl_73;
  assign mux_23_nl = MUX_s_1_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_37_m1c,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_Q_or_36_m1c,
      T_LINE_if_if_slc_T_LINE_if_acc_8_svs);
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_or_5_nl = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      | or_tmp_581;
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_or_6_nl = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      | or_tmp_583;
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_2_nl = MUX1HOT_v_17_9_2(17'b00000000001010001,
      17'b00000000000011011, ({8'b11111111 , T_LINE_if_acc_1_itm_12_4}), 17'b00000000000000001,
      17'b11111111111010011, 17'b11111111110101111, ({1'b1 , acc_rsci_idat_mxwt}),
      17'b11111111100011011, 17'b11111111111100101, {operator_20_15_true_AC_TRN_AC_WRAP_2_or_5_nl
      , operator_20_15_true_AC_TRN_AC_WRAP_2_or_6_nl , (fsm_output[3]) , (fsm_output[6])
      , (fsm_output[22]) , T_LINE_if_if_dividend1_or_1_itm , (fsm_output[1]) , (fsm_output[26])
      , T_LINE_if_if_dividend1_or_itm});
  assign operator_20_15_true_AC_TRN_AC_WRAP_2_or_7_nl = (~(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      | or_tmp_581 | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      | or_tmp_583 | (fsm_output[3]) | (fsm_output[6]) | (fsm_output[22]) | T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
      | T_LINE_if_if_dividend1_mul_cmp_a_mx0c2 | (fsm_output[26]) | T_LINE_if_if_dividend1_mul_cmp_a_mx0c1
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
      ({4'b0000 , T_LINE_t_7_0_sva , 4'b0001}), ({13'b1111111111111 , (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_1_mx0w1[4:2])}),
      ({10'b0000000000 , (z_out_1[7:2])}), ({{4{x1_t_42_31_lpi_3[11]}}, x1_t_42_31_lpi_3}),
      ({{4{x2_t_42_31_lpi_3[11]}}, x2_t_42_31_lpi_3}), (~ threshold_23_8_lpi_3),
      ({8'b00000000 , (z_out_1[10:3])}), ({{5{y1_t_42_32_lpi_3[10]}}, y1_t_42_32_lpi_3}),
      ({{5{y2_t_42_32_lpi_3[10]}}, y2_t_42_32_lpi_3}), {ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse
      , or_tmp_581 , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      , or_tmp_583 , (fsm_output[3]) , (fsm_output[6]) , (fsm_output[22]) , T_LINE_if_if_dividend1_mul_cmp_a_mx0c0
      , T_LINE_if_if_dividend1_mul_cmp_a_mx0c2 , (fsm_output[1]) , (fsm_output[26])
      , T_LINE_if_if_dividend1_mul_cmp_a_mx0c1 , T_LINE_if_if_dividend1_mul_cmp_a_mx0c3});
  assign nl_acc_nl = ({operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_2_nl , operator_20_15_true_AC_TRN_AC_WRAP_2_or_7_nl})
      + conv_u2u_17_18({operator_20_15_true_AC_TRN_AC_WRAP_2_mux1h_3_nl , 1'b1});
  assign acc_nl = nl_acc_nl[17:0];
  assign z_out = readslicef_18_17_1(acc_nl);
  assign T_LINE_mux1h_1_nl = MUX1HOT_v_11_3_2(({{3{T_LINE_t_7_0_sva[7]}}, T_LINE_t_7_0_sva}),
      R_LINE_r_10_0_sva, (signext_11_6({(T_LINE_if_acc_1_itm_12_4[1:0]) , T_LINE_if_acc_1_itm_3_0})),
      {(fsm_output[22]) , (fsm_output[26]) , or_731_cse});
  assign nl_z_out_1 = T_LINE_mux1h_1_nl + 11'b00000000001;
  assign z_out_1 = nl_z_out_1[10:0];
  assign T_LINE_if_T_LINE_if_mux_13_nl = MUX_v_5_2_2(({(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_1[2:0])
      , (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[31:30])}),
      ({{2{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_32_30[2]}},
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_32_30}),
      T_LINE_if_or_3_itm);
  assign T_LINE_if_and_22_nl = MUX_v_5_2_2(5'b00000, T_LINE_if_T_LINE_if_mux_13_nl,
      T_LINE_if_nor_1_cse);
  assign T_LINE_if_T_LINE_if_mux_14_nl = MUX_s_1_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[29]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_8_itm,
      T_LINE_if_or_3_itm);
  assign T_LINE_if_and_23_nl = T_LINE_if_T_LINE_if_mux_14_nl & T_LINE_if_nor_1_cse;
  assign T_LINE_if_T_LINE_if_mux_15_nl = MUX_v_2_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[28:27]),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_28_27,
      T_LINE_if_or_3_itm);
  assign T_LINE_if_and_24_nl = MUX_v_2_2_2(2'b00, T_LINE_if_T_LINE_if_mux_15_nl,
      T_LINE_if_nor_1_cse);
  assign T_LINE_if_T_LINE_if_mux_16_nl = MUX_v_2_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[26:25]),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_26_25,
      T_LINE_if_or_3_itm);
  assign T_LINE_if_and_25_nl = MUX_v_2_2_2(2'b00, T_LINE_if_T_LINE_if_mux_16_nl,
      T_LINE_if_nor_1_cse);
  assign T_LINE_if_T_LINE_if_mux_17_nl = MUX_s_1_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[24]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_7_itm,
      T_LINE_if_or_3_itm);
  assign T_LINE_if_and_26_nl = T_LINE_if_T_LINE_if_mux_17_nl & T_LINE_if_nor_1_cse;
  assign T_LINE_if_T_LINE_if_mux_18_nl = MUX_v_2_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[23:22]),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_23_22,
      T_LINE_if_or_3_itm);
  assign T_LINE_if_and_27_nl = MUX_v_2_2_2(2'b00, T_LINE_if_T_LINE_if_mux_18_nl,
      T_LINE_if_nor_1_cse);
  assign T_LINE_if_T_LINE_if_mux_19_nl = MUX_s_1_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[21]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_6_itm,
      T_LINE_if_or_3_itm);
  assign T_LINE_if_and_28_nl = T_LINE_if_T_LINE_if_mux_19_nl & T_LINE_if_nor_1_cse;
  assign T_LINE_if_T_LINE_if_mux_20_nl = MUX_v_3_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[20:18]),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_20_18,
      T_LINE_if_or_3_itm);
  assign T_LINE_if_and_29_nl = MUX_v_3_2_2(3'b000, T_LINE_if_T_LINE_if_mux_20_nl,
      T_LINE_if_nor_1_cse);
  assign T_LINE_if_T_LINE_if_mux_21_nl = MUX_s_1_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[17]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_4_itm,
      T_LINE_if_or_3_itm);
  assign T_LINE_if_and_30_nl = T_LINE_if_T_LINE_if_mux_21_nl & T_LINE_if_nor_1_cse;
  assign T_LINE_if_T_LINE_if_mux_22_nl = MUX_s_1_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[16]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_3_itm,
      T_LINE_if_or_3_itm);
  assign T_LINE_if_or_24_nl = (T_LINE_if_T_LINE_if_mux_22_nl & (~ (fsm_output[2])))
      | (fsm_output[4]);
  assign T_LINE_if_T_LINE_if_mux_23_nl = MUX_v_2_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[15:14]),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_15_14,
      T_LINE_if_or_3_itm);
  assign T_LINE_if_and_32_nl = MUX_v_2_2_2(2'b00, T_LINE_if_T_LINE_if_mux_23_nl,
      T_LINE_if_nor_1_cse);
  assign T_LINE_if_mux1h_45_nl = MUX1HOT_v_3_3_2(3'b001, (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[13:11]),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_13_11,
      {(fsm_output[2]) , (fsm_output[6]) , T_LINE_if_or_3_itm});
  assign T_LINE_if_not_9_nl = ~ (fsm_output[4]);
  assign T_LINE_if_and_33_nl = MUX_v_3_2_2(3'b000, T_LINE_if_mux1h_45_nl, T_LINE_if_not_9_nl);
  assign T_LINE_if_T_LINE_if_mux_24_nl = MUX_s_1_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[10]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_2_itm,
      T_LINE_if_or_3_itm);
  assign T_LINE_if_and_34_nl = T_LINE_if_T_LINE_if_mux_24_nl & T_LINE_if_nor_1_cse;
  assign T_LINE_if_T_LINE_if_mux_25_nl = MUX_v_2_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[9:8]),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_9_8,
      T_LINE_if_or_3_itm);
  assign T_LINE_if_and_35_nl = MUX_v_2_2_2(2'b00, T_LINE_if_T_LINE_if_mux_25_nl,
      T_LINE_if_nor_1_cse);
  assign T_LINE_if_mux1h_46_nl = MUX1HOT_s_1_3_2((~ (T_LINE_t_7_0_sva[7])), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[7]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_4_else_mux_itm,
      {(fsm_output[2]) , (fsm_output[6]) , T_LINE_if_or_3_itm});
  assign T_LINE_if_and_36_nl = T_LINE_if_mux1h_46_nl & (~ (fsm_output[4]));
  assign T_LINE_if_mux1h_47_nl = MUX1HOT_v_2_3_2((~ (T_LINE_t_7_0_sva[6:5])), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[6:5]),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_1_lpi_3_dfm_1_6_5,
      {(fsm_output[2]) , (fsm_output[6]) , T_LINE_if_or_3_itm});
  assign T_LINE_if_not_11_nl = ~ (fsm_output[4]);
  assign T_LINE_if_and_37_nl = MUX_v_2_2_2(2'b00, T_LINE_if_mux1h_47_nl, T_LINE_if_not_11_nl);
  assign T_LINE_if_mux1h_48_nl = MUX1HOT_s_1_3_2((~ (T_LINE_t_7_0_sva[4])), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[4]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_itm,
      {(fsm_output[2]) , (fsm_output[6]) , T_LINE_if_or_3_itm});
  assign T_LINE_if_and_38_nl = T_LINE_if_mux1h_48_nl & (~ (fsm_output[4]));
  assign T_LINE_if_mux1h_49_nl = MUX1HOT_s_1_3_2((~ (T_LINE_t_7_0_sva[3])), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[3]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_9_itm,
      {(fsm_output[2]) , (fsm_output[6]) , T_LINE_if_or_3_itm});
  assign T_LINE_if_and_39_nl = T_LINE_if_mux1h_49_nl & (~ (fsm_output[4]));
  assign T_LINE_if_mux1h_50_nl = MUX1HOT_s_1_3_2((~ (T_LINE_t_7_0_sva[2])), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[2]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_5_itm,
      {(fsm_output[2]) , (fsm_output[6]) , T_LINE_if_or_3_itm});
  assign T_LINE_if_and_40_nl = T_LINE_if_mux1h_50_nl & (~ (fsm_output[4]));
  assign T_LINE_if_mux1h_51_nl = MUX1HOT_s_1_3_2((~ (T_LINE_t_7_0_sva[1])), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[1]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_10_itm,
      {(fsm_output[2]) , (fsm_output[6]) , T_LINE_if_or_3_itm});
  assign T_LINE_if_and_41_nl = T_LINE_if_mux1h_51_nl & (~ (fsm_output[4]));
  assign T_LINE_if_mux1h_52_nl = MUX1HOT_s_1_3_2((~ (T_LINE_t_7_0_sva[0])), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[0]),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_else_mux_1_itm,
      {(fsm_output[2]) , (fsm_output[6]) , T_LINE_if_or_3_itm});
  assign T_LINE_if_or_25_nl = T_LINE_if_mux1h_52_nl | (fsm_output[4]);
  assign T_LINE_if_or_26_nl = (~((fsm_output[2]) | (fsm_output[4]) | or_tmp_600))
      | (fsm_output[6]) | or_tmp_599;
  assign T_LINE_if_mux1h_53_nl = MUX1HOT_v_33_5_2(({21'b000000000000000000000 , (~
      T_LINE_t_7_0_sva) , 4'b0001}), ({1'b1 , (~ ac_math_atan_pi_2mi_return_1_69_38_sva_1)}),
      (signext_33_18({(~ T_LINE_if_acc_5_psp_1) , (~ (T_LINE_if_acc_2_itm_12_4[3:0]))
      , (~ T_LINE_if_acc_1_itm_3_0)})), ({(~ reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd)
      , (~ reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd_1)}),
      ({reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd
      , reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd_1}),
      {(fsm_output[2]) , (fsm_output[6]) , (fsm_output[4]) , or_tmp_599 , or_tmp_600});
  assign nl_acc_2_nl = ({T_LINE_if_and_22_nl , T_LINE_if_and_23_nl , T_LINE_if_and_24_nl
      , T_LINE_if_and_25_nl , T_LINE_if_and_26_nl , T_LINE_if_and_27_nl , T_LINE_if_and_28_nl
      , T_LINE_if_and_29_nl , T_LINE_if_and_30_nl , T_LINE_if_or_24_nl , T_LINE_if_and_32_nl
      , T_LINE_if_and_33_nl , T_LINE_if_and_34_nl , T_LINE_if_and_35_nl , T_LINE_if_and_36_nl
      , T_LINE_if_and_37_nl , T_LINE_if_and_38_nl , T_LINE_if_and_39_nl , T_LINE_if_and_40_nl
      , T_LINE_if_and_41_nl , T_LINE_if_or_25_nl , T_LINE_if_or_26_nl}) + conv_s2u_34_36({T_LINE_if_mux1h_53_nl
      , 1'b1});
  assign acc_2_nl = nl_acc_2_nl[35:0];
  assign z_out_2 = readslicef_36_35_1(acc_2_nl);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_2_nl
      = MUX_v_3_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_1[2:0]),
      (signext_3_1(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0[26])),
      or_731_cse);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_2_nl
      = MUX_v_3_2_2(3'b000, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_2_nl,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_nor_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_3_nl
      = MUX_v_6_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[31:26]),
      (signext_6_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0[26:25])),
      or_731_cse);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_3_nl
      = MUX_v_6_2_2(6'b000000, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux_3_nl,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_nor_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_3_nl
      = MUX1HOT_v_7_4_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[25:19]),
      (~ (reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd[25:19])),
      (~ (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_32_6[25:19])),
      (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0[24:18]),
      {(fsm_output[6]) , or_tmp_602 , or_tmp_603 , or_731_cse});
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_nor_3_nl
      = ~(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_1_nl
      = MUX_v_7_2_2(7'b0000000, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_3_nl,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_nor_3_nl);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_4_nl
      = MUX1HOT_v_19_8_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0[18:0]),
      (~ (reg_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_1_ftd[18:0])),
      (~ (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_dfm_32_6[18:0])),
      ({(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_26_0[17:0])
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_19_sva}),
      (~ (T_LINE_if_if_dividend1_sva[18:0])), (~ (T_LINE_if_else_dividend1_sva[18:0])),
      (~ (T_LINE_if_if_dividend2_sva[18:0])), (~ (T_LINE_if_else_dividend2_sva[18:0])),
      {(fsm_output[6]) , or_tmp_602 , or_tmp_603 , or_731_cse , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse});
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_and_2_nl
      = (ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva[26])
      & (~ ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_mux_3_nl
      = MUX_v_27_2_2(ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_acc_psp_sva,
      ({reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd
      , reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1}),
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_R_sva_27);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_1_nl
      = or_tmp_602 | or_tmp_603 | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_78_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_77_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_80_cse
      | ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_N_and_79_cse;
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_5_nl
      = MUX1HOT_v_32_3_2(ac_math_atan_pi_2mi_return_1_69_38_sva_1, 32'b00000000000000000000000000000001,
      (signext_32_28({ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_and_2_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_for_mux_3_nl})),
      {(fsm_output[6]) , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_or_1_nl
      , or_731_cse});
  assign nl_z_out_3 = ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_2_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_3_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_and_1_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_4_nl})
      + conv_u2u_32_35(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_else_mux1h_5_nl);
  assign z_out_3 = nl_z_out_3[34:0];
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_2_nl
      = MUX_v_35_2_2(({(~ (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_1_4_0_sva_1[2:0]))
      , (~ ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_1_sva_3_31_0)}),
      (signext_35_33({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_32_30
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_29
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_28_27
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_26
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_25
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_24
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_23
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_22
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_21
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_20
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_19
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_18
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_17
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_16
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_15_14
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_13
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_12
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_11
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_10
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_9
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_8
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_7
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_6
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_5
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_4
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_3
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_2_1
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_0})),
      fsm_output[7]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_3_nl
      = MUX_v_34_2_2(({T_LINE_if_acc_2_itm_13 , T_LINE_if_acc_2_itm_12_4 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_35_18_sva_7_4
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_angle_over_pi_1_35_18_sva_3_0
      , 16'b0000000000000000}), (signext_34_33(~ z_out_6)), fsm_output[7]);
  assign nl_acc_4_nl = conv_s2u_36_37({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_2_nl
      , 1'b1}) + conv_s2u_35_37({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_3_nl
      , 1'b1});
  assign acc_4_nl = nl_acc_4_nl[36:0];
  assign z_out_4 = readslicef_37_36_1(acc_4_nl);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux1h_5_nl
      = MUX1HOT_v_44_4_2(44'b00000000000000000000000000000000000000000001, ({{11{z_out_6[32]}},
      z_out_6}), (~ T_LINE_if_if_dividend1_mul_cmp_z_oreg), 44'b11111111111111111111111111111111111111110101,
      {or_tmp_611 , (fsm_output[7]) , or_tmp_613 , or_731_cse});
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_25_nl
      = MUX_v_9_2_2((signext_9_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_32_30[2:1])),
      T_LINE_if_acc_2_itm_12_4, or_tmp_613);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_26_nl
      = MUX_v_9_2_2(9'b000000000, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_25_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_itm);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_26_nl
      = MUX_s_1_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_32_30[0]),
      (R_LINE_r_10_0_sva[1]), or_tmp_613);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_27_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_26_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_27_nl
      = MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_29,
      (R_LINE_r_10_0_sva[0]), or_tmp_613);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_28_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_27_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_26_nl
      = ~(or_tmp_611 | or_tmp_613 | or_731_cse);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_29_nl
      = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_28_27,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_26_nl);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_28_nl
      = MUX_s_1_2_2((~ reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_26,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_30_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_28_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_29_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[25])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_25,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_31_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_29_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_30_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[24])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_24,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_32_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_30_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_31_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[23])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_23,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_33_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_31_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_32_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[22])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_22,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_34_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_32_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_33_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[21])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_21,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_35_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_33_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_34_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[20])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_20,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_36_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_34_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_35_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[19])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_19,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_37_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_35_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_36_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[18])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_18,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_38_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_36_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_37_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[17])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_17,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_39_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_37_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_38_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[16])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_16,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_40_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_38_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_39_nl
      = MUX_v_2_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[15:14])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_15_14,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_41_nl
      = MUX_v_2_2_2(2'b00, ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_39_nl,
      ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_40_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[13])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_13,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_42_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_40_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_41_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[12])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_12,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_43_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_41_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_42_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[11])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_11,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_44_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_42_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_43_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[10])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_10,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_45_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_43_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_44_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[9])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_9,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_46_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_44_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_45_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[8])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_8,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_47_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_45_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_46_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[7])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_7,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_48_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_46_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_47_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[6])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_6,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_49_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_47_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_48_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[5])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_5,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_50_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_48_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_49_nl
      = MUX_s_1_2_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[4])),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_4,
      fsm_output[7]);
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_51_nl
      = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux_49_nl
      & ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_nor_4_itm;
  assign ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux1h_6_nl
      = MUX1HOT_v_4_4_2((~ (reg_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_2_uD_qr_lpi_3_dfm_ftd_1[3:0])),
      ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_3
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_2_1
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_1_lpi_3_dfm_1_0}),
      4'b0001, (z_out_1[5:2]), {or_tmp_611 , (fsm_output[7]) , or_tmp_613 , or_731_cse});
  assign nl_z_out_5 = ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux1h_5_nl
      + conv_s2u_40_44({ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_26_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_27_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_28_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_29_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_30_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_31_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_32_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_33_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_34_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_35_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_36_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_37_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_38_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_39_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_40_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_41_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_42_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_43_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_44_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_45_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_46_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_47_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_48_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_49_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_50_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_and_51_nl
      , ac_math_ac_div_20_15_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_20_15_AC_TRN_AC_WRAP_1_neg_D_mux1h_6_nl});
  assign z_out_5 = nl_z_out_5[43:0];

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


  function automatic [33:0] MUX_v_34_2_2;
    input [33:0] input_0;
    input [33:0] input_1;
    input [0:0] sel;
    reg [33:0] result;
  begin
    case (sel)
      1'b0 : begin
        result = input_0;
      end
      default : begin
        result = input_1;
      end
    endcase
    MUX_v_34_2_2 = result;
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


  function automatic [31:0] signext_32_28;
    input [27:0] vector;
  begin
    signext_32_28= {{4{vector[27]}}, vector};
  end
  endfunction


  function automatic [32:0] signext_33_18;
    input [17:0] vector;
  begin
    signext_33_18= {{15{vector[17]}}, vector};
  end
  endfunction


  function automatic [33:0] signext_34_33;
    input [32:0] vector;
  begin
    signext_34_33= {{1{vector[32]}}, vector};
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


  function automatic [5:0] signext_6_2;
    input [1:0] vector;
  begin
    signext_6_2= {{4{vector[1]}}, vector};
  end
  endfunction


  function automatic [8:0] signext_9_2;
    input [1:0] vector;
  begin
    signext_9_2= {{7{vector[1]}}, vector};
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


  function automatic [43:0] conv_s2u_40_44 ;
    input [39:0]  vector ;
  begin
    conv_s2u_40_44 = {{4{vector[39]}}, vector};
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


  function automatic [34:0] conv_u2u_32_35 ;
    input [31:0]  vector ;
  begin
    conv_u2u_32_35 = {{3{1'b0}}, vector};
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
  wire [27:0] fsm_output;
  wire HROW_equal_tmp;
  wire HCOL_equal_tmp;
  wire for_for_nor_tmp;
  wire or_dcpl_27;
  wire or_dcpl_33;
  wire or_tmp_25;
  wire or_tmp_46;
  wire and_38_cse;
  wire and_82_cse;
  wire and_95_cse;
  wire and_92_cse;
  reg HACC_slc_HACC_acc_6_itm;
  reg HACC_idx_HACC_idx_acc_conv_2f_and_itm;
  reg [15:0] reg_HACC_slc_HACC_acc_7_39_14_psp_ftd;
  wire or_61_ssc;
  wire or_64_ssc;
  reg reg_heightIn_rsc_triosy_obj_ld_core_psct_cse;
  reg reg_acc_rsci_ivld_core_psct_cse;
  reg reg_data_in_rsci_irdy_core_psct_cse;
  reg reg_acc_tmp_rsc_cgo_cse;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_and_cse;
  wire nor_3_cse;
  wire and_45_cse;
  wire or_55_rmff;
  reg [7:0] HACC_t_7_0_sva;
  wire [32:0] operator_33_3_true_AC_TRN_AC_WRAP_rshift_itm;
  wire [34:0] z_out;
  wire [35:0] nl_z_out;
  wire [34:0] z_out_1;
  wire [35:0] z_out_2;
  wire [38:0] z_out_3;
  wire [15:0] z_out_5;
  wire [15:0] z_out_6;
  wire [16:0] nl_z_out_6;
  wire [18:0] z_out_7;
  wire [19:0] nl_z_out_7;
  wire [32:0] z_out_8;
  wire [33:0] nl_z_out_8;
  reg [10:0] operator_11_false_io_read_widthIn_rsc_cse_sva;
  reg [9:0] operator_10_false_io_read_heightIn_rsc_cse_sva;
  reg [9:0] HROW_y_sva;
  reg [4:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva;
  reg [32:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva;
  reg [4:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1;
  reg [7:0] HACC_t_7_0_sva_1;
  reg [18:0] WRITE_i_18_0_sva;
  reg [38:0] HACC_mul_1_itm;
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
  reg [12:0] HACC_acc_10_psp;
  reg [14:0] HACC_idx_acc_8_psp;
  wire HACC_idx_acc_3_psp_sva_mx0c0;
  wire HACC_idx_acc_3_psp_sva_mx0c3;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx0c0;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx0c1;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0;
  wire [31:0] ac_math_atan_pi_2mi_return_69_38_sva_1;
  wire [32:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0;
  wire [32:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva_1;
  wire [18:0] for_conc_3_itm_18_0;
  wire [18:0] for_conc_4_itm_18_0;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_or_cse;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_or_cse;
  wire and_514_cse;
  wire and_512_cse;
  wire HACC_t_or_cse;
  wire [37:0] HACC_HACC_mux_rgt;
  reg [4:0] HACC_mul_2_itm_37_33;
  reg [32:0] HACC_mul_2_itm_32_0;
  wire or_95_ssc;
  wire HACC_idx_and_ssc;
  reg reg_HACC_idx_acc_3_psp_ftd;
  reg [4:0] reg_HACC_idx_acc_3_psp_ftd_1;
  reg [10:0] reg_HACC_idx_acc_3_psp_ftd_2;
  wire HACC_idx_and_1_cse;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm;
  wire [25:0] HACC_acc_7_itm_39_14;
  wire WRITE_nor_1_seb;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_ac2;
  wire z_out_4_2;
  wire ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_1_ac2_0;

  wire[18:0] WRITE_i_asn_WRITE_i_18_0_sva_for_and_nl;
  wire[18:0] for_mux1h_4_nl;
  wire[10:0] HCOL_x_HCOL_x_and_nl;
  wire[10:0] HCOL_x_mux_nl;
  wire[0:0] HCOL_x_nor_nl;
  wire[0:0] or_71_nl;
  wire[0:0] or_74_nl;
  wire[0:0] nor_14_nl;
  wire[0:0] or_nl;
  wire[11:0] HACC_mux1h_6_nl;
  wire[11:0] HACC_acc_5_nl;
  wire[12:0] nl_HACC_acc_5_nl;
  wire[0:0] or_272_nl;
  wire[0:0] or_94_nl;
  wire[9:0] HACC_mux_1_nl;
  wire[9:0] HACC_acc_12_nl;
  wire[10:0] nl_HACC_acc_12_nl;
  wire[0:0] or_96_nl;
  wire[34:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_mux_1_nl;
  wire[31:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_nl;
  wire[2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_mux_nl;
  wire[1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_mux_nl;
  wire[0:0] HACC_idx_HACC_idx_acc_conv_2f_or_nl;
  wire[0:0] HACC_idx_HACC_idx_acc_conv_2f_and_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_xor_nl;
  wire[32:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_mux_3_nl;
  wire[39:0] HACC_acc_7_nl;
  wire[40:0] nl_HACC_acc_7_nl;
  wire[9:0] operator_10_false_acc_nl;
  wire[10:0] nl_operator_10_false_acc_nl;
  wire[0:0] Hough_Algorithm_HW_1296_864_houghTransform_not_1_nl;
  wire[0:0] for_nor_nl;
  wire[0:0] for_nor_1_nl;
  wire[16:0] for_mux1h_2_nl;
  wire[1:0] for_mux1h_5_nl;
  wire[0:0] for_or_nl;
  wire[1:0] HACC_mux_nl;
  wire[0:0] HACC_idx_or_3_nl;
  wire[0:0] HACC_idx_or_4_nl;
  wire[34:0] WRITE_WRITE_or_3_nl;
  wire[34:0] WRITE_mux_1_nl;
  wire[31:0] WRITE_mux1h_3_nl;
  wire[35:0] acc_1_nl;
  wire[36:0] nl_acc_1_nl;
  wire[4:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_21_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_22_nl;
  wire[1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_23_nl;
  wire[1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_24_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_25_nl;
  wire[1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_26_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_27_nl;
  wire[2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_28_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_29_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_30_nl;
  wire[1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_31_nl;
  wire[2:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_32_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_33_nl;
  wire[1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_34_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_35_nl;
  wire[1:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_36_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_37_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_38_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_39_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_40_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_41_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_22_nl;
  wire[32:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux1h_2_nl;
  wire[36:0] acc_2_nl;
  wire[37:0] nl_acc_2_nl;
  wire[2:0] HACC_idx_HACC_idx_HACC_idx_nor_1_nl;
  wire[2:0] HACC_idx_mux_17_nl;
  wire[1:0] HACC_idx_HACC_idx_and_15_nl;
  wire[1:0] HACC_idx_mux_18_nl;
  wire[0:0] HACC_idx_not_24_nl;
  wire[0:0] HACC_idx_HACC_idx_and_16_nl;
  wire[0:0] HACC_idx_mux_19_nl;
  wire[1:0] HACC_idx_HACC_idx_and_17_nl;
  wire[1:0] HACC_idx_mux_20_nl;
  wire[0:0] HACC_idx_not_26_nl;
  wire[0:0] HACC_idx_HACC_idx_and_18_nl;
  wire[0:0] HACC_idx_mux_21_nl;
  wire[0:0] HACC_idx_HACC_idx_and_19_nl;
  wire[0:0] HACC_idx_mux_22_nl;
  wire[0:0] HACC_idx_HACC_idx_and_20_nl;
  wire[0:0] HACC_idx_mux_23_nl;
  wire[0:0] HACC_idx_HACC_idx_and_21_nl;
  wire[0:0] HACC_idx_mux_24_nl;
  wire[0:0] HACC_idx_HACC_idx_and_22_nl;
  wire[0:0] HACC_idx_mux_25_nl;
  wire[0:0] HACC_idx_HACC_idx_and_23_nl;
  wire[0:0] HACC_idx_mux_26_nl;
  wire[0:0] HACC_idx_HACC_idx_and_24_nl;
  wire[0:0] HACC_idx_mux_27_nl;
  wire[0:0] HACC_idx_HACC_idx_and_25_nl;
  wire[0:0] HACC_idx_mux_28_nl;
  wire[0:0] HACC_idx_HACC_idx_and_26_nl;
  wire[0:0] HACC_idx_mux_29_nl;
  wire[0:0] HACC_idx_HACC_idx_and_27_nl;
  wire[0:0] HACC_idx_mux_30_nl;
  wire[15:0] HACC_idx_mux1h_9_nl;
  wire[0:0] HACC_idx_HACC_idx_and_28_nl;
  wire[0:0] HACC_idx_mux_31_nl;
  wire[0:0] HACC_idx_or_5_nl;
  wire[33:0] HACC_idx_mux1h_10_nl;
  wire[16:0] HACC_idx_acc_6_nl;
  wire[17:0] nl_HACC_idx_acc_6_nl;
  wire[11:0] HACC_mux_5_nl;
  wire[26:0] HACC_mux_6_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_nl;
  wire[0:0] ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_3_nl;
  wire[16:0] acc_4_nl;
  wire[17:0] nl_acc_4_nl;
  wire[1:0] HCOL_HCOL_and_1_nl;
  wire[1:0] HCOL_mux_2_nl;
  wire[0:0] HCOL_nor_4_nl;
  wire[1:0] HCOL_and_2_nl;
  wire[1:0] HCOL_mux1h_4_nl;
  wire[0:0] HCOL_nor_5_nl;
  wire[0:0] HCOL_or_3_nl;
  wire[0:0] HCOL_mux1h_5_nl;
  wire[10:0] HCOL_mux1h_6_nl;
  wire[0:0] HCOL_or_4_nl;
  wire[14:0] HCOL_HCOL_nand_1_nl;
  wire[14:0] HCOL_mux1h_7_nl;
  wire[0:0] HCOL_or_5_nl;
  wire[0:0] HCOL_not_4_nl;
  wire[15:0] HACC_idx_HACC_idx_acc_conv_2f_mux_1_nl;
  wire[0:0] HACC_idx_HACC_idx_acc_conv_2f_or_2_nl;
  wire[17:0] WRITE_mux1h_4_nl;
  wire[0:0] WRITE_WRITE_or_4_nl;
  wire[0:0] WRITE_WRITE_and_2_nl;
  wire[1:0] WRITE_WRITE_and_3_nl;
  wire[0:0] WRITE_WRITE_or_5_nl;
  wire[2:0] HROW_HROW_and_19_nl;
  wire[0:0] HROW_HROW_and_20_nl;
  wire[1:0] HROW_HROW_and_21_nl;
  wire[0:0] HROW_HROW_and_22_nl;
  wire[0:0] HROW_HROW_and_23_nl;
  wire[0:0] HROW_HROW_and_24_nl;
  wire[0:0] HROW_HROW_and_25_nl;
  wire[0:0] HROW_HROW_and_26_nl;
  wire[0:0] HROW_HROW_and_27_nl;
  wire[0:0] HROW_HROW_and_28_nl;
  wire[0:0] HROW_HROW_and_29_nl;
  wire[0:0] HROW_HROW_and_30_nl;
  wire[0:0] HROW_HROW_and_31_nl;
  wire[0:0] HROW_HROW_and_32_nl;
  wire[1:0] HROW_HROW_and_33_nl;
  wire[0:0] HROW_HROW_and_34_nl;
  wire[0:0] HROW_HROW_and_35_nl;
  wire[0:0] HROW_HROW_and_36_nl;
  wire[0:0] HROW_HROW_and_37_nl;
  wire[0:0] HROW_mux_10_nl;
  wire[0:0] HROW_mux_11_nl;
  wire[0:0] HROW_mux_12_nl;
  wire[0:0] HROW_mux_13_nl;
  wire[0:0] HROW_mux_14_nl;
  wire[0:0] HROW_mux_15_nl;
  wire[0:0] HROW_mux_16_nl;
  wire[1:0] HROW_mux_17_nl;
  wire[0:0] HROW_mux_18_nl;
  wire[32:0] HROW_mux_19_nl;

  // Interconnect Declarations for Component Instantiations 
  wire [32:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_a;
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_a = {ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_32_30
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
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_0};
  wire [4:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_s;
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_s = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva;
  wire [6:0] nl_ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr;
  assign nl_ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr
      = {2'b0, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva};
  wire [32:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a;
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a = {ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_32_30
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
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_0};
  wire [4:0] nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s;
  assign nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva;
  wire [0:0] nl_houghTransform_core_core_fsm_inst_acc_tmp_vinit_C_0_tr0;
  assign nl_houghTransform_core_core_fsm_inst_acc_tmp_vinit_C_0_tr0 = for_for_nor_tmp;
  wire [0:0] nl_houghTransform_core_core_fsm_inst_HCOL_C_0_tr0;
  assign nl_houghTransform_core_core_fsm_inst_HCOL_C_0_tr0 = ~ (z_out_7[8]);
  wire [0:0] nl_houghTransform_core_core_fsm_inst_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1_tr0;
  assign nl_houghTransform_core_core_fsm_inst_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1_tr0
      = ~ HACC_slc_HACC_acc_6_itm;
  wire [0:0] nl_houghTransform_core_core_fsm_inst_HACC_C_15_tr0;
  assign nl_houghTransform_core_core_fsm_inst_HACC_C_15_tr0 = ~ HACC_slc_HACC_acc_6_itm;
  wire [0:0] nl_houghTransform_core_core_fsm_inst_WRITE_C_2_tr0;
  assign nl_houghTransform_core_core_fsm_inst_WRITE_C_2_tr0 = ~ HACC_slc_HACC_acc_6_itm;
  mgc_shift_r_v5 #(.width_a(32'sd33),
  .signd_a(32'sd1),
  .width_s(32'sd5),
  .width_z(32'sd33)) operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg (
      .a(nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_a[32:0]),
      .s(nl_operator_33_3_true_AC_TRN_AC_WRAP_rshift_rg_s[4:0]),
      .z(operator_33_3_true_AC_TRN_AC_WRAP_rshift_itm)
    );
  Hough_Algorithm_HW_1296_864mgc_rom_22_70_32_1_60  ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg
      (
      .addr(nl_ac_math_atan_pi_2mi_read_rom_ac_math_atan_pi_pow2_table_rom_map_1_rg_addr[6:0]),
      .data_out(ac_math_atan_pi_2mi_return_69_38_sva_1)
    );
  mgc_shift_r_v5 #(.width_a(32'sd33),
  .signd_a(32'sd1),
  .width_s(32'sd5),
  .width_z(32'sd33)) operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg (
      .a(nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_a[32:0]),
      .s(nl_operator_33_3_true_AC_TRN_AC_WRAP_1_rshift_rg_s[4:0]),
      .z(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva_1)
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
      .acc_tmp_rsc_cgo_iro(or_55_rmff),
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
      .ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1_tr0(nl_houghTransform_core_core_fsm_inst_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_C_1_tr0[0:0]),
      .HACC_C_15_tr0(nl_houghTransform_core_core_fsm_inst_HACC_C_15_tr0[0:0]),
      .HCOL_C_1_tr0(HCOL_equal_tmp),
      .HROW_C_0_tr0(HROW_equal_tmp),
      .WRITE_C_2_tr0(nl_houghTransform_core_core_fsm_inst_WRITE_C_2_tr0[0:0])
    );
  assign or_55_rmff = and_82_cse | (fsm_output[19]) | (fsm_output[24]) | (fsm_output[20])
      | (fsm_output[18]) | (fsm_output[21]) | (fsm_output[1]) | (fsm_output[2]) |
      and_45_cse;
  assign and_45_cse = HACC_slc_HACC_acc_6_itm & (fsm_output[26]);
  assign HACC_t_or_cse = (fsm_output[21]) | (fsm_output[3]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_or_cse
      = (fsm_output[7]) | (fsm_output[9]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_and_cse
      = core_wen & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_or_cse;
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_or_cse
      = (fsm_output[9:8]!=2'b00);
  assign and_514_cse = (~ HACC_idx_HACC_idx_acc_conv_2f_and_itm) & (fsm_output[9]);
  assign and_512_cse = HACC_idx_HACC_idx_acc_conv_2f_and_itm & (fsm_output[9]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_mux_3_nl
      = MUX_v_33_2_2((z_out_2[32:0]), z_out_8, and_514_cse);
  assign HACC_HACC_mux_rgt = MUX_v_38_2_2(({5'b00000 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_mux_3_nl}),
      (z_out_3[37:0]), fsm_output[11]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0
      = z_out_4_2 & (~ (z_out_7[18]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0
      = ~(z_out_4_2 | (z_out_7[18]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0
      = MUX_v_33_2_2(z_out_8, (z_out_2[32:0]), HACC_idx_HACC_idx_acc_conv_2f_and_itm);
  assign nl_HACC_acc_7_nl = conv_s2u_39_40(HACC_mul_1_itm) + conv_s2u_38_40({HACC_mul_2_itm_37_33
      , HACC_mul_2_itm_32_0});
  assign HACC_acc_7_nl = nl_HACC_acc_7_nl[39:0];
  assign HACC_acc_7_itm_39_14 = readslicef_40_26_14(HACC_acc_7_nl);
  assign nl_operator_10_false_acc_nl = operator_10_false_io_read_heightIn_rsc_cse_sva
      + 10'b1111111111;
  assign operator_10_false_acc_nl = nl_operator_10_false_acc_nl[9:0];
  assign HROW_equal_tmp = HROW_y_sva == operator_10_false_acc_nl;
  assign HCOL_equal_tmp = (WRITE_i_18_0_sva[10:0]) == (z_out_5[10:0]);
  assign for_for_nor_tmp = ~((WRITE_i_18_0_sva!=19'b0000000000000000000));
  assign nor_3_cse = ~((fsm_output[27]) | (fsm_output[24]));
  assign and_38_cse = (~ HROW_equal_tmp) & (fsm_output[23]);
  assign or_dcpl_27 = (fsm_output[7:6]!=2'b00);
  assign or_dcpl_33 = (fsm_output[17:16]!=2'b00);
  assign and_82_cse = HROW_equal_tmp & (fsm_output[23]);
  assign and_92_cse = for_for_nor_tmp & (fsm_output[2]);
  assign and_95_cse = (~ for_for_nor_tmp) & (fsm_output[2]);
  assign or_tmp_25 = nor_3_cse & (~ (fsm_output[0])) & (~((fsm_output[25]) | (fsm_output[1])))
      & (~ (fsm_output[26]));
  assign or_tmp_46 = or_dcpl_27 | (fsm_output[5]) | ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_or_cse;
  assign HACC_idx_acc_3_psp_sva_mx0c0 = (fsm_output[3]) | (fsm_output[21]) | (fsm_output[10]);
  assign HACC_idx_acc_3_psp_sva_mx0c3 = (fsm_output[17:15]!=3'b000);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx0c0
      = (z_out_7[18]) & (fsm_output[7]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx0c1
      = (~ (z_out_7[18])) & (fsm_output[7]);
  assign for_conc_3_itm_18_0 = MUX_v_19_2_2(19'b1100001101001111111, (z_out[18:0]),
      fsm_output[2]);
  assign Hough_Algorithm_HW_1296_864_houghTransform_not_1_nl = ~ (fsm_output[23]);
  assign for_conc_4_itm_18_0 = MUX_v_19_2_2(19'b0000000000000000000, WRITE_i_18_0_sva,
      Hough_Algorithm_HW_1296_864_houghTransform_not_1_nl);
  assign or_61_ssc = (fsm_output[2:1]!=2'b00);
  assign or_64_ssc = (fsm_output[26]) | (fsm_output[23]);
  assign for_nor_nl = ~(and_82_cse | and_45_cse | (fsm_output[18]));
  assign acc_tmp_rsci_re_d = {1'b1 , for_nor_nl};
  assign for_nor_1_nl = ~((fsm_output[20]) | (fsm_output[1]) | and_95_cse);
  assign acc_tmp_rsci_we_d = {1'b1 , for_nor_1_nl};
  assign acc_tmp_rsci_data_in_d = MUX_v_16_2_2(16'b0000000000000000, z_out_5, (fsm_output[20]));
  assign for_mux1h_2_nl = MUX1HOT_v_17_4_2((for_conc_3_itm_18_0[18:2]), (z_out_2[16:0]),
      ({reg_HACC_idx_acc_3_psp_ftd , reg_HACC_idx_acc_3_psp_ftd_1 , reg_HACC_idx_acc_3_psp_ftd_2}),
      (for_conc_4_itm_18_0[18:2]), {or_61_ssc , (fsm_output[18]) , (fsm_output[20])
      , or_64_ssc});
  assign for_or_nl = (fsm_output[18]) | (fsm_output[20]);
  assign for_mux1h_5_nl = MUX1HOT_v_2_3_2((for_conc_3_itm_18_0[1:0]), (HACC_t_7_0_sva[1:0]),
      (for_conc_4_itm_18_0[1:0]), {or_61_ssc , for_or_nl , or_64_ssc});
  assign acc_tmp_rsci_addr_d = {for_mux1h_2_nl , for_mux1h_5_nl};
  assign or_95_ssc = or_dcpl_27 | ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_or_cse;
  assign HACC_idx_and_ssc = core_wen & ((fsm_output[18]) | (fsm_output[14]) | HACC_idx_acc_3_psp_sva_mx0c0
      | or_tmp_46 | HACC_idx_acc_3_psp_sva_mx0c3);
  assign HACC_idx_and_1_cse = (~ or_dcpl_33) & HACC_idx_acc_3_psp_sva_mx0c3;
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm
      = and_514_cse | and_512_cse;
  assign WRITE_nor_1_seb = ~((fsm_output[8:7]!=2'b00));
  always @(posedge clk) begin
    if ( rst ) begin
      reg_heightIn_rsc_triosy_obj_ld_core_psct_cse <= 1'b0;
      reg_acc_tmp_rsc_cgo_cse <= 1'b0;
      reg_acc_rsci_ivld_core_psct_cse <= 1'b0;
      reg_data_in_rsci_irdy_core_psct_cse <= 1'b0;
      operator_10_false_io_read_heightIn_rsc_cse_sva <= 10'b0000000000;
      operator_11_false_io_read_widthIn_rsc_cse_sva <= 11'b00000000000;
      WRITE_i_18_0_sva <= 19'b0000000000000000000;
      HACC_acc_10_psp <= 13'b0000000000000;
      HACC_idx_acc_8_psp <= 15'b000000000000000;
      HACC_mul_1_itm <= 39'b000000000000000000000000000000000000000;
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1
          <= 5'b00000;
      HACC_idx_HACC_idx_acc_conv_2f_and_itm <= 1'b0;
    end
    else if ( core_wen ) begin
      reg_heightIn_rsc_triosy_obj_ld_core_psct_cse <= (~ HACC_slc_HACC_acc_6_itm)
          & (fsm_output[26]);
      reg_acc_tmp_rsc_cgo_cse <= or_55_rmff;
      reg_acc_rsci_ivld_core_psct_cse <= fsm_output[25];
      reg_data_in_rsci_irdy_core_psct_cse <= and_38_cse | ((~ HCOL_equal_tmp) & (fsm_output[22]))
          | and_92_cse;
      operator_10_false_io_read_heightIn_rsc_cse_sva <= MUX_v_10_2_2(heightIn_rsci_idat,
          operator_10_false_io_read_heightIn_rsc_cse_sva, or_tmp_25);
      operator_11_false_io_read_widthIn_rsc_cse_sva <= MUX_v_11_2_2(widthIn_rsci_idat,
          operator_11_false_io_read_widthIn_rsc_cse_sva, or_tmp_25);
      WRITE_i_18_0_sva <= MUX_v_19_2_2(WRITE_i_asn_WRITE_i_18_0_sva_for_and_nl, (z_out[18:0]),
          or_nl);
      HACC_acc_10_psp <= MUX1HOT_v_13_3_2((z_out_5[12:0]), HACC_acc_10_psp, ({1'b0
          , HACC_mux1h_6_nl}), {(fsm_output[4]) , or_tmp_46 , or_94_nl});
      HACC_idx_acc_8_psp <= MUX_v_15_2_2(({5'b00000 , HACC_mux_1_nl}), (z_out_5[14:0]),
          fsm_output[17]);
      HACC_mul_1_itm <= MUX1HOT_v_39_3_2(({4'b0000 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_mux_1_nl}),
          ({7'b0000000 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_nl}),
          z_out_3, {(fsm_output[8]) , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_or_cse
          , (fsm_output[12])});
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1
          <= MUX_v_5_2_2((z_out_7[4:0]), ({2'b00 , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_mux_nl}),
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_or_cse);
      HACC_idx_HACC_idx_acc_conv_2f_and_itm <= MUX1HOT_s_1_3_2((z_out_2[35]), HACC_idx_HACC_idx_acc_conv_2f_or_nl,
          HACC_idx_HACC_idx_acc_conv_2f_and_nl, {(fsm_output[8]) , (fsm_output[13])
          , (fsm_output[14])});
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      acc_rsci_idat <= 16'b0000000000000000;
    end
    else if ( core_wen & (fsm_output[25]) ) begin
      acc_rsci_idat <= acc_tmp_rsci_data_out_d_oreg;
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HROW_y_sva <= 10'b0000000000;
    end
    else if ( core_wen & ((fsm_output[2]) | (fsm_output[23])) ) begin
      HROW_y_sva <= MUX_v_10_2_2(10'b0000000000, (z_out_8[9:0]), (fsm_output[23]));
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HACC_t_7_0_sva <= 8'b00000000;
    end
    else if ( core_wen & HACC_t_or_cse ) begin
      HACC_t_7_0_sva <= MUX_v_8_2_2(8'b00000000, HACC_t_7_0_sva_1, (fsm_output[21]));
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
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
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_0
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
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva
          <= 5'b00000;
    end
    else if ( ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_and_cse
        ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_32_30
          <= MUX_v_3_2_2(({{2{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}),
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[32:30]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_29
          <= MUX_s_1_2_2((z_out_7[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[29]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_28_27
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}),
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[28:27]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_26
          <= MUX_s_1_2_2((z_out_7[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[26]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_25
          <= MUX_s_1_2_2((z_out_7[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[25]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_24
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[24]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_23
          <= MUX_s_1_2_2((z_out_7[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[23]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_22
          <= MUX_s_1_2_2((z_out_7[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[22]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_21
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[21]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_20
          <= MUX_s_1_2_2((z_out_7[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[20]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_19
          <= MUX_s_1_2_2((z_out_7[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[19]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_18
          <= MUX_s_1_2_2((z_out_7[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[18]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_17
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[17]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_16
          <= MUX_s_1_2_2((z_out_7[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[16]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_15_14
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0}),
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[15:14]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_13
          <= MUX_s_1_2_2((z_out_7[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[13]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_12
          <= MUX_s_1_2_2((z_out_7[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[12]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_11
          <= MUX_s_1_2_2((z_out_7[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[11]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_10
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[10]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_9
          <= MUX_s_1_2_2((z_out_7[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[9]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_8
          <= MUX_s_1_2_2((z_out_7[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[8]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_7
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[7]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_6
          <= MUX_s_1_2_2((z_out_7[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[6]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_5
          <= MUX_s_1_2_2((z_out_7[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[5]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_4
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_and_15_cse_mx0w0,
          (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[4]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_3
          <= MUX_s_1_2_2((z_out_7[18]), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[3]),
          fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_0
          <= (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[0])
          & (fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_32_30
          <= MUX_v_3_2_2(3'b000, (z_out_1[32:30]), (fsm_output[9]));
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_29
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
          (z_out_1[29]), fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_28_27
          <= MUX_v_2_2_2(2'b00, (z_out_1[28:27]), (fsm_output[9]));
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_26_25
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (z_out_1[26:25]), fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_24
          <= (z_out_1[24]) & (fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_23_22
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (z_out_1[23:22]), fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_21
          <= (z_out_1[21]) & (fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_20_18
          <= MUX_v_3_2_2(({{2{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (z_out_1[20:18]), fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_17
          <= (z_out_1[17]) & (fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_16
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
          (z_out_1[16]), fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_15_14
          <= MUX_v_2_2_2(2'b00, (z_out_1[15:14]), (fsm_output[9]));
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_13_11
          <= MUX_v_3_2_2(({{2{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (z_out_1[13:11]), fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_10
          <= (z_out_1[10]) & (fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_9_8
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (z_out_1[9:8]), fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_7
          <= (z_out_1[7]) & (fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_6_5
          <= MUX_v_2_2_2(({{1{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}},
          ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0}),
          (z_out_1[6:5]), fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_4
          <= (z_out_1[4]) & (fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_3
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
          (z_out_1[3]), fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_2
          <= (z_out_1[2]) & (fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_1
          <= MUX_s_1_2_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_nor_5_cse_mx0w0,
          (z_out_1[1]), fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_0
          <= (z_out_1[0]) & (fsm_output[9]);
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva
          <= MUX_v_5_2_2(5'b00000, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1,
          (fsm_output[9]));
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1
          <= 2'b00;
    end
    else if ( core_wen & (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx0c0
        | ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx0c1
        | (fsm_output[9])) ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1
          <= MUX1HOT_v_2_3_2(2'b01, ({{1{z_out_4_2}}, z_out_4_2}), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_dfm_mx0[2:1]),
          {ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx0c0
          , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1_mx0c1
          , (fsm_output[9])});
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva
          <= 33'b000000000000000000000000000000000;
    end
    else if ( core_wen & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_or_cse
        ) begin
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva
          <= MUX_v_33_2_2(operator_33_3_true_AC_TRN_AC_WRAP_rshift_itm, ({(z_out_1[32:6])
          , 6'b000000}), fsm_output[9]);
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HACC_slc_HACC_acc_6_itm <= 1'b0;
    end
    else if ( core_wen & ((fsm_output[25]) | (fsm_output[8]) | (fsm_output[11]))
        ) begin
      HACC_slc_HACC_acc_6_itm <= MUX1HOT_s_1_3_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_xor_nl,
          (z_out_5[6]), (z_out_7[12]), {(fsm_output[8]) , (fsm_output[11]) , (fsm_output[25])});
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HACC_mul_2_itm_37_33 <= 5'b00000;
    end
    else if ( ((fsm_output[11:9]!=3'b000)) & core_wen ) begin
      HACC_mul_2_itm_37_33 <= HACC_HACC_mux_rgt[37:33];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HACC_mul_2_itm_32_0 <= 33'b000000000000000000000000000000000;
    end
    else if ( ((fsm_output[11]) | (fsm_output[9])) & core_wen ) begin
      HACC_mul_2_itm_32_0 <= HACC_HACC_mux_rgt[32:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      HACC_t_7_0_sva_1 <= 8'b00000000;
    end
    else if ( core_wen & (fsm_output[10]) ) begin
      HACC_t_7_0_sva_1 <= z_out_6[7:0];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      reg_HACC_slc_HACC_acc_7_39_14_psp_ftd <= 16'b0000000000000000;
    end
    else if ( core_wen & (fsm_output[13]) ) begin
      reg_HACC_slc_HACC_acc_7_39_14_psp_ftd <= HACC_acc_7_itm_39_14[25:10];
    end
  end
  always @(posedge clk) begin
    if ( rst ) begin
      reg_HACC_idx_acc_3_psp_ftd <= 1'b0;
      reg_HACC_idx_acc_3_psp_ftd_1 <= 5'b00000;
      reg_HACC_idx_acc_3_psp_ftd_2 <= 11'b00000000000;
    end
    else if ( HACC_idx_and_ssc ) begin
      reg_HACC_idx_acc_3_psp_ftd <= z_out_2[16];
      reg_HACC_idx_acc_3_psp_ftd_1 <= MUX1HOT_v_5_5_2(({3'b000 , HACC_mux_nl}), ({2'b00
          , (z_out_5[13:11])}), (z_out_6[15:11]), reg_HACC_idx_acc_3_psp_ftd_1, (z_out_2[15:11]),
          {or_tmp_46 , (fsm_output[14]) , HACC_idx_and_1_cse , or_dcpl_33 , (fsm_output[18])});
      reg_HACC_idx_acc_3_psp_ftd_2 <= MUX1HOT_v_11_4_2((z_out_5[10:0]), reg_HACC_idx_acc_3_psp_ftd_2,
          (z_out_6[10:0]), (z_out_2[10:0]), {HACC_idx_or_3_nl , HACC_idx_or_4_nl
          , HACC_idx_and_1_cse , (fsm_output[18])});
    end
  end
  assign HCOL_x_mux_nl = MUX_v_11_2_2((WRITE_i_18_0_sva[10:0]), reg_HACC_idx_acc_3_psp_ftd_2,
      fsm_output[22]);
  assign HCOL_x_nor_nl = ~((fsm_output[27]) | (fsm_output[0]) | (fsm_output[25])
      | (fsm_output[26]) | (fsm_output[2]) | (fsm_output[23]));
  assign HCOL_x_HCOL_x_and_nl = MUX_v_11_2_2(11'b00000000000, HCOL_x_mux_nl, HCOL_x_nor_nl);
  assign or_71_nl = (~((~ nor_3_cse) | (fsm_output[0]) | (fsm_output[25]) | (fsm_output[1])
      | (fsm_output[26]) | (fsm_output[2]) | (fsm_output[23]))) | and_38_cse | and_92_cse;
  assign or_74_nl = (fsm_output[26:25]!=2'b00);
  assign for_mux1h_4_nl = MUX1HOT_v_19_3_2(19'b1100001101001111111, ({8'b00000000
      , HCOL_x_HCOL_x_and_nl}), WRITE_i_18_0_sva, {(fsm_output[1]) , or_71_nl , or_74_nl});
  assign nor_14_nl = ~(and_82_cse | (fsm_output[27]) | (fsm_output[0]));
  assign WRITE_i_asn_WRITE_i_18_0_sva_for_and_nl = MUX_v_19_2_2(19'b0000000000000000000,
      for_mux1h_4_nl, nor_14_nl);
  assign or_nl = and_95_cse | (fsm_output[24]);
  assign nl_HACC_acc_5_nl = conv_u2s_11_12(WRITE_i_18_0_sva[10:0]) + conv_s2s_11_12({1'b1
      , (~ (operator_11_false_io_read_widthIn_rsc_cse_sva[10:1]))}) + 12'b000000000001;
  assign HACC_acc_5_nl = nl_HACC_acc_5_nl[11:0];
  assign or_272_nl = (fsm_output[17]) | (fsm_output[11]);
  assign HACC_mux1h_6_nl = MUX1HOT_v_12_3_2(HACC_acc_5_nl, (HACC_acc_10_psp[11:0]),
      (z_out_5[11:0]), {(fsm_output[10]) , or_272_nl , (fsm_output[16])});
  assign or_94_nl = or_dcpl_33 | (fsm_output[11:10]!=2'b00);
  assign nl_HACC_acc_12_nl = conv_s2u_9_10({(reg_HACC_idx_acc_3_psp_ftd_1[1:0]) ,
      (reg_HACC_idx_acc_3_psp_ftd_2[10:4])}) + ({(HACC_t_7_0_sva[6:0]) , 3'b111});
  assign HACC_acc_12_nl = nl_HACC_acc_12_nl[9:0];
  assign or_96_nl = (fsm_output[9:7]!=3'b000);
  assign HACC_mux_1_nl = MUX_v_10_2_2(HACC_acc_12_nl, (HACC_idx_acc_8_psp[9:0]),
      or_96_nl);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_mux_1_nl
      = MUX_v_35_2_2(z_out, z_out_1, z_out_2[35]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_else_and_nl
      = MUX_v_32_2_2(32'b00000000000000000000000000000000, (HACC_mul_1_itm[31:0]),
      (fsm_output[9]));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_mux_nl
      = MUX_v_2_2_2(({{1{z_out_4_2}}, z_out_4_2}), 2'b01, z_out_7[18]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_acc_a_mux_nl
      = MUX_v_3_2_2((signext_3_2(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_mux_nl)),
      (HACC_mul_1_itm[34:32]), fsm_output[9]);
  assign HACC_idx_HACC_idx_acc_conv_2f_or_nl = (HACC_acc_7_itm_39_14[9:0]!=10'b0000000000);
  assign HACC_idx_HACC_idx_acc_conv_2f_and_nl = HACC_idx_HACC_idx_acc_conv_2f_and_itm
      & (z_out_5[13]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_xor_nl
      = (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_1_ac2_0
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_ac2)
      ^ ((HACC_idx_acc_8_psp[9]) | (~ (fsm_output[7])));
  assign HACC_mux_nl = MUX_v_2_2_2((z_out_5[12:11]), (reg_HACC_idx_acc_3_psp_ftd_1[1:0]),
      or_95_ssc);
  assign HACC_idx_or_3_nl = HACC_idx_acc_3_psp_sva_mx0c0 | (fsm_output[14]) | ((~
      or_95_ssc) & or_tmp_46);
  assign HACC_idx_or_4_nl = or_95_ssc | or_dcpl_33;
  assign WRITE_mux_1_nl = MUX_v_35_2_2(({{16{WRITE_i_18_0_sva[18]}}, WRITE_i_18_0_sva}),
      ({(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1[2:0])
      , (HACC_mul_1_itm[31:0])}), fsm_output[8]);
  assign WRITE_WRITE_or_3_nl = MUX_v_35_2_2(WRITE_mux_1_nl, 35'b11111111111111111111111111111111111,
      (fsm_output[2]));
  assign WRITE_mux1h_3_nl = MUX1HOT_v_32_3_2(32'b00000000000000000000000000000001,
      ac_math_atan_pi_2mi_return_69_38_sva_1, ({{13{WRITE_i_18_0_sva[18]}}, WRITE_i_18_0_sva}),
      {(fsm_output[24]) , (fsm_output[8]) , (fsm_output[2])});
  assign nl_z_out = WRITE_WRITE_or_3_nl + conv_u2u_32_35(WRITE_mux1h_3_nl);
  assign z_out = nl_z_out[34:0];
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_21_nl
      = MUX_v_5_2_2(({(ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1[2:0])
      , (HACC_mul_1_itm[31:30])}), ({{2{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_32_30[2]}},
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_32_30}),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_22_nl
      = MUX_s_1_2_2((HACC_mul_1_itm[29]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_29,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_23_nl
      = MUX_v_2_2_2((HACC_mul_1_itm[28:27]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_28_27,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_24_nl
      = MUX_v_2_2_2((HACC_mul_1_itm[26:25]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_26_25,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_25_nl
      = MUX_s_1_2_2((HACC_mul_1_itm[24]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_24,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_26_nl
      = MUX_v_2_2_2((HACC_mul_1_itm[23:22]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_23_22,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_27_nl
      = MUX_s_1_2_2((HACC_mul_1_itm[21]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_21,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_28_nl
      = MUX_v_3_2_2((HACC_mul_1_itm[20:18]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_20_18,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_29_nl
      = MUX_s_1_2_2((HACC_mul_1_itm[17]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_17,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_30_nl
      = MUX_s_1_2_2((HACC_mul_1_itm[16]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_16,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_31_nl
      = MUX_v_2_2_2((HACC_mul_1_itm[15:14]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_15_14,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_32_nl
      = MUX_v_3_2_2((HACC_mul_1_itm[13:11]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_13_11,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_33_nl
      = MUX_s_1_2_2((HACC_mul_1_itm[10]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_10,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_34_nl
      = MUX_v_2_2_2((HACC_mul_1_itm[9:8]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_9_8,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_35_nl
      = MUX_s_1_2_2((HACC_mul_1_itm[7]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_7,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_36_nl
      = MUX_v_2_2_2((HACC_mul_1_itm[6:5]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_6_5,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_37_nl
      = MUX_s_1_2_2((HACC_mul_1_itm[4]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_4,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_38_nl
      = MUX_s_1_2_2((HACC_mul_1_itm[3]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_3,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_39_nl
      = MUX_s_1_2_2((HACC_mul_1_itm[2]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_2,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_40_nl
      = MUX_s_1_2_2((HACC_mul_1_itm[1]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_1,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_41_nl
      = MUX_s_1_2_2((HACC_mul_1_itm[0]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_x_lpi_4_dfm_1_0,
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_1_itm);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_22_nl
      = (~ and_512_cse) | (fsm_output[8]) | and_514_cse;
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux1h_2_nl
      = MUX1HOT_v_33_3_2(({1'b1 , (~ ac_math_atan_pi_2mi_return_69_38_sva_1)}), (~
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva_1),
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_y_2mi_sva_1,
      {(fsm_output[8]) , and_514_cse , and_512_cse});
  assign nl_acc_1_nl = ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_21_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_22_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_23_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_24_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_25_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_26_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_27_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_28_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_29_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_30_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_31_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_32_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_33_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_34_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_35_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_36_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_37_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_38_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_39_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_40_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux_41_nl
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_or_22_nl})
      + conv_s2u_34_36({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_if_mux1h_2_nl
      , 1'b1});
  assign acc_1_nl = nl_acc_1_nl[35:0];
  assign z_out_1 = readslicef_36_35_1(acc_1_nl);
  assign HACC_idx_mux_17_nl = MUX_v_3_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva_1[2:0]),
      (signext_3_1(~ (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_32_30[2]))),
      fsm_output[9]);
  assign HACC_idx_HACC_idx_HACC_idx_nor_1_nl = ~(MUX_v_3_2_2(HACC_idx_mux_17_nl,
      3'b111, (fsm_output[18])));
  assign HACC_idx_mux_18_nl = MUX_v_2_2_2((~ (HACC_mul_1_itm[31:30])), (ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_32_30[1:0]),
      fsm_output[9]);
  assign HACC_idx_not_24_nl = ~ (fsm_output[18]);
  assign HACC_idx_HACC_idx_and_15_nl = MUX_v_2_2_2(2'b00, HACC_idx_mux_18_nl, HACC_idx_not_24_nl);
  assign HACC_idx_mux_19_nl = MUX_s_1_2_2((~ (HACC_mul_1_itm[29])), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_29,
      fsm_output[9]);
  assign HACC_idx_HACC_idx_and_16_nl = HACC_idx_mux_19_nl & (~ (fsm_output[18]));
  assign HACC_idx_mux_20_nl = MUX_v_2_2_2((~ (HACC_mul_1_itm[28:27])), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_28_27,
      fsm_output[9]);
  assign HACC_idx_not_26_nl = ~ (fsm_output[18]);
  assign HACC_idx_HACC_idx_and_17_nl = MUX_v_2_2_2(2'b00, HACC_idx_mux_20_nl, HACC_idx_not_26_nl);
  assign HACC_idx_mux_21_nl = MUX_s_1_2_2((~ (HACC_mul_1_itm[26])), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_26,
      fsm_output[9]);
  assign HACC_idx_HACC_idx_and_18_nl = HACC_idx_mux_21_nl & (~ (fsm_output[18]));
  assign HACC_idx_mux_22_nl = MUX_s_1_2_2((~ (HACC_mul_1_itm[25])), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_25,
      fsm_output[9]);
  assign HACC_idx_HACC_idx_and_19_nl = HACC_idx_mux_22_nl & (~ (fsm_output[18]));
  assign HACC_idx_mux_23_nl = MUX_s_1_2_2((~ (HACC_mul_1_itm[24])), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_24,
      fsm_output[9]);
  assign HACC_idx_HACC_idx_and_20_nl = HACC_idx_mux_23_nl & (~ (fsm_output[18]));
  assign HACC_idx_mux_24_nl = MUX_s_1_2_2((~ (HACC_mul_1_itm[23])), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_23,
      fsm_output[9]);
  assign HACC_idx_HACC_idx_and_21_nl = HACC_idx_mux_24_nl & (~ (fsm_output[18]));
  assign HACC_idx_mux_25_nl = MUX_s_1_2_2((~ (HACC_mul_1_itm[22])), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_22,
      fsm_output[9]);
  assign HACC_idx_HACC_idx_and_22_nl = HACC_idx_mux_25_nl & (~ (fsm_output[18]));
  assign HACC_idx_mux_26_nl = MUX_s_1_2_2((~ (HACC_mul_1_itm[21])), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_21,
      fsm_output[9]);
  assign HACC_idx_HACC_idx_and_23_nl = HACC_idx_mux_26_nl & (~ (fsm_output[18]));
  assign HACC_idx_mux_27_nl = MUX_s_1_2_2((~ (HACC_mul_1_itm[20])), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_20,
      fsm_output[9]);
  assign HACC_idx_HACC_idx_and_24_nl = HACC_idx_mux_27_nl & (~ (fsm_output[18]));
  assign HACC_idx_mux_28_nl = MUX_s_1_2_2((~ (HACC_mul_1_itm[19])), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_19,
      fsm_output[9]);
  assign HACC_idx_HACC_idx_and_25_nl = HACC_idx_mux_28_nl & (~ (fsm_output[18]));
  assign HACC_idx_mux_29_nl = MUX_s_1_2_2((~ (HACC_mul_1_itm[18])), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_18,
      fsm_output[9]);
  assign HACC_idx_HACC_idx_and_26_nl = HACC_idx_mux_29_nl & (~ (fsm_output[18]));
  assign HACC_idx_mux_30_nl = MUX_s_1_2_2((~ (HACC_mul_1_itm[17])), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_17,
      fsm_output[9]);
  assign HACC_idx_HACC_idx_and_27_nl = HACC_idx_mux_30_nl & (~ (fsm_output[18]));
  assign HACC_idx_mux1h_9_nl = MUX1HOT_v_16_3_2(({HACC_idx_acc_8_psp , 1'b0}), (~
      (HACC_mul_1_itm[16:1])), ({ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_16
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
      , ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1}),
      {(fsm_output[18]) , (fsm_output[8]) , (fsm_output[9])});
  assign HACC_idx_mux_31_nl = MUX_s_1_2_2((~ (HACC_mul_1_itm[0])), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_0,
      fsm_output[9]);
  assign HACC_idx_HACC_idx_and_28_nl = HACC_idx_mux_31_nl & (~ (fsm_output[18]));
  assign HACC_idx_or_5_nl = (~ (fsm_output[18])) | (fsm_output[8]) | (fsm_output[9]);
  assign nl_HACC_idx_acc_6_nl = ({reg_HACC_idx_acc_3_psp_ftd_2 , (HACC_t_7_0_sva[7:2])})
      + conv_s2s_16_17({(HACC_acc_10_psp[11:0]) , (reg_HACC_idx_acc_3_psp_ftd_2[3:0])});
  assign HACC_idx_acc_6_nl = nl_HACC_idx_acc_6_nl[16:0];
  assign HACC_idx_mux1h_10_nl = MUX1HOT_v_34_3_2((signext_34_17(HACC_idx_acc_6_nl)),
      ({(HACC_idx_acc_8_psp[9:0]) , (reg_HACC_idx_acc_3_psp_ftd_2[3:0]) , (HACC_acc_10_psp[3:0])
      , 16'b0000000000000000}), (signext_34_33(~ ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva)),
      {(fsm_output[18]) , (fsm_output[8]) , (fsm_output[9])});
  assign nl_acc_2_nl = conv_s2u_36_37({HACC_idx_HACC_idx_HACC_idx_nor_1_nl , HACC_idx_HACC_idx_and_15_nl
      , HACC_idx_HACC_idx_and_16_nl , HACC_idx_HACC_idx_and_17_nl , HACC_idx_HACC_idx_and_18_nl
      , HACC_idx_HACC_idx_and_19_nl , HACC_idx_HACC_idx_and_20_nl , HACC_idx_HACC_idx_and_21_nl
      , HACC_idx_HACC_idx_and_22_nl , HACC_idx_HACC_idx_and_23_nl , HACC_idx_HACC_idx_and_24_nl
      , HACC_idx_HACC_idx_and_25_nl , HACC_idx_HACC_idx_and_26_nl , HACC_idx_HACC_idx_and_27_nl
      , HACC_idx_mux1h_9_nl , HACC_idx_HACC_idx_and_28_nl , HACC_idx_or_5_nl}) +
      conv_s2u_35_37({HACC_idx_mux1h_10_nl , 1'b1});
  assign acc_2_nl = nl_acc_2_nl[36:0];
  assign z_out_2 = readslicef_37_36_1(acc_2_nl);
  assign HACC_mux_5_nl = MUX_v_12_2_2((HACC_acc_10_psp[11:0]), ({{1{reg_HACC_idx_acc_3_psp_ftd_2[10]}},
      reg_HACC_idx_acc_3_psp_ftd_2}), fsm_output[11]);
  assign HACC_mux_6_nl = MUX_v_27_2_2((ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva[32:6]),
      (HACC_mul_2_itm_32_0[32:6]), fsm_output[11]);
  assign z_out_3 = conv_u2u_39_39($signed(HACC_mux_5_nl) * $signed(HACC_mux_6_nl));
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_1_ac2_0
      = MUX_s_1_2_2((z_out_7[4]), (HACC_idx_acc_8_psp[9]), fsm_output[7]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_nl
      = MUX_s_1_2_2((z_out_7[2]), (HACC_idx_acc_8_psp[8]), fsm_output[7]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_3_nl
      = MUX_s_1_2_2((z_out_7[3]), (HACC_idx_acc_8_psp[9]), fsm_output[7]);
  assign ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_ac2
      = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_2_nl
      & ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_3_nl;
  assign z_out_4_2 = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_mux_1_ac2_0
      ^ ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_and_ac2;
  assign HCOL_mux_2_nl = MUX_v_2_2_2((signext_2_1(~ (reg_HACC_idx_acc_3_psp_ftd_1[3]))),
      (acc_tmp_rsci_data_out_d_oreg[15:14]), fsm_output[20]);
  assign HCOL_nor_4_nl = ~(HACC_t_or_cse | (fsm_output[10]) | (fsm_output[5]) | (fsm_output[4])
      | (fsm_output[16]) | (fsm_output[11]) | (fsm_output[14]) | (fsm_output[22]));
  assign HCOL_HCOL_and_1_nl = MUX_v_2_2_2(2'b00, HCOL_mux_2_nl, HCOL_nor_4_nl);
  assign HCOL_mux1h_4_nl = MUX1HOT_v_2_3_2((~ (reg_HACC_idx_acc_3_psp_ftd_1[2:1])),
      (reg_HACC_slc_HACC_acc_7_39_14_psp_ftd[15:14]), (acc_tmp_rsci_data_out_d_oreg[13:12]),
      {(fsm_output[17]) , (fsm_output[14]) , (fsm_output[20])});
  assign HCOL_nor_5_nl = ~(HACC_t_or_cse | (fsm_output[10]) | (fsm_output[5]) | (fsm_output[4])
      | (fsm_output[16]) | (fsm_output[11]) | (fsm_output[22]));
  assign HCOL_and_2_nl = MUX_v_2_2_2(2'b00, HCOL_mux1h_4_nl, HCOL_nor_5_nl);
  assign HCOL_mux1h_5_nl = MUX1HOT_s_1_5_2((HACC_t_7_0_sva[7]), (reg_HACC_idx_acc_3_psp_ftd_1[4]),
      (~ (reg_HACC_idx_acc_3_psp_ftd_1[0])), (reg_HACC_slc_HACC_acc_7_39_14_psp_ftd[13]),
      (acc_tmp_rsci_data_out_d_oreg[11]), {(fsm_output[5]) , (fsm_output[16]) , (fsm_output[17])
      , (fsm_output[14]) , (fsm_output[20])});
  assign HCOL_or_3_nl = (HCOL_mux1h_5_nl & (~(HACC_t_or_cse | (fsm_output[10]) |
      (fsm_output[11]) | (fsm_output[22])))) | (fsm_output[4]);
  assign HCOL_mux1h_6_nl = MUX1HOT_v_11_10_2((WRITE_i_18_0_sva[10:0]), ({1'b0 , HROW_y_sva}),
      ({(HACC_t_7_0_sva[6:0]) , 4'b0001}), ({3'b000 , (~ HACC_t_7_0_sva)}), ({(reg_HACC_idx_acc_3_psp_ftd_1[3:0])
      , (reg_HACC_idx_acc_3_psp_ftd_2[10:4])}), (~ reg_HACC_idx_acc_3_psp_ftd_2),
      ({5'b00000 , (HACC_t_7_0_sva_1[7:2])}), (reg_HACC_slc_HACC_acc_7_39_14_psp_ftd[12:2]),
      operator_11_false_io_read_widthIn_rsc_cse_sva, (acc_tmp_rsci_data_out_d_oreg[10:0]),
      {HACC_t_or_cse , (fsm_output[10]) , (fsm_output[5]) , (fsm_output[4]) , (fsm_output[16])
      , (fsm_output[17]) , (fsm_output[11]) , (fsm_output[14]) , (fsm_output[22])
      , (fsm_output[20])});
  assign HCOL_or_4_nl = (~(HACC_t_or_cse | (fsm_output[5]) | (fsm_output[4]) | (fsm_output[16])
      | (fsm_output[17]) | (fsm_output[11]) | (fsm_output[14]) | (fsm_output[22])
      | (fsm_output[20]))) | (fsm_output[10]);
  assign HCOL_or_5_nl = HACC_t_or_cse | (fsm_output[16]) | (fsm_output[20]);
  assign HCOL_mux1h_7_nl = MUX1HOT_v_15_7_2(15'b111111111111110, ({6'b000000 , (operator_10_false_io_read_heightIn_rsc_cse_sva[9:1])}),
      ({6'b000000 , (~ (HACC_acc_10_psp[12:4]))}), ({3'b111 , HACC_t_7_0_sva , 4'b1110}),
      ({(reg_HACC_idx_acc_3_psp_ftd_1[1:0]) , reg_HACC_idx_acc_3_psp_ftd_2 , 2'b10}),
      15'b000000000101100, 15'b111111100011010, {HCOL_or_5_nl , (fsm_output[10])
      , (fsm_output[5]) , (fsm_output[4]) , (fsm_output[17]) , (fsm_output[11]) ,
      (fsm_output[14])});
  assign HCOL_not_4_nl = ~ (fsm_output[22]);
  assign HCOL_HCOL_nand_1_nl = ~(MUX_v_15_2_2(15'b000000000000000, HCOL_mux1h_7_nl,
      HCOL_not_4_nl));
  assign nl_acc_4_nl = ({HCOL_HCOL_and_1_nl , HCOL_and_2_nl , HCOL_or_3_nl , HCOL_mux1h_6_nl
      , HCOL_or_4_nl}) + conv_s2u_16_17({HCOL_HCOL_nand_1_nl , 1'b1});
  assign acc_4_nl = nl_acc_4_nl[16:0];
  assign z_out_5 = readslicef_17_16_1(acc_4_nl);
  assign HACC_idx_HACC_idx_acc_conv_2f_mux_1_nl = MUX_v_16_2_2(({(reg_HACC_idx_acc_3_psp_ftd_1[2:0])
      , reg_HACC_idx_acc_3_psp_ftd_2 , (reg_HACC_slc_HACC_acc_7_39_14_psp_ftd[1:0])}),
      ({{8{HACC_t_7_0_sva[7]}}, HACC_t_7_0_sva}), fsm_output[10]);
  assign HACC_idx_HACC_idx_acc_conv_2f_or_2_nl = HACC_idx_HACC_idx_acc_conv_2f_and_itm
      | (fsm_output[10]);
  assign nl_z_out_6 = HACC_idx_HACC_idx_acc_conv_2f_mux_1_nl + conv_u2u_1_16(HACC_idx_HACC_idx_acc_conv_2f_or_2_nl);
  assign z_out_6 = nl_z_out_6[15:0];
  assign WRITE_mux1h_4_nl = MUX1HOT_v_18_4_2(({6'b000000 , (WRITE_i_18_0_sva[18:7])}),
      ({(~ (HACC_idx_acc_8_psp[9:0])) , (~ (reg_HACC_idx_acc_3_psp_ftd_2[3:0])) ,
      (~ (HACC_acc_10_psp[3:0]))}), ({{13{ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva[4]}},
      ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_i_4_0_sva}),
      ({10'b0000000001 , (~ data_in_rsci_idat_mxwt)}), {(fsm_output[25]) , (fsm_output[7])
      , (fsm_output[8]) , (fsm_output[3])});
  assign WRITE_WRITE_or_4_nl = (~((fsm_output[8]) | (fsm_output[3]))) | (fsm_output[25]);
  assign WRITE_WRITE_and_2_nl = (~ (fsm_output[3])) & WRITE_nor_1_seb;
  assign WRITE_WRITE_and_3_nl = MUX_v_2_2_2(2'b00, ({{1{WRITE_nor_1_seb}}, WRITE_nor_1_seb}),
      (fsm_output[3]));
  assign WRITE_WRITE_or_5_nl = (fsm_output[25]) | (fsm_output[3]);
  assign nl_z_out_7 = conv_s2u_18_19(WRITE_mux1h_4_nl) + conv_u2u_17_19({WRITE_WRITE_or_4_nl
      , (signext_4_1(fsm_output[25])) , 2'b00 , (signext_7_6({WRITE_WRITE_and_2_nl
      , ({{1{WRITE_nor_1_seb}}, WRITE_nor_1_seb}) , WRITE_WRITE_and_3_nl , WRITE_nor_1_seb}))
      , 1'b0 , WRITE_WRITE_or_5_nl , 1'b1});
  assign z_out_7 = nl_z_out_7[18:0];
  assign HROW_HROW_and_19_nl = MUX_v_3_2_2(3'b000, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_32_30,
      (fsm_output[9]));
  assign HROW_HROW_and_20_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_29
      & (fsm_output[9]);
  assign HROW_HROW_and_21_nl = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_28_27,
      (fsm_output[9]));
  assign HROW_HROW_and_22_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_26
      & (fsm_output[9]);
  assign HROW_HROW_and_23_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_25
      & (fsm_output[9]);
  assign HROW_HROW_and_24_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_24
      & (fsm_output[9]);
  assign HROW_HROW_and_25_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_23
      & (fsm_output[9]);
  assign HROW_HROW_and_26_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_22
      & (fsm_output[9]);
  assign HROW_HROW_and_27_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_21
      & (fsm_output[9]);
  assign HROW_HROW_and_28_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_20
      & (fsm_output[9]);
  assign HROW_HROW_and_29_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_19
      & (fsm_output[9]);
  assign HROW_HROW_and_30_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_18
      & (fsm_output[9]);
  assign HROW_HROW_and_31_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_17
      & (fsm_output[9]);
  assign HROW_HROW_and_32_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_16
      & (fsm_output[9]);
  assign HROW_HROW_and_33_nl = MUX_v_2_2_2(2'b00, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_15_14,
      (fsm_output[9]));
  assign HROW_HROW_and_34_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_13
      & (fsm_output[9]);
  assign HROW_HROW_and_35_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_12
      & (fsm_output[9]);
  assign HROW_HROW_and_36_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_11
      & (fsm_output[9]);
  assign HROW_HROW_and_37_nl = ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_10
      & (fsm_output[9]);
  assign HROW_mux_10_nl = MUX_s_1_2_2((HROW_y_sva[9]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_9,
      fsm_output[9]);
  assign HROW_mux_11_nl = MUX_s_1_2_2((HROW_y_sva[8]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_8,
      fsm_output[9]);
  assign HROW_mux_12_nl = MUX_s_1_2_2((HROW_y_sva[7]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_7,
      fsm_output[9]);
  assign HROW_mux_13_nl = MUX_s_1_2_2((HROW_y_sva[6]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_6,
      fsm_output[9]);
  assign HROW_mux_14_nl = MUX_s_1_2_2((HROW_y_sva[5]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_5,
      fsm_output[9]);
  assign HROW_mux_15_nl = MUX_s_1_2_2((HROW_y_sva[4]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_4,
      fsm_output[9]);
  assign HROW_mux_16_nl = MUX_s_1_2_2((HROW_y_sva[3]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_3,
      fsm_output[9]);
  assign HROW_mux_17_nl = MUX_v_2_2_2((HROW_y_sva[2:1]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_2_1,
      fsm_output[9]);
  assign HROW_mux_18_nl = MUX_s_1_2_2((HROW_y_sva[0]), ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_y_lpi_4_dfm_1_0,
      fsm_output[9]);
  assign HROW_mux_19_nl = MUX_v_33_2_2(33'b000000000000000000000000000000001, ac_math_ac_sincos_cordic_45_10_AC_TRN_AC_WRAP_27_3_AC_TRN_AC_WRAP_for_x_2mi_sva,
      fsm_output[9]);
  assign nl_z_out_8 = ({HROW_HROW_and_19_nl , HROW_HROW_and_20_nl , HROW_HROW_and_21_nl
      , HROW_HROW_and_22_nl , HROW_HROW_and_23_nl , HROW_HROW_and_24_nl , HROW_HROW_and_25_nl
      , HROW_HROW_and_26_nl , HROW_HROW_and_27_nl , HROW_HROW_and_28_nl , HROW_HROW_and_29_nl
      , HROW_HROW_and_30_nl , HROW_HROW_and_31_nl , HROW_HROW_and_32_nl , HROW_HROW_and_33_nl
      , HROW_HROW_and_34_nl , HROW_HROW_and_35_nl , HROW_HROW_and_36_nl , HROW_HROW_and_37_nl
      , HROW_mux_10_nl , HROW_mux_11_nl , HROW_mux_12_nl , HROW_mux_13_nl , HROW_mux_14_nl
      , HROW_mux_15_nl , HROW_mux_16_nl , HROW_mux_17_nl , HROW_mux_18_nl}) + HROW_mux_19_nl;
  assign z_out_8 = nl_z_out_8[32:0];

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


  function automatic [10:0] MUX1HOT_v_11_4_2;
    input [10:0] input_3;
    input [10:0] input_2;
    input [10:0] input_1;
    input [10:0] input_0;
    input [3:0] sel;
    reg [10:0] result;
  begin
    result = input_0 & {11{sel[0]}};
    result = result | ( input_1 & {11{sel[1]}});
    result = result | ( input_2 & {11{sel[2]}});
    result = result | ( input_3 & {11{sel[3]}});
    MUX1HOT_v_11_4_2 = result;
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


  function automatic [33:0] MUX1HOT_v_34_3_2;
    input [33:0] input_2;
    input [33:0] input_1;
    input [33:0] input_0;
    input [2:0] sel;
    reg [33:0] result;
  begin
    result = input_0 & {34{sel[0]}};
    result = result | ( input_1 & {34{sel[1]}});
    result = result | ( input_2 & {34{sel[2]}});
    MUX1HOT_v_34_3_2 = result;
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


  function automatic [4:0] MUX1HOT_v_5_5_2;
    input [4:0] input_4;
    input [4:0] input_3;
    input [4:0] input_2;
    input [4:0] input_1;
    input [4:0] input_0;
    input [4:0] sel;
    reg [4:0] result;
  begin
    result = input_0 & {5{sel[0]}};
    result = result | ( input_1 & {5{sel[1]}});
    result = result | ( input_2 & {5{sel[2]}});
    result = result | ( input_3 & {5{sel[3]}});
    result = result | ( input_4 & {5{sel[4]}});
    MUX1HOT_v_5_5_2 = result;
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


  function automatic [25:0] readslicef_40_26_14;
    input [39:0] vector;
    reg [39:0] tmp;
  begin
    tmp = vector >> 14;
    readslicef_40_26_14 = tmp[25:0];
  end
  endfunction


  function automatic [1:0] signext_2_1;
    input [0:0] vector;
  begin
    signext_2_1= {{1{vector[0]}}, vector};
  end
  endfunction


  function automatic [33:0] signext_34_17;
    input [16:0] vector;
  begin
    signext_34_17= {{17{vector[16]}}, vector};
  end
  endfunction


  function automatic [33:0] signext_34_33;
    input [32:0] vector;
  begin
    signext_34_33= {{1{vector[32]}}, vector};
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


  function automatic [3:0] signext_4_1;
    input [0:0] vector;
  begin
    signext_4_1= {{3{vector[0]}}, vector};
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


  function automatic [39:0] conv_s2u_38_40 ;
    input [37:0]  vector ;
  begin
    conv_s2u_38_40 = {{2{vector[37]}}, vector};
  end
  endfunction


  function automatic [39:0] conv_s2u_39_40 ;
    input [38:0]  vector ;
  begin
    conv_s2u_39_40 = {vector[38], vector};
  end
  endfunction


  function automatic [11:0] conv_u2s_11_12 ;
    input [10:0]  vector ;
  begin
    conv_u2s_11_12 =  {1'b0, vector};
  end
  endfunction


  function automatic [15:0] conv_u2u_1_16 ;
    input [0:0]  vector ;
  begin
    conv_u2u_1_16 = {{15{1'b0}}, vector};
  end
  endfunction


  function automatic [18:0] conv_u2u_17_19 ;
    input [16:0]  vector ;
  begin
    conv_u2u_17_19 = {{2{1'b0}}, vector};
  end
  endfunction


  function automatic [34:0] conv_u2u_32_35 ;
    input [31:0]  vector ;
  begin
    conv_u2u_32_35 = {{3{1'b0}}, vector};
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



