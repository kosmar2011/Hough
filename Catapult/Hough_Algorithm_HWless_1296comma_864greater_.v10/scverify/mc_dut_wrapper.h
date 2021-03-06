// Generated by ZYANG
#ifndef INCLUDED_CCS_DUT_WRAPPER_H
#define INCLUDED_CCS_DUT_WRAPPER_H

#ifndef SC_USE_STD_STRING
#define SC_USE_STD_STRING
#endif

#include <systemc.h>
#include <mc_simulator_extensions.h>

#ifdef CCS_SYSC
namespace HDL {
#endif
#if defined(CCS_DUT_SYSC)
// alias ccs_DUT_wrapper to namespace enclosure of either cycle or RTL SystemC netlist
namespace
    ccs_design {
#if defined(CCS_DUT_CYCLE)
#include "cycle.cxx"
#else
#if defined(CCS_DUT_RTL)
#include "rtl.cxx"
#endif
#endif
}
typedef
    ccs_design::HDL::Hough_Algorithm_HW<1296,864> ccs_DUT_wrapper;

#else

// Create a foreign module wrapper around
    // the HDL
#ifdef VCS_SYSTEMC
// VCS support - ccs_DUT_wrapper is derived from VCS-generated SystemC wrapper around HDL code
class ccs_DUT_wrapper : public TOP_HDL_ENTITY
{
public:
  ccs_DUT_wrapper(const sc_module_name& nm, const char *hdl_name)
  : TOP_HDL_ENTITY(nm)
  {
  // elaborate_foreign_module(hdl_name);
  }

  ~ccs_DUT_wrapper() {}
};

#else
// non VCS simulators - ccs_DUT_wrapper is derived from mc_foreign_module (adding 2nd ctor arg)
class ccs_DUT_wrapper: public mc_foreign_module
{
public:
  // Interface Ports
  sc_in<bool> clk;
  sc_in<sc_logic> rst;
  sc_in<sc_lv<8> > data_in_rsc_dat;
  sc_in<sc_logic> data_in_rsc_vld;
  sc_out<sc_logic> data_in_rsc_rdy;
  sc_in<sc_lv<11> > widthIn_rsc_dat;
  sc_out<sc_logic> widthIn_rsc_triosy_lz;
  sc_in<sc_lv<10> > heightIn_rsc_dat;
  sc_out<sc_logic> heightIn_rsc_triosy_lz;
  sc_out<sc_lv<11> > x1_rsc_dat;
  sc_out<sc_logic> x1_rsc_vld;
  sc_in<sc_logic> x1_rsc_rdy;
  sc_out<sc_lv<10> > y1_rsc_dat;
  sc_out<sc_logic> y1_rsc_vld;
  sc_in<sc_logic> y1_rsc_rdy;
  sc_out<sc_lv<11> > x2_rsc_dat;
  sc_out<sc_logic> x2_rsc_vld;
  sc_in<sc_logic> x2_rsc_rdy;
  sc_out<sc_lv<10> > y2_rsc_dat;
  sc_out<sc_logic> y2_rsc_vld;
  sc_in<sc_logic> y2_rsc_rdy;
public:
  ccs_DUT_wrapper(const sc_module_name& nm, const char *hdl_name)
  :
    mc_foreign_module(nm, hdl_name), 
    clk("clk"), 
    rst("rst"), 
    data_in_rsc_dat("data_in_rsc_dat"), 
    data_in_rsc_vld("data_in_rsc_vld"), 
    data_in_rsc_rdy("data_in_rsc_rdy"), 
    widthIn_rsc_dat("widthIn_rsc_dat"), 
    widthIn_rsc_triosy_lz("widthIn_rsc_triosy_lz"), 
    heightIn_rsc_dat("heightIn_rsc_dat"), 
    heightIn_rsc_triosy_lz("heightIn_rsc_triosy_lz"), 
    x1_rsc_dat("x1_rsc_dat"), 
    x1_rsc_vld("x1_rsc_vld"), 
    x1_rsc_rdy("x1_rsc_rdy"), 
    y1_rsc_dat("y1_rsc_dat"), 
    y1_rsc_vld("y1_rsc_vld"), 
    y1_rsc_rdy("y1_rsc_rdy"), 
    x2_rsc_dat("x2_rsc_dat"), 
    x2_rsc_vld("x2_rsc_vld"), 
    x2_rsc_rdy("x2_rsc_rdy"), 
    y2_rsc_dat("y2_rsc_dat"), 
    y2_rsc_vld("y2_rsc_vld"), 
    y2_rsc_rdy("y2_rsc_rdy")
  {
    elaborate_foreign_module(hdl_name);
  }

  ~ccs_DUT_wrapper() {}
};
#endif

#endif

#ifdef CCS_SYSC
} // end namespace HDL
#endif
#endif


