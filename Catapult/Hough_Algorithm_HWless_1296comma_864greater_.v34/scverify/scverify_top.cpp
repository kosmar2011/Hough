#include <string>
#include <fstream>
#include <iostream>
#include "mc_testbench.h"
#include <mc_reset.h>
#include <mc_transactors.h>
#include <mc_scverify.h>
#include <mc_stall_ctrl.h>
#include "ccs_ioport_trans_rsc_v1.h"
#include <mc_monitor.h>
#include <mc_simulator_extensions.h>
#include "mc_dut_wrapper.h"
#include "ccs_probes.cpp"
#include <mt19937ar.c>
#ifndef TO_QUOTED_STRING
#define TO_QUOTED_STRING(x) TO_QUOTED_STRING1(x)
#define TO_QUOTED_STRING1(x) #x
#endif
#ifndef TOP_HDL_ENTITY
#define TOP_HDL_ENTITY Hough_Algorithm_HW_1296_864
#endif
// Hold time for the SCVerify testbench to account for the gate delay after downstream synthesis in pico second(s)
// Hold time value is obtained from 'top_gate_constraints.cpp', which is generated at the end of RTL synthesis
#ifdef CCS_DUT_GATE
extern double __scv_hold_time;
extern double __scv_hold_time_RSCID_1;
extern double __scv_hold_time_RSCID_2;
extern double __scv_hold_time_RSCID_3;
extern double __scv_hold_time_RSCID_4;
extern double __scv_hold_time_RSCID_5;
extern double __scv_hold_time_RSCID_6;
extern double __scv_hold_time_RSCID_7;
#else
double __scv_hold_time = 0.0; // default for non-gate simulation is zero
double __scv_hold_time_RSCID_1 = 0;
double __scv_hold_time_RSCID_2 = 0;
double __scv_hold_time_RSCID_3 = 0;
double __scv_hold_time_RSCID_4 = 0;
double __scv_hold_time_RSCID_5 = 0;
double __scv_hold_time_RSCID_6 = 0;
double __scv_hold_time_RSCID_7 = 0;
#endif

class scverify_top : public sc_module
{
public:
  sc_signal<sc_logic>                                     rst;
  sc_signal<sc_logic>                                     rst_n;
  sc_signal<sc_logic>                                     SIG_SC_LOGIC_0;
  sc_signal<sc_logic>                                     SIG_SC_LOGIC_1;
  sc_signal<sc_logic>                                     TLS_design_is_idle;
  sc_signal<bool>                                         TLS_design_is_idle_reg;
  sc_clock                                                clk;
  mc_programmable_reset                                   rst_driver;
  sc_signal<sc_logic>                                     TLS_rst;
  sc_signal<sc_lv<8> >                                    TLS_data_in_rsc_dat;
  sc_signal<sc_logic>                                     TLS_data_in_rsc_vld;
  sc_signal<sc_logic>                                     TLS_data_in_rsc_rdy;
  sc_signal<sc_lv<11> >                                   TLS_widthIn_rsc_dat;
  sc_signal<sc_logic>                                     TLS_widthIn_rsc_triosy_lz;
  sc_signal<sc_lv<10> >                                   TLS_heightIn_rsc_dat;
  sc_signal<sc_logic>                                     TLS_heightIn_rsc_triosy_lz;
  sc_signal<sc_lv<11> >                                   TLS_x1_rsc_dat;
  sc_signal<sc_logic>                                     TLS_x1_rsc_vld;
  sc_signal<sc_logic>                                     TLS_x1_rsc_rdy;
  sc_signal<sc_lv<10> >                                   TLS_y1_rsc_dat;
  sc_signal<sc_logic>                                     TLS_y1_rsc_vld;
  sc_signal<sc_logic>                                     TLS_y1_rsc_rdy;
  sc_signal<sc_lv<11> >                                   TLS_x2_rsc_dat;
  sc_signal<sc_logic>                                     TLS_x2_rsc_vld;
  sc_signal<sc_logic>                                     TLS_x2_rsc_rdy;
  sc_signal<sc_lv<10> >                                   TLS_y2_rsc_dat;
  sc_signal<sc_logic>                                     TLS_y2_rsc_vld;
  sc_signal<sc_logic>                                     TLS_y2_rsc_rdy;
  ccs_DUT_wrapper                                         Hough_Algorithm_HW_1296_864_INST;
  ccs_in_wait_trans_rsc_v1<1,8 >                          data_in_rsc_INST;
  ccs_out_wait_trans_rsc_v1<1,11 >                        x1_rsc_INST;
  ccs_out_wait_trans_rsc_v1<1,10 >                        y1_rsc_INST;
  ccs_out_wait_trans_rsc_v1<1,11 >                        x2_rsc_INST;
  ccs_out_wait_trans_rsc_v1<1,10 >                        y2_rsc_INST;
  ccs_in_trans_rsc_v1<1,11 >                              widthIn_rsc_INST;
  ccs_in_trans_rsc_v1<1,10 >                              heightIn_rsc_INST;
  tlm::tlm_fifo<ac_int<8, false > >                       TLS_in_fifo_data_in;
  tlm::tlm_fifo<mc_wait_ctrl>                             TLS_in_wait_ctrl_fifo_data_in;
  tlm::tlm_fifo<int>                                      TLS_in_fifo_data_in_sizecount;
  sc_signal<sc_logic>                                     TLS_data_in_rsc_trdone;
  mc_channel_input_transactor<ac_int<8, false >,8,false>  transactor_data_in;
  tlm::tlm_fifo<ac_int<11, false > >                      TLS_out_fifo_x1;
  tlm::tlm_fifo<mc_wait_ctrl>                             TLS_out_wait_ctrl_fifo_x1;
  sc_signal<sc_logic>                                     TLS_x1_rsc_trdone;
  mc_output_transactor<ac_int<11, false >,11,false>       transactor_x1;
  tlm::tlm_fifo<ac_int<10, false > >                      TLS_out_fifo_y1;
  tlm::tlm_fifo<mc_wait_ctrl>                             TLS_out_wait_ctrl_fifo_y1;
  sc_signal<sc_logic>                                     TLS_y1_rsc_trdone;
  mc_output_transactor<ac_int<10, false >,10,false>       transactor_y1;
  tlm::tlm_fifo<ac_int<11, false > >                      TLS_out_fifo_x2;
  tlm::tlm_fifo<mc_wait_ctrl>                             TLS_out_wait_ctrl_fifo_x2;
  sc_signal<sc_logic>                                     TLS_x2_rsc_trdone;
  mc_output_transactor<ac_int<11, false >,11,false>       transactor_x2;
  tlm::tlm_fifo<ac_int<10, false > >                      TLS_out_fifo_y2;
  tlm::tlm_fifo<mc_wait_ctrl>                             TLS_out_wait_ctrl_fifo_y2;
  sc_signal<sc_logic>                                     TLS_y2_rsc_trdone;
  mc_output_transactor<ac_int<10, false >,10,false>       transactor_y2;
  tlm::tlm_fifo<ac_int<11, false > >                      TLS_in_fifo_widthIn;
  tlm::tlm_fifo<mc_wait_ctrl>                             TLS_in_wait_ctrl_fifo_widthIn;
  mc_trios_input_monitor                                  trios_monitor_widthIn_rsc_triosy_lz_INST;
  mc_input_transactor<ac_int<11, false >,11,false>        transactor_widthIn;
  tlm::tlm_fifo<ac_int<10, false > >                      TLS_in_fifo_heightIn;
  tlm::tlm_fifo<mc_wait_ctrl>                             TLS_in_wait_ctrl_fifo_heightIn;
  mc_trios_input_monitor                                  trios_monitor_heightIn_rsc_triosy_lz_INST;
  mc_input_transactor<ac_int<10, false >,10,false>        transactor_heightIn;
  mc_testbench                                            testbench_INST;
  sc_signal<sc_logic>                                     catapult_start;
  sc_signal<sc_logic>                                     catapult_done;
  sc_signal<sc_logic>                                     catapult_ready;
  sc_signal<sc_logic>                                     in_sync;
  sc_signal<sc_logic>                                     out_sync;
  sc_signal<sc_logic>                                     inout_sync;
  sc_signal<unsigned>                                     wait_for_init;
  sync_generator                                          sync_generator_INST;
  catapult_monitor                                        catapult_monitor_INST;
  ccs_probe_monitor                                      *ccs_probe_monitor_INST;
  sc_event                                                generate_reset_event;
  sc_event                                                deadlock_event;
  sc_signal<sc_logic>                                     deadlocked;
  sc_signal<sc_logic>                                     maxsimtime;
  sc_event                                                max_sim_time_event;
  sc_signal<sc_logic>                                     OFS_houghTransform_inst_houghTransform_core_inst_houghTransform_core_staller_inst_core_wen;
  sc_signal<sc_logic>                                     OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_staller_inst_core_wen;
  sc_signal<sc_logic>                                     OFS_houghTransform_inst_houghTransform_core_inst_houghTransform_core_data_in_rsci_inst_houghTransform_core_data_in_rsci_data_in_wait_ctrl_inst_data_in_rsci_irdy_core_sct;
  sc_signal<sc_logic>                                     OFS_houghTransform_inst_houghTransform_core_inst_houghTransform_core_data_in_rsci_inst_houghTransform_core_data_in_rsci_data_in_wait_ctrl_inst_data_in_rsci_ivld;
  sc_signal<sc_logic>                                     OFS_houghTransform_inst_houghTransform_core_inst_houghTransform_core_acc_rsci_inst_houghTransform_core_acc_rsci_acc_wait_ctrl_inst_acc_rsci_irdy;
  sc_signal<sc_logic>                                     OFS_houghTransform_inst_houghTransform_core_inst_houghTransform_core_acc_rsci_inst_houghTransform_core_acc_rsci_acc_wait_ctrl_inst_acc_rsci_ivld_core_sct;
  sc_signal<sc_logic>                                     OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_x1_rsci_inst_getMaxLine_core_x1_rsci_x1_wait_ctrl_inst_x1_rsci_irdy;
  sc_signal<sc_logic>                                     OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_x1_rsci_inst_getMaxLine_core_x1_rsci_x1_wait_ctrl_inst_x1_rsci_ivld_core_sct;
  sc_signal<sc_logic>                                     OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_y1_rsci_inst_getMaxLine_core_y1_rsci_y1_wait_ctrl_inst_y1_rsci_irdy;
  sc_signal<sc_logic>                                     OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_y1_rsci_inst_getMaxLine_core_y1_rsci_y1_wait_ctrl_inst_y1_rsci_ivld_core_sct;
  sc_signal<sc_logic>                                     OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_x2_rsci_inst_getMaxLine_core_x2_rsci_x2_wait_ctrl_inst_x2_rsci_irdy;
  sc_signal<sc_logic>                                     OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_x2_rsci_inst_getMaxLine_core_x2_rsci_x2_wait_ctrl_inst_x2_rsci_ivld_core_sct;
  sc_signal<sc_logic>                                     OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_y2_rsci_inst_getMaxLine_core_y2_rsci_y2_wait_ctrl_inst_y2_rsci_irdy;
  sc_signal<sc_logic>                                     OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_y2_rsci_inst_getMaxLine_core_y2_rsci_y2_wait_ctrl_inst_y2_rsci_ivld_core_sct;
  sc_signal<sc_logic>                                     OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_acc_rsci_inst_getMaxLine_core_acc_rsci_acc_wait_ctrl_inst_acc_rsci_irdy_core_sct;
  sc_signal<sc_logic>                                     OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_acc_rsci_inst_getMaxLine_core_acc_rsci_acc_wait_ctrl_inst_acc_rsci_ivld;
  sc_signal<sc_logic>                                     OFS_data_in_rsc_vld;
  sc_signal<sc_logic>                                     OFS_x1_rsc_rdy;
  sc_signal<sc_logic>                                     OFS_y1_rsc_rdy;
  sc_signal<sc_logic>                                     OFS_x2_rsc_rdy;
  sc_signal<sc_logic>                                     OFS_y2_rsc_rdy;
  sc_signal<sc_logic>                                     TLS_enable_stalls;
  sc_signal<unsigned short>                               TLS_stall_coverage;

  void TLS_rst_method();
  void drive_TLS_data_in_rsc_trdone();
  void drive_TLS_x1_rsc_trdone();
  void drive_TLS_y1_rsc_trdone();
  void drive_TLS_x2_rsc_trdone();
  void drive_TLS_y2_rsc_trdone();
  void max_sim_time_notify();
  void start_of_simulation();
  void setup_debug();
  void debug(const char* varname, int flags, int count);
  void generate_reset();
  void install_observe_foreign_signals();
  void deadlock_watch();
  void deadlock_notify();

  // Constructor
  SC_HAS_PROCESS(scverify_top);
  scverify_top(const sc_module_name& name)
    : rst("rst")
    , rst_n("rst_n")
    , SIG_SC_LOGIC_0("SIG_SC_LOGIC_0")
    , SIG_SC_LOGIC_1("SIG_SC_LOGIC_1")
    , TLS_design_is_idle("TLS_design_is_idle")
    , TLS_design_is_idle_reg("TLS_design_is_idle_reg")
    , CCS_CLK_CTOR(clk, "clk", 1, SC_NS, 0.5, 0, SC_NS, false)
    , rst_driver("rst_driver", 2.000000, false)
    , TLS_rst("TLS_rst")
    , TLS_data_in_rsc_dat("TLS_data_in_rsc_dat")
    , TLS_data_in_rsc_vld("TLS_data_in_rsc_vld")
    , TLS_data_in_rsc_rdy("TLS_data_in_rsc_rdy")
    , TLS_widthIn_rsc_dat("TLS_widthIn_rsc_dat")
    , TLS_widthIn_rsc_triosy_lz("TLS_widthIn_rsc_triosy_lz")
    , TLS_heightIn_rsc_dat("TLS_heightIn_rsc_dat")
    , TLS_heightIn_rsc_triosy_lz("TLS_heightIn_rsc_triosy_lz")
    , TLS_x1_rsc_dat("TLS_x1_rsc_dat")
    , TLS_x1_rsc_vld("TLS_x1_rsc_vld")
    , TLS_x1_rsc_rdy("TLS_x1_rsc_rdy")
    , TLS_y1_rsc_dat("TLS_y1_rsc_dat")
    , TLS_y1_rsc_vld("TLS_y1_rsc_vld")
    , TLS_y1_rsc_rdy("TLS_y1_rsc_rdy")
    , TLS_x2_rsc_dat("TLS_x2_rsc_dat")
    , TLS_x2_rsc_vld("TLS_x2_rsc_vld")
    , TLS_x2_rsc_rdy("TLS_x2_rsc_rdy")
    , TLS_y2_rsc_dat("TLS_y2_rsc_dat")
    , TLS_y2_rsc_vld("TLS_y2_rsc_vld")
    , TLS_y2_rsc_rdy("TLS_y2_rsc_rdy")
    , Hough_Algorithm_HW_1296_864_INST("rtl", TO_QUOTED_STRING(TOP_HDL_ENTITY))
    , data_in_rsc_INST("data_in_rsc", true)
    , x1_rsc_INST("x1_rsc", true)
    , y1_rsc_INST("y1_rsc", true)
    , x2_rsc_INST("x2_rsc", true)
    , y2_rsc_INST("y2_rsc", true)
    , widthIn_rsc_INST("widthIn_rsc", true)
    , heightIn_rsc_INST("heightIn_rsc", true)
    , TLS_in_fifo_data_in("TLS_in_fifo_data_in", -1)
    , TLS_in_wait_ctrl_fifo_data_in("TLS_in_wait_ctrl_fifo_data_in", -1)
    , TLS_in_fifo_data_in_sizecount("TLS_in_fifo_data_in_sizecount", 1)
    , TLS_data_in_rsc_trdone("TLS_data_in_rsc_trdone")
    , transactor_data_in("transactor_data_in", 0, 8, 0)
    , TLS_out_fifo_x1("TLS_out_fifo_x1", -1)
    , TLS_out_wait_ctrl_fifo_x1("TLS_out_wait_ctrl_fifo_x1", -1)
    , TLS_x1_rsc_trdone("TLS_x1_rsc_trdone")
    , transactor_x1("transactor_x1", 0, 11, 0)
    , TLS_out_fifo_y1("TLS_out_fifo_y1", -1)
    , TLS_out_wait_ctrl_fifo_y1("TLS_out_wait_ctrl_fifo_y1", -1)
    , TLS_y1_rsc_trdone("TLS_y1_rsc_trdone")
    , transactor_y1("transactor_y1", 0, 10, 0)
    , TLS_out_fifo_x2("TLS_out_fifo_x2", -1)
    , TLS_out_wait_ctrl_fifo_x2("TLS_out_wait_ctrl_fifo_x2", -1)
    , TLS_x2_rsc_trdone("TLS_x2_rsc_trdone")
    , transactor_x2("transactor_x2", 0, 11, 0)
    , TLS_out_fifo_y2("TLS_out_fifo_y2", -1)
    , TLS_out_wait_ctrl_fifo_y2("TLS_out_wait_ctrl_fifo_y2", -1)
    , TLS_y2_rsc_trdone("TLS_y2_rsc_trdone")
    , transactor_y2("transactor_y2", 0, 10, 0)
    , TLS_in_fifo_widthIn("TLS_in_fifo_widthIn", -1)
    , TLS_in_wait_ctrl_fifo_widthIn("TLS_in_wait_ctrl_fifo_widthIn", -1)
    , trios_monitor_widthIn_rsc_triosy_lz_INST("trios_monitor_widthIn_rsc_triosy_lz_INST")
    , transactor_widthIn("transactor_widthIn", 0, 11, 0)
    , TLS_in_fifo_heightIn("TLS_in_fifo_heightIn", -1)
    , TLS_in_wait_ctrl_fifo_heightIn("TLS_in_wait_ctrl_fifo_heightIn", -1)
    , trios_monitor_heightIn_rsc_triosy_lz_INST("trios_monitor_heightIn_rsc_triosy_lz_INST")
    , transactor_heightIn("transactor_heightIn", 0, 10, 0)
    , testbench_INST("user_tb")
    , catapult_start("catapult_start")
    , catapult_done("catapult_done")
    , catapult_ready("catapult_ready")
    , in_sync("in_sync")
    , out_sync("out_sync")
    , inout_sync("inout_sync")
    , wait_for_init("wait_for_init")
    , sync_generator_INST("sync_generator", true, false, false, false, 274116359, 274116359, 0)
    , catapult_monitor_INST("Monitor", clk, true, 274116359LL, 212443464LL)
    , ccs_probe_monitor_INST(NULL)
    , deadlocked("deadlocked")
    , maxsimtime("maxsimtime")
  {
    rst_driver.reset_out(TLS_rst);

    Hough_Algorithm_HW_1296_864_INST.clk(clk);
    Hough_Algorithm_HW_1296_864_INST.rst(TLS_rst);
    Hough_Algorithm_HW_1296_864_INST.data_in_rsc_dat(TLS_data_in_rsc_dat);
    Hough_Algorithm_HW_1296_864_INST.data_in_rsc_vld(TLS_data_in_rsc_vld);
    Hough_Algorithm_HW_1296_864_INST.data_in_rsc_rdy(TLS_data_in_rsc_rdy);
    Hough_Algorithm_HW_1296_864_INST.widthIn_rsc_dat(TLS_widthIn_rsc_dat);
    Hough_Algorithm_HW_1296_864_INST.widthIn_rsc_triosy_lz(TLS_widthIn_rsc_triosy_lz);
    Hough_Algorithm_HW_1296_864_INST.heightIn_rsc_dat(TLS_heightIn_rsc_dat);
    Hough_Algorithm_HW_1296_864_INST.heightIn_rsc_triosy_lz(TLS_heightIn_rsc_triosy_lz);
    Hough_Algorithm_HW_1296_864_INST.x1_rsc_dat(TLS_x1_rsc_dat);
    Hough_Algorithm_HW_1296_864_INST.x1_rsc_vld(TLS_x1_rsc_vld);
    Hough_Algorithm_HW_1296_864_INST.x1_rsc_rdy(TLS_x1_rsc_rdy);
    Hough_Algorithm_HW_1296_864_INST.y1_rsc_dat(TLS_y1_rsc_dat);
    Hough_Algorithm_HW_1296_864_INST.y1_rsc_vld(TLS_y1_rsc_vld);
    Hough_Algorithm_HW_1296_864_INST.y1_rsc_rdy(TLS_y1_rsc_rdy);
    Hough_Algorithm_HW_1296_864_INST.x2_rsc_dat(TLS_x2_rsc_dat);
    Hough_Algorithm_HW_1296_864_INST.x2_rsc_vld(TLS_x2_rsc_vld);
    Hough_Algorithm_HW_1296_864_INST.x2_rsc_rdy(TLS_x2_rsc_rdy);
    Hough_Algorithm_HW_1296_864_INST.y2_rsc_dat(TLS_y2_rsc_dat);
    Hough_Algorithm_HW_1296_864_INST.y2_rsc_vld(TLS_y2_rsc_vld);
    Hough_Algorithm_HW_1296_864_INST.y2_rsc_rdy(TLS_y2_rsc_rdy);

    data_in_rsc_INST.rdy(TLS_data_in_rsc_rdy);
    data_in_rsc_INST.vld(TLS_data_in_rsc_vld);
    data_in_rsc_INST.dat(TLS_data_in_rsc_dat);
    data_in_rsc_INST.clk(clk);
    data_in_rsc_INST.add_attribute(*(new sc_attribute<double>("CLK_SKEW_DELAY", __scv_hold_time_RSCID_1)));

    x1_rsc_INST.rdy(TLS_x1_rsc_rdy);
    x1_rsc_INST.vld(TLS_x1_rsc_vld);
    x1_rsc_INST.dat(TLS_x1_rsc_dat);
    x1_rsc_INST.clk(clk);
    x1_rsc_INST.add_attribute(*(new sc_attribute<double>("CLK_SKEW_DELAY", __scv_hold_time_RSCID_4)));

    y1_rsc_INST.rdy(TLS_y1_rsc_rdy);
    y1_rsc_INST.vld(TLS_y1_rsc_vld);
    y1_rsc_INST.dat(TLS_y1_rsc_dat);
    y1_rsc_INST.clk(clk);
    y1_rsc_INST.add_attribute(*(new sc_attribute<double>("CLK_SKEW_DELAY", __scv_hold_time_RSCID_5)));

    x2_rsc_INST.rdy(TLS_x2_rsc_rdy);
    x2_rsc_INST.vld(TLS_x2_rsc_vld);
    x2_rsc_INST.dat(TLS_x2_rsc_dat);
    x2_rsc_INST.clk(clk);
    x2_rsc_INST.add_attribute(*(new sc_attribute<double>("CLK_SKEW_DELAY", __scv_hold_time_RSCID_6)));

    y2_rsc_INST.rdy(TLS_y2_rsc_rdy);
    y2_rsc_INST.vld(TLS_y2_rsc_vld);
    y2_rsc_INST.dat(TLS_y2_rsc_dat);
    y2_rsc_INST.clk(clk);
    y2_rsc_INST.add_attribute(*(new sc_attribute<double>("CLK_SKEW_DELAY", __scv_hold_time_RSCID_7)));

    widthIn_rsc_INST.dat(TLS_widthIn_rsc_dat);
    widthIn_rsc_INST.clk(clk);
    widthIn_rsc_INST.add_attribute(*(new sc_attribute<double>("CLK_SKEW_DELAY", __scv_hold_time_RSCID_2)));

    heightIn_rsc_INST.dat(TLS_heightIn_rsc_dat);
    heightIn_rsc_INST.clk(clk);
    heightIn_rsc_INST.add_attribute(*(new sc_attribute<double>("CLK_SKEW_DELAY", __scv_hold_time_RSCID_3)));

    transactor_data_in.in_fifo(TLS_in_fifo_data_in);
    transactor_data_in.in_wait_ctrl_fifo(TLS_in_wait_ctrl_fifo_data_in);
    transactor_data_in.sizecount_fifo(TLS_in_fifo_data_in_sizecount);
    transactor_data_in.bind_clk(clk, true, rst);
    transactor_data_in.add_attribute(*(new sc_attribute<int>("MC_TRANSACTOR_EVENT", 0 )));
    transactor_data_in.register_block(&data_in_rsc_INST, data_in_rsc_INST.basename(), TLS_data_in_rsc_trdone, 0,
        0, 1);

    transactor_x1.out_fifo(TLS_out_fifo_x1);
    transactor_x1.out_wait_ctrl_fifo(TLS_out_wait_ctrl_fifo_x1);
    transactor_x1.bind_clk(clk, true, rst);
    transactor_x1.add_attribute(*(new sc_attribute<int>("MC_TRANSACTOR_EVENT", 0 )));
    transactor_x1.register_block(&x1_rsc_INST, x1_rsc_INST.basename(), TLS_x1_rsc_trdone, 0, 0, 1);

    transactor_y1.out_fifo(TLS_out_fifo_y1);
    transactor_y1.out_wait_ctrl_fifo(TLS_out_wait_ctrl_fifo_y1);
    transactor_y1.bind_clk(clk, true, rst);
    transactor_y1.add_attribute(*(new sc_attribute<int>("MC_TRANSACTOR_EVENT", 0 )));
    transactor_y1.register_block(&y1_rsc_INST, y1_rsc_INST.basename(), TLS_y1_rsc_trdone, 0, 0, 1);

    transactor_x2.out_fifo(TLS_out_fifo_x2);
    transactor_x2.out_wait_ctrl_fifo(TLS_out_wait_ctrl_fifo_x2);
    transactor_x2.bind_clk(clk, true, rst);
    transactor_x2.add_attribute(*(new sc_attribute<int>("MC_TRANSACTOR_EVENT", 0 )));
    transactor_x2.register_block(&x2_rsc_INST, x2_rsc_INST.basename(), TLS_x2_rsc_trdone, 0, 0, 1);

    transactor_y2.out_fifo(TLS_out_fifo_y2);
    transactor_y2.out_wait_ctrl_fifo(TLS_out_wait_ctrl_fifo_y2);
    transactor_y2.bind_clk(clk, true, rst);
    transactor_y2.add_attribute(*(new sc_attribute<int>("MC_TRANSACTOR_EVENT", 0 )));
    transactor_y2.register_block(&y2_rsc_INST, y2_rsc_INST.basename(), TLS_y2_rsc_trdone, 0, 0, 1);

    trios_monitor_widthIn_rsc_triosy_lz_INST.trios(TLS_widthIn_rsc_triosy_lz);
    trios_monitor_widthIn_rsc_triosy_lz_INST.register_mon(&catapult_monitor_INST);

    transactor_widthIn.in_fifo(TLS_in_fifo_widthIn);
    transactor_widthIn.in_wait_ctrl_fifo(TLS_in_wait_ctrl_fifo_widthIn);
    transactor_widthIn.bind_clk(clk, true, rst);
    transactor_widthIn.add_attribute(*(new sc_attribute<int>("MC_TRANSACTOR_EVENT", 0 )));
    transactor_widthIn.register_block(&widthIn_rsc_INST, widthIn_rsc_INST.basename(), TLS_widthIn_rsc_triosy_lz,
        0, 0, 1);

    trios_monitor_heightIn_rsc_triosy_lz_INST.trios(TLS_heightIn_rsc_triosy_lz);
    trios_monitor_heightIn_rsc_triosy_lz_INST.register_mon(&catapult_monitor_INST);

    transactor_heightIn.in_fifo(TLS_in_fifo_heightIn);
    transactor_heightIn.in_wait_ctrl_fifo(TLS_in_wait_ctrl_fifo_heightIn);
    transactor_heightIn.bind_clk(clk, true, rst);
    transactor_heightIn.add_attribute(*(new sc_attribute<int>("MC_TRANSACTOR_EVENT", 0 )));
    transactor_heightIn.register_block(&heightIn_rsc_INST, heightIn_rsc_INST.basename(), TLS_heightIn_rsc_triosy_lz,
        0, 0, 1);

    testbench_INST.clk(clk);
    testbench_INST.ccs_data_in(TLS_in_fifo_data_in);
    testbench_INST.ccs_wait_ctrl_data_in(TLS_in_wait_ctrl_fifo_data_in);
    testbench_INST.ccs_sizecount_data_in(TLS_in_fifo_data_in_sizecount);
    testbench_INST.ccs_x1(TLS_out_fifo_x1);
    testbench_INST.ccs_wait_ctrl_x1(TLS_out_wait_ctrl_fifo_x1);
    testbench_INST.ccs_y1(TLS_out_fifo_y1);
    testbench_INST.ccs_wait_ctrl_y1(TLS_out_wait_ctrl_fifo_y1);
    testbench_INST.ccs_x2(TLS_out_fifo_x2);
    testbench_INST.ccs_wait_ctrl_x2(TLS_out_wait_ctrl_fifo_x2);
    testbench_INST.ccs_y2(TLS_out_fifo_y2);
    testbench_INST.ccs_wait_ctrl_y2(TLS_out_wait_ctrl_fifo_y2);
    testbench_INST.ccs_widthIn(TLS_in_fifo_widthIn);
    testbench_INST.ccs_wait_ctrl_widthIn(TLS_in_wait_ctrl_fifo_widthIn);
    testbench_INST.ccs_heightIn(TLS_in_fifo_heightIn);
    testbench_INST.ccs_wait_ctrl_heightIn(TLS_in_wait_ctrl_fifo_heightIn);
    testbench_INST.design_is_idle(TLS_design_is_idle_reg);
    testbench_INST.enable_stalls(TLS_enable_stalls);
    testbench_INST.stall_coverage(TLS_stall_coverage);

    sync_generator_INST.clk(clk);
    sync_generator_INST.rst(rst);
    sync_generator_INST.in_sync(in_sync);
    sync_generator_INST.out_sync(out_sync);
    sync_generator_INST.inout_sync(inout_sync);
    sync_generator_INST.wait_for_init(wait_for_init);
    sync_generator_INST.catapult_start(catapult_start);
    sync_generator_INST.catapult_ready(catapult_ready);
    sync_generator_INST.catapult_done(catapult_done);

    catapult_monitor_INST.rst(rst);


    SC_METHOD(TLS_rst_method);
      sensitive_pos << TLS_rst;
      dont_initialize();

    SC_METHOD(drive_TLS_data_in_rsc_trdone);
      sensitive << TLS_data_in_rsc_rdy;
      sensitive << TLS_data_in_rsc_vld;
      sensitive << rst;

    SC_METHOD(drive_TLS_x1_rsc_trdone);
      sensitive << TLS_x1_rsc_vld;
      sensitive << TLS_x1_rsc_rdy;

    SC_METHOD(drive_TLS_y1_rsc_trdone);
      sensitive << TLS_y1_rsc_vld;
      sensitive << TLS_y1_rsc_rdy;

    SC_METHOD(drive_TLS_x2_rsc_trdone);
      sensitive << TLS_x2_rsc_vld;
      sensitive << TLS_x2_rsc_rdy;

    SC_METHOD(drive_TLS_y2_rsc_trdone);
      sensitive << TLS_y2_rsc_vld;
      sensitive << TLS_y2_rsc_rdy;

    SC_METHOD(max_sim_time_notify);
      sensitive << max_sim_time_event;
      dont_initialize();

    SC_METHOD(generate_reset);
      sensitive << generate_reset_event;
      sensitive << testbench_INST.reset_request_event;

    SC_METHOD(deadlock_watch);
      sensitive << clk;
      sensitive << OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_staller_inst_core_wen;
      sensitive << OFS_houghTransform_inst_houghTransform_core_inst_houghTransform_core_staller_inst_core_wen;
      dont_initialize();

    SC_METHOD(deadlock_notify);
      sensitive << deadlock_event;
      dont_initialize();


    #if defined(CCS_SCVERIFY) && defined(CCS_DUT_RTL) && !defined(CCS_DUT_SYSC) && !defined(CCS_SYSC) && !defined(CCS_DUT_POWER)
        ccs_probe_monitor_INST = new ccs_probe_monitor("ccs_probe_monitor");
    ccs_probe_monitor_INST->clk(clk);
    ccs_probe_monitor_INST->rst(rst);
    #endif
    SIG_SC_LOGIC_0.write(SC_LOGIC_0);
    SIG_SC_LOGIC_1.write(SC_LOGIC_1);
    mt19937_init_genrand(19650218UL);
    install_observe_foreign_signals();
  }
};
void scverify_top::TLS_rst_method() {
  std::ostringstream msg;
  msg << "TLS_rst active @ " << sc_time_stamp();
  SC_REPORT_INFO("HW reset", msg.str().c_str());
  data_in_rsc_INST.clear();
  x1_rsc_INST.clear();
  y1_rsc_INST.clear();
  x2_rsc_INST.clear();
  y2_rsc_INST.clear();
  widthIn_rsc_INST.clear();
  heightIn_rsc_INST.clear();
}

void scverify_top::drive_TLS_data_in_rsc_trdone() {
  if (rst.read()==sc_dt::Log_1) { assert(TLS_data_in_rsc_rdy.read()!= SC_LOGIC_1); }
  TLS_data_in_rsc_trdone.write(TLS_data_in_rsc_rdy.read() & TLS_data_in_rsc_vld.read() & ~rst.read());
}

void scverify_top::drive_TLS_x1_rsc_trdone() {
  if (rst.read()==sc_dt::Log_1) { assert(TLS_x1_rsc_vld.read()!= SC_LOGIC_1); }
  TLS_x1_rsc_trdone.write(TLS_x1_rsc_vld.read() & TLS_x1_rsc_rdy.read());
}

void scverify_top::drive_TLS_y1_rsc_trdone() {
  if (rst.read()==sc_dt::Log_1) { assert(TLS_y1_rsc_vld.read()!= SC_LOGIC_1); }
  TLS_y1_rsc_trdone.write(TLS_y1_rsc_vld.read() & TLS_y1_rsc_rdy.read());
}

void scverify_top::drive_TLS_x2_rsc_trdone() {
  if (rst.read()==sc_dt::Log_1) { assert(TLS_x2_rsc_vld.read()!= SC_LOGIC_1); }
  TLS_x2_rsc_trdone.write(TLS_x2_rsc_vld.read() & TLS_x2_rsc_rdy.read());
}

void scverify_top::drive_TLS_y2_rsc_trdone() {
  if (rst.read()==sc_dt::Log_1) { assert(TLS_y2_rsc_vld.read()!= SC_LOGIC_1); }
  TLS_y2_rsc_trdone.write(TLS_y2_rsc_vld.read() & TLS_y2_rsc_rdy.read());
}

void scverify_top::max_sim_time_notify() {
  testbench_INST.set_failed(true);
  testbench_INST.check_results();
  SC_REPORT_ERROR("System", "Specified maximum simulation time reached");
  sc_stop();
}

void scverify_top::start_of_simulation() {
  char *SCVerify_AUTOWAIT = getenv("SCVerify_AUTOWAIT");
  if (SCVerify_AUTOWAIT) {
    int l = atoi(SCVerify_AUTOWAIT);
    transactor_data_in.set_auto_wait_limit(l);
    transactor_x1.set_auto_wait_limit(l);
    transactor_y1.set_auto_wait_limit(l);
    transactor_x2.set_auto_wait_limit(l);
    transactor_y2.set_auto_wait_limit(l);
    transactor_widthIn.set_auto_wait_limit(l);
    transactor_heightIn.set_auto_wait_limit(l);
  }
}

void scverify_top::setup_debug() {
#ifdef MC_DEFAULT_TRANSACTOR_LOG
  static int transactor_data_in_flags = MC_DEFAULT_TRANSACTOR_LOG;
  static int transactor_x1_flags = MC_DEFAULT_TRANSACTOR_LOG;
  static int transactor_y1_flags = MC_DEFAULT_TRANSACTOR_LOG;
  static int transactor_x2_flags = MC_DEFAULT_TRANSACTOR_LOG;
  static int transactor_y2_flags = MC_DEFAULT_TRANSACTOR_LOG;
  static int transactor_widthIn_flags = MC_DEFAULT_TRANSACTOR_LOG;
  static int transactor_heightIn_flags = MC_DEFAULT_TRANSACTOR_LOG;
#else
  static int transactor_data_in_flags = MC_TRANSACTOR_UNDERFLOW | MC_TRANSACTOR_WAIT;
  static int transactor_x1_flags = MC_TRANSACTOR_UNDERFLOW | MC_TRANSACTOR_WAIT;
  static int transactor_y1_flags = MC_TRANSACTOR_UNDERFLOW | MC_TRANSACTOR_WAIT;
  static int transactor_x2_flags = MC_TRANSACTOR_UNDERFLOW | MC_TRANSACTOR_WAIT;
  static int transactor_y2_flags = MC_TRANSACTOR_UNDERFLOW | MC_TRANSACTOR_WAIT;
  static int transactor_widthIn_flags = MC_TRANSACTOR_UNDERFLOW | MC_TRANSACTOR_WAIT;
  static int transactor_heightIn_flags = MC_TRANSACTOR_UNDERFLOW | MC_TRANSACTOR_WAIT;
#endif
  static int transactor_data_in_count = -1;
  static int transactor_x1_count = -1;
  static int transactor_y1_count = -1;
  static int transactor_x2_count = -1;
  static int transactor_y2_count = -1;
  static int transactor_widthIn_count = -1;
  static int transactor_heightIn_count = -1;

  // At the breakpoint, modify the local variables
  // above to turn on/off different levels of transaction
  // logging for each variable. Available flags are:
  //   MC_TRANSACTOR_EMPTY       - log empty FIFOs (on by default)
  //   MC_TRANSACTOR_UNDERFLOW   - log FIFOs that run empty and then are loaded again (off)
  //   MC_TRANSACTOR_READ        - log all read events
  //   MC_TRANSACTOR_WRITE       - log all write events
  //   MC_TRANSACTOR_LOAD        - log all FIFO load events
  //   MC_TRANSACTOR_DUMP        - log all FIFO dump events
  //   MC_TRANSACTOR_STREAMCNT   - log all streamed port index counter events
  //   MC_TRANSACTOR_WAIT        - log user specified handshake waits
  //   MC_TRANSACTOR_SIZE        - log input FIFO size updates

  std::ifstream debug_cmds;
  debug_cmds.open("scverify.cmd",std::fstream::in);
  if (debug_cmds.is_open()) {
    std::cout << "Reading SCVerify debug commands from file 'scverify.cmd'" << std::endl;
    std::string line;
    while (getline(debug_cmds,line)) {
      std::size_t pos1 = line.find(" ");
      if (pos1 == std::string::npos) continue;
      std::size_t pos2 = line.find(" ", pos1+1);
      std::string varname = line.substr(0,pos1);
      std::string flags = line.substr(pos1+1,pos2-pos1-1);
      std::string count = line.substr(pos2+1);
      debug(varname.c_str(),std::atoi(flags.c_str()),std::atoi(count.c_str()));
    }
    debug_cmds.close();
  } else {
    debug("transactor_data_in",transactor_data_in_flags,transactor_data_in_count);
    debug("transactor_x1",transactor_x1_flags,transactor_x1_count);
    debug("transactor_y1",transactor_y1_flags,transactor_y1_count);
    debug("transactor_x2",transactor_x2_flags,transactor_x2_count);
    debug("transactor_y2",transactor_y2_flags,transactor_y2_count);
    debug("transactor_widthIn",transactor_widthIn_flags,transactor_widthIn_count);
    debug("transactor_heightIn",transactor_heightIn_flags,transactor_heightIn_count);
  }
}

void scverify_top::debug(const char* varname, int flags, int count) {
  sc_module *xlator_p = 0;
  sc_attr_base *debug_attr_p = 0;
  if (strcmp(varname,"transactor_data_in") == 0)
    xlator_p = &transactor_data_in;
  if (strcmp(varname,"transactor_x1") == 0)
    xlator_p = &transactor_x1;
  if (strcmp(varname,"transactor_y1") == 0)
    xlator_p = &transactor_y1;
  if (strcmp(varname,"transactor_x2") == 0)
    xlator_p = &transactor_x2;
  if (strcmp(varname,"transactor_y2") == 0)
    xlator_p = &transactor_y2;
  if (strcmp(varname,"transactor_widthIn") == 0)
    xlator_p = &transactor_widthIn;
  if (strcmp(varname,"transactor_heightIn") == 0)
    xlator_p = &transactor_heightIn;
  if (xlator_p) {
    debug_attr_p = xlator_p->get_attribute("MC_TRANSACTOR_EVENT");
    if (!debug_attr_p) {
      debug_attr_p = new sc_attribute<int>("MC_TRANSACTOR_EVENT",flags);
      xlator_p->add_attribute(*debug_attr_p);
    }
    ((sc_attribute<int>*)debug_attr_p)->value = flags;
  }

  if (count>=0) {
    debug_attr_p = xlator_p->get_attribute("MC_TRANSACTOR_COUNT");
    if (!debug_attr_p) {
      debug_attr_p = new sc_attribute<int>("MC_TRANSACTOR_COUNT",count);
      xlator_p->add_attribute(*debug_attr_p);
    }
    ((sc_attribute<int>*)debug_attr_p)->value = count;
  }
}

// Process: SC_METHOD generate_reset
void scverify_top::generate_reset() {
  static bool activate_reset = true;
  static bool toggle_hw_reset = false;
  if (activate_reset || sc_time_stamp() == SC_ZERO_TIME) {
    setup_debug();
    activate_reset = false;
    rst.write(SC_LOGIC_1);
    rst_driver.reset_driver();
    generate_reset_event.notify(2.000000 , SC_NS);
  } else {
    if (toggle_hw_reset) {
      toggle_hw_reset = false;
      generate_reset_event.notify(2.000000 , SC_NS);
    } else {
      transactor_data_in.reset_streams();
      transactor_x1.reset_streams();
      transactor_y1.reset_streams();
      transactor_x2.reset_streams();
      transactor_y2.reset_streams();
      transactor_widthIn.reset_streams();
      transactor_heightIn.reset_streams();
      rst.write(SC_LOGIC_0);
    }
    activate_reset = true;
  }
}

void scverify_top::install_observe_foreign_signals() {
#if !defined(CCS_DUT_SYSC) && defined(DEADLOCK_DETECTION)
#if defined(CCS_DUT_CYCLE) || defined(CCS_DUT_RTL)
  OBSERVE_FOREIGN_SIGNAL(OFS_houghTransform_inst_houghTransform_core_inst_houghTransform_core_staller_inst_core_wen,
      /scverify_top/rtl/houghTransform_inst/houghTransform_core_inst/houghTransform_core_staller_inst/core_wen);
  OBSERVE_FOREIGN_SIGNAL(OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_staller_inst_core_wen, /scverify_top/rtl/getMaxLine_inst/getMaxLine_core_inst/getMaxLine_core_staller_inst/core_wen);
  OBSERVE_FOREIGN_SIGNAL(OFS_houghTransform_inst_houghTransform_core_inst_houghTransform_core_data_in_rsci_inst_houghTransform_core_data_in_rsci_data_in_wait_ctrl_inst_data_in_rsci_irdy_core_sct,
      /scverify_top/rtl/houghTransform_inst/houghTransform_core_inst/houghTransform_core_data_in_rsci_inst/houghTransform_core_data_in_rsci_data_in_wait_ctrl_inst/data_in_rsci_irdy_core_sct);
  OBSERVE_FOREIGN_SIGNAL(OFS_houghTransform_inst_houghTransform_core_inst_houghTransform_core_data_in_rsci_inst_houghTransform_core_data_in_rsci_data_in_wait_ctrl_inst_data_in_rsci_ivld,
      /scverify_top/rtl/houghTransform_inst/houghTransform_core_inst/houghTransform_core_data_in_rsci_inst/houghTransform_core_data_in_rsci_data_in_wait_ctrl_inst/data_in_rsci_ivld);
  OBSERVE_FOREIGN_SIGNAL(OFS_houghTransform_inst_houghTransform_core_inst_houghTransform_core_acc_rsci_inst_houghTransform_core_acc_rsci_acc_wait_ctrl_inst_acc_rsci_irdy,
      /scverify_top/rtl/houghTransform_inst/houghTransform_core_inst/houghTransform_core_acc_rsci_inst/houghTransform_core_acc_rsci_acc_wait_ctrl_inst/acc_rsci_irdy);
  OBSERVE_FOREIGN_SIGNAL(OFS_houghTransform_inst_houghTransform_core_inst_houghTransform_core_acc_rsci_inst_houghTransform_core_acc_rsci_acc_wait_ctrl_inst_acc_rsci_ivld_core_sct,
      /scverify_top/rtl/houghTransform_inst/houghTransform_core_inst/houghTransform_core_acc_rsci_inst/houghTransform_core_acc_rsci_acc_wait_ctrl_inst/acc_rsci_ivld_core_sct);
  OBSERVE_FOREIGN_SIGNAL(OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_x1_rsci_inst_getMaxLine_core_x1_rsci_x1_wait_ctrl_inst_x1_rsci_irdy,
      /scverify_top/rtl/getMaxLine_inst/getMaxLine_core_inst/getMaxLine_core_x1_rsci_inst/getMaxLine_core_x1_rsci_x1_wait_ctrl_inst/x1_rsci_irdy);
  OBSERVE_FOREIGN_SIGNAL(OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_x1_rsci_inst_getMaxLine_core_x1_rsci_x1_wait_ctrl_inst_x1_rsci_ivld_core_sct,
      /scverify_top/rtl/getMaxLine_inst/getMaxLine_core_inst/getMaxLine_core_x1_rsci_inst/getMaxLine_core_x1_rsci_x1_wait_ctrl_inst/x1_rsci_ivld_core_sct);
  OBSERVE_FOREIGN_SIGNAL(OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_y1_rsci_inst_getMaxLine_core_y1_rsci_y1_wait_ctrl_inst_y1_rsci_irdy,
      /scverify_top/rtl/getMaxLine_inst/getMaxLine_core_inst/getMaxLine_core_y1_rsci_inst/getMaxLine_core_y1_rsci_y1_wait_ctrl_inst/y1_rsci_irdy);
  OBSERVE_FOREIGN_SIGNAL(OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_y1_rsci_inst_getMaxLine_core_y1_rsci_y1_wait_ctrl_inst_y1_rsci_ivld_core_sct,
      /scverify_top/rtl/getMaxLine_inst/getMaxLine_core_inst/getMaxLine_core_y1_rsci_inst/getMaxLine_core_y1_rsci_y1_wait_ctrl_inst/y1_rsci_ivld_core_sct);
  OBSERVE_FOREIGN_SIGNAL(OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_x2_rsci_inst_getMaxLine_core_x2_rsci_x2_wait_ctrl_inst_x2_rsci_irdy,
      /scverify_top/rtl/getMaxLine_inst/getMaxLine_core_inst/getMaxLine_core_x2_rsci_inst/getMaxLine_core_x2_rsci_x2_wait_ctrl_inst/x2_rsci_irdy);
  OBSERVE_FOREIGN_SIGNAL(OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_x2_rsci_inst_getMaxLine_core_x2_rsci_x2_wait_ctrl_inst_x2_rsci_ivld_core_sct,
      /scverify_top/rtl/getMaxLine_inst/getMaxLine_core_inst/getMaxLine_core_x2_rsci_inst/getMaxLine_core_x2_rsci_x2_wait_ctrl_inst/x2_rsci_ivld_core_sct);
  OBSERVE_FOREIGN_SIGNAL(OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_y2_rsci_inst_getMaxLine_core_y2_rsci_y2_wait_ctrl_inst_y2_rsci_irdy,
      /scverify_top/rtl/getMaxLine_inst/getMaxLine_core_inst/getMaxLine_core_y2_rsci_inst/getMaxLine_core_y2_rsci_y2_wait_ctrl_inst/y2_rsci_irdy);
  OBSERVE_FOREIGN_SIGNAL(OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_y2_rsci_inst_getMaxLine_core_y2_rsci_y2_wait_ctrl_inst_y2_rsci_ivld_core_sct,
      /scverify_top/rtl/getMaxLine_inst/getMaxLine_core_inst/getMaxLine_core_y2_rsci_inst/getMaxLine_core_y2_rsci_y2_wait_ctrl_inst/y2_rsci_ivld_core_sct);
  OBSERVE_FOREIGN_SIGNAL(OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_acc_rsci_inst_getMaxLine_core_acc_rsci_acc_wait_ctrl_inst_acc_rsci_irdy_core_sct,
      /scverify_top/rtl/getMaxLine_inst/getMaxLine_core_inst/getMaxLine_core_acc_rsci_inst/getMaxLine_core_acc_rsci_acc_wait_ctrl_inst/acc_rsci_irdy_core_sct);
  OBSERVE_FOREIGN_SIGNAL(OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_acc_rsci_inst_getMaxLine_core_acc_rsci_acc_wait_ctrl_inst_acc_rsci_ivld,
      /scverify_top/rtl/getMaxLine_inst/getMaxLine_core_inst/getMaxLine_core_acc_rsci_inst/getMaxLine_core_acc_rsci_acc_wait_ctrl_inst/acc_rsci_ivld);
  OBSERVE_FOREIGN_SIGNAL(OFS_data_in_rsc_vld, /scverify_top/rtl/data_in_rsc_vld);
  OBSERVE_FOREIGN_SIGNAL(OFS_x1_rsc_rdy, /scverify_top/rtl/x1_rsc_rdy);
  OBSERVE_FOREIGN_SIGNAL(OFS_y1_rsc_rdy, /scverify_top/rtl/y1_rsc_rdy);
  OBSERVE_FOREIGN_SIGNAL(OFS_x2_rsc_rdy, /scverify_top/rtl/x2_rsc_rdy);
  OBSERVE_FOREIGN_SIGNAL(OFS_y2_rsc_rdy, /scverify_top/rtl/y2_rsc_rdy);
#endif
#endif
}

void scverify_top::deadlock_watch() {
#if !defined(CCS_DUT_SYSC) && defined(DEADLOCK_DETECTION)
#if defined(CCS_DUT_CYCLE) || defined(CCS_DUT_RTL)
#if defined(MTI_SYSTEMC) || defined(NCSC) || defined(VCS_SYSTEMC)
  if (!clk) {
    if (rst == SC_LOGIC_0 &&
      (OFS_getMaxLine_inst_getMaxLine_core_inst_getMaxLine_core_staller_inst_core_wen.read() == SC_LOGIC_0)
      && (OFS_houghTransform_inst_houghTransform_core_inst_houghTransform_core_staller_inst_core_wen.read() ==
          SC_LOGIC_0)
      && (OFS_data_in_rsc_vld.read() == SC_LOGIC_1)
      && (OFS_x1_rsc_rdy.read() == SC_LOGIC_1)
      && (OFS_y1_rsc_rdy.read() == SC_LOGIC_1)
      && (OFS_x2_rsc_rdy.read() == SC_LOGIC_1)
      && (OFS_y2_rsc_rdy.read() == SC_LOGIC_1)
    ) {
      deadlocked.write(SC_LOGIC_1);
      deadlock_event.notify(2466993727, SC_NS);
    } else {
      if (deadlocked.read() == SC_LOGIC_1)
        deadlock_event.cancel();
      deadlocked.write(SC_LOGIC_0);
    }
  }
#endif
#endif
#endif
}

void scverify_top::deadlock_notify() {
  if (deadlocked.read() == SC_LOGIC_1) {
    testbench_INST.check_results();
    SC_REPORT_ERROR("System", "Simulation deadlock detected");
    sc_stop();
  }
}

#if defined(MC_SIMULATOR_OSCI) || defined(MC_SIMULATOR_VCS)
int sc_main(int argc, char *argv[]) {
  sc_report_handler::set_actions("/IEEE_Std_1666/deprecated", SC_DO_NOTHING);
  scverify_top scverify_top("scverify_top");
  sc_start();
  return scverify_top.testbench_INST.failed();
}
#else
MC_MODULE_EXPORT(scverify_top);
#endif
