// ----------------------------------------------------------------------------
// User Testbench Interface Header
//
//    HLS version: 10.5a/871028 Production Release
//       HLS date: Tue Apr 14 07:55:32 PDT 2020
//  Flow Packages: HDL_Tcl 8.0a, SCVerify 10.4
//
//   Generated by: user2@edatools.ee.duth.gr
// Generated date: Wed Jul 07 20:13:51 EEST 2021
//
// ----------------------------------------------------------------------------

#ifndef CCS_TESTBENCH_H
#define CCS_TESTBENCH_H
#include <ac_int.h>
#include <ac_channel.h>
#include "mc_wait_ctrl.h"
#include <string.h>
#include <iostream>

class testbench
{
   public:
   int argc;
   char** argv;
   int main(); //CCS_MAIN
   static bool enable_idle_sync_mode;
   static unsigned short idle_sync_stable_cycles;
   static void set_enable_stalls(bool flag);
   static void reset_request();
   static bool data_in_ignore;
   static bool data_in_skip;
   static bool data_in_skip_quiet;
   static bool data_in_skip_once;
   static bool data_in_skip_noerr;
   static int  data_in_array_comp_first;
   static int  data_in_array_comp_last;
   static mc_wait_ctrl data_in_wait_ctrl;
   static bool widthIn_ignore;
   static bool widthIn_skip;
   static bool widthIn_skip_quiet;
   static bool widthIn_skip_once;
   static bool widthIn_skip_noerr;
   static int  widthIn_array_comp_first;
   static int  widthIn_array_comp_last;
   static mc_wait_ctrl widthIn_wait_ctrl;
   static bool heightIn_ignore;
   static bool heightIn_skip;
   static bool heightIn_skip_quiet;
   static bool heightIn_skip_once;
   static bool heightIn_skip_noerr;
   static int  heightIn_array_comp_first;
   static int  heightIn_array_comp_last;
   static mc_wait_ctrl heightIn_wait_ctrl;
   static bool x1_ignore;
   static bool x1_skip;
   static bool x1_skip_quiet;
   static bool x1_skip_once;
   static bool x1_skip_noerr;
   static int  x1_array_comp_first;
   static int  x1_array_comp_last;
   static bool x1_use_mask;
   static ac_int<11, false > x1_output_mask;
   static mc_wait_ctrl x1_wait_ctrl;
   static bool y1_ignore;
   static bool y1_skip;
   static bool y1_skip_quiet;
   static bool y1_skip_once;
   static bool y1_skip_noerr;
   static int  y1_array_comp_first;
   static int  y1_array_comp_last;
   static bool y1_use_mask;
   static ac_int<10, false > y1_output_mask;
   static mc_wait_ctrl y1_wait_ctrl;
   static bool x2_ignore;
   static bool x2_skip;
   static bool x2_skip_quiet;
   static bool x2_skip_once;
   static bool x2_skip_noerr;
   static int  x2_array_comp_first;
   static int  x2_array_comp_last;
   static bool x2_use_mask;
   static ac_int<11, false > x2_output_mask;
   static mc_wait_ctrl x2_wait_ctrl;
   static bool y2_ignore;
   static bool y2_skip;
   static bool y2_skip_quiet;
   static bool y2_skip_once;
   static bool y2_skip_noerr;
   static int  y2_array_comp_first;
   static int  y2_array_comp_last;
   static bool y2_use_mask;
   static ac_int<10, false > y2_output_mask;
   static mc_wait_ctrl y2_wait_ctrl;
   #ifndef CCS_SCVERIFY_USE_CCS_BLOCK
   static void exec_run( ac_channel<ac_int<8, false > > &data_in,  ac_int<11, false > &widthIn,  ac_int<10, false > &heightIn,  ac_channel<ac_int<11, false > > &x1,  ac_channel<ac_int<10, false > > &y1,  ac_channel<ac_int<11, false > > &x2,  ac_channel<ac_int<10, false > > &y2);
   #endif
   explicit testbench(int _argc, const char* const *_argv)
      :argc(_argc), argv(const_cast<char**>(_argv))
   {
   }
   ~testbench()
   {
   }
   private:
   testbench() {}
};
extern void mc_testbench_reset_request();
extern void mc_testbench_data_in_skip(bool v);
extern void mc_testbench_widthIn_skip(bool v);
extern void mc_testbench_heightIn_skip(bool v);
extern void mc_testbench_x1_skip(bool v);
extern void mc_testbench_y1_skip(bool v);
extern void mc_testbench_x2_skip(bool v);
extern void mc_testbench_y2_skip(bool v);
#endif //CCS_TESTBENCH_H
