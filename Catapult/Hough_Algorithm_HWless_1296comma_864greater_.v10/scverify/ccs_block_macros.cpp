void mc_testbench_capture_IN( ac_channel<ac_int<8, false > > &data_in,  ac_int<11, false > &widthIn,  ac_int<10, false > &heightIn,  ac_channel<ac_int<11, false > > &x1,  ac_channel<ac_int<10, false > > &y1,  ac_channel<ac_int<11, false > > &x2,  ac_channel<ac_int<10, false > > &y2) { mc_testbench::capture_IN(data_in,widthIn,heightIn,x1,y1,x2,y2); }
void mc_testbench_capture_OUT( ac_channel<ac_int<8, false > > &data_in,  ac_int<11, false > &widthIn,  ac_int<10, false > &heightIn,  ac_channel<ac_int<11, false > > &x1,  ac_channel<ac_int<10, false > > &y1,  ac_channel<ac_int<11, false > > &x2,  ac_channel<ac_int<10, false > > &y2) { mc_testbench::capture_OUT(data_in,widthIn,heightIn,x1,y1,x2,y2); }
void mc_testbench_wait_for_idle_sync() {mc_testbench::wait_for_idle_sync(); }

