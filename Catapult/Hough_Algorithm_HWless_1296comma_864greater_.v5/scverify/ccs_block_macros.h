// ccs_block_macros.h
#include "ccs_testbench.h"

#ifndef EXCLUDE_CCS_BLOCK_INTERCEPT
#ifndef INCLUDE_CCS_BLOCK_INTERCEPT
#define INCLUDE_CCS_BLOCK_INTERCEPT
#ifdef  CCS_DESIGN_FUNC_Hough_Algorithm_HW_1296_864_run
extern void mc_testbench_capture_IN( ac_channel<ac_int<8, false > > &data_in,  ac_int<11, false > &widthIn,  ac_int<10, false > &heightIn,  ac_channel<ac_int<11, false > > &x1,  ac_channel<ac_int<10, false > > &y1,  ac_channel<ac_int<11, false > > &x2,  ac_channel<ac_int<10, false > > &y2);
extern void mc_testbench_capture_OUT( ac_channel<ac_int<8, false > > &data_in,  ac_int<11, false > &widthIn,  ac_int<10, false > &heightIn,  ac_channel<ac_int<11, false > > &x1,  ac_channel<ac_int<10, false > > &y1,  ac_channel<ac_int<11, false > > &x2,  ac_channel<ac_int<10, false > > &y2);
extern void mc_testbench_wait_for_idle_sync();

//Capture DUT class template specialization
template < int imageWidth, int imageHeight >
struct ccs_class_spec {
  static void* get( void* _this ) { return NULL; }
};
template<>
struct ccs_class_spec<1296,864> {
  static void* get( void* _this ) { return _this; }
};

//Generic template class and DUT specialization
template < int imageWidth, int imageHeight >
class ccs_intercept
{
  public:
  void capture_THIS( void* _this ) {}
  void capture_IN( ac_channel<ac_int<8, false > > &data_in,ac_int<11, false > &widthIn,ac_int<10, false > &heightIn,ac_channel<ac_int<11, false > > &x1,ac_channel<ac_int<10, false > > &y1,ac_channel<ac_int<11, false > > &x2,ac_channel<ac_int<10, false > > &y2 ) {}
  void capture_OUT(  ac_channel<ac_int<8, false > > &data_in,ac_int<11, false > &widthIn,ac_int<10, false > &heightIn,ac_channel<ac_int<11, false > > &x1,ac_channel<ac_int<10, false > > &y1,ac_channel<ac_int<11, false > > &x2,ac_channel<ac_int<10, false > > &y2 ) {}
  void wait_for_idle_sync() {}
  ccs_intercept() {}
};

template<>
class ccs_intercept<1296,864>
{
  public:
  void capture_THIS( void* _this ) {
    if ( dut == NULL ) dut = _this;
    cur_inst = _this;
    if ( capture_msg && dut != NULL && dut == cur_inst ) {
      std::cout << "SCVerify intercepting C++ function 'Hough_Algorithm_HW<1296, 864>::run' for RTL block 'Hough_Algorithm_HW_1296_864'" << std::endl;
      capture_msg = false;
    }
  }
  void capture_IN(  ac_channel<ac_int<8, false > > &data_in,  ac_int<11, false > &widthIn,  ac_int<10, false > &heightIn,  ac_channel<ac_int<11, false > > &x1,  ac_channel<ac_int<10, false > > &y1,  ac_channel<ac_int<11, false > > &x2,  ac_channel<ac_int<10, false > > &y2 ) {
    if ( dut != NULL && dut == cur_inst )
      mc_testbench_capture_IN(data_in,widthIn,heightIn,x1,y1,x2,y2);
  }
  void capture_OUT(  ac_channel<ac_int<8, false > > &data_in,  ac_int<11, false > &widthIn,  ac_int<10, false > &heightIn,  ac_channel<ac_int<11, false > > &x1,  ac_channel<ac_int<10, false > > &y1,  ac_channel<ac_int<11, false > > &x2,  ac_channel<ac_int<10, false > > &y2 ) {
    if ( dut != NULL && dut == cur_inst )
      mc_testbench_capture_OUT(data_in,widthIn,heightIn,x1,y1,x2,y2);
  }
  void wait_for_idle_sync() {
    if ( dut != NULL && dut == cur_inst )
      mc_testbench_wait_for_idle_sync();
  }
  ccs_intercept(): dut(NULL), cur_inst(NULL), capture_msg(true){}
  private:
  void *dut, *cur_inst;
  bool capture_msg;
};

#define ccs_intercept_run_40 \
  run(ac_channel<ac_int<8, false > > &data_in,ac_int<11, false > &widthIn,ac_int<10, false > &heightIn,ac_channel<ac_int<11, false > > &x1,ac_channel<ac_int<10, false > > &y1,ac_channel<ac_int<11, false > > &x2,ac_channel<ac_int<10, false > > &y2) {\
    static ccs_intercept<imageWidth,imageHeight> ccs_intercept_inst;\
    void* ccs_intercept_this = this;\
    ccs_intercept_this = ccs_class_spec<imageWidth,imageHeight>::get(ccs_intercept_this);\
    ccs_intercept_inst.capture_THIS(ccs_intercept_this);\
    ccs_intercept_inst.capture_IN(data_in,widthIn,heightIn,x1,y1,x2,y2);\
    ccs_real_run(data_in,widthIn,heightIn,x1,y1,x2,y2);\
    ccs_intercept_inst.capture_OUT(data_in,widthIn,heightIn,x1,y1,x2,y2);\
  }\
  void ccs_real_run
#else
#define ccs_intercept_run_40 run
#endif //CCS_DESIGN_FUNC_Hough_Algorithm_HW_1296_864_run
#endif //INCLUDE_CCS_BLOCK_INTERCEPT
#endif //EXCLUDE_CCS_BLOCK_INTERCEPT

// getMaxLine 144 BLOCK
#define ccs_intercept_getMaxLine_144 getMaxLine
// houghTransform 61 BLOCK
#define ccs_intercept_houghTransform_61 houghTransform
// Hough_Algorithm_HW 17 MODULE
#define ccs_intercept_Hough_Algorithm_HW_17 Hough_Algorithm_HW
// atan_pi_2mi 248 INLINE
#define ccs_intercept_atan_pi_2mi_248 atan_pi_2mi
// K 255 INLINE
#define ccs_intercept_K_255 K
// ac_sincos_cordic 282 INLINE
#define ccs_intercept_ac_sincos_cordic_282 ac_sincos_cordic
// ac_shift_left 313 INLINE
#define ccs_intercept_ac_shift_left_313 ac_shift_left
// ac_shift_right 168 INLINE
#define ccs_intercept_ac_shift_right_168 ac_shift_right
// ac_div 367 INLINE
#define ccs_intercept_ac_div_367 ac_div
// ac_div 288 INLINE
#define ccs_intercept_ac_div_288 ac_div
// main 24 INLINE
#define ccs_intercept_main_24 main
// plotLine 123 INLINE
#define ccs_intercept_plotLine_123 plotLine
// plotLineLow 145 INLINE
#define ccs_intercept_plotLineLow_145 plotLineLow
// plotLineHigh 166 INLINE
#define ccs_intercept_plotLineHigh_166 plotLineHigh
