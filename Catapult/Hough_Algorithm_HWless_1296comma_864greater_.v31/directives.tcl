//  Catapult Ultra Synthesis 10.5a/871028 (Production Release) Tue Apr 14 07:55:32 PDT 2020
//  
//  Copyright (c) Mentor Graphics Corporation, 1996-2020, All Rights Reserved.
//                        UNPUBLISHED, LICENSED SOFTWARE.
//             CONFIDENTIAL AND PROPRIETARY INFORMATION WHICH IS THE
//                 PROPERTY OF MENTOR GRAPHICS OR ITS LICENSORS
//  
//  Running on Linux user2@edatools.ee.duth.gr 2.6.32-754.22.1.el6.x86_64 x86_64 aol
//  
//  Package information: SIFLIBS v23.5_1.0, HLS_PKGS v23.5_1.0, 
//                       SIF_TOOLKITS v23.5_1.0, SIF_XILINX v23.5_1.0, 
//                       SIF_ALTERA v23.5_1.0, CCS_LIBS v23.5_1.0, 
//                       CDS_PPRO v10.3c_2, CDS_DesigChecker v10.5a, 
//                       CDS_OASYS v19.1_3.7, CDS_PSR v19.2_0.9, 
//                       DesignPad v2.78_1.0
//  
solution new -state initial
solution options defaults
solution options set /Output/GenerateCycleNetlist false
solution options set /Flows/Enable-SCVerify yes
solution options set /Flows/SCVerify/USE_CCS_BLOCK true
solution file add ./hough/Hough_tb.cpp -type C++
directive set -DESIGN_GOAL area
directive set -SPECULATE true
directive set -MERGEABLE true
directive set -REGISTER_THRESHOLD 256
directive set -MEM_MAP_THRESHOLD 32
directive set -LOGIC_OPT false
directive set -FSM_ENCODING none
directive set -FSM_BINARY_ENCODING_THRESHOLD 64
directive set -REG_MAX_FANOUT 0
directive set -NO_X_ASSIGNMENTS true
directive set -SAFE_FSM false
directive set -REGISTER_SHARING_MAX_WIDTH_DIFFERENCE 8
directive set -REGISTER_SHARING_LIMIT 0
directive set -ASSIGN_OVERHEAD 0
directive set -TIMING_CHECKS true
directive set -MUXPATH true
directive set -REALLOC true
directive set -UNROLL no
directive set -IO_MODE super
directive set -CHAN_IO_PROTOCOL use_library
directive set -ARRAY_SIZE 1024
directive set -REGISTER_IDLE_SIGNAL false
directive set -IDLE_SIGNAL {}
directive set -STALL_FLAG false
directive set -TRANSACTION_DONE_SIGNAL true
directive set -DONE_FLAG {}
directive set -READY_FLAG {}
directive set -START_FLAG {}
directive set -RESET_CLEARS_ALL_REGS use_library
directive set -CLOCK_OVERHEAD 20.000000
directive set -OPT_CONST_MULTS use_library
directive set -CHARACTERIZE_ROM false
directive set -PROTOTYPE_ROM true
directive set -ROM_THRESHOLD 64
directive set -CLUSTER_ADDTREE_IN_WIDTH_THRESHOLD 0
directive set -CLUSTER_ADDTREE_IN_COUNT_THRESHOLD 0
directive set -CLUSTER_OPT_CONSTANT_INPUTS true
directive set -CLUSTER_RTL_SYN false
directive set -CLUSTER_FAST_MODE false
directive set -CLUSTER_TYPE combinational
directive set -PIPELINE_RAMP_UP true
go new
solution design set {Hough_Algorithm_HW<1296, 864>::houghTransform} -block
solution design set {Hough_Algorithm_HW<1296, 864>::getMaxLine} -block
solution library add nangate-45nm_beh -- -rtlsyntool OasysRTL -vendor Nangate -technology 045nm
solution library add ram_nangate-45nm_pipe_beh
solution library add ram_nangate-45nm-dualport_beh
solution library add ram_nangate-45nm-separate_beh
solution library add ram_nangate-45nm-singleport_beh
solution library add ram_nangate-45nm-register-file_beh
solution library add rom_nangate-45nm_beh
solution library add rom_nangate-45nm-sync_regin_beh
solution library add rom_nangate-45nm-sync_regout_beh
solution library add ccs_sample_mem
solution library add amba
solution library add ML_amba
solution library add ccs_fpga_hic
go libraries
directive set -CLOCKS {clk {-CLOCK_PERIOD 1.0 -CLOCK_EDGE rising -CLOCK_UNCERTAINTY 0.0 -CLOCK_HIGH_TIME 0.5 -RESET_SYNC_NAME rst -RESET_ASYNC_NAME arst_n -RESET_KIND sync -RESET_SYNC_ACTIVE high -RESET_ASYNC_ACTIVE low -ENABLE_ACTIVE high}}
go assembly
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/HCOL -ITERATIONS 1296
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/HROW -ITERATIONS 864
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/HCOL -PIPELINE_INIT_INTERVAL 0
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/WRITE -PIPELINE_INIT_INTERVAL 1
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/HACC -UNROLL no
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/HACC -PIPELINE_INIT_INTERVAL 1
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/for#1 -UNROLL 4
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/HROW -PIPELINE_INIT_INTERVAL 0
go architect
go extract
