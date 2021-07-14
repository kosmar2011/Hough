# written for flow package OasysRTL 
set sdc_version 1.7 

set_operating_conditions typical
set_load 2.0 [all_outputs]
## driver/slew constraints on inputs
set data_inputs [list data_in_rsc_vld {data_in_rsc_dat[*]} x1_rsc_rdy y1_rsc_rdy x2_rsc_rdy y2_rsc_rdy widthIn_rsc_vld {widthIn_rsc_dat[*]} {heightIn_rsc_z[*]}]
set_driving_cell -no_design_rule -library NangateOpenCellLibrary -lib_cell BUF_X2 -pin Z $data_inputs
set_units -time ns
# time_factor: 1.0

create_clock -name clk -period 1.0 -waveform { 0.0 0.5 } [get_ports {clk}]
set_clock_uncertainty 0.0 [get_clocks {clk}]

create_clock -name virtual_io_clk -period 1.0
## IO TIMING CONSTRAINTS
set_input_delay -clock [get_clocks {clk}] 0.0 [get_ports {rst}]
set_input_delay -clock [get_clocks {clk}] 0.0 [get_ports {data_in_rsc_dat[*]}]
set_input_delay -clock [get_clocks {clk}] 0.0 [get_ports {data_in_rsc_vld}]
set_output_delay -clock [get_clocks {clk}] 0.0 [get_ports {data_in_rsc_rdy}]
set_input_delay -clock [get_clocks {clk}] 0.0 [get_ports {widthIn_rsc_dat[*]}]
set_input_delay -clock [get_clocks {clk}] 0.0 [get_ports {widthIn_rsc_vld}]
set_output_delay -clock [get_clocks {clk}] 0.0 [get_ports {widthIn_rsc_rdy}]
set_output_delay -clock [get_clocks {clk}] 0.0 [get_ports {widthIn_rsc_triosy_lz}]
set_input_delay -clock [get_clocks {clk}] 0.0 [get_ports {heightIn_rsc_z[*]}]
set_output_delay -clock [get_clocks {clk}] 0.0 [get_ports {heightIn_rsc_triosy_lz}]
set_output_delay -clock [get_clocks {clk}] 0.0 [get_ports {x1_rsc_dat[*]}]
set_output_delay -clock [get_clocks {clk}] 0.0 [get_ports {x1_rsc_vld}]
set_input_delay -clock [get_clocks {clk}] 0.0 [get_ports {x1_rsc_rdy}]
set_output_delay -clock [get_clocks {clk}] 0.0 [get_ports {y1_rsc_dat[*]}]
set_output_delay -clock [get_clocks {clk}] 0.0 [get_ports {y1_rsc_vld}]
set_input_delay -clock [get_clocks {clk}] 0.0 [get_ports {y1_rsc_rdy}]
set_output_delay -clock [get_clocks {clk}] 0.0 [get_ports {x2_rsc_dat[*]}]
set_output_delay -clock [get_clocks {clk}] 0.0 [get_ports {x2_rsc_vld}]
set_input_delay -clock [get_clocks {clk}] 0.0 [get_ports {x2_rsc_rdy}]
set_output_delay -clock [get_clocks {clk}] 0.0 [get_ports {y2_rsc_dat[*]}]
set_output_delay -clock [get_clocks {clk}] 0.0 [get_ports {y2_rsc_vld}]
set_input_delay -clock [get_clocks {clk}] 0.0 [get_ports {y2_rsc_rdy}]

