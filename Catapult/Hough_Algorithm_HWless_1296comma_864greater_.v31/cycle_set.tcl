
# Loop constraints
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp CSTEPS_FROM {{. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main CSTEPS_FROM {{. == 2} {.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/acc_tmp:vinit CSTEPS_FROM {{. == 1} {.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1 CSTEPS_FROM {{. == 15} {.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for CSTEPS_FROM {{. == 2} {.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for CSTEPS_FROM {{. == 2} {.. == 7}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for CSTEPS_FROM {{. == 2} {.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for CSTEPS_FROM {{. == 2} {.. == 13}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW CSTEPS_FROM {{. == 1} {.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL CSTEPS_FROM {{. == 2} {.. == 0}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC CSTEPS_FROM {{. == 8} {.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/WRITE CSTEPS_FROM {{. == 3} {.. == 1}}

# IO operation constraints
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/operator_<11,false>:io_read(widthIn:rsc.@) CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/operator_<10,false>:io_read(heightIn:rsc.@) CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HCOL:din:io_read(data_in) CSTEPS_FROM {{.. == 0}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/WRITE/WRITE:io_write(acc) CSTEPS_FROM {{.. == 2}}

# Sync operation constraints

# Real operation constraints
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/acc_tmp:vinit/for:write_mem(acc_tmp:rsc.@) CSTEPS_FROM {{.. == 0}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/acc_tmp:vinit/for:acc CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1:acc#6 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1:acc#7 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1:acc#8 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1:acc CSTEPS_FROM {{.. == 3}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:if:acc CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:else:if:acc#1 CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:and CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:mux#1 CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:mux#2 CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::atan_pi_2mi:read_rom(ac_math::atan_pi_pow2_table.rom_map_1) CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-1:operator>><33,3,true,AC_TRN,AC_WRAP>:rshift CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-1:operator>><33,3,true,AC_TRN,AC_WRAP>#1:rshift CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc#3 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc#1 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:else:acc CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:else:acc#1 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:else:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:mux CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:mux#1 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:acc CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-1:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:mux#2 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:write_mem(sin_out:rsc.@) CSTEPS_FROM {{.. == 5}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:write_mem(cos_out:rsc.@) CSTEPS_FROM {{.. == 5}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1:acc#9 CSTEPS_FROM {{.. == 5}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1:acc#10 CSTEPS_FROM {{.. == 5}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1:acc#11 CSTEPS_FROM {{.. == 6}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1:acc#2 CSTEPS_FROM {{.. == 6}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:if:acc#1 CSTEPS_FROM {{.. == 6}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:else:if:acc#1 CSTEPS_FROM {{.. == 7}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:and#1 CSTEPS_FROM {{.. == 7}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:mux#4 CSTEPS_FROM {{.. == 7}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:mux#5 CSTEPS_FROM {{.. == 7}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::atan_pi_2mi:read_rom(ac_math::atan_pi_pow2_table.rom_map_1)#1 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-2:operator>><33,3,true,AC_TRN,AC_WRAP>:rshift CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-2:operator>><33,3,true,AC_TRN,AC_WRAP>#1:rshift CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc#3 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc#1 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:else:acc CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:else:acc#1 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:else:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:mux#3 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:mux#4 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:acc CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-2:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:mux#5 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:write_mem(sin_out:rsc.@)#1 CSTEPS_FROM {{.. == 8}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:write_mem(cos_out:rsc.@)#1 CSTEPS_FROM {{.. == 8}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1:acc#12 CSTEPS_FROM {{.. == 8}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1:acc#13 CSTEPS_FROM {{.. == 8}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1:acc#14 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1:acc#3 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:if:acc#2 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:else:if:acc#1 CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:and#2 CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:mux#7 CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:mux#8 CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::atan_pi_2mi:read_rom(ac_math::atan_pi_pow2_table.rom_map_1)#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-3:operator>><33,3,true,AC_TRN,AC_WRAP>:rshift CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-3:operator>><33,3,true,AC_TRN,AC_WRAP>#1:rshift CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc#3 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc#1 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:else:acc CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:else:acc#1 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:else:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:mux#6 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:mux#7 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:acc CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-3:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:mux#8 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:write_mem(sin_out:rsc.@)#2 CSTEPS_FROM {{.. == 11}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:write_mem(cos_out:rsc.@)#2 CSTEPS_FROM {{.. == 11}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1:acc#15 CSTEPS_FROM {{.. == 11}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1:acc#16 CSTEPS_FROM {{.. == 11}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1:acc#17 CSTEPS_FROM {{.. == 12}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1:acc#4 CSTEPS_FROM {{.. == 12}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:if:acc#3 CSTEPS_FROM {{.. == 12}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:else:if:acc#1 CSTEPS_FROM {{.. == 12}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:and#3 CSTEPS_FROM {{.. == 12}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:mux#10 CSTEPS_FROM {{.. == 13}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:mux#11 CSTEPS_FROM {{.. == 13}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::atan_pi_2mi:read_rom(ac_math::atan_pi_pow2_table.rom_map_1)#3 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-4:operator>><33,3,true,AC_TRN,AC_WRAP>:rshift CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-4:operator>><33,3,true,AC_TRN,AC_WRAP>#1:rshift CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc#3 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc#1 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:else:acc CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:else:acc#1 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:else:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:mux#9 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:mux#10 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:acc CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:mux#11 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:write_mem(sin_out:rsc.@)#3 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:write_mem(cos_out:rsc.@)#3 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1:acc#5 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/for#1/for#1-4:acc CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HCOL:acc CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/HACC:read_mem(cos_out:rsc.@) CSTEPS_FROM {{.. == 0}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/HACC:acc CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/HACC:mul CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/HACC:read_mem(sin_out:rsc.@) CSTEPS_FROM {{.. == 0}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/HACC:acc#4 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/HACC:mul#1 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/HACC:acc#2 CSTEPS_FROM {{.. == 3}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/HACC:round_r:acc CSTEPS_FROM {{.. == 3}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/HACC:round_r:HACC:round_r:acc:conv_2f:acc CSTEPS_FROM {{.. == 3}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/HACC:idx:acc#1 CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/HACC:idx:acc#2 CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/HACC:idx:acc#3 CSTEPS_FROM {{.. == 5}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/HACC:idx:acc CSTEPS_FROM {{.. == 5}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/HACC:read_mem(acc_tmp:rsc.@) CSTEPS_FROM {{.. == 5}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/HACC:HACC:acc CSTEPS_FROM {{.. == 7}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/HACC:write_mem(acc_tmp:rsc.@) CSTEPS_FROM {{.. == 7}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/HACC:acc#3 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HACC/operator<<8,false>:acc CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/operator-<11,false>:acc CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HCOL:equal CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HCOL/HCOL:acc#1 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/operator-<10,false>:acc CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HROW:equal CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/HROW/HROW:acc CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/WRITE/WRITE:read_mem(acc_tmp:rsc.@) CSTEPS_FROM {{.. == 0}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/WRITE/WRITE:acc#1 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/houghTransform/core/core:rlp/main/WRITE/WRITE:acc CSTEPS_FROM {{.. == 1}}

# Probe constraints

# Loop constraints
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp CSTEPS_FROM {{. == 0}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main CSTEPS_FROM {{. == 2} {.. == 0}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for CSTEPS_FROM {{. == 5} {.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for CSTEPS_FROM {{. == 2} {.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE CSTEPS_FROM {{. == 1} {.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE CSTEPS_FROM {{. == 17} {.. == 0}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#1:for CSTEPS_FROM {{. == 1} {.. == 6}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#3:for CSTEPS_FROM {{. == 1} {.. == 13}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#5:for CSTEPS_FROM {{. == 1} {.. == 6}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#7:for CSTEPS_FROM {{. == 1} {.. == 11}}

# IO operation constraints
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:acc_in:io_read(acc) CSTEPS_FROM {{.. == 0}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/Hough_Algorithm_HW<1296,864>::getMaxLine:io_write(x1) CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/Hough_Algorithm_HW<1296,864>::getMaxLine:io_write(y1) CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/Hough_Algorithm_HW<1296,864>::getMaxLine:io_write(x2) CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/Hough_Algorithm_HW<1296,864>::getMaxLine:io_write(y2) CSTEPS_FROM {{.. == 1}}

# Sync operation constraints

# Real operation constraints
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/for:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/for:acc#3 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/for:acc#4 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/for:for:acc CSTEPS_FROM {{.. == 3}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:if:acc CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:else:if:acc#1 CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:and CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:mux#1 CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:mux#2 CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::atan_pi_2mi:read_rom(ac_math::atan_pi_pow2_table.rom_map_1) CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/operator>><33,3,true,AC_TRN,AC_WRAP>:rshift CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/operator>><33,3,true,AC_TRN,AC_WRAP>#1:rshift CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc#3 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc#1 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:if:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:else:acc CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:else:acc#1 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:else:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:mux CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:mux#1 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:acc CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:for:mux#2 CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:write_mem(sin_out#1:rsc.@) CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/ac_math::ac_sincos_cordic<52,10,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP>:write_mem(cos_out#1:rsc.@) CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/for:acc#1 CSTEPS_FROM {{.. == 5}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/for/for:acc CSTEPS_FROM {{.. == 5}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator>=<24,16,false,AC_TRN,AC_WRAP,16,false>:acc#1 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:if:acc CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:if:aelse:acc CSTEPS_FROM {{.. == 2}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:if:if:read_mem(sin_out#1:rsc.@) CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:if:if:dividend1:read_mem(cos_out#1:rsc.@) CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>:acc#3 CSTEPS_FROM {{.. == 3}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>:acc#2 CSTEPS_FROM {{.. == 5}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>:acc#4 CSTEPS_FROM {{.. == 3}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>:acc CSTEPS_FROM {{.. == 5}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:uN:qif:acc CSTEPS_FROM {{.. == 6}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:uN:mux CSTEPS_FROM {{.. == 6}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:uD:qif:acc CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:uD:mux CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#1:neg_D:acc CSTEPS_FROM {{.. == 6}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#1:for/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#1:for:mux#1 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#1:for/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#1:for:acc#1 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#1:for/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#1:for:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#1:for/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#1:for:acc CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#1:if#1:acc CSTEPS_FROM {{.. == 8}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#1:mux CSTEPS_FROM {{.. == 8}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:else:else:acc#1 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:else:else:acc CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:else:mux CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:mux CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:else:mux#1 CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:mux#1 CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:else:mux#2 CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:mux#2 CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:else:mux#3 CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:mux#3 CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:else:mux#4 CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:mux#4 CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:else:mux#5 CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:mux#5 CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator+<20,15,true,AC_TRN,AC_WRAP>:acc CSTEPS_FROM {{.. == 11}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>#1:acc#2 CSTEPS_FROM {{.. == 7}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>#1:acc#3 CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>#1:acc#1 CSTEPS_FROM {{.. == 12}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>#1:acc#4 CSTEPS_FROM {{.. == 7}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>#1:acc CSTEPS_FROM {{.. == 12}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:uN:qif:acc CSTEPS_FROM {{.. == 13}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:uN:mux CSTEPS_FROM {{.. == 13}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:uD:qif:acc CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:uD:mux CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#3:neg_D:acc CSTEPS_FROM {{.. == 11}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#3:for/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#3:for:mux#1 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#3:for/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#3:for:acc#1 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#3:for/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#3:for:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#3:for/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#3:for:acc CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#3:if#1:acc CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#3:mux CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:else:else:acc#1 CSTEPS_FROM {{.. == 15}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:else:else:acc CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:else:mux CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:mux CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:else:mux#1 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:mux#1 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:else:mux#2 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:mux#2 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:else:mux#3 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:mux#3 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:else:mux#4 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:mux#4 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:else:mux#5 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:mux#5 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator+<20,15,true,AC_TRN,AC_WRAP>#1:acc CSTEPS_FROM {{.. == 17}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>#2:acc#2 CSTEPS_FROM {{.. == 3}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>#2:acc#1 CSTEPS_FROM {{.. == 5}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>#2:acc#3 CSTEPS_FROM {{.. == 3}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>#2:acc CSTEPS_FROM {{.. == 5}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:uN:qif:acc CSTEPS_FROM {{.. == 6}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:uN:mux CSTEPS_FROM {{.. == 6}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:uD:qif:acc CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:uD:mux CSTEPS_FROM {{.. == 4}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#5:neg_D:acc CSTEPS_FROM {{.. == 6}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#5:for/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#5:for:mux#1 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#5:for/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#5:for:acc#1 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#5:for/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#5:for:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#5:for/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#5:for:acc CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#5:if#1:acc CSTEPS_FROM {{.. == 7}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#5:mux CSTEPS_FROM {{.. == 7}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:else:else:acc#1 CSTEPS_FROM {{.. == 8}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:else:else:acc CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:else:mux CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:mux CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:else:mux#1 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:mux#1 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:else:mux#2 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:mux#2 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:else:mux#3 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:mux#3 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:else:mux#4 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:mux#4 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:else:mux#5 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:mux#5 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:else:mux#6 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:mux#6 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:else:mux#7 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:mux#7 CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator+<20,15,true,AC_TRN,AC_WRAP>#2:acc CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>#3:acc#2 CSTEPS_FROM {{.. == 7}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>#3:acc#3 CSTEPS_FROM {{.. == 8}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>#3:acc#1 CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>#3:acc#4 CSTEPS_FROM {{.. == 7}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator-<28,13,true,AC_TRN,AC_WRAP>#3:acc CSTEPS_FROM {{.. == 10}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:uN:qif:acc CSTEPS_FROM {{.. == 11}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:uN:mux CSTEPS_FROM {{.. == 11}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:uD:qif:acc CSTEPS_FROM {{.. == 7}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:uD:mux CSTEPS_FROM {{.. == 7}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#7:neg_D:acc CSTEPS_FROM {{.. == 9}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#7:for/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#7:for:mux#1 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#7:for/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#7:for:acc#1 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#7:for/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#7:for:acc#2 CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#7:for/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#7:for:acc CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#7:if#1:acc CSTEPS_FROM {{.. == 12}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#7:mux CSTEPS_FROM {{.. == 12}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:else:else:acc#1 CSTEPS_FROM {{.. == 13}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:else:else:acc CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:else:mux CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:mux CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:else:mux#1 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:mux#1 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:else:mux#2 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:mux#2 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:else:mux#3 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:mux#3 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:else:mux#4 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:mux#4 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:else:mux#5 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:mux#5 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:else:mux#6 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:mux#6 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:else:mux#7 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:mux#7 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/operator+<20,15,true,AC_TRN,AC_WRAP>#3:acc CSTEPS_FROM {{.. == 15}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:mux#15 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:if:T_LINE:if:and CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:mux#16 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:else:mux#8 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:mux#8 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:mux#17 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:else:mux#9 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:mux#9 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:mux#18 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:else:mux#10 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#4:mux#10 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:mux#19 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:T_LINE:mux1h CSTEPS_FROM {{.. == 15}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:else:mux#8 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:mux#8 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:mux#21 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:else:mux#9 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:mux#9 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:mux#22 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:else:mux#10 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#6:mux#10 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:mux#23 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:if:T_LINE:if:and#7 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:mux#24 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:else:mux#6 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:mux#6 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:mux#25 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:else:mux#7 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:mux#7 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:mux#26 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:else:mux#8 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:mux#8 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:mux#27 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:else:mux#9 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>:mux#9 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:mux#28 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:T_LINE:mux1h#1 CSTEPS_FROM {{.. == 17}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:else:mux#6 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:mux#6 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:mux#30 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:else:mux#7 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:mux#7 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:mux#31 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:else:mux#8 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:mux#8 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:mux#32 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:else:mux#9 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/ac_math::ac_div<20,15,AC_TRN,AC_WRAP,27,3,AC_TRN,AC_WRAP,20,15,AC_TRN,AC_WRAP>#2:mux#9 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:mux#33 CSTEPS_FROM {{.. == 16}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:acc#1 CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/T_LINE/T_LINE:acc CSTEPS_FROM {{.. == 14}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/R_LINE:acc CSTEPS_FROM {{.. == 1}}
directive set /Hough_Algorithm_HW<1296,864>/getMaxLine/core/core:rlp/main/R_LINE/operator<<27,12,true,AC_TRN,AC_WRAP>:acc CSTEPS_FROM {{.. == 1}}

# Probe constraints
