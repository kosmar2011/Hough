#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
cd "$SCRIPT_DIR"/build

#Algorithm with Catapult
#g++ -I /opt/mentor/catapult/Mgc_home/shared/include/ -I "$SCRIPT_DIR"/include -o "$SCRIPT_DIR"/build/main "$SCRIPT_DIR"/hough/bmp_io.cpp "$SCRIPT_DIR"/hough/Hough_tb.cpp

#Algorithm
g++ -I "$SCRIPT_DIR"/include/ -g -o "$SCRIPT_DIR"/build/main "$SCRIPT_DIR"/hough/bmp_io.cpp "$SCRIPT_DIR"/hough/HoughAlg_tb.cpp

#Algorithm CMD
#g++ -I ./include/ -o ./build/main ./hough/bmp_io.cpp ./hough/Hough_tb.cpp

#Run Build with Catapult
# "$SCRIPT_DIR"/build/main "$SCRIPT_DIR"/images/canny.bmp "$SCRIPT_DIR"/images/outHLS.bmp

#Run Build
"$SCRIPT_DIR"/build/main "$SCRIPT_DIR"/images/canny.bmp "$SCRIPT_DIR"/images/out.bmp

#Run Build CMD
#./build/main ./images/canny.bmp ./images/out.bmp