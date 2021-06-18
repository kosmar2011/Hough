#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
cd "$SCRIPT_DIR"/build

#Algorithm with Catapult
#g++ -I /opt/mentor/catapult/Mgc_home/shared/include/ -I "$SCRIPT_DIR"/include -o "$SCRIPT_DIR"/build/main "$SCRIPT_DIR"/hough/bmp_io.cpp "$SCRIPT_DIR"/hough/Hough_tb.cpp

#Algorithm
g++ -I "$SCRIPT_DIR"/include -o "$SCRIPT_DIR"/build/main "$SCRIPT_DIR"/hough/bmp_io.cpp "$SCRIPT_DIR"/hough/Hough_tb.cpp

#Run Build
"$SCRIPT_DIR"/build/main hls_tutorials/image/people_gray.bmp out.bmp