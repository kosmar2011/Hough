#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
cd "$SCRIPT_DIR"/build


while getopts c: flag
do
    case "${flag}" in
        c) use_catapult=${OPTARG};;
    esac
done

if $use_catapult == true
then
    g++ -I /opt/mentor/catapult/Mgc_home/shared/include/ -I "$SCRIPT_DIR"/include/ -o "$SCRIPT_DIR"/build/main -std=c++0x "$SCRIPT_DIR"/hough/bmp_io.cpp "$SCRIPT_DIR"/hough/Hough_tb.cpp
    "$SCRIPT_DIR"/build/main "$SCRIPT_DIR"/images/canny.bmp "$SCRIPT_DIR"/images/outHLS.bmp
    display "$SCRIPT_DIR"/images/outHLS.bmp
else
    g++ -I "$SCRIPT_DIR"/include/ -g -o "$SCRIPT_DIR"/build/main -std=c++0x "$SCRIPT_DIR"/hough/bmp_io.cpp "$SCRIPT_DIR"/hough/HoughAlg_tb.cpp
    "$SCRIPT_DIR"/build/main "$SCRIPT_DIR"/images/canny.bmp "$SCRIPT_DIR"/images/out.bmp
    display "$SCRIPT_DIR"/images/out.bmp	
fi

#Algorithm CMD
#g++ -I /opt/mentor/catapult/Mgc_home/shared/include/ -I ./include/ -o ./build/main ./hough/bmp_io.cpp ./hough/Hough_tb.cpp
#Run Build CMD
#./build/main ./images/canny.bmp ./images/out.bmp
