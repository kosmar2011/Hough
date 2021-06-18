#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
cd "$SCRIPT_DIR"/build

g++ hough/Hough_tb.cpp -o testbench -I . -I /opt/mentor/catapult/Mgc_home/shared/include/

