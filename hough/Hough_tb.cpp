// read bmp image
// run canny+blur in matlab
// run hough algorithm to get biggest line
// plot line to see if correct
// save image with red line from coordinates

// HOUGH 
// https://github.com/brunokeymolen/hough/blob/master/hough.cpp

// HLS CATAPULT
// https://github.com/hlslibs/hls_tutorials/tree/master/WalkThroughs/EdgeDetect/src

using namespace std;

#include "Hough.hpp"
#include "bmp_io.h"

#include <stdio.h>
#include <stdlib.h>




//#include <mc_scverify.h>


void plotLineLow(unsigned char data_in, int x1, int y1, int x2, int y2, int width);
void plotLineHigh(unsigned char data_in, int x1, int y1, int x2, int y2, int width);
void plotLine(unsigned char data_in, int x1, int y1, int x2, int y2, int width);

int main(int argc, char *argv[]){
    const int iW = 1296;
    const int iH = 864;

    Hough_Algorithm inst0;

    unsigned long int width = iW;
    long int height         = iH;
    unsigned char *rarray = new unsigned char[iW*iH];
    unsigned char *garray = new unsigned char[iW*iH];
    unsigned char *barray = new unsigned char[iW*iH];

    cout << "Loading Input File" << endl;

    if (argc < 3){
      cout << "Usage: " << argv[0] << " <inputbmp> <outputbmp_alg> <outputbmp_ba>" << endl;
      return(-1);
    }

    std::string bmpIn(argv[1]);  // input bitmap file
    std::string bmpAlg(argv[2]); // output bitmap (algorithm)

    bmp_read((char*)bmpIn.c_str(), &width, &height, &rarray, &garray, &barray);
    assert(width==iW);
    assert(height==iH);

    unsigned char *dat_in_orig = new unsigned char[iH*iW];
    int x1, y1, x2, y2;

    unsigned  cnt = 0;
    for (int y = 0; y < iH; y++){
        for (int x = 0; x < iW; x++){
          dat_in_orig[cnt] = rarray[cnt]; // just using red component (pseudo monochrome)
          cnt++;
        }
    }

    cout << "Running" << endl;

    inst0.run(dat_in_orig, x1, y1, x2, y2);

    plotLine(dat_in_orig, x1, y1, x2, y2, iW);

    cnt = 0;
    for (int y = 0; y < iH; y++){
        for (int x = 0; x < iW; x++){
            int alg = (int)*(dat_in_orig+cnt);
            cnt++;
            garray[cnt] = alg;  // repurposing 'green' array to the original algorithmic edge-detect output
        }
    }

    cout << "Writing algorithmic bitmap output to: " << bmpAlg << endl;
    bmp_24_write((char*)bmpAlg.c_str(), iW,  iH, garray, garray, garray);

    delete(dat_in_orig);
    delete(rarray);
    delete(garray);
    delete(barray);

    cout << "Finished" << endl;
    return(0);
}

void plotLine(unsigned char *data_in, int x1, int y1, int x2, int y2, int iW){
    if(abs(y2 - y1) < abs(x2 - x1)){
        if(x1 > x2) {
            plotLineLow(data_in, x2, y2, x1, y1, iW);
        } else {
            plotLineLow(data_in, x1, y1, x2, y2, iW);
        }
    } else {
        if(y1 > y2){
            plotLineHigh(data_in, x2, y2, x1, y1, iW);
        } else {
            plotLineHigh(data_in, x1, y1, x2, y2, iW);
        }
    }
}

void plotLineLow(unsigned char *data_in, int x1, int y1, int x2, int y2, int iW){
    int dx = x2-x1;
    int dy = y2-y1;
    int yi = 1;
    if(dy < 0){
        yi = -1;
        dy = -dy;
    }
    int D = (2 * dy) - dx;
    int y = y1;
    for (int x = x1; x < x2; x++){
        data_in[y * iW + x] = 255; // isos lathso
        if (D > 0){
            y = y + yi;
            D = D + (2 * (dy - dx));
        } else {
            D = D + 2*dy;
        }
    }   
}

void plotLineHigh(unsigned char *data_in, int x1, int y1, int x2, int y2, int iW){
    int dx = x2-x1;
    int dy = y2-y1;
    int xi = 1;
    if(dx < 0){
        xi = -1;
        dx = -dx;
    }
    int D = (2 * dx) - dy;
    int x = x1;
    for (int y = y1; y < y2; y++){
        data_in[y * iW + x] = 255;
        if (D > 0){
            x = x + xi;
            D = D + (2 * (dx - dy));
        } else {
            D = D + 2*dx;
        }
    }   
}
