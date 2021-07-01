// HOUGH 
// https://github.com/brunokeymolen/hough/blob/master/hough.cpp

// HLS CATAPULT
// https://github.com/hlslibs/hls_tutorials/tree/master/WalkThroughs/EdgeDetect/src

using namespace std;

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "assert.h"

#include "HoughHW.h"
//#include "HoughAlg.h"
#include "bmp_io.hpp"

#include <mc_scverify.h>

void plotLineLow(unsigned char* data_in, int x1, int y1, int x2, int y2, int width);
void plotLineHigh(unsigned char* data_in, int x1, int y1, int x2, int y2, int width);
void plotLine(unsigned char* data_in, int x1, int y1, int x2, int y2, int width);

CCS_MAIN(int argc, char *argv[]){
    const int iW = 1296;
    const int iH = 864;
    //Hough_Algorithm_Alg       inst0;
    Hough_Algorithm_HW<iW,iH> inst1;

	int bsize = 8;
    int width = iW;
    int height = iH;
    unsigned char *rarray = new unsigned char[iW*iH];
    unsigned char *garray = new unsigned char[iW*iH];
    unsigned char *barray = new unsigned char[iW*iH];
	
	Hough_Algorithm_HW<iW,iH>::maxW widthIn = iW;
    Hough_Algorithm_HW<iW,iH>::maxH heightIn = iH;
    

    cout << "Loading Input File" << endl;

    if (argc < 2){
      cout << "Usage: " << argv[0] << " <inputbmp> <outputbmp>" << endl;
      return(-1);
    }

    std::string bmpIn(argv[1]);  // input bitmap file
    std::string bmpAlg(argv[2]); // output bitmap (algorithm)

    bmp_read((char*)bmpIn.c_str(), &width, &height, &bsize, &rarray, &garray, &barray);
    assert(width==iW);
    assert(height==iH);

    unsigned char* dat_in_orig = new unsigned char[iH*iW];
    //unsigned char* line1        = new unsigned char[iH*iW];
    unsigned char* line2        = new unsigned char[iH*iW];

    ac_channel<uint8> dat_in;
    ac_channel<Hough_Algorithm_HW<iW,iH>::maxW> x1_hw, x2_hw;
    ac_channel<Hough_Algorithm_HW<iW,iH>::maxH> y1_hw, y2_hw;
    int x1_alg = 0, y1_alg = 0, x2_alg = 0, y2_alg = 0;
    unsigned int x1 = 0, y1 = 0, x2 = 0, y2 = 0;
	

    unsigned  cnt = 0;
    for (int y = 0; y < iH; y++){
        for (int x = 0; x < iW; x++){
          dat_in.write(rarray[cnt]);
          dat_in_orig[cnt] = rarray[cnt]; // just using red component (pseudo monochrome)
          //line1[cnt] = 0;
          line2[cnt] = 0;
          cnt++;
        }
    }

    cout << "Running" << endl;
    //inst0.run(dat_in_orig, x1_alg, y1_alg, x2_alg, y2_alg);
    inst1.run(dat_in, widthIn, heightIn, x1_hw, y1_hw, x2_hw, y2_hw);
    

    x1 = x1_hw.read();
    y1 = y1_hw.read();
    x2 = x2_hw.read();
    y2 = y2_hw.read();

    cout << "Drawing Line\n";
    // plotLine(line1, x1_alg, y1_alg, x2_alg, y2_alg, iW); //normal algorithm
    plotLine(line2, x1, y1, x2, y2, iW);                 //hardware
    
    cout << "Calculating difference between output points: \n" << endl;
    //printf("ALG POINTS: x1_alg = %d, y1_alg = %d, x2_alg = %d, y2_alg = %d\n", x1_alg, y1_alg, x2_alg, y2_alg);
	cout << "HW  POINTS: x1_hw  = " << x1 << ", y1_hw = " << y1 << ", x2_hw = " << x2 << ", y2_hw = " << y2 << endl;
    cout << "Calculating RGB bitmap\n";
    cnt = 0;
    for (int y = 0; y < iH; y++){
        for (int x = 0; x < iW; x++){
            int alg = (int)*(dat_in_orig+cnt);

            int ln  = (int)*(line2+cnt);
            cnt++;
            rarray[cnt] = (ln==0) ? alg :  0;
            garray[cnt] = (ln==0) ? alg : ln;
            barray[cnt] = (ln==0) ? alg :  0;
        }
    }

    cout << "Writing algorithmic bitmap output to: " << bmpAlg << endl;

    bmp_write((char*)bmpAlg.c_str(), iW,  iH, bsize, rarray, garray, barray);


    delete(dat_in_orig);
    delete(rarray);
    delete(garray);
    delete(barray);
    cout << "Finished" << endl;
    CCS_RETURN(0);
}



void plotLine(unsigned char *data_in, int x1, int y1, int x2, int y2, int iW){
    // auto tmp = data_in;
    if(abs(y2 - y1) < abs(x2 - x1)){
        if(x1 > x2) {
            printf("plotLine_1\n");
            plotLineLow(data_in, x2, y2, x1, y1, iW);
        } else {
            printf("plotLine_2\n");
            plotLineLow(data_in, x1, y1, x2, y2, iW);
        }
    } else {
        if(y1 > y2){
            printf("plotLine_3\n");
            plotLineHigh(data_in, x2, y2, x1, y1, iW);
        } else {
            printf("plotLine_4\n");
            plotLineHigh(data_in, x1, y1, x2, y2, iW);
        }
    }
    // assert(tmp != data_in); // prepei na exei allaksei kati as poume ti fasi
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
        data_in[y * iW + x] = 255; 
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
