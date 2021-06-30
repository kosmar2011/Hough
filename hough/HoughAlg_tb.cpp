// HOUGH 
// https://github.com/brunokeymolen/hough/blob/master/hough.cpp

// HLS CATAPULT
// https://github.com/hlslibs/hls_tutorials/tree/master/WalkThroughs/EdgeDetect/src

using namespace std;

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "assert.h"

#include "HoughAlg.hh"
#include "bmp_io.hh"




//#include <mc_scverify.h>

int main(int argc, char *argv[]){
    const unsigned short iW = 1296;
    const unsigned short iH = 864;
    int bsize = 8;

    Hough_Algorithm inst0;

    int width = iW;
    int height = iH;
    unsigned char *rarray = new unsigned char[iW*iH];
    unsigned char *garray = new unsigned char[iW*iH];
    unsigned char *barray = new unsigned char[iW*iH];

    cout << " ###########################" << endl; 
    cout << " ALGORITHM WITHOUT CATAPULT!" << endl;
    cout << " ###########################" << endl << endl; 
    
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
    unsigned char* line        = new unsigned char[iH*iW];

    int x1 = 0, y1 = 0, x2 = 0, y2 = 0;

    unsigned  cnt = 0;
    for (int y = 0; y < iH; y++){
        for (int x = 0; x < iW; x++){
          dat_in_orig[cnt] = rarray[cnt]; // just using red component (pseudo monochrome)
          line[cnt] = 0;
          cnt++;
        }
    }

    cout << "Running\n";
    inst0.run(dat_in_orig, x1, y1, x2, y2);
    printf("LINE POINTS: x1 = %d, y1 = %d, x2 = %d, y2 = %d\n", x1, y1, x2, y2);

    cout << "Drawing Line\n";
    plotLine(line, x1, y1, x2, y2, iW); 
    
    cout << " Calculating RGB bitmap\n";
    cnt = 0;
    for (int y = 0; y < iH; y++){
        for (int x = 0; x < iW; x++){
            int alg = (int)*(dat_in_orig+cnt);
            int ln  = (int)*(line+cnt);
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
    return(0);
}
