#ifndef _INCLUDED_HOUGH_H_
#define _INCLUDED_HOUGH_H_

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "assert.h"

#include <ac_fixed.h>
#include <ac_channel.h>
#include <ac_math/ac_sincos_cordic.h>
#include <ac_math/ac_sqrt_pwl.h>


#include <mc_scverify.h>

void plotLineLow(unsigned char* data_in, int x1, int y1, int x2, int y2, int width);
void plotLineHigh(unsigned char* data_in, int x1, int y1, int x2, int y2, int width);
void plotLine(unsigned char* data_in, int x1, int y1, int x2, int y2, int width);

template<int imageWidth, int imageHeight>
class Hough_Algorithm{
	
    typedef uint8       pixelType;
    typedef ac_fixed<8,3,true>  angType;
    pixelType theta_len_acc = 180;
    
    ac_fixed<2,2,false> two = 2;
    ac_fixed<4,1,false> sqrt_two;
    ac_math::ac_sqrt_pwl(two, sqrt_two);
    ac_fixed<12, 10, false> rho_len = ( (sqrt_two * (ac_fixed<1,1,false>)(imageHeight > imageWidth ? imageHeight : imageWidth)) / 2.0); //thelei prosoxi i diairesi kai o poll/smos
    ac_fixed<19, 14, false> rho_len_acc = rho_len * 2.0;
    ac_fixed<8,1,false> DEG2RAD = 0.017453293;
    
    //pixelType *acc = (unsigned int*)malloc(rho_len_acc * theta_len_acc * sizeof(unsigned int));
    ac_channel<pixelType> acc;
		
public:	

    typedef ac_int<ac::nbits<imageWidth+1>::val, false>  maxW;
    typedef ac_int<ac::nbits<imageHeight+1>::val, false> maxH;
    Hough_Algorithm() {}

#pragma hls_design interface
    void CCS_BLOCK(run)(ac_channel<pixelType> &data_in,
						maxW &widthIn,
						maxH &heightIn,
						ac_channel<pixelType> &x1, ac_channel<pixelType> &y1, 
						ac_channel<pixelType> &x2, ac_channel<pixelType> &y2) //points are pixelType because they take indeces from data_in
	{
        // printf("run_0\n");
        // printf("rho_len_acc * theta_len_acc = %f\n", rho_len_acc * theta_len_acc);

        houghTransform(data_in, widthIn, heightIn, acc);

        // printf("run_1\n");

        getMaxLine(acc, widthIn, heightIn, x1, y1, x2, y2);

        // printf("run_2\n");

    }

private:

#pragma hls_design
    void houghTransform(ac_channel<pixelType> &data_in,
                        maxW &widthIn,
                        maxH &heightIn, 
                        ac_channel<pixelType> &acc){
        

        ac_fixed<ac::nbits<imageWidth+1>::val+4, ac::nbits<imageWidth+1>::val,false> center_x = widthIn  / 2;
		ac_fixed<ac::nbits<imageHeight+1>::val+4, ac::nbits<imageHeight+1>::val,false> center_y = heightIn / 2;
        ac_fixed<17, 11, true> r = 0;

        pixelType acc_buf;
        angType cos_out;
        angType sin_out;
        // printf("transform_0\n");
        // int count = 0;
    
        HROW: for (maxH y = 0; y < heightIn; y++){
            // printf("y = %d\n", y);
            HCOL: for (maxW x = 0; x < widthIn; x++){
                // printf("x = %d\n", x);
                // printf("data_in = %d\n", (int)(data_in[y * widthIn + x]));
                
                // if ( (data_in[(y * widthIn) + x]) > 250 ){ 
                pixelType din = data_in.read();
                if ( din > 250 ){ //Mipos xreiazetai na valo .to_uint() ????
                    // printf("in the if: %d\n", ++count); 
                    HACC: for (pixelType t = 0; t < theta_len_acc; t++){
                        // printf("t = %d\n", t);
                        
                        ac_math::ac_cos_cordic((ac_fixed<9,9,false>)t * DEG2RAD, cos_out)
                        ac_math::ac_sin_cordic((ac_fixed<9,9,false>)t * DEG2RAD, sin_out)
                        r = ( (ac_fixed<ac::nbits<imageWidth+1>::val+2, ac::nbits<imageWidth+1>::val,false>)x - center_x) * cos_out;
                        r = r + ((ac_fixed<ac::nbits<imageHeight+1>::val+2, ac::nbits<imageHeight+1>::val,false>)y - center_y) * sin_out;

                        // printf("r = %f\n", r);
                        // printf("r + rho_len = %f\n", r + rho_len);
                        // printf("index = %d\n", (int)((round(r + rho_len) * 180.0)) + t);
                        
                        // acc[ (int)((round(r + rho_len) * 180.0)) + t]++;



                        
                        // printf("%d\n", ++count);
                    }
                }
            }
        }
        return;
        // printf("transform_1\n");
    }

#pragma hls_design
    void getMaxLine(unsigned int *acc, int &x1, int &y1, int &x2, int &y2){
        // printf("max_line_0\n");
        int threshold = imageWidth > imageHeight ? imageWidth/4 : imageHeight/4;
        printf("threshold = %d\n", threshold);
        int max = 0;
        for (int r = 0, i = 0; r < rho_len_acc; r++){   
            for (int t = 0; t < theta_len_acc; t++){
                if ((int)acc[ (r * theta_len_acc) + t ] >= threshold){

                    if ( t>= 45 && t<=135){
                        // y = (r - x cos(t)) / sin(t)
                        x1 = 0;
                        y1 = ((double)(r-(rho_len_acc/2)) - ((x1 - (imageWidth/2) ) * cos(t * DEG2RAD))) / sin(t * DEG2RAD) + (imageHeight / 2);
                        x2 = imageWidth - 0;
                        y2 = ((double)(r-(rho_len_acc/2)) - ((x2 - (imageWidth/2) ) * cos(t * DEG2RAD))) / sin(t * DEG2RAD) + (imageHeight / 2);
                    } else {
                        // x = (r - y sin(t)) / cos(t);
                        y1 = 0;
                        x1 = ((double)(r-(rho_len_acc/2)) - ((y1 - (imageHeight/2) ) * sin(t * DEG2RAD))) / cos(t * DEG2RAD) + (imageWidth / 2);
						y2 = imageHeight - 0;
						x2 = ((double)(r-(rho_len_acc/2)) - ((y2 - (imageHeight/2) ) * sin(t * DEG2RAD))) / cos(t * DEG2RAD) + (imageWidth / 2);
                    }
                }
                // printf("x1 = %d, y1 = %d, x2 = %d, y2 = %d\n", x1, y1, x2, y2);

            }
        }
        // printf("max_line_1\n");
        // printf("x1 = %d, y1 = %d, x2 = %d, y2 = %d\n", x1, y1, x2, y2);
    }

};

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

#endif