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



template<int imageWidth, int imageHeight>
class Hough_Algorithm{
	
    typedef uint8               pixelType;
    typedef ac_fixed<8,3,true>  angType;

    pixelType theta_len_acc = 180;    
    ac_fixed<2,2,false> two = 2;
    ac_fixed<4,1,false> sqrt_two;
    ac_math::ac_sqrt_pwl(two, sqrt_two);
    ac_fixed<12, 10, false> rho_len = ( (sqrt_two * (ac_fixed<1,1,false>)(imageHeight > imageWidth ? imageHeight : imageWidth)) / 2.0); //thelei prosoxi i diairesi kai o poll/smos
    ac_fixed<19, 14, false> rho_len_acc = rho_len * 2.0;
    ac_fixed<8,1,false> DEG2RAD = 0.017453293;
    
    uint19 acc_tmp_len = rho_len_acc.to_uint() * theta_len_acc + 1; 
    pixelType acc_tmp[acc_tmp_len];
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

        angType cos_out;
        angType sin_out;
        // printf("transform_0\n");
        // int count = 0;
        
        for (uint19 i = 0; i < acc_tmp_len; i++){
            acc_tmp[i] = 0;
        }

        HROW: for (maxH y = 0; ; y++){
            // printf("y = %d\n", y);
            HCOL: for (maxW x = 0; ; x++){
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
                        
                        acc_tmp[ (int)((round(r + rho_len) * 180.0)) + t]++;
                        // printf("%d\n", ++count);
                    }
                }
                if (x == maxW(widthIn-1)) 
                    break;
            }
            if (y == maxH(heightIn-1)) 
                break;
        }

        for (uint19 i = 0; i < acc_tmp_len; i++){
            acc.write(acc_tmp[i])
        }
        return;
        // printf("transform_1\n");
    }

#pragma hls_design
    void getMaxLine(ac_channel<pixelType> &acc, 
                        maxW &widthIn,
						maxH &heightIn,
						ac_channel<pixelType> &x1, ac_channel<pixelType> &y1, 
						ac_channel<pixelType> &x2, ac_channel<pixelType> &y2){
        // printf("max_line_0\n");
        if(widthIn > heightIn){
            ac_fixed<ac::nbits<imageWidth+1>::val+4, ac::nbits<imageWidth+1>::val,false> threshold = widthIn/4;
            // maxW threshold = widthIn/4;
        } else {
            ac_fixed<ac::nbits<imageHeight+1>::val+4, ac::nbits<imageHeight+1>::val,false> threshold = heightIn/4;    
            // maxH threshold = heightIn/4;
        }
        maxW x1_t, x2_t;
        maxH y1_t, y2_t;

        angType cos_t;
        angType sin_t;
        // printf("threshold = %d\n", threshold);

        R_LINE: for (uint14 r = 0; r < rho_len_acc; r++){   
            T_LINE: for (pixelType t = 0; t < theta_len_acc; t++){
                // if ((int)acc[ (r * theta_len_acc) + t ] >= threshold){
                acc_in = acc.read();
                if (acc_in >= threshold){
                    
                    ac_math::ac_cos_cordic((ac_fixed<9,9,false>)t * DEG2RAD, cos_t);
                    ac_math::ac_sin_cordic((ac_fixed<9,9,false>)t * DEG2RAD, sin_t);
                    // cos_t = cos(t * DEG2RAD);
                    // sin_t = sin(t * DEG2RAD);
                    
                    if ( t>= 45 && t<=135){
                        // y = (r - x cos(t)) / sin(t)
                        x1_t = 0;
                        y1_t = ((double)(r-(rho_len_acc/2)) - ((x1_t - (widthIn/2) ) * cos_t)) / sin_t + (heightIn / 2);
                        x2_t = widthIn - 0;
                        y2_t = ((double)(r-(rho_len_acc/2)) - ((x2_t - (widthIn/2) ) * cos_t)) / sin_t + (heightIn / 2);
                    } else {
                        // x = (r - y sin(t)) / cos(t);
                        y1_t = 0;
                        x1_t = ((double)(r-(rho_len_acc/2)) - ((y1_t - (heightIn/2) ) * sin_t)) / cos_t + (widthIn / 2);
						y2_t = heightIn - 0;
						x2_t = ((double)(r-(rho_len_acc/2)) - ((y2_t - (heightIn/2) ) * sin_t)) / cos_t + (widthIn / 2);
                    }
                }
                // printf("x1_t = %d, y1_t = %d, x2_t = %d, y2_t = %d\n", x1_t, y1_t, x2_t, y2_t);

            }
        }
        // printf("max_line_1\n");
        // printf("x1_t = %d, y1_t = %d, x2_t = %d, y2_t = %d\n", x1_t, y1_t, x2_t, y2_t);

        x1.write(x1_t);
        y1.write(y1_t);
        x2.write(x2_t);
        y2.write(y2_t);
    }

};

#endif