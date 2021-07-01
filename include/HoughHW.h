#ifndef _INCLUDED_HOUGH_H_HW
#define _INCLUDED_HOUGH_H_HW


#include <cmath>
#include <ac_fixed.h>
#include <ac_channel.h>
#include <ac_math/ac_sincos_cordic.h>


#include <mc_scverify.h>



template<int imageWidth, int imageHeight>
class Hough_Algorithm_HW{
    typedef uint8               pixelType;
    typedef ac_fixed<8,3,true>  angType;
	
    ac_channel<pixelType> acc;
		
public:	


    typedef ac_int<ac::nbits<imageWidth+1>::val, false>  maxW;
    typedef ac_int<ac::nbits<imageHeight+1>::val, false> maxH;
    typedef ac_fixed<ac::nbits<imageWidth+1>::val, ac::nbits<imageWidth+1>::val, false>  maxW_f;
    typedef ac_fixed<ac::nbits<imageHeight+1>::val, ac::nbits<imageHeight+1>::val, false>  maxH_f;

    Hough_Algorithm_HW() {};

#pragma hls_design interface
//#pragma hls_design top
    void CCS_BLOCK(run)(ac_channel<pixelType> &data_in,
						maxW &widthIn,
						maxH &heightIn,
						ac_channel<maxW> &x1, ac_channel<maxH> &y1, 
						ac_channel<maxW> &x2, ac_channel<maxH> &y2) //points are pixelType because they take indeces from data_in
	{
        printf("run_0\n");

        houghTransform(data_in, widthIn, heightIn, acc);

        printf("run_1\n");

        getMaxLine(acc, widthIn, heightIn, x1, y1, x2, y2);

        printf("run_2\n");

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
        
        ac_int<19,false> acc_tmp_len = 400000; // megalytero apo to kanoniko afoy to thelei constant 

		pixelType acc_tmp[400000]; //must have constant value 

        angType cos_out;
        angType sin_out;
        printf("transform_0\n");
        // int count = 0;
        
        ac_fixed<49, 42, false> rho_len = ( ((1.41421356) * (imageHeight > imageWidth ? imageHeight : imageWidth)) / 2); //thelei prosoxi i diairesi kai o poll/smos
        for (uint19 i = 0; i < acc_tmp_len; i++){
            acc_tmp[i] = 0;
        }

		ac_fixed<8,1,false> DEG2RAD = 0.017453293;
        HROW: for (maxH y = 0; ; y++){
            //printf("y = %d\n", y);
            HCOL: for (maxW x = 0; ; x++){
                //printf("x = %d\n", x);
                // printf("data_in = %d\n", (int)(data_in[y * widthIn + x]));
                
                // if ( (data_in[(y * widthIn) + x]) > 250 ){ 
                pixelType din = data_in.read();
                if ( din > 250 ){ //Mipos xreiazetai na valo .to_uint() ????
                    // printf("in the if: %d\n", ++count); 
                    HACC: for (pixelType t = 0; t < 180; t++){
                        //printf("t = %d\n", t);
                        ac_fixed<16,9,true> in = (ac_fixed<16,9,true>)((ac_fixed<9,9,true>)t * DEG2RAD);
                        ac_math::ac_cos_cordic(in, cos_out);
                        ac_math::ac_sin_cordic(in, sin_out);
                        //cout << "in = " << in << endl;
                        r = ( (maxW_f)x - center_x) * cos_out;
                        r = r + ((maxH_f)y - center_y) * sin_out;
                        //if(t==150) 
						//	break;
						//cout << "r = " << r << endl;
                        //printf("r = %f\n", r);
                        // printf("r + rho_len = %f\n", r + rho_len);
                        // printf("index = %d\n", (int)((round(r + rho_len) * 180.0)) + t);
                        //cout << (unsigned int)((int)((round((r + rho_len).to_double()) * 180.0)) + t) << endl;
                        acc_tmp[(int)(((r + rho_len).to_double()) * 180.0) + t]++;
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
            acc.write(acc_tmp[i]);
        }
   
        printf("transform_1\n");
    }

#pragma hls_design
    void getMaxLine(ac_channel<pixelType> &acc, 
                        maxW &widthIn,
						maxH &heightIn,
						ac_channel<maxW> &x1, ac_channel<maxH> &y1, 
						ac_channel<maxW> &x2, ac_channel<maxH> &y2){
        printf("max_line_0\n");
        
        int threshold = (imageWidth > imageHeight) ? imageWidth/4 : imageHeight/4;
        maxW_f x1_t, x2_t;
        maxH_f y1_t, y2_t;

        angType cos_t;
        angType sin_t;
        //cout << "threshold = " << threshold << endl;
        ac_fixed<18,1,false> DEG2RAD = 0.017453293;
		//ac_fixed<12, 10, false> rho_len = ( ((ac_fixed<8, 1, false>)(1.41421356) * (ac_fixed<1,1,false>)(imageHeight > imageWidth ? imageHeight : imageWidth)) / 2); //thelei prosoxi i diairesi kai o poll/smos
		ac_fixed<22, 14, false> rho_len = ( (1.41421356 * (imageHeight > imageWidth ? imageHeight : imageWidth)) / 2); //thelei prosoxi i diairesi kai o poll/smos

		//cout << "rho_len = " << rho_len << endl;

        ac_fixed<23, 15, false> rho_len_acc = rho_len * 2;
		//cout << "rho_len_acc = " << rho_len_acc << endl;
        R_LINE: for (uint14 r = 0; r < rho_len_acc; r++){   
            T_LINE: for (pixelType t = 0; t < 180; t++){
                // if ((int)acc[ (r * theta_len_acc) + t ] >= threshold){
                pixelType acc_in = acc.read();
                //cout << "acc_in = " << acc_in << endl;
                if (acc_in >= 300){
                    ac_fixed<30,9,true> in = (ac_fixed<30,9,true>)((ac_fixed<9,9,true>)t * DEG2RAD);
                    ac_math::ac_cos_cordic(in, cos_t);
                    //cout << "cos_t = " << cos_t << endl;
                    ac_math::ac_sin_cordic(in, sin_t);
                    //cout << "sin_t = " << sin_t << endl;
                    
                    if (cos_t==0){
						cos_t = 0.0001;
					} else if (sin_t==0){
						sin_t = 0.0001;
					}
                    // cos_t = cos(t * DEG2RAD);
                    // sin_t = sin(t * DEG2RAD);
                    
                    if ( t>= 45 && t<=135){
                        // y = (r - x cos(t)) / sin(t)
                        x1_t = 0;
                        y1_t = ((r-(rho_len_acc/2)) - ((x1_t - (widthIn/2) ) * cos_t)) / sin_t + (heightIn / 2);
                        //cout << "x1_t = " << x1_t << ", y1_t = " << y1_t << endl;
                        x2_t = widthIn - 0;
                        y2_t = ((r-(rho_len_acc/2)) - ((x2_t - (widthIn/2) ) * cos_t)) / sin_t + (heightIn / 2);
                    } else {
                        // x = (r - y sin(t)) / cos(t);
                        y1_t = 0;
                        x1_t = ((r-(rho_len_acc/2)) - ((y1_t - (heightIn/2) ) * sin_t)) / cos_t + (widthIn / 2);
						//cout << "x1_t = " << x1_t << ", y1_t = " << y1_t << endl;

						y2_t = heightIn - 0;
						x2_t = ((r-(rho_len_acc/2)) - ((y2_t - (heightIn/2) ) * sin_t)) / cos_t + (widthIn / 2);
                    }
                }
                // printf("x1_t = %d, y1_t = %d, x2_t = %d, y2_t = %d\n", x1_t, y1_t, x2_t, y2_t);

            }
        }
        printf("max_line_1\n");
        // printf("x1_t = %d, y1_t = %d, x2_t = %d, y2_t = %d\n", x1_t, y1_t, x2_t, y2_t);

        x1.write(x1_t.to_uint());
        y1.write(y1_t.to_uint());
        x2.write(x2_t.to_uint());
        y2.write(y2_t.to_uint());
    }

};

#endif
