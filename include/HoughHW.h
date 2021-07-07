#ifndef _INCLUDED_HOUGH_H_HW
#define _INCLUDED_HOUGH_H_HW


#include <cmath>
#include <ac_fixed.h>
#include <ac_channel.h>
#include <ac_math/ac_sincos_cordic.h>
#include <ac_math/ac_div.h>


#include <mc_scverify.h>



template<int imageWidth, int imageHeight>
class Hough_Algorithm_HW{
    typedef uint8               pixelType;
    typedef uint16              pixelType2x;
    typedef ac_fixed<27,3,true>  angType;
    typedef ac_fixed<20,15,true> divType;
	
    ac_channel<pixelType2x> acc;
		
public:	


    typedef ac_int<ac::nbits<imageWidth+1>::val, false>  maxW;
    typedef ac_int<ac::nbits<imageHeight+1>::val, false> maxH;
    typedef ac_fixed<ac::nbits<imageWidth+1>::val, ac::nbits<imageWidth+1>::val>  			maxW_f;  //evgala ta false 
    typedef ac_fixed<ac::nbits<imageHeight+1>::val, ac::nbits<imageHeight+1>::val>  			maxH_f;
    typedef ac_fixed<ac::nbits<imageWidth+1>::val + 16, ac::nbits<imageWidth+1>::val + 1>  	maxW_f_x2;
	typedef ac_fixed<ac::nbits<imageHeight+1>::val + 16, ac::nbits<imageHeight+1>::val + 1>  	maxH_f_x2;
	
	
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
                        ac_channel<pixelType2x> &acc){
        

        maxW_f_x2 center_x = widthIn  / 2;
		maxW_f_x2 center_y = heightIn / 2;
        ac_fixed<26, 12, true> r = 0;
        
        printf("transform_0\n");
        int count = 0;
        
        maxW_f rho_len = ( ((1.41421356) * (imageHeight > imageWidth ? imageHeight : imageWidth)) / 2); //thelei prosoxi i diairesi kai o poll/smos
        maxW_f_x2 rho_len_acc = rho_len * 2;
		
		int acc_tmp_len = 400000; 
		pixelType2x acc_tmp[400000]; //must have constant value 
		
		for (int i = 0; i < acc_tmp_len; i++){
            acc_tmp[i] = 0;
        }
        
        ac_fixed<26,1,false> DEG2RAD = 0.017453293;

        angType cos_out[180];
        angType sin_out[180];
        for (int t = 0; t < 180; t++){
			ac_math::ac_sincos_cordic((ac_fixed<26,9>)t* DEG2RAD, (angType)1, sin_out[t], cos_out[t]);
		}

        HROW: for (maxH y = 0; ; y++){
            printf("y = %d\n", y);
            HCOL: for (maxW x = 0; ; x++){
                //printf("x = %d\n", x);
                // printf("data_in = %d\n", (int)(data_in[y * widthIn + x]));
                
                // if ( (data_in[(y * widthIn) + x]) > 250 ){ 
                pixelType din = data_in.read();
                if ( din.to_uint() > 250 ){ 
                    // printf("in the if: %d\n", ++count); 
                    HACC: for (pixelType t = 0; t < 180; t++){
                        //printf("t = %d\n", t);
                        //ac_fixed<16,9,true> in = (ac_fixed<9,9>)t * DEG2RAD;
                        //ac_math::ac_cos_cordic((ac_fixed<27,9>)t* DEG2RAD, cos_out);
                        //ac_math::ac_sin_cordic((ac_fixed<27,9>)t* DEG2RAD, sin_out);
                        
                        //cout << "in = " << in << endl;
                        r = ( (maxW_f_x2)x - center_x) * cos_out[t] + ((maxH_f_x2)y - center_y) * sin_out[t];
                        //if(t==150) 
						//	break;
						//cout << "r = " << r << endl;
                        //printf("r = %f\n", r);
                        // printf("r + rho_len = %f\n", r + rho_len);
                        // printf("index = %d\n", (int)((round(r + rho_len) * 180.0)) + t);
                        //cout << (unsigned int)((int)((round((r + rho_len).to_double()) * 180.0)) + t) << endl;
                        int round_r = (int)((r + rho_len).to_double());
                        int round_r_mult = (int)(round_r * 180);
                        int idx = round_r_mult + t.to_uint();

                        acc_tmp[idx]++;
                        //printf("count is = d\n", count);
                        //count+=1;
                    }
                }
                if (x == maxW(widthIn-1)) 
                    break;
            }
            if (y == maxH(heightIn-1)) 
                break;
        }

        WRITE: for (int i = 0; i < acc_tmp_len; i++){
            acc.write(acc_tmp[i]);
        }
   
        printf("transform_1\n");
    }

#pragma hls_design
    void getMaxLine(ac_channel<pixelType2x> &acc, 
                        maxW &widthIn,
						maxH &heightIn,
						ac_channel<maxW> &x1, ac_channel<maxH> &y1, 
						ac_channel<maxW> &x2, ac_channel<maxH> &y2){
        printf("max_line_0\n");
        
        ac_fixed<24,16,false> threshold = (imageWidth > imageHeight) ? imageWidth/4 : imageHeight/4;
        maxW_f_x2 x1_t, x2_t;
        maxH_f_x2 y1_t, y2_t;

        //cout << "threshold = " << threshold << endl;
		//ac_fixed<12, 10, false> rho_len = ( ((ac_fixed<8, 1, false>)(1.41421356) * (ac_fixed<1,1,false>)(imageHeight > imageWidth ? imageHeight : imageWidth)) / 2); //thelei prosoxi i diairesi kai o poll/smos
		maxW_f rho_len = (1.41421356 * (imageHeight > imageWidth ? imageHeight : imageWidth)) / 2; //thelei prosoxi i diairesi kai o poll/smos

		//cout << "rho_len = " << rho_len << endl;
        ac_fixed<26,1,false> DEG2RAD = 0.017453293;

		angType cos_out[180];
        angType sin_out[180];
        for (int t = 0; t < 180; t++){
			ac_math::ac_sincos_cordic((ac_fixed<26,9>)t* DEG2RAD, (angType)1, sin_out[t], cos_out[t]);
		}
		
        maxW_f_x2 rho_len_acc = rho_len * (ac_fixed<2,2,false>)2;
		//cout << "rho_len_acc = " << rho_len_acc << endl;
        R_LINE: for (int r = 0; r < rho_len_acc; r++){   
            T_LINE: for (int t = 0; t < 180; t++){
                // if ((int)acc[ (r * theta_len_acc) + t ] >= threshold){
                pixelType2x acc_in = acc.read();
                cout << "acc_in = " << acc_in << endl;
                if (acc_in >= threshold){
					threshold = acc_in;
                    //ac_fixed<15,9,true> in = (ac_fixed<9,9,false>)t; //* DEG2RAD;
                    //ac_fixed<16,9,true> in = (ac_fixed<9,9>)t * DEG2RAD;

                    //ac_math::ac_cos_cordic((ac_fixed<9,9>)t* DEG2RAD, cos_t);
                    //cout << "cos_t = " << cos_t << endl;
                    //ac_math::ac_sin_cordic((ac_fixed<9,9>)t* DEG2RAD, sin_t);
                    //cout << "sin_t = " << sin_t << endl;
                    //ac_math::ac_sincos_cordic((ac_fixed<9,9>)t* DEG2RAD, (angType)1, sin_out[t], cos_out[t]);

                    /*if (cos_t==0){
						cos_t = (angType)0.000001;
						cout << "t = " << t << endl;
						cout << "CHANGED TO --> cos_t = " << cos_t << endl;
					} else if (sin_t==0){
						sin_t = (angType)0.000001;
						cout << "t = " << t << endl;
						cout << "CHANGED TO --> sin_t = " << sin_t << endl;
					}*/
                    // cos_t = cos(t * DEG2RAD);
                    // sin_t = sin(t * DEG2RAD);
                    
                    if ( t>= 45 && t<=135){
                        // y = (r - x cos(t)) / sin(t)
                        x1_t = 0;
                        divType dividend1 = (r-(rho_len_acc/2) - ((x1_t - (imageWidth/2) ) * cos_out[t]));
                        divType result1;
                        ac_math::ac_div(dividend1, sin_out[t], result1);
                        y1_t =  result1  + (imageHeight / 2);
                        cout << "x1_t = " << x1_t << ", y1_t = " << y1_t << endl;                        
                        
                        x2_t = imageWidth - 0;
                        divType dividend2 = (r-(rho_len_acc/2) - ((x2_t - (imageWidth/2) ) * cos_out[t]));
                        divType result2;
                        ac_math::ac_div(dividend2, sin_out[t], result2);
                        y2_t = result2 + (imageHeight / 2);
                    } else {
                        // x = (r - y sin(t)) / cos(t);
                        y1_t = 0;
                        divType dividend1 = (r-(rho_len_acc/2) - ((y1_t - (imageHeight/2) ) * sin_out[t]));
                        divType result1;                        
                        ac_math::ac_div(dividend1, cos_out[t], result1);
                        x1_t =  result1  + (imageWidth / 2);
						cout << "x1_t = " << x1_t << ", y1_t = " << y1_t << endl;
                        
						y2_t = imageHeight - 0;
						divType dividend2 = (r-(rho_len_acc/2) - ((y2_t - (imageHeight/2) ) * sin_out[t]));
                        divType result2;                        
                        ac_math::ac_div(dividend2, cos_out[t], result2);
						x2_t = result2 + (imageWidth / 2);
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
