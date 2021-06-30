#ifndef _INCLUDED_HOUGH_H_
#define _INCLUDED_HOUGH_H_



#include <ac_fixed.h>
#include <ac_channel.h>
#include <ac_math/ac_sincos_cordic.h>


#include <mc_scverify.h>



template<int imageWidth, int imageHeight>
class Hough_Algorithm_HW{
    typedef uint8               pixelType;
    typedef ac_fixed<8,3,true>  angType;


    // static pixelType theta_len_acc = 180;    
    // static  sqrt_two = ;
    
    
    //static 
    
    // uint19 acc_tmp_len = rho_len_acc.to_uint() * theta_len_acc + 1; 
	
    ac_channel<pixelType> acc;
		
public:	


    typedef ac_int<ac::nbits<imageWidth+1>::val, false>  maxW;
    typedef ac_int<ac::nbits<imageHeight+1>::val, false> maxH;
    typedef ac_fixed<ac::nbits<imageWidth+1>::val, ac::nbits<imageWidth+1>::val, false>  maxW_f;
    typedef ac_fixed<ac::nbits<imageHeight+1>::val, ac::nbits<imageHeight+1>::val, false>  maxH_f;

    Hough_Algorithm_HW() {};

#pragma hls_design interface
    void CCS_BLOCK(run)(ac_channel<pixelType> &data_in,
						maxW &widthIn,
						maxH &heightIn,
						ac_channel<maxW> &x1, ac_channel<maxH> &y1, 
						ac_channel<maxW> &x2, ac_channel<maxH> &y2) //points are pixelType because they take indeces from data_in
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
        
        static ac_int<19,false> acc_tmp_len = 400000; // megalytero apo to kanoniko afoy to thelei constant 

		static pixelType acc_tmp[400000]; //must have constant value 

        angType cos_out;
        angType sin_out;
        // printf("transform_0\n");
        // int count = 0;
        
        static ac_fixed<12, 10, false> rho_len = ( ((ac_fixed<8, 1, false>)(1.41421356) * (ac_fixed<1,1,false>)(imageHeight > imageWidth ? imageHeight : imageWidth)) / 2); //thelei prosoxi i diairesi kai o poll/smos
        for (uint19 i = 0; i < acc_tmp_len; i++){
            acc_tmp[i] = 0;
        }

		ac_fixed<8,1,false> DEG2RAD = 0.017453293;
        HROW: for (maxH y = 0; ; y++){
            // printf("y = %d\n", y);
            HCOL: for (maxW x = 0; ; x++){
                // printf("x = %d\n", x);
                // printf("data_in = %d\n", (int)(data_in[y * widthIn + x]));
                
                // if ( (data_in[(y * widthIn) + x]) > 250 ){ 
                pixelType din = data_in.read();
                if ( din > 250 ){ //Mipos xreiazetai na valo .to_uint() ????
                    // printf("in the if: %d\n", ++count); 
                    HACC: for (pixelType t = 0; t < 180; t++){
                        // printf("t = %d\n", t);
                        ac_fixed<16,5,true> in = (ac_fixed<16,5,true>)((ac_fixed<9,9,true>)t * DEG2RAD);
                        ac_math::ac_cos_cordic(in, cos_out);
                        ac_math::ac_sin_cordic(in, sin_out);
                        r = ( (maxW_f)x - center_x) * cos_out;
                        r = r + ((maxH_f)y - center_y) * sin_out;

                        // printf("r = %f\n", r);
                        // printf("r + rho_len = %f\n", r + rho_len);
                        // printf("index = %d\n", (int)((round(r + rho_len) * 180.0)) + t);
                        
                        acc_tmp[(ac_int<19,false>) ((r + rho_len * 180) + t)]++;
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
   
        // printf("transform_1\n");
    }

#pragma hls_design
    void getMaxLine(ac_channel<pixelType> &acc, 
                        maxW &widthIn,
						maxH &heightIn,
						ac_channel<maxW> &x1, ac_channel<maxH> &y1, 
						ac_channel<maxW> &x2, ac_channel<maxH> &y2){
        // printf("max_line_0\n");
        maxW threshold;
        if(widthIn > heightIn){
            threshold = widthIn/4;
            // maxW threshold = widthIn/4;
        } else {
            threshold = heightIn/4;    
            // maxH threshold = heightIn/4;
        }
        maxW_f x1_t, x2_t;
        maxH_f y1_t, y2_t;

        angType cos_t;
        angType sin_t;
        // printf("threshold = %d\n", threshold);
        ac_fixed<8,1,false> DEG2RAD = 0.017453293;
		static ac_fixed<12, 10, false> rho_len = ( ((ac_fixed<8, 1, false>)(1.41421356) * (ac_fixed<1,1,false>)(imageHeight > imageWidth ? imageHeight : imageWidth)) / 2); //thelei prosoxi i diairesi kai o poll/smos

        static ac_fixed<19, 14, false> rho_len_acc = rho_len * (ac_fixed<2,2,false>)(2);

        R_LINE: for (uint14 r = 0; r < rho_len_acc; r++){   
            T_LINE: for (pixelType t = 0; t < 180; t++){
                // if ((int)acc[ (r * theta_len_acc) + t ] >= threshold){
                pixelType acc_in = acc.read();
                if (acc_in >= threshold){
                    
                    ac_math::ac_cos_cordic((ac_fixed<9,9,true>)t * DEG2RAD, cos_t);
                    ac_math::ac_sin_cordic((ac_fixed<9,9,true>)t * DEG2RAD, sin_t);
                    // cos_t = cos(t * DEG2RAD);
                    // sin_t = sin(t * DEG2RAD);
                    
                    if ( t>= 45 && t<=135){
                        // y = (r - x cos(t)) / sin(t)
                        x1_t = 0;
                        y1_t = ((r-(rho_len_acc/2)) - ((x1_t - (widthIn/2) ) * cos_t)) / sin_t + (heightIn / 2);
                        x2_t = widthIn - 0;
                        y2_t = ((r-(rho_len_acc/2)) - ((x2_t - (widthIn/2) ) * cos_t)) / sin_t + (heightIn / 2);
                    } else {
                        // x = (r - y sin(t)) / cos(t);
                        y1_t = 0;
                        x1_t = ((r-(rho_len_acc/2)) - ((y1_t - (heightIn/2) ) * sin_t)) / cos_t + (widthIn / 2);
						y2_t = heightIn - 0;
						x2_t = ((r-(rho_len_acc/2)) - ((y2_t - (heightIn/2) ) * sin_t)) / cos_t + (widthIn / 2);
                    }
                }
                // printf("x1_t = %d, y1_t = %d, x2_t = %d, y2_t = %d\n", x1_t, y1_t, x2_t, y2_t);

            }
        }
        // printf("max_line_1\n");
        // printf("x1_t = %d, y1_t = %d, x2_t = %d, y2_t = %d\n", x1_t, y1_t, x2_t, y2_t);

        x1.write(x1_t.to_uint());
        y1.write(y1_t.to_uint());
        x2.write(x2_t.to_uint());
        y2.write(y2_t.to_uint());
    }

};

#endif
