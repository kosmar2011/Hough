#ifndef _INCLUDED_HOUGH_H_
#define _INCLUDED_HOUGH_H_

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

void plotLineLow(unsigned char* data_in, int x1, int y1, int x2, int y2, int width);
void plotLineHigh(unsigned char* data_in, int x1, int y1, int x2, int y2, int width);
void plotLine(unsigned char* data_in, int x1, int y1, int x2, int y2, int width);

class Hough_Algorithm{

    int imageWidth  = 1296;
    int imageHeight =  864;
    int theta_len_acc = 180;
    const double rho_len = ( (sqrt(2.0) * (double)(imageHeight > imageWidth ? imageHeight : imageWidth)) / 2.0);

    const double rho_len_acc = rho_len * 2.0;
    const double DEG2RAD = 0.017453293;

    public:

    // Constructor
    Hough_Algorithm() {}

    void run(unsigned char *data_in, int &x1, int &y1, int &x2, int &y2){
        unsigned int *acc = (unsigned int*)calloc(rho_len_acc * theta_len_acc, sizeof(unsigned int));
        // printf("run_0\n");
        // printf("rho_len_acc * theta_len_acc = %f\n", rho_len_acc * theta_len_acc);

        houghTransform(data_in, acc);
        // printf("run_1\n");
        getMaxLine(acc, x1, y1, x2, y2);
        // printf("run_2\n");

        free(acc);
    }

    void houghTransform(unsigned char *data_in, unsigned int *acc){
        
        double center_x = imageWidth / 2;
		double center_y = imageHeight / 2;
        double r = 0;
        // printf("transform_0\n");
        int count = 0;

        for (int y = 0; y < imageHeight; y++){
            // printf("y = %d\n", y);
            for (int x = 0; x < imageWidth; x++){
                // printf("x = %d\n", x);
                // printf("data_in = %d\n", (int)(data_in[y * imageWidth + x]));
                if ((int)(data_in[(y * imageWidth) + x]) > 200){
                    // printf("in the if: %d\n", ++count); //to proto pou ftanei edo error
                    for (int t = 0; t < 180; t++){
                        // printf("t = %d\n", t);
                        r = ( ((double)x - center_x) * cos((double)t * DEG2RAD)) + (((double)y - center_y) * sin((double)t * DEG2RAD));
                        // printf("r = %f\n", r);
                        // printf("r + rho_len = %f\n", r + rho_len);
                        // printf("index = %d\n", (int)((round(r + rho_len) * 180.0)) + t);
						acc[ (int)((round(r + rho_len) * 180.0)) + t]++;
                        // printf("%d\n", ++count);
                    }
                    // printf("in the if after loop\n");
                }
            }
        }
        // printf("transform_1\n");
    }

    void getMaxLine(unsigned int *acc, int &x1, int &y1, int &x2, int &y2){
        // printf("max_line_0\n");
        int max = 0;
        for (int r = 0, i = 0; r < rho_len_acc; r++){   
            for (int t = 0; t < theta_len_acc; t++){
                if ((int)acc[ (r * theta_len_acc) + t ] >= max){
                    max = (int)acc[ (r * theta_len_acc) + t ];
                    // int x1 = 0, x2 = 0, y1 = 0, y2 = 0;
                    if ( t>= 45 && t<=135){
                        // y = (r - x cos(t)) / sin(t)
                        // x1 = 0;
						y1 = ((double)(r-(rho_len_acc/2)) - ((x1 - (imageWidth/2) ) * cos(t * DEG2RAD))) / sin(t * DEG2RAD) + (imageHeight / 2);
                        x2 = imageWidth - 0;
                        y2 = ((double)(r-(rho_len_acc/2)) - ((x2 - (imageWidth/2) ) * cos(t * DEG2RAD))) / sin(t * DEG2RAD) + (imageHeight / 2);
                    } else {
                        // x = (r - y sin(t)) / cos(t);
                        // y1 = 0;
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

#endif