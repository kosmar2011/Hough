#ifndef _INCLUDED_HOUGH_H_
#define _INCLUDED_HOUGH_H_

class Hough_Algorithm_Alg{

    const int imageWidth  = 1296;
    int imageHeight =  864;
    int theta_len_acc = 180;
    const double rho_len = ( (sqrt(2.0) * (double)(imageHeight > imageWidth ? imageHeight : imageWidth)) / 2.0);

    const double rho_len_acc = rho_len * 2.0;
    const double DEG2RAD = 0.017453293;

public:

    // Constructor
    Hough_Algorithm_Alg() {};

    void run(unsigned char *data_in, int &x1, int &y1, int &x2, int &y2){
        unsigned int *acc = (unsigned int*)calloc(rho_len_acc * theta_len_acc, sizeof(unsigned int));
        // printf("run_0\n");
        // printf("rho_len_acc * theta_len_acc = %f\n", rho_len_acc * theta_len_acc);
        printf("rho_len_acc = %f\n", rho_len_acc);

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
                if ((int)(data_in[(y * imageWidth) + x]) > 250){
                    // printf("in the if: %d\n", ++count); 
                    for (int t = 0; t < 180; t++){
                        // printf("t = %d\n", t);
                        r = ( ((double)x - center_x) * cos((double)t * DEG2RAD)) + (((double)y - center_y) * sin((double)t * DEG2RAD));
                        // printf("r = %f\n", r);
                        // printf("r + rho_len = %f\n", r + rho_len);
                        // printf("index = %d\n", (int)((round(r + rho_len) * 180.0)) + t);
                        int idx = (int)((round(r + rho_len) * 180.0)) + t;
                        acc[idx]++;
                        // printf("%d\n", ++count);
                    }
                    // printf("in the if after loop\n");
                }
            }
        }
        return;
        // printf("transform_1\n");
    }

    void getMaxLine(unsigned int *acc, int &x1, int &y1, int &x2, int &y2){
        // printf("max_line_0\n");
        int threshold = imageWidth > imageHeight ? imageWidth/4 : imageHeight/4;
        printf("threshold = %d\n", threshold);

        for (int r = 0, i = 0; r < rho_len_acc; r++){   
            for (int t = 0; t < theta_len_acc; t++){
                if ((int)acc[ (r * theta_len_acc) + t ] >= threshold){

                    if ( t>= 45 && t<=135){
                        // y = (r - x cos(t)) / sin(t)
                        x1 = 0;
                        printf("r-(rho_len_acc/2) = %f\n", (double)(r-(rho_len_acc/2)));
                        y1 = ((double)(r-(rho_len_acc/2)) - ((x1 - (imageWidth/2) ) * cos(t * DEG2RAD))) / sin(t * DEG2RAD) + (imageHeight / 2);
                        x2 = imageWidth - 0;
                        y2 = ((double)(r-(rho_len_acc/2)) - ((x2 - (imageWidth/2) ) * cos(t * DEG2RAD))) / sin(t * DEG2RAD) + (imageHeight / 2);
                    } else {
                        // x = (r - y sin(t)) / cos(t);
                        y1 = 0;
                        printf("r-(rho_len_acc/2) = %f\n", (double)(r-(rho_len_acc/2)));
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


#endif
