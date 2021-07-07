# Ηough
Hough Transform in C for Catapult HLS

C++ Code is synthesized in Catapult and the algorithm works correctly. 
Naive pipelining implemented, maybe it can be done better.

# Run the algorithm
bash run.sh -c true

For the catapult version and for the simple c++ algorithm:

bash run.sh -c false

For the algorithm to work correctly you need to perform Canny Edge Detection in the image first. I do that with Matlab and I specify on which image I perform the algorithm in run.sh. 
