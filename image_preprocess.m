clear;
clc;

I = imread('people_gray.bmp');
I_c = uint64(edge(I(:,:,1),'Canny'));
Image3D = zeros(size(I,1), size(I,2), 3);
for i=1:3
    Image3D(:,:,i) = I_c;
    size(Image3D)
end

figure; imshow(Image3D);
imwrite(Image3D, 'canny.bmp');

