trainImage = fopen("hw9data/test-labels", 'r', 'b');
magicNumber = fread(trainImage, 1, 'int32');
nImages = fread(trainImage, 1, 'int32');
data = zeros(nImages, 1);
for i=1:nImages
    img = fread(trainImage, 1, 'uchar');
    data(i,:) = transpose(img);
end
csvwrite("test_label.csv", data);

% img2 = zeros(28, 28);
% for i=1:28
%     img2(i,:) = img((i-1)*28+1:i*28);
% end
% img2 = img2./255;
% imshow(img2)
