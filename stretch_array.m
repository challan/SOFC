clear all
clc
sizeArr=[704,324,360];
nx=sizeArr(1);
ny=sizeArr(2);
nz=sizeArr(3);
% Read the compressed microstructral data
fname = ['data/AIST_LSCF_0h_704_324_360.dat'];
fid = fopen(fname);
skip = fread(fid,1,'int32');
a = fread(fid,nx*ny*nz,'double');
fclose(fid);
phi = reshape(a, [nx ny nz]);
clear a;

% Stretch the microstructural data in z-direction
[ty tx tz]=...
  ndgrid(linspace(1,size(phi,1),nx),linspace(1,size(phi,2),ny),...
         linspace(1,size(phi,3),630));
%size(ty)
phi_stretched=interp3(phi,tx,ty,tz);

% Write out the file
fname = ['data/AIST_LSCF_0h_704_324_630.dat'];
fileID = fopen(fname,'wb');
fwrite(fileID,phi_stretched,'double');
fclose(fileID);
size(phi)
size(phi_stretched)
