;███╗   ███╗ █████╗ ██╗███╗   ██╗    ██████╗ ██████╗  ██████╗  ██████╗ ██████╗  █████╗ ███╗   ███╗
;████╗ ████║██╔══██╗██║████╗  ██║    ██╔══██╗██╔══██╗██╔═══██╗██╔════╝ ██╔══██╗██╔══██╗████╗ ████║
;██╔████╔██║███████║██║██╔██╗ ██║    ██████╔╝██████╔╝██║   ██║██║  ███╗██████╔╝███████║██╔████╔██║
;██║╚██╔╝██║██╔══██║██║██║╚██╗██║    ██╔═══╝ ██╔══██╗██║   ██║██║   ██║██╔══██╗██╔══██║██║╚██╔╝██║
;██║ ╚═╝ ██║██║  ██║██║██║ ╚████║    ██║     ██║  ██║╚██████╔╝╚██████╔╝██║  ██║██║  ██║██║ ╚═╝ ██║
;╚═╝     ╚═╝╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝    ╚═╝     ╚═╝  ╚═╝ ╚═════╝  ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝
PRO reconstruct_3D
;; Nov 6th 2015 This code is to reconstruct the 3D microstructure by reading in the 2D grayscale images

;; The entire dimension of the AIST_LSCF_300h sample is [871,481,358]. Since I have to evolve the structure using a source code
;; that is MPI based, the dimension of the 3D volume array must be divisible by 16x8x6. As such, I will truncate parts of the data and
;; only read in [800,400,300]

sizeArr=[694,302,360]
nx=sizeArr[0]
ny=sizeArr[1]
nz=sizeArr[2]

phi=DBLARR(nx,ny,nz)


for i=32,391 do begin
  filename='~/Desktop/SOFC/AIST_LSCF/control/reconstruction/Rotated Cropped Images/AIST_LSCF_0h_'+string(i,format='(I3.3)')+'.tif'
  print,filename
  image1 = READ_IMAGE(filename)
  print,size(image,/dimension)
  slice=i-32
  phi(*,*,slice)=image1(0:nx-1,0:ny-1)
endfor

  print,'Min & Max of the Grayscale Data : ',min(phi),max(phi)
  filename='data/AIST_LSCF_0h_694_302_360_bf_thresh.dat'
  print,filename
  writeForData, phi, filename 

	slice=dblarr(nx,ny)
	slice = phi[*,*,0]

  slice=abs(fft(slice,-1))	
	p = IMAGE(BYTSCL(slice))
	stop
	
  pore_phase=where(phi lt 100)
  LSCF_phase=where(phi gt 150)
  phi[pore_phase]=100.
  phi[LSCF_phase]=150.
  phi=(phi-100.)/50.

  ;print,'Min & Max of the Thresheld Data : ',min(phi),max(phi)
  ;filename='data/AIST_LSCF_0h_704_324_360.dat'
  ;print,filename
  ;writeForData, phi, filename 
  
END
;██████╗ ███████╗ █████╗ ██████╗     ██╗███╗   ██╗██████╗ ██╗   ██╗████████╗
;██╔══██╗██╔════╝██╔══██╗██╔══██╗    ██║████╗  ██║██╔══██╗██║   ██║╚══██╔══╝
;██████╔╝█████╗  ███████║██║  ██║    ██║██╔██╗ ██║██████╔╝██║   ██║   ██║
;██╔══██╗██╔══╝  ██╔══██║██║  ██║    ██║██║╚██╗██║██╔═══╝ ██║   ██║   ██║
;██║  ██║███████╗██║  ██║██████╔╝    ██║██║ ╚████║██║     ╚██████╔╝   ██║
;╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚═════╝     ╚═╝╚═╝  ╚═══╝╚═╝      ╚═════╝    ╚═╝
PRO readForData,filename,size, data
  data = DBLARR(size[0],size[1],size[2])
  OPENR, lun, filename, /GET_LUN, /F77_UNFORMATTED
  READU, lun, data
  FREE_LUN, lun
END
;██╗    ██╗██████╗ ██╗████████╗███████╗     ██████╗ ██╗   ██╗████████╗██████╗ ██╗   ██╗████████╗
;██║    ██║██╔══██╗██║╚══██╔══╝██╔════╝    ██╔═══██╗██║   ██║╚══██╔══╝██╔══██╗██║   ██║╚══██╔══╝
;██║ █╗ ██║██████╔╝██║   ██║   █████╗      ██║   ██║██║   ██║   ██║   ██████╔╝██║   ██║   ██║   
;██║███╗██║██╔══██╗██║   ██║   ██╔══╝      ██║   ██║██║   ██║   ██║   ██╔═══╝ ██║   ██║   ██║   
;╚███╔███╔╝██║  ██║██║   ██║   ███████╗    ╚██████╔╝╚██████╔╝   ██║   ██║     ╚██████╔╝   ██║   
; ╚══╝╚══╝ ╚═╝  ╚═╝╚═╝   ╚═╝   ╚══════╝     ╚═════╝  ╚═════╝    ╚═╝   ╚═╝      ╚═════╝    ╚═╝   
;
PRO writeForData, data, filename
  OPENW, lun,filename, /GET_LUN, /F77_UNFORMATTED;, /SWAP_ENDIAN
  WRITEU, lun, data
  FREE_LUN, lun
END