;██████╗ ████████╗███████╗██╗   ██╗    ███╗   ███╗███████╗████████╗██╗  ██╗ ██████╗ ██████╗
;██╔═══██╗╚══██╔══╝██╔════╝██║   ██║    ████╗ ████║██╔════╝╚══██╔══╝██║  ██║██╔═══██╗██╔══██╗
;██║   ██║   ██║   ███████╗██║   ██║    ██╔████╔██║█████╗     ██║   ███████║██║   ██║██║  ██║
;██║   ██║   ██║   ╚════██║██║   ██║    ██║╚██╔╝██║██╔══╝     ██║   ██╔══██║██║   ██║██║  ██║
;╚██████╔╝   ██║   ███████║╚██████╔╝    ██║ ╚═╝ ██║███████╗   ██║   ██║  ██║╚██████╔╝██████╔╝
;╚═════╝    ╚═╝   ╚══════╝ ╚═════╝     ╚═╝     ╚═╝╚══════╝   ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚═════╝
PRO OTSU_Method

  mainDir='~/Desktop/SOFC/AIST_LSCF/control/reconstruction/'  
  
 
  restore,mainDir+'data/AIST_LSCF_0h_694_302_360_Stage1_2DFFT_GrayScale_170111.sav';phi

;  wh_under=where(phi lt 0)
;  phi[wh_under]=0.d0
;  wh_over=where(phi gt 255)
;  phi[wh_over]=255.d0

  pdf1 = HISTOGRAM(phi,min=0,max=255)
  pdf1=double(pdf1)
  Find_threshold,pdf1,threshold1
  print,'threshold = ',threshold1
  
  
  restore,mainDir+'data/AIST_LSCF_0h_694_302_630_Stage1_2DFFT_GrayScale_170111.sav';phi
  
;  wh_under=where(phi lt 0)
;  phi[wh_under]=0.d0
;  wh_over=where(phi gt 255)
;  phi[wh_over]=255.d0
  
  pdf2 = HISTOGRAM(magnifiedphi,min=0,max=255)
  pdf2=double(pdf2)
  Find_threshold,pdf2,threshold1
  print,'threshold = ',threshold1
 
 
  filename=mainDir+'data/AIST_LSCF_0h_694_302_360_vs_630_Stage1_2DFFT_GrayScale_170111.eps'
  makeplot, filename, pdf1, pdf2
 stop

  sizeArr1=[694,302,360]
  nx=sizeArr1[0]
  ny=sizeArr1[1]
  nz=sizeArr1[2]
  
  phi=dblarr(nx,ny,nz)


  for i=32,391 do begin
  filename='~/Desktop/SOFC/AIST_LSCF/control/Rotated Cropped Images/AIST_LSCF_0h_'+string(i,format='(I3.3)')+'.tif'
  print,filename
  image1 = READ_IMAGE(filename)
  phi_image_gray=dblarr(nx,ny)
  phi_image_gray(*,*)=image1(0:nx-1,0:ny-1)
  print,min(phi_image_gray),max(phi_image_gray)
 
  pdf1 = HISTOGRAM(phi_image_gray,min=0,max=255)
  pdf1=double(pdf1)
  Find_threshold,pdf1,threshold1
  phi_image_gray=phi_image_gray*(150.d0/threshold1[0])
  pdf2 = HISTOGRAM(phi_image_gray,min=0,max=255)
  pdf2=double(pdf2)
  Find_threshold,pdf2,threshold2
  
  string='Threshold value changes from '+STRCOMPRESS(threshold1, /REMOVE_ALL)+' to '+STRCOMPRESS(threshold2[0], /REMOVE_ALL)
  print,string
  
  wh=where(phi_image_gray gt 255)
  phi_image_gray[wh]=255
  wh=where(phi_image_gray lt 100)
  phi_image_gray[wh]=100

  filename='~/Desktop/SOFC/AIST_LSCF/control/Rotated Cropped Normalized Images/AIST_LSCF_0h_'+string(i,format='(I3.3)')+'.tif'
  print,filename  
  WRITE_TIFF, filename, (phi_image_gray) 

  filename='~/Desktop/SOFC/AIST_LSCF/control/Rotated Cropped Normalized Images/histograms/AIST_LSCF_0h_'+string(i,format='(I3.3)')+'_histograms.eps'
  makeplot, filename, pdf1, pdf2

  phi(*,*,i-32)=phi_image_gray(*,*)
  endfor


  print,min(phi),max(phi)
  
  mainDir='~/Desktop/SOFC/AIST_LSCF/control/reconstruction/'
  filename=mainDir+''
  save,phi,filename=mainDir+'data/AIST_LSCF_0h_694_302_360_Stage1_160909.sav'

  
END
PRO Find_threshold,pdf, threshold

  dx=1
  p=pdf/total(pdf)/dx
  size_pdf=size(p,/dimensions)
  size_pdf=size_pdf[0]
  xaxis=dblarr(size_pdf)
  sigma_array=dblarr(size_pdf)
  sigma_array[*]=0.d0
for t=0,size_pdf-1 do begin  
  xaxis[t]=t
endfor
  
for t=0,size_pdf-2 do begin
  w1=total(p[0:t])
  w2=total(p[t+1:size_pdf-1])
  mu1=0.d0
  mu2=0.d0
  if w1*w2 gt 0 then begin
    mu1=total(p[0:t]*xaxis[0:t])/w1
    mu2=total(p[t+1:size_pdf-1]*xaxis[t+1:size_pdf-1])/w2
  endif
  sigma_array[t]=w1*w2*(mu1-mu2)^2.0
  ;print,t,w1,mu1,w2,mu2
endfor
  threshold=where(sigma_array eq max(sigma_array))
  sorted_sigma=sort(sigma_array)
  ;print,'Ideal Threshold Value is = ' ,threshold;
  ;if there are two maxima
  ;print,'Ideal Threshold Value is = ' ,(sorted_sigma[254]+sorted_sigma[255])/2.
END
PRO makeplot, filename, pdf1, pdf2

;If you want to save as .eps
!p.font=0
set_plot, 'ps'
Device, filename=filename, preview=2, /tt_font, /encapsulated, /color, bits_per_pixel=8, $
  xsize=6, ysize=6, /inches, pre_depth=8, pre_xsize=5, pre_ysize=5, /TIMES

LoadCT, 39, Ncolors=levels, Bottom=bottom
Plot, pdf2, $
  Color=[0], background=255,$
  Title='Histogram ', $
  XTitle='Pixel Value', $
  YTitle='Frequency', $
  XRange=[0,280],CHARSIZE=1.5,THICK=1;$    Position=[0.125, 0.125, 0.9, 0.925], $

  OPLOT,pdf1,color=[25],THICK=5
  OPLOT,pdf2,color=[250],THICK=5
  OPLOT,[90,90],[0,max(pdf2)],Linestyle=0,THICK=5,Color=[25]   
;  OPLOT,[128,128],[0,120000],Linestyle=0,THICK=5,Color=[250]
  DEVICE, /SYMBOL, FONT_INDEX=9
  DEVICE, /TIMES, FONT_INDEX=4
  
  background_color=[255]
  Lines=[0,0]
  thick=[5,5]
  items=['[694,302,360]','[694,302,630]']
  colors=[25,250]
  ; Location of legend in data coordinates.
  yloc = (!Y.CRange[1] - !Y.CRange[0]) * 0.97 + !Y.CRange[0]
  xloc = (!X.CRange[1] - !X.CRange[0]) * 0.5 + !X.CRange[0]

  ; Add the legend.
  Al_Legend, items, Lines=lines,Color=colors, Background_color=background_color, $
    thick=thick,Position=[xloc,yloc];,charsize=1.3  
  
  ;XYOUTS,0.53,0.87,'!9'+String(172B)+'!4 Desired Threshold Value = 125',/NORMAL 
device,/close
set_plot,'x'
END
PRO readForData,filename,size, data
  data = DBLARR(size[0],size[1],size[2])
  OPENR, lun, filename, /GET_LUN, /F77_UNFORMATTED
  READU, lun, data
  FREE_LUN, lun
END