
PRO FFT_filtering_2D

  sizeArr=[694,302,360]
  nx=sizeArr[0]
  ny=sizeArr[1]
  nz=sizeArr[2]

  mainDir='~/Desktop/SOFC/AIST_LSCF/control/reconstruction/'

  restore,mainDir+'data/AIST_LSCF_0h_694_302_630_Stage1_2DFFT_GrayScale_170111.sav';magnifiedphi
 
  writeFortranData, magnifiedphi, '~/Desktop/SOFC/AIST_LSCF/control/reconstruction/data/AIST_LSCF_0h_694_302_630_Stage1_2DFFT_Grayscale_170111.dat'

    phi=magnifiedphi
 
;    pore_wh=where(phi lt 90)
;    LSCF_wh=where(phi ge 90)
;    
;    phi[pore_wh]=0.d0
;    phi[LSCF_wh]=1.d0
 
    pore_wh=where(phi lt 40)
    pore_int_wh=where((phi ge 40) and (phi lt 90))
    LSCF_wh=where(phi gt 140)
    LSCF_int_wh=where((phi le 140) and (phi ge 90))
  
    phi[pore_wh]=0.d0
    phi[pore_int_wh]= (phi[pore_int_wh]-40.d0)/100.d0
    phi[LSCF_wh]=1.d0
    phi[LSCF_int_wh]= (phi[LSCF_int_wh]-40.d0)/100.d0

    writeFortranData, phi, '~/Desktop/SOFC/AIST_LSCF/control/reconstruction/data/AIST_LSCF_0h_694_302_630_Stage1_2DFFT_OP_170111.dat'
    print,'done writing'
stop

  phi=dblarr(nx,ny,nz)
  z=32
  for z=32,391 do begin
    filename='~/Desktop/SOFC/AIST_LSCF/control/Rotated Cropped Normalized Images/AIST_LSCF_0h_'+string(z,format='(I3.3)')+'.tif'
    print,filename
    slice = READ_IMAGE(filename)

    slice=(slice-100.d0)*255d0/155.d0
    slice_fft=fft(slice,-1, /CENTER)
    logpower = ALOG10(ABS(slice_fft)^2)



    Re=real_part(slice_fft)
    Im=imaginary(slice_fft)
    y_indices=FLTARR(nx,ny)
    x_indices=FLTARR(nx,ny)
    for i=0, nx-1 do begin
      x_indices[i,*]=float(i)
    endfor
    for j=0, ny-1 do begin
      y_indices[*,j]=float(j)
    endfor
   
   
    NY_m=NY/2
    NX_m=NX/2
    sigma_y = 2
    char_freq1=NX_m-92;253, corresponds to frequency=-0.133
    char_freq2=NX_m+92;441, corresponds to frequency=0.133
    screening_width1=50 ;filtering the low-frequency noise
    screening_width2=200 ; filtering the high-frequency noise
    delta=0.5
 
    ;Apply filter along the y-direction at range of x-coordinates [char_freq1-screening_width2:char_freq1+screening_width1] and [char_freq2-screening_width1:char_freq2+screening_width2] 
    ;The tanh function ensures a smooth filtering  
    Re[char_freq1-screening_width2:char_freq1+screening_width1,*]= Re[char_freq1-screening_width2:char_freq1+screening_width1,*]*((0.5-0.5*tanh((y_indices[char_freq1-screening_width2:char_freq1+screening_width1,*]-(double(NY_m)-sigma_y))/delta))+$
    (0.5+0.5*tanh((y_indices[char_freq1-screening_width2:char_freq1+screening_width1,*]-(double(NY_m)+sigma_y))/delta)))
    Re[char_freq2-screening_width1:char_freq2+screening_width2,*]= Re[char_freq2-screening_width1:char_freq2+screening_width2,*]*((0.5-0.5*tanh((y_indices[char_freq2-screening_width1:char_freq2+screening_width2,*]-(double(NY_m)-sigma_y))/delta))+$
    (0.5+0.5*tanh((y_indices[char_freq1-screening_width2:char_freq1+screening_width1,*]-(double(NY_m)+sigma_y))/delta)))
    Im[char_freq1-screening_width2:char_freq1+screening_width1,*]= Im[char_freq1-screening_width2:char_freq1+screening_width1,*]*((0.5-0.5*tanh((y_indices[char_freq1-screening_width2:char_freq1+screening_width1,*]-(double(NY_m)-sigma_y))/delta))+$
    (0.5+0.5*tanh((y_indices[char_freq1-screening_width2:char_freq1+screening_width1,*]-(double(NY_m)+sigma_y))/delta)))
    Im[char_freq2-screening_width1:char_freq2+screening_width2,*]= Im[char_freq2-screening_width1:char_freq2+screening_width2,*]*((0.5-0.5*tanh((y_indices[char_freq1-screening_width2:char_freq1+screening_width1,*]-(double(NY_m)-sigma_y))/delta))+$
    (0.5+0.5*tanh((y_indices[char_freq1-screening_width2:char_freq1+screening_width1,*]-(double(NY_m)+sigma_y))/delta)))
    
    slice_fft_filtered=dcomplex(Re,Im)
    logpower_filtered = alog10(ABS(slice_fft_filtered)^2)
    neg=where(logpower_filtered lt -10.)

    logpower_filtered[neg]=-10.0

    filtered_slice=fft(slice_fft_filtered,/INVERSE, /CENTER)

    Re2=real_part(filtered_slice)
    Im2=imaginary(filtered_slice)

    phi[*,*,z-32]=Re2[*,*]

    filtered_slice_real=Re2
;    p = IMAGE((slice), LAYOUT = [2, 2, 1], min_value=0,max_value=255)
;    p = IMAGE(BYTSCL(logpower), LAYOUT = [2, 2, 2], /CURRENT)   
;    p = IMAGE(BYTSCL(logpower_filtered), LAYOUT = [2, 2, 4], /CURRENT)    
;    p = IMAGE(filtered_slice_real, LAYOUT = [2, 2, 3], min_value=0,max_value=255, /CURRENT)
;    stop
  endfor


  wh_under=where(phi lt 0)
  phi[wh_under]=0.d0
  wh_over=where(phi gt 255)
  phi[wh_over]=255.d0

  magnifiedphi = CONGRID(phi, 694, 302, 630, /INTERP)

    save,phi,filename=mainDir+'data/AIST_LSCF_0h_694_302_360_Stage1_2DFFT_GrayScale_170111.sav';phi
    save,magnifiedphi,filename=mainDir+'data/AIST_LSCF_0h_694_302_630_Stage1_2DFFT_GrayScale_170111.sav';phi
  
;  pore_wh=where(phi lt 20)
;  pore_int_wh=where((phi ge 20) and (phi lt 90))
;  LSCF_wh=where(phi gt 150)
;  LSCF_int_wh=where((phi le 150) and (phi ge 90))
; 
;  phi[pore_wh]=0.d0
;  phi[pore_int_wh]= (phi[pore_int_wh]-20.d0)/140.d0
;  phi[LSCF_wh]=1.d0
;  phi[LSCF_int_wh]= (phi[LSCF_int_wh]-90.d0)/120.d0  
;  
;  
;  
;
;
;writeFortranData, phi, '~/Desktop/SOFC/AIST_LSCF/control/reconstruction/data/AIST_LSCF_0h_694_302_360_Stage1_2DFFT_OP_170111.dat'
;writeFortranData, magnifiedphi, '~/Desktop/SOFC/AIST_LSCF/control/reconstruction/data/AIST_LSCF_0h_694_302_630_Stage1_2DFFT_OP_170111.dat'

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
    XTitle='Grayscale', $
    YTitle='Frequency', $
    XRange=[0,300],CHARSIZE=1.5,THICK=1;$    Position=[0.125, 0.125, 0.9, 0.925], $

  OPLOT,pdf1,color=[25],THICK=5
  OPLOT,pdf2,color=[250],THICK=5
    OPLOT,[154,154],[0,max(pdf1)],Linestyle=0,THICK=5,Color=[25]
    OPLOT,[156,156],[0,max(pdf1)],Linestyle=0,THICK=5,Color=[250]
  DEVICE, /SYMBOL, FONT_INDEX=9
  DEVICE, /TIMES, FONT_INDEX=4

  background_color=[255]
  Lines=[0,0]
  thick=[5,5]
  items=['Bf 2D FFT','Af 2D FFT']
  colors=[25,250]
  ; Location of legend in data coordinates.
  yloc = (!Y.CRange[1] - !Y.CRange[0]) * 0.95 + !Y.CRange[0]
  xloc = (!X.CRange[1] - !X.CRange[0]) * 0.6 + !X.CRange[0]

  ; Add the legend.
  Al_Legend, items, Lines=lines,Color=colors, Background_color=background_color, $
    thick=thick,Position=[xloc,yloc];,charsize=1.3

  ;XYOUTS,0.53,0.87,'!9'+String(172B)+'!4 Desired Threshold Value = 125',/NORMAL
  device,/close
  set_plot,'x'
END
PRO draw_contour,mainDir,phi,filtered_phi_Re2

  levels=255
  bottom=0
  Device, Decomposed=0

output=mainDir+'contours/AIST_LSCF_0h_100_100_z100_Stage1_160930.eps'
;output=mainDir+'contours/AIST_LSCF_0h_100_100_y0_Stage1_3DFFT_Re_160915.eps'

;If you want to save as .eps
!p.font=0
set_plot, 'ps'
Device, filename=output, preview=2, /tt_font, /encapsulated, /color, bits_per_pixel=8, $
  xsize=5, ysize=5, /inches, pre_depth=8, pre_xsize=6, pre_ysize=6, /TIMES

  LoadCT, 0;, Ncolors=levels, Bottom=bottom


  slice=dblarr(100,100)
  slice[*,*]=phi[0:99,0:99,100]
   
  contour,slice,/fill,nlevels=60,background=255,title='Interfacial Locations, z=100 plane', $
      xstyle=1,ystyle=1,xthick=4,ythick=4,XCHARSIZE=1.5,YCHARSIZE=1.5,position=[0.1,0.1,0.9,0.9],/nodata
  
  tv,slice,.5001,.5001,XSIZE=4,YSIZE=4,/inches

  LoadCT, 39, Ncolors=levels, Bottom=bottom

  contour,slice,levels=[150],c_linestyle=0, c_colors=[150],C_THICK=2,/overplot
    
  slice[*,*]=filtered_phi_Re2[0:99,0:99,100]
  contour,slice,levels=[90],c_linestyle=0, c_colors=[250],C_THICK=2,/overplot  

  background_color=[255]
  Lines=[0,0]
  thick=[5,5]
  items=['bf 3D FFT','af 3D FFT']
  colors=[150,250]
  ; Location of legend in data coordinates.
  yloc = (!Y.CRange[1] - !Y.CRange[0]) * 0.97 + !Y.CRange[0]
  xloc = (!X.CRange[1] - !X.CRange[0]) * 0.55 + !X.CRange[0]

  ; Add the legend.
  Al_Legend, items, Lines=lines,Color=colors, Background_color=background_color, $
    thick=thick,Position=[xloc,yloc];,charsize=1.3

;If you want to save as .eps
device,/close
set_plot,'x'
END
PRO readForData,filename,size, data
  data = DBLARR(size[0],size[1],size[2])
  OPENR, lun, filename, /GET_LUN, /F77_UNFORMATTED
  READU, lun, data
  FREE_LUN, lun
END
PRO writeFortranData, data, filename
  OPENW, lun,filename, /GET_LUN, /F77_UNFORMATTED
  WRITEU, lun, data
  FREE_LUN, lun
END