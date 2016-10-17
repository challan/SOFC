
PRO FFT_filtering_2D

  sizeArr=[694,302,360]
  nx=sizeArr[0]
  ny=sizeArr[1]
  nz=sizeArr[2]

  mainDir='~/Desktop/SOFC/AIST_LSCF/control/reconstruction/'

  restore,mainDir+'data/AIST_LSCF_0h_200_200_200_Stage1_160924.sav';phi
  phi1=phi
  phi1=phi1[0:199,0:199,0:199]

  slice=dblarr(200,200)
  slice[*,*]=phi1[*,*,0] 
;  p = IMAGE(slice, LAYOUT = [3, 2, 1], min_value=0,max_value=255,margin=0.01)
;  slice[*,*]=phi1[0,*,*]
;  p = IMAGE(slice, LAYOUT = [3, 2, 2], min_value=0,max_value=255,margin=0.01,/current)
;  slice[*,*]=phi1[*,0,*]
;  p = IMAGE(slice, LAYOUT = [3, 2, 3], min_value=0,max_value=255,margin=0.01,/current)  

  restore,mainDir+'data/AIST_LSCF_0h_200_200_200_Stage1_2DFFT_160930.sav';phi
  
  phi2=phi
  phi2=phi2[0:199,0:199,0:199]

  phi=0b

  slice=dblarr(200,200)
  slice[*,*]=phi2[*,*,0]
;  p = IMAGE(slice, LAYOUT = [3, 2, 4], min_value=0,max_value=255,margin=0.01,/current)
;  slice[*,*]=phi2[0,*,*]
;  p = IMAGE(slice, LAYOUT = [3, 2, 5], min_value=0,max_value=255,margin=0.01,/current)
;  slice[*,*]=phi2[*,0,*]
;  p = IMAGE(slice, LAYOUT = [3, 2, 6], min_value=0,max_value=255,margin=0.01,/current)

  draw_contour,mainDir,phi1,phi2
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


    NY_m=NY/2
    NX_m=NX/2
    wx=2 ; width of frequency band where the signal is unfiltered
    wy=2 ; width of frequency band where the signla is filtered
    sigma_y = wy
    sigma_x = wx
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
    
    char_freq1=NX_m-80;253
    char_freq2=NX_m+80;441
    screening_width1=50 ;filtering the low-frequency noise
    screening_width2=200 ; filtering the high-frequency noise
    delta=0.5
    Re[char_freq1-screening_width2:char_freq1+screening_width1,*]= Re[char_freq1-screening_width2:char_freq1+screening_width1,*]*((0.5-0.5*tanh((y_indices[char_freq1-screening_width2:char_freq1+screening_width1,*]-(double(NY_m)-sigma_y))/delta))+$
    (0.5+0.5*tanh((y_indices[char_freq1-screening_width2:char_freq1+screening_width1,*]-(double(NY_m)+sigma_y))/delta)))
    Re[char_freq2-screening_width1:char_freq2+screening_width2,*]= Re[char_freq2-screening_width1:char_freq2+screening_width2,*]*((0.5-0.5*tanh((y_indices[char_freq2-screening_width1:char_freq2+screening_width2,*]-(double(NY_m)-sigma_y))/delta))+$
    (0.5+0.5*tanh((y_indices[char_freq1-screening_width2:char_freq1+screening_width1,*]-(double(NY_m)+sigma_y))/delta)))
    Im[char_freq1-screening_width2:char_freq1+screening_width1,*]= Im[char_freq1-screening_width2:char_freq1+screening_width1,*]*((0.5-0.5*tanh((y_indices[char_freq1-screening_width2:char_freq1+screening_width1,*]-(double(NY_m)-sigma_y))/delta))+$
    (0.5+0.5*tanh((y_indices[char_freq1-screening_width2:char_freq1+screening_width1,*]-(double(NY_m)+sigma_y))/delta)))
    Im[char_freq2-screening_width1:char_freq2+screening_width2,*]= Im[char_freq2-screening_width1:char_freq2+screening_width2,*]*((0.5-0.5*tanh((y_indices[char_freq1-screening_width2:char_freq1+screening_width1,*]-(double(NY_m)-sigma_y))/delta))+$
    (0.5+0.5*tanh((y_indices[char_freq1-screening_width2:char_freq1+screening_width1,*]-(double(NY_m)+sigma_y))/delta)))
    
    ;Apply filter along the y-direction at range of x-coordinates [0:NX_m-wx] and [NX_m+wx:NX-1]
;    Re[char_freq1-screening_width2:char_freq1+screening_width1,*]= Re[char_freq1-screening_width2:char_freq1+screening_width1,*]*(1-exp(-1.*(y_indices[char_freq1-screening_width2:char_freq1+screening_width1,*]-double(NY_m))^2./(2.*sigma_y^2.)))
;    Re[char_freq2-screening_width1:char_freq2+screening_width2,*]= Re[char_freq2-screening_width1:char_freq2+screening_width2,*]*(1-exp(-1.*(y_indices[char_freq2-screening_width1:char_freq2+screening_width2,*]-double(NY_m))^2./(2.*sigma_y^2.)))
;    Im[char_freq1-screening_width2:char_freq1+screening_width1,*]= Im[char_freq1-screening_width2:char_freq1+screening_width1,*]*(1-exp(-1.*(y_indices[char_freq1-screening_width2:char_freq1+screening_width1,*]-double(NY_m))^2./(2.*sigma_y^2.)))
;    Im[char_freq2-screening_width1:char_freq2+screening_width2,*]= Im[char_freq2-screening_width1:char_freq2+screening_width2,*]*(1-exp(-1.*(y_indices[char_freq2-screening_width1:char_freq2+screening_width2,*]-double(NY_m))^2./(2.*sigma_y^2.)))

    ;Apply filter along the x-direction at range of y-coordinates [NY_m-wy:NY_m+wy]
;    Re[NX_m-wx:NX_m+wx,NY_m-wy:NY_m+wy]=Re[NX_m-wx:NX_m+wx,NY_m-wy:NY_m+wy]*exp(-1.*(x_indices[NX_m-wx:NX_m+wx,NY_m-wy:NY_m+wy]-double(NX_m))^2./(2.*sigma_x^2.))
;    Im[NX_m-wx:NX_m+wx,NY_m-wy:NY_m+wy]=Im[NX_m-wx:NX_m+wx,NY_m-wy:NY_m+wy]*exp(-1.*(x_indices[NX_m-wx:NX_m+wx,NY_m-wy:NY_m+wy]-double(NX_m))^2./(2.*sigma_x^2.))

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
  ;p1 = IMAGE(BYTSCL(abs(Re2-slice)))

  phi=phi[0:199,0:199,0:199]
  
  wh_under=where(phi lt 0)
  phi[wh_under]=0.d0
  wh_over=where(phi gt 255)
  phi[wh_over]=255.d0
    
  save,phi,filename=mainDir+'data/AIST_LSCF_0h_200_200_200_Stage1_2DFFT_160930.sav';phi
  

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