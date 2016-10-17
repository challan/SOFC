
pro FFT_filtering

  mainDir='~/Desktop/SOFC/AIST_LSCF/control/reconstruction/'

;  restore,mainDir+'data/AIST_LSCF_0h_694_302_360_Stage1_160909.sav';phi
;  phi=phi[0:nx-1,0:ny-1,0:nz-1]
;  filename=mainDir+'data/AIST_LSCF_0h_694_302_360_bf_thresh.dat'
;  readForData,filename,[694,302,360], phi
  
  sizeArr=[200,200,200]
  nx=sizeArr[0]
  ny=sizeArr[1]
  nz=sizeArr[2]

  restore,mainDir+'data/AIST_LSCF_0h_200_200_200_Stage1_160924.sav';phi
;  restore,mainDir+'data/AIST_LSCF_0h_200_200_200_Stage1_3DFFT_Re.sav'
;  draw_contour,mainDir,phi,filtered_phi_Re2
  slice=dblarr(nx,ny)
  
  slice[*,*]=phi[*,*,58]
  print,min(slice[0,*]),max(slice[0,*]),mean(slice[0,*])
  col1=dblarr(ny)
  col1[*]=slice[0,*]
  p = IMAGE((slice), LAYOUT = [2, 2, 1],min_value=0,max_value=255 )

  slice[*,*]=phi[*,*,52]
  print,min(slice[0,*]),max(slice[0,*]),mean(slice[0,*])
  col2=dblarr(ny)
  col2[*]=slice[0,*] 
  p = IMAGE((slice), LAYOUT = [2, 2, 3],/CURRENT, min_value=0,max_value=255 )
  
  slice[*,*]=phi[0,*,*]
  slice[*,58]=0
  p = IMAGE((slice), LAYOUT = [2, 2, 2], /CURRENT,min_value=0,max_value=255)
  
  slice[*,*]=phi[0,*,*]  
  slice[*,52]=0  
  p = IMAGE((slice), LAYOUT = [2, 2, 4], /CURRENT,min_value=0,max_value=255)
  
  filename=mainDir+'contours/AIST_LSCF_0h_200_200_200_Stage1_160924_Plot1.eps'
  makeplot, filename, col1, col2
  
 stop
 
  phi_fft=fft(phi,-1, /CENTER)
  logpower = ALOG10(ABS(phi_fft)^2)
  
  NX_m=NX/2  
  NY_m=NY/2
  NZ_m=NZ/2

  Re=real_part(phi_fft)
  Im=imaginary(phi_fft)

  factor=dblarr(nx,ny,nz)
  factor[*,*,*]=1.0
  delta=1.0d0
  high_freq_band_radius=90.d0
  low_freq_band_radius=10.d0
  wy=2 ; width of frequency band where the signla is filtered
  
  for i = 0, nx-1 do begin
    x = float(i)
    for j = NY_m-wy, NY_m+wy do begin
      y = float(j)
      for k = 0, nz-1 do begin
        z = float(k)
        circle = SQRT((x-NX_m)^2.+(z-NZ_m)^2.)
        tmp = 0.5d0+0.5d0*TANH((circle-high_freq_band_radius)/delta)
        factor(i,j,k)=tmp
      endfor
    endfor
  endfor

  for i = 0, nx-1 do begin
    x = float(i)
    for j = NY_m-wy, NY_m+wy do begin
      y = float(j)
      for k = 0, nz-1 do begin
        z = float(k)
        circle = SQRT((x-NX_m)^2.+(z-NZ_m)^2.)
        tmp = 0.5d0+0.5d0*TANH((low_freq_band_radius-circle)/delta)
        factor(i,j,k)=factor(i,j,k)+tmp
      endfor
    endfor
  endfor

  Re=Re*factor
  Im=Im*factor
  
 
  phi_fft_filtered=dcomplex(Re,Im)
  logpower_filtered = alog10(ABS(phi_fft_filtered)^2)
  neg=where(logpower_filtered lt -10.)
  logpower_filtered[neg]=-10.0
  
  save,logpower_filtered,filename=mainDir+'data/AIST_LSCF_0h_200_200_200_Stage1_3DFFT_Amplitude.sav'
  

  filtered_phi=fft(phi_fft_filtered,/INVERSE, /CENTER)
  
  filtered_phi_Re2=real_part(filtered_phi)
  filtered_phi_Im2=imaginary(filtered_phi)

  save,filtered_phi_Re2,filename=mainDir+'data/AIST_LSCF_0h_200_200_200_Stage1_3DFFT_Re.sav'

  slice[*,*]=filtered_phi_Re2[*,*,0]
  p = IMAGE((slice), LAYOUT = [3, 2, 4], /CURRENT )
  slice[*,*]=filtered_phi_Re2[0,*,*]
  p = IMAGE((slice), LAYOUT = [3, 2, 5], /CURRENT)
  slice[*,*]=filtered_phi_Re2[*,0,*]
  p = IMAGE((slice), LAYOUT = [3, 2, 6], /CURRENT) 
  
  draw_contour,mainDir,phi,filtered_phi_Re2
  
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

output=mainDir+'contours/AIST_LSCF_0h_100_100_z0_Stage1_160924.eps'
;output=mainDir+'contours/AIST_LSCF_0h_100_100_y0_Stage1_3DFFT_Re_160915.eps'

;If you want to save as .eps
!p.font=0
set_plot, 'ps'
Device, filename=output, preview=2, /tt_font, /encapsulated, /color, bits_per_pixel=8, $
  xsize=5, ysize=5, /inches, pre_depth=8, pre_xsize=6, pre_ysize=6, /TIMES

  LoadCT, 0;, Ncolors=levels, Bottom=bottom

  slice=dblarr(100,100)  
  slice[*,*]=filtered_phi_Re2[0:99,0:99,0]
    
  contour,slice,/fill,nlevels=60,background=255,title='Interfacial Locations, z=0 plane', $
      xstyle=1,ystyle=1,xthick=4,ythick=4,XCHARSIZE=1.5,YCHARSIZE=1.5,position=[0.1,0.1,0.9,0.9],/nodata

;  wh_over=where(slice gt 255)
;  slice[wh_over]=255.d0
;  
;  wh_under=where(slice lt 100)
;  slice[wh_under]=100.d0
    
  tv,slice,.5001,.5001,XSIZE=4,YSIZE=4,/inches

  LoadCT, 39, Ncolors=levels, Bottom=bottom
  slice[*,*]=phi[0:99,0:99,0]
  contour,slice,levels=[150],c_linestyle=0, c_colors=[150],C_THICK=1,/overplot  
  slice[*,*]=filtered_phi_Re2[0:99,0:99,0]
  contour,slice,levels=[156],c_linestyle=0, c_colors=[50],C_THICK=1,/overplot  

  background_color=[255]
  Lines=[0,0]
  thick=[5,5]
  items=['bf 3D FFT','af 3D FFT']
  colors=[150,50]
  ; Location of legend in data coordinates.
  yloc = (!Y.CRange[1] - !Y.CRange[0]) * 0.97 + !Y.CRange[0]
  xloc = (!X.CRange[1] - !X.CRange[0]) * 0.55 + !X.CRange[0]

  ; Add the legend.
  Al_Legend, items, Lines=lines,Color=colors, Background_color=background_color, $
    thick=thick,Position=[xloc,yloc];,charsize=1.3

;If you want to save as .eps
device,/close
set_plot,'x'
stop
END
PRO readForData,filename,size, data
  data = DBLARR(size[0],size[1],size[2])
  OPENR, lun, filename, /GET_LUN, /F77_UNFORMATTED
  READU, lun, data
  FREE_LUN, lun
END