
PRO FFT_filtering_1D

  mainDir='~/Desktop/SOFC/AIST_LSCF/control/reconstruction/'

  sizeArr=[694,302,360]
  nx=sizeArr[0]
  ny=sizeArr[1]
  nz=sizeArr[2]

  line=dblarr(nx)
  frequency=dblarr(nx)
  for i=1,nx-1 do begin
    frequency[i]=-1.0*(nx/2.-1.)/(nx)+(i-1.)/nx
  endfor
    frequency[0]=0.5


  z=32
    filename='~/Desktop/SOFC/AIST_LSCF/control/Rotated Cropped Normalized Images/AIST_LSCF_0h_'+string(z,format='(I3.3)')+'.tif'
    print,filename
    slice = READ_IMAGE(filename)
;    p=image(slice,min_value=0,max_value=255,layout=[1,2,1])
;   
;    slice=(slice-100.d0)*255d0/155.d0
;    p=image(slice,min_value=0,max_value=255,layout=[1,2,2],/current)
    
    line[*]=slice[*,130]

    find_peakwavelength, line,nx
    

    line_fft=fft(line,-1, /CENTER)
    logpower = ALOG10(ABS(line_fft)^2)


    Re=real_part(line_fft)
    Im=imaginary(line_fft)

    no=nx/2-3
    penalty1_array=dblarr(no)
    penalty2_array=dblarr(no)
    screening_width_array=dblarr(no)
    zero_array=dblarr(no)
    i=0
 ;   for nx_width=2,no+1 do begin
  
    char_freq1=nx/2-80;253
    char_freq2=nx/2+80;441
    screening_width1=50 ;filtering the low-frequency noise
    screening_width2=200 ; filtering the high-frequency noise
    Re[char_freq1-screening_width2:char_freq1+screening_width1]=0.0
    Re[char_freq2-screening_width1:char_freq2+screening_width2]=0.0
    Im[char_freq1-screening_width2:char_freq1+screening_width1]=0.0
    Im[char_freq2-screening_width1:char_freq2+screening_width2]=0.0
    
    line_fft_filtered=dcomplex(Re,Im)
    logpower_filtered = alog10(ABS(line_fft_filtered)^2)
    neg=where(logpower_filtered lt -10.)

    logpower_filtered[neg]=-10.0

    filtered_line=fft(line_fft_filtered,/INVERSE, /CENTER)

    Re2=real_part(filtered_line)
    Im2=imaginary(filtered_line)

    filtered_line_real=Re2
    
;    stats, line,filtered_line_real,nx,penalty1,penalty2
;    penalty1_array[i]=penalty1
;    penalty2_array[i]=penalty2
;    screening_width_array[i]=screening_width
;    i=i+1
    
    ;endfor  
    
;    p = PLOT(screening_width_array,penalty1_array, LAYOUT = [1, 2, 1], "r4-", YTITLE='Penalty1', XTITLE='Screening Width', $
;          TITLE="$% Change in Vol. Frac.$",yrange=[-0.1,0.2],xrange=[2,no+1])
;    
;    p = PLOT(screening_width_array,zero_array, LAYOUT = [1, 2, 1], "k1-",/current,YTITLE='Penalty1', XTITLE='Screening Width', $
;          TITLE="$% Change in Vol. Frac.$",yrange=[-0.1,0.2],xrange=[2,no+1])          
;          
;    p = PLOT(screening_width_array,penalty2_array, LAYOUT = [1, 2, 2], "b4-", YTITLE='Penalty2', XTITLE='Screening Width', $
;          TITLE="% Change in Variance in the LSCF Phase",yrange=[-0.2,0.1],xrange=[2,no+1], /CURRENT)    
;    p = PLOT(screening_width_array,zero_array, LAYOUT = [1, 2, 2], "k1-",/current,YTITLE='Penalty2', XTITLE='Screening Width', $
;          TITLE="% Change in Variance in the LSCF Phase",yrange=[-0.2,0.1],xrange=[2,no+1])   
    
             
    p = PLOT(line, LAYOUT = [2, 2, 1],"r1-", YTITLE='Grayscale ($\phi$)', XTITLE='X-coordinate', $
      TITLE="Grayscale",yrange=[0,300],xrange=[0,700])
    p = PLOT(frequency,ABS(line_fft), LAYOUT = [2, 2, 2],"k1-", YTITLE='Amplitude', XTITLE='Frequency', $
      TITLE="Amplitude ($|z|$)",yrange=[0,10.],xrange=[-0.5,0.5], /CURRENT)   
    
    p = PLOT(filtered_line_real, LAYOUT = [2, 2, 3],"b2-", YTITLE='Grayscale ($\phi$)', XTITLE='X-coordinate', $
      TITLE="Grayscale",yrange=[0,300],xrange=[0,700],/CURRENT)
    p = PLOT(line, LAYOUT = [2, 2, 3],"r1-", YTITLE='Grayscale ($\phi$)', XTITLE='X-coordinate', $
      TITLE="Grayscale",yrange=[0,300],xrange=[0,700],/CURRENT)      
    p = PLOT(frequency,ABS(line_fft_filtered), LAYOUT = [2, 2, 4],"k1-", YTITLE='Amplitude', XTITLE='Frequency', $
      TITLE="Amplitude ($|z|$)",yrange=[0,10.],xrange=[-0.5,0.5], /CURRENT)    

  
END
PRO stats, data1,data2,nx,penalty1,penalty2

  LSCF_phase=where(data1 ge 150)
  var1=variance(data1[LSCF_phase])
  vol_frac1=float(size(LSCF_phase,/dimensions))/float(nx)
  
  LSCF_phase=where(data2 ge 150)
  var2=variance(data2[LSCF_phase])
  vol_frac2=float(size(LSCF_phase,/dimensions))/float(nx)
  
  ;Minimize penalty 1 (we want to preseve the volume fraction)
  penalty1=(vol_frac2/vol_frac1-1)
  ;Minimize penalty 2 (we want to reduce the variance of grayscale values in the LSCF phase) 
  penalty2=(var2/var1-1)
  
;  print,strcompress(var1),strcompress(var2)
END
PRO find_peakwavelength, data,nx
  wh_peaks=where(data gt 150)
  no_peaks=size(wh_peaks,/dimensions)
  distance_array=intarr(no_peaks)
  distance_array[*]=-1  
  k=0
  peak_position1=700
  for i=2,nx-2 do begin
    x1=data[i]-150.d0
    x2=data[i-1]-150.d0
    x3=x1*x2
    if (x3 gt 0) then begin ; if you are in the same phase
     deriminus=data[i]-data[i-1]
     deriplus=data[i+1]-data[i] 
    ; if you are at the index corresponding to where the bright peak is located
    if ((data[i] gt 150) and (deriminus gt 0) and (deriplus lt 0)) then begin
      peak_position2=i
      distance_array[k]=peak_position2-peak_position1     
      peak_position1=peak_position2
      k=k+1
      
    endif      
    endif
    
    if (x3 lt 0) then begin
      peak_position1=700
    endif
    
  endfor
  
  wh_intervals=where(distance_array gt 0)
  no_intervals=size(wh_intervals,/dimensions)
  interval_array=dblarr(no_intervals)
  interval_array[*]=distance_array[wh_intervals]
  print,mean(interval_array)
  
  stop
  

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