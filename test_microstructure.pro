PRO test_multiarray
sizeArr1=[704,324,630]
nx=sizeArr1[0]
ny=sizeArr1[1]
nz=sizeArr1[2]


filename='data/AIST_LSCF_0h_704_324_630.dat'
readFortranData, filename,sizeArr1,phi_stretched

sizeArr2=[704,324,360]
nx=sizeArr2[0]
ny=sizeArr2[1]
nz=sizeArr2[2]
filename='data/AIST_LSCF_0h_704_324_360.dat'
readFortranData,filename,sizeArr2,phi_unstretched


nlevels=255
bottom=3
Device, Decomposed=0
LoadCT, 25, Ncolors=levels, Bottom=bottom

output='images/Isolines_AIST_LSCF_0h_704_324_360_630_at_z_equal_360_630.eps'

;If you want to save as .eps
!p.font=0
set_plot, 'ps'
Device, filename=output, preview=2, /tt_font, /encapsulated, /color, bits_per_pixel=8, $
  xsize=5, ysize=5, /inches, pre_depth=8, pre_xsize=6, pre_ysize=6, /TIMES


contour,phi_unstretched[0:100,0:100,359],levels=[0.5],c_linestyle=0, c_colors=[0],background=256,C_THICK=5,$
	title='Isolines at z=360 / 630', $
	xstyle=1,ystyle=1,xthick=4,ythick=4,XCHARSIZE=1.5,YCHARSIZE=1.5

contour,phi_stretched[0:100,0:100,629],levels=[0.5],c_linestyle=1, c_colors=[255],C_THICK=5,/overplot
	
;contour,slice,/FILL,NLEVELS=255,xstyle=1,ystyle=1,xthick=4,ythick=4,XCHARSIZE=1.5,YCHARSIZE=1.5,$
;	title='Isolines at z=1'
;ColorBar4, Divisions=5, Range=[min_slice, max_slice], Format='(F5.2)', $
;  Color=0, NColors=levels, Bottom=bottom , Position = [0.13, 0.968, 0.92, 0.998], charsize=1.2

;If you want to save as .eps
device,/close
set_plot,'x'


END
;========================================
PRO writeMatlabData, data, filename
  OPENW, lun, filename, /get_lun
  WRITEU, lun, data
  FREE_LUN, lun
END
;========================================
PRO readMatlabData, filename,sizeArr, data
data = DBLARR( sizeArr[0], sizeArr[1], sizeArr[2])
OPENR, lun, filename, /GET_LUN
READU, lun, data
FREE_LUN, lun
END
;========================================
PRO readFortranData, filename,size, data
  data = DBLARR( size[0], size[1], size[2])
  OPENR, lun, filename, /GET_LUN, /F77_UNFORMATTED
  READU, lun, data
  FREE_LUN, lun
END
;========================================
PRO writeFortranData, data, filename
  OPENW, lun,filename, /GET_LUN, /F77_UNFORMATTED
  WRITEU, lun, data
  FREE_LUN, lun
END
;;________________________________________________________________________________________
PRO COLORBAR4, BOTTOM=bottom, CHARSIZE=charsize, COLOR=color, DIVISIONS=divisions, $
   FORMAT=format, POSITION=position, MAXRANGE=maxrange, MINRANGE=minrange, NCOLORS=ncolors, $
   TITLE=title, VERTICAL=vertical, TOP=top, RIGHT=right, MINOR=minor, $
   RANGE=range, FONT=font, TICKLEN=ticklen, _EXTRA=extra, INVERTCOLORS=invertcolors
  
   ; Return to caller on error.
  
On_Error, 2
  
   ; Save the current plot state.
  
bang_p = !P
bang_x = !X
bang_Y = !Y
bang_Z = !Z
bang_Map = !Map
  
   ; Are scalable pixels available on the device?
  
IF (!D.Flags AND 1) NE 0 THEN scalablePixels = 1 ELSE scalablePixels = 0
  
   ; Which release of IDL is this?
  
thisRelease = Float(!Version.Release)
  
    ; Check and define keywords.
  
IF N_ELEMENTS(ncolors) EQ 0 THEN BEGIN
  
   ; Most display devices to not use the 512 colors available to
   ; the PostScript device. This presents a problem when writing
   ; general-purpose programs that can be output to the display or
   ; to the PostScript device. This problem is especially bothersome
   ; if you don't specify the number of colors you are using in the
   ; program. One way to work around this problem is to make the
   ; default number of colors the same for the display device and for
   ; the PostScript device. Then, the colors you see in PostScript are
   ; identical to the colors you see on your display. Here is one way to
   ; do it.
  
   IF scalablePixels THEN BEGIN
      oldDevice = !D.NAME
  
         ; What kind of computer are we using? SET_PLOT to appropriate
         ; display device.
  
      thisOS = !VERSION.OS_FAMILY
      thisOS = STRMID(thisOS, 0, 3)
      thisOS = STRUPCASE(thisOS)
      CASE thisOS of
         'MAC': SET_PLOT, thisOS
         'WIN': SET_PLOT, thisOS
         ELSE: SET_PLOT, 'X'
      ENDCASE
  
         ; Here is how many colors we should use.
  
      ncolors = !D.TABLE_SIZE
      SET_PLOT, oldDevice
    ENDIF ELSE ncolors = !D.TABLE_SIZE
ENDIF
IF N_ELEMENTS(bottom) EQ 0 THEN bottom = 0B
IF N_ELEMENTS(charsize) EQ 0 THEN charsize = 1.0
IF N_ELEMENTS(format) EQ 0 THEN format = '(I5)'
IF N_ELEMENTS(color) EQ 0 THEN color = !P.Color
IF N_ELEMENTS(minrange) EQ 0 THEN minrange = 0
IF N_ELEMENTS(maxrange) EQ 0 THEN maxrange = ncolors
IF N_ELEMENTS(ticklen) EQ 0 THEN ticklen = 0.2
IF N_ELEMENTS(minor) EQ 0 THEN minor = 2
IF N_ELEMENTS(range) NE 0 THEN BEGIN
   minrange = range[0]
   maxrange = range[1]
ENDIF
IF N_ELEMENTS(divisions) EQ 0 THEN divisions = 6
IF N_ELEMENTS(font) EQ 0 THEN font = !P.Font
IF N_ELEMENTS(title) EQ 0 THEN title = ''
  
IF KEYWORD_SET(vertical) THEN BEGIN
   bar = REPLICATE(1B,20) # BINDGEN(ncolors)
   IF Keyword_Set(invertcolors) THEN bar = Reverse(bar, 2)
   IF N_ELEMENTS(position) EQ 0 THEN BEGIN
      position = [0.88, 0.1, 0.95, 0.9]
   ENDIF ELSE BEGIN
      IF position[2]-position[0] GT position[3]-position[1] THEN BEGIN
         position = [position[1], position[0], position[3], position[2]]
      ENDIF
      IF position[0] GE position[2] THEN Message, "Position coordinates can't be reconciled."
      IF position[1] GE position[3] THEN Message, "Position coordinates can't be reconciled."
   ENDELSE
ENDIF ELSE BEGIN
   bar = BINDGEN(ncolors) # REPLICATE(1B, 20)
   IF Keyword_Set(invertcolors) THEN bar = Reverse(bar, 1)
   IF N_ELEMENTS(position) EQ 0 THEN BEGIN
      position = [0.1, 0.88, 0.9, 0.95]
   ENDIF ELSE BEGIN
      IF position[3]-position[1] GT position[2]-position[0] THEN BEGIN
         position = [position[1], position[0], position[3], position[2]]
      ENDIF
      IF position[0] GE position[2] THEN Message, "Position coordinates can't be reconciled."
      IF position[1] GE position[3] THEN Message, "Position coordinates can't be reconciled."
   ENDELSE
ENDELSE
  
   ; Scale the color bar.
  
 bar = BYTSCL(bar, TOP=(ncolors-1 < (255-bottom))) + bottom
  
   ; Get starting locations in NORMAL coordinates.
  
xstart = position(0)
ystart = position(1)
  
   ; Get the size of the bar in NORMAL coordinates.
  
xsize = (position(2) - position(0))
ysize = (position(3) - position(1))
  
   ; Display the color bar in the window. Sizing is
   ; different for PostScript and regular display.
  
IF scalablePixels THEN BEGIN
  
   TV, bar, xstart, ystart, XSIZE=xsize, YSIZE=ysize, /Normal
  
ENDIF ELSE BEGIN
  
   bar = CONGRID(bar, CEIL(xsize*!D.X_VSize), CEIL(ysize*!D.Y_VSize), /INTERP)
  
        ; Decomposed color off if device supports it.
  
   CASE  StrUpCase(!D.NAME) OF
        'X': BEGIN
            IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
            Device, Decomposed=0
            ENDCASE
        'WIN': BEGIN
            IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
            Device, Decomposed=0
            ENDCASE
        'MAC': BEGIN
            IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
            Device, Decomposed=0
            ENDCASE
        ELSE:
   ENDCASE
  
   TV, bar, xstart, ystart, /Normal
  
      ; Restore Decomposed state if necessary.
  
   CASE StrUpCase(!D.NAME) OF
      'X': BEGIN
         IF thisRelease GE 5.2 THEN Device, Decomposed=thisDecomposed
         ENDCASE
      'WIN': BEGIN
         IF thisRelease GE 5.2 THEN Device, Decomposed=thisDecomposed
         ENDCASE
      'MAC': BEGIN
         IF thisRelease GE 5.2 THEN Device, Decomposed=thisDecomposed
         ENDCASE
      ELSE:
   ENDCASE
  
ENDELSE
  
   ; Annotate the color bar.
  
IF KEYWORD_SET(vertical) THEN BEGIN
  
   IF KEYWORD_SET(right) THEN BEGIN
  
      PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=1, $
         YTICKS=divisions, XSTYLE=1, YSTYLE=9, $
         POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
         YTICKFORMAT='(A1)', XTICKFORMAT='(A1)', YTICKLEN=ticklen , $
         YRANGE=[minrange, maxrange], FONT=font, _EXTRA=extra, YMINOR=minor
  
      AXIS, YAXIS=1, YRANGEdi=[minrange, maxrange], YTICKFORMAT=format, YTICKS=divisions, $
         YTICKLEN=ticklen, YSTYLE=1, COLOR=color, CHARSIZE=charsize, $
         FONT=font, YTITLE=title, _EXTRA=extra, YMINOR=minor
  
   ENDIF ELSE BEGIN
  
      PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=1, $
         YTICKS=divisions, XSTYLE=1, YSTYLE=9, YMINOR=minor, $
         POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
         YTICKFORMAT=format, XTICKFORMAT='(A1)', YTICKLEN=ticklen , $
         YRANGE=[minrange, maxrange], FONT=font, YTITLE=title, _EXTRA=extra
  
      AXIS, YAXIS=1, YRANGE=[minrange, maxrange], YTICKFORMAT='(A1)', YTICKS=divisions, $
         YTICKLEN=ticklen, YSTYLE=1, COLOR=color, CHARSIZE=charsize, $
         FONT=font, _EXTRA=extra, YMINOR=minor
  
   ENDELSE
  
ENDIF ELSE BEGIN
  
   IF KEYWORD_SET(top) THEN BEGIN
  
      PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=divisions, $
         YTICKS=1, XSTYLE=9, YSTYLE=1, $
         POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
         YTICKFORMAT='(A1)', XTICKFORMAT='(A1)', XTICKLEN=ticklen, $
         XRANGE=[minrange, maxrange], FONT=font, _EXTRA=extra, XMINOR=minor
  
      AXIS, XTICKS=divisions, XSTYLE=1, COLOR=color, CHARSIZE=charsize, $
         XTICKFORMAT=format, XTICKLEN=ticklen, XRANGE=[minrange, maxrange], XAXIS=1, $
         FONT=font, XTITLE=title, _EXTRA=extra, XCHARSIZE=charsize, XMINOR=minor
  
   ENDIF ELSE BEGIN
  
      PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=divisions, $
         YTICKS=1, XSTYLE=1, YSTYLE=1, TITLE=title, $
         POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
         YTICKFORMAT='(A1)', XTICKFORMAT=format, XTICKLEN=ticklen, $
         XRANGE=[minrange, maxrange], FONT=font, XMinor=minor, _EXTRA=extra
  
    ENDELSE
  
ENDELSE
  
   ; Restore the previous plot and map system variables.
  
!P = bang_p
!X = bang_x
!Y = bang_y
!Z = bang_z
!Map = bang_map
  
END