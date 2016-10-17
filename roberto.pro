;			Roberto, stack2
;
; Replace 'stack2' with the name of the array that you want to see
; Can also add a pointcurv to color the intefaces, like
;
;			Roberto, stack2, pointcurv=pointcurv
;
; You can also see a 2D array by replacing 'stack2' with the name of the 2D array.
;
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------


;			VERSION  2.4 


;----------------------------------------------------------------------------
;----------------------------------------------------------------------------                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
;----------------------------------------------------------------------------


pro roberto_all
mainDir1='~/Desktop/CP/Set_3/Yongwoo_AC_p71/'
maindir1='~/Desktop/CP/Set_3/AC_p80/'
;mainDir1='~/Desktop/CP/Set_3/experiment/20A/'
;sizeArr=[512,512,512]
sizeArr=[256,256,256]
;sizeArr=[2048,2048,64]
;sizeArr=[200,200,200]
nx = sizeArr[0]
ny = sizeArr[1]
nz = sizeArr[2]

    ;filename1='~/Desktop/Classes/MATH671/figures/AC_delta2_t23000_256.dat'
    ;filename1=mainDir1+'Delta2/AC_delta2_t22200_256.dat'
    ;filename1=mainDir1+'Delta2/smoothing_surface_pg58A/AC_delta2_t20000_lev109surflap3000_256.dat'
    ;filename1=mainDir1+'Delta4/AC_delta4_t10000_lev071lap30_256.dat'
    ;filename1=mainDir1+'Delta8/AC_delta8_t5000_lev031lap30_256.dat'
    ;filename1=mainDir1+'AC_delta4_t10000_256.dat'
    ;filename1=mainDir1+'stack_t010_lev200lap30_256.dat'
    filename1=mainDir1+'AC_delta4_t10000_lev071lap30_256.dat'
    print, filename1
    readFile1, surf, filename1, sizeArr

    ;filename2=mainDir1+'Delta2/smoothing_surface_pg58A/AC_delta2_t20000_lev109surflap2000_256.dat'
    ;filename2=mainDir1+'Delta2/AC_delta2_t22200_256.dat'
    ;filename2=mainDir1+'Delta4/AC_delta4_t10000_10100_Hbox_lev071lap30_256.dat'
    ;filename2=mainDir1+'Delta8/AC_delta8_t5000_5050_Hbox_lev031lap30_256.dat'
    filename2=mainDir1+'AC_delta4_t10000_lev071lap30_256.dat'
    ;filename2=mainDir2+'AC_t0_200_Hbox_lev109lap30_256.dat'
    ;filename2='~/Desktop/CP/Set_3/sphere_64/sphere_flip_Hbox_r40_38_lev112lev112lap30.dat'
    print, filename2 
    readFile2, velo, filename2, sizeArr


roberto,surf,pointCurv=velocityVals

END

PRO readFile1, data, filename1, sizeArr
  data = DBLARR( sizeArr[0], sizeArr[1], sizeArr[2] )
  OPENR, lun, filename1, /GET_LUN, /F77_UNFORMATTED;, /SWAP_ENDIAN
  READU, lun, data;,format='(E20.12)'
  FREE_LUN, lun
  print, '  DONE reading phi'
END
PRO readFile2, data, filename2, sizeArr
  data = FLTARR( sizeArr[0], sizeArr[1], sizeArr[2] )
  OPENR, lun, filename2, /GET_LUN, /F77_UNFORMATTED;, /SWAP_ENDIAN
  READU, lun, data;,format='(E20.12)'
  FREE_LUN, lun
  print, '  DONE reading vel'
END




PRO Roberto_Help, sEvent
; This event handler is for the ABOUT and HELP options

; This is the version history text needed for later
	AboutMessage =['                        VERSION  2.4   ', $
		' ', ' ', $
		'             ___________' + 'May '+STRCOMPRESS(19)+', ' + $ 
		STRCOMPRESS(2003) + '___________', $
		' ', $
		'I decided today that I wanted to have a customized version of the ', $
		'XOBJVIEW program.  I wanted to dyamically change things in the ' , $
		'volumes, like add axis and change the axis color or to change the', $
		'color of the background.  I also hated the fact that the TIFF files',$
		'created by XOBJVIEW needed to be flipped.  Granted it only takes two',$
		'seconds to do it in PHOTOSHOP, but it just added to the anoying factors',$
		'of XOBJVIEW.  I also thought that it would be a better idea to have ' , $
		'some sort of a slider to control the level of zoom.' , $
		' ', $
		'             ___________' + 'June '+STRCOMPRESS(30)+', ' + $ 
		STRCOMPRESS(2003) + '___________', $
		' ', $
		'FINISHED with version 1.0.  I am sooooo happy!', $
		' ', $
		'             ___________' + 'July '+STRCOMPRESS(30)+', ' + $ 
		STRCOMPRESS(2003) + '___________', $
		' ', $
		'Finished with version 2.0.  Fixed a lot of bugs and included a lot of',$
		'other features suggested by Dave.  Added the ability to create a surface',$
		'from a 2D array.']

	HelpMessage = ['All self explanatory.', $
		' ', ' ', $
		'If you do not know what a button does place the mouse over it (this does not work on sliders).']
	Widget_Control, sEvent.top, Get_UValue=sState, /No_Copy

; What information is wanted?
	Widget_Control, sEvent.id, GET_UValue=want

	CASE want OF

		'ABOUT': BEGIN
			BEEP
			RESULT = DIALOG_MESSAGE (AboutMessage, /INFORMATION, TITLE='ABOUT')
			END

		'HELP': BEGIN
			BEEP
			RESULT = DIALOG_MESSAGE (HelpMessage, /INFORMATION, TITLE='HELP')
			END
	ENDCASE

;Put the info structure back.
	Widget_Control, sEvent.top, Set_UValue=sState, /No_Copy

END


;----------------------------------------------------------------------------
PRO Roberto_Event, sEvent

	WIDGET_CONTROL, sEvent.id, GET_UVALUE=uval
;	WIDGET_CONTROL, sEvent.top, GET_UVALUE=sState, /NO_COPY

; Handle KILL requests.
	IF TAG_NAMES(sEvent, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
		WIDGET_CONTROL, sEvent.top, GET_UVALUE=sState
		OBJ_DESTROY, sState.oHolder
		WIDGET_CONTROL, sEvent.top, /DESTROY
		RETURN
	ENDIF

; Handle other events.
	CASE uval OF
	'DRAW': BEGIN
		WIDGET_CONTROL, sEvent.top, GET_UVALUE=sState, /NO_COPY

		IF (sEvent.type EQ 4) THEN BEGIN	; Expose.
			sState.oWindow->Draw, sState.oView
			WIDGET_CONTROL, sEvent.top, SET_UVALUE=sState, /NO_COPY
			RETURN
		ENDIF

	; Handle trackball updates.
		bHaveTransform = sState.oTrack->Update( sEvent, TRANSFORM=qmat )
		IF (bHaveTransform NE 0) THEN BEGIN
			sState.oGroup->GetProperty, TRANSFORM=t
			sState.oGroup->SetProperty, TRANSFORM=t#qmat
			sState.oWindow->Draw, sState.oView
		ENDIF

	; Button press.
		IF (sEvent.type EQ 0) THEN BEGIN
			IF (sEvent.press EQ 4) THEN BEGIN ; Right mouse.
				sState.oWindow->draw, sState.oView
				WIDGET_CONTROL, sEvent.top, SET_UVALUE=sState, /NO_COPY
				RETURN
			ENDIF ELSE BEGIN ; other mouse button.
				sState.btndown = 1b
				sState.oWindow->SetProperty, QUALITY=sState.dragq
				WIDGET_CONTROL, sState.wDraw, /DRAW_MOTION
				sState.oWindow->Draw, sState.oView
			ENDELSE  
		ENDIF

	; Button motion.
		IF (sEvent.type EQ 2) AND (sState.btndown EQ 4b) THEN BEGIN ; Right mouse button.
			pick = sState.oWindow->PickData(sState.oView, $
				sState.oSurface, [sEvent.x,sEvent.y], dataxyz)
		ENDIF

	; Button release.
		IF (sEvent.type EQ 1) THEN BEGIN
			IF (sState.btndown EQ 1b) THEN BEGIN
				sState.oWindow->SetProperty, QUALITY=2
				sState.oWindow->Draw, sState.oView
			ENDIF
			sState.btndown = 0b
			WIDGET_CONTROL, sState.wDraw, DRAW_MOTION=0
		ENDIF
		WIDGET_CONTROL, sEvent.top, SET_UVALUE=sState, /NO_COPY
	END

	'QUIT': BEGIN
		WIDGET_CONTROL, sEvent.top, GET_UVALUE=sState
		OBJ_DESTROY, sState.oHolder
		WIDGET_CONTROL, sEvent.top, /DESTROY
		RETURN
	END

; Color Bar minimum and maximum values
	'MIN': BEGIN
		WIDGET_CONTROL, sEvent.top, GET_UVALUE=sState, /NO_COPY
		CASE sState.wantColor OF
		0: BEGIN
			widget_control, sState.minSlider, SET_VALUE=sState.colorBarRange[0]
			END
		1: BEGIN
			widget_control, sState.minSlider, GET_VALUE=newMin
			widget_control, sState.maxSlider, GET_VALUE=maxVal
			getColors, sState.pointCurv, newMin, maxVal, color, $
				sState.aRed, sState.aBlue, sState.aGreen
			sState.oMyPolygons -> SetProperty, VERT_COLORS = color
			sState.oColorBar -> SetProperty, Range=[newMin, maxVal]
			sState.oWindow->Draw, sState.oView
			END
		ENDCASE
		WIDGET_CONTROL, sEvent.top, SET_UVALUE=sState, /NO_COPY


		END
	'MAX': BEGIN
		WIDGET_CONTROL, sEvent.top, GET_UVALUE=sState, /NO_COPY
		CASE sState.wantColor OF
		0: BEGIN
			widget_control, sState.maxSlider, SET_VALUE=sState.colorBarRange[1]
			END
		1: BEGIN
			widget_control, sState.maxSlider, GET_VALUE=newMax
			widget_control, sState.minSlider, GET_VALUE=minVal
		;	color = getColors (sState.pointCurv, minVal, newMax)
			getColors, sState.pointCurv, minVal, newMax, color, $
				sState.aRed, sState.aBlue, sState.aGreen
			sState.oMyPolygons -> SetProperty, VERT_COLORS = color
			sState.oColorBar -> SetProperty, Range=[minVal, newMax]
			sState.oWindow->Draw, sState.oView
			END
		ENDCASE
		WIDGET_CONTROL, sEvent.top, SET_UVALUE=sState, /NO_COPY
		END
	ENDCASE

END


;----------------------------------------------------------------------------
PRO Roberto_Output, sEvent
; This event handler creates image files.

	Widget_Control, sEvent.top, Get_UValue=sState, /No_Copy

	Widget_Control, /Hourglass
	Wait, 0.5

	sState.oWindow->GetProperty, IMAGE_DATA = snapshot
	imageSize = SIZE(image)

; What kind of file is wanted?
	Widget_Control, sEvent.id, GET_UValue=whichFileType

	CASE whichFileType OF
	'JPEG': BEGIN
		filename = Dialog_Pickfile(/Write, File='my_image.jpg')
		IF filename NE '' THEN Write_JPEG, filename, snapshot, True=1, Quality=100
		END
	'TIFF': BEGIN
		filename = Dialog_Pickfile(/Write, File='my_image.tif')
		IF filename NE '' THEN Write_TIFF, filename, Reverse(snapshot,3)
		END
	'EPS': BEGIN
		filename = Dialog_Pickfile(/Write, File='my_image.eps')
		IF filename NE '' THEN BEGIN
		sState.oWindow->GetProperty, Dimensions=viewDimensions, Units=viewUnits
		clipboard = Obj_New('IDLgrClipboard', Dimensions=viewDimensions, Unit=viewUnits)
		;clipboard = Obj_New('IDLgrClipboard', Dimensions=[.1,.1], Unit=1)
		clipboard->Draw, sState.oView, /Postscript, Filename=filename
		Obj_Destroy, clipboard
		ENDIF
		END
	'MOVIE2D' : BEGIN
		filename = Dialog_Pickfile(/DIRECTORY, GET_PATH=path)
		degrees = 10.0
		rotations = 360.0 / degrees

		FOR I = 1, rotations DO BEGIN
			IF I LT 10 THEN number = strcompress( STRING(0) + STRING(0) + STRING(I) , /remove_all )
			IF I LT 100 AND I GE 10 THEN number = strcompress( STRING(0) + STRING(I) , /remove_all )
			IF I GT 99 THEN number = STRING(I)
			sState.oWindow->GetProperty, IMAGE_DATA = snapshot
			filename = strcompress( path + '/pic' + number + '.jpg' , /remove_all )
			Write_JPEG, filename, snapshot, True=1, Quality=100
			sState.oGroup->Rotate, [0,1,0], -degrees
			sState.oWindow->Draw, sState.oView
		ENDFOR

		END
	'MOVIE3D' : BEGIN
		filename = Dialog_Pickfile(/DIRECTORY, GET_PATH=path)
		degrees = 45.0
		rotations = 360.0 / degrees
		counter = 1L

		FOR J = 1, rotations DO BEGIN
			FOR I = counter, rotations*rotations, rotations DO BEGIN
				IF I LT 10 THEN number = strcompress( STRING(0) + STRING(0) + STRING(I) , /remove_all )
				IF I LT 100 AND I GE 10 THEN number = strcompress( STRING(0) + STRING(I) , /remove_all )
				IF I GT 99 THEN number = STRING(I)
				sState.oWindow->GetProperty, IMAGE_DATA = snapshot
				filename = strcompress( path + '/pic' + number + '.jpg' , /remove_all )
				Write_JPEG, filename, snapshot, True=1, Quality=100
				sState.oGroup->Rotate, [1,0,0], -degrees
				sState.oWindow->Draw, sState.oView
			ENDFOR
			sState.oGroup->Rotate, [0,1,0], degrees
			sState.oWindow->Draw, sState.oView
			counter = counter + 1L
		ENDFOR

		END
	ENDCASE

;Put the info structure back.
	Widget_Control, sEvent.top, Set_UValue=sState, /No_Copy

END


;----------------------------------------------------------------------------
PRO Roberto_Properties, sEvent
; This event handler to set the graphic properties.

	Widget_Control, sEvent.top, Get_UValue=sState, /No_Copy

; What property is wanted?
	Widget_Control, sEvent.id, Get_UValue=newProperty

	CASE newProperty OF

;Reset to the standard colors
	'OG': BEGIN
		sState.oView->SetProperty, Color=[0,0,0]
		sState.xAxis->SetProperty, Color=[255,255,0]
		sState.yAxis->SetProperty, Color=[255,255,0]
		sState.zAxis->SetProperty, Color=[255,255,0]
		sState.dragq = 0
		END

; Background color.
	'BBLACK': sState.oView->SetProperty, Color=[0,0,0]
	'BWHITE': sState.oView->SetProperty, Color=[255,255,255]
	'BCHARCOAL': sState.oView->SetProperty, Color=[80,80,80]
	'BGRAY': sState.oView->SetProperty, Color=[135, 135, 135]

; Axes colors.
	'ABLACK': BEGIN
		sState.xAxis->SetProperty, Color=[0,0,0]
		sState.yAxis->SetProperty, Color=[0,0,0]
		sState.zAxis->SetProperty, Color=[0,0,0]
		END
	'AWHITE': BEGIN
		sState.xAxis->SetProperty, Color=[255,255,255]
		sState.yAxis->SetProperty, Color=[255,255,255]
		sState.zAxis->SetProperty, Color=[255,255,255]
		END
	'AGREEN': BEGIN
		sState.xAxis->SetProperty, Color=[0,255,0]
		sState.yAxis->SetProperty, Color=[0,255,0]
		sState.zAxis->SetProperty, Color=[0,255,0]
		END
	'AYELLOW': BEGIN
		sState.xAxis->SetProperty, Color=[255,255,0]
		sState.yAxis->SetProperty, Color=[255,255,0]
		sState.zAxis->SetProperty, Color=[255,255,0]
		END
	'ANAVY': BEGIN
		sState.xAxis->SetProperty, Color=[0, 0, 115]
		sState.yAxis->SetProperty, Color=[0, 0, 115]
		sState.zAxis->SetProperty, Color=[0, 0, 115]
		END

; Drag Quality
	'LOW': sState.dragq = 0
	'MEDIUM': sState.dragq = 1
	'HIGH': sState.dragq = 2

; Polygon Colors
	'RED': BEGIN
		widget_control, sState.rSlider, GET_VALUE=newRed
		sState.oMyPolygons->GetProperty, COLOR=OGcolor
		sState.oMyPolygons->setProperty, COLOR=[newRed,OGcolor[1],OGcolor[2]]		
		END
	'GREEN': BEGIN
		widget_control, sState.gSlider, GET_VALUE=newGreen
		sState.oMyPolygons->GetProperty, COLOR=OGcolor
		sState.oMyPolygons->setProperty, COLOR=[OGcolor[0],newGreen,OGcolor[2]]
		END
	'BLUE': BEGIN
		widget_control, sState.bSlider, GET_VALUE=newBlue
		sState.oMyPolygons->GetProperty, COLOR=OGcolor
		sState.oMyPolygons->setProperty, COLOR=[OGcolor[0],OGcolor[1],newBlue]
		END
	'RESETC': BEGIN
		widget_control, sState.rSlider, SET_VALUE=sState.pColor[0]
		widget_control, sState.gSlider, SET_VALUE=sState.pColor[1]
		widget_control, sState.bSlider, SET_VALUE=sState.pColor[2]
		sState.oMyPolygons->setProperty, COLOR=sState.pColor
		END
	'AXIS': BEGIN
		sState.xAxis->GetProperty, HIDE=hidden
		CASE hidden OF
			0: BEGIN
				axisString = 'Show Axis'
				sState.xAxis->SetProperty, HIDE=1
				sState.yAxis->SetProperty, HIDE=1
				sState.zAxis->SetProperty, HIDE=1
				END
			1: BEGIN
				axisString = 'Hide Axis'
				sState.xAxis->SetProperty, HIDE=0
				sState.yAxis->SetProperty, HIDE=0
				sState.zAxis->SetProperty, HIDE=0
				END
		ENDCASE
		WIDGET_CONTROL, sState.wShowAxis, SET_VALUE=axisString
		END

; Lighting Schemes
	'ONE': BEGIN
		sState.oL1->SetProperty, HIDE=1
		sState.oL2->SetProperty, HIDE=0
		sState.oL3->SetProperty, HIDE=1
		END
	'TWO': BEGIN
		sState.oL1->SetProperty, HIDE=0
		sState.oL2->SetProperty, HIDE=1
		sState.oL3->SetProperty, HIDE=1
		END
	'THREE': BEGIN
		sState.oL1->SetProperty, HIDE=1
		sState.oL2->SetProperty, HIDE=1
		sState.oL3->SetProperty, HIDE=0
		END

; Transparency
	'TRANSPARENT': BEGIN
		WIDGET_CONTROL, sState.wTransparent, GET_VALUE=transString
		IF (transString EQ 'Solid') THEN BEGIN
			newTransString = 'Transparent'
			nullObject=OBJ_NEW()
			sState.oMyPolygons->SetProperty, TEXTURE_MAP=nullObject
			sState.oMyPolygons1->SetProperty, TEXTURE_MAP=nullObject

		ENDIF ELSE BEGIN
			newTransString = 'Solid'
			sState.oMyPolygons->SetProperty, TEXTURE_MAP=sState.oMyImage
			sState.oMyPolygons1->SetProperty, TEXTURE_MAP=sState.oMyImage
		ENDELSE
		WIDGET_CONTROL, sState.wTransparent, SET_VALUE=newTransString
		END
	'TSLIDER': BEGIN
		WIDGET_CONTROL, sState.wTransparent, GET_VALUE=transString
		IF (transString EQ 'Transparent') THEN BEGIN
			widget_control, sState.tSlider, SET_VALUE=255
		ENDIF ELSE BEGIN
			widget_control, sState.tSlider, GET_VALUE=newTrans
			sState.Pattern[0,*,*] = newTrans
			Pattern = BYTARR(2,4,4)
			Pattern[0,*,*] = BYTE(newTrans)
			Pattern[1,*,*] = 50B
			oMyNewImage = OBJ_NEW('IDLgrImage', Pattern, interleave=0)
			sState.oMyImage = oMyNewImage
			sState.oMyPolygons -> SetProperty, TEXTURE_MAP = sState.oMyImage
			sState.oMyPolygons1 -> SetProperty, TEXTURE_MAP = sState.oMyImage
		ENDELSE
		END

; Reset orientation
	'RESET': BEGIN
		sState.oGroup->SetProperty, Transform=sState.origTransform
		END

	ENDCASE

; Redraw the graphic.
	sState.oWindow->Draw, sState.oView

;Put the info structure back.
	Widget_Control, sEvent.top, Set_UValue=sState, /No_Copy

END


;----------------------------------------------------------------------------
PRO Roberto_Zoom, sEvent
; This is to set the value of zoom
	Widget_Control, sEvent.id, Get_UValue=newProperty

; Get the aspect ratio
	Widget_Control, sEvent.top, Get_UValue=sState, /No_Copy
	aspect = sState.aspect

; Get the view and put it in
	getView, newProperty, aspect, myView
	sState.oView->SetProperty, VIEWPLANE_RECT=myView

; Change the text
	scalingString = 'Zoom : ' + newProperty + ' %'
	WIDGET_CONTROL, sState.wScalingLabel, SET_VALUE=scalingString

; Save the value of the zoom
	CASE newProperty OF
		'0'  : zoomVal = 0
		'10' : zoomVal = 10
		'20' : zoomVal = 20
		'30' : zoomVal = 30
		'40' : zoomVal = 40
		'50' : zoomVal = 50
		'60' : zoomVal = 60
		'70' : zoomVal = 70
		'80' : zoomVal = 80
		'90' : zoomVal = 90
		'100': zoomVal = 100
	ENDCASE
	sState.zoomVal = zoomVal

; Redraw the graphic.
	sState.oWindow->Draw, sState.oView

; Put the info structure back.
	Widget_Control, sEvent.top, Set_UValue=sState, /No_Copy

END

;----------------------------------------------------------------------------
PRO getView, zoomFactor, aspect, myView
; This function is used to calculate the VIEW used to zoom in and out

; Set up the equation for the zoom
	zeroVal = FLOAT(1.9) 	; Larger value makes it zoom less
	topVal  = FLOAT(0.5)
	slope   = FLOAT( (topVal-zeroVal)/100.0 )

; What view property is wanted?
	CASE zoomFactor OF
		'0'  : sqrt2 = slope *   0.0 + zeroVal
		'10' : sqrt2 = slope *  10.0 + zeroVal
		'20' : sqrt2 = slope *  20.0 + zeroVal
		'30' : sqrt2 = slope *  30.0 + zeroVal
		'40' : sqrt2 = slope *  40.0 + zeroVal
		'50' : sqrt2 = slope *  50.0 + zeroVal
		'60' : sqrt2 = slope *  60.0 + zeroVal
		'70' : sqrt2 = slope *  70.0 + zeroVal
		'80' : sqrt2 = slope *  80.0 + zeroVal
		'90' : sqrt2 = slope *  90.0 + zeroVal
		'100': sqrt2 = slope * 100.0 + zeroVal
	ENDCASE

	myview = [ -sqrt2*0.5, -sqrt2*0.5, sqrt2, sqrt2 ]
	IF (aspect GT 1) THEN BEGIN
		myview[0] = myview[0] - ((aspect-1.0)*myview[2])/2.0
		myview[2] = myview[2] * aspect
	ENDIF ELSE BEGIN
		myview[1] = myview[1] - (((1.0/aspect)-1.0)*myview[3])/2.0
		myview[3] = myview[3] / aspect
	ENDELSE

;	RETURN, myView

END

;----------------------------------------------------------------------------
PRO Roberto_Color, sEvent
; This event handler is to color the interfaces
        Widget_Control, sEvent.top, Get_UValue=sState, /No_Copy
        WIDGET_CONTROL, sEvent.top, /HOURGLASS

	CASE sState.realPoint OF
	0 : BEGIN
		BEEP
		text = ['You did not supply a valid pointcurv array at the begining.', $
			'',  $
			'To color the interfaces, re-run this program with a valid', $
			'pointcurv array.']
		nothing = DIALOG_MESSAGE (text)
		END

	1 : BEGIN
		CASE sState.wantColor OF
		0 : BEGIN
			CASE sState.cap OF
			0 : BEGIN
				min = sState.colorBarRange[0]
				max = sState.colorBarRange[1]
			;	color = getColors (sState.pointCurv, min, max)
				getColors, sState.pointCurv, min, max, color, $
					sState.aRed, sState.aBlue, sState.aGreen
				sState.oMyPolygons->SetProperty, VERT_COLORS=color
				sState.oBarModel->SetProperty, Hide=0
				buttonString = 'BW Surface'
				sState.wantColor = 1
				END

			1 : BEGIN
				BEEP
				message=['You can not color the interfaces if the microstructure is capped!', $
					'', $
					'To color the interface, reconstruct the interface with out capping it.']
				nothing = DIALOG_MESSAGE (message)
				END
			ENDCASE		; Of Cap
			END
		1 : BEGIN
			buttonString = 'Color Surface'
        	        sState.wantColor = 0
        	        sState.oBarModel->SetProperty, Hide=1
			sState.oMyPolygons->SetProperty, VERT_COLOR=1
			END
		ENDCASE			; Of wantColor

		END
	ENDCASE
                
        WIDGET_CONTROL, sState.wColor, SET_VALUE=buttonString
   
; Redraw the graphic.
        sState.oWindow->Draw, sState.oView

;Put the info structure back.
        Widget_Control, sEvent.top, Set_UValue=sState, /No_Copy
                        
END

;----------------------------------------------------------------------------
PRO getColors, pointCurv, minVal, maxVal, color, red, blue, green
; This function calculates the colors for the vertex points
	vertexSize    = SIZE(pointCurv)
	nVertexPoints = vertexSize[1] 
	colorVals     = INTARR( nVertexPoints )
	colorVals[*]  = 0
	valRes = 255D / ( DOUBLE(maxVal)-DOUBLE(minVal) )

	FOR I = 0L, nVertexPoints-1 DO BEGIN
		colorVals[i]  = ( ( pointCurv[I] - minVal ) * valRes )
		IF ( colorVals[I] LE 0 )   THEN colorVals[I] = 0
		IF ( colorVals[I] GE 255 ) THEN colorVals[I] = 255
	ENDFOR

	cSize     = SIZE( colorVals )
	cDim      = cSize[1]
	color     = BYTARR( 3, cDim )

	FOR I = LONG(0), cDim - 1 DO BEGIN
		J = BYTE ( colorVals[I] )
		color[0,I] = Red  [J]
		color[1,I] = Green[J]
		color[2,I] = Blue [J]
	ENDFOR

END


;----------------------------------------------------------------------------
PRO Roberto, allData, PointCurv=pointCurv

; Set it so that it multi-threakds when the array is 20,000
	CPU, TPOOL_MIN_ELTS = 20000

	FORWARD_FUNCTION normalize

	device, GET_SCREEN_SIZE=scr
	xdim = scr[0] * 0.8; * 0.8
	ydim = scr[1] * 0.8; * 0.8

	dataSize = SIZE(allData)
print,datasize
;stop
	CASE dataSize[0] OF

	2: BEGIN
		x = Findgen(dataSize[1])
		y = Findgen(dataSize[2])
		cap = 0
		END
	3: BEGIN
		threshold = 0.5
		PRINT, 'min. value: ', MIN(alldata)
		PRINT, 'max. value: ', MAX(alldata)
		READ, threshold, PROMPT = 'ENTER threshold: '
		secondPhase = 0
		;READ, secondPhase, PROMPT = 'Is there a second phase? (0=no, 1=yes): '
		IF (secondPhase EQ 1) THEN BEGIN
			READ, threshold1, PROMPT = 'What is the pixel value of the second phase?: '
		ENDIF
		calib = DINDGEN(3)     ;calibration for x,y,z
                calib[0] = 0.041958d0
                calib[1] = 0.041958d0
                calib[2] = 0.04376d0
		calib[0] = 1d0
		calib[1] = 1d0
		calib[2] = 1d0
		calibration=1
		;READ, calibration, PROMPT = 'ENTER x-calibration (micrometer per pixel): '
		;calib[0] = calibration
		;READ, calibration, PROMPT = 'ENTER y-calibration (micrometer per pixel): '
		;calib[1] = calibration
		;READ, calibration, PROMPT = 'ENTER z-calibration (micrometer per pixel): '
		;calib[2] = calibration

		;Take every dx point in the array.  Makes drawing much faster.
		;dx = 1
		;dy = 1
		;dz = 1

		; Ask if they want to cap the solid or liquid only if they are not going to color the surface
		cap=0

		IF (N_ELEMENTS(pointcurv) EQ 0) THEN colorSurf=0 ELSE colorSurf=1

		CASE colorSurf OF
			0: BEGIN
				;READ, cap, PROMPT = 'Cap the microstructure? (0=no, 1=yes):  ' 
	
				IF (cap EQ 1) THEN BEGIN
					maxV = max(allData)
					minV = min(allData)
					;tempData = alldata[0:*:dx, 0:*:dy, 0:*:dz]
					temData = allData
					solid = 1
					READ, solid, PROMPT = 'liquid or solid (0=liquid, 1=solid):  '
	
					IF (solid EQ 0) THEN BEGIN	; they want liquid
					;	tempData = -TEMPORARY(tempData) + 255B
						data = -FLOAT(TEMPORARY(tempData))
						ratio = (maxV - minV) / ( MAX(data) - MIN(data) )
						tempData = ( data - MIN(data) ) * TEMPORARY(ratio) + TEMPORARY(minV)
						data = 0B
					ENDIF
	
					sizeSurf = SIZE (tempData, /DIMENSIONS)
					allData2 = FLTARR(sizeSurf[0]+2, sizeSurf[1]+2, sizeSurf[2]+2)
					allData2[1:sizeSurf[0], 1:sizeSurf[1], 1:sizeSurf[2]] = TEMPORARY(tempData)
				ENDIF ELSE BEGIN
				;	allData2 = alldata[0:*:dx, 0:*:dy, 0:*:dz]
					allData2 = alldata
				ENDELSE
				END
			1: BEGIN
				allData2 = alldata
			;	allData2 = alldata[0:*:dx, 0:*:dy, 0:*:dz]
			;	print, 'did loop'
				END
		ENDCASE



;		IF (N_ELEMENTS(pointcurv) EQ 0) THEN BEGIN
;			READ, cap, PROMPT = 'Cap the microstructure? (0=no, 1=yes):  ' 
;
;			; If they want to cap it and if they want to do it for the solid or liquid
;			IF (cap EQ 1) THEN BEGIN
;				maxV = max(allData)
;				minV = min(allData)
;				tempData = alldata[0:*:dx, 0:*:dy, 0:*:dz]
;				solid = 1
;				READ, solid, PROMPT = 'liquid or solid (0=liquid, 1=solid):  '
;
;				IF (solid EQ 0) THEN BEGIN	; they want liquid
;				;	tempData = -TEMPORARY(tempData) + 255B
;					data = -FLOAT(TEMPORARY(tempData))
;					ratio = (maxV - minV) / ( MAX(data) - MIN(data) )
;					tempData = ( data - MIN(data) ) * TEMPORARY(ratio) + TEMPORARY(minV)
;					data = 0B
;				ENDIF
;
;				sizeSurf = SIZE (tempData, /DIMENSIONS)
;				allData2 = FLTARR(sizeSurf[0]+2, sizeSurf[1]+2, sizeSurf[2]+2)
;				allData2[1:sizeSurf[0], 1:sizeSurf[1], 1:sizeSurf[2]] = TEMPORARY(tempData)
;			ENDIF
;		ENDIF ELSE BEGIN
;			allData2 = alldata[0:*:dx, 0:*:dy, 0:*:dz]
;			print, 'did loop'
;		ENDELSE

		; now get the surface description in form of polygons
		SHADE_VOLUME, allData2, threshold, v, p, yrange=yrange, zrange=zrange

		IF (secondPhase EQ 1) THEN BEGIN
			allData3 = (allData2 EQ threshold1) * 255
			SHADE_VOLUME, allData3, threshold1, v1, p1, yrange=yrange1, zrange=zrange1
		ENDIF
;----------------------------------------------------------------------------
;	mainDir = '~/Desktop/genusNew2/SS010/'
;	dirName = 'LIQUID'
;	choice = '1'
;	READ, choice,  PROMPT = '	ENTER particle number you want to see:   '
;	filename = strcompress( mainDir + dirName + '/particle' + choice + '.dat', /remove_all )
;	restore, filename
;	restore, filename='~/desktop/liquid/3/particle1.dat'
;	v = TEMPORARY (vertex)
;	p = TEMPORARY (polygons)
;----------------------------------------------------------------------------

		; V now the vertex points and P the connections.  Recalibrate the vertex.
		maxCalib = MAX( calib )
    v[0,*] = v[0,*] * calib[0]
		v[1,*] = v[1,*] * calib[1]
		v[2,*] = v[2,*] * calib[2]

		IF (secondPhase EQ 1) THEN BEGIN
			v1[0,*] = v1[0,*] * calib[0]
			v1[1,*] = v1[1,*] * calib[1]
			v1[2,*] = v1[2,*] * calib[2]
		ENDIF

		END
	ENDCASE

; Compute viewplane rect based on aspect ratio.
	aspect = FLOAT(xdim) / FLOAT(ydim)

; get the view for the initial zoom
	zoomVal = 30		; has to be a multiple of 10 and goes from 0 to 100
	zoom = strcompress( string(zoomVal) , /remove_all )
	getView, zoom, aspect, myView

; Create view.  
	CASE dataSize[0] OF
	3: BEGIN
		oView = OBJ_NEW('IDLgrView', PROJECTION=2, EYE=3, ZCLIP=[1.4,-1.4],$
			VIEWPLANE_RECT=myview, COLOR=[0,0,0])
		END
	2: BEGIN
		oView = OBJ_NEW('IDLgrView', Color=[0,0,0], $
			Viewplane_Rect=[-1.2,-1.1,2.3,2.3])
		END
	ENDCASE

; Create models.
	oTop = OBJ_NEW('IDLgrModel')
	oGroup = OBJ_NEW('IDLgrModel')
	oTop->Add, oGroup

	aRed    = BYTARR(256)
	aGreen  = BYTARR(256)
	aBlue   = BYTARR(256)
	LOADCT, 13 , /SILENT ; 39
	TVLCT, aRed, aGreen, aBlue, /GET
	myPalette = OBJ_NEW('IDLgrPalette', aRed, aGreen, aBlue)

; Transparency stuff
	Pattern = BYTARR(2,4,4)
	Pattern[0,*,*]=255B
	Pattern[1,*,*]=50B
	oMyImage = OBJ_NEW('IDLgrImage', Pattern, interleave=0)

CASE dataSize[0] OF
	2: BEGIN
		oSurface = OBJ_NEW('IDLgrSurface', allData, x, y, Color=[255,255,255], Shading=1, Style=2)
		s = Size(alldata, /Dimensions)
		oSurface->SetProperty, Vert_Colors=Reform(BytScl(allData), s[0]*s[1]), Palette=myPalette
		oGroup -> Add, oSurface
		xrange=[ 0, dataSize[1] ]
		yrange=[ 0, dataSize[2] ]
		zrange=[ MIN(allData), MAX(allData) ]
		pColor = BYTARR(3)
		pColor = [200,200,211]
		oMyPolygons = OBJ_NEW('IDLgrPolygon')
		oMyPolygons1 = OBJ_NEW('IDLgrPolygon')

		size1 = Obj_New('IDLgrFont', 'Helvetica', Size=20)
		size2 = Obj_New('IDLgrFont', 'Helvetica', Size=24)
		xTitle = Obj_New('IDLgrText', 'X', FONT=size2, Color=[255,255,0])
		yTitle = Obj_New('IDLgrText', 'Y', FONT=size2, Color=[255,255,0])
		zTitle = Obj_New('IDLgrText', 'Z', FONT=size2, Color=[255,255,0])

		tickLens = 0.05
		minor = 4

		xAxis = Obj_New("IDLgrAxis", 0, Color=[255,255,0], Ticklen=tickLens, $
    			 TITLE=xTitle, /EXACT, RANGE=xrange, MINOR=minor )
		xAxis->GetProperty, TickText=xAxisText
		xAxisText->SetProperty, Font=size1

		yAxis = Obj_New("IDLgrAxis", 1, Color=[255,255,0], Ticklen=tickLens, $
	    		Range=yrange, /EXACT, TITLE=yTitle, MINOR=minor)
		yAxis->GetProperty, TickText=yAxisText
		yAxisText->SetProperty, Font=size1
		
		zAxis = Obj_New("IDLgrAxis", 2, Color=[255,255,0], Ticklen=tickLens, $
	    		Range=zrange, /EXACT, TITLE=zTitle, MINOR=minor)	
		zAxis->GetProperty, TickText=zAxisText
		zAxisText->SetProperty, Font=size1
	
		oGroup->Add, xAxis
		oGroup->Add, yAxis
		oGroup->Add, zAxis

		oSurface->GetProperty, XRange=xrange, YRange=yrange, ZRange=zrange
		xAxis->GetProperty, CRange=xrange
		yAxis->GetProperty, CRange=yrange
		zAxis->GetProperty, CRange=zrange

		s = Size(alldata)
		surfaceAspect = Float(s[2]) / s[1]
		windowAspect = 1.0
		sA = SIZE(surfaceAspect)
		margin = 0

		IF (surfaceAspect LE windowAspect) THEN BEGIN
			xstart = margin
			ystart = 0.5 - (0.5 - margin) * (surfaceAspect / windowAspect)
			xend = 1.0 - margin
			yend = 0.5 + (0.5 - margin) * (surfaceAspect/ windowAspect)
		ENDIF ELSE BEGIN
			xstart = 0.5 - (0.5 - margin) * ( windowAspect/ surfaceAspect)
			ystart = margin
			xend = 0.5 + (0.5 - margin) * ( windowAspect/ surfaceAspect)
			yend = 1.0 - margin
		ENDELSE

		pos = [xstart, ystart, xend, yend]
		pos = [pos[0], pos[2], pos[1], pos[3], 0.0, 1.0] - 0.5
		positions = [ pos[0], pos[1] ]
		xs = Normalize(xrange, Position = positions)
		ys = Normalize(yrange, Position = positions)
		zs = Normalize(zrange, Position = positions)
		xAxis->SetProperty, Location=[9999.0, pos[2], pos[4]], XCoord_Conv=xs
		yAxis->SetProperty, Location=[pos[0], 9999.0, pos[4]], YCoord_Conv=ys
		zAxis->SetProperty, Location=[pos[0],  pos[3], 9999.0], ZCoord_Conv=zs
		oSurface->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs

		END

	3: BEGIN
		; Compute data bounds. 
		sz = SIZE(allData2)
		szMax = float(max(sz[1:3]))

		; uncomment the following if you want to have the axis to only bound the what you can see
		; to do this, you must uncomment three more lines down bellow
		; v[0,*] = v[0,*] - min (v[0,*])
		; v[1,*] = v[1,*] - min (v[1,*])
		; v[2,*] = v[2,*] - min (v[2,*])

		xVals = v[0, *]
		yVals = v[1, *]
		zVals = v[2, *]

		xMax = MAX(xVals)
		yMax = MAX(yVals)
		zMax = MAX(zVals)
		xMin = MIN(xVals)
		yMin = MIN(yVals)
		zMin = MIN(zVals)

		IF (secondPhase EQ 1) THEN BEGIN
			xVals1 = v1[0, *]
			yVals1 = v1[1, *]
			zVals1 = v1[2, *]

			xCombSize = size (v, /dimensions)
			xCombSize1 = size (v1, /dimensions)
			xComb = FLTARR (xCombSize[1] + xCombSize1[1])
			yComb = xComb
			zComb = xComb
			xComb[ 0:xCombSize[1]-1] = v[0, *]
			xComb[ xCombSize[1]:xCombSize[1]+xCombSize1[1]-1 ] = v1[0, *]
			yComb[ 0:xCombSize[1]-1] = v[1, *]
			yComb[ xCombSize[1]:xCombSize[1]+xCombSize1[1]-1 ] = v1[1, *]
			zComb[ 0:xCombSize[1]-1] = v[1, *]
			zComb[ xCombSize[1]:xCombSize[1]+xCombSize1[1]-1 ] = v1[1, *]

			xMax = MAX(xComb)
			yMax = MAX(yComb)
			zMax = MAX(zComb)
			xMin = MIN(xComb)
			yMin = MIN(yComb)
			zMin = MIN(zComb)
		ENDIF

		xmm = xMax - xMin
		ymm = yMax - yMin
		zmm = zMax - zMin
		xyzmm = [xmm, ymm, zmm ]
		xyzSpan = MAX( xyzmm )
		xs = [0.0,1.0/xyzSpan]
		ys = [0.0,1.0/xyzSpan]
		zs = [0.0,1.0/xyzSpan]
		xs[1] = MIN( [xs[1], ys[1], zs[1]] ) * 0.9
		ys[1] = xs[1]
		zs[1] = ys[1]
		xs[0] = -0.5*(xMax+xMin)*xs[1]
		ys[0] = -0.5*(yMax+yMin)*ys[1]
		zs[0] = -0.5*(zMax+zMin)*zs[1]

		; Initial polygon colors
		pColor = BYTARR(3)
		;pColor = [200,200,211]
                pColor = [100,200,200]

;		restore, filename='~/Desktop/CP/codes/color.dat'
;		oMyPolygons = OBJ_NEW('IDLgrPolygon', xVals, yVals, zVals, $
;			POLYGONS=p, VERT_COLOR=color, SHADING=1)

		oMyPolygons = OBJ_NEW('IDLgrPolygon', xVals, yVals, zVals, $
			POLYGONS=p, COLOR=pColor, SHADING=1)

		IF (secondPhase EQ 1) THEN BEGIN
			oMyPolygons1 = OBJ_NEW('IDLgrPolygon', xVals1, yVals1, zVals1, $
				POLYGONS=p1, COLOR=[255, 0, 0], SHADING=1)
			oGroup->Add, oMyPolygons1
		ENDIF ELSE BEGIN
			oMyPolygons1 = OBJ_NEW('IDLgrPolygon', xVals, yVals, zVals, $
				POLYGONS=p, COLOR=pColor, SHADING=1, hide=1)
			oGroup->Add, oMyPolygons1
		ENDELSE

; ************************************************
;		restore, filename='~/desktop/igor/964/velo.dat'
;		sizeSurf = SIZE (velo, /DIMENSIONS)
;		velo2 = FLTARR(sizeSurf[0]+2, sizeSurf[1]+2, sizeSurf[2]+2)
;		velo2[1:sizeSurf[0], 1:sizeSurf[1], 1:sizeSurf[2]] = TEMPORARY(velo)
;		velocityVals = INTERPOLATE ( TEMPORARY(velo2), xVals, yVals, zVals )
;		minVel = -.007
;		maxVel =  .007
;		velRes = 255 / (maxVel-minVel)
;		numVertex = size (v, /DIMENSIONS)
;		colorVals = INTARR( numVertex[1] )
;		FOR i = 0L, numVertex[1]-1 DO BEGIN
;			colorVals[i]  = ( ( velocityVals[i] - minVel ) * velRes )
;			IF ( colorVals[i] LE 0 )   THEN colorVals[i] = 0 
;			IF ( colorVals[i] GE 255 ) THEN colorVals[i] = 255
;		ENDFOR
;
;		cSize     = SIZE( colorVals )
;		cDim      = cSize[1]
;		color     = BYTARR( 3, cDim )
;		FOR i = LONG(0), cDim - 1 DO BEGIN
;			j = BYTE ( colorVals[i] )
;			color[0,i] = aRed[j]
;			color[1,i] = aGreen[j]
;			color[2,i] = aBlue[j]
;		ENDFOR

;		restore, filename='~/Desktop/CP/codes/color.dat'
;		oMyPolygons = OBJ_NEW('IDLgrPolygon', xVals, yVals, zVals, $
;			POLYGONS=p, VERT_COLOR=color, SHADING=1)

; For Katsuyo.  This part colors the 3D stack with by height
;		zvals = zvals-40.0
;		zvals = -zvals
;		c0max = Max(zvals)
;		c0min = Min(zvals)
;		zPoints = SIZE (zvals, /DIMENSIONS)
;		zPoints = zPoints[1]
;		c0fact = 255.0 / (c0max-c0min)
;		color0 = BYTARR( zPoints )
;		FOR i=LONG(0), zPoints-1 DO BEGIN
;			meanColor  = ( ( zvals[i] - c0min ) * c0fact )
;			IF ( meanColor GE 0.0 ) AND ( meanColor LE 255.0 ) THEN color0[i] = BYTE(meanColor) 
;			IF ( meanColor LT 0.0 )   THEN color0[i] = 0B
;			IF ( meanColor GT 255.0 ) THEN color0[i] = 255B
;		ENDFOR
;		aRed      = BYTARR(256)
;		aGreen    = BYTARR(256)
;		aBlue     = BYTARR(256)
;		LOADCT, 13 , /SILENT ;39
;		TVLCT, aRed, aGreen, aBlue, /GET
;		cSize     = SIZE( color0 )
;		cDim      = cSize[1]
;		color     = BYTARR( 3, cDim )
;		FOR i = LONG(0), cDim - 1 DO BEGIN
;			j = BYTE ( color0[i] )
;			color[0,i] = aRed[j]
;			color[1,i] = aGreen[j]
;			color[2,i] = aBlue[j]
;		ENDFOR
;		oMyPolygons = OBJ_NEW('IDLgrPolygon', xVals, yVals, zVals, $
;			POLYGONS=p, VERT_COLOR=color, SHADING=1)
; ************************************************

		oGroup->Add, oMyPolygons

		s = SIZE(alldata2)

;		xrange=[ 0, s[1] ]
;		yrange=[ 0, s[2] ]
;		zrange=[ 0, s[3] ]
    xrange=[ 0, s[1]* calib[0] ]
    yrange=[ 0, s[2]* calib[1] ]
    zrange=[ 0, s[3]* calib[2] ]
		; uncomment this if you want the axis to only bound what you can see
		; xrange=[ xMin, xMax ]
		; yrange=[ yMin, yMax ]
		; zrange=[ zMin, zMax ]

		PRINT, '   Dimensions of X, Y, and Z in �m: ', ROUND(xrange[1]), ROUND(yrange[1]), ROUND(zrange[1])
		PRINT, '   Approximate number of triangles: ', MESH_NUMTRIANGLES(p)

		sizeData = SIZE(allData, /DIMENSIONS)
		sizes     = [ xrange[1], yrange[1], zrange[1] ]
		maxLength = MAX(sizes)

		size1 = Obj_New('IDLgrFont', 'Helvetica', Size=14)
		size2 = Obj_New('IDLgrFont', 'Helvetica', Size=16)
 ;[mm]
		;xTitle = Obj_New('IDLgrText', 'X [�m]', FONT=size2, Color=[255,255,0])
		;yTitle = Obj_New('IDLgrText', 'Y [�m]', FONT=size2, Color=[255,255,0])
		;zTitle = Obj_New('IDLgrText', 'Z [�m]', FONT=size2, Color=[255,255,0])
		xTitle = Obj_New('IDLgrText', 'X', FONT=size2, Color=[255,255,0])
    yTitle = Obj_New('IDLgrText', 'Y', FONT=size2, Color=[255,255,0])
    zTitle = Obj_New('IDLgrText', 'Z', FONT=size2, Color=[255,255,0])

		xAxis = Obj_New("IDLgrAxis", 0, Color=[255,255,0], Ticklen=-1.0*maxLength/60.0, $
    			 TITLE=xTitle, /EXACT, RANGE=xrange, MINOR=3, MAJOR=3) ; , MINOR=3 MINOR=0, MAJOR=0
		xAxis->GetProperty, TickText=xAxisText
		xAxisText->SetProperty, Font=size1

		yAxis = Obj_New("IDLgrAxis", 1, Color=[255,255,0], Ticklen=-1.0*maxLength/60.0, $
	    		Range=yrange, /EXACT, TITLE=yTitle, MINOR=3, MAJOR=3) ; , MINOR=3
		yAxis->GetProperty, TickText=yAxisText
		yAxisText->SetProperty, Font=size1
		
		zAxis = Obj_New("IDLgrAxis", 2, Color=[255,255,0], Ticklen=-1.0*maxLength/60.0, $
	   		/EXACT, TITLE=zTitle, Range=zrange, MINOR=3)	; MAJOR=2, MINOR=3
		zAxis->GetProperty, TickText=zAxisText
		zAxisText->SetProperty, Font=size1
	
		oGroup->Add, xAxis
		oGroup->Add, yAxis
		oGroup->Add, zAxis

		oGroup->Scale,     xs[1],ys[1],zs[1]
		oGroup->Translate, xs[0],ys[0],zs[0]

	;	IF (secondPhase EQ 1) THEN BEGIN
	;		oGroup1->Scale,     xs[1],ys[1],zs[1]
	;		oGroup1->Translate, xs[0],ys[0],zs[0]
	;	ENDIF

		END
	ENDCASE

; Create some lights.  Making two lighting models
	oL1 = OBJ_NEW('IDLgrModel')
	oL2 = OBJ_NEW('IDLgrModel')
	oL3 = OBJ_NEW('IDLgrModel')
	oTop->Add, oL1
	oTop->Add, oL2
	oTop->Add, oL3

	CASE dataSize[0] OF
		2: oL2->SetProperty, HIDE=1
		3: BEGIN
			oL1->SetProperty, HIDE=1
			oL3->SetProperty, HIDE=1
		END
	ENDCASE

	; Lighting Option #2
	oLight1 = OBJ_NEW('IDLgrLight', LOCATION=[-2*xrange[1], -yrange[1], 2*zrange[1]], $
		TYPE=1, INTENSITY=0.4)
	oL1->Add, oLight1
	oLight2 = OBJ_NEW('IDLgrLight', TYPE=1, INTENSITY=0.7, $	; .7
		Location  = [1.5*xrange[1], yrange[0], zrange[1]/2], $	 
		Direction = [-xrange[1]/2, yrange[1], zrange[1]/2])	
	oL1->Add, oLight2
	oLight3 = OBJ_NEW('IDLgrLight', TYPE=1, INTENSITY=0.7, $	; .7
		Location  = [-xrange[1]/2, yrange[0], zrange[1]/2], $	 
		Direction = [1.5*xrange[1], yrange[1], zrange[1]/2])	
	oL1->Add, oLight3
	oLight4 = OBJ_NEW('IDLgrLight', TYPE=1, INTENSITY=0.7, $	; .7
		Location  = [xrange[1]/2, yrange[1]/2, -zrange[1]], $	 
		Direction = [xrange[1]/2, yrange[1]/2, zrange[1]])	
	oL1->Add, oLight4
	oLight5 = OBJ_NEW('IDLgrLight', TYPE=1, INTENSITY=.7, $		; 1
		Location  = [xrange[1]/2, yrange[0], zrange[1]/4], $	 
		Direction = [xrange[1]/2, yrange[1], zrange[1]/4])	
	oL1->Add, oLight5
	oLight6 = OBJ_NEW('IDLgrLight', TYPE=1, INTENSITY=.7, $	; .7
		Location  = [xrange[1]/2, yrange[0], -zrange[1]], $	 
		Direction = [xrange[1]/2, 3*yrange[1]/4, zrange[1]])	
	oL1->Add, oLight6
	oLight6->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs


	; Default OPTION
	ambientLight = Obj_New('IDLgrLight', Type=0, Intensity=0.2)	; 0.3
	rotatingLight = Obj_New('IDLgrLight', Type=1, Intensity=0.60, $	; .5
		Location=[xrange[1], yrange[1], 4*zrange[1]], $	 
		Direction=[xrange[0], yrange[0], zrange[0]])
	fillLight = Obj_New('IDLgrLight', Type=3, Intensity=0.2, $ 	; .2
		Location=[(xrange[1]-xrange[0])/2.0, (yrange[1]-yrange[0])/2.0, -2*Abs(zrange[0])], $ 	
		Direction=[(xrange[1]-xrange[0])/2.0, (yrange[1]-yrange[0])/2.0, zrange[1]])		
	nonrotatingLight = Obj_New('IDLgrLight', Type=1, Intensity=0.4, $	; .4
		Location=[-xrange[1], (yrange[1]-yrange[0])/2.0, 4*zrange[1]], $
		Direction=[xrange[1], (yrange[1]-yrange[0])/2.0, zrange[0]])

;	testLight = Obj_New('IDLgrLight', Type=1, Intensity=0.6, $	; .4
;		Location=[-xrange[1]/2.0, yrange[1], -zrange[1]], $
;		Direction=[xrange[1], (yrange[1]-yrange[0])/2.0, zrange[1]])
;	testLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
;	oL2->Add, testLight

	rotatingLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
	fillLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
	nonrotatingLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs

	oL2->Add, ambientLight
	oL2->Add, fillLight
	oL2->Add, rotatingLight
	oL2->Add, nonrotatinglight

	; THIRD LIGHT OPTION
	ambientLight2 = Obj_New('IDLgrLight', Type=0, Intensity=0.2)
	rotatingLight2 = Obj_New('IDLgrLight', Type=2, Intensity=0.40, $
		Location=[xrange[1], yrange[1], 4*zrange[1]],$	; +
		Direction=[xrange[0], yrange[0], zrange[0]])		; +0
	fillLight2 = Obj_New('IDLgrLight', Type=3, Intensity=0.7, $ 
		Location=[(xrange[1]-xrange[0])/2.0, (yrange[1]-yrange[0])/2.0, -2*Abs(zrange[0])], $ 	
		Direction=[(xrange[1]-xrange[0])/2.0, (yrange[1]-yrange[0])/2.0, zrange[1]])		
	nonrotatingLight2 = Obj_New('IDLgrLight', Type=1, Intensity=0.3, $
		Location=[-xrange[1], (yrange[1]-yrange[0])/2.0, 4*zrange[1]], $
		Direction=[xrange[1], (yrange[1]-yrange[0])/2.0, zrange[0]])

	rotatingLight2->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
	fillLight2->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
	nonrotatingLight2->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs

	testLight = Obj_New('IDLgrLight', Type=1, Intensity=0.3, $; , color=[255,0,0],$ 
		Location =[xrange[0], -((yrange[1]-yrange[0])/2.0), 16*Abs(zrange[1])], $
		Direction=[(xrange[1]-xrange[0])/2.0, yrange[1], -zrange[1]])
	testLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs

	oL3->Add, ambientLight2
	oL3->Add, fillLight2
	oL3->Add, rotatingLight2
	oL3->Add, nonrotatinglight2
;	oL3->Add, testLight

; Place the top model in the view.
	oView->Add, oTop

;
;--------------------------     INITIAL ROTATION       -----------------------
;
; Rotate to standard view for first draw.
  oGroup->Rotate, [1,0,0], -70  ; -40
  oGroup->Rotate, [0,1,0], 30 ; 20
  oGroup->Rotate, [0,0,1], 10  ; 0
; 0,50,90
; Color Bar initial range
	cbMin = -0.1; -0.03
	cbMax =  0.1; 0.03

; Color Bar absolute max and min
	cbAMin = -5.0
	cbAMax =  5.0

; Color bar models that I will use later
	colorBarRange = [ cbMin, cbMax ]
	oBarModel = OBJ_NEW('IDLgrModel')
	oTop->Add, oBarModel
	  want=1
	  
	  name = 'Empty'  
	IF (colorSurf EQ 1) THEN BEGIN 
  READ, want, PROMPT = 'what are you going to see? (1=H, 2=K, 3=DHDt, 4=DKDt, 5=velocity, 6=others)  ' 



  CASE want OF
    1: name = 'Mean Curvature'
    2: name = 'Gaussian Curvature'
    3: name = 'DH/Dt convective'
    4: name = 'DK/Dt convective'
    5: name = 'Normal Velocity'
    6: READ, name, PROMPT = 'Name of color bar (ex: Mean Curvature) :     ' 
  ENDCASE
  ENDIF
  labelString = STRARR(1)
  labelString = name
	oColorBar = Obj_New('VColorBar',title=labelString, Palette=myPalette, Range=colorBarRange, $
		Color=[255,255,255], Minor=3, Position=[0.80,-0.40,0.85,0.40])
	oBarModel->Add, oColorBar
	oBarModel->SetProperty, HIDE=1


; Create a trackball.
	oTrack = OBJ_NEW('Trackball', [xdim/2.0, ydim/2.0], xdim/2.0)

; Check to see if there was a pointcurv array supplied, if not, create a fake pointcurv array
	IF (Keyword_Set(pointCurv) EQ 0) THEN BEGIN
		pointcurv = DBLARR(2,10)
		realPoint = 0
	ENDIF ELSE BEGIN
		realPoint = 1
	ENDELSE

; Create a holder object for easy destruction.
	oHolder = OBJ_NEW('IDL_Container')
	oHolder->Add, oView
	oHolder->Add, oTrack

; Get the transformation so that later we can get back to original view
	oGroup->GetProperty, Transform=origTransform

; Create the widgets.
	wBase = WIDGET_BASE(/COLUMN, XPAD=0, YPAD=0, $
		TITLE="ROBERTO", /TLB_KILL_REQUEST_EVENTS, $
		TLB_FRAME_ATTR=1, MBAR=barBase)

;Create the menu bar.
; FILE Menu
	fileMenu = WIDGET_BUTTON(barBase, VALUE='File', /MENU)

	QuitButton = WIDGET_BUTTON(fileMenu, VALUE='Quit', UVALUE='QUIT', $
   		UNAME='sState:quit')
; OUTPUT Menu
	outputMenu = WIDGET_BUTTON(barBase, VALUE='Output', /MENU)

	button = Widget_Button(outputMenu, Value='TIFF File', $
 		UValue='TIFF', Event_Pro='Roberto_Output')
	button = Widget_Button(outputMenu, Value='JPEG File', $
		UValue='JPEG', Event_Pro='Roberto_Output')
	button = Widget_Button(outputMenu, Value='EPS File', $
		UValue='EPS', Event_Pro='Roberto_Output')
	button = Widget_Button(outputMenu, Value='MOVIE 2D', $
		UValue='MOVIE2D', Event_Pro='Roberto_Output')
	button = Widget_Button(outputMenu, Value='MOVIE 3D', $
		UValue='MOVIE3D', Event_Pro='Roberto_Output')

; PROPERTIES Menu
	propMenu = WIDGET_BUTTON(barBase, VALUE='Properties', /MENU)

   ; Background Color
	bcolor = Widget_Button(propMenu, Value='Background Color', /Menu)
	dummy = Widget_Button(bcolor, Value='Black', $
		Event_Pro='Roberto_Properties', UValue='BBLACK')
	dummy = Widget_Button(bcolor, Value='White', $
		Event_Pro='Roberto_Properties', UValue='BWHITE')
	dummy = Widget_Button(bcolor, Value='Charcoal', $
		Event_Pro='Roberto_Properties', UValue='BCHARCOAL')
	dummy = Widget_Button(bcolor, Value='Gray', $
		Event_Pro='Roberto_Properties', UValue='BGRAY')

   ; Axes Color
	acolor = Widget_Button(propMenu, Value='Axes Color', /Menu)
	dummy = Widget_Button(acolor, Value='Black', $
	   Event_Pro='Roberto_Properties', UValue='ABLACK')
	dummy = Widget_Button(acolor, Value='White', $
	   Event_Pro='Roberto_Properties', UValue='AWHITE')
	dummy = Widget_Button(acolor, Value='Yellow', $
	   Event_Pro='Roberto_Properties', UValue='AYELLOW')
	dummy = Widget_Button(acolor, Value='Green', $
	   Event_Pro='Roberto_Properties', UValue='AGREEN')
	dummy = Widget_Button(acolor, Value='Navy Blue', $
	   Event_Pro='Roberto_Properties', UValue='ANAVY')

   ; Drag Quality
 	dragButton = Widget_Button(propMenu, VALUE="Drag Quality", /MENU)
	dummy = WIDGET_BUTTON(DragButton, $
	   VALUE='Low', UVALUE='LOW', Event_Pro='Roberto_Properties')
	dummy = WIDGET_BUTTON(DragButton, $
	   VALUE='Medium', UVALUE='MEDIUM', Event_Pro='Roberto_Properties')
	dummy = WIDGET_BUTTON(DragButton, $
	   VALUE='High', UVALUE='HIGH', Event_Pro='Roberto_Properties')

   ; Original View
	OGViewButton = Widget_Button(propMenu, VALUE="Reset", $
		/SEPARATOR, UVALUE='OG', Event_Pro='Roberto_Properties')

; ZOOM Menu
	zoomMenu = WIDGET_BUTTON(barBase, VALUE='Zoom', /MENU)
	button = Widget_Button(zoomMenu, Value='0', $
 		UValue='0', Event_Pro='Roberto_Zoom')
	button = Widget_Button(zoomMenu, Value='10', $
 		UValue='10', Event_Pro='Roberto_Zoom')
	button = Widget_Button(zoomMenu, Value='20', $
 		UValue='20', Event_Pro='Roberto_Zoom')
	button = Widget_Button(zoomMenu, Value='30', $
 		UValue='30', Event_Pro='Roberto_Zoom')
	button = Widget_Button(zoomMenu, Value='40', $
 		UValue='40', Event_Pro='Roberto_Zoom')
	button = Widget_Button(zoomMenu, Value='50', $
 		UValue='50', Event_Pro='Roberto_Zoom')
	button = Widget_Button(zoomMenu, Value='60', $
 		UValue='60', Event_Pro='Roberto_Zoom')
	button = Widget_Button(zoomMenu, Value='70', $
 		UValue='70', Event_Pro='Roberto_Zoom')
	button = Widget_Button(zoomMenu, Value='80', $
 		UValue='80', Event_Pro='Roberto_Zoom')
	button = Widget_Button(zoomMenu, Value='90', $
 		UValue='90', Event_Pro='Roberto_Zoom')
	button = Widget_Button(zoomMenu, Value='100', $
 		UValue='100', Event_Pro='Roberto_Zoom')

; LIGHTING Menu
	helpMenu = WIDGET_BUTTON(barBase, VALUE='Lighting', /MENU)
	button = Widget_Button(helpMenu, Value='Scheme 1 (default)', $
 		UValue='ONE', Event_Pro='Roberto_Properties')
	button = Widget_Button(helpMenu, Value='Scheme 2', $
 		UValue='TWO', Event_Pro='Roberto_Properties')
	button = Widget_Button(helpMenu, Value='Scheme 3', $
 		UValue='THREE', Event_Pro='Roberto_Properties')

; HELP Menu
	helpMenu = WIDGET_BUTTON(barBase, VALUE='Help', /MENU)
	button = Widget_Button(helpMenu, Value='About', $
 		UValue='ABOUT', Event_Pro='Roberto_Help')
	button = Widget_Button(helpMenu, Value='Help', $
 		UValue='HELP', Event_Pro='Roberto_Help')

; Create a sub base of the top base (wBase).
	wSubBase = WIDGET_BASE(wBase, COLUMN=2)

; Left Side with all of the controls
	wLeftbase = WIDGET_BASE(wSubBase, COLUMN=1)

	wScalingBase = WIDGET_BASE(wLeftBase, /COLUMN, YPAD=10)

	scalingString = 'Zoom : ' + zoom + ' %'
	wScalingLabel = WIDGET_LABEL(wScalingBase, VALUE=scalingString)

	divider = WIDGET_LABEL(wLeftBase, VALUE='----------------------', /ALIGN_CENTER)
	wLabel = WIDGET_LABEL(wLeftBase, VALUE='Polygon Colors')
	rSlider = WIDGET_SLIDER(wLeftBase, MINIMUM=0, MAXIMUM=255, TITLE='Red', $
		VALUE=pColor[0], UVALUE='RED', Event_Pro='Roberto_Properties')
	gSlider = WIDGET_SLIDER(wLeftBase, MINIMUM=0, MAXIMUM=255, TITLE='Green', $
		VALUE=pColor[1], UVALUE='GREEN', Event_Pro='Roberto_Properties')
	bSlider = WIDGET_SLIDER(wLeftBase, MINIMUM=0, MAXIMUM=255, TITLE='Blue', $
		VALUE=pColor[2], UVALUE='BLUE', Event_Pro='Roberto_Properties')
	saveView = WIDGET_BUTTON(wLeftBase, Value='Reset Colors', Uvalue='RESETC', $
		Event_Pro='Roberto_Properties', /ALIGN_CENTER)

	divider = WIDGET_LABEL(wLeftBase, VALUE='----------------------', /ALIGN_CENTER)
	wShowAxis = WIDGET_BUTTON(wLeftBase, Value='Hide Axis', Uvalue='AXIS', $
		Event_Pro='Roberto_Properties', /ALIGN_CENTER)

	divider = WIDGET_LABEL(wLeftBase, VALUE='----------------------', /ALIGN_CENTER)
	wColor = WIDGET_BUTTON(wLeftBase, Value='Color Surface', Uvalue='COLOR', $
		Event_Pro='Roberto_Color', /ALIGN_CENTER)
	wantColor = 0
	maxSlider = CW_FSLIDER (wLeftBase, TITLE='Maximum Color Bar Value', MINIMUM=cbAMin, $
		MAXIMUM=cbAMax, /EDIT, UVALUE='MAX', VALUE=cbMax)
	minSlider = CW_FSLIDER (wLeftBase, TITLE='Minimum Color Bar Value', $
		MINIMUM=cbAMin, MAXIMUM=cbAMax, /EDIT, UVALUE='MIN', VALUE=cbMin)

	divider = WIDGET_LABEL(wLeftBase, VALUE='----------------------', /ALIGN_CENTER)
	wTransparent = WIDGET_BUTTON(wLeftBase, Value='Transparent', Uvalue='TRANSPARENT', $
		Event_Pro='Roberto_Properties', /ALIGN_CENTER)
	tSlider = WIDGET_SLIDER(wLeftBase, MINIMUM=0, MAXIMUM=255, TITLE='Transparency Slider', $
		VALUE=255, UVALUE='TSLIDER', Event_Pro='Roberto_Properties')

	divider = WIDGET_LABEL(wLeftBase, VALUE='----------------------', /ALIGN_CENTER)
;	saveView = WIDGET_BUTTON(wLeftBase, Value='Save Settings', Uvalue='SAVE', $
;		Event_Pro='Roberto_Settings', /ALIGN_CENTER)
;	saveView = WIDGET_BUTTON(wLeftBase, Value='Get Settings', Uvalue='GET', $
;		Event_Pro='Roberto_Settings', /ALIGN_CENTER)
	saveView = WIDGET_BUTTON(wLeftBase, Value='Reset Orientation', Uvalue='RESET', $
		Event_Pro='Roberto_Properties', /ALIGN_CENTER)

; Create the right Base that has the drawing area.
	wRightbase = WIDGET_BASE(wSubBase)
	wDraw = WIDGET_DRAW(wRightBase, GRAPHICS_LEVEL=2, XSIZE=xdim, YSIZE=ydim, $
		/EXPOSE_EVENTS, /BUTTON_EVENTS, UVALUE='DRAW', RETAIN=0 )

	wGuiBase = WIDGET_BASE(wBase, /ROW)
	WIDGET_CONTROL, wBase, /REALIZE
	WIDGET_CONTROL, /HOURGLASS

; Get the window id of the drawable.
	WIDGET_CONTROL, wDraw, GET_VALUE=oWindow

; Save state
	sState = {btndown: 0b,			$ 
		dragq: 0,			$	; Drag Quality
		viewPlane: myView,		$	; View Plane
		oHolder: oHolder,		$	; Holder
		oTrack:oTrack,			$ 	; Track Ball
		wDraw: wDraw,			$	; Draw
		oWindow: oWindow,		$	; Window
		oMyPolygons: oMyPolygons,	$	; Polygons
		oMyPolygons1: oMyPolygons1,	$	; Polygons for second phase
		oMyImage:oMyImage,		$	; Transparency Stuff
		pattern:pattern, 		$	; Patern for transparency
		wTransparent:wTransparent, 	$	; Transparent Button
		tSlider:tSlider, 		$	; Transparency Slider
		wColor:wColor, 			$	; Color interfaces button
		wantColor:wantColor, 		$	; Do they want color
		cap:cap, 			$	; Is the microstructure capped?
		origTransform: origTransform,	$	; OG zoom
		zoomVal:zoomVal, 		$	; Zoom Value
		oView: oView,			$	; View
		oL1:oL1, 			$	; Light Scheme 1
		oL2:oL2, 			$	; Light Scheme 2
		oL3:oL3, 			$	; Light Scheme 3
		xAxis:xAxis, 			$	; The X Axis object
		yAxis:yAxis, 			$	; The Y Axis object
		zAxis:zAxis, 			$	; The Z Axis object
		rSlider:rSlider,		$	; Red Slider
		gSlider:gSlider,		$	; Green Slider
		bSlider:bSlider,		$	; Blue Slider
		pColor:pColor, 			$	; OG Polygon Colors
		minSlider:minSlider,		$	; Minumum Slider (Color Bar)
		maxSlider:maxSlider, 		$	; Maximum Slider (Color Bar)
		oColorBar:oColorBar, 		$	; Color Bar
		colorBarRange:colorBarRange, 	$	; Color Bar Range
		aRed:aRed, 			$	; red
		aBlue:aBlue, 			$	; blue
		aGreen:aGreen, 			$	; green
		pointCurv:pointCurv, 		$	; Point Curv Array
		realPoint:realPoint, 		$	; Is it a real pointcurv?
		aspect:aspect,	 		$ 	; Aspect Ratio
		wScalingLabel: wScalingLabel, 	$	; Scaling String
		wShowAxis: wShowAxis, 		$	; Show/Hide Axis
		oBarModel:oBarModel, 		$	; Color Bar Model
		oGroup: oGroup			$	; Group
		}  

	WIDGET_CONTROL, wBase, SET_UVALUE=sState, /NO_COPY
	XMANAGER, 'Roberto', wBase, /NO_BLOCK

	PRINT, '	------------------------------------------'
	PRINT, '	---------------ROBERTO DONE---------------'
	PRINT, '	------------------------------------------'
END



;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
;--------FROM HERE DOWN IS JUST TO MAKE THE COLOR BAR---------------------
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
;+
; NAME:
;       VCOLORBAR
;
; FILENAME:
;
;       vcolorbar__define.pro
;
; PURPOSE:
;
;       The purpose of this program is to create a vertical
;       colorbar object to be used in conjunction with other
;       IDL 5 graphics objects.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;       IDL Object Graphics.
;
; CALLING SEQUENCE:
;
;       thisColorBar = Obj_New('VColorBar')
;
; REQUIRED INPUTS:
;
;       None.
;
; INIT METHOD KEYWORD PARAMETERS:
;
;       COLOR: A three-element array representing the RGB values of a color
;          for the colorbar axes and annotation. The default value is
;          white: [255,255,255].
;
;       NAME: The name associated with this object.
;
;       NCOLORS: The number of colors associated with the colorbar. The
;          default is 256.
;
;       MAJOR: The number of major tick divisions on the colorbar axes.
;          The default is 5.
;
;       MINOR: The number of minor tick marks on the colorbar axes.
;          The default is 4.
;
;       PALETTE: A palette object for the colorbar. The default palette
;           is a gray-scale palette object.
;
;       POSITION: A four-element array specifying the position of the
;           colorbar in the arbitary coordinate system of the viewplane
;           rectangle. The default position is [0.90, 0.10, 0.95, 0.90].
;
;       RANGE: The range associated with the colorbar axis. The default
;           is [0, NCOLORS].
;
;       TITLE: A string containing a title for the colorbar axis
;           annotation. The default is a null string.
;
; OTHER METHODS:
;
;       Clamp (Procedure): Given a two-element array in the data range of
;          the colorbar, the colorbar image is clamped to this range. In
;          other words, the range of colors is clamped to the specified
;          range. Values above or below the range in the colorbar are set to
;          the minimum and maximum range values, respectively.
;
;       GetProperty (Procedure): Returns colorbar properties in keyword
;          parameters as defined for the INIT method. Keywords allowed are:
;
;               COLOR
;               MAJOR
;               MINOR
;               NAME
;               PALETTE
;               POSITION
;               RANGE
;               TITLE
;               TRANSFORM
;
;       SetProperty (Procedure): Sets colorbar properties in keyword
;          parameters as defined for the INIT method. Keywords allowed are:
;
;               COLOR
;               NAME
;               MAJOR
;               MINOR
;               PALETTE
;               POSITION
;               RANGE
;               TITLE
;               TRANSFORM
;
; SIDE EFFECTS:
;
;       A VCOLORBAR object is created. The colorbar INHERITS IDLgrMODEL.
;       Thus, all IDLgrMODEL methods and keywords can also be used. It is
;       the model that is selected in a selection event, since the SELECT_TARGET
;       keyword is set for the model.
;
; RESTRICTIONS:
;
;       None.
;
; EXAMPLE:
;
;       To create a colorbar object and add it to a plot view object, type:
;
;       thisColorBarObject = Obj_New('VColorBar')
;       plotView->Add, thisColorBarObject
;       plotWindow->Draw, plotView
;-
;
;###########################################################################


FUNCTION Normalize, range, Position=position

    ; This is a utility function to calculate the scale factor
    ; required to position a vector of specified range at a
    ; specific position given in normalized coordinates.

IF (N_Elements(position) EQ 0) THEN position = [0.0, 1.0]

scale = [((position[0]*range[1])-(position[1]*range[0])) / $
    (range[1]-range[0]), (position[1]-position[0])/(range[1]-range[0])]

RETURN, scale
END
;-------------------------------------------------------------------------


PRO VColorBar::Clamp, datarange

; This method clamps the data to a particular data range.

self->GetProperty, Range=currentRange

thisclamp = Bytscl(datarange, Max=currentRange[1], Min=currentRange[0])
bar = BytScl(Replicate(1B,10) # Bindgen(self.ncolors), Min=thisclamp[0], Max=thisclamp[1])
self.thisImage->SetProperty, Data=bar
END
;-------------------------------------------------------------------------



FUNCTION VColorBar::INIT, Position=position, $
    NColors=ncolors, Title=title, Palette=palette, $
    Major=major, Minor=minor, Range=range, Color=color, $
    _Extra=extra, Name=name

   ; Catch possible errors.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Dialog_Message(!Error_State.Msg)
   Message, !Error_State.Msg, /Informational
   RETURN, 0
ENDIF

   ; Initialize model superclass.

IF (self->IDLgrModel::Init(_EXTRA=extra) NE 1) THEN RETURN, 0

    ; Define default values for keywords, if necessary.

IF N_Elements(name) EQ 0 THEN name=''
IF N_Elements(color) EQ 0 THEN self.color = [0,0,0] $
   ELSE self.color = color
thisFont = Obj_New('IDLgrFont', 'Helvetica', Size=11.0)
self.thisFont = thisFont
IF N_Elements(title) EQ 0 THEN title=''

thisTitle = Obj_New('IDLgrText', title, Color=self.color, $
    Font=thisFont, Recompute_Dimensions=2, /Enable_Formatting, _Extra=extra)

IF N_Elements(ncolors) EQ 0 THEN self.ncolors = 256 $
   ELSE self.ncolors = ncolors
IF N_Elements(palette) EQ 0 THEN BEGIN
    red = (green = (blue = BIndGen(self.ncolors)))
    self.palette = Obj_New('IDLgrPalette', red, green, blue)
ENDIF ELSE self.palette = palette
IF N_Elements(range) EQ 0 THEN self.range = [0, self.ncolors] $
   ELSE self.range = range
IF N_Elements(major) EQ 0 THEN self.major = 5 $
   ELSE self.major = major
IF N_Elements(minor) EQ 0 THEN self.minor = 4 $
   ELSE self.minor = minor
IF N_Elements(position) EQ 0 THEN self.position = [0.90, 0.10, 0.95, 0.90] $
   ELSE self.position = position

    ; Create the colorbar image. Get its size.

bar = REPLICATE(1B,10) # BINDGEN(self.ncolors)
s = SIZE(bar, /Dimensions)
xsize = s[0]
ysize = s[1]

    ; Create the colorbar image object. Add palette to it.

thisImage = Obj_New('IDLgrImage', bar, Palette=self.palette)
xs = Normalize([0,xsize], Position=[0,1.])
ys = Normalize([0,ysize], Position=[0,1.])
thisImage->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys

   ; Create a polygon object. Add the image as a texture map. We do
   ; this so the image can rotate in 3D space.

thisPolygon = Obj_New('IDLgrPolygon', [0, 1, 1, 0], [0, 0, 1, 1], [0,0,0,0], $
   Texture_Map=thisImage, Texture_Coord = [[0,0], [1,0], [1,1], [0,1]], color=[255,255,255])

    ; Scale the polygon into the correct position.

xs = Normalize([0,1], Position=[self.position(0), self.position(2)])
ys = Normalize([0,1], Position=[self.position(1), self.position(3)])
thispolygon->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys

    ; Create scale factors to position the axes.

longScale = Normalize(self.range, Position=[self.position(1), self.position(3)])
shortScale = Normalize([0,1], Position=[self.position(0), self.position(2)])

    ; Create the colorbar axes. 1000 indicates this location ignored.

shortAxis1 = Obj_New("IDLgrAxis", 0, Color=self.color, Ticklen=0.025, $
    Major=1, Range=[0,1], /NoText, /Exact, XCoord_Conv=shortScale,  $
    Location=[1000, self.position(1), 0.001])
shortAxis2 = Obj_New("IDLgrAxis", 0, Color=self.color, Ticklen=0.025, $0.001
    Major=1, Range=[0,1], /NoText, /Exact, XCoord_Conv=shortScale,  $
    Location=[1000, self.position(3), 0.001], TickDir=1)

textAxis = Obj_New("IDLgrAxis", 1, Color=self.color, Ticklen=0.025, $
    Major=self.major, Minor=self.minor, Title=thisTitle, Range=self.range, /Exact, $
    YCoord_Conv=longScale, Location=[self.position(0), 1000, 0.001], _Extra=extra)
textAxis->GetProperty, TickText=thisText
thisText->SetProperty, Font=self.thisFont, Recompute_Dimensions=2

longAxis2 = Obj_New("IDLgrAxis", 1, Color=self.color, /NoText, Ticklen=0.025, $
    Major=self.major, Minor=self.minor, Range=self.range, TickDir=1, $
    YCoord_Conv=longScale, Location=[self.position(2), 1000, 0.001], /Exact)

    ; Add the parts to the colorbar model.

self->Add, shortAxis1
self->Add, shortAxis2
self->Add, textAxis
self->Add, longAxis2
self->Add, thisPolygon

   ; Assign the name.

self->IDLgrModel::SetProperty, Name=name, Select_Target=1

    ; Create a container object and put the objects into it.

thisContainer = Obj_New('IDL_Container')
thisContainer->Add, thisFont
thisContainer->Add, thisImage
thisContainer->Add, thisText
thisContainer->Add, thisTitle
thisContainer->Add, self.palette
thisContainer->Add, textAxis
thisContainer->Add, shortAxis1
thisContainer->Add, shortAxis2
thisContainer->Add, longAxis2

    ; Update the SELF structure.

self.thisImage = thisImage
self.thisFont = thisFont
self.thisText = thisText
self.textAxis = textAxis
self.shortAxis1 = shortAxis1
self.shortAxis2 = shortAxis2
self.longAxis2 = longAxis2
self.thisContainer = thisContainer
self.thisTitle = thisTitle

RETURN, 1
END
;-------------------------------------------------------------------------



PRO VColorBar::Cleanup

    ; Lifecycle method to clean itself up.

Obj_Destroy, self.thisContainer
self->IDLgrMODEL::Cleanup
END
;-------------------------------------------------------------------------



PRO VColorBar::GetProperty, Position=position, Text=text, $
    Title=title, Palette=palette, Major=major, Minor=minor, $
    Range=range, Color=color, Name=name, $
    Transform=transform, _Ref_Extra=extra

    ; Get the properties of the colorbar.

IF Arg_Present(position) THEN position = self.position
IF Arg_Present(text) THEN text = self.thisText
IF Arg_Present(title) THEN self.thisTitle->GetProperty, Strings=title
IF Arg_Present(palette) THEN palette = self.palette
IF Arg_Present(major) THEN major = self.major
IF Arg_Present(minor) THEN minor = self.minor
IF Arg_Present(range) THEN range = self.range
IF Arg_Present(color) THEN color = self.color
IF Arg_Present(name) THEN self->IDLgrMODEL::GetProperty, Name=name
IF Arg_Present(transform) THEN self->IDLgrMODEL::GetProperty, Transform=transform
IF Arg_Present(extra) THEN self->IDLgrMODEL::GetProperty, _Extra=extra

END
;-------------------------------------------------------------------------



PRO VColorBar::SetProperty, Position=position, $
    Title=title, Palette=palette, Major=major, Minor=minor, $
    Range=range, Color=color, Name=name, Transform=transform, _Extra=extra

    ; Set properties of the colorbar.

IF N_Elements(position) NE 0 THEN BEGIN
    self.position = position

        ; Find the size of the image.

    self.thisImage->GetProperty, Data=image
    s = Size(image)
    xsize = s(1)
    ysize = s(2)
    xs = Normalize([0,xsize], Position=[position(0), position(2)])
    ys = Normalize([0,ysize], Position=[position(1), position(3)])
    self.thisImage->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys

        ; Create new scale factors to position the axes.

    longScale = Normalize(self.range, $
       Position=[self.position(1), self.position(3)])
    shortScale = Normalize([0,1], $
       Position=[self.position(0), self.position(2)])

        ; Position the axes. 1000 indicates this position ignored.

    self.textaxis->SetProperty, YCoord_Conv=longScale, $
       Location=[self.position(0), 1000, 0]
    self.longaxis2->SetProperty, YCoord_Conv=longScale, $
       Location=[self.position(2), 1000, 0]
    self.shortAxis1->SetProperty, XCoord_Conv=shortScale, $
       Location=[1000, self.position(1), 0]
    self.shortAxis2->SetProperty, XCoord_Conv=shortScale, $
       Location=[1000, self.position(3), 0]

ENDIF
IF N_Elements(title) NE 0 THEN self.thisTitle->SetProperty, Strings=title
IF N_Elements(transform) NE 0 THEN self->IDLgrMODEL::SetProperty, Transform=transform
IF N_Elements(palette) NE 0 THEN BEGIN
    self.palette = palette
    self.thisImage->SetProperty, Palette=palette
ENDIF
IF N_Elements(major) NE 0 THEN BEGIN
    self.major = major
    self.textAxis->SetProperty, Major=major
    self.longAxis2->SetProperty, Major=major
END
IF N_Elements(minor) NE 0 THEN BEGIN
    self.minor = minor
    self.textAxis->SetProperty, Minor=minor
    self.longAxis2->SetProperty, Minor=minor
END
IF N_Elements(range) NE 0 THEN BEGIN
    self.range = range
    longScale = Normalize(range, $
       Position=[self.position(1), self.position(3)])
    self.textAxis->SetProperty, Range=range, YCoord_Conv=longScale
    self.longAxis2->SetProperty, Range=range, YCoord_Conv=longScale
ENDIF
IF N_Elements(color) NE 0 THEN BEGIN
    self.color = color
    self.textAxis->SetProperty, Color=color
    self.longAxis2->SetProperty, Color=color
    self.shortAxis1->SetProperty, Color=color
    self.shortAxis2->SetProperty, Color=color
    self.thisText->SetProperty, Color=color
ENDIF
IF N_Elements(name) NE 0 THEN self->IDLgrMODEL::SetProperty, Name=name
IF N_Elements(extra) NE 0 THEN BEGIN
   self->IDLgrMODEL::SetProperty, _Extra=extra
   self.textAxis->SetProperty, _Extra=extra
   self.thisTitle->SetProperty, _Extra=extra
ENDIF
END
;-------------------------------------------------------------------------



PRO VColorBar__Define

colorbar = { VCOLORBAR, $
             INHERITS IDLgrMODEL, $      ; Inherits the Model Object.
             Position:FltArr(4), $       ; The position of the colorbar.
             Palette:Obj_New(), $        ; The colorbar palette.
             thisImage:Obj_New(), $      ; The colorbar image.
             imageModel:Obj_New(), $     ; The colorbar image model.
             thisContainer:Obj_New(), $  ; Container for cleaning up.
             thisFont:Obj_New(), $       ; The annotation font object.
             thisText:Obj_New(), $       ; The bar annotation text object.
             thisTitle: Obj_New(), $     ; The title of the colorbar.
             textAxis:Obj_New(), $       ; The axis containing annotation.
             shortAxis1:Obj_New(), $     ; A short axis.
             shortAxis2:Obj_New(), $     ; A second short axis.
             longAxis2:Obj_New(), $      ; The other long axis.
             NColors:0, $                ; The number of colors in the bar.
             Major:0, $                  ; Number of major axis intervals.
             Minor:0, $                  ; Number of minor axis intervals.
             Color:BytArr(3), $          ; Color of axes and annotation.
             Range:FltArr(2) }           ; The range of the colorbar axis.

END
;-------------------------------------------------------------------------
