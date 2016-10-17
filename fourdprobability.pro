; restore, filename = '/Data/Homer-2/Research/arrays/stack2/NI?samples/stack2Dend?_NI?.dat'

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
PRO Roberto_Help, sEvent
; This event handler is for the ABOUT and HELP options

; This is the version history text needed for later
	AboutMessage =['                        VERSION  2.0   ', $
		' ', ' ', $
		'             ___________' + 'May '+STRCOMPRESS(19)+', ' + $ 
		STRCOMPRESS(2003) + '___________', $
		' ', $
		'I decided today that I wanted to have a customized version of the ', $
		'XOBJVIEW program.  I wanted to dynamically change things in the ' , $
		'volumes, like add axis and change the axis color or to change the', $
		'color of the background.  I also hated the fact that the TIFF files',$
		'created by XOBJVIEW needed to be flipped.  Granted it only takes two',$
		'seconds to do it in PHOTOSHOP, but it just added to the annoying factors',$
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


PRO Roberto2_Event, sEvent

	WIDGET_CONTROL, sEvent.id, GET_UVALUE=uval


; Handle KILL requests.
	IF TAG_NAMES(sEvent, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
		WIDGET_CONTROL, sEvent.top, GET_UVALUE=sState
		;OBJ_DESTROY, sState.oHolder
		HEAP_FREE, sState.oHolder
		WIDGET_CONTROL, sEvent.top, /DESTROY
		HEAP_GC
		RETURN
	ENDIF

; Handle other events.
	CASE uval OF
	'DRAW': BEGIN
		WIDGET_CONTROL, sEvent.top, GET_UVALUE=sState, /NO_COPY

	; Handle trackball updates.
		bHaveTransform = sState.oTrack->Update( sEvent, TRANSFORM=qmat, Translate = sState.translate )
		IF (bHaveTransform NE 0) THEN BEGIN
			sState.oGroup->GetProperty, TRANSFORM=t
			t = t#qmat
			sState.oGroup->SetProperty, TRANSFORM=t
			GetYPR, t, ypr
			WIDGET_CONTROL, sState.yaw, Set_Value=ypr[0]
			WIDGET_CONTROL, sState.pitch, Set_Value=ypr[1]
			WIDGET_CONTROL, sState.roll, Set_Value=ypr[2]
			sState.oWindow->Draw, sState.oViewGroup
			WIDGET_CONTROL, sEvent.top, SET_UVALUE=sState, /NO_COPY
			RETURN

		ENDIF
	
		IF (sEvent.type EQ 4) THEN BEGIN	; Type 4 indicates an intial draw
			sState.oWindow->Draw, sState.oViewGroup
			WIDGET_CONTROL, sEvent.top, SET_UVALUE=sState, /NO_COPY
			RETURN
		ENDIF
	
	; Button press.
		IF (sEvent.type EQ 0) THEN BEGIN
			
			Case sEvent.Press OF
			4: BEGIN ; Right mouse.
				picked = sState.oWindow->select(sState.oView, [sEvent.x, sEvent.y])
				If OBJ_VALID(picked[0]) GT 0 Then Begin
				IF (OBJ_CLASS(picked[0]) EQ 'IDLGRPOLYGON') OR (OBJ_CLASS(picked[0]) EQ 'IDLGRPOLYLINE') THEN BEGIN
					;picked[0]->GetProperty, Alpha_Channel = alpha
					;picked[0]->SetProperty, Alpha_Channel = 0.75*alpha
					;sState.oWindow->Draw, sstate.oViewGroup
					;picked[0]->SetProperty, Alpha_Channel = 0.5*alpha
					;sState.oWindow->Draw, sstate.oViewGroup
					;picked[0]->SetProperty, Alpha_Channel = 0.25*alpha
					;sState.oWindow->Draw, sstate.oViewGroup
					picked[0]->SetProperty, Hide = 0
					;picked[0]->GetProperty, Alpha_Channel = alpha
					;wh = Where(picked[0] eq sState.oMyPolygons)

					;If (wh NE -1) GT N_ELEMENTS(sState.viewlist-2) Then BEGIN
;					If (wh NE -1) Then BEGIN
;						sState.viewlist[wh+1] = 1-sState.viewlist[wh+1]
;						sState.viewlist[0] = 0
;						WIDGET_CONTROL, sState.objpicker, Set_Value = sState.viewlist
;					ENDIF
				sState.oWindow->Draw, sstate.oViewGroup
				ENDIF
				ENDIF

				WIDGET_CONTROL, sEvent.top, SET_UVALUE=sState, /NO_COPY
				RETURN
				End
			1: BEGIN ;LEFT mouse button.
				sEVENT.PRESS = 1b
				sState.oWindow->SetProperty, QUALITY=sState.dragq
				WIDGET_CONTROL, sState.wDraw, /DRAW_MOTION
				
				END
			2: BEGIN ;Middle Button - select an object
				picked = sState.oWindow->select(sState.oView, [sEvent.x, sEvent.y])

				If OBJ_VALID(picked[0]) GT 0 Then Begin
					IF (OBJ_CLASS(picked[0]) EQ 'IDLGRPOLYGON') THEN BEGIN				
					sState.currentOBJ = OBJ_NEW()
					
					sState.currentOBJ[0] = picked[0]

					sState.currentOBJ[0]->GetProperty, Name = pName, Color=color, Alpha_Channel = alpha, Style = style
					
					WIDGET_CONTROL, sState.labelWid, SET_VALUE=STRING((pName))
					WIDGET_CONTROL, sState.rSlider, SET_VALUE=color[0]
					WIDGET_CONTROL, sState.gSlider, SET_VALUE=color[1]
					WIDGET_CONTROL, sState.bSlider, SET_VALUE=color[2]
					WIDGET_CONTROL, sState.tSlider, SET_VALUE=alpha*255
					WIDGET_CONTROL, sState.fillwire, Set_Value = 2-style
					ENDIF	
				EndIF ELSE BEGIN
					;sState.currentOBJ = OBJ_NEW()
					sState.currentOBJ = sState.oMyPolygons[0:sState.npart-1]
					WIDGET_CONTROL, sState.labelWid, SET_VALUE='ALL OBJECTS'
				ENDELSE
				
			END
			8: Begin ; Scroll wheel
				sState.oGroup -> Rotate, [0,1,0], -5
				sState.oGroup->GetProperty, TRANSFORM=t
				GetYPR, t, ypr
				WIDGET_CONTROL, sState.yaw, Set_Value=ypr[0]
				WIDGET_CONTROL, sState.pitch, Set_Value=ypr[1]
				WIDGET_CONTROL, sState.roll, Set_Value=ypr[2]
				sState.oWindow->Draw, sstate.oViewGroup
			END
			16: Begin ;scroll wheel back
				sState.oGroup -> Rotate, [0,1,0], 5
				sState.oWindow->Draw, sstate.oViewGroup
				sState.oGroup->GetProperty, TRANSFORM=t
				GetYPR, t, ypr
				WIDGET_CONTROL, sState.yaw, Set_Value=ypr[0]
				WIDGET_CONTROL, sState.pitch, Set_Value=ypr[1]
				WIDGET_CONTROL, sState.roll, Set_Value=ypr[2]
				sState.oWindow->Draw, sstate.oViewGroup
			END
			ELSE:
			ENDCASE 
			

		ENDIF



	; Button release.
		IF (sEvent.type EQ 1) THEN BEGIN
			IF (sEVENT.Release EQ 1b) THEN BEGIN
				sState.oWindow->SetProperty, QUALITY=2
				sState.oWindow->Draw, sstate.oViewGroup
			ENDIF
			sState.btndown = 0b
			WIDGET_CONTROL, sState.wDraw, DRAW_MOTION=0
		ENDIF
		WIDGET_CONTROL, sEvent.top, SET_UVALUE=sState, /NO_COPY
	END
	
	'Yaw': BEGIN
		WIDGET_Control, sEvent.top, GET_UVALUE=sState,/No_Copy
		WIDGET_Control, sState.yaw, Get_Value=yaw
		WIDGET_Control, sState.pitch, Get_Value=pitch
		WIDGET_Control, sState.roll, Get_Value=roll

		ypr = Float([yaw,pitch,roll])/!radeg
		
		t = [[Cos(ypr[1])*Cos(ypr[0]),Cos(ypr[1])*Sin(ypr[0]), -Sin(ypr[1])], $
		 [Sin(ypr[2])*Sin(ypr[1])*Cos(ypr[0])-Cos(ypr[2])*Sin(ypr[0]), $
			Sin(ypr[2])*Sin(ypr[1])*Sin(ypr[0])+Cos(ypr[2])*Cos(ypr[0]), $
			Cos(ypr[1])*Sin(ypr[2])],$
		 [Cos(ypr[2])*Sin(ypr[1])*Cos(ypr[0])+Sin(ypr[2])*Sin(ypr[0]), $
			Cos(ypr[2])*Sin(ypr[1])*Sin(ypr[0])-Sin(ypr[2])*Cos(ypr[0]), $
			Cos(ypr[1])*Cos(ypr[2])]]
		
		t = (t)
		sState.oGroup->GetProperty, TRANSFORM=told
		told[0:2,0:2] = t
		sState.oGroup->SetProperty, TRANSFORM=told
		sState.oWindow->Draw, sState.oViewGroup
   		WIDGET_CONTROL, sEvent.top, SET_UVALUE=sState, /NO_COPY
	END


	'QUIT': BEGIN
		WIDGET_CONTROL, sEvent.top, GET_UVALUE=sState
		;OBJ_DESTROY, sState.oHolder
		HEAP_FREE, sState.oHolder
		WIDGET_CONTROL, sEvent.top, /DESTROY
		HEAP_GC
		RETURN
	END
	ELSE:
	
	ENDCASE
	
END


;----------------------------------------------------------------------------
PRO Roberto2_Output, sEvent
; This event handler creates image files.

	Widget_Control, sEvent.top, Get_UValue=sState, /No_Copy

	Widget_Control, /Hourglass
	Wait, 0.5

	sState.oWindow->GetProperty, IMAGE_DATA = snapshot
	imageSize = SIZE(image)

; What kind of file is wanted?
	Widget_Control, sEvent.id, GET_UValue=whichFileType
	filter = "~/Desktop/"
	CASE whichFileType OF
	'JPEG': BEGIN
		filename = Dialog_Pickfile(/Write, File='my_image.jpg', Filter=filter)
		IF filename NE '' THEN Write_JPEG, filename, snapshot, True=1, Quality=100
		END
	'TIFF': BEGIN
		filename = Dialog_Pickfile(/Write, File='my_image.tif', Filter=filter)
		IF filename NE '' THEN Write_TIFF, filename, Reverse(snapshot,3)
		END
	'EPS': BEGIN
		filename = Dialog_Pickfile(/Write, File='my_image.eps', Filter=filter)
		IF filename NE '' THEN BEGIN
			sState.oWindow->GetProperty, Dimensions=viewDimensions, Units=viewUnits
			clipboard = Obj_New('IDLgrClipboard', Dimensions=[1600,1600], Unit=viewUnits)
			clipboard->Draw, sstate.oViewGroup, /Postscript, Filename=filename
			Obj_Destroy, clipboard
		ENDIF
		END
	'Movie': BEGIN
		
		filename = Dialog_Pickfile(/Write, File='0', Filter='*.jpg')
		Widget_Control, /Hourglass
    IF filename NE '' THEN BEGIN
      count = 0
      step=-1
      For i=0, -89, step Do Begin
        sState.oWindow->GetProperty, IMAGE_DATA = snapshot
        Write_JPEG, STRCOMPRESS(filename+String(count)+'.jpg', /REMOVE_ALL) , snapshot, True=1, Quality=100
        count = count+1
        sState.oGroup->Rotate, [1,0,0], step
        sState.oWindow->Draw, sstate.oViewGroup
      EndFor
      

     step=-1
     For i=0, -359, step Do Begin
       sState.oWindow->GetProperty, IMAGE_DATA = snapshot
       Write_JPEG, STRCOMPRESS(filename+String(count)+'.jpg', /REMOVE_ALL) , snapshot, True=1, Quality=100
       count = count+1
       sState.oGroup->Rotate, [0,1,0], step
       sState.oWindow->Draw, sstate.oViewGroup
     EndFor
        
      
      step=1
      For i=-89, 0, step Do Begin
        sState.oWindow->GetProperty, IMAGE_DATA = snapshot
        Write_JPEG, STRCOMPRESS(filename+String(count)+'.jpg', /REMOVE_ALL) , snapshot, True=1, Quality=100
        count = count+1
        sState.oGroup->Rotate, [1,0,0], step
        sState.oWindow->Draw, sstate.oViewGroup
      EndFor   
     
     
    ENDIF		
		
		;;original version
;		IF filename NE '' THEN BEGIN
;			count = 0
;			step=3
;			For i=0, 360, step Do Begin
;				sState.oWindow->GetProperty, IMAGE_DATA = snapshot
;				Write_JPEG, STRCOMPRESS(filename+String(count)+'.jpg', /REMOVE_ALL) , snapshot, True=1, Quality=100
;				count = count+1
;				sState.oGroup->Rotate, [0,1,0], step
;				sState.oWindow->Draw, sstate.oViewGroup
;			EndFor
;		ENDIF
		END

	'RECORD': BEGIN
		Widget_Control, sState.recordbutton, Get_Value=value
		temp =''
		IF value EQ 'Start Recording' THEN BEGIN
			file = Dialog_Pickfile(/Write, File='0', Filter='*.jpg', Get_Path= temp)
			sState.filename=file
			IF sState.filename NE '' THEN BEGIN
			Widget_Control, sState.recordbutton, Set_Value='Recording'
		
		
		
			ENDIF
		ENDIF ELSE BEGIN
		Widget_Control, sState.recordbutton, Set_Value='Start Recording'	
		ENDELSE
		END

	ENDCASE


;Put the info structure back.
	Widget_Control, sEvent.top, Set_UValue=sState, /No_Copy

END


;----------------------------------------------------------------------------
PRO Roberto2_Properties, sEvent
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
		sState.dragq = 1
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
		sState.oLOIM->SetProperty, HIDE=1
		sState.oL1->SetProperty, HIDE=1
		sState.oL2->SetProperty, HIDE=0
		sState.lightstatus=[1,0]
		END
	'TWO': BEGIN
		sState.oLOIM->SetProperty, HIDE=1
		sState.oL1->SetProperty, HIDE=0
		sState.oL2->SetProperty, HIDE=1
		sState.lightstatus=[0,1]
		END
	'THREE': BEGIN
	  sstate.oloim->setproperty, hide=0
	  sstate.ol1->setproperty, hide=0
    sstate.ol2->setproperty, hide=0
    sstate.lightstatus=[1,1]
    END
    
;Transparency Slider
	'TSLIDER': BEGIN
			Widget_control, sState.tSlider, GET_VALUE=newTrans
			
			
			wh = Where(sState.CurrentOBJ NE OBJ_NEW(), count)
			For i=0, count-1 Do Begin
				sState.currentOBJ[wh[i]] -> SetProperty, Alpha_channel = newTrans/255.0
			EndFor
		;ENDELSE
		END
	'brFront' : BEGIN
		obj = sState.currentobj[0]
		wh = Where(sState.oMicro->Get(/all,count=count) EQ obj )
		
		For j=0,count-1 DO BEGIN
			sState.oMicro->Move, wh, ((wh-1) > 3)
			wh = (wh-1) > 3
		ENDFOR
		
		END
	'sndBack' : BEGIN
		obj = sState.currentobj[0]
		wh = Where(sState.oMicro->Get(/all, Count=count) EQ obj )
		
		For j=0,count-1 DO BEGIN
			sState.oMicro->Move, wh, ((wh+1) < (count-1))
			wh = (wh+1) < (count-1)
		ENDFOR
		
		
		END
	'brForward': BEGIN
		obj = sState.currentobj[0]
		wh = Where(sState.oMicro->Get(/all) EQ obj )
		sState.oMicro->Move, wh, (wh-1 > 3)
		END
	'sndBackward': BEGIN
		obj = sState.currentobj[0]
		wh = Where(sState.oMicro->Get(/all, Count=count) EQ obj )
		sState.oMicro->Move, wh, (wh+1 < count-1)
		END
	
	
	'Reset':Begin
			WIDGET_CONTROL, sState.wTranslate, Get_Value=value
			If value EQ  0  Then Begin
				sState.oGroup->SetProperty, Transform = sState.origTransform
				GetYPR, sState.origTransform, ypr
				WIDGET_CONTROL, sState.yaw, Set_Value=ypr[0]
				WIDGET_CONTROL, sState.pitch, Set_Value=ypr[1]
				WIDGET_CONTROL, sState.roll, Set_Value=ypr[2]
			ENDIF ELSE BEGIN
				sState.oGroup->GetProperty, Transform = t
				t[3,*]=[0,0,0,1] 
				t[*,3]=[0,0,0,1] 
				sState.oGroup->SetProperty, Transform = t
			ENDELSE

	END
	ENDCASE

; Redraw the graphic.
	sState.oWindow->Draw, sstate.oViewGroup

;Put the info structure back.
	Widget_Control, sEvent.top, Set_UValue=sState, /No_Copy

END


;----------------------------------------------------------------------------
PRO Roberto2_Zoom, sEvent
; This is to set the value of zoom
	;Widget_Control, sEvent.id, Get_UValue=newProperty
	Widget_Control, sEvent.top, Get_UValue=sState, /No_Copy
	Widget_Control, sState.zoomSlider, Get_Value=newProperty

; Get the aspect ratio
	
	aspect = sState.aspect

; Get the view and put it in
	getView, newProperty, aspect, myView
	sState.oView->SetProperty, VIEWPLANE_RECT=myView


	sState.zoomVal = newProperty

; Redraw the graphic.
	sState.oWindow->Draw, sstate.oViewGroup

; Put the info structure back.
	Widget_Control, sEvent.top, Set_UValue=sState, /No_Copy

END

;----------------------------------------------------------------------------
PRO getView, zoomFactor, aspect, myView
; This function is used to calculate the VIEW used to zoom in and out

; Set up the equation for the zoom
	zeroVal = 2.5;FLOAT(1.9) 	; Larger value makes it zoom less
	topVal  = FLOAT(0.1)
	slope   = FLOAT( (topVal-zeroVal)/100.0 )

; What view property is wanted?
;	CASE zoomFactor OF
;		'0'  : sqrt2 = slope *   0.0 + zeroVal
;		'10' : sqrt2 = slope *  10.0 + zeroVal
;		'20' : sqrt2 = slope *  20.0 + zeroVal
;		'30' : sqrt2 = slope *  30.0 + zeroVal
;		'40' : sqrt2 = slope *  40.0 + zeroVal
;		'50' : sqrt2 = slope *  50.0 + zeroVal
;		'60' : sqrt2 = slope *  60.0 + zeroVal
;		'70' : sqrt2 = slope *  70.0 + zeroVal
;		'80' : sqrt2 = slope *  80.0 + zeroVal
;		'90' : sqrt2 = slope *  90.0 + zeroVal
;		'100': sqrt2 = slope * 100.0 + zeroVal
;	ENDCASE

	sqrt2 = slope * zoomFactor + zeroVal

	myview = [ -sqrt2*0.5, -sqrt2*0.5, sqrt2, sqrt2 ]
	IF (aspect GT 1) THEN BEGIN
		myview[0] = myview[0] - ((aspect-1.0)*myview[2])/2.0
		myview[2] = myview[2] * aspect
	ENDIF ELSE BEGIN
		myview[1] = myview[1] - (((1.0/aspect)-1.0)*myview[3])/2.0
		myview[3] = myview[3] / aspect
	ENDELSE

END


FUNCTION Dave_caps, sEvent

			Widget_Control, sEvent.top, Get_UValue=sState, /No_Copy
			Widget_control, sState.wcaps, Get_Value=value
			planes = -1
			If value[0] EQ 0 Then planes = sState.clipplanes[*,0]
			If value[1] EQ 0 Then planes = sState.clipplanes[*,1]
			If (value[0] EQ 0) AND (value[1] EQ 0)  Then planes = sState.clipplanes
			
			For i=0, sState.npart-1 Do sState.oMyPolygons[i]->SetProperty, Clip_Planes=planes
			sState.oWindow->Draw, sstate.oViewGroup


	; Put the info structure back.
	Widget_Control, sEvent.top, Set_UValue=sState, /No_Copy
	Return, 1
END

;FUNCTION Dave_OBJPicker, sEvent
;; This is the control for the Hide/Show Control Box
;	
;	Widget_Control, sEvent.top, Get_UValue=sState, /No_Copy
;;	Widget_Control, sState.objpicker, Get_Value=currentState
;
;	diff = currentstate-sState.viewlist
;
;	wh = where(ABS(diff) GT 0)
;
;	IF wh GE 1 Then Begin
;		sState.oMyPolygons[wh-1]->SetProperty, Hide = 1-currentState[wh]
;		currentState[0] = MIN(currentState[1:*])
;	ENDIF ELSE BEGIN
;		FOR i=0, N_ELEMENTS(sState.oMyPolygons)-1 DO sState.oMyPolygons[i]->SetProperty, Hide = 1-currentState[0]
;		currentState[1:*]=currentState[0]
;	ENDELSE
;	sState.oWindow->Draw, sstate.oViewGroup
;	
;	
;	sState.viewlist = currentState
;;	Widget_Control, sState.objpicker, Set_Value=currentState
;
;
;	; Put the info structure back.
;	Widget_Control, sEvent.top, Set_UValue=sState, /No_Copy
;	Return, 1
;END

;_________________________________________________________________________________________________________________

PRO Dave_Rescale, sEvent
; This is to set the value of zoom
	
	Widget_Control, sEvent.top, Get_UValue=sState, /No_Copy
	oPart = sState.oMyPolygons[0:sState.nPart-1]
	;Now Figure out the data ranges
xrange = [1e9,-1.0]
yrange = [1e9,-1.0]
zrange = [1e9,-1.0]

For i=0, N_ELEMENTS(oPart)-1 Do Begin
	oPart[i]->GetProperty, Hide=hide
	IF hide EQ 0 Then BEGIN
	oPart[i]->GetProperty, XRANGE=xrangetemp
	oPart[i]->GetProperty, YRANGE=yrangetemp
	oPart[i]->GetProperty, ZRANGE=zrangetemp
	
	xrange[0] = xrange[0] < xrangetemp[0]
	yrange[0] = yrange[0] < yrangetemp[0]
	zrange[0] = zrange[0] < zrangetemp[0]
	
	xrange[1] = xrange[1] > xrangetemp[1]
	yrange[1] = yrange[1] > yrangetemp[1]
	zrange[1] = zrange[1] > zrangetemp[1]
	ENDIF
ENDFOR

xmin = xrange[0]
xmax = xrange[1]
ymin = yrange[0]
ymax = yrange[1]
zmin = zrange[0]
zmax = zrange[1]

; Compute data bounds. 
	xmm = xMax - xMin
	ymm = yMax - yMin
	zmm = zMax - zMin
	xyzmm = [xmm, ymm, zmm ]

; Compute coordinate conversion to normalize.
	xyzSpan = MAX( xyzmm )
	xs = [0.0,1.0/xyzSpan]
	ys = [0.0,1.0/xyzSpan]
	zs = [0.0,1.0/xyzSpan]

	xs[1] = MIN( [xs[1], ys[1], zs[1]] ) *0.9
	ys[1] = xs[1]
	zs[1] = ys[1]

	xs[0] = -0.5*(xMax+xMin)*xs[1]
	ys[0] = -0.5*(yMax+yMin)*ys[1]
	zs[0] = -0.5*(zMax+zMin)*zs[1]
	
	sState.oMicro->Reset
	sState.oMicro->Scale,     xs[1],ys[1],zs[1]
	sState.oMicro->Translate, xs[0],ys[0],zs[0]


	sState.xaxis->SetProperty, Range=xrange-xmin, Ticklen=0.02/xs[1], Location=[xmin,ymin,zmin]
	sState.yaxis->SetProperty, Range=yrange-ymin, Ticklen=0.02/xs[1], Location=[xmin,ymin,zmin]
	sState.zaxis->SetProperty, Range=zrange-zmin, Ticklen=0.02/xs[1], Location=[xmin,ymin,zmin]

	sState.xaxis->GetProperty, XCoord_Conv = xcoord
	sState.yaxis->GetProperty, YCoord_Conv = ycoord
	sState.zaxis->GetProperty, ZCoord_Conv = zcoord

	xcoord[0] = xmin
	ycoord[0] = ymin
	zcoord[0] = zmin

	sState.xaxis->SetProperty, XCoord_Conv = xcoord
	sState.yaxis->SetProperty, YCoord_Conv = ycoord
	sState.zaxis->SetProperty, ZCoord_Conv = zcoord

	;sState.oMicro->SetProperty, CLIP_Planes = [[0,0,-1,-zmin], [0,0,1,-zmax],[0,-1,0,-ymin], [0,1,0,-ymax],[-1,0,0,-xmin], [1,0,0,-xmax]  ]
	;For i=0, sState.npart-1 Do sState.oMyPolygons[i]->SetProperty, Clip_Planes=[[0,0,-1,-zmin], [0,0,1,-zmax],[0,-1,0,-ymin], [0,1,0,-ymax],[-1,0,0,-xmin], [1,0,0,-xmax]  ]
	sState.oWindow->Draw, sState.oViewGroup	
	Widget_Control, sEvent.top, Set_UValue=sState, /No_Copy
	

END

Pro GetYPR, t, ypr
	
	temp = (t[0:2, 0:2])

	y = ATAN(temp[1,0]/temp[0,0]) + !PI*(temp[0,0] LT 0)
	p = ATAN((-temp[2,0])/SQRT(temp[2,1]^2. + temp[2,2]^2.)) + !PI*(SQRT(temp[2,1]^2. + temp[2,2]^2.) LT 0)
	r = ATAN(temp[2,1]/temp[2,2]) + !PI*(temp[2,2] LT 0)
	
	y = 2*!PI*(y LT 0) + y
	p = 2*!PI*(p LT 0) + p
	r = 2*!PI*(r LT 0) + r
	ypr = [y,p,r]*!radeg
	If Total(finite(ypr)) NE 3 Then Stop

END

;_________________________________________________________________________________________________________________
;----------------------------------------------------------------------------
PRO Roberto2_Settings, sEvent
; This event handler to save and get the orientation that they want.

	Widget_Control, sEvent.top, Get_UValue=sState, /No_Copy

; What property is wanted?
	Widget_Control, sEvent.id, Get_UValue=newOrient
	WIDGET_CONTROL, sEvent.top, /HOURGLASS

	CASE newOrient OF

	'SAVE':	BEGIN
		; Gather all the variables together that are going to be saved
		dragq   = sState.dragq
		zoomVal = sState.zoomVal
		sState.oView->GetProperty, COLOR=bkColor
		sState.xAxis->GetProperty, COLOR=aColor
		sState.oMyPolygons->GetProperty, COLOR=pColor
		sState.oMyPolygons->GetProperty, BOTTOM=pbColor
		diffBack = sState.diffBack
		sState.xAxis->GetProperty, HIDE=aHide
		sState.oL1->GetProperty, HIDE=L1Hide
		sState.oL2->GetProperty, HIDE=L2Hide

		; Put them into one variable
		saved={ dragq:dragq, 			$	; Drag Quality
			zoomVal:zoomVal, 		$	; Zoom Value
			bkColor:bkColor, 		$	; Background Color
			aColor:aColor, 			$	; Axis Colors
			pColor:pColor, 			$	; Polygon Colors
			pbColor:pbColor, 		$	; Polygon Back Colors
			diffBack:diffBack, 		$	; Variable for back of polygons
			aHide:aHide, 			$	; Hidden Axis?
			L1Hide:L1Hide, 			$	; Hidden Lights?
			L2Hide:L2Hide			$	; Hidden Lights?
			}

		; Have them pick the file to write to
		file = dialog_pickfile(/WRITE, FILTER='*.view', FILE='.view')
		sizeFile = STRLEN(file)
		IF (sizeFile GT 2) THEN BEGIN
			SAVE, saved, filename=file
		ENDIF ELSE BEGIN		; in case they hit cancel
			BEEP
			nothing = DIALOG_MESSAGE ('Damn you for not picking a file!')
		ENDELSE
		END
	'GET':	BEGIN
		file = DIALOG_PICKFILE(/READ, FILTER='*.view')
		sizeFile = STRLEN(file)
		IF (sizeFile GT 2) THEN BEGIN
			RESTORE, FILENAME=file
			sState.zoomVal = saved.zoomVal
			zoom = strcompress( string(saved.zoomVal) , /remove_all )
			aspect = sState.aspect
			getView , zoom, aspect , myView
			sState.oView->SetProperty, VIEWPLANE_RECT=myView
			scalingString = 'Zoom : ' + zoom + ' %'
			WIDGET_CONTROL, sState.wScalingLabel, SET_VALUE=scalingString
			sState.dragq = saved.dragq
			sState.oView->SetProperty, COLOR=saved.bkColor
			sState.xAxis->SetProperty, COLOR=saved.aColor
			sState.yAxis->SetProperty, COLOR=saved.aColor
			sState.zAxis->SetProperty, COLOR=saved.aColor
			sState.oMyPolygons->SetProperty, COLOR=saved.pColor
			widget_control, sState.rSlider, SET_VALUE=saved.pColor[0]
			widget_control, sState.gSlider, SET_VALUE=saved.pColor[1]
			widget_control, sState.bSlider, SET_VALUE=saved.pColor[2]
			sState.oMyPolygons->SetProperty, BOTTOM=saved.pbColor
			widget_control, sState.rSlider2, SET_VALUE=saved.pbColor[0]
			widget_control, sState.gSlider2, SET_VALUE=saved.pbColor[1]
			widget_control, sState.bSlider2, SET_VALUE=saved.pbColor[2]
			sState.diffBack = saved.diffBack
			CASE sState.diffBack OF
				0: buttonString = 'Different Back'
				1: buttonString = 'Same Back'
			ENDCASE
			WIDGET_CONTROL, sState.wBackColor, SET_VALUE=buttonString
			sState.xAxis->SetProperty, HIDE=saved.aHide
			sState.yAxis->SetProperty, HIDE=saved.aHide
			sState.zAxis->SetProperty, HIDE=saved.aHide
			CASE saved.aHide OF
				0: axisString = 'Hide Axis'
				1: axisString = 'Show Axis'
			ENDCASE
			WIDGET_CONTROL, sState.wShowAxis, SET_VALUE=axisString
			sState.oL1->SetProperty, HIDE=saved.L1Hide
			sState.oL2->SetProperty, HIDE=saved.L2Hide
			sstat.oloim->setproperty, hide=saved.loimhide
		ENDIF ELSE BEGIN			; in case they hit cancel
			BEEP
			nothing = DIALOG_MESSAGE ('Damn you for not picking a file!')
		ENDELSE
		END
	'RESET': BEGIN
		sState.oMicro->SetProperty, Transform=sState.origTransform
		END
	ENDCASE

; Redraw the graphic.
	sState.oWindow->Draw, sstate.oViewGroup

;Put the info structure back.
	Widget_Control, sEvent.top, Set_UValue=sState, /No_Copy

END

Pro Dave_2DOIM_Event, sEvent

Widget_Control, sEvent.id, Get_UValue=action
Widget_Control, sEvent.top, Get_UValue=tempPointer

CASE action OF
	'DONE': BEGIN
		Widget_Control, (*tempPointer).table, Get_Value=newVect
		(*tempPointer).newvect = newvect
		WIDGET_CONTROL, sEvent.top, /DESTROY
		
	END

	'CANCEL':BEGIN
		
		WIDGET_CONTROL, sEvent.top, /DESTROY
	END
	'TChange':Begin
		
	END

	ELSE:
	
	ENDCASE

END



;Pro DaveColor_Polygons, sEvent
; 
;
;; This event handler to set the graphic properties.
;
;	Widget_Control, sEvent.top, Get_UValue=sState, /No_Copy
;
;; What new color scheme is wanted?
;	Widget_Control, sEvent.id, Get_UValue=newProperty
;	;sState.oOIMLedgend->SetProperty, Hide=1
;	WIDGET_CONTROL, sState.rSlider, Sensitive=1
;	WIDGET_CONTROL, sState.gSlider, Sensitive=1
;	WIDGET_CONTROL, sState.bSlider, Sensitive=1
;
;	
;	sState.oL1->SetProperty, Hide=sState.lightstatus[0]
;	sState.oL2->SetProperty, Hide=sState.lightstatus[1]
;	sState.oLOIM->SetProperty, Hide = 1
;	
;	For i=0, sState.nPart-1 Do Begin
;			sState.oMyPolygons[i] -> SetProperty, Vert_Colors = 0,shading=1
;	EndFor	
;	
;CASE newProperty OF
;
;	'defaultC': BEGIN
;		For i=0, sState.nPart-1 Do Begin
;			sState.oMyPolygons[i] -> SetProperty, Color = sState.dColor, shading=1
;			WIDGET_CONTROL, sState.rSlider, SET_VALUE=sState.dColor[0]
;			WIDGET_CONTROL, sState.gSlider, SET_VALUE=sState.dColor[1]
;			WIDGET_CONTROL, sState.bSlider, SET_VALUE=sState.dColor[2]
;
;		EndFor
;	END
;	'randomC': BEGIN
;		For i=0, sState.nPart-1 Do Begin
;			rColor = BYTE(255*RandomU(seed, 3))
;			sState.oMyPolygons[i] -> SetProperty, Color = rColor, shading=1
;			WIDGET_CONTROL, sState.rSlider, SET_VALUE=rColor[0]
;			WIDGET_CONTROL, sState.gSlider, SET_VALUE=rColor[1]
;			WIDGET_CONTROL, sState.bSlider, SET_VALUE=rColor[2]
;		EndFor
;	END
;	'userC': BEGIN
;		For i=0, sState.nPart-1 Do Begin
;			sState.oMyPolygons[i] -> SetProperty, Color = sState.uColor[1:3,i], shading=1
;			WIDGET_CONTROL, sState.rSlider, SET_VALUE=sState.uColor[1,i]
;			WIDGET_CONTROL, sState.gSlider, SET_VALUE=sState.uColor[2,i]
;			WIDGET_CONTROL, sState.bSlider, SET_VALUE=sState.uColor[3,i]
;		EndFor
;	END
;
;	'2DOIM': BEGIN
;			WIDGET_CONTROL, /Hourglass
;			WIDGET_CONTROL, sState.rSlider, Sensitive=0
;			WIDGET_CONTROL, sState.gSlider, Sensitive=0
;			WIDGET_CONTROL, sState.bSlider, Sensitive=0
;			sState.oL1-> SetProperty, Hide=1
;			sState.oL2-> SetProperty, Hide=1
;			sState.oLOIM-> SetProperty, Hide=0
;			
;			wOIM2Din = Widget_Base(Group_Leader=sEvent.top,Column=1, Event_PRO='Dave_2DOIM_Event', /Floating)
;			temp = Widget_Label(wOIM2Din, Value='Please enter a direction.')
;			temp = Widget_Label(wOIM2Din, Value='Color represents crystal direction parallel with direction.')
;			inputTable = Widget_Table(wOIM2Din, Alignment=1, /Edit, $
;				Column_Labels=['x','y','z'],Row_Label=['Direction'], Value=sState.OIM2DVECT, UVALUE='TChange', Event_PRO='Dave_2DOIM_Event')
;
;			buttonBase = WIDGET_BASE(wOIM2Din, Column=2)
;			Done = Widget_Button(buttonBase, Value='DONE', UVALUE='DONE', EVENT_Pro='DAVE_2DOIM_Event')
;			Cancel = Widget_Button(buttonBase, Value='Cancel', UVALUE='CANCEL', EVENT_Pro='DAVE_2DOIM_Event')
;			tempstate= {newvect:sState.OIM2DVect, table:inputTable}
;			tempPointer = PTR_NEW(tempstate)
;			
;			WIDGET_CONTROL, wOIM2Din, /REALIZE
;			WIDGET_CONTROL, wOIM2Din, Set_UVAL = tempPointer
;			XMANAGER, '2D OIM Manager', wOIM2Din, Event_Handler='Dave_2DOIM_Event'
;				
;			tempState.newvect=(*tempPointer).newvect
;			
;			ptr_free, tempPointer
;			
;			
;			tempState.newvect = tempState.newVect/Sqrt(Total(tempState.newvect^2.))
;			
;			sState.OIM2DVECT = tempState.newvect
;			
;			newtext = 'Color represents crystal direction parallel with [' + $
;				 String(sState.OIM2DVECT, format = '(2(f5.2, ", "),f5.2 )' ) + ']'
;			sState.oOrientationText->SetProperty, Strings = newtext
;			
;
;		For i=0, sState.nPart-1 Do Begin
;			sState.oMyPolygons[i] -> GetProperty, name = pname
;			pname = Fix(pname)
;			wh = where(sState.rot[0,*] eq pname)
;			IF wh[0] NE -1 Then Begin
;				rotmat = makeEUlerRot(sState.rot[ 1:3, wh[0] ])
;				normals = TRANSPOSE(rotmat##sState.OIM2Dvect)
;				normals = CubicSym(normals)
;				cubic_color, normals, RGB
;				sState.oMyPolygons[i] -> SetProperty, Color = RGB
;			ENDIF ELSE BEGIN
;				sState.oMyPolygons[i] -> SetProperty, hide = 1
;				sState.viewList[i+1] = 0
;				sState.viewList[0] = 0
;			ENDELSE
;;			Widget_Control, sState.objpicker, Set_Value=sState.viewlist
;		EndFor
;		sState.oOIMLedgend->SetProperty, Hide=0
;	END
;
;	'normalC': BEGIN
;			WIDGET_CONTROL, /Hourglass
;			WIDGET_CONTROL, sState.rSlider, Sensitive=0
;			WIDGET_CONTROL, sState.gSlider, Sensitive=0
;			WIDGET_CONTROL, sState.bSlider, Sensitive=0
;			sState.oL1-> SetProperty, Hide=1
;			sState.oL2-> SetProperty, Hide=1
;			sState.oLOIM-> SetProperty, Hide=0
;
;			newtext = 'Color represents crystal direction parallel with each surface polygon normal'
;			sState.oOrientationText->SetProperty, Strings = newtext
;
;		For i=0, sState.nPart-1 Do Begin
;			sState.oMyPolygons[i] -> GetProperty, name = pname
;			pname = Fix(pname)
;			wh = where(sState.rot[0,*] eq pname)
;			IF wh[0] NE -1 Then Begin
;				rotmat = makeEUlerRot(sState.rot[ 1:3, wh[0] ])
;				
;				sState.oMyPolygons[i] -> GetProperty, Data = v, Polygons=p
;				normals = Compute_Mesh_Normals(v, p)
;				For j = 0l, N_Elements(normals[0,*])-1 Do BEGIN
;					normals[*,j] = TRANSPOSE(rotmat##normals[*,j])
;				ENDFOR
;				normals = CubicSym(normals)
;				cubic_color, normals, RGB
;				sState.oMyPolygons[i] -> SetProperty, Vert_colors = RGB, shading=1
;			ENDIF ELSE BEGIN
;				sState.oMyPolygons[i] -> SetProperty, hide = 1
;				sState.viewList[i+1] = 0
;				sState.viewList[0] = 0
;			ENDELSE
;;			Widget_Control, sState.objpicker, Set_Value=sState.viewlist
;		EndFor
;		sState.oOIMLedgend->SetProperty, Hide=0
;	END
;	'wire': BEGIN
;		For i=0, N_Elements(sState.oMyPolygons)-1 Do Begin
;			sState.oMyPolygons[i] -> GetProperty, Style=style
;			If style GE 2 Then style = -1
;			sState.oMyPolygons[i] -> SetProperty, Style = (style+1)
;		EndFor
;		
;	END
;
;	'colorSlider': BEGIN
;			Widget_control, sState.rSlider, GET_VALUE=newRED
;			Widget_control, sState.gSlider, GET_VALUE=newGreen
;			Widget_control, sState.bSlider, GET_VALUE=newBlue
;			newColor = [newRed, newGreen, newBlue]
;			wh = Where(sState.CurrentOBJ NE OBJ_NEW(), count)
;			For i=0, count-1 Do Begin
;				sState.currentOBJ[wh[i]] -> SetProperty, Color = newColor
;			EndFor
;		
;		END
;		
;
;EndCase
;
;; Redraw the graphic.
;	sState.oWindow->Draw, sstate.oViewGroup
;
;; Put the info structure back.
;	Widget_Control, sEvent.top, Set_UValue=sState, /No_Copy
;
;
;END


Function Dave_Translate, sEvent

	Widget_Control, sEvent.top, Get_UValue=sState, /No_Copy

	Widget_control, sState.wTranslate, GET_VALUE=translate
	
	sState.translate=translate
		
	; Put the info structure back.
	Widget_Control, sEvent.top, Set_UValue=sState, /No_Copy

END


;Function Dave_FillWire, sEvent
;
;Widget_Control, sEvent.top, Get_UValue=sState, /No_Copy
;
;			Widget_control, sState.FillWire, GET_VALUE=style
;			style = 2-style
;			
;			wh = Where(sState.CurrentOBJ NE OBJ_NEW(), count)
;			For i=0, count-1 Do Begin
;				sState.currentOBJ[wh[i]] -> SetProperty, STYLE=style
;			EndFor
;; Redraw the graphic.
;	sState.oWindow->Draw, sstate.oViewGroup
;
;; Put the info structure back.
;	Widget_Control, sEvent.top, Set_UValue=sState, /No_Copy
;
;END

;----------------------------------------------------------------------------
PRO Robertovert3da, alldata, k1vals, k2vals, vnew


;;From calculate_ISD_3D_4D_adv_scaled_pg35.pro
  mainDir='~/Desktop/SOFC/AIST_LSCF/control/reconstruction/'
  restore,mainDir+'data/AIST_LSCF_0h_200_200_200_Stage1_3DFFT_Amplitude.sav';logpower_filter

kappares1=      1.0
kappares2=      1.0
kappares3=      1.0


  xTitle = Obj_New('IDLgrText', 'X-axis', FONT=size2, Color=[255,255,0])
  yTitle = Obj_New('IDLgrText', 'Y-axis', FONT=size2, Color=[255,255,0])
  zTitle = Obj_New('IDLgrText', 'Z-axis', FONT=size2, Color=[255,255,0])


;number=TOTAL(kappa3Dnew[*,*,*])
;curv_3 = DOUBLE(kappa3Dnew[*,*,*])/DOUBLE(number)  ;  Data set for the curvature plots
;curv_3=curv_3/(double(kappares2)*double(kappares1)*double(kappares3))
alldata=logpower_filtered
print, min(alldata),max(alldata)



;alldata is the 3d array created from plot3d.pro; k1vals=xaxis, k2vals=yaxis, vnew=zaxis
; Set it so that it multi-threakds when the array is 20,000
	CPU, TPOOL_MIN_ELTS = 20000

	device, GET_SCREEN_SIZE=scr
	ydim = scr[1] * 0.80 ; * 0.85
	xdim = ydim   * 1.0 ; * 0.85

	dataSize = SIZE(allData)

    PRINT, 'min. value: ', MIN(alldata)
    PRINT, 'max. value: ', MAX(alldata)

    l = (max(alldata)-min(alldata))/6.0d0
		


		cap=0

		IF cap EQ 1 THEN BEGIN
			max = max(allData)
			tempData = allData
			solid = 0
			;READ, solid, PROMPT = 'liquid or solid (0=liquid, 1=solid):  '

			IF solid EQ 0 THEN BEGIN	; they want liquid
				tempData = BYTSCL( -tempData[*,*,*] + max )
			ENDIF

			sizeSurf = SIZE (tempData, /DIMENSIONS)
			allData2 = BYTARR(sizeSurf[0]+2, sizeSurf[1]+2, sizeSurf[2]+2)
			allData2[1:sizeSurf[0], 1:sizeSurf[1], 1:sizeSurf[2]] = TEMPORARY(tempData)
		ENDIF ELSE BEGIN
			allData2 = allData
		ENDELSE

; now get the surface description in form of polygons
;SHADE_VOLUME, allData2, -100, v, p
;alldata3 = temporary(alldata2)
;shade_volume, alldata3, -50, v2, p2
;alldata4 = temporary(alldata3)
;shade_volume, alldata4, 0, v3, p3
;alldata5 = temporary(alldata4)
;shade_volume, alldata5, 50, v4, p4
;alldata6 = temporary(alldata5)
;shade_volume, alldata6, 100, v5, p5
;alldata2 = alldata6

;isosurface, alldata2, min(alldata)+l/10, v, p   ;l/20
;nvertexpoints= size(v, /dimensions)
;nvertexpoints= nvertexpoints[1]
;alldata3 = temporary(alldata2)
;isosurface, alldata3,min(alldata)+ l/5, v2, p2 ;l/10
;alldata4 = temporary(alldata3)
;isosurface, alldata4, min(alldata)+l/2, v3, p3
;alldata5 = temporary(alldata4)
;isosurface, alldata5, min(alldata)+l, v4, p4
;alldata6 = temporary(alldata5)
;isosurface, alldata6, min(alldata)+2*l, v5, p5
;alldata2 = alldata6

isosurface, alldata2, -5, v, p   ;l/20
nvertexpoints= size(v, /dimensions)
nvertexpoints= nvertexpoints[1]
alldata3 = temporary(alldata2)
isosurface, alldata3,-4, v2, p2 ;l/10
alldata4 = temporary(alldata3)
isosurface, alldata4, -3, v3, p3
alldata5 = temporary(alldata4)
isosurface, alldata5, -2, v4, p4
alldata6 = temporary(alldata5)
isosurface, alldata6, -1.5, v5, p5
alldata2 = alldata6

;For the colarbar
  colorBarRange = [ -5, -1.5 ]

; Compute viewplane rect based on aspect ratio.
	aspect = FLOAT(xdim) / FLOAT(ydim)

; get the view for the initial zoom
	zoomVal = 20		; has to be a multiple of 10 and goes from 0 to 100
	zoom = strcompress( string(zoomVal) , /remove_all )
	getView, zoom, aspect, myView

; Create view.  
	oView = OBJ_NEW('IDLgrView', PROJECTION=1, EYE=4, ZCLIP=[1.4,-1.4],$
		VIEWPLANE_RECT=myview, COLOR=[127,127,127])
	oviewoim = obj_new('idlgrview', transparent=1)
	oviewgroup = obj_new('idlgrviewgroup')

	oviewgroup->add, oview
	oviewgroup->add, oviewoim

; Create models.
	oOIMLedgend = OBJ_NEW('IDLgrModel', Hide=1)
	
	oTop = OBJ_NEW('IDLgrModel')
	oGroup = OBJ_NEW('IDLgrModel')
	oMicro = OBJ_NEW('IDLgrModel')
	oXstal = OBJ_NEW('IDLgrModel')
	oTop->Add, oGroup
	oGroup-> Add, oMicro

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

		; Compute data bounds. 
		s = SIZE(alldata2)

    calib = DINDGEN(3)     ;calibration for x,y,z
		
    calib[0] = 60.0d0/s[1]
    calib[1] = 60.0d0/s[2]
    calib[2] = 60.0d0/s[3]
    

    calib[0] = 1d0
    calib[1] = 1d0
    calib[2] = 1d0
;stop
		xVals = (v[0, *]-s[1]/2)*calib[0]
		yVals = (v[1, *]-s[2]/2)*calib[1]
		zVals = (v[2, *]-s[3]/2)*calib[2]
    xVals2 = (v2[0, *]-s[1]/2)*calib[0]
    yVals2 = (v2[1, *]-s[2]/2)*calib[1]
    zVals2 = (v2[2, *]-s[3]/2)*calib[2]
    xVals3 = (v3[0, *]-s[1]/2)*calib[0]
    yVals3 = (v3[1, *]-s[2]/2)*calib[1]
    zVals3 = (v3[2, *]-s[3]/2)*calib[2]
    xVals4 = (v4[0, *]-s[1]/2)*calib[0]
    yVals4 = (v4[1, *]-s[2]/2)*calib[1]
    zVals4 = (v4[2, *]-s[3]/2)*calib[2]
    xVals5 = (v5[0, *]-s[1]/2)*calib[0]
    yVals5 = (v5[1, *]-s[2]/2)*calib[1]
    zVals5 = (v5[2, *]-s[3]/2)*calib[2]

		xMax = MAX(xVals)
		yMax = MAX(yVals)
		zMax = MAX(zVals)
		xMin = MIN(xVals)
		yMin = MIN(yVals)
		zMin = MIN(zVals)

		xmm = xMax - xMin
		ymm = yMax - yMin
		zmm = zMax - zMin
		xyzmm = [xmm, ymm, zmm ]

		; Compute coordinate conversion to normalize.
		xyzSpan = MAX( xyzmm )
		xs = [0,1.0/xyzSpan]
		ys = [0,1.0/xyzSpan]
		zs = [0,1.0/xyzSpan]

		;xs[1] = MIN( [xs[1], ys[1], zs[1]] ) * 0.9
		;ys[1] = xs[1]
		;zs[1] = ys[1]

 		xs[0] = -0.5*(xMax+xMin)*xs[1]
		ys[0] = -0.5*(yMax+yMin)*ys[1]
		zs[0] = -0.5*(zMax+zMin)*zs[1]

; Initial polygon colors
    dcolor = [200,200,211]

    pColor = BYTARR(3)
    pColor = [0,0,0]

    pcolor2 = bytarr(3)
    pcolor2 = [0,0,255]
    
    pcolor3 = bytarr(3)
    pcolor3 = [0,255,150]
    
    pcolor4 = bytarr(3)
    pcolor4 = [180,255,0]
    
    pcolor5 = bytarr(3)
    pcolor5 = [255,0,0]
		
    oMyPolygons = OBJ_NEW('IDLgrPolygon', xVals5, yVals5, zVals5, $
      POLYGONS=p5, COLOR=pColor5, alpha_channel = 1.0, SHADING=1, style = 2)
    omicro->Add, oMyPolygons
    oMyPolygons = OBJ_NEW('IDLgrPolygon', xVals4, yVals4, zVals4, $
      POLYGONS=p4, COLOR=pColor4, alpha_channel = 0.4, SHADING=1, style = 2)
    omicro->Add, oMyPolygons
    oMyPolygons = OBJ_NEW('IDLgrPolygon', xVals3, yVals3, zVals3, $
      POLYGONS=p3, COLOR=pColor3, alpha_channel = 0.3, SHADING=1, style = 2)
    omicro->Add, oMyPolygons
    oMyPolygons = OBJ_NEW('IDLgrPolygon', xVals2, yVals2, zVals2, $
      POLYGONS=p2, COLOR=pColor2, alpha_channel = 0.2, SHADING=1, style = 2)
    omicro->Add, oMyPolygons
    oMyPolygons = OBJ_NEW('IDLgrPolygon', xVals, yVals, zVals, $
      POLYGONS=p, COLOR=pColor, alpha_channel = 0.1, SHADING=1, style = 2)
    omicro->Add, oMyPolygons

     xrange=[ -3,1 ]
     yrange=[ -2,2 ]
     zrange=[ -3,3 ]

    xrange=[ (-s[1]/2)* calib[0],(s[1]/6)*calib[0] ]
    yrange=[ (-s[2]/4)* calib[1],(s[2]/2)*calib[1] ]
    zrange=[ (-s[3]/4)* calib[2],(s[3]/3)*calib[2] ]


	size1 = Obj_New('IDLgrFont', 'Helvetica', Size=14)
	size2 = Obj_New('IDLgrFont', 'Helvetica', Size=13)



	xAxis = Obj_New("IDLgrAxis", 0, Color=[255,255,255], ticklen=0.02, $
    		  /EXACT, RANGE=xrange,Thick = 3.0,TITLE=xTitle,Location=[xmin, yMin, zMin]); Ticklen=0.02/xs[1],TITLE=xTitle, Location=[xmin, yMin, zMin], Name='X AXIS')
	xAxis->GetProperty, TickText=xAxisText
	xAxisText->SetProperty, Font=size1
;	xAxis->GetProperty, Xcoord_Conv=xcoord
;	xcoord[0] = xcoord[0]+xmin
  xAxis->SetProperty, tickdir=2 ;Textbaseline=[1,0,0], 
;	xAxis->SetProperty, Xcoord_Conv=xcoord

	yAxis = Obj_New("IDLgrAxis", 1, Color=[255,255,255], Ticklen=0.02, $
    		Range=yrange, /EXACT,  Thick = 3.0,TITLE=yTitle,Location=[xMin, yMin, zMin]);, TITLE=yTitle,Location=[xMin, yMin, zMin], Name='Y Axis')
	yAxis->GetProperty, TickText=yAxisText
	yAxisText->SetProperty, Font=size1
;	yAxis->GetProperty, ycoord_Conv=ycoord
;	ycoord[0] = ycoord[0]+ymin
  yAxis->SetProperty, tickdir=2 ;Textbaseline=[0,1,0],
;	yAxis->SetProperty, ycoord_Conv=ycoord	
  omicro-> Add, yAxis
;	 yAxis = Obj_New("IDLgrAxis", 1, Color=[255,255,255], Ticklen=1, $
;        Range=yrange, /EXACT,  Thick = 3.0, location=[0,0,0.1])
;  yAxis->GetProperty, TickText=yAxisText
;  yAxisText->SetProperty, Font=size1
;  yAxis->SetProperty, Textbaseline=[0,1,0], tickdir=2
;    omicro-> Add, yAxis
; yAxis = Obj_New("IDLgrAxis", 1, Color=[255,255,255], Ticklen=1, $
;        Range=yrange, /EXACT,  Thick = 3.0, location=[0,0,0.2])
;  yAxis->GetProperty, TickText=yAxisText
;  yAxisText->SetProperty, Font=size1
;  yAxis->SetProperty, Textbaseline=[0,1,0], tickdir=2 
;    omicro-> Add, yAxis
;  yAxis = Obj_New("IDLgrAxis", 1, Color=[255,255,255], Ticklen=1, $
;        Range=yrange, /EXACT,  Thick = 3.0, location=[0,0,0.3])
;  yAxis->GetProperty, TickText=yAxisText
;  yAxisText->SetProperty, Font=size1
;  yAxis->SetProperty, Textbaseline=[0,1,0], tickdir=2
;  omicro-> Add, yAxis
	zAxis = Obj_New("IDLgrAxis", 2, Color=[255,255,255], Ticklen=0.02, $
    		Range=zrange, /EXACT,  Thick = 3.0, Location=[xMin, ymin, zMin], TITLE=zTitle);, TITLE=zTitle,Location=[xMin, ymin, zMin], Name='Z Axis')
;	zAxis->GetProperty, Zcoord_Conv=zcoord
;	zcoord[0] = zcoord[0]+1
;	zAxis->SetProperty, Zcoord_Conv=zcoord
;	zAxis->SetProperty, tickdir=2 ;Textbaseline=[0,0,1], 
;	zTitle->SetProperty, Baseline=[0,1,0],updir=[0,0,1], alignment = 1

	zAxis->GetProperty, TickText=zAxisText
	zAxisText->SetProperty, Font=size1
	
	omicro-> Add, xAxis
	omicro-> Add, zAxis

	omicro->Scale,     xs[1],ys[1],zs[1]
	omicro->Translate, xs[0],ys[0],zs[0]

; Create some lights.  Making two lighting models
	oL1 = OBJ_NEW('IDLgrModel')
	oL2 = OBJ_NEW('IDLgrModel')
	oLOIM = OBJ_NEW('IDLgrModel') ;OIM lighting model
	oTop->Add, oL1
	oTop->Add, oL2
	oTop->add, oloim

	CASE dataSize[0] OF
		2: oL2->SetProperty, HIDE=1
		3: oL1->SetProperty, HIDE=1
	ENDCASE

	oLights1 = OBJ_NEW('IDLgrLight', LOCATION=[-0.5,-0.5,4], TYPE=1, INTENSITY=0.4)
	oL1->Add, oLights1
	oLights2 = OBJ_NEW('IDLgrLight', TYPE=2, INTENSITY=0.6)
	oL1->Add, oLights2
	oLights3 = OBJ_NEW('IDLgrLight', TYPE=0, INTENSITY=0.25)
	oL1->Add, oLights3

	ambientLight = Obj_New('IDLgrLight', Type=0, Intensity=0.2)
	rotatingLight = Obj_New('IDLgrLight', Type=1, Intensity=0.60, $
		Location=[xrange[1], yrange[1], 4*zrange[1]], $
		Direction=[xrange[0], yrange[0], zrange[0]])
	fillLight = Obj_New('IDLgrLight', Type=3, Intensity=0.2, $
		Location=[(xrange[1]-xrange[0])/2.0, (yrange[1]-yrange[0])/2.0, -2*Abs(zrange[0])], $
		Direction=[(xrange[1]-xrange[0])/2.0, (yrange[1]-yrange[0])/2.0, zrange[1]])
	nonrotatingLight = Obj_New('IDLgrLight', Type=1, Intensity=0.4, $
		Location=[-xrange[1], (yrange[1]-yrange[0])/2.0, 4*zrange[1]], $
		Direction=[xrange[1], (yrange[1]-yrange[0])/2.0, zrange[0]])

	oL2->Add, ambientLight
	oL2->Add, fillLight
	oL2->Add, rotatingLight
	oL2->Add, nonrotatinglight

	rotatingLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
	fillLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
	nonrotatingLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs

	oOIML1 = OBJ_NEW('IDLgrLight', Location=[-2,2,5], TYPE=2, INTENSITY=0.55)
	oOIMl2 = OBJ_NEW('IDLgrLight', Location=[2,2,5], TYPE=2, INTENSITY=0.55)
	oOIMl3 = OBJ_NEW('IDLgrLight', Location=[0,-3,-2], TYPE=2, INTENSITY=0.41)
	oOIML4 = OBJ_NEW('IDLgrLight', TYPE=0, INTENSITY=0.05)
	
  oLOIM->Add, oOIML1
	oLOIM->Add, oOIML2
	oLOIM->Add, oOIML3
	oLOIM->Add, oOIML4
	
	oL1->SetProperty, Hide=0
	oL2->SetProperty, Hide=0
	oLOIM -> SetProperty, Hide=0

; Place the top model in the view.
	oView->Add, oTop


;
;--------------------------     INITIAL ROTATION       -----------------------

;outputname='~/Desktop/IDL/kappa3D_kappa1_kappa2_DHDt_version6_scaled_roll
;for angle=0,-2,-1 do begin
;outputname='~/Desktop/IDL/kappa3D_kappa1_kappa2_DHDt_version6_scaled_rollangle_'+STRING(abs(angle), FORMAT='(I2.2)')+'view.tiff'
; Rotate to standard view for first draw.
	oGroup->Rotate, [1,0,0], 0	; (-40,20,0) for standard view
	oGroup->Rotate, [0,1,0], 0	; (0,0,0) for top view with +z coming out
	oGroup->Rotate, [0,0,1], 0; 

; Color Bar initial range
	cbMin = min(alldata)
	cbMax = max(alldata)

; Color Bar absolute max and min
	cbAMin = -0.05
	cbAMax =  0.05

; Color bar models that I will use later

	oBarModel = OBJ_NEW('IDLgrModel')
	oTop->Add, oBarModel 
	oColorBar = Obj_New('VColorBar', Palette=myPalette, Range=colorbarrange, $
		Color=[255,255,255], Minor=3, Position=[0.80,-0.40,0.85,0.40])  ;[0.80,-0.40,0.85,0.40], [0.50,-0.20,0.55,0.40]
	oBarModel->Add, oColorBar
	oBarModel->SetProperty, HIDE=0

; Create a trackball.
	oTrack = OBJ_NEW('Trackball', [xdim/2.0, ydim/2.0], xdim/2.0)

; Create a holder object for easy destruction.
	oHolder = OBJ_NEW('IDL_Container')
	oHolder->Add, oViewgroup
	oHolder->Add, oTrack

; Get the transformation so that later we can get back to original view
	oGroup->GetProperty, Transform=origTransform

;XOBJVIEW_WRITE_IMAGE,'~/figure.tiff','tiff'
;TV, image  
;; Write the image to a TIFF file named myfile.tif:  
;WRITE_TIFF, 'myfile.tif', image  
;endfor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;the GUI

; Create the widgets.
	wBase = WIDGET_BASE(/COLUMN, XPAD=0, YPAD=0, $
		TITLE="NEW VIEWER", /TLB_KILL_REQUEST_EVENTS, $
		TLB_FRAME_ATTR=1, MBAR=barBase)

;Create the menu bar.
; FILE Menu
	fileMenu = WIDGET_BUTTON(barBase, VALUE='File', /MENU)

	QuitButton = WIDGET_BUTTON(fileMenu, VALUE='Quit', UVALUE='QUIT', $
   		UNAME='sState:quit')
; OUTPUT Menu
	outputMenu = WIDGET_BUTTON(barBase, VALUE='Output', /MENU)

	button = Widget_Button(outputMenu, Value='TIFF File', $
 		UValue='TIFF', Event_Pro='Roberto2_Output')
	button = Widget_Button(outputMenu, Value='JPEG File', $
		UValue='JPEG', Event_Pro='Roberto2_Output')
	button = Widget_Button(outputMenu, Value='EPS File', $
		UValue='EPS', Event_Pro='Roberto2_Output')
	button = Widget_Button(outputMenu, Value='Movie', $
		UValue='Movie', Event_Pro='Roberto2_Output')

; PROPERTIES Menu
	propMenu = WIDGET_BUTTON(barBase, VALUE='Properties', /MENU)

   ; Background Color
	bcolor = Widget_Button(propMenu, Value='Background Color', /Menu)
	dummy = Widget_Button(bcolor, Value='Black', $
		Event_Pro='Roberto2_Properties', UValue='BBLACK')
	dummy = Widget_Button(bcolor, Value='White', $
		Event_Pro='Roberto2_Properties', UValue='BWHITE')
	dummy = Widget_Button(bcolor, Value='Charcoal', $
		Event_Pro='Roberto2_Properties', UValue='BCHARCOAL')
	dummy = Widget_Button(bcolor, Value='Gray', $
		Event_Pro='Roberto2_Properties', UValue='BGRAY')

   ; Axes Color
	acolor = Widget_Button(propMenu, Value='Axes Color', /Menu)
	dummy = Widget_Button(acolor, Value='Black', $
	   Event_Pro='Roberto2_Properties', UValue='ABLACK')
	dummy = Widget_Button(acolor, Value='White', $
	   Event_Pro='Roberto2_Properties', UValue='AWHITE')
	dummy = Widget_Button(acolor, Value='Yellow', $
	   Event_Pro='Roberto2_Properties', UValue='AYELLOW')
	dummy = Widget_Button(acolor, Value='Green', $
	   Event_Pro='Roberto2_Properties', UValue='AGREEN')
	dummy = Widget_Button(acolor, Value='Navy Blue', $
	   Event_Pro='Roberto2_Properties', UValue='ANAVY')

   ; Drag Quality
 	dragButton = Widget_Button(propMenu, VALUE="Drag Quality", /MENU)
	dummy = WIDGET_BUTTON(DragButton, $
	   VALUE='Low', UVALUE='LOW', Event_Pro='Roberto2_Properties')
	dummy = WIDGET_BUTTON(DragButton, $
	   VALUE='Medium', UVALUE='MEDIUM', Event_Pro='Roberto2_Properties')
	dummy = WIDGET_BUTTON(DragButton, $
	   VALUE='High', UVALUE='HIGH', Event_Pro='Roberto2_Properties')

   ; Original View
	OGViewButton = Widget_Button(propMenu, VALUE="Reset", $
		/SEPARATOR, UVALUE='OG', Event_Pro='Roberto2_Properties')


; LIGHTING Menu
	helpMenu = WIDGET_BUTTON(barBase, VALUE='Lighting', /MENU)
	button = Widget_Button(helpMenu, Value='Scheme 1', $
 		UValue='ONE', Event_Pro='Roberto2_Properties')
	button = Widget_Button(helpMenu, Value='Scheme 2', $
 		UValue='TWO', Event_Pro='Roberto2_Properties')
 	button = widget_button(helpmenu, value='Scheme 3 (default)',$
 	  Uvalue='THREE', event_pro='roberto2_properties')

;; Coloring Menu
;	colorMenu = WIDGET_BUTTON(barBase, VALUE='Object Coloring', /MENU)
;	button = Widget_Button(colorMenu, Value='Default Color', $
; 		UValue='defaultC', Event_Pro='DaveColor_Polygons')
;	button = Widget_Button(colorMenu, Value='Random Color', $
; 		UValue='randomC', Event_Pro='DaveColor_Polygons')
;	button = Widget_Button(colorMenu, Value='User Color', $
; 		UValue='userC', Event_Pro='DaveColor_Polygons')
;	button = Widget_Button(colorMenu, Value='2D OIM Coloring', $
; 		UValue='2DOIM', Event_Pro='DaveColor_Polygons')
;	button = Widget_Button(colorMenu, Value='Normal Coloring', $
; 		UValue='normalC', Event_Pro='DaveColor_Polygons')
;	button = Widget_Button(colorMenu, Value='Points/Wire/Fill', $
; 		UValue='wire', Event_Pro='DaveColor_Polygons')

; HELP Menu
	helpMenu = WIDGET_BUTTON(barBase, VALUE='Help', /MENU)
	button = Widget_Button(helpMenu, Value='About', $
 		UValue='ABOUT', Event_Pro='Roberto2_Help')
	button = Widget_Button(helpMenu, Value='Help', $
 		UValue='HELP', Event_Pro='Roberto2_Help')

; Create a sub base of the top base (wBase).
	wSubBase = WIDGET_BASE(wBase, COLUMN=2)

; Left Side with all of the controls
	wLeftbase = WIDGET_BASE(wSubBase, COLUMN=1, y_scroll_size=ydim-30, x_scroll_size=190,xsize=190, /scroll)
;Set up two tabs - one for plot controls and one for indivudual objects


	wTab = WIDGET_TAB(wLeftbase, /ALIGN_TOP, UVALUE='TAB' )
;The First tab
;	wObjTab= WIDGET_BASE(wTab, COLUMN=1, TITLE='Obj Control')

;The second tab - yeah, its backwards.
	wPlotTab= WIDGET_BASE(wTab, COLUMN=1, TITLE='View Control')
	;divider = WIDGET_LABEL(wLeftBase, VALUE='----------------------', /ALIGN_CENTER)
	wShowAxis = WIDGET_BUTTON(wPlotTab, Value='Hide Axis', Uvalue='AXIS', $
		Event_Pro='Roberto2_Properties', /ALIGN_CENTER);, TOOLTIP='Show/Hide the axis')
	
	divider = WIDGET_LABEL(wPlotTab, VALUE='----------------------', /ALIGN_CENTER)
	wsubLeftbase1 = WIDGET_BASE(wPlotTab, COLUMN=2, /Align_Center)
	wTranslate = CW_BGROUP(wsubLeftBase1, ['Rotate','Translate'], /Exclusive, Set_Value=0,COLUMN=2 ,$
		   UValue='Translate', Event_FUNC='Dave_Translate'  )
	;wTranslate = WIDGET_BUTTON(wsubLeftBase1, Value='  Rotate  ', Uvalue='Translate', $
	;	Event_Pro='Roberto2_Properties', /ALIGN_CENTER, TOOLTIP='Toggle Rotate/Translate')
	wReset = WIDGET_BUTTON(wPlotTab, Value=' Reset ', Uvalue='Reset', $
		Event_Pro='Roberto2_Properties', /ALIGN_CENTER, TOOLTIP='Reset Rotation/Translate')

	
;	wRescale = WIDGET_BUTTON(wPlotTab, Value='Rescale', Uvalue='Rescale', $
;		Event_Pro='Dave_ReScale', /ALIGN_CENTER, TOOLTIP='Rescale The Plot')
	
	zoomSlider = WIDGET_SLIDER(wPlotTab, MINIMUM=0, MAXIMUM=100, TITLE='Zoom', $
		VALUE=zoom, UVALUE='ZSLIDER', Event_Pro='Roberto2_Zoom', /DRAG)	
	
	EulerBase = WIDGET_BASE(wPlotTab, Row =3)
	
	GetYPR, origTransform, ypr

	wYaw = CW_FSLIDER(eulerbase, Value=ypr[0], /EDIT , /DRAG, TITLE='Yaw', $
		UVALUE = 'Yaw',  Format='(f0.1)', Min=0, Max=360)
	wPitch  = CW_FSLIDER(eulerbase, Value=ypr[1], /EDIT , /DRAG,TITLE='Pitch', $
		UVALUE = 'Yaw',Format='(f0.1)', Min=0, Max=360 )
	wRoll = CW_FSLIDER(eulerbase, Value=ypr[2], /EDIT , /DRAG,TITLE='Roll', $
		UVALUE = 'Yaw', Format='(f0.1)', Min=0, Max=360 )
	
;	capBase= Widget_Base(wPlotTab, Column=1)
;	wCaps = CW_BGROUP(capbase, ['Top Cap', 'Bottom Cap'], /NONEXCLUSIVE, Set_Value=[1,1],COLUMN=2 ,$
;		  UValue='capswitch', Event_FUNC='Dave_caps'  )
	
	
;Stuff in the first tab	

;	divider = WIDGET_LABEL(wObjTab, VALUE='----------------------', /ALIGN_CENTER)
;
;	particleinfo = WIDGET_LABEL(wObjTab, VALUE='Current Selected Object:', /ALIGN_CENTER)
;	;pLabel	  = String('Nothing')
;	labelWid = WIDGET_LABEL(wObjTab, VALUE='All Objects', /Dynamic_Resize)
;
;	;divider = WIDGET_LABEL(wObjTab, VALUE='----------------------', /ALIGN_CENTER)
;	tSlider = WIDGET_SLIDER(wObjTab, MINIMUM=0, MAXIMUM=255, TITLE='Transparency', $
;		VALUE=255, UVALUE='TSLIDER', Event_Pro='Roberto2_Properties')
;	buttonBase = WIDGET_BASE(wObjTab, Column=2)
;	brFront = WIDGET_BUTTON(buttonbase, Value='Bring to Front', Event_Pro='Roberto2_Properties', UValue='brFront')
;	brForward = WIDGET_BUTTON(buttonbase, Value='Bring Forward', Event_Pro='Roberto2_Properties', UValue='brForward')
;	sndBack = WIDGET_BUTTON(buttonbase, Value='Send to Back', Event_Pro='Roberto2_Properties', UValue='sndBack')
;	sndBackward = WIDGET_BUTTON(buttonbase, Value='Send Back One', Event_Pro='Roberto2_Properties', UValue='sndBackward')

;	rSlider = WIDGET_SLIDER(wObjTab, MINIMUM=0, MAXIMUM=255, TITLE='Red', $
;		VALUE=dColor[0], UVALUE='colorSlider', Event_Pro='DaveColor_Polygons')
;	gSlider = WIDGET_SLIDER(wObjTab, MINIMUM=0, MAXIMUM=255, TITLE='Green', $
;		VALUE=dColor[1], UVALUE='colorSlider', Event_Pro='DaveColor_Polygons')
;	bSlider = WIDGET_SLIDER(wObjTab, MINIMUM=0, MAXIMUM=255, TITLE='Blue', $
;		VALUE=dColor[2], UVALUE='colorSlider', Event_Pro='DaveColor_Polygons')
;  divider = WIDGET_LABEL(wSubLeftBase2, VALUE='----------------------', /ALIGN_CENTER)
;	list = ['Fill', 'Wire', 'Points']
;	fillwire = CW_BGROUP(wObjTab, list, /EXCLUSIVE, Set_Value=0,COLUMN=3 ,$
;		   UValue='fillwire', Event_FUNC='Dave_FillWire'  )
;	;recordButton = WIDGET_BUTTON(wSubLeftBase2, Value = 'Start Recording', UValue='RECORD', Event_PRO='Roberto2_Output', Sensitive=0)
;	moviecounter = 0

;Setup the ObjPicker
;	divider = WIDGET_LABEL(wLeftBase, VALUE='----------------------')
;	title = WIDGET_LABEL(wLeftBase, VALUE='Toggle Show/Hide Object:')
;	viewlist = Intarr(N_ELements(plist)+1)+1
;	
;	list = ['All', Reform( STRCOMPRESS((plist)), N_ELEMENTS(plist) )]
;	objpicker = CW_BGROUP(wLeftBase, list, /NONEXCLUSIVE, Set_Value=viewlist,COLUMN=2 ,$
;		 /scroll, x_scroll_size=130,  UValue='objpick', Event_FUNC='Dave_OBJPicker'  )
	

; Create the right Base that has the drawing area.
	wRightbase = WIDGET_BASE(wSubBase)

	wDraw = WIDGET_DRAW(wRightBase, GRAPHICS_LEVEL=2, XSIZE=xdim, YSIZE=ydim, $
		/EXPOSE_EVENTS, /BUTTON_EVENTS, UVALUE='DRAW', RETAIN=0, RENDERER=0 )

	wGuiBase = WIDGET_BASE(wBase, /ROW)
	WIDGET_CONTROL, wBase, /REALIZE
	WIDGET_CONTROL, /HOURGLASS

; Get the window id of the drawable.
	WIDGET_CONTROL, wDraw, GET_VALUE=oWindow

; Save state
	sState = {btndown: 0b,			$ 	; Mouse Control
		dragq: 2,			$	; Drag Quality
		viewPlane: myView,		$	; View Plane for 10% zoom
		oHolder: oHolder,		$	; Holder
		oTrack:oTrack,			$ 	; Track Ball
		wDraw: wDraw,			$	; Draw
		oWindow: oWindow,		$	; Window
		oMyPolygons: omypolygons,		$	; Polygons
;		nPart:nPart,			$	; Number of particles, excluding planes/arrows
		zoomSlider:zoomSlider, 		$	; ZoomSlider
;		tSlider:tSlider, 		$	; Transparency Slider
;		rSlider:rSlider, 		$	; Red Slider
;		gSlider:gSlider, 		$	; Blue Slider
;		bSlider:bSlider, 		$	; Green Slider
;   recordbutton:recordButton, 	$	; Record button 
;		moviecounter:0,			$	; movie frame counter
;		filename:'',			$	; movie filename
;		objpicker:objpicker,		$	; Object picker interface
;		viewList:viewlist, 		$	; true/flase list for the current hide value
		origTransform:origTransform,	$	; OG zoom
		zoomVal:zoomVal, 		$	; Zoom Value
		oL1:oL1, 			$	; Light Scheme 1
		oL2:oL2, 			$	; Light Scheme 2
		oLOIM:oLOIM,			$	;OIM Light Scheme
		lightstatus:[0,1],		$	;Light scheme status
		xAxis:xaxis,			$
		yAxis:yaxis,			$
		zAxis:zaxis,			$
;		labelWid:labelWid,		$	; label of particle
;		CurrentObj:CurrentObj,		$	; currently selected object
		dColor:dColor, 		$	; OG Polygon Colors
;		uColor:uColor, 		$	; User Defined Colors
;		fillwire:fillwire,	$	; State of object for points/wire/fill
		aspect:aspect,	 	$ 	; Aspect Ratio
		wShowAxis: wShowAxis, 	$	; Show/Hide Axis Button
;		wRescale: wrescale, 	$	; Rescale Button
		wTranslate:wTranslate,	$	; translate/rotate button
		translate:0,		$	; translate status
		yaw:wYaw,		$	;yaw slider
		pitch:wpitch,		$	;pitch slider
		roll:wroll,		$	;roll slider
;		clipplanes:clipplanes,	$	;clip planes for top/bottom caps
;		wCaps:wcaps, 		$	;widget for caps
		oGroup: oGroup,		$	; Group - nonrotating group
		oMicro:oMicro,			$; Microstructure - rotatating
		oTop:oTop,			$	; Topgroup - non rotating
		oView: oView,			$	; View
		oViewGroup: oViewGroup}			; View Group
		;oOIMLedgend: oOIMLedgend,	$	; OIMLedgend Group
		;oOrientationText:oOrientationText,$	; Some info about the orientation coloring
		;OIM2DVECT: [0,0,1.0],		$	; OIM 2d color vector
		;rot:rot				}	; euler rotation angles for each particle }  

	WIDGET_CONTROL, wBase, SET_UVALUE=sState, /NO_COPY
	XMANAGER, 'Roberto2', wBase, /NO_BLOCK


	PRINT, '	Viewer Launched'

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
thisFont = Obj_New('IDLgrFont', 'Helvetica', Size=14.0)
self.thisFont = thisFont
IF N_Elements(title) EQ 0 THEN title='log!d10!n(|z|!e2!n )'

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
