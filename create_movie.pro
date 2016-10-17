PRO create_movie
 
   mainDir='/Users/challan/Desktop/Video/'
   aviFilename = mainDir+'AC_flattening.mp4'
   
   
   ; Set up the video player for output.
   video = IDLffVideoWrite(aviFilename, Format='mp4')
   framerate = 1
   nx_ori=1136
   ny_ori=942
   framedims = [nx_ori, ny_ori]
   stream = video.AddVideoStream(framedims[0], framedims[1], framerate)
   FOR iter=1,6 DO BEGIN
      image_mod=bytarr(3,nx_ori,ny_ori)  
      iter_file=string(iter,FORMAT='(i1.1)') 
      ; Read the high-resolution PNG file.
      Fig_name=mainDir+'AC_flat_t'+iter_file+'.jpg'
      image_ori = READ_IMAGE(Fig_name)
      ;File_Delete, Fig_name
      sizeArr=size(image_ori,/dimensions)
      nx=sizeArr[1]
      ny=sizeArr[2]
      print,nx,ny
      w=200
      image_mod[*,0:nx_ori-1,0:ny_ori-1]=image_ori[*,w:nx-w-1,0:ny-1]
      ; Add the high-resolution image to the video stream.
      void = video -> Put(stream, image_mod)
  ENDFOR
  

  CD, Current=currentDir
  Print, 'File "' + aviFilename + '" written to ' + currentDir + '.'

END ;*****************************************************************
