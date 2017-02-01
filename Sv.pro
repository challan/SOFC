PRO calculate_Sv

;filenamephi='../as_fired/AC_smoothing/AIST_LSCF_0h_ACsmooth_t10_688_296_624.dat'
filenamephi='../as_fired/Surface_Diffusion/AIST_LSCF_SurfMob_t800000_0h_688_296_624.dat'
sizeArr=[688,296,624]

;filenamephi='../900c_300h/AIST_LSCF_300h_844_431_623_Stage1_2DFFT_OP_170112.dat'
;sizeArr=[844,431,623]

nx=sizeArr[0]
ny=sizeArr[1]
nz=sizeArr[2]

print,filenamephi
readFile_DBLARR, phi, filenamephi, sizeArr

print,min(phi),max(phi),mean(phi)
phi_low_half=phi[0:nx-1,0:ny-1,0:nz/2]
phi_up_half=phi[0:nx-1,0:ny-1,nz/2:nz-1]

	calculate_surface_area, phi_low_half, tri_area_low_half
	calculate_surface_area, phi_up_half, tri_area_up_half

Sv_1partition=total(tri_area_low_half+tri_area_up_half)/(double(sizeArr[0])*double(sizeArr[1])*double(sizeArr[2]))
tri_area=0b
tri_area_total=0b
print,'Sv1=',Sv_1partition
print,'1/Sv1=',1.d0/Sv_1partition
print,'Done calculating Sv from a single partition'

END

;==================================================================================================
;==================================================================================================
PRO calculate_surface_area, phi, tri_area
threshold = 0.5d0

SHADE_VOLUME, phi, threshold, v, p
CPU, TPOOL_MIN_ELTS = 20000
numberVertices2 = MESH_DECIMATE(v, p, decimatedPolygons, VERTICES = decimatedVertices)

v=decimatedVertices 
p=decimatedPolygons

;arrange the connectivity list into 4 by # of triangles. eg. first row = 3, v1, v2, v3
p = Reform(p,4, N_ELEMENTS(p)/4, /over)
ntri = n_elements(p)/4  ; # of triangles
tri_norm = fltarr(3,ntri)
tri_area = fltarr(ntri)

; Center of each triangle.

xcenter = FINDGEN(ntri)
ycenter = FINDGEN(ntri)
zcenter = FINDGEN(ntri)

;v[a,b]. a is for the axes. b is for the coordinate of the responding axes.
xcenter[*] = (v[0,p[1,*]]+v[0,p[2,*]]+v[0,p[3,*]])/3
ycenter[*] = (v[1,p[1,*]]+v[1,p[2,*]]+v[1,p[3,*]])/3
zcenter[*] = (v[2,p[1,*]]+v[2,p[2,*]]+v[2,p[3,*]])/3
;print, "Done calculating the center coordinate"
;find the distance between the two sides of each triangle to calculate the area 
;using cross product. Area of triangle=|norm|/2
;Also assume the triangle is planar.

;vector1&2 are the sides of the triangles
vector1 = v[0:2,p[2,*]] - v[0:2,p[1,*]]
vector2 = v[0:2,p[3,*]] - v[0:2,p[1,*]]


; v3 is the triangle normal.Cross product of two vectors
v3 = fltarr(3,N_elements(vector1)/3)
v3[0,*] = vector1[1,*]*vector2[2,*] - vector1[2,*]*vector2[1,*]
v3[1,*] = vector1[2,*]*vector2[0,*] - vector1[0,*]*vector2[2,*]
v3[2,*] = vector1[0,*]*vector2[1,*] - vector1[1,*]*vector2[0,*]

tri_norm=v3
v3=0b
;area of parallelogram is Sqrt(Total(tri_norm^2.,1)). area of triangle is half of area of parallelogram.
tri_area = Sqrt(Total(tri_norm^2.,1))/2.0
;print, "Done calculating the area of triangles"
END
;==================================================================================================
;==================================================================================================
PRO readFile_FLTARR, data, filename1, sizeArr
	data = FLTARR( sizeArr[0], sizeArr[1], sizeArr[2] )
	OPENR, lun, filename1, /GET_LUN, /F77_UNFORMATTED;, /SWAP_ENDIAN
	READU, lun, data;,format='(E20.12)'
	FREE_LUN, lun
	;print, '	DONE reading phi'
END
PRO readFile_DBLARR, data, filename1, sizeArr
	data = DBLARR( sizeArr[0], sizeArr[1], sizeArr[2] )
	OPENR, lun, filename1, /GET_LUN, /F77_UNFORMATTED;, /SWAP_ENDIAN
	READU, lun, data;,format='(E20.12)'
	FREE_LUN, lun
	;print, '	DONE reading phi'
END
PRO writeFortran, data, filename
       OPENW, lun, filename, /get_lun, /F77_UNFORMATTED
       writeu,lun,data
       ;printf, lun, data;, format='(E20.12)'
       FREE_LUN, lun
       PRINT, '        DONE writting'
END