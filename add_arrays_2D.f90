!!================================================================================
!!================================================================================
!! October 24th 2016 - This code combines two-dimensional partitions outputted from "2D_KKS_SrO_mpi2.f90"
module simulation
implicit none

! Declare variables:---------------------------------------+
INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND (p=6, r=37) ! Single data kind
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND (p=13)      ! Double data kind
!----------------------------------------------------------+

integer:: nx=2000,ny=2000,np=4,d1=2,d2=2

end module simulation
!!================================================================================
!!================================================================================
program add_arrays
use simulation
implicit none
	call iterate
end program
!!================================================================================
!!================================================================================
subroutine iterate
use simulation
implicit none
real(kind=DBL):: phi(1:nx,1:ny) 
real(kind=DBL):: phi_temp(1:nx/d1,1:ny/d2)
integer:: output,from1,to1,from2,to2,Id2,Id1,rank,i,d
character*100  filename1
character*100  filename2
CHARACTER(LEN=2) :: rank_no
CHARACTER(LEN=4) :: format_string	

do output=1,np

	rank=output-1
	Id2 = floor(rank/real((d1)))                       ! index of each partition (starts from zero)
	Id1 = rank - id2*(d1)

	print*,rank,Id1,Id2
	from1 = nx/(d1)*(Id1)+1 
	to1   = nx/(d1)*(Id1+1)
	from2 = ny/(d2)*(Id2)+1 
	to2   = ny/(d2)*(Id2+1)

	print*,from1,to1,from2,to2
	
	if (d1*d2 < 10) then
		format_string="(i1)"	
	else 
		format_string="(i2)"	
	endif
		
	write(rank_no,format_string)rank

	filename1='data/SrO_on_LSCF/161024_A/SrO_on_LSCF_Conc_t200000_rank'//trim(rank_no)//'_161024_A.dat'
 	write(*,*) filename1
 	open  ( unit = 1, file = filename1, status='old', &
               form = 'unformatted')
	read  (1) phi_temp(1:nx/d1,1:ny/d2)
	close(1)
	phi(from1:to1,from2:to2)=phi_temp(:,:)

end do

filename2='data/SrO_on_LSCF/161024_A/SrO_on_LSCF_Conc_t200000_161024_A.dat'
write(*,*) filename2
open(UNIT=2,FILE=filename2,STATUS='REPLACE',ACTION='READWRITE',form = 'unformatted')
write(2)phi(:,:)
close(2)

end


!!Use makefile to compile
