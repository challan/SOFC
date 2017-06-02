!  _______         _                   _ _         
! |__   __|       | |                 (_) |        
!    | | ___  _ __| |_ _   _  ___  ___ _| |_ _   _ 
!    | |/ _ \| '__| __| | | |/ _ \/ __| | __| | | |
!    | | (_) | |  | |_| |_| | (_) \__ \ | |_| |_| |
!    |_|\___/|_|   \__|\__,_|\___/|___/_|\__|\__, |
!                                             __/ |
!                                            |___/ 
!Min-Ju Choe 2017
!Text editors such as TextWrangler and/or Atom is highly recommended (space formatting Fortran 9x).
!This program calculates the tortuosity of a porous microstructure.
!Three methods of calculating the trajectory path are employed 1) city block, 2) quasi euclidian, and 3) chess-board.
!Since large sample sizes are assumed at the final plane, we ignore pathways that do not reach the end.

! Modified by Chal Park May 30th 2017 - The quasi-euclidian subroutine is updated
! such that the same triple loop is executed twice but with different order of looping.
! This takes care of scenarios where the distance values might be different depending on 
! the distance value of the neighboring grid points at the previous plane. 

! Modified by Chal Park June 1st 2017 - This code updated to select the minimum distance value
! among 13 nearest neighboring points in the previous + current jth plane. The multiple loop
! over the ith and kth direction ensures that all possible path ways are considered.

module simulation
implicit none

! Declare variables:---------------------------------------+
INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND (p=6, r=37) ! Single data kind
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND (p=13)      ! Double data kind
!----------------------------------------------------------+
integer,parameter        ::  px=10,py=7,pz=10
end module simulation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Program Tortuosity
use simulation
Implicit None
!......................................................................................... 
Integer:: phi(1:px,1:py,1:pz)
Real:: DIST(1:px,1:py,1:pz)

phi(:,:,:)=0
! phi(5,1:py,5)=1
! phi(4,5:py,5)=1
! phi(3,py,5)=1
!! Initial Condition 1
phi(3,1:2,5)=1
phi(4,1:3,5)=1
phi(5,:,5)=1
phi(4,5:py,4:5)=1
phi(3,6:py,4:5)=1
phi(5,py,4)=1

!! Initial Condition 2
! phi(2,1:3,5)=1
! phi(2:5,4,5)=1
! phi(5,5:py,5)=1

DIST(:,:,:) = 0.d0	

	!call the subroutine to calculate tortuosity
	call quasi_euclidian(phi,DIST)

End Program Tortuosity
!   ____                  _   ______           _ _     _ _             
!  / __ \                (_) |  ____|         | (_)   | (_)            
! | |  | |_   _  __ _ ___ _  | |__  _   _  ___| |_  __| |_  __ _ _ __  
! | |  | | | | |/ _` / __| | |  __|| | | |/ __| | |/ _` | |/ _` | '_ \ 
! | |__| | |_| | (_| \__ \ | | |___| |_| | (__| | | (_| | | (_| | | | |
!  \___\_\\__,_|\__,_|___/_| |______\__,_|\___|_|_|\__,_|_|\__,_|_| |_|
!This method flags each location based on adjacency as well as edge sharing.
!Corner blocks are not considered in each iteration, but are eventually flagged when an adjacent/edge block is found.
!Tortuosity will be lower than city block.
SUBROUTINE quasi_euclidian(phi,dist)
use simulation
Implicit None
Integer :: i,j,k,l,latdir_check,count_exch													!Iterating integers
Real :: marker,counter,ydir,xdir,zdir														!Counters and markers
Integer:: phi(1:px,1:py,1:pz)
Real:: dist(1:px,1:py,1:pz)

!!The multiple do loops in the ith and kth direction ensures that the minimum
!! distance value is selected regardless of the direction of the tortuous path. 
	Do j=2,2
		Do i=2,px-1
			Do k=2,pz-1
				if (phi(i,j,k) .eq. 1) then
					call distance_calculator1(dist,phi,i,j,k)
				endif
			EndDo
			Do k=pz-1,2,-1
				if (phi(i,j,k) .eq. 1) then
					call distance_calculator1(dist,phi,i,j,k)
				endif
			EndDo			
		EndDo
		Do i=px-1,2,-1
			Do k=2,pz-1
				if (phi(i,j,k) .eq. 1) then
					call distance_calculator1(dist,phi,i,j,k)
				endif
			EndDo
			Do k=pz-1,2,-1
				if (phi(i,j,k) .eq. 1) then
					call distance_calculator1(dist,phi,i,j,k)
				endif
			EndDo			
		EndDo
	EndDo
 
	Do j=3,py
		Do i=2,px-1
			Do k=2,pz-1
				if (phi(i,j,k) .eq. 1) then
					call distance_calculator2(dist,i,j,k)
				endif
			EndDo
			Do k=pz-1,2,-1
				if (phi(i,j,k) .eq. 1) then
					call distance_calculator2(dist,i,j,k)
				endif
			EndDo			
		EndDo
		Do i=px-1,2,-1
			Do k=2,pz-1
				if (phi(i,j,k) .eq. 1) then
					call distance_calculator2(dist,i,j,k)
				endif
			EndDo
			Do k=pz-1,2,-1
				if (phi(i,j,k) .eq. 1) then
					call distance_calculator2(dist,i,j,k)
				endif
			EndDo			
		EndDo
	EndDo
	
! counter => no. of grid points with non-zero distance value at the last plane
	counter = 0
	Do i = 2,px-1; Do k = 2,pz-1
		If(DIST(i,py,k) /= 0) Then
			counter = counter + 1
		EndIf	
	EndDo; EndDo

	print*,DIST(5,py,5),DIST(4,py,5),DIST(3,py,5)
	print*,DIST(5,py,4),DIST(4,py,4),DIST(3,py,4)

	print*, 'counter: ',counter

	print*, 'Tortuosity: ', (dble(sum(DIST(2:px-1,py,2:pz-1)))/(dble(counter)))/(dble(py))

END SUBROUTINE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE	distance_calculator1(dist,phi,i,j,k)
use simulation
Implicit None
Integer :: i,j,k,phi(1:px,1:py,1:pz)
Real:: new_dist,dist(1:px,1:py,1:pz)	
!! For grid points in the second plane (especially case)
!! Finds the distance value at a grid point within the phase = 1	
!! Calculates the accumulated distance based on the predetermined distance values at 13 
!! nearest neighbors in the previous plane and in the current plane.
!! The smallest non-zero distance value is assigned.
!! The subroutine takes into account situations where the grid point is surrounded the other phase
!! in the previous and current planes (in this case it will assign the distance of 0).

if(phi(i,j-1,k) .eq. 1) then
	new_dist=dist(i,j-1,k)+1.d0
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	new_dist=0.d0
endif
if(phi(i-1,j-1,k) .eq. 1) then
	new_dist=dist(i-1,j-1,k)+sqrt(2.d0)
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	new_dist=0.d0
endif			
if(phi(i+1,j-1,k) .eq. 1) then
	new_dist=dist(i+1,j-1,k)+sqrt(2.d0)
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	new_dist=0.d0	
endif			
if(phi(i,j-1,k-1) .eq. 1) then
	new_dist=dist(i,j-1,k-1)+sqrt(2.d0)
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	new_dist=0.d0	
endif				
if(phi(i,j-1,k+1) .eq. 1) then
	new_dist=dist(i,j-1,k+1)+sqrt(2.d0)
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	new_dist=0.d0	
endif		
!! Calculate distances from the nearest neighbors within the same plane
if(dist(i-1,j,k-1) .ne. 0.d0) then
	new_dist=dist(i-1,j,k-1)+sqrt(2.d0)
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	new_dist=0.d0
endif
if(dist(i-1,j,k) .ne. 0.d0) then
	new_dist=dist(i-1,j,k)+1.d0
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	new_dist=0.d0
endif
if(dist(i-1,j,k+1) .ne. 0.d0) then
	new_dist=dist(i-1,j,k+1)+sqrt(2.d0)
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	new_dist=0.d0
endif				
if(dist(i,j,k-1) .ne. 0.d0) then
	new_dist=dist(i,j,k-1)+1.d0
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	new_dist=0.d0
endif	
if(dist(i,j,k+1) .ne. 0.d0) then
	new_dist=dist(i,j,k+1)+1.d0
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	new_dist=0.d0
endif				
if(dist(i+1,j,k-1) .ne. 0.d0) then
	new_dist=dist(i+1,j,k-1)+sqrt(2.d0)
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	new_dist=0.d0
endif	
if(dist(i+1,j,k) .ne. 0.d0) then
	new_dist=dist(i+1,j,k)+1.d0
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	new_dist=0.d0
endif
if(dist(i+1,j,k+1) .ne. 0.d0) then
	new_dist=dist(i+1,j,k+1)+sqrt(2.d0)
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	new_dist=0.d0
endif
END SUBROUTINE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE	distance_calculator2(dist,i,j,k)
use simulation
Implicit None
Integer :: i,j,k
Real:: new_dist, dist(1:px,1:py,1:pz)
!! Finds the distance value at a grid point within the phase = 1	
!! Calculates the accumulated distance based on the predetermined distance values at 13 
!! nearest neighbors in the previous plane and in the current plane.
!! The smallest non-zero distance value is assigned.
!! The subroutine takes into account situations where the grid point is surrounded the other phase
!! in the previous and current planes (in this case it will assign the distance of 0).

new_dist=0.d0

if(dist(i,j-1,k) .ne. 0.d0) then
	new_dist=dist(i,j-1,k)+1.d0
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	print*,i,j,k,dist(i,j,k),'here1'	
endif
if(dist(i-1,j-1,k) .ne. 0.d0) then
	new_dist=dist(i-1,j-1,k)+sqrt(2.d0)
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	print*,i,j,k,dist(i,j,k),'here2'
endif			
if(dist(i+1,j-1,k) .ne. 0.d0) then
	new_dist=dist(i+1,j-1,k)+sqrt(2.d0)
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	print*,i,j,k,dist(i,j,k),'here3'	
endif			
if(dist(i,j-1,k-1) .ne. 0.d0) then
	new_dist=dist(i,j-1,k-1)+sqrt(2.d0)
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	print*,i,j,k,dist(i,j,k),'here4'
endif				
if(dist(i,j-1,k+1) .ne. 0.d0) then
	new_dist=dist(i,j-1,k+1)+sqrt(2.d0)
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	print*,i,j,k,dist(i,j,k),'here5'	
endif		
!! Calculate distances from the nearest neighbors within the same plane
if(dist(i-1,j,k-1) .ne. 0.d0) then
	new_dist=dist(i-1,j,k-1)+sqrt(2.d0)
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	print*,i,j,k,dist(i,j,k),'here6'
endif
if(dist(i-1,j,k) .ne. 0.d0) then
	new_dist=dist(i-1,j,k)+1.d0
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	print*,i,j,k,dist(i,j,k),'here7'
endif
if(dist(i-1,j,k+1) .ne. 0.d0) then
	new_dist=dist(i-1,j,k+1)+sqrt(2.d0)
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	print*,i,j,k,dist(i,j,k),'here8'
endif				
if(dist(i,j,k-1) .ne. 0.d0) then
	new_dist=dist(i,j,k-1)+1.d0
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	print*,i,j,k,dist(i,j,k),'here9'
endif	
if(dist(i,j,k+1) .ne. 0.d0) then
	new_dist=dist(i,j,k+1)+1.d0
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	print*,i,j,k,dist(i,j,k),'here10'
endif
				
if(dist(i+1,j,k-1) .ne. 0.d0) then
	new_dist=dist(i+1,j,k-1)+sqrt(2.d0)
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	print*,i,j,k,dist(i,j,k),'here11'
endif	
if(dist(i+1,j,k) .ne. 0.d0) then
	new_dist=dist(i+1,j,k)+1.d0
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	print*,i,j,k,dist(i,j,k),'here12'
endif
if(dist(i+1,j,k+1) .ne. 0.d0) then
	new_dist=dist(i+1,j,k+1)+sqrt(2.d0)
		if (new_dist .lt. dist(i,j,k)) then
			dist(i,j,k)=new_dist
		elseif (dist(i,j,k) .eq. 0.d0) then
			dist(i,j,k)=new_dist
		endif
	print*,i,j,k,dist(i,j,k),'here13'
endif
END SUBROUTINE