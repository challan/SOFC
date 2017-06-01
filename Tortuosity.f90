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
Real:: DIST1(1:px,1:py,1:pz)
Real:: DIST2(1:px,1:py,1:pz)

phi(:,:,:)=0

phi(3,1:py/2-3,5)=1
phi(4,1:py/2-2,5)=1

phi(5,:,5)=1

phi(4,5:py,4:5)=1
phi(3,6:py,4:5)=1
phi(5,py,4)=1
DIST1(:,:,:) = 0.d0	
DIST2(:,:,:) = 0.d0
	!call the subroutine to calculate tortuosity
	call quasi_euclidian(phi,DIST1,DIST2)

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
SUBROUTINE quasi_euclidian(phi,DIST1,DIST2)
use simulation
Implicit None
Integer :: i,j,k,l,latdir_check,count_exch													!Iterating integers
Real :: marker,counter,ydir,xdir,zdir														!Counters and markers
Integer:: phi(1:px,1:py,1:pz)
Real:: DIST1(1:px,1:py,1:pz),DIST2(1:px,1:py,1:pz)
                                                                                                              
	ydir = 0.d0 
																																					
	Print*, 'Initialized first plane'

	Do j = 2,2
		Do i = 1,px; Do k = 1,pz
			If(i .NE. 1 .AND. i .NE. px .AND. k .NE. 1 .AND. k .NE. pz) Then		
				If(phi(i,j-1,k) == 1 .AND. phi(i,j,k) == 1) Then 							
					ydir = DIST1(i,j-1,k) + 1													
					DIST1(i,j,k) = ydir															
				EndIf
			EndIf				
		EndDo; EndDo
	EndDo	

	DIST2=DIST1
	Print*, 'Seeded first distance plane'

!! Since the tortuosity value at the last plane changes depending on the order of the loop, we 
!! repeat the same quasi-euclidian iteration but with different order of loop.	
	Do j = 3,py
		Do i = 1,px; Do k = 1,pz
			If(i .NE. 1 .AND. i .NE. px .AND. k .NE. 1 .AND. k .NE. pz) Then			
				If(phi(i,j-1,k) == 1 .AND. phi(i,j,k) == 1 .AND. DIST1(i,j-1,k) /= 0) Then 	
					ydir = DIST1(i,j-1,k) + 1															
					DIST1(i,j,k) = ydir																			
						If(phi(i+1,j,k) == 1 .AND. DIST1(i+1,j,k) == 0) Then 					!Enter this if loop if the solid is actually connected
							ydir = DIST1(i,j-1,k) + sqrt(2.0d0)									!This loop checks the new flag and its 4 adjacent points (edge to previous)						
							DIST1(i+1,j,k) = ydir															
						EndIf
						If(phi(i-1,j,k) == 1 .AND. DIST1(i-1,j,k) == 0) Then 	
							ydir = DIST1(i,j-1,k) + sqrt(2.0d0)														
							DIST1(i-1,j,k) = ydir														
						EndIf	
						If(phi(i,j,k+1) == 1 .AND. DIST1(i,j,k+1) == 0) Then 	
							ydir = DIST1(i,j-1,k) + sqrt(2.0d0)											
							DIST1(i,j,k+1) = ydir														
						EndIf					
						If(phi(i,j,k-1) == 1 .AND. DIST1(i,j,k-1) == 0) Then 	
							ydir = DIST1(i,j-1,k) + sqrt(2.0d0)										
							DIST1(i,j,k-1) = ydir													
						EndIf				
				EndIf
			EndIf					
		EndDo; EndDo
		
		Do latdir_check = 1,100 																
			Do i = px,1,-1; Do k = 1,pz
				If(i .NE. 1 .AND. i .NE. px .AND. k .NE. 1 .AND. k .NE. pz) Then		
					If(DIST1(i,j,k) /= 0 .AND. DIST1(i+1,j,k) == 0 .AND. phi(i+1,j,k) == 1) Then
						DIST1(i+1,j,k) = DIST1(i,j,k) + 1
					EndIf
					If(DIST1(i,j,k) /= 0 .AND. DIST1(i-1,j,k) == 0 .AND. phi(i-1,j,k) == 1) Then
						DIST1(i-1,j,k) = DIST1(i,j,k) + 1
					EndIf	
					If(DIST1(i,j,k) /= 0 .AND. DIST1(i,j,k+1) == 0 .AND. phi(i,j,k+1) == 1) Then
						DIST1(i,j,k+1) = DIST1(i,j,k) + 1
					EndIf	
					If(DIST1(i,j,k) /= 0 .AND. DIST1(i,j,k-1) == 0 .AND. phi(i,j,k-1) == 1) Then
						DIST1(i,j,k-1) = DIST1(i,j,k) + 1
					EndIf	

					If(DIST1(i,j,k) /= 0 .AND. DIST1(i+1,j,k+1) == 0 .AND. phi(i+1,j,k+1) == 1) Then
						DIST1(i+1,j,k+1) = DIST1(i,j,k) + sqrt(2.0d0)
					EndIf
					If(DIST1(i,j,k) /= 0 .AND. DIST1(i-1,j,k-1) == 0 .AND. phi(i-1,j,k-1) == 1) Then
						DIST1(i-1,j,k-1) = DIST1(i,j,k) + sqrt(2.0d0)
					EndIf	
					If(DIST1(i,j,k) /= 0 .AND. DIST1(i-1,j,k+1) == 0 .AND. phi(i-1,j,k+1) == 1) Then
						DIST1(i-1,j,k+1) = DIST1(i,j,k) + sqrt(2.0d0)
					EndIf	
					If(DIST1(i,j,k) /= 0 .AND. DIST1(i+1,j,k-1) == 0 .AND. phi(i+1,j,k-1) == 1) Then
						DIST1(i+1,j,k-1) = DIST1(i,j,k) + sqrt(2.0d0)
					EndIf		
				EndIf
					
			EndDo; EndDo
		EndDo	
		Print*, 'Checked connection to adjacent locations', j

	EndDo

	Do j = 3,py
		Do i = px,1,-1; Do k = 1,pz
			If(i .NE. 1 .AND. i .NE. px .AND. k .NE. 1 .AND. k .NE. pz) Then			
				If(phi(i,j-1,k) == 1 .AND. phi(i,j,k) == 1 .AND. DIST2(i,j-1,k) /= 0) Then 	
					ydir = DIST2(i,j-1,k) + 1															
					DIST2(i,j,k) = ydir																			
						If(phi(i+1,j,k) == 1 .AND. DIST2(i+1,j,k) == 0) Then 					!Enter this if loop if the solid is actually connected
							ydir = DIST2(i,j-1,k) + sqrt(2.0d0)									!This loop checks the new flag and its 4 adjacent points (edge to previous)						
							DIST2(i+1,j,k) = ydir															
						EndIf
						If(phi(i-1,j,k) == 1 .AND. DIST2(i-1,j,k) == 0) Then 	
							ydir = DIST2(i,j-1,k) + sqrt(2.0d0)														
							DIST2(i-1,j,k) = ydir														
						EndIf	
						If(phi(i,j,k+1) == 1 .AND. DIST2(i,j,k+1) == 0) Then 	
							ydir = DIST2(i,j-1,k) + sqrt(2.0d0)											
							DIST2(i,j,k+1) = ydir														
						EndIf					
						If(phi(i,j,k-1) == 1 .AND. DIST2(i,j,k-1) == 0) Then 	
							ydir = DIST2(i,j-1,k) + sqrt(2.0d0)										
							DIST2(i,j,k-1) = ydir													
						EndIf				
				EndIf
			EndIf					
		EndDo; EndDo
		
		Do latdir_check = 1,100 																
			Do i = px,1,-1; Do k = 1,pz
				If(i .NE. 1 .AND. i .NE. px .AND. k .NE. 1 .AND. k .NE. pz) Then		
					If(DIST2(i,j,k) /= 0 .AND. DIST2(i+1,j,k) == 0 .AND. phi(i+1,j,k) == 1) Then
						DIST2(i+1,j,k) = DIST2(i,j,k) + 1
					EndIf
					If(DIST2(i,j,k) /= 0 .AND. DIST2(i-1,j,k) == 0 .AND. phi(i-1,j,k) == 1) Then
						DIST2(i-1,j,k) = DIST2(i,j,k) + 1
					EndIf	
					If(DIST2(i,j,k) /= 0 .AND. DIST2(i,j,k+1) == 0 .AND. phi(i,j,k+1) == 1) Then
						DIST2(i,j,k+1) = DIST2(i,j,k) + 1
					EndIf	
					If(DIST2(i,j,k) /= 0 .AND. DIST2(i,j,k-1) == 0 .AND. phi(i,j,k-1) == 1) Then
						DIST2(i,j,k-1) = DIST2(i,j,k) + 1
					EndIf	

					If(DIST2(i,j,k) /= 0 .AND. DIST2(i+1,j,k+1) == 0 .AND. phi(i+1,j,k+1) == 1) Then
						DIST2(i+1,j,k+1) = DIST2(i,j,k) + sqrt(2.0d0)
					EndIf
					If(DIST2(i,j,k) /= 0 .AND. DIST2(i-1,j,k-1) == 0 .AND. phi(i-1,j,k-1) == 1) Then
						DIST2(i-1,j,k-1) = DIST2(i,j,k) + sqrt(2.0d0)
					EndIf	
					If(DIST2(i,j,k) /= 0 .AND. DIST2(i-1,j,k+1) == 0 .AND. phi(i-1,j,k+1) == 1) Then
						DIST2(i-1,j,k+1) = DIST2(i,j,k) + sqrt(2.0d0)
					EndIf	
					If(DIST2(i,j,k) /= 0 .AND. DIST2(i+1,j,k-1) == 0 .AND. phi(i+1,j,k-1) == 1) Then
						DIST2(i+1,j,k-1) = DIST2(i,j,k) + sqrt(2.0d0)
					EndIf		
				EndIf
					
			EndDo; EndDo
		EndDo	
		Print*, 'Checked connection to adjacent locations', j

	EndDo

! counter => no. of grid points with non-zero distance value at the last plane
! count_exch => no. of grid points with unequal distance values based on different loop direction	
	counter = 0
	count_exch = 0
	Do i = 1,px; Do j = py,py; Do k = 1,pz
		If(DIST1(i,j,k) /= 0) Then
			counter = counter + 1
		EndIf
		If(DIST2(i,j,k) .lt. DIST1(i,j,k)) Then
			DIST1(i,j,k) = DIST2(i,j,k)
			count_exch = count_exch +1
		EndIf		
	EndDo; EndDo; EndDo

	print*,DIST1(5,py,5),DIST1(4,py,5),DIST1(3,py,5)
	print*,DIST1(5,py,4),DIST1(4,py,4),DIST1(3,py,4)

	print*, 'exchanged distance values: ', count_exch
	print*, 'counter: ',counter
	print*, 'Tortuosity: ', (dble(sum(DIST1(1:px,py:py,1:pz)))/(dble(counter)))/(dble(py))

END SUBROUTINE
	

