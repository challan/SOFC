!! October 10th 2016 - This is a 2D KKS model code. The two governing equations employed
!! in this code are Eqn. 31 & 32 shown in Kim, S. G., Kim, W. T., & Suzuki, T. (1999). PRE, 60(6 Pt B), 7186-7197.
module simulation
implicit none

! Declare variables:---------------------------------------+
INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND (p=6, r=37) ! Single data kind
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND (p=13)      ! Double data kind
!----------------------------------------------------------+

! Simulation Variables
! System Size
integer,parameter        :: nx=100,ny=100
! Spatial and time-stepping sizes
real(kind=8),parameter :: dt=.001d0,dx=1.d0,dy=1.d0
! Interfacial width controlling parameters
real(kind=8),parameter :: eps2=4.0d0,W=8.0d0
! Coefficients to the free-energy curves in the form of A1(c-Cm)^2+A0
real(kind=8),parameter :: A1Al=1.d0, CmAl=0.08d0, A0Al=0.0d0
real(kind=8),parameter :: A1Bt=1.d0, CmBt=.82d0, A0Bt=0.0d0
!Equilibrium concentration of Sr in each phase
real(kind=dbl),parameter :: CSr_s=.08d0, CSr_p=0.9d0, CSr_v=0.d0

! I/O Variables
integer,parameter        :: it_st=1, it_ed=50000, it_mod=10000
character(len=100), parameter :: s = "SrO_on_LSCF"
character(len=10), parameter :: dates="161206_A"

end module simulation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Main Program
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program KKS_2D
use simulation
implicit none

real(kind=DBL), DIMENSION(0:nx+1,0:ny+1) :: Conc
real(kind=DBL), DIMENSION(0:nx+1,0:ny/2+1) :: Conc_Dom1, Pot_Dom1, phi_Dom1
real(kind=DBL), DIMENSION(0:nx+1,ny/2:ny+1) :: Conc_Dom2, Pot_Dom2
integer:: iter,i,j,k

	iter=0
	call initial_conds(phi_Dom1,Conc,Conc_Dom1,Conc_Dom2)
	call write_output(Conc_Dom1,Conc_Dom2,iter)
	
do iter=it_st,it_ed

	!!Apply No-Flux BC for the concentration of Sr.
	call boundary_conds_conc(phi_Dom1,Conc_Dom1,Conc_Dom2)	

	
	!!Calculate chem. pot. for the concentration parameter
	call calculate_potentials(phi_Dom1, Conc_Dom1,Conc_Dom2,Pot_Dom1,Pot_Dom2)
	
	!!Apply  No-Flux BC for chemical potential for the Cahn-Hilliard eqn.
	call boundary_conds_potentials(Conc_Dom2, Pot_Dom1,Pot_Dom2)
	
	!! Iterate the Cahn-Hilliard Equation
	call Iterations(Conc_Dom1,Conc_Dom2,Pot_Dom1,Pot_Dom2)
		
	if (mod(iter,it_mod) .eq. 0) then	
		call write_output(Conc_Dom1,Conc_Dom2,iter)
	endif

enddo
write(*,*) "!!!!!!!!!!!! END Iteration !!!!!!!!!!!!!!"

end program
!*********************************************************************
!*********************************************************************
subroutine boundary_conds_conc(phi_Dom1,Conc_Dom1,Conc_Dom2)	
use simulation
implicit none
! Impose periodic boundary conditions
	real(kind=DBL), DIMENSION(0:nx+1,0:ny/2+1) :: Conc_Dom1, phi_Dom1
	real(kind=DBL), DIMENSION(0:nx+1,ny/2:ny+1) :: Conc_Dom2
	integer :: i,j	

	
	!!No-Flux BC 
	Conc_Dom1(0,:)=Conc_Dom1(1,:)
	Conc_Dom1(nx+1,:)=Conc_Dom1(nx,:)
	Conc_Dom1(:,0)=Conc_Dom1(:,1)

	!!No-Flux BC 
	Conc_Dom2(0,:)=Conc_Dom2(1,:)
	Conc_Dom2(nx+1,:)=Conc_Dom2(nx,:)
	Conc_Dom2(:,ny+1)=Conc_Dom2(:,ny)	

	! Check where the edge of the substrate meets the particle phase
	! At the interface between the substrate and particle phase, set Conc_Dom1=Conc_Dom2.	
	do i=1,nx
		if (Conc_Dom2(i,ny/2+1) .gt. 0.5) then
			Conc_Dom1(i,ny/2+1)=Conc_Dom2(i,ny/2+1)
			Conc_Dom2(i,ny/2)=Conc_Dom1(i,ny/2)
			phi_Dom1(i,ny/2+1)=1.d0
		else
		Conc_Dom1(i,ny/2+1)=Conc_Dom1(i,ny)	
		Conc_Dom2(i,ny/2)=Conc_Dom2(i,ny/2+1)
		endif		
	enddo
	
end subroutine
!*********************************************************************
!*********************************************************************
subroutine calculate_potentials(phi_Dom1, Conc_Dom1,Conc_Dom2,Pot_Dom1,Pot_Dom2)
use simulation
implicit none
! Impose periodic boundary conditions
	real(kind=DBL), DIMENSION(0:nx+1,0:ny/2+1) :: Conc_Dom1, ConcBt, Pot_Dom1, phi_Dom1
	real(kind=DBL), DIMENSION(0:nx+1,ny/2:ny+1) :: Conc_Dom2, Pot_Dom2, dgdc
	integer :: i,j	
	
	!For Domain 1
	! Auxillary beta phase composition
	! Beta phase is the SrO phase that sits in the ghost layer (from domain 1)
    ConcBt=A1Al*(Conc_Dom1-CmAl*(1.0d0-phi_Dom1))+A1Bt*CmBt*(1.0d0-phi_Dom1)
    ConcBt=ConcBt/(A1Al*phi_Dom1+A1Bt*(1-phi_Dom1))

	! Derivative of the Free-energy function for Alpha phase
	Pot_Dom1=2.0d0*A1Bt*(ConcBt-CmBt)

	!For Domain2
	!First derivative of the bulk free e.g. density
	dgdc=.5d0*W*Conc_Dom2*(1.0d0-Conc_Dom2)*(1.0d0-2.0d0*Conc_Dom2)	
	
	forall(i=1:nx,j=ny/2+1:ny)
 		Pot_Dom2(i,j)=dgdc(i,j)-eps2*((Conc_Dom2(i+1,j)+Conc_Dom2(i-1,j)-2.d0*Conc_Dom2(i,j))/(dx*dx) &
 			+(Conc_Dom2(i,j+1)+Conc_Dom2(i,j-1)-2.d0*Conc_Dom2(i,j))/(dy*dy))
   	end forall

end subroutine
!*********************************************************************
!*********************************************************************
subroutine boundary_conds_potentials(Conc_Dom2, Pot_Dom1,Pot_Dom2)	
use simulation
implicit none
! Impose periodic boundary conditions
	real(kind=DBL), DIMENSION(0:nx+1,0:ny/2+1) :: Pot_Dom1, Conc_Dom2
	real(kind=DBL), DIMENSION(0:nx+1,ny/2:ny+1) :: Pot_Dom2
	integer :: i,j	
	
	!!No-Flux BC 
	Pot_Dom2(0,:)=Pot_Dom2(1,:)
	Pot_Dom2(nx+1,:)=Pot_Dom2(nx,:)
	Pot_Dom2(:,ny+1)=Pot_Dom2(:,ny)	
	
	!No-Flux BC at the interface between substrate and vapor phase
	!Non-zero flux at the interface between substrate and particle
	do i=1,nx
		if (Conc_Dom2(i,ny/2+1) .gt. 0.5) then
			Pot_Dom2(i,ny/2)=Pot_Dom1(i,ny/2)
		else
		Pot_Dom2(i,ny/2)=Pot_Dom2(i,ny/2+1)
		endif		
	enddo
	
end subroutine
!*********************************************************************
!*********************************************************************
subroutine Iterations(Conc_Dom1,Conc_Dom2,Pot_Dom1,Pot_Dom2)
use simulation
implicit none
! Impose periodic boundary conditions
	real(kind=DBL), DIMENSION(0:nx+1,0:ny/2+1) :: Conc_Dom1, Pot_Dom1, div_Dom1, Mob_Dom1
	real(kind=DBL), DIMENSION(0:nx+1,ny/2:ny+1) :: Conc_Dom2, Pot_Dom2, div_Dom2, Mob_Dom2
	integer :: i,j
	
	Mob_Dom1=1.d0
	Mob_Dom2=1.d0
	
	!For Domain 1
	forall(i=1:nx,j=1:ny/2)
    	div_Dom1(i,j)=((Mob_Dom1(i,j)+Mob_Dom1(i+1,j))*(Pot_Dom1(i+1,j)-Pot_Dom1(i,j))-(Mob_Dom1(i,j)+Mob_Dom1(i-1,j))*(Pot_Dom1(i,j)-Pot_Dom1(i-1,j))) / (2.d0*dx*dx) + &
    			 ((Mob_Dom1(i,j)+Mob_Dom1(i,j+1))*(Pot_Dom1(i,j+1)-Pot_Dom1(i,j))-(Mob_Dom1(i,j)+Mob_Dom1(i,j-1))*(Pot_Dom1(i,j)-Pot_Dom1(i,j-1))) / (2.d0*dy*dy)
   	end forall
	Conc_Dom1=Conc_Dom1+dt*div_Dom1
	
	!For Domain2	
	forall(i=1:nx,j=ny/2+1:ny)
    	div_Dom2(i,j)=((Mob_Dom2(i,j)+Mob_Dom2(i+1,j))*(Pot_Dom2(i+1,j)-Pot_Dom2(i,j))-(Mob_Dom2(i,j)+Mob_Dom2(i-1,j))*(Pot_Dom2(i,j)-Pot_Dom2(i-1,j))) / (2.d0*dx*dx) + &
    			 ((Mob_Dom2(i,j)+Mob_Dom2(i,j+1))*(Pot_Dom2(i,j+1)-Pot_Dom2(i,j))-(Mob_Dom2(i,j)+Mob_Dom2(i,j-1))*(Pot_Dom2(i,j)-Pot_Dom2(i,j-1))) / (2.d0*dy*dy)
   	end forall
	Conc_Dom2=Conc_Dom2+dt*div_Dom2
	    
end subroutine
!*********************************************************************
!*********************************************************************
subroutine initial_conds(phi_Dom1,Conc,Conc_Dom1,Conc_Dom2)
use simulation
implicit none

	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1) :: Conc
	real(kind=DBL), DIMENSION(0:nx+1,0:ny/2+1) :: Conc_Dom1, phi_Dom1
	real(kind=DBL), DIMENSION(0:nx+1,ny/2:ny+1) :: Conc_Dom2
	real(kind=DBL) :: xcenter, ycenter, radius, delta, circle, x_coord, y_coord
	integer :: i,j
	
!Concentration in the substrate	
	Conc(:,1:ny/2)=CSr_s*1.2d0

!Structural parameter in the substrate
	phi_Dom1(:,:)=0.0d0

!Concentration in the vapor	
	Conc(:,ny/2+1:ny)=CSr_v

! The particle sits on the substrate and has a hemispherical shape.
	xcenter=real(nx/2,kind=dbl)
	ycenter=real(ny/2+1,kind=dbl)
	radius=10.d0
	delta=2.d0

	do i=1,nx
		x_coord=real(i,kind=dbl)
		do j=ny/2+1,ny
			y_coord=real(j,kind=dbl)
			circle=sqrt((x_coord-xcenter)**2+(y_coord-ycenter)**2)
    		Conc(i,j)=0.5d0*(1.d0+tanh((radius-circle)/delta))*CSr_p
		enddo
	enddo	

	Conc_Dom1(:,1:ny/2)=Conc(:,1:ny/2)
	Conc_Dom2(:,ny/2+1:ny)=Conc(:,ny/2+1:ny)
		 	
end subroutine
!*********************************************************************
!********************************************************************* 	
subroutine write_output(Conc_Dom1,Conc_Dom2,iter)
use simulation
implicit none
	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1) :: Conc
	real(kind=DBL), DIMENSION(0:nx+1,0:ny/2+1) :: Conc_Dom1
	real(kind=DBL), DIMENSION(0:nx+1,ny/2:ny+1) :: Conc_Dom2
	INTEGER :: iter
	CHARACTER(LEN=100) :: filename
	CHARACTER(LEN=10) :: iteration
	CHARACTER(LEN=4) :: format_string	

	write(*,*) "Write Output at iter=",iter 
	write(*,*) "Maximum Value of Dom1=", MAXVAL(Conc_Dom1(1:nx,1:ny/2)) 
	write(*,*) "Minimum Value of Dom1=", MINVAL(Conc_Dom1(1:nx,1:ny/2))
	write(*,*) "Total Conc in Dom1=", sum(Conc_Dom1(1:nx,1:ny/2))	
 	write(*,*) "Maximum Value of Dom2=", MAXVAL(Conc_Dom2(1:nx,ny/2+1:ny)) 
	write(*,*) "Minimum Value of Dom2=", MINVAL(Conc_Dom2(1:nx,ny/2+1:ny)) 
	write(*,*) "Total Conc in Dom2=", sum(Conc_Dom2(1:nx,ny/2+1:ny))	
	
	if (iter < 10) then
		format_string="(i1)"	
	elseif (iter < 100) then
		format_string="(i2)"	
	elseif (iter < 1000) then
		format_string="(i3)"	
	elseif (iter < 10000) then
		format_string="(i4)"
	elseif (iter < 100000) then
		format_string="(i5)"
	elseif (iter < 1000000) then
		format_string="(i6)"
	elseif (iter < 10000000) then
		format_string="(i7)"
	else
		format_string="(i8)"
	endif
	
	write(iteration,format_string)iter	

	Conc(:,1:ny/2)=Conc_Dom1(:,1:ny/2)
	Conc(:,ny/2+1:ny)=Conc_Dom2(:,ny/2+1:ny)
	
	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_Conc_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(1,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(1) Conc(1:nx,1:ny)
	close(1)	

	
end subroutine
!*********************************************************************
!********************************************************************* 	
subroutine write_out(phi,iter)
use simulation
implicit none
	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::phi
	INTEGER :: iter
	CHARACTER(LEN=100) :: filename
	CHARACTER(LEN=10) :: iteration
	CHARACTER(LEN=4) :: format_string	
	
	if (iter < 10) then
		format_string="(i1)"	
	elseif (iter < 100) then
		format_string="(i2)"	
	elseif (iter < 1000) then
		format_string="(i3)"	
	elseif (iter < 10000) then
		format_string="(i4)"
	elseif (iter < 100000) then
		format_string="(i5)"
	elseif (iter < 1000000) then
		format_string="(i6)"
	elseif (iter < 10000000) then
		format_string="(i7)"
	else
		format_string="(i8)"
	endif
	
	write(iteration,format_string)iter	
		
	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_Phi_Pot_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(1,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(1) phi(1:nx,1:ny)
	close(1)		
	
end subroutine
!*********************************************************************
!********************************************************************* 	
subroutine read_input(Conc,phi,iter)
use simulation
implicit none
	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1) :: Conc,phi
	real(kind=8) ::max_c,min_c,max_phi,min_phi
	INTEGER :: iter,i
	CHARACTER(LEN=100) :: filename
	CHARACTER(LEN=10) :: iteration
	CHARACTER(LEN=4) :: format_string	


	if (iter < 10) then
		format_string="(i1)"	
	elseif (iter < 100) then
		format_string="(i2)"	
	elseif (iter < 1000) then
		format_string="(i3)"	
	elseif (iter < 10000) then
		format_string="(i4)"
	elseif (iter < 100000) then
		format_string="(i5)"
	elseif (iter < 1000000) then
		format_string="(i6)"
	elseif (iter < 10000000) then
		format_string="(i7)"
	else
		format_string="(i8)"
	endif
	
	write(iteration,format_string)iter


	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_phi_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(1,file=filename,form='unformatted',STATUS='old')!,ACCESS="STREAM")
	read(1) phi(1:nx,1:ny)
	close(1)	



	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_Conc_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	open(2,file=filename,form='unformatted',STATUS='old')
	read(2) Conc(1:nx,1:ny)
	close(2)


	write(*,*) "Read Input at iter=",iter
	write(*,*) "Maximum Value of Phi=", MAXVAL(phi) 
	write(*,*) "Minimum Value of Phi=", MINVAL(phi) 	
 	write(*,*) "Maximum Value of Conc=", MAXVAL(Conc) 
	write(*,*) "Minimum Value of Conc=", MINVAL(Conc) 
	
end subroutine  