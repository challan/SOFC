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
real(kind=8),parameter :: eps2=4.0d0,W=8.0d0, eta=1.d-8
! Coefficients to the free-energy curves in the form of A1(c-Cm)^2+A0
real(kind=8),parameter :: A1Al=1.d0, CmAl=0.08d0, A0Al=0.0d0
real(kind=8),parameter :: A1Bt=1.d0, CmBt=.9d0, A0Bt=0.0d0
!Equilibrium concentration of Sr in each phase
real(kind=dbl),parameter :: CSr_s=0.08d0, CSr_p=0.9d0, CSr_v=0.d0
!Contact angle of the particle on the substrate
real(kind=dbl),parameter :: angle=70.d0
! I/O Variables
integer,parameter        :: it_st=1000000, it_ed=5000000, it_mod=1000000
character(len=100), parameter :: s = "SrO_on_LSCF"
character(len=10), parameter :: dates="170109_B"

end module simulation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Main Program
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program SrO_Growth_2D
use simulation
implicit none

real(kind=DBL), DIMENSION(0:nx+1,0:ny+1) :: Conc,Pot,Conc_Dom1,Conc_Dom2,Psi_Dom1,Psi_Dom2,Pot_Dom1,Pot_Dom2
integer:: iter,i,j,interface_width

  	iter=0
  	call initial_conds(Conc,Psi_Dom1,Psi_Dom2)
! 	Conc_Dom1=Conc
! 	Conc_Dom2=Conc
! 	
! 	!Apply No-Flux BC for the concentration of Sr.
! 	call boundary_conds_conc(Conc_Dom1,Conc_Dom2)	
! 	
! 	!Calculate chem. pot. for the concentration parameter
! 	call calculate_potentials(Conc_Dom1,Conc_Dom2,Psi_Dom1,Psi_Dom2,Pot_Dom1,Pot_Dom2)	
! 	
!   	call write_output(Conc_Dom1,Conc_Dom2,Conc,Pot_Dom1,Pot_Dom2,Pot,iter)

	iter=it_st
	call read_input(Conc_Dom1,Conc_Dom2,iter)
	
	!!Calculate chem. pot. for the concentration parameter
	call calculate_potentials(Conc_Dom1,Conc_Dom2,Psi_Dom1,Psi_Dom2,Pot_Dom1,Pot_Dom2)	
	
!  	Conc=Conc_Dom1*Psi_Dom1+Conc_Dom2*Psi_Dom2
! 	do j=1,ny
! 		write(*,*) Conc(51,j)
! 	enddo
! 	stop
	
do iter=it_st+1,it_ed

	!!Apply No-Flux BC for the concentration of Sr.
	call boundary_conds_conc(Conc_Dom1,Conc_Dom2)	
	
	!!Calculate chem. pot. for the concentration parameter
	call calculate_potentials(Conc_Dom1,Conc_Dom2,Psi_Dom1,Psi_Dom2,Pot_Dom1,Pot_Dom2)
	
	!!Apply  No-Flux BC for chemical potential for the Cahn-Hilliard eqn.
	call boundary_conds_pot(Pot_Dom1,Pot_Dom2)	
	
	!! Iterate the Cahn-Hilliard Equation
	call Iterations(Conc_Dom1,Conc_Dom2,Psi_Dom1,Psi_Dom2,Pot_Dom1,Pot_Dom2,interface_width)
		
 	if (mod(iter,it_mod) .eq. 0) then
	 	Conc=Conc_Dom1*Psi_Dom1+Conc_Dom2*Psi_Dom2
		Pot=Pot_Dom1*Psi_Dom1+Pot_Dom2*Psi_Dom2
		call write_output(Conc_Dom1,Conc_Dom2,Conc,Pot_Dom1,Pot_Dom2,Pot,iter)
		write(*,*) '=============================='
		write(*,*) 'Iteration = ',iter	
		write(*,*) Conc_Dom1(51,1),Conc_Dom1(51,51)
		write(*,*) Conc_Dom2(51,1),Conc_Dom2(51,51)				
		write(*,*) '=============================='
	endif

enddo
write(*,*) "!!!!!!!!!!!! END Iteration !!!!!!!!!!!!!!"

end program
!*********************************************************************
!*********************************************************************
subroutine boundary_conds_conc(Conc_Dom1,Conc_Dom2)	
use simulation
implicit none
! Impose periodic boundary conditions
	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1) :: Conc_Dom1,Conc_Dom2

	!!No-Flux BC 
	Conc_Dom1(0,:)=Conc_Dom1(1,:)
	Conc_Dom1(nx+1,:)=Conc_Dom1(nx,:)
	Conc_Dom1(:,0)=Conc_Dom1(:,1)
	Conc_Dom1(:,ny+1)=Conc_Dom1(:,ny)
	
	Conc_Dom2(0,:)=Conc_Dom2(1,:)
	Conc_Dom2(nx+1,:)=Conc_Dom2(nx,:)
	Conc_Dom2(:,ny+1)=Conc_Dom2(:,ny)
	Conc_Dom2(:,0)=Conc_Dom2(:,1)
	
end subroutine
!*********************************************************************
!*********************************************************************
subroutine calculate_potentials(Conc_Dom1,Conc_Dom2,Psi_Dom1,Psi_Dom2,Pot_Dom1,Pot_Dom2)
use simulation
implicit none
! Impose periodic boundary conditions
	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1) :: Conc_Dom1,Conc_Dom2,Psi_Dom1,Psi_Dom2,Pot_Dom1,Pot_Dom2
	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1) :: Hfunc_Dom1,dgdc_Dom2,free_eg_Dom2 
	integer :: i,j	
	real(kind=DBL) :: pi, Conc_Dm2
	pi=4.*atan(1.)
	
	!For Domain 1
	! Derivative of the Free-energy function for substrate phase
	Pot_Dom1=2.0d0*A1Al*(Conc_Dom1-CmAl)

	!For Domain2
	!bulk free e.g. density
	free_eg_Dom2=0.25d0*W*(Conc_Dom2**2.d0)*((CSr_p-Conc_Dom2)**2.d0)
	!First derivative of the bulk free e.g. density
	dgdc_Dom2=.5d0*W*Conc_Dom2*(2.d0*Conc_Dom2-CSr_p)*(Conc_Dom2-CSr_p)
	
	forall(i=1:nx,j=1:ny)
 		Pot_Dom2(i,j)=dgdc_Dom2(i,j)-eps2*( ((Psi_Dom2(i,j)+Psi_Dom2(i+1,j))*(Conc_Dom2(i+1,j)-Conc_Dom2(i,j))-(Psi_Dom2(i,j)+Psi_Dom2(i-1,j))*(Conc_Dom2(i,j)-Conc_Dom2(i-1,j))) / (2.d0*dx*dx) + &
		((Psi_Dom2(i,j)+Psi_Dom2(i,j+1))*(Conc_Dom2(i,j+1)-Conc_Dom2(i,j))-(Psi_Dom2(i,j)+Psi_Dom2(i,j-1))*(Conc_Dom2(i,j)-Conc_Dom2(i,j-1))) / (2.d0*dy*dy) )/(Psi_Dom2(i,j)+eta) - &
		(((Psi_Dom2(i+1,j)-Psi_Dom2(i-1,j))/(2.d0*dx))**2.d0 + ((Psi_Dom2(i,j+1)-Psi_Dom2(i,j-1))/(2.d0*dy))**2.d0)**0.5d0*(eps2*2.d0*free_eg_Dom2(i,j))**0.5d0*(cos(angle*pi/180.d0))/(Psi_Dom2(i,j)+eta)
   	end forall
	

	
end subroutine
!*********************************************************************
!*********************************************************************
subroutine boundary_conds_pot(Pot_Dom1,Pot_Dom2)	
use simulation
implicit none
! Impose periodic boundary conditions
	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1) :: Pot_Dom1,Pot_Dom2

	!!No-Flux BC 
	Pot_Dom1(0,:)=Pot_Dom1(1,:)
	Pot_Dom1(nx+1,:)=Pot_Dom1(nx,:)
	Pot_Dom1(:,0)=Pot_Dom1(:,1)
	Pot_Dom1(:,ny+1)=Pot_Dom1(:,ny)

	!!No-Flux BC 
	Pot_Dom2(0,:)=Pot_Dom2(1,:)
	Pot_Dom2(nx+1,:)=Pot_Dom2(nx,:)
	Pot_Dom2(:,0)=Pot_Dom2(:,1)
	Pot_Dom2(:,ny+1)=Pot_Dom2(:,ny)
	
end subroutine
!*********************************************************************
!*********************************************************************
subroutine Iterations(Conc_Dom1,Conc_Dom2,Psi_Dom1,Psi_Dom2,Pot_Dom1,Pot_Dom2,interface_width)
use simulation
implicit none
! Impose periodic boundary conditions
	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1) :: Conc_Dom1,Conc_Dom2,Psi_Dom1,Psi_Dom2,Pot_Dom1,Pot_Dom2,div_Dom1,div_Dom2
	real(kind=DBL) :: Mob_Dom1, Mob_Dom2, dotprod_Dom1, dotprod_Dom2, mag_grad_Psi_Dom1, mag_grad_Psi_Dom2 
	integer :: i,j,interface_width
	
	Mob_Dom1=1.d0
	Mob_Dom2=1.d0
	interface_width=0
	
	do i=1,nx
	do j=1,ny
		! dotprod_Dom1 and dotprod_Dom2 are dot product between gradient of psi and gradient of concentration.
    	dotprod_Dom1=(Psi_Dom1(i+1,j)-Psi_Dom1(i-1,j))/(2.d0*dx)*(Conc_Dom1(i+1,j)-Conc_Dom1(i-1,j))/(2.d0*dx) + &
    				 (Psi_Dom1(i,j+1)-Psi_Dom1(i,j-1))/(2.d0*dy)*(Conc_Dom1(i,j+1)-Conc_Dom1(i,j-1))/(2.d0*dy)
     	dotprod_Dom2=(Psi_Dom2(i+1,j)-Psi_Dom2(i-1,j))/(2.d0*dx)*(Conc_Dom2(i+1,j)-Conc_Dom2(i-1,j))/(2.d0*dx) + &
    				 (Psi_Dom2(i,j+1)-Psi_Dom2(i,j-1))/(2.d0*dy)*(Conc_Dom2(i,j+1)-Conc_Dom2(i,j-1))/(2.d0*dy)
 
 		! mag_grad_Psi_Dom1 and mag_grad_Psi_Dom2 are the magnitudes of gradient of Psi
    	mag_grad_Psi_Dom1=(((Psi_Dom1(i+1,j)-Psi_Dom1(i-1,j))/(2.d0*dx))**2.d0 + ((Psi_Dom1(i,j+1)-Psi_Dom1(i,j-1))/(2.d0*dy))**2.d0)**0.5d0
     	mag_grad_Psi_Dom2=(((Psi_Dom2(i+1,j)-Psi_Dom2(i-1,j))/(2.d0*dx))**2.d0 + ((Psi_Dom2(i,j+1)-Psi_Dom2(i,j-1))/(2.d0*dy))**2.d0)**0.5d0

		! For Domain 1. Substrate/particle interface is defined as grad Psi_Dom1 dot grad Conc_Dom1 < 0
		! For Domain 2. Substrate/particle interface is defined as grad Psi_Dom2 dot grad Conc_Dom2 > 0

     	if (mag_grad_Psi_Dom2 .gt. 0.2d0 .and. Conc_Dom2(i,j) .gt. 0.4d0 ) then
    		interface_width=interface_width+1
    		div_Dom1(i,j)=Mob_Dom1*(((Psi_Dom1(i,j)+Psi_Dom1(i+1,j))*(Pot_Dom1(i+1,j)-Pot_Dom1(i,j))-(Psi_Dom1(i,j)+Psi_Dom1(i-1,j))*(Pot_Dom1(i,j)-Pot_Dom1(i-1,j))) / (2.d0*dx*dx) + &
    			 ((Psi_Dom1(i,j)+Psi_Dom1(i,j+1))*(Pot_Dom1(i,j+1)-Pot_Dom1(i,j))-(Psi_Dom1(i,j)+Psi_Dom1(i,j-1))*(Pot_Dom1(i,j)-Pot_Dom1(i,j-1))) / (2.d0*dy*dy)) - & 
     			 mag_grad_Psi_Dom1*(Pot_Dom1(i,j)-Pot_Dom2(i,j)+0.0518)       			 
    		div_Dom2(i,j)=Mob_Dom2*(((Psi_Dom2(i,j)+Psi_Dom2(i+1,j))*(Pot_Dom2(i+1,j)-Pot_Dom2(i,j))-(Psi_Dom2(i,j)+Psi_Dom2(i-1,j))*(Pot_Dom2(i,j)-Pot_Dom2(i-1,j))) / (2.d0*dx*dx) + &
    			 ((Psi_Dom2(i,j)+Psi_Dom2(i,j+1))*(Pot_Dom2(i,j+1)-Pot_Dom2(i,j))-(Psi_Dom2(i,j)+Psi_Dom2(i,j-1))*(Pot_Dom2(i,j)-Pot_Dom2(i,j-1))) / (2.d0*dy*dy)) - &
     			 mag_grad_Psi_Dom2*(Pot_Dom2(i,j)-Pot_Dom1(i,j)-0.0518)  	    			  								 
    	else
    		div_Dom1(i,j)=Mob_Dom1*( ((Psi_Dom1(i,j)+Psi_Dom1(i+1,j))*(Pot_Dom1(i+1,j)-Pot_Dom1(i,j))-(Psi_Dom1(i,j)+Psi_Dom1(i-1,j))*(Pot_Dom1(i,j)-Pot_Dom1(i-1,j))) / (2.d0*dx*dx) + &
    			 ((Psi_Dom1(i,j)+Psi_Dom1(i,j+1))*(Pot_Dom1(i,j+1)-Pot_Dom1(i,j))-(Psi_Dom1(i,j)+Psi_Dom1(i,j-1))*(Pot_Dom1(i,j)-Pot_Dom1(i,j-1))) / (2.d0*dy*dy) )
    			 
    		div_Dom2(i,j)=Mob_Dom2*( ((Psi_Dom2(i,j)+Psi_Dom2(i+1,j))*(Pot_Dom2(i+1,j)-Pot_Dom2(i,j))-(Psi_Dom2(i,j)+Psi_Dom2(i-1,j))*(Pot_Dom2(i,j)-Pot_Dom2(i-1,j))) / (2.d0*dx*dx) + &
    			 ((Psi_Dom2(i,j)+Psi_Dom2(i,j+1))*(Pot_Dom2(i,j+1)-Pot_Dom2(i,j))-(Psi_Dom2(i,j)+Psi_Dom2(i,j-1))*(Pot_Dom2(i,j)-Pot_Dom2(i,j-1))) / (2.d0*dy*dy) )	
		endif      			  								 

   	enddo
   	enddo	   
 	 		
	!Iteration in Domain 1
	Conc_Dom1=Conc_Dom1+dt*div_Dom1/(Psi_Dom1+eta)
	
	!Iteration in Domain 2
	Conc_Dom2=Conc_Dom2+dt*div_Dom2/(Psi_Dom2+eta)
	 
end subroutine
!*********************************************************************
!*********************************************************************
subroutine initial_conds(Conc,Psi_Dom1,Psi_Dom2)
use simulation
implicit none

	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1) :: Conc, Psi_Dom1, Psi_Dom2
	real(kind=DBL) :: xcenter, ycenter, radius, delta, circle, x_coord, y_coord, mag_grad
	integer :: i,j
	CHARACTER(LEN=100) :: filename
	CHARACTER(LEN=10) :: iteration
	CHARACTER(LEN=4) :: format_string	
			
!Concentration in the substrate	
	Conc(:,1:ny/2)=CSr_s*1.1d0

!Concentration in the vapor	
	Conc(:,ny/2+1:ny)=CSr_v

! The particle sits on the substrate and has a hemispherical shape.
	xcenter=real(nx/2,kind=dbl)
	ycenter=real(ny/2+1,kind=dbl)
	radius=10.d0
	delta=2.d0

	do i=nx/2-15,nx/2+15
		x_coord=real(i,kind=dbl)
		do j=ny/2+1,ny
			y_coord=real(j,kind=dbl)
			circle=sqrt((x_coord-xcenter)**2+(y_coord-ycenter)**2)
    		Conc(i,j)=0.5d0*(1.d0+tanh((radius-circle)/delta))*CSr_p
		enddo
	enddo	

!Domain parameters 
	Psi_Dom1(:,:)=0.0d0 ! for the substrate phase
	Psi_Dom2(:,:)=0.0d0 ! for the particle/vapor phases

	ycenter=real(ny/2+1,kind=dbl)
	delta=1.d0
	do j=1,ny
		y_coord=real(j,kind=dbl)
		Psi_Dom1(:,j)=0.5d0*(1.d0+tanh((ycenter-y_coord)/delta))
	enddo
	
	!!No-Flux BC 
	Psi_Dom1(0,:)=Psi_Dom1(1,:)
	Psi_Dom1(nx+1,:)=Psi_Dom1(nx,:)
	Psi_Dom1(:,0)=Psi_Dom1(:,1)
	Psi_Dom1(:,ny+1)=Psi_Dom1(:,ny)
	
	Psi_Dom2=1.0d0-Psi_Dom1

	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_Psi_Dom2_'//trim(dates)//'.dat'
	write(*,*) filename
	open(1,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(1) Psi_Dom2(1:nx,1:ny)
	close(1)	

! 	do i=1,nx
! 		mag_grad=abs((Psi_Dom2(51,i+1)-Psi_Dom2(51,i-1))/(2.d0*dx))
! 		if (mag_grad .gt. 1.d-2) then 
! 			write(*,*) i, mag_grad
! 		endif
! 		mag_grad=0.d0
! 	enddo
! 
! 	stop
end subroutine
!*********************************************************************
!********************************************************************* 	
subroutine write_output(Conc_Dom1,Conc_Dom2,Conc,Pot_Dom1,Pot_Dom2,Pot,iter)
use simulation
implicit none
	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1) :: Conc_Dom1,Conc_Dom2,Conc,Pot_Dom1,Pot_Dom2,Pot
	INTEGER :: iter,interface_width
	CHARACTER(LEN=100) :: filename
	CHARACTER(LEN=10) :: iteration
	CHARACTER(LEN=4) :: format_string	

	
	write(*,*) "Write Output at iter=",iter 
	write(*,*) "Maximum Value of Dom1=", MAXVAL(Conc(1:nx,1:ny/2)) 
	write(*,*) "Minimum Value of Dom1=", MINVAL(Conc(1:nx,1:ny/2))
	write(*,*) "Total Conc in Dom1=", sum(Conc(1:nx,1:ny/2))	
 	write(*,*) "Maximum Value of Dom2=", MAXVAL(Conc(1:nx,ny/2+1:ny)) 
	write(*,*) "Minimum Value of Dom2=", MINVAL(Conc(1:nx,ny/2+1:ny)) 
	write(*,*) "Total Conc in Dom2=", sum(Conc(1:nx,ny/2+1:ny))
	write(*,*) "Total Conc in Dom1+Dom2=", sum(Conc(1:nx,1:ny))
		
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

	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_Conc_Dom1_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(1,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(1) Conc_Dom1(1:nx,1:ny)
	close(1)	

	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_Conc_Dom2_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(1,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(1) Conc_Dom2(1:nx,1:ny)
	close(1)


	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_Pot_Dom1_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(1,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(1) Pot_Dom1(1:nx,1:ny)
	close(1)

	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_Pot_Dom2_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(1,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(1) Pot_Dom2(1:nx,1:ny)
	close(1)

! 	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_ChemPot_t'//trim(iteration)//'_'//trim(dates)//'.dat'
! 	write(*,*) filename
! 	open(1,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
! 	write(1) Pot(1:nx,1:ny)
! 	close(1)

end subroutine
!*********************************************************************
!********************************************************************* 	
subroutine read_input(Conc_Dom1,Conc_Dom2,iter)
use simulation
implicit none
	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1) :: Conc_Dom1,Conc_Dom2
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

	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_Conc_Dom1_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(1,file=filename,form='unformatted',STATUS='old')
	read(1) Conc_Dom1(1:nx,1:ny)
	close(1)
	
	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_Conc_Dom2_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(1,file=filename,form='unformatted',STATUS='old')
	read(1) Conc_Dom2(1:nx,1:ny)
	close(1)	

end subroutine