!! November 2nd 2016 - This is designed to simulate SrO growth on Sr-rich surface of LSCF cathode material. 
!! The code incorporates the KKS model for the growth of SrO on the surface and a diffusion equation 
!! to govern the concentration in both the bulk and Sr-rich surface phase.
!! KKS model comes from Kim, S. G., Kim, W. T., & Suzuki, T. (1999). PRE, 60(6 Pt B), 7186-7197.
!! The governing equations are derived in Chal's notebook5 pg61.
module simulation
implicit none

! Declare variables:---------------------------------------+
INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND (p=6, r=37) ! Single data kind
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND (p=13)      ! Double data kind
!----------------------------------------------------------+

! Simulation Variables
! System Size
integer,parameter        :: nx=200, ny=200, nz=30
! Spatial and time-stepping sizes
! Spatial and time-stepping sizes
real(kind=8),parameter :: dt=.01d0,dx=1.d0,dy=1.d0,dz=1.d0
! Interfacial width controlling parameters
real(kind=8),parameter :: eps2=2.d0,W=4.d0,Conc_ini=0.15d0
! Chemical and interfacial kinetic mobilities
! M_s is surface mobility Sr (for the growth of SrO on the Sr-rich surface)
! D_b is diffusion coefficient of Sr in the bulk LSCF
! L_phi is interface kinetic mobility
real(kind=8),parameter :: M_s=0.1d0,M_b=.01d0,L_phi=1.0d0
! Bu stands for the bulk LSCF phase 
! Su stands for the Sr-rich surface phase
! Sr stands for SrO oxide phase
!Coefficients to the free-energy curves in the form of A1(c-Cm)^2+A0
real(kind=8),parameter :: A1Su=1.d0, CmSu=0.13d0, A0Su=0.1d0
real(kind=8),parameter :: A1Sr=1.d0, CmSr=0.5d0, A0Sr=0.0d0

! I/O Variables
integer,parameter        :: it_st=100000, it_md=100000,it_ed=300000, it_mod=20000
character(len=100), parameter :: s = "SrO_on_LSCF"
character(len=10), parameter :: dates="161109_A"

end module simulation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Main Program
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program KKS_3D
use simulation
implicit none

real(kind=8), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: Conc, div, phi, phi_old, Phi_Pot, flux_x,flux_y,flux_z,dG_dCSu

real(kind=8):: tolerance, max_c, min_c, max_phi, min_phi
integer:: iter,i,j,k

	iter=0
  	!call initial_conds(Conc,phi)
	!call write_output(flux_x,flux_y,flux_z,dG_dCSu,Conc,phi,iter)
	call read_input(Conc,phi,it_st)
		
! do iter=it_st,it_md
! 
! 	!!Apply Periodic BC for the surface concentration
! 	call boundary_conds_3D(Conc)	
! 	!!Apply Periodic BC for the structural parameter
! 	call boundary_conds_3D(phi)
! 	
! 	! Divergence of grad mu
! 	call calculate_divergence_bulk(phi,Conc,div,flux_x,flux_y,flux_z,dG_dCSu)
!   	!! Diffusion Iteration
!     Conc=Conc+dt*div
! 	
! 	if (mod(iter,it_mod) .eq. 0) then	
! 		call write_output(flux_x,flux_y,flux_z,dG_dCSu,Conc,phi,iter)
! 	endif
! 
! enddo

	
do iter=it_md+1,it_ed
	
	!!Apply Periodic BC for the surface concentration
	call boundary_conds_3D(Conc)	
	!!Apply Periodic BC for the structural parameter
	call boundary_conds_3D(phi)

    phi_old=phi
	call calculate_potential_bulk(phi,Conc,Phi_Pot)	
	!!Apply Periodic BC for chemical potential for the Allen-Cahn eqn.
	call boundary_conds_3D(Phi_Pot)
	!! Allen-Cahn Iteration
	phi=phi-L_phi*Phi_Pot*dt

	!!Apply Periodic BC for the bulk concentration
	call boundary_conds_3D(Conc)
	! Divergence of grad mu on the bulk
	call calculate_divergence_bulk(phi_old,Conc,div,flux_x,flux_y,flux_z,dG_dCSu)
  	!! Diffusion Iteration
    Conc=Conc+dt*div
		
	if (mod(iter,it_mod) .eq. 0) then	
		call write_output(flux_x,flux_y,flux_z,dG_dCSu,Conc,phi,iter)
	endif

enddo
write(*,*) "!!!!!!!!!!!! END Iteration !!!!!!!!!!!!!!"

end program
!*********************************************************************
!*********************************************************************
! Thermodynamic variables defined in Sr-rich surface and SrO oxide phases.
subroutine calculate_functions_bulk(Conc,phi,Hfunc_phi,ConcSr,ConcSu,GSr,GSu,dG_dCSu,dgdphi,dHfunc_dphi)
use simulation
implicit none
	real(kind=8), intent(in) ::Conc(0:nx+1,0:ny+1,0:nz+1)
	real(kind=8), intent(in) ::phi(0:nx+1,0:ny+1,0:nz+1)
	real(kind=8), intent(out) ::Hfunc_phi(0:nx+1,0:ny+1,0:nz+1)
	real(kind=8), intent(out) ::ConcSr(0:nx+1,0:ny+1,0:nz+1)
	real(kind=8), intent(out) ::ConcSu(0:nx+1,0:ny+1,0:nz+1)
	real(kind=8), intent(out) ::GSr(0:nx+1,0:ny+1,0:nz+1)
	real(kind=8), intent(out) ::GSu(0:nx+1,0:ny+1,0:nz+1)	
	real(kind=8), intent(out) ::dG_dCSu(0:nx+1,0:ny+1,0:nz+1)
	real(kind=8), intent(out) ::dgdphi(0:nx+1,0:ny+1,0:nz+1)
	real(kind=8), intent(out) ::dHfunc_dphi(0:nx+1,0:ny+1,0:nz+1)
		
	! Monotonically increasing function	(phi=1 inside SrO and 0 in the Sr-rich surface)
	Hfunc_phi=(phi**2.d0)*(3.0d0-2.d0*phi)

    ! Auxillary surface phase composition
    ConcSu=A1Sr*(Conc-CmSr*Hfunc_phi)+A1Su*CmSu*Hfunc_phi
    ConcSu=ConcSu/(A1Su*Hfunc_phi+A1Sr*(1.0d0-Hfunc_phi))
	! Auxillary SrO phase composition
    ConcSr=A1Su*(Conc-CmSu*(1.0d0-Hfunc_phi))+A1Sr*CmSr*(1.0d0-Hfunc_phi)
    ConcSr=ConcSr/(A1Su*Hfunc_phi+A1Sr*(1-Hfunc_phi))

	! Free-energy function for each of the phases
	GSu=A1Su*(ConcSu-CmSu)**2.0d0+A0Su
	GSr=A1Sr*(ConcSr-CmSr)**2.0d0+A0Sr

	! Derivative of the Free-energy function for Surface phase
	dG_dCSu=2.0d0*A1Su*(ConcSu-CmSu)

	! First derivative of the bulk free e.g. density
	dgdphi=.5d0*W*phi*(1.0d0-phi)*(1.0d0-2.0d0*phi)

	! First derivative of the monotonically increasing function Hfunc
	dHfunc_dphi=6.0d0*phi-6.0d0*phi**2.d0
	
end subroutine
!*********************************************************************
!*********************************************************************
! Diffusion equation for Sr concentration
subroutine calculate_divergence_bulk(phi,Conc,div,flux_x,flux_y,flux_z,dG_dCSu)
use simulation
implicit none
! Calculate the divergence of the gradient of the chemical potential (derivative of the free-energy curve).
	real(kind=8), DIMENSION(0:nx+1,0:ny+1,0:nz+1) ::phi, Conc, Chem_Mob, div, flux_x, flux_y, flux_z

	real(kind=8), DIMENSION(0:nx+1,0:ny+1,0:nz+1) ::Hfunc_phi, ConcSr, ConcSu, GSr, GSu
	real(kind=8), DIMENSION(0:nx+1,0:ny+1,0:nz+1) ::dG_dCSu, dgdphi, dHfunc_dphi

	integer:: i,j,k
	
	call calculate_functions_bulk(Conc,phi,Hfunc_phi,ConcSr,ConcSu,GSr,GSu,dG_dCSu,dgdphi,dHfunc_dphi)

	! Bulk diffusivity in the bulk LSCF phase
 	Chem_Mob=M_s*Hfunc_phi+(1-Hfunc_phi)*M_b

	!!Divergence of M(phi) * grad(df_dc)
    forall(i=1:nx,j=1:ny,k=1:nz)
    div(i,j,k)=((Chem_Mob(i,j,k)+Chem_Mob(i+1,j,k))*(dG_dCSu(i+1,j,k)-dG_dCSu(i,j,k))-(Chem_Mob(i,j,k)+Chem_Mob(i-1,j,k))*(dG_dCSu(i,j,k)-dG_dCSu(i-1,j,k))) / (2.d0*dx*dx) + &
    			 ((Chem_Mob(i,j,k)+Chem_Mob(i,j+1,k))*(dG_dCSu(i,j+1,k)-dG_dCSu(i,j,k))-(Chem_Mob(i,j,k)+Chem_Mob(i,j-1,k))*(dG_dCSu(i,j,k)-dG_dCSu(i,j-1,k))) / (2.d0*dy*dy) + &
    			 ((Chem_Mob(i,j,k)+Chem_Mob(i,j,k+1))*(dG_dCSu(i,j,k+1)-dG_dCSu(i,j,k))-(Chem_Mob(i,j,k)+Chem_Mob(i,j,k-1))*(dG_dCSu(i,j,k)-dG_dCSu(i,j,k-1))) / (2.d0*dz*dz)
    flux_x(i,j,k)=-1.0d0*Chem_Mob(i,j,k)*(dG_dCSu(i+1,j,k)-dG_dCSu(i-1,j,k))/(2.d0*dx)
    flux_y(i,j,k)=-1.0d0*Chem_Mob(i,j,k)*(dG_dCSu(i,j+1,k)-dG_dCSu(i,j-1,k))/(2.d0*dy)
    flux_z(i,j,k)=-1.0d0*Chem_Mob(i,j,k)*(dG_dCSu(i,j,k+1)-dG_dCSu(i,j,k-1))/(2.d0*dz)
    end forall	

end subroutine
!*********************************************************************
!*********************************************************************
subroutine calculate_potential_bulk(phi,Conc,Phi_Pot)
use simulation
implicit none
! Calculate the potential term in the Allen-Cahn equation.
	real(kind=8), DIMENSION(0:nx+1,0:ny+1,0:nz+1) ::phi, Conc, Phi_Pot

	real(kind=8), DIMENSION(0:nx+1,0:ny+1,0:nz+1) ::Hfunc_phi, ConcSr, ConcSu, GSr, GSu
	real(kind=8), DIMENSION(0:nx+1,0:ny+1,0:nz+1) ::dG_dCSu, dgdphi, dHfunc_dphi

	integer:: i,j,k
	
	call calculate_functions_bulk(Conc,phi,Hfunc_phi,ConcSr,ConcSu,GSr,GSu,dG_dCSu,dgdphi,dHfunc_dphi)	

	! While the order parameter is defined in 3D, the oxide only grows on the surface and therefore the Laplacian is evaluated in 2D (surface Laplacian)
	forall(i=1:nx,j=1:ny,k=1:nz)
 		Phi_Pot(i,j,k)=dgdphi(i,j,k)-eps2*((phi(i+1,j,k)+phi(i-1,j,k)-2.d0*phi(i,j,k))/(dx*dx) &
 			+(phi(i,j+1,k)+phi(i,j-1,k)-2.d0*phi(i,j,k))/(dy*dy)) &
 			+dHfunc_dphi(i,j,k)*(GSr(i,j,k)-GSu(i,j,k)-(ConcSr(i,j,k)-ConcSu(i,j,k))*dG_dCSu(i,j,k))
   	end forall

end subroutine
!*********************************************************************
!*********************************************************************
subroutine boundary_conds_3D(phi)
use simulation
implicit none
! Impose no-flux Neumann boundary conditions
	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: phi

	!!no-flux BC 
	phi(0,:,:)=phi(1,:,:)
	phi(nx+1,:,:)=phi(nx,:,:)
	phi(:,0,:)=phi(:,1,:)
	phi(:,ny+1,:)=phi(:,ny,:)
	phi(:,:,0)=phi(:,:,1)
	phi(:,:,nz+1)=phi(:,:,nz)
		
end subroutine
!*********************************************************************
!*********************************************************************
subroutine boundary_conds_2D(phi)
use simulation
implicit none
! Impose no-flux Neumann boundary conditions
	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1) :: phi

	!!no-flux BC 
	phi(0,:)=phi(1,:)
	phi(nx+1,:)=phi(nx,:)
	phi(:,0)=phi(:,1)
	phi(:,ny+1)=phi(:,ny)
		
end subroutine
!*********************************************************************
!*********************************************************************
subroutine initial_conds(Conc,phi)
use simulation
implicit none

	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: Conc, phi	
	real(kind=8) :: x_coord, y_coord, xcenter, ycenter, radius, delta, circle
	
	integer :: i,j,k
	
!The concentration in the bulk	
	Conc(:,:,:)=Conc_ini

!Structural parameter in the bulk 
	phi(:,:,:)=0.d0
	
!Structural parameter on the surface assumes a value of 1 in the SrO phase and 0 on the Sr-rich surface phase
	xcenter=real(nx/2,kind=dbl)
	ycenter=real(ny/2,kind=dbl)
	radius=7.d0
	delta=2.d0
	
	k=nz
	do i=1,nx
		x_coord=real(i,kind=dbl)
		do j=1,ny
			y_coord=real(j,kind=dbl)
			circle=sqrt((x_coord-xcenter)**2+(y_coord-ycenter)**2)
    		phi(i,j,k)=0.5d0*(1.d0+tanh((radius-circle)/delta))
		enddo
	enddo
		 	
end subroutine
!*********************************************************************
!********************************************************************* 	
subroutine write_output(flux_x,flux_y,flux_z,dG_dCSu,Conc,phi,iter)
use simulation
implicit none
	real(kind=8), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: flux_x,flux_y,flux_z,dG_dCSu,Conc, phi
	INTEGER :: iter
	CHARACTER(LEN=100) :: filename
	CHARACTER(LEN=10) :: iteration
	CHARACTER(LEN=4) :: format_string	

	write(*,*) "Write Output at iter=",iter 
	write(*,*) "Total Conc Value =",sum(Conc(1:nx,1:ny,1:nz)) 	
	write(*,*) "Maximum Value of Conc=", MAXVAL(Conc) 
	write(*,*) "Minimum Value of Conc=", MINVAL(Conc) 	
 	write(*,*) "Maximum Value of phi=", MAXVAL(phi) 
	write(*,*) "Minimum Value of phi=", MINVAL(phi) 
	
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

	
	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_Conc_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(1,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(1) Conc(1:nx,1:ny,1:nz)
	close(1)	
! 
	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_phi_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(2,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(2) phi(1:nx,1:ny,1:nz)
	close(2)
	
! 	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_flux_x_t'//trim(iteration)//'_'//trim(dates)//'.dat'
! 	write(*,*) filename
! 	open(2,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
! 	write(2) flux_x(1:nx,1:ny,1:nz)
! 	close(2)
! 	
! 	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_flux_y_t'//trim(iteration)//'_'//trim(dates)//'.dat'
! 	write(*,*) filename
! 	open(2,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
! 	write(2) flux_y(1:nx,1:ny,1:nz)
! 	close(2)
! 	
! 	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_flux_z_t'//trim(iteration)//'_'//trim(dates)//'.dat'
! 	write(*,*) filename
! 	open(2,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
! 	write(2) flux_z(1:nx,1:ny,1:nz)
! 	close(2)
! 
! 	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_dG_dCSu_t'//trim(iteration)//'_'//trim(dates)//'.dat'
! 	write(*,*) filename
! 	open(2,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
! 	write(2) dG_dCSu(1:nx,1:ny,1:nz)
! 	close(2)
			
end subroutine
!*********************************************************************
!********************************************************************* 	
subroutine read_input(Conc,phi,iter)
use simulation
implicit none
	real(kind=8), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: Conc, phi
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

	
	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_Conc_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(1,file=filename,form='unformatted',STATUS='old')
	read(1) Conc(1:nx,1:ny,1:nz)
	close(1)	
! 
	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_phi_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(2,file=filename,form='unformatted',STATUS='old')
	read(2) phi(1:nx,1:ny,1:nz)
	close(2)

	write(*,*) "Write Output at iter=",iter 
	write(*,*) "Total Conc Value =",sum(Conc(1:nx,1:ny,1:nz)) 	
	write(*,*) "Maximum Value of Conc=", MAXVAL(Conc) 
	write(*,*) "Minimum Value of Conc=", MINVAL(Conc) 	
 	write(*,*) "Maximum Value of phi=", MAXVAL(phi) 
	write(*,*) "Minimum Value of phi=", MINVAL(phi) 
			
end subroutine
!*********************************************************************