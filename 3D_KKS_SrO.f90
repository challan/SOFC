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
integer,parameter        :: nx=50, ny=50, nz=30
! Spatial and time-stepping sizes
! Spatial and time-stepping sizes
real(kind=8),parameter :: dt=.01d0,dx=1.d0,dy=1.d0,dz=1.d0
! Interfacial width controlling parameters
real(kind=8),parameter :: eps2=2.d0,W=4.d0,Conc_ini=0.41d0
! Chemical and interfacial kinetic mobilities
! M_s is surface mobility Sr (for the growth of SrO on the Sr-rich surface)
! M_b is bulk mobility of Sr in the bulk LSCF
! L_phi is interface kinetic mobility
real(kind=8),parameter :: M_s=0.1d0,M_b=.01d0,L_phi=1.0d0
! Bu stands for the bulk LSCF phase 
! Su stands for the Sr-rich surface phase
! Sr stands for SrO oxide phase
!Coefficients to the free-energy curves in the form of A1(c-Cm)^2+A0
real(kind=8),parameter :: A1Bu=1.d0, CmBu=0.4d0, A0Bu=0.1d0
real(kind=8),parameter :: A1Su=1.d0, CmSu=0.5d0, A0Su=0.1d0
real(kind=8),parameter :: A1Sr=1.d0, CmSr=0.d0, A0Sr=0.0d0

! I/O Variables
integer,parameter        :: it_st=1, it_md=100000,it_ed=500000, it_mod=50000
character(len=100), parameter :: s = "SrO_on_LSCF"
character(len=10), parameter :: dates="161102_A"

end module simulation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Main Program
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program KKS_3D
use simulation
implicit none

real(kind=8), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: psi, Conc, div
real(kind=8), DIMENSION(0:nx+1,0:ny+1) :: phi, SurfaceConc, SurfaceConc_old, SurfaceDiv, SurfaceDiv_AllPositive, Phi_Pot

real(kind=8):: tolerance, max_c, min_c, max_phi, min_phi
integer:: iter,i,j,k

integer, dimension(:), allocatable :: a, b

 	iter=1
  	call initial_conds(psi,Conc,phi,SurfaceConc)

	write(*,*) 'Done equilibrating the concentration field @ iter=', iter-1
	write(*,*) "Total Conc Value =",sum(Conc(1:nx,1:ny,1:nz)) 	
	call write_output(Conc,SurfaceConc,iter-1)
	
write(*,*) "!!!!!!!!!!!! Begin Iteration !!!!!!!!!!!!!!"
	
do iter=1,it_md

	!!Apply Periodic BC for the surface concentration
	call boundary_conds_2D(SurfaceConc)	
	! Divergence of grad mu on the surface
	call calculate_divergence_surface(phi,SurfaceConc,SurfaceDiv,SurfaceDiv_AllPositive)
	SurfaceConc=SurfaceConc+dt*SurfaceDiv
	
	!!Apply Periodic BC for the bulk concentration
	call boundary_conds_3D(Conc)
	! Divergence of grad mu on the bulk
	call calculate_divergence_bulk(psi,Conc,div)
	
	div(:,:,nz)=div(:,:,nz)+SurfaceDiv_AllPositive(:,:)
  	!! Diffusion Iteration
    Conc=Conc+dt*(div)
		
	if (mod(iter,it_mod) .eq. 0) then	
		call write_output(Conc,SurfaceConc,iter)	
	endif
enddo

	write(*,*) 'Done equilibrating the concentration field @ iter=', iter-1
!	write(*,*) "Total Conc Value =",sum(Conc(1:nx,1:ny,1:nz)) 	
!	call write_output(Conc,SurfaceConc,iter-1)
		
do iter=it_md+1,it_ed
	
	!Update the surface concentration field
	!SurfaceConc(:,:)=Conc(:,:,nz)
	
	!!Apply Periodic BC for the surface concentration
	call boundary_conds_2D(SurfaceConc)	
	!!Apply Periodic BC for the structural parameter
	call boundary_conds_2D(phi)

	! Make sure both phi iteration uses SurfaceConc at the current time step
	SurfaceConc_old=SurfaceConc

	! Divergence of grad mu on the surface
	call calculate_divergence_surface(phi,SurfaceConc,SurfaceDiv,SurfaceDiv_AllPositive)
    SurfaceConc=SurfaceConc+dt*SurfaceDiv
    
	call calculate_potential_surface(phi,SurfaceConc_old,Phi_Pot)	
	!!Apply Periodic BC for chemical potential for the Allen-Cahn eqn.
	call boundary_conds_2D(Phi_Pot)
	!! Allen-Cahn Iteration
	phi=phi-L_phi*Phi_Pot*dt

	!!Apply Periodic BC for the bulk concentration
	call boundary_conds_3D(Conc)
	! Divergence of grad mu on the bulk
	call calculate_divergence_bulk(psi,Conc,div)
    div(:,:,nz)=div(:,:,nz)+SurfaceDiv_AllPositive(:,:)        
  	!! Diffusion Iteration
    Conc=Conc+dt*div
	
	
	if (mod(iter,it_mod) .eq. 0) then	
		call write_output(Conc,SurfaceConc,iter)
	endif

enddo
write(*,*) "!!!!!!!!!!!! END Iteration !!!!!!!!!!!!!!"

end program
!*********************************************************************
!*********************************************************************
! Thermodynamic variables defined in bulk+Sr-rich surface phases.
subroutine calculate_functions_bulk(Conc,psi,Hfunc_psi,ConcB,ConcS,dG_dCB)
use simulation
implicit none

	real(kind=8), intent(in) ::Conc(0:nx+1,0:ny+1,0:nz+1)
	real(kind=8), intent(in) ::psi(0:nx+1,0:ny+1,0:nz+1)

	real(kind=8), intent(out) ::Hfunc_psi(0:nx+1,0:ny+1,0:nz+1)
	real(kind=8), intent(out) ::ConcB(0:nx+1,0:ny+1,0:nz+1)
	real(kind=8), intent(out) ::ConcS(0:nx+1,0:ny+1,0:nz+1)
	real(kind=8), intent(out) ::dG_dCB(0:nx+1,0:ny+1,0:nz+1)
		
	! Monotonically increasing function	
	Hfunc_psi=(psi**2.d0)*(3.0d0-2.d0*psi)

    ! Auxillary bulk LSCF phase composition
    ConcB=A1Su*(Conc-CmSu*Hfunc_psi)+A1Bu*CmBu*Hfunc_psi
    ConcB=ConcB/(A1Bu*Hfunc_psi+A1Su*(1.0d0-Hfunc_psi))
	! Auxillary Sr-rich surface phase composition
    ConcS=A1Bu*(Conc-CmBu*(1.0d0-Hfunc_psi))+A1Su*CmSu*(1.0d0-Hfunc_psi)
    ConcS=ConcS/(A1Bu*Hfunc_psi+A1Su*(1-Hfunc_psi))

	! Derivative of the Free-energy function for the bulk LSCF phase
	dG_dCB=2.0d0*A1Bu*(ConcB-CmBu)
	
end subroutine
!*********************************************************************
!*********************************************************************
subroutine calculate_divergence_bulk(psi,Conc,div)
use simulation
implicit none
! Iterate the diffusion equation in terms of the gradient of the chemical potential (derivative of the free-energy curve).
	real(kind=8), DIMENSION(0:nx+1,0:ny+1,0:nz+1) ::psi, Conc, Chem_Mob, div
	real(kind=8), DIMENSION(0:nx+1,0:ny+1,0:nz+1) ::Hfunc_psi, ConcB, ConcS, dG_dCB

	integer:: i,j,k
	
	call calculate_functions_bulk(Conc,psi,Hfunc_psi,ConcB,ConcS,dG_dCB)
	
	!The diffusivity dependent mobility term smoothly varies between M_b and M_s across the interfacial region with the interpolation function h(psi)
 	Chem_Mob=Hfunc_psi*M_s+(1.d0-Hfunc_psi)*M_b
		
	!!Divergence of M(phi) * grad(df_dc)
    forall(i=1:nx,j=1:ny,k=1:nz)
    	div(i,j,k)=((Chem_Mob(i,j,k)+Chem_Mob(i+1,j,k))*(dG_dCB(i+1,j,k)-dG_dCB(i,j,k))-(Chem_Mob(i,j,k)+Chem_Mob(i-1,j,k))*(dG_dCB(i,j,k)-dG_dCB(i-1,j,k))) / (2.d0*dx*dx) + &
    			 ((Chem_Mob(i,j,k)+Chem_Mob(i,j+1,k))*(dG_dCB(i,j+1,k)-dG_dCB(i,j,k))-(Chem_Mob(i,j,k)+Chem_Mob(i,j-1,k))*(dG_dCB(i,j,k)-dG_dCB(i,j-1,k))) / (2.d0*dy*dy) + &
    			 ((Chem_Mob(i,j,k)+Chem_Mob(i,j,k+1))*(dG_dCB(i,j,k+1)-dG_dCB(i,j,k))-(Chem_Mob(i,j,k)+Chem_Mob(i,j,k-1))*(dG_dCB(i,j,k)-dG_dCB(i,j,k-1))) / (2.d0*dz*dz)
    end forall

end subroutine
!*********************************************************************
!*********************************************************************
! Thermodynamic variables defined in Sr-rich surface and SrO oxide phases.
subroutine calculate_functions_surface(SurfaceConc,phi,Hfunc_phi,SurfaceConcSr,SurfaceConcSu,GSr,GSu,dG_dCSu,dgdphi,dHfunc_dphi)
use simulation
implicit none
	real(kind=8), intent(in) ::SurfaceConc(0:nx+1,0:ny+1)
	real(kind=8), intent(in) ::phi(0:nx+1,0:ny+1)

	real(kind=8), intent(out) ::Hfunc_phi(0:nx+1,0:ny+1)
	real(kind=8), intent(out) ::SurfaceConcSr(0:nx+1,0:ny+1)
	real(kind=8), intent(out) ::SurfaceConcSu(0:nx+1,0:ny+1)
	real(kind=8), intent(out) ::GSr(0:nx+1,0:ny+1)
	real(kind=8), intent(out) ::GSu(0:nx+1,0:ny+1)	
	real(kind=8), intent(out) ::dG_dCSu(0:nx+1,0:ny+1)
	real(kind=8), intent(out) ::dgdphi(0:nx+1,0:ny+1)
	real(kind=8), intent(out) ::dHfunc_dphi(0:nx+1,0:ny+1)
		
	! Monotonically increasing function	(phi=1 inside SrO and 0 in the Sr-rich surface)
	Hfunc_phi=(phi**2.d0)*(3.0d0-2.d0*phi)

    ! Auxillary surface phase composition
    SurfaceConcSu=A1Sr*(SurfaceConc-CmSr*Hfunc_phi)+A1Su*CmSu*Hfunc_phi
    SurfaceConcSu=SurfaceConcSu/(A1Su*Hfunc_phi+A1Sr*(1.0d0-Hfunc_phi))
	! Auxillary SrO phase composition
    SurfaceConcSr=A1Su*(SurfaceConc-CmSu*(1.0d0-Hfunc_phi))+A1Sr*CmSr*(1.0d0-Hfunc_phi)
    SurfaceConcSr=SurfaceConcSr/(A1Su*Hfunc_phi+A1Sr*(1-Hfunc_phi))

	! Free-energy function for each of the phases
	GSu=A1Su*(SurfaceConcSu-CmSu)**2.0d0+A0Su
	GSr=A1Sr*(SurfaceConcSr-CmSr)**2.0d0+A0Sr

	! Derivative of the Free-energy function for Surface phase
	dG_dCSu=2.0d0*A1Su*(SurfaceConcSu-CmSu)

	! First derivative of the bulk free e.g. density
	dgdphi=.5d0*W*phi*(1.0d0-phi)*(1.0d0-2.0d0*phi)

	! First derivative of the monotonically increasing function Hfunc
	dHfunc_dphi=6.0d0*phi-6.0d0*phi**2.d0
	
end subroutine
!*********************************************************************
!*********************************************************************
! Diffusion equation for Sr concentration on the Sr-rich surface where the SrO oxide grows
subroutine calculate_divergence_surface(phi,SurfaceConc,SurfaceDiv,SurfaceDiv_AllPositive)
use simulation
implicit none
! Calculate the divergence of the gradient of the chemical potential (derivative of the free-energy curve).
	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::phi, SurfaceConc, Chem_Mob, SurfaceDiv, SurfaceDiv_AllPositive

	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::Hfunc_phi, SurfaceConcSr, SurfaceConcSu, GSr, GSu
	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::dG_dCSu, dgdphi, dHfunc_dphi

	integer:: i,j
	
	call calculate_functions_surface(SurfaceConc,phi,Hfunc_phi,SurfaceConcSr,SurfaceConcSu,GSr,GSu,dG_dCSu,dgdphi,dHfunc_dphi)

	!The diffusivity dependent mobility term is treated as constant (surface mobility of Sr)
 	Chem_Mob=M_s
			
	!!Divergence of M(phi) * grad(df_dc)
    do i=1,nx
    do j=1,nx
    	SurfaceDiv(i,j)=((Chem_Mob(i,j)+Chem_Mob(i+1,j))*(dG_dCSu(i+1,j)-dG_dCSu(i,j))-(Chem_Mob(i,j)+Chem_Mob(i-1,j))*(dG_dCSu(i,j)-dG_dCSu(i-1,j))) / (2.d0*dx*dx) + &
    			 ((Chem_Mob(i,j)+Chem_Mob(i,j+1))*(dG_dCSu(i,j+1)-dG_dCSu(i,j))-(Chem_Mob(i,j)+Chem_Mob(i,j-1))*(dG_dCSu(i,j)-dG_dCSu(i,j-1))) / (2.d0*dy*dy)
    	SurfaceDiv_AllPositive(i,j)=SurfaceDiv(i,j)
    	if (SurfaceDiv_AllPositive(i,j) > 0.d0) then 
    		SurfaceDiv_AllPositive(i,j) =0.d0
    	endif
    enddo
    enddo

end subroutine
!*********************************************************************
!*********************************************************************
subroutine calculate_potential_surface(phi,SurfaceConc,Phi_Pot)
use simulation
implicit none
! Calculate the potential term in the Allen-Cahn equation.
	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::phi, SurfaceConc, Phi_Pot

	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::Hfunc_phi, SurfaceConcSr, SurfaceConcSu, GSr, GSu
	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::dG_dCSu, dgdphi, dHfunc_dphi

	integer:: i,j
	
	call calculate_functions_surface(SurfaceConc,phi,Hfunc_phi,SurfaceConcSr,SurfaceConcSu,GSr,GSu,dG_dCSu,dgdphi,dHfunc_dphi)	

	forall(i=1:nx,j=1:ny)
 		Phi_Pot(i,j)=dgdphi(i,j)-eps2*((phi(i+1,j)+phi(i-1,j)-2.d0*phi(i,j))/(dx*dx) &
 			+(phi(i,j+1)+phi(i,j-1)-2.d0*phi(i,j))/(dy*dy)) &
 			+dHfunc_dphi(i,j)*(GSr(i,j)-GSu(i,j)-(SurfaceConcSr(i,j)-SurfaceConcSu(i,j))*dG_dCSu(i,j))
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
subroutine initial_conds(psi,Conc,phi,SurfaceConc)
use simulation
implicit none

	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: Conc,psi
	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1) :: phi,SurfaceConc
	
	real(kind=8) :: x_coord, y_coord, xcenter, ycenter, radius, delta, circle
	
	integer :: i,j
	
!Domain parameter assumes a value of 1 in the Sr-rich surface phase and 0 in the bulk LSCF phase
	psi(:,:,:)=0.d0
	psi(:,:,nz:nz+1)=1.d0

!The concentration in bulk	
	Conc(:,:,:)=Conc_ini

!The concentration in surface
    SurfaceConc(:,:)=Conc_ini  		
	
!Structural parameter on the surface assumes a value of 1 in the SrO phase and 0 on the Sr-rich surface phase
	xcenter=real(nx/2,kind=dbl)
	ycenter=real(ny/2,kind=dbl)
	radius=5.d0
	delta=2.d0
	do i=1,nx
		x_coord=real(i,kind=dbl)
		do j=1,ny
			y_coord=real(j,kind=dbl)
			circle=sqrt((x_coord-xcenter)**2+(y_coord-ycenter)**2)
    		phi(i,j)=0.5d0*(1.d0+tanh((radius-circle)/delta))
		enddo
	enddo
		 	
end subroutine
!*********************************************************************
!********************************************************************* 	
subroutine write_output(Conc,SurfaceConc,iter)
use simulation
implicit none
	real(kind=8), DIMENSION(0:nx+1,0:ny+1,0:nz+1) ::Conc
	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::SurfaceConc
	INTEGER :: iter
	CHARACTER(LEN=100) :: filename
	CHARACTER(LEN=10) :: iteration
	CHARACTER(LEN=4) :: format_string	

	write(*,*) "Write Output at iter=",iter 
	write(*,*) "Total Conc Value =",sum(Conc(1:nx,1:ny,1:nz)) 	
	write(*,*) "Maximum Value of Conc=", MAXVAL(Conc) 
	write(*,*) "Minimum Value of Conc=", MINVAL(Conc) 	
 	write(*,*) "Maximum Value of SurfaceConc=", MAXVAL(SurfaceConc) 
	write(*,*) "Minimum Value of SurfaceConc=", MINVAL(SurfaceConc) 
	
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
	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_SurfaceConc_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(2,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(2) SurfaceConc(1:nx,1:ny)
	close(2)

	
end subroutine
!*********************************************************************
!********************************************************************* 	
subroutine read_input(Conc,SurfaceConc,iter)
use simulation
implicit none
	real(kind=8), DIMENSION(0:nx+1,0:ny+1,0:nz+1) ::Conc
	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::SurfaceConc
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

	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_SurfaceConc_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(2,file=filename,form='unformatted',STATUS='old')
	read(2) SurfaceConc(1:nx,1:ny)
	close(2)


	write(*,*) "Write Output at iter=",iter 
	write(*,*) "Maximum Value of Conc=", MAXVAL(Conc) 
	write(*,*) "Minimum Value of Conc=", MINVAL(Conc) 	
 	write(*,*) "Maximum Value of SurfaceConc=", MAXVAL(SurfaceConc) 
	write(*,*) "Minimum Value of SurfaceConc=", MINVAL(SurfaceConc) 	
	
end subroutine  
	