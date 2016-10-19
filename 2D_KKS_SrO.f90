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
integer,parameter        :: nx=2000,ny=2000
! Spatial and time-stepping sizes
real(kind=8),parameter :: dt=.01d0,dx=1.d0,dy=1.d0
! Interfacial width controlling parameters
real(kind=8),parameter :: epsilon2=4.0d0,W=8.0d0
! Chemical and interfacial kinetic mobilities
! Al stands for the matrix alpha phase (LSCF)
! Bt stands for the beta precipitate phase (SrO)
real(kind=8),parameter :: M_al=1.d0,M_bt=1.d0,L_phi=1.0d0
! Coefficients to the free-energy curves in the form of A1(c-Cm)^2+A0
real(kind=8),parameter :: A1Al=1.d0, CmAl=0.d0, A0Al=0.0d0
real(kind=8),parameter :: A1Bt=4.d0, CmBt=1.d0, A0Bt=0.0d0


! I/O Variables
integer,parameter        :: it_st=1, it_ed=100000, it_mod=10000
character(len=100), parameter :: s = "SrO_on_LSCF"
character(len=10), parameter :: dates="161018_B"

end module simulation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Main Program
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program KKS_2D
use simulation
implicit none

real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::phi, phi_old, Conc, Conc_old, Phi_Pot
real(kind=8):: tolerance, max_c, min_c, max_phi, min_phi
integer:: iter,i,j,k

 	iter=1
  	call initial_conds(phi,Conc)
	
write(*,*) "!!!!!!!!!!!! Begin Iteration !!!!!!!!!!!!!!"
	tolerance=1.d-2
	
do while (abs(max_c-1.d0) > tolerance)
	max_c=maxval(Conc)
	!!Apply Periodic BC for the concentration
	call boundary_conds(Conc)
	!!Apply Periodic BC for the concentration
	call boundary_conds(phi)
	
	!!Diffusion Iteration
	call Diffusion_eqn(phi,Conc)
	
	if (mod(iter,it_mod) .eq. 0) then	
		call write_output(phi,Conc,iter)
	endif
	iter=iter+1	
enddo

	write(*,*) 'Done equilibrating the concentration field @ iter=', iter
	call write_output(phi,Conc,iter)
	
	k=iter
	
do iter=it_st+(k-1),it_ed

	!!Apply Periodic BC for the concentration
	call boundary_conds(Conc)
	!!Apply Periodic BC for the structural parameter
	call boundary_conds(phi)

	Conc_old=Conc
	phi_old=phi

	!!Diffusion Iteration
	call Diffusion_eqn(phi_old,Conc)
		
	call calculate_potential(phi,Conc_old,Phi_Pot)
	
	!!Apply Periodic BC for chemical potential for the Allen-Cahn eqn.
	call boundary_conds(Phi_Pot)

	!!Allen-Cahn Iteration
	call Allen_Cahn_eqn(phi,Phi_Pot)

	if (mod(iter,it_mod) .eq. 0) then	
		call write_output(phi,Conc,iter)	
	endif

enddo
write(*,*) "!!!!!!!!!!!! END Iteration !!!!!!!!!!!!!!"


end program
!*********************************************************************
!*********************************************************************
subroutine calculate_functions(Conc,phi,Hfunc,ConcAl,ConcBt,GAl,GBt,dG_dCAl,ddG_dC2,dgdphi,dHfunc_dphi)
use simulation
	real(kind=8), intent(in) ::Conc(0:nx+1,0:ny+1)
	real(kind=8), intent(in) ::phi(0:nx+1,0:ny+1)

	real(kind=8), intent(out) ::Hfunc(0:nx+1,0:ny+1)
	real(kind=8), intent(out) ::ConcAl(0:nx+1,0:ny+1)
	real(kind=8), intent(out) ::ConcBt(0:nx+1,0:ny+1)
	real(kind=8), intent(out) ::GAl(0:nx+1,0:ny+1)
	real(kind=8), intent(out) ::GBt(0:nx+1,0:ny+1)	
	real(kind=8), intent(out) ::dG_dCAl(0:nx+1,0:ny+1)
	real(kind=8), intent(out) ::ddG_dC2(0:nx+1,0:ny+1)
	real(kind=8), intent(out) ::dgdphi(0:nx+1,0:ny+1)
	real(kind=8), intent(out) ::dHfunc_dphi(0:nx+1,0:ny+1)
		
	! Monotonically increasing function	
	Hfunc=(phi**2.d0)*(3.0d0-2.d0*phi)

    ! Auxillary alpha phase composition
    ConcAl=A1Bt*(Conc-CmBt*Hfunc)+A1Al*CmAl*Hfunc
    ConcAl=ConcAl/(A1Al*Hfunc+A1Bt*(1.0d0-Hfunc))
! 	write(*,*) maxval(ConcAl),minval(ConcAl)
	! Auxillary beta phase composition
    ConcBt=A1Al*(Conc-CmAl*(1.0d0-Hfunc))+A1Bt*CmBt*(1.0d0-Hfunc)
    ConcBt=ConcBt/(A1Al*Hfunc+A1Bt*(1-Hfunc))

	! Free-energy function for each of the phases
	GAl=A1Al*(ConcAl-CmAl)**2.0d0+A0Al
	GBt=A1Bt*(ConcBt-CmBt)**2.0d0+A0Bt

	! Derivative of the Free-energy function for Alpha phase
	dG_dCAl=2.0d0*A1Al*(ConcAl-CmAl)

	! Second Derivative of the Free-energy function	w.r.t. overall composition
	ddG_dC2=((2.0d0*A1Bt)*(2.0d0*A1Al))/((1.d0-Hfunc)*(2.0d0*A1Bt)+Hfunc*(2.0d0*A1Al))	

	!First derivative of the bulk free e.g. density
	dgdphi=.5d0*W*phi*(1.0d0-phi)*(1.0d0-2.0d0*phi)

	!First derivative of the monotonically increasing function Hfunc
	dHfunc_dphi=6.0d0*phi-6.0d0*phi**2.d0
	
end subroutine
!*********************************************************************
!*********************************************************************
subroutine Allen_Cahn_eqn(phi,Phi_Pot)
use simulation
! Iterate the Allen-Cahn equation.
	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::phi, Phi_Pot
 
  	!! Allen-Cahn Iteration
	phi=phi-L_phi*Phi_Pot*dt

end subroutine
!*********************************************************************
!*********************************************************************
subroutine calculate_potential(phi,Conc,Phi_Pot)
use simulation
! Calculate the potential term in the Allen-Cahn equation.
	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::phi, Conc

	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::Hfunc, ConcAl, ConcBt, GAl, GBt
	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::dG_dCAl, ddG_dC2, dgdphi, dHfunc_dphi

	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::Phi_Pot	

	integer:: i,j

	call calculate_functions(Conc,phi,Hfunc,ConcAl,ConcBt,GAl,GBt,dG_dCAl,ddG_dC2,dgdphi,dHfunc_dphi)

	forall(i=1:nx,j=1:ny)
		Phi_Pot(i,j)=dgdphi(i,j)-eps2*((phi(i+1,j)+phi(i-1,j)-2.d0*phi(i,j))/(dx*dx) &
			+(phi(i,j+1)+phi(i,j-1)-2.d0*phi(i,j))/(dy*dy)) &
			+dHfunc_dphi(i,j)*(GBt(i,j)-GAl(i,j)-(ConcBt(i,j)-ConcAl(i,j))*dG_dCAl(i,j))
   	end forall


end subroutine
!*********************************************************************
!*********************************************************************
subroutine Diffusion_eqn(phi,Conc)
use simulation
! Iterate the diffusion equation in terms of the gradient of the chemical potential (derivative of the free-energy curve).
	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::phi, Conc, Chem_Mob, div

	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::Hfunc, ConcAl, ConcBt, GAl, GBt
	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::dG_dCAl, ddG_dC2, dgdphi, dHfunc_dphi

	integer:: i,j
	
	call calculate_functions(Conc,phi,Hfunc,ConcAl,ConcBt,GAl,GBt,dG_dCAl,ddG_dC2,dgdphi,dHfunc_dphi)
	

	!The diffusivity dependent mobility term smoothly varies between M_al and M_bt across the interfacial region with the interpolation function h(phi)
	Chem_Mob=Hfunc*M_bt+(1.d0-Hfunc)*M_al
	
	!!Divergence of M(phi) * grad(df_dc)
    forall(i=1:nx,j=1:ny)
    	div(i,j)=((Chem_Mob(i,j)+Chem_Mob(i+1,j))*(dG_dCAl(i+1,j)-dG_dCAl(i,j))-(Chem_Mob(i,j)+Chem_Mob(i-1,j))*(dG_dCAl(i,j)-dG_dCAl(i-1,j))) / (2.d0*dx*dx) + &
    			 ((Chem_Mob(i,j)+Chem_Mob(i,j+1))*(dG_dCAl(i,j+1)-dG_dCAl(i,j))-(Chem_Mob(i,j)+Chem_Mob(i,j-1))*(dG_dCAl(i,j)-dG_dCAl(i,j-1))) / (2.d0*dy*dy)
    end forall 
  	!! Diffusion Iteration
    Conc=Conc+dt*div

end subroutine
!*********************************************************************
!*********************************************************************
subroutine boundary_conds(phi)
use simulation
! Impose periodic boundary conditions
	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1) :: phi

	!!Periodic BC 
	phi(0,:)=phi(nx,:)
	phi(nx+1,:)=phi(1,:)
	phi(:,0)=phi(:,ny)
	phi(:,ny+1)=phi(:,1)	
	
end subroutine
!*********************************************************************
!*********************************************************************
subroutine initial_conds(phi,Conc)
use simulation

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

	
	filename='data/'//trim(s)//'/'//trim(dates)//'/SrO_on_LSCF_phi_t0_161017_Matlab.dat'
	write(*,*) filename
	open(1,file=filename,form='unformatted',STATUS='old',ACCESS="STREAM")
	read(1) phi(1:nx,1:ny)
	close(1)	
	
! 	Initialization of the concentration value
  	Conc(:,:)=0.2d0	

	write(*,*) "Read Initial Condition"
	write(*,*) "Maximum Value of Phi=", MAXVAL(phi) 
	write(*,*) "Minimum Value of Phi=", MINVAL(phi) 	
 	write(*,*) "Maximum Value of Conc=", MAXVAL(Conc) 
	write(*,*) "Minimum Value of Conc=", MINVAL(Conc) 
		 	
end subroutine
!*********************************************************************
!********************************************************************* 	
subroutine write_output(phi,Conc,iter)
use simulation

	real(kind=8), DIMENSION(0:nx+1,0:ny+1) ::phi,Conc
	INTEGER :: iter
	CHARACTER(LEN=100) :: filename
	CHARACTER(LEN=10) :: iteration
	CHARACTER(LEN=4) :: format_string	

	write(*,*) "Write Output at iter=",iter 
	write(*,*) "Maximum Value of Phi=", MAXVAL(phi) 
	write(*,*) "Minimum Value of Phi=", MINVAL(phi) 	
 	write(*,*) "Maximum Value of Conc=", MAXVAL(Conc) 
	write(*,*) "Minimum Value of Conc=", MINVAL(Conc) 
	
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
	open(1,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(1) phi(1:nx,1:ny)
	close(1)	

	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_Conc_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(2,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(2) Conc(1:nx,1:ny)
!	write(2) Conc(:,:)

	close(2)

	
end subroutine
!*********************************************************************
!********************************************************************* 	
subroutine read_input(Conc,phi,iter)
use simulation

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
	