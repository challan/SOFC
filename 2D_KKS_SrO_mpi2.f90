!! Original Author Hsun-Yi Chen. This code was designed to run SBM using 3-direction MPI
!! Edited by Chal Park. This code now runs AC dynamics using 3-direction MPI.
!! Problem of this code is that number of processors per direction comes in power of two's.
!! i.e. I can only do MPI by 2x2x2, 4x4x4, 8x8x8, 16x16x16.
!! On Kraken, where each node carries 12 CPUs, this leads to waste of CPU's as multiples of 12
!! does not add up to powers of two. 
!!
!! August 9th 2012 - This code runs CH dynamics using 3-direction MPI. The CH equation comes from 
!! Yongwoo's para_CHeq.f90
!!
!! October 16th 2015 - This code combines CH_mpi3.f90 and CH_SurfMob_single_proc.f90. 
!!
!! November 7th 2015 - Finally fed up with the shortcoming of this code where it can only read in
!!					   number of cores that is powers of two. The code can now partition in
!!					   any number of cores as long as the dimension of the data array is divisible by the number of cores
!!
!! October 24th 2016 - This code comes from CH_SurfMob_mpi3.f90. It combines the 2D_KKS_SrO.f90 code.
!!					   Thus, this code is designed for two-dimensional arrays (mpi in 2 dimensions only)

module simulation
implicit none
! Declare variables:---------------------------------------+
INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND (p=6, r=37) ! Single data kind
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND (p=13)      ! Double data kind
!----------------------------------------------------------+

INCLUDE 'mpif.h'
INTEGER, PARAMETER :: d1 = 8, d2 = 8       ! partition number in each direction 
INTEGER, PARAMETER :: bb = 1                       ! layers of ghost grid
INTEGER, PARAMETER :: r1Tag=101,l1Tag=102,f1Tag=103,b1Tag=104    ! communicate tag

integer,parameter        :: nx=2000,ny=2000
! Spatial and time-stepping sizes
real(kind=8),parameter :: dt=.0001d0,dx=1.d0,dy=1.d0
! Interfacial width controlling parameters
real(kind=8),parameter :: eps2=4.0d0,W=8.0d0
! Chemical and interfacial kinetic mobilities
! Al stands for the matrix alpha phase (LSCF)
! Bt stands for the beta precipitate phase (SrO)
real(kind=8),parameter :: M_al=0.22d0,M_bt=1.25d0,L_phi=1.0d0
! Coefficients to the free-energy curves in the form of A1(c-Cm)^2+A0
real(kind=8),parameter :: A1Al=1.d0, CmAl=0.d0, A0Al=0.0d0
real(kind=8),parameter :: A1Bt=200.d0, CmBt=1.d0, A0Bt=0.0d0

! I/O Variables
integer,parameter        :: it_st=450000, it_md=500000, it_ed=550000, it_mod=50000
character(len=100), parameter :: s = "SrO_on_LSCF"
character(len=10), parameter :: dates="161025_A"
character(len=100), parameter :: outdir="data/SrO_on_LSCF/161025_A/"

end module simulation
!*********************************************************************
!*********************************************************************
!*********************************************************************
!*********************************************************************
PROGRAM KKS_SrO_mpi2
use simulation
implicit none

INTEGER errcode
INTEGER rank, nsize
INTEGER L1, L2, S1, S2, Id1, Id2, Rm1, Rm2
INTEGER nstatus(MPI_STATUS_SIZE)

call MPI_INIT (errcode)

call MPI_COMM_RANK (MPI_COMM_WORLD, rank, errcode)
call MPI_COMM_SIZE (MPI_COMM_WORLD, nsize, errcode)
!write(*,*)'rank=',rank

L1 = floor(nx/real(d1))                           ! non-ghost length of each partition
Rm1 = mod(nx,d1)                                  ! remanet grids, will feed to the last partition 
L2 = floor(ny/real(d2))
Rm2 = mod(ny,d2)

Id2 = floor(rank/real((d1)))                       ! index of each partition (starts from zero)
Id1 = rank - id2*(d1)

if(Id1 == 0)then                                    ! starting point of domain is set to 1, end point is nx
   S1 = 1-bb										! starting point of each partition
   L1=L1+2*bb										! the length of each partition including the ghost layers
elseif(Id1 == d1-1)then   
   S1 = Id1*L1+1-bb
   L1 = L1+2*bb+Rm1
else
   S1 = Id1*L1+1-bb
   L1= L1+2*bb
endif

if(Id2 == 0)then                                          ! starting point of domain is set to 1, end point is ny
   S2 = 1-bb
   L2= L2+2*bb
elseif(Id2 == d2-1)then   
   S2 = Id2*L2+1-bb
   L2 = L2+2*bb+Rm2
else
   S2 = Id2*L2+1-bb
   L2 = L2+2*bb
endif


call main(rank,nsize,Id1,Id2,S1,S2,L1,L2)

call MPI_FINALIZE(errcode)

END PROGRAM
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE main(rank,nsize,Id1,Id2,S1,S2,L1,L2)
use simulation
implicit none

INTEGER errcode,nstatus(MPI_STATUS_SIZE)
INTEGER L1, L2, S1, S2, Id1, Id2, Is1, Is2, Ie1, Ie2, rank, nsize
real(kind=DBL), DIMENSION(S1:S1+L1-1,S2:S2+L2-1) ::phi, phi_old, Conc, Conc_old, Phi_Pot
real(kind=8):: tolerance, max_c, min_c, max_phi, min_phi
integer:: iter,i,j,k
character(len=10) currenttime
! real grid points inside the subdomain (without halo cells) 
Is1 = S1 + bb
Ie1 = S1 + L1 - 1 - bb
Is2 = S2 + bb
Ie2 = S2 + L2 - 1 - bb


call initial_conds(rank,Is1,Ie1,Is2,Ie2,phi,Conc)
	

do iter=it_st+1,it_md
	
	!!Apply Periodic BC for the concentration
	call commuBC(rank,Id1,Id2,L1,L2,S1,S2,Conc)
	!!Apply Periodic BC for the concentration
	call commuBC(rank,Id1,Id2,L1,L2,S1,S2,phi)
	
	!!Diffusion Iteration
	call Diffusion_eqn(Is1,Ie1,Is2,Ie2,phi,Conc)
	
	if (mod(iter,it_mod) .eq. 0) then
		if (rank .eq. 0) then	
			call DATE_AND_TIME(time=currenttime) 
			write(*,*) 'Current Time = ',currenttime
		endif	 
		call write_output(rank,Is1,Ie1,Is2,Ie2,phi,Conc,iter)		
	endif	
		
	iter=iter+1	
enddo

	
do iter=it_md+1,it_ed

	!!Apply Periodic BC for the concentration
	call commuBC(rank,Id1,Id2,L1,L2,S1,S2,Conc)

	!!Apply Periodic BC for the structural parameter
	call commuBC(rank,Id1,Id2,L1,L2,S1,S2,phi)

	Conc_old=Conc
	phi_old=phi

	!!Diffusion Iteration
	call Diffusion_eqn(Is1,Ie1,Is2,Ie2,phi_old,Conc)
	
	call calculate_potential(Is1,Ie1,Is2,Ie2,phi,Conc_old,Phi_Pot)

	!!Apply Periodic BC for chemical potential for the Allen-Cahn eqn.
	call commuBC(rank,Id1,Id2,L1,L2,S1,S2,Phi_Pot)

	!!Allen-Cahn Iteration
	call Allen_Cahn_eqn(Is1,Ie1,Is2,Ie2,phi,Phi_Pot)
	
	if (mod(iter,it_mod) .eq. 0) then
		if (rank .eq. 0) then	
			call DATE_AND_TIME(time=currenttime) 
			write(*,*) 'Current Time = ',currenttime
		endif	 
		call write_output(rank,Is1,Ie1,Is2,Ie2,phi,Conc,iter)		
	endif

enddo

if (rank .eq. 0) then	
	write(*,*) "!!!!!!!!!!!! END Iteration !!!!!!!!!!!!!!"
endif


END SUBROUTINE main
!*********************************************************************
!*********************************************************************
subroutine calculate_functions(Is1,Ie1,Is2,Ie2,Conc,phi,Hfunc,ConcAl,ConcBt,GAl,GBt,dG_dCAl,ddG_dC2,dgdphi,dHfunc_dphi)
use simulation
implicit none

	real(kind=8), intent(in) ::Conc(Is1-1:Ie1+1,Is2-1:Ie2+1)
	real(kind=8), intent(in) ::phi(Is1-1:Ie1+1,Is2-1:Ie2+1)

	real(kind=8), intent(out) ::Hfunc(Is1-1:Ie1+1,Is2-1:Ie2+1)
	real(kind=8), intent(out) ::ConcAl(Is1-1:Ie1+1,Is2-1:Ie2+1)
	real(kind=8), intent(out) ::ConcBt(Is1-1:Ie1+1,Is2-1:Ie2+1)
	real(kind=8), intent(out) ::GAl(Is1-1:Ie1+1,Is2-1:Ie2+1)
	real(kind=8), intent(out) ::GBt(Is1-1:Ie1+1,Is2-1:Ie2+1)	
	real(kind=8), intent(out) ::dG_dCAl(Is1-1:Ie1+1,Is2-1:Ie2+1)
	real(kind=8), intent(out) ::ddG_dC2(Is1-1:Ie1+1,Is2-1:Ie2+1)
	real(kind=8), intent(out) ::dgdphi(Is1-1:Ie1+1,Is2-1:Ie2+1)
	real(kind=8), intent(out) ::dHfunc_dphi(Is1-1:Ie1+1,Is2-1:Ie2+1)
	
	INTEGER Is1, Is2, Ie1, Ie2
		
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
subroutine Allen_Cahn_eqn(Is1,Ie1,Is2,Ie2,phi,Phi_Pot)
use simulation
implicit none

! Iterate the Allen-Cahn equation.
	real(kind=8), DIMENSION(Is1-1:Ie1+1,Is2-1:Ie2+1) ::phi, Phi_Pot
	INTEGER Is1, Is2, Ie1, Ie2
 
  	!! Allen-Cahn Iteration
	phi=phi-L_phi*Phi_Pot*dt
	
end subroutine
!*********************************************************************
!*********************************************************************
subroutine calculate_potential(Is1,Ie1,Is2,Ie2,phi,Conc,Phi_Pot)
use simulation
implicit none
! Calculate the potential term in the Allen-Cahn equation.
	real(kind=8), DIMENSION(Is1-1:Ie1+1,Is2-1:Ie2+1) ::phi, Conc

	real(kind=8), DIMENSION(Is1-1:Ie1+1,Is2-1:Ie2+1) ::Hfunc, ConcAl, ConcBt, GAl, GBt
	real(kind=8), DIMENSION(Is1-1:Ie1+1,Is2-1:Ie2+1) ::dG_dCAl, ddG_dC2, dgdphi, dHfunc_dphi

	real(kind=8), DIMENSION(Is1-1:Ie1+1,Is2-1:Ie2+1) ::Phi_Pot	

	integer:: i, j, Is1, Is2, Ie1, Ie2

	call calculate_functions(Is1,Ie1,Is2,Ie2,Conc,phi,Hfunc,ConcAl,ConcBt,GAl,GBt,dG_dCAl,ddG_dC2,dgdphi,dHfunc_dphi)

	forall(i=Is1:Ie1,j=Is2:Ie2)
 		Phi_Pot(i,j)=dgdphi(i,j)-eps2*((phi(i+1,j)+phi(i-1,j)-2.d0*phi(i,j))/(dx*dx) &
 			+(phi(i,j+1)+phi(i,j-1)-2.d0*phi(i,j))/(dy*dy)) &
 			+dHfunc_dphi(i,j)*(GBt(i,j)-GAl(i,j)-(ConcBt(i,j)-ConcAl(i,j))*dG_dCAl(i,j))
   	end forall

! 	write(*,*) 'GAl min=',minval(GAl),', max=',maxval(GAl)
! 	write(*,*) 'GBt min=',minval(GBt),', max=',maxval(GBt)
! 	write(*,*) 'ConcAl min=',minval(ConcAl),', max=',maxval(ConcAl)
! 	write(*,*) 'ConcBt min=',minval(ConcBt),', max=',maxval(ConcBt)
! 	write(*,*) 'dG_dCAl min=',minval(dG_dCAl),', max=',maxval(dG_dCAl)
! 	write(*,*) 'dHfunc_dphi min=',minval(dHfunc_dphi),', max=',maxval(dHfunc_dphi)
! 	write(*,*) 'Phi_Pot min=',minval(Phi_Pot),', max=',maxval(Phi_Pot)

end subroutine
!*********************************************************************
!*********************************************************************
subroutine Diffusion_eqn(Is1,Ie1,Is2,Ie2,phi,Conc)
use simulation
implicit none

! Iterate the diffusion equation in terms of the gradient of the chemical potential (derivative of the free-energy curve).
	real(kind=8), DIMENSION(Is1-1:Ie1+1,Is2-1:Ie2+1) ::phi, Conc, Chem_Mob, div

	real(kind=8), DIMENSION(Is1-1:Ie1+1,Is2-1:Ie2+1) ::Hfunc, ConcAl, ConcBt, GAl, GBt
	real(kind=8), DIMENSION(Is1-1:Ie1+1,Is2-1:Ie2+1) ::dG_dCAl, ddG_dC2, dgdphi, dHfunc_dphi

	integer:: i,j,Is1,Ie1,Is2,Ie2
	
	call calculate_functions(Is1,Ie1,Is2,Ie2,Conc,phi,Hfunc,ConcAl,ConcBt,GAl,GBt,dG_dCAl,ddG_dC2,dgdphi,dHfunc_dphi)
	
	!The diffusivity dependent mobility term smoothly varies between M_al and M_bt across the interfacial region with the interpolation function h(phi)
 	Chem_Mob=Hfunc*M_bt+(1.d0-Hfunc)*M_al
		
	!!Divergence of M(phi) * grad(df_dc)
    forall(i=Is1:Ie1,j=Is2:Ie2)
    	div(i,j)=((Chem_Mob(i,j)+Chem_Mob(i+1,j))*(dG_dCAl(i+1,j)-dG_dCAl(i,j))-(Chem_Mob(i,j)+Chem_Mob(i-1,j))*(dG_dCAl(i,j)-dG_dCAl(i-1,j))) / (2.d0*dx*dx) + &
    			 ((Chem_Mob(i,j)+Chem_Mob(i,j+1))*(dG_dCAl(i,j+1)-dG_dCAl(i,j))-(Chem_Mob(i,j)+Chem_Mob(i,j-1))*(dG_dCAl(i,j)-dG_dCAl(i,j-1))) / (2.d0*dy*dy)
    end forall 
  	!! Diffusion Iteration
    Conc=Conc+dt*div

end subroutine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE commuBC(rank,Id1,Id2,L1,L2,S1,S2,phi1)
use simulation
implicit none

INTEGER S1, S2, Id1, Id2, L1, L2
INTEGER nstatus(MPI_STATUS_SIZE), errcode, rank
REAL(KIND=8) :: phi1(S1:S1+L1-1,S2:S2+L2-1)

! Interior boundary condition and exterior boundary condition. all periodic
! communication in nx direction
IF(d1>0)THEN

  !  Interior boundary condition
  If(Id1 .NE. d1-1)call MPI_SEND(phi1(S1+L1-1-bb,S2:S2+L2-1),bb*L2,MPI_DOUBLE_PRECISION,rank+1,r1Tag,MPI_COMM_WORLD,errcode)    !Send real grid
  If(Id1 .NE. 0)call MPI_RECV(phi1(S1,S2:S2+L2-1), bb*L2,MPI_DOUBLE_PRECISION,rank-1,r1Tag,MPI_COMM_WORLD,nstatus,errcode)          	!receive ghost grid
  If(Id1 .NE. 0)call MPI_SEND(phi1(S1+bb,S2:S2+L2-1), bb*L2,MPI_DOUBLE_PRECISION,rank-1,l1Tag,MPI_COMM_WORLD,errcode)			!Send real grid
  If(Id1 .NE. d1-1)call MPI_RECV(phi1(S1+L1-1,S2:S2+L2-1),bb*L2,MPI_DOUBLE_PRECISION,rank+1,l1Tag,MPI_COMM_WORLD,nstatus,errcode)		!receive ghost grid
  !	 Exterior boundary condition
  If(Id1 .EQ. d1-1)call MPI_SEND(phi1(S1+L1-1-bb,S2:S2+L2-1),bb*L2,MPI_DOUBLE_PRECISION,rank-(d1-1),r1Tag,MPI_COMM_WORLD,errcode)
  If(Id1 .EQ. 0)call MPI_RECV(phi1(S1,S2:S2+L2-1), bb*L2,MPI_DOUBLE_PRECISION,rank+(d1-1),r1Tag,MPI_COMM_WORLD,nstatus,errcode)
  If(Id1 .EQ. 0)call MPI_SEND(phi1(S1+bb,S2:S2+L2-1), bb*L2,MPI_DOUBLE_PRECISION,rank+(d1-1),l1Tag,MPI_COMM_WORLD,errcode)
  If(Id1 .EQ. d1-1)call MPI_RECV(phi1(S1+L1-1,S2:S2+L2-1),bb*L2,MPI_DOUBLE_PRECISION,rank-(d1-1),l1Tag,MPI_COMM_WORLD,nstatus,errcode)
  
END IF
!  communication in ny direction
IF(d2>0)THEN

  !  Interior boundary condition
  If(Id2 .NE. d2-1)call MPI_SEND(phi1(S1:S1+L1-1,S2+L2-1-bb),L1*bb,MPI_DOUBLE_PRECISION,rank+d1,f1Tag,MPI_COMM_WORLD,errcode)
  If(Id2 .NE. 0)call MPI_RECV(phi1(S1:S1+L1-1,S2),L1*bb,MPI_DOUBLE_PRECISION,rank-d1,f1Tag,MPI_COMM_WORLD,nstatus,errcode)
  If(Id2 .NE. 0)call MPI_SEND(phi1(S1:S1+L1-1,S2+bb),L1*bb,MPI_DOUBLE_PRECISION,rank-d1,b1Tag,MPI_COMM_WORLD,errcode)
  If(Id2 .NE. d2-1)call MPI_RECV(phi1(S1:S1+L1-1,S2+L2-1),L1*bb,MPI_DOUBLE_PRECISION,rank+d1,b1Tag,MPI_COMM_WORLD,nstatus,errcode)
  !	 Exterior boundary condition   
  If(Id2 .EQ. d2-1)call MPI_SEND(phi1(S1:S1+L1-1,S2+L2-1-bb),L1*bb,MPI_DOUBLE_PRECISION,rank-(d1)*(d2-1),f1Tag,MPI_COMM_WORLD,errcode)
  If(Id2 .EQ. 0)call MPI_RECV(phi1(S1:S1+L1-1,S2),L1*bb,MPI_DOUBLE_PRECISION,rank+(d1)*(d2-1),f1Tag,MPI_COMM_WORLD,nstatus,errcode)
  If(Id2 .EQ. 0)call MPI_SEND(phi1(S1:S1+L1-1,S2+bb),L1*bb,MPI_DOUBLE_PRECISION,rank+(d1)*(d2-1),b1Tag,MPI_COMM_WORLD,errcode)
  If(Id2 .EQ. d2-1)call MPI_RECV(phi1(S1:S1+L1-1,S2+L2-1),L1*bb,MPI_DOUBLE_PRECISION,rank-(d1)*(d2-1),b1Tag,MPI_COMM_WORLD,nstatus,errcode)
   
END IF

END SUBROUTINE 
!*********************************************************************
!********************************************************************* 	
subroutine read_input(rank,Is1,Ie1,Is2,Ie2,Conc,phi,iter)
use simulation
implicit none
	real(kind=DBL), DIMENSION(Is1-1:Ie1+1,Is2-1:Ie2+1) :: Conc,phi
	real(kind=8) ::max_c,min_c,max_phi,min_phi
	INTEGER :: iter,i,Is1,Ie1,Is2,Ie2,rank
	CHARACTER(LEN=100) :: filename
	CHARACTER(LEN=10) :: iteration
	CHARACTER(LEN=4) :: format_string
	CHARACTER(LEN=2) :: rank_no	

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
	
	if (d1*d2 < 10) then
		format_string="(i1)"	
	else 
		format_string="(i2)"	
	endif	
	
	write(rank_no,format_string)rank

	DO i=1,2
  		if(rank_no(i:i)=='')rank_no(i:i)='0'
	END DO	


	filename=''//trim(outdir)//''//trim(s)//'_phi_t'//trim(iteration)//'_rank'//trim(rank_no)//'_'//trim(dates)//'.dat'
	open(1,file=filename,form='unformatted',STATUS='old')!,ACCESS="STREAM")
	read(1) phi(Is1:Ie1,Is2:Ie2)
	close(1)	


	filename=''//trim(outdir)//''//trim(s)//'_Conc_t'//trim(iteration)//'_rank'//trim(rank_no)//'_'//trim(dates)//'.dat'
	open(2,file=filename,form='unformatted',STATUS='old')
	read(2) Conc(Is1:Ie1,Is2:Ie2)
	close(2)
	
	if (rank .eq. 0) then	
	write(*,*) "======================================================="
	write(*,*) "Read Input at iter=",trim(iteration)," for rank=",trim(rank_no) 
	write(*,*) "Maximum Value of Phi=", MAXVAL(phi) 
	write(*,*) "Minimum Value of Phi=", MINVAL(phi) 	
 	write(*,*) "Maximum Value of Conc=", MAXVAL(Conc) 
	write(*,*) "Minimum Value of Conc=", MINVAL(Conc) 
	write(*,*) "======================================================="
	endif
	
end subroutine 
!*********************************************************************
!*********************************************************************
subroutine write_output(rank,Is1,Ie1,Is2,Ie2,phi,Conc,iter)
use simulation
implicit none
	real(kind=8), DIMENSION(Is1-1:Ie1+1,Is2-1:Ie2+1) ::phi,Conc
	INTEGER :: iter,Is1,Ie1,Is2,Ie2,rank,i
	CHARACTER(LEN=100) :: filename
	CHARACTER(LEN=10) :: iteration
	CHARACTER(LEN=4) :: format_string	
	CHARACTER(LEN=2) :: rank_no	

	
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
	
	if (d1*d2 < 10) then
		format_string="(i1)"	
	else 
		format_string="(i2)"	
	endif	
	write(rank_no,format_string)rank

	DO i=1,2
  		if(rank_no(i:i)=='')rank_no(i:i)='0'
	END DO	

	if (rank .eq. 0) then
	write(*,*) "======================================================="
	write(*,*) "Write Output at iter=",trim(iteration)," for rank=",trim(rank_no) 
	write(*,*) "Maximum Value of Phi=", MAXVAL(phi) 
	write(*,*) "Minimum Value of Phi=", MINVAL(phi) 	
 	write(*,*) "Maximum Value of Conc=", MAXVAL(Conc) 
	write(*,*) "Minimum Value of Conc=", MINVAL(Conc) 
	write(*,*) "======================================================="
	endif
	
	filename=''//trim(outdir)//''//trim(s)//'_phi_t'//trim(iteration)//'_rank'//trim(rank_no)//'_'//trim(dates)//'.dat'
	open(1,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(1) phi(Is1:Ie1,Is2:Ie2)
	close(1)	

	filename=''//trim(outdir)//''//trim(s)//'_Conc_t'//trim(iteration)//'_rank'//trim(rank_no)//'_'//trim(dates)//'.dat'
	open(2,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(2) Conc(Is1:Ie1,Is2:Ie2)
	close(2)
	
end subroutine
!*********************************************************************
!********************************************************************* 	
subroutine initial_conds(rank,Is1,Ie1,Is2,Ie2,phi,Conc)
use simulation
implicit none

	real(kind=DBL), DIMENSION(1:nx,1:ny) :: phi_initial
	real(kind=DBL), DIMENSION(Is1-1:Ie1+1,Is2-1:Ie2+1) :: Conc,phi
	real(kind=8) ::max_c,min_c,max_phi,min_phi
	INTEGER :: iter,i,Is1,Ie1,Is2,Ie2,rank
	CHARACTER(LEN=100) :: filename
	
	filename=''//trim(outdir)//''//trim(s)//'_phi_t0_Matlab.dat'
	open(1,file=filename,form='unformatted',STATUS='old',ACCESS="STREAM")
	read(1) phi_initial(1:nx,1:ny)
	close(1)	
	
	phi(Is1:Ie1,Is2:Ie2)=phi_initial(Is1:Ie1,Is2:Ie2)

! 	Initialization of the concentration value
  	Conc(:,:)=0.2d0	


	write(*,*) "Read Initial Condition for rank=",rank
! 	write(*,*) "Maximum Value of Phi=", MAXVAL(phi) 
! 	write(*,*) "Minimum Value of Phi=", MINVAL(phi) 	
!  	write(*,*) "Maximum Value of Conc=", MAXVAL(Conc) 
! 	write(*,*) "Minimum Value of Conc=", MINVAL(Conc) 
		 	
end subroutine