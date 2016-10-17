function main
    clear all;
    clc;
   
    %no. of grid points
    nx=201;
    %grid spacing
    dx=1.0;
    %Diffusion coefficient for the concentration
    D=1.0;
    %Mobility coefficient for the structure parameter
    M=1.0;
    %Timestep, von-Neumann condition for diffusion eqn dt<=(dx^2)/(2D)
    maxdt=(dx^2)/(2.0*D);
    dt=0.1*maxdt;

    %Constants for parabolic free energies of the two phases
    %f(c)=A0+A1*(C-Cm)^2
    %For matrix phase (alpha)
    A0Al=0.0;
    A1Al=1;
    CmAl=0.2; % the center of the parabola
    
    %For precipiate phase (beta)
    A0Bt=0.0;
    A1Bt=4;
    CmBt=.8; % the center of the parabola

    %well-height parameter
    W=10.0;
    %gradient energy coefficient
    eps2=5.0;
    %interfacial thickness 
    delta=16.0*sqrt(2.0*eps2/W);

    %Average alloy composition
    CAlloy=0.5  ;     

    %X-coordinate array
    x_coord=linspace(-100,100,nx);

    %Initial Order-Parameter (one precipitate in the middle)
    %Precipitate phase = 1, matrix phase =0
    phi=0.5*(1+tanh(2.0*(x_coord)/delta));
   
    %Initial composition
    Conc=CAlloy*ones([1,nx]);
    
    iter=0;
    make_plots(x_coord,phi,Conc,iter)
    filename='160908_B/profiles_iter%d_160908.pdf'
    sprintf(filename,iter)
    save2pdf(sprintf(filename,iter))
    
%    free_eg_curves(A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl) 
%    free_eg_surface(W,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl)

    it_part1=2000000;
%     for iter=1:it_part1
%         Conc=diff_iter(phi,Conc,nx,D,dt,dx,W,eps2,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl);
%         if mod(iter,500000) == 0
%             make_plots(x_coord,phi,Conc,iter)
%             filename='160908_B/profiles_iter%d_160908.pdf'
%             sprintf(filename,iter)
%             save2pdf(sprintf(filename,iter))
%         end
%     end
%     save('160908_B/Variables_Part1.mat')
%     load('160908_B/Variables_Part1.mat')
     it_part2=5000000;
%     for iter=it_part1+1:it_part2
%         Conc_old=Conc;
%         phi_old=phi;
%         Conc=diff_iter(phi_old,Conc_old,nx,D,dt,dx,W,eps2,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl);
%         phi=AC_iter(phi_old,Conc_old,nx,dt,dx,M,W,eps2,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl);
%         
%         if mod(iter,500000) == 0
%             make_plots(x_coord,phi,Conc,iter)
%             filename='160908_B/profiles_iter%d_160908.pdf'
%             save2pdf(sprintf(filename,iter))
%             pause(.1)
%         end
%     end
%     save('160908_B/Variables_Final.mat')
    load('160908_B/Variables_Final.mat')

    delta=4.0*sqrt(2.0*eps2/W);
    phi1=0.5*(1+tanh(2.0*(x_coord)/delta));
 
    close all          
    plot(x_coord,phi,'-b','linewidth',3);
    hold on;
    plot(x_coord,phi1,'*r','linewidth',3);

    axis([-100 100 0 1]);
    set(gca,'xtick',[-100,-50,0,50,100],'fontsize',25,'linewidth',2.5,'fontweight','bold')
    ylabel({'Value of $\phi$ and $C$'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    xlabel({'X-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    legend({'Numerical','Analytical'},'fontsize',30,'interpreter','latex') 
    title(sprintf('iter=%d',iter))
    save2pdf('phi_profiles_num_anal.pdf')
end
function ans=make_plots(x_coord,phi,Conc,iter)
    % Plot order-parameter and concentration field
    close all          
    plot(x_coord,phi,'-b','linewidth',3);
    hold on;
    plot(x_coord,Conc,'-r','linewidth',3);

    axis([-100 100 0 1]);
    set(gca,'xtick',[-100,-50,0,50,100],'fontsize',25,'linewidth',2.5,'fontweight','bold')
    ylabel({'Value of $\phi$ and $C$'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    xlabel({'X-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    legend({'$\phi$','C'},'fontsize',30,'interpreter','latex') 
    title(sprintf('iter=%d',iter))
    %        frames(frame_no) = getframe(gcf);
    %     movie2avi(frames,'Growth_Part2.avi','fps',10);     
end
function ans=free_eg_curves(A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl)
    Conc=linspace(0,1,100); 
    close all
    GAl=A0Al+A1Al*(Conc-CmAl).^2.0;
    GBt=A0Bt+A1Bt*(Conc-CmBt).^2.0;   
    plot(Conc,GAl,'-b','linewidth',3);
    hold on
    plot(Conc,GBt,'-r','linewidth',3);    
    set(gca,'fontsize',20,'linewidth',2.5,'fontweight','bold')    
    xlabel({'$c$'},'fontsize',25,'FontWeight','Bold','interpreter','latex')        
    ylabel({'$f(c,\phi)$'},'fontsize',25,'FontWeight','Bold','interpreter','latex') 
    legend({'$f^\alpha$','$f^\beta$'},'fontsize',30,'interpreter','latex')
    save2pdf('160908/free_energy_curves_160908.pdf')
end
function ans=free_eg_surface(W,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl)
    close all
    phi1=linspace(0,1,100);
    [phi1,Conc1]=meshgrid(phi1,phi1);
    GAl=GAl(phi1,Conc1,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl);
    GBt=GBt(phi1,Conc1,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl);
    g=g(phi1,W);
    Hfunc=Hfunc(phi1);
    total_free_eg=Hfunc.*GBt+(1-Hfunc).*GAl+g;
    wh_over=find(total_free_eg > 1);
    %total_free_eg(wh_over)=1.1;
    colormap(hsv);
    surf(Conc1,phi1,total_free_eg);
    axis([0 1 0 1 0 1])
    set(gca,'xtick',[0,0.2,0.5,1.0],'fontsize',20,'linewidth',2.5,'fontweight','bold')
    xlabel({'$c$'},'fontsize',25,'FontWeight','Bold','interpreter','latex')        
    ylabel({'$\phi$'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    zlabel({'$f(c,\phi)$'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    close all
    plot(phi1(20,:),total_free_eg(20,:),'-b','linewidth',3)
    set(gca,'fontsize',20,'linewidth',2.5,'fontweight','bold')    
    xlabel({'$\phi$'},'fontsize',25,'FontWeight','Bold','interpreter','latex')        
    ylabel({'$f(c,\phi)$'},'fontsize',25,'FontWeight','Bold','interpreter','latex') 
    legend({'$f(c=0.2,\phi)$'},'fontsize',30,'interpreter','latex','location','northwest')
    save2pdf('free_energy_along_C0_160908.pdf')
end
function ans=Hfunc(phi)
    % monotonously changing function h(phi) 
    ans=(phi.^2.0).*(3.0-2.0*phi);
end
function ans=dHfunc_dphi(phi)
    % monotonously changing function h(phi) 
    ans=6.0*phi-6.0*phi.^2.0;
end
function ans=ConcAl(phi,Conc,A1Al,CmAl,A1Bt,CmBt)
    % Auxillary alpha phase composition
    num=A1Bt*(Conc-CmBt*Hfunc(phi))+A1Al*CmAl*Hfunc(phi);
    den=A1Al*Hfunc(phi)+A1Bt*(1.0-Hfunc(phi));
    ans=num./den;
end
function ans=ConcBt(phi,Conc,A1Al,CmAl,A1Bt,CmBt)
    % Auxillary beta phase composition
    num=A1Al*(Conc-CmAl*(1.0-Hfunc(phi)))+A1Bt*CmBt*(1.0-Hfunc(phi));
    den=A1Al*Hfunc(phi)+A1Bt*(1-Hfunc(phi));
    ans=num./den;
end
function ans=GAl(phi,Conc,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl)
    % Free energy curve of alpha phase
    ConcAl=ConcAl(phi,Conc,A1Al,CmAl,A1Bt,CmBt);
    ans=A1Al*(ConcAl-CmAl).^2.0+A0Al;    
end
function ans=GBt(phi,Conc,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl)
    % Free energy curve of beta phase
    ConcBt=ConcBt(phi,Conc,A1Al,CmAl,A1Bt,CmBt);
    ans=A1Bt*(ConcBt-CmBt).^2.0+A0Bt;    
end
function ans=dG_dCAl(phi,Conc,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl)
    % First derivative of the free energy w.r.t. alpha composition
    ConcAl=ConcAl(phi,Conc,A1Al,CmAl,A1Bt,CmBt);
    ans=2.0*A1Al*(ConcAl-CmAl);    
end
function ans=ddG_dC2Al(A1Al)
    % Second derivative of the free energy w.r.t. alpha composition
    ans=2.0*A1Al;    
end
function ans=ddG_dC2Bt(A1Bt)
    % Second derivative of the free energy w.r.t. beta composition
    ans=2.0*A1Bt;    
end
function ans=ddG_dC2(phi,A1Al,A1Bt)
    % Second derivative of the free energy w.r.t. overall composition
    num=(ddG_dC2Bt(A1Bt)*ddG_dC2Al(A1Al));
    dem=(1-Hfunc(phi))*ddG_dC2Bt(A1Bt)+Hfunc(phi)*ddG_dC2Al(A1Al);
    ans=num./dem;
end
function ans=lap1D(input,dx,nx)
    % 1D Laplacian
    ans=(circshift(input',-1)'+circshift(input',1)'-2.0*input)/dx^2.0;
    ans(1)=(input(2)+input(1)-2.0*input(1))/dx^2.0;
    ans(nx)=(input(nx)+input(nx-1)-2.0*input(nx))/dx^2.0;
    
end
function ans=g(phi,W)
    % g(phi)
    ans=W*phi.^2.0.*(1-phi).^2.0/4.0;
end
function ans=dgdphi(phi,W)
    % dg/dphi
   ans= W*phi.*(1.0-phi).*(1.0-2.0*phi)/2.0;
end
function ans=chem_pot(phi,Conc,nx,dx,W,eps2,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl)
    % chemical potential term in the Allen-Cahn equation
    GAl=GAl(phi,Conc,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl);
    GBt=GBt(phi,Conc,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl);
    ConcAl=ConcAl(phi,Conc,A1Al,CmAl,A1Bt,CmBt);
    ConcBt=ConcBt(phi,Conc,A1Al,CmAl,A1Bt,CmBt);
    dG_dCAl=dG_dCAl(phi,Conc,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl);
    ans=dgdphi(phi,W)-eps2*lap1D(phi,dx,nx)...
    +dHfunc_dphi(phi).*(GBt-GAl-(ConcBt-ConcAl).*dG_dCAl);
end
function phi=AC_iter(phi,Conc,nx,dt,dx,M,W,eps2,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl)
    % Explicit time iteration of the Allen-Cahn equation    
    phi=phi-M*chem_pot(phi,Conc,nx,dx,W,eps2,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl)*dt;
end
function Conc=diff_iter(phi,Conc,nx,D,dt,dx,W,eps2,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl)
    % Explicit time iteration of the diffusion equation
    dG_dCAl=dG_dCAl(phi,Conc,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl);
    ddG_dC2=ddG_dC2(phi,A1Al,A1Bt);    
    %central differencing and effective mobility at i+1/2
    central_diff1=(circshift(dG_dCAl',-1)'-dG_dCAl)/dx;
    effective_M1=(D./circshift(ddG_dC2',-1)'+D./ddG_dC2)/2.0;
    %central differencing and effective mobility at i-1/2    
    central_diff2=(dG_dCAl-circshift(dG_dCAl',1)')/dx;
    effective_M2=(D./circshift(ddG_dC2',1)'+D./ddG_dC2)/2.0;
    % no-flux boundary condition imposed at the ends of the domain
    central_diff1(nx)=0;
    central_diff2(1)=0;
    Conc=Conc+dt*(central_diff1.*effective_M1-central_diff2.*effective_M2)/dx;
end
