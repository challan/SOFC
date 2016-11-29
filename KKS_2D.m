function main
    clear all;
    clc;
   
    %no. of grid points
    nx=100;
    ny=100;
    %grid spacing
    dx=1.0;
    dy=1.0;
    %inter-diffusion mobility for the concentration
    D=1.0;
    %Mobility coefficient for the structure parameter
    M=1.0;
    %Timestep, von-Neumann condition for diffusion eqn dt<=(dx^2)/(2D)
    maxdt=(dx^2)/(2.0*D);
    dt=0.01;

    %Constants for parabolic free energies of the two phases
    %f(c)=A0+A1*(C-Cm)^2
    %For matrix phase (alpha)
    A0Al=.1;
    A1Al=1.;
    CmAl=0.5; % the center of the parabola
    
    %For precipiate phase (beta)
    A0Bt=0.0;
    A1Bt=1.0;
    CmBt=0.0; % the center of the parabola

    %well-height parameter
    W=4.0;
    %gradient energy coefficient
    eps2=2.0;
    %interfacial thickness 
    delta=2.0;

    %Average alloy composition
    CAlloy=0.48;       
   
    %X & Y-coordinate array
    [x_coord,y_coord]=meshgrid(1:nx,1:nx);

    %Initial Order-Parameter (one precipitate in the middle)
    %Precipitate phase = 1, matrix phase =0
    radius=10.0;
    xcenter=nx/2;
    ycenter=ny/2;
    circle=sqrt((x_coord-xcenter).^2+(y_coord-ycenter).^2);
    phi=0.5*(1+tanh((radius-circle)/delta));

    %Initial composition
    Conc=CAlloy*ones(nx,ny);
        
    iter=0;
    
%     free_eg_curves(A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl) 
%    free_eg_surface(W,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl)
 
    for iter=1:40000
        Conc=diff_iter(phi,Conc,D,dt,dx,dy,W,eps2,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl);       
        if mod(iter,5000) == 0
            iter
            sum(phi(:))
            make_SurfacePlots(nx,x_coord,y_coord,phi,Conc,iter)
            pause(0.1)
%             fname = ['data/SrO_on_LSCF/161019_B/Variables_t%d_161019_B.sav'];
%             save(sprintf(fname,iter))
        end
    end
%     stop
%     fname = ['data/SrO_on_LSCF/161019_B/Variables_t%d_161019_B.sav'];
%     save(sprintf(fname,iter)) 
    
    k=iter;
    for iter=k+1:60000
        Conc_old=Conc;
        phi_old=phi;
        Conc=diff_iter(phi_old,Conc_old,D,dt,dx,dy,W,eps2,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl);
        phi=AC_iter(phi_old,Conc_old,dt,dx,dy,M,W,eps2,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl);       
        if mod(iter,5000) == 0
            iter
            sum(phi(:))
            make_SurfacePlots(nx,x_coord,y_coord,phi,Conc,iter)
             pause(0.1)
%             fname = ['data/SrO_on_LSCF/161019_B/Variables_t%d_161019_B.sav'];
%             save(sprintf(fname,iter))
        end
    end

end
function ans=make_SurfacePlots(nx,x_coord,y_coord,phi,Conc,iter)
    % Plot order-parameter and concentration field
    close all,
    hfig=figure(1);
    set(hfig,'Position', [100, 100, 800, 800]);  
    
%     subplot(2,1,1)
% 
%     surface(x_coord,y_coord,phi);
%     shading interp;
%     az = 0;
%     el = 90;
%     view(az, el);     
%     axis([1 50 1 50 0 1]);
%     set(gca,'fontsize',25,'linewidth',2.5,'fontweight','bold')    
%     xlabel({'X-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
%     ylabel({'Y-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex') 
%     zlabel({'$\phi$'},'fontsize',30,'FontWeight','Bold','interpreter','latex') 
%     title(sprintf('iter=%d',iter),'fontsize',30)
%     
%     subplot(2,1,2)
    surface(x_coord,y_coord,Conc);
    shading interp;
    az = 45;
    el = 0;
    view(az, el);     
    axis([1 nx, 1 nx, -0.2 0.6]);
    set(gca,'fontsize',25,'linewidth',2.5,'fontweight','bold')    
    xlabel({'X-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    ylabel({'Y-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex') 
    zlabel({'$C$'},'fontsize',30,'FontWeight','Bold','interpreter','latex') 

%     filename='data/SrO_on_LSCF/161019_B/jpg/profiles_iter%d.jpg'
%     saveas(gcf,sprintf(filename,iter))
%     filename='161013_A/pdf/profiles_iter%d.pdf'
%     save2pdf(sprintf(filename,iter))
  
end
function ans=free_eg_curves(A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl)
    Conc=linspace(-0.5,1.5,200); 
    close all
    GAl=A0Al+A1Al*(Conc-CmAl).^2.0;
    GBt=A0Bt+A1Bt*(Conc-CmBt).^2.0;   
    plot(Conc,GAl,'-b','linewidth',3);
    hold on
    plot(Conc,GBt,'-r','linewidth',3); 
    axis([-0.5 1.5 0 4])
    set(gca,'fontsize',20,'linewidth',2.5,'fontweight','bold')    
    xlabel({'$c$'},'fontsize',25,'FontWeight','Bold','interpreter','latex')        
    ylabel({'$f(c,\phi)$'},'fontsize',25,'FontWeight','Bold','interpreter','latex') 
    legend({'$f^\alpha$','$f^\beta$'},'fontsize',30,'interpreter','latex','location','northwest')
    save2pdf('data/SrO_on_LSCF/161025_A/free_energy_curves_1601025.pdf')
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
    save2pdf('free_energy_along_C0_160907.pdf')
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
function ans=lap2D(input,dx,dy)
    % 1D Laplacian
    % circshift(input,-1)= input(i+1)
    % circshift(input,1)= input(i-1)
    lapx=(circshift(input,[0,-1])+circshift(input,[0,1])-2.0*input)/dx^2.0;
    lapy=(circshift(input,[-1,0])+circshift(input,[1,0])-2.0*input)/dy^2.0;
    ans=lapx+lapy;

end
function ans=g(phi,W)
    % g(phi)
    ans=W*phi.^2.0.*(1-phi).^2.0/4.0;
end
function ans=dgdphi(phi,W)
    % dg/dphi
   ans= W*phi.*(1.0-phi).*(1.0-2.0*phi)/2.0;
end
function ans=chem_pot(phi,Conc,dx,dy,W,eps2,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl)
    % chemical potential term in the Allen-Cahn equation
    GAl=GAl(phi,Conc,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl);
    GBt=GBt(phi,Conc,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl);
    ConcAl=ConcAl(phi,Conc,A1Al,CmAl,A1Bt,CmBt);
    ConcBt=ConcBt(phi,Conc,A1Al,CmAl,A1Bt,CmBt);
    dG_dCAl=dG_dCAl(phi,Conc,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl);
    ans=dgdphi(phi,W)-eps2*lap2D(phi,dx,dy)...
    +dHfunc_dphi(phi).*(GBt-GAl-(ConcBt-ConcAl).*dG_dCAl);    
end
function phi=AC_iter(phi,Conc,dt,dx,dy,M,W,eps2,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl)
    % Explicit time iteration of the Allen-Cahn equation    
    phi=phi-M*chem_pot(phi,Conc,dx,dy,W,eps2,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl)*dt;
end
function Conc=diff_iter(phi,Conc,D,dt,dx,dy,W,eps2,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl)
    % Explicit time iteration of the diffusion equation
    dG_dCAl=dG_dCAl(phi,Conc,A0Bt,A1Bt,CmBt,A0Al,A1Al,CmAl);
    ddG_dC2=ddG_dC2(phi,A1Al,A1Bt);    
    %central differencing and effective mobility at i+1/2
    central_diff1=(circshift(dG_dCAl,[0,-1])-dG_dCAl)/dx;
    effective_M1=(D./circshift(ddG_dC2,[0,-1])+D./ddG_dC2)/2.0;
    %central differencing and effective mobility at i-1/2    
    central_diff2=(dG_dCAl-circshift(dG_dCAl,[0,1]))/dx;
    effective_M2=(D./circshift(ddG_dC2,[0,1])+D./ddG_dC2)/2.0;
    x_comp=(central_diff1.*effective_M1-central_diff2.*effective_M2)/dx;    

    %central differencing and effective mobility at j+1/2
    central_diff3=(circshift(dG_dCAl,[-1,0])-dG_dCAl)/dx;
    effective_M3=(D./circshift(ddG_dC2,[-1,0])+D./ddG_dC2)/2.0;
    %central differencing and effective mobility at j-1/2    
    central_diff4=(dG_dCAl-circshift(dG_dCAl,[1,0]))/dx;
    effective_M4=(D./circshift(ddG_dC2,[1,0])+D./ddG_dC2)/2.0;
    y_comp=(central_diff3.*effective_M3-central_diff4.*effective_M4)/dy;    
    Conc=Conc+dt*(x_comp+y_comp);
end
