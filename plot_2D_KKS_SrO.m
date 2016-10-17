function main
clear all
clc
sizeArr=[2000,2000];
nx=sizeArr(1);
ny=sizeArr(2);

    x=linspace(1,2000,nx);
    [x_coord,y_coord]=meshgrid(x,x);

iter=500;    
    % Read the compressed microstructral data
    fname = ['data/SrO_on_LSCF_Conc_t500_161014.dat'];
    fid = fopen(sprintf(fname,iter));
    skip = fread(fid,1,'int32');
    a = fread(fid,nx*ny,'double');
    fclose(fid);
    Conc = reshape(a, [nx ny]);
    clear a;
    max(Conc(:));
    min(Conc(:));
    
    [row,col]=find(Conc < -0.5)
    
    x=linspace(1,2000,nx);
    [x_coord,y_coord]=meshgrid(x,x);    
    close all,
    hfig=figure(1);
    set(hfig,'Position', [100, 100, 1000, 1000]);     
    surface(x_coord,y_coord,Conc);
    shading interp
    caxis([-1, 1])
    axis([1 2000 1 2000 -1, 1]);   
    c = colorbar;
    ylabel(c, '$C$','fontsize',25,'FontWeight','Bold','interpreter','latex')    
    set(gca,'xtick',[1,500,1000,1500,2000],'ytick',[1,500,1000,1500,2000],'fontsize',25,'linewidth',2.5,'fontweight','bold')    
    xlabel({'X-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    ylabel({'Y-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex') 
    zlabel({'$C$'},'fontsize',30,'FontWeight','Bold','interpreter','latex') 
    title(sprintf('iter=%d',iter),'fontsize',30)    
    az = 0;
    el = 90;
    view(az, el); 
    stop      
%   \nabla \cdot M\nabla df/dc
%     fname = ['data/SrO_on_LSCF_phi_t%d_161011.dat'];
%     fid = fopen(sprintf(fname,iter));
%     skip = fread(fid,1,'int32');
%     a = fread(fid,nx*ny,'double');
%     fclose(fid);
%     phi = reshape(a, [nx ny]);
%     clear a;
    
    fname = ['data/SrO_on_LSCF_Divergence_t0_161011.dat'];
    fid = fopen(fname);
    skip = fread(fid,1,'int32');
    a = fread(fid,(nx+2)*(ny+2),'double');
    fclose(fid);
    dG_dCAl = reshape(a, [nx+2 ny+2]);
    clear a;        
    
    make_SurfacePlots(x_coord,y_coord,phi,Conc,iter)

end
function ans=make_SurfacePlots(x_coord,y_coord,phi,Conc,iter)
    % Plot order-parameter and concentration field
    close all,
    hfig=figure(1);
    set(hfig,'OuterPosition', [0, 0, 1200, 1200],'Position', [100, 100, 1000, 1000]);  
    
%     subplot(2,1,1)

    surface(x_coord,y_coord,phi);   
    shading interp
    c = colorbar;
    ylabel(c, '$\phi$','fontsize',25,'FontWeight','Bold','interpreter','latex')
    axis([0 2000 0 2000 0 1]);
    set(gca,'xtick',[0,500,1000,1500,2000],'ytick',[0,500,1000,1500,2000],'fontsize',25,'linewidth',2.5,'fontweight','bold')    
    xlabel({'X-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    ylabel({'Y-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex') 
    zlabel({'$\phi$'},'fontsize',30,'FontWeight','Bold','interpreter','latex') 
    title(sprintf('Order-parameter @ iter=%d',iter),'fontsize',30)
    az = 0;
    el = 90;
    view(az, el); 
    
    filename='data/jpg/phi_profiles_iter%d.jpg'
    saveas(gcf,sprintf(filename,iter))
    
    close all,
    hfig=figure(1);
    set(hfig,'OuterPosition', [0, 0, 1200, 1200],'Position', [100, 100, 1000, 1000]);  
    surface(x_coord,y_coord,Conc);
    shading interp
    caxis([0, 1])
    c = colorbar;
    ylabel(c, '$c$','fontsize',25,'FontWeight','Bold','interpreter','latex')
    axis([0 2000 0 2000 0 1]);
    set(gca,'xtick',[0,500,1000,1500,2000],'ytick',[0,500,1000,1500,2000],'fontsize',25,'linewidth',2.5,'fontweight','bold')
    xlabel({'X-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    ylabel({'Y-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex') 
    zlabel({'$C$'},'fontsize',30,'FontWeight','Bold','interpreter','latex') 
    title(sprintf('Concentration @ iter=%d',iter),'fontsize',30)
    az = 0;
    el = 90;
    view(az, el);
    
    filename='data/jpg/Conc_profiles_iter%d.jpg'
    saveas(gcf,sprintf(filename,iter))
    
    filename='data/jpg/Conc_profiles_iter%d.jpg'
    saveas(gcf,sprintf(filename,iter))

  
end