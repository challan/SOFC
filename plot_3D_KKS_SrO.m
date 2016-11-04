function main
clear all
clc
sizeArr=[50,50,30];
nx=sizeArr(1);
ny=sizeArr(2);
nz=sizeArr(3);

iter=500000;    
    % Read the compressed microstructral data
    fname = ['data/SrO_on_LSCF/161102_A/SrO_on_LSCF_Conc_t%d_161102_A.dat'];
    fid = fopen(sprintf(fname,iter));
    skip = fread(fid,1,'int32');
    a = fread(fid,nx*ny*nz,'double');
    fclose(fid); 
    Conc = reshape(a, [nx ny,nz]);
    clear a;
    Domain=Conc;
    Domain(1:nx/2,:,:)=0;
    Domain(nx/2+1:nx,:,:)=1; 
    
    [x,y,z]=meshgrid(1:nx,1:ny,1:nz);   
    close all,
    hfig=figure(1);
    set(hfig,'Position', [100, 100, 1000, 1000]);    

%      sl=slice(Conc,1,25,1)
%      shading interp

    gbLevel = 0.5;

    p1 = patch(isosurface(x,y,z,Domain,gbLevel,'above'));
    p2 = patch(isocaps(x,y,z,Domain,gbLevel,'above'));  
    
    isocolors(Conc,p1),
    isocolors(Conc,p2),

    p1.FaceColor = 'interp'; p1.EdgeColor = 'none'; p2.FaceColor = 'interp'; p2.EdgeColor = 'none';

    set(gca,'Projection','perspective'); view(3); daspect([1,1,1]);
    axis on; lightangle(45,60); lighting phong; colorbar;
    box on; set(gca, 'color', [1 1 1]); set(gcf, 'color', [1 1 1]);    

    c = colorbar;
    ylabel(c, '$Conc$','fontsize',25,'FontWeight','Bold','interpreter','latex') 
    caxis([0.3, 0.5])
    
    axis([1 nx 1 nx 1 nz]);      
    set(gca,'fontsize',25,'linewidth',2.5,'fontweight','bold')    
    xlabel({'X-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    ylabel({'Y-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex') 
    zlabel({'Z-Coordinate'},'fontsize',30,'FontWeight','Bold','interpreter','latex') 
    title(sprintf('iter=%d',iter),'fontsize',30)       
    filename='data/SrO_on_LSCF/161102_A/jpg/Concentration_Field_iter%d_3DView.jpg'
    saveas(gcf,sprintf(filename,iter))     
    pause(2)
    stop
    
    fname = ['data/SrO_on_LSCF/161102_A/SrO_on_LSCF_SurfaceConc_t%d_161102_A.dat'];
    fid = fopen(sprintf(fname,iter));
    skip = fread(fid,1,'int32');
    a = fread(fid,nx*ny,'double');
    fclose(fid); 
    Conc = reshape(a, [nx ny]); 
    clear a;    
    
    x=linspace(1,nx);
    [x_coord,y_coord]=meshgrid(x,x);    
    close all,
    hfig=figure(1);
    set(hfig,'Position', [100, 100, 1000, 1000]);     
    surface(Conc);
    shading interp
    caxis([-0.1, 1])    
    axis([1 nx 1 nx -0.1 1]);   
    c = colorbar;
    ylabel(c, '$Conc$','fontsize',25,'FontWeight','Bold','interpreter','latex')    
    set(gca,'xtick',[1,nx/4*1,nx/4*2,nx/4*3,nx],'ytick',[1,nx/4*1,nx/4*2,nx/4*3,nx],'fontsize',25,'linewidth',2.5,'fontweight','bold')    
    xlabel({'X-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    ylabel({'Y-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex') 
    zlabel({'$Conc$'},'fontsize',30,'FontWeight','Bold','interpreter','latex') 
    title(sprintf('iter=%d',iter),'fontsize',30)    
    az = 45;
    el = 20;
    view(az, el); 
     filename='data/SrO_on_LSCF/161102_A/jpg/Concentration_Field_iter%d_2DTopView.jpg'
     saveas(gcf,sprintf(filename,iter))     

    wh_precipitate=find(Conc <0.25);
    area_frac=size(wh_precipitate)/(nx*ny);
    fprintf('\nThe %% area fraction of precipitate at iter=%d is %f%%\n\n',iter, area_frac(1)*100);

end