function main
clear all
clc
sizeArr=[50,50,50];
nx=sizeArr(1);
ny=sizeArr(2);
nz=sizeArr(3);


% time=[70000,80000,90000,100000,120000,140000];
% perc_area=[1.4,2.9,5.5,9.2,20.7,38.0];
% close all,
%     plot(time*0.01,sqrt(perc_area/(pi)),'-*r','linewidth',3);    
%     set(gca,'fontsize',20,'linewidth',2.5,'fontweight','bold')    
%     xlabel({'$t$'},'fontsize',25,'FontWeight','Bold','interpreter','latex')        
%     ylabel({'Radius'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
%     title({'Radius vs Time'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
%     save2pdf('data/SrO_on_LSCF/161122_A/jpg/Radius_vs_time_plot.pdf')
%     stop


iter=50000;    
    % Read the compressed microstructral data
    fname = ['data/SrO_on_LSCF/161122_A/SrO_on_LSCF_C_Sr_t%d_161122_A.dat'];
    fid = fopen(sprintf(fname,iter));
    skip = fread(fid,1,'int32');
    a = fread(fid,nx*ny*nz,'double');
    fclose(fid); 
    Conc = reshape(a, [nx ny nz]);
    clear a;

    fname = ['data/SrO_on_LSCF/161122_A/SrO_on_LSCF_C_La_t%d_161122_A.dat'];
    fid = fopen(sprintf(fname,iter));
    skip = fread(fid,1,'int32');
    a = fread(fid,nx*ny*nz,'double');
    fclose(fid); 
    phi = reshape(a, [nx ny nz]); 
    clear a;    
%     wh_precipitate=find(phi(:,:,nz) >0.5);
%     area_frac=size(wh_precipitate)/(nx*ny);
%     fprintf('\nThe %% area fraction of precipitate at iter=%d is %f%%\n\n',iter, area_frac(1)*100);        
%     fname = ['data/SrO_on_LSCF/161122_A/SrO_on_LSCF_flux_x_t%d_161122_A.dat'];
%     fid = fopen(sprintf(fname,iter));
%     skip = fread(fid,1,'int32');
%     a = fread(fid,nx*ny*nz,'double');
%     fclose(fid); 
%     flux_x = reshape(a, [nx ny nz]); 
%     clear a;    
%     fname = ['data/SrO_on_LSCF/161122_A/SrO_on_LSCF_flux_y_t%d_161122_A.dat'];
%     fid = fopen(sprintf(fname,iter));
%     skip = fread(fid,1,'int32');
%     a = fread(fid,nx*ny*nz,'double');
%     fclose(fid); 
%     flux_y = reshape(a, [nx ny nz]); 
%     clear a;  
%     fname = ['data/SrO_on_LSCF/161122_A/SrO_on_LSCF_flux_z_t%d_161122_A.dat'];
%     fid = fopen(sprintf(fname,iter));
%     skip = fread(fid,1,'int32');
%     a = fread(fid,nx*ny*nz,'double');
%     fclose(fid); 
%     flux_z = reshape(a, [nx ny nz]); 
%     clear a;
%     fname = ['data/SrO_on_LSCF/161122_A/SrO_on_LSCF_dG_dCSu_t%d_161122_A.dat'];
%     fid = fopen(sprintf(fname,iter));
%     skip = fread(fid,1,'int32');
%     a = fread(fid,nx*ny*nz,'double');
%     fclose(fid); 
%     mu = reshape(a, [nx ny nz]); 
%     clear a; 
%     
%     for i=1:2:nx
%         flux_x(i,:,:)=0;
%         flux_y(i,:,:)=0;
%         flux_z(i,:,:)=0;
%     end
%     for i=1:2:ny
%         flux_x(:,i,:)=0;
%         flux_y(:,i,:)=0;
%         flux_z(:,i,:)=0;
%     end
%     for i=1:2:nz
%         flux_x(:,:,i)=0;
%         flux_y(:,:,i)=0;
%         flux_z(:,:,i)=0;
%     end
%% Isosurface with Concentration   
    Domain=Conc;
    Domain(1:nx/2,:,:)=0;
    Domain(nx/2+1:nx,:,:)=1;
    %     Domain(:,:,:)=1;
    [x,y,z]=meshgrid(1:nx,1:ny,1:nz);   
    close all,
    hfig=figure(1);
    set(hfig,'Position', [100, 100, 1000, 1000]);    
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
    caxis([0.0, 1.0])
    
    axis([1 nx 1 nx 1 nz]);      
    set(gca,'fontsize',25,'linewidth',2.5,'fontweight','bold')    
    xlabel({'X-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    ylabel({'Y-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex') 
    zlabel({'Z-Coordinate'},'fontsize',30,'FontWeight','Bold','interpreter','latex') 
    title(sprintf('%diter',iter),'fontsize',30)        
    filename='data/SrO_on_LSCF/161122_A/jpg/Sr_Conc_iter%d_3DView.jpg'
    saveas(gcf,sprintf(filename,iter))     
    pause(2)
     stop
%     
% %% Isosurface with Structural Parameter 
    [x,y,z]=meshgrid(1:nx,1:ny,1:nz);   
    close all,
    hfig=figure(1);
    set(hfig,'Position', [100, 100, 1000, 1000]);    
    gbLevel = 0.5;

    p1 = patch(isosurface(x,y,z,Domain,gbLevel,'above'));
    p2 = patch(isocaps(x,y,z,Domain,gbLevel,'above'));  
    
    isocolors(phi,p1),
    isocolors(phi,p2),

    p1.FaceColor = 'interp'; p1.EdgeColor = 'none'; p2.FaceColor = 'interp'; p2.EdgeColor = 'none';

    set(gca,'Projection','perspective'); view(3); daspect([1,1,1]);
    axis on; lightangle(45,60); lighting phong; colorbar;
    box on; set(gca, 'color', [1 1 1]); set(gcf, 'color', [1 1 1]);    

    c = colorbar;
    ylabel(c, '$\phi$','fontsize',25,'FontWeight','Bold','interpreter','latex') 
    caxis([0, 0.16])
    
    axis([1 nx 1 nx 1 nz]);      
    set(gca,'fontsize',25,'linewidth',2.5,'fontweight','bold')    
    xlabel({'X-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    ylabel({'Y-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex') 
    zlabel({'Z-Coordinate'},'fontsize',30,'FontWeight','Bold','interpreter','latex') 
%     txt1 = '%% Area Fraction of SrO = %5.1f';
%     title({sprintf('time=%ds',iter*0.1);sprintf(txt1,area_frac(1)*100)},'fontsize',30)        
%     filename='data/SrO_on_LSCF/161122_A/jpg/OrderParameter_iter%d_3DView.jpg'
%     saveas(gcf,sprintf(filename,iter))     
    pause(2)    
Stop    
 

%% Plot Flux Arrows (Top view with conc. as background)    

%     Domain=Conc;
%     Domain(:,:,:)=1;
%     close all,
%     hfig=figure(1);
%     set(hfig,'Position', [100, 100, 700, 700]);
%     
%     [x,y,z]=meshgrid(1:ny,1:nx,1:nz); 
%     q=quiver3(x,y,z,flux_y,flux_x,flux_z,3);
%     c = q.Color;
%     q.Color = 'red';
%     d=q.LineWidth;
%     q.LineWidth=1;
%      e=q.MaxHeadSize;
%      q.MaxHeadSize=.5;
%     hold on
%     
%     gbLevel = 0.5;
%     [x,y,z]=meshgrid(1:ny,1:nx,1:nz); 
%     p1 = patch(isosurface(x,y,z,Domain,gbLevel,'above'));
%     p2 = patch(isocaps(x,y,z,Domain,gbLevel,'above'));  
%     
%     isocolors(Conc,p1),
%     isocolors(Conc,p2),
% 
%     p1.FaceColor = 'interp'; p1.EdgeColor = 'none'; p2.FaceColor = 'interp'; p2.EdgeColor = 'none';
%    
%     set(gca,'Projection','perspective'); daspect([1,1,1]);
%     axis on; 
%     box on; set(gca, 'color', [1 1 1]); set(gcf, 'color', [1 1 1]);    
%   
%     az = 0;
%     el = 90;
%     view(az, el); 
%     
%     c = colorbar;
%     ylabel(c, '$C$','fontsize',25,'FontWeight','Bold','interpreter','latex') 
%     caxis([0, 0.5])    
%     
%     axis([1 nx 1 nx 1 nz+1]);      
%     set(gca,'fontsize',25,'ztick',[],'linewidth',2.5,'fontweight','bold')    
%     xlabel({'X-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
%     ylabel({'Y-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex') 
%     txt1 = '%% Area Fraction of SrO = %5.1f';
%     title({sprintf('time=%ds',iter*0.1);sprintf(txt1,area_frac(1)*100)},'fontsize',30)        
%     filename='data/SrO_on_LSCF/161122_A/jpg/Flux_iter%d_3DTopView_conc.jpg'
%     saveas(gcf,sprintf(filename,iter))     
%     pause(0.1)  
    
%% Plot Flux Arrows (Top view with chem. pot. as background)    

    Domain=Conc;
    Domain(:,:,:)=0;
    Domain(:,:,1:nz)=1;
    close all,
    hfig=figure(1);
    set(hfig,'Position', [100, 100, 1000, 1000]);
    
    [x,y,z]=meshgrid(1:ny,1:nx,1:nz);
     
    q=quiver3(x,y,z,flux_y,flux_x,flux_z,3);
    c = q.Color;
    q.Color = 'red';
    d=q.LineWidth;
    q.LineWidth=1;
     e=q.MaxHeadSize;
     q.MaxHeadSize=.5;
    hold on
    
    gbLevel = 0.5;
    [x,y,z]=meshgrid(1:ny,1:nx,1:nz); 
    p1 = patch(isosurface(x,y,z,Domain,gbLevel,'above'));
    p2 = patch(isocaps(x,y,z,Domain,gbLevel,'above'));  
    
    isocolors(mu,p1),
    isocolors(mu,p2),

    p1.FaceColor = 'interp'; p1.EdgeColor = 'none'; p2.FaceColor = 'interp'; p2.EdgeColor = 'none';
   
    set(gca,'Projection','perspective'); daspect([1,1,1]);
    axis on; 
    box on; set(gca, 'color', [1 1 1]); set(gcf, 'color', [1 1 1]);    
  
    az = 0;
    el = 50;
    view(az, el); 
    
%     c = colorbar;
%     ylabel(c, '$\mu$','fontsize',25,'FontWeight','Bold','interpreter','latex') 
%     caxis([-0.01, 0.04])    
    
    axis([1 nx 1 nx 1 nz+1]);      
    set(gca,'fontsize',25,'ztick',[],'linewidth',2.5,'fontweight','bold')    
    xlabel({'X-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    ylabel({'Y-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex') 
    txt1 = '%% Area Fraction of SrO = %5.1f';
    title({sprintf('time=%ds',iter*0.1);sprintf(txt1,area_frac(1)*100)},'fontsize',30)        
    filename='data/SrO_on_LSCF/161122_A/jpg/Flux_iter%d_3DTopView_chempot.jpg'
    saveas(gcf,sprintf(filename,iter))     
    pause(0.1)
    
 %% Plot Flux Arrows (Crosssection view)    
    ycut=24;
    Domain=Conc;
    Domain(1:ycut,:,:)=0;
    Domain(ycut+1:ny,:,:)=1;
    close all,
    hfig=figure(1);
    set(hfig,'Position', [100, 100, 1000, 1000]);
  
    [x,y,z]=meshgrid(1:nx,ycut:ny,1:nz);
    flux_x(:,:,nz)=0;
    flux_y(:,:,nz)=0;
    flux_z(:,:,nz)=0;
    flux_x2=flux_x(ycut:ny,1:nx,1:nz);
    flux_y2=flux_y(ycut:ny,1:nx,1:nz);
    flux_z2=flux_z(ycut:ny,1:nx,1:nz);
    flux_x2(2:ny-ycut+1,1:nx,1:nz)=0;
    flux_y2(2:ny-ycut+1,1:nx,1:nz)=0;
    flux_z2(2:ny-ycut+1,1:nx,1:nz)=0;
    
    q=quiver3(x,y,z,flux_y2,flux_x2,flux_z2,3);
    c = q.Color;
    q.Color = 'red';
    d=q.LineWidth;
    q.LineWidth=1;
    e=q.MaxHeadSize;
    q.MaxHeadSize=.5;
    
    gbLevel = 0.5;
    [x,y,z]=meshgrid(1:ny,1:nx,1:nz); 
    p1 = patch(isosurface(x,y,z,Domain,gbLevel,'above'));
    p2 = patch(isocaps(x,y,z,Domain,gbLevel,'above'));  
    
    isocolors(mu,p1),
    isocolors(mu,p2),

    p1.FaceColor = 'interp'; p1.EdgeColor = 'none'; p2.FaceColor = 'interp'; p2.EdgeColor = 'none';
   
    set(gca,'Projection','perspective'); daspect([1,1,1]);
    axis on; lightangle(45,60); lighting phong; 
    box on; set(gca, 'color', [1 1 1]); set(gcf, 'color', [1 1 1]);    
    az =-40;
    el = 20;
    view(az, el);  
    
%      c = colorbar;
%      ylabel(c, '$C$','fontsize',25,'FontWeight','Bold','interpreter','latex') 
%      caxis([0, 0.5])    
    
    axis([1 nx ycut-5 ycut+5 1 nz]);      
    set(gca,'fontsize',25,'ytick',[],'linewidth',2.5,'fontweight','bold')    
    xlabel({'X-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    zlabel({'Z-Coordinate'},'fontsize',30,'FontWeight','Bold','interpreter','latex') 
    txt1 = '%% Area Fraction of SrO = %5.1f';
    title({sprintf('time=%ds',iter*0.1);sprintf(txt1,area_frac(1)*100)},'fontsize',30)        
    filename='data/SrO_on_LSCF/161122_A/jpg/Flux_iter%d_3DCrossSectionView_chempot.jpg'
    saveas(gcf,sprintf(filename,iter))     
    pause(0.1)     


end