function main
clear all
clc
sizeArr=[2000,2000];
nx=sizeArr(1);
ny=sizeArr(2);

Conc=ones(nx,ny);


time=[1000000,2000000,3000000,4000000,5000000,6000000,7000000,8000000,9000000];
perc_area=[3.036300,5.368200,7.362625,9.056200,10.496800,11.718500,12.760925,13.657875,14.425700];
    plot(time*0.01,perc_area,'-*r','linewidth',3);    
    set(gca,'fontsize',20,'linewidth',2.5,'fontweight','bold')    
    xlabel({'$t$'},'fontsize',25,'FontWeight','Bold','interpreter','latex')        
    ylabel({'Percent Area of Precipitate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    title({'Percent Area vs Time'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    save2pdf('data/SrO_on_LSCF/161029_A/jpg/Percent_Area_vs_time_plot.pdf')
    stop

iter=9000000;    
    % Read the compressed microstructral data
    fname = ['data/SrO_on_LSCF/161029_A/SrO_on_LSCF_Conc_t%d_161029_A.dat'];
    fid = fopen(sprintf(fname,iter));
    skip = fread(fid,1,'int32');
    a = fread(fid,nx*ny,'double');
    fclose(fid);
    Conc = reshape(a, [nx ny]);
    clear a;
    min(Conc(:))
    wh_precipitate=find(Conc >0.5);
    area_frac=size(wh_precipitate)/(nx^2);
    fprintf('\nThe %% area fraction of precipitate at iter=%d is %f%%\n\n',iter, area_frac(1)*100);
    
    x=linspace(1,nx,nx);
    [x_coord,y_coord]=meshgrid(x,x);    
    close all,
    hfig=figure(1);
    set(hfig,'Position', [100, 100, 1000, 1000]);     
    surface(x_coord,y_coord,Conc);
    shading interp
    caxis([-0.2, 1])    
    axis([1 nx 1 nx -0.2 1]);   
    c = colorbar;
    ylabel(c, '$Conc$','fontsize',25,'FontWeight','Bold','interpreter','latex')    
    set(gca,'xtick',[1,nx/4*1,nx/4*2,nx/4*3,nx],'ytick',[1,nx/4*1,nx/4*2,nx/4*3,nx],'fontsize',25,'linewidth',2.5,'fontweight','bold')    
    xlabel({'X-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    ylabel({'Y-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex') 
    zlabel({'$Conc$'},'fontsize',30,'FontWeight','Bold','interpreter','latex') 
    title(sprintf('iter=%d',iter),'fontsize',30)    
    az = 45;
    el = 0;
    view(az, el); 
    filename='data/SrO_on_LSCF/161029_A/jpg/Concentration_Field_iter%d_CrossSecView.jpg'
    saveas(gcf,sprintf(filename,iter))     
    pause(2)
    
    az = 0;
    el = 90;
    view(az, el); 
    filename='data/SrO_on_LSCF/161029_A/jpg/Concentration_Field_iter%d_TopView.jpg'
    saveas(gcf,sprintf(filename,iter)) 
end