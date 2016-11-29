function main
    clear all
    clc

    time=[20000,40000,60000,80000];
    char_len=[73.023535,83.359538,90.626953,96.370599];    
    
    plot(time.^(0.25),char_len,'-*r','linewidth',3);    
    set(gca,'fontsize',20,'linewidth',2.5,'fontweight','bold')    
    xlabel({'$t^{1/4}$'},'fontsize',25,'FontWeight','Bold','interpreter','latex')        
    ylabel({'$S_v^{-1}$'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    title({'Cahn-Hilliard Equation with Variable Mobility'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    save2pdf('data/CH_SurfMob/jpg/Char_length_vs_time_plot.pdf')
    stop
    
    sizeArr=[256,256,256];
    nx=sizeArr(1);
    ny=sizeArr(2);
    nz=sizeArr(3);

    iter=8000000    
    % Read the compressed microstructral data
    fname = ['data/CH_SurfMob/CH_SurfMob_t%d_256.dat'];
    fid = fopen(sprintf(fname,iter));
    skip = fread(fid,1,'int32');
    a = fread(fid,nx*ny*nz,'float');
    fclose(fid);
    Conc = reshape(a, [nx ny,nz]);
    clear a;    
    
    Conc=Conc(1:nx/2,1:ny/2,1:nz/2);
    
    hfig=figure(1);
    set(hfig,'Position', [100, 100, 1000, 1000]);     
    p=patch(isosurface(Conc,0.5));
    set(p,'FaceColor','red','EdgeColor','none');
    daspect([1 1 1])
    view(3); 
    camlight 
    lighting gouraud
    axis([1 128 1 128 1 128]);   
    set(gca,'xtick',[1,100],'ytick',[1,100],'ztick',[1,100],...
        'fontsize',25,'linewidth',2.5,'fontweight','bold')    
    xlabel({'X-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    ylabel({'Y-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex') 
    zlabel({'Z-Coordinate'},'fontsize',30,'FontWeight','Bold','interpreter','latex') 
    title(sprintf('iter=%d',iter),'fontsize',30)    
    
    filename='data/CH_SurfMob/jpg/CH_SurfMob_t%d_256.jpg'
    saveas(gcf,sprintf(filename,iter))

    verts = get(p, 'Vertices');
    faces = get(p, 'Faces');
    a = verts(faces(:, 2), :) - verts(faces(:, 1), :);
    b = verts(faces(:, 3), :) - verts(faces(:, 1), :);
    c = cross(a, b, 2);
    area = 1/2 * sum(sqrt(sum(c.^2, 2)));
    char_len=1/(area/(nx*ny*nz))
    fprintf('\nThe characteristic length at iter=%d is %f\n\n',iter, char_len);
    
    

end