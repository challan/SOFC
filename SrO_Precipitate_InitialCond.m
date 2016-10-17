function main
    
    nx=2000;
    total_precipitates=100;% number of precipitates
    rng('shuffle');
    % rand_coords is a 2D array that contains the random x,y coordinates 
    % that could be the location of the precipitate. +30 is included just 
    % in case the random coordinates are rejected as a result of the
    % overlapping of the precipitates.
    rand_coords=rand(2,total_precipitates*3);
    rand_coords=floor(rand_coords*nx);
    
    % Actual coordinates of the precipitates
    precipit_coords=ones(2,total_precipitates);
    precipit_coords=100.d0*precipit_coords;
    
    %Introduce a sample precipitate in the center of the domain
    %Precipitate phase = 1, matrix phase =0
    x=linspace(1,2000,nx);
    [x_coord,y_coord]=meshgrid(x,x);    
    radius=10.0;
    xcenter_ori=1000;
    ycenter_ori=1000;
    delta=2.0;
    circle=sqrt((x_coord-xcenter_ori).^2+(y_coord-ycenter_ori).^2);
    phi_template=0.5*(1+tanh((radius-circle)/delta));
    phi=zeros(nx,nx);
    % Start with a precipitate in the center of the domain
    phi=phi+phi_template;

    coords_no=1; % index for rand_coords array
    no_of_precipitates=2; % index for precipit_coords
    while no_of_precipitates < 101
        coords_no
        no_of_precipitates
        xcenter=rand_coords(1,coords_no);
        ycenter=rand_coords(2,coords_no);
        
        % x,y coordinates of the precipitates that are introduced to the
        % system
        precipit_xcoords=precipit_coords(1,:);
        precipit_ycoords=precipit_coords(2,:);

        % distances between the precipitates that are already introduced in
        % the system and the new coordinate drawn from the rand_coords
        % array.
        distances=sqrt((precipit_xcoords-xcenter).^2-(precipit_ycoords-ycenter).^2);
        
        % Make sure that no two precipitates are within the distance of 15
        % from each other
        if min(distances) > 30 
            shift_in_x=xcenter-xcenter_ori;
            shift_in_y=ycenter-ycenter_ori; 
            phi_single_precipitate=circshift(phi_template,[-1*shift_in_y,-1*shift_in_x]);
            phi=phi+phi_single_precipitate;
            %Update the precipitate coordinate array
            precipit_coords(1,no_of_precipitates)=xcenter;
            precipit_coords(2,no_of_precipitates)=ycenter;
            coords_no=coords_no+1;
            no_of_precipitates=no_of_precipitates+1;
        else
            coords_no=coords_no+1;
        end       
    end    
   
    wh_over=find(phi > 1.0);
    max(phi(:))
    close all,
    hfig=figure(1);
    set(hfig,'Position', [100, 100, 1000, 1000]);     
    surface(x_coord,y_coord,phi);
    shading interp
    caxis([0, 1])
    c = colorbar;
    ylabel(c, '$\phi$','fontsize',25,'FontWeight','Bold','interpreter','latex')    
    axis([1 2000 1 2000 0 1]);
    set(gca,'xtick',[1,500,1000,1500,2000],'ytick',[1,500,1000,1500,2000],'fontsize',25,'linewidth',2.5,'fontweight','bold')    
    xlabel({'X-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex')
    ylabel({'Y-Coordinate'},'fontsize',25,'FontWeight','Bold','interpreter','latex') 
    zlabel({'$\phi$'},'fontsize',30,'FontWeight','Bold','interpreter','latex') 
    title('SrO Precipitate Initial Condition','fontsize',30)    
    az = 0;
    el = 90;
    view(az, el); 
    
    

    % Write out the file
    fname = ['data/SrO_on_LSCF_phi_t0_161017_Matlab.dat'];
    fileID = fopen(fname,'wb');
    fwrite(fileID,phi,'double');
    fclose(fileID);
    size(phi)
    
end