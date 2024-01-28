
# Define grid limits
x1=-8;x2=0;y1=43;y2=48.1
# Define cell dimensions #
ax=0.25;ay=0.25#pelgas and juvena
# Define smoothing radius
u=1
# Define iteration number
ni=200
# Define grid
define.grid.poly.mask(x1,x2,y1,y2,ax,ay,u,ni,poligonit=FALSE)