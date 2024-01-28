
# Define grid limits
x1=-8;x2=0;y1=43;y2=48.1
# Define cell dimensions #> 111*0.01= 1.11km ; 0.014*80 = 1.12km
ax=1;ay=1#evhoe
# Define smoothing radius
u=1
# Define iteration number
ni=200

# To define a polygon in which to make the gridmaps,
# put polygon longitudes in a xpol vector and 
# polygon latitudes in ypol vector

data(PELGASpolygon)
xpol=PELGASpolygon$x
ypol=PELGASpolygon$y

# Define grid
define.grid.poly.mask(x1,x2,y1,y2,ax,ay,u,ni,poligonit=FALSE)



### to check the polygon ####
# Plot grid and polygon
#plot(matxg,matyg,main='Defined grid and polygon',
#     xlab='',ylab='')
#add polygon
#sel=inout(pts=cbind(as.vector(matxg),as.vector(matyg)),
#          poly=cbind(xpol,ypol),bound=T,quiet=T)
#lines(xpol,ypol)
#coast()
#points(matxg[sel],matyg[sel],pch=16)
#legend('topleft',c('Discarded','Selected'),pch=c(1,16))
