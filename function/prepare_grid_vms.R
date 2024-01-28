
# Define grid limits
x1=-8;x2=0;y1=43;y2=48.1
# Define cell dimensions 
ax=0.25;ay=0.25#vms
# Define smoothing radius = search the point around the cell
u=1
# Define iteration number
ni=200

# load PELGAS polygon
data("PELGASpolygon")
# define new polygon to be used for block averaging
xpol=PELGASpolygon$x
ypol=PELGASpolygon$y

# 3. Define and select a user-defined polygon
define.grid.poly.mask(x1,x2,y1,y2,ax,ay,u,ni,shelf=TRUE,
                      poligonit=TRUE)
# load PELGAS polygon
data("PELGASpolygon")
# define new polygon to be used for block averaging
xpol=PELGASpolygon$x
ypol=PELGASpolygon$y

inout(pts=cbind(as.vector(matxg),as.vector(matyg)),
               poly=cbind(xpol,ypol),bound=T,quiet=T)










## Plot grid and polygon
#plot(matxg,matyg,main='Grid nodes in polygon',
#     xlab='',ylab='')
##add polygon
#sel=inout(pts=cbind(as.vector(matxg),as.vector(matyg)),
#          poly=cbind(xpol,ypol),bound=T,quiet=T)
#lines(xpol,ypol)
#coast()
#points(matxg[sel],matyg[sel],pch=16)
#legend('topleft',c('Discarded','Selected'),pch=c(1,16))

#vecxg <- sel*vecxg
#matxg <- matrix(data = vecxg, nrow = 27, ncol = 21)
#
#vecyg <- sel*vecyg
#matyg <- matrix(data = vecyg, nrow = 27, ncol = 21)
#
#vecig <- sel*vecig
#matig <- matrix(data = vecig, nrow = 27, ncol = 21)
#
#vecjg <- sel*vecjg
#matjg <- matrix(data = vecjg, nrow = 27, ncol = 21)