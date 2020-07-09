
# https://mgimond.github.io/Spatial/raster-operations-in-r.html
# https://geocompr.robinlovelace.net/spatial-operations.html


dmspols <- raster(file.path(project_file_path, "Data", "RawData", "Nighttime Lights", "DMSP_OLS", "Individual Files", "eth_dmspols_2012.tif"))
dmspols_z <- raster(file.path(project_file_path, "Data", "RawData", "Nighttime Lights", "DMSP_OLS_INTERCALIBRATED_ZHANG2016", "F102012.tif"))
viirs <- raster(file.path(project_file_path, "Data", "RawData", "Nighttime Lights", "VIIRS", "Annual", "mean", "eth_viirs_mean_2012.tif"))

# viirs_pts <- rasterToPoints(viirs, spatial=T) 
# viirs_crd <- viirs_pts %>% coordinates() %>%
#   as.data.frame()

addis <- data.frame(id = 1,
           lat = 9.03,
           lon = 38.74) 
coordinates(addis) <- ~lon + lat
crs(addis) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
addis <- gBuffer(addis, width=30/111.12, byid=T)

viirs <- crop(viirs, addis)
dmspols <- crop(dmspols, addis)

plot(viirs)
plot(dmspols)

# raster.gaussian.smooth <- function(x, sigma = 2, n = 5, type = mean, ...) {  
#   if (!inherits(x, "RasterLayer")) stop("MUST BE RasterLayer OBJECT")
#   gm <- gaussian.kernel(sigma=sigma, n=n)
#   return( raster::focal(x, w = gm, fun = type, na.rm=TRUE, pad=FALSE, ...) )
# }  

sigma = 2
n = 5
gm <- gaussian.kernel(sigma=sigma, n=n)
gm <- gm * 1/max(gm) # standardize so max is 1

# 1. KDE
viirs_s <- raster::focal(viirs, w = gm, fun = "mean")
viirs_s_re <- resample(viirs_s, dmspols)

# 2. Log
viirs_s_re <- log(viirs_s_re + 1)

# 3. Sigmoid
sigmoid <- function(x, a, b, c, d){
  out <- a + b*(1 / (1 + exp(-c*(x-d)) ))
  return(out)
}

viirs_s_re_sig <- viirs_s_re
viirs_s_re_sig[] <- sigmoid(viirs_s_re_sig[], 6.5, 57.4,-1.9,10.8)



plot(viirs)
plot(viirs_s)
plot(viirs_s_re)
plot(viirs_s_re_sig)
plot(dmspols)




gf <- focalWeight(viirs, 2 , type="Gauss")
viirs_k <- focal(viirs,
                 w = weights)


library(spatstat)


library(ks)


plot(viirs)
a <- raster.gaussian.smooth(viirs, sigma = 1, n = 5, nc=11)




library(raster)
r <- raster(nrows=500, ncols=500, xmn=571823, xmx=616763, 
            ymn=4423540, ymx=4453690)
proj4string(r) <- crs("+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs")
r[] <- runif(ncell(r), 1000, 2500)
r <- focal(r, focalWeight(r, 150, "Gauss") )

# Calculate Gaussian smoothing with sigma(s) = 1-4
g1 <- raster.gaussian.smooth(r, sigma=1, nc=11)







set.seed(8192)
x <- 2^rnorm(100)
fhat <- kde(x=x, positive=TRUE)
plot(fhat, col=3)
points(c(0.5, 1), predict(fhat, x=c(0.5, 1)))

## large data example on non-default grid
## 151 x 151 grid = [-5,-4.933,..,5] x [-5,-4.933,..,5]
set.seed(8192)
x <- rmvnorm.mixt(10000, mus=c(0,0), Sigmas=invvech(c(1,0.8,1)))
fhat <- kde(x=x, binned=TRUE, compute.cont=TRUE, xmin=c(-5,-5), xmax=c(5,5), bgridsize=c(151,151))
plot(fhat)



kde2d(x = viirs_crd$x,
      y = viirs_crd$y)


library(spatstat)
spatstat::density
MASS::kde2d


pt.kde <- sp.kde(x = viirs_pts[1:10,], 
                 bw = res(viirs)[1]*2,
                 newdata = dmspols)



dmspols[] %>% mean(na.rm=T)




library(spatialEco)
library(sp)
library(raster)
data(meuse)
coordinates(meuse) <- ~x+y

# Unweighted KDE (spatial locations only)				
pt.kde <- sp.kde(x = meuse, 
                 bw = res(viirs)[1]*5, standardize = TRUE, 
                 nr=104, nc=78, scale.factor = 10000 )

# Plot results
plot(pt.kde, main="Unweighted kde")
points(meuse, pch=20, col="red") 

#### Using existing raster(s) to define grid ####

# Weighted KDE using cadmium and extent with row & col to define grid
e <- c(178605, 181390, 329714, 333611) 
cadmium.kde <- sp.kde(x = meuse, y = meuse$cadmium, bw = 1000,  
                      nr = 104, nc = 78, newdata = e, 
                      standardize = TRUE, 
                      scale.factor = 10000  )
plot(cadmium.kde)
points(meuse, pch=19)

