
## Datos

pdf(file="figs/leveplotSISavOrig.pdf")
  library(raster)
  library(rasterVis)
  SISav <- raster('data/SISav')
  levelplot(SISav)
dev.off()

## Fronteras

  library(maps)
  library(mapdata)
  library(maptools)
  
  ext <- as.vector(extent(SISav))
  boundaries <- map('worldHires',
                    xlim=ext[1:2], ylim=ext[3:4],
                    plot=FALSE)
  boundaries <- map2SpatialLines(boundaries,
                                 proj4string=CRS(projection(SISav)))

## Fronteras

pdf(file="figs/leveplotSISavBoundaries.pdf")
  levelplot(SISav) + layer(sp.lines(boundaries, lwd=0.5))
dev.off()

## Hill shading

## - Download a Digital Elevation Model (DEM) from the DIVA-GIS service.

  old <- setwd(tempdir())
  download.file('http://biogeo.ucdavis.edu/data/diva/msk_alt/ESP_msk_alt.zip, 'ESP_msk_alt.zip')
  unzip('ESP_msk_alt.zip', exdir='.')
  
  DEM <- raster('ESP_msk_alt')

## Hill shading
## - Compute the hill shade raster with =terrain= and =hillShade= from =raster=.

  slope <- terrain(DEM, 'slope')
  aspect <- terrain(DEM, 'aspect')
  hs <- hillShade(slope=slope, aspect=aspect,
                  angle=20, direction=30)

  setwd(old)

## Hill Shading
## - Combine the result with the previous map using semitransparency.

png(filename="figs/hillShading.png",res=300,height=2000,width=2000)
  ## hillShade theme: gray colors and semitransparency
  hsTheme <- modifyList(GrTheme(), list(regions=list(alpha=0.6)))
  
  levelplot(SISav, panel=panel.levelplot.raster,
            margin=FALSE, colorkey=FALSE) +
      levelplot(hs, par.settings=hsTheme, maxpixels=1e6) +
      layer(sp.lines(boundaries, lwd=0.5))
dev.off()

## 3D

  plot3D(DEM, maxpixels=5e4)

## The output scene can be exported to several formats such as WebGL with
## =writeWebGL= to be rendered in a browser, or =STL= with =writeSTL=, a
## format commonly used in 3D printing. Files using this format are
## [[https://github.com/oscarperpinan/spacetime-vis/blob/gh-pages/images/DEM.stl][viewed easily on GitHub]].

writeSTL('figs/DEM.stl')

## Datos
## This section illustrates how to read and display rasters with
## categorical information using information from the NEO-NASA
## project.

  library(raster)
  ## China and India  
  ext <- extent(65, 135, 5, 55)
  
  pop <- raster('data/875430rgb-167772161.0.FLOAT.TIFF')
  pop <- crop(pop, ext)
  pop[pop==99999] <- NA
  
  landClass <- raster('data/241243rgb-167772161.0.TIFF')
  landClass <- crop(landClass, ext)

## RAT

  landClass[landClass %in% c(0, 254)] <- NA
  ## Only four groups are needed:
  ## Forests: 1:5
  ## Shrublands, etc: 6:11
  ## Agricultural/Urban: 12:14
  ## Snow: 15:16
  landClass <- cut(landClass, c(0, 5, 11, 14, 16))
  ## Add a Raster Attribute Table and define the raster as categorical data
  landClass <- ratify(landClass)
  ## Configure the RAT: first create a RAT data.frame using the
  ## levels method; second, set the values for each class (to be
  ## used by levelplot); third, assign this RAT to the raster
  ## using again levels
  rat <- levels(landClass)[[1]]
  rat$classes <- c('Forest', 'Land', 'Urban', 'Snow')
  levels(landClass) <- rat

## levelplot

pdf(file="figs/landClass.pdf")
  library(rasterVis)
  
  pal <- c('palegreen4', # Forest
           'lightgoldenrod', # Land
           'indianred4', # Urban
           'snow3')      # Snow
  
  catTheme <- modifyList(rasterTheme(),
                         list(panel.background = list(col='lightskyblue1'),
                              regions = list(col= pal)))
  
  levelplot(landClass, maxpixels=3.5e5, par.settings=catTheme,
            panel=panel.levelplot.raster)
dev.off()

## Relación con cuantitativos

pdf(file="figs/populationNASA.pdf")
  pPop <- levelplot(pop, zscaleLog=10, par.settings=BTCTheme,
                    maxpixels=3.5e5, panel=panel.levelplot.raster)
  pPop
dev.off()

## Histograma

pdf(file="figs/histogramLandClass.pdf")
  s <- stack(pop, landClass)
  names(s) <- c('pop', 'landClass')
  histogram(~log10(pop)|landClass, data=s,
            scales=list(relation='free'))
dev.off()

## Datos

  library(raster)
  library(zoo)
  library(rasterVis)
  
  SISdm <- brick('data/SISgal')
  
  timeIndex <- seq(as.Date('2011-01-01'), by='day', length=365)
  SISdm <- setZ(SISdm, timeIndex)
  names(SISdm) <- format(timeIndex, '%a_%Y%m%d')

## Level Plots

pdf(file="figs/SISdm.pdf")
  levelplot(SISdm, layers=1:12, panel=panel.levelplot.raster)
dev.off()

## zApply

  SISmm <- zApply(SISdm, by=as.yearmon, fun='mean')

pdf(file="figs/SISmm.pdf")
  levelplot(SISmm, panel=panel.levelplot.raster)
dev.off()

## Histogram

pdf(file="figs/SISdm_histogram.pdf")
  histogram(SISdm, FUN=as.yearmon)
dev.off()

## BWPlot

pdf(file="figs/SISdm_boxplot.pdf")
  bwplot(SISdm, FUN=as.yearmon)
dev.off()

## Splom

png(filename="figs/SISmm_splom.png",res=600,height=4000,width=4000)
  splom(SISmm, xlab='', plot.loess=TRUE)
dev.off()

## Hovmoller

pdf(file="figs/SISdm_hovmoller_lat.pdf")
  hovmoller(SISdm, par.settings=BTCTheme())
dev.off()

## xyplot

png(filename="figs/SISmm_xyplot.png",res=300,height=2000,width=2000)
  xyplot(SISdm, digits=1, col='black', lwd=0.2, alpha=0.6)
dev.off()

## Horizonplot

pdf(file="figs/SISdm_horizonplot.pdf")
  horizonplot(SISdm, digits=1,
              col.regions=rev(brewer.pal(n=6, 'PuOr')),
              xlab='', ylab='Latitude')
dev.off()

## Animation

  cft <- brick('data/cft_20130417_0000.nc')
  ## use memory instead of file
  cft[] <- getValues(cft)
  ## set projection
  projLCC2d <- "+proj=lcc +lon_0=-14.1 +lat_0=34.823 +lat_1=43 +lat_2=43 +x_0=536402.3 +y_0=-18558.61 +units=km +ellps=WGS84"
  projection(cft) <- projLCC2d
  #set time index
  timeIndex <- seq(as.POSIXct('2013-04-17 01:00:00', tz='UTC'), length=96, by='hour')
  cft <- setZ(cft, timeIndex)
  names(cft) <- format(timeIndex, 'D%d_H%H')

## Spatial Context: Administrative Boundaries

  library(maptools)
  library(rgdal)
  library(maps)
  library(mapdata)
  
  
  projLL <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
  cftLL <- projectExtent(cft, projLL)
  cftExt <- as.vector(bbox(cftLL))
  boundaries <- map('worldHires',
                    xlim=cftExt[c(1,3)], ylim=cftExt[c(2,4)],
                    plot=FALSE)
  boundaries <- map2SpatialLines(boundaries, proj4string=projLL)
  boundaries <- spTransform(boundaries, CRS(projLCC2d))

## Producing the Frames and the Movie

  cloudTheme <- rasterTheme(region=brewer.pal(n=9, 'Blues'))

  tmp <- tempdir()
  trellis.device(png, file=paste0(tmp, '/Rplot%02d.png'),
                        res=300, width=1500, height=1500)
  levelplot(cft, layout=c(1, 1), par.settings=cloudTheme) +
      layer(sp.lines(boundaries, lwd=0.6))
  dev.off()

## ffmpeg

  old <- setwd(tmp)
  ## Create a movie with ffmpeg using 6 frames per second a bitrate of 300kbs
  movieCMD <- 'ffmpeg -r 6 -b 300k -i Rplot%02d.png output.mp4'
  system(movieCMD)
  file.remove(dir(pattern='Rplot'))
  file.copy('output.mp4', paste0(old, '/figs/cft.mp4'), overwrite=TRUE)
  setwd(old)

## Static Image

pdf(file="figs/cft.pdf")
  levelplot(cft, layers=25:48, layout=c(6, 4),
            par.settings=cloudTheme,
            names.attr=paste0(sprintf('%02d', 1:24), 'h'),
            panel=panel.levelplot.raster) +
      layer(sp.lines(boundaries, lwd=0.6))
dev.off()

## Data

  library(raster)
  library(rasterVis)
  
  wDir <- raster('data/wDir')/180*pi
  wSpeed <- raster('data/wSpeed')
  windField <- stack(wSpeed, wDir)
  names(windField) <- c('magnitude', 'direction')

## Vectorplot

pdf(file="figs/vectorplot.pdf")
  vectorplot(windField, isField=TRUE, par.settings=BTCTheme(),
             colorkey=FALSE, scales=list(draw=FALSE))
dev.off()

## streamplot

pdf(file="figs/streamplot.pdf")
  myTheme <- streamTheme(region=rev(brewer.pal(n=4, name='Greys')),
                                      symbol=BTC(n=9, beg=20))
  streamplot(windField, isField=TRUE,
             par.settings=myTheme,
             droplet=list(pc=12),
             streamlet=list(L=5, h=5),
             scales=list(draw=FALSE),
             panel=panel.levelplot.raster)
dev.off()
