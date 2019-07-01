library(sf)
library(spatstat)
library(maptools)
library(raster)
library(cartography)
library(tanaka)
url <- "https://gist.githubusercontent.com/rCarto/747164575e3f216a123c3092d0ce91
62/raw/f12390464f255b5f9760c577ab6bf5456cf61a40/iris75.geojson"
raw <- st_read(url)
feat <- as(st_transform(raw, 2154), 'Spatial')
coords <- coordinates(feat)
pts <- ppp(x = coords[,1], y = coords[,2], window = as.owin(feat, 10), 
           marks = feat$P14_POP)
ds <- density.ppp(x = pts, sigma = 300, eps = 100, weights = pts$marks)
ras <- raster(ds, crs = proj4string(feat)) * 1000 * 1000
ras[is.na(ras)] <- 0
bks <- c(seq(0,50000, 5000), 54100)
pal <- hcl.colors(n = 11, palette = "inferno")
800*1.5
500
png(filename = "img/paris.png", width = 800, height = 500, res = 100)
par(mar = c(0,0,1.2,0), bg = "ivory1")
tanaka(x = ras, breaks = bks, col = pal, mask = feat, legend.pos = "n")
legendChoro(pos = "topright", breaks = bks, col = pal, nodata = F,
            title.txt = "Inhabitants\nper sq. km *", cex = 0.8)
layoutLayer(title = "Smoothed Population Density, Paris 2014",
            col = "ivory1", tabtitle = T, coltitle = "black",
            frame = T, scale = 1, sources = "Giraud & Lambert, 2019",
            author = 
     "Contours...Iris® - IGN 2017, Recensements de la population - Insee 2017")
north(pos = c(661000,6857900))
text(x = 654500, y = 6856900, adj = 0, cex = 0.6, font = 3,
     labels=
'(*) Kernel Density Estimation with\n    a gaussian kernel (sigma = 300 m)')
dev.off()





feat <- st_transform(raw, 2154)
feat$DENS <- (feat$P14_POP / st_area(feat))*(1000*1000)
x <- st_set_geometry(feat, NULL)
x <- x[,1:2]
View(x)

bks <- c(seq(0,50000, 5000), 132631)
pal <- hcl.colors(n = 11, palette = "inferno")
png(filename = "img/paris0.png", width = 800, height = 500, res = 100)
par(mar = c(0,0,1.2,0), bg = "ivory1")
choroLayer(feat,var = "DENS", breaks = bks, col = pal, legend.pos = "n", border = NA)
legendChoro(pos = "topright", breaks = bks, col = pal, nodata = F,
            title.txt = "Inhabitants\nper sq. km *", cex = 0.8)
layoutLayer(title = "Population Density, Paris 2014",
            col = "ivory1", tabtitle = T, coltitle = "black",
            frame = T, scale = 1, sources = "Giraud & Lambert, 2019",
            author = 
              "Contours...Iris® - IGN 2017, Recensements de la population - Insee 2017")
north(pos = c(661000,6857900))
dev.off()
par(mar = c(0,0,0,0))
plot(feat$geometry)
png(filename = "img/paris.png", width = 800, height = 500, res = 100)
par(mar = c(0,0,1.2,0), bg = "ivory1")
tanaka(x = ras, breaks = bks, col = pal, mask = feat, legend.pos = "n")
legendChoro(pos = "topright", breaks = bks, col = pal, nodata = F,
            title.txt = "Inhabitants\nper sq. km *", cex = 0.8)
layoutLayer(title = "Smoothed Population Density, Paris 2014",
            col = "ivory1", tabtitle = T, coltitle = "black",
            frame = T, scale = 1, sources = "Giraud & Lambert, 2019",
            author = 
              "Contours...Iris® - IGN 2017, Recensements de la population - Insee 2017")
north(pos = c(661000,6857900))
text(x = 654500, y = 6856900, adj = 0, cex = 0.6, font = 3,
     labels=
       '(*) Kernel Density Estimation with\n    a gaussian kernel (sigma = 300 m)')
dev.off()