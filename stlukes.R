# Data Source: PLUTO and MapPLUTO, available at:
# http://www.nyc.gov/html/dcp/html/bytes/applbyte.shtml

shape_fp <- "/Users/aaron/rdata/stlukes/Manhattan"
shape_fn <- "MNMapPLUTO.shp"

plutofp <- "/Users/aaron/rdata/stlukes/nyc_pluto_13v2"
plutofn <- "MN.csv"

setwd(plutofp)
pluto <- read.csv(plutofn, header=TRUE)
head(pluto)

######################################################################################################
## make corrections to new buildings (after 1969) in HD based on certificates of occupancy ###########
######################################################################################################

pluto$NumFloors[pluto$Block == 631 & pluto$Lot == 33] <- 0
pluto$NumFloors[pluto$Block == 612 & pluto$Lot == 28] <- 4
pluto$NumFloors[pluto$Block == 624 & pluto$Lot == 7502] <- 6
pluto$NumFloors[pluto$Block == 630 & pluto$Lot == 140] <- 3
pluto$NumFloors[pluto$Block == 552 & pluto$Lot == 7501] <- 6
pluto$NumFloors[pluto$Block == 572 & pluto$Lot == 70] <- 6
pluto$NumFloors[pluto$Block == 612 & pluto$Lot == 7501] <- 6
pluto$NumFloors[pluto$Block == 620 & pluto$Lot == 7501] <- 6
pluto$NumFloors[pluto$Block == 607 & pluto$Lot == 1] <- 17
pluto$YearBuilt[pluto$Block == 607 & pluto$Lot == 1] <- 2015


######################################################################################################
## MEASURE: CATEGORICAL NUMBER OF FLOORS
######################################################################################################

pluto$CatNumFloor[NumFloors < 5] <- 1
pluto$CatNumFloor[NumFloors > 4 & NumFloors < 7] <- 2
pluto$CatNumFloor[NumFloors > 6 & NumFloors < 11] <- 3
pluto$CatNumFloor[NumFloors > 10] <- 4
pluto$CatNumFloor[HistDist != "Greenwich Village"] <- 5
pluto$CatNumFloor <- factor(pluto$CatNumFloor, labels=c("4 or fewer floors", "5-6 floors", "7-9 floors", "10+ floors", "Not in HD"))

######################################################################################################
## MEASURE: CATEGORICAL NUMBER OF FLOORS, BUILT AFTER 1969
######################################################################################################

pluto$CatNumFloor_a[NumFloors < 5] <- 1
pluto$CatNumFloor_a[NumFloors > 4 & NumFloors < 7] <- 2
pluto$CatNumFloor_a[NumFloors > 6 & NumFloors < 11] <- 3
pluto$CatNumFloor_a[NumFloors > 10] <- 4
pluto$CatNumFloor_a[YearBuilt < 1970] <- 5
pluto$CatNumFloor_a[HistDist != "Greenwich Village"] <- 6
pluto$CatNumFloor_a <- factor(pluto$CatNumFloor_a, labels=c("4 or fewer floors", "5-6 floors", "7-9 floors", "10+ floors", "No Cons +1969", "Not in HD"))

colnames(pluto)

formapmerge <- pluto
formapmerge$id <- formapmerge$BBL
head(formapmerge)
formapmerge$BBL <- NULL




gvpluto <- pluto[pluto$HistDist == "Greenwich Village",]
summary(gvpluto)

table1 <- table(gvpluto$CatNumFloor)
table(gvpluto$CatNumFloor)
nrow(gvpluto)
prop.table(table1)

table(gvpluto$CatNumFloor_a)

2 / nrow(gvpluto[gvpluto$NumFloors > 9, ])
table(gvpluto[gvpluto$NumFloors > 9, "YearBuilt"])


"The total number of buildings in the Historic District:"
nrow(gvpluto)
"The number of buildings 15 floors or taller:"
nrow(gvpluto[gvpluto$NumFloors > 14,])
"The number of buildings 15 floors or taller built prior to 1940:"
nrow(gvpluto[gvpluto$NumFloors > 14 & gvpluto$YearBuilt > 1939,])

"The number of buildings with area of 70000sqft or greater:"
nrow(gvpluto[gvpluto$BldgArea > 69999,])


# packages
library(ggplot2)
library(maptools)
library(rgdal)
library(rgeos)
library(grid)


# map projection definition (do not change)
projx <- "+proj=lcc +lat_1=41.03333333333333 +lat_2=40.66666666666666 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000.0000000001 +y_0=0 +datum=NAD83 +units=us-ft +no_defs"

###################### GET MAP PROJECTION (SHAPE) FOR NYC ZIP CODES ################################################

# read shape file
setwd(shape_fp)
ogrListLayers(shape_fn)
nyc_shp <- readOGR(shape_fn, layer="MNMapPLUTO")
nycs <- fortify(nyc_shp, region = "BBL")

head(nycs)

summary(nycs)
summary(nyc_shp)


xLongitude <- c(-74.008578, -73.992732)
xLatitude <- c(40.739963, 40.729362)
conv <- data.frame(xLongitude, xLatitude)
conv

pLongitude <- -74.007491
pLatitude <- 40.731988
proposed <- data.frame(pLongitude, pLatitude)
pmap <- data.frame(project(cbind(proposed$pLongitude, proposed$pLatitude), projx))
pmap

lLongitude <- c(-73.999357, -74.001970, -74.006081, -73.996367, -73.995178)
lLatitude <- c(40.733893, 40.735111, 40.735144, 40.732248, 40.736642)
lStName <- c("Sixth Avenue", "Seventh Avenue", "Hudson Street", "Fifth Avenue", "14th Street")
lrot <- c(59, 64, 82, 59, 333)
l <- data.frame(lLongitude, lLatitude, lStName)
lmap <- data.frame(project(cbind(l$lLongitude, l$lLatitude), projx))
lmap <- cbind(lmap, lStName, lrot)
lmap

# project function in rgdal package to convert lat/long to projected coordinates
map <- data.frame(project(cbind(conv$xLongitude, conv$xLatitude), projx))
map

xwidth <- map$X1[2] - map$X1[1]
ywidth <- map$X2[1] - map$X2[2]
xtoy <- xwidth / ywidth

xwidth
ywidth
xtoy



####### MERGE ###############

formap <- merge(nycs, formapmerge, by="id", all.x=TRUE)
formap <- formap[order(formap$order), ]
head(formap)

############################################### draw the map ##############################################    
new <- ggplot() +
  geom_polygon(data=formap, aes(x = long, y = lat, group=group, fill= CatNumFloor_a), color="#A2A3A4") +
#  geom_text(data=newbuild, aes(x = XCoord, y = YCoord, label= NumFloors), size=2) + 
  geom_text(data=lmap, aes(x = X1, y = X2, label= lStName), size=3, angle=lrot) + 
  geom_point(data=pmap, aes(x = X1, y = X2), color = "#E53510", size=4) +
  coord_cartesian(xlim=c(map$X1[1], map$X1[2]), ylim=c(map$X2[2], map$X2[1])) +
  theme(line = element_blank()) +
  theme(rect = element_blank()) +
  theme(text = element_blank()) +
  theme(title = element_blank()) + 
  theme(plot.margin = unit(c(0,0,0,0), "in")) +
  theme(legend.position="none") +
  scale_fill_manual(values=c("#56A921", "#A8BD1D", "#D19117", "#E53510", "#FFFFFF", "#A2A3A4")) 

all <- ggplot() +
  geom_polygon(data=formap, aes(x = long, y = lat, group=group, fill= CatNumFloor), color="#A2A3A4") +
#  geom_text(data= gvpluto, aes(x = XCoord, y = YCoord, label= YearBuilt), size=2) + 
  geom_text(data=lmap, aes(x = X1, y = X2, label= lStName), size=3, angle=lrot) + 
  geom_point(data=pmap, aes(x = X1, y = X2), color = "#E53510", size=4) +
  coord_cartesian(xlim=c(map$X1[1], map$X1[2]), ylim=c(map$X2[2], map$X2[1])) +
  theme(line = element_blank()) +
  theme(rect = element_blank()) +
  theme(text = element_blank()) +
  theme(title = element_blank()) + 
  theme(plot.margin = unit(c(0,0,0,0), "in")) +
  theme(legend.position="none") +
  scale_fill_manual(values=c("#56A921", "#A8BD1D", "#D19117", "#E53510","#A2A3A4")) 

offset <- 0.0245
offset2 <- 0.0005

fp <-"/Users/aaron/rdata/stlukes"
setwd(fp)
png("all.png", height = 1200, width = 1200 * xtoy, res=144)
print(all)

grid.rect(x = 0.43, y=0.08 + offset, width=0.25, height=0.16, gp=gpar(col="black", fill="white"))

grid.rect(x = 0.36, y=0.14, width=0.05, height=0.02, gp=gpar(col="white", fill="#56A921"))
grid.rect(x = 0.36, y=0.12, width=0.05, height=0.02, gp=gpar(col="white", fill="#A8BD1D"))
grid.rect(x = 0.36, y=0.1, width=0.05, height=0.02, gp=gpar(col="white", fill="#D19117"))
grid.rect(x = 0.36, y=0.08, width=0.05, height=0.02, gp=gpar(col="white", fill="#E53510"))
grid.circle(x = 0.33, y=0.045, r=0.0075, gp=gpar(col="white", fill="#E53510"))
grid.lines(c(0.305,0.555), c(0.065,0.065))

grid.text("Number of Floors", x = 0.325, y=0.165, just="left", gp = gpar(fontsize=14, fontface="bold"))
grid.text("4 or fewer", x = 0.4, y=0.14, just="left", gp = gpar(fontsize=10, fontface="bold"))
grid.text("5 - 6", x =  0.4, y=0.12, just="left", gp = gpar(fontsize=10, fontface="bold"))
grid.text("7 - 9", x =  0.4, y=0.1, just="left", gp = gpar(fontsize=10, fontface="bold"))
grid.text("10 or more", x =  0.4, y=0.08, just="left", gp = gpar(fontsize=10, fontface="bold"))
grid.text("Proposed Development", x = 0.34, y=0.045, just="left", gp = gpar(fontsize=12, fontface="bold"))

grid.rect(x = 0.75-offset2, y=0.960-offset2, width=0.5, height=0.08, gp=gpar(col="black", fill="white"))

grid.text("Building Height", x = 0.53, y=0.979, just="left", gp = gpar(fontsize=18, fontface="bold"))
grid.text("Greenwich Village Historic District", x = 0.53, y=0.949 - offset2, just="left", gp = gpar(fontsize=16, fontface="bold"))

grid.text("Data Source: City of New York, Department of City Planning", x = 0.6, y=0.04, just="left", gp = gpar(fontsize=8, fontface="bold"))

dev.off()


png("new.png", height = 1200, width = 1200 * xtoy, res=144)
print(new)

grid.rect(x = 0.43, y=0.08 + offset, width=0.25, height=0.16, gp=gpar(col="black", fill="white"))

grid.rect(x = 0.36, y=0.14, width=0.05, height=0.02, gp=gpar(col="white", fill="#56A921"))
grid.rect(x = 0.36, y=0.12, width=0.05, height=0.02, gp=gpar(col="white", fill="#A8BD1D"))
grid.rect(x = 0.36, y=0.1, width=0.05, height=0.02, gp=gpar(col="white", fill="#D19117"))
grid.rect(x = 0.36, y=0.08, width=0.05, height=0.02, gp=gpar(col="white", fill="#E53510"))
grid.circle(x = 0.33, y=0.045, r=0.0075, gp=gpar(col="white", fill="#E53510"))
grid.lines(c(0.305,0.555), c(0.065,0.065))

grid.text("Number of Floors", x = 0.325, y=0.165, just="left", gp = gpar(fontsize=14, fontface="bold"))
grid.text("4 or fewer", x = 0.4, y=0.14, just="left", gp = gpar(fontsize=10, fontface="bold"))
grid.text("5 - 6", x =  0.4, y=0.12, just="left", gp = gpar(fontsize=10, fontface="bold"))
grid.text("7 - 9", x =  0.4, y=0.1, just="left", gp = gpar(fontsize=10, fontface="bold"))
grid.text("10 or more", x =  0.4, y=0.08, just="left", gp = gpar(fontsize=10, fontface="bold"))
grid.text("Proposed Development", x = 0.34, y=0.045, just="left", gp = gpar(fontsize=12, fontface="bold"))

grid.rect(x = 0.75-offset2, y=0.95-offset2, width=0.5, height=0.1, gp=gpar(col="black", fill="white"))

grid.text("Building Height", x = 0.53, y=0.98, just="left", gp = gpar(fontsize=18, fontface="bold"))
grid.text("Construction Since 1969", x = 0.53, y=0.95, just="left", gp = gpar(fontsize=16, fontface="bold"))
grid.text("Greenwich Village Historic District", x = 0.53, y=0.92, just="left", gp = gpar(fontsize=16, fontface="bold"))

grid.text("Data Source: City of New York, Department of City Planning", x = 0.6, y=0.04, just="left", gp = gpar(fontsize=8, fontface="bold"))

dev.off()




# raw shapefile
ggplot() +
  geom_polygon(data=nycs, aes(x = long, y = lat, group=group)) +
  coord_cartesian(xlim=c(map$X1[1], map$X1[2]), ylim=c(map$X2[2], map$X2[1])) +
  theme(line = element_blank()) +
  theme(rect = element_blank()) +
  theme(text = element_blank()) +
  theme(title = element_blank()) + 
  theme(plot.margin = unit(c(0,0,0,0), "in")) +
  theme(legend.position="none")
