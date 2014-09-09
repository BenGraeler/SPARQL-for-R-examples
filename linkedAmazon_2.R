# linked Amazon in R, part II by Benedikt Gr?ler, ben.graeler@uni-muenster.de
library(SPARQL) # min version 1.9
library(sp)

# setting the endpoint
endpoint <- "http://spatial.linkedscience.org/sparql"

# defining a first querry as spatial setup
q <- "SELECT ?cell ?row ?col ?polygon
WHERE { 
   ?cell a <http://linkedscience.org/lsv/ns#Item> ;
         <http://spatial.linkedscience.org/context/amazon/Lin> ?row ;
         <http://spatial.linkedscience.org/context/amazon/Col> ?col ;
         <http://observedchange.com/tisc/ns#geometry> ?polygon .
}"

# getting the data piece-wise to reduce the XML's size
res <- SPARQL(url=endpoint, q)$results

for(var in c("DEFOR_2002", "DEFOR_2003", "DEFOR_2004", "DEFOR_2005", "DEFOR_2006",
            "DEFOR_2007","DEFOR_2008")) {
  tmp_q <- paste("SELECT ?cell ?",var,"\n WHERE { \n ?cell a <http://linkedscience.org/lsv/ns#Item> ;\n <http://spatial.linkedscience.org/context/amazon/",var,"> ?",var," .\n }\n",sep="")
  cat(tmp_q)
  res <- merge(res, SPARQL(endpoint, tmp_q)$results, by="cell")
}

# helper function
createPolygons <- function(alistElem) {
  Polygon(matrix(unlist(lapply(strsplit(strsplit(alistElem,";")[[1]],","),as.numeric)),ncol=2,byrow=T))
}

polys <- lapply(res$polygon, createPolygons)

allPolys <- NULL
for ( i in 1:length(polys)) {
  if(i%%100 == 0) cat(i,"\n")
  allPolys <- append(allPolys, list(Polygons(list(polys[[i]]), ID=i)))
}

# create SpatialPolygons object
spPoly <- SpatialPolygons(allPolys, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# creating the SpatialPolygonsDataFrame
amazon <- res
amazon$row <- -res$row

amazonPoly <- SpatialPolygonsDataFrame(spPoly, data=amazon)

# added boundaries to the plot
load(url("http://www.gadm.org./data/rda/BRA_adm1.RData"))

spplot(amazonPoly,"DEFOR_2002", sp.layout = list("sp.polygons", gadm), 
       col.regions=rev(heat.colors(17))[-1], main="realtive deforestation in 2002 by pixel")

# aggregate pixels to state
# get centroids
getCentroids <- function(spP) {
  t(sapply(spP@polygons, function(x) x@labpt))
}

amazonCentr <- SpatialPointsDataFrame(coords=getCentroids(amazonPoly), data=amazonPoly@data)
proj4string(amazonCentr) <- proj4string(amazonPoly)
proj4string(gadm) <- proj4string(amazonPoly)

# absolute deforestation per state
addDefor <- function(x) {
  sum(x,na.rm=T)*25*25 # assuming cells of approx. 25kmx25km
}

aggAmazonAdd <- aggregate(amazonCentr[,-(1:4)], gadm, addDefor)

# relative deforestation per state
aggAmazonRel <- aggregate(amazonCentr[,-(1:4)], gadm, mean, na.rm=T)

aggAmazonAdd$name <- gadm$HASC_1
aggAmazonRel$name <- gadm$HASC_1

# correct for some centroids
countCells <- function(x) sum(!is.na(x))
aggAmazonCount <- aggregate(amazonCentr[,-(1:4)], gadm, countCells)
aggAmazonCount$name <- gadm$HASC_1

bool <- which(apply(aggAmazonCount@data[,1:7]>50,1,prod)==1)

aggAmazonAdd <- aggAmazonAdd[bool,]
aggAmazonRel <- aggAmazonRel[bool,]


# plot aggregates
spplot(aggAmazonAdd, "DEFOR_2002", col.regions=rev(heat.colors(21)), ylim=amazonPoly@bbox[2,],
       main="deforested area per state during 2002 [km]?", at=(0:21)*500,
       sp.layout=list("sp.text", getCentroids(aggAmazonAdd), aggAmazonAdd$name))

spplot(aggAmazonRel, "DEFOR_2002", col.regions=rev(heat.colors(20)), 
       ylim=amazonPoly@bbox[2,], main="relative deforested area per state during 2002",
       at=(0:15)/1000, sp.layout=list("sp.text", getCentroids(aggAmazonRel), aggAmazonRel$name))

# transposing the dataframe for plotting
trdf <- as.data.frame(t(aggAmazonRel@data[-8]))
colnames(trdf) <- as.character(aggAmazonRel@data$name)

library(lattice)
xyplot(BR.AC+BR.AP+BR.AM+BR.MA+BR.MT+BR.PA+BR.RO+BR.RR+BR.TO~2002:2008, 
       data=trdf, type="l", xlab="year", ylab="relative deforestation", 
       auto.key=list(space = "right", points = FALSE, lines = TRUE), 
       main="relative deforestation per state during different years")

