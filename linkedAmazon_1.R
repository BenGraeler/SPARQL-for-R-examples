# linked Amazon in R, part I by Benedikt Graeler, ben.graeler@uni-muenster.de
library(SPARQL)
library(sp)

# setting the endpoint
endpoint <- "http://spatial.linkedscience.org/sparql"

# defining a first querry as spatial setup
q <- "SELECT ?cell ?row ?col ?polygon
WHERE { 
   ?cell a <http://linkedscience.org/lsv/ns#Item> ;
         <http://spatial.linkedscience.org/context/amazon/Lin> ?row ;
         <http://spatial.linkedscience.org/context/amazon/Col> ?col ;
         <http://linkedscience.org/lsv/ns#border> ?polygon .
}"

# getting the data piece-wise to reduce the XML's size

res <- SPARQL(url=endpoint, q)$results

for(var in c("DEFOR_2002", "DEFOR_2003", "DEFOR_2004", "DEFOR_2005", "DEFOR_2006",
            "DEFOR_2007","DEFOR_2008")) {
  tmp_q <- paste("SELECT ?cell ?",var,"\n WHERE { \n ?cell a <http://linkedscience.org/lsv/ns#Item> ;\n <http://spatial.linkedscience.org/context/amazon/",var,"> ?",var," .\n }\n",sep="")
  cat(tmp_q)
  res <- merge(res, SPARQL(endpoint, tmp_q)$results, by="cell")
}

# creating the SpatialPixelsDataFrame
amazon <- res
amazon$row <- -res$row # swapping the y-axis

coordinates(amazon) <- ~ col+row
gridded(amazon) <- TRUE

# single map
spplot(amazon,"DEFOR_2002",col.regions=rev(heat.colors(17))[-1], at=(0:16)/100,
       main="relative deforestation per pixel during 2002")

# time series of maps
spplot(amazon, c("DEFOR_2002", "DEFOR_2003", "DEFOR_2004", "DEFOR_2005", 
                 "DEFOR_2006", "DEFOR_2007","DEFOR_2008"), 
       col.regions=rev(heat.colors(26))[-1], at=(0:20)/80,as.table=T,
       main="relative deforestation per pixel")

# cumulative deforestation per year
# assuming grid cells of 25km x 25km
cumDefor <- apply(amazon@data[,-c(1,2)],2,function(x) sum(x)*25*25) 

plot(2002:2008,cumDefor,type="b", col="blue", ylab="Deforestation [km?]",
     xlab="year", main="Deforestation from 2002 to 2008", ylim=c(0,26000))