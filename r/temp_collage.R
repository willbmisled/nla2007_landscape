#http://www.masalmon.eu/2017/03/19/facesofr/


library(tidyverse)
library(sf)

load('C:/bryan/rscripts/nla2007_landscape/data/blk2010.rda')


#get nla lakes
lakes<-st_read('L:/Public/Milstead_Lakes/NLA_2007/GIS/gis_final/National_LakePoly.shp')
lakes<-st_transform(lakes, "+init=ESRI:102008")


i=sample(1:1159,1)
land<-st_transform(blk2010[[i]]$census_data, "+init=ESRI:102008")
buf<-st_difference(st_buffer(lakes[i,], 5000), st_geometry(lakes[i,]))
plot(land[17], 
     main = blk2010[[i]]$census_data$SITEID[1])
plot(lakes[i,1], add=TRUE, col='green')


st_crs(lakes)

st_difference(st_buffer(lakes[i,], 5000), st_geometry(lakes[i,]))

########################
plot(blk2010[[i]]$census_data[17], 
     main = blk2010[[i]]$census_data$SITEID[1], 
     col = blk2010[[i]]$census_data$pro_pop)




plot(blk2010[[i]]$census_data[17])


ggplot(blk2010[[1]]$census_data) + 
  geom_sf(aes(fill = pro_pop)) + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())


i=1
plot(st_geometry(blk2010[[i]]$census_data))



a<-data.frame(x = rnorm(10), y = rnorm(10))

  
  ggplot(a, aes(x, y)) +
       geom_point()
  
  
a<-which(is.na(samp$pop2000))
b<-which(is.na(samp$pop2010))
       
table(b%in%a)


todo<-which(is.na(samp$pop2010))
for(i in todo){
  print(samp$nla_id[i])
}

