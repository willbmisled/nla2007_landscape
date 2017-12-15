library(sf)
library(goatscape)  #install('C:/bryan/rscripts/goatscape')
library(tigris)
library(tidyverse)
library(magrittr)
options(tigris_use_cache = TRUE)

# #preallocate lists for output
# grp2000<-vector(mode = "list", length = 1159)
# blk2010<-vector(mode = "list", length = 1159)
# 
# #create df to track output
# samp<-data.frame(row=1:1159)
# samp$nla_id<-lakes$SITEID
# samp$pop2000<-NA
# samp$pop2010<-NA
# samp$time<-NA


#get nla lakes
lakes<-st_read('L:/Public/Milstead_Lakes/NLA_2007/GIS/gis_final/National_LakePoly.shp')

#convert to albers
lakes<-st_transform(lakes, "+init=ESRI:102008")

#load saved data (if continuing)
load(here::here('data/pop.rda'))

#get census data
todo<-which(is.na(samp$pop2000))
for(i in todo){
#for(i in c(668:1159)){
  #record start time
  start<-Sys.time()
  
  #select lake and buffer
  buf<-NA  #remove old output
  buf<-tryCatch(st_difference(st_buffer(lakes[i,], 5000), st_geometry(lakes[i,])), error = function(e) NA)
  
  #get census 2000
  grp2000[[i]]<- tryCatch(get_census(buf, year = 2000, spatial = TRUE, level = 'block_group', delete_zips = TRUE), 
                          error = function(e) NA)
  samp$pop2000[i]<-tryCatch(grp2000[[i]]$est_population, error = function(e) NA)
  
  #get census 2010
  blk2010[[i]]<- tryCatch(get_census(buf, year = 2010, spatial = TRUE, level = 'block', delete_zips = TRUE), 
                          error = function(e) NA)
  samp$pop2010[i]<-tryCatch(blk2010[[i]]$est_population, error = function(e) NA)
  
#calc processing time
  samp$time[i]<-difftime(Sys.time(), start, units = "min")

#add counter and save
  print(i)
  print(Sys.time())
  plot(blk2010[[i]]$census_data[17], 
       main = paste('i=',i, '; ', blk2010[[i]]$census_data$SITEID[1], '; pop=', round(samp$pop2010[i]), '; min=', round(samp$time[i]), sep=''))
  if(i %in% seq(9, 1159, 25)) save(grp2000, blk2010, samp, file = here::here('data/pop.rda'))
} 

#save(grp2000, samp, file = here::here('data/grp2000.rda'))
#save(blk2010, samp, file = here::here('data/blk2010.rda'))


################

go<-function(i) { #lake row num
  plot(lakes[i,1], main = paste(lakes$SITEID[i], "; area = ", round(lakes$LakeArea[i]), sep=''))
}

#go(911)

  ##################


#lets do the block group data for 2010 also

# #preallocate lists for output
grp2010<-vector(mode = "list", length = 1159)
#samp1<-samp
samp$time1<-NA
samp$pop2010bg<-NA

#get census data
todo<-which(is.na(samp$pop2010bg))
for(i in todo){
  #record start time
  start<-Sys.time()
  
  #select lake and buffer
  buf<-NA  #remove old output
  buf<-tryCatch(st_difference(st_buffer(lakes[i,], 5000), st_geometry(lakes[i,])), error = function(e) NA)
  
  #get census 2010
  grp2010[[i]]<- tryCatch(get_census(buf, year = 2010, spatial = TRUE, level = 'block_group', delete_zips = TRUE), 
                          error = function(e) NA)
  samp$pop2010bg[i]<-tryCatch(grp2010[[i]]$est_population, error = function(e) NA)
  
  #calc processing time
  samp$time1[i]<-difftime(Sys.time(), start, units = "min")
  
  #add counter and save
  print(i)
  print(Sys.time())
  plot(grp2010[[i]]$census_data[17], 
       main = paste('i=',i, '; ', grp2010[[i]]$census_data$SITEID[1], '; pop=', round(samp$pop2010bg[i]), '; min=', round(samp$time1[i]), sep=''))
  if(i %in% seq(9, 1159, 25)) save(grp2010, samp, file = here::here('data/grp2010.rda'))
}  

###########################################################get census data
for(i in c(1:1159)){
  #for(i in c(58:59)){
  #select lake and buffer
  buf<-NA  #remove old output
  buf<-tryCatch(st_difference(st_buffer(lakes[i,], 5000), st_geometry(lakes[i,])), error = function(e) NA)

  #get census 2010
  grp2010[[i]]<- tryCatch(get_census(buf, year = 2010, spatial = TRUE, level = 'block_group', delete_zips = TRUE),
                          error = function(e) NA)
  samp1$pop2010bg[i]<-tryCatch(grp2010[[i]]$est_population, error = function(e) NA)

  #add counter and save
  print(i)
  print(samp1$pop2010bg[i])
  plot(grp2010[[i]]$census_data[17], main = grp2010[[i]]$census_data$SITEID[1])
  if(i %in% seq(9, 1159, 25)) save(grp2010, samp1, file = here::here('data/pop1.rda') )
}

##################

head(samp)

par()
plot(samp$pop2010, samp$pop2010bg)
summary(lm(samp$pop2010~samp$pop2010bg))  # Rsq = .9974


#check the nla_id the same for all datasets and samp
chk_id<-data.frame(id2000  = rep('a', 1159),
                   id2010 = rep('b', 1159),
                   id2010bg = rep('c', 1159),
                   stringsAsFactors = FALSE)
for(i in 1:1159) {
  tt<-grp2000[[i]]$census_data 
    st_geometry(tt) <- NULL
  chk_id$id2000[i]<-as.character(tt[1, 1])

  
  tt<-grp2010[[i]]$census_data 
  st_geometry(tt) <- NULL
  chk_id$id2010[i]<-as.character(tt[1, 1])
  
  tt<-blk2010[[i]]$census_data 
  st_geometry(tt) <- NULL
  chk_id$id2010bg[i]<-as.character(tt[1, 1])
}

all.equal(as.character(samp$nla_id), chk_id$id2000)
all.equal(chk_id$id2000, chk_id$id2010)
all.equal(chk_id$id2010bg, chk_id$id2010)


############


bg2000<-grp2000[[i]]$census_data 
st_geometry(bg2000) <- NULL

nla_id<-as.character(bg2000[1,1])



#recreate df 'samp' and compare to original 

#data.frame to store data
samp_pop<-NA                ######later


i=1

bg2000<-grp2000[[i]]$census_data 
st_geometry(bg2000) <- NULL

nla_id<-as.character(bg2000[1,1])



as.character(grp2000[[1]]$census_data[1,1])
grp2000[[1]]$est_population

a<-as.tibble(grp2000[[i]]$census_data[-14]) %>%
  mutate(nla_id = as.character(SITEID))
str(a)

st_geometry(nc) <- NULL

a<-grp2000[[i]]$census_data 
st_geometry(a) <- NULL
