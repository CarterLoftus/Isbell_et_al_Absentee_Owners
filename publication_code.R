
############## Load libraries ##############

library(data.table)
library(stringr)
library(lubridate)
library(plyr)
library(sp)
library(adehabitatHR)
library(rgdal)
library(raster)
library(maptools)

############## Separate and clean the data ##############



df <- fread( 'Leopards, vervets, and baboons in Laikipia, Kenya.csv' ) ## reads in the data downloaded from Movebank

df <- as.data.frame( df ) ## turns the data.table into a data.frame

df <- df[ , names( df ) %in% c( 'timestamp' , 'ground-speed' , 'location-long' , 'location-lat' , 'heading' , 'individual-taxon-canonical-name' , 'individual-local-identifier' ) ] ## keep only the necessary columns

df$timestamp <- as.POSIXct(x= df$timestamp, format=c("%Y-%m-%d %H:%M:%S"), tz='UTC') ## turns the timestamp into a POSIX element

df$timestamp <- df$timestamp + 3*60*60  ## this makes the timestamp into local time by adding three hours. But don't be confused by the fact that it is still labeled as UTC. The timestamp is now local Kenyan time. I prefer to keep all timestamps in UTC regardless of their actual time zone

df <- df[ , c( 7, 1, 2, 3, 4, 5, 6 ) ] ## reorders the columns in a logical order

write.csv(df, "Leopards, vervets, and baboons in Laikipia, Kenya_trim.csv", row.names = F) ## save this trimmed data.frame for future use

## in case the above steps have already been done, one can start here:
df <- fread( "Leopards, vervets, and baboons in Laikipia, Kenya_trim.csv" ) 

## shorten the column names for convenience
names( df ) <- c( 'id', 'timestamp', 'lon', 'lat', 'speed', 'heading', 'species' )

## remove rows with unsuccessful GPS fixes
df <- df[ !is.na( df$lat ), ]

## Make the timestamp column into a POSIX element
df$timestamp <- as.POSIXct( df$timestamp, tz = 'UTC')

## split the dataframe into a list of dataframes. Each element of the list is all of the data for one species
split_df <- split( df, df$species )

## add a day and time column for each dataframe. The day column is the day of the study period. Day 1 is the first day that any individual of the given species began collecting successful fixes

for(i in 1:length( split_df ) ){
  ## add the day column
  split_df[[ i ]]$day <- as.numeric( as.Date( split_df[[ i ]]$timestamp) - min( as.Date( split_df[[ i ]]$timestamp ) ) + 1)
  
  ## add the time column. This is the same as the timestamp, but with the date removed
  split_df[[ i ]]$time <- str_split_fixed( split_df[[ i ]]$timestamp, " ", 2 )[ ,2 ]
}

## separate out the data.frames of the different species
## save the data of each species in the alphabetical order of their individuals' names. This is not essential, but is nice for organization.

verv_df <- split_df$Chlorocebus
verv_df <- verv_df[order(as.numeric(as.factor(verv_df$id))),]

leo_df <- split_df$`Panthera pardus`
leo_df <- leo_df[order(as.numeric(as.factor(leo_df$id))),]

bab_df <- split_df$Papio
bab_df <- bab_df[order(as.numeric(as.factor(bab_df$id))),]

## write the csv for each species separately
write.csv(verv_df,"verv_gps.csv", row.names = F)
write.csv(leo_df,"leo_gps.csv", row.names = F)
write.csv(bab_df,"bab_gps.csv", row.names = F)

############## Standardizing timestamps to determine synchronous locations ############## 


### The analysis proceeds from here using only one species. For this particular analysis, we focus on the vervets

## First, specify which species we are working with

which_spec <- "vervet"  ## options are "baboon", "vervet", or "leopard"

## read in the dataframe

if( which_spec == "baboon" ){
  spec_df <- read.csv( "bab_gps.csv" )
}else{
  if( which_spec == "vervet" ){
    spec_df <- read.csv( "verv_gps.csv" )
  }else{
    if( which_spec == "leopard" ){
      spec_df <- read.csv( "leo_gps.csv" )
    }
  }
}


## Make the timestamp into a POSIX element. Remember, even though it is labeled UTC, it is actually EAT (East Africa Time)
spec_df$timestamp <- as.POSIXct( x= spec_df$timestamp, tz = "UTC" )

## Make the time into a character, not a factor
spec_df$time <- as.character( spec_df$time )

## Make a column to label the group that the individual belongs to. Note that this is only relevant for the primates

if( which_spec != "leopard" ){
  spec_df$group <- str_split_fixed( spec_df$id, " ", 3)[ ,2 ]
}

## Visualize the data collection period for each animal
plot( as.numeric( as.factor( spec_df$id ) ) ~ spec_df$timestamp, cex=0.3, pch=16, main = "When does each collar collect data?", xlab="", xaxt='n', yaxt='n', ylab="ID") ## Make the plot. Breaks in the solid dots represent breaks in successful data collection for a given individual. 

axis(2,at=1:length(unique(as.factor(spec_df$id))),labels=sort(unique(as.factor(spec_df$id))),las=1,cex=0.3) ## Label the y-axis

axis.POSIXct(1,at=seq(min(spec_df$timestamp),max(spec_df$timestamp),by="10 day"), labels = as.Date(seq(min(spec_df$timestamp),max(spec_df$timestamp),by="10 day")), las=2,cex.axis = 0.5) ## Label the x-axis


## The following code is to used round the timestamps to the nearest of a set of potential timestamps that are standardized across the individuals in the study. This allows us to work with their data as though the GPS fixes of different animals were exactly simultaneous. Note that the round_date() function in package 'lubridate' will do the following process more efficiently

## first, save the exact timestamp in its own column called 'fix_timestamp' just so we have it later if we ever were to need to know it

spec_df$fix_timestamp <- spec_df$timestamp

## Now we will make an vector of times starting at the earliest time in the dataframe, rounded to the nearest 15 minutes (because the sampling interval of this GPS data is 15 minutes), and ending at the latest time in the dataframe rounded to the nearest 15 minutes. The vector will include all times at 15 minute intervals between those earliest and latest times. Think of this as the "ideal" GPS sampling schedule that was aimed for when programming the collars.

start <- round_date(min(spec_df$fix_timestamp), '15 mins')
end <- round_date(max(spec_df$fix_timestamp), '15 mins')
all_times <- seq(from = start, to = end, by = '15 min')

## This one line finds the closest standardized time to the exact fix time, and makes a column for the dataframe with the closest standardized time for each observation. This line can take a while to run.
spec_df$timestamp <- sapply(spec_df$fix_timestamp, function(x) all_times[ which.min(as.numeric( abs(x-all_times), units='mins' )) ])

## Turn our new standardized timestamps into a POSIX element
spec_df$timestamp <- as.POSIXct(spec_df$timestamp, origin = '1970-01-01', tz = "UTC")

as.numeric(max(abs(spec_df$fix_timestamp - spec_df$timestamp)))/60 ## shows the biggest time difference between the real time (fix_timestamp) and the rounded time (timestamp), in minutes. This by definition cannot be more than half of the sampling interval

hist(as.numeric(abs(spec_df$fix_timestamp - spec_df$timestamp)), breaks=100, xlab='Difference between timestamp and fix_timestamp (sec)', main="How far off are the fixes?") ## This shows the histogram of the differences between the real time and the rounded time. As long as most of these are pretty close to zero, we are fine to round the times to get simultaneous fixes in this way.

## In the process above, we may have created what are essentially duplicate fixes, if two real timestamps rounded to the same standardized timestamp. So now we need to remove the duplicate fixes. We will do this by removing the fix whose real timestamp is farther from the standardized timestamp

## split the dataframe into a list of dataframes. Each element of the list will be the data for a single individual
df_list <- split(spec_df, f = spec_df$id)

## Make a new list to fill with the individuals' data with the duplicates removed
new_df_list <- vector('list',length = length(df_list))

for(n in 1:length(df_list)){ ## for each individual...
  
  dat <- df_list[[n]] # save their data
  
  dat$timestamp <- as.POSIXct(dat$timestamp,tz = 'UTC')
  
  dat$fix_timestamp <- as.POSIXct(dat$fix_timestamp,tz = 'UTC')
  
  ind <- which(duplicated(dat$timestamp)) # find the duplicates
  
  if(length(ind) > 0){ # if there are duplicates...
    
    for(m in 1:length(ind)){ # loop through the following for the number of duplicates that there are
      
      temp_ind <- min(which(duplicated(dat$timestamp))) # find the index of the first duplicate timestamp
      
      dup_time <- dat$timestamp[temp_ind] # find the standardized time associated with that index
      
      new_temp <- dat[dat$timestamp == dup_time,] # subset out the rows that have this duplicated timestamp
      
      fix_to_drop <- new_temp$fix_timestamp[which.max(abs((new_temp$fix_timestamp - new_temp$timestamp)))] # find the fix that, of this subset, that is furthest in time from the standardized time
      
      dat <- dat[dat$fix_timestamp != fix_to_drop,] # remove it from the dataframe
    }
  }
  new_df_list[[n]]<- dat # after removal of duplicates, put this individual's data into the new list
}


spec_df <- ldply(new_df_list) ## Turns this list of individuals' dataframes back into one complete dataframe


## write the new dataframe into a csv

if(which_spec == "baboon"){
  write.csv(spec_df, "spec_df.csv", row.names = F)
}else{
  if(which_spec == "vervet"){
    write.csv(spec_df, "verv_df.csv", row.names = F)
  }else{
    if(which_spec == "leopard"){
      write.csv(spec_df, "leo_df.csv", row.names = F)
    }
  }
}


############## Determine home ranges and home range overlap zones ##############


if(which_spec == "baboon"){
  spec_df <- read.csv("bab_df.csv")
}else{
  if(which_spec == "vervet"){
    spec_df <- read.csv("verv_df.csv")
  }else{
    if(which_spec == "leopard"){
      spec_df <- read.csv("leo_df.csv")
    }
  }
}


# Make timestamp POSIX class 
spec_df$timestamp<-as.POSIXct(spec_df$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC")

### We want a track and home range at the group level, not the individual level. So remove any duplicate timestamps at the group level. We are going to treat the monkey groups as individuals here, so temporarily overwrite the id column with the information in the group column, and then remove duplicates

if(which_spec != "leopard"){
  spec_df$group <- str_split_fixed(spec_df$id, " ", 3)[,2]
  spec_df$id <- spec_df$group
}

spec_df <- spec_df[ !duplicated( spec_df[, c( 'id', 'timestamp' ) ] ), ]


### finding home ranges using the adehabitatHR package

crs_longlat <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

crs_utm <- CRS("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

## create a SpatialPointsDataFrame to use for finding the home ranges with adeHabitat
temp_sp <- SpatialPointsDataFrame(coords = spec_df[, c('lon','lat')], proj4string = crs_longlat, data = spec_df) 

## Transform the coordinates to UTMs
new_sp <- spTransform(temp_sp, crs_utm)

## First, estimate the utilization density
kud <- kernelUD(new_sp[, 'id'],
                h = "href", 
                same4all = F, 
                grid = 100,
                hlim = c(0.1, 2.0),
                kern = c("bivnorm"),
                boundary = NULL,
                extent = 0.2)

## Calculating home ranges from kernel utilization densities

## specify that we want the 99% KDE as our home range
level = 99

homerange <- getverticeshr(kud,
                           percent = level,
                           unin = 'm',
                           unout = 'm2')

### write the SpatialPolygonsDataFrame as a shapefile
dir.create( "Home ranges_adeHabitat/" )
writeOGR(homerange, paste( "Home ranges_adeHabitat/", which_spec, sep = ""), layer = paste("Home ranges_", level, sep = ''), driver="ESRI Shapefile")

## now just using the 99% KDE home ranges to find the home range overlap zones

#for each dyad, find the polygon of intersection between the dyad's home range polygons
for(i in 1 : (nrow(homerange)-1) ){
  
  for(j in (i+1) : nrow(homerange) ){
    
    #for each dyad, find the polygon of intersection between the dyad's home range polygons
    intersection <- intersect(homerange[i,]
                              , homerange[j,])
    
    if(!is.null(intersection)){
      
      ## if there is an intersection between the polygons, write the home range overlap zone polygon as a shapefile
      writeOGR(intersection, paste("Home ranges_adeHabitat/", which_spec, sep = ""), layer = paste("HR_overlap_", homerange$id[i], '_', homerange$id[j], sep = ''), driver="ESRI Shapefile")
    }
  }
}


## we will use the 50% KDE to define the home range core areas 
core_level = 50

core_area <- getverticeshr(kud,
                           percent = core_level,
                           unin = 'm',
                           unout = 'm2')

### write the SpatialPolygonsDataFrame as a shapefile
writeOGR(core_area, paste("Home ranges_adeHabitat/", which_spec, sep = ""), layer = paste("Core areas_", core_level, sep = ''), driver="ESRI Shapefile")

### now creating diel and nocturnal core areas
## first, I'll do the diel core area
## just ensure that the 'time' column comes from the 'timestamp' column and not the 'fix_timestamp' column
spec_df$time <- as.character(str_split_fixed(spec_df$timestamp, " ", 2)[, 2])

## subset to the diel hours
day_dat <- spec_df[(spec_df$time >= "06:30:00") & (spec_df$time <= "18:45:00"), ]

## create a SpatialPointsDataFrame to use for finding the home ranges with adeHabitat
day_sp_latlon <- SpatialPointsDataFrame( coords = day_dat[, c( 'lon', 'lat' ) ], proj4string = crs_longlat, data = day_dat) 

## Transform the coordinates to UTMs
day_sp <- spTransform(day_sp_latlon, crs_utm)

## First, estimate the utilization density

day_kud <- kernelUD(day_sp[, 'id'],
                h = "href", 
                same4all = F, 
                grid = 100,
                hlim = c(0.1, 2.0),
                kern = c("bivnorm"),
                boundary = NULL,
                extent = 0.2)

## Calculating core area from kernel utilization densities

day_core_area <- getverticeshr( day_kud,
                           percent = core_level,
                           unin = 'm',
                           unout = 'm2')

### write the SpatialPolygonsDataFrame as a shapefile
writeOGR(day_core_area, paste("Home ranges_adeHabitat/", which_spec, "/Day core areas", sep = ""), layer = paste("Day_core areas_", core_level, sep = ''), driver="ESRI Shapefile")

### And now the nocturnal core areas
## subset to the nighttime hours
night_dat <- spec_df[spec_df$time < "06:30:00" | spec_df$time > "18:45:00", ]

## create a SpatialPointsDataFrame to use for finding the home ranges with adeHabitat
night_sp_latlon <- SpatialPointsDataFrame(coords = night_dat[, c('lon','lat')], proj4string = crs_longlat, data = night_dat) 

## Transform the coordinates to UTMs
night_sp <- spTransform(night_sp_latlon, crs_utm)

## First, estimate the utilization density

night_kud <- kernelUD(night_sp[, 'id'],
                h = "href", 
                same4all = F, 
                grid = 100,
                hlim = c(0.1, 2.0),
                kern = c("bivnorm"),
                boundary = NULL,
                extent = 0.2)

## Calculating core area from kernel utilization densities

night_core_area <- getverticeshr(night_kud,
                           percent = core_level,
                           unin = 'm',
                           unout = 'm2')

### write the SpatialPolygonsDataFrame as a shapefile
writeOGR(night_core_area, paste("Home ranges_adeHabitat/", which_spec, "/Night core areas", sep = ""), layer = paste("Night_core areas_", core_level, sep = ''), driver="ESRI Shapefile")

### and lastly, the intergroup encounter zones  ## note that these lat-lon values could be derived directly, without hard-coding, from the dyadic distance data.frame produced below. Due to division of labor among the authors however, these values were determined in excel from the daydic distance data.frame, and then hard-coded here.

## first for KU and HP
min_lat <- 0.3168657
max_lat <- 0.3176727
min_lon <- 36.9078925
max_lon <- 36.909040

## turn these values into a matrix of coordinates
ku_hp <- matrix(c(min_lat, min_lon, min_lat, max_lon, max_lat, max_lon, max_lat, min_lon, min_lat, min_lon), ncol = 2, byrow = T)
ku_hp <- ku_hp[, c(2,1)]

## turn the coordinates into polygons
ku_hp_poly <- Polygon(ku_hp, hole = F)
ku_hp_polys <- Polygons(list(ku_hp_poly), ID = c('KU_HP'))

## now for KU and BR
min_lat <- 0.3016534
max_lat <- 0.3036502
min_lon <- 36.9073264
max_lon <- 36.9077639

## turn these values into a matrix of coordinates
ku_br <- matrix(c(min_lat, min_lon, min_lat, max_lon, max_lat, max_lon, max_lat, min_lon, min_lat, min_lon), ncol = 2, byrow = T)
ku_br <- ku_br[, c(2,1)]

## turn the coordinates into polygons
ku_br_poly <- Polygon(ku_br, hole = F)
ku_br_polys <- Polygons(list(ku_br_poly), ID = c('KU_BR'))

## turn the intergroup encounter zones into a spatial polygons dataframe
sp_df <- SpatialPolygons(list(ku_hp_polys, ku_br_polys), proj4string = crs_longlat)

## add ID information to the polygons
sp_df$id <- c("KU_HP", "KU_BR")

## transform the polygons to a UTM coordinate reference system
new_sp_df <- spTransform(sp_df, crs_utm)

### write the SpatialPolygonsDataFrame as a shapefile
writeOGR(sp_df, "Home ranges_adeHabitat/vervet", layer = "IGEZ_utm", driver="ESRI Shapefile")


############## Determine dyadic distances at each timestep ##############

## read in the relevant data.frame
if(which_spec == "baboon"){
  spec_df <- read.csv("spec_df.csv")
}else{
  if(which_spec == "vervet"){
    spec_df <- read.csv("verv_df.csv")
  }else{
    if(which_spec == "leopard"){
      spec_df <- read.csv("leo_df.csv")
    }
  }
}

## make the timestamp class POSIX
spec_df$timestamp <- as.POSIXct(x= spec_df$timestamp,tz = "UTC") ## Note that it's actually EAT. I just want to avoid problems associated with labeling it EAT

## make time a character instead of factor
spec_df$time <- as.character(spec_df$time) 

## save the names of the individuals
tag_names <- as.character(unique(spec_df$id))


### Do not change this. This just let's us know that there are approximately 111200 meters in a degree of latitude/longitude when at the equator. Note that this simple conversion works ONLY when at the equator. ## if repeating this analysis, it would be better to use UTMs to calculate dyadic distances
degree2MeterConversion <- 111200

## create a vector of all the unique timestamps represented in the data, from earliest time to latest time
sorted_time <- sort(unique(spec_df$timestamp))


## create an empty array. This will be an array of n x n matrices (n = number of individuals) that give the dyadic distances at a given time. The third dimension will be time, so the first matrix slice will be dyadic distances at the earliest timestamp in the study and the last matrix slice will be the dyadic distances at the latest timestamp in the study. Note that there a more efficient ways of obtaining a dyadic distance dataframe, and these will be implemented in the Loftus et al. package that is in prep
myArray <- array(NA, c(length(tag_names), length(tag_names), length(sorted_time)))
dimnames(myArray) <- list(tag_names, tag_names, sorted_time)

lower_triangle <- 1e20 ## just a random number used that we can easily identify later. This number will fill in the lower triangle and the diagonal of the dyadic distance matrices, which represents redundant and trivial, respectively, information.

for(i in 1:length(sorted_time)){ ## for each unique timestamp...

  nnMatrix<-matrix(lower_triangle , nrow = length(tag_names), ncol = length(tag_names),
                   dimnames = list(tag_names,
                                   tag_names)) # create an n x n matrix filled with 1e20 (we will come back through later and remove these)
  
  timeDat <- spec_df[ spec_df$timestamp == sorted_time[ i ], ] # subset the whole data set to just the data at our current unique timestamp
  
  for(a in 1:(length(tag_names)-1)){
    for(b in (a+1):length(tag_names)){
      # for each dyad, subset the subsetted dataframe to just the data for individual A at that time, and make another subset with individual B at that time
      idA<-timeDat[timeDat[,'id']==tag_names[a],]
      idB<-timeDat[timeDat[,'id']==tag_names[b],]
      
      if(nrow(idA)==0 | nrow(idB)==0){
        
        ## If either of them don't have data at this time, insert an NA into the dyadic distance matrix
        nnMatrix[a,b] <- NA 
        
      }else{
        
        ## If both have data, calculate the distance between them and insert it into the dyadic distance matrix
        nnMatrix[a,b]<-sqrt((idA[,'lon']-idB[,'lon'])**2 + (idA[,'lat']-idB[,'lat'])**2) 
        
      }
    }
  }
  ## Insert the dyadic distance matrix at this timestamp into the array for the whole study period
  myArray[ , , i ]<-nnMatrix
}


## Name the dimensions of the complete array
rownames(myArray) <- tag_names
colnames(myArray) <- tag_names
dimnames(myArray)[[3]] <- sorted_time

## multiply all the values by the degree-to-meter conversion, so that our distances are in meters and not degrees
myArray <- myArray*degree2MeterConversion

## turn the array into a dataframe so that we can save it as a csv
newDF <- adply(myArray, c(1,2,3), .id = c("id1","id2","timestamp"))

## name the column displaying the dyadic distances
names(newDF)[4]<-"DyadDist"

newDF$timestamp <- as.numeric(as.character(newDF$timestamp)) ## Because the timestamp is a factor, have to change it to as.character first

## now make the timestamp class POSIX
newDF$timestamp <- as.POSIXct(newDF$timestamp, origin='1970-01-01',tz='UTC')


## remove the rows that are redundant or trivial. After this removal, the rows that have an NA in them under DyadDist are those times at which one of the individuals does not have data 
newDF <- newDF[- which(newDF$DyadDist == (lower_triangle * degree2MeterConversion)), ]

## add the group identity information back in for the primates, and remove dyadic distances that are measured between individuals in the same group. We are not interested in those values in this analysis
if(which_spec != "leopard"){
  # label group identity
  newDF$group1 <- str_split_fixed(newDF$id1, " ", 3)[, 2]
  newDF$group2 <- str_split_fixed(newDF$id2, " ", 3)[, 2]
  
  ## cut out intra-group dyadic distances
  newDF <- newDF[ newDF$group1 != newDF$group2, ] 
}

## create a copy of the dataframe that includes only rows with known dyadic distances
newspec_df <- newDF[!is.na(newDF$DyadDist),]

dir.create( "dyadDistDFs/" )
dir.create( paste("dyadDistDFs/", which_spec,"/", sep = "") )

## write a csv of the copy without the NAs
write.csv(newspec_df, paste("dyadDistDFs/", which_spec,"/", which_spec, "_dyadDistDF_no_NAs.csv", sep = ""), row.names = F)

############## Use the dyadic distances to pull out potential encounter events ############## 

## set the distance threshold for what we consider a potential encounter 
dist_thres <- 50 # change this if runnning the leopard interactions

## set a time threshold. Encounters that happen within this number of minutes will be merged with each other rather than considered as distinct encounters
time_thres <- 75 ## interactions that occur within this number of minutes of each other will be merged

## set the sampling interval of the study
samp_int <- 15

## read in the dyadic distance dataframe that we just created
newDF <- read.csv(paste("dyadDistDFs/", which_spec,"/", which_spec, "_dyadDistDF_no_NAs.csv", sep = ""))

## make the ids characters instead of factors
newDF$id1 <- as.character(newDF$id1)
newDF$id2 <- as.character(newDF$id2)

## make the timestamp a POSIX element
newDF$timestamp <- as.POSIXct(newDF$timestamp,tz='UTC')

## the following function will provide a unique name to each unique group dyad
dy_name <- function(vec) {
  temp <- sort(c(vec['group1'],vec['group2'])) 
  return(paste(temp[1],temp[2],sep = '_'))
}

## for the primates, we want to know encounters at the group level, not the individual level. So we will remove rows from the dyadic distance dataframe that are redundant. (i.e. if group 1 has individuals A and B and group 2 has individual C, we don't need to know about an encounter between A and C and an encounter between B and C if they are simultaneous). We will keep the rows that minimize the dyadic distance so we don't miss any encounters that may have potentially happened. Note that this could section of the code would be made more efficient by using an aggregate function, aggregating across timestamps and using min() on the dyadic distance column as the aggregating function as the aggregating function

if(which_spec != "leopard"){
  newDF$group1 <- as.character(newDF$group1)
  newDF$group2 <- as.character(newDF$group2)
  
  ## use function from above to assign dyad ID name
  newDF$dyadID <- apply(newDF, 1, FUN = dy_name)
  
  ## give each row and index that will stay with it upon subsetting
  newDF$ind <- 1:nrow(newDF)
  
  ## create an empty vector of the indices to keep. We will fill this with the following loop
  inds_to_keep <- c()
  
  for(dy in unique(newDF$dyadID)){ ## for each group dyad...
    
    # subset the data to just this group dyad
    dyDF <- newDF[newDF$dyadID == dy, ]
    
    # save a vector of the unique times at which this group dyad has dyadic distance data
    times <- unique(dyDF$timestamp)
    
    for(time in times){ # for each time...
      
      # subset the dyad's data to this time 
      timeDF <- dyDF[dyDF$timestamp == time,] 
      
      # find the index of the row with the lowest dyadic distance. This is the row we will keep
      temp_ind <- timeDF$ind[which.min(timeDF$DyadDist)]
      
      # add the index of this row to the vector of indices to keep in our final dataframe
      inds_to_keep <- c(inds_to_keep, temp_ind)
      
    }
  }
}

## only keep the indices that have the minimum dyadic distance for a given group dyad at a given timestamp
newDF <- newDF[newDF$ind %in% inds_to_keep,] 

## remove the temporary index column
newDF <- newDF[ , -which(names(newDF) == 'ind')]

## create a dataframe with 0 rows that we will add rows to
dyadDurat<-data.frame(id1=character(), id2=character(), start_timestamp=character(), end_timestamp=character(), duration=character())

## save the names of the individuals in the study
tag_names <- unique(c(newDF$id1, newDF$id2))

for(a in 1:(length(tag_names)-1)){
  for(b in (a+1):(length(tag_names))){
    
    # for each dyad, subset the dyadic distance data to only the data for the dyad members, and to observations in which they are closer to each other than the distance threshold hyperparameter
    tempDF <- newDF[(newDF$id1==tag_names[a] & newDF$id2==tag_names[b] & newDF$DyadDist < dist_thres) | (newDF$id1==tag_names[b] & newDF$id2==tag_names[a] & newDF$DyadDist < dist_thres) , ]
    
    if(nrow(tempDF) != 0){ # if the dyad comes within this distance of each other...
      # find the time differences between observations of potential encounters
      diff_min <- diff(tempDF$timestamp) 
      
      # find out how many of these are consecutive GPS fixes
      tsig <- c(F,(abs(diff_min) == samp_int)) 
      
      # find the indices that represent the start of a potential encounter (i.e. more than the sampling interval after the previous time the dyad was in close proximity)
      startIndex <- which(tsig == F)
      
      # find the indices associated with the ends of potential encounters (when the next time of close proximity is more than the sampling interval after an observation of proximity)
      endIndex <- c((startIndex[-1] - 1), nrow(tempDF))
      
      # find the duration of each potential encounter
      dur_interact <- as.numeric(tempDF$timestamp[endIndex] - tempDF$timestamp[startIndex], units = 'mins')
      
      if(length(startIndex) >= 1){ # if there is at least one potential encounter (which there should be because we only went into this if statement for dyads that do come within the threshold for proximity)
        
        # make a temporary data frame documenting this potential encounter
        tempDF2 <- data.frame( id1 = rep(tag_names[a], each = length( startIndex )) , id2 = rep(tag_names[b], each = length( startIndex )), start_timestamp = tempDF$timestamp[startIndex], end_timestamp = tempDF$timestamp[endIndex], duration= dur_interact )
        
        # add this temporary dataframe to the running dataframe of all potential encounter events in the data
        dyadDurat<-rbind(dyadDurat,tempDF2)
      } 
    }  
  }
}


## Add the group identity and the dyad name to the encounter data
if(which_spec != 'leopard'){
  # add group identity information
  dyadDurat$group1 <- str_split_fixed(dyadDurat$id1," ",3)[,2]
  dyadDurat$group2 <- str_split_fixed(dyadDurat$id2," ",3)[,2]
  
  # add dyadID
  dyadDurat$dyadID <- apply(dyadDurat, 1, FUN = dy_name)
}else{
  
  #for leopards, the dyad ID will just be the individual-level dyad, rather than the group-level dyad used for the monkeys
  dyadDurat$dyadID <- paste(dyadDurat$id1, dyadDurat$id2, sep = '_')
}

## merge interactions that occur within the time threshold of each other. For the monkeys, this merge will occur across members of the same group. So it will merge all potential encounters between two groups within time threshold, not just two individuals.

## create a dataframe with 0 rows. After we merged encounter data to it, this will be the final data of potential encounters 
finalDurat <- data.frame(id1=character(), id2=character(), start_timestamp=character(), end_timestamp=character(), duration=character())

## save the unique group dyads
dyad_names <- unique(dyadDurat$dyadID)

for(i in 1:length(dyad_names)){ # for each unique dyad...
  
  # subset the encounter data to just this dyad
  dyadDF <- dyadDurat[dyadDurat$dyadID == dyad_names[i],]
  
  if(nrow(dyadDF) != 0){ # if this dyad does have encounters...
    
    # order the encounters in consecutive order by the time the encounter started
    dyadDF <- dyadDF[order(dyadDF$start_timestamp),]
    
    # find the time between the end of one interaction and the start of the next
    time_between <- as.numeric(dyadDF$start_timestamp[-1] - dyadDF$end_timestamp[- nrow(dyadDF)], units = 'mins')  
    
    # determine which of these are under the time threshold required for them to be considered two distinct encounters, and render a vector of booleans telling whether they are NOT distinct (i.e. True means they should be merged)
    bool <- c(F, time_between < time_thres) 
    
    # find when the distinct encounters (of those that require merging) end (indicated by a -1)
    inds <- diff( c( bool, F ) ) 
    
    # in the row where an encounter starts, replace the original end time with the end time that should result from merging encounters within the threshold time
    dyadDF$end_timestamp[inds == 1] <- dyadDF$end_timestamp[inds == -1]
    
    # only keep the rows that have the start of a distinct encounter
    dyadDF <- dyadDF[ bool == F, ]
    
    # add this dyad's ID to the total running dataframe containing all of the merged encounters
    finalDurat <- rbind(finalDurat, dyadDF)
  }
}


# recalculate the durations of the encounters
finalDurat$duration <- as.numeric(finalDurat$end_timestamp - finalDurat$start_timestamp, units = 'mins')

dir.create( "DyadDurats/" )
dir.create( paste("DyadDurats/", which_spec ) )
# write the csv of the data of the potential encounters
write.csv(finalDurat,paste("DyadDurats/", which_spec, "_dyadDurat.csv", sep = ""), row.names = F)



########## Subsetting the data to times when a group is making an incursion onto their neighbors home range. This is defined by anytime the group is either past the midpoint of the intergroup encounter zone that they share with their neighbor, or anytime that they are on their neighbors exclusive 99% home range (i.e. outside of their own 99% home range)  ##############

if(which_spec == "baboon"){
  spec_df <- read.csv("bab_df.csv")
}else{
  if(which_spec == "vervet"){
    spec_df <- read.csv("verv_df.csv")
  }else{
    if(which_spec == "leopard"){
      spec_df <- read.csv("leo_df.csv")
    }
  }
}

# Make timestamp POSIX class 
spec_df$timestamp<-as.POSIXct(spec_df$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC")

## create a SpatialPointsDataFrame
temp_sp <- SpatialPointsDataFrame(coords = spec_df[, c('lon','lat')], proj4string = crs_longlat, data = spec_df) 

## Transform the coordinates to UTMs
new_sp <- spTransform(temp_sp, crs_utm)

## save the set of unique group names to be used in the loop below
group_names <- as.character( unique( new_sp$group) )

## create an empty dataframe that we will fill in the loop below. This dataframe will include all GPS fixes that occur within the homerange overlap zones
overlap_df <- as.data.frame( matrix(NA, nrow = 0, ncol = (ncol(new_sp) + 1)))

## give the columns of the dataframe the appropriate names
names(overlap_df) <- c( names(new_sp), 'overlap')

for(i in 1 : (length(group_names) - 1) ){
  for(j in (i+1) : length(group_names) ){ ## for each group dyad...
    
    ## save a list of the vervet home range shapefiles
    files <- list.files(path = paste("Home ranges_adeHabitat/", which_spec, "/", sep = ""), pattern = "*.shp", full.names = TRUE, recursive = TRUE)
    
    ## save the name of the shapefile layer of the overlap zone for this group dyad
    layer_name <- paste("HR_overlap_", group_names[i], '_', group_names[j], sep = '')
    
    
    if( sum( grepl( layer_name, files) ) > 0 ){ ## if this group dyad has overlapping home ranges...
      
      ## read in the home range overlap shapefile
      overlap <- readOGR(dsn = paste("Home ranges_adeHabitat/", which_spec, "/", sep = ""), layer = paste("HR_overlap_", group_names[i], '_', group_names[j], sep = ''))
      
      
      for( k in c( i, j ) ){ ## for both groups in this dyad...
        
        ## subset the SpatialPointsDataframe to only this group's GPS fixes
        group_points <- new_sp[ new_sp$group == group_names[k], ]
        
        ## find GPS fixes that are inside the overlap area 
        inter <- over(x = group_points, y = overlap, returnList = F)
        
        ## find the indices of the GPS fixes inside the overlap area
        inds <- which( !is.na( inter$id_1 ) )
        
        ## subset the data to these fixes that occur within the overlap area
        temp <- group_points@data[inds, ]
    
        ## add a column to the dataframe that provides the name of the other group that shares this home range overlap zone currently being used
        temp$overlap <- group_names[ c( i , j )[ k != c( i, j ) ] ]
        
        ## add this dataframe to the running dataframe of all locations occuring within a home range overlap zone
        overlap_df <- rbind(overlap_df, temp)
        
        ## remove the temporary dataframe
        rm(temp)
      }
    }
  }
}

## add the rest of the information about the neighboring group that the focal group is overlapping with. This will be relevant because we want to know the other group's location at this time when the focal group is in the home range overlap zone
overlap_df <- merge( overlap_df, spec_df, by.x = c('overlap', 'timestamp'), by.y = c('group', 'timestamp'), all.x = T, all.y = F)

## reorder the dataframe by timestamp
overlap_df <- overlap_df[ order(overlap_df$timestamp), ]

## keep the columns that provide useful information
overlap_df <- overlap_df[ , c( 'id.x','group','timestamp', 'lon.x', 'lat.x', 'fix_timestamp.x', 'overlap', 'lon.y', 'lat.y', 'fix_timestamp.y')]

## rename the columns
names(overlap_df) <- c( 'id','group','timestamp', 'lon', 'lat', 'fix_timestamp', 'overlap_group', 'lon_overlap_group', 'lat_overlap_group', 'fix_timestamp_overlap_group')

## remove rows where the id and timestamp are duplicated. This is essentially removing rows that are redundantly telling us the location of several other individuals in a neighboring group when the focal group is in the overlap zone. Knowing the location of one individual in the neighboring group is sufficient here
overlap_df <- overlap_df[ !duplicated(overlap_df[ , c('id', 'timestamp')]), ]

dir.create( "Home range overlap zone use/" )
dir.create( paste0("Home range overlap zone use/", which_spec,"/" ) )
## write the dataframe to a csv
write.csv(overlap_df, paste0("Home range overlap zone use/", which_spec,"/", "overlap_df.csv"), row.names = F)

## remove reporting of individuals in their home range overlap zone when another individual in their group has already been reported in the home range overlap zone at that time. This information is redundant when we assume that one individual being in the overlap area is equivalent to the group being in the overlap area
trim_overlap <- overlap_df[ !duplicated(overlap_df[ , c('group', 'timestamp')]), ] 

## write this trimmed dataframe to a csv
write.csv(trim_overlap, paste0("Home range overlap zone use/", which_spec,"/", "trim_overlap.csv"), row.names = F)

## now we will subset the data to GPS fixes when a group travels into the "exclusive" home range of another group. i.e. when they are in another group's 99% home range, but not in their own
homeranges <- readOGR(dsn = paste("Home ranges_adeHabitat/", which_spec, "/", sep = ""), layer = 'Home ranges_99')

## create an empty dataframe with 0 rows that we will add to. This dataframe will contain all GPS points that occur when a group is outside its own 99% home range and on a different groups 99% home range
incur_df <- as.data.frame( matrix(NA, nrow = 0, ncol = (ncol(new_sp) + 1)))

## add the appropriate column names
names(incur_df) <- c( names(new_sp), 'incur')

for(i in 1 : (length(group_names) - 1) ){
  for(j in (i+1) : length(group_names) ){ ## for each group dyad...
    
    ## save a list of the vervet home range shapefiles (this line could be left outside of the loop )
    files <- list.files(path = paste("Home ranges_adeHabitat/", which_spec, "/", sep = ""), pattern = "*.shp", full.names = TRUE, recursive = TRUE)
  
    ## save the name of the shapefile layer of the overlap zone for this group dyad
    layer_name <- paste("HR_overlap_", group_names[i], '_', group_names[j], sep = '')
    
    if( sum( grepl( layer_name, files) ) > 0 ){ ## if the group dyad has home range overlap...
      
      ## load the shapefile the home range overlap of this dyad
      overlap <- readOGR(dsn = paste("Home ranges_adeHabitat/", which_spec, "/", sep = ""), layer = paste("HR_overlap_", group_names[i], '_', group_names[j], sep = ''))
      
      for( k in c( i, j ) ){ ## for each member of the dyad...
        
        ## subset the SpatialPointsDataframe to the GPS fixes of just this group
        group_points <- new_sp[ new_sp$group == group_names[k], ]
        
        ## subset the home ranges to the home ranges of all other groups (just excluding the focal group)
        temp_hrs <- homeranges[ homeranges$id != group_names[k], ]
        
        ## save the GPS fixes that are in the home range overlap area
        inter <- over(x = group_points, y = overlap, returnList = F)
        
        ## save the GPS fixes that are in the 99% home ranges of other groups
        incursion <- over(x = group_points, y = temp_hrs, returnList = F)
        
        ## save the indices of the GPS fixes that are not in the home range overlap area but are in the 99% home range of the other group in the group dyad. These are by defnition the fixes that are on other groups' exclusive home ranges
        inds <- which( is.na( inter$id_1 ) & incursion$id == group_names[ c( i, j )[ k != c( i, j ) ] ] )
        
        ## save the data associated with these indices
        temp <- group_points@data[inds, ]
        
        ## make a column denoting the name of the other group on whose home range the focal group is intruding
        temp$incur <- group_names[ c( i , j )[ k != c( i, j ) ] ]
        
        ## add this dataframe to the running dataframe of intrusions (or incursions) that we are building
        incur_df <- rbind(incur_df, temp)
        
        ## remove the temporary dataframe and SpatialPolygonsDataframe
        rm(temp)
        rm(temp_hrs)
      }
    }
  }
}

## add the rest of the information about the neighboring group that the focal group is intruding on. This will be relevant because we want to know the other group's location at this time when the focal group is intruding on their exclusive home range
incur_df <- merge( incur_df, spec_df, by.x = c('incur', 'timestamp'), by.y = c('group', 'timestamp'), all.x = T, all.y = F, sort = F)

## keep the columns that provide useful information
incur_df <- incur_df[ , c( 'id.x','group','timestamp', 'lon.x', 'lat.x', 'fix_timestamp.x', 'incur', 'lon.y', 'lat.y', 'fix_timestamp.y')]

## rename the columns
names(incur_df) <- c( 'id','group','timestamp', 'lon', 'lat', 'fix_timestamp', 'incur_group', 'lon_incur_group', 'lat_incur_group', 'fix_timestamp_incur_group')

## reorder the columns by timestamp
incur_df <- incur_df[ order(incur_df$timestamp), ]

## remove rows where the id and timestamp are duplicated. This is essentially removing rows that are redundantly telling us the location of several other individuals in a neighboring group when the focal group is in that group's exclusive home range. Knowing the location of one individual in the neighboring group is sufficient here.
incur_df <- incur_df[ !duplicated(incur_df[ , c('id', 'timestamp')]), ]

## write the dataframe to a csv
write.csv(incur_df, paste0("Home range overlap zone use/", which_spec,"/", "incur_df.csv"), row.names = F)

## remove reporting of individuals in their neighboring group's exclusive home ranges when another individual in their group has already been reported in the neighboring group's exclusive home range at that time. This information is redundant when we assume that one individual having intruded is equivalent to the whole group intruding
trim_incur <- incur_df[ !duplicated(incur_df[ , c('group', 'timestamp')]), ] 

## write the data to a csv
write.csv(trim_incur, paste0("Home range overlap zone use/", which_spec,"/", "trim_incur.csv"), row.names = F)

##### Now combining the GPS fixes in the overlap zone that are past the north-south midpoint of the intergroup encounter zones with the GPS fixes that occur on neighboring groups exclusive home ranges. Together, these GPS fixes will represent intrusions onto a neighboring group's territory ######

## read in the dataframes
incur_df <- read.csv(paste0("Home range overlap zone use/", which_spec,"/", "incur_df.csv"))
overlap_df <- read.csv(paste0("Home range overlap zone use/", which_spec,"/", "overlap_df.csv"))

## again saving the latitudes of the intergroup encounter zones for the relevant dyads
ku_hp_min_lat <- 0.3168657
ku_hp_max_lat <- 0.3176727

## find the north-south midpoint of the IGEZ for KU and HP
ku_hp_border <- (ku_hp_min_lat + ku_hp_max_lat) / 2 # for some reason, the base mean() function was not working here


ku_br_min_lat <- 0.3016534
ku_br_max_lat <- 0.3036502

## find the north-south midpoint of the IGEZ for KU and BR
ku_br_border <- (ku_br_min_lat + ku_br_max_lat) / 2 

## subset GPS fixes in the home range overlap area to only those that are past the midpoint of the IGEZ
sub_overlap <- overlap_df[ (overlap_df$group == "BR" & overlap_df$lat > ku_br_border) | (overlap_df$group == "KU" & overlap_df$lat < ku_br_border) | (overlap_df$group == "KU" & overlap_df$lat > ku_hp_border) | (overlap_df$group == "HP" & overlap_df$lat < ku_hp_border), ]

## make the column names of both dataframes the same as we will merge them in the next line
names(incur_df) <- names(sub_overlap)

## merge the dataframe containing fixes in the overlap zone that are past the midpoint of the IGEZ and the dataframe containing fixes occuring on neighboring groups' "exclusive" home ranges
sub_overlap <- rbind(sub_overlap, incur_df) 

## add the dyadic distances between the focal group and the group on whose territory they are intruding
sub_overlap$dyad_dist <- sqrt( ( sub_overlap$lon - sub_overlap$lon_overlap_group )**2 + ( sub_overlap$lat - sub_overlap$lat_overlap_group )**2 ) * degree2MeterConversion

## write the dataframe to a csv
write.csv(sub_overlap, paste0("Home range overlap zone use/", which_spec,"/", "sub_overlap.csv"), row.names = F)

## again trim the dataframe so we only get one row per timestamp for each group when they are beyond the midpoint of their IGEZ while in their home range overlap zone.
trim_sub_overlap <- sub_overlap[ !duplicated(sub_overlap[ , c('group', 'timestamp')]), ] 

## write the dataframe as a csv
write.csv(trim_sub_overlap, paste0("Home range overlap zone use/", which_spec,"/", "trim_sub_overlap.csv"), row.names = F)

######## Calculating home range diameter. Here we are just calculating the north-south distance between the northern and southern most point of home ranges given the linear layout of the home ranges #########

## load in the home range shapefiles
homeranges <- readOGR(dsn = paste("Home ranges_adeHabitat/", which_spec, "/", sep = ""), layer = 'Home ranges_99')

## create an dataframe of the home range diameter for each group that we will fill in
hr_diams <- data.frame( id = homeranges$id, diameter = NA)

for( id in homeranges$id ){ ## for each group
  
  ## subset the home range SpatialPolygonsDataframe to only this group
  id_range <- homeranges[ homeranges$id == id, ]
  
  ## subtract the southern most point of the home range polygon from the northern most point of the home range polygon, and save this diameter
  diam <- extent( id_range ) [ 4 ] - extent( id_range ) [ 3 ]
  
  ## add this diameter measurement to the dataframe
  hr_diams[ hr_diams$id == id, 'diameter' ] <- diam
  
}

## write the home range diameter dataframe to a csv 
write.csv( hr_diams, "homerange_diameter.csv", row.names = F )

########## Calculating population density #############

## take the union of all home ranges
total_range <- unionSpatialPolygons(homeranges,rep(1, length(homeranges )))

## calculate the area of the union of the home ranges
total_area <- area(total_range)

## divide by 1e6 to get the measurement from m^2 to km^2
area_km <- total_area * 1e-6

## total area is 1.722722 km^2 which translates to 108.9335 hectares. There are 71 individuals, so the population density is:

71 / 108.9335 ## in indiviuals per hectare

71 / area_km ## in individual per square kilometer

######## Calculating daily travel distance ###########        
### Daily travel distance measurements are quite sensitive to outlier GPS fixes. So we will first remove outlier GPS fixes before calculating daily travel distance ####

## read in the dataframe
if(which_spec == "baboon"){
  spec_df <- read.csv("bab_df.csv")
}else{
  if(which_spec == "vervet"){
    spec_df <- read.csv("verv_df.csv")
  }else{
    if(which_spec == "leopard"){
      spec_df <- read.csv("leo_df.csv")
    }
  }
}

## save the set of individual names
ids <- as.character( unique( spec_df$id ) )

## turn the timestamp into a POSIXct element
spec_df$fix_timestamp <- as.POSIXct( spec_df$fix_timestamp, tz = "UTC" )

## create a column for travel distance (distance from last GPS fix) and step speed (distance from last GPS fix divided by time since last GPS fix). These first distance and speed calculations will only be used to determine outlier GPS fixes
spec_df$trav_dist <- NA
spec_df$step_speed <- NA

for( id in ids ){ ## for each individual...
  
  ## subset the dataframe to only this individual's data
  id_dat <- spec_df[ spec_df$id == id, ]
  
  ## save the days for which this individual has data
  days <- as.character( unique( id_dat$day ) )
  
  for( day in days ){ ## for each day of this individual's data...
    
    ## subset the individual's data to only this one day
    day_dat <- id_dat[ id_dat$day == day, ]
     
    if( min( day_dat$time ) < "06:00:00" & max( day_dat$time ) > "18:00:00"){ ## if this day represents full day of data
      
      ## the next three lines calculate the distance from the previous fix for every fix on this day
      shift_lat <- c( NA, day_dat$lat[ - nrow( day_dat ) ] )
      shift_lon <- c( NA, day_dat$lon[ - nrow( day_dat ) ] )
      day_dat$trav_dist <- sqrt( (day_dat$lat - shift_lat)**2 + (day_dat$lon - shift_lon)**2 ) * degree2MeterConversion
      
      ## divide these travel distances by the time since the last fix to get the travel speed
      day_dat$step_speed <- day_dat$trav_dist / c(NA, as.numeric( diff( day_dat$fix_timestamp ), units = "mins" ) )
      
      ## insert this data from this day back into the individual's dataframe
      id_dat[ id_dat$day == day, ] <- day_dat
      
    }
  }
  ## insert this individual's data back in to the complete dataframe
  spec_df[ spec_df$id == id, ] <- id_dat
}

## show the histogram of travel sleeps so we can pick a reasonable cutoff to flag as potential outliers
hist(spec_df$step_speed * 15, breaks = 200, main = 'Histogram of travel speeds', xlab = 'Meters per fifteen minutes')

## the 99.9th percentile of travel speeds seems reasonable
abline( v = c( quantile( spec_df$step_speed * 15, 0.999 , na.rm = T)), col = 'red')

## flag the GPS fixes that are greater than the 99.9th percentile of travel speeds. We will check if these are outliers by seeing if the individual is within 200 meter of any of her groupmates at the time of these fixes. If she is not, then the GPS fix will be removed from the data
inds_to_check <- which( spec_df$step_speed * 15 > quantile( spec_df$step_speed * 15, 0.999, na.rm = T) )

## create an empty vector. We will fill this with the indices to remove from the data based on the procedure described above and below
inds_to_remove <- c()

for( ind in inds_to_check){ ## for each potential outlier...
  
  ## remove the fow associated with this GPS fix from the dataframe and save this as a dataframe
  other_df <- spec_df[ - ind, ]
  
  ## save also just row associated with this GPS fix
  temp <- spec_df[ ind, ]
  
  groupmate <- other_df[ other_df$timestamp == temp$timestamp & other_df$group == temp$group, ] ## subset the data to the data for the individual's groupmates at the time of this potential outlier GPS fix
  
  if( nrow( groupmate ) != 0 ){ ## if there are groupmates at this time...
    
    ## calculate the distance of the individual to each groupmate
    spread <- sqrt( (temp$lat - groupmate$lat)**2 + (temp$lon - groupmate$lon)**2 ) * degree2MeterConversion
    
    if( sum( spread <= 200 ) == 0 ){ ## if the individual is greater than 200 meters away from all of its groupmates...
      
      ## then add this GPS fix to the list of GPS fixes to remove as outliers
      inds_to_remove <- c( inds_to_remove, ind )
      
    }  
    
    
  }else{ ## if the individual's groupmates have not data at this timestamp...
    
    ## then add this GPS fix to the list of GPS fixes to remove as outliers
    inds_to_remove <- c( inds_to_remove, ind )
  }
  
}

## remove the confirmed outliers
spec_df <- spec_df[ - inds_to_remove,  ]


###### Now for the daily travel distance calculation ############

## Daily travel distance is also sensitive to GPS jitter when animals remain stationary for long periods of time. So we will spatially discretize the data before calculating the daily travel distance (see Methods of Isbell et al. for written details of this procedure).

## visualizing nighttime travel distances (when the vervets are likely not moving) will give us an idea of the scale at which we should spatially discretize the data. So first, subset the data to only nighttime GPS fixes
night <- spec_df[ spec_df$time > "23:00:00" | spec_df$time < "03:00:00", ]

## view historgram of nighttime travel distances
hist( night$trav_dist [night$trav_dist < 100], breaks = 200, main = 'Distances between consecutive nocturnal fixes', xlab = 'Distance from last fix (meters)' )

## it looks as though GPS jitter of more than 20 meters is quite rare, so we will spatially discretize the data to 20 meters

### Due to the recursive nature of the spatial discretization, the following code is hard to explain line by line so it is not commented. See Isbell et al. for a written explanation of the calculation or contact me at jcloftus@ucdavis.edu with questions. In sum, the following lines spatially discretize the data to 20 meters and calculate a daily travel distance for each individual ###

spec_df$spat.disc.dist <- NA

spec_df$lon_disc <- spec_df$lon

spec_df$lat_disc <- spec_df$lat

n <- 20 ### spatial discretization threshold

tag_names <- unique( spec_df$id )

for(j in tag_names){
  
  idDat <- spec_df[spec_df$id == j,] ## subset the data for the given group
  
  days <- as.character( unique( idDat$day ) )
  
  for( day in days ){
    
    day_dat <- idDat[ idDat$day == day, ]
    
    if( min( day_dat$time ) < "06:00:00" & max( day_dat$time ) > "18:00:00"){
      
      
      temp.lat<-day_dat$lat[ 1 ] ## set their first location to the first temp location
      temp.lon<-day_dat$lon[ 1 ]
      
      temp.count <- 1
      
      dist <- 0
      
      i <- 0 ## set counter to 0
      
      totalDist <- 0 ## set step length distance to 0
      
      while(i <= nrow(day_dat)){
        
        while(dist < n){
          
          if(i == nrow(day_dat)){
            break
          }
          
          if(i == 0 || i == temp.count){
            i <- i+1
            
            dist <- sqrt((day_dat$lat[i]-temp.lat)**2 + (day_dat$lon[i]-temp.lon)**2) * degree2MeterConversion
            
          }else{
            
            day_dat$spat.disc.dist[i] <- 0
            day_dat$lat_disc[i] <- day_dat$lat[temp.count]
            day_dat$lon_disc[i] <- day_dat$lon[temp.count]
            i<-i+1
            dist<-sqrt((day_dat$lat[i]-temp.lat)**2 + (day_dat$lon[i]-temp.lon)**2) * degree2MeterConversion
          }
        }
        
        if(dist < n){
          day_dat$spat.disc.dist[i] <- 0
          break
          
        }else{
          
          day_dat$spat.disc.dist[i] <- dist
          temp.lat<-day_dat$lat[i]
          temp.lon<-day_dat$lon[i]
          temp.count <- i
          dist <- 0
          
        }
      }
      
      idDat[ idDat$day == day, ] <- day_dat
      
    }
    
  }
  
  spec_df[spec_df$id == j,] <- idDat
  
}

## for each individual on each day, sum their step lengths to generate their daily travel distances
daily_trav_dists <- aggregate( spec_df$spat.disc.dist, by = list( spec_df$id, spec_df$day), FUN = sum, na.rm = T)

## rename the columns
names( daily_trav_dists ) <- c( 'id', 'day', 'trav_dist' )

## remove rows reporting that individuals have traveled a distance of 0 meters on a given day. These 0 values result from days on which the individual does not have a full day of data (because no step lengths are calculated on these days )
daily_trav_dists <- daily_trav_dists[ daily_trav_dists$trav_dist != 0, ]

## calculate the mean daily travel distance for each individual
mean_trav_dists <- aggregate( daily_trav_dists$trav_dist, by = list( daily_trav_dists$id ), FUN = mean ) 

## rename the dataframe columns
names( mean_trav_dists ) <- c( 'id', 'mean_trav_dist' )

## calculate the standard deviation of the daily travel distance for each individual
sd_trav_dists <- aggregate( daily_trav_dists$trav_dist, by = list( daily_trav_dists$id ), FUN = sd ) 

## rename the dataframe columns
names( sd_trav_dists ) <- c( 'id', 'sd_trav_dist' )

## merge the mean and standard deviation of travel distances into dataframe
summary_trav_dists <- merge( mean_trav_dists, sd_trav_dists, by = 'id' )

## write the dataframe to a csv
write.csv( summary_trav_dists, "summary_trav_dists.csv", row.names = F )



