#######################################################
######## Acer rubrum herbarium phenology study ######## 
#######################################################
require(ggmap)
require(weatherr)
require(weatherData)
library(rnoaa)
library(dplyr)
library(lubridate)


ggele = function(lat=0,lon=0, output=c('elevation','elevation/resolution','all')) {
output <- match.arg(output)
if ((length(lat) != length(lon)) | (any(abs(lat)>90)) | (any(abs(lon)>180))) stop('Longitude and latitude should have equal length and within the valid range')
inpar = paste(paste0(lat,',', lon),collapse = ' | ')
url = paste0("https://maps.googleapis.com/maps/api/elevation/json?locations=", inpar)
u = paste0(paste0(readLines(url), collapse = "\n"), "\n")
tmp = try(RJSONIO::fromJSON(u))
if (class(tmp) == 'try-error') stop('Error: fail to obtain data from Google Elevation API')
if (tmp$status != 'OK') stop('Request denied')
switch(output, 'elevation' = {y=sapply(tmp$results,function(x) x$elevation);names(y)=1:length(tmp$results);y}, 'elevation/resolution' = {y=sapply(tmp$results,function(x) c(x$elevation,x$resolution));y=as.data.frame(y);colnames(y)=1:length(tmp$results);rownames(y) = c('elevation','resolution');y}, all = tmp)
}

#############
# IMPORT DATA
#############
library(readr)
acru.dat <- read_csv("~/Downloads/ACRU_herbdata_ALL.csv")
View(acru.dat)

##export
#write.csv(acru.dat, "/Acer phenology/ACRU_herbdata_ALL.csv")

#######################################################
######## Acer rubrum herbarium phenology study ######## 
#######################################################
require(ggmap)
require(weatherr)
require(weatherData)
library(rnoaa)
library(dplyr)
library(lubridate)

#############
# IMPORT DATA
#############
library(readr)
#acru.dat <- read_csv("DACRU_herbdata_ALL.csv")

######################################
# GEOREFERENCE DATA (via Google Maps)
######################################
for(i in 1:dim(acru.dat)[[1]]){
  # Does the specimen already have lat lon data? If not, then proceed:
  if(is.na(acru.dat$latitude[[i]])){
    # Is State AND County information available?
    # If not, code lat / lon at NA
    if(is.na(acru.dat$state[[i]])){
      acru.dat$latitude[[i]] = NA
      acru.dat$longitude[[i]] = NA
      print(paste(i, acru.dat$uniqueID[[i]],"has no state information",sep = ' '))
    } else {
      if(is.na(acru.dat$county[[i]]) & is.na(acru.dat$citytown[[i]]) & is.na(acru.dat$location[[i]])){
        acru.dat$latitude[[i]] = NA
        acru.dat$longitude[[i]] = NA
        print(paste(i, acru.dat$uniqueID[[i]],"has no county, city, or location information",sep = ' '))
      } else {
        # 1. look up location + city + county + state
        site = paste(acru.dat$location[[i]],acru.dat$citytown[[i]],paste(acru.dat$county[[i]],'County',sep = ' '),acru.dat$state[[i]],sep = ', ')
        geo.out = geocode(site, output = "latlon" , source = "google")
        # Was the site information able to be geocoded? If yes then add data to existing dataset:
        if(!any(is.na(geo.out))) {
          acru.dat$latitude[[i]] = geo.out$lat
          acru.dat$longitude[[i]] = geo.out$lon
          print(paste(i, acru.dat$uniqueID[[i]],"has geodata (level 1)",sep = ' '))
        } else { 
          # if site info not available, try with city, county and state
          # 2. look up city, county + state
          site = paste(acru.dat$citytown[[i]],paste(acru.dat$county[[i]],'County',sep = ' '),acru.dat$state[[i]],sep = ', ')
          geo.out = geocode(site, output = "latlon" , source = "google")
          # Was the site information able to be geocoded? If yes then add data to existing dataset:
          if(!any(is.na(geo.out))) {
            acru.dat$latitude[[i]] = geo.out$lat
            acru.dat$longitude[[i]] = geo.out$lon
            print(paste(i, acru.dat$uniqueID[[i]]," has geodata (level 2)",sep = ' '))
          } else {
            # if site info not available, try with county and state
            # 3. look up county + state
            site = paste(paste(acru.dat$county[[i]],'County',sep = ' '),acru.dat$state[[i]],sep = ', ')
            geo.out = geocode(site, output = "latlon" , source = "google")
            if(!any(is.na(geo.out))) {
              acru.dat$latitude[[i]] = geo.out$lat
              acru.dat$longitude[[i]] = geo.out$lon
              print(paste(i, acru.dat$uniqueID[[i]],"has geodata (level 3)",sep = ' '))
            } else {
              # if site info not available, code data as NA
              acru.dat$latitude[[i]] = NA
              acru.dat$longitude[[i]] = NA
              print(paste(i, acru.dat$uniqueID[[i]],"has no geodata (checked all levels)",sep = ' '))
            }
          }
        }
      }
    }
  } else { print(paste(i, acru.dat$uniqueID[[i]],"has geodata",sep = ' '))}
} 

############################################
# GET ELEVATION DATA FROM GMTED2010
############################################
# Removes pre-existing elevation estimates from JHRL that were in ft
# Collect elevation from Google API - DOES NOT WORK
#acru.dat$elevation = NA
for(i in 1:dim(acru.dat)[[1]]){
  Sys.sleep(1)
  # Does the specimen already have elevation data? If NO, then proceed:
  if(is.na(acru.dat$elevation[[i]])){
    # Does the specimen have lat/lon data? If YES, then proceed:
    if(!is.na(acru.dat$latitude[[i]])){
      elv = ggele(lat = acru.dat$latitude[[i]],lon = acru.dat$longitude[[i]])
      acru.dat$elevation[[i]] = elv[[1]]
      print(paste(i,acru.dat$uniqueID[[i]],'elevation is',elv[[1]],sep = ' '))
    } else {
      print(paste(i,acru.dat$uniqueID[[i]],'does not have elevation data',sep = ' '))
    }
  } else{ print(paste(i,acru.dat$uniqueID[[i]],'already has elevation data',sep = ' ')) }
}

# Extract elevation data from USGS GMTED2010 30" Arc Resolution
# Set temporary directory to save large raster files. You can delete afterwards
tmp_dir ='~/temp/raster/' 
rasterOptions(tmpdir = tmp_dir)

# Convert Lat/Lon into a coordinate object for extraction
acru.geo  = acru.dat[,c('longitude','latitude')]
acru.geo = subset(acru.geo,!is.na(longitude)) # remove points without out lat/lon
coordinates(acru.geo)=~longitude+latitude

# Load elevation raster file
gmted2010 = raster('env.gmted2010.elev_mean.tif')

# Extract elevation data from raster file. 
extdat.mn = extract(gmted2010, acru.geo, fun = median, na.rm = T)

acru.geo$elevation = extdat.mn
acru.geo = as.data.frame(acru.geo)
acru.geo$uniqueID = rownames(acru.geo)
acru.geo = acru.geo[,c('uniqueID','elevation')]

acru.dat = merge(acru.dat,acru.geo,by='uniqueID',all.x=T)


########################################
# IDENTIFY NEAREST GHCN CLIMATE STATIONS
########################################
# GHCN INFO @ https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
radius = 25 # 25km radius from lat/lon
ghcnd.stations = ghcnd_stations() # generate a list of GHCN stations
ghcnd.stations.df = as.data.frame(ghcnd.stations[ghcnd.stations$element %in% c('TMIN','TMAX'),])
acru.dat$id = acru.dat$uniqueID
acru.dat$stationID = NA
acru.dat$stationDistance = NA

# Identify GHCN stations within preset radius
for(i in 1:length(acru.dat$uniqueID)){
  # Does specimen have station data? If NO, then:
  if(is.na(acru.dat$stationID[[i]])){
    # Does specimen have lat / lon and year data? If YES, then:
    if(!is.na(acru.dat$latitude[[i]]) & !is.na(acru.dat$year[[i]])){
      stations = meteo_nearby_stations(acru.dat[i,], lat_colname = "latitude", lon_colname = "longitude", station_data = ghcnd.stations.df,radius = radius)
      stations = as.data.frame(stations)
      SpmnYr   = acru.dat$year[[i]]
      # Were stations found within radius of specimen? If NO, then:
      if(!nrow(stations)) {
        print(paste(i,acru.dat$uniqueID[[i]],"no stations within",radius,'km of specimen',sep = ' '))
      } else {
        # Check if station has temperature data for year specimen was collected
        for(k in 1:dim(stations)[[1]]){
          StatDat = ghcnd.stations.df[ghcnd.stations.df$id == stations[k,1],c('first_year','last_year')]
          if(StatDat$first_year[[1]] <= SpmnYr & StatDat$last_year[[1]] >= SpmnYr){
            acru.dat$stationID[[i]] = stations[k,1]
            acru.dat$stationDistance[[i]] = round(stations[k,5],1)
            print(paste(i,acru.dat$uniqueID[[i]],"nearest station is",stations[k,1],'at',round(stations[k,5],1),'km',sep = ' '))
            break
          }
        }
      } 
    } else {print(paste(i,acru.dat$uniqueID[[i]],'does not have location and/or year',sep = ' '))}
  } else {print(paste(i,acru.dat$uniqueID[[i]],'has station info',sep = ' '))}
}

########################################
# DOWNLOAD CLIMATE DATA FOR GHCN
########################################

monthly.climate.data = NULL
daily.climate.data   = NULL
clim.out = NULL
for(i in 1:length(acru.dat$uniqueID)){
  # Does specimen have station data? If YES, then:
  if(!is.na(acru.dat$stationID[[i]])){
    SpmnYr   = acru.dat$year[[i]]
    StatClim = ghcnd_search(stationid = acru.dat$stationID[[i]],date_min = paste(acru.dat$year[[i]],'-01-01', sep=''),
                            date_max = paste(acru.dat$year[[i]],'-12-31', sep=''),var = c('TMIN','TMAX'))
    
    #are both tmin and tmax present? If YES, then:
    if(all(c('tmin','tmax') %in% names(StatClim))) {
      print(paste('climate data downloaded for',SpmnYr,'tmin:', dim(StatClim[[1]])[[1]],'tmax:',dim(StatClim[[2]])[[1]],sep = ' '))
      # Generate empty data frame for a year with all days
      dd = seq(as.Date(paste(acru.dat$year[[i]],'-01-01', sep='')),as.Date(paste(acru.dat$year[[i]],'-12-31', sep='')),by='day')
      dd = as.data.frame(cbind(as.character(dd),month(dd)))
      names(dd) = c('date','month')
      dd$date = as.Date(dd$date)
      
      for(l in 1:length(StatClim)){
        # Does the data frame have data in it? If YES, then:
        if(length(StatClim[[l]][,1]) > 0) {
          StatClim[[l]]$month = month(StatClim[[l]]$date)
        } 
        dd = merge(dd,StatClim[[l]],by='date',all=T)
      }
      
      # create a list of daily data
      day.dat = dd[,c('date','month.x','id.x','tmin','tmax')]
      names(day.dat) = c('date','month','stationID','tmin','tmax')
      day.dat$uniqueID = acru.dat$uniqueID[[i]]
      daily.climate.data[[i]] = day.dat
      
      # Calculate Monthly Means
      mns = as.data.frame(matrix(NA,12,1))
      names(mns) = c('month')
      mns$month = as.factor(1:12)
      
      dd$month = month(dd$date)
      if(all(is.na(dd$tmin))) {
        dd$tmin = rep(-999,length(dd$tmin))
      }
      if(all(is.na(dd$tmax))) {
        dd$tmax = rep(-999,length(dd$tmax))
      }
      mn1 = aggregate(tmin ~ as.factor(month), dd, mean,na.action = na.omit)
      mn2 = aggregate(tmax ~ as.factor(month), dd, mean,na.action = na.omit)
      ll1 = aggregate(tmin ~ as.factor(month), dd, length,na.action = na.omit)
      ll2 = aggregate(tmax ~ as.factor(month), dd, length,na.action = na.omit)
      mns = merge(mns,mn1,by.x='month',by.y='as.factor(month)',all=T)
      mns = merge(mns,mn2,by.x='month',by.y='as.factor(month)',all=T)
      mns = merge(mns,ll1,by.x='month',by.y='as.factor(month)',all=T)
      mns = merge(mns,ll2,by.x='month',by.y='as.factor(month)',all=T)
      names(mns) = c('month','tmin','tmax','tminN','tmaxN')
      
      mn.ls = NULL
      for(n in 2:dim(mns)[[2]]) {
        tt = c(mns[,n])
        names(tt) = as.character(month(seq(as.Date("2000/1/1"), by = "month", length.out = 12),label=T,abbr=T))
        mn.ls = rbind(mn.ls, tt)
      }
      rownames(mn.ls) = seq(1:dim(mn.ls)[[1]])
      mn.ls = as.data.frame(mn.ls)
      mn.ls$variable = names(mns)[2:dim(mns)[[2]]]
      
      mn.ls$runID = i
      mn.ls$uniqueID = acru.dat$uniqueID[[i]]
      mn.ls[mn.ls == -999] = NA 
      monthly.climate.data = rbind(monthly.climate.data,mn.ls)
      
      print(paste(i,acru.dat$uniqueID[[i]],'has climate data for both climate variables',sep = ' '))
    }
    
    #is only one of tmin or tmax present? If YES, then:
    if(any(c('tmin','tmax') %in% names(StatClim)) & length(names(StatClim)) == 1) {
      print(paste('climate data downloaded for',SpmnYr,names(StatClim), dim(StatClim[[1]])[[1]],sep = ' '))
      # Generate empty data frame for a year with all days
      dd = seq(as.Date(paste(acru.dat$year[[i]],'-01-01', sep='')),as.Date(paste(acru.dat$year[[i]],'-12-31', sep='')),by='day')
      dd = as.data.frame(cbind(as.character(dd),month(dd)))
      names(dd) = c('date','month')
      dd$date = as.Date(dd$date)
      
      for(l in 1:length(StatClim)){
        # Does the data frame have data in it? If YES, then:
        if(length(StatClim[[l]][,1]) > 0) {
          StatClim[[l]]$month = month(StatClim[[l]]$date)
        } 
        dd = merge(dd,StatClim[[l]],by='date',all=T)
      }
      
      mns = as.data.frame(matrix(NA,12,1))
      names(mns) = c('month')
      mns$month = as.factor(1:12)
      # Calculate Monthly Means
      dd$month = month(dd$date)
      if(names(StatClim) == 'tmax') {
        if(all(is.na(dd$tmax))) {
          dd$tmax = rep(-999,length(dd$tmax))
        }
        mm = aggregate(tmax ~ as.factor(month), dd, mean,na.action = na.pass)
        ll = aggregate(tmax ~ as.factor(month), dd, length,na.action = na.omit)
        mns$tmin.x = rep(NA,12)
        mns = merge(mns,mm,by.x='month',by.y='as.factor(month)',all=T)
        mns$tmin.y = rep(NA,12)
        mns = merge(mns,ll,by.x='month',by.y='as.factor(month)',all=T)
      }
      if(names(StatClim) == 'tmin') {
        if(all(is.na(dd$tmin))) {
          dd$tmin = rep(-999,length(dd$tmin))
        }
        mm = aggregate(tmin ~ as.factor(month), dd, mean,na.action = na.pass)
        ll = aggregate(tmin ~ as.factor(month), dd, length,na.action = na.omit)
        mns = merge(mns,mm,by.x='month',by.y='as.factor(month)',all=T)
        mns$tmax.x = rep(NA,12)
        mns = merge(mns,mm,by.x='month',by.y='as.factor(month)',all=T)
        mns$tmax.y = rep(NA,12)
      }
      
      
      
      names(mns) = c('month','tmin','tmax','tminN','tmaxN')
      mn.ls = NULL
      for(n in 2:dim(mns)[[2]]) {
        tt = c(mns[,n])
        names(tt) = as.character(month(seq(as.Date("2000/1/1"), by = "month", length.out = 12),label=T,abbr=T))
        mn.ls = rbind(mn.ls, tt)
      }
      rownames(mn.ls) = seq(1:dim(mn.ls)[[1]])
      mn.ls = as.data.frame(mn.ls)
      mn.ls$variable = names(mns)[2:dim(mns)[[2]]]
      
      mn.ls$runID = i
      mn.ls$uniqueID = acru.dat$uniqueID[[i]]
      mn.ls[mn.ls == -999] = NA 
      clim.out = rbind(clim.out,mn.ls)
      print(paste(i,acru.dat$uniqueID[[i]],'has climate data for one temp variable',sep = ' '))
    }
    
  }else{ 
    mns.out = c(i, acru.dat$uniqueID[[i]],rep(NA,36))
    clim.out = rbind(clim.out,mns.out)
    print(paste(i,acru.dat$uniqueID[[i]],'does not have station data available',sep = ' ')) }
}

# Calculate Mean Monthly Temperature
monthly.temp.data = subset(monthly.climate.data,variable == c('tmin','tmax'))
monthly.tmean.data = aggregate(monthly.temp.data[,c(1:12)], by = list(monthly.temp.data$uniqueID),mean, na.rm = TRUE) # Tmean = (tmin+tmax)/2

colnames(monthly.tmean.data)[1] <- "uniqueID"

# Merge Monthly Tmean with main data.frame
acru.dat = merge(acru.dat,monthly.tmean.data,by='uniqueID',all.x=T)

### FINAL DATA SET OUTPUT ####

write.csv(acru.dat,'acru.dat.output.csv')


