library(sas7bdat)#for opening sas file
library(dplyr)#for reshaping
library(leaflet)
library(rgdal) #for reading/writing geo files
library(rgeos) #for simplification
library(noncensus)
library(zipcode)

dat=readOGR(dsn=path.expand("~/nycdatascience/projects/project2_shiny/cb_2014_us_zcta510_500k"),layer="cb_2014_us_zcta510_500k")#read in shape data, its big
data(zip_codes)
nyc_zips=zip_codes[zip_codes$state=="NY",]#first must specify we only mean zips in NYS (other states have there own "Brooklyn")

nyc_zips=nyc_zips[nyc_zips$zip<11700,]#captures the zips in NYC and some extra
nyc_zips=nyc_zips[nyc_zips$zip>10000,]
nyc_zips=nyc_zips[nyc_zips$latitude<40.9,]#gets rid of most of the extra stuff to the north

subdat=dat[dat$GEOID10 %in% nyc_zips$zip,]

# ----- Transform to EPSG 4326 - WGS84 (required)
subdat=spTransform(subdat, CRS("+init=epsg:4326"))

# ----- save the data slot
subdat_data=subdat@data[,c("GEOID10", "ALAND10")]

# ----- simplification yields a SpatialPolygons class #not good enough for NYC, so i omit the simplification
#subdat<-gSimplify(subdat,tol=0.01, topologyPreserve=TRUE)

# ----- to write to geojson we need a SpatialPolygonsDataFrame
subdat=SpatialPolygonsDataFrame(subdat, data=subdat_data)
writeOGR(subdat,dsn=path.expand("~/nycdatascience/projects/project2_shiny/nyc_zip_mapping"),"nyc_zip_data",driver="ESRI Shapefile")#this wsrites a much smaller polygon shape file so that I don't need to upload as much.

### this is just to do the first big filters on each data set so they dont take as long to load, and so that they are small enough to be uploaded to github

#first prepare the health data, the data isn't big to begin with, but the sas7bdat reading takes a bit of time, so I do it here once only.
health=read.sas7bdat("chs2013_public.sas7bdat", debug=FALSE)#get the full health survey data, its in a funny SAS format, so i need this function to read it

health_zip=select(health,bmi,diabetes=diabetes13,sex,race=newrace,age=agegroup,borough,uhf42)#just get those columns we will use (borough is just kept as a check that I'm doing the UHF conversion correctly)
write.csv(health_zip, file='health_zip.csv') 


#and also the food data
food_raw=read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv",stringsAsFactors = FALSE)#its a little large, 180mb,so may take a bit
food=select(food_raw,name=DBA,zip=ZIPCODE,cuisine=CUISINE.DESCRIPTION,date=INSPECTION.DATE,address=BUILDING, street=STREET)#take only those columns we want
write.csv(food, file='food.csv') 

