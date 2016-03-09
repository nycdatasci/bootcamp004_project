# # Data Description - Forest Cover Classification - Machine learning data
# #   
# Given is the attribute name, attribute type, the measurement unit and
# a brief description.  The forest cover type is the classification 
# problem.  The order of this listing corresponds to the order of 
# numerals along the rows of the database.
# 
# Name                                     Data Type    Measurement                       Description
# 
# Elevation                               quantitative    meters                       Elevation in meters
# Aspect                                  quantitative    azimuth                      Aspect in degrees azimuth
# Slope                                   quantitative    degrees                      Slope in degrees
# Horizontal_Distance_To_Hydrology        quantitative    meters                       Horz Dist to nearest surface water features
# Vertical_Distance_To_Hydrology          quantitative    meters                       Vert Dist to nearest surface water features
# Horizontal_Distance_To_Roadways         quantitative    meters                       Horz Dist to nearest roadway
# Hillshade_9am                           quantitative    0 to 255 index               Hillshade index at 9am, summer solstice
# Hillshade_Noon                          quantitative    0 to 255 index               Hillshade index at noon, summer soltice
# Hillshade_3pm                           quantitative    0 to 255 index               Hillshade index at 3pm, summer solstice
# Horizontal_Distance_To_Fire_Points      quantitative    meters                       Horz Dist to nearest wildfire ignition points
# Wilderness_Area (4 binary columns)      qualitative     0 (absence) or 1 (presence)  Wilderness area designation
# Soil_Type (40 binary columns)           qualitative     0 (absence) or 1 (presence)  Soil Type designation
# Cover_Type (7 types)                    integer         1 to 7                       Forest Cover Type designation
# 
# Code Designations:
#   
#   Wilderness Areas:  	1 -- Rawah Wilderness Area
# 2 -- Neota Wilderness Area
# 3 -- Comanche Peak Wilderness Area
# 4 -- Cache la Poudre Wilderness Area
# 
# Soil Types:             1 to 40 : based on the USFS Ecological
# Landtype Units (ELUs) for this study area:
#   
#   Study Code USFS ELU Code			Description
# 1	   2702	1	Cathedral family - Rock outcrop complex, extremely stony.
# 2	   2703	2	Vanet - Ratake families complex, very stony.
# 3	   2704	3	Haploborolis - Rock outcrop complex, rubbly.
# 4	   2705	4	Ratake family - Rock outcrop complex, rubbly.
# 5	   2706	2	Vanet family - Rock outcrop complex complex, rubbly.
# 6	   2717	2	Vanet - Wetmore families - Rock outcrop complex, stony.
# 7	   3501	5	Gothic family.
# 8	   3502	6	Supervisor - Limber families complex.
# 9	   4201	7	Troutville family, very stony.
# 10	 4703  8	Bullwark - Catamount families - Rock outcrop complex, rubbly.
# 11	   4704	8	Bullwark - Catamount families - Rock land complex, rubbly.
# 12	   4744	9	Legault family - Rock land complex, stony.
# 13	   4758	10	Catamount family - Rock land - Bullwark family complex, rubbly.
# 14	   5101	11	Pachic Argiborolis - Aquolis complex.
# 15	   5151	12	unspecified in the USFS Soil and ELU Survey.
# 16	   6101	13 	Cryaquolis - Cryoborolis complex.
# 17	   6102	14	Gateview family - Cryaquolis complex.
# 18	   6731	15	Rogert family, very stony.
# 19	   7101	13	Typic Cryaquolis - Borohemists complex.
# 20	   7102	14	Typic Cryaquepts - Typic Cryaquolls complex.
# 21	   7103 15		Typic Cryaquolls - Leighcan family, till substratum complex.
# 22	   7201	16	Leighcan family, till substratum, extremely bouldery.
# 23	   7202	16	Leighcan family, till substratum - Typic Cryaquolls complex.
# 24	   7700	16	Leighcan family, extremely stony.
# 25	   7701	16	Leighcan family, warm, extremely stony.
# 26	   7702	17	Granile - Catamount families complex, very stony.
# 27	   7709	16	Leighcan family, warm - Rock outcrop complex, extremely stony.
# 28	   7710	16	Leighcan family - Rock outcrop complex, extremely stony.
# 29	   7745	18	Como - Legault families complex, extremely stony.
# 30	   7746	18	Como family - Rock land - Legault family complex, extremely stony.
# 31	   7755	16	Leighcan - Catamount families complex, extremely stony.
# 32	   7756	10	Catamount family - Rock outcrop - Leighcan family complex, extremely stony.
# 33	   7757	16	Leighcan - Catamount families - Rock outcrop complex, extremely stony.
# 34	   7790	19	Cryorthents - Rock land complex, extremely stony.
# 35	   8703	20	Cryumbrepts - Rock outcrop - Cryaquepts complex.
# 36	   8707	21	Bross family - Rock land - Cryumbrepts complex, extremely stony.
# 37	   8708	20	Rock outcrop - Cryumbrepts - Cryorthents complex, extremely stony.
# 38	   8771	16	Leighcan - Moran families - Cryaquolls complex, extremely stony.
# 39	   8772	19	Moran family - Cryorthents - Leighcan family complex, extremely stony.
# 40	   8776	19	Moran family - Cryorthents - Rock land complex, extremely stony.
# 
# Note:   First digit:  climatic zone             Second digit:  geologic zones
# 1.  lower montane dry                   1.  alluvium
# 2.  lower montane                       2.  glacial
# 3.  montane dry                         3.  shale
# 4.  montane                             4.  sandstone
# 5.  montane dry and montane             5.  mixed sedimentary
# 6.  montane and subalpine               6.  unspecified in the USFS ELU Survey
# 7.  subalpine                           7.  igneous and metamorphic
# 8.  alpine                              8.  volcanic
# 
# The third and fourth ELU digits are unique to the mapping unit 
# and have no special meaning to the climatic or geologic zones.
# 
# Forest Cover Type Classes:	1 -- Spruce/Fir
# 2 -- Lodgepole Pine
# 3 -- Ponderosa Pine
# 4 -- Cottonwood/Willow
# 5 -- Aspen
# 6 -- Douglas-fir
# 7 -- Krummholz


#setwd('C://Users/Aravind/Documents/GitHub/bootcamp004_project/Project4-Machinelearning/Aravind_thomas')
#setwd('/Users/tkolasa/dev/nycdatascience/projects/bootcamp004_project/Project4-Machinelearning/Aravind_thomas')

library(ggplot2)
library(dplyr)
library(GGally)
library(clusterSim)
#library(VGAM)
library(caret)
library(glmnet)
library(tree)
library(randomForest)
library(gbm)
library(NISTunits)

forestdata = read.csv('train.csv', header=TRUE)
# View(forestdata)
# summary(forestdata[,2:11])

# adding a new variable for covernames based on cover type

forestdata$covername = 'a'
forestdata$covername[forestdata$Cover_Type==1] = 'Spruce-fir'
forestdata$covername[forestdata$Cover_Type==2] = 'Lodgepole Pine'
forestdata$covername[forestdata$Cover_Type==3] = 'Ponderosa Pine'
forestdata$covername[forestdata$Cover_Type==4] = 'Cottonwood-Willow'
forestdata$covername[forestdata$Cover_Type==5] = 'Aspen'
forestdata$covername[forestdata$Cover_Type==6] = 'Douglas-fir'
forestdata$covername[forestdata$Cover_Type==7] = 'Krummholz'

# Create single Wilderness_Area column
forestdata$Wilderness_Area = 0
for (i in 12:15) {
  forestdata$Wilderness_Area[forestdata[,i] == 1] = i-11  
}

#plotting the correlation matrix of the quantitative factors 
forestcorrelation=cor(forestdata[,2:11])

#creating mean of hillshade
forestdata$Hillshade_mean = (forestdata$Hillshade_9am + forestdata$Hillshade_3pm + forestdata$Hillshade_Noon) / 3


#creating distance to hydrology as a function of horizontal and vertical 
forestdata$Euclidean_Distance_To_Hydrology = (forestdata$Horizontal_Distance_To_Hydrology^2 + forestdata$Vertical_Distance_To_Hydrology^2)^.5

forestdata$Aspect[forestdata$Aspect == 360] = 0

rose_diagram_df = forestdata[c('Aspect', 'covername')]

rose_diagram_df['aspect_group'] = cut(rose_diagram_df$Aspect, breaks=c(-1,seq(20,360, by = 20)), labels=FALSE)

forestdata$aspect_group=rose_diagram_df$aspect_group
forestdata$aspect_group_shift= forestdata$aspect_group+3
forestdata$aspect_group_shift[forestdata$aspect_group_shift==19]= 1 
forestdata$aspect_group_shift[forestdata$aspect_group_shift==20]= 2 
forestdata$aspect_group_shift[forestdata$aspect_group_shift==21]= 3 

# Create single Soil_Type column
forestdata$Soil_Type = 0
for (i in 16:55) {
  forestdata$Soil_Type[forestdata[,i] == 1] = i-15  
}

#Create log Elevation 
forestdata$log_elevation=log(forestdata$Elevation)

#Squared Hillshades 
forestdata$Hillshade_9am_sq=forestdata$Hillshade_9am^2
forestdata$Hillshade_noon_sq=forestdata$Hillshade_Noon^2
forestdata$Hillshade_3pm_sq=forestdata$Hillshade_3pm^2

#Cosine of Slope
forestdata$cosine_slope=cos(NISTdegTOradian(forestdata$Slope))

#Whether aspect group shift is < 10 or not 
forestdata$aspect_group_class=as.numeric(forestdata$aspect_group_shift)

for (i in 1:nrow(forestdata)) {
  if (forestdata$aspect_group_class[i]>10) {
    forestdata$aspect_group_class[i]=0 
  } else {
     forestdata$aspect_group_class[i]=1
   }
}

forestdata$aspect_group_class=as.factor(forestdata$aspect_group_class)

# Group families 

soil_family=c(1, 2, 3, 4, 2, 2, 5, 6, 7, 8, 8, 9, 10, 11, 12, 13, 14, 15, 13, 14, 15, 16, 16, 16,
  16, 17, 16, 16, 18, 18, 16, 10, 16, 19, 20, 21, 20, 16, 19, 19)

forestdata$soil_family=as.numeric(forestdata$Soil_Type)

for (i in 1:nrow(forestdata)) {
forestdata$soil_family[i]=soil_family[forestdata$soil_family[i]]
}

forestdata$soil_family=as.factor(forestdata$soil_family)

rock_type=c('stony', 'stony', 'rubbly', 'rubbly', 'rubbly', 'stony', 'neither', 'neither', 'stony', 'rubbly', 'rubbly', 
  'stony', 'rubbly', 'neither', 'neither', 'neither', 'neither', 'stony', 'neither', 'neither', 'neither', 
  'stony', 'neither', 'stony', 'stony', 'stony', 'stony', 'stony', 'stony', 'stony', 'stony', 'stony', 
  'stony', 'stony', 'neither', 'stony', 'stony', 'stony', 'stony', 'stony')

rock_type_factor = c(1,2,3)
rock_type=rock_type_factor[as.factor(rock_type)]

#3 - stony, 1 -neither 2- rubbly 

forestdata$soil_rock_type=as.numeric(forestdata$Soil_Type)

for (i in 1:nrow(forestdata)) {
forestdata$soil_rock_type[i]=rock_type[forestdata$soil_rock_type[i]]
}

forestdata$soil_rock_type=as.factor(forestdata$soil_rock_type)
#interaction between hillshade noon and 9 am 

forestdata$interaction_9amnoon= forestdata$Hillshade_9am*forestdata$Hillshade_Noon
forestdata$interaction_noon3pm=forestdata$Hillshade_Noon*forestdata$Hillshade_3pm
forestdata$interaction_9am3pm= forestdata$Hillshade_9am*forestdata$Hillshade_3pm



#################### converting to factors ########################

forestdata$Soil_Type=as.factor(forestdata$Soil_Type)
forestdata$Wilderness_Area=as.factor(forestdata$Wilderness_Area)

forestdata$covername=as.factor(forestdata$covername)
forestdata$Cover_Type=as.factor(forestdata$Cover_Type)
forestdata$aspect_group=as.factor(forestdata$aspect_group)
forestdata$aspect_group_shift=as.factor(forestdata$aspect_group_shift)

forestdata1= forestdata[,-c(22,30)]

for(i in 12:53) {
  forestdata1[,i]= as.factor(forestdata1[,i])
}

temp=forestdata1[,1:54]
forestdata2=forestdata1
forestdata1=temp


# replicating the feature  creations with TEST data set 

foresttest = read.csv('test.csv', header = TRUE)
foresttest$Cover_Type = sample(1:7, nrow(foresttest), replace = TRUE)
testmodel=foresttest[,-c(22,30)]
for(i in 12:54) {
  testmodel[,i] = as.factor(testmodel[,i])
}

foresttest$Soil_Type = 0
for (i in 16:55) {
  foresttest$Soil_Type[foresttest[,i] == 1] = i-15  
}
foresttest$Wilderness_Area = 0
for (i in 12:15) {
  foresttest$Wilderness_Area[foresttest[,i] == 1] = i-11  
}

foresttest$Soil_Type = as.factor(foresttest$Soil_Type)
foresttest$Wilderness_Area = as.factor(foresttest$Wilderness_Area)
foresttest$Cover_Type = as.factor(foresttest$Cover_Type)

# adding a new variable for covernames based on cover type

foresttest$covername = 'a'
foresttest$covername[foresttest$Cover_Type==1] = 'Spruce-fir'
foresttest$covername[foresttest$Cover_Type==2] = 'Lodgepole Pine'
foresttest$covername[foresttest$Cover_Type==3] = 'Ponderosa Pine'
foresttest$covername[foresttest$Cover_Type==4] = 'Cottonwood-Willow'
foresttest$covername[foresttest$Cover_Type==5] = 'Aspen'
foresttest$covername[foresttest$Cover_Type==6] = 'Douglas-fir'
foresttest$covername[foresttest$Cover_Type==7] = 'Krummholz'


#creating mean of hillshade
foresttest$Hillshade_mean = (foresttest$Hillshade_9am + foresttest$Hillshade_3pm + foresttest$Hillshade_Noon) / 3


#creating distance to hydrology as a function of horizontal and vertical 
foresttest$Euclidean_Distance_To_Hydrology = (foresttest$Horizontal_Distance_To_Hydrology^2 + foresttest$Vertical_Distance_To_Hydrology^2)^.5

foresttest$Aspect[foresttest$Aspect == 360] = 0

rose_diagram_df = foresttest[c('Aspect', 'covername')]

rose_diagram_df['aspect_group'] = cut(rose_diagram_df$Aspect, breaks=c(-1,seq(20,360, by = 20)), labels=FALSE)

foresttest$aspect_group=rose_diagram_df$aspect_group
foresttest$aspect_group_shift= foresttest$aspect_group+3
foresttest$aspect_group_shift[foresttest$aspect_group_shift==19]= 1 
foresttest$aspect_group_shift[foresttest$aspect_group_shift==20]= 2 
foresttest$aspect_group_shift[foresttest$aspect_group_shift==21]= 3 


#Create log Elevation 
foresttest$log_elevation=log(foresttest$Elevation)

#Squared Hillshades 
foresttest$Hillshade_9am_sq=foresttest$Hillshade_9am^2
foresttest$Hillshade_noon_sq=foresttest$Hillshade_Noon^2
foresttest$Hillshade_3pm_sq=foresttest$Hillshade_3pm^2

#Cosine of Slope
foresttest$cosine_slope=cos(NISTdegTOradian(foresttest$Slope))

#Whether aspect group shift is < 10 or not 
foresttest$aspect_group_class=as.numeric(foresttest$aspect_group_shift)
foresttest$aspect_group_class=0

foresttest$aspect_group_class[foresttest$aspect_group_shift >10]=1

foresttest$aspect_group_class=as.factor(foresttest$aspect_group_class)

# Group families 

soil_family=c(1, 2, 3, 4, 2, 2, 5, 6, 7, 8, 8, 9, 10, 11, 12, 13, 14, 15, 13, 14, 15, 16, 16, 16,
              16, 17, 16, 16, 18, 18, 16, 10, 16, 19, 20, 21, 20, 16, 19, 19)

foresttest$soil_family=as.numeric(foresttest$Soil_Type)

foresttest$soil_family=soil_family[foresttest$soil_family]

foresttest$soil_family=as.factor(foresttest$soil_family)

rock_type=c('stony', 'stony', 'rubbly', 'rubbly', 'rubbly', 'stony', 'neither', 'neither', 'stony', 'rubbly', 'rubbly', 
            'stony', 'rubbly', 'neither', 'neither', 'neither', 'neither', 'stony', 'neither', 'neither', 'neither', 
            'stony', 'neither', 'stony', 'stony', 'stony', 'stony', 'stony', 'stony', 'stony', 'stony', 'stony', 
            'stony', 'stony', 'neither', 'stony', 'stony', 'stony', 'stony', 'stony')

rock_type_factor = c(1,2,3)
rock_type=rock_type_factor[as.factor(rock_type)]

#3 - stony, 1 -neither 2- rubbly 

foresttest$soil_rock_type=as.numeric(foresttest$Soil_Type)

foresttest$soil_rock_type=rock_type[foresttest$Soil_Type]

foresttest$soil_rock_type=as.factor(foresttest$soil_rock_type)
#interaction between hillshade noon and 9 am 

foresttest$interaction_9amnoon= foresttest$Hillshade_9am*foresttest$Hillshade_Noon
foresttest$interaction_noon3pm=foresttest$Hillshade_Noon*foresttest$Hillshade_3pm
foresttest$interaction_9am3pm= foresttest$Hillshade_9am*foresttest$Hillshade_3pm


xfactors <- model.matrix(forestdata$Cover_Type ~ forestdata$Elevation +
                           forestdata$Aspect +
                           forestdata$Slope +
                           forestdata$Horizontal_Distance_To_Hydrology +
                           forestdata$Vertical_Distance_To_Hydrology +
                           forestdata$Horizontal_Distance_To_Roadways +
                           forestdata$Hillshade_9am +
                           forestdata$Hillshade_Noon +
                           forestdata$Hillshade_3pm +
                           forestdata$Horizontal_Distance_To_Fire_Points +
                           forestdata$Wilderness_Area +                  #removes the first area
                           forestdata$Wilderness_Area1 +
                           forestdata$Horizontal_Distance_To_Fire_Points +
                           forestdata$Soil_Type +
                           forestdata$Soil_Type1)[,-1]
x=as.matrix(xfactors)


set.seed(0)
train = sample(1:nrow(x), 85*nrow(x)/100)
test = (-train)

y = forestdata$Cover_Type
y.test = y[test]


xfactorstest <- model.matrix(foresttest$Cover_Type ~ foresttest$Elevation +
                               foresttest$Aspect +
                               foresttest$Slope +
                               foresttest$Horizontal_Distance_To_Hydrology +
                               foresttest$Vertical_Distance_To_Hydrology +
                               foresttest$Horizontal_Distance_To_Roadways +
                               foresttest$Hillshade_9am +
                               foresttest$Hillshade_Noon +
                               foresttest$Hillshade_3pm +
                               foresttest$Horizontal_Distance_To_Fire_Points +
                               foresttest$Wilderness_Area +
                               foresttest$Wilderness_Area1 +
                               foresttest$Soil_Type +
                               foresttest$Soil_Type1)[,-1]
xtest=as.matrix(xfactorstest)

xtest = xtest[, -c(20, 28)]


foresttest1= foresttest[,-c(22,30)]


foresttest1$Soil_Type = 0
for (i in 12:53) {
  foresttest1[,i]=as.factor(foresttest1[,i])
}


foresttest$Soil_Type=as.factor(foresttest$Soil_Type)
foresttest$Wilderness_Area=as.factor(foresttest$Wilderness_Area)

foresttest$covername=as.factor(foresttest$covername)
foresttest$Cover_Type=as.factor(foresttest$Cover_Type)
foresttest$aspect_group=as.factor(foresttest$aspect_group)
foresttest$aspect_group_shift=as.factor(foresttest$aspect_group_shift)
foresttest$soil_family=as.factor(foresttest$soil_family)
foresttest$aspect_group_class=as.factor(foresttest$aspect_group_class)


foresttest1$Soil_Type=as.factor(foresttest1$Soil_Type)
foresttest1$Wilderness_Area=as.factor(foresttest1$Wilderness_Area)

foresttest1$soil_family=as.factor(foresttest1$soil_family)
foresttest1$covername=as.factor(foresttest1$covername)
foresttest1$Cover_Type=as.factor(foresttest1$Cover_Type)
foresttest1$aspect_group=as.factor(foresttest1$aspect_group)
foresttest1$aspect_group_shift=as.factor(foresttest1$aspect_group_shift)
foresttest1$aspect_group_class=as.factor(foresttest1$aspect_group_class)






#-------------------------  Exploratory Visuals ------------------------------------------------#


# Studying Cover name
ggplot(forestdata, aes(x=covername)) + geom_bar(aes(group=covername, colour=covername, fill=covername), alpha=0.3)+ggtitle('T')

# Training set has an equal number of observations for each cover type


# Studying Elevation distribution and densities across Cover type
ggplot(forestdata, aes(x=Elevation)) + geom_histogram(aes(group=covername, colour=covername, fill=covername), alpha=0.3)+ggtitle('T')
ggplot(forestdata, aes(x=Elevation)) + geom_density()
ggplot(forestdata, aes(x=Elevation)) + geom_density(aes(group=covername, colour=covername, fill=covername), alpha=0.3)

# Studying Aspect across cover type
ggplot(forestdata, aes(x=Aspect)) + geom_histogram(aes(group=covername, colour=covername, fill=covername), alpha=0.3)+ggtitle('T')
ggplot(forestdata, aes(x=Aspect)) + geom_density()
ggplot(forestdata, aes(x=Aspect)) + geom_density(aes(group=covername, colour=covername, fill=covername), alpha=0.3)
# Rose diagram
# make Aspect of 360 = 0

ggplot(forestdata, aes(x=aspect_group_shift, fill=covername )) +
  geom_bar() +
  coord_polar()

ggplot(rose_diagram_df, aes(x=aspect_group, fill=covername) ) +
  geom_bar() +
  coord_polar()

ggplot(forestdata, aes(x=aspect_group_shift, fill=Wilderness_Area )) +
  geom_bar() +
  coord_polar()


#The aspect value is higher across once side of the direction of slope,
#however, at first glance , the covernames seem equitably distributed

# Studying Slope
ggplot(forestdata, aes(x=Slope)) + geom_histogram(aes(group=covername, colour=covername, fill=covername), alpha=0.3)+ggtitle('T')
ggplot(forestdata, aes(x=Slope)) + geom_density()
ggplot(forestdata, aes(x=Slope)) + geom_density(aes(group=covername, colour=covername, fill=covername), alpha=0.3)

# The slope densities across covertypes do not seem to segregate them 
# as much, it  does not seem like an important predictor of Cover type


# Studying Distance to Roadways
ggplot(forestdata, aes(x=Horizontal_Distance_To_Roadways)) + geom_histogram(aes(group=covername, colour=covername, fill=covername), alpha=0.3)+ggtitle('T')
ggplot(forestdata, aes(x=Horizontal_Distance_To_Roadways)) + geom_density()
ggplot(forestdata, aes(x=Horizontal_Distance_To_Roadways)) + geom_density(aes(group=covername, colour=covername, fill=covername), alpha=0.3)

# Hillshade average
ggplot(forestdata, aes(x=Hillshade_mean)) + geom_histogram(aes(group=covername, colour=covername, fill=covername), alpha=0.3)+ggtitle('T')
ggplot(forestdata, aes(x=Hillshade_mean)) + geom_density()
ggplot(forestdata, aes(x=Hillshade_mean)) + geom_density(aes(group=covername, colour=covername, fill=covername), alpha=0.3)
# Hillshade 9AM
ggplot(forestdata, aes(x=Hillshade_9am)) + geom_histogram(aes(group=covername, colour=covername, fill=covername), alpha=0.3)+ggtitle('T')
ggplot(forestdata, aes(x=Hillshade_9am)) + geom_density()
ggplot(forestdata, aes(x=Hillshade_9am)) + geom_density(aes(group=covername, colour=covername, fill=covername), alpha=0.3)
# Hillshade 3PM
ggplot(forestdata, aes(x=Hillshade_3pm)) + geom_histogram(aes(group=covername, colour=covername, fill=covername), alpha=0.3)+ggtitle('T')
ggplot(forestdata, aes(x=Hillshade_3pm)) + geom_density()
ggplot(forestdata, aes(x=Hillshade_3pm)) + geom_density(aes(group=covername, colour=covername, fill=covername), alpha=0.3)
# Hillshade noon
ggplot(forestdata, aes(x=Hillshade_Noon)) + geom_histogram(aes(group=covername, colour=covername, fill=covername), alpha=0.3)+ggtitle('T')
ggplot(forestdata, aes(x=Hillshade_Noon)) + geom_density()
ggplot(forestdata, aes(x=Hillshade_Noon)) + geom_density(aes(group=covername, colour=covername, fill=covername), alpha=0.3)


# Distance to Fire points
ggplot(forestdata, aes(x=Horizontal_Distance_To_Fire_Points)) + geom_histogram(aes(group=covername, colour=covername, fill=covername), alpha=0.3)+ggtitle('T')
ggplot(forestdata, aes(x=Horizontal_Distance_To_Fire_Points)) + geom_density()
ggplot(forestdata, aes(x=Horizontal_Distance_To_Fire_Points)) + geom_density(aes(group=covername, colour=covername, fill=covername), alpha=0.3)

# Hydrology
# Horizontal distance to Hydrology
ggplot(forestdata, aes(x=Horizontal_Distance_To_Hydrology)) + geom_histogram(aes(group=covername, colour=covername, fill=covername), alpha=0.3)+ggtitle('T')
ggplot(forestdata, aes(x=Horizontal_Distance_To_Hydrology)) + geom_density()
ggplot(forestdata, aes(x=Horizontal_Distance_To_Hydrology)) + geom_density(aes(group=covername, colour=covername, fill=covername), alpha=0.3)

# Vertical distance to Hydrology
ggplot(forestdata, aes(x=Vertical_Distance_To_Hydrology)) + geom_histogram(aes(group=covername, colour=covername, fill=covername), alpha=0.3)+ggtitle('T')
ggplot(forestdata, aes(x=Vertical_Distance_To_Hydrology)) + geom_density()
ggplot(forestdata, aes(x=Vertical_Distance_To_Hydrology)) + geom_density(aes(group=covername, colour=covername, fill=covername), alpha=0.3)

# Euclidean Distance from Hydrology
ggplot(forestdata, aes(x=Euclidean_Distance_To_Hydrology)) + geom_histogram(aes(group=covername, colour=covername, fill=covername), alpha=0.3)+ggtitle('T')
ggplot(forestdata, aes(x=Euclidean_Distance_To_Hydrology)) + geom_density()
ggplot(forestdata, aes(x=Euclidean_Distance_To_Hydrology)) + geom_density(aes(group=covername, colour=covername, fill=covername), alpha=0.3)
# Since Horizontal distance is so much larger, these looks basically the same as the horizontal distance charts

# Studying Wilderness Area
ggplot(forestdata, aes(x=Wilderness_Area)) + geom_histogram(aes(group=covername, colour=covername, fill=covername), alpha=0.3)+ggtitle('T')

# Studying Soil Type
ggplot(forestdata, aes(x=Soil_Type)) + geom_histogram(aes(group=covername, colour=covername, fill=covername), alpha=0.3)+ggtitle('T')


# Correlations
ggpairs(data = forestdata, columns = 8:10, title = "Correlations", mapping = aes(colour = Cover_Type, alpha = .3))

#By Wilderness Area
ggpairs(data = forestdata, columns = 8:10, title = "Correlations", mapping = aes(colour = Wilderness_Area, alpha = .3))

ggpairs(data = forestdata, columns = c(5,6,7,11), title = "Correlations", mapping = aes(colour = Wilderness_Area, alpha = .3))



# all 

ggpairs(data = forestdata, columns = 2:11, title = "Correlations", mapping = aes(colour = Cover_Type, alpha = .3))

#3d Scatter plot

set.seed(100)

library(plotly)
plot_ly(forestdata, x = Hillshade_9am, y = Hillshade_Noon, z = Hillshade_3pm, type = "scatter3d", mode = "markers",color = Cover_Type)

plot_ly(forestdata, x =Elevation, y = Slope, z = Hillshade_9am, type = "scatter3d", mode = "markers",color = Cover_Type)

plot_ly(forestdata, x =Hillshade_3pm, y = cosine_slope, z = Hillshade_9am, type = "scatter3d", mode = "markers",color = Cover_Type)









# K means clustering of the data


wssplot = function(data, nc = 15, seed = 0) {
  wss = (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] = sum(kmeans(data, centers = i, iter.max = 10, nstart = 5)$withinss)
  }
  plot(1:nc, wss, type = "b",
       xlab = "Number of Clusters",
       ylab = "Within-Cluster Variance",
       main = "Scree Plot for the K-Means Procedure")
}


forestdatanorm<-data.frame(scale(forestdata[,-c(1,12:58)]))
#forestdatanorm=subset(forestdatanorm, select=-c(Soil_Type7,Soil_Type15))

View(forestdatanorm)
fit7 <- kmeans(forestdatanorm, 7,nstart=10) 
fit4 <- kmeans(forestdatanorm, 4,nstart=10)

wssplot(forestdatanorm)

#  Daavies bouldin index not working !
# cl1 <- pam(forestdatanorm, 4)
# distancekmeans<-dist(data_ratio)
# print(index.DB(forestdatanorm, cl1$clustering,distancekmeans, centrotypes="medoids"))


table7clusters=table(fit7$cluster,forestdata$covername)
table4clusters=table(fit4$cluster,forestdata$covername)
chisq.test(table4clusters)
chisq.test(table7clusters)





# Functions to save model details
summary_table = data.frame()
modelsummary = function(confusion_matrix, table, model_name, kaggle_score, kaggle_rank) {
  # Creates summary statistics for each model
  temp = as.data.frame(colMeans(confusion_matrix$byClass))
  model_names = t(as.data.frame(c(model_name, kaggle_score, kaggle_rank)))
  colnames(model_names) = c('model', 'kaggle_score', 'kaggle_rank')
  k = cbind(model_names, t(confusion_matrix$overall), t(temp))
  table = rbind(table,k)
  rownames(table) = 1:nrow(table)
  return(table)
}

fitted_class_df = data.frame(1:15120)
append_fitted_class = function(fitted_classes, table, model_name) {
  # adds a column with the fitted classifications for each model
  temp = as.data.frame(fitted_classes)
  colnames(temp) = model_name
  table = cbind(table, temp)
  if (colnames(table)[1] == "X1.15120") {
    table = table[-1]
  }
  return(table)
}

######### Modeling ################
# LOGISTIC REGRESSION USING GLMnet 


grid = 10^seq(5, -7, length = 100)



# Ridge multinomial logistic regression
glmmod.cv.ridge <- cv.glmnet(x[train,],y[train],alpha=0,family='multinomial', nfolds = 10, lambda = grid)
plot(glmmod.cv.ridge)
best_lambda_ridge = glmmod.cv.ridge$lambda.min

# save fitted classifications on the training set for later
glmmod_best_lambda_ridge_training = predict(glmmod.cv.ridge, type = "class", s = best_lambda_ridge, x)

glmmod_best_lambda_ridge = predict(glmmod.cv.ridge, type = "class", s = best_lambda_ridge, x[test,])
glmmod_best_lambda_ridge = as.data.frame(glmmod_best_lambda_ridge)

confusionMatrix(glmmod_best_lambda_ridge[,1], y.test, positive = '1')
# accuracy = 0.6629 for our test set in train.csv for 70-30 CV and default lambda
# accuracy = 0.709 for our test set in train.csv for 85-15 CV and lambda from broader grid
coef(glmmod.cv.ridge)



# Lasso multinomial logistic regression
glmmod.cv.lasso <- cv.glmnet(x[train,],y[train],alpha=1,family='multinomial', nfolds = 10)
plot(glmmod.cv.lasso)
best_lambda_lasso = glmmod.cv.lasso$lambda.min

glmmod_best_lambda_lasso = predict(glmmod.cv.lasso, type = "class", s = best_lambda_lasso, x[test,])
glmmod_best_lambda_lasso = as.data.frame(glmmod_best_lambda_lasso)
confusionMatrix(glmmod_best_lambda_lasso[,1], y.test, positive = '1')
# accuracy = 0.7099 within the test set in train.csv
# accuracy = 0.7099 for our test set in train.csv for 85-15 CV
coef(glmmod.cv.lasso)


# Class 7 predicted with higher elevations, class 4 predicted with lower elevations, as expected
# Wilderness_Area1 not in the model


# elastic multinomial logistic regression
glmmod.cv.elastic <- cv.glmnet(x[train,],y[train],alpha=0.5,family='multinomial', nfolds = 10)
plot(glmmod.cv.elastic)
best_lambda_elastic = glmmod.cv.elastic$lambda.min

glmmod_best_lambda_elastic = predict(glmmod.cv.elastic, type = "class", s = best_lambda_elastic, x[test,])
glmmod_best_lambda_elastic = as.data.frame(glmmod_best_lambda_elastic)
confusionMatrix(glmmod_best_lambda_elastic[,1], y.test, positive = '1') 
# accuracy = 0.7108 within the test set in train.csv
coef(glmmod.cv.elastic)



# Run on test set ####

glmmod_best_lambda = predict(glmmod.cv, type = "class", s = best_lambda, as.matrix(foresttest))
glmmod_best_lambda = as.data.frame(glmmod_best_lambda)
confusionMatrix(glmmod_best_lambda[,1], y.test, positive = '1')


glmmod_best_lambda_ridge_test = predict(glmmod.cv.ridge, type = "class", s = best_lambda_ridge, xtest)
glmmod_best_lambda_ridge_test = as.data.frame(glgmmod_best_lambda_ridge_test)

glmmod_best_lambda_ridge_test
ridge_submission1 = foresttest[,c(1,56)]
ridge_submission1$Cover_Type = glmmod_best_lambda_ridge_test[,1]

write.csv(ridge_submission1, 'ridge2_submission2.csv', row.names = FALSE)

# kaggle score = 0.55696, 1489th place for 70-30 split
# kaggle score = 0.59550, 1414th place for 85-15 split
# 


k = as.data.frame(1:565892)
for (i in 1:99) {
  print(i)
  temp = predict(glmmod.cv.lasso, type = "class", s = glmmod.cv.lasso$lambda[i], xtest)
  temp = as.data.frame(temp)
  colnames(temp) = as.character(glmmod.cv.lasso$lambda[i])
  k = cbind(k, temp)
}
#write.csv(k, 'lasso_lambda_blend.csv', row.names = FALSE)
library(modeest)
apply(as.numeric(k[1,]), 1, mfv)
modes = 1:565892
for (i in 1:nrow(k)) {
  modes[i] = mfv(as.numeric(k[i,]))
}


#glmmod_best_lambda_lasso_test = predict(glmmod.cv.lasso, type = "class", s = best_lambda_lasso, xtest)

glmmod_best_lambda_lasso_test = as.data.frame(glmmod_best_lambda_lasso_test)

lasso_submission1 = foresttest[,c(1,56)]
lasso_submission1$Cover_Type = glmmod_best_lambda_lasso_test[,1]

write.csv(lasso_submission1, 'lasso_submission2.csv', row.names = FALSE)
# kaggle score = 0.59594, 1411th place for 70-30 split
# kaggle score = 0.59526, 1415th place for 85-15 split
# kaggle score = 0.59443 if we took the lowest 50 percintile of lambdas and calculated the mode of the predictions

glmmod_best_lambda_elastic_test = predict(glmmod.cv.lasso, type = "class", s = best_lambda_elastic, xtest)
glmmod_best_lambda_elastic_test = as.data.frame(glmmod_best_lambda_elastic_test)

elastic_submission2 = foresttest[,c(1,56)]
elastic_submission2$Cover_Type = glmmod_best_lambda_elastic_test[,1]

write.csv(elastic_submission2, 'elastic_submission2.csv', row.names = FALSE)
# kaggle score = 0.59588, 1412th place for 70-30 split
# kaggle score = 0.59520, 1415th place for 85-15 split

# using All the 15120 for the training :
glmmod.cv.elastic_all <- cv.glmnet(x,y,alpha=0.5,family='multinomial', nfolds = 10)
plot(glmmod.cv.elastic_all)
best_lambda_elastic_all = glmmod.cv.elastic_all$lambda.min

glmmod_best_lambda_elastic_test_all = predict(glmmod.cv.elastic_all, type = "class", s = best_lambda_elastic_all, xtest)
glmmod_best_lambda_elastic_test_all = as.data.frame(glmmod_best_lambda_elastic_test_all)

elastic_submission3 = foresttest[,c(1,56)]
elastic_submission3$Cover_Type = glmmod_best_lambda_elastic_test_all[,1]

write.csv(elastic_submission3, 'elastic_submission3.csv', row.names = FALSE) 
# kaggle score = 0.59496, 1417th place





# DECISION TREES

foresttrain = sample(1:nrow(forestdata1), 8*nrow(forestdata1)/10)
forestdata1.train = forestdata1[foresttrain, ]
forestdata1.test = forestdata1[-foresttrain, ]

tree.forestdata= tree(Cover_Type ~ . - Id, split = "gini", data = forestdata1, subset = foresttrain) #max depth reached?

#Fitting an initial random forest to the training subset.
set.seed(0)
rf.forestdata1 = randomForest(Cover_Type ~ . - Id, data = forestdata1, subset = foresttrain, importance = TRUE, ntree=500)
rf.forestdata1 = randomForest(Cover_Type ~ . - Id, data = forestdata1, importance = TRUE, ntree=500) # subset

rf.initialpred = predict(rf.forestdata1, forestdata1.test, type="class")
confusionMatrix(rf.initialpred, forestdata1.test[,54], positive='1')

importance(rf.forestdata1)
varImpPlot(rf.forestdata1)

#final testing 

rf.initialpred_test = predict(rf.forestdata1, testmodel, test, type = "class")
rf.initialpred_test = as.data.frame(rf.initialpred_test)

initialrfsubmission = foresttest[,c(1,56)]
initialrfsubmission$Cover_Type = rf.initialpred_test[,1]

write.csv(initialrfsubmission, 'rf_submission2.csv', row.names = FALSE)


# rf_submission1.csv Kaggle score 0.68932 with basic random forest , Rank  1286
# rf_submission2.csv Kaggle score of 0.70748 for rf of entire train set. Rank 1210

# Random forest with cross validation 

rf.cv1=rfcv(forestdata1[,1:53], forestdata1[,54], cv.fold=5)
rf.cvtest = predict(rf.cv1, testmodel, test, type = "class")
rf.initialpred_test = as.data.frame(rf.initialpred_test)

initialrfsubmission = foresttest[,c(1,56)]
initialrfsubmission$Cover_Type = rf.initialpred_test[,1]

write.csv(initialrfsubmission, 'rf_submission2.csv', row.names = FALSE)

importance(rf.forestdata1)
varImpPlot(rf.forestdata1)

confusionMatrix(as.vector(rf.cv1$predicted), forestdata1[,54], positive='1')


# --------------Tuning the random forest ------


#varying the number of parameters randomforest

accuracy = numeric(30)
for (mtry in 1:30) {
  fit = randomForest(Cover_Type ~ . - Id, data = forestdata1, subset=foresttrain,importance = TRUE, ntree=100, mtry = mtry)
  fit.pred = predict(fit, forestdata1.test, type="class")
  accuracy[mtry]=confusionMatrix(fit.pred, forestdata1.test[,54], positive='1')$overall[1]
  cat("We're performing iteration", mtry, "\n")
}

#Visualizing the OOB error.
ggplot()
plot(1:30, accuracy, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "accuracy",
     main = "Random Forest changing Variables")

dev.off()

accuracy=as.data.frame(accuracy)
accuracy$Variables=1:30
ggplot(data=accuracy,
       aes(x=Variables, y=accuracy, colour="red")) +
  geom_line()

ggplotly()



#finally choosing mtry=13 and ntree=500 on the whole dataset based on the accuracy plot 

rf.tuned = randomForest(Cover_Type ~ . - Id , data = forestdata1, importance = TRUE, ntree=500,mtry=13) # subset
importance(rf.tuned)
varImpPlot(rf.tuned)

#final testing 

rf.tunedtest = predict(rf.tuned, testmodel, type = "class")
rf.tunedtest = as.data.frame(rf.tunedtest)

tunedrandomforest1 = foresttest[,c(1,56)]
tunedrandomforest1$Cover_Type = rf.tunedtest[,1]

write.csv(tunedrandomforest1, 'rf_tunedtomtry13.csv', row.names = FALSE)
# This improved the accuracy of Kaggle test set to 0.75168 Rank 792.


rf.tuned17 = randomForest(Cover_Type ~ . - Id , data = forestdata1, importance = TRUE, ntree=500,mtry=17) # subset
importance(rf.tuned17)
varImpPlot(rf.tuned17)

#final testing 

rf.tunedtest = predict(rf.tuned17, testmodel, type = "class")
rf.tunedtest = as.data.frame(rf.tunedtest)

tunedrandomforest1 = foresttest[,c(1,56)]
tunedrandomforest1$Cover_Type = rf.tunedtest[,1]

write.csv(tunedrandomforest1, 'rf_tunedtomtry17.csv', row.names = FALSE)

# This marginally improves Kaggle score to 0.754 Rank 672





# After feature engineering -tuning again
foresttrain2 = sample(1:nrow(forestdata2), 8*nrow(forestdata2)/10)
forestdata2.train = forestdata2[foresttrain2, ]
forestdata2.test = forestdata2[-foresttrain2, ]


accuracy2 = numeric(30)
for (mtry in 1:30) {
  fit1 = randomForest(Cover_Type ~ . - Id- covername -Wilderness_Area, data = forestdata2[,-c(61,68,67,69)], subset=foresttrain2,importance = TRUE, ntree=100, mtry = mtry)
  fit1.pred = predict(fit1, forestdata2.test, type="class")
  accuracy2[mtry]=confusionMatrix(fit1.pred, forestdata1.test[,54], positive='1')$overall[1]
  cat("We're performing iteration", mtry, "\n")
}

#Visualizing the OOB error.
plot(1:30, accuracy2, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "accuracy2",
     main = "Random Forest changing Variables")





rf_feature = randomForest(Cover_Type ~ . - Id -covername -Wilderness_Area, data = forestdata2[,-c(61,68)], importance = TRUE, ntree=500,mtry=13) # subset
importance(rf_feature)
varImpPlot(rf_feature)

#final testing 



rf.feature.test = predict(rf_feature,foresttest1[,-c(55,68)], type = "class")
rf.feature.test = as.data.frame(rf.feature.test)

featurerandomforest1 = foresttest[,c(1,56)]
featurerandomforest1$Cover_Type = rf.feature.test[,1]

write.csv(featurerandomforest1, 'rf_featureengineering13.csv', row.names = FALSE)

# this resulted only in 0.69115




rf_feature20 = randomForest(Cover_Type ~ . - Id -covername -Wilderness_Area, data = forestdata2[,-c(61,68)], importance = TRUE, ntree=500,mtry=20) # subset
importance(rf_feature20)
varImpPlot(rf_feature20)

#final testing 



rf.feature.test20 = predict(rf_feature20,foresttest1[,-c(55,68)], type = "class")
rf.feature.test20 = as.data.frame(rf.feature.test20)

featurerandomforest20 = foresttest[,c(1,56)]
featurerandomforest20$Cover_Type = rf.feature.test20[,1]

write.csv(featurerandomforest20, 'rf_featureengineering20.csv', row.names = FALSE)

# this resulted  in improvement to 0.71929 




rf_feature30 = randomForest(Cover_Type ~ . - Id -covername -Wilderness_Area, data = forestdata2[,-c(61,68,67,69)], importance = TRUE, ntree=500,mtry=30) # subset
importance(rf_feature30)
varImpPlot(rf_feature30)

#final testing 



rf.feature.test30 = predict(rf_feature30,foresttest1[,-c(55,68,67,69)], type = "class")
rf.feature.test30 = as.data.frame(rf.feature.test30)

featurerandomforest30 = foresttest[,c(1,56)]
featurerandomforest30$Cover_Type = rf.feature.test30[,1]

write.csv(featurerandomforest30, 'rf_featureengineering30.csv', row.names = FALSE)











# Boosting

boost.forestdata1 = gbm( Cover_Type~ . -Id, data = forestdata1.train,
                   distribution = "multinomial",
                   n.trees = 1000,
                   interaction.depth = 3)

boostsummary = summary(boost.forestdata1)
boostsummary[boostsummary$rel.inf > 0,]

boost.initialpred = predict(boost.forestdata1, forestdata1.test, n.trees=1000, type='response')

boost.initialpred = apply(boost.initialpred, 1, which.max)
confusionMatrix(boost.initialpred, forestdata1.test[,54], positive='1')

importance(rf.forestdata1)
varImpPlot(rf.forestdata1)

apply(boost.initialpred, 1, which.max)
nrow(forestdata1.test)


# Extra Trees 

library(extraTrees)

et <- extraTrees(x,y, mtry=13,numRandomCuts = 2,nodesize = 3,numThreads = 3)
yhat <- predict(et, xtest)

# Error in .jarray(m) : java.lang.OutOfMemoryError: Java heap space! not working






# ARTIFICIAL NEURAL NETWORKS

library(nnet)
library(neuralnet)

# scale only quantitative variables
forestdata1.train.scaled = cbind( scale(forestdata1.train[,2:11]), forestdata1.train[,12:54])
forestdata1.test.scaled = cbind( scale(forestdata1.test[,2:11]), forestdata1.test[,12:54])

foresttestnn = foresttest
foresttestnn = foresttestnn[, -c(22, 30)]

forestdata1.scaled = cbind( scale(forestdata1[,2:11]), forestdata1[,12:54])
foresttest.scaled = cbind( scale(foresttestnn[,2:11]), foresttestnn[,12:54])

nnet.pred1 = nnet(forestdata1.train.scaled[,1:52], class.ind(forestdata1.train.scaled[,53]), size=10, softmax=TRUE)
nnet.pred1summary = predict(nnet.pred1,forestdata1.test.scaled[,1:52], type="class")
confusionMatrix(nnet.pred1summary, forestdata1.test.scaled[,53], positive='1')


nnet.pred2= nnet(forestdata1.train.scaled[,1:52], class.ind(forestdata1.train.scaled[,53]), size=25, softmax=TRUE,MaxNWts=2000,maxit=300)
nnet.pred2summary=predict(nnet.pred2, forestdata1.test.scaled[,1:52], type="class")
confusionMatrix(nnet.pred2summary, forestdata1.test.scaled[,53], positive='1')
# .8181 for maxit=300
# .8194 for maxit=500

# run on all train
nnet.pred2.all = nnet(forestdata1.scaled[,1:52], class.ind(forestdata1.scaled[,53]), size=25, softmax=TRUE,MaxNWts=2000,maxit=300)
nnet.pred2summary.all=predict(nnet.pred2, foresttest.scaled[,1:52], type="class")

nnet.pred2summary.all = as.data.frame(nnet.pred2summary.all)

initialnnetsubmission = foresttest[,c(1,56)]
initialnnetsubmission$Cover_Type = nnet.pred2summary.all[,1]

write.csv(initialnnetsubmission, 'nnet_submission2.csv', row.names = FALSE)
# kaggle score = 0.50867 then .51487, then 0.52113/0.52432 for 80% of training




nnet.pred3= nnet(forestdata1.train.scaled[,1:52], class.ind(forestdata1.train.scaled[,53]), size=40, softmax=TRUE,MaxNWts=3000,maxit=500)
nnet.pred3summary=predict(nnet.pred3,forestdata1.test.scaled[,1:52], type="class")
confusionMatrix(nnet.pred3summary, forestdata1.test.scaled[,53], positive='1')


# Trying neuralnet package - 
set.seed(0)

forestdata1.train.scaled = cbind( scale(forestdata1.train[,2:11]), forestdata1.train[,12:54])
forestdata1.test.scaled = cbind( scale(forestdata1.test[,2:11]), forestdata1.test[,12:54])

forestdata1.train.scaled = cbind( scale(forestdata1.train[,2:11]), forestdata1.train[,12:54])
forestdata1.test.scaled = cbind( scale(forestdata1.test[,2:11]), forestdata1.test[,12:54])


forestdata1.train.scaled = as.data.frame(forestdata1.train.scaled)
forestdata1.test.scaled = as.data.frame(forestdata1.test.scaled)

string1 = paste(colnames(forestdata1.train.scaled), collapse='+')
string1 = paste("~", string1)
formula1 = as.formula(string1)

forestdata1.train.scaled.nnet = model.matrix(formula1, data=forestdata1.train.scaled)
forestdata1.test.scaled.nnet = model.matrix(formula1, data=forestdata1.test.scaled)

forestdata1.train.scaled.nnet = cbind(forestdata1.train.scaled.nnet, rep(0, dim(forestdata1.train.scaled.nnet)[1]))
colnames(forestdata1.train.scaled.nnet)[60] = 'Cover_Type1'
# Add Cover_Type1 column
for (i in 1:dim(forestdata1.train.scaled.nnet)[1]) {
  if (sum(forestdata1.train.scaled.nnet[i,54:59]) == 0) {
      forestdata1.train.scaled.nnet[i,60] = 1
    }
}

forestdata1.test.scaled.nnet = cbind(forestdata1.test.scaled.nnet, rep(0, dim(forestdata1.test.scaled.nnet)[1]))
colnames(forestdata1.test.scaled.nnet)[60] = 'Cover_Type1'
# Add Cover_Type1 column
for (i in 1:dim(forestdata1.test.scaled.nnet)[1]) {
  if (sum(forestdata1.test.scaled.nnet[i,54:59]) == 0) {
    forestdata1.test.scaled.nnet[i,60] = 1
  }
}


#forestdata1.train.scaled.nnet = as.data.frame(forestdata1.train.scaled.nnet)
#forestdata1.train.scaled.nnet = cbind(forestdata1.train.scaled.nnet[,2:53], forestdata1.train.scaled[,53])

string2 = paste(colnames(forestdata1.train.scaled.nnet[,2:53]), collapse='+')
string2 = paste("Cover_Type1+Cover_Type2+Cover_Type3+Cover_Type4+Cover_Type5+Cover_Type6+Cover_Type7~", string2)
formula2 = as.formula(string2)


neuralnet.pred2 = neuralnet(formula2, hidden = c(5), data = forestdata1.train.scaled.nnet, linear.output = FALSE)
plot(neuralnet.pred2)


neuralnet.pred2.test = compute(neuralnet.pred2, forestdata1.test.scaled.nnet[,2:53])
apply(neuralnet.pred2.test, 1, which.max)




#Evaluating the model performance on the test set.
cor(predicted_strength3, concrete_test$strength)
plot(predicted_strength3, concrete_test$strength)

