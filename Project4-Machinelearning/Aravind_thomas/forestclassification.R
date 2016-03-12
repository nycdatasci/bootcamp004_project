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
nnet.pred2.all = nnet(forestdata1.scaled[,1:52], class.ind(forestdata1.scaled[,53]), size=100, softmax=TRUE,MaxNWts=4000,maxit=400)
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



# Tuning Neural Networks





#-------------------------------- XGBOOST -------------------------------------

library(xgboost)
library(methods) #?
library(caret)

# foresttestxgb = data.matrix(foresttest1)
y = as.matrix(as.numeric(forestdata1[,54]) - 1)
forestdataxgb = sparse.model.matrix(Cover_Type ~ .-1 -Id, data = forestdata1)
foresttestxgb = foresttest1
foresttestxgb = sparse.model.matrix(Cover_Type ~ .-1 -Id, data = testmodel) #as.matrix(foresttestxgb)

param = list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 7+1)

cv.nround = 800
cv.nfold = 3

bst.cv = xgb.cv(param=param, data = forestdataxgb, label = y,
                nfold = cv.nfold, nrounds = cv.nround, prediction = T)

min.logloss.idx = which.min(bst.cv$dt[, test.mlogloss.mean]) 
min.logloss.idx 

pred.cv = matrix(bst.cv$pred, nrow=length(bst.cv$pred)/8, ncol=8)
pred.cv = max.col(pred.cv, "last")

confusionMatrix(factor(y+1), factor(pred.cv))


bst = xgboost(param=param, data = forestdataxgb, label = y,
                nfold = cv.nfold, nrounds = min.logloss.idx)

pred_xgb1 = predict(bst, foresttestxgb)

pred_xgb1 = matrix(pred_xgb1, nrow=8, ncol=length(pred_xgb1)/8)
pred_xgb1 = t(pred_xgb1)
pred_xgb1 = max.col(pred_xgb1, "last")



pred_xgb1_summary = as.data.frame(pred_xgb1)

xgb_submission1 = foresttest[,c(1,56)]
xgb_submission1$Cover_Type = pred_xgb1_summary[,1]

write.csv(xgb_submission1, 'xgb_submission2_800.csv', row.names = FALSE)
# kaggle accuracy = 0.70033, rank = 1258 - nround=50

# kaggle accuracy =0.73438 , rank = 991 - nround 104





#Using XG Boost on the complete dataset 


y = as.matrix(as.numeric(forestdata2[,54]) - 1)
forestdataxgb = sparse.model.matrix(Cover_Type ~ .-1 -Id -covername-Wilderness_Area, data = forestdata2[,-c(61,68,69)])
foresttestxgb = foresttest1
foresttestxgb = sparse.model.matrix(Cover_Type ~ .-1 -Id-covername-Wilderness_Area, data = foresttest1[,-c(55,68,69)]) #as.matrix(foresttestxgb)

param = list("objective" = "multi:softprob",
             "eval_metric" = "mlogloss",
             "num_class" = 7+1)

cv.nround = 200
cv.nfold = 3

bst.cv = xgb.cv(param=param, data = forestdataxgb, label = y,
                nfold = cv.nfold, nrounds = cv.nround, prediction = T)

min.logloss.dx = which.min(bst.cv$dt[, test.mlogloss.mean])

min.logloss.dx 

pred.cv = matrix(bst.cv$pred, nrow=length(bst.cv$pred)/8, ncol=8)
pred.cv = max.col(pred.cv, "last")

confusionMatrix(factor(y+1), factor(pred.cv))



bst = xgboost(param=param, data = forestdataxgb, label = y,
              nfold = cv.nfold, nrounds = min.logloss.dx)

pred_xgb1 = predict(bst, foresttestxgb)

pred_xgb1 = matrix(pred_xgb1, nrow=8, ncol=length(pred_xgb1)/8)
pred_xgb1 = t(pred_xgb1)
pred_xgb1 = max.col(pred_xgb1, "last")



pred_xgb1_summary = as.data.frame(pred_xgb1)

xgb_submission1 = foresttest[,c(1,56)]
xgb_submission1$Cover_Type = pred_xgb1_summary[,1]

write.csv(xgb_submission1, 'xgb_submission2_800_feature.csv', row.names = FALSE)

# Compute feature importance matrix
library(Ckmeans.1d.dp)

names <- dimnames(forestdataxgb)[[2]]


importance_matrix <- xgb.importance(names, model = bst)


xgb.plot.importance(importance_matrix[1:20,])



y = as.matrix(as.numeric(forestdata2[,54]) - 1)
forestdataxgb = sparse.model.matrix(Cover_Type ~ .-1 -Id -covername-Wilderness_Area, data = forestdata2[,-c(61,68,69)])
foresttestxgb = foresttest1
foresttestxgb = sparse.model.matrix(Cover_Type ~ .-1 -Id-covername-Wilderness_Area, data = foresttest1[,-c(55,68,69)]) #as.matrix(foresttestxgb)




#re order the dataset in the order of importance of features run the XGBOOST again

loglossxg= 2:72
accuracyxg=2:72

for (j in 2:72)
{
  reducednames=importance_matrix[1:j,]$Feature
  forestdataxgb1=forestdataxgb[,reducednames]
}



for (j in 2:72)
{  
  reducednames=importance_matrix[1:j,]$Feature
  reducednames
  forestdataxgb1=forestdataxgb[,reducednames]
  
  param = list("objective" = "multi:softprob",
               "eval_metric" = "mlogloss",
               "num_class" = 7+1)
  
  cv.nround = 150
  cv.nfold = 3
  
  bst.cv = xgb.cv(param=param, data = forestdataxgb1, label = y,
                  nfold = cv.nfold, nrounds = cv.nround, prediction = T)
  
  min.logloss.dx = which.min(bst.cv$dt[, test.mlogloss.mean])
  
  loglossxg[j]=min.logloss.dx 
  
  pred.cv = matrix(bst.cv$pred, nrow=length(bst.cv$pred)/8, ncol=8)
  pred.cv = max.col(pred.cv, "last")
  
  k=confusionMatrix(factor(y+1), factor(pred.cv),positive='1')
  accuracyxg[j]=k$overall[1]
}





bst = xgboost(param=param, data = forestdataxgb[,reducednames[1:62]], label = y,
              nfold = 10, nrounds = 109)

pred_xgb1 = predict(bst, foresttestxgb[,reducednames[1:62]])

pred_xgb1 = matrix(pred_xgb1, nrow=8, ncol=length(pred_xgb1)/8)
pred_xgb1 = t(pred_xgb1)
pred_xgb1 = max.col(pred_xgb1, "last")



pred_xgb1_summary = as.data.frame(pred_xgb1)

xgb_submission1 = foresttest[,c(1,56)]
xgb_submission1$Cover_Type = pred_xgb1_summary[,1]

write.csv(xgb_submission1, 'xgb_final_submission_62var.csv', row.names = FALSE)



loglossxg=loglossxg[2:72,]
loglossxg=as.data.frame(loglossxg)

Variables=2:72
p1=ggplot(data=accuracyxg,
       aes(x=Variables, y=accuracyxg$`accuracyxg[2:72]`)) +
  geom_line(colour="red")+ ggtitle("Xgboost Cross Validation Accuracy - Sequential Variable Addition")+ 
    xlab("Number of Variables added")+ ylab("Accuracy levels")

p2=ggplot(data=loglossxg,
       aes(x=Variables, y=loglossxg)) +
  geom_line(colour="blue")+ ggtitle("Xgboost Cross Validation min logloss round - Sequential Variable Addition")+ 
  xlab("Number of Variables added")+ ylab("Number of rounds for minimized logloss  ")

ggplotly()

# function found at http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # ncol: Number of columns of plots, nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p1,p2)








# Extra Trees 

options( java.parameters = "-Xmx4g" )
library(rJava)

library( "RWeka" )

library(extraTrees)


et <- extraTrees(x, y, mtry=13, numRandomCuts = 2, nodesize = 3, numThreads = 3, ntree=50)
yhat <- predict(et, xtest)
# Error in .jarray(m) : java.lang.OutOfMemoryError: Java heap space! not working


yhat = as.numeric(yhat)
yhat = yhat - rep(1, length(yhat))
yhat = as.data.frame(yhat)

extratrees_submission1 = foresttest[,c(1,56)]
extratrees_submission1$Cover_Type = yhat[,1]

write.csv(extratrees_submission1, 'extratrees_submission1.csv', row.names = FALSE)
# kaggle accuracy = 0.79220, rank = 224



# Tuning parameters

set.seed(1)
train = sample(1:nrow(x), 8*nrow(x)/10)
test = (-train)

y = forestdata$Cover_Type
y.test = y[test]




ntree = seq(50, 300, 10)
accuracy_et = 1:length(ntree)

for (i in 1:length(ntree))
{
  et_tune1 = extraTrees(x[train,], y[train], mtry=13, numRandomCuts = 2, nodesize = 3, numThreads = 3, ntree=ntree[i])
  yhat = predict(et_tune1, x[test,])
  confusion = confusionMatrix(yhat, y.test, positive = '1')
  accuracy_et[i] = confusion$overall[1]
  print(i)
}

plot(ntree, accuracy_et, pch = 16, type = "b",
     xlab = "Number of Trees",
     ylab = "accuracy",
     main = "Extra Trees Accuracy Changing Number of Trees")
# choose 200 trees


mtry_et = 1:30
accuracy_et2 = 1:30

for (i in 1:length(mtry_et))
{
  et_tune2 = extraTrees(x[train,], y[train], mtry=i, numRandomCuts = 2, nodesize = 3, numThreads = 3, ntree=200)
  yhat = predict(et_tune2, x[test,])
  confusion2 = confusionMatrix(yhat, y.test, positive = '1')
  accuracy_et2[i] = confusion2$overall[1]
  print(i)
}

plot(mtry_et, accuracy_et2, pch = 16, type = "b",
     xlab = "Number of Variables",
     ylab = "accuracy",
     main = "Extra Trees Accuracy Changing Number of Variables at Each Split")
# choose 8-10 variables (n^.5 is the suggested number)






accuracy_et3 = matrix(ncol = 3, nrow = 100)
counter=100
for (i in 11:20)
{
  for (j in 1:10)
  {
    et_tune3 = extraTrees(x[train,], y[train], mtry=10, numRandomCuts = i, nodesize = j, numThreads = 3, ntree=200)
    yhat = predict(et_tune3, x[test,])
    confusion3 = confusionMatrix(yhat, y.test, positive = '1')
    accu = confusion3$overall[1]
    accuracy_et3[counter,1] = accu
    accuracy_et3[counter,2] = i
    accuracy_et3[counter,3] = j
    print(counter)
    counter = counter+1
  }
}


accuracy_et3=as.data.frame(accuracy_et3)

library(plotly)
colnames(accuracy_et3)=c("Accuracy","Number of random cuts","Node Size")

plot_ly(accuracy_et3, x =`Number of random cuts` ,y =`Node Size`, z = Accuracy, type = "scatter3d", mode = "markers")


plot_ly(accuracy_et3, x = `Number of random cuts` , y = `Accuracy`, 
        mode = "markers", color=`Node Size`)





#based on the plots we decided on the following for the final run without feature engineering 
# mtry = 10 ,  ntrees = 200  ,  Node size = 2, Number of random cuts = 5


et_tuned_final1 <- extraTrees(x, y, mtry=10, numRandomCuts = 5, nodesize = 2, numThreads = 3, ntree=200)
yhat_tuned <- predict(et_tuned_final1, xtest)
# Error in .jarray(m) : java.lang.OutOfMemoryError: Java heap space! not working


yhat_tuned = as.data.frame(yhat_tuned)

extratrees_submission1 = foresttest[,c(1,56)]
extratrees_submission1$Cover_Type = yhat_tuned[,1]

write.csv(extratrees_submission1, 'extratrees_submission_tuned1.csv', row.names = FALSE)
# kaggle accuracy = 0.79247, rank = 224






# creating matrices for Feature engineered data


xfactors_extra <- model.matrix(Cover_Type ~. - Id - Soil_Type - soil_family -covername - Wilderness_Area , data = forestdata2)[,-1]
x = as.matrix(xfactors_extra)

set.seed(0)
train_extra = sample(1:nrow(x), 80*nrow(x)/100)
test_extra = (-train)

y_extra = forestdata2$Cover_Type
y_extra.test = y_extra[test]

xtest_extra <- model.matrix(Cover_Type ~. - Id - Soil_Type - soil_family -covername - Wilderness_Area ,data = foresttest)[,-1]

xtest_extra = xtest_extra[,-c(21,29)]
xtest_extra = as.matrix(xtest_extra)




et2 <- extraTrees(xfactors_extra, y_extra, mtry=13, numRandomCuts = 4, nodesize = 3, numThreads = 3, ntree=700)
yhat2 <- predict(et, xtest_extra)


yhat2 = as.numeric(yhat)
yhat2 = yhat - rep(1, length(yhat))
yhat2 = as.data.frame(yhat)

extratrees_submission2 = foresttest[,c(1,56)]
extratrees_submission2$Cover_Type = yhat2[,1]

write.csv(extratrees_submission2, 'extratrees_submission2_allfeatures.csv', row.names = FALSE)


# Extra trees feature engineering resulted in  0.78739 rank 308 ---mtry=13, numRandomCuts = 4, nodesize = 3, numThreads = 3, ntree=700











#  Deep learning with H20


# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("methods","statmod","stats","graphics","RCurl","jsonlite","tools","utils")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
#install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-turan/3/R")))
library(h2o)
library(devtools)


h2o.init(nthreads=-1, max_mem_size="3G")
h2o.removeAll() ## clean slate - just in case

# re-load the dataframe in H20 format
#forest_h20 <- h2o.importFile(path = normalizePath("C://Users/Aravind/Documents/GitHub/bootcamp004_project/Project4-Machinelearning/Aravind_thomas/train.csv"))
#foresttest_h20 <- h2o.importFile(path = normalizePath("C://Users/Aravind/Documents/GitHub/bootcamp004_project/Project4-Machinelearning/Aravind_thomas/test.csv"))
forest_h20 <- h2o.importFile(path = normalizePath('/Users/tkolasa/dev/nycdatascience/projects/bootcamp004_project/Project4-Machinelearning/Aravind_thomas/train.csv'))
foresttest_h20 <- h2o.importFile(path = normalizePath('/Users/tkolasa/dev/nycdatascience/projects/bootcamp004_project/Project4-Machinelearning/Aravind_thomas/test.csv'))



# par(mfrow=c(1,1)) # reset canvas
# plot(h2o.tabulate(forestdata1, "Elevation",                       "Cover_Type"))
# plot(h2o.tabulate(df, "Horizontal_Distance_To_Roadways", "Cover_Type"))
# plot(h2o.tabulate(df, "Soil_Type",                       "Cover_Type"))
# plot(h2o.tabulate(df, "Horizontal_Distance_To_Roadways", "Elevation" ))
# 

forest_h20 = forest_h20[,-1]

for (i in 11:55)
{
  forest_h20[,i]=as.factor(forest_h20[,i])
}
  
for (i in 12:55)
{
  foresttest_h20[,i]=as.factor(foresttest_h20[,i])
}

response <- "Cover_Type"
predictors <- setdiff(names(forest_h20), response)
predictors

#initial sampling
set.seed(0)
train_h20 = sample(1:nrow(x), 80*nrow(x)/100)
train_h20=sort(train_h20)
validation_h20 = (-train_h20)
validation_h20=sort(validation_h20)

forest_h20_train= forest_h20[train_h20,]
forest_h20_validation=forest_h20[validation_h20,]

# final sampling
set.seed(0)
train_h20_final = sample(1:nrow(x), 95*nrow(x)/100)
train_h20=sort(train_h20)
validation_h20 = (-train_h20)
validation_h20=sort(validation_h20)

forest_h20_train= forest_h20[train_h20,]
forest_h20_validation=forest_h20[validation_h20,]



m1 <- h2o.deeplearning(
  model_id="deep_learning_initial", 
  training_frame=forest_h20_train, 
  validation_frame=forest_h20_validation,   ## validation dataset: used for scoring and early stopping
  x=predictors,
  y=response,
  activation="Rectifier",  ## default
  hidden=c(130,150,70),## default: 2 hidden layers with 200 neurons each, 
  epochs = 20 ,    
  variable_importances=T,
  epsilon =1e-7 ## not enabled by default
)
summary(m1)




m2 <- h2o.deeplearning(
  model_id="dl_model_faster", 
  training_frame=forest_h20_train, 
  validation_frame=forest_h20_validation,
  x=predictors,
  y=response,
  hidden=c(64,32,64),                  ## small network, runs faster
  epochs=100000,                      ## hopefully converges earlier...
  score_validation_samples=10000,      ## sample the validation dataset (faster)
  stopping_rounds=2,
  stopping_metric="misclassification", ## could be "MSE","logloss","r2"
  stopping_tolerance=0.01,
  epsilon=1e-7
)
summary(m2)
plot(m2)




m3 <- h2o.deeplearning(
  model_id="dl_model_tuned", 
  training_frame=forest_h20_train, 
  validation_frame=forest_h20_validation, 
  x=predictors, 
  y=response, 
  overwrite_with_best_model=F,    ## Return the final model after 10 epochs, even if not the best
  hidden=c(128,128,128),          ## more hidden layers -> more complex interactions
  epochs=10,                      ## to keep it short enough
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  adaptive_rate=F,                ## manually tuned learning rate
  rate=0.01, 
  rate_annealing=2e-6,            
  momentum_start=0.2,             ## manually tuned momentum
  momentum_stable=0.4, 
  momentum_ramp=1e7, 
  l1=1e-5,                        ## add some L1/L2 regularization
  l2=1e-5,
  max_w2=10                       ## helps stability for Rectifier
) 
summary(m3)



h2o.performance(m3, train=T)       ## sampled training data (from model building)
h2o.performance(m3, valid=T)       ## sampled validation data (from model building)
h2o.performance(m3, data=forest_h20_train)    ## full training data
h2o.performance(m3, data=forest_h20_validation)    ## full validation data


head(as.data.frame(h2o.varimp(m1)))




performancem2=h2o.performance(m2, train=T)  


#Calculating accuracy:
pred <- h2o.predict(m2,forest_h20_validation)
pred
Accuracy_m2 <- pred$predict == forest_h20_validation$Cover_Type
mean(Accuracy_m2)






#hyperparameter tuning


hyper_params <- list(
  hidden=list(c(30),c(50),c(70),c(100),c(130),c(160),c(200),c(250),c(30,60),c(60,90),c(90,120),c(120,150),c(30,60,90),c(60,90,120),c(90,120,160)), 
  input_dropout_ratio=c(0,0.05),
  rate=c(0.01,0.02),
  rate_annealing=c(1e-8,1e-7,1e-6))

hyper_params
grid <- h2o.grid(
  "deeplearning",
  model_id="dl_grid", 
  training_frame=forest_h20_train,
  validation_frame=forest_h20_validation, 
  x=predictors, 
  y=response,
  epochs=10,
  stopping_metric="misclassification",
  stopping_tolerance=1e-2,        ## stop when logloss does not improve by >=1% for 2 scoring events
  stopping_rounds=2,
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  adaptive_rate=F,                ## manually tuned learning rate
  momentum_start=0.5,             ## manually tuned momentum
  momentum_stable=0.9, 
  momentum_ramp=1e7, 
  l1=1e-5,
  l2=1e-5,
  activation=c("Rectifier"),
  max_w2=10,                      ## can help improve stability for Rectifier
  hyper_params=hyper_params
)



scores <- cbind(as.data.frame(unlist((lapply(grid@model_ids, function(x) 
{ h2o.confusionMatrix(h2o.performance(h2o.getModel(x),valid=T))$Error[8] })) )), unlist(grid@model_ids))
names(scores) <- c("misclassification","model")
sorted_scores <- scores[order(scores$misclassification),]
head(sorted_scores)
best_model <- h2o.getModel(as.character(sorted_scores$model[1]))
print(best_model@allparameters)
best_err <- sorted_scores$misclassification[1]
print(best_err)



pred_final_deeplearning <- h2o.predict(best_model,foresttest_h20[,-1] )
deeplearning_pred=as.data.frame(pred_final_deeplearning[,1])
head(deeplearning_pred)


deeplearning_submission1 = foresttest[,c(1,56)]
deeplearning_submission1$Cover_Type = deeplearning_pred[,1]

write.csv(deeplearning_submission1, 'deep_learing_submission1.csv', row.names = FALSE)

# accuracy = 0.59460, rank = 1418





# GBM via H2O -------------------------------------------

gbm1 = h2o.gbm(
  training_frame = forest_h20_train,
  validation_frame = forest_h20_validation,
  x=predictors, 
  y=response,
  model_id = "gbm_covType1",
  seed = 7)

summary(gbm1)

# accuracy
h2o.hit_ratio_table(gbm1,valid = T)[1,2]
# accuracy = .818783 for default parameters


gbm2 = h2o.gbm(
  training_frame = forest_h20_train,
  validation_frame = forest_h20_validation,
  x=predictors, 
  y=response,
  model_id = "gbm_covType2",
  seed = 7,
  ntrees = 250,
  max_depth = 18,
  min_rows = 10,
  learn_rate = .1  )

# accuracy
h2o.hit_ratio_table(gbm2,valid = T)[1,2]
# 0.88459

pred_final_gbm_h2o <- h2o.predict(gbm2,foresttest_h20[,-1] )
pred_final_gbm_h2o=as.data.frame(pred_final_gbm_h2o[,1])
head(pred_final_gbm_h2o)


gbm_h2o_submission1 = foresttest[,c(1,56)]
gbm_h2o_submission1$Cover_Type = pred_final_gbm_h2o[,1]

write.csv(gbm_h2o_submission1, 'gbm_h2o_submission1.csv', row.names = FALSE)

# kaggle accuracy = 0.76640, rank = 467




gbm3 = h2o.gbm(
  training_frame = forest_h20_train,
  validation_frame = forest_h20_validation,
  x=predictors, 
  y=response,
  model_id = "gbm_covType3",
  seed = 7,
  ntrees = 1000,
  max_depth = 18,
  min_rows = 10,
  learn_rate = .01,
  sample_rate = 0.7,          ## use a random 70% of the rows to fit each tree
  col_sample_rate = 0.7,       ## use 70% of the columns to fit each tree
  stopping_rounds  = 2,
  stopping_tolerance = 0.01,
  score_each_iteration = T)

# accuracy
h2o.hit_ratio_table(gbm3,valid = T)[1,2]
# 0.8561508


# found that ntrees is the same after 40, so we will try 40 and 200
# found max_depth of 20 is best for ntrees = 100 or 50
hyper_params_gbm = list(
  ntrees = c(40, 100, 200),
  max_depth = 20,
  min_rows = c(5, 10, 15),
  learn_rate = c(.01, .1, .3)
)

grid_gbm = h2o.grid(
  "gbm",
  model_id = "gbm_covType3",  
  training_frame = forest_h20_train,
  validation_frame = forest_h20_validation,
  x=predictors, 
  y=response,
  seed = 7,
  sample_rate = 0.7,
  col_sample_rate = 0.7,
  stopping_rounds  = 2,
  stopping_tolerance = 0.01,
  score_each_iteration = T,
  #max_depth = 18,
  #min_rows = 10,
  #learn_rate = .1,
  hyper_params = hyper_params_gbm)


scores <- cbind(as.data.frame(unlist((lapply(grid_gbm@model_ids, function(x) 
{ h2o.confusionMatrix(h2o.performance(h2o.getModel(x),valid=T))$Error[8] })) )), unlist(grid_gbm@model_ids))
names(scores) <- c("misclassification","model")
sorted_scores <- scores[order(scores$misclassification),]
sorted_scores

# best result had ntrees=200, max_depth=20, learn_rate=.1, min_rows=10
# 2nd best result had ntrees=100, max_depth=20, learn_rate=.1, min_rows=10
# 3rd best result had ntrees=40, max_depth=20, learn_rate=.1, min_rows=10



gbm_final = h2o.gbm(
  training_frame = forest_h20_train,
  validation_frame = forest_h20_validation,
  x=predictors, 
  y=response,
  model_id = "gbm_covTypefinal",
  seed = 7,
  ntrees = 200,
  max_depth = 20,
  min_rows = 10,
  learn_rate = .1,
  sample_rate = 0.7,          ## use a random 70% of the rows to fit each tree
  col_sample_rate = 0.7,       ## use 70% of the columns to fit each tree
  stopping_rounds  = 2,
  stopping_tolerance = 0.01,
  score_each_iteration = T,
  nfolds = 10)

pred_gbm_h2o_tuned1 <- h2o.predict(gbm_final,foresttest_h20[,-1] )
pred_gbm_h2o_tuned1 = as.data.frame(pred_gbm_h2o_tuned1[,1])
head(pred_gbm_h2o_tuned1)

gbm_h2o_submission_tuned1 = foresttest[,c(1,56)]
gbm_h2o_submission_tuned1$Cover_Type = pred_gbm_h2o_tuned1[,1]

write.csv(gbm_h2o_submission_tuned1, 'gbm_h2o_submission_tuned2.csv', row.names = FALSE)
# kaggle accuracy = 0.76119 when old was 0.76640, rank = 521
# submission 2 with cv 80/20 kaggle accuracy = 0.74509, rank = 911





gbm_final2 = h2o.gbm(
  training_frame = forest_h20_train,
  validation_frame = forest_h20_validation,
  x=predictors, 
  y=response,
  model_id = "gbm_covTypefinal",
  seed = 7,
  ntrees = 200,
  max_depth = 20,
  min_rows = 10,
  learn_rate = .1#,
#   sample_rate = 0.7,          ## use a random 70% of the rows to fit each tree
#   col_sample_rate = 0.7,       ## use 70% of the columns to fit each tree
#   stopping_rounds  = 2,
#   stopping_tolerance = 0.01,
#   score_each_iteration = T,
#  nfolds = 10
)

pred_gbm_h2o_tuned2 <- h2o.predict(gbm_final2,foresttest_h20[,-1] )
pred_gbm_h2o_tuned2 = as.data.frame(pred_gbm_h2o_tuned2[,1])
head(pred_gbm_h2o_tuned2)

gbm_h2o_submission_tuned2 = foresttest[,c(1,56)]
gbm_h2o_submission_tuned2$Cover_Type = pred_gbm_h2o_tuned2[,1]

write.csv(gbm_h2o_submission_tuned2, 'gbm_h2o_submission_tuned3.csv', row.names = FALSE)
# submission without misc. parameters: kaggle accuracy = 0.76586, rank = 479



h2o.shutdown(prompt=FALSE)










