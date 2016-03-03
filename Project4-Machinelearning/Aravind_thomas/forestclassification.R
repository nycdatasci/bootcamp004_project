

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
# 1	   2702		Cathedral family - Rock outcrop complex, extremely stony.
# 2	   2703		Vanet - Ratake families complex, very stony.
# 3	   2704		Haploborolis - Rock outcrop complex, rubbly.
# 4	   2705		Ratake family - Rock outcrop complex, rubbly.
# 5	   2706		Vanet family - Rock outcrop complex complex, rubbly.
# 6	   2717		Vanet - Wetmore families - Rock outcrop complex, stony.
# 7	   3501		Gothic family.
# 8	   3502		Supervisor - Limber families complex.
# 9	   4201		Troutville family, very stony.
# 10	   4703		Bullwark - Catamount families - Rock outcrop complex, rubbly.
# 11	   4704		Bullwark - Catamount families - Rock land complex, rubbly.
# 12	   4744		Legault family - Rock land complex, stony.
# 13	   4758		Catamount family - Rock land - Bullwark family complex, rubbly.
# 14	   5101		Pachic Argiborolis - Aquolis complex.
# 15	   5151		unspecified in the USFS Soil and ELU Survey.
# 16	   6101		Cryaquolis - Cryoborolis complex.
# 17	   6102		Gateview family - Cryaquolis complex.
# 18	   6731		Rogert family, very stony.
# 19	   7101		Typic Cryaquolis - Borohemists complex.
# 20	   7102		Typic Cryaquepts - Typic Cryaquolls complex.
# 21	   7103		Typic Cryaquolls - Leighcan family, till substratum complex.
# 22	   7201		Leighcan family, till substratum, extremely bouldery.
# 23	   7202		Leighcan family, till substratum - Typic Cryaquolls complex.
# 24	   7700		Leighcan family, extremely stony.
# 25	   7701		Leighcan family, warm, extremely stony.
# 26	   7702		Granile - Catamount families complex, very stony.
# 27	   7709		Leighcan family, warm - Rock outcrop complex, extremely stony.
# 28	   7710		Leighcan family - Rock outcrop complex, extremely stony.
# 29	   7745		Como - Legault families complex, extremely stony.
# 30	   7746		Como family - Rock land - Legault family complex, extremely stony.
# 31	   7755		Leighcan - Catamount families complex, extremely stony.
# 32	   7756		Catamount family - Rock outcrop - Leighcan family complex, extremely stony.
# 33	   7757		Leighcan - Catamount families - Rock outcrop complex, extremely stony.
# 34	   7790		Cryorthents - Rock land complex, extremely stony.
# 35	   8703		Cryumbrepts - Rock outcrop - Cryaquepts complex.
# 36	   8707		Bross family - Rock land - Cryumbrepts complex, extremely stony.
# 37	   8708		Rock outcrop - Cryumbrepts - Cryorthents complex, extremely stony.
# 38	   8771		Leighcan - Moran families - Cryaquolls complex, extremely stony.
# 39	   8772		Moran family - Cryorthents - Leighcan family complex, extremely stony.
# 40	   8776		Moran family - Cryorthents - Rock land complex, extremely stony.
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


#setwd('c://dataset/forestcover')

library(ggplot2)
library(dplyr)
library(GGally)


forestdata= read.csv('train.csv', header=TRUE)
View(forestdata)
summary(forestdata[2:15,])

# adding a new variable for covernames based on cover type

forestdata$covername='a'
forestdata$covername[forestdata$Cover_Type==1]='Spruce-fir'
forestdata$covername[forestdata$Cover_Type==2]='Lodgepole Pine'
forestdata$covername[forestdata$Cover_Type==3]='Ponderosa Pine'
forestdata$covername[forestdata$Cover_Type==4]='Cottonwood-Willow'
forestdata$covername[forestdata$Cover_Type==5]='Aspen'
forestdata$covername[forestdata$Cover_Type==6]='Douglas-fir'
forestdata$covername[forestdata$Cover_Type==7]='Krummholz'



# Studying Elevation distribution and densities across Cover type
ggplot(forestdata, aes(x=Elevation)) + geom_histogram(aes(group=covername, colour=covername, fill=covername), alpha=0.3)+ggtitle('T')
ggplot(forestdata, aes(x=Elevation)) + geom_density()
ggplot(forestdata, aes(x=Elevation)) + geom_density(aes(group=covername, colour=covername, fill=covername), alpha=0.3)


ggpairs(forestdata[,2:5],alpha=0.2)


