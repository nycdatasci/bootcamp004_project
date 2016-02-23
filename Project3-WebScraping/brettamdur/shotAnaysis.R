
require(dplyr)
require(ggplot2)
require(NbClust)

setwd('C:/Users/bamdur/Documents/NYCDSA/projects/scraping/')
shotsOrig <- read.csv('./output_1027-1123.csv', header = FALSE, stringsAsFactors = FALSE)
shotsOrig <- rbind(shotsOrig, read.csv('./output_1124-1221.csv', header = FALSE, stringsAsFactors = FALSE))
shotsOrig <- rbind(shotsOrig, read.csv('./output_1222-0125.csv', header = FALSE, stringsAsFactors = FALSE))
shotsOrig <- rbind(shotsOrig, read.csv('output_0126-0211.csv', header = FALSE, stringsAsFactors = FALSE))
names(shotsOrig) <- c('description', 'shotID', 'made', 'playerName', 'playerID', 'quarter', 'homeAway', 'xcoord', 'ycoord', 'gameId')
save(shotsOrig, file="shotsOrig.rda")

shotsOrig <- load('./shotsOrig.rda')


#### calculate shooting percentage ####
#### NOTE: this shows calculating averages for subgroups using dplyr ####
#### Note the use of ungroup() ####
shootpct <- group_by(shotsOrig, playerName) %>%
			summarise(. , total = n())

shootpct2 <- group_by(shootpct, playerName) %>%
				mutate(. , totalShots = sum(total)) %>%
				filter(. , made == 'true') %>%
				mutate(. , pct = total/totalShots) %>%
				ungroup() %>%
				arrange(. , desc(totalShots)) %>%
				filter(. , totalShots > 250)

########################

shotsBogut <- filter(shotsOrig, playerName == 'Andrew Bogut', homeAway == 'h')
shotsBogut <- select(shotsBogut, xcoord, ycoord)
resBogut<-NbClust(shotsBogut, diss=NULL, distance = "euclidean", min.nc=2, max.nc=8,
				  method = "centroid", index = "kl")

wssplot(shotsBogut)
resBogut$Best.nc

set.seed(10)
kmBogut = kmeans(shotsBogut, centers = resBogut$Best.nc[1])

gBogut <- ggplot(shotsBogut, aes(x = xcoord, y = ycoord))  + 
	geom_jitter() + 
	geom_point(data=as.data.frame(kmBogut$centers), aes(x = xcoord, y = ycoord), color='blue', size = 4) +
	ylim(50, 92) +
	xlim(0, 50)

gBogut



##### for getting rid of outliers #####

zscores = mutate(shotsBogut, xzscore = ((xcoord - mean(xcoord)) / sd(xcoord)), yzscore = ((ycoord - mean(ycoord)) / sd(ycoord)))
# do below for x and y in abs(xzscore) and abs(yzscore)
# zscores <- arrange(zscores, desc(abs(xzscore)))
zscoresOut <- filter(zscores, abs(xzscore) >= 2 | abs(yzscore) >= 2)
# need to remove entries in zscoresOut from x dataframe

#### reasses k means after no outliers ####
