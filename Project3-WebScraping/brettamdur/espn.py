# -*- coding: utf-8 -*-
"""
Created on Fri Feb 19 18:04:09 2016

@author: bamdur
"""
from bs4 import BeautifulSoup
import datetime
import re
import requests
import csv

#%%
# build the date in the format used by espn website
def buildRequestDate(dateObject):
    if dateObject.month < 10:
        newMonth = "0" + str(dateObject.month)
    else:
        newMonth = str(dateObject.month)
    if dateObject.day < 10:
        newDay = "0" + str(dateObject.day)
    else:
        newDay = str(dateObject.day)    
    return str(i.year) + newMonth + newDay
#%%
############################################################
#### GET GAME IDS FOR GAMES IN THE SPECIFIED DATE RANGE ####    
############################################################    
startDate = datetime.datetime(2016,1,26) # year, month, date
endDate = datetime.datetime(2016,2,11) 

allGameTags = []
i  = startDate
while(i <= endDate):
    # gets the schedule for one week in html 
    requestDate = buildRequestDate(i)
    requestURL = 'http://espn.go.com/nba/schedule/_/date/' + requestDate
    htmlString = requests.get(requestURL).text
    
    # get the tags we need
    calendarPage = BeautifulSoup(htmlString)
    thisGameTags = [t for t in calendarPage.select('a[href*=gameId]')]
    
    # add the tags we need to the list of all tags from downloaded html
    allGameTags = allGameTags + thisGameTags
    
    i = i + datetime.timedelta(days=7)

#%%
# extracts the gameId from an html tag
def getId(tagLine):
    startPos = (re.search('gameId=', str(tagLine)).end())
    endPos = startPos + 9
    return str(tagLine)[startPos:endPos]

# loop through each element of allGameTags to extract gameIds and put them in a list
gameIds = [getId(x) for x in allGameTags]

#%%
###################################################
#### GET SHOT INFO FOR ALL GAMES IN DATE RANGE ####
###################################################

# gets game info for all shots in one game
def getGameShotInfo(gameId):
    requestURL = 'http://espn.go.com/nba/gamepackage/data/shot?gameId=' + gameId
    gameXML = requests.get(requestURL).text
    gamePage = BeautifulSoup(gameXML)
    gameShotList = list(gamePage.find_all('shot'))
    allGameShots = [str(shotRow).split('"')[1::2] for shotRow in gameShotList]
    
    for i in allGameShots:
        i.extend([gameId])
    return allGameShots

shotsCombined = []
for i in gameIds:
    gameShots = getGameShotInfo(i)
    shotsCombined.extend(gameShots)
    
#%%
# write the shot info to csv
with open("output_0126-0211.csv", "w", newline='') as f:
    writer = csv.writer(f)
    writer.writerows(shotsCombined)
f.close()

  