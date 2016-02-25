from bs4 import BeautifulSoup
from urllib2 import urlopen
import csv
import re
import pandas as pd
from retrypy import retry
@retry.decorate(times=4)
def urlopen_with_retry(some_url):
    return urlopen(some_url)
#initialize a dataframe with no rows
marvel_df = pd.DataFrame({'character': [],'title': [],'date': []} )
# make a list (manually) of the characters I am interested in
characters=["http://marvel.wikia.com/wiki/Category:Steven_Rogers_%28Earth-616%29/Appearances","http://marvel.wikia.com/wiki/Category:Peter_Parker_%28Earth-616%29/Appearances",
'http://marvel.wikia.com/wiki/Category:James_Howlett_%28Earth-616%29/Appearances','http://marvel.wikia.com/wiki/Category:Anthony_Stark_%28Earth-616%29/Appearances',
'http://marvel.wikia.com/wiki/Category:Thor_Odinson_%28Earth-616%29/Appearances']
for char in range(len(characters)):
    current_url=characters[char]
    print current_url#debug
    issuelinks=[]#initialize as an empty list for the issues
    navlinks=['dummy']#intialize a list for navigation links, has one dummy string to avoid errors
    stop_cond=0
    
    while stop_cond==0: #this should keep going until we've exhausted the navigation pages for the current characters appearance
    #the stopping condition is that the final navlink hasn't changed in the last step
        soup = BeautifulSoup(urlopen_with_retry(current_url).read())	
        issue_table=soup.find('div',{'id':'mw-pages'})#this table has all the links we want with no repeats
        issues = issue_table.find_all('a', href= re.compile('^(?!.*Appearances).*$'))#save all links except those for navigation to next/previous in list
        navs=issue_table.find_all('a',href=re.compile('pagefrom'))#save the nav links so we can get the other pages
        
        for i in issues:
            issuelinks.append("http://marvel.wikia.com" + i['href'])#update list
            
        for i in navs:
            navlinks.append("http://marvel.wikia.com"+i['href'])#update list
            
        if len(issue_table.find_all('a',href=re.compile('pagefrom')))==0:  #if no next page  
           stop_cond=1
        
        current_url=navlinks[-1]#set current url to the last entry in navlinks, as this should be the "next" page
        print len(issuelinks) #debug
         
    for i in xrange(len(issuelinks)):#len(issuelinks)
        print i#debug
        current_url=issuelinks[i]	
        soup = BeautifulSoup(urlopen_with_retry(current_url).read())   
        character=characters[char]
        title=soup.h1.text#title is really easy
        #taking publish date and not release date, this is only because the one is in a better format, (month and year) release dates indicated when its actually first on shelves, so there is a ~two month lag
        #date link should end in a month, and the original link should contain the word "category"
        date=soup.find('a',href=re.compile(',_January|,_February|,_March|,_April|,_May|,_June|,_July|,_August|,_September|,_October|,_November|,_December$'))
        new_row=pd.DataFrame({'character': [character],'title': [title],'date': [date]} )
        marvel_df=marvel_df.append(new_row)

#save as a csv, we can the up the strings later
marvel_df.to_csv('marvel.csv',encoding='utf16')
