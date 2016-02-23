
# coding: utf-8

# For this project, I'm aiming to scrape three websites with information useful for understanding import and export patterns for select food commodities:
# 1. MIT Observatory of Economic Complexity
#     - the data used from this site would provide import and export values for the commodities studied in the US Department of Agriculture datasets (R Shiny Project)
# 2. UNData - an online service created by the United Nations Statistics Division
#     - This site contains the original datasets offered by the MIT Observatory of Economic Complexity. While the API's offered by MIT are more streamlined, the UNData has the complete set of data for different product classifications. 
# 3. YCharts and Index Mundi - two services that provide a variety of commodity market data. 
#     - A key part of the webscraping aspect of the study is to go from comparing metric tons for consumption, production, import, and export, to USD for import, export, and spot prices. 

# In[11]:

from IPython.display import HTML
from bs4 import BeautifulSoup
import requests
import re
import pandas as pd
import json
import sys
import requests
import urllib2
from IPython.display import HTML


# In[36]:

url_import = 'http://atlas.media.mit.edu/sitc/import/1962.2012/usa/all/show/'
text = requests.get(url_import).text


# In[119]:

len(text)


# In[44]:

#Form of prettify seems to be the same as the original import, in json. 
response = urllib2.urlopen('http://atlas.media.mit.edu/sitc/import/1962.2012/usa/all/show/').read()
Pythonsoup = json.loads(response)


# In[60]:

Pythonsoup.values()


# In[90]:

#Right now, Pythonsoup is a nested list of dictionaries. Need to convert it to a 
#Data frame that can be easily exported as a csv. 
Pythonsoup


# In[75]:

import csv


# In[89]:

Pythonsoup2 = pd.DataFrame(Pythonsoup[0])

Pythonsoup2.head()


# In[ ]:


# In[25]:

from collections import defaultdict
import csv
from datetime import date
import itertools
import texttable
import pandas as pd


# In[101]:

response2 = urllib2.urlopen('http://atlas.media.mit.edu/sitc/export/1962.2012/usa/all/show/').read()
Pythonsoup3 = json.loads(response2)
print Pythonsoup3


# In[102]:


Pythonsoup3 = Pythonsoup3.values()


# In[103]:

type(Pythonsoup3)


# In[104]:

print(Pythonsoup3)


# In[105]:

Pythonsoup4 = pd.DataFrame(Pythonsoup3[0])

Pythonsoup4.head()


# In[107]:

Pythonsoup4.to_csv("Export_SITC", sep='\t')
Pythonsoup2.to_csv("Import_SITC", sep = '\t')


# Despite trying various different API calls for different trade keys, it seems that the data which was returned for both the import and export calls -- even for various products -- only returns a dataset of export values, for all years featured in the SITC. 

# In[2]:

#Inspecting tables from Index Mundi
Swine = requests.get('http://www.indexmundi.com/commodities/?commodity=pork&months=360').text
stat = BeautifulSoup(Swine)


# In[3]:

stat


# The above table typifies the data received from index mundi. The table rows seemed to be marked with the tag <tr> with <td> descendents. Luckily, the tables from index mundi all show on one page, so scraping should be fairly straightforward. 

# In[5]:

#Inspecting tables from YCharts
YCharts = requests.get('https://ycharts.com/indicators/us_consumer_price_index_pork').text
stat1 = BeautifulSoup(YCharts)


# In[6]:

stat1


# Once again, the table in this site is marked with a tag. However, the issue here is that the table goes for multple pages that require clicking on links at the bottom to view. Hence, it's necessary to inspect the url to see what happens when different pages and different "date ranges" are selected. 
# 
# Base URL:
# https://ycharts.com/indicators/us_consumer_price_index_pork
# 
# URL after page change:
# https://ycharts.com/indicators/us_consumer_price_index_pork
# 
# Uh oh! the URL remains the same despite changing pages. I was hoping, initially, that there could be someting in the URL string that I could write a function for. Unfortunately, this is not the case. 
# 
# The next step is to inspect the html on the browser itself to see if anything is different in the body -- besides the table row values -- that indicates a page change. Using the Google Chrome inspection tool, I pointed to the "Next" button whcih changes the page and found the element: <a ng-click="newPage('prev')">Prev</a>
# 
# Specifically, for the 'First' 'Prev' 'Next' and 'Last' buttons, there is a <a ng-click="newPage" element that calls to a different page. For the last page, it is:
# <a ng-click="newPage('last')">Last</a>
# <a class="nextBtn" ng-click="newPage('next')">Next</a>
# 
# While looking at the Network panel in the inspector toolbar, I clicked the 'next' button to see if any specific URL requests were being made. Here's what I found:
# 
# https://ycharts.com/indicators/us_consumer_price_index_pork.json?endDate=01/31/2016&pageNum=7&startDate=01/31/1947
# 
# This is quite different from the original URL. Here, there is clearly a page descriptor at 2016&pageNum=7&startDate=01/31/1947. This "pageNum" part of the form is what I need. The response to the click gives the following:
# 
# "<div class="dataColLeft">    <div class="padR">    <table class="histDataTable">        <tr>            <th scope="col" class="col1 colHeadLrg">Data for this Date Range</th>            <th scope="col" class="col2 colHeadLrg">&nbsp;</th>        </tr>                <tr>            <td class="col1">Nov. 30, 1986</td>            <td class="col2">                114.50            </td>        </tr>                <tr>            <td class="col1">Oct. 31, 1986</td>            <td class="col2">                112.90            </td>        </tr>                <tr>            <td class="col1">Sept. 30, 1986</td>            <td class="col2">                111.70            </td>        </tr>                <tr>            <td class="col1">Aug. 31, 1986</td>            <td class="col2">                110.70            </td>        </tr>                <tr>            <td class="col1">July 31, 1986</td>            <td class="col2">                106.90            </td>        </tr>                <tr>            <td class="col1">June 30, 1986</td>            <td class="col2">                101.30            </td>        </tr>                <tr>            <td class="col1">May 31, 1986</td>            <td class="col2">                102.40            </td>        </tr>                <tr>            <td class="col1">April 30, 1986</td>            <td class="col2">                102.50            </td>        </tr>                <tr>            <td class="col1">March 31, 1986</td>            <td class="col2">                102.20            </td>        </tr>                <tr>            <td class="col1">Feb. 28, 1986</td>            <td class="col2">                102.40            </td>        </tr>                <tr>            <td class="col1">Jan. 31, 1986</td>            <td class="col2">                101.90            </td>        </tr>                <tr>            <td class="col1">Dec. 31, 1985</td>            <td class="col2">                100.00            </td>        </tr>                <tr>            <td class="col1">Nov. 30, 1985</td>            <td class="col2">                99.40            </td>        </tr>                <tr>            <td class="col1">Oct. 31, 1985</td>            <td class="col2">                96.60            </td>        </tr>                <tr>            <td class="col1">Sept. 30, 1985</td>            <td class="col2">                96.80            </td>        </tr>                <tr>            <td class="col1">Aug. 31, 1985</td>            <td class="col2">                97.50            </td>        </tr>                <tr>            <td class="col1">July 31, 1985</td>            <td class="col2">                98.30            </td>        </tr>                <tr>            <td class="col1">June 30, 1985</td>            <td class="col2">                98.30            </td>        </tr>                <tr>            <td class="col1">May 31, 1985</td>            <td class="col2">                99.30            </td>        </tr>                <tr>            <td class="col1">April 30, 1985</td>            <td class="col2
# 
# ">                99.90            </td>        </tr>                <tr>            <td class="col1">March 31, 1985</td>            <td class="col2">                101.30            </td>        </tr>                <tr>            <td class="col1">Feb. 28, 1985</td>            <td class="col2">                100.70            </td>        </tr>                <tr>            <td class="col1">Jan. 31, 1985</td>            <td class="col2">                100.10            </td>        </tr>                <tr>            <td class="col1">Dec. 31, 1984</td>            <td class="col2">                99.70            </td>        </tr>                <tr>            <td class="col1">Nov. 30, 1984</td>            <td class="col2">                98.10            </td>        </tr>            </table>    </div><!-- #padR --></div><div class="dataColRt">    <div class="padL">    <table class="histDataTable">        <tr>            <th scope="col" class="col1 colHeadLrg">&nbsp;</th>            <th scope="col" class="col2 colHeadLrg">&nbsp;</th>        </tr>                <tr>            <td class="col1">Oct. 31, 1984</td>            <td class="col2">                98.40            </td>        </tr>                <tr>            <td class="col1">Sept. 30, 1984</td>            <td class="col2">                98.70            </td>        </tr>                <tr>            <td class="col1">Aug. 31, 1984</td>            <td class="col2">                100.10            </td>        </tr>                <tr>            <td class="col1">July 31, 1984</td>            <td class="col2">                99.40            </td>        </tr>                <tr>            <td class="col1">June 30, 1984</td>            <td class="col2">                99.40            </td>        </tr>                <tr>            <td class="col1">May 31, 1984</td>            <td class="col2">                99.50            </td>        </tr>                <tr>            <td class="col1">April 30, 1984</td>            <td class="col2">                99.50            </td>        </tr>                <tr>            <td class="col1">March 31, 1984</td>            <td class="col2">                98.10            </td>        </tr>                <tr>            <td class="col1">Feb. 29, 1984</td>            <td class="col2">                97.70            </td>        </tr>                <tr>            <td class="col1">Jan. 31, 1984</td>            <td class="col2">                97.50            </td>        </tr>                <tr>            <td class="col1">Dec. 31, 1983</td>            <td class="col2">                94.20            </td>        </tr>                <tr>            <td class="col1">Nov. 30, 1983</td>            <td class="col2">                94.10            </td>        </tr>                <tr>            <td class="col1">Oct. 31, 1983</td>            <td class="col2">                95.10            </td>        </tr>                <tr>            <td class="col1">Sept. 30, 1983</td>            <td class="col2">                95.90            </td>        </tr>                <tr>            <td class="col1">Aug. 31, 1983</td>            <td class="col2">                96.40            </td>        </tr>                <tr>            <td class="col1">July 31, 1983</td>            <td class="col2">                97.90            </td>        </tr>                <tr>            <td class="col1">June 30, 1983</td>            <td class="col2">                100.80            </td>        </tr>                <tr>            <td class="col1">May 31, 1983</td>            <td class="col2">                103.10            </td>        </tr>                <tr>            <td class="col1">April 30, 1983</td>            <td class="col2">                105.10            </td>        </tr>                <tr>            <td class="col1">March 31, 1983</td>            <td class="col2">                106.90            </td>        </tr>                <tr>            <td class="col1">Feb. 28, 1983</td>            <td class="col2">                106.90            </td>        </tr>                <tr>            <td class="col1">Jan. 31, 1983</td>            <td class="col2">                106.00            </td>        </tr>                <tr>            <td class="col1">Dec. 31, 1982</td>            <td class="col2">                105.50            </td>        </tr>                <tr>            <td class="col1">Nov. 30, 1982</td>            <td class="col2">                107.00            </td>        </tr>                <tr>            <td class="col1">Oct. 31, 1982</td>            <td class="col2">                106.90            </td>        </tr>            </table>    </div><!-- #padR -->    </div><div class="clear"></div>"
# 
# A table formatted in JSON. What is interesting about the repsonse is that it describes the last page number. By changing each pageNum argument, the response points me directly to that part of the table. With this information in mind, the resulting functions for Y Charts and Index Mundi are described below. 
# 
# 

# In[123]:

#Trying to import the jason
url_import = 'https://ycharts.com/indicators/us_consumer_price_index_pork.json?endDate=01/31/2016&pageNum=7&startDate=01/31/1947'
text1 = requests.get(url_import).text
text1


# ...And that is what can happen. The Y Charts website requires the user to register to the site. Despite the fact that I am an active member of the site and signed into the site on this browser, the Y Charts website will not allow me to see any page of the table beyond the first. For the moment, I have two choices:
#     A - manually pull each link and each page (13 tables with nearly 30 pages for each table)
#     B - try to feed my password cookie into a function and see if I can still pull the data. 
# 
# Choice B seems well worth it. We will give it a try, but not right now. Let's move onto Index Mundi. 

# In[32]:

#For Index Mundi, beef values:
from bs4 import BeautifulSoup
import requests
response = requests.get('http://www.indexmundi.com/commodities/?commodity=beef&months=360').text

soup = BeautifulSoup(response)
Mundi =  open("results.txt","w")
table = soup.find_all('table', class_="tblData")
souptable = table[0]
souptable


# In[51]:

type(souptable)


# In[49]:

soup_string = str(souptable)
#converting bs4 element into string


# In[64]:

import requests
from bs4 import BeautifulSoup
Date=[]
Price=[]
Change=[]

for i in souptable.findAll('tr'):
    td = i.findAll('td')
    for s, p in enumerate(td):
        if s==0:
            Date.append(p.text)
        if s==1:
            Price.append(p.text)
        if s==2:
            Change.append(p.text)
print Price
print type (Price)


# In[67]:

#Right now, the above list is a list of unicode strings. 
Date = [item.encode('utf-8') for item in Date]
Price = [item.encode('utf-8') for item in Price]
Change = [item.encode('utf-8') for item in Change]
print Price


# In[80]:

Price2 = map(lambda item: float(item),Price)
print Price2
#Now, the below price values -- which are the most important -- are integers. 


# In[87]:

CPI_beef = pd.DataFrame({'Date' : Date,
 'Price' : Price,
 'Change':Change
  })


# In[88]:

print CPI_beef


# In[89]:

#The above data frame is good to go for exporting!
import csv
CPI_beef.to_csv("CPI_beef", sep='\t')


# The above process can be applied for the remaining products necessary for analysis from Index Mundi: Coconut Oil, Palm Oil, and Pork(swine). 

# In[109]:

#For Pork(swine)
from bs4 import BeautifulSoup
import requests
response = requests.get('http://www.indexmundi.com/commodities/?commodity=pork&months=360').text
soup = BeautifulSoup(response)
Mundi =  open("results.txt","w")
table = soup.find_all('table', class_="tblData")
souptable = table[0]
souptable


# In[113]:

soup_string = str(souptable)


# In[114]:

import requests
from bs4 import BeautifulSoup
Date_Pork =[]
Price_Pork =[]
Change_Pork =[]

for i in souptable.findAll('tr'):
    td = i.findAll('td')
    for s, p in enumerate(td):
        if s==0:
            Date_Pork.append(p.text)
        if s==1:
            Price_Pork.append(p.text)
        if s==2:
            Change_Pork.append(p.text)


# In[115]:

#Right now, the above list is a list of unicode strings. 
Date_Pork = [item.encode('utf-8') for item in Date_Pork]
Price_Pork = [item.encode('utf-8') for item in Price_Pork]
Change_Pork = [item.encode('utf-8') for item in Change_Pork]


# In[118]:

Price_Pork2 = map(lambda item: float(item),Price_Pork)


# In[119]:

CPI_Pork = pd.DataFrame({'Date_Pork' : Date_Pork,
 'Price_Pork2' : Price_Pork,
 'Change_Pork':Change_Pork
  })
print(CPI_Pork)


# In[120]:

import csv
CPI_Pork.to_csv("CPI_Pork", sep='\t')


# In[129]:

#For Coconut Oil
response = requests.get('http://www.indexmundi.com/commodities/?commodity=coconut-oil&months=360').text

soup = BeautifulSoup(response)
Mundi =  open("results.txt","w")
table = soup.find_all('table', class_="tblData")
souptable = table[0]
soup_string = str(souptable)

Date_Coco =[]
Price_Coco =[]
Change_Coco =[]

for i in souptable.findAll('tr'):
    td = i.findAll('td')
    for s, p in enumerate(td):
        if s==0:
            Date_Coco.append(p.text)
        if s==1:
            Price_Coco.append(p.text)
        if s==2:
            Change_Coco.append(p.text)

print Change_Coco


# In[132]:


Date_Coco = [item.encode('utf-8') for item in Date_Coco]
Price_Coco = [item.encode('utf-8') for item in Price_Coco]
Change_Coco = [item.encode('utf-8') for item in Change_Coco]

print Price_Coco



# In[ ]:

CPI_Coco = pd.DataFrame({'Date_Coco' : Date_Coco,
 'Price_Coco2' : Price_Coco,
 'Change_Coco':Change_Coco
  })
print(CPI_Coco)


# In[134]:

CPI_Coco = pd.DataFrame({'Date_Coco' : Date_Coco,
 'Price_Coco2' : Price_Coco,
 'Change_Coco':Change_Coco
  })
print(CPI_Coco)


# In[135]:

CPI_Coco.to_csv("CPI_Coco", sep='\t')


# In[137]:

#For Palm Oil
response = requests.get('http://www.indexmundi.com/commodities/?commodity=palm-oil&months=360').text

soup = BeautifulSoup(response)
Mundi =  open("results.txt","w")
table = soup.find_all('table', class_="tblData")
souptable = table[0]
soup_string = str(souptable)

Date_Palm =[]
Price_Palm =[]
Change_Palm =[]

for i in souptable.findAll('tr'):
    td = i.findAll('td')
    for s, p in enumerate(td):
        if s==0:
            Date_Palm.append(p.text)
        if s==1:
            Price_Palm.append(p.text)
        if s==2:
            Change_Palm.append(p.text)
            
Date_Palm = [item.encode('utf-8') for item in Date_Palm]
Price_Palm = [item.encode('utf-8') for item in Price_Palm]
Change_Palm = [item.encode('utf-8') for item in Change_Palm]

CPI_Palm = pd.DataFrame({'Date_Palm' : Date_Palm,
 'Price_Palm' : Price_Palm,
 'Change_Palm':Change_Palm
  })

print CPI_Palm


# In[138]:

CPI_Palm.to_csv("CPI_Palm", sep='\t')


# In[ ]:



