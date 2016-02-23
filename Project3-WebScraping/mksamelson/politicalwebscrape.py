# -*- coding: utf-8 -*-
"""
Created on Wed Feb 17 20:43:59 2016

@author: Matt
"""
import numpy as np
import pandas as pd
import re
import mechanize
import cookielib
from bs4 import BeautifulSoup

# Browser
br = mechanize.Browser()

# Cookie Jar
cj = cookielib.LWPCookieJar()
br.set_cookiejar(cj)

# Browser options
br.set_handle_equiv(True)
br.set_handle_gzip(True)
br.set_handle_redirect(True)
br.set_handle_referer(True)
br.set_handle_robots(False)

# Follows refresh 0 but not hangs on refresh > 0
br.set_handle_refresh(mechanize._http.HTTPRefreshProcessor(), max_time=1)

# Want debugging messages?
#br.set_debug_http(True)
#br.set_debug_redirects(True)
#br.set_debug_responses(True)

# User-Agent (this is cheating, ok?)
br.addheaders = [('User-agent', 'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.1) Gecko/2008071615 Fedora/3.0.1-1.fc9 Firefox/3.0.1')]


# Open Opensecrets.org Senate & Congress Summary Page

r = br.open('http://www.opensecrets.org/politicians/summary_all.php')
html = r.read()

soup = BeautifulSoup(html)
#print type(soup)
page = soup.prettify()
#print soup.prettify()

#soup.find_all(lambda tag: tag.get('class') == ['event-title'])
titleTags = soup.find_all('td')
#print(titleTags)

#Extract Financial Data (Raised, Spent, Cash on Hand, Debts) from Table
#and lay out in a 539 x 4 dataframe

table = soup.find('table', {'id': 'data'})
financial_info = table.find_all(lambda tag: tag.get('class') == ['number'])
financial_info = map(lambda x: x.get_text(),financial_info)
financial_info = np.array(financial_info)
financial_info = pd.DataFrame(financial_info.reshape((539,4)))


# Extract Column Names from Table Into a list for Subsequent Naming 
#of DataFrame

headers = soup.find_all('thead')
headers = [tag.get_text() for tag in headers]
headers = str(headers) 
pattern = r'([A-Z][a-z]*\s[a-z]*\s[A-Z][a-z]*|[A-Z][a-z]*)'
headers = re.findall(pattern, headers)


#Extract Politician Name from Table

name = table.find_all('a')
name = map(lambda x: x.get_text(),name)
name = np.array(name)
name = pd.DataFrame(name.reshape((539,1)))


# Extract link to individual politician page
# summary table
politician_code_id = []
for link in table.find_all('a'):
    politician_code_id.append(link.get('href'))
politician_code_id = pd.DataFrame(politician_code_id)

#Extract Chamber and Party from Table

#table = soup.find('table', {'id': 'data'})
party_chamber = table.find_all(lambda tag: tag.get('class') == ['center'])
party_chamber = map(lambda x: x.get_text(),party_chamber)
party_chamber = np.array(party_chamber)
party_chamber = pd.DataFrame(party_chamber.reshape((539,2)))


#Extract Politician State from Table

table2 = str(table)
state = []
#pattern = '<td>\w+\s?\w*</td>'
pattern = '<td>\w+.*</td>'
state = re.findall(pattern, table2)
for i in range(len(state)):
    state[i] = re.sub('<td>|</td>','',state[i])
state = pd.DataFrame(np.array(state).reshape((539,1)))    
    
#Concatonate DataFrames Into A summary_df    
    
summary_df = pd.concat([name, state, party_chamber,financial_info], axis=1)  
summary_df.columns = headers 

#Generate urls for politician-specific pages and attache to summary_df

for i in range(len(politician_code_id)):
    m = re.search(r"cid=\w[0-9]+", politician_code_id.iloc[i,0])
    politician_code_id.iloc[i,0] = m.group(0) 

politician_code_id2 = "http://www.opensecrets.org/politicians/contrib.php?" + politician_code_id + "&cycle=2016&type=C&newMem=N&recs=100"

summary_df.to_csv('politiciansummary.csv', sep=',')





#-------------------------------------

#r2 = br.open('http://www.opensecrets.org/politicians/contrib.php?cid=N00029127&cycle=2016&type=C&newMem=N&recs=100')
#html2 = r2.read()
#
#soup2 = BeautifulSoup(html2)
##print type(soup)
#page2 = soup2.prettify()
#one_pol_table = soup2.table
#print one_pol_table

#<table class="tablesorter" id="topContrib" style="max-width:600px;font-size:11px;">

#Show the source
#print html
## or
#print br.response().read()
#
## Show the html title
#print br.title()
#
## Show the response headers
#print r.info()
## or
#print br.response().info()
#
## Show the available forms
#for f in br.forms():
#    print f

## Select the first (index zero) form
#br.select_form(nr=0)
#
## Let's search
#br.form['q']='weekend codes'
#br.submit()
#print br.response().read()
#
## Looking at some results in link format
#for l in br.links(url_regex='stockrt'):
#   print l
