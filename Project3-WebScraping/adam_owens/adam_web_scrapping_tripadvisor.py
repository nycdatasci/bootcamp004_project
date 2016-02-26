# -*- coding: utf-8 -*-
"""
Created on Sun Feb 21 21:11:24 2016

@author: adamowens
"""

import os
import sys
import time
import argparse
import logging
import requests
from BeautifulSoup import BeautifulSoup
import re
import pandas as pd
from html import HTML
h = HTML()

#################################################################################
log = logging.getLogger(__name__)
log.setLevel(logging.INFO)
loghandler = logging.StreamHandler(sys.stderr)
loghandler.setFormatter(logging.Formatter("[%(asctime)s] %(message)s"))
log.addHandler(loghandler)

base_url = "http://www.tripadvisor.com/"
user_agent = "Mozilla/44.0.2 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.109 Safari/9.0.2"
df = pd.DataFrame()

def get_city_page(city, state, datadir):
    """ Returns the URL of the list of the hotels in a city.
    Parameters
    ----------
    city : str
    state : str
    datadir : str
    Returns
    -------
    url : str
        The relative link to the website with the hotels list.
    """
    # Build the request URL
    url = base_url + "city=" + city + "&state=" + state
    # Request the HTML page
    print url
    headers = {'User-Agent': user_agent}
    response = requests.get(url, headers=headers)
    html = response.text.encode('utf-8')
    with open(os.path.join(datadir, city + '-tourism-page.html'), "w") as h:
        h.write(html)

    # Use BeautifulSoup to extract the url for the list of hotels in
    # the city and state.

    soup = BeautifulSoup(html)
    li = soup.find("li", {"class": "hotels twoLines"})
    city_url = li.find('a', href=True)
    return city_url['href']

def get_hotellist_page(city_url, page_count, city, datadir='data/'):
    """ Returns the hotel list HTML. The URL of the list is the result of
    get_city_page(). Also, saves a copy of the HTML to the disk. 
    Parameters
    ----------
    city_url : str
        The relative URL of the hotels in the city we are interested in.
    page_count : int
        The page that we want to fetch. Used for keeping track of our progress.
    city : str
        The name of the city that we are interested in.
    datadir : str, default is 'data/'
        The directory in which to save the downloaded html.
    Returns
    -------
    html : str
        The HTML of the page with the list of the hotels.
    """
    if city_url == None:
        sys.exit()
    url = base_url + city_url
    # Sleep 2 sec before starting a new http request
    time.sleep(2)
    # Request page
    headers = { 'User-Agent' : user_agent }
    response = requests.get(url, headers=headers)
    html = response.text.encode('utf-8')
    # Save the webpage
    with open(os.path.join(datadir, city + '-hotelist-' + str(page_count) + '.html'), "w") as h:
        h.write(html)
    return html

def parse_hotellist_page(city, state, html):
    """Parses the website with the hotel list and prints the hotel name, the
    number of stars and the number of reviews it has. If there is a next page
    in the hotel list, it returns a list to that page. Otherwise, it exits the
    script. 
    """
    soup = BeautifulSoup(html)
    #set global dataframe   
    global df

    # Extract hotel name, star rating and number of reviews
    hotel_boxes = soup.findAll('div', {'class' :'prw_rup prw_meta_short_cell_listing'})
    if not hotel_boxes:
        log.info("#################################### Option 2 ######################################")
        hotel_boxes = soup.findAll('div', {'class' :' listing_info jfy'})

    if not hotel_boxes:
        log.info("#################################### Option 3 ######################################")
        hotel_boxes = soup.findAll('div', {'class' :'listing easyClear  p13n_imperfect'})
   
    print 'the number of hotels found is ' + str(len(hotel_boxes))
    
    for hotel_box in hotel_boxes:
        #Scrapte Hotel Name
        hotel_name = hotel_box.find("a", {"target" : "_blank"}).find(text=True)
        log.info("Hotel name: %s" % hotel_name.strip())
        #Scrape City Rank
        city_rankings = hotel_box.find("div", {"class" : "slim_ranking"})
        if city_rankings != None:
            city_rankings = city_rankings.find(text=True)
            city_rankings = city_rankings[0:-1]            
            city_rank = int(re.search(r'\d+', city_rankings).group())            
        #Scrape Ad Features
        htl_tags = hotel_box.findAll("div", {"class" : "clickable_tags"})
        if htl_tags != None:        
            for features in htl_tags:
                htl_features = features.find("span", {"class" : "tag"})
                if htl_features != None:
                    htl_features = htl_features.find(text=True)
        #Scrape Stars
        stars = hotel_box.find("img", {"class" : "sprite-ratings"})
        if stars:
            log.info("Stars: %s" % stars['alt'].split()[0])
            stars = "%s" % stars['alt'].split()[0]
        #Scrape Review Coount  
        num_reviews = hotel_box.find("span", {'class': "more"})
        if num_reviews != None:
            num_reviews = num_reviews.find(text=True)
            num_reviews = num_reviews[0:-1]

        else: num_reviews = ""
        num_reviews = re.sub(',','', num_reviews)
        num_reviews = re.sub('Review','', num_reviews)
        
        if num_reviews != "":
            num_reviews  = int(num_reviews.strip())
        elif num_reviews == '':
            num_reviews = 0
    
        df = df.append({
                    'City': city, 'State': state, 
                    'Hotel_Name': hotel_name, 'Review_Count': num_reviews,
                    'Star_Rating': stars, 'Hotel_Features': htl_features, 
                    'City_Ranking': city_rank}, ignore_index=True)
        #save to file
        df.to_csv('./data/tripadvisors2.csv', encoding='utf-8')
   
   # Get next URL page if exists, otherwise exit
    div = soup.find("div", {"class" : "unified pagination standard_pagination"})

    if div == None:    
        if soup.find('span', {'class' : 'nav next disabled'}):
            log.info("We reached last page")
            sys.exit()
    # If not, return the url to the next page
    hrefs = div.findAll('a', href= True)
    for href in hrefs:
        if href.find(text = True) == 'Next':
            log.info("Next url is %s" % href['href'])
            return href['href']

def scrape_hotels(city, state, datadir='data/'):
    """Runs the main scraper code
    Parameters
    ----------
    city : str
        The name of the city for which to scrape hotels.
    state : str
        The state in which the city is located.
    datadir : str, default is 'data/'
        The directory under which to save the downloaded html.
    """

    # Get current directory
    current_dir = os.getcwd()
    
    # Create datadir if does not exist
    if not os.path.exists(os.path.join(current_dir, datadir)):
        os.makedirs(os.path.join(current_dir, datadir))

    # Get URL to obtaint the list of hotels in a specific city
    city_url = get_city_page(city, state, datadir)
    c = 0
    while(True):
        c += 1
        html = get_hotellist_page(city_url, c, city, datadir)
        city_url = parse_hotellist_page(city, state, html)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Scrape tripadvisor')
    parser.add_argument('-datadir', type=str,
                        help='Directory to store raw html files',
                        default="data/")
    parser.add_argument('-state', type=str,
                        help='State for which the hotel data is required.',
                        required=True)
    parser.add_argument('-city', type=str,
                        help='City for which the hotel data is required.',
                        required=True)

    args = parser.parse_args()
    scrape_hotels(args.city, args.state, args.datadir)

#################################################################################

df_citystate = pd.DataFrame({'City': ("Chicago","New York City","Los Angeles","Salt Lake City"),
                             'State': ("Illinois","New York","California", "Utah")}) 

scrape_hotels("Salt Lake City", "Utah", datadir = 'datascience/data/')
