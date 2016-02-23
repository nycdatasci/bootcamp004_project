from bs4 import BeautifulSoup
import requests
import re
import urllib2
from bs4 import BeautifulSoup

image_type ="animation" #"comedy" # "action"
query = "animation Movie Posters"#"2014 Comdey Movie Posters" #"2014 Action Movie Posters"
url = "http://www.bing.com/images/search?q=" + query + "&qft=+filterui:color2-bw+filterui:imagesize-large&FORM=R5IR3"

page = urllib2.urlopen("http://www.bing.com/images/search?q=animation%20Movie%20Posters&qs=n&form=QBIR&pq=animation%20movie%20posters&sc=4-23&sp=-1&sk=").read()
soup = BeautifulSoup(page)
images = [a['src'] for a in soup.find_all("img", {"src": re.compile("mm.bing.net")})]


counter = 0
for img in images:
    with open(image_type + "_"+ str(counter)+"."+"jpg",'wb') as f:
        f.write(urllib2.urlopen(img).read())
    counter += 1

image_type ="comedy" # "action"
query = "Comdey Movie Posters" #"2014 Action Movie Posters"
url = "http://www.bing.com/images/search?q=" + query + "&qft=+filterui:color2-bw+filterui:imagesize-large&FORM=R5IR3"

page = urllib2.urlopen("http://www.bing.com/images/search?q=Comdey%20Movie%20Posters&qs=n&form=QBIR&pq=comdey%20movie%20posters&sc=1-20&sp=-1&sk=").read()
soup = BeautifulSoup(page)
images = [a['src'] for a in soup.find_all("img", {"src": re.compile("mm.bing.net")})]


counter = 0
for img in images:
    with open(image_type + "_"+ str(counter)+"."+"jpg",'wb') as f:
        f.write(urllib2.urlopen(img).read())
    counter += 1

image_type = "action"
query = "Action Movie Posters"
url = "http://www.bing.com/images/search?q=" + query + "&qft=+filterui:color2-bw+filterui:imagesize-large&FORM=R5IR3"

page = urllib2.urlopen("http://www.bing.com/images/search?q=Action%20Movie%20Posters&qs=n&form=QBIR&pq=action%20movie%20posters&sc=3-20&sp=-1&sk=").read()
soup = BeautifulSoup(page)
images = [a['src'] for a in soup.find_all("img", {"src": re.compile("mm.bing.net")})]


counter = 0
for img in images:
    with open(image_type + "_"+ str(counter)+"."+"jpg",'wb') as f:
        f.write(urllib2.urlopen(img).read())
    counter += 1

image_type = "Horror"
query = "2014 Horror Movie Posters"
url = "http://www.bing.com/images/search?q=" + query + "&qft=+filterui:color2-bw+filterui:imagesize-large&FORM=R5IR3"

page = urllib2.urlopen("http://www.bing.com/images/search?q=2014%20Horror%20Movie%20Posters&qs=n&form=QBIR&pq=2014%20horror%20movie%20posters&sc=1-25&sp=-1&sk=").read()
soup = BeautifulSoup(page)
images = [a['src'] for a in soup.find_all("img", {"src": re.compile("mm.bing.net")})]


counter = 0
for img in images:
    with open(image_type + "_"+ str(counter)+"."+"jpg",'wb') as f:
        f.write(urllib2.urlopen(img).read())
    counter += 1