###Yankees Promotions Scraper###

import csv
import dryscrape
from bs4 import BeautifulSoup
sess = dryscrape.Session()
urls = ['http://newyork.yankees.mlb.com/schedule/promotions.jsp?c_id=nyy&y=2009','http://newyork.yankees.mlb.com/schedule/promotions.jsp?c_id=nyy&y=2010',
'http://newyork.yankees.mlb.com/schedule/promotions.jsp?c_id=nyy&y=2011', 'http://newyork.yankees.mlb.com/schedule/promotions.jsp?c_id=nyy&y=2012', 
'http://newyork.yankees.mlb.com/schedule/promotions.jsp?c_id=nyy&y=2013', 'http://newyork.yankees.mlb.com/schedule/promotions.jsp?c_id=nyy&y=2014',
'http://newyork.yankees.mlb.com/schedule/promotions.jsp?c_id=nyy&y=2015', 'http://newyork.yankees.mlb.com/schedule/promotions.jsp?c_id=nyy&y=2016']

all_tables = []
for url in urls:
	sess.visit(url)
	response = sess.body()
	soup = BeautifulSoup(response, "html.parser")
	tables = soup.findAll('table', {'class': 'data_grid'})
	all_tables.append(tables)

list_of_rows = []
for year in all_tables:
	for table in year:
		for row in table.findAll('tr'):
			list_of_cells = []
			for cell in row.findAll('td'):
				text = cell.text.replace('&nbsp;', '').encode('ascii', 'ignore')
				list_of_cells.append(text)
			list_of_rows.append(list_of_cells)

outfile = open('./NYY.csv', 'wb')
writer = csv.writer(outfile)
writer.writerows(list_of_rows)

