#!/usr/bin/env python

import sys
import os

season = int(sys.argv[1])

for year in (season, season+1):
    months = range(9, 13) if season == year else range(1, 8)
    for month in months:
        for day in range(1, 32):
            os.system('scrapy crawl nba -a scrape_date=%04d%02d%02d' % (year, month, day))