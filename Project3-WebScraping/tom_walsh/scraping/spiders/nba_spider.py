import scrapy
import re
import time
import json

from scraping.items import PlayByPlay, GameEvent, Lineup


class NbaSpider(scrapy.Spider):
    name = "nba"
    allowed_domains = ["nba.com"]

    lineup_pattern = 'http://stats.nba.com/stats/leaguedashlineups?Conference=&DateFrom=%s&DateTo=%s&Division=&GameID=&GameSegment=&GroupQuantity=5&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=%d&PlusMinus=N&Rank=N&Season=%s&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&TeamID=0&VsConference=&VsDivision='

    def __init__(self, scrape_date=None, *args, **kwargs):
        super(NbaSpider, self).__init__(*args, **kwargs)
        if scrape_date is None:
            scrape_date = str(int(time.strftime('%Y%m%d')) - 1)
        match = re.search('(\d{4})(\d{2})(\d{2})', scrape_date)
        year = int(match.group(1))
        month = int(match.group(2))
        day = int(match.group(3))
        self.date = '%02d%%2F%02d%%2F%04d' % (month, day, year)
        self.season = '%04d-%02d' % ((year, (year+1) % 100) if month > 7 else (year-1, year % 100))
        self.scrape_date = scrape_date
        self.start_urls = ['http://www.nba.com/gameline/%s/' % scrape_date]

    def parse(self, response):
        for href in response.css("a.recapAnc::attr('href')") + response.css("div.nbaFnlMnRecapDiv > a::attr('href')"):
            url = response.urljoin(href.extract())
            yield scrapy.Request(url, callback=self.parse_game_recap)
        for period in range(1,15):
            url = self.lineup_pattern % (self.date, self.date, period, self.season)
            yield scrapy.Request(url, callback=self.parse_lineups)


    def parse_game_recap(self, response):
        away = None
        home = None
        quarter = None
        date = re.search('(\d+)', response.url).group(1)
        game_id = re.search('([A-Z]+)', response.url).group(1)
        print response.url
        print game_id
        pbp_item = PlayByPlay()
        for index, row in enumerate(response.xpath('//div[@id="nbaGIPBP"]//tr')):
            if int(row.xpath('@class="nbaGIPBPTeams"').extract_first()) == 1:
                (away, home) = [x.strip() for x in row.xpath('td/text()').extract()]
            else:
                pbp_item['quarter'] = quarter
                pbp_item['game_id'] = game_id
                pbp_item['index'] = index
                pbp_item['date'] = date
                for field in row.xpath('td'):
                    field_class = str(field.xpath('@class').extract_first())
                    if field_class == 'nbaGIPbPTblHdr':
                        name = row.xpath('td/a/@name')
                        if len(name) > 0:
                            quarter = row.xpath('td/a/@name').extract_first()
                            pbp_item['quarter'] = quarter
                    elif len(field.xpath('@id')) > 0:
                        event_item = GameEvent()
                        event_item['type'] = field.xpath('@id').extract_first()
                        event_item['text'] = field.xpath('div/text()').extract_first()
                        event_item['quarter'] = quarter
                        event_item['game_id'] = game_id
                        event_item['date'] = date
                        event_item['index'] = index
                        yield event_item
                    else:
                        text = field.xpath('text()').extract_first().strip()
                        if len(text) == 0:
                            continue
                        else:
                            if field_class == 'nbaGIPbPLft' or field_class == 'nbaGIPbPLftScore':
                                pbp_item['team'] = away
                                pbp_item['text'] = text
                            elif field_class == 'nbaGIPbPRgt' or field_class == 'nbaGIPbPRgtScore':
                                pbp_item['team'] = home
                                pbp_item['text'] = text
                            elif field_class == 'nbaGIPbPMid':
                                pbp_item['clock'] = text
                            elif field_class == 'nbaGIPbPMidScore':
                                pbp_item['clock'] = text
                                pbp_item['score'] = field.xpath('text()').extract()[1].strip()
                            else:
                                raise ValueError("Unknown class: %s" % field_class)
                if 'clock' in pbp_item:
                    yield pbp_item
                    pbp_item = PlayByPlay()

    def parse_lineups(self, response):
        jsonresponse = json.loads(response.body_as_unicode())
        headers = dict([(i, str(j.lower())) for i, j in enumerate(jsonresponse['resultSets'][0]['headers'])])
        for row in jsonresponse['resultSets'][0]['rowSet']:
            item = Lineup()
            item['date'] = self.scrape_date
            item['period'] = int(re.search('Period=(\d+)', response.url).group(1))
            for index, value in enumerate(row):
                field = headers[index]
                item[field] = value
            yield item



