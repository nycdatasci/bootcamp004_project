# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

import scrapy
import re


class PlayByPlay(scrapy.Item):
    game_id = scrapy.Field()
    quarter = scrapy.Field()
    period = scrapy.Field()
    clock = scrapy.Field()
    score = scrapy.Field()
    team = scrapy.Field()
    text = scrapy.Field()
    index = scrapy.Field()
    date = scrapy.Field()
    events = scrapy.Field()
    seconds = scrapy.Field()
    team_abbreviation = scrapy.Field()

    def index_fields(self):
        return {
            'game_id': self['game_id'],
            'index': self['index'],
            'quarter': self['quarter'],
            'date': self['date']
         }


class GameEvent(scrapy.Item):
    type = scrapy.Field()
    text = scrapy.Field()
    quarter = scrapy.Field()
    period = scrapy.Field()
    game_id = scrapy.Field()
    index = scrapy.Field()
    date = scrapy.Field()
    events = scrapy.Field()
    clock = scrapy.Field()
    seconds = scrapy.Field()
    team_abbreviation = scrapy.Field()

    def index_fields(self):
        return {
            'game_id': self['game_id'],
            'index': self['index'],
            'quarter': self['quarter'],
            'date': self['date']
         }


class Lineup(scrapy.Item):
    group_set = scrapy.Field()
    group_id = scrapy.Field()
    group_name = scrapy.Field()
    team_id = scrapy.Field()
    team_abbreviation = scrapy.Field()
    gp = scrapy.Field()
    w = scrapy.Field()
    l = scrapy.Field()
    w_pct = scrapy.Field()
    min = scrapy.Field()
    fgm = scrapy.Field()
    fga = scrapy.Field()
    fg_pct = scrapy.Field()
    fg3m = scrapy.Field()
    fg3a = scrapy.Field()
    fg3_pct = scrapy.Field()
    ftm = scrapy.Field()
    fta = scrapy.Field()
    ft_pct = scrapy.Field()
    oreb = scrapy.Field()
    dreb = scrapy.Field()
    reb = scrapy.Field()
    ast = scrapy.Field()
    tov = scrapy.Field()
    stl = scrapy.Field()
    blk = scrapy.Field()
    blka = scrapy.Field()
    pf = scrapy.Field()
    pfd = scrapy.Field()
    pts = scrapy.Field()
    plus_minus = scrapy.Field()
    period = scrapy.Field()
    date = scrapy.Field()

    def index_fields(self):
        return {
            'group_id': self['group_id'],
            'team_id': self['team_id'],
            'date': self['date'],
            'period': self['period']
         }

