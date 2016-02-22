# -*- coding: utf-8 -*-

import pymongo
import re
# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: http://doc.scrapy.org/en/latest/topics/item-pipeline.html

class ScrapingPipeline(object):
    def process_item(self, item, spider):
        return item

class QuarterProcessor(object):
    def process_item(self, item, spider):
        if 'quarter' in item:
            m = re.match('(Q|OT|H)(\d+)', item['quarter'])
            if m.group(1) in ('Q', 'H'):
                item['period'] = int(m.group(2))
            elif m.group(1) == 'OT':
                item['period'] = int(m.group(2)) + 4
            else:
                raise ValueError("Can't process quarter: %s" % item['quarter'])
        return item

class ClockProcessor(object):
    def process_item(self, item, spider):
        if 'clock' in item:
            (minutes, seconds) = item['clock'].split(':')
            item['seconds'] = float(minutes) * 60.0 + float(seconds)
        return item

class TextProcessor(object):
    SHOT_RE = re.compile('(.+?) (((Tip|Alley Oop|Cutting|Dunk|Pullup|Turnaround|Running|Driving|Hook|Jump|3pt|Layup|Fadeaway|Bank|No) ?)+) [Ss]hot: (Made|Missed)( \((\d+) PTS\))?')
    REBOUND_RE = re.compile('(.+?) Rebound \(Off:(\d+) Def:(\d+)\)')
    TEAM_REBOUND_RE = re.compile('Team Rebound')
    DEFENSE_RE = re.compile('(Block|Steal): ?(.+?) \((\d+) (BLK|ST)\)')
    ASSIST_RE = re.compile('Assist: (.+?) \((\d+) AST\)')
    TIMEOUT_RE = re.compile('Team Timeout : (Short|Regular|No Timeout|Official)')
    TURNOVER_RE = re.compile('(.+?) Turnover : ((Out of Bounds|Poss)? ?(- )?(Out Of Bounds|Basket from Below|Illegal Screen|No|Swinging Elbows|Double Dribble|Illegal Assist|Inbound|Palming|Kicked Ball|Jump Ball|Lane|Backcourt|Offensive Goaltending|Discontinue Dribble|Lost Ball|Foul|Bad Pass|Traveling|Step Out of Bounds|3 Second|Offensive Foul|Player Out of Bounds)( Violation)?( Turnover)?) \((\d+) TO\)')
    TEAM_TURNOVER_RE = re.compile('Team Turnover : ((8 Second Violation|5 Sec Inbound|Backcourt|Shot Clock|Offensive Goaltending|3 Second)( Violation)?( Turnover)?)')
    FOUL_RE = re.compile('(.+?) Foul: (Clear Path|Flagrant|Away From Play|Personal Take|Inbound|Loose Ball|Offensive|Offensive Charge|Personal|Shooting|Personal Block|Shooting Block|Defense 3 Second)( Type (\d+))? \((\d+) PF\)( \(\d+ FTA\))? \((.+?)\)')
    JUMP_RE = re.compile('Jump Ball (.+?) vs (.+)( \((.+?) gains possession\))?')
    VIOLATION_RE = re.compile('(.+?) Violation:(Defensive Goaltending|Kicked Ball|Lane|Jump Ball|Double Lane)( \((.+?)\))?')
    FREE_THROW_RE = re.compile('(.+?) Free Throw (Flagrant|Clear Path)? ?(\d) of (\d) (Missed)? ?(\((\d+) PTS\))?')
    TECHNICAL_FT_RE = re.compile('(.+?) Free Throw Technical (Missed)? ?(\((\d+) PTS\))?')
    SUB_RE = re.compile('(.+?) Substitution replaced by (.+?)$')
    TEAM_VIOLATION_RE = re.compile('Team Violation : (Delay Of Game) \((.+?)\)')
    CLOCK_RE = re.compile('\((\d+:\d+)\)')
    TEAM_RE = re.compile('\[([A-Z]+)\]')
    TECHNICAL_RE = re.compile('(.+?) Technical (- )?([A-Z]+)? ?\((.+?)\)')
    DOUBLE_TECH_RE = re.compile('Double Technical - (.+?), (.+?) \((.+?)\)')
    DOUBLE_FOUL_RE = re.compile('Foul : (Double Personal) - (.+?) \((\d+) PF\), (.+?) \((\d+) PF\) \((.+?)\)')
    EJECTION_RE = re.compile('(.+?) Ejection:(First Flagrant Type 2|Second Technical)')

    # pts, tov, fta, pf, blk, reb, blka, ftm, fg3a, pfd, ast, fg3m, fgm, dreb, fga, stl, oreb

    def process_item(self, item, spider):
        text = item.get('text', None)
        if text:
            item['events'] = []
        while text:
            l = len(text)
            m = self.SHOT_RE.match(text)
            if m:
                event = {'player': m.group(1), 'fga': 1, 'type': m.group(2)}
                if '3pt' in m.group(2):
                    event['fg3a'] = 1
                    if m.group(5) == 'Made':
                        event['fg3m'] = 1
                        event['fgm'] = 1
                        event['pts'] = 3
                else:
                    if m.group(5) == 'Made':
                        event['fg3m'] = 1
                        event['fgm'] = 1
                        event['pts'] = 2
                item['events'].append(event)
                text = text[m.end():].strip()
            m = self.REBOUND_RE.match(text)
            if m:
                event = {'player': m.group(1), 'reb': 1}
                item['events'].append(event)
                text = text[m.end():].strip()
            m = self.DEFENSE_RE.match(text)
            if m:
                event = {'player': m.group(2)}
                if m.group(1) == 'Block':
                    item['events'][-1]['blka'] = 1
                    event['blk'] = 1
                else:
                    event['stl'] = 1
                item['events'].append(event)
                text = text[m.end():].strip()
            m = self.ASSIST_RE.match(text)
            if m:
                event = {'player': m.group(1), 'ast': 1}
                item['events'].append(event)
                text = text[m.end():].strip()
            m = self.TIMEOUT_RE.match(text)
            if m:
                event = {'timeout': m.group(1)}
                item['events'].append(event)
                text = text[m.end():].strip()
            m = self.TURNOVER_RE.match(text)
            if m:
                event = {'player': m.group(1), 'tov': 1, 'note': m.group(2)}
                item['events'].append(event)
                text = text[m.end():].strip()
            m = self.TEAM_TURNOVER_RE.match(text)
            if m:
                event = {'turnover': m.group(1)}
                item['events'].append(event)
                text = text[m.end():].strip()
            m = self.TEAM_REBOUND_RE.match(text)
            if m:
                item['events'].append({'rebound': 'team'})
                text = text[m.end():].strip()
            m = self.FOUL_RE.match(text)
            # TODO: Are all of these actual personal fouls?
            if m:
                event = {'player': m.group(1), 'pf': 1, 'note': m.group(2)}
                if m.group(4):
                    event['type'] = m.group(4)
                item['events'].append(event)
                text = text[m.end():].strip()
            m = self.DOUBLE_FOUL_RE.match(text)
            if m:
                item['events'].append({'player': m.group(2), 'pf': 1, 'note': m.group(1), 'against': m.group(3)})
                item['events'].append({'player': m.group(3), 'pf': 1, 'note': m.group(1), 'against': m.group(2)})
                text = text[m.end():].strip()
            m = self.JUMP_RE.match(text)
            if m:
                item['events'].append({'player': m.group(1), 'jump': 'home'})
                item['events'].append({'player': m.group(2), 'jump': 'away'})
                if m.group(3):
                    item['events'].append({'player': m.group(4), 'jump': 'possession'})
                text = text[m.end():].strip()
            m = self.VIOLATION_RE.match(text)
            if m:
                event = {'player': m.group(1), 'violation': m.group(2)}
                item['events'].append(event)
                text = text[m.end():].strip()
            m = self.FREE_THROW_RE.match(text)
            if m:
                event = {'player': m.group(1), 'fta': 1, 'attempt': m.group(3), 'of': m.group(4)}
                if m.group(5) is None:
                    event['pts'] = 1
                    event['ftm'] = 1
                if m.group(2):
                    event['special'] = m.group(2)
                item['events'].append(event)
                text = text[m.end():].strip()
            m = self.TECHNICAL_FT_RE.match(text)
            if m:
                event = {'player': m.group(1), 'fta': 1, 'ftm': 1, 'special': 'Technical'}
                if m.group(2) is None:
                    event['pts'] = 1
                    event['ftm'] = 1
                item['events'].append(event)
                text = text[m.end():].strip()
            m = self.SUB_RE.match(text)
            if m:
                item['events'].append({'player': m.group(1), 'sub': 'out'})
                item['events'].append({'player': m.group(2), 'sub': 'in'})
                text = text[m.end():].strip()
            m = self.TEAM_VIOLATION_RE.match(text)
            if m:
                item['events'].append({'violation': m.group(1)})
                text = text[m.end():].strip()
            m = self.CLOCK_RE.match(text)
            if m:
                item['clock'] = m.group(1)
                text = text[m.end():].strip()
            m = self.TEAM_RE.match(text)
            if m:
                item['team_abbreviation'] = m.group(1)
                text = text[m.end():].strip()
            m = self.TECHNICAL_RE.match(text)
            if m:
                if m.group(3):
                    item['events'].append({'team': m.group(3), 'technical': m.group(1)})
                else:
                    item['events'].append({'player': m.group(1), 'technical': True})
                text = text[m.end():].strip()
            m = self.DOUBLE_TECH_RE.match(text)
            if m:
                item['events'].append({'player': m.group(1), 'technical': True})
                item['events'].append({'player': m.group(2), 'technical': True})
                text = text[m.end():].strip()
            m = self.EJECTION_RE.match(text)
            if m:
                item['events'].append({'player': m.group(1), 'ejection': True, 'note': m.group(2)})
                text = text[m.end():].strip()

            if len(text) == l:
                raise ValueError('Could not parse text: %s' % text)
            if len(text) == 0:
                text = None

        return item

#TODO, figure out offensive/defensive rebounds... we need to know teams for that

class MongoPipeline(object):

    def __init__(self, mongo_uri, mongo_db):
        self.mongo_uri = mongo_uri
        self.mongo_db = mongo_db

    @classmethod
    def from_crawler(cls, crawler):
        return cls(
            mongo_uri=crawler.settings.get('MONGO_URI'),
            mongo_db=crawler.settings.get('MONGO_DATABASE', 'items')
        )

    def open_spider(self, spider):
        self.client = pymongo.MongoClient(self.mongo_uri)
        self.db = self.client[self.mongo_db]

    def close_spider(self, spider):
        self.client.close()

    def process_item(self, item, spider):
        self.db[item.__class__.__name__].replace_one(item.index_fields(), dict(item), True)
        return item

# class LineupMatchingPipeline(object):
#
#     def __init__(self, mongo_uri, mongo_db):
#         self.mongo_uri = mongo_uri
#         self.mongo_db = mongo_db
#
#     @classmethod
#     def from_crawler(cls, crawler):
#         return cls(
#             mongo_uri=crawler.settings.get('MONGO_URI'),
#             mongo_db=crawler.settings.get('MONGO_DATABASE', 'items')
#         )
#
#     # def process_item(self, item, spider):
#     #     return item
#
#     def close_spider(self, spider):
#         self.client = pymongo.MongoClient(self.mongo_uri)
#         self.db = self.client[self.mongo_db]
#         game_ids = self.db.GameEvent.find({'date': spider.scrape_date}).distinct('game_id')
#         for game_id in game_ids:
#             away = game_id[:3]
#             home = game_id[3:]
#             periods = self.db.Lineup.find({'date': str(spider.scrape_date), 'team_abbreviation': {"$in": [home, away]}}).distinct('period')
#             periods.sort()
#             for period in periods:
#                 quarter = 'OT%d' % (period - 4) if period > 4 else 'Q%d' % period
#                 plays = self.db.PlayByPlay.find({'date': spider.scrape_date, 'game_id': game_id, 'quarter': quarter}).sort('index')
#                 events = self.db.GameEvent.find({'date': spider.scrape_date, 'game_id': game_id, 'quarter': quarter}).sort('index')
#                 home_lineups = self.db.Lineup.find({'date': str(spider.scrape_date), 'team_abbreviation': home, 'period': period}).sort('min', pymongo.DESCENDING)
#                 away_lineups = self.db.Lineup.find({'date': str(spider.scrape_date), 'team_abbreviation': away, 'period': period}).sort('min', pymongo.DESCENDING)
#                 home_start_lineup_i = 0
#                 away_start_lineup_i = 0
#                 home_lineup_i = 0
#                 away_lineup_i = 0
#                 event_i = 0
#                 new_events = []
#                 while True:
#                     try:
#                         for play_i, play in enumerate(plays):
#                             while event_i < events.count() and events[event_i]['index'] < play['index']:
#                                 event = events[event_i]
#                                 for name, type in player_names(event).items():
#                                     if type == 'home' and player_id(home_lineups[home_lineup_i], name) is None:
#                                         raise HomeException
#                                     elif type == 'away' and player_id(away_lineups[away_lineup_i], name) is None:
#                                         raise AwayException
#                                     elif type == 'pos' \
#                                             and player_id(away_lineups[away_lineup_i], name) is None \
#                                             and player_id(home_lineups[home_lineup_i], name) is None:
#                                         raise NeitherException
#                                 event_i += 1
#                             for name, type in player_names(play).items():
#                                 pass
#
#                     except HomeException:
#                         print "home player didn't match"
#                         home_start_lineup_i += 1
#                         home_lineup_i = home_start_lineup_i
#                         away_lineup_i = away_start_lineup_i
#                         event_i = 0
#                         new_events = []
#                         continue
#                     except AwayException:
#                         print "away player didn't match"
#                         away_start_lineup_i += 1
#                         home_lineup_i = home_start_lineup_i
#                         away_lineup_i = away_start_lineup_i
#                         event_i = 0
#                         new_events = []
#                         continue
#                     except NeitherException:
#                         print "player wasn't in either lineup"
#                         home_start_lineup_i += 1
#                         away_start_lineup_i += 1
#                         home_lineup_i = home_start_lineup_i
#                         away_lineup_i = away_start_lineup_i
#                         event_i = 0
#                         new_events = []
#                         continue
#                     break
#
#         self.client.close()
#
# class HomeException(Exception): pass
# class AwayException(Exception): pass
# class NeitherException(Exception): pass
