import re
def player_names(pbp_dict):
    m = re.match('\((\d+:\d\d)\) Jump Ball (.+) vs (.+) \((.+) gains possession\)', pbp_dict['text'])
    if m:
        return {m.group(2): 'home', m.group(3): 'away', m.group(4): 'pos'}
    m = re.match('(.+) Substitution replaced by (.+)', pbp_dict['text'])
    if m:
        return {m.group(1): 'out', m.group(2): int}
    m = re.match('(.+) 3pt Shot: Made \((\d+) PTS\) Assist: (.+) \((\d+) AST\)', pbp_dict['text'])
    if m:
        return {m.group(1): 'off', m.group(2): 'off'}
    m = re.match('(.+) Turnaround Jump Shot: Missed Block: (.+) \((\d+) BLK\)', pbp_dict['text'])
    if m:
        return {m.group(1): 'off', m.group(2): 'def'}
    if 'Team Rebound' == pbp_dict['text']:
        return {}
    print pbp_dict['text']
    raise ValueError


def player_id(lineup_dict, name):
    player_names = lineup_dict['group_name'].split(' - ')
    name_parts = name.split()
    for i, player_name in enumerate(player_names):
        if all(x in player_name for x in name_parts):
            player_ids = lineup_dict['group_id'].split(' - ')
            return int(player_ids[i])
    return None
