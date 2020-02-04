import requests
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

from itertools import chain
from datetime import timedelta, date, datetime, timezone
from sqlalchemy import create_engine
from sqlalchemy_utils import database_exists, create_database

from secrets import *

league_id = 84057

dbname = 'cluj'
engine = create_engine('postgres://%s:%s@localhost/%s'%(AUTH['db_user'],AUTH['db_pass'],dbname))

if not database_exists(engine.url):
    create_database(engine.url)

cookies = {"swid": AUTH['swid'],
           "espn_s2": AUTH['espn_s2']}

today = datetime.strftime(datetime.now(), format='%Y-%m-%d')
print("Today is {}".format(today))

def pull_injured_players():

    url = 'https://fantasy.espn.com/apis/v3/games/FBA/seasons/2020/segments/0/leagues/' + str(league_id)
    params={"view": "kona_player_info"}
    r = requests.get(url, params=params, cookies=cookies)
    data = r.json()

    injured_out = []
    injured_dtd = []
    for player in data['players']:
        for stat_item in player['player']['stats']:
            if stat_item['id'] == '002020':
                if 'injuryStatus' in player['player'].keys():
                    if player['player']['injuryStatus'] == 'OUT':
                        injured_out.append(player['player']['fullName'])
                    if player['player']['injuryStatus'] == 'DAY_TO_DAY':
                        injured_dtd.append(player['player']['fullName'])

    return(injured_out)

def matchup_end_date():

    # get matchup end date and match up period using current day
    sql = """
        SELECT *
        FROM matchup_end_dates
        WHERE end_date >= '{}'
        ORDER BY end_date ASC
    """.format(today)
    matchup = pd.read_sql(sql, engine)
    matchup_end_date = matchup.end_date.values[0]
    matchupPeriod = matchup.matchup_period.values[0]
    print("Matchup end date is {}".format(matchup_end_date))
    print("Matchup period is {}".format(matchupPeriod))

    return(matchup_end_date, matchupPeriod)

def pull_matchup_data(teamId, injured_out, matchupPeriod):

    url = 'https://fantasy.espn.com/apis/v3/games/FBA/seasons/2020/segments/0/leagues/' + str(league_id)
    params={"view": "mBoxscore"}
    r = requests.get(url, params=params, cookies=cookies)
    data = r.json()

    team_totals = {}
    opponent_totals = {}

    for matchup in data['schedule']:
        if ((matchup['away']['teamId'] == teamId) or (matchup['home']['teamId'] == teamId)) & (matchup['matchupPeriodId'] == matchupPeriod):

            if matchup['away']['teamId'] == teamId:
                team_role = 'away'
                opponent_role = 'home'
            else:
                team_role = 'home'
                opponent_role = 'away'

            team_players = []
            opponent_players = []
            for entry in matchup[team_role]['rosterForCurrentScoringPeriod']['entries']:
                # 12 is bench players if I also want to remove them
                # but have to make sure their lineup is set correctly
                if (entry['lineupSlotId'] != 13) & (entry['playerPoolEntry']['player']['fullName'] not in injured_out):
                    team_players.append({'fullName':entry['playerPoolEntry']['player']['fullName'],
                                        'proTeamId':entry['playerPoolEntry']['player']['proTeamId']})
            for entry in matchup[opponent_role]['rosterForCurrentScoringPeriod']['entries']:
                if (entry['lineupSlotId'] != 13) & (entry['playerPoolEntry']['player']['fullName'] not in injured_out):
                    opponent_players.append({'fullName':entry['playerPoolEntry']['player']['fullName'],
                                        'proTeamId':entry['playerPoolEntry']['player']['proTeamId']})

            team_totals['points'] = matchup[team_role]['cumulativeScore']['scoreByStat']['0']['score']
            team_totals['blocks'] = matchup[team_role]['cumulativeScore']['scoreByStat']['1']['score']
            team_totals['steals'] = matchup[team_role]['cumulativeScore']['scoreByStat']['2']['score']
            team_totals['assists'] = matchup[team_role]['cumulativeScore']['scoreByStat']['3']['score']
            team_totals['rebounds'] = matchup[team_role]['cumulativeScore']['scoreByStat']['6']['score']
            team_totals['threes'] = matchup[team_role]['cumulativeScore']['scoreByStat']['17']['score']
            team_totals['fga'] = matchup[team_role]['cumulativeScore']['scoreByStat']['14']['score']
            team_totals['fgm'] = matchup[team_role]['cumulativeScore']['scoreByStat']['13']['score']
            team_totals['fta'] = matchup[team_role]['cumulativeScore']['scoreByStat']['16']['score']
            team_totals['ftm'] = matchup[team_role]['cumulativeScore']['scoreByStat']['15']['score']

            opponent_totals['points'] = matchup[opponent_role]['cumulativeScore']['scoreByStat']['0']['score']
            opponent_totals['blocks'] = matchup[opponent_role]['cumulativeScore']['scoreByStat']['1']['score']
            opponent_totals['steals'] = matchup[opponent_role]['cumulativeScore']['scoreByStat']['2']['score']
            opponent_totals['assists'] = matchup[opponent_role]['cumulativeScore']['scoreByStat']['3']['score']
            opponent_totals['rebounds'] = matchup[opponent_role]['cumulativeScore']['scoreByStat']['6']['score']
            opponent_totals['threes'] = matchup[opponent_role]['cumulativeScore']['scoreByStat']['17']['score']
            opponent_totals['fga'] = matchup[opponent_role]['cumulativeScore']['scoreByStat']['14']['score']
            opponent_totals['fgm'] = matchup[opponent_role]['cumulativeScore']['scoreByStat']['13']['score']
            opponent_totals['fta'] = matchup[opponent_role]['cumulativeScore']['scoreByStat']['16']['score']
            opponent_totals['ftm'] = matchup[opponent_role]['cumulativeScore']['scoreByStat']['15']['score']

            break

    results = {}
    results['team_players'] = pd.DataFrame(team_players)
    results['opponent_players'] = pd.DataFrame(opponent_players)
    results['team_totals'] = team_totals
    results['opponent_totals'] = opponent_totals

    return(results)

def get_n_games(matchup_end_date, players_df):

    teamIdList = '(' + ', '.join(players_df.proTeamId.astype(str)) + ')'
    sql = """
        SELECT *
        FROM espn_team_ids eid
        JOIN nba_schedule sch ON eid.scraped_name = sch.home_team OR eid.scraped_name = sch.away_team
        WHERE espn_team_id IN {} AND sch.season_end_year = 2020 AND start_date >= '{}' AND start_date <= '{}'
    """.format(teamIdList, today, matchup_end_date)
    games = pd.read_sql(sql, engine)

    teamCounts = pd.DataFrame(players_df.proTeamId.value_counts())
    teamCounts.rename(columns={'proTeamId':'team_count'}, inplace=True)
    teamCounts = teamCounts.loc[teamCounts.team_count > 1]
    for i in range(len(teamCounts)):
        teamId = teamCounts.index.values[i]
        team_count = teamCounts.team_count.values[i]
        games = games.append([games.loc[games.espn_team_id == teamId]] * (team_count - 1), ignore_index=True)

    return(games)

def simulate_data(n_samples, matchup_end_date, players_df):

    samples = []

    for i in range(len(players_df)):

        player = players_df.iloc[i]['fullName']
        teamId = players_df.iloc[i]['proTeamId']
        #print("Processing " + player)

        # get player boxscores
        # join on player_name_comparison in case name doesn't match, this query should work ok
        sql = """
            SELECT * FROM boxscores b
            LEFT JOIN player_name_comparison c ON b.name = c.boxscore_name
            WHERE (c.espn_name = %(player)s) OR (b.name = %(player)s)
                AND b.date >= '2019-10-22'
        """
        player_boxscores = pd.read_sql(sql, engine, params = {'player': player})

        # get n_games for this matchup
        sql = """
            SELECT COUNT(*)
            FROM espn_team_ids eid
            JOIN nba_schedule sch ON eid.scraped_name = sch.home_team OR eid.scraped_name = sch.away_team
            WHERE espn_team_id = {} AND sch.season_end_year = 2020 AND start_date >= '{}' AND start_date <= '{}'
        """.format(teamId, today, matchup_end_date)
        n_games = pd.read_sql(sql, engine).values[0][0]
        #print(n_games)

        for sample in range(n_samples):
            player_samples = player_boxscores.sample(replace=True, n=n_games)
            player_samples['sample_i'] = sample
            samples.append(player_samples.to_dict('records'))

    samples_df = pd.DataFrame(list(chain.from_iterable(samples)))
    return(samples_df)

def generate_team_totals(n_samples, team_samples_df, team_totals):

    if team_samples_df.shape[0] > 0:
        team_totals_samples = []
        # iterate over samples
        for sample in range(n_samples):

            sample_totals = {}
            team_samples_df.loc[team_samples_df['sample_i']==sample]
            sample_totals['sample_i'] = sample
            for stat in ['assists','blocks','fga','fgm','fta','ftm','points','rebounds','steals','threes']:
                sample_totals[stat] = team_totals[stat] + team_samples_df.loc[team_samples_df['sample_i']==sample][stat].sum()
            team_totals_samples.append(sample_totals)

        team_totals_samples_df = pd.DataFrame(team_totals_samples)
        team_totals_samples_df['ft_pct'] = team_totals_samples_df.ftm / team_totals_samples_df.fta
        team_totals_samples_df['fg_pct'] = team_totals_samples_df.fgm / team_totals_samples_df.fga
    else: # no games for this team
        team_totals_samples_df = pd.DataFrame([team_totals for i in range(n_samples)])
        team_totals_samples_df['sample_i'] = list(range(0, n_samples))
        team_totals_samples_df['ft_pct'] = team_totals_samples_df.ftm / team_totals_samples_df.fta
        team_totals_samples_df['fg_pct'] = team_totals_samples_df.fgm / team_totals_samples_df.fga

    return(team_totals_samples_df)

def compare_totals(teamId, team_totals_samples_df, opponent_totals_samples_df):

    merged = team_totals_samples_df.merge(opponent_totals_samples_df, on='sample_i')
    results = {}
    for stat in ['assists','blocks','fg_pct','ft_pct','points','rebounds','steals','threes']:
        results[stat] = (merged[stat+'_x'] > merged[stat+'_y']).sum()
    results['team'] = teamId
    results['date'] = today
    df = pd.DataFrame(results, index=[0])
    df.to_sql('predictions', con=engine, if_exists='append', index=False)
    return(df)
