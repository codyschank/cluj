import time
import pytz
import requests
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

from itertools import chain
from datetime import timedelta, date, datetime, timezone
from basketball_reference_web_scraper import client

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
#print("Today is {}".format(today))

def player_rater():

    url = 'https://fantasy.espn.com/apis/v3/games/FBA/seasons/2020/segments/0/leagues/' + str(league_id)
    params={"view": "kona_player_info"}
    r = requests.get(url, params=params, cookies=cookies)
    data = r.json()

    stat_list = []
    team_id_list = []
    injured_list = []
    injuryStatus_list = []
    for player in data['players']:
        #print(player['player']['fullName'])
        for stat_item in player['player']['stats']:
            #looks like season totals AND averages id = '002020'
            if stat_item['id'] == '002020':
                if '40' in stat_item['stats'].keys(): # '40' is minutes
                    stat_item['stats']['playerName'] = player['player']['fullName']
                    stat_list.append(stat_item['stats'])
                    team_id_list.append(player['onTeamId'])
                    injured_list.append(player['player']['injured'])
                    injuryStatus_list.append(player['player']['injuryStatus'])

    stats_df = pd.DataFrame(stat_list)
    stats_df['onTeamId'] = team_id_list
    stats_df['injured'] = injured_list
    stats_df['injuryStatus'] = injuryStatus_list

    col_rename_dict = {'0':'pts', '1':'blocks', '2':'steals', '3':'ast', '6':'reb', '13':'fgm', '14': 'fga',
                   '15': 'ftm', '16':'fta', '17':'threes', '40':'min', '42':'gp'}
    stats_df.rename(columns=col_rename_dict, inplace=True)
    stats_df['fg_pct'] = stats_df['fgm'] / stats_df['fga']
    stats_df['ft_pct'] = stats_df['ftm'] / stats_df['fta']
    stats_df = stats_df[['playerName','onTeamId','injuryStatus','pts','blocks','steals','ast','reb','fgm','fga','fg_pct','ftm','fta','ft_pct','threes','min','gp']]

    stats_df['avg_ast'] = stats_df['ast'] / stats_df['gp']
    stats_df['avg_blocks'] = stats_df['blocks'] / stats_df['gp']
    stats_df['avg_steals'] = stats_df['steals']/ stats_df['gp']
    stats_df['avg_reb'] = stats_df['reb'] / stats_df['gp']
    stats_df['avg_pts'] = stats_df['pts'] / stats_df['gp']
    stats_df['avg_threes'] = stats_df['threes'] / stats_df['gp']

    stats_df = stats_df.loc[stats_df['gp'] > 10] # only use players with more than 10 games in player rater

    stats_df.loc[:,'ast_avg_rank'] = stats_df.apply(lambda x: ( (x['avg_ast'] - stats_df['avg_ast'].mean()) / stats_df['avg_ast'].std()) , axis = 1)
    stats_df.loc[:,'blocks_avg_rank'] = stats_df.apply(lambda x: ( (x['avg_blocks'] - stats_df['avg_blocks'].mean()) / stats_df['avg_blocks'].std()) , axis = 1)
    stats_df.loc[:,'steals_avg_rank'] = stats_df.apply(lambda x: ( (x['avg_steals'] - stats_df['avg_steals'].mean()) / stats_df['avg_steals'].std()) , axis = 1)
    stats_df.loc[:,'reb_avg_rank'] = stats_df.apply(lambda x: ( (x['avg_reb'] - stats_df['avg_reb'].mean()) / stats_df['avg_reb'].std()) , axis = 1)
    stats_df.loc[:,'pts_avg_rank'] = stats_df.apply(lambda x: ( (x['avg_pts'] - stats_df['avg_pts'].mean()) / stats_df['avg_pts'].std()) , axis = 1)
    stats_df.loc[:,'threes_avg_rank'] = stats_df.apply(lambda x: ( (x['avg_threes'] - stats_df['avg_threes'].mean()) / stats_df['avg_threes'].std()) , axis = 1)

    stats_df.loc[:,'ast_total_rank'] = stats_df.apply(lambda x: ( (x['ast'] - stats_df['ast'].mean()) / stats_df['ast'].std()) , axis = 1)
    stats_df.loc[:,'blocks_total_rank'] = stats_df.apply(lambda x: ( (x['blocks'] - stats_df['blocks'].mean()) / stats_df['blocks'].std()) , axis = 1)
    stats_df.loc[:,'steals_total_rank'] = stats_df.apply(lambda x: ( (x['steals'] - stats_df['steals'].mean()) / stats_df['steals'].std()) , axis = 1)
    stats_df.loc[:,'reb_total_rank'] = stats_df.apply(lambda x: ( (x['reb'] - stats_df['reb'].mean()) / stats_df['reb'].std()) , axis = 1)
    stats_df.loc[:,'pts_total_rank'] = stats_df.apply(lambda x: ( (x['pts'] - stats_df['pts'].mean()) / stats_df['pts'].std()) , axis = 1)
    stats_df.loc[:,'threes_total_rank'] = stats_df.apply(lambda x: ( (x['threes'] - stats_df['threes'].mean()) / stats_df['threes'].std()) , axis = 1)

    stats_df.loc[:,'fta_rank'] = stats_df.apply(lambda x: ( (x['fta'] - stats_df['fta'].mean()) / stats_df['fta'].std()) , axis = 1)
    stats_df.loc[:,'fga_rank'] = stats_df.apply(lambda x: ( (x['fga'] - stats_df['fga'].mean()) / stats_df['fga'].std()) , axis = 1)
    stats_df.loc[:,'ft_pct_rank'] = stats_df.apply(lambda x: ( (x['ft_pct'] - stats_df['ft_pct'].mean()) / stats_df['ft_pct'].std()) , axis = 1)
    stats_df.loc[:,'fg_pct_rank'] = stats_df.apply(lambda x: ( (x['fg_pct'] - stats_df['fg_pct'].mean()) / stats_df['fg_pct'].std()) , axis = 1)
    stats_df['fg_rank_adj'] = stats_df['fg_pct_rank'] * stats_df['fga_rank']
    stats_df['ft_rank_adj'] = stats_df['ft_pct_rank'] * stats_df['fta_rank']

    cat_ranks = ['ast_total_rank','blocks_total_rank','steals_total_rank','reb_total_rank','pts_total_rank','threes_total_rank',
                'ast_avg_rank','blocks_avg_rank','steals_avg_rank','reb_avg_rank','pts_avg_rank','threes_avg_rank',
                'fg_rank_adj','ft_rank_adj']
    stats_df['total_rank'] = stats_df[cat_ranks].sum(axis=1)
    stats_df.sort_values('total_rank', inplace=True, ascending= False)

    final_stats_df = stats_df[['playerName','onTeamId','injuryStatus','total_rank'] + cat_ranks]

    return(final_stats_df)


def pull_boxscores(day):
    boxscores = client.player_box_scores(day=day.day, month=day.month, year=day.year)
    for item in boxscores:
        item.update( {"date":datetime.strftime(day.date(), format = '%Y-%m-%d')})
        item.update( {"season_year":'2019-2020'})
        item.update( {"season_type":'regular'})
    boxscores_df = pd.DataFrame(boxscores)
    boxscores_df['rebounds'] = boxscores_df.offensive_rebounds + boxscores_df.defensive_rebounds
    boxscores_df.rename(columns={'attempted_field_goals':'fga', 'attempted_free_throws':'fta',
                                'made_three_point_field_goals':'threes', 'made_field_goals':'fgm',
                                'made_free_throws':'ftm'}, inplace=True)
    boxscores_df['twos'] = boxscores_df.fgm - boxscores_df.threes
    boxscores_df['points'] = (boxscores_df.threes * 3) + (boxscores_df.twos * 2) + (boxscores_df.ftm * 1)
    boxscores_df.drop(columns=['attempted_three_point_field_goals','defensive_rebounds','offensive_rebounds',
                              'game_score','slug','turnovers','outcome','twos','personal_fouls','location'], inplace=True)
    boxscores_df['opponent'] = boxscores_df.opponent.apply(lambda x: x.name)
    boxscores_df['team'] = boxscores_df.team.apply(lambda x: x.name)
    boxscores_df.to_sql('boxscores', con=engine, if_exists='append', index=False)
    print(boxscores_df.shape)

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
                AND b.season_year = '2019-2020'
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

        # this does linear weights
        date_diffs = (pd.to_datetime(player_boxscores.date) - pd.to_datetime(today)).dt.days
        min_diff = min(date_diffs)
        weights = date_diffs+abs(min_diff)
        player_boxscores['weights'] = weights

        for sample in range(n_samples):
            player_samples = player_boxscores.sample(replace=True, n=n_games)
            #player_samples = player_boxscores.sample(replace=True, n=n_games, weights='weights')
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

def compare_totals(teamId, team_totals_samples_df, opponent_totals_samples_df, matchupPeriod, matchup_end_date):

    merged = team_totals_samples_df.merge(opponent_totals_samples_df, on='sample_i')
    results = {}
    for stat in ['assists','blocks','fg_pct','ft_pct','points','rebounds','steals','threes']:
        results[stat] = (merged[stat+'_x'] > merged[stat+'_y']).sum()
    results['team'] = teamId
    results['date'] = today
    results['matchupPeriod'] = matchupPeriod
    results['days_to_end'] = (matchup_end_date - datetime.strptime(today,'%Y-%m-%d').date()).days
    df = pd.DataFrame(results, index=[0])
    df.to_sql('predictions', con=engine, if_exists='append', index=False)
    return(df)
