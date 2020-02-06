# cluj

Now using the ESPN API, which is completely undocumented, but perhaps a better way to pull than data than web scraping. 

Player Rater:
- Base on averages, not totals (but filter out players with small sample size)

Database
- Data does include playoffs, so far I have 2018-19 reg season and playoffs, 2019-20 regular season
- Add a column for position? Could pull this from espn
- Create a table to hold matchup outcomes, for determining accuracy of predictions

Boxscore simulater/predictor:
- Create some code to look at the accuracy of predictions
- Actually create a model to predict scoring based on opponent, teammates injured (at same position)
- How to account for situations where more players have games than you can start on a given day 
- Count up games for each day, flag days where there are more than 9, and if so, drop games from the worst performing players

Modeling:
- Factors that might influence performance: home vs away, years in the league, random intercept for opponent (some measure of within year difficulty)
- Is there any temporal autocorrelation in the performance (where performance is more similar for games that are near each other in time)?

Main: 
- Create a "dashboard" using Rshiny
- Create a trade proposer

References:
https://stmorse.github.io/journal/espn-fantasy-v3.html