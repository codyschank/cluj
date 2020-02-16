# cluj

Now using the ESPN API, which is completely undocumented, but perhaps a better way to pull than data than web scraping. 

Player Rater:
- Base on averages, not totals (but filter out players with small sample size)

Database
- Data does include playoffs, so far I have 2018-19 reg season and playoffs, 2019-20 regular season
- Add a column for position? Could pull this from espn

Boxscore simulater/predictor:
- Create some code to look at the accuracy of predictions
- Create a function to pull most recent matchup when it ends and write to matchup_results, schedule to run once a week? Or maybe it runs daily with other daily stuff, but only actually completes if the matchup just ended
- Actually create a model to predict scoring based on opponent, teammates injured (at same position)
- How to account for situations where more players have games than you can start on a given day 
- Count up games for each day, flag days where there are more than 9, and if so, drop games from the worst performing players
- Don't account for ties yet

Modeling:
- Factors that might influence performance: home vs away, years in the league, random intercept for opponent (some measure of within year difficulty)
- Is there any temporal autocorrelation in the performance (where performance is more similar for games that are near each other in time)?

Main: 
- Create a "dashboard" using Rshiny
- Create a trade proposer
- Record DTD tag somewhere in DB and make predictions about missing games, will help with simulations. Pie in the sky use NLP to parse player updates to try and predict if they will play.

References:
https://stmorse.github.io/journal/espn-fantasy-v3.html
https://stmorse.github.io/journal/espn-fantasy-python.html
https://github.com/mkreiser/ESPN-Fantasy-Football-API (for info about cookies)