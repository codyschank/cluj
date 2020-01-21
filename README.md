# cluj

Now using the ESPN API, which is completely undocumented, but perhaps a better way to pull than data than web scraping. 

Player Rater:
- Base on averages, not totals (but filter out players with small sample size)

Boxscore simulater/predictor:
- Write a script to calculate probabilities for each fantasy matchup every day, and see how well calibrated the model is. 
- How to weight sampling towards more recent games
- Actually create a model to predict scoring based on opponent, teammates injured (at same position)
- How to account for situations where more players have games than you can start on a given day 
    -Count up games for each day, flag days where there are more than 9, and if so, drop games from the worst performing players

Main: 
- Schedule boxscore scraper to run every day in the morning
- Schedule matchup data to run every day in the morning
- Is it possible to schedule git to push?
- Create a "dashboard" using Rshiny

References:
https://stmorse.github.io/journal/espn-fantasy-v3.html