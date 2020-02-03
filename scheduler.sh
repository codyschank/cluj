PATH=/Users/codyschank/anaconda3/bin:/usr/local/bin
jupyter nbconvert --to notebook --execute /Users/codyschank/cluj/yesterdays_boxscores.ipynb
jupyter nbconvert --ExecutePreprocessor.timeout=6000 --to notebook --execute /Users/codyschank/cluj/Simulate_matchup_results.ipynb
cd /Users/codyschank/cluj/
git add -A
git commit -m 'Daily scheduled job'
git push --set-upstream origin master
