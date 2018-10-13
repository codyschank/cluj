library(RCurl)
library(XML)

xmlFun = function(x){
  y<-xpathSApply(x,'./a',xmlAttrs)
  if(length(y)>0){
    list(href=y,orig=xmlValue(x))
  }else{
    xmlValue(x)
  }
}

get_gamelog = function(player,nbateam,outputFolder,currentDay){
  
  print(paste0("processing ",player))
  if(nbateam=="Utah"){nbateam="utah"}
  if(nbateam=="utah-jazz"){nbateam="utah"}
  
  # first see if there is a csv for the player
  # this is especially important for backtest, since it goes back to players multiple times.
  gl_file = paste0(outputFolder,player," ",nbateam," ",currentDay,".csv")
  possibleError = tryCatch(suppressWarnings(expr = (gl=read.csv(gl_file, check.names=F))), error=function(e) e)
  
  if(inherits(possibleError, "error")){
  
    print(paste0("downloading ",player))
    
    # get nbateam roster 
    url = paste0("http://www.espn.com/nba/team/roster/_/name/",nbateam)
    url_parsed = htmlParse(getURL(url), asText = TRUE)
    tableNodes = getNodeSet(url_parsed, '//*[@id="my-players-table"]')
    playerTable = readHTMLTable(tableNodes[[1]], header=T, stringsAsFactors=F)
    names(playerTable) = playerTable[1,]
    playerTable = playerTable[-1,]
    
    playerNodes = getNodeSet(url_parsed, '//*[@class="sortcell"]')
    playerLink = getHTMLLinks(playerNodes[[which(playerTable$NAME == player)]])
    url = gsub("/_","/gamelog/_",playerLink)
    url_parsed = htmlParse(getURL(url), asText = TRUE)
    tableNodes = getNodeSet(url_parsed, "//table")
    gl = readHTMLTable(tableNodes[[2]], header=T, stringsAsFactors=F)
    names(gl) = gl[1,]
    gl = gl[-1,]
    gl = gl[!(gl$DATE == "DATE"),]
    gl = gl[!(gl$DATE %in% c("October","November","December","January","February","March","April")),]
    gl = gl[1:(which(gl$DATE == "REGULAR SEASON STATS")-1),]
    gl = gl[gl$MIN >0 ,] # keep only games played in 
    gl = gl[!is.na(gl$DATE),]
    
    gl$FGM = unlist(lapply(gl[,"FGM-FGA"],function(x) unlist(strsplit(x,split="-"))[[1]]))
    gl$FGA = unlist(lapply(gl[,"FGM-FGA"],function(x) unlist(strsplit(x,split="-"))[[2]]))
    gl$FTM = unlist(lapply(gl[,"FTM-FTA"],function(x) unlist(strsplit(x,split="-"))[[1]]))
    gl$FTA = unlist(lapply(gl[,"FTM-FTA"],function(x) unlist(strsplit(x,split="-"))[[2]]))
    gl[,"3PM"] = unlist(lapply(gl[,"3PM-3PA"],function(x) unlist(strsplit(x,split="-"))[[1]]))
    
    gl$octy = ((gl$PTS>0) & (gl$REB>0) & (gl$AST>0) & (gl$STL>0) & (gl$BLK>0) & (gl[,"3PM"]>0) & (gl[,"FG%"] >=0.4) & (gl[,"FT%"] >=0.75))*1
    
    variables = c("REB","BLK","AST","STL","PTS","3PM","FGM","FGA","FTM","FTA","octy")
    for(variable in variables){gl[,variable] = as.numeric(gl[,variable])}
    gl$player_name = player
    write.csv(gl,gl_file)
  }
  return(gl)
  
}

get_ngames = function(nbateam,daysInWeek){
  if(nbateam=="Utah"){nbateam="utah"}
  if(nbateam=="utah-jazz"){nbateam="utah"}
  url = paste0("http://www.espn.com/nba/team/schedule/_/name/",nbateam)
  url_parsed = htmlParse(getURL(url), asText = TRUE)
  tableNodes = getNodeSet(url_parsed, "//table")
  teamSchedule = readHTMLTable(tableNodes[[1]], header=T, stringsAsFactors=F)
  teamSchedule = teamSchedule[!(teamSchedule[,1] %in% c("OCTOBER","NOVEMBER","DECEMBER","JANUARY","FEBRUARY","MARCH","APRIL")),]
  for(i in seq(1,nrow(teamSchedule))){
    month = substr(teamSchedule[i,1],6,8)
    if(month %in% c("Oct","Nov","Dec")){year=2017}else{year=2018}
    teamSchedule[i,"date"] = paste(substr(teamSchedule[i,1],6,nchar(teamSchedule[i,1])),year)
  }
  ngames = sum(as.Date(teamSchedule$date, format="%b %d %Y") %in% daysInWeek)
  return(ngames)
}

get_fantasy_roster = function(leagueID,teamID){
  url = paste0("http://games.espn.com/fba/clubhouse?leagueId=",leagueID,"&teamId=",teamID,"&seasonId=2018")
  url_parsed = htmlParse(getURL(url), asText = TRUE)
  tableNodes = getNodeSet(url_parsed, '//*[@id="playertable_0"]')
  playerTable = readHTMLTable(tableNodes[[1]], header=T, stringsAsFactors=F)
  names(playerTable) = playerTable[1,]
  playerTable = playerTable[-1,]
  playersTeams = playerTable[,"PLAYER, TEAM POS"]
  playersTeams = playersTeams[which(playersTeams!="PLAYER, TEAM POS")]
  playersTeams = playersTeams[which(playersTeams!="")]
  nbateams = unlist(lapply(playersTeams,function(x) unlist(strsplit(x,split=","))[[2]]))
  nbateams = unlist(lapply(nbateams,function(x) unlist(strsplit(x,split="\\s+"))[[2]]))
  players = unlist(lapply(playersTeams,function(x) unlist(strsplit(x,split=","))[[1]]))
  
  fantasy_roster = data.frame(cbind(players,nbateams))
  fantasy_roster = fantasy_roster[!(grepl("\\*",players)),] # removes injured players
  names(fantasy_roster) = c("playername","nbateam")

  fantasy_roster$owner = xpathApply(url_parsed, '//*[@class="per-info"]', xmlValue)[[1]]
  
  fantasy_roster$playername = as.character(fantasy_roster$playername)
  fantasy_roster$nbateam = as.character(fantasy_roster$nbateam)
  fantasy_roster$owner = as.character(fantasy_roster$owner)
  
  return(fantasy_roster)
}

get_team_schedule = function(leagueID,teamID){
 
  url = paste0("http://games.espn.com/fba/schedule?leagueId=",leagueID,"&teamId=",teamID)
  url_parsed = htmlParse(getURL(url), asText = TRUE)
  tableNodes = getNodeSet(url_parsed, "//table")
  team_schedule = readHTMLTable(tableNodes[[2]], header=T, elFun = xmlFun, stringsAsFactors=F)
  names(team_schedule) = team_schedule[1,]
  team_schedule = team_schedule[-1,]
  
  team_schedule$begin_day = unlist(lapply(X=team_schedule[,1],FUN=function(x) substr(x,regexpr("\\(",x)[1]+1, regexpr("\\-",x)[1]-2)))
  team_schedule$end_day = unlist(lapply(X=team_schedule[,1],FUN=function(x) substr(x,regexpr("\\-",x)[1]+2, regexpr("\\)",x)[1]-1)))
  
  for(i in seq(1,nrow(team_schedule))){
    begin_month = substr(team_schedule[i,"begin_day"],1,3)
    if(begin_month %in% c("Oct","Nov","Dec")){begin_year=2017}else{begin_year=2018}
    if(!is.na(suppressWarnings(as.numeric(team_schedule[i,"end_day"])))){
      team_schedule[i,"end_day"] = paste(substr(team_schedule[i,"begin_day"],1,3),team_schedule[i,"end_day"])
    }
    end_month = substr(team_schedule[i,"end_day"],1,3)
    if(end_month %in% c("Oct","Nov","Dec")){end_year=2017}else{end_year=2018}
    team_schedule[i,"begin_day"] = paste(team_schedule[i,"begin_day"],begin_year)
    team_schedule[i,"end_day"] = paste(team_schedule[i,"end_day"],end_year)
    
    index1 = regexpr("&teamId=",team_schedule[i,"OWNER(S)"])[1]
    index2 = regexpr("&seasonId=",team_schedule[i,"OWNER(S)"])[1]
    team_schedule[i,"teamID2"] = as.numeric(substr(team_schedule[i,"OWNER(S)"],index1+nchar("&teamId="),index2-1))
  }
  
  return(team_schedule)
}

sim_matchup = function(leagueID,teamID,nsims,currentDay,outputFolder, dropPlayers){
  
  team_schedule = get_team_schedule(leagueID,teamID) 
  
  schedule_index = (as.Date(team_schedule$begin_day, format="%b %d %Y") <= as.Date(currentDay, format="%b %d %Y")) & 
    (as.Date(team_schedule$end_day, format="%b %d %Y") >= as.Date(currentDay, format="%b %d %Y"))
  
  startDay = team_schedule[schedule_index,"begin_day"]
  
  teamID2 = team_schedule[schedule_index,"teamID2"]
  daysInWeek = seq(as.Date(currentDay, format="%b %d %Y"),as.Date(team_schedule[schedule_index,"end_day"], format="%b %d %Y"), by="days")
  daysInWeek=daysInWeek[1:2] # hardcoded to subtract the weird extra few days at the end of this matchup
  scoringPeriods = c(6,13,20,27,34,41,48,55,62,69,76,83,90,97,104,111,112,119,133,140,147,154,161,168) # hardcoded
  scoringPeriodId = scoringPeriods[which(schedule_index)]

  url = paste0("http://games.espn.com/fba/boxscorefull?leagueId=",leagueID,"&teamId=",teamID,
               "&scoringPeriodId=",scoringPeriodId,"&seasonId=2018&view=matchup&version=full")
  url_parsed = htmlParse(getURL(url), asText = TRUE)
  tableNodes = getNodeSet(url_parsed, "//table")

  fantasy_boxscore1 = readHTMLTable(tableNodes[[3]], header=T, stringsAsFactors=F)
  names(fantasy_boxscore1)= fantasy_boxscore1[2,]
  fantasy_boxscore1 = fantasy_boxscore1[-(1:2),]
  team1_current_totals = data.frame(fantasy_boxscore1[nrow(fantasy_boxscore1),],check.names=F)
  fantasy_boxscore1 = fantasy_boxscore1[-(nrow(fantasy_boxscore1)),]
  team1_current_octys = 0
  
  fantasy_boxscore2 = readHTMLTable(tableNodes[[4]], header=T, stringsAsFactors=F) 
  names(fantasy_boxscore2)= fantasy_boxscore2[2,]
  fantasy_boxscore2 = fantasy_boxscore2[-(1:2),]
  team2_current_totals = data.frame(fantasy_boxscore2[nrow(fantasy_boxscore2),],check.names=F)
  fantasy_boxscore2 = fantasy_boxscore2[-(nrow(fantasy_boxscore2)),]
  team2_current_octys = 0
 
  team1_roster = get_fantasy_roster(leagueID,teamID)
  team1_roster = team1_roster[!team1_roster$playername %in% dropPlayers,]
  nplayers = nrow(team1_roster)
  n = nplayers*nsims
  
  variables = c("REB","BLK","AST","STL","PTS","3PM","FGM","FGA","FTM","FTA") #"octy"

  sim_team1 = data.frame(player=rep(team1_roster$playername,nsims), 
                         REB=rep(0,n),BLK=rep(0,n),AST=rep(0,n),STL=rep(0,n),PTS=rep(0,n),
                         FGM=rep(0,n),FGA=rep(0,n),FTM=rep(0,n),FTA=rep(0,n),octy=rep(0,n))
  sim_team1 = sim_team1[order(sim_team1$player),]
  sim_team1[,"nsim"] = rep(seq(1,nsims),nplayers)
  sim_team1[,"3PM"] = rep(0,n)
  
  for(i in seq(1,nplayers)){
    game_log = get_gamelog(team1_roster[i,"playername"],team1_roster[i,"nbateam"],outputFolder,currentDay)
    ngames = get_ngames(team1_roster[i,"nbateam"],daysInWeek)
    if(ngames>0){
      for(j in seq(1,nsims)){
        games.sample = game_log[round(runif(ngames, 1, nrow(game_log)),0),]
        sim_team1[(sim_team1$player==team1_roster[i,"playername"]) & (sim_team1$nsim==j),variables] = 
          colSums(games.sample[,variables])
      }
    }
  }
  
  team2_roster = get_fantasy_roster(leagueID,teamID2) # how to get teamID2?
  team2_roster = team2_roster[!team2_roster$playername %in% dropPlayers,]
  nplayers = nrow(team2_roster)
  n = nplayers*nsims
  
  sim_team2 = data.frame(player=rep(team2_roster$playername,nsims), 
                         REB=rep(0,n),BLK=rep(0,n),AST=rep(0,n),STL=rep(0,n),PTS=rep(0,n),
                         FGM=rep(0,n),FGA=rep(0,n),FTM=rep(0,n),FTA=rep(0,n),octy=rep(0,n))
  sim_team2 = sim_team2[order(sim_team2$player),]
  sim_team2[,"nsim"] = rep(seq(1,nsims),nplayers)
  sim_team2[,"3PM"] = rep(0,n)
  
  for(i in seq(1,nplayers)){
    game_log = get_gamelog(team2_roster[i,"playername"],team2_roster[i,"nbateam"],outputFolder,currentDay)
    ngames = get_ngames(team2_roster[i,"nbateam"],daysInWeek)
    if(ngames>0){
      for(j in seq(1,nsims)){
        games.sample = game_log[round(runif(ngames, 1, nrow(game_log)),0),]
        sim_team2[(sim_team2$player==team2_roster[i,"playername"]) & (sim_team2$nsim==j),variables] = 
          colSums(games.sample[,variables])
      }
    }
  }
  
  
  team1_current_totals$FGM = unlist(strsplit(team1_current_totals[,"FGM/FGA"],split="/"))[1]
  team1_current_totals$FGA = unlist(strsplit(team1_current_totals[,"FGM/FGA"],split="/"))[2]
  team1_current_totals$FTM = unlist(strsplit(team1_current_totals[,"FTM/FTA"],split="/"))[1]
  team1_current_totals$FTA = unlist(strsplit(team1_current_totals[,"FTM/FTA"],split="/"))[2]
  for(variable in variables){team1_current_totals[,variable] = as.numeric(team1_current_totals[,variable])}
  
  sims1.agg = data.frame(aggregate(sim_team1[,variables],by=list(sim_team1$nsim),FUN=sum), check.names=F)
  team1_current_totals_rep = team1_current_totals
  team1_current_totals_rep[seq_len(nrow(sims1.agg)),] = team1_current_totals
  
  team1_final = sims1.agg[,variables] + team1_current_totals_rep[,variables]
  
  team2_current_totals$FGM = unlist(strsplit(team2_current_totals[,"FGM/FGA"],split="/"))[1]
  team2_current_totals$FGA = unlist(strsplit(team2_current_totals[,"FGM/FGA"],split="/"))[2]
  team2_current_totals$FTM = unlist(strsplit(team2_current_totals[,"FTM/FTA"],split="/"))[1]
  team2_current_totals$FTA = unlist(strsplit(team2_current_totals[,"FTM/FTA"],split="/"))[2]
  for(variable in variables){team2_current_totals[,variable] = as.numeric(team2_current_totals[,variable])}
  
  sims2.agg = data.frame(aggregate(sim_team2[,variables],by=list(sim_team2$nsim),FUN=sum), check.names=F)
  team2_current_totals_rep = team2_current_totals
  team2_current_totals_rep[seq_len(nrow(sims2.agg)),] = team2_current_totals
  
  team2_final = sims2.agg[,variables] + team2_current_totals_rep[,variables]
  
  team1_final[,"FG%"] = team1_final$FGM/team1_final$FGA
  team1_final[,"FT%"] = team1_final$FTM/team1_final$FTA
  team2_final[,"FG%"] = team2_final$FGM/team2_final$FGA
  team2_final[,"FT%"] = team2_final$FTM/team2_final$FTA
  
  categories = c("REB","BLK","AST","STL","PTS","3PM","FG%","FT%")
  team1_final$cats_won = rowSums(team1_final[,categories] > team2_final[,categories])
  
  owner = unique(team1_roster$owner)
  
  jpeg(paste0(outputFolder,owner,".jpeg"))
  hist(team1_final$cats_won,breaks=seq(-0.5,8.5),main=paste0(owner," (n sims = ",nsims,")"),xlab="Categories won")
  dev.off()
  
  write.csv(table(team1_final$cats_won)/nsims,paste0(outputFolder,owner,"_cats_won.csv"), row.names=T)
  
  write.csv(colSums(team1_final[,categories] > team2_final[,categories])/nsims,paste0(outputFolder,owner,"_cats_compare.csv"), row.names=T)
  
  return(team1_final)
  
}

back_test = function(leagueID,teamID,nsims,beginDay,currentDay,outputFolder){
  
  team_schedule = get_team_schedule(leagueID,teamID) 
  
  schedule_index = (as.Date(team_schedule$begin_day, format="%b %d %Y") <= as.Date(beginDay, format="%b %d %Y")) & 
    (as.Date(team_schedule$end_day, format="%b %d %Y") >= as.Date(beginDay, format="%b %d %Y"))
  
  teamID2 = team_schedule[schedule_index,"teamID2"]
  scoringPeriods = c(6,13,20,27,34,41,48,55,62,69,76,83,90,97,104,111,112,119,133,140,147) # hardcoded
  scoringPeriodId = scoringPeriods[which(schedule_index)]
  
  url = paste0("http://games.espn.com/fba/boxscorefull?leagueId=",leagueID,"&teamId=",teamID,
               "&scoringPeriodId=",scoringPeriodId,"&seasonId=2018&view=matchup&version=full")
  url_parsed = htmlParse(getURL(url), asText = TRUE)
  tableNodes = getNodeSet(url_parsed, "//table")
  
  fantasy_boxscore1 = readHTMLTable(tableNodes[[3]], header=T, stringsAsFactors=F)
  names(fantasy_boxscore1)= fantasy_boxscore1[2,]
  fantasy_boxscore1 = fantasy_boxscore1[-(1:2),]
  team1_current_totals = data.frame(fantasy_boxscore1[nrow(fantasy_boxscore1),],check.names=F)
  fantasy_boxscore1 = fantasy_boxscore1[-(nrow(fantasy_boxscore1)),]
  
  fantasy_boxscore2 = readHTMLTable(tableNodes[[4]], header=T, stringsAsFactors=F) 
  names(fantasy_boxscore2)= fantasy_boxscore2[2,]
  fantasy_boxscore2 = fantasy_boxscore2[-(1:2),]
  team2_current_totals = data.frame(fantasy_boxscore2[nrow(fantasy_boxscore2),],check.names=F)
  fantasy_boxscore2 = fantasy_boxscore2[-(nrow(fantasy_boxscore2)),]
  
  playersTeams = fantasy_boxscore1[,"PLAYER, TEAM POS"]
  playersTeams = playersTeams[which(playersTeams!="PLAYER, TEAM POS")]
  playersTeams = playersTeams[which(playersTeams!="")]
  nbateams = unlist(lapply(playersTeams,function(x) unlist(strsplit(x,split=","))[[2]]))
  nbateams = unlist(lapply(nbateams,function(x) unlist(strsplit(x,split="\\s+"))[[2]]))
  players = unlist(lapply(playersTeams,function(x) unlist(strsplit(x,split=","))[[1]]))
  players = gsub("\\*","",players)
  fantasy_boxscore1$playername = players
  fantasy_boxscore1$nbateam = nbateams
  
  playersTeams = fantasy_boxscore2[,"PLAYER, TEAM POS"]
  playersTeams = playersTeams[which(playersTeams!="PLAYER, TEAM POS")]
  playersTeams = playersTeams[which(playersTeams!="")]
  nbateams = unlist(lapply(playersTeams,function(x) unlist(strsplit(x,split=","))[[2]]))
  nbateams = unlist(lapply(nbateams,function(x) unlist(strsplit(x,split="\\s+"))[[2]]))
  players = unlist(lapply(playersTeams,function(x) unlist(strsplit(x,split=","))[[1]]))
  players = gsub("\\*","",players)
  fantasy_boxscore2$playername = players
  fantasy_boxscore2$nbateam = nbateams
  
  variables = c("REB","BLK","AST","STL","PTS","3PM","FGM","FGA","FTM","FTA")
 
  nplayers = nrow(fantasy_boxscore1)
  n = nplayers*nsims
  
  # I need to check how this dat
  sim_team1 = data.frame(player=rep(fantasy_boxscore1$playername,nsims), 
                         REB=rep(0,n),BLK=rep(0,n),AST=rep(0,n),STL=rep(0,n),PTS=rep(0,n),
                         FGM=rep(0,n),FGA=rep(0,n),FTM=rep(0,n),FTA=rep(0,n))
  sim_team1 = sim_team1[order(sim_team1$player),]
  sim_team1[,"nsim"] = rep(seq(1,nsims),nplayers)
  sim_team1[,"3PM"] = rep(0,n)
  
  for(i in seq(1,nplayers)){
    game_log = get_gamelog(fantasy_boxscore1[i,"playername"],fantasy_boxscore1[i,"nbateam"],outputFolder,currentDay) 
    ngames = fantasy_boxscore1[i,"GP"]
    if(ngames>0){
      for(j in seq(1,nsims)){
        games.sample = game_log[round(runif(ngames, 1, nrow(game_log)),0),]
        sim_team1[(sim_team1$player==fantasy_boxscore1[i,"playername"]) & (sim_team1$nsim==j),variables] = 
          colSums(games.sample[,variables])
      }
    }
  }
  
  nplayers = nrow(fantasy_boxscore2)
  n = nplayers*nsims
  
  # I need to check how this dat
  sim_team2 = data.frame(player=rep(fantasy_boxscore2$playername,nsims), 
                         REB=rep(0,n),BLK=rep(0,n),AST=rep(0,n),STL=rep(0,n),PTS=rep(0,n),
                         FGM=rep(0,n),FGA=rep(0,n),FTM=rep(0,n),FTA=rep(0,n))
  sim_team2 = sim_team2[order(sim_team2$player),]
  sim_team2[,"nsim"] = rep(seq(1,nsims),nplayers)
  sim_team2[,"3PM"] = rep(0,n)
  
  for(i in seq(1,nplayers)){
    game_log = get_gamelog(fantasy_boxscore2[i,"playername"],fantasy_boxscore2[i,"nbateam"],outputFolder,currentDay) 
    ngames = fantasy_boxscore2[i,"GP"]
    if(ngames>0){
      for(j in seq(1,nsims)){
        games.sample = game_log[round(runif(ngames, 1, nrow(game_log)),0),]
        sim_team2[(sim_team2$player==fantasy_boxscore2[i,"playername"]) & (sim_team2$nsim==j),variables] = 
          colSums(games.sample[,variables])
      }
    }
  }
  
  sims1.agg = data.frame(aggregate(sim_team1[,variables],by=list(sim_team1$nsim),FUN=sum), check.names=F)
  sims2.agg = data.frame(aggregate(sim_team2[,variables],by=list(sim_team2$nsim),FUN=sum), check.names=F)
  
  sims1.agg[,"FG%"] = sims1.agg$FGM/sims1.agg$FGA
  sims1.agg[,"FT%"] = sims1.agg$FTM/sims1.agg$FTA
  sims2.agg[,"FG%"] = sims2.agg$FGM/sims2.agg$FGA
  sims2.agg[,"FT%"] = sims2.agg$FTM/sims2.agg$FTA
  
  team1_current_totals$FGM = unlist(strsplit(team1_current_totals[,"FGM/FGA"],split="/"))[1]
  team1_current_totals$FGA = unlist(strsplit(team1_current_totals[,"FGM/FGA"],split="/"))[2]
  team1_current_totals$FTM = unlist(strsplit(team1_current_totals[,"FTM/FTA"],split="/"))[1]
  team1_current_totals$FTA = unlist(strsplit(team1_current_totals[,"FTM/FTA"],split="/"))[2]
  for(variable in variables){team1_current_totals[,variable] = as.numeric(team1_current_totals[,variable])}
  
  team2_current_totals$FGM = unlist(strsplit(team2_current_totals[,"FGM/FGA"],split="/"))[1]
  team2_current_totals$FGA = unlist(strsplit(team2_current_totals[,"FGM/FGA"],split="/"))[2]
  team2_current_totals$FTM = unlist(strsplit(team2_current_totals[,"FTM/FTA"],split="/"))[1]
  team2_current_totals$FTA = unlist(strsplit(team2_current_totals[,"FTM/FTA"],split="/"))[2]
  for(variable in variables){team2_current_totals[,variable] = as.numeric(team2_current_totals[,variable])}
  
  categories = c("REB","BLK","AST","STL","PTS","3PM","FG%","FT%")
  
  sims1.agg$cats_won = rowSums(sims1.agg[,categories] > sims2.agg[,categories])
  
  #actual_cats_won = sum(team1_current_totals[,categories] > team2_current_totals[,categories])
  #table(sims1.agg$cats_won)/nsims
  
  #cat = categories[3]
  #hist(sims1.agg[,cat], col=rgb(1,0,0,0.5))
  #hist(sims2.agg[,cat], col=rgb(0,0,1,0.5), add=T)
  #abline(v=team1_current_totals[,cat],col=rgb(1,0,0,0.5),lwd=3)
  #abline(v=team2_current_totals[,cat],col=rgb(0,0,1,0.5),lwd=3)
  
  sim.final = data.frame(colSums(sims1.agg[,categories]>sims2.agg[,categories])/nsims)
  names(sim.final) = "probability"
  sim.final$actual = t(team1_current_totals[,categories] > team2_current_totals[,categories])*1
  sim.final$teamID = teamID
  sim.final$teamID2 = teamID2
  return(sim.final)
  
}

back_test_parallel = function(inRow){
  
  library(RCurl)
  library(XML)
  
  outputFolder=inRow["outputFolder"][[1]]
  leagueID=as.numeric(inRow["leagueID"][[1]])
  teamID=as.numeric(inRow["teamID"][[1]])
  nsims=as.numeric(inRow["nsims"][[1]])
  beginDay=inRow["beginDay"][[1]]
  currentDay=inRow["currentDay"][[1]]
  
  team_schedule = get_team_schedule(leagueID,teamID) 
  
  schedule_index = (as.Date(team_schedule$begin_day, format="%b %d %Y") <= as.Date(beginDay, format="%b %d %Y")) & 
    (as.Date(team_schedule$end_day, format="%b %d %Y") >= as.Date(beginDay, format="%b %d %Y"))
  
  teamID2 = team_schedule[schedule_index,"teamID2"]
  scoringPeriods = c(6,13,20,27,34,41,48,55,62,69,76,83,90,97,104,111,112,119,133,140,147) # hardcoded
  scoringPeriodId = scoringPeriods[which(schedule_index)]
  print(teamID2)
  
  url = paste0("http://games.espn.com/fba/boxscorefull?leagueId=",leagueID,"&teamId=",teamID,
               "&scoringPeriodId=",scoringPeriodId,"&seasonId=2018&view=matchup&version=full")
  url_parsed = htmlParse(getURL(url), asText = TRUE)
  tableNodes = getNodeSet(url_parsed, "//table")
  
  fantasy_boxscore1 = readHTMLTable(tableNodes[[3]], header=T, stringsAsFactors=F)
  names(fantasy_boxscore1)= fantasy_boxscore1[2,]
  fantasy_boxscore1 = fantasy_boxscore1[-(1:2),]
  team1_current_totals = data.frame(fantasy_boxscore1[nrow(fantasy_boxscore1),],check.names=F)
  fantasy_boxscore1 = fantasy_boxscore1[-(nrow(fantasy_boxscore1)),]
  
  fantasy_boxscore2 = readHTMLTable(tableNodes[[4]], header=T, stringsAsFactors=F) 
  names(fantasy_boxscore2)= fantasy_boxscore2[2,]
  fantasy_boxscore2 = fantasy_boxscore2[-(1:2),]
  team2_current_totals = data.frame(fantasy_boxscore2[nrow(fantasy_boxscore2),],check.names=F)
  fantasy_boxscore2 = fantasy_boxscore2[-(nrow(fantasy_boxscore2)),]
  
  playersTeams = fantasy_boxscore1[,"PLAYER, TEAM POS"]
  playersTeams = playersTeams[which(playersTeams!="PLAYER, TEAM POS")]
  playersTeams = playersTeams[which(playersTeams!="")]
  nbateams = unlist(lapply(playersTeams,function(x) unlist(strsplit(x,split=","))[[2]]))
  nbateams = unlist(lapply(nbateams,function(x) unlist(strsplit(x,split="\\s+"))[[2]]))
  players = unlist(lapply(playersTeams,function(x) unlist(strsplit(x,split=","))[[1]]))
  players = gsub("\\*","",players)
  fantasy_boxscore1$playername = players
  fantasy_boxscore1$nbateam = nbateams
  
  playersTeams = fantasy_boxscore2[,"PLAYER, TEAM POS"]
  playersTeams = playersTeams[which(playersTeams!="PLAYER, TEAM POS")]
  playersTeams = playersTeams[which(playersTeams!="")]
  nbateams = unlist(lapply(playersTeams,function(x) unlist(strsplit(x,split=","))[[2]]))
  nbateams = unlist(lapply(nbateams,function(x) unlist(strsplit(x,split="\\s+"))[[2]]))
  players = unlist(lapply(playersTeams,function(x) unlist(strsplit(x,split=","))[[1]]))
  players = gsub("\\*","",players)
  fantasy_boxscore2$playername = players
  fantasy_boxscore2$nbateam = nbateams
  
  variables = c("REB","BLK","AST","STL","PTS","3PM","FGM","FGA","FTM","FTA")
  
  nplayers = nrow(fantasy_boxscore1)
  n = nplayers*nsims
  
  # I need to check how this dat
  sim_team1 = data.frame(player=rep(fantasy_boxscore1$playername,nsims), 
                         REB=rep(0,n),BLK=rep(0,n),AST=rep(0,n),STL=rep(0,n),PTS=rep(0,n),
                         FGM=rep(0,n),FGA=rep(0,n),FTM=rep(0,n),FTA=rep(0,n))
  sim_team1 = sim_team1[order(sim_team1$player),]
  sim_team1[,"nsim"] = rep(seq(1,nsims),nplayers)
  sim_team1[,"3PM"] = rep(0,n)
  
  for(i in seq(1,nplayers)){
    game_log = get_gamelog(fantasy_boxscore1[i,"playername"],fantasy_boxscore1[i,"nbateam"],outputFolder,currentDay) 
    ngames = fantasy_boxscore1[i,"GP"]
    if(ngames>0){
      for(j in seq(1,nsims)){
        games.sample = game_log[round(runif(ngames, 1, nrow(game_log)),0),]
        sim_team1[(sim_team1$player==fantasy_boxscore1[i,"playername"]) & (sim_team1$nsim==j),variables] = 
          colSums(games.sample[,variables])
      }
    }
  }
  
  nplayers = nrow(fantasy_boxscore2)
  n = nplayers*nsims
  
  # I need to check how this dat
  sim_team2 = data.frame(player=rep(fantasy_boxscore2$playername,nsims), 
                         REB=rep(0,n),BLK=rep(0,n),AST=rep(0,n),STL=rep(0,n),PTS=rep(0,n),
                         FGM=rep(0,n),FGA=rep(0,n),FTM=rep(0,n),FTA=rep(0,n))
  sim_team2 = sim_team2[order(sim_team2$player),]
  sim_team2[,"nsim"] = rep(seq(1,nsims),nplayers)
  sim_team2[,"3PM"] = rep(0,n)
  
  for(i in seq(1,nplayers)){
    game_log = get_gamelog(fantasy_boxscore2[i,"playername"],fantasy_boxscore2[i,"nbateam"],outputFolder,currentDay) 
    ngames = fantasy_boxscore2[i,"GP"]
    if(ngames>0){
      for(j in seq(1,nsims)){
        games.sample = game_log[round(runif(ngames, 1, nrow(game_log)),0),]
        sim_team2[(sim_team2$player==fantasy_boxscore2[i,"playername"]) & (sim_team2$nsim==j),variables] = 
          colSums(games.sample[,variables])
      }
    }
  }
  
  sims1.agg = data.frame(aggregate(sim_team1[,variables],by=list(sim_team1$nsim),FUN=sum), check.names=F)
  sims2.agg = data.frame(aggregate(sim_team2[,variables],by=list(sim_team2$nsim),FUN=sum), check.names=F)
  
  sims1.agg[,"FG%"] = sims1.agg$FGM/sims1.agg$FGA
  sims1.agg[,"FT%"] = sims1.agg$FTM/sims1.agg$FTA
  sims2.agg[,"FG%"] = sims2.agg$FGM/sims2.agg$FGA
  sims2.agg[,"FT%"] = sims2.agg$FTM/sims2.agg$FTA
  
  team1_current_totals$FGM = unlist(strsplit(team1_current_totals[,"FGM/FGA"],split="/"))[1]
  team1_current_totals$FGA = unlist(strsplit(team1_current_totals[,"FGM/FGA"],split="/"))[2]
  team1_current_totals$FTM = unlist(strsplit(team1_current_totals[,"FTM/FTA"],split="/"))[1]
  team1_current_totals$FTA = unlist(strsplit(team1_current_totals[,"FTM/FTA"],split="/"))[2]
  for(variable in variables){team1_current_totals[,variable] = as.numeric(team1_current_totals[,variable])}
  
  team2_current_totals$FGM = unlist(strsplit(team2_current_totals[,"FGM/FGA"],split="/"))[1]
  team2_current_totals$FGA = unlist(strsplit(team2_current_totals[,"FGM/FGA"],split="/"))[2]
  team2_current_totals$FTM = unlist(strsplit(team2_current_totals[,"FTM/FTA"],split="/"))[1]
  team2_current_totals$FTA = unlist(strsplit(team2_current_totals[,"FTM/FTA"],split="/"))[2]
  for(variable in variables){team2_current_totals[,variable] = as.numeric(team2_current_totals[,variable])}
  
  categories = c("REB","BLK","AST","STL","PTS","3PM","FG%","FT%")
  
  sims1.agg$cats_won = rowSums(sims1.agg[,categories] > sims2.agg[,categories])
  
  #actual_cats_won = sum(team1_current_totals[,categories] > team2_current_totals[,categories])
  #table(sims1.agg$cats_won)/nsims
  
  #cat = categories[3]
  #hist(sims1.agg[,cat], col=rgb(1,0,0,0.5))
  #hist(sims2.agg[,cat], col=rgb(0,0,1,0.5), add=T)
  #abline(v=team1_current_totals[,cat],col=rgb(1,0,0,0.5),lwd=3)
  #abline(v=team2_current_totals[,cat],col=rgb(0,0,1,0.5),lwd=3)
  
  sim.final = data.frame(colSums(sims1.agg[,categories]>sims2.agg[,categories])/nsims)
  names(sim.final) = "probability"
  sim.final$actual = t(team1_current_totals[,categories] > team2_current_totals[,categories])*1
  sim.final$teamID = teamID
  sim.final$teamID2 = teamID2
  
  write.csv(sim.final,paste0(outputFolder,beginDay," ",teamID," backtest.csv"))

}

get_PR = function(leagueID){
  startIndexes = seq(0,400,50)
  for(startIndex in startIndexes){
    url = paste0("http://games.espn.com/fba/playerrater?leagueId=",leagueID,"&teamId=-2147483648&splitTypeId=4&seasonId=2018&startIndex=",startIndex)
    url_parsed = htmlParse(getURL(url), asText = TRUE)
    tableNodes = getNodeSet(url_parsed, '//*[@id="playertable_0"]')
    pr = readHTMLTable(tableNodes[[1]], header=T, stringsAsFactors=F)
    names(pr) = pr[1,]
    pr = pr[-1,]
    if(startIndex==0){finalPR = pr}else{finalPR=rbind(finalPR,pr)}
  }
  finalPR$player.name = unlist(lapply(finalPR[,"PLAYER, TEAM POS"],function(x) unlist(strsplit(x,split=","))[[1]]))
  finalPR$team.pos = unlist(lapply(finalPR[,"PLAYER, TEAM POS"],function(x) unlist(strsplit(x,split=","))[[2]]))
  finalPR$player.team = gsub("(^\\s+)|(\\s+$)", "", substr(finalPR$team.pos,1,4))
  finalPR$injured = grepl("\\*",finalPR$player.name)
  finalPR$player.name = gsub("\\*","",finalPR$player.name)
  variables = c("REB","AST","BLK","PTS","STL","3PM","FG%","FT%")
  for(variable in variables){finalPR[,variable] = as.numeric(finalPR[,variable])}
  return(finalPR)
}

trade_compare = function(currentPR,fromPlayers,toPlayers){
  
  variables = c("REB","AST","BLK","PTS","STL","3PM","FG%","FT%")
  teams =c("BEB","BXV","DEKK","DOIS","FDT","FORS","gov","HH","Ingl","WILL","Wood","YOC")
  
  fromTeam = unique(currentPR[currentPR$player.name %in% fromPlayers,"TYPE"])
  toTeam = unique((currentPR[currentPR$player.name %in% toPlayers,"TYPE"]))
  tempPR = currentPR
  
  tempPR[tempPR$player.name %in% fromPlayers,"TYPE"] = toTeam
  tempPR[tempPR$player.name %in% toPlayers,"TYPE"] = fromTeam
  
  # only set up for differences of one player
  # just assumes worst overall player dropped, and best overall FA added
  if(length(fromPlayers)>length(toPlayers)){
    dropPlayer = currentPR[currentPR$TYPE==toTeam,"player.name"][13] 
    addPlayer = currentPR[currentPR$TYPE=="FA","player.name"][1]
    tempPR[tempPR$player.name==dropPlayer,"TYPE"] = "FA"
    tempPR[tempPR$player.name==addPlayer,"TYPE"] = fromTeam
  }
  if(length(toPlayers)>length(fromPlayers)){
    dropPlayer = currentPR[currentPR$TYPE==fromTeam,"player.name"][13] 
    addPlayer = currentPR[currentPR$TYPE=="FA","player.name"][1]
    tempPR[tempPR$player.name==dropPlayer,"TYPE"] = "FA"
    tempPR[tempPR$player.name==addPlayer,"TYPE"] = toTeam
  }
  
  # write something if there are more players on one side or the other to drop worst player from team receiving more players
  
  currentAgg = aggregate(currentPR[,variables], by = list(currentPR[,"TYPE"]), FUN=mean)
  currentAgg = currentAgg[currentAgg$Group.1 %in% teams,]
  currentAgg$sum = rowSums(currentAgg[,variables]) 
  current = currentAgg[currentAgg$Group.1=="Ingl",variables] - colMeans(currentAgg[currentAgg$Group.1!="Ingl",variables])
  
  tempAgg = aggregate(tempPR[,variables], by = list(tempPR[,"TYPE"]), FUN=mean)
  tempAgg = tempAgg[tempAgg[,1] %in% teams,]
  tempAgg$sum = rowSums(tempAgg[,variables]) 
  temp = tempAgg[tempAgg$Group.1=="Ingl",variables] - colMeans(tempAgg[tempAgg$Group.1!="Ingl",variables])
  
  catDiff = temp-current
  catMean = rowMeans(catDiff)
  finalCompare = cbind(catDiff,catMean)
  return(finalCompare)
  
  #sum(tempAgg[tempAgg$Group.1=="Ingl",variables] > tempAgg[tempAgg$Group.1=="WILL",variables]) - sum(currentAgg[currentAgg$Group.1=="Ingl",variables] > currentAgg[currentAgg$Group.1=="WILL",variables])
  
  #rather than using player rater simulate matchup with 3 or 4 games for each player
  #give probability that each number of cats is won, whats probability that 5 or more are won
  
}

#######################
### run sim_matchup ###
#######################

dropPlayers = c("Andre Drummond")

leagueID = 84057
currentDay = "Apr 7 2018"
nsims = 1000
outputFolder = "/Users/Cody/Dropbox/cluj/"
dir.create(outputFolder, showWarnings = F)

# only works once the week has started
for(teamID in seq(1,12)){
  teamID=10
  print(paste("processing team",teamID))
  team_final = sim_matchup(leagueID=leagueID,teamID=teamID,nsims=nsims,currentDay=currentDay,outputFolder=outputFolder,dropPlayers=dropPlayers) 
}

#player
gl_file = paste0(outputFolder,player," ",nbateam," ",currentDay,".csv")
game_log = read.csv(gl_file,stringsAsFactors = F, check.names = F)
View(game_log)
plot(game_log$BLK)
acf(game_log$BLK)

ngames=3
round(runif(ngames, 1, nrow(game_log)),0)

nrow(game_log)
round(rnorm(ngames, mean = nrow(game_log), sd = 3),0)


##########################
### octy investigation ###
##########################

csvs = list.files(outputFolder,pattern="2018.csv",full.names=T)
merged = data.frame(do.call("rbind", lapply(csvs, read.csv, header=TRUE,stringsAsFactors=F,check.names=F)),check.names=F)

merged$octy = (merged$REB>0) & (merged$STL>0) & (merged$BLK>0) &( merged$AST>0) & (merged[,"3PM"]>0) & (merged[,"FTM"]>0) & (merged[,"FT%"]>=0.7) & (merged[,"FG%"]>=0.35)
octy_agg_sum = aggregate(octy ~ player_name, data=merged, "sum") 
View(octy_agg_sum)
octy_agg_rate = aggregate(octy ~ player_name, data=merged, "mean") 
View(octy_agg_rate)

write.csv(octy_agg_sum,paste0(outputFolder,"octy_agg_sum.csv"),row.names=F)
write.csv(octy_agg_rate,paste0(outputFolder,"octy_agg_rate.csv"),row.names=F)

merged_agg = merge(octy_agg_rate,currentPR[,c("player.name","TYPE")],by.x="player_name",by.y="player.name")
merged_agg_team = aggregate(octy ~ TYPE, data=merged_agg, "mean")
View(merged_agg_team)

sum(merged$REB>0) / nrow(merged)
sum(merged$STL>0) / nrow(merged)
sum(merged$BLK>0) / nrow(merged)
sum(merged$AST>0) / nrow(merged)
sum(merged[,"3PM"]>0) / nrow(merged)
sum(merged[,"FT%"]>=0.7) / nrow(merged)
sum(merged[,"FG%"]>=0.35) / nrow(merged)

octys = merged[(merged$REB>0) & (merged$STL>0) & (merged$BLK>0) &( merged$AST>0) & (merged[,"3PM"]>0) & (merged[,"FTM"]>0) ,]
nrow(octys)
sum(octys[,"FG%"] > 0.35)
sum(octys[,"FT%"] > 0.7)

octys_current_percentages = octys[(octys[,"FG%"] >= 0.4) & (octys[,"FT%"] >= 0.8),]
nrow(octys_current_percentages)


plot(density(octys[,"FG%"]))
abline(v=0.35)
plot(density(octys[,"FT%"]))
abline(v=0.7)

# use spatstat to show the intensity of the points
plot(octys[,"FG%"],octys[,"FT%"])
abline(h=0.7)
abline(v=0.35)

plot(density(merged$REB))
View(merged)

##############################
### run back_test_parallel ###
##############################

leagueID = 84057
currentDay = "Feb 24 2018"
nsims = 1000
outputFolder = "/Users/Cody/Dropbox/cluj/"
dir.create(outputFolder, showWarnings = F)

teamIDs = seq(1,12)
beginDays = c("Oct 17 2017","Oct 23 2017","Oct 30 2017","Nov 6 2017","Nov 13 2017","Nov 20 2017",
              "Nov 20 2017","Dec 4 2017","Dec 11 2017","Dec 18 2017","Dec 25 2017","Jan 1 2018",
              "Jan 8 2018","Jan 15 2018","Jan 22 2018","Jan 29 2018")
n = length(beginDays) * length(teamIDs)

sim.input = data.frame(beginDay = rep(beginDays,length(teamIDs)), leagueID = rep(leagueID,n), currentDay = rep(currentDay,n), 
                      nsims = rep(nsims,n), outputFolder = rep(outputFolder,n),stringsAsFactors=F)
sim.input = sim.input[order(sim.input$beginDay),]
sim.input$teamID = rep(teamIDs,length(beginDays))

library(parallel)
# Initiate cluster
n.cores = 10
cl = makeCluster(spec = rep("localhost",n.cores), mc = getOption("cl.cores", n.cores), outfile=paste0(outputFolder,"log.txt"))
clusterExport(cl=cl, varlist=c("xmlFun","get_gamelog", "get_fantasy_roster", "get_team_schedule"))
parApply(sim.input,MARGIN=1,FUN=back_test_parallel,cl=cl)
stopCluster(cl)
gc()

### now write code to assemble backtest csvs and do AUC calc
# do some sort of histogram for probs that has how many times it actual hit, expected vs what we see.
# remember we are doouble counting too, need to figure out to remove the same games
csvs = list.files(outputFolder,full.names=TRUE,pattern="backtest")
merged = do.call("rbind", lapply(csvs, read.csv, header=TRUE,stringsAsFactors=FALSE))
merged$candidate_model = paste("model",merged$candidate_model,sep="")


############
### misc ###
############

library(pROC)
roc_obj <- roc(merged[,3], merged[,2])
auc(roc_obj)
plot(roc_obj)

library(ggplot2)
library(reshape2)
finalAgg_rs = melt(finalAgg, id.vars=c("Group.1"),measure.vars=variables)
ggplot(finalAgg_rs, aes(x=Group.1, y=value, color=variable)) + geom_point(size = 4) 



#######################
### trade exploring ###
#######################

leagueID = 84057
currentPR = get_PR(leagueID)

currentPR = currentPR[!(currentPR$injured),]

teams =c("BEB","BXV","DEKK","DOIS","FDT","FORS","gov","HH","Ingl","WILL","Wood","YOC")
variables = c("REB","AST","BLK","PTS","STL","3PM","FG%","FT%")
trade_compare_df = read.csv("/Users/Cody/Dropbox/cluj/trade_explore.csv", stringsAsFactors = F)
trade_compare_df[,c(variables,"catMean")] = NA

for(i in seq(1,nrow(trade_compare_df))){
  fromPlayers = trimws(unlist(strsplit(trade_compare_df[i,"fromPlayers"],",")))
  toPlayers = trimws(unlist(strsplit(trade_compare_df[i,"toPlayers"],",")))
  compared = trade_compare(currentPR,fromPlayers,toPlayers)
  trade_compare_df[i,c(variables,"catMean")] = compared
}

View(trade_compare_df)

currentAgg = aggregate(currentPR[,variables], by = list(currentPR[,"TYPE"]), FUN=mean)
currentAgg = currentAgg[currentAgg$Group.1 %in% teams,]
currentAgg$sum = rowSums(currentAgg[,variables]) 
current = currentAgg[currentAgg$Group.1=="Ingl",variables] - colMeans(currentAgg[currentAgg$Group.1!="Ingl",variables])
current

tempAgg = currentAgg
tempAgg[tempAgg$Group.1=="Ingl",variables] - tempAgg[tempAgg$Group.1=="WILL",variables]
sum((tempAgg[tempAgg$Group.1=="Ingl",variables] - tempAgg[tempAgg$Group.1=="WILL",variables])>0)
tempAgg[tempAgg$Group.1=="Ingl",variables] - tempAgg[tempAgg$Group.1=="BEB",variables]
sum((tempAgg[tempAgg$Group.1=="Ingl",variables] - tempAgg[tempAgg$Group.1=="BEB",variables])>0)
tempAgg[tempAgg$Group.1=="Ingl",variables] - tempAgg[tempAgg$Group.1=="gov",variables]
sum((tempAgg[tempAgg$Group.1=="Ingl",variables] - tempAgg[tempAgg$Group.1=="gov",variables])>0)
tempAgg[tempAgg$Group.1=="Ingl",variables] - tempAgg[tempAgg$Group.1=="YOC",variables]
sum((tempAgg[tempAgg$Group.1=="Ingl",variables] - tempAgg[tempAgg$Group.1=="YOC",variables])>0)
tempAgg[tempAgg$Group.1=="Ingl",variables] - tempAgg[tempAgg$Group.1=="DOIS",variables]
sum((tempAgg[tempAgg$Group.1=="Ingl",variables] - tempAgg[tempAgg$Group.1=="DOIS",variables])>0)
