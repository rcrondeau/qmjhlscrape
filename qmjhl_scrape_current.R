library(jsonlite)
library(tidyjson)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(stringr)
library(rvest)
library(stringi)
library(XML)
library(plyr)
library(selectr)
library(gdata)
library(flexdashboard)

scheduleALL <- data.table()
pxpALL <- data.table()
teamsALL <- data.table()
rosterALL <- data.table()
draftALL <- data.table()
boxALL <- data.table()

#lhjmq Seasons
json <- fromJSON("http://cluster.leaguestat.com/feed/?feed=modulekit&view=seasons&key=c680916776709578&fmt=json&client_code=lhjmq&lang=en&league_code=&fmt=json")
season <- data.table(json$SiteKit$Seasons)


#lhjmq Schedule
season.filter <- filter(season, career == 1)
season.filter <- filter(season.filter, playoff == 0)
season.filter <- filter(season.filter, !grepl("Pre",season_name))
season.filter <- filter(season.filter, !grepl("Tie",season_name))


#Get Season
seasonid <- season.filter$season_id[1]
season.name <- season.filter$season_name[1]
scheduleURL <- paste(c("http://cluster.leaguestat.com/feed/?feed=modulekit&view=schedule&key=c680916776709578&fmt=json&client_code=lhjmq&lang=en&season_id=", seasonid, "&team_id=undefined&league_code=&fmt=json"), collapse = "")
scheduleJSON <- fromJSON(scheduleURL)
schedule <- data.table(scheduleJSON$SiteKit$Schedule)
schedule$seasonid <- seasonid
schedule$season.name <- season.name
scheduleALL <- rbind(scheduleALL, schedule)
print(paste(c("Record ", 1, " of ", nrow(season)), collapse = ""))


#lhjmq Teams
seasonid <- season.filter$season_id[1]
season.name <- season.filter$season_name[1]
teamsURL <- paste(c("http://cluster.leaguestat.com/feed/?feed=modulekit&view=teamsbyseason&key=c680916776709578&fmt=json&client_code=lhjmq&season_id=", seasonid, "&lang=en&fmt=json"), collapse = "")
teamsJSON <- fromJSON(teamsURL)
teams <- data.table(teamsJSON$SiteKit$Teamsbyseason)
teams$seasonid <- seasonid
teams$season.name <- season.name
teamsALL <- rbind(teamsALL, teams)
print(paste(c("Record ", 1, " of ", 1), collapse = ""))


#lhjmq Roster
for (i in 1:nrow(teamsALL)) {
  seasonid <- teamsALL$seasonid[i]
  season.name <- teamsALL$season.name[i]
  teamid <- teamsALL$id[i]
  team.name <- teamsALL$name[i]
  rosterURL <- paste(c("http://cluster.leaguestat.com/feed/?feed=modulekit&view=roster&key=c680916776709578&fmt=json&client_code=lhjmq&lang=en&season_id=", seasonid, "&team_id=", teamid, "&fmt=json"), collapse = "")
  rosterJSON <- fromJSON(rosterURL)
  rosterTB <- rosterJSON$SiteKit$Roster
  rosterTBL <- matrix(rosterTB)
  #rosterTBL <- head.matrix(rosterTBL, -2)
  for (n in 1:nrow(rosterTBL)) {
    player <- rosterTB[[n]]
    playerid <- player$player_id
    pidl <- length(playerid)
    if(length(player$draftinfo) > 0) {
      playerDRAFT <- data.frame(flatten(player$draftinfo))
      draft <- data.table(playerDRAFT)
      draft$playerid <- playerid
      draftALL <- rbind(draftALL, draft, fill = TRUE)
    }
    player <- rbind(player, fill = NULL)
    player <- data.frame(player)
    player <- data.table(player)
    if(length(playerid) > 0 ) {
      player <- subset(player, select = -c(draftinfo))
      player$seasonid <- seasonid
      player$teamname <- team.name
      player$teamid <- teamid
      rosterALL <- rbind(rosterALL, player, fill = TRUE)
      rosterALL <- lapply(rosterALL, as.character)
    }
  }
  print(paste(c("Record ", i, " of ", nrow(teamsALL)), collapse = ""))
}
rosterALL <- data.frame(rosterALL)
rosterALL <- data.table(rosterALL)
rosterALL <- rosterALL[, lapply(.SD, as.character)]


#lhjmq Boxscores
scheduleBOX <- filter(scheduleALL, status == 4)
for (i in 1:nrow(scheduleBOX)) {
  gameid <- scheduleBOX$game_id[i]
  seasonid <- scheduleBOX$season_id[i]
  season.name <- scheduleBOX$season.name[i]
  boxURL <- paste(c("http://cluster.leaguestat.com/feed/index.php?feed=gc&key=f109cf290fcf50d4&client_code=lhjmq&game_id=", gameid, "&lang_code=en&fmt=json&tab=gamesummary"), collapse = "")
  boxJSON <- fromJSON(boxURL)
  #Get Team IDs
  boxMETA <- boxJSON$GC$Gamesummary$meta
  homeid <- boxMETA$home_team
  visitorid <- boxMETA$visiting_team
  #Home Team
  boxGoalies <- data.table(flatten(boxJSON$GC$Gamesummary$home_team_lineup$goalies))
  boxPlayers <- data.table(flatten(boxJSON$GC$Gamesummary$home_team_lineup$players))
  boxHOME <- rbind(boxGoalies, boxPlayers, fill = TRUE)
  boxHOME$homeid <- homeid
  #Road Team
  boxGoalies <- data.table(flatten(boxJSON$GC$Gamesummary$visitor_team_lineup$goalies))
  boxPlayers <- data.table(flatten(boxJSON$GC$Gamesummary$visitor_team_lineup$players))
  boxVISITOR <- rbind(boxGoalies, boxPlayers, fill = TRUE)
  boxVISITOR$visitorid <- visitorid
  #Combine
  box <- rbind(boxHOME, boxVISITOR, fill = TRUE)
  box$gameid <- gameid
  box$seasonid <- seasonid
  box$season.name <- season.name
  boxALL <- rbind(boxALL, box, fill = TRUE)
  print(paste(c("Record ", i, " of ", nrow(scheduleBOX)), collapse = ""))
}


#lhjmq PXP
schedulePXP <- filter(scheduleALL, status == 4)
for (i in 1:nrow(schedulePXP)) {
  gameid <- schedulePXP$game_id[i]
  seasonid <- schedulePXP$season_id[i]
  season.name <- schedulePXP$season.name[i]
  pxpURL <- paste(c("http://cluster.leaguestat.com/feed/index.php?feed=gc&key=f109cf290fcf50d4&client_code=lhjmq&game_id=", gameid, "&lang_code=en&fmt=json&tab=pxpverbose"), collapse = "")
  pxpJSON <- fromJSON(pxpURL)
  pxp <- data.table(flatten(pxpJSON$GC$Pxpverbose))
  boxURL <- paste(c("http://cluster.leaguestat.com/feed/index.php?feed=gc&key=f109cf290fcf50d4&client_code=lhjmq&game_id=", gameid, "&lang_code=en&fmt=json&tab=gamesummary"), collapse = "")
  boxJSON <- fromJSON(boxURL)
  #Get Team IDs
  boxMETA <- boxJSON$GC$Gamesummary$meta
  homeid <- boxMETA$home_team
  visitorid <- boxMETA$visiting_team
  #pxp <- subset(pxp, select = -c(plus, minus))
  pxp$gameid <- gameid
  pxp$seasonid <- seasonid
  pxp$season.name <- season.name
  pxp$team.ga <- ifelse(pxp$team_id == "homeid", visitorid, homeid)
  pxp$team.gf <- ifelse(pxp$team_id == "homeid", homeid, visitorid)
  schedulePXP$v.shots <- boxJSON$GC$Gamesummary$totalShots$visitor
  schedulePXP$h.shots <- boxJSON$GC$Gamesummary$totalShots$home
  pxpALL <- rbind(pxpALL, pxp, fill = TRUE)
  print(paste(c("Record ", i, " of ", nrow(schedulePXP)), collapse = ""))
}
pxpALL.save <- lapply(pxpALL, as.character)
pxpALL.save <- data.frame(pxpALL.save)
pxpALL <- data.table(pxpALL)


#Plus Minus
dt.plus.all <- data.table()
dt.goals <- filter(pxpALL, event == "goal")
for (n in 1:nrow(dt.goals)) {
  dt.plus <- data.table()
  goalid <- dt.goals$id[n]
  plus <- dt.goals$plus[n]
  df.plus <- plus[[1]]
  if(length(df.plus) > 0) {
    for (a in 1:nrow(df.plus)) {
      df.plus1 <- df.plus[a,]
      df.plus1$plus1.fullname <- paste(df.plus1$first_name, df.plus1$last_name, sep = " ")
      names(df.plus1) <- c(paste("plus",a,".playerid", sep = ""), paste("plus",a,".jerseynumber", sep = ""), paste("plus",a,".teamid", sep = ""), paste("plus",a,".teamcode", sep = ""), paste("plus",a,".firstname", sep = ""), paste("plus",a,".lastname", sep = ""), paste("plus",a,".fullname", sep = ""))
      dt.plus1 <- data.table(df.plus1)
      if(length(dt.plus) == 0) {
        dt.plus <- rbind(dt.plus, dt.plus1)
      } else {
        dt.plus <- cbind(dt.plus, dt.plus1)
      }
    }
    dt.plus$goalid <- goalid
    dt.plus.all <- rbind(dt.plus.all, dt.plus, fill = TRUE)
    print(paste(c("Record ", n, " of ", nrow(dt.goals)), collapse = ""))
  }
}

dt.minus.all <- data.table()
for (n in 1:nrow(dt.goals)) {
  dt.minus <- data.table()
  goalid <- dt.goals$id[n]
  minus <- dt.goals$minus[n]
  df.minus <- minus[[1]]
  if(length(df.minus) > 0) {
    for (a in 1:nrow(df.minus)) {
      df.minus1 <- df.minus[a,]
      df.minus1$minus1.fullname <- paste(df.minus1$first_name, df.minus1$last_name, sep = " ")
      names(df.minus1) <- c(paste("minus",a,".playerid", sep = ""), paste("minus",a,".jerseynumber", sep = ""), paste("minus",a,".teamid", sep = ""), paste("minus",a,".teamcode", sep = ""), paste("minus",a,".firstname", sep = ""), paste("minus",a,".lastname", sep = ""), paste("minus",a,".fullname", sep = ""))
      dt.minus1 <- data.table(df.minus1)
      if(length(dt.minus) == 0) {
        dt.minus <- rbind(dt.minus, dt.minus1)
      } else {
        dt.minus <- cbind(dt.minus, dt.minus1)
      }
    }
    dt.minus$goalid <- goalid
    dt.minus.all <- rbind(dt.minus.all, dt.minus, fill = TRUE)
    print(paste(c("Record ", n, " of ", nrow(dt.goals)), collapse = ""))
  }
}

# ATTACH PLUS TO GOALS
pxpALL <- merge(pxpALL, dt.plus.all, by.x = "id", by.y = "goalid", all.x = TRUE)

# ATTACH MINUS TO GOALS
pxpALL <- merge(pxpALL, dt.minus.all, by.x = "id", by.y = "goalid", all.x = TRUE)

# GET GOAL TABLES
dt.goals.all <- filter(pxpALL, event == "goal")
dt.goals.pp <- filter(dt.goals.all, power_play == 1)
dt.goals.sh <- filter(dt.goals.all, short_handed == 1)
dt.goals.en <- filter(dt.goals.all, empty_net == 1)
dt.goals.ps <- filter(dt.goals.all, penalty_shot == 1)
dt.goals.ev <- subset(dt.goals.all, power_play == 0 & short_handed == 0 & empty_net == 0 & penalty_shot == 0)

