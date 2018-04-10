 
dt.rosters <- rosterALL
dt.pbp <- pxpALL
dt.box <- boxALL
dt.box$gpkey.box <- paste(dt.box$player_id, dt.box$seasonid, sep = "")
dt.team <- teamsALL

# CREATE SCORING DATA TABLE

#PLAYER
dt.player.data.all <- data.table()

for (x in 1:nrow(dt.rosters)) {
  dt.player.data <- data.table("seasonid" = NA)
  playerid <- dt.rosters$player_id[x]
  seasonid <- dt.rosters$seasonid[x]
  gpkey <- as.numeric(paste(playerid, seasonid, sep = ""))
  dt.player.data$seasonid <- dt.rosters$seasonid[x]
  dt.player.data$playerid <- dt.rosters$player_id[x]
  dt.player.data$teamid <- dt.rosters$teamid[x]
  #GET GP
  dt.player.data$gp <- sum(dt.box$gpkey.box == gpkey)
  dt.box.f <- filter(dt.box, dt.box$gpkey.box == gpkey)
  dt.player.data$shots <- sum(as.numeric(dt.box.f$shots))
  #GET GOALS ASSISTS
  dt.player.data$goals <- sum(dt.goals.all$goal_scorer.player_id == playerid)
  dt.player.data$assist1 <- sum(dt.goals.all$assist1_player.player_id == playerid, na.rm = TRUE)
  dt.player.data$assist2 <- sum(dt.goals.all$assist2_player.player_id == playerid, na.rm = TRUE)
  dt.player.data$points <- dt.player.data$goals + dt.player.data$assist1 + dt.player.data$assist2
  #GET PP GOALS ASSISTS
  dt.player.data$goals.pp <- sum(dt.goals.pp$goal_scorer.player_id == playerid)
  dt.player.data$assist1.pp <- sum(dt.goals.pp$assist1_player.player_id == playerid, na.rm = TRUE)
  dt.player.data$assist2.pp <- sum(dt.goals.pp$assist2_player.player_id == playerid, na.rm = TRUE)
  dt.player.data$points.pp <- dt.player.data$goals.pp + dt.player.data$assist1.pp + dt.player.data$assist2.pp
  #GET SH GOALS ASSISTS
  dt.player.data$goals.sh <- sum(dt.goals.sh$goal_scorer.player_id == playerid)
  dt.player.data$assist1.sh <- sum(dt.goals.sh$assist1_player.player_id == playerid, na.rm = TRUE)
  dt.player.data$assist2.sh <- sum(dt.goals.sh$assist2_player.player_id == playerid, na.rm = TRUE)
  dt.player.data$points.sh <- dt.player.data$goals.sh + dt.player.data$assist1.sh + dt.player.data$assist2.sh
  #GET EN GOALS ASSISTS
  dt.player.data$goals.en <- sum(dt.goals.en$goal_scorer.player_id == playerid)
  dt.player.data$assist1.en <- sum(dt.goals.en$assist1_player.player_id == playerid, na.rm = TRUE)
  dt.player.data$assist2.en <- sum(dt.goals.en$assist2_player.player_id == playerid, na.rm = TRUE)
  dt.player.data$points.en <- dt.player.data$goals.en + dt.player.data$assist1.en + dt.player.data$assist2.en
  #GET PS GOALS
  dt.player.data$goals.ps <- sum(dt.goals.ps$goal_scorer.player_id == playerid)
  #GET EV GOALS ASSISTS
  dt.player.data$goals.ev <- dt.player.data$goals - (dt.player.data$goals.pp + dt.player.data$goals.sh + dt.player.data$goals.en + dt.player.data$goals.ps)
  dt.player.data$assist1.ev <- dt.player.data$assist1 - (dt.player.data$assist1.pp + dt.player.data$assist1.sh + dt.player.data$assist1.en)
  dt.player.data$assist2.ev <- dt.player.data$assist2 - (dt.player.data$assist2.pp + dt.player.data$assist2.sh + dt.player.data$assist2.en)
  dt.player.data$points.ev <- dt.player.data$goals.ev + dt.player.data$assist1.ev + dt.player.data$assist2.ev
  #ON ICE GF EVEN
  dt.player.data$gfev1 <- sum(dt.goals.ev$plus1.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfev2 <- sum(dt.goals.ev$plus2.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfev3 <- sum(dt.goals.ev$plus3.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfev4 <- sum(dt.goals.ev$plus4.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfev5 <- sum(dt.goals.ev$plus5.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfev6 <- sum(dt.goals.ev$plus6.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfev <- dt.player.data$gfev1 + dt.player.data$gfev2 + dt.player.data$gfev3 + dt.player.data$gfev4 + dt.player.data$gfev5 +dt.player.data$gfev6
  #ON ICE GA EVEN
  dt.player.data$gaev1 <- sum(dt.goals.ev$minus1.playerid == playerid, na.rm = TRUE)
  dt.player.data$gaev2 <- sum(dt.goals.ev$minus2.playerid == playerid, na.rm = TRUE)
  dt.player.data$gaev3 <- sum(dt.goals.ev$minus3.playerid == playerid, na.rm = TRUE)
  dt.player.data$gaev4 <- sum(dt.goals.ev$minus4.playerid == playerid, na.rm = TRUE)
  dt.player.data$gaev5 <- sum(dt.goals.ev$minus5.playerid == playerid, na.rm = TRUE)
  dt.player.data$gaev6 <- sum(dt.goals.ev$minus6.playerid == playerid, na.rm = TRUE)
  dt.player.data$gaev <- dt.player.data$gaev1 + dt.player.data$gaev2 + dt.player.data$gaev3 + dt.player.data$gaev4 + dt.player.data$gaev5 +dt.player.data$gaev6
  #ON ICE GF EMPTY
  dt.player.data$gfen1 <- sum(dt.goals.en$plus1.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfen2 <- sum(dt.goals.en$plus2.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfen3 <- sum(dt.goals.en$plus3.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfen4 <- sum(dt.goals.en$plus4.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfen5 <- sum(dt.goals.en$plus5.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfen6 <- sum(dt.goals.en$plus6.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfen <- dt.player.data$gfen1 + dt.player.data$gfen2 + dt.player.data$gfen3 + dt.player.data$gfen4 + dt.player.data$gfen5 +dt.player.data$gfen6
  #ON ICE GA EMPTY
  dt.player.data$gaen1 <- sum(dt.goals.en$minus1.playerid == playerid, na.rm = TRUE)
  dt.player.data$gaen2 <- sum(dt.goals.en$minus2.playerid == playerid, na.rm = TRUE)
  dt.player.data$gaen3 <- sum(dt.goals.en$minus3.playerid == playerid, na.rm = TRUE)
  dt.player.data$gaen4 <- sum(dt.goals.en$minus4.playerid == playerid, na.rm = TRUE)
  dt.player.data$gaen5 <- sum(dt.goals.en$minus5.playerid == playerid, na.rm = TRUE)
  dt.player.data$gaen6 <- sum(dt.goals.en$minus6.playerid == playerid, na.rm = TRUE)
  dt.player.data$gaen <- dt.player.data$gaen1 + dt.player.data$gaen2 + dt.player.data$gaen3 + dt.player.data$gaen4 + dt.player.data$gaen5 +dt.player.data$gaen6
  #ON ICE GF PP
  dt.player.data$gfpp1 <- sum(dt.goals.pp$plus1.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfpp2 <- sum(dt.goals.pp$plus2.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfpp3 <- sum(dt.goals.pp$plus3.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfpp4 <- sum(dt.goals.pp$plus4.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfpp5 <- sum(dt.goals.pp$plus5.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfpp6 <- sum(dt.goals.pp$plus6.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfpp <- dt.player.data$gfpp1 + dt.player.data$gfpp2 + dt.player.data$gfpp3 + dt.player.data$gfpp4 + dt.player.data$gfpp5 +dt.player.data$gfpp6
  #ON ICE GA PP
  dt.player.data$gapp1 <- sum(dt.goals.pp$minus1.playerid == playerid, na.rm = TRUE)
  dt.player.data$gapp2 <- sum(dt.goals.pp$minus2.playerid == playerid, na.rm = TRUE)
  dt.player.data$gapp3 <- sum(dt.goals.pp$minus3.playerid == playerid, na.rm = TRUE)
  dt.player.data$gapp4 <- sum(dt.goals.pp$minus4.playerid == playerid, na.rm = TRUE)
  dt.player.data$gapp5 <- sum(dt.goals.pp$minus5.playerid == playerid, na.rm = TRUE)
  dt.player.data$gapp6 <- sum(dt.goals.pp$minus6.playerid == playerid, na.rm = TRUE)
  dt.player.data$gapp <- dt.player.data$gapp1 + dt.player.data$gapp2 + dt.player.data$gapp3 + dt.player.data$gapp4 + dt.player.data$gapp5 +dt.player.data$gapp6
  #ON ICE GF PK
  dt.player.data$gfsh1 <- sum(dt.goals.sh$plus1.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfsh2 <- sum(dt.goals.sh$plus2.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfsh3 <- sum(dt.goals.sh$plus3.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfsh4 <- sum(dt.goals.sh$plus4.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfsh5 <- sum(dt.goals.sh$plus5.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfsh6 <- sum(dt.goals.sh$plus6.playerid == playerid, na.rm = TRUE)
  dt.player.data$gfsh <- dt.player.data$gfsh1 + dt.player.data$gfsh2 + dt.player.data$gfsh3 + dt.player.data$gfsh4 + dt.player.data$gfsh5 +dt.player.data$gfsh6
  #ON ICE GA PK
  dt.player.data$gash1 <- sum(dt.goals.sh$minus1.playerid == playerid, na.rm = TRUE)
  dt.player.data$gash2 <- sum(dt.goals.sh$minus2.playerid == playerid, na.rm = TRUE)
  dt.player.data$gash3 <- sum(dt.goals.sh$minus3.playerid == playerid, na.rm = TRUE)
  dt.player.data$gash4 <- sum(dt.goals.sh$minus4.playerid == playerid, na.rm = TRUE)
  dt.player.data$gash5 <- sum(dt.goals.sh$minus5.playerid == playerid, na.rm = TRUE)
  dt.player.data$gash6 <- sum(dt.goals.sh$minus6.playerid == playerid, na.rm = TRUE)
  dt.player.data$gash <- dt.player.data$gash1 + dt.player.data$gash2 + dt.player.data$gash3 + dt.player.data$gash4 + dt.player.data$gash5 +dt.player.data$gash6
  #ADD SEASONID AND GF/GA
  dt.player.data$seasonid <- seasonid
  dt.player.data$season.name <- season.name
  dt.player.data$ga <- dt.player.data$gaev + dt.player.data$gaen + dt.player.data$gapp + dt.player.data$gash
  dt.player.data$gf <- dt.player.data$gfev + dt.player.data$gfen + dt.player.data$gfpp + dt.player.data$gfsh
  #BIND DATA
  dt.player.data.all <- rbind(dt.player.data.all, dt.player.data, fill = TRUE)
  print(paste(c("Record ", x, " of ", nrow(dt.rosters)), collapse = ""))
}

#TEAM
dt.team.data.all <- data.table()

for (x in 1:nrow(dt.team)) {
  dt.team.data <- data.table("team.seasonid" = NA)
  teamid <- dt.team$id[x]
  dt.team.data$team.seasonid <- seasonid
  dt.team.data$teamid <- dt.team$id[x]
  #Get Team Shots
  dt.box.f <- filter(schedulePXP, schedulePXP$home_team == teamid)
  dt.team.data$h.shotsf <- sum(as.numeric(dt.box.f$h.shots),na.rm=TRUE)
  dt.team.data$v.shotsa <- sum(as.numeric(dt.box.f$v.shots),na.rm=TRUE)
  dt.box.f <- filter(schedulePXP, schedulePXP$visiting_team == teamid)
  dt.team.data$v.shotsf <- sum(as.numeric(dt.box.f$v.shots),na.rm=TRUE)
  dt.team.data$h.shotsa <- sum(as.numeric(dt.box.f$h.shots),na.rm=TRUE)
  dt.team.data$team.shots.for <- dt.team.data$h.shotsf + dt.team.data$v.shotsf
  dt.team.data$team.shots.against <- dt.team.data$h.shotsa + dt.team.data$v.shotsa
  #GET GOALS ASSISTS
  team.goals.all <- filter(dt.goals.all, team_id == teamid)
  #team.goals.all$assist1_player_id <- ifelse(team.goals.all$assist1_player_id == 0, NA, team.goals.all$assist1_player_id)
  dt.team.data$team.goals <- nrow(team.goals.all)
  #dt.team.data$team.goals <- as.numeric(dt.team.data$team.goals)
  dt.team.data$team.assist1 <- sum(complete.cases(team.goals.all$assist1_player.player_id))
  dt.team.data$team.assist2 <- sum(complete.cases(team.goals.all$assist2_player.player_id))
  dt.team.data$team.points <- dt.team.data$team.goals + dt.team.data$team.assist1 + dt.team.data$team.assist2
  #GET AGAINST GOALS
  team.goals.against.all <- filter(dt.goals.all, team.ga == teamid)
  team.goals.against.all$team.ga <- as.numeric(team.goals.against.all$team.ga)
  dt.team.data$team.GA <- nrow(team.goals.against.all)
  #dt.team.data$team.GA <- as.numeric(dt.team.data$team.GA)
  #GET AGAINST GOALS PP
  team.goals.against.all <- filter(dt.goals.pp, team.ga == teamid)
  dt.team.data$team.GA.PP <- nrow(team.goals.against.all)
  #dt.team.data$team.GA.PP <- as.numeric(dt.team.data$team.GA.PP)
  #GET AGAINST GOALS SH
  team.goals.against.all <- filter(dt.goals.sh, team.ga == teamid)
  dt.team.data$team.GA.SH <- nrow(team.goals.against.all)
  #dt.team.data$team.GA.SH <- as.numeric(dt.team.data$team.GA.SH)
  #GET AGAINST GOALS EN
  team.goals.against.all <- filter(dt.goals.en, team.ga == teamid)
  dt.team.data$team.GA.EN <- nrow(team.goals.against.all)
  #dt.team.data$team.GA.EN <- as.numeric(dt.team.data$team.GA.EN)
  #GET AGAINST GOALS PS
  team.goals.against.all <- filter(dt.goals.ps, team.ga == teamid)
  dt.team.data$team.GA.PS <- nrow(team.goals.against.all)
  #dt.team.data$team.GA.PS <- as.numeric(dt.team.data$team.GA.PS)
  #GET AGAINST GOALS EV
  team.goals.against.all <- filter(dt.goals.ev, team.ga == teamid)
  dt.team.data$team.GA.EV <- nrow(team.goals.against.all)
  #dt.team.data$team.GA.EV <- as.numeric(dt.team.data$team.GA.EV)
  #GET PP GOALS ASSISTS
  team.goals.pp <- filter(dt.goals.pp, team_id == teamid)
  dt.team.data$team.goals.pp <- nrow(team.goals.pp)
  #dt.team.data$team.goals.pp <- as.numeric(dt.team.data$team.goals.pp)
  dt.team.data$team.assist1.pp <- sum(complete.cases(team.goals.pp$assist1_player.player_id))
  dt.team.data$team.assist2.pp <- sum(complete.cases(team.goals.pp$assist2_player.player_id))
  dt.team.data$team.points.pp <- dt.team.data$team.goals.pp + dt.team.data$team.assist1.pp + dt.team.data$team.assist2.pp
  #GET SH GOALS ASSISTS
  team.goals.sh <- filter(dt.goals.sh, team_id == teamid)
  dt.team.data$team.goals.sh <- nrow(team.goals.sh)
  #dt.team.data$team.goals.sh <- as.numeric(dt.team.data$team.goals.sh)
  dt.team.data$team.assist1.sh <- sum(complete.cases(team.goals.sh$assist1_player.player_id))
  dt.team.data$team.assist2.sh <- sum(complete.cases(team.goals.sh$assist2_player.player_id))
  dt.team.data$team.points.sh <- dt.team.data$team.goals.sh + dt.team.data$team.assist1.sh + dt.team.data$team.assist2.sh
  #GET EN GOALS ASSISTS
  team.goals.en <- filter(dt.goals.en, team_id == teamid)
  dt.team.data$team.goals.en <- nrow(team.goals.en)
  #dt.team.data$team.goals.en <- as.numeric(dt.team.data$team.goals.en)
  dt.team.data$team.assist1.en <- sum(complete.cases(team.goals.en$assist1_player.player_id))
  dt.team.data$team.assist2.en <- sum(complete.cases(team.goals.en$assist2_player.player_id))
  dt.team.data$team.points.en <- dt.team.data$team.goals.en + dt.team.data$team.assist1.en + dt.team.data$team.assist2.en
  #GET PS GOALS
  dt.team.data$team.goals.ps <- sum(dt.goals.ps$team_id == teamid)
  #dt.team.data$team.goals.ps <- as.numeric(dt.team.data$team.goals.ps)
  #GET EV GOALS ASSISTS
  dt.team.data$team.goals.ev <- dt.team.data$team.goals - (dt.team.data$team.goals.pp + dt.team.data$team.goals.sh + dt.team.data$team.goals.en + dt.team.data$team.goals.ps)
  dt.team.data$team.assist1.ev <- dt.team.data$team.assist1 - (dt.team.data$team.assist1.pp + dt.team.data$team.assist1.sh + dt.team.data$team.assist1.en)
  dt.team.data$team.assist2.ev <- dt.team.data$team.assist2 - (dt.team.data$team.assist2.pp + dt.team.data$team.assist2.sh + dt.team.data$team.assist2.en)
  dt.team.data$team.points.ev <- dt.team.data$team.goals.ev + dt.team.data$team.assist1.ev + dt.team.data$team.assist2.ev
  dt.team.data$seasonid <- seasonid
  dt.team.data.all <- rbind(dt.team.data.all, dt.team.data, fill = TRUE)
  print(paste(c("Record ", x, " of ", nrow(dt.team)), collapse = ""))
}


dt.player.data.all2 <- merge(dt.player.data.all, dt.team.data.all, by = "teamid", all.x = TRUE, allow.cartesian = TRUE)
dt.rosters.final <- merge(dt.rosters, dt.player.data.all2, by.x = "player_id", by.y ="playerid", all.x = TRUE)
dt.rosters.final <- dt.rosters.final[!duplicated(dt.rosters.final[, c("player_id")]),]

#Remove Goalies
dt.rosters.final <- data.table(filter(dt.rosters.final, !grepl("G",position)))
