library(scales)
library(gdata)
#Clean Workspace
#gdata::keep(dt.rosters.final, draft_combined, sure = TRUE)

#Clean Stats Tables
dt.rosters.final$pos.clean <- ifelse(dt.rosters.final$position_id == 1, "D", "F")
rosterTable <- dt.rosters.final[, c(36,30,39,25,44:53,63:66,73,80,101,123:125,131:137,142:146,156:159,161)]
#Remove Goalies
#rosterTable <- filter(rosterTable, !grepl("G",position))
#Get Draft Eligibility
rosterTable$BD.Mod <- as.Date(rosterTable$rawbirthdate, "%Y-%m-%d")
rosterTable$birth.year <- as.numeric(substr(rosterTable$rawbirthdate, 1, 4))
rosterTable$draft.end <- as.Date(paste(rosterTable$birth.year, 9, 15, sep = "-"), "%Y-%m-%d")
rosterTable$draft.year <- ifelse(rosterTable$BD.Mod > rosterTable$draft.end, rosterTable$birth.year + 19, rosterTable$birth.year + 18)
rosterTable$draft.season <- as.numeric(substr(rosterTable$season.name, 1, 4)) + 1
rosterTable$draft.eligible <- ifelse(as.numeric(rosterTable$draft.season) == as.numeric(rosterTable$draft.year), "Y", "N")
fwrite(rosterTable, 'data/rosterTable.csv')
#Get Stats Table
rosterTable.stats <-  rosterTable[,c(1,41,47,3:5,7:10,22,6,25,27:30)]
names(rosterTable.stats) <- c("Name", "Position", "Draft.Eligible", "Team", "Birth Date", "GP", "G", "A1", "A2","PTS", "Season", "Shots", "Team.Shots", "Team.G", "Team.A1", "Team.A2", "Team.PTS")
rosterTable.stats$PRIMARY.PTS <- round(rosterTable.stats$G + rosterTable.stats$A1,digits=2)
rosterTable.stats$TEAM.PRIMARY.PTS <- round(rosterTable.stats$Team.G + rosterTable.stats$Team.A1,digits=2)
rosterTable.stats$SH.PERC <- round(rosterTable.stats$G/rosterTable.stats$Shots,digits=2)
rosterTable.stats$PERC.TEAM.SHOTS <- round(rosterTable.stats$Shots/rosterTable.stats$Team.Shots,digits=2)
rosterTable.stats$PERC.TEAM.PTS <- round(rosterTable.stats$PTS/rosterTable.stats$Team.PTS,digits=2)
rosterTable.stats$PERC.TEAM.PrPTS <- round(rosterTable.stats$PRIMARY.PTS/rosterTable.stats$TEAM.PRIMARY.PTS,digits=2)
rosterTable.stats <- rosterTable.stats[,c(11,1:10,18,22,23,12,13,20,21)]
fwrite(rosterTable.stats, 'data/rosterTable_stats.csv')
#Get PP Table
rosterTable.pp <- rosterTable[,c(1,41,47,3:5,11:14,22,33:36)]
names(rosterTable.pp) <- c("Name", "Position", "Draft.Eligible", "Team", "Birth Date", "GP", "PPG", "PPA1", "PPA2","PPPTS", "Season","Team.PPG","Team.PPA1","Team.PPA2","Team.PPPTS")
rosterTable.pp$PRIMARY.PPPTS <- round(rosterTable.pp$PPG + rosterTable.pp$PPA1,digits=2)
rosterTable.pp$PERC.PPPTS <- round(rosterTable.pp$PPPTS/rosterTable.stats$PTS,digits=2)
rosterTable.pp$PERC.TEAM.PPPTS <- round(rosterTable.pp$PPPTS/rosterTable.pp$Team.PPPTS,digits=2)
rosterTable.pp <- rosterTable.pp[,c(11,1:10,16,17,18)]
fwrite(rosterTable.pp, 'data/rosterTable_pp.csv')
#Get EV Table
rosterTable.ev <- rosterTable[,c(1,41,47,3:5,15:18,22,32,37:40,19,20)]
names(rosterTable.ev) <- c("Name", "Position", "Draft.Eligible", "Team", "Birth Date", "GP", "EVG", "EVA1", "EVA2","EVPTS", "Season", "Team.EVGA", "Team.EVGF", "Team.EVA1", "Team.EVA2", "Team.EVPTS","OI.EVGF","OI.EVGA")
rosterTable.ev$EVGF.PERC <- round(rosterTable.ev$OI.EVGF/(rosterTable.ev$OI.EVGF + rosterTable.ev$OI.EVGA),digits=2)
rosterTable.ev$TEAM.EVGF.PERC <- round(rosterTable.ev$Team.EVGF/(rosterTable.ev$Team.EVGF + rosterTable.ev$Team.EVGA),digits=2)
rosterTable.ev$EVGF.PERC.REL <- round(rosterTable.ev$EVGF.PERC - rosterTable.ev$TEAM.EVGF.PERC,digits=2)
rosterTable.ev$PRIMARY.EVPTS <- round(rosterTable.ev$EVG + rosterTable.ev$EVA1,digits=2)
rosterTable.ev$TEAM.PRIMARY.EVPTS <- round(rosterTable.ev$Team.EVGF + rosterTable.ev$Team.EVA1,digits=2)
rosterTable.ev$PERC.TEAM.EVPrPTS <- round(rosterTable.ev$PRIMARY.EVPTS / rosterTable.ev$TEAM.PRIMARY.EVPTS,digits=2)
rosterTable.ev$IPP.EV <- round(rosterTable.ev$EVPTS/rosterTable.ev$OI.EVGF,digits=2)
rosterTable.ev$IPPP.EV <- round(rosterTable.ev$PRIMARY.EVPTS/rosterTable.ev$OI.EVGF,digits=2)
rosterTable.ev <- rosterTable.ev[,c(11,1:10,16:26)]
fwrite(rosterTable.ev, 'data/rosterTable_ev.csv')

#Get Universal ID
#rosterTable$uni.id <- paste(tolower(dt.rosters.final$last_name), tolower(substr(dt.rosters.final$first_name, 1,2)), rosterTable$draft.season, tolower(dt.rosters.final$pos.clean) , rosterTable$birth.year, sep = "")



#Get Filter Lists
team.names <- rosterTable$teamname
#team.names <- c(team.names, "All")
season.names <- rosterTable$season.name
#season.names <- c(season.names, "All")
draft.eligible <- rosterTable$draft.eligible
#draft.eligible <- c(draft.eligible, "Both")
position.f <- rosterTable$pos.clean
#position.f <- c(position.f, "Both")
