##2018 sim
OHSAA_df <- read.csv('data sets/OHSAA ALL.csv',stringsAsFactors=F)
final_elos <- elo_def(return_me='log')[[2]]
final_elos_nonOH <- elo_def(return_me='log')[[3]]
JE_games <- read.csv('data sets/JoeEitel GameLog18.csv',stringsAsFactors=F)
JE_games_nonOH <- read.csv('data sets/JoeEitel GameLog18 nonOH.csv',stringsAsFactors=F)

final_elos$hist_avg <- apply(cbind(final_elos[,c(paste0(2014:2017,' Week 15'))],NA),1,function(j) sum(unlist(c(rep(.25,4),NA)*j),na.rm=T))

elo_df18 <- final_elos[,c(names(final_elos)[1:8],'hist_avg','Player_adj')]
elo_df18 <- rbind(elo_df18,'1502'=c(NA,'Styrker',rep(NA,8)),'1201'=c(NA,'Berlin',rep(NA,8)))
elo_df18$Current <- as.numeric(elo_df18$Current)
elo_df18$hist_avg <- as.numeric(elo_df18$hist_avg)

elo_df18$Div <- OHSAA_df$Div.2018[match(row.names(elo_df18),OHSAA_df$OHSAA.ID)]
elo_df18$Reg <- OHSAA_df$Reg.2018[match(row.names(elo_df18),OHSAA_df$OHSAA.ID)]

div_reg <- aggregate(Current~Div,elo_df18,mean)
elo_df18$div_avg <- div_reg$Current[elo_df18$Div]
elo_df18$Current <- ifelse(is.na(elo_df18$Current),elo_df18$div_avg,elo_df18$Current)
elo_df18$hist_avg <- ifelse(is.na(elo_df18$hist_avg),elo_df18$Current,elo_df18$hist_avg)

JE_games$opp_startElo <- elo_df18[paste0(JE_games$Opp_ID),'Current']
sched_reg <- aggregate(opp_startElo~Tm_ID,JE_games,mean,na.rm=T)
elo_df18$sched_avg <- sched_reg$opp_startElo[match(row.names(elo_df18),sched_reg$Tm_ID)]

player_adj_pred_18 <- c(predict(player_adj,season18_df),0,0)
player_adj_pred_18[is.na(player_adj_pred_18)] <- 0
elo_df18$Player_adj <- player_adj_pred_18

elo_df18$Current <- elo_df18$Current * .5 + elo_df18$sched_avg * .3 + elo_df18$hist_avg * .2 + elo_df18$Player_adj
elo_df18$Begin <- elo_df18$Current

elo_df18['475',]

mean(elo_df18$sched_avg,na.rm=T)

mean(elo_df18$Player_adj)

head(elo_df18)





names(final_elos)


