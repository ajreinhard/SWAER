full18_df <- read.csv('output/2018 prep/full_2018.csv',stringsAsFactors=F)
OHSAA_df <- read.csv('output/OHSAA ALL.csv',stringsAsFactors=F)

ratings <- elo_def(return_me='log')[[2]]
ratings_nonOH <- elo_def(return_me='log')[[3]]

elo_df <- data.frame(Team=OHSAA_df$Best.Name,Abbr=OHSAA_df$HyTek,Div=OHSAA_df$Div.2018)
elo_df <- elo_df[which(!is.na(elo_df$Div)),]
row.names(elo_df) <- OHSAA_df$OHSAA.ID[which(!is.na(OHSAA_df$Div.2018))]
ratings$curr_hist_avg <- apply(cbind(ratings[,c(paste0(2014:2017,' Week 15'))],NA),1,function(j) sum(unlist(c(rep(.25,4),NA)*j),na.rm=T))

elo_df[,c('End2017','hist_avg')] <- ratings[match(row.names(elo_df),row.names(ratings)),c('Current','curr_hist_avg')]

div_reg <- aggregate(End2017~Div,elo_df,mean)
elo_df$div_avg <- div_reg$End2017[match(elo_df$Div,div_reg$Div)]
elo_df$Current <- ifelse(is.na(elo_df$End2017),elo_df$div_avg,elo_df$End2017)
elo_df$hist_avg <- ifelse(is.na(elo_df$hist_avg),elo_df$Current,elo_df$hist_avg)

full18_df <- full18_df[which(full18_df$Tm_ID %in% row.names(elo_df)),]
full18_df$Opp_End2017 <- elo_df$Current[match(full18_df$Opp_ID,row.names(elo_df))]
full18_df$Opp_End2017_nonOH <- ratings_nonOH$Current[match(full18_df$Opp_ID,row.names(ratings_nonOH))]
full18_df$Tm_DivAvg <- elo_df$div_avg[match(full18_df$Tm_ID,row.names(elo_df))]

full18_df$rating2017 <- ifelse(is.na(full18_df$Opp_End2017),ifelse(is.na(full18_df$Opp_End2017_nonOH),full18_df$Tm_DivAvg,full18_df$Opp_End2017_nonOH),full18_df$Opp_End2017)

sched_avg <- aggregate(rating2017~Tm_ID,full18_df,mean)
elo_df$sched_avg <- sched_avg$rating2017[match(row.names(elo_df),sched_avg$Tm_ID)]

elo_df$player_adj <- player_adj_pred_18[paste0('2017_',row.names(elo_df))]
elo_df$player_adj[is.na(elo_df$player_adj)] <- 0

elo_df$Current <- elo_df$Current * .6 + elo_df$sched_avg * .2 + elo_df$hist_avg * .2 + elo_df$player_adj

elo_df$SWAER <- (elo_df$Current-1500)/20.7

start <- elo_df$SWAER
names(start) <- elo_df$Team
start[order(start),]
sort(start)
start[order(start[,1]),1]

head(elo_df)
tail(elo_df)