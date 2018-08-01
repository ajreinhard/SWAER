###get data from current model
final_elos <- elo_def(return_me='log')[[2]]
final_elos_nonOH <- elo_def(return_me='log')[[3]]
final_elos_nonOH[,names(final_elos)[-c(which(names(final_elos) %in% names(final_elos_nonOH)))]] <- NA
final_elos[,names(final_elos_nonOH)[-c(which(names(final_elos_nonOH) %in% names(final_elos)))]] <- NA
spred_adj <- 19.4
bracket <- read.csv('output/Playoffs Bracket.csv',stringsAsFactors=F)

model_elos <- rbind(final_elos, final_elos_nonOH)
model_elos <- model_elos[order(model_elos$Reg),]
model_elos$TeamID <- row.names(model_elos)

Reg_Season <- rbind(JE_games,JE_games_nonOH)
Reg_Season['131232','Multi_Week'] <- 1
Reg_Season['88046','Multi_Week'] <- 1
Reg_Season['83261','Multi_Week'] <- 1

####begin season prep
all_seedings <- lapply(2017:2007,function(y) {

Reg_Season <- Reg_Season[which(Reg_Season$Season==y & Reg_Season$Playoff==0),]
Reg_Season <- Reg_Season[order(Reg_Season$Week),]

Reg_Season$home_adv <- ifelse(Reg_Season$Loc=='H',25,ifelse(Reg_Season$Loc=='A',-25,0))
Reg_Season$Game_ID <- paste0(ifelse(Reg_Season$Tm_ID<Reg_Season$Opp_ID,paste0(Reg_Season$Tm_ID,'_',Reg_Season$Opp_ID),paste0(Reg_Season$Opp_ID,'_',Reg_Season$Tm_ID)),'_',Reg_Season$Week)
Reg_Season$WinID <- ifelse(Reg_Season$Win==1,Reg_Season$Tm_ID,Reg_Season$Opp_ID)
Reg_Season <- Reg_Season[which(Reg_Season$Excl_Harbin==0 & is.na(Reg_Season$Multi_Week)),]

####begin matrix setup
model_elos$Reg <- Reg_Season$Opp_Reg[match(model_elos$TeamID,Reg_Season$Opp_ID)]
model_elos$Reg[which(model_elos$Reg==0)] <- NA
model_elos$Div <- teams_df$Div[match(paste0(y,'_',model_elos$TeamID),teams_df$YrID)]
model_elos$Div <- ifelse(is.na(model_elos$Div),Reg_Season$nonOHSAA_div[match(model_elos$TeamID,Reg_Season$Opp_ID)], model_elos$Div)

region <- lapply(1:28, function(x) which(model_elos$Reg==x))
sim_df <- Reg_Season[match(unique(Reg_Season$Game_ID),Reg_Season$Game_ID),c('Game_ID','Tm_ID','Opp_ID','Week','home_adv','Opp_Div','Conf_Matchup','Tm_OH','WinID')]
sim_df <- sim_df[which(sim_df$Week <= 10 & sim_df$Week > 0),]
if (y >= 2013) {game_cnt <- c(112,56,28,14,7)} else {game_cnt <- c(96,48,24,12,6)}
if (y < 2013) {pts_adj <- 0.5} else {pts_adj <- 0}

conf_uni <- unique(sim_df$Conf_Matchup)
conf_uni <- conf_uni[!is.na(conf_uni)]
Conf <- lapply(conf_uni, function(x) which(sim_df$Conf_Matchup==x))
names(Conf) <- conf_uni

Tm_Wk <- paste0(sim_df$Tm_ID,'_',sim_df$Week)
Opp_Wk <- paste0(sim_df$Opp_ID,'_',sim_df$Week)
match_id_tm <- sapply(row.names(model_elos),function(x) match(paste0(x,'_',1:10),Tm_Wk))
match_id_opp <- sapply(row.names(model_elos),function(x) match(paste0(x,'_',1:10),Opp_Wk))
match_id <- ifelse(is.na(match_id_tm),match_id_opp,match_id_tm)
opp_id_mx <- ifelse(is.na(match_id_tm),matrix(sim_df$Tm_ID[match_id],10),matrix(sim_df$Opp_ID[match_id],10))
L1_mx <- (7-matrix(model_elos[paste0(opp_id_mx),'Div'],10))/2 + 3.5 - pts_adj
L1_mx[,which(is.na(model_elos$Reg))] <- (7-ifelse(!is.na(opp_id_mx[,which(is.na(model_elos$Reg))]),matrix(rep(model_elos$Div[which(is.na(model_elos$Reg))],10),10,byrow=T),NA))/2 + 3.5 - pts_adj
games_cnt_L1 <- apply(opp_id_mx,2,function(x) length(which(!is.na(x))))
games_cnt_L2 <- apply(matrix(games_cnt_L1[paste0(opp_id_mx)],10),2,sum,na.rm=T)
my_auto_fun = function(elo_d) easy_auto_fun(list(elo_d=elo_d,
		t5 = 3.76, t4 = 2.7, t3 = -3.53, t2 = -6.58, t1 = 1.16,
		t5p = 12, t4p = 11, t3p = 6, t2p = 5, t1p = 1
			))
OH_teams <- row.names(model_elos)[which(!is.na(model_elos$Reg))]
####end season prep

##start with weekly
po_yr <- lapply(1:10, function(wkst) {
curr_elos <- model_elos
curr_elos$Begin <- c(model_elos[,paste0(y,' Week ',wkst-1)])
curr_elos$Current <- curr_elos$Begin
Div_repl <- aggregate(Begin~Div,curr_elos,FUN=mean,subset= !is.na(Reg))

wk_df <- sim_df[which(sim_df$Week>=wkst),]

wk_df$Tm_elo_pre <- curr_elos[paste0(wk_df$Tm_ID),'Current']
wk_df$Opp_elo_pre <- curr_elos[paste0(wk_df$Opp_ID),'Current']
wk_df$Opp_elo_pre <- ifelse(is.na(wk_df$Opp_elo_pre), Div_repl[wk_df$Opp_Div,2], wk_df$Opp_elo_pre)
wk_df$elo_diff <- wk_df$Tm_elo_pre-wk_df$Opp_elo_pre+wk_df$home_adv
wk_df$spread <- wk_df$elo_diff/spred_adj
wk_df$Win_Prob <- 1/(1+10^(-wk_df$elo_diff/400))
wk_df$Win <- NA
wk_df$Over <- NA
full_pred <- wk_df

if (wkst!=1) {
played <- sim_df[which(sim_df$Week<wkst),]
played[,names(full_pred)[-c(which(names(full_pred) %in% names(played)))]] <- NA
played$Win <- played$WinID
played$Win_Prob <- ifelse(played$Win==played$Tm_ID,1,0)
played$Over <- 1
full_pred <- rbind(played,full_pred)
}


tm_win_prob <- ifelse(matrix(full_pred$Tm_ID[match_id],10)==opp_id_mx, 1-matrix(full_pred$Win_Prob[match_id],10), matrix(full_pred$Win_Prob[match_id],10))
L1_pts <- apply(tm_win_prob * L1_mx,2,sum,na.rm=T)
L2_pts <- apply(tm_win_prob * matrix(L1_pts[paste0(opp_id_mx)],10),2,sum,na.rm=T)
total_avg <- (L1_pts/games_cnt_L1)+(L2_pts/games_cnt_L2)*10

L1_pts_curr <- apply(tm_win_prob * L1_mx * matrix(full_pred$Over[match_id],10),2,sum,na.rm=T)
L2_pts_curr <- apply(tm_win_prob * matrix(L1_pts_curr[paste0(opp_id_mx)],10) * matrix(full_pred$Over[match_id],10),2,sum,na.rm=T)
L1_pts_curr_avg <- apply(tm_win_prob * L1_mx * matrix(full_pred$Over[match_id],10),2,mean,na.rm=T)
total_avg_curr <- L1_pts_curr_avg+(L2_pts_curr/games_cnt_L2)*10

seed_unordered <- unlist(lapply(region, function(rg) rank(-total_avg[rg],ties.method='random')))
seed <- seed_unordered[paste0(row.names(curr_elos))]

seed_unordered_curr <- unlist(lapply(region, function(rg) rank(-total_avg_curr[rg],ties.method='min')))
seed_curr <- seed_unordered_curr[paste0(row.names(curr_elos))]

seed_df <- curr_elos[,c('School','TeamID','Reg','Div')]
seed_df$Season <- y
seed_df$Week <- wkst-1
seed_df$curr_seed <- seed_curr
seed_df$proj_seed <- seed
seed_df$curr_avg <- total_avg_curr
seed_df$proj_avg <- total_avg
seed_df[which(!is.na(seed_df$Reg)),]
})
po_yr <- do.call(rbind,po_yr)
po_yr
})

crystal_df <- do.call(rbind,all_seedings)
write.csv(crystal_df,'output/crystal.csv',row.names=F)



