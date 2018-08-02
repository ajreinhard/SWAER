###Figure out ties
###multi week games
###week 0 games
#begin sim
dyn_sim <- function(wkst,yr,sim_df,region,match_id,opp_id_mx,L1_mx,games_cnt_L1,games_cnt_L2,game_cnt,my_auto_fun,OH_teams,model_elos,Conf,bracket,k_mar,wk_slope,non_OHSAA_adj,std_dev) {

curr_elos <- model_elos
Div_repl <- aggregate(Begin~Div,curr_elos,FUN=mean,subset= !is.na(Reg))

if (wkst!=11) {
full_pred <- c()
for (w in wkst:10) {

wk_df <- sim_df[which(sim_df$Week==w),]

wk_df$Tm_elo_pre <- curr_elos[paste0(wk_df$Tm_ID),'Current']
wk_df$Opp_elo_pre <- curr_elos[paste0(wk_df$Opp_ID),'Current']
wk_df$Opp_elo_pre <- ifelse(is.na(wk_df$Opp_elo_pre), Div_repl[wk_df$Opp_Div,2], wk_df$Opp_elo_pre)
wk_df$elo_diff <- wk_df$Tm_elo_pre-wk_df$Opp_elo_pre+wk_df$home_adv
#wk_df$spread <- wk_df$elo_diff/spred_adj
wk_df$spread <- ifelse(wk_df$elo_diff>0,main,-main) * (abs(wk_df$elo_diff)/spred_adj)^pwr
wk_df$Win_Prob <- 1/(1+10^(-wk_df$elo_diff/400))
wk_df$Std_dev <- (wk_df$elo_diff/spred_adj)/qnorm(wk_df$Win_Prob)
wk_df$Score_pred <- rnorm(nrow(wk_df)) * wk_df$Std_dev + wk_df$spread
wk_df$Win <- ifelse(wk_df$Score_pred>0, wk_df$Tm_ID, wk_df$Opp_ID)
#wk_df$elo_change <- (pnorm(wk_df$Score_pred/wk_df$Std_dev)-wk_df$Win_Prob) * k_mar * wk_slope[w] * ifelse(wk_df$Tm_OH==1,1,non_OHSAA_adj) + my_auto_fun(wk_df$elo_diff)
#wk_df$elo_change <- ifelse(wk_df$Score_pred>(wk_df$spread),1,-1) * (pnorm(abs((wk_df$Score_pred-(wk_df$spread))/std_dev))-.5) * k_mar * wk_slope[w] * ifelse(wk_df$Tm_OH==1,1,non_OHSAA_adj) + my_auto_fun(wk_df$elo_diff)
#wk_df$elo_change <- ifelse(wk_df$Score_pred>(wk_df$spread),1,-1) * (log(abs(wk_df$Score_pred-wk_df$spread)+1)+my_auto_fun(wk_df$elo_diff)) * k_mar * wk_slope[w] * ifelse(wk_df$Tm_OH==1 & wk_df$nonOHSAA==0,1,non_OHSAA_adj)
wk_df$elo_change <- ifelse(wk_df$Score_pred>wk_df$spread,1,-1) * (abs(wk_df$Score_pred-wk_df$spread)/7) * k_mar * wk_slope[w] * ifelse(wk_df$Tm_OH==1 & wk_df$nonOHSAA==0,1,non_OHSAA_adj)
wk_df$elo_change[is.na(wk_df$elo_change)] <- 0
wk_df$Tm_elo_post <- wk_df$Tm_elo_pre + wk_df$elo_change
wk_df$Opp_elo_post <- wk_df$Opp_elo_pre - wk_df$elo_change
new_elo_df <- data.frame(row.names=c(wk_df$Tm_ID,wk_df$Opp_ID),NewElo=c(wk_df$Tm_elo_post,wk_df$Opp_elo_post))
curr_elos$Current <- ifelse(is.na(new_elo_df[paste0(row.names(curr_elos)),'NewElo']),curr_elos$Current,new_elo_df[paste0(row.names(curr_elos)),'NewElo'])
full_pred <- c(full_pred,list(wk_df))
}
full_pred <- do.call(rbind,full_pred)
} else {
full_pred <- sim_df[which(sim_df$Week==50),]
}

if (wkst!=1) {
played <- sim_df[which(sim_df$Week<wkst),]
played[,names(full_pred)[-c(which(names(full_pred) %in% names(played)))]] <- NA
played$Win <- played$WinID
full_pred <- rbind(played,full_pred)
}

##Conf Champs
conf_champs <- unlist(lapply(Conf, function(x) {
tms <- c(full_pred$Tm_ID[x],full_pred$Opp_ID[x])
tms_wins <- table(factor(full_pred$Win[x],unique(tms)))
tms_gms <- table(factor(tms,unique(tms)))
tms_wpcts <- tms_wins/tms_gms
champs <- names(sort(-tms_wpcts))[which(min(sort(-tms_wpcts))==sort(-tms_wpcts))]
if (length(champs) > 1) {
all_h2h <- expand.grid(tm1=champs,tm2=champs)
all_h2h <- paste0(all_h2h$tm1,'_',all_h2h$tm2)
tie_br <- table(full_pred$Win[match(all_h2h,paste0(full_pred$Tm_ID,'_',full_pred$Opp_ID))])
if(length(tie_br)!=0) {
champs <- names(tie_br)[which(max(tie_br)==tie_br)]
}}
return(champs)
}))
##

###start seeding calc
L1_pts <- apply(ifelse(matrix(full_pred$Win[match_id],10)==opp_id_mx, 0, L1_mx),2,sum,na.rm=T)
L2_pts <- apply(ifelse(matrix(full_pred$Win[match_id],10)==opp_id_mx, 0 ,matrix(L1_pts[paste0(opp_id_mx)],10)),2,sum,na.rm=T)
total_avg <- (L1_pts/games_cnt_L1)+(L2_pts/games_cnt_L2)*10

seed_unordered <- unlist(lapply(region, function(rg) rank(-total_avg[rg],ties.method='random')))
seed <- seed_unordered[paste0(row.names(curr_elos))]


all_po_df <- c()
#begin playoffs
if (yr<=2012) {reg_format <- 1:24} else if(yr>=2016) {reg_format <- 1:28} else {reg_format <- 3:26}
playoffs_df <- expand.grid(Fav=c(1,4,2,3),Reg=reg_format) 
playoffs_df$Dog <- 9-playoffs_df$Fav
if (reg_format[1]==3) playoffs_df <- rbind(playoffs_df, data.frame(Fav=rep(c(1,8,4,5,3,6,7,2),2),Reg=c(rep(1,8),rep(2,8)),Dog=rep(c(16,9,13,12,14,11,10,15),2)))
playoffs_df[,c('FavID','FavID2','Fav_elo')] <- curr_elos[match(paste0(playoffs_df$Reg,'_',playoffs_df$Fav),paste0(curr_elos$Reg,'_',seed)),c('TeamID','School','Current')]
playoffs_df[,c('DogID','DogID2','Dog_elo')] <- curr_elos[match(paste0(playoffs_df$Reg,'_',playoffs_df$Dog),paste0(curr_elos$Reg,'_',seed)),c('TeamID','School','Current')]

if (wkst >= 11) {
playoffs_df <- bracket[which(bracket$Week==wkst & bracket$Season==yr),]
playoffs_df$Fav_elo <- curr_elos[paste0(playoffs_df$FavID),'Begin']
playoffs_df$Dog_elo <- curr_elos[paste0(playoffs_df$DogID),'Begin']
playoffs_df$Season <- NULL
playoffs_df$Week <- NULL
playoffs_df$Reg <- NULL
playoffs_df$Fav <- NULL
playoffs_df$Dog <- NULL
}


for (p in max(c(wkst-10),1):5) {
playoffs_df$elo_diff <- playoffs_df$Fav_elo - playoffs_df$Dog_elo + ifelse(p==1,25,0)
playoffs_df$Win_Prob <- 1/(1+10^(-playoffs_df$elo_diff/400))
playoffs_df$spread <- ifelse(playoffs_df$elo_diff>0,main,-main) * (abs(playoffs_df$elo_diff)/spred_adj)^pwr
playoffs_df$Std_dev <- (playoffs_df$elo_diff/spred_adj)/qnorm(playoffs_df$Win_Prob)
playoffs_df$Score_pred <- rnorm(nrow(playoffs_df)) * playoffs_df$Std_dev + playoffs_df$spread
playoffs_df$elo_change <- ifelse(playoffs_df$Score_pred>playoffs_df$spread,1,-1) * (abs(playoffs_df$Score_pred-playoffs_df$spread)/7) * k_mar * wk_slope[p+10]

advance_df <- data.frame(ifelse(matrix(playoffs_df$Score_pred > 0 ,game_cnt[p],5),
	as.matrix(cbind(playoffs_df[,c('FavID','FavID2','Fav_elo','elo_change')],1)),
	as.matrix(cbind(playoffs_df[,c('DogID','DogID2','Dog_elo','elo_change')],-1))),
	stringsAsFactors=F)
advance_df$elo_rating <- as.numeric(advance_df$X3) + (as.numeric(advance_df$X4) * as.numeric(advance_df$X5))

#re-seeding
if (p==3) {
re_seeding <- c(sapply(1:game_cnt[5], function(x) sample(((x-1)*4) + 1:4,4)))
if (reg_format[1]==3) re_seeding[1:4] <- 1:4
advance_df <- advance_df[re_seeding,]
}

playoffs_df[,c('WinID','WinID2','Winner_Rating')] <- advance_df[,c('X1','X2','elo_rating')]
all_po_df <- c(all_po_df, list(playoffs_df))

if (p!=5) {
playoffs_df <- cbind(advance_df[seq(1,game_cnt[p],2),c('X1','X2','elo_rating')],advance_df[seq(2,game_cnt[p],2),c('X1','X2','elo_rating')])
names(playoffs_df) <- c('FavID','FavID2','Fav_elo','DogID','DogID2','Dog_elo')
}}

if (wkst >= 12) {
past_gms <- bracket[which(bracket$Week<wkst & bracket$Season==yr),]
past_gms[,names(all_po_df[[1]])[-c(which(names(all_po_df[[1]]) %in% names(past_gms)))]] <- NA
past_gms$Season <- NULL
past_gms$Week <- NULL
#past_gms$Reg <- NULL
#past_gms$Fav <- NULL
#past_gms$Dog <- NULL
all_po_df <- c(list(past_gms),all_po_df)
}

if (wkst != 11) for (i in 1:3) all_po_df[[1]][,1] <- NULL
all_po_df <- do.call(rbind,all_po_df)
#end playoff sim

#what to return
po_res <- table(factor(as.numeric(c(all_po_df$FavID,all_po_df$DogID,rev(all_po_df$WinID)[game_cnt[5]:1])),OH_teams))
win_res <- table(factor(full_pred$Win,OH_teams))
conf_res <- rep(0,length(OH_teams))
conf_res[which(OH_teams %in% conf_champs)] <- 1
return(c(po_res,seed[paste0(OH_teams)],conf_res))
}
#end sim

##2018 sim
OHSAA_df <- read.csv('data sets/OHSAA ALL.csv',stringsAsFactors=F)
final_elos <- elo_def(return_me='log')[[2]]
final_elos_nonOH <- elo_def(return_me='log')[[3]]
JE_games18 <- read.csv('data sets/JoeEitel GameLog18.csv',stringsAsFactors=F)
JE_games18_nonOH <- read.csv('data sets/JoeEitel GameLog18 nonOH.csv',stringsAsFactors=F)
JE_games18$Tm_OH <- 1
JE_games18_nonOH$Week <- JE_games18_nonOH$Week-1
JE_games18_nonOH$Tm_OH <- 0
JE_games18 <- JE_games18[which(JE_games18$Tm_ID!=9057),]
JE_games18_nonOH <- JE_games18_nonOH[which(JE_games18_nonOH$Tm_ID!=1201),]
JE_games_all <- rbind(JE_games18,JE_games18_nonOH)
JE_games_all$nonOHSAA[which(JE_games18$Opp_ID==1201)] <- 0

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

JE_games_all$opp_startElo <- elo_df18[paste0(JE_games_all$Opp_ID),'Current']
sched_reg <- aggregate(opp_startElo~Tm_ID,JE_games_all,mean,na.rm=T,subset= nonOHSAA==0)
elo_df18$sched_avg <- sched_reg$opp_startElo[match(row.names(elo_df18),sched_reg$Tm_ID)]

player_adj_pred_18 <- c(predict(player_adj,season18_df),0,0)
player_adj_pred_18[is.na(player_adj_pred_18)] <- 0
elo_df18$Player_adj <- player_adj_pred_18

elo_df18$Current <- elo_df18$Current * .5 + elo_df18$sched_avg * .3 + elo_df18$hist_avg * .2 + elo_df18$Player_adj
elo_df18$Begin <- elo_df18$Current

#nonOH
tm_yr <- as.numeric(sapply(strsplit(nonOH_div$X,'_'),function(x) x[1]))
tm_id <- sapply(strsplit(nonOH_div$X,'_'),function(x) x[2]) 
most_recent <- aggregate(tm_yr~tm_id,FUN=max)

elo_df18_nonOH <- data.frame(row.names=unique(JE_games_all$Tm_ID[which(JE_games_all$Tm_OH==0)]))

###make a guess on teams with no listed division
JE_div <- JE_games_all$nonOHSAA_div[match(row.names(elo_df18_nonOH),JE_games_all$Opp_ID)]
old_id <- paste0(most_recent$tm_yr[match(row.names(elo_df18_nonOH),most_recent$tm_id)],'_',row.names(elo_df18_nonOH))
last_div <- nonOH_div$nonOH_div[match(old_id,nonOH_div$X)]
guess_div <- rep(NA,length(JE_div))
guess_div[which(row.names(elo_df18_nonOH)=='9916')] <- 6 #KIPP
guess_div[which(row.names(elo_df18_nonOH)=='9918')] <- 2 #COF
OHSAA_gm <- ifelse(is.na(match(row.names(elo_df18_nonOH),JE_games_all$Tm_ID)),0,1)
remaining_div_pick <- row.names(elo_df18_nonOH)[which(OHSAA_gm==1 & is.na(last_div) & is.na(JE_div) & is.na(guess_div))]
unknown_div_opp <- sapply(remaining_div_pick,function(x) JE_games_all$Tm_ID[which(JE_games_all$Opp_ID==x & JE_games_all$Tm_OH==1)])
guess_div[match(names(unknown_div_opp),row.names(elo_df18_nonOH))] <- OHSAA_df$Div.2018[match(unknown_div_opp,OHSAA_df$OHSAA.ID)]

elo_df18_nonOH$City <- NA
elo_df18_nonOH$School <- final_elos_nonOH$School[match(row.names(elo_df18_nonOH),row.names(final_elos_nonOH))]
elo_df18_nonOH$Div <- ifelse(is.na(JE_div),ifelse(is.na(last_div),guess_div,last_div),JE_div)
elo_df18_nonOH$Begin <- final_elos_nonOH$Current[match(row.names(elo_df18_nonOH),row.names(final_elos_nonOH))]
elo_df18_nonOH$Current <- elo_df18_nonOH$Begin
elo_df18_nonOH$Reg <- NA
elo_df18_nonOH$div_avg <- div_reg[elo_df18_nonOH$Div,2]
elo_df18_nonOH[,c('sched_avg','hist_avg','Player_adj')] <- NA

elo_df18_nonOH['398','Current'] <- elo_df18['398','Current']
elo_df18_nonOH['9057','Current'] <- elo_df18['9057','Current']

elo_df18_full <- rbind(elo_df18[which(!is.na(elo_df18$Reg)),],elo_df18_nonOH)
model_elos <- elo_df18_full[order(elo_df18_full$Reg),]
model_elos$TeamID <- row.names(model_elos)
bracket <- read.csv('data sets/Playoffs Bracket.csv',stringsAsFactors=F)

spred_adj <- 18
std_dev <- 0
home_adv_flat <- 20
home_adv_var <- .25
home_adv_nonOH <- 30
home_adv_nonOH_both <- 30
k_mar <- 30
wk_slope <- slope_lin(x=.08,y=1.25,p=.85)
non_OHSAA_adj <- .55
y <- 2018

##remove lakeside vs mcdowell(PA) in week 9
JE_games_all <- JE_games_all[-c(which(JE_games_all$Opp_ID=='12530' & JE_games_all$Tm_ID=='140'),which(JE_games_all$Tm_ID=='12530' & JE_games_all$Opp_ID=='140')),]
JE_games_all <- JE_games_all[-c(which(JE_games_all$Opp_ID=='12650' & JE_games_all$Tm_ID=='25237'),which(JE_games_all$Tm_ID=='12650' & JE_games_all$Opp_ID=='25237')),]

##remove IN/IL playoff games & TBD games
JE_games_all <- JE_games_all[-c(which(JE_games_all$Opp_ID=='13499' | JE_games_all$Tm_ID=='13499')),]
JE_games_all <- JE_games_all[-c(which(JE_games_all$Opp_ID=='31999' | JE_games_all$Tm_ID=='31999')),]
JE_games_all <- JE_games_all[-c(which(JE_games_all$Opp_ID=='21999' | JE_games_all$Tm_ID=='21999')),]

JE_games_all$nonOHSAA_div <- elo_df18_nonOH$Div[match(JE_games_all$Opp_ID,row.names(elo_df18_nonOH))]
JE_games_all$Opp_Div <- OHSAA_df$Div.2018[match(JE_games_all$Opp_ID,OHSAA_df$OHSAA.ID)]
JE_games_all$Tm_lon <- OHSAA_df$lon[match(JE_games_all$Tm_ID,OHSAA_df$OHSAA.ID)]
JE_games_all$Tm_lat <- OHSAA_df$lat[match(JE_games_all$Tm_ID,OHSAA_df$OHSAA.ID)]
JE_games_all$Opp_lon <- OHSAA_df$lon[match(JE_games_all$Opp_ID,OHSAA_df$OHSAA.ID)]
JE_games_all$Opp_lat <- OHSAA_df$lat[match(JE_games_all$Opp_ID,OHSAA_df$OHSAA.ID)]
JE_games_all$Tm_Conf <- OHSAA_df$ConfSub.2018[match(JE_games_all$Tm_ID,OHSAA_df$OHSAA.ID)]
JE_games_all$Opp_Conf <- OHSAA_df$ConfSub.2018[match(JE_games_all$Opp_ID,OHSAA_df$OHSAA.ID)]
JE_games_all$Conf_Matchup <- ifelse(JE_games_all$Tm_Conf==JE_games_all$Opp_Conf,JE_games_all$Tm_Conf,NA)
JE_games_all$Conf_Matchup[which(JE_games_all$Conf_Matchup=='IND')] <- NA
JE_games_all$Conf_Matchup[which(JE_games_all$Conf_Matchup=='OH VAL')] <- NA
JE_games_all$trav_est <- sqrt((JE_games_all$Tm_lon-JE_games_all$Opp_lon)^2+(JE_games_all$Tm_lat-JE_games_all$Opp_lat)^2)*69
JE_games_all$trav_est[which(JE_games_all$Loc=='A')] <- -JE_games_all$trav_est[which(JE_games_all$Loc=='A')]
JE_games_all$trav_est[which(JE_games_all$Loc=='N')] <- 0


JE_games_all$home_adv <- home_adv_flat + home_adv_var * JE_games_all$trav_est
JE_games_all$home_adv[which(JE_games_all$Loc=='A')] <- -JE_games_all$home_adv[which(JE_games_all$Loc=='A')]
JE_games_all$home_adv[which(JE_games_all$Loc=='H')] <- JE_games_all$home_adv[which(JE_games_all$Loc=='H')]
JE_games_all$home_adv[which(JE_games_all$Loc=='N')] <- 0

JE_games_all$home_adv[which(JE_games_all$Loc=='H' & is.na(JE_games_all$home_adv))] <- home_adv_nonOH
JE_games_all$home_adv[which(JE_games_all$Loc=='A' & is.na(JE_games_all$home_adv))] <- -home_adv_nonOH

JE_games_all$home_adv[which(JE_games_all$Loc=='A' & JE_games_all$nonOHSAA==1 & JE_games_all$Tm_OH==0)] <- -home_adv_nonOH_both
JE_games_all$home_adv[which(JE_games_all$Loc=='H' & JE_games_all$nonOHSAA==1 & JE_games_all$Tm_OH==0)] <- home_adv_nonOH_both

Reg_Season <- JE_games_all
Reg_Season <- Reg_Season[which(Reg_Season$Playoff==0),]
Reg_Season <- Reg_Season[order(Reg_Season$Week),]

Reg_Season$Game_ID <- paste0(ifelse(Reg_Season$Tm_ID<Reg_Season$Opp_ID,paste0(Reg_Season$Tm_ID,'_',Reg_Season$Opp_ID),paste0(Reg_Season$Opp_ID,'_',Reg_Season$Tm_ID)),'_',Reg_Season$Week)
Reg_Season$WinID <- ifelse(Reg_Season$Win==1,Reg_Season$Tm_ID,Reg_Season$Opp_ID)
Reg_Season <- Reg_Season[which(Reg_Season$Excl_Harbin==0 & is.na(Reg_Season$Multi_Week)),]

region <- lapply(1:28, function(x) which(model_elos$Reg==x))
sim_df <- Reg_Season[match(unique(Reg_Season$Game_ID),Reg_Season$Game_ID),c('Game_ID','Tm_ID','Opp_ID','Week','home_adv','Opp_Div','Conf_Matchup','Tm_OH','nonOHSAA','WinID')]
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
my_auto_fun = function(elo_d) 0
OH_teams <- row.names(model_elos)[which(!is.na(model_elos$Reg))]
####end season prep

begin <- proc.time()
###full retro sim
j_odds <- lapply(1:1, function(z) {
sim_done <- sapply(1:100,function(x) dyn_sim(1,y,sim_df,region,match_id,opp_id_mx,L1_mx,games_cnt_L1,games_cnt_L2,game_cnt,my_auto_fun,OH_teams,model_elos,Conf,bracket,k_mar,wk_slope,non_OHSAA_adj,std_dev))
split_at <- nrow(sim_done)/3
playoffs <- sim_done[1:split_at,]
wins <- sim_done[(split_at+1):(split_at*2),]
conf <- sim_done[(split_at*2+1):(split_at*3),]
win_dist <- apply(wins,1,function(x) table(factor(x,1:8)))/100
po_dist <- apply(playoffs,1,function(x) table(factor(x,0:6)))/100
conf_dist <- apply(conf,1,sum)/100

return(list(win_dist,po_dist,conf_dist))
})

szn_win_dist <- do.call(cbind,lapply(1:1, function(j) rbind(Season=y,Week=j, j_odds[[j]][[1]])))
szn_po_dist <- do.call(cbind,lapply(1:1, function(j) rbind(Season=y,Week=j, j_odds[[j]][[2]])))
conf_po_dist <- do.call(cbind,lapply(1:1, function(j) rbind(Season=y,Week=j, j_odds[[j]][[3]])))

seeding <- t(szn_win_dist)
playoffs <- t(szn_po_dist)
conf <- t(conf_po_dist)
row.names(conf) <- row.names(playoffs)


miss <- 1-apply(seeding[,3:10],1,sum)
home <- apply(seeding[,3:6],1,sum)
seeding <- cbind(seeding,miss,home)

write.csv(seeding, 'C:/Users/Owner/Desktop/SWAER/output/2018/seeding1.csv')
write.csv(playoffs, 'C:/Users/Owner/Desktop/SWAER/output/2018/playoffs1.csv')
write.csv(conf, 'C:/Users/Owner/Desktop/SWAER/output/2018/conf1.csv')
proc.time() - begin
