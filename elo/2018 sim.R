###Figure out ties
###multi week games
###week 0 games
#begin sim
dyn_sim <- function(wkst,yr,sim_df,region,match_id,opp_id_mx,L1_mx,games_cnt_L1,games_cnt_L2,game_cnt,my_auto_fun,OH_teams,model_elos,Conf,bracket,k_mar,wk_slope,non_OHSAA_adj,std_dev) {

curr_elos <- model_elos
Div_repl <- aggregate(div_avg~Div,curr_elos,FUN=mean,subset= !is.na(Reg))

if (wkst!=11) {
full_pred <- c()
for (w in wkst:10) {

wk_df <- sim_df[which(sim_df$Week==w),]
wk_df$Opp_Div <- ifelse(is.na(wk_df$Opp_Div),curr_elos[paste0(wk_df$Tm_ID),'Div'],wk_df$Opp_Div)


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
guess_div[which(row.names(elo_df18_nonOH)=='9918')] <- 7 #COF
OHSAA_gm <- ifelse(is.na(match(row.names(elo_df18_nonOH),JE_games_all$Tm_ID)),0,1)
remaining_div_pick <- row.names(elo_df18_nonOH)[which(OHSAA_gm==1 & is.na(last_div) & is.na(JE_div) & is.na(guess_div))]
unknown_div_opp <- sapply(remaining_div_pick,function(x) JE_games_all$Tm_ID[which(JE_games_all$Opp_ID==x & JE_games_all$Tm_OH==1)])
guess_div[match(names(unknown_div_opp),row.names(elo_df18_nonOH))] <- OHSAA_df$Div.2018[match(unknown_div_opp,OHSAA_df$OHSAA.ID)]

elo_df18_nonOH$City <- NA
elo_df18_nonOH$Reg <- NA
elo_df18_nonOH$Div <- ifelse(is.na(JE_div),ifelse(is.na(last_div),guess_div,last_div),JE_div)
elo_df18_nonOH$div_avg <- div_reg[elo_df18_nonOH$Div,2]
elo_df18_nonOH$School <- final_elos_nonOH$School[match(row.names(elo_df18_nonOH),row.names(final_elos_nonOH))]
elo_df18_nonOH$Begin <- final_elos_nonOH$Current[match(row.names(elo_df18_nonOH),row.names(final_elos_nonOH))]
elo_df18_nonOH$Current <- ifelse(is.na(elo_df18_nonOH$Begin),elo_df18_nonOH$div_avg,elo_df18_nonOH$Begin)

elo_df18_nonOH[,c('sched_avg','hist_avg','Player_adj')] <- NA

elo_df18_nonOH['398','Current'] <- elo_df18['398','Current']
elo_df18_nonOH['9057','Current'] <- elo_df18['9057','Current']

elo_df18_full <- rbind(elo_df18[which(!is.na(elo_df18$Reg)),],elo_df18_nonOH)
model_elos <- elo_df18_full[order(elo_df18_full$Reg),]
model_elos$TeamID <- row.names(model_elos)


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

#remove COF Acad from Harbin
JE_games_all$Excl_Harbin[c(which(JE_games_all$Opp_ID=='9918' | JE_games_all$Tm_ID=='9918'))] <- 1


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


##start with weekly
po_yr <- lapply(1:1, function(wkst) {

curr_elos <- model_elos
#change to div_avg
Div_repl <- aggregate(div_avg~Div,curr_elos,FUN=mean,subset= !is.na(Reg))

wk_df <- sim_df[which(sim_df$Week>=wkst),]

#exlcluding many second level pts because no opp div for nonOH v nonOH teams
#looks like I would already edit this into the JE file for prev sims
wk_df$Opp_Div <- ifelse(is.na(wk_df$Opp_Div),curr_elos[paste0(wk_df$Tm_ID),'Div'],wk_df$Opp_Div)

wk_df$Tm_elo_pre <- curr_elos[paste0(wk_df$Tm_ID),'Current']
wk_df$Opp_elo_pre <- curr_elos[paste0(wk_df$Opp_ID),'Current']
wk_df$Opp_elo_pre <- ifelse(is.na(wk_df$Opp_elo_pre), Div_repl[wk_df$Opp_Div,2], wk_df$Opp_elo_pre)
wk_df$elo_diff <- wk_df$Tm_elo_pre-wk_df$Opp_elo_pre+wk_df$home_adv
wk_df$spread <- ifelse(wk_df$elo_diff>0,main,-main) * (abs(wk_df$elo_diff)/spred_adj)^pwr
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

#tm_adj is discounting L2 points that would be gained by beating the team getting the points

tm_win_prob <- ifelse(matrix(full_pred$Tm_ID[match_id],10)==opp_id_mx, 1-matrix(full_pred$Win_Prob[match_id],10), matrix(full_pred$Win_Prob[match_id],10))
L1_pts <- apply(tm_win_prob * L1_mx,2,sum,na.rm=T)
tm_adj <- apply(tm_win_prob * (1-tm_win_prob) * matrix(rep((7-curr_elos$Div)/2+3.5+pts_adj,10),nrow=10,byrow=T),2,sum,na.rm=T)
L2_pts <- apply(tm_win_prob * matrix(L1_pts[paste0(opp_id_mx)],10),2,sum,na.rm=T)-tm_adj
total_avg <- (L1_pts/games_cnt_L1)+(L2_pts/games_cnt_L2)*10


L1_pts_curr <- apply(tm_win_prob * L1_mx * matrix(full_pred$Over[match_id],10),2,sum,na.rm=T)
L2_pts_curr <- apply(tm_win_prob * matrix(L1_pts_curr[paste0(opp_id_mx)],10) * matrix(full_pred$Over[match_id],10),2,sum,na.rm=T)
L1_pts_curr_avg <- apply(tm_win_prob * L1_mx * matrix(full_pred$Over[match_id],10),2,mean,na.rm=T)
total_avg_curr <- L1_pts_curr_avg+(L2_pts_curr/games_cnt_L2)*10
total_avg_curr <- ifelse(is.na(total_avg_curr),0,total_avg_curr)

seed_unordered <- unlist(lapply(region, function(rg) rank(-total_avg[rg],ties.method='random')))
seed <- seed_unordered[paste0(row.names(curr_elos))]

seed_unordered_curr <- unlist(lapply(region, function(rg) rank(-total_avg_curr[rg],ties.method='min',na.last = TRUE)))
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

write.csv(po_yr,'C:/Users/Owner/Desktop/SWAER/output/2018/crystal1.csv',row.names=F)

proc.time() - begin



###pre-season 2018
spread_adj <- 18
master_tms_id <- paste0(final_df$Season,'_',final_df$Week,'_',OHSAA_df$OHSAA.ID[match(final_df$Tm_Code,OHSAA_df$HyTek)])
master_opp_id <- paste0(final_df$Season,'_',final_df$Week,'_',OHSAA_df$OHSAA.ID[match(final_df$Opp_Code,OHSAA_df$HyTek)])
master_tms <- paste0(final_df$Season,'_',final_df$Week,'_',final_df$Tm_Code)
master_opp <- paste0(final_df$Season,'_',final_df$Opp_Code)

tm_list <- paste0('2018_1_',row.names(elo_df18_full)[which(!is.na(elo_df18_full$Reg))])
tm_list_HT <- paste0('2018_1_',OHSAA_df$HyTek[match(row.names(elo_df18_full)[which(!is.na(elo_df18_full$Reg))],OHSAA_df$OHSAA.ID)])
tm_list_no_wk <- paste0('2018_',OHSAA_df$HyTek[match(row.names(elo_df18_full)[which(!is.na(elo_df18_full$Reg))],OHSAA_df$OHSAA.ID)])


SWAER <- (elo_df18_full[which(!is.na(elo_df18_full$Reg)),'Current']-mean(elo_df18_full[which(!is.na(elo_df18_full$Reg)),'Current']))/spread_adj
div_avg <- aggregate(Current~Div,elo_df18_full,mean)
iSWAER <- elo_df18_full$Current-div_avg[elo_df18_full$Div,2]
iSWAER <- iSWAER[which(!is.na(elo_df18_full$Reg))]/spread_adj

SZN_Rank <- rank(-SWAER[which(!is.na(elo_df18_full$Reg))],ties.method='first')
SZN_DivRank <- unlist(lapply(1:7, function(x) rank(-SWAER[which(elo_df18_full$Div==x & !is.na(elo_df18_full$Reg))],ties.method='first')))
SZN_DivRank <- SZN_DivRank[match(1:715,unlist(lapply(1:7, function(x) which(elo_df18_full$Div==x & !is.na(elo_df18_full$Reg)))))]

final_df$Ovr_Rank <- ifelse(is.na(SZN_Rank[match(master_tms,tm_list_HT)]),final_df$Ovr_Rank,SZN_Rank[match(master_tms,tm_list_HT)])
final_df$Div_Rank <- ifelse(is.na(SZN_DivRank[match(master_tms,tm_list_HT)]),final_df$Div_Rank,SZN_DivRank[match(master_tms,tm_list_HT)])
final_df$SWAER <- ifelse(is.na(SWAER[match(master_tms,tm_list_HT)]),final_df$SWAER,SWAER[match(master_tms,tm_list_HT)])
final_df$Opp_Div_Rank <- ifelse(is.na(SZN_DivRank[match(master_opp,tm_list_no_wk)]),final_df$Opp_Div_Rank,SZN_DivRank[match(master_opp,tm_list_no_wk)])
final_df$Opp_Rank <- ifelse(is.na(SZN_Rank[match(master_opp,tm_list_no_wk)]),final_df$Opp_Rank,SZN_Rank[match(master_opp,tm_list_no_wk)])

seeding_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/2018/seeding1.csv',stringsAsFactors=F)
playoffs_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/2018/playoffs1.csv',stringsAsFactors=F)
conf_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/2018/conf1.csv',stringsAsFactors=F)
crystal_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/2018/crystal1.csv',stringsAsFactors=F)

crystal_df$Week <- crystal_df$Week + 1

playoffs_df$Final4 <- playoffs_df$X4+playoffs_df$X5+playoffs_df$X6
row.names(seeding_df) <- paste0(seeding_df$Season,'_',seeding_df$Week,'_',seeding_df$X)
row.names(conf_df) <- paste0(conf_df$Season,'_',conf_df$Week,'_',conf_df$X)
row.names(playoffs_df) <- paste0(playoffs_df$Season,'_',playoffs_df$Week,'_',playoffs_df$X)
row.names(crystal_df) <- paste0(crystal_df$Season,'_',crystal_df$Week,'_',crystal_df$TeamID)

final_df$Home11 <- ifelse(is.na(match(master_tms_id,row.names(seeding_df))),final_df$Home11,paste0(sprintf('%.0f',round(seeding_df$home[match(master_tms_id,row.names(seeding_df))],3)*100),'%'))
final_df$Conf <- ifelse(is.na(match(master_tms_id,row.names(conf_df))),final_df$Conf,paste0(sprintf('%.0f',round(conf_df$X.1[match(master_tms_id,row.names(conf_df))],3)*100),'%'))
final_df$In <- ifelse(is.na(match(master_tms_id,row.names(playoffs_df))),final_df$In, paste0(sprintf('%.0f',round(1-playoffs_df$X0[match(master_tms_id,row.names(playoffs_df))],3)*100),'%'))
final_df$Final4 <- ifelse(is.na(match(master_tms_id,row.names(playoffs_df))),final_df$Final4, paste0(sprintf('%.0f',round(playoffs_df$Final4[match(master_tms_id,row.names(playoffs_df))],3)*100),'%'))
final_df$Champ <- ifelse(is.na(match(master_tms_id,row.names(playoffs_df))),final_df$Champ, paste0(sprintf('%.0f',round(playoffs_df$X6[match(master_tms_id,row.names(playoffs_df))],3)*100),'%'))
final_df$Curr_Seed <- ifelse(is.na(match(master_tms_id,row.names(crystal_df))),final_df$Curr_Seed, crystal_df$curr_seed[match(master_tms_id,row.names(crystal_df))])
final_df$Proj_Seed <- ifelse(is.na(match(master_tms_id,row.names(crystal_df))),final_df$Proj_Seed, crystal_df$proj_seed[match(master_tms_id,row.names(crystal_df))])

final_df$Q_score[is.na(final_df$Q_score)] <- ''
write.csv(final_df,'C:/Users/Owner/Desktop/SWAER/output/final_df2.csv',row.names=F)



###view quick rankings
elo_df18_full[which(elo_df18_full$Div==5 & !is.na(elo_df18_full$Reg))[match(1:10,SZN_DivRank[which(elo_df18_full$Div==5 & !is.na(elo_df18_full$Reg))])],]
elo_df18_full[which(SZN_DivRank==1),]
elo_df18_full['666',]


###write 2018 rows into rankings and record files
rankings <- read.csv('C:/Users/Owner/Desktop/SWAER/output/hist rankings.csv',stringsAsFactors=F)
hist_rank18 <- data.frame(Tm_ID=row.names(elo_df18_full)[which(!is.na(elo_df18_full$Reg))],Season=2018,Week=0,Div=elo_df18_full$Div[which(!is.na(elo_df18_full$Reg))],Reg=elo_df18_full$Reg[which(!is.na(elo_df18_full$Reg))],DivRank=SZN_DivRank,OvrRank=SZN_Rank,iSWAER=iSWAER,SWAER=SWAER)
rankings <- rbind(rankings,hist_rank18)
write.csv(rankings,'C:/Users/Owner/Desktop/SWAER/output/hist rankings.csv',row.names=F)

records <- read.csv('C:/Users/Owner/Desktop/SWAER/output/rec rankings.csv',stringsAsFactors=F)
hist_rec18 <- data.frame(Week=0,Tm_ID=row.names(elo_df18_full)[which(!is.na(elo_df18_full$Reg))],Season=2018)
hist_rec18[,c('W','L','T','C_W','C_L','C_T')] <- 0
hist_rec18$Conf <- OHSAA_df$ConfSub.2018[match(hist_rec18$Tm_ID,OHSAA_df$OHSAA.ID)]
records <- rbind(records,hist_rec18)
write.csv(records,'C:/Users/Owner/Desktop/SWAER/output/rec rankings.csv',row.names=F)









###full bracket crystal ball
crystal_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/2018/crystal1.csv',stringsAsFactors=F)

curr_elos <- model_elos
wkst <- 0
yr <- 2018

seed <- crystal_df$proj_seed[match(paste0(curr_elos$TeamID,'_',wkst,'_',yr),paste0(crystal_df$TeamID,'_',crystal_df$Week,'_',crystal_df$Season))]

#begin playoffs
if (yr<=2012) {reg_format <- 1:24} else if(yr>=2016) {reg_format <- 1:28} else {reg_format <- 3:26}
playoffs_df <- expand.grid(Fav=c(1,4,3,2),Reg=reg_format) 
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

crystal_sim <- sapply(1:100, function(k) {
all_po_df <- c()
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
table(factor(as.numeric(c(all_po_df$FavID,all_po_df$DogID,rev(all_po_df$WinID)[game_cnt[5]:1])),OH_teams))
})
#end playoff sim

#might not use layout specified above
crystal_res <- t(apply(crystal_sim,1,function(x) table(factor(x,0:6))))
crystal_res <- cbind(curr_elos[which(!is.na(curr_elos$Reg)),],crystal_res)
crystal_res$placement <- NA

champ_order <- unlist(sapply(1:7, function(x) rank(-crystal_res[which(crystal_res$Div==x),'6'],ties.method='random')))
crystal_res[which(champ_order==1),]


####using general sim to backfill
playoff_res_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/2018/playoffs1.csv',stringsAsFactors=F)

curr_elos <- model_elos
curr_elos$Seed <- seed
curr_elos <- curr_elos[which(seed<=8),]
po_sim <- playoff_res_df[match(paste0(curr_elos$TeamID,'_',wkst+1,'_',yr),paste0(playoff_res_df$X,'_',playoff_res_df$Week,'_',playoff_res_df$Season)),paste0('X',1:6)]
po_sim <- cbind(curr_elos,po_sim)
po_sim$reg_quad <- paste0(po_sim$Reg,'_',ifelse(po_sim$Seed>=5,9-po_sim$Seed,po_sim$Seed))
po_sim <- po_sim[order(po_sim$reg_quad),]
po_sim <- po_sim[order(po_sim$Reg),]

po_sim$placement <- NA



for (j in 5:2) po_sim[,paste0('Rnd',j)] <- apply(po_sim[,paste0('X',6:j)],1,sum)

champ_order <- unlist(sapply(1:7, function(x) rank(-po_sim[which(po_sim$Div==x),'X6'],ties.method='random')))
po_sim$placement[which(champ_order==1)] <- 6
po_sim$Rnd5[which(po_sim$Reg %in% po_sim$Reg[which(champ_order==1)])] <- 0
po_sim$Rnd4[which(po_sim$Reg %in% po_sim$Reg[which(champ_order==1)])] <- 0

runner_order <- unlist(sapply(1:7, function(x) rank(-po_sim[which(po_sim$Div==x),'Rnd5'],ties.method='random')))
po_sim$placement[which(runner_order==1)] <- 5
po_sim$Rnd4[which(po_sim$Reg %in% po_sim$Reg[which(runner_order==1)])] <- 0
reg_order <- unlist(sapply(1:28, function(x) rank(-po_sim[which(po_sim$Reg==x),'Rnd4'],ties.method='random')))
po_sim$placement[which(reg_order==1 & po_sim$Rnd4!=0)] <- 4

po_sim$reg_semi <- paste0(po_sim$Reg,'_',ifelse(is.na(match(po_sim$Seed,c(1,4,5,8))),1,2))
placed_quads <- aggregate(placement~reg_semi,po_sim,max)
po_sim$Rnd3[which(po_sim$reg_semi %in% placed_quads[,1])] <- 0
reg_semi_order <- unlist(sapply(1:28, function(x) rank(-po_sim[which(po_sim$Reg==x),'Rnd3'],ties.method='random')))
po_sim$placement[which(reg_semi_order==1)] <- 3

reg_quads <- aggregate(placement~reg_quad,po_sim,max)
po_sim$Rnd2[which(po_sim$reg_quad %in% reg_quads[,1])] <- 0
reg_quad_order <- unlist(sapply(unique(po_sim$reg_quad), function(x) rank(-po_sim[which(po_sim$reg_quad==x),'Rnd2'],ties.method='random')))
po_sim$placement[which(reg_quad_order==1 & po_sim$Rnd2!=0)] <- 2


####begin bracket creation
png('C:/Users/Owner/Documents/GitHub/SWAER/bracket1.png',width=800, height=450)
my_div <- 5

reg_in_champ <- po_sim$Reg[which(po_sim$placement>=5 & po_sim$Div==my_div)]
reg_out_champ <- po_sim$Reg[which(po_sim$placement==4 & po_sim$Div==my_div)]
use_mapping <- ifelse(which(names(trav_order(my_div)) %in% c(paste0(reg_in_champ[1],'v',reg_in_champ[2]),paste0(reg_out_champ[1],'v',reg_out_champ[2])))==1,names(trav_order(my_div)[2]),names(trav_order(my_div)[1]))
reg_side <- strsplit(use_mapping,'v')

####create a full bracket df
#dev.new(width=800, height=450)
par(mar=c(0,0,4,0))
plot(NA,ylim=c(0,48),xlim=c(0,94),main=paste0('OHSAA Division ',as.roman(my_div),' Preseason SWAER "Crystal Ball" Playoff Forecast'),axes=F,cex.main=1.5)

arrows(rep(0,16),seq(1,48,3),rep(10,16),seq(1,48,3),length=0)
arrows(rep(10,8),seq(2.5,48,6),rep(20,8),seq(2.5,48,6),length=0)
arrows(rep(20,4),seq(5.5,48,12),rep(30,4),seq(5.5,48,12),length=0)
arrows(rep(30,2),seq(11,48,24),rep(40,2),seq(11,48,24),length=0)
arrows(40,30,50,30,length=0)

arrows(rep(94,16),seq(1,48,3),rep(84,16),seq(1,48,3),length=0)
arrows(rep(84,8),seq(2.5,48,6),rep(74,8),seq(2.5,48,6),length=0)
arrows(rep(74,4),seq(5.5,48,12),rep(64,4),seq(5.5,48,12),length=0)
arrows(rep(64,2),seq(11,48,24),rep(54,2),seq(11,48,24),length=0)
arrows(44,16,54,16,length=0)

arrows(rep(10,8),seq(1,48,6),rep(10,8),seq(4,48,6),length=0)
arrows(rep(20,4),seq(2.5,48,12),rep(20,4),seq(8.5,48,12),length=0)
arrows(rep(30,2),seq(5.5,48,24),rep(30,2),seq(17.5,48,24),length=0)
arrows(40,11,40,35,length=0)

arrows(rep(84,8),seq(1,48,6),rep(84,8),seq(4,48,6),length=0)
arrows(rep(74,4),seq(2.5,48,12),rep(74,4),seq(8.5,48,12),length=0)
arrows(rep(64,2),seq(5.5,48,24),rep(64,2),seq(17.5,48,24),length=0)
arrows(54,11,54,35,length=0)

place_ord <- expand.grid(Seed=c(1,8,4,5,6,3,7,2),Reg=((my_div-1)*4+1):((my_div-1)*4+4))
place_ord$RegSeed <- paste0(place_ord$Reg,'_',place_ord$Seed)
place_ord$mat_row <- match(place_ord$RegSeed,paste0(po_sim$Reg,'_',po_sim$Seed))
place_ord$school <- po_sim$School[place_ord$mat_row]
place_ord$half <- 1
place_ord$half[which(place_ord$Reg==reg_side[[1]][1] | place_ord$Reg==reg_side[[1]][2])] <- 2
place_ord$placement <- po_sim$placement[place_ord$mat_row]

text(rep(1,16),seq(46.2,1,-3),place_ord$school[which(place_ord$half==1)],adj=c(0,0),cex=.5)
text(rep(11,8),seq(44.7,1,-6),place_ord$school[which(place_ord$half==1 & place_ord$placement>=2)],adj=c(0,0),cex=.5)
text(rep(21,4),seq(41.7,1,-12),place_ord$school[which(place_ord$half==1 & place_ord$placement>=3)],adj=c(0,0),cex=.5)
text(rep(31,2),seq(35.2,1,-24),place_ord$school[which(place_ord$half==1 & place_ord$placement>=4)],adj=c(0,0),cex=.5)
text(41,30.2,place_ord$school[which(place_ord$half==1 & place_ord$placement>=5)],adj=c(0,0),cex=.5)

text(rep(93,16),seq(46.2,1,-3),place_ord$school[which(place_ord$half==2)],adj=c(1,0),cex=.5)
text(rep(83,8),seq(44.7,1,-6),place_ord$school[which(place_ord$half==2 & place_ord$placement>=2)],adj=c(1,0),cex=.5)
text(rep(73,4),seq(41.7,1,-12),place_ord$school[which(place_ord$half==2 & place_ord$placement>=3)],adj=c(1,0),cex=.5)
text(rep(63,2),seq(35.2,1,-24),place_ord$school[which(place_ord$half==2 & place_ord$placement>=4)],adj=c(1,0),cex=.5)
text(53,16.2,place_ord$school[which(place_ord$half==2 & place_ord$placement>=5)],adj=c(1,0),cex=.5)

arrows(41,24,53,24,length=0)
text(47,25,place_ord$school[which(place_ord$placement>=6)],cex=.5)
dev.off()






po_sim[,c('lon','lat')] <- OHSAA_df[match(row.names(po_sim),OHSAA_df$OHSAA.ID),c('lon','lat')]
reg_midp <- aggregate(cbind(lon,lat)~Reg,po_sim,mean,subset= placement>=3)
r_trav_est <- function(r1,r2) sqrt(sum(((reg_midp[r1,2:3]-reg_midp[r2,2:3]) * c(52,69))^2))

trav_order <- function(div) {
reg1 <- (div-1)*4
final_v <- c(r_trav_est(reg1+1,reg1+2) + r_trav_est(reg1+3,reg1+4),r_trav_est(reg1+1,reg1+3) + r_trav_est(reg1+2,reg1+4),r_trav_est(reg1+1,reg1+4) + r_trav_est(reg1+2,reg1+3))
names(final_v) <- c(paste0(reg1+1,'v',reg1+2),paste0(reg1+1,'v',reg1+3),paste0(reg1+1,'v',reg1+4))
sort(final_v)
}








######harbin "bank" idea
tm_id <- '1354'

sum(diag(opp_win_mx * div_mx) * tm_win_prob[,tm_id])
opp_win_mx <- tm_win_prob[,paste0(opp_id_mx[,tm_id])]
div_mx <- L1_mx[,match(opp_id_mx[,tm_id],names(L1_pts))]
div_mx <- div_mx/9.7
diag(opp_win_mx) <- 1 
tm_win_mx <- matrix(rep(tm_win_prob[,tm_id],10),10,byrow=T)
diag(div_mx) <- L1_mx[,match(tm_id,names(L1_pts))]/10

full_harbin <- tm_win_mx * opp_win_mx * div_mx
sum(full_harbin,na.rm=T)
apply(full_harbin,2,sum,na.rm=T)


head(opp_id_mx)

