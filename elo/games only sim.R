just_games <- function(wkst,yr,sim_df,my_auto_fun,model_elos,k_mar,wk_slope,non_OHSAA_adj,std_dev) {

curr_elos <- model_elos
curr_elos$Begin <- c(model_elos[,paste0(yr,' Week ',wkst-1)])
curr_elos$Current <- curr_elos$Begin
Div_repl <- aggregate(Begin~Div,curr_elos,FUN=mean,subset= !is.na(Reg))

if (wkst!=11) {
full_pred <- c()
for (w in wkst:10) {

wk_df <- sim_df[which(sim_df$Week==w),]

wk_df$Tm_elo_pre <- curr_elos[paste0(wk_df$Tm_ID),'Current']
wk_df$Opp_elo_pre <- curr_elos[paste0(wk_df$Opp_ID),'Current']
wk_df$Opp_elo_pre <- ifelse(is.na(wk_df$Opp_elo_pre), Div_repl[wk_df$Opp_Div,2], wk_df$Opp_elo_pre)
wk_df$elo_diff <- wk_df$Tm_elo_pre-wk_df$Opp_elo_pre+wk_df$home_adv
wk_df$spread <- wk_df$elo_diff/spred_adj
wk_df$Win_Prob <- 1/(1+10^(-wk_df$elo_diff/400))
wk_df$Std_dev <- (wk_df$elo_diff/spred_adj)/qnorm(wk_df$Win_Prob)
wk_df$Score_pred <- rnorm(nrow(wk_df)) * wk_df$Std_dev + wk_df$spread
wk_df$Win <- ifelse(wk_df$Score_pred>0, wk_df$Tm_ID, wk_df$Opp_ID)
wk_df$elo_change <- ifelse(wk_df$Score_pred>(wk_df$spread),1,-1) * (log(abs(wk_df$Score_pred-wk_df$spread)+1)+my_auto_fun(wk_df$elo_diff)) * k_mar * wk_slope[w] * ifelse(wk_df$Tm_OH==1 & wk_df$nonOHSAA==0,1,non_OHSAA_adj)
wk_df$elo_change[is.na(wk_df$elo_change)] <- 0
wk_df$Tm_elo_post <- wk_df$Tm_elo_pre + wk_df$elo_change
wk_df$Opp_elo_post <- wk_df$Opp_elo_pre - wk_df$elo_change
new_elo_df <- data.frame(row.names=c(wk_df$Tm_ID,wk_df$Opp_ID),NewElo=c(wk_df$Tm_elo_post,wk_df$Opp_elo_post))
curr_elos$Current <- ifelse(is.na(new_elo_df[paste0(row.names(curr_elos)),'NewElo']),curr_elos$Current,new_elo_df[paste0(row.names(curr_elos)),'NewElo'])
curr_elos[,paste0(yr,' Week ',w, 'Sim')] <- curr_elos$Current
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
return(curr_elos)
#return(full_pred)
}



final_elos <- elo_def(return_me='log')[[2]]
final_elos_nonOH <- elo_def(return_me='log')[[3]]
final_elos_nonOH[,names(final_elos)[-c(which(names(final_elos) %in% names(final_elos_nonOH)))]] <- NA
final_elos[,names(final_elos_nonOH)[-c(which(names(final_elos_nonOH) %in% names(final_elos)))]] <- NA

spred_adj <- 20
std_dev <- 0
home_adv_flat <- 20
home_adv_var <- .25
home_adv_nonOH <- 30
home_adv_nonOH_both <- 30
k_mar <- 30
wk_slope <- slope_lin(x=.08,y=1.25,p=.85)
non_OHSAA_adj <- .55

model_elos <- rbind(final_elos, final_elos_nonOH)
model_elos <- model_elos[order(model_elos$Reg),]
model_elos$TeamID <- row.names(model_elos)

JE_games$home_adv <- home_adv_flat + home_adv_var * JE_games$trav_est
JE_games$home_adv[which(JE_games$Loc=='A')] <- -JE_games$home_adv[which(JE_games$Loc=='A')]
JE_games$home_adv[which(JE_games$Loc=='H')] <- JE_games$home_adv[which(JE_games$Loc=='H')]
JE_games$home_adv[which(JE_games$Loc=='N')] <- 0

JE_games$home_adv[which(JE_games$Loc=='H' & is.na(JE_games$home_adv))] <- home_adv_nonOH
JE_games$home_adv[which(JE_games$Loc=='A' & is.na(JE_games$home_adv))] <- -home_adv_nonOH

JE_games_nonOH$home_adv <- 0
JE_games_nonOH$home_adv[which(JE_games_nonOH$Loc=='A' & JE_games_nonOH$nonOHSAA==0)] <- -home_adv_nonOH
JE_games_nonOH$home_adv[which(JE_games_nonOH$Loc=='H' & JE_games_nonOH$nonOHSAA==0)] <- home_adv_nonOH
JE_games_nonOH$home_adv[which(JE_games_nonOH$Loc=='A' & JE_games_nonOH$nonOHSAA==1)] <- -home_adv_nonOH_both
JE_games_nonOH$home_adv[which(JE_games_nonOH$Loc=='H' & JE_games_nonOH$nonOHSAA==1)] <- home_adv_nonOH_both

Reg_Season <- rbind(JE_games,JE_games_nonOH)
Reg_Season['131232','Multi_Week'] <- 1
Reg_Season['88046','Multi_Week'] <- 1
Reg_Season['83261','Multi_Week'] <- 1

y <- 2017
#start season

Reg_Season <- Reg_Season[which(Reg_Season$Season==y & Reg_Season$Playoff==0),]
Reg_Season <- Reg_Season[order(Reg_Season$Week),]

Reg_Season$Game_ID <- paste0(ifelse(Reg_Season$Tm_ID<Reg_Season$Opp_ID,paste0(Reg_Season$Tm_ID,'_',Reg_Season$Opp_ID),paste0(Reg_Season$Opp_ID,'_',Reg_Season$Tm_ID)),'_',Reg_Season$Week)
Reg_Season$WinID <- ifelse(Reg_Season$Win==1,Reg_Season$Tm_ID,Reg_Season$Opp_ID)
Reg_Season <- Reg_Season[which(Reg_Season$Excl_Harbin==0 & is.na(Reg_Season$Multi_Week)),]

####begin matrix setup
model_elos$Reg <- Reg_Season$Opp_Reg[match(model_elos$TeamID,Reg_Season$Opp_ID)]
model_elos$Reg[which(model_elos$Reg==0)] <- NA
model_elos$Div <- teams_df$Div[match(paste0(y,'_',model_elos$TeamID),teams_df$YrID)]
model_elos$Div <- ifelse(is.na(model_elos$Div),Reg_Season$nonOHSAA_div[match(model_elos$TeamID,Reg_Season$Opp_ID)], model_elos$Div)

sim_df <- Reg_Season[match(unique(Reg_Season$Game_ID),Reg_Season$Game_ID),c('Game_ID','Tm_ID','Opp_ID','Week','home_adv','Opp_Div','Conf_Matchup','Tm_OH','nonOHSAA','WinID')]
sim_df <- sim_df[which(sim_df$Week <= 10 & sim_df$Week > 0),]

my_auto_fun = function(elo_d) 0

wkst <- 1
end_rec <- sapply(1:100, function(i) just_games(wkst,y,sim_df,my_auto_fun,model_elos,k_mar,wk_slope,non_OHSAA_adj,std_dev))



end_rec <- sapply(1:100, function(i) {
only1 <- just_games(wkst,y,sim_df,my_auto_fun,model_elos,k_mar,wk_slope,non_OHSAA_adj,std_dev)
only1[,paste0(y,' Week ',wkst-1)]-only1[,paste0(y,' Week ',10, 'Sim')]
})
summary(end_rec[4,])


end_rec <- sapply(1:100, function(i) {
only1 <- just_games(wkst,y,sim_df,my_auto_fun,model_elos,k_mar,wk_slope,non_OHSAA_adj,std_dev)
c(only1$Tm_elo_post[which(only1$Tm_ID=='385')] - only1$Tm_elo_pre[which(only1$Tm_ID=='385')],only1$Opp_elo_post[which(only1$Opp_ID=='385')] - only1$Opp_elo_pre[which(only1$Opp_ID=='385')])
})
apply(end_rec,1,summary)[,order(c(only1$Week[which(only1$Tm_ID=='385')],only1$Week[which(only1$Opp_ID=='385')]))]

only1[which(only1$Opp_ID=='385' & only1$Week==1),]
test_score_pred <- rnorm(100) * 13.9 + 2
table(ifelse(test_score_pred>0,1,0))/100
elo_ch <- ifelse(test_score_pred>2,1,-1) * log(abs(test_score_pred-2)+1) * 1.25 * 30
summary(elo_ch)

