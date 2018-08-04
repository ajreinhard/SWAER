rm(list=ls())
setwd('C:/Users/Owner/Documents/GitHub/SWAER')

teams_df <- read.csv('data sets/JoeEitel Teams.csv',stringsAsFactors=F)
JE_games <- read.csv('data sets/JoeEitel GameLog.csv',stringsAsFactors=F)
JE_games_nonOH <- read.csv('data sets/JoeEitel GameLog NON_OH.csv',stringsAsFactors=F)
nonOH_div <- read.csv('elo/nonOH_div.csv',stringsAsFactors=F)

nonOH_div$nonOH_div[which(nonOH_div$nonOH_div==0 & as.numeric(substr(nonOH_div$X,1,4))>=2013)] <- 7
nonOH_div$nonOH_div[which(nonOH_div$nonOH_div==0 & as.numeric(substr(nonOH_div$X,1,4))<2013)] <- 6

JE_games_nonOH$Opp_Div[which(is.na(JE_games_nonOH$Opp_Div) & JE_games_nonOH$Season>=2013)] <- 7
JE_games_nonOH$Opp_Div[which(is.na(JE_games_nonOH$Opp_Div) & JE_games_nonOH$Season<2013)] <- 6

JE_games$Tm_OH <- 1
JE_games_nonOH$Tm_Conf <- NA
JE_games_nonOH$Opp_Conf <- NA
JE_games_nonOH$Conf_Matchup <- NA
JE_games_nonOH$Tm_OH <- 0

####quick elo function
do_elo <- function(elo_parms) {

#apply elo parms
JE_games$home_adv <- elo_parms$home_adv_flat + (elo_parms$home_adv_var * JE_games$trav_est)
JE_games$home_adv[which(JE_games$Loc=='A')] <- -JE_games$home_adv[which(JE_games$Loc=='A')]
JE_games$home_adv[which(JE_games$Loc=='H')] <- JE_games$home_adv[which(JE_games$Loc=='H')]
JE_games$home_adv[which(JE_games$Loc=='N')] <- 0

JE_games$home_adv[which(JE_games$Loc=='H' & is.na(JE_games$home_adv))] <- elo_parms$home_adv_nonOH
JE_games$home_adv[which(JE_games$Loc=='A' & is.na(JE_games$home_adv))] <- -elo_parms$home_adv_nonOH

JE_games_nonOH$home_adv <- 0
JE_games_nonOH$home_adv[which(JE_games_nonOH$Loc=='A' & JE_games_nonOH$nonOHSAA==0)] <- -elo_parms$home_adv_nonOH
JE_games_nonOH$home_adv[which(JE_games_nonOH$Loc=='H' & JE_games_nonOH$nonOHSAA==0)] <- elo_parms$home_adv_nonOH
JE_games_nonOH$home_adv[which(JE_games_nonOH$Loc=='A' & JE_games_nonOH$nonOHSAA==1)] <- -elo_parms$home_adv_nonOH_both
JE_games_nonOH$home_adv[which(JE_games_nonOH$Loc=='H' & JE_games_nonOH$nonOHSAA==1)] <- elo_parms$home_adv_nonOH_both
###

most_recent_season <- aggregate(Yr~TeamID,data=teams_df,FUN=max)
elo_df <- data.frame(row.names=most_recent_season$TeamID,t(sapply(1:nrow(most_recent_season),function(x) teams_df[which(teams_df$TeamID==most_recent_season$TeamID[x] & teams_df$Yr==most_recent_season$Yr[x]),c('City','School')])))
elo_df$City <- unlist(elo_df$City)
elo_df$School <- unlist(elo_df$School)

###adjust for reg_fac doing a sched average of all 1500s
elo_parms$Div_start <- elo_parms$Div_start + (elo_parms$Div_start-1500) * elo_parms$reg_fac

###nonOHSAA
nonOH_elo_df <- data.frame(row.names=unique(JE_games_nonOH$Tm_ID))
nonOH_elo_df$Div <- nonOH_div$nonOH_div[match(paste0('2002_',row.names(nonOH_elo_df)),nonOH_div$X)]
nonOH_elo_df$Current <- elo_parms$Div_start[nonOH_elo_df$Div]
###


elo_df$Div <- teams_df$Div[match(paste0('2002_',row.names(elo_df)),teams_df$YrID)]
elo_df$Begin <- elo_parms$Div_start[elo_df$Div]
elo_df$Begin[is.na(elo_df$Begin)] <- 1500
elo_df$Current <- elo_df$Begin

all_elo <- c()
for (yr in 2002:2017) {

old_div <- elo_df$Div
elo_df$Div <- teams_df$Div[match(paste0(yr,'_',row.names(elo_df)),teams_df$YrID)]
elo_df$Reg <- teams_df$Reg[match(paste0(yr,'_',row.names(elo_df)),teams_df$YrID)]

div_reg <- aggregate(Current~Div,data=elo_df,FUN=mean)
elo_df$div_avg <- div_reg$Current[match(elo_df$Div,div_reg$Div)]
elo_df$div_avg <- ifelse(is.na(elo_df$div_avg),elo_df$Current,elo_df$div_avg)

elo_df$Current <- ifelse(is.na(old_div) & !is.na(elo_df$Div) & elo_df$Current==1500,elo_df$div_avg,elo_df$Current)

#non OHSAA
nonOH_elo_df$School <- nonOH_div$School[match(paste0(yr,'_',row.names(nonOH_elo_df)),nonOH_div$X)]
nonOH_elo_df$Div <- nonOH_div$nonOH_div[match(paste0(yr,'_',row.names(nonOH_elo_df)),nonOH_div$X)]
nonOH_elo_df$ST <- nonOH_div$ST[match(paste0(yr,'_',row.names(nonOH_elo_df)),nonOH_div$X)]
nonOH_elo_df$div_avg <- div_reg$Current[match(nonOH_elo_df$Div,div_reg$Div)]
nonOH_elo_df$Current <- ifelse(!is.na(nonOH_elo_df$Div) & is.na(nonOH_elo_df$Current),nonOH_elo_df$div_avg,nonOH_elo_df$Current)
nonOH_elo_df$Current <- nonOH_elo_df$Current * (1-elo_parms$non_OH_fac) + nonOH_elo_df$div_avg * elo_parms$non_OH_fac
nonOH_elo_df$ST_adj <- elo_parms$ST_adj[paste0(nonOH_elo_df$ST)]
nonOH_elo_df$ST_adj[is.na(nonOH_elo_df$ST_adj)] <- 1
nonOH_elo_df$Current <- nonOH_elo_df$Current * nonOH_elo_df$ST_adj
#

reg_sched <- merge(data.frame(Opp_ID=as.numeric(row.names(elo_df)),Elo=elo_df$Current),JE_games[which(JE_games$Season==yr & JE_games$Playoff==0),c('Tm_ID','Opp_ID')],by='Opp_ID',all.y=T)

sched_reg <- aggregate(Elo~Tm_ID,reg_sched,mean,na.rm=T)
elo_df$sched_avg <- sched_reg$Elo[match(row.names(elo_df),sched_reg$Tm_ID)]
elo_df$sched_avg <- ifelse(is.na(elo_df$sched_avg),elo_df$Current,elo_df$sched_avg)

if (yr > (2002+length(elo_parms$look_back)+1)) {
elo_df$hist_avg <- apply(cbind(elo_df[,c(paste0(yr-(1:length(elo_parms$look_back)),' Week 15'))],NA),1,function(j) sum(unlist(c(elo_parms$look_back,NA)*j),na.rm=T))
elo_df$hist_avg <- ifelse(is.na(elo_df$hist_avg),elo_df$Current,elo_df$hist_avg)
elo_df$Begin <- elo_df$Current * (1-elo_parms$reg_fac-elo_parms$hist_fac-elo_parms$div_fac) + elo_df$sched_avg * (elo_parms$reg_fac) + elo_df$hist_avg * (elo_parms$hist_fac) + elo_df$div_avg * (elo_parms$div_fac)
} else {
elo_df$Begin <- elo_df$Current * (1-elo_parms$reg_fac-elo_parms$div_fac) + elo_df$sched_avg * (elo_parms$reg_fac) + elo_df$div_avg * (elo_parms$div_fac)
}

if (yr >= 2015 & elo_parms$pl_adj == TRUE) {
elo_df$Player_adj <- elo_parms$mod_player_adj[match(paste0(yr,'_',row.names(elo_df)),names(elo_parms$mod_player_adj))]
elo_df$Player_adj[is.na(elo_df$Player_adj)] <- 0
elo_df$Begin <- elo_df$Begin + elo_df$Player_adj
}

elo_df$Current <- elo_df$Begin
elo_df[,paste0(yr,' Week 0')] <- elo_df$Begin
nonOH_elo_df[,paste0(yr,' Week 0')] <- nonOH_elo_df$Current

gms_by_wk <- lapply(1:15, function(x) rbind(JE_games[which(JE_games$Season==yr & JE_games$Week==x),],JE_games_nonOH[which(JE_games_nonOH$Season==yr & JE_games_nonOH$Week==x),]))

for (wk in 1:15) {
week_df <- gms_by_wk[[wk]]

week_df$Tm_elo.curr <- ifelse(week_df$Tm_OH==0, nonOH_elo_df$Current[match(week_df$Tm_ID,row.names(nonOH_elo_df))], elo_df$Current[match(week_df$Tm_ID,row.names(elo_df))])
week_df$Opp_elo.curr <- ifelse(week_df$nonOHSAA==1, nonOH_elo_df$Current[match(week_df$Opp_ID,row.names(nonOH_elo_df))],elo_df$Current[match(week_df$Opp_ID,row.names(elo_df))])
week_df$Opp_elo.curr <- ifelse(is.na(week_df$Opp_elo.curr) & week_df$Tm_OH==0, div_reg$Current[week_df$Opp_Div] * nonOH_elo_df[paste0(week_df$Tm_ID),'ST_adj'], week_df$Opp_elo.curr)
week_df$elo_diff <- week_df$Tm_elo.curr - week_df$Opp_elo.curr + week_df$home_adv
week_df$elo_win <- 1/(10^(-week_df$elo_diff/400)+1)

if (elo_parms$K_mult == TRUE) week_df$Tm_elo.new <- week_df$Tm_elo.curr + (week_df$Win-week_df$elo_win) * elo_parms$K * elo_parms$margin_adj(week_df$Score_diff, week_df$elo_diff) * elo_parms$wk_slope[wk]
#if (elo_parms$K_mult == FALSE) week_df$Tm_elo.new <- week_df$Tm_elo.curr + (week_df$Win-week_df$elo_win) * elo_parms$K * elo_parms$wk_slope[wk]  + elo_parms$K_mar * (elo_parms$margin_adj(week_df$Score_diff, week_df$elo_diff) - week_df$elo_win) * elo_parms$wk_slope[wk] * ifelse(week_df$Tm_OH==1 & week_df$nonOHSAA==0,1,elo_parms$non_OH_gm) + elo_parms$auto_corr_adj(week_df$elo_diff)
if (elo_parms$K_mult == FALSE) week_df$Tm_elo.new <- week_df$Tm_elo.curr + (week_df$Win-week_df$elo_win) * elo_parms$K * elo_parms$wk_slope[wk]  + elo_parms$K_mar * (elo_parms$margin_adj(week_df$Score_diff, week_df$elo_diff)+elo_parms$auto_corr_adj(week_df$elo_diff)) * elo_parms$wk_slope[wk] * ifelse(week_df$Tm_OH==1 & week_df$nonOHSAA==0,1,elo_parms$non_OH_gm)
week_df$adj_amount <- elo_parms$auto_corr_adj(week_df$elo_diff)
#na teams will have no elo change
week_df$Tm_elo.new <- ifelse(is.na(week_df$Tm_elo.new) | week_df$Excl_Elo==1,week_df$Tm_elo.curr,week_df$Tm_elo.new)

elo_df$Current[match(week_df$Tm_ID[which(week_df$Tm_OH==1)],row.names(elo_df))] <- week_df$Tm_elo.new[which(week_df$Tm_OH==1)]
nonOH_elo_df$Current[match(week_df$Tm_ID[which(week_df$Tm_OH==0)],row.names(nonOH_elo_df))] <- week_df$Tm_elo.new[which(week_df$Tm_OH==0)]

nonOH_elo_df[,paste0(yr,' Week ',wk)] <- nonOH_elo_df$Current
elo_df[,paste0(yr,' Week ',wk)] <- elo_df$Current

gms_by_wk[[wk]] <- week_df
}

gms_by_wk <- do.call(rbind,gms_by_wk)
all_elo <- c(all_elo,list(gms_by_wk))
}

all_elo <- do.call(rbind,all_elo)

mean17 <- mean(ifelse(all_elo$elo_win[which(all_elo$Win==1 & all_elo$Season==2017 & all_elo$Tm_OH==1 & all_elo$nonOHSAA==0 & all_elo$Excl_Elo==0)]>.5,1,0))
logloss <- -mean(log(all_elo$elo_win[which(all_elo$Win==1 & all_elo$Season==2017 & all_elo$Tm_OH==1 & all_elo$nonOHSAA==0 & all_elo$Excl_Elo==0)]),na.rm=T)
elo_mean <- mean(elo_df$Current)
mean_all <- mean(ifelse(all_elo$elo_win[which(all_elo$Win==1 & all_elo$Season>=2007 & all_elo$Tm_OH==1 & all_elo$nonOHSAA==0 & all_elo$Excl_Elo==0)]>.5,1,0),na.rm=T)
logloss_all <- -mean(log(all_elo$elo_win[which(all_elo$Win==1 & all_elo$Season>=2007 & all_elo$Tm_OH==1 & all_elo$nonOHSAA==0 & all_elo$Excl_Elo==0)]),na.rm=T)


if (elo_parms$return_me=='cycle') return(c(mean17,logloss,mean_all,logloss_all,elo_mean))
if (elo_parms$return_me=='log') return(list(all_elo,elo_df,nonOH_elo_df))
}


#####looping function for cross-validation
cv_testing <- function(elo_def,testing_var,inputs,...) {
print(paste0(round((length(inputs)+1)/15,2),' min expected wait time'))
flush.console()
begin <- proc.time()

output <- sapply(inputs, function(x) {
formals(elo_def)[[testing_var]] <- x
elo_def(return_me='cycle',...)
})

if (is.list(inputs)) {inputs <- 1:length(inputs)}
current_def <- elo_def()

par(mfrow=c(1,2))
plot(inputs,output[1,],type='l',col='red',ylim=c(min(output[c(1,3),],.8106,current_def[c(1,3)]),max(output[c(1,3),],.8106,current_def[c(1,3)])),main='Accuracy')
lines(cbind(inputs,output[3,]),type='l',col='green')
lines(cbind(inputs,.8106),type='l',col='blue')
lines(cbind(inputs,current_def[3]),type='l',col='gold')
plot(inputs,output[2,],type='l',col='red',ylim=c(min(output[c(2,4),],.4157,current_def[c(2,4)]),max(output[c(2,4),],.4157,current_def[c(2,4)])),main='Log Loss')
lines(cbind(inputs,output[4,]),type='l',col='green')
lines(cbind(inputs,.4157),type='l',col='blue')
lines(cbind(inputs,current_def[4]),type='l',col='gold')
legend('right',legend=c('2017','Drew 2017','all years','Default'),lty=c(1,1),col=c('red','blue','green','gold'))

colnames(output) <- inputs

print(proc.time() - begin)
print(output)
print(apply(output[c(1,3),],1,function(x) paste0(round(max(x),4)*100,' @ ',inputs[rev(order(x))[1]])))
print(apply(output[c(2,4),],1,function(x) paste0(round(min(x),4)*100,' @ ',inputs[order(x)[1]])))
}

###internal functions
slope_lin <- function(x=0.08,y=1.25,p=.85) c(seq(y,y-x*9,-x),rep(p,5))
re_base <- function(x) x/sum(x)
easy_auto_fun = function(term_list) {
term_list$elo_d <- ifelse(term_list$elo_d>=800,800,ifelse(term_list$elo_d<=-800,-800,term_list$elo_d))
return(term_list$elo_d^5/(term_list$t5*10^term_list$t5p)+term_list$elo_d^4/ifelse(term_list$elo_d>0,(term_list$t4*10^term_list$t4p),-(term_list$t4*10^term_list$t4p))+term_list$elo_d^3/(term_list$t3*10^term_list$t3p)+term_list$elo_d^2/ifelse(term_list$elo_d>0,(term_list$t2*10^term_list$t2p),-(term_list$t2*10^term_list$t2p))+term_list$elo_d/(term_list$t1*10^term_list$t1p))
}
easy_auto_fun2 = function(term_list) {
term_list$elo_d <- ifelse(term_list$elo_d>=800,800,ifelse(term_list$elo_d<=-800,-800,term_list$elo_d))
return(term_list$elo_d^3/(term_list$t3*10^term_list$t3p)+term_list$elo_d^2/ifelse(term_list$elo_d>0,(term_list$t2*10^term_list$t2p),-(term_list$t2*10^term_list$t2p))+term_list$elo_d/(term_list$t1*10^term_list$t1p))
}
log_odds <- function(x) 1/(1+exp(-x))
main <- 1.7
pwr <- .84
############################
###elo default parms
############################
####Drew had log loss of .4157 & 81.06% accuracy in 2017
elo_def <- function(K = 0, K_mar = 30, home_adv_flat = 20, home_adv_var = .25, home_adv_nonOH = 30, home_adv_nonOH_both = 30,
	reg_fac = .3, div_fac = 0, hist_fac = .2, non_OH_gm = .55, non_OH_fac = 0,
	look_back = c(.25,.25,.25,.25), pl_adj = TRUE, mod_player_adj = player_adj_pred,
	Div_start = c(1700,1600,1550,1460,1410,1280),
	K_mult = FALSE,
	wk_slope = slope_lin(x=0.08,y=1.25,p=.85),
	margin_adj = function(mar, elo_d) ifelse(mar>(ifelse(elo_d>0,main,-main) * (abs(elo_d)/18)^pwr),1,-1) * abs(mar-ifelse(elo_d>0,main,-main) * (abs(elo_d)/18)^pwr)/7,
	auto_corr_adj = function(elo_d) 0,
	ST_adj = c('OH'=1),
	return_me = 'cycle') {

do_elo(list(K = K, K_mar = K_mar, home_adv_flat = home_adv_flat, home_adv_var = home_adv_var, home_adv_nonOH = home_adv_nonOH, home_adv_nonOH_both = home_adv_nonOH_both,
	reg_fac = reg_fac, div_fac = div_fac, hist_fac = hist_fac, non_OH_gm = non_OH_gm, non_OH_fac = non_OH_fac,
	look_back = look_back, pl_adj = pl_adj, mod_player_adj = mod_player_adj,
	Div_start = Div_start,
	K_mult = K_mult,
	wk_slope = wk_slope,
	margin_adj = margin_adj,
	auto_corr_adj = auto_corr_adj,
	ST_adj = ST_adj,
	return_me = return_me))}

#auto_corr_adj = function(elo_d) ifelse(abs(elo_d)<=1000,ifelse(elo_d>0,1,-1) * ((elo_d^2)/400000) - elo_d/500,ifelse(elo_d>0,.97,-.97)),
#auto_corr_adj = function(elo_d) ifelse(abs(elo_d)<=1000,ifelse(elo_d>0,1,-1) * ((elo_d^2)/200000) - elo_d/400,ifelse(elo_d>0,.97,-.97)),
#4.85, 20, 40.82 log loss with 420k & 710

######old margin adj
	#margin_adj = function(mar, elo_d) pnorm(mar/((elo_d/20.7)/qnorm((1/(1+10^(-elo_d/400)))))),
	#auto_corr_adj = function(elo_d) easy_auto_fun(list(elo_d=elo_d,
		#t5 = 3.93, t4 = 2.94, t3 = -3.67, t2 = -6.38, t1 = 1.21,
		#t5p = 12, t4p = 11, t3p = 6, t2p = 5, t1p = 1
			#)),

#	margin_adj = function(mar, elo_d) ifelse(mar>(elo_d/26),1,-1) * (pnorm(abs((mar-(elo_d/26)))/25)-.5),
#	auto_corr_adj = function(elo_d) easy_auto_fun2(list(elo_d=elo_d,
#		t3 = 1.40, t2 = 4.66, t1 = -1.20,
#		t3p = 7, t2p = 6, t1p = 1
#			)),
#	margin_adj = function(mar, elo_d) ifelse(mar>(elo_d/20),1,-1) * log((abs(mar-(elo_d/20))+1)/1),


elo_def()

cv_testing(elo_def, 'K', seq(170,190,5),K_mult=T,margin_adj = function(mar, elo_d) 1, K=180) 
pred_all <- elo_def(K_mult=T,margin_adj = function(mar, elo_d) ((abs(mar)+3)^0.8)/(7.5+.006*elo_d) * ifelse(mar>0,-1,1), K=180,return_me='log')[[1]]
elo_def(K_mult=T,margin_adj = function(mar, elo_d) ((abs(mar)+3)^.9)/(6+0.003*ifelse(mar*elo_d>0,abs(elo_d),-abs(elo_d))), K=70)

(21^.8)/2.4

margin_adj(20,20*20)
margin_adj(-20,20*-20)


3/.002/20


pred_all <- elo_def(K_mult=T,margin_adj = function(mar, elo_d) ((abs(mar)+3)^.9)/(6+0.003*ifelse(mar*elo_d>0,abs(elo_d),-abs(elo_d))), K=70,return_me='log')[[1]]





1.5-.006 * 400

pwr <- .81
main <- 2.3

pwr <- 1
main <- 1

margin_adj = function(mar, elo_d) ifelse(mar>(ifelse(elo_d>0,main,-main) * (abs(elo_d)/20)^pwr),1,-1) * log(abs(mar-ifelse(elo_d>0,main,-main) * (abs(elo_d)/20)^pwr)+1),

margin_adj = function(mar, elo_d) ifelse(mar>(elo_d/20),1,-1) * log(abs(mar-(elo_d/20))+1) * ((1.6171 * (abs(elo_d)/20)^.8637)/(abs(elo_d)/20))
margin_adj = function(mar, elo_d) ifelse(mar>(ifelse(elo_d>0,1.6171,-1.6171) * (abs(elo_d)/20)^.8637),1,-1) * log(abs(mar-ifelse(elo_d>0,1.6171,-1.6171) * (abs(elo_d)/20)^.8637)+1)

(1.6171 * (-40)^.8637)

margin_adj(30,20*40)
margin_adj(30,20*40)


margin_adj(-10,20*30)

((1.6171 * (40)^.8637)/(40))

####elo to point spread conversion
pred_all <- elo_def(return_me='log')[[1]]
elo_val <- lm(Score_diff ~ elo_diff-1, data=pred_all,subset= Season>=2007 & pred_all$Tm_OH==1 & pred_all$nonOHSAA==0)
summary(elo_val)
1/elo_val$coeff[1]

head(pred_all)
####elo to point spread conversion
pred_all <- elo_def(return_me='log')[[1]]
pred_all$elo_ch <- pred_all$Tm_elo.curr-pred_all$Tm_elo.new
pred_all$elo_diffsq <- pred_all$elo_diff ^2
elo_val <- lm(elo_ch ~ elo_diff+elo_diffsq-1, data=pred_all,subset= Season>=2007)
summary(elo_val)
1/elo_val$coeff[1]




auto_corr_adj = function(elo_d) ifelse(abs(elo_d)<=20000000,ifelse(elo_d>0,1,-1) * ((elo_d^2)/320000) - elo_d/500,ifelse(elo_d>0,.4,-.4))
auto_corr_adj = function(elo_d) ifelse(elo_d>0,1,-1) * ((elo_d^2)/420000) - elo_d/710
plot(auto_corr_adj(seq(0,800,20))*-30)

auto_corr_adj(20*50)

elo_def(auto_corr_adj= function(x) 0, margin_adj = function(mar, elo_d) ifelse(mar>(elo_d/21),1,-1) * (pnorm(abs((mar-(elo_d/21))/(elo_d/21)/qnorm((1/(1+10^(-elo_d/400))))))-.5), K_mar=200)
cv_testing(elo_def, 'K_mar', seq(120,180,20),auto_corr_adj= function(x) 0, margin_adj = function(mar, elo_d) ifelse(mar>(elo_d/21),1,-1) * (pnorm(abs((mar-(elo_d/21))/(elo_d/21)/qnorm((1/(1+10^(-elo_d/400))))))-.5))

cv_testing(elo_def, 'K_mar', seq(120,200,20),margin_adj = function(mar, elo_d) ifelse(mar>(elo_d/17),1,-1) * log((abs(mar-(elo_d/17))+1)/2.5)/(18+.008*elo_d))
cv_testing(elo_def, 'K_mar', seq(10,40,10),margin_adj = function(mar, elo_d) ifelse(mar>(elo_d/17),1,-1) * log((abs(mar-(elo_d/17))+1)/1))
cv_testing(elo_def, 'K_mar', seq(20,40,5))

ratings <- elo_def(auto_corr_adj= function(x) 0, margin_adj = function(mar, elo_d) ifelse(mar>(elo_d/21),1,-1) * log((abs(mar-(elo_d/21))+1)/2),K_mar=40,return_me='log')[[1]]
head(ratings)
elo_def(auto_corr_adj= function(x) 0, margin_adj = function(mar, elo_d) ifelse(mar>(elo_d/20.5),1,-1) * log((abs(mar-(elo_d/20.5))+1)/2.5),K_mar=40)

margin_adj = function(mar, elo_d) ifelse(mar>(elo_d/21),1,-1) * log(abs(mar-(elo_d/21))+1/2))
margin_adj(40,100)

my_funs <- lapply(13:19, function(g) {
function(mar, elo_d) ifelse(mar>(elo_d/g),1,-1) * (pnorm(abs((mar-(elo_d/g))/(elo_d/g)/qnorm((1/(1+10^(-elo_d/400))))))-.5)
})
cv_testing(elo_def, 'margin_adj', my_funs ,auto_corr_adj= function(x) 0, K_mar=200)

plot(pred_all$elo_diff[which(pred_all$Season>=2007 & !is.na(pred_all$Score_diff) & !is.na(pred_all$elo_diff))], resid(elo_val), 
ylab="Residuals", xlab="Elo Diff", 
main="Elo Diff Residual Plot") 

(100/25)/qnorm((1/(1+10^(-100/400))))


rev(sort(table(nonOH_div$ST)))

###best ST_adj... don't know how I feel about it
logger <- lapply(seq(.9,1.1,.05),function(x) c('WV'=.9,'KY'=1.1,'PA'=1.1,'IN'=1.1,'MI'=1.1,'ON'=1.1,'OH'=.9))

cv_testing(elo_def, 'home_adv_var', seq(.1,.3,.05))
cv_testing(elo_def, 'home_adv_flat', seq(10,30,5))
cv_testing(elo_def, 'K_mar', seq(20,40,5))
cv_testing(elo_def, 'hist_fac', seq(.15,.35,.05))
cv_testing(elo_def, 'reg_fac', seq(.2,.35,.05))
cv_testing(elo_def, 'home_adv_nonOH', seq(10,30,5))
cv_testing(elo_def, 'home_adv_nonOH_both', seq(25,50,5))
cv_testing(elo_def, 'non_OH_gm', seq(.4,.6,.05))
cv_testing(elo_def, 'ST_adj', logger)

elo_def(ST_adj=c('WV'=.85,'PA'=1.35,'MI'=1.2,'KY'=1.05,'IN'=1.15,'OH'=.7))
elo_def(ST_adj=c('WV'=.85,'PA'=1.35,'MI'=1.2,'KY'=1.05,'IN'=1.15))


rev(sort(table(nonOH_div$ST)))

elo_def(return_me='log')[[3]]


elo_def(look_back = re_base(c(4,3,2,1)))
elo_def(look_back = re_base(c(1.1,1,.9,.8)))


testing_LB <- lapply(seq(.5,1.5,.1),function(z) re_base(slope_lin(x=.05,y=z,p=1)[1:4]))
testing_LB <- lapply(2:6,function(z) re_base(c(1.5,rep(1,z-1))))
testing_LB <- lapply(1:8,function(z) re_base(rep(1,z)))

cv_testing(elo_def, 'look_back', testing_LB)
non_OH <- elo_def(return_me='log',ST_adj = c('ON'=.9))[[3]]
non_OH[which(non_OH$ST=='ON'),]

review <- elo_def(margin_adj=function(mar, elo_d) pnorm(mar/25), K_mult=F, K=130,return_me='log')[[1]]
head(review)


######
##quick slope for each week
slope_lin <- function(x=0.08,y=1.25,p=.85) ifelse(c(seq(y,y-x*9,-x),rep(p,5))<0,0,c(seq(y,y-x*9,-x),rep(p,5)))

diff_slope <- sapply(seq(.7,.9,.05),function(j) c(j,elo_def(wk_slope=slope_lin(p=j))))
diff_slope <- sapply(seq(1.2,1.4,.05),function(j) c(j,elo_def(wk_slope=slope_lin(y=j))))
diff_slope <- sapply(seq(.07,.1,.005),function(j) c(j,elo_def(wk_slope=slope_lin(x=j))))

diff_slope[1,order(diff_slope[5,])[1]]

elo_def()
data.frame(adj_K=slope_lin()*275)
########


elo_def(pl_adj=F,mod_player_adj=0)
#######
#all-district adjustment
library(reshape)

all_dist <- read.csv('data sets/All District.csv',stringsAsFactors=F)

all_dist$Graduating <- ifelse(all_dist$Year=='sr','Done','Back')
all_dist$Pos[which(all_dist$Pos=='AP-D')] <- 'WRDB'
all_dist$Pos[which(all_dist$Pos=='AP')] <- 'WRDB'
all_dist$Pos[which(all_dist$Pos=='PN')] <- 'KI'
all_dist$Pos[which(all_dist$Pos=='DL')] <- 'LN'
all_dist$Pos[which(all_dist$Pos=='LB')] <- 'BK'
all_dist$Pos[which(all_dist$Pos=='DB')] <- 'WRDB'
all_dist$Pos[which(all_dist$Pos=='RB')] <- 'BK'
all_dist$Pos[which(all_dist$Pos=='WR')] <- 'WRDB'
all_dist$Pos[which(all_dist$Pos=='OL')] <- 'LN'
all_dist$Pos[which(all_dist$Pos=='QB')] <- 'BK'

all_dist <- all_dist[which(all_dist$Pos!='KI'),]
all_dist$my_val <- 1

all_dist_cast <- cast(all_dist, TeamID+Season~Graduating+Pos, length, value='my_val')
pos_names <- names(all_dist_cast)[-c(1:2)]
all_dist_cast$Yr_ID <- paste0(all_dist_cast$Season+1,'_',all_dist_cast$TeamID)

ratings <- elo_def(return_me='log',pl_adj=F,mod_player_adj=0)[[2]]
season17 <- ratings[,c('Div','2017 Week 0','2017 Week 15')]
season17$Yr_ID <- paste0('2017_',row.names(ratings))
names(season17)[2:3] <- c('Begin','End')

season16 <- ratings[,c('Div','2016 Week 0','2016 Week 15')]
season16$Yr_ID <- paste0('2016_',row.names(ratings))
names(season16)[2:3] <- c('Begin','End')

season15 <- ratings[,c('Div','2015 Week 0','2015 Week 15')]
season15$Yr_ID <- paste0('2015_',row.names(ratings))
names(season15)[2:3] <- c('Begin','End')

season18 <- ratings[,c('Div','2017 Week 15','2017 Week 15')]
season18$Yr_ID <- paste0('2018_',row.names(ratings))
names(season18)[2:3] <- c('Begin','End')

season <- rbind(season15,season16,season17,season18)
season$Change <- season[,3] - season[,2]
row.names(season) <- season$Yr_ID

season <- cbind(season,all_dist_cast[match(season$Yr_ID,all_dist_cast$Yr_ID),])
season[,pos_names] <- ifelse(is.na(season[,pos_names]),0,as.matrix(season[,pos_names]))

div_pos_names <- paste0('Div_',pos_names)
season[,c(paste0(div_pos_names))] <- season[,pos_names] * season$Div
season$Season <- sapply(strsplit(row.names(season),'_'),function(x) x[1])
gap <- aggregate(Change~Season,season,mean)
season$Change <- season$Change - gap$Change[match(season$Season,gap$Season)]

season18_df <- season[which(season$Season==2018),]
season <- season[which(season$Change!=0),]

player_adj <- lm(Change ~ ., data=season[,c('Change',pos_names,div_pos_names)])
summary(player_adj)
player_adj_pred <- predict(player_adj,season)
player_adj_pred[is.na(player_adj_pred)] <- 0


#elo_def()
#elo_def(pl_adj=F)
summary(player_adj_pred)/spread_adj

pos_only <- matrix(player_adj$coeff[paste0(pos_names)],7,length(pos_names),byrow=T)
pos_div <- matrix(player_adj$coeff[paste0(div_pos_names)],7,length(pos_names),byrow=T) * 1:7
full_coeff_mx <- pos_only + pos_div
colnames(full_coeff_mx) <- pos_names
full_coeff_mx*100


###auto-correl function testing
#auto_corr_adj = function(elo_d) -elo_d^3/8000000+elo_d^2/ifelse(elo_d>0,5000000,-5000000)+elo_d/14

no_adj = function(elo_d) 0
my_auto_fun = function(elo_d) easy_auto_fun(list(elo_d=elo_d,
		t5 = 1, t4 = 1, t3 = -1.55, t2 = 1.25, t1 = 2.44,
		t5p = 5000, t4p = 5000, t3p = 7, t2p = 7, t1p = 1
			))

my_auto_fun = function(elo_d) easy_auto_fun(list(elo_d=elo_d,
		t5 = 3.93, t4 = 2.94, t3 = -3.67, t2 = -6.38, t1 = 1.21,
		t5p = 12, t4p = 11, t3p = 6, t2p = 5, t1p = 1
			))
my_auto_fun = function(elo_d) easy_auto_fun(list(elo_d=elo_d,
		t5 = 3.93, t4 = 2.94, t3 = -3.67, t2 = -6.38, t1 = 1.21,
		t5p = 12, t4p = 11, t3p = 6, t2p = 5, t1p = 1
			))
my_auto_fun = function(elo_d) easy_auto_fun2(list(elo_d=elo_d,
	t3 = 1.75, t2 = 3.24, t1 = -2.86,
	t3p = 7, t2p = 6, t1p = 1
			))

my_auto_fun(10)
#elo_def(auto_corr_adj=my_auto_fun)
#elo_def(auto_corr_adj= function(x) 0)
elo_def(margin_adj = function(mar, elo_d) ifelse(mar>(elo_d/20.7),1,-1) * (log_odds(abs((mar-(elo_d/20.7))/15))-.5), auto_corr_adj = my_auto_fun, K_mar=300)
elo_def(margin_adj = function(mar, elo_d) ifelse(mar>(elo_d/20.7),1,-1) * (log_odds(abs((mar-(elo_d/20.7))/15))-.5), auto_corr_adj = my_auto_fun, K_mar=300)



###save prediction csv
pred_all <- elo_def(return_me='log')[[1]]
pred_all$use <- 0
pred_all$use[which((pred_all$Result=='W' | pred_all$Result=='W*') & !is.na(pred_all$Opp_Div) & pred_all$Notes!='forfeit')] <- 1
most_recent_season <- aggregate(Yr~TeamID,data=teams_df,FUN=max)
unique_teams <- teams_df[which(teams_df$YrID %in% paste0(most_recent_season$Yr,'_',most_recent_season$TeamID)),c('TeamID','City','School')]
unique_teams$full_name <- sapply(1:nrow(unique_teams),function(x) paste0(unique_teams$City[x],' ',sub(paste0(unique_teams$City[x]),'',unique_teams$School[x])))
unique_teams$full_name <- gsub(' -','-',gsub('  ',' ',trimws(unique_teams$full_name)))
pred_all$Team <- unique_teams$full_name[match(pred_all$Tm_ID,unique_teams$TeamID)]
pred_all$Opp <- unique_teams$full_name[match(pred_all$Opp_ID,unique_teams$TeamID)]
pred_all$Div <- teams_df$Div[match(paste0(pred_all$Season,'_',pred_all$Tm_ID),teams_df$YrID)]
pred_all$Reg <- teams_df$Reg[match(paste0(pred_all$Season,'_',pred_all$Tm_ID),teams_df$YrID)]
pred_all$AJ_Pick <- ifelse(pred_all$elo_win > .5, 1, 0)
pred_all$AJ_Correct <- ifelse(pred_all$Win==pred_all$AJ_Pick,1,0)
pred_all$LogLoss <- -log(ifelse(pred_all$Win==1,pred_all$elo_win,1-pred_all$elo_win))
pred_all$GameID <- paste0(pred_all$Season,'_',pred_all$Week,'_',pred_all$Tm_ID)

#write.csv(pred_all,'output/all_pred.csv',row.names=F)
#####


####attempt to fit the auto-correlation differences grouped by 20's
elo_change <- pred_all$Tm_elo.new-pred_all$Tm_elo.curr
elo_group <- cut(pred_all$elo_diff,seq(-1000,1000,20))
actual_miss <- aggregate(elo_change~elo_group,FUN=mean)
AC_used <- aggregate(pred_all$adj_amount~elo_group,FUN=mean)
mean(abs(actual_miss[,2]))
plot(actual_miss,ylim=c(-30,30))
lines(rep(0,100),col='gold')

main <- 2.4
pwr <- .81
margin_adj = function(mar, elo_d) ifelse(mar>(ifelse(elo_d>0,main,-main) * (abs(elo_d)/23)^pwr),1,-1) * abs(mar-ifelse(elo_d>0,main,-main) * (abs(elo_d)/23)^pwr)/7
pred_all$new_ch <- margin_adj(pred_all$Score_diff,pred_all$elo_diff) * 30 * slope_lin(x=0.08,y=1.25,p=.85)[pred_all$Week]
new_res <- aggregate(pred_all$new_ch~elo_group,FUN=mean)
lines(new_res[,2],col='blue')
mean(abs(new_res[,2]))


####


###calib
#pred_all <- elo_def(return_me='log')[[1]]

pred_all$pred <- round(pred_all$elo_win, 2)
pred_all$pred[pred_all$pred==1] <- .99
pred_all$pred[pred_all$pred==0] <- .01
pred_all$pred[which(pred_all$Excl_Elo==1)] <- NA
my_calib <- aggregate(Win~pred,pred_all,mean,subset= Season>=2007)
#my_calib <- aggregate(Win~pred,pred_all,mean,subset= Season==2017)

plot(my_calib,type='l')
lines(my_calib[,1],my_calib[,1],col='red')



pred_all$spread_off <- pred_all$Score_diff - pred_all$elo_diff/23
spread_miss <- aggregate(pred_all$Score_diff~elo_group,FUN=mean)
plot(1:50,spread_miss[51:100,2])
lines(1:50,col='red')
lines(main*(1:50)^(pwr),col='blue')

model <- glm(log(spread_miss[51:100,2])~log(1:50-.5))
lines(exp(model$coeff[1]) * (1:50-.5) ^ model$coeff[2],col='green')
summary(model)
main <- exp(model$coeff[1])
pwr <- model$coeff[2]




head(pred_all)


log_pred <- ifelse(pred_all$elo_diff>0,log(pred_all$elo_diff/23),-log(-pred_all$elo_diff/23))
log_score <- ifelse(pred_all$Score_diff>0,log(pred_all$Score_diff),-log(-pred_all$Score_diff))

missing <- which(is.na(log_score) | is.na(log_pred) | log_score==Inf | log_pred==Inf)
log_score <- log_score[-c(missing)]
log_pred <- log_pred[-c(missing)]

model2 <- glm(log_score~log_pred)
summary(model2)
main <- exp(model2$coeff[1])
pwr <- model2$coeff[2]


(1:50)/20/(main*(1:50)^(pwr))
#write.csv(spread_miss,'misc/spread_adj.csv')
term1 <- seq(-990,990,20)
1/glm(actual_miss[,2]~term1-1)$coeff[1]



#elo_def(margin_adj = function(mar, elo_d) ifelse(mar>(elo_d/19.4),1,-1) * log((abs(mar-(elo_d/19.4))+1)/1) - (elo_d/365 - ifelse(elo_d>0,1,-1) * (elo_d^2)/250000))

margin_adj = function(mar, elo_d) ifelse(mar>(elo_d/19.4),1,-1) * log((abs(mar-(elo_d/19.4))+1)/1) - (elo_d/365 - (elo_d*elo_d)/250000)
margin_adj(15:25,400)
400/17

term5 <- seq(-790,790,20)^5
term4 <- seq(-790,790,20)^4
term3 <- seq(-790,790,20)^3
term2 <- seq(-790,790,20)^2
term1 <- seq(-790,790,20)

#fit_autocorr <- lm((actual_miss[,2]-AC_used[,2])~term3+term2+term1-1)
#summary(fit_autocorr)
#pred <- predict(fit_autocorr,data.frame(term5,term4,term3,term2,term1))
#pred <- predict(fit_autocorr,data.frame(term3,term2,term1))

lines(pred+AC_used[,2],col='red')
lines(actual_miss[,2]-AC_used[,2]-pred,col='blue')
lines(rep(0,80),col='gold')
lines(AC_used,col='green')

mean(abs(actual_miss[,2]))
mean(abs(actual_miss[,2]-AC_used[,2]-pred))

options(scipen=0)
-1/fit_autocorr$coeff
options(scipen=999)


######make some box plots for scores
800/17
spr_adj <- 18
elo_group <- cut(pred_all$elo_diff,c(-Inf,seq(-800,800,spr_adj),Inf))
pred_all$spread <- pred_all$elo_diff/spr_adj
pred_all$spread_miss <- pred_all$Score_diff - pred_all$spread
#barplot(table(elo_group))


start_grp <- length(levels(elo_group))/2+1
end_grp <- length(levels(elo_group))

bx_p <- lapply(levels(elo_group)[start_grp:end_grp], function(x) pred_all$spread_miss[which(elo_group==x & pred_all$Season>=2007)])
names(bx_p) <- levels(elo_group)[start_grp:end_grp]
par(las=2)
par(mar=c(3,5,1,1))
boxplot(bx_p, horizontal=T)
lines(rep(0,start_grp),1:start_grp,col='red')
#lines(rep(16,42),1:42,col='blue')
#lines(rep(-16,42),1:42,col='blue')
est_center <- c(-Inf,seq(-800,800,spr_adj),Inf)[start_grp:end_grp] + spr_adj/2
lines((est_center/spr_adj)/qnorm(1/(1+10^(-est_center/400))) * -.84,1:(start_grp-1),col='blue')
lines((est_center/spr_adj)/qnorm(1/(1+10^(-est_center/400))) * .84,1:(start_grp-1),col='blue')

adj_amount <- sapply(levels(elo_group)[start_grp:end_grp], function(x) mean(pred_all$adj_amount[which(elo_group==x & pred_all$Season>=2007)],na.rm=T))
mean_diff <- sapply(bx_p,mean,na.rm=T)
miss_by <- lapply(bx_p,function(x) ifelse((x+1)>0,1,-1)*log(abs(x)+1))
factor_diff <- sapply(miss_by,function(x) mean(mean(x,na.rm=T)/(x-mean(x,na.rm=T)),na.rm=T))
factor_diff <- sapply(miss_by,function(x) mean(x,na.rm=T)) - adj_amount
plot(factor_diff)


mean_diff2 <- mean_diff[-c(length(mean_diff))]
est_center2 <- est_center[-c(length(est_center))]
factor_diff2 <- factor_diff[-c(length(factor_diff))]
mult_fac <- factor_diff2/est_center2+1
est_center2_sq <- est_center2^2
 

mod_1 <- glm(mult_fac~est_center2)
mod_2 <- glm(factor_diff2~est_center2+est_center2_sq-1)

1/coef(mod_2)[1]
1/coef(mod_2)[2]
summary(mod_2)




plot(est_extra*30)

plot(1.0034 - est_center2/193000)







numb <- 16
elo_dif <- c(seq(10,800,20),Inf)[numb]
elo_dif/19.3
std <- (elo_dif/19.3)/qsn(1/(1+10^(-elo_dif/400)),alpha=1)
barplot(table(cut(bx_p[[numb]],c(-Inf,seq(-60,60,5),Inf))))
col_names <- names(table(cut(bx_p[[numb]],c(-Inf,seq(-60,60,5),Inf))))
lines(c(0,0,0,diff(psn(c(seq(-60,60,5))/std,alpha=1)*length(bx_p[[numb]]))))

options(scipen=999)

library(emg)
bx_p2 <- lapply(levels(elo_group)[42:82], function(x) pred_all$Score_diff[which(elo_group==x & pred_all$Season>=2007)])
est_emg_par <- sapply(bx_p2[1:40], function(x) coef(emg.mle(x[which(!is.na(x))])))
plot(est_emg_par[3,],type='l')
est_emg_par <- rbind(est_emg_par,(est_center/15.2)[1:40])

spread_interv <- apply(est_emg_par, 2, function(x) qemg(c(.01,.5,.99),x[1],x[2],x[3]))
plot(spread_interv[2,],type='l',col='red',ylim=c(-40,70))
lines(spread_interv[1,],type='l',col='blue')
lines(spread_interv[3,],type='l',col='blue')


win_prob_check <- apply(est_emg_par, 2, function(x) qemg(c(.05,.5,.95),x[1],x[2],x[3]))
win_prob <- apply(est_emg_par, 2, function(x) pemg(10,x[1],x[2],x[3]))
plot(1/(1+10^(-est_center/400)),type='l')
lines(1-win_prob,col='red')


elo_sq <- est_center[1:40]^2
elo_exp <- exp(est_emg_par[4,])
log_sig <- exp(est_emg_par[3,])
elo_conv <- 

mu_trend <- lm(est_emg_par[1,]~est_center[1:40]-1)
sig_trend <- lm(est_emg_par[2,]~est_center[1:40])
lam_trend <- lm(est_emg_par[3,]~est_center[1:40])
summary(mu_trend)
summary(sig_trend)
summary(lam_trend)

1/mu_trend$coeff[1]

margin_adj = function(mar, elo_d) pnorm(mar/((elo_d/19.3)/qnorm((1/(1+10^(-elo_d/400))))))
margin_adj = function(mar, elo_d) pemg(mar, 0, sig_trend$coeff[1] + elo_d * sig_trend$coeff[2], lam_trend$coeff[1] + lam_trend$coeff[2] * elo_d)

margin_adj(20,1/mu_trend$coeff[1]*20)
1/(1+10^(-1/mu_trend$coeff[1]*20/400))

1/mu_trend$coeff[1]

385*.188

####save current rankings
best_att <- elo_def(return_me='log')[[2]]
#write.csv(best_att[which(!is.na(best_att$Div)),c('City','School','Div','Reg','Begin','Current')],'output/end 2017 rankings.csv')

best_att$FullName <- sapply(1:nrow(best_att),function(x) paste0(best_att$City[x],' ',sub(paste0(best_att$City[x]),'',best_att$School[x])))
best_att$FullName <- gsub(' -','-',gsub('  ',' ',trimws(best_att$FullName)))
best_att[c('232','248','424'),'FullName'] <- best_att[c('232','248','424'),'School']
#write.csv(best_att[,c('FullName','Div','Reg','Begin','Current')],'output/end 2017 rankings.csv')

all_weeks <- c(sapply(2007:2017, function(x) paste0(x, ' Week ',0:15)))
all_hist <- best_att[,c('FullName',all_weeks)]
all_hist[,all_weeks] <- (all_hist[,all_weeks]-1500)/19.3

write.csv(all_hist,'output/hist rankings.csv')


best_att_nonOH <- elo_def(return_me='log')[[3]]
best_att_nonOH$SchoolST <- paste0(best_att_nonOH$School,' (',best_att_nonOH$ST,')')


###


###home_adv review
pred_no_H_adv <- elo_def(return_me='log',home_adv = 0)[[1]]
pred_no_H_adv$home_adv[pred_no_H_adv$Loc=='H'] <- 1
pred_no_H_adv$home_adv[pred_no_H_adv$Loc=='A'] <- -1

flat <- lm(Score_diff~elo_diff+home_adv,data=pred_no_H_adv)
trav <- lm(Score_diff~elo_diff+trav_est,data=pred_no_H_adv)
comb <- lm(Score_diff~elo_diff+trav_est+home_adv,data=pred_no_H_adv)

summary(flat)
summary(trav)
summary(comb)

1/flat$coeff[2]
1/trav$coeff[2]
1/comb$coeff[2]

flat$coeff[3] * 1/flat$coeff[2]
1/trav$coeff[3]
(1/trav$coeff[2])/ (1/trav$coeff[3])

comb$coeff[4] * 1/comb$coeff[2]
(1/comb$coeff[2])/ (1/comb$coeff[3])
####

###review division rating
cbind(aggregate(Tm_elo.new~Div,pred_all,mean,subset= Week==10 & Season==2012),aggregate(Tm_elo.curr~Div,pred_all,mean,subset= Week==1 & Season==2002))


###review rating inflation
ratings <- elo_def(return_me='log')[[2]]
mean(ratings[which(!is.na(ratings$Div)),'2017 Week 15'])
ratings2 <- ratings[,-c(1:8)]
ratings2$hist_avg <- NULL
ratings2$Player_adj <- NULL
plot(apply(ratings2,2,mean,na.rm=T),type='l')

mean(ratings[which(!is.na(ratings$Div)),'2017 Week 1'] - ratings[which(!is.na(ratings$Div)),'2017 Week 0'])



sapply(1:7, function(x) mean(ratings[which(ratings[,'2017 Week 15'] - ratings[,'2017 Week 0']!=0 & ratings$Div==x),'2017 Week 0']))
table(ratings$Div[which(ratings[,'2017 Week 15'] - ratings[,'2017 Week 0']!=0)])

aggregate(Current~ST,ratings,mean)
table(ratings$ST)
head(ratings)




###make a rank df
#read in HyTek & Best Att before
best_att <- elo_def(return_me='log')[[2]]
spread_adj <- 18

ranks_df <- lapply(2017:2002, function(yr) {
best_att$Div <- teams_df$Div[match(paste0(yr,'_',row.names(best_att)),teams_df$YrID)]

SZN_SWAER <- best_att[which(!is.na(best_att$Div)),paste0(yr,' Week ',0:15)]
SZN_avg <- apply(SZN_SWAER,2,mean)
SZN_SWAER <- (SZN_SWAER-SZN_avg)/spread_adj
SZN_Rank <- apply(-SZN_SWAER,2,rank,ties.method='first')
tms_div <- best_att$Div[which(!is.na(best_att$Div))]
div_avg <- aggregate(.~tms_div,SZN_SWAER,mean)[,-c(1)]
SZN_iSWAER <- SZN_SWAER - div_avg[tms_div,]

SZN_DivRank <- lapply(1:7, function(x) apply(-best_att[which(best_att$Div==x),paste0(yr,' Week ',0:15)],2,rank,ties.method='first'))
SZN_DivRank <- do.call(rbind,SZN_DivRank)

teams_list <- row.names(best_att)[which(!is.na(best_att$Div))]
ranks_tm <- lapply(teams_list, function(tm) data.frame(Tm_ID=tm,Season=substr(names(SZN_SWAER),1,4),Week=as.numeric(substr(names(SZN_SWAER),11,12)),Div=best_att[paste0(tm),'Div'],Reg=best_att[paste0(tm),'Reg'],DivRank=SZN_DivRank[paste0(tm),],OvrRank=SZN_Rank[paste0(tm),],iSWAER=matrix(SZN_iSWAER[paste0(tm),]),SWAER=matrix(SZN_SWAER[paste0(tm),]),row.names=NULL))
ranks_yr <- do.call(rbind,ranks_tm)

return(ranks_yr)
})

ranks_all <- do.call(rbind,ranks_df)
ranks_all$iSWAER <- unlist(ranks_all$iSWAER)
ranks_all$SWAER <- unlist(ranks_all$SWAER)

ranks_all$Reg <- teams_df$Reg[match(paste0(ranks_all$Season,'_',ranks_all$Tm_ID),teams_df$YrID)]

write.csv(ranks_all,'C:/Users/Owner/Desktop/SWAER/output/hist rankings.csv',row.names=F)

tm_avg <- aggregate(iSWAER~Tm_ID,ranks_all,mean)
tm_avg <- tm_avg[order(-tm_avg[,2]),]
###end rank df

###start record df
pred_all <- elo_def(return_me='log')[[1]]
pred_all$Rec_Result <- substr(pred_all$Result,1,1)

pred_all$W <- ifelse(pred_all$Rec_Result=='W',1,0)
pred_all$L <- ifelse(pred_all$Rec_Result=='L',1,0)
pred_all$T <- ifelse(pred_all$Rec_Result=='T',1,0)

pred_all$C_W <- ifelse(pred_all$Rec_Result=='W' & !is.na(pred_all$Conf_Matchup) & pred_all$Week <= 10,1,0)
pred_all$C_L <- ifelse(pred_all$Rec_Result=='L' & !is.na(pred_all$Conf_Matchup) & pred_all$Week <= 10,1,0)
pred_all$C_T <- ifelse(pred_all$Rec_Result=='T' & !is.na(pred_all$Conf_Matchup) & pred_all$Week <= 10,1,0)

rec_list <- lapply(1:15, function(wk) cbind(Week=wk,aggregate(cbind(W,L,T,C_W,C_L,C_T)~Tm_ID+Season,pred_all,sum,subset= Week<=wk)))
rec_df <- do.call(rbind,rec_list)

rec_df$Conf <- pred_all$Tm_Conf[match(paste0(rec_df$Season,'_',rec_df$Tm_ID),paste0(pred_all$Season,'_',pred_all$Tm_ID))]
rec_df[which(rec_df$Conf=='IND' || rec_df$Conf=='OH VAL'),c('C_W','C_L','C_T')] <- 0

write.csv(rec_df,'C:/Users/Owner/Desktop/SWAER/output/rec rankings.csv',row.names=F)
head(rec_df)

###end record df

#read in HyTek & Best Att before
HyTek <- read.csv('output/OHSAA ALL.csv',stringsAsFactors=F)
best_att <- elo_def(return_me='log')[[2]]
pred_all <- elo_def(return_me='log')[[1]]
pred_all$Tm <- HyTek$OHSAA.Tourn[match(pred_all$Tm_ID,HyTek$OHSAA.ID)]
pred_all$Opp <- HyTek$OHSAA.Tourn[match(pred_all$Opp_ID,HyTek$OHSAA.ID)]

p <- function(..., sep='') {
    paste(..., sep=sep, collapse=sep)
}

tm_hist <- lapply(2017:2007, function(yr) {
pred_all$Opp_ID[which(is.na(pred_all$Opp_Div) & pred_all$nonOHSAA==0)] <- '0000'

pred_all$non_OH_Opp <- paste0(nonOH_div$School[match(paste0(yr,'_',pred_all$Opp_ID),nonOH_div$X)],' (',nonOH_div$ST[match(paste0(yr,'_',pred_all$Opp_ID),nonOH_div$X)],')')
pred_all$Opp <- ifelse(is.na(pred_all$Opp),pred_all$non_OH_Opp,pred_all$Opp)
pred_all$Opp_ID[which(pred_all$nonOHSAA==1)] <- '0000'
best_att$Div <- teams_df$Div[match(paste0(yr,'_',row.names(best_att)),teams_df$YrID)]

pred_all <- pred_all[which(pred_all$Season==yr & pred_all$Tm_OH==1),]

SZN_SWAER <- best_att[which(!is.na(best_att$Div)),paste0(yr,' Week ',0:14)]
SZN_SWAER <- (SZN_SWAER-1500)/19.4
SZN_Rank <- apply(-SZN_SWAER,2,rank,ties.method='first')
SZN_Rank <- rbind(SZN_Rank,'0000'=rep('',15))

tms_div <- best_att$Div[which(!is.na(best_att$Div))]
div_avg <- aggregate(.~tms_div,SZN_SWAER,mean)[,-c(1)]
SZN_iSWAER <- SZN_SWAER - div_avg[tms_div,]

SZN_DivRank <- lapply(1:7, function(x) apply(-best_att[which(best_att$Div==x),paste0(yr,' Week ',0:14)],2,rank,ties.method='first'))
SZN_DivRank <- do.call(rbind,SZN_DivRank)
SZN_DivRank <- rbind(SZN_DivRank,'0000'=rep(NA,15))

pred_all$Tm_iSWAER <- sapply(1:nrow(pred_all), function(x) SZN_iSWAER[paste0(pred_all$Tm_ID[x]),pred_all$Week[x]])
pred_all$Opp_iSWAER <- sapply(1:nrow(pred_all), function(x) SZN_iSWAER[paste0(pred_all$Opp_ID[x]),pred_all$Week[x]])
pred_all$SWAER <- sapply(1:nrow(pred_all), function(x) SZN_SWAER[paste0(pred_all$Tm_ID[x]),pred_all$Week[x]])
pred_all$Ovr_Rank <- sapply(1:nrow(pred_all), function(x) SZN_Rank[paste0(pred_all$Tm_ID[x]),pred_all$Week[x]])
pred_all$Div_Rank <- sapply(1:nrow(pred_all), function(x) SZN_DivRank[paste0(pred_all$Tm_ID[x]),pred_all$Week[x]])
pred_all$Opp_Rank <- sapply(1:nrow(pred_all), function(x) SZN_Rank[paste0(pred_all$Opp_ID[x]),pred_all$Week[x]])
pred_all$Opp_Div_Rank <- sapply(1:nrow(pred_all), function(x) SZN_DivRank[paste0(pred_all$Opp_ID[x]),pred_all$Week[x]])

pred_all$tm_info <- paste0('#',pred_all$Ovr_Rank,' / #',pred_all$Div_Rank,' / ',round(pred_all$SWAER,1))

pred_all$Q_score <- paste0(ifelse(pred_all$Win==1,'W ','L '),pred_all$Tm_score,'-',pred_all$Opp_score)
pred_all$Q_date <- paste0(as.numeric(substr(pred_all$Date,6,7)),'/',as.numeric(substr(pred_all$Date,9,10)))
pred_all$Q_pred <- sprintf('%.1f',round(pred_all$elo_win,3)*100)
pred_all$spread <- round(pred_all$elo_diff/19.4,1)

data.frame(pred_all)
})

##used to be in loop
#pred_all$link <- paste0('#',pred_all$Opp_Rank,' <a>', best_att$FullName[match(pred_all$Opp_ID,row.names(best_att))],'</a>')
#pred_all$link[which(pred_all$Opp_ID=='0000')] <- pred_all$Opp[which(pred_all$Opp_ID=='0000')]
#pred_all$Q_pred <- paste0('<a style="color:',ifelse(pred_all$AJ_Correct==1,'green">','red">'),,'%</a>')


#href=https://sites.google.com/site/swaermodel/teams/', HyTek$HyTek[match(pred_all$Opp_ID,HyTek$OHSAA.ID)]
#'<img height="20" src="',HyTek$Left[match(pred_all$Opp_ID,HyTek$OHSAA.ID)],'"> 

basic <- pred_all[,c('Week','Q_date','tm_info','link','Q_score','spread','Q_pred','Tm_ID')]

basic$rows <- paste0('<tr><td>',apply(basic,1,function(x) p(x[1:7],sep='</td><td>')),'</td></tr>')
tables <- sapply(row.names(best_att),function(x) p(basic$rows[which(basic$Tm_ID==x)]))

col_width <- paste0("<col width='",p(c(30,40,120,200,80,60),"'><col width='"),"60'>")
headers <- paste0("<tr><td>",p(c('Week','Date','Ovr/Div/SWAER','Opponent','Result','Spread'),'</td><td>'),"Win%</td></tr>")
records <- sapply(row.names(best_att),function(x) p(table(factor(pred_all$Win[which(pred_all$Tm_ID==x)],c(1,0))),sep='-'))
other_info <- paste0('<h2>',yr,' (',records,')</h2>')
tables <- paste0(other_info,'<table id="',yr,'szn" bodercolor="grey" border="4" cellspacing="0" cellpadding="4"><colgroup>',col_width,'</colgroup><thead>',headers,'</thead><tbody>',tables,'</tbody></table>')
#tables
##
both_helmets <- paste0(paste0('<img height=150 src="',helmets[1,],'">'),paste0('<img height=150 src="',helmets[2,],'">'))
full_pg <- cbind(both_helmets,tm_hist)


#write.csv(tables,'output/SZN Table16.csv')
#write.csv(full_pg,'output/hist Tables.csv')

#######
#move out of above loop because it takes too long
####Create csv file to save in google sheets
HyTek <- read.csv('data sets/OHSAA ALL.csv',stringsAsFactors=F)
pred_all <- elo_def(return_me='log')[[1]]

JE_games_all$Tm_elo.curr <- elo_df18_full[paste0(JE_games_all$Tm_ID),'Current']
JE_games_all$Opp_elo.curr <- elo_df18_full[paste0(JE_games_all$Opp_ID),'Current']
JE_games_all$elo_diff <- JE_games_all$Tm_elo.curr-JE_games_all$Opp_elo.curr+JE_games_all$home_adv
JE_games_all$elo_win <- 1/(1+10^(-JE_games_all$elo_diff/400))
JE_games_all[,c('Tm_elo.new','adj_amount')] <- NA
JE_games_all$opp_startElo <- NULL

pred_all <- rbind(pred_all,JE_games_all)
spred_adj <- 18

pred_all$Q_score <- ifelse(pred_all$Result=='','',paste0(ifelse(pred_all$Win==1,'W ','L '),pred_all$Tm_score,'-',pred_all$Opp_score))
pred_all$Q_date <- paste0(as.numeric(substr(pred_all$Date,6,7)),'/',as.numeric(substr(pred_all$Date,9,10)))
pred_all$Q_pred <- sprintf('%.1f',round(pred_all$elo_win,3)*100)
pred_all$spread <- round(pred_all$elo_diff/spred_adj,1)

pred_all$Tm <- HyTek$Best.Name[match(pred_all$Tm_ID,HyTek$OHSAA.ID)]
pred_all$Opp <- HyTek$Best.Name[match(pred_all$Opp_ID,HyTek$OHSAA.ID)]

pred_all$non_OH_Opp <- paste0(nonOH_div$Best.Name[match(paste0(pred_all$Season,'_',pred_all$Opp_ID),nonOH_div$X)],' (',nonOH_div$ST[match(paste0(pred_all$Season,'_',pred_all$Opp_ID),nonOH_div$X)],')')
pred_all$Opp <- ifelse(is.na(pred_all$Opp),pred_all$non_OH_Opp,pred_all$Opp)

big_pred_all <- pred_all
big_pred_all$Q_pred <- paste0(sprintf('%.1f',round(big_pred_all$elo_win,3)*100),'%')
big_pred_all$Opp_Code <- HyTek$HyTek[match(big_pred_all$Opp_ID,HyTek$OHSAA.ID)]
big_pred_all$Tm_Code <- HyTek$HyTek[match(big_pred_all$Tm_ID,HyTek$OHSAA.ID)]
big_pred_all$Opp_Helm <- HyTek$Helm.Left[match(big_pred_all$Opp_ID,HyTek$OHSAA.ID)]
nonOH_helm <- nonOH_div$Helm[match(paste0(big_pred_all$Season,'_',big_pred_all$Opp_ID),nonOH_div$X)]
big_pred_all$Opp_Helm <- ifelse(is.na(nonOH_helm),big_pred_all$Opp_Helm,nonOH_helm)

big_pred_all$Q_score[which(big_pred_all$Result=='T')] <- paste0('T ',substr(big_pred_all$Q_score[which(big_pred_all$Result=='T')],3,nchar(big_pred_all$Q_score[which(big_pred_all$Result=='T')])))

#some are byes (all should be)
#some are names that don't exist for OHSAA tournament anymore
#some are Olyer & Riverview East in 2010/2011
big_pred_all$Opp[which(big_pred_all$Opp=='NA (NA)')] <- HyTek$Best.Name[match(big_pred_all$Opp_ID[which(big_pred_all$Opp=='NA (NA)')],HyTek$OHSAA.ID)]
big_pred_all$Tm[which(big_pred_all$Tm==0)] <- HyTek$Best.Name[match(big_pred_all$Tm_ID[which(big_pred_all$Tm==0)],HyTek$OHSAA.ID)]
big_pred_all <- big_pred_all[which(big_pred_all$Q_pred!='NA%'),]

#find all bye weeks
all_tms <- paste0(big_pred_all$Season,'_',big_pred_all$Tm_ID)
find_byes <- lapply(unique(all_tms),function(x) cbind(x,seq(1,10)[-c(big_pred_all$Week[which(x==all_tms)])]))
have_byes <- lapply(which(sapply(find_byes,length)!=1), function(x) find_byes[[x]])
byes_df <- do.call(rbind,have_byes)

bye_wk_df <- data.frame(matrix(c(''),nrow(byes_df),ncol(big_pred_all)),stringsAsFactors=F)
names(bye_wk_df) <- names(big_pred_all)
bye_wk_df$Season <- sapply(strsplit(byes_df[,1],'_'),function(x) x[1])
bye_wk_df$Tm_ID <- sapply(strsplit(byes_df[,1],'_'),function(x) x[2])
bye_wk_df$Week <- byes_df[,2]
bye_wk_df$Opp <- 'BYE WEEK'
bye_wk_df$Tm_Code <- HyTek$HyTek[match(bye_wk_df$Tm_ID,HyTek$OHSAA.ID)]
for (j in 1:ncol(bye_wk_df)) bye_wk_df[,j] <- as(bye_wk_df[,j],sapply(big_pred_all,class)[j])
szn_wk <- paste0(big_pred_all$Season,'_',big_pred_all$Week)
common_date <- sapply(unique(szn_wk), function(x) names(rev(sort(table(big_pred_all$Q_date[which(szn_wk==x)]))))[1])
bye_wk_df$Q_date <- common_date[paste0(bye_wk_df$Season,'_',bye_wk_df$Week)]
bye_wk_df$Opp_Code <- NA
big_pred_all <- rbind(big_pred_all,bye_wk_df)
big_pred_all <- big_pred_all[order(big_pred_all$Week),]

big_pred_all$Q_score[which(big_pred_all$Q_score=='L NA-NA')] <- big_pred_all$Notes[which(big_pred_all$Q_score=='L NA-NA')]
big_pred_all$Q_score[which(big_pred_all$Q_score=='L 0-1')] <- 'L Forfeit'
big_pred_all$Q_score[which(big_pred_all$Q_score=='W 1-0')] <- 'W Forfeit'
big_pred_all$Q_Opp_Div <- ifelse(is.na(big_pred_all$Opp_Div),big_pred_all$nonOHSAA_div,big_pred_all$Opp_Div)

big_pred_all$Q_Loc <- ifelse(big_pred_all$Loc=='A','@','')
big_pred_all$Q_Loc[which(big_pred_all$Loc=='N')] <- '(N)'
big_pred_all$spread[is.na(big_pred_all$spread)] <- ''
pred_all <- big_pred_all

rankings_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/hist rankings.csv',stringsAsFactors=F)
seeding_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/seeding.csv',stringsAsFactors=F)
playoffs_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/playoffs.csv',stringsAsFactors=F)
conf_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/conf.csv',stringsAsFactors=F)
crystal_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/crystal.csv',stringsAsFactors=F)

rankings_df$Week <- rankings_df$Week + 1
crystal_df$Week <- crystal_df$Week + 1

playoffs_df$Final4 <- playoffs_df$X4+playoffs_df$X5+playoffs_df$X6
row.names(rankings_df) <- paste0(rankings_df$Season,'_',rankings_df$Week,'_',rankings_df$Tm_ID)
row.names(seeding_df) <- paste0(seeding_df$Season,'_',seeding_df$Week,'_',seeding_df$X)
row.names(conf_df) <- paste0(conf_df$Season,'_',conf_df$Week,'_',conf_df$X)
row.names(playoffs_df) <- paste0(playoffs_df$Season,'_',playoffs_df$Week,'_',playoffs_df$X)
row.names(crystal_df) <- paste0(crystal_df$Season,'_',crystal_df$Week,'_',crystal_df$TeamID)

pred_all[,c('Div_Rank','Ovr_Rank','Tm_iSWAER','SWAER')] <- rankings_df[paste0(pred_all$Season,'_',pred_all$Week,'_',pred_all$Tm_ID),c('DivRank','OvrRank','iSWAER','SWAER')]
pred_all[,c('Opp_Div_Rank','Opp_Rank')] <- rankings_df[paste0(pred_all$Season,'_',pred_all$Week,'_',pred_all$Opp_ID),c('DivRank','OvrRank')]
pred_all[,c('Home11')] <- seeding_df[paste0(pred_all$Season,'_',pred_all$Week,'_',pred_all$Tm_ID),c('home')]
pred_all[,c('Conf')] <- conf_df[paste0(pred_all$Season,'_',pred_all$Week,'_',pred_all$Tm_ID),c('X.1')]
pred_all[,c('Out','Final4','Champ')] <- playoffs_df[paste0(pred_all$Season,'_',pred_all$Week,'_',pred_all$Tm_ID),c('X1','Final4','X6')]
pred_all[,c('Curr_Seed','Proj_Seed')] <- crystal_df[paste0(pred_all$Season,'_',pred_all$Week,'_',pred_all$Tm_ID),c('curr_seed','proj_seed')]


pred_all$tm_info <- paste0('#',pred_all$Ovr_Rank,' / #',pred_all$Div_Rank,' / ',round(pred_all$SWAER,1))

pred_all$Q_Opp_Div <- paste0('D',pred_all$Q_Opp_Div)
pred_all$Q_Opp_Div[which(pred_all$Q_Opp_Div=='DNA')] <- 'CLUB'

pred_all$Conf_Matchup[which(pred_all$Conf_Matchup=='IND')] <- NA
pred_all$Conf_Matchup[which(pred_all$Conf_Matchup=='OH VAL')] <- NA

pred_all$Opp_Code <- ifelse(is.na(pred_all$Opp_Code),pred_all$Opp_ID,pred_all$Opp_Code)
big_pred_all <- pred_all

big_pred_all$In <- 1-big_pred_all$Out
big_pred_all$In <- paste0(sprintf('%.0f',round(big_pred_all$In,3)*100),'%')
big_pred_all$Out <- paste0(sprintf('%.0f',round(big_pred_all$Out,3)*100),'%')
big_pred_all$Home11 <- paste0(sprintf('%.0f',round(big_pred_all$Home11,3)*100),'%')
big_pred_all$Conf <- paste0(sprintf('%.0f',round(big_pred_all$Conf,3)*100),'%')
big_pred_all$Final4 <- paste0(sprintf('%.0f',round(big_pred_all$Final4,3)*100),'%')
big_pred_all$Champ <- paste0(sprintf('%.0f',round(big_pred_all$Champ,3)*100),'%')

final_df <- big_pred_all[,c('Tm_Code','Season','Q_date','Ovr_Rank','SWAER','Div_Rank','tm_info','Opp_Helm','Opp_Rank','Opp_Code','Opp','Opp_Div','Q_score','spread','Q_pred','Week','Q_Loc','Q_Opp_Div','Opp_Div_Rank','Conf_Matchup','Playoff','Excl_Harbin','Conf','Curr_Seed','Proj_Seed','In','Home11','Final4','Champ')]
write.csv(final_df,'C:/Users/Owner/Desktop/SWAER/output/final_df.csv',row.names=F)






####
####find a rivalry
big_pred_all$ID1 <- ifelse(big_pred_all$Tm_ID > as.numeric(big_pred_all$Opp_ID), big_pred_all$Tm_ID, big_pred_all$Opp_ID)
big_pred_all$ID2 <- ifelse(big_pred_all$Tm_ID > as.numeric(big_pred_all$Opp_ID), big_pred_all$Opp_ID, big_pred_all$Tm_ID)
big_pred_all$Riv_ID <- paste0(big_pred_all$ID1,'_',big_pred_all$ID2)

#no out of state tms or double counting
big_pred_all$Riv_ID[which(big_pred_all$Opp_ID=='0000')] <- NA
big_pred_all$Riv_ID[which(big_pred_all$ID1==big_pred_all$Tm_ID & big_pred_all$ID2==big_pred_all$Opp_ID)] <- NA

big_pred_all$Abs_MOV <- abs(big_pred_all$Score_diff)
big_pred_all$Comb_iSWAER <- big_pred_all$Tm_iSWAER + big_pred_all$Opp_iSWAER

Riv_agg <- aggregate(cbind(Win,Abs_MOV,Comb_iSWAER,Tm_lon,Tm_lat,Opp_lon,Opp_lat)~Riv_ID,big_pred_all,mean,na.rm=T)
Riv_cnt <- table(big_pred_all$Riv_ID)
Riv_agg$Games <- Riv_cnt[paste0(Riv_agg$Riv_ID)]
Riv_agg$Dist <- (((Riv_agg$Tm_lon - Riv_agg$Opp_lon)*52) ^ 2 + ((Riv_agg$Tm_lat - Riv_agg$Opp_lat)*69) ^ 2) ^ .5
rivals_df <- Riv_agg[,c('Win','Abs_MOV','Comb_iSWAER','Games','Dist')]
row.names(rivals_df) <- Riv_agg$Riv_ID
rivals_df <- rivals_df[which(rivals_df$Games >= 8),]
table(rivals_df$Games)

#changing some parms for scaling
sc_rivals_df <- rivals_df
sc_rivals_df$Win <- -abs(.5-rivals_df$Win)
sc_rivals_df$Abs_MOV <- -rivals_df$Abs_MOV
sc_rivals_df$Dist <- -rivals_df$Dist

rivals_df$avg_Z <- apply(apply(sc_rivals_df,2,scale),1,mean)
rivals_df$rating <- round(pnorm(scale(rivals_df$avg_Z))*10,1)
rivals_df <- rivals_df[order(-rivals_df$avg_Z),]
head(rivals_df)

tm_ids <- do.call(rbind,strsplit(row.names(rivals_df),'_'))
rivals_df1 <- rivals_df
rivals_df2 <- rivals_df

rivals_df1$Tm <- tm_ids[,2]
rivals_df1$Opp <- tm_ids[,1]

rivals_df2$Tm <- tm_ids[,1]
rivals_df2$Opp <- tm_ids[,2]
rivals_df2$Win <- 1-rivals_df2$Win

full_rivals_df <- rbind(rivals_df1,rivals_df2)
full_rivals_df <- full_rivals_df[order(-full_rivals_df$avg_Z),]
full_rivals_df$tm_name <- best_att[paste0(full_rivals_df$Tm),'School']
full_rivals_df$opp_name <- best_att[paste0(full_rivals_df$Opp),'School']

write.csv(full_rivals_df,'output/rivals.csv',row.names=F)



full_rivals_df[which(full_rivals_df$Tm=='1042'),]

best_rate <- sapply(row.names(best_att), function(x) length(which(full_rivals_df$rating[which(full_rivals_df$Tm==x)]>=8)))
table(best_rate)
sort(best_rate)

full_rivals_df[which(full_rivals_df$Tm==958),]
table(big_pred_all$Season)


barplot(table(rivals_df$rating))

head(rivals_df)
summary(rivals_df$rating)
rivals_df['1624_306',]

pnorm(1.2)

head(rivals_df)
big_pred_all[which(big_pred_all$Riv_ID=='958_394'),]

#avg_riv <- matrix(apply(rivals_df,2,mean),nrow=nrow(rivals_df),ncol=5,byrow=T)
#sd_riv <- matrix(apply(rivals_df,2,sd),nrow=nrow(rivals_df),ncol=5,byrow=T)
)/apply(rivals_df,2,sd))
head(rivals_df)

options(scipen=999)
apply(rivals_df,2,sd)


apply(rivals_df,2,scale)[which(Riv_agg$Riv_ID=='1406_1358'),]

head(Riv_agg)
head(Riv_cnt)



#####
look_at <- pred_all[which(pred_all$Tm_OH==1 & pred_all$Season>=2007 & pred_all$AJ_Correct==0),]
look_at$spread <- look_at$elo_diff/19.1
look_at[order(look_at$elo_diff)[1:10],]

look_at <- pred_all[which(pred_all$Tm_OH==1 & pred_all$Season>=2007 & pred_all$AJ_Correct==0),]











mean(pred_all$LogLoss[which(pred_all$use==1 & pred_all$Season>=2006)])
weekly_pk <- aggregate(AJ_Pick~Week, data=pred_all, FUN=sum, subset = use==1 & Season==2017)
aggregate(LogLoss~Week, data=pred_all, FUN=mean, subset = use==1)

weekly_pk
plot(weekly_pk ,type='l')
plot(weekly ,type='l')






str(pred_all)
pred_all$elo_fav <- ifelse(pred_all$Tm_elo.curr > pred_all$Opp_elo.curr,pred_all$Tm_elo.curr,pred_all$Opp_elo.curr)
###





#begin looking at wk slope
slope_list <- unlist(lapply(seq(.65,1.15,.05), function(x) {
lapply(1:15, function(y) {
norm <- rep(1,15)
norm[y] <- x
norm
})}),recursive=F)

slope_list_new <- unlist(lapply(seq(-.1,.1,.05),function(x) {
lapply(1:15, function(y) {
trend_slope[y] <- trend_slope[y]+x
trend_slope
})}),recursive=F)


slope_testing <- sapply(1:length(slope_list), function(x) elo_def(wk_slope=slope_list[[x]]))
try_slope <- rebase_slope(seq(.65,1.15,.05)[apply(matrix(slope_testing[4,],15),1,function(x) order(x)[1])])

slope_testing_new <- sapply(1:length(slope_list_new), function(x) elo_def(wk_slope=slope_list_new[[x]]))
new_trend_slope <- trend_slope+seq(-.1,.1,.05)[apply(matrix(slope_testing_new[4,],15),1,function(x) order(x)[1])]
retry_slope <- rebase_slope(c(predict(lm(new_trend_slope[1:10]~c(1:10))),rep(predict(lm(new_trend_slope[1:10]~c(1:10)))[10],5)))

plot(rebase_slope(retry_slope))

elo_def(wk_slope=try_slope)
elo_def(wk_slope=trend_slope)
elo_def(wk_slope=best_slope)
elo_def()

elo_def(wk_slope=rebase_slope(retry_slope))

best_slope <- slope_fun(.061,1.389)
best_slope <- slope_fun(.05,1.1)
plot(best_slope)





plot(rebase_slope(c(15:1)+20),type='l',col='red',ylim=c(.6,1.5))
lines(best_slope,col='green')
lines(trend_slope,col='blue')
lines(try_slope,col='gold')

length(best_slope)

trend_slope <- rebase_slope(c(reg_szn_trend,rep(reg_szn_trend[10],5)))


plot(try_slope)
reg_szn_trend <- predict(lm(try_slope[1:10]~c(1:10)))

#other parms
#wk_slope <- try_slope
#margin_adj <- function(mar, elo_d)  pnorm(mar/30) - 1/(10^(-elo_d/400)+1)
#margin_adj <- function(mar, elo_d) ifelse(mar>0,1,-1) * log(abs(mar)+1) * (2.2/(abs(elo_d)/1000+2.2))
#margin_adj <- function(mar, elo_d) log(abs(mar)+1)
#margin_adj <- function(mar, elo_d, wk) 1

####notes for gaussian margin formula
####K & k_mar at 120, no reg, home_adv at 60 gets .4197 log loss & 80.8% accuracy
####K 135 & k_mar 155, no reg, home_adv at 25 gets .416 log loss & 80.3% accuracy

####notes for nates margin formula
####K 75, home_adv at 25, reg at .3 gets log loss .4139 & 80.1% accuracy
####K 80, home_adv at 25, reg at .3 gets log loss .4122 & 79.9% accuracy without nate's adj
####K 80, home_adv at 25, reg at .3 and week adj of (1:15+20) with reg_adj gets log loss .4095 & 80.4% accuracy



#####solve best possible slope
wk_slope <- rep(1,15)

#inputs <- seq(40,70,5)
output <- sapply(1:15, function(w) {
sapply(seq(.65,1.15,.05), function(rel) {
wk_slope[w] <- rel
margin_adj <- function(mar, elo_d, wk) log(abs(mar)+1) * wk_slope[wk]
do_elo(K,K_mar,home_adv,reg_fac,margin_adj,'sched',TRUE,'cycle')
#aggregate(elo_win~Week,data=logger[[1]], FUN= function(x) mean(-log(x),na.rm=T), subset= Season>=2006 & Win==1)[,2]
})})

logloss_for_wks <- sapply(1:15, function(x) output[c(0:10*5+4),x])
try_slope <- rebase_slope(seq(.65,1.15,.05)[apply(logloss_for_wks,2,function(x) order(x)[1])])

suggested_k <- inputs[sapply(1:15, function(x) order(output[x,])[1])]
lm(suggested_k[1:10]~c(1:10))
lm(suggested_k[11:15]~c(11:15))
plot(suggested_k)

slope_min <- c(29+1:10*2.3,134-6.5*11:15)

par(mfrow=c(1,1))
plot(inputs,output[1,],type='l',ylim=c(.3,.7))
for (j in 2:5) lines(inputs,output[j,])
text(x=rep(8,60),y=output[1:5,8],labels=c(1:5))

plot(sapply(1:15, function(x) lm(inputs~output[x,])$coefficients[2]))

sapply(1:15, function(x) lm(1:8~output[x,])$coefficients[2]))

slope(1:15,1:15)
apply(output,1,BestSlope)
#####

aggregate(Current~Div,best_att,FUN=mean)

best_picks <- do_elo(K,K_mar,home_adv,reg_fac,margin_adj,'sched',TRUE,'log')[[1]]
best_picks <- best_picks[which(best_picks$Season==2017),]
best_picks$tm_date <- paste0(best_picks$Tm_ID,'_',best_picks$Date)
write.csv(merge(best_picks,drew_df,by='tm_date',all=T),'output/drew_compare.csv',row.names=F)

unique(tm_opp_ID)

str(best_picks)
str(drew_df_single)


##crystal ball
library(nnet)
table(c(pred_all$Tm_score,pred_all$Opp_score))/length(c(pred_all$Tm_score,pred_all$Opp_score))

score_list <- expand.grid(Winner=2:56,Loser=c(0,2:56))
score_list <- score_list[which(score_list$Loser < score_list$Winner),]
paste0(score_list$Winner,'-',score_list$Loser)

crystal_ball <- pred_all[which(pred_all$elo_diff>=0 & pred_all$Season>2006 & pred_all$Season<2017),c('Season','Week','Div','elo_diff','Tm_elo.curr','Tm_score','Opp_score')]
crystal_ball$final <- as.factor(paste0(crystal_ball$Tm_score,'-',crystal_ball$Opp_score))

cb_mod <- multinom(final~Div+elo_diff, data=crystal_ball, MaxNWts=10000000, subset= Season==2009)
summary(cb_mod)
sort(table(predict(cb_mod,pred_all[which(pred_all$elo_diff>=0 & pred_all$Season==2017),],type='class')))

library(rpart)
tree_mod <- rpart(final~Div+elo_diff+Week+Tm_elo.curr, data=crystal_ball, control=rpart.control(cp=.0005))
sort(table(predict(tree_mod,pred_all[which(pred_all$elo_diff>=0 & pred_all$Season==2017),],type='class')))


?glm

str(crystal_ball)
str(pred_all)

options(scipen=999)
