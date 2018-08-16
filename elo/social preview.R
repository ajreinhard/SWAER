library(png)
final_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/final_df2.csv',stringsAsFactors=F)
rec_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/rec rankings.csv',stringsAsFactors=F)
hist_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/hist rankings.csv',stringsAsFactors=F)
crystal_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/crystal.csv',stringsAsFactors=F)
seeding_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/2018/seeding1.csv',stringsAsFactors=F)
OHSAA_df <- read.csv('C:/Users/Owner/Documents/GitHub/SWAER/data sets/OHSAA ALL.csv',stringsAsFactors=F)
setwd('C:/Users/Owner/Desktop/SWAER/helmets/pics')



which(OHSAA_df$HyTek=='CLSI')
which(OHSAA_df$OHSAA.ID==394)
tm_ln <- 563

last_yr_rank <- hist_df[match(paste0('2017_15_',OHSAA_df$OHSAA.ID[tm_ln]),paste0(hist_df$Season,'_',hist_df$Week,'_',hist_df$Tm_ID)),]


tm_rec <- rec_df[match(paste0('2017_15_',OHSAA_df$OHSAA.ID[tm_ln]),paste0(rec_df$Season,'_',rec_df$Week,'_',rec_df$Tm_ID)),c('W','L','T','C_W','C_L','C_T')]
reg_rec <- ifelse(tm_rec$T==0,paste0(tm_rec$W,'-',tm_rec$L),paste0(tm_rec$W,'-',tm_rec$L,'-',tm_rec$T))
con_rec <- ifelse(tm_rec$C_T==0,paste0(tm_rec$C_W,'-',tm_rec$C_L),paste0(tm_rec$C_W,'-',tm_rec$C_L,'-',tm_rec$C_T))
fin_rec <- ifelse(con_rec=='0-0',reg_rec,paste0(reg_rec,', ',con_rec))

seed17 <- crystal_df$curr_seed[match(paste0('2017_10_',OHSAA_df$OHSAA.ID[tm_ln]),paste0(crystal_df$Season,'_',crystal_df$Week,'_',crystal_df$TeamID))]

tm_df <- final_df[which(final_df$Tm_Code==OHSAA_df$HyTek[tm_ln] & final_df$Season==2018),]
tm_df$Opp_Div_Rank <- ifelse(is.na(tm_df$Opp_Div_Rank),'',paste0(' #',tm_df$Opp_Div_Rank))
tm_df$Opp_Rank <- ifelse(is.na(tm_df$Opp_Rank),'',paste0('#',tm_df$Opp_Rank))

pull_col <- c(paste0('X',1:8),'miss')
seed_dist <- seeding_df[which(seeding_df$X==OHSAA_df$OHSAA.ID[tm_ln]),pull_col]
seed_nm <- c('OUT',paste0('#',8:1))
reg_rnk <- c(tm_df$Div_Rank[1],final_df$Div_Rank[match(paste0(OHSAA_df$HyTek[which(OHSAA_df$Reg.2018==OHSAA_df$Reg.2018[tm_ln])],'_2018'),paste0(final_df$Tm_Code,'_',final_df$Season))])
conf_rnk <- c(tm_df$Ovr_Rank[1],final_df$Ovr_Rank[match(paste0(OHSAA_df$HyTek[which(OHSAA_df$Conf.2018==OHSAA_df$Conf.2018[tm_ln])],'_2018'),paste0(final_df$Tm_Code,'_',final_df$Season))])
cnty_rnk <- c(tm_df$Ovr_Rank[1],final_df$Ovr_Rank[match(paste0(OHSAA_df$HyTek[which(OHSAA_df$County==OHSAA_df$County[tm_ln])],'_2018'),paste0(final_df$Tm_Code,'_',final_df$Season))])


win_pct <- as.numeric(substr(tm_df$Q_pred,1,nchar(tm_df$Q_pred)-1))/100
wins_prob <- expand.grid(lapply(win_pct,function(x) c(x,1-x)))
wins_res <- expand.grid(lapply(win_pct,function(x) c(1,0)))
win_dist <- aggregate(apply(wins_prob,1,prod)~apply(wins_res,1,sum),FUN=sum)
rec_nm <- paste0(win_dist[,1],'-',nrow(tm_df)-win_dist[,1])
opp_helm <- paste0(OHSAA_df$OHSAA.ID[match(tm_df$Opp_Code,OHSAA_df$HyTek)],'_L.png')

last_mtg <- aggregate(Season~Opp_Code,final_df,FUN=max,subset= Tm_Code==OHSAA_df$HyTek[tm_ln] & Season!=2018)
last_yr_mtg <- last_mtg[match(tm_df$Opp_Code,last_mtg[,1]),2]
mtg_id <- paste0(tm_df$Tm_Code,'_',tm_df$Opp_Code,'_',last_yr_mtg)
last_score <- final_df$Q_score[match(mtg_id,paste0(final_df$Tm_Code,'_',final_df$Opp_Code,'_',final_df$Season))]
last_gm <- ifelse(is.na(last_score),'',paste0(last_score,' (\'',substr(last_yr_mtg,3,4),')'))

wins_exp <- round(sum(win_pct),0)
rec_exp <- paste0(wins_exp,' - ',nrow(tm_df)-wins_exp)

new_col <- (255-col2rgb(OHSAA_df$Color1[tm_ln]))/4+col2rgb(OHSAA_df$Color1[tm_ln])
col_1 <- rgb(new_col['red',1]/255,new_col['green',1]/255,new_col['blue',1]/255)
new_col <- (255-col2rgb(OHSAA_df$Color2[tm_ln]))/4+col2rgb(OHSAA_df$Color2[tm_ln])
col_2 <- rgb(new_col['red',1]/255,new_col['green',1]/255,new_col['blue',1]/255)


#dev.new(width=450, height=800)
png('C:/Users/Owner/Documents/GitHub/SWAER/my sample3.png',width=450, height=800)
pic_mx <- matrix(c(1,1,2,4,3,4), 3, 2, byrow = TRUE)
layout(pic_mx,widths=c(2.25,2.25),heights=c(4,2,2),TRUE)
par(mar=c(0,0,4,0),bg=col_1)

tm_str <- paste0(tm_df$Q_date,'  ',tm_df$Opp_Rank,' ',tm_df$Opp)
plot(NA,ylim=c(0,20),xlim=c(-1,11),main=paste0('#',tm_df$Ovr_Rank[1],' ',OHSAA_df$Best.Name[tm_ln],' ',OHSAA_df$Mascot[tm_ln],'\n 2018 Season Preview'),axes=F,cex.main=2)
for (i in 1:10) {
arrows(0,(10-i)*2+.6,win_pct[i]*10,(10-i)*2+.6,length=0,lwd=10,col=col_2)
if (is.na(tm_df$Conf_Matchup[i])) {
text(0,(10-i)*2+1,bquote(.(tm_str[i])[.(tm_df$Opp_Div_Rank[i])]^.(tm_df$Q_Opp_Div[i])),adj = c(0,0),cex=2)
} else {
text(0,(10-i)*2+1,bquote(bold(.(tm_str[i]))[.(tm_df$Opp_Div_Rank[i])]^.(tm_df$Q_Opp_Div[i])),adj = c(0,0),cex=2)
}
text(11,(10-i)*2+.3,tm_df$Q_pred[i],adj = c(1,0),cex=2)
text(11,(10-i)*2+1.3,last_gm[i],adj = c(1,0),cex=2)
rasterImage(readPNG(opp_helm[i]),-1.5,(10-i)*2,0,(10-i)*2+2)
}

par(mar=c(1,3,0,0))
barplot(win_dist[,2],names.arg=rec_nm,horiz=T,las=1,col=col_2,axes=F)
split_by <- round(max(win_dist[,2])*25,0)/100
axis(1,seq(0,4*split_by,split_by),lab=paste0(seq(0,4*split_by,split_by)*100,'%'))
par(mar=c(2,3,1,0))
barplot(rev(unlist(seed_dist)),names.arg=seed_nm,horiz=T,las=1,col=col_2,axes=F)
split_by <- round(max(unlist(seed_dist))*25,0)/100
axis(1,seq(0,4*split_by,split_by),lab=paste0(seq(0,4*split_by,split_by)*100,'%'))
par(mar=c(0,0,0,0))
plot(NA,ylim=c(0,20),xlim=c(0,11),axes=F)
text(0,20,bquote(bold(.('Probability of...'))),adj = c(0,0),cex=2)
text(0,19,'Making the Playoffs:',adj = c(0,0),cex=2)
text(0,18,'Home Playoff Game:',adj = c(0,0),cex=2)
text(0,17,'Regional Championship:',adj = c(0,0),cex=2)
text(0,16,'State Championship:',adj = c(0,0),cex=2)
text(0,15,'Conference Title:',adj = c(0,0),cex=2)
text(0,13,bquote(bold(.('Power Ranking'))),adj = c(0,0),cex=2)
text(0,12,'Divison:',adj = c(0,0),cex=2)
text(0,11,'Region:',adj = c(0,0),cex=2)
text(0,10,'Conference:',adj = c(0,0),cex=2)
text(0,9,'County:',adj = c(0,0),cex=2)
text(0,7,bquote(bold(.('2017 Recap'))),adj = c(0,0),cex=2)


text(11,19,tm_df$In[1],adj = c(1,0),cex=2)
text(11,18,tm_df$Home11[1],adj = c(1,0),cex=2)
text(11,17,tm_df$Final4[1],adj = c(1,0),cex=2)
text(11,16,tm_df$Champ[1],adj = c(1,0),cex=2)
text(11,15,tm_df$Conf[1],adj = c(1,0),cex=2)
text(11,12,paste0('#',tm_df$Div_Rank[1],' of ',length(which(OHSAA_df$Div.2018==OHSAA_df$Div.2018[tm_ln]))),adj = c(1,0),cex=2)
text(11,11,paste0('#',rank(reg_rnk,ties.method='min')[1],' of ',length(reg_rnk)-1),adj = c(1,0),cex=2)
text(11,10,paste0('#',rank(conf_rnk,ties.method='min')[1],' of ',length(conf_rnk)-1),adj = c(1,0),cex=2)
text(11,9,paste0('#',rank(cnty_rnk,ties.method='min')[1],' of ',length(cnty_rnk)-1),adj = c(1,0),cex=2)
text(0,6,fin_rec,adj = c(0,0),cex=2)
text(11,6,paste0('#',seed17,' Seed in Reg ',last_yr_rank$Reg),adj = c(1,0),cex=2)
text(0,5,paste0('#',last_yr_rank$OvrRank,' Overall / D',last_yr_rank$Div,' #',last_yr_rank$DivRank),adj = c(0,0),cex=2)




dev.off()



?text




head(final_df)


j<-2
plot(1,1, main=expression(bquote(jj)^j))
plot(1, 1, main = bquote(title[.(j)]))
text(1,1,bquote(title[.(j)]))