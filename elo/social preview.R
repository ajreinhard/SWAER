library(png)
final_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/final_df2.csv',stringsAsFactors=F)
seeding_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/2018/seeding1.csv',stringsAsFactors=F)
OHSAA_df <- read.csv('C:/Users/Owner/Documents/GitHub/SWAER/data sets/OHSAA ALL.csv',stringsAsFactors=F)
setwd('C:/Users/Owner/Desktop/SWAER/helmets/pics')




tm_ln <- 56

tm_df <- final_df[which(final_df$Tm_Code==OHSAA_df$HyTek[tm_ln] & final_df$Season==2018),]
tm_df$Opp_Div_Rank <- ifelse(is.na(tm_df$Opp_Div_Rank),'',paste0(' #',tm_df$Opp_Div_Rank))
tm_df$Opp_Rank <- ifelse(is.na(tm_df$Opp_Rank),'',paste0('#',tm_df$Opp_Rank))

pull_col <- c(paste0('X',1:8),'miss')
seed_dist <- seeding_df[which(seeding_df$X==OHSAA_df$OHSAA.ID[tm_ln]),pull_col]
seed_nm <- c('OUT',paste0('#',8:1))

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

dev.new(width=4.5, height=8)
pic_mx <- matrix(c(1,1,2,4,3,4), 3, 2, byrow = TRUE)
layout(pic_mx,widths=c(2.25,2.25),heights=c(4,2,2),TRUE)
par(mar=c(0,0,2.5,0),bg=OHSAA_df$Color1[tm_ln])

tm_str <- paste0(tm_df$Q_date,'  ',tm_df$Opp_Rank,' ',tm_df$Opp)
plot(NA,ylim=c(0,20),xlim=c(-1,11),main=paste0('#',tm_df$Ovr_Rank[1],' ',OHSAA_df$Best.Name[tm_ln],' ',OHSAA_df$Mascot[tm_ln],'\n 2018 Season Preview'),axes=F)
for (i in 1:10) {
arrows(0,(10-i)*2+.6,win_pct[i]*10,(10-i)*2+.6,length=0,lwd=10,col=OHSAA_df$Color2[tm_ln])
if (is.na(tm_df$Conf_Matchup[i])) {
text(0,(10-i)*2+1,bquote(.(tm_str[i])[.(tm_df$Opp_Div_Rank[i])]^.(tm_df$Q_Opp_Div[i])),adj = c(0,0))
} else {
text(0,(10-i)*2+1,bquote(bold(.(tm_str[i]))[.(tm_df$Opp_Div_Rank[i])]^.(tm_df$Q_Opp_Div[i])),adj = c(0,0))
}
#text(1,(10-i)*2+.3,tm_df$Q_pred[i],adj = c(1,0))
text(11,(10-i)*2+1.3,last_gm[i],adj = c(1,0))
rasterImage(readPNG(opp_helm[i]),-1.5,(10-i)*2,0,(10-i)*2+2)
}

par(mar=c(1,3,0,0))
barplot(win_dist[,2],names.arg=rec_nm,horiz=T,las=1,col=OHSAA_df$Color2[tm_ln])
par(mar=c(2,3,1,0))
barplot(rev(unlist(seed_dist)),names.arg=seed_nm,horiz=T,las=1,col=OHSAA_df$Color2[tm_ln])
par(mar=c(0,0,0,0))
plot(NA,ylim=c(0,20),xlim=c(0,11),axes=F)
text(0,20,bquote(bold(.('Probability of...'))),adj = c(0,0))
text(0,19,'Making the Playoffs:',adj = c(0,0))
text(0,18,'Home Playoff Game:',adj = c(0,0))
text(0,17,'Regional Championship:',adj = c(0,0))
text(0,16,'State Championship:',adj = c(0,0))
text(0,15,'Conference Title:',adj = c(0,0))
text(0,13,bquote(bold(.('Power Ranking'))),adj = c(0,0))


text(11,19,tm_df$In[1],adj = c(1,0))
text(11,18,tm_df$Home11[1],adj = c(1,0))
text(11,17,tm_df$Final4[1],adj = c(1,0))
text(11,16,tm_df$Champ[1],adj = c(1,0))
text(11,15,tm_df$Conf[1],adj = c(1,0))
text(11,12,paste0('#',tm_df$Div_Rank[1]),adj = c(1,0))




#col2rgb('blue')
(9/16)*10
?barplot
wins <- round(sum(as.numeric(substr(tm_df$Q_pred,1,4))/100),0)






head(final_df)


j<-2
plot(1,1, main=expression(bquote(jj)^j))
plot(1, 1, main = bquote(title[.(j)]))
text(1,1,bquote(title[.(j)]))