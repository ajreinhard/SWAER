library(RJSONIO)
library(png)
setwd('C:/Users/Owner/Documents/GitHub/SWAER')
OHSAA_df <- read.csv('data sets/OHSAA ALL.csv',stringsAsFactors=F)
final_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/final_df2.csv',stringsAsFactors=F)

board_df <- final_df[,c('Season','Week','Q_date','Q_Loc','Tm_Code','Div_Rank','Ovr_Rank','Opp_Code','Opp_Rank','Opp_Div_Rank','Q_Opp_Div',


####making some flipped over helmets
tm_id <- sapply(strsplit(dir('helmets'),'_'),function(x) x[[1]])
single_helm <- names(which(table(tm_id)!=2))
need_left <- single_helm[which(!is.na(match(paste0(single_helm,'_R.png'),dir('helmets'))))]
need_right <- single_helm[which(!is.na(match(paste0(single_helm,'_L.png'),dir('helmets'))))]

write.table(c(paste0(need_left,'_L'),paste0(need_right,'_R'),'Fake Helmets.txt')

png(paste0('helmets/',need_left[1],'_L.png'),bg='transparent',width = 180, height = 138)
par(mar=c(0,0,0,0))
plot(NA,xlim=c(180,1),ylim=c(1,138))
rasterImage(readPNG(paste0('helmets/',need_left[1],'_R.png')),1,1,180,138)
dev.off()