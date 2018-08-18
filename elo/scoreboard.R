library(RJSONIO)
library(png)
setwd('C:/Users/Owner/Documents/GitHub/SWAER')
OHSAA_df <- read.csv('data sets/OHSAA ALL.csv',stringsAsFactors=F)
final_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/final_df2.csv',stringsAsFactors=F)

head(final_df)
board_df <- final_df[,c('Season','Week','Q_date','Q_Loc','Tm_Code','Div_Rank','Ovr_Rank','Opp','Opp_Code','Opp_Rank','Opp_Div_Rank','Q_Opp_Div','Q_score','Q_pred','spread','Excl_Harbin','my_link')]
board_df$Tm_Div <- board_df$Q_Opp_Div[match(paste0(board_df$Tm_Code,'_',board_df$Season),paste0(board_df$Opp_Code,'_',board_df$Season))]
board_df$Tm <- board_df$Opp[match(paste0(board_df$Tm_Code,'_',board_df$Season),paste0(board_df$Opp_Code,'_',board_df$Season))]
board_df$Tm_Helm <- paste0(OHSAA_df$OHSAA.ID[match(board_df$Tm_Code,OHSAA_df$HyTek)],'_L')
board_df$Opp_Helm <- paste0(OHSAA_df$OHSAA.ID[match(board_df$Opp_Code,OHSAA_df$HyTek)],'_R')
board_df$Opp_Helm <- ifelse(board_df$Opp_Helm=='NA_R',paste0(board_df$Opp_Code,'_R'),board_df$Opp_Helm)

board_df$keep <- 0
board_df$keep[which(board_df$Q_Loc=='@')] <- 1
board_df$keep[which(board_df$Q_Loc=='(N)' & board_df$spread>0)] <- 1
board_df$keep[matrix(which(board_df$Q_Loc=='(N)' & board_df$spread==0),2)[1,]] <- 1
board_df$keep[which(board_df$my_link==1)] <- 1
board_df <- board_df[which(board_df$keep==1),]




rank_list1 <- lapply(2007:2010, function(yr) {
lapply(1:15, function(wk) {
board_df[which(board_df$Week==wk & board_df$Season==yr),]
})
})

rank_list2 <- lapply(2011:2014, function(yr) {
lapply(1:15, function(wk) {
board_df[which(board_df$Week==wk & board_df$Season==yr),]
})
})

rank_list3 <- lapply(2015:2018, function(yr) {
lapply(1:15, function(wk) {
board_df[which(board_df$Week==wk & board_df$Season==yr),]
})
})


names(rank_list1) <- 2007:2010
names(rank_list2) <- 2011:2014
names(rank_list3) <- 2015:2018
exportJson1 <- toJSON(rank_list1)
exportJson2 <- toJSON(rank_list2)
exportJson3 <- toJSON(rank_list3)
write(exportJson1, paste0('C:/Users/Owner/Desktop/SWAER/output/GameList_2007-10.json'))
write(exportJson2, paste0('C:/Users/Owner/Desktop/SWAER/output/GameList_2011-14.json'))
write(exportJson3, paste0('C:/Users/Owner/Desktop/SWAER/output/GameList_2015-17.json'))



##redo
all_helms <- read.table('data/Fake Helmets.txt',stringsAsFactors=F)
direction <- sapply(strsplit(all_helms$V1,'_'),function(x) x[[2]])
tm_id <- sapply(strsplit(all_helms$V1,'_'),function(x) x[[1]])
need_right <- tm_id[which(direction=='R')]
need_left <- tm_id[which(direction=='L')]

####making some flipped over helmets
tm_id <- sapply(strsplit(dir('helmets'),'_'),function(x) x[[1]])
single_helm <- names(which(table(tm_id)!=2))
need_left <- single_helm[which(!is.na(match(paste0(single_helm,'_R.png'),dir('helmets'))))]
need_right <- single_helm[which(!is.na(match(paste0(single_helm,'_L.png'),dir('helmets'))))]

#so I have a record of the new helmets I created
write.table(c(paste0(need_left,'_L'),paste0(need_right,'_R')),'Fake Helmets.txt',quote=F,row.names=F,col.names=F)

#make em!
for (j in 1:length(need_left)) {
png(paste0('helmets/',need_left[j],'_L.png'),bg='transparent',width = 180, height = 138)
par(mar=c(0,0,0,0))
plot(NA,xlim=c(180,1),ylim=c(1,138),axes=F)
rasterImage(readPNG(paste0('helmets/',need_left[j],'_R.png')),1,1,180,138)
dev.off()
}

for (j in 1:length(need_right)) {
png(paste0('helmets/',need_right[j],'_R.png'),bg='transparent',width = 180, height = 138)
par(mar=c(0,0,0,0))
plot(NA,xlim=c(180,1),ylim=c(1,138),axes=F)
rasterImage(readPNG(paste0('helmets/',need_right[j],'_L.png')),1,1,180,138)
dev.off()
}

