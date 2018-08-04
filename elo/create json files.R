library(RJSONIO)
setwd('C:/Users/Owner/Documents/GitHub/SWAER')
OHSAA_df <- read.csv('data sets/OHSAA ALL.csv',stringsAsFactors=F)


#create json files for Team Pages
records <- read.csv('C:/Users/Owner/Desktop/SWAER/output/rec rankings.csv',stringsAsFactors=F)
rankings <- read.csv('C:/Users/Owner/Desktop/SWAER/output/hist rankings.csv',stringsAsFactors=F)
records$Week[which(records$Season==2018)] <- 15
rankings$Week[which(rankings$Season==2018)] <- 15

records <- records[which(records$Week==15),]
records$Total <- ifelse(records$T==0, paste0(records$W,'-',records$L),paste0(records$W,'-',records$L,'-',records$T))
records$ConfR <- ifelse(records$C_T==0, paste0(records$C_W,'-',records$C_L),paste0(records$C_W,'-',records$C_L,'-',records$C_T))
records$ConfR[which(records$Conf=='IND')] <- '0-0'
records$ConfR[which(records$Conf=='OH VAL')] <- '0-0'
records$Final <- ifelse(records$ConfR=='0-0',records$Total,paste0(records$Total,', ',records$ConfR))

rankings <- rankings[which(rankings$Week==15),]


final_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/final_df2.csv',stringsAsFactors=F)
final_list <- list(
	final_df[which(final_df$Season>=2007 & final_df$Season<=2009),],
	final_df[which(final_df$Season>=2010 & final_df$Season<=2012),],
	final_df[which(final_df$Season>=2013 & final_df$Season<=2015),],
	final_df[which(final_df$Season>=2016 & final_df$Season<=2018),])
df_names <- c('2007-09','2010-12','2013-15','2016-18')

for (k in 1:4) {
###
my_df <- final_list[[k]]

Team_IDs <- OHSAA_df$OHSAA.ID[match(unique(my_df$Tm_Code),OHSAA_df$HyTek)]

list1 <- lapply(unique(my_df$Tm_Code), function(x) my_df[which(my_df$Tm_Code==x),c(2:29)])
by_yr <- lapply(1:length(list1), function(x) {
lapply(2002:2018, function(y) {
list('Team'=c('Season'=y, 
'Conf'=records$Conf[which(records$Tm_ID==Team_IDs[x] & records$Season==y)],
'Record'=records$Final[which(records$Tm_ID==Team_IDs[x] & records$Season==y)],
'Finish'=rankings[which(rankings$Tm_ID==Team_IDs[x] & rankings$Season==y),c('Div','Reg','DivRank','OvrRank','Tm_ID')]
),
'Games'=list1[[x]][which(list1[[x]]$Season==y),c(2:28)])
})
})
row_cnt <- sapply(by_yr,function(x) sapply(x,nrow))

for (i in 1:length(by_yr)) {
for (j in 17:1) {
if (nrow(by_yr[[i]][[j]][[2]])==0) {
by_yr[[i]][[j]] <- NULL
} else {
names(by_yr[[i]])[j] <- 2001 + j
}}}

names(by_yr) <- unique(my_df$Tm_Code)

exportJson <- toJSON(by_yr)
write(exportJson, paste0('C:/Users/Owner/Desktop/SWAER/output/TmPgs_',df_names[k],'.json'))
}
#

##create JSON files for head of Team Pages
tm_list <- apply(OHSAA_df, 1, list)
names(tm_list) <- OHSAA_df$HyTek

exportJson2 <- toJSON(tm_list)
write(exportJson2, paste0('Tm_Info.json'))



##
###Start with Brackets DF
##
playoffs_df <- read.csv('output/Playoffs Bracket.csv',stringsAsFactors=F)
JE_games <- read.csv('output/JoeEitel GameLog.csv',stringsAsFactors=F)
JE_games <- JE_games[which(JE_games$Playoff==1),]
hist_rankings <- read.csv('output/hist rankings.csv',stringsAsFactors=F)

playoffs_df$Round <- playoffs_df$Week - 10

#look up region
playoffs_df$fav_lookup <- playoffs_df$Reg[match(paste0(playoffs_df$Season,'_',playoffs_df$FavID),paste0(playoffs_df$Season,'_',playoffs_df$FavID))]
playoffs_df$fav_lookup2 <- playoffs_df$Reg[match(paste0(playoffs_df$Season,'_',playoffs_df$FavID),paste0(playoffs_df$Season,'_',playoffs_df$DogID))]
playoffs_df$Reg <- ifelse(is.na(playoffs_df$fav_lookup),playoffs_df$fav_lookup2,playoffs_df$fav_lookup)
#look up top line seeds
playoffs_df$fav_lookup <- playoffs_df$Fav[match(paste0(playoffs_df$Season,'_',playoffs_df$FavID),paste0(playoffs_df$Season,'_',playoffs_df$FavID))]
playoffs_df$fav_lookup2 <- playoffs_df$Dog[match(paste0(playoffs_df$Season,'_',playoffs_df$FavID),paste0(playoffs_df$Season,'_',playoffs_df$DogID))]
playoffs_df$Fav <- ifelse(is.na(playoffs_df$fav_lookup),playoffs_df$fav_lookup2,playoffs_df$fav_lookup)
#look up bottom line seeds
playoffs_df$fav_lookup <- playoffs_df$Dog[match(paste0(playoffs_df$Season,'_',playoffs_df$DogID),paste0(playoffs_df$Season,'_',playoffs_df$DogID))]
playoffs_df$fav_lookup2 <- playoffs_df$Fav[match(paste0(playoffs_df$Season,'_',playoffs_df$DogID),paste0(playoffs_df$Season,'_',playoffs_df$FavID))]
playoffs_df$Dog <- ifelse(is.na(playoffs_df$fav_lookup),playoffs_df$fav_lookup2,playoffs_df$fav_lookup)

#Fav OHSAA matches
playoffs_df$Top_Team <- OHSAA_df$Best.Name[match(playoffs_df$FavID,OHSAA_df$OHSAA.ID)]
playoffs_df$Top_Helm <- OHSAA_df$Helm.Left[match(playoffs_df$FavID,OHSAA_df$OHSAA.ID)]
playoffs_df$Top_HyTek <- OHSAA_df$HyTek[match(playoffs_df$FavID,OHSAA_df$OHSAA.ID)]
#Dog OHSAA matches
playoffs_df$Bottom_Team <- OHSAA_df$Best.Name[match(playoffs_df$DogID,OHSAA_df$OHSAA.ID)]
playoffs_df$Bottom_Helm <- OHSAA_df$Helm.Left[match(playoffs_df$DogID,OHSAA_df$OHSAA.ID)]
playoffs_df$Bottom_HyTek <- OHSAA_df$HyTek[match(playoffs_df$DogID,OHSAA_df$OHSAA.ID)]

po_id <- paste0(playoffs_df$FavID,'_',playoffs_df$DogID,'_',playoffs_df$Season,'_',playoffs_df$Week)
JE_id <- paste0(JE_games$Tm_ID,'_',JE_games$Opp_ID,'_',JE_games$Season,'_',JE_games$Week)
playoffs_df[,c('Top_Score','Bottom_Score')] <- JE_games[match(po_id,JE_id),c('Tm_score','Opp_score')]

#SWAER Div Rankings
playoffs_df$Top_DivRank <- hist_rankings$DivRank[match(paste0(playoffs_df$Season,'_',playoffs_df$Week,'_',playoffs_df$FavID),paste0(hist_rankings$Season,'_',hist_rankings$Week+1,'_',hist_rankings$TeamID))]
playoffs_df$Bottom_DivRank <- hist_rankings$DivRank[match(paste0(playoffs_df$Season,'_',playoffs_df$Week,'_',playoffs_df$DogID),paste0(hist_rankings$Season,'_',hist_rankings$Week+1,'_',hist_rankings$TeamID))]

playoffs_df$Bottom_Winner <- ifelse(playoffs_df$Top_Score < playoffs_df$Bottom_Score,'winner','')
playoffs_df$Top_Winner <- ifelse(playoffs_df$Top_Score < playoffs_df$Bottom_Score,'','winner')


playoffs_df$Div <- hist_rankings$Div[match(paste0(playoffs_df$Season,'_',playoffs_df$Week,'_',playoffs_df$FavID),paste0(hist_rankings$Season,'_',hist_rankings$Week+1,'_',hist_rankings$TeamID))]
pull_cols <- c('Fav','Dog',names(playoffs_df)[15:26])

div_list <- c(lapply(2007:2012,function(x) lapply(seq(0,20,4),function(y) y+(1:4))),
		lapply(2013:2015,function(x) c(list(1:2),lapply(seq(2,22,4),function(y) y+(1:4)))),
		lapply(2016:2017,function(x) lapply(seq(0,24,4),function(y) y+(1:4))))
names(div_list) <- 2007:2017

po_list <- lapply(2007:2017, function(yr) {
reg_list <- div_list[[paste0(yr)]]
lapply(1:length(reg_list), function(div_reg) {
reg_set <- lapply(reg_list[[div_reg]], function(reg) {
lapply(1:3, function(rnd) {
playoffs_df[which(playoffs_df$Reg==reg & playoffs_df$Season==yr & playoffs_df$Round==rnd),pull_cols]
})
})
po_finals <- lapply(3:5, function(rnd) {
playoffs_df[which(playoffs_df$Div==div_reg & playoffs_df$Season==yr & playoffs_df$Round==rnd),pull_cols]
})
full_list <- c(list(po_finals),reg_set)
names(full_list) <- c('State',c(reg_list[[div_reg]]))
return(full_list)
})
})

names(po_list) <- 2007:2017
exportJson <- toJSON(po_list)
write(exportJson, paste0('Playoff Brackets.json'))



####create rankings JSON
#also need to pull in rankings and records
po_df <- read.csv('output/playoffs.csv',stringsAsFactors=F)
conf_df <- read.csv('output/conf.csv',stringsAsFactors=F)
crystal <- read.csv('output/crystal.csv',stringsAsFactors=F)
bracket <- read.csv('output/Playoffs Bracket.csv',stringsAsFactors=F)


rankings$ID <- paste0(rankings$Tm_ID,'_',rankings$Season,'_',rankings$Week)

#Win Loss Records
records$ID <- paste0(records$Tm_ID,'_',records$Season,'_',records$Week)
records$Total <- ifelse(records$T==0, paste0(records$W,'-',records$L),paste0(records$W,'-',records$L,'-',records$T))
records$ConfR <- ifelse(records$C_T==0, paste0(records$C_W,'-',records$C_L),paste0(records$C_W,'-',records$C_L,'-',records$C_T))
records$ConfR[which(records$Conf=='IND')] <- '0-0'
records$ConfR[which(records$Conf=='OH VAL')] <- '0-0'
rankings$Total <- records$Total[match(rankings$ID,records$ID)]
rankings$Total <- ifelse(is.na(rankings$Total),'0-0',rankings$Total)

rankings$ID <- paste0(rankings$Tm_ID,'_',rankings$Season,'_',ifelse(rankings$Week>=11,10,rankings$Week))
rankings$ConfR <- records$ConfR[match(rankings$ID,records$ID)]
rankings$ConfR <- ifelse(is.na(rankings$ConfR),'0-0',rankings$ConfR)

rankings$ID <- paste0(rankings$Tm_ID,'_',rankings$Season)
records$ID <- paste0(records$Tm_ID,'_',records$Season)
rankings$Conf <- records$Conf[match(rankings$ID,records$ID)]

#playoffs
rankings$ID <- paste0(rankings$Tm_ID,'_',rankings$Season,'_',ifelse(rankings$Week>=14,14,rankings$Week))

po_df$ID <- paste0(po_df$X,'_',po_df$Season,'_',po_df$Week-1)
po_df$In <- 1-po_df$X0
po_df$Final4 <- po_df$X4+po_df$X5+po_df$X6

rankings$State <- po_df$X6[match(rankings$ID,po_df$ID)]
rankings$Final4 <- po_df$Final4[match(rankings$ID,po_df$ID)]
rankings$Playoffs <- po_df$In[match(rankings$ID,po_df$ID)]

#conf
conf_df$ID <- paste0(conf_df$X,'_',conf_df$Season,'_',conf_df$Week-1)
rankings$ConfChamp <- conf_df$X.1[match(rankings$ID,conf_df$ID)]

#crystal
crystal$ID <- paste0(crystal$TeamID,'_',crystal$Season,'_',crystal$Week)
rankings$HarbinCurr <- crystal$curr_avg[match(rankings$ID,crystal$ID)]
rankings$HarbinProj <- crystal$proj_avg[match(rankings$ID,crystal$ID)]
rankings$SeedCurr <- crystal$curr_seed[match(rankings$ID,crystal$ID)]
rankings$SeedProj <- crystal$proj_seed[match(rankings$ID,crystal$ID)]

#actual seed
rankings$ID <- paste0(rankings$Tm_ID,'_',rankings$Season)
teams_df$ID <- paste0(teams_df$TeamID,'_',teams_df$Yr)
SeedActual <- teams_df$Seed[match(rankings$ID,teams_df$ID )]
SeedActual <- as.numeric(as.character(SeedActual))

rankings$SeedCurr <- ifelse(rankings$Week>=10,SeedActual,rankings$SeedCurr)
rankings$SeedProj <- ifelse(rankings$Week>=10,SeedActual,rankings$SeedProj)
rankings$HarbinCurr[which(rankings$Week==0)] <- 0

#some seeds had tie breakers.. look into later
#rankings[which(rankings$Week==10 & SeedActual>=9 & rankings$Playoffs==1),]

rankings$ID <- paste0(rankings$Tm_ID,'_',rankings$Season,'_',rankings$Week)
rankings$ID2 <- paste0(rankings$Tm_ID,'_',rankings$Season,'_0')
rankings$Change <- rankings$SWAER-rankings$SWAER[match(rankings$ID2,rankings$ID)]
rankings$County <- OHSAA_df$County[match(rankings$Tm_ID,OHSAA_df$OHSAA.ID)]
rankings$Lon <- OHSAA_df$lon[match(rankings$Tm_ID,OHSAA_df$OHSAA.ID)]
rankings$Lat <- OHSAA_df$lat[match(rankings$Tm_ID,OHSAA_df$OHSAA.ID)]
rankings$Team <- OHSAA_df$HyTek[match(rankings$Tm_ID,OHSAA_df$OHSAA.ID)]

rankings$ConfMain <- sapply(strsplit(rankings$Conf,' - '),function(x) x[1])
rankings$ConfSub <- sapply(strsplit(rankings$Conf,' - '),function(x) x[2])
rankings$Conf <- NULL

rankings$ID <- NULL
rankings$ID2 <- NULL
rankings$HarbinCurr <- NULL
rankings$HarbinProj <- NULL
 
state_champs <- paste0(JE_games$Season[which(JE_games$Week==15 & JE_games$Win==1)],'_',JE_games$Tm_ID[which(JE_games$Week==15 & JE_games$Win==1)])
rankings$State[which(rankings$Week==15)] <- 0
rankings$State[which(rankings$Week==15 & paste0(rankings$Season,'_',rankings$Tm_ID) %in% state_champs)] <- 1

rankings$State <- paste0(round(rankings$State,3)*100,'%')
rankings$Final4 <- paste0(round(rankings$Final4 ,3)*100,'%')
rankings$Playoffs <- paste0(round(rankings$Playoffs ,3)*100,'%')
rankings$ConfChamp <- paste0(round(rankings$ConfChamp ,3)*100,'%')

rankings$SWAER <- round(rankings$SWAER,1)
rankings$iSWAER <- round(rankings$iSWAER,1)
rankings$Change <- round(rankings$Change ,1)


rank_list1 <- lapply(2007:2010, function(yr) {
lapply(0:15, function(wk) {
rankings[which(rankings$Week==wk & rankings$Season==yr),]
})
})

rank_list2 <- lapply(2011:2014, function(yr) {
lapply(0:15, function(wk) {
rankings[which(rankings$Week==wk & rankings$Season==yr),]
})
})

rank_list3 <- lapply(2015:2017, function(yr) {
lapply(0:15, function(wk) {
rankings[which(rankings$Week==wk & rankings$Season==yr),]
})
})


names(rank_list1) <- 2007:2010
names(rank_list2) <- 2011:2014
names(rank_list3) <- 2015:2017
exportJson1 <- toJSON(rank_list1)
exportJson2 <- toJSON(rank_list2)
exportJson3 <- toJSON(rank_list3)
write(exportJson1, paste0('RankList_2007-10.json'))
write(exportJson2, paste0('RankList_2011-14.json'))
write(exportJson3, paste0('RankList_2015-17.json'))



###make some shapes for regions
library(grDevices)
lon <- c(467, 365, 401, 365, 365, 401, 401, 365, 415, 401, 365, 365, 365, 365, 365, 381, 365)
lat <- c(133, 71, 161, 71, 71, 161, 161, 71, 35, 161, 71, 71, 71, 71, 71, 116, 71)

shape_mapping <- rankings[which(rankings$Week=='0'),c('Season','Tm_ID','Team','Div','Reg','ConfMain','Conf','Lon','Lat')]


p <- function(..., sep='_') {
    paste(..., sep=sep, collapse=sep)
}


get_shape <- function(x) {
combo <- paste0(shape_mapping$Season,'_',x)
coord_list <- lapply(unique(combo), function(j) list(shape_mapping$Lon[which(combo==j & !is.na(shape_mapping$Lon))],shape_mapping$Lat[which(combo==j & !is.na(shape_mapping$Lat))]))
rnd_list <- lapply(coord_list, function(j) list(round(((j[[1]]--84.82)/(-80.51--84.82))*500, 0), round(((41.97-j[[2]])/(41.97-38.42))*500,0)))
chull_list <- lapply(rnd_list, function(j) paste0(j[[1]][chull(j[[1]],j[[2]])],',',j[[2]][chull(j[[1]],j[[2]])]))
paste_list <- sapply(chull_list, p, sep=' ')

names(paste_list) <- unique(combo)
return(paste_list)
}

shp_reg <- get_shape(shape_mapping$Reg)
shp_conf <- get_shape(shape_mapping$Conf)
shp_confmain <- get_shape(shape_mapping$ConfMain)
all_shp <- c(shp_reg,shp_conf,shp_confmain)
shp_names <- t(sapply(strsplit(names(all_shp),'_'),function(x) c(x[[1]],x[[2]])))
#shp_grp <- ifelse(!is.na(as.numeric(shp_names[,2])),paste0('Reg',shp_names[,2]),shp_names[,2])
shp_grp <- shp_names[,2]
shp_yr <- as.numeric(shp_names[,1])
shp_div <- ceiling(as.numeric(shp_names[,2])/4)
shp_div[which(shp_yr>=2013 & shp_yr <= 2015)] <- ceiling((as.numeric(shp_names[which(shp_yr>=2013 & shp_yr <= 2015),2])+2)/4)
shp_div[which(is.na(shp_div))] <- 'CONF'
table(shp_div)

shp_list <- lapply(2008:2017, function(y) {
lapply(c('CONF',rev(rev(sort(unique(shp_div[which(shp_yr==y)])))[c(-1)])), function(d) {
con_hull <- all_shp[which(shp_div==d & shp_yr==y)]
names(con_hull) <- shp_grp[which(shp_div==d & shp_yr==y)]
return(con_hull)
})
})

names(shp_list) <- 2008:2017
exportJson <- toJSON(shp_list)
write(exportJson, paste0('map shapes.json'))

