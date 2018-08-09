library(XML)
setwd('C:/Users/A097092/Desktop/Extra/HS Football')


p <- function(..., sep='_') {
    paste(..., sep=sep, collapse=sep)
}
proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))



###pull 2018 OH team sched on JoeEitel
files <- dir('JoeEitel/2018',full.name=T) 

JE_sched <- lapply(files, function(z) {
the_tree <- htmlTreeParse(z, useInternal=T)
sched <- sapply(xpathSApply(the_tree, '//table/tbody/tr/td'),function(y) xmlValue(y, trim=T))
opp_ID <- substr(xpathSApply(the_tree, '//table/tbody/tr/td[3]//@href'),18,nchar(xpathSApply(the_tree, '//table/tbody/tr/td[3]//@href'))-10)

if (tail(sched,1)=='# - Ohio playoff game') {sched <- sched[1:(length(sched)-1)]}
if (tail(sched,1)=='* - game does not count in OHSAA rankings') {sched <- sched[1:(length(sched)-1)]}
cbind(2018,substr(z,15,nchar(z)-4),opp_ID,t(matrix(sched,7)),1:(length(sched)/7))
})

JE_sched <- do.call(rbind,JE_sched)
row.names(JE_sched) <- NULL

#create df with all games from JoeEitel
JE_games <- data.frame(Season=JE_sched[,1],Week=JE_sched[,11],Tm_ID=JE_sched[,2],Opp_ID=JE_sched[,3],Loc=JE_sched[,5],Result=JE_sched[,8],Notes=JE_sched[,10],stringsAsFactors=F)
JE_games$Date <- as.Date(paste0(JE_sched[,4],'/',JE_sched[,1]),format='%m/%d/%Y')
JE_games$Playoff <- ifelse(substr(JE_sched[,6],1,1)=='#',1,0)
JE_games$Tm_score <- as.numeric(sapply(strsplit(JE_sched[,9],'-'),function(x) x[1]))
JE_games$Opp_score <- as.numeric(sapply(strsplit(JE_sched[,9],'-'),function(x) x[2]))

JE_games$Score_diff <- JE_games$Tm_score - JE_games$Opp_score
JE_games$Win <- ifelse(substr(JE_games$Result,1,1)=='W',1,0)

JE_games$Cal_Week <- format(JE_games$Date-1,'%W')
tm_cal_wk <- paste0(JE_games$Season,'_',JE_games$Cal_Week,'_',JE_games$Tm_ID)
mult_games_wk <- names(which(table(tm_cal_wk)>1))
JE_games$Multi_Week <- NA
JE_games$Multi_Week[matrix(which(tm_cal_wk %in% mult_games_wk),2)[1,]] <- 1

season_begin <- aggregate(Cal_Week~Season,JE_games,FUN=min)
JE_games$Week <- as.numeric(JE_games$Cal_Week)-as.numeric(season_begin$Cal_Week[match(JE_games$Season,season_begin$Season)])+1

JE_games$Win[JE_games$Result=='T'] <- .5
JE_games$Excl_Harbin <- ifelse(substr(JE_sched[,8],nchar(JE_sched[,8]),nchar(JE_sched[,8]))=='*',1,0)

JE_games$Excl_Elo <- 0
JE_games$Excl_Elo[which(JE_games$Notes %in% c('suspended','cancelled','forfeit','double forfeit'))] <- 1

JE_games$Opp_Reg <- substr(JE_sched[,7],regexpr(':',JE_sched[,7])+1,nchar(JE_sched[,7])-1)
JE_games$nonOHSAA <- ifelse(JE_games$Opp_Reg=='' | JE_games$Opp_Reg==0, 1, 0)
JE_games$nonOHSAA_div <- ifelse(JE_games$nonOHSAA==1,substr(JE_sched[,7],2,2),NA)

write.csv(JE_games,'output/JoeEitel GameLog18.csv',row.names=F)
#write.table(data.frame('x'=unique(JE_games$Opp_ID[which(JE_games$nonOHSAA==1)])),'JoeEitel/nonOH.txt',row.names=F)


###
###pull 2018 nonOH team sched on JoeEitel
files <- dir('JoeEitel/2018nonOH',full.name=T) 

JE_sched <- lapply(files, function(z) {
the_tree <- htmlTreeParse(z, useInternal=T)
sched <- sapply(xpathSApply(the_tree, '//table/tbody/tr/td'),function(y) xmlValue(y, trim=T))
opp_ID <- substr(xpathSApply(the_tree, '//table/tbody/tr/td[3]//@href'),18,nchar(xpathSApply(the_tree, '//table/tbody/tr/td[3]//@href'))-10)

if (tail(sched,1)=='# - Ohio playoff game') {sched <- sched[1:(length(sched)-1)]}
if (tail(sched,1)=='* - game does not count in OHSAA rankings') {sched <- sched[1:(length(sched)-1)]}
cbind(2018,substr(z,20,nchar(z)-4),opp_ID,t(matrix(sched,7)),1:(length(sched)/7))
})

JE_sched <- do.call(rbind,JE_sched)
row.names(JE_sched) <- NULL

#create df with all games from JoeEitel
JE_games <- data.frame(Season=JE_sched[,1],Week=JE_sched[,11],Tm_ID=JE_sched[,2],Opp_ID=JE_sched[,3],Loc=JE_sched[,5],Result=JE_sched[,8],Notes=JE_sched[,10],stringsAsFactors=F)
JE_games$Date <- as.Date(paste0(JE_sched[,4],'/',JE_sched[,1]),format='%m/%d/%Y')
JE_games$Playoff <- ifelse(substr(JE_sched[,6],1,1)=='#',1,0)
JE_games$Tm_score <- as.numeric(sapply(strsplit(JE_sched[,9],'-'),function(x) x[1]))
JE_games$Opp_score <- as.numeric(sapply(strsplit(JE_sched[,9],'-'),function(x) x[2]))

JE_games$Score_diff <- JE_games$Tm_score - JE_games$Opp_score
JE_games$Win <- ifelse(substr(JE_games$Result,1,1)=='W',1,0)

JE_games$Cal_Week <- format(JE_games$Date-1,'%W')
tm_cal_wk <- paste0(JE_games$Season,'_',JE_games$Cal_Week,'_',JE_games$Tm_ID)
mult_games_wk <- names(which(table(tm_cal_wk)>1))
JE_games$Multi_Week <- NA
JE_games$Multi_Week[matrix(which(tm_cal_wk %in% mult_games_wk),2)[1,]] <- 1

season_begin <- aggregate(Cal_Week~Season,JE_games,FUN=min)
JE_games$Week <- as.numeric(JE_games$Cal_Week)-as.numeric(season_begin$Cal_Week[match(JE_games$Season,season_begin$Season)])+1

JE_games$Win[JE_games$Result=='T'] <- .5
JE_games$Excl_Harbin <- ifelse(substr(JE_sched[,8],nchar(JE_sched[,8]),nchar(JE_sched[,8]))=='*',1,0)

JE_games$Excl_Elo <- 0
JE_games$Excl_Elo[which(JE_games$Notes %in% c('suspended','cancelled','forfeit','double forfeit'))] <- 1

JE_games$Opp_Reg <- substr(JE_sched[,7],regexpr(':',JE_sched[,7])+1,nchar(JE_sched[,7])-1)
JE_games$nonOHSAA <- ifelse(JE_games$Opp_Reg=='' | JE_games$Opp_Reg==0, 1, 0)
JE_games$nonOHSAA_div <- ifelse(JE_games$nonOHSAA==1,substr(JE_sched[,7],2,2),NA)

write.csv(JE_games,'output/JoeEitel GameLog18 nonOH.csv',row.names=F)

###adding names for non_OH teams
JE_tms <- sapply(files, function(z) {
the_tree <- htmlTreeParse(z, useInternal=T)
sapply(xpathSApply(the_tree, '//body/div[1]//text()'),function(y) xmlValue(y, trim=T))[1:2]
})

nonOH_tms <- data.frame(Season=2018,TeamID=substr(colnames(JE_tms),20,nchar(colnames(JE_tms))-4),stringsAsFactors=F)
nonOH_tms$School <- JE_tms[1,]
nonOH_tms$From <- JE_tms[2,]
nonOH_tms$State <- substr(nonOH_tms$From,nchar(nonOH_tms$From)-1,nchar(nonOH_tms$From))
###

###bring everything together to make sim df
JE_games <- read.csv('output/JoeEitel GameLog18.csv',stringsAsFactors=F)
JE_games_nonOH <- read.csv('output/JoeEitel GameLog18 nonOH.csv',stringsAsFactors=F)
JE_games$Tm_OH <- 1
JE_games_nonOH$Week <- JE_games_nonOH$Week-1
JE_games_nonOH$Tm_OH <- 0
JE_games <- JE_games[which(JE_games$Tm_ID!=9057),]
JE_games_nonOH <- JE_games_nonOH[which(JE_games_nonOH$Tm_ID!=1201),]
JE_games_all <- rbind(JE_games,JE_games_nonOH)
JE_games_all$nonOHSAA[which(JE_games$Opp_ID==1201)] <- 0

OHSAA_df <- read.csv('output/OHSAA ALL.csv',stringsAsFactors=F)
nonOH_div <- read.csv('output/nonOH_div.csv',stringsAsFactors=F)

tm_yr <- as.numeric(sapply(strsplit(nonOH_div$X,'_'),function(x) x[1]))
tm_id <- sapply(strsplit(nonOH_div$X,'_'),function(x) x[2]) 
most_recent <- aggregate(tm_yr~tm_id,FUN=max)

OHSAA_df$Div.2018[match(JE_games_all$Tm_ID,OHSAA_df$OHSAA.ID)]

non_OHSAA18 <- data.frame(row.names=sort(unique(JE_games_all$Opp_ID[which(JE_games_all$nonOHSAA==1)])))
non_OHSAA18$OHSAA_gm <- ifelse(is.na(match(row.names(non_OHSAA18),JE_games_all$Tm_ID)),0,1)
non_OHSAA18$JE_name <- nonOH_tms$School[match(row.names(non_OHSAA18),nonOH_tms$TeamID)]
non_OHSAA18$ST <- nonOH_tms$State[match(row.names(non_OHSAA18),nonOH_tms$TeamID)]
non_OHSAA18$JE_div <- JE_games_all$nonOHSAA_div[match(row.names(non_OHSAA18),JE_games_all$Opp_ID)]
old_id <- paste0(most_recent$tm_yr[match(row.names(non_OHSAA18),most_recent$tm_id)],'_',row.names(non_OHSAA18))
non_OHSAA18$last_div<- nonOH_div$nonOH_div[match(old_id,nonOH_div$X)]

non_OHSAA18$guess_div <- NA
non_OHSAA18$guess_div[which(row.names(non_OHSAA18)=='9916')] <- 6 #KIPP
non_OHSAA18$guess_div[which(row.names(non_OHSAA18)=='9918')] <- 2 #COF

remaining_div_pick <- row.names(non_OHSAA18)[which(non_OHSAA18$OHSAA_gm==1 & is.na(non_OHSAA18$last_div) & is.na(non_OHSAA18$JE_div) & is.na(non_OHSAA18$guess_div))]
unknown_div_opp <- sapply(remaining_div_pick,function(x) JE_games_all$Tm_ID[which(JE_games_all$Opp_ID==x & JE_games_all$Tm_OH==1)])
non_OHSAA18$guess_div[match(names(unknown_div_opp),row.names(non_OHSAA18))] <- OHSAA_df$Div.2018[match(unknown_div_opp,OHSAA_df$OHSAA.ID)]
non_OHSAA18$use_div <- ifelse(is.na(non_OHSAA18$JE_div),ifelse(is.na(non_OHSAA18$last_div),non_OHSAA18$guess_div,non_OHSAA18$last_div),non_OHSAA18$JE_div)

JE_games_all$opp_div <- OHSAA_df$Div.2018[match(JE_games_all$Opp_ID,OHSAA_df$OHSAA.ID)]
JE_games_all$nonOHSAA_div <- non_OHSAA18$use_div[match(JE_games_all$Opp_ID,row.names(non_OHSAA18))]









####pulling 2018 schedules from MaxPreps
sched_order <- read.table('MaxPreps/2018 sched.txt',stringsAsFactors=F)
OHSAA_df <- read.csv('output/OHSAA ALL.csv',stringsAsFactors=F)

all_sched <- lapply(dir('MaxPreps/Sched18',full.names=T), function(x) {
the_tree <- htmlTreeParse(x, useInternal=T)
tm <- sched_order$V1[as.numeric(substr(x,18,nchar(x)-4))]
sched <- sapply(xpathSApply(the_tree, '//div[@class="contest-name"]'),function(y) xmlValue(y, trim=T))
opp <- sapply(strsplit(sched,' \r\n'),function(j) rev(j)[1])
home_away <- ifelse(sapply(strsplit(sched,' \r\n'),length)==1,'H','A')
ugly_link <- NA
ugly_link[which(opp!='TBA')] <- xpathSApply(the_tree, '//div[@class="contest-name"]//@href')

gm_link <- NA
gm_link[which(opp!='TBA')] <- xpathSApply(the_tree, '//td[@class="result last"]/a[1]/@href')

class_names <- xpathSApply(the_tree, '//td[@class="contestdetails"]/div/@class')
rolling_gm_cnt <- sapply(1:length(class_names),function(z) length(which(class_names[1:z]=='contest-name')))
all_loc <- rep(NA,length(home_away))

loc <- gsub('Location: ','',sapply(xpathSApply(the_tree, '//div[@class="contest-location"]'),function(y) xmlValue(y, trim=T)))
all_loc[rolling_gm_cnt[which(class_names=='contest-location')]] <- loc

gm_date <- sapply(xpathSApply(the_tree, '//td[@class="contestdate first"]/abbr[1]'),function(y) xmlValue(y, trim=T))
time <- sapply(xpathSApply(the_tree, '//td[@class="contestdate first"]/abbr[2]'),function(y) xmlValue(y, trim=T))

data.frame(tm,opp,'home'=home_away,all_loc,'date'=gm_date,time,ugly_link,gm_link,stringsAsFactors=F)
})

all_oh_gms18 <- do.call(rbind,all_sched)
all_oh_gms18$match_game <- match(paste0(all_oh_gms18$gm_link,all_oh_gms18$home),paste0(all_oh_gms18$gm_link,ifelse(all_oh_gms18$home=='H','A','H')))


all_oh_gms18$Tm_ID <- OHSAA_df$OHSAA.ID[match(all_oh_gms18$tm,OHSAA_df$MaxPreps)]
all_oh_gms18$Opp_ID <- all_oh_gms18$Tm_ID[all_oh_gms18$match_game]

all_oh_gms18$mxp_ID <- substr(all_oh_gms18$ugly_link,36,71)

mxp_df <- read.csv('output/max preps ID.csv',stringsAsFactors=F)
all_oh_gms18$Opp_ID2 <- mxp_df$Tm_ID[match(all_oh_gms18$mxp_ID,mxp_df$mxp_ID)]
all_oh_gms18$Opp_ID <- ifelse(is.na(all_oh_gms18$Opp_ID),all_oh_gms18$Opp_ID2,all_oh_gms18$Opp_ID)

all_oh_gms18$Opp_ID[which(all_oh_gms18$opp=='Stryker (Stryker, OH)')] <- 1502

all_oh_gms18 <- all_oh_gms18[which(all_oh_gms18$opp!='Fayette Christian (Washington Courthouse, OH)'),]
all_oh_gms18 <- all_oh_gms18[which(all_oh_gms18$opp!='Fairfield (Leesburg, OH)'),]
all_oh_gms18 <- all_oh_gms18[which(all_oh_gms18$opp!='Franklin Monroe (Pitsburg, OH)'),]
all_oh_gms18 <- all_oh_gms18[which(all_oh_gms18$opp!='TBA'),]

all_oh_gms18$date_fm <- as.Date(paste0(all_oh_gms18$date,'/2018'),format='%m/%d/%Y')
all_oh_gms18$Cal_Week <- format(all_oh_gms18$date_fm,'%W')

all_oh_gms18 <- all_oh_gms18[which(all_oh_gms18$Cal_Week<=43 & all_oh_gms18$Cal_Week>=34),]
all_oh_gms18$Week <- as.numeric(all_oh_gms18$Cal_Week)-33
all_oh_gms18$Gm_ID <- paste0(all_oh_gms18$Tm_ID,'_',all_oh_gms18$Week)

export_tbl <- all_oh_gms18[,c('Week','tm','opp','home','all_loc','date','time','Tm_ID','Opp_ID','Gm_ID')]
write.csv(export_tbl,'output/gamelogOH 2018.csv',row.names=F)


####pulling 2017 & earlier schedules from MaxPreps

hist_sched <- lapply(dir('MaxPreps/School Page',full.names=T), function(x) {
the_tree <- htmlTreeParse(x, useInternal=T)
tm <- substr(x,22,nchar(x)-4)
sched <- sapply(xpathSApply(the_tree, '//div[@class="contest-name"]'),function(y) xmlValue(y, trim=T))
if (length(sched)!=0) {
opp <- sapply(strsplit(sched,' \r\n'),function(j) rev(j)[1])
home_away <- ifelse(sapply(strsplit(sched,' \r\n'),length)==1,'H','A')
ugly_link <- rep(NA,length(home_away))
ugly_link[which(opp!='TBA')] <- xpathSApply(the_tree, '//div[@class="contest-name"]//@href')

gm_link <- rep(NA,length(home_away))
gm_link[which(opp!='TBA')] <- xpathSApply(the_tree, '//td[@class="result last"]/a[1]/@href')

class_names <- xpathSApply(the_tree, '//td[@class="contestdetails"]//div/@class')
rolling_gm_cnt <- sapply(1:length(class_names),function(z) length(which(class_names[1:z]=='contest-name')))
all_loc <- rep(NA,length(home_away))

loc <- gsub('Location: ','',sapply(xpathSApply(the_tree, '//div[@class="contest-location"]'),function(y) xmlValue(y, trim=T)))
all_loc[rolling_gm_cnt[which(class_names=='contest-location')]] <- loc

gm_date <- sapply(xpathSApply(the_tree, '//td[@class="contestdate first"]/abbr[1]'),function(y) xmlValue(y, trim=T))
time <- sapply(xpathSApply(the_tree, '//td[@class="contestdate first"]/abbr[2]'),function(y) xmlValue(y, trim=T))

return(data.frame(tm,opp,'home'=home_away,all_loc,'date'=gm_date,time,ugly_link,gm_link,stringsAsFactors=F))
}
})

all_yr <- ifelse(is.na(as.numeric(substr(all_oh_gms$tm,nchar(all_oh_gms$tm)-1,nchar(all_oh_gms$tm)))),as.numeric(substr(all_oh_gms$tm,nchar(all_oh_gms$tm),nchar(all_oh_gms$tm))),as.numeric(substr(all_oh_gms$tm,nchar(all_oh_gms$tm)-1,nchar(all_oh_gms$tm))))

all_oh_gms <- do.call(rbind,hist_sched)
all_oh_gms$match_game <- match(paste0(all_oh_gms$gm_link,all_oh_gms$home),paste0(all_oh_gms$gm_link,ifelse(all_oh_gms$home=='H','A','H')))
all_oh_gms$Tm_ID <- OHSAA_df$OHSAA.ID[match(substr(all_oh_gms$tm,1,nchar(all_oh_gms$tm)-nchar(all_yr)-1),OHSAA_df$MaxPreps)]
all_oh_gms$Opp_ID <- all_oh_gms$Tm_ID[all_oh_gms$match_game]

all_oh_gms$mxp_ID <- substr(all_oh_gms$ugly_link,36,71)


JE_games <- read.csv('output/JoeEitel GameLog.csv',stringsAsFactors=F)

JE_games_ID <- paste0(JE_games$Tm_ID,'_',as.numeric(substr(JE_games$Date,6,7)),'/',as.numeric(substr(JE_games$Date,9,10)),'/',JE_games$Season)
all_oh_gms_ID <- paste0(all_oh_gms$Tm_ID,'_',all_oh_gms$date,'/',all_yr+2000)

all_oh_gms$Opp_ID_mat <- JE_games$Opp_ID[match(all_oh_gms_ID,JE_games_ID)]
all_oh_gms$Opp_ID <- ifelse(is.na(all_oh_gms$Opp_ID),all_oh_gms$Opp_ID_mat,all_oh_gms$Opp_ID)
all_oh_gms$Opp_ID[which(all_oh_gms$opp=='TBA')] <- NA


table(is.na(all_oh_gms$Opp_ID_mat))
all_oh_gms[which((is.na(all_oh_gms$Opp_ID) & is.na(all_oh_gms$Opp_ID_mat)))[2],]


mxp_df <- data.frame('Tm_ID'=unique(all_oh_gms$Opp_ID[which(!is.na(all_oh_gms$Opp_ID))]),stringsAsFactors=F)
mxp_df$mxp_ID <- all_oh_gms$mxp_ID[match(mxp_df$Tm_ID,all_oh_gms$Opp_ID)]
write.csv(mxp_df,'output/max preps ID.csv',row.names=F)


ifelse(all_oh_gms$Tm_ID>all_oh_gms$Opp_ID,paste0(all_oh_gms$Tm_ID,'_',all_oh_gms$Opp_ID),paste0(all_oh_gms$Opp_ID,'_'all_oh_gms$Tm_ID)

############OH Helm Links

#can be from any page on OH_Helmets
title <- xpathSApply(the_tree, '//ul/li/a/@title')
link <- xpathSApply(the_tree, '//ul/li/a/@href')
OH_helm_df <- data.frame(title,link,stringsAsFactors=F)

files1 <- dir('OH Helmets/main',full.name=T)
files2 <- dir('OH Helmets/second',full.name=T)

OH_pgs1 <- lapply(files1, function(z) {
the_tree <- htmlTreeParse(z, useInternal=T)
pg_title <- sapply(xpathSApply(the_tree, '//title/text()'),function(y) xmlValue(y, trim=T))
tm_id <- substr(z,17,nchar(z)-4)
c(tm_id,pg_title)
})

OH_pgs2 <- lapply(files2, function(z) {
the_tree <- htmlTreeParse(z, useInternal=T)
pg_title <- sapply(xpathSApply(the_tree, '//title/text()'),function(y) xmlValue(y, trim=T))
tm_id <- substr(z,19,nchar(z)-4)
c(tm_id,pg_title)
})

OH_pgs <- data.frame(rbind(do.call(rbind,OH_pgs1),do.call(rbind,OH_pgs2)),stringsAsFactors=F)
names(OH_pgs) <- c('TeamID','Title')
OH_pgs$actual_title <- sapply(strsplit(OH_pgs$Title,' - '),function(x) x[2])
OH_pgs$link <- OH_helm_df$link[match(OH_pgs$actual_title,OH_helm_df$title)]
OH_pgs$Helm_ID <- substr(OH_pgs$link,31,nchar(OH_pgs$link))


OHSAA_df <- read.csv('output/OHSAA ALL.csv',stringsAsFactors=F)
mxp_df <- read.csv('output/max preps ID.csv',stringsAsFactors=F)

OHSAA_df$oh_helm <- OH_pgs$Helm_ID[match(OHSAA_df$OHSAA.ID,OH_pgs$TeamID)]
OHSAA_df$maxpreps_ugly <- mxp_df$mxp_ID[match(OHSAA_df$OHSAA.ID,mxp_df$Tm_ID)]
###


###extract helmets from each page
setwd('C:/Users/Owner/Desktop/SWAER/helmets')
OHSAA_df <- read.csv('C:/Users/Owner/Documents/GitHub/SWAER/data sets/OHSAA ALL.csv',stringsAsFactors=F)
files <- dir('main_page',full.name=T)

all_oh_helm <- lapply(files, function(z) {
the_tree <- htmlTreeParse(z, useInternal=T)
helmets <- matrix(xpathSApply(the_tree, '//div[@class="webs-parent webs-parent-2 webs-row"]//img/@src'),nrow=2)
yr_head <- matrix(sapply(xpathSApply(the_tree, '//div[@class="webs-text "]'),function(y) xmlValue(y, trim=T)),nrow=2)[2,]
t(rbind(substr(z,11,nchar(z)-4),yr_head,helmets,1:length(yr_head)))
})

oh_helm_df <- do.call(rbind,all_oh_helm)
helm_R <- oh_helm_df[which(oh_helm_df[,5]==1),4]
helm_L <- oh_helm_df[which(oh_helm_df[,5]==1),3]
tm_helmID <- oh_helm_df[which(oh_helm_df[,5]==1),1]
tm_OHID <- OHSAA_df$OHSAA.ID[match(tm_helmID,OHSAA_df$oh_helm)]
full_R <- c(matrix(c(helm_R,paste0(tm_OHID,'_R')),2,byrow=T))
full_L <- c(matrix(c(helm_L,paste0(tm_OHID,'_L')),2,byrow=T))
write.table(c(full_R,full_L),'oh_helm_pics.txt',row.names=F,quote=F,col.names=F)

table(substr(helm_R,nchar(helm_R)-4,nchar(helm_R)))

#####



#####stadium pages
all_stadium <- sapply(dir('Stadium/pages',full.names=T), function(x) {
the_tree <- htmlTreeParse(x, useInternal=T)
home_teams <- sapply(xpathSApply(the_tree, '//div[@class="col-lg-4"]//text()'),function(y) xmlValue(y, trim=T))
home_teams <- p(home_teams[-c(1,which(home_teams==''))])
address <- sapply(xpathSApply(the_tree, '//div[@class="col-lg-12"]//text()'),function(y) xmlValue(y, trim=T))[4]
other <- sapply(xpathSApply(the_tree, '//div[@class="col-lg-6"]//text()'),function(y) xmlValue(y, trim=T))[c(3,7,24)]
st_name <- sapply(xpathSApply(the_tree, '//div[@class="col-xs-12"]//text()'),function(y) xmlValue(y, trim=T))[2]
fld_name <- sapply(xpathSApply(the_tree, '//div[@class="col-xs-12"]//text()'),function(y) xmlValue(y, trim=T))[4]
c(st_name,fld_name,address,other,home_teams)
})

write.csv(t(all_stadium),'output/stadiums.csv')



xpathSApply(the_tree, '//div[@class="col-sm-12 text-left"]//text()')
text_vector <- sapply(xpathSApply(the_tree, '//div[@class="displaySection"]//text()'),function(y) xmlValue(y, trim=T))
text_vector





######OHSAA PAGES
all_pages <- lapply(dir('OHSAA Page/School Page',full.names=T), function(x) {
#x <- dir('OHSAA Page/School Page',full.names=T)[1]
the_tree <- htmlTreeParse(x, useInternal=T)
xpathSApply(the_tree, '//div[@class="schoolHeader"]//text()')
text_vector <- sapply(xpathSApply(the_tree, '//div[@class="displaySection"]//text()'),function(y) xmlValue(y, trim=T))
text_vector
})

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

OHSAA_df <- data.frame(t(sapply(all_pages, function(x) x[2:4])),stringsAsFactors=F)
names(OHSAA_df) <- c('School','Address','Address2')
OHSAA_df$City <- substr(OHSAA_df$Address2,1,regexpr(',',OHSAA_df$Address2)-1)
OHSAA_df$Zip <- trim(substr(OHSAA_df$Address2,nchar(OHSAA_df$Address2)-9,nchar(OHSAA_df$Address2)))
OHSAA_df$Address2 <- NULL
OHSAA_df$AddressFull <- paste0(OHSAA_df$Address,', ',OHSAA_df$City,', OH ',OHSAA_df$Zip)
row.names(OHSAA_df) <- sapply(all_pages,function(x) x[grep('OHSAA ID:',x)+1])
OHSAA_df$Public <- sapply(all_pages,function(x) x[grep('School Type:',x)+1])
OHSAA_df$County <- sapply(all_pages,function(x) x[grep('County:',x)+1])
OHSAA_df$Dist <- sapply(all_pages,function(x) x[grep('OHSAA District:',x)+1])
OHSAA_df$Name <- sapply(all_pages,function(x) x[grep('OHSAA Tournament Name:',x)+1])
Colors <- sapply(all_pages,function(x) x[grep('School Colors:',x)+2])
OHSAA_df$Color1 <- sapply(strsplit(Colors,'\r\n'),function(x) x[1])
OHSAA_df$Color2 <- trim(sapply(strsplit(Colors,'\r\n'),function(x) x[2]))
OHSAA_df$Color2 <- substr(OHSAA_df$Color2,3,nchar(OHSAA_df$Color2))
OHSAA_df$Color3 <- trim(sapply(strsplit(Colors,'\r\n'),function(x) x[3]))
OHSAA_df$Color3 <- substr(OHSAA_df$Color2,3,nchar(OHSAA_df$Color3))
OHSAA_df$Enrollment <- sapply(strsplit(full_enr,'\r\n'),function(x) x[1])
OHSAA_df$Conf <- sapply(all_pages,function(x) x[grep('Primary Athletic Conference:',x)+1])
OHSAA_df$Mascot <- sapply(all_pages,function(x) x[grep('Boys Mascot',x)+1])
full_enr <- sapply(all_pages,function(x) x[grep('Boys Enrollment:',x)+2])
OHSAA_df$Enrollment <- sapply(strsplit(full_enr,'\r\n'),function(x) x[1])
OHSAA_df$Enr_Class <- trim(sapply(strsplit(full_enr,'\r\n'),function(x) x[2]))

write.csv(OHSAA_df,'output/OHSAA pages.csv')
########

#####all 2017 Conf to JE_games
OHSAA_all <- read.csv('output/OHSAA ALL.csv',stringsAsFactors=F)

JE_games$Tm_OH <- NULL
szn17 <- which(JE_games$Season==2017)
JE_games$Tm_Conf <- NA
JE_games$Opp_Conf <- NA
JE_games$Tm_Conf[szn17] <- OHSAA_all$ConfSub.2017[match(JE_games$Tm_ID[szn17],OHSAA_all$OHSAA.ID)]
JE_games$Opp_Conf[szn17] <- OHSAA_all$ConfSub.2017[match(JE_games$Opp_ID[szn17],OHSAA_all$OHSAA.ID)]
JE_games$Conf_Matchup <- ifelse(JE_games$Opp_Conf==JE_games$Tm_Conf,JE_games$Tm_Conf,NA)

#see how many conf games each team plays
tms_check <- unique(JE_games$Tm_ID[szn17])
conf_gm_cnt <- t(sapply(tms_check,function(x) table(factor(JE_games$Conf_Matchup[which(JE_games$Tm_ID==x & JE_games$Season==2017 & JE_games$Playoff==0)],0:1))))

all_conf <- unique(JE_games$Tm_Conf)[-c(1)]

conf_matchrow <- lapply(all_conf,function(x) which(JE_games$Conf_Matchup==x & JE_games$Playoff==0))
names(conf_matchrow) <- all_conf

conf_gms_played <- lapply(conf_matchrow,function(x) table(JE_games$Tm_ID[x]))
rev(sort(sapply(conf_gms_played,sum)))
diff_amt <- sapply(lapply(conf_gms_played, table),length)
lapply(which(diff_amt!=1), function(x) sort(conf_gms_played[[x]]))


table(JE_games$Opp_Conf[which(JE_games$Tm_ID==1359 & JE_games$Season==2017 & JE_games$Playoff==0)])
JE_games[which(JE_games$Tm_ID==1359 & JE_games$Season==2017 & JE_games$Playoff==0),]

write.csv(JE_games,'output/JoeEitel GameLog.csv',row.names=F)

####conf hist to JE Games
conf_master <- read.csv('output/conf hist.csv',stringsAsFactors=F)
JE_games <- read.csv('output/JoeEitel GameLog.csv',stringsAsFactors=F)
head(JE_games)
conf_list <- lapply(1:nrow(conf_master),function(x) cbind(Tm_ID=conf_master[x,1],t(conf_master[x,2:12])))
conf_df <- do.call(rbind,conf_list)
seas <- substr(row.names(conf_df),2,5)
conf_df <- data.frame(Season=seas,conf_df,row.names=NULL,stringsAsFactors=F)
conf_df$YrTm <- paste0(conf_df$Season,'_',conf_df$Tm_ID)

JE_games$Tm_Conf <- conf_df$X1[match(paste0(JE_games$Season,'_',JE_games$Tm_ID),conf_df$YrTm)]
JE_games$Opp_Conf <- conf_df$X1[match(paste0(JE_games$Season,'_',JE_games$Opp_ID),conf_df$YrTm)]
JE_games$Conf_Matchup <- ifelse(JE_games$Opp_Conf==JE_games$Tm_Conf,JE_games$Tm_Conf,NA)


########some stuff for harbin to fix if re-pulling data
JE_games_nonOH$Excl_Harbin[which(JE_games_nonOH$Notes=='cancelled')] <- 1
JE_games_nonOH$Excl_Harbin[which(JE_games_nonOH$Week==0)] <- 1

JE_games_nonOH$Tm_OH <- NULL
JE_games_nonOH$Excl_Harbin[which(JE_games_nonOH$Notes=='forfeit')] <- 0
JE_games_nonOH$Excl_Harbin[which(substr(JE_games_nonOH$Result,nchar(JE_games_nonOH$Result),nchar(JE_games_nonOH$Result))=='*')] <- 1
write.csv(JE_games_nonOH,'output/JoeEitel GameLog NON_OH.csv',row.names=F)

JE_games$Tm_OH <- NULL
JE_games$Excl_Harbin[which(JE_games$Notes=='forfeit')] <- 0
JE_games$Excl_Harbin[which(substr(JE_games$Result,nchar(JE_games$Result),nchar(JE_games$Result))=='*')] <- 1
write.csv(JE_games,'output/JoeEitel GameLog.csv',row.names=F)


####OLD
####pull scores from JoeEitel scores section
all_games <- lapply(dir('JoeEitel/Scores',full.names=T), function(x) {
the_tree <- htmlTreeParse(x, useInternal=T)
text_vector <- sapply(xpathSApply(the_tree, '//text()'),function(y) xmlValue(y, trim=T))
ot_row <- which(substr(text_vector,1,2)=='OT')
cancel <- which(text_vector=='cancelled')
game_mx <- matrix(text_vector[-c(1:5,ot_row,ot_row-1,cancel,cancel-1)],4)
cbind(t(game_mx),x)
})

games <- lapply(all_games, function(z) {
away <- sapply(strsplit(z[,1],'  '),function(x) x[2])
date <- sapply(strsplit(z[,1],'  '),function(x) x[1])
home <- substr(z[,3],4,nchar(z[,3]))
data.frame(date, away, home, away_sc=z[,2],home_sc=z[,4],wkyr=z[,5],stringsAsFactors=F)
})

games <- do.call(rbind, games)
games$home_sc <- as.numeric(games$home_sc)
games$away_sc <- as.numeric(games$away_sc)

all_teams <- names(which(table(c(games$home,games$away))>30))
####


##get standings
files <- dir('JoeEitel/Standings',full.name=T)
files1 <- files[as.numeric(substr(files,23,26))>=2002 & as.numeric(substr(files,23,26))<2016]
files2 <- files[as.numeric(substr(files,23,26))>=2016]

all_teams1 <- lapply(files1, function(x) {
the_tree <- htmlTreeParse(x, useInternal=T)
get_col <- function(col_num) sapply(xpathSApply(the_tree, paste0('//table/tr/td[',col_num,']')),function(y) xmlValue(y, trim=T))[-c(1)]
data.frame(file=x,TeamID=get_col(3),City=get_col(4),School=get_col(5),Rec=get_col(2),stringsAsFactors=F)
})

all_teams2 <- lapply(files2, function(x) {
the_tree <- htmlTreeParse(x, useInternal=T)
get_col <- function(col_num) sapply(xpathSApply(the_tree, paste0('//table/tbody/tr/td[',col_num,']')),function(y) xmlValue(y, trim=T))
data.frame(file=x,TeamID=get_col(3),City=get_col(4),School=get_col(5),Rec=get_col(2),stringsAsFactors=F)
})

all_teams1 <- lapply(all_teams1, function(x) cbind(Seed=row.names(x),x))
all_teams2 <- lapply(all_teams2, function(x) cbind(Seed=row.names(x),x))

teams_df <- rbind(do.call(rbind, all_teams1),do.call(rbind, all_teams2))
teams_df$Yr <- as.numeric(substr(teams_df$file,23,26))
teams_df$Reg <- as.numeric(substr(teams_df$file,32,33))

teams_df$Div <- NA
teams_df$Div[which(teams_df$Yr < 2013 | teams_df$Yr >= 2016)] <- ceiling(teams_df$Reg[which(teams_df$Yr < 2013 | teams_df$Yr >= 2016)]/4)
teams_df$Div[which(teams_df$Yr >= 2013 & teams_df$Yr < 2016)] <- ceiling((teams_df$Reg[which(teams_df$Yr >= 2013 & teams_df$Yr < 2016)]+2)/4)

missing_szn <- names(table(teams_df$TeamID))[which(table(teams_df$TeamID)!=16)]
missing_school <- teams_df$School[match(missing_szn,teams_df$TeamID)]
yrs_missed <- sapply(missing_szn, function(x) {
yr_g <- teams_df$Yr[which(teams_df$TeamID==x)]
c(2002:2017)[which(is.na(match(2002:2017,yr_g)))]
})

teams_df$YrID <- paste0(teams_df$Yr,'_',teams_df$TeamID)
teams_df$Wins <- as.numeric(sapply(strsplit(teams_df$Rec,'-'),function(x) x[1]))
teams_df$Losses <- as.numeric(sapply(strsplit(teams_df$Rec,'-'),function(x) x[2]))
teams_df$file <- NULL
teams_df$Rec <- NULL

#teams_df[which(teams_df$TeamID==9057),]
#write.csv(teams_df,'output/JoeEitel Teams.csv',row.names=F)
#write.table(paste0('http://www.joeeitel.com/hsfoot/teams.jsp?year=',teams_df$Yr,'&teamID=',teams_df$TeamID),'JoeEitel/TeamSeason.txt',quote=F,col.names=F,row.names=F)
##

#######make a full playoff df
some_po_df <- lapply(2007:2017, function(yr) {
if (yr<=2012) {reg_format <- 1:24} else if(yr>=2016) {reg_format <- 1:28} else {reg_format <- 3:26}
playoffs_df <- expand.grid(Fav=c(1,4,2,3),Reg=reg_format) 
playoffs_df$Dog <- 9-playoffs_df$Fav
if (reg_format[1]==3) playoffs_df <- rbind(playoffs_df, data.frame(Fav=rep(c(1,8,4,5,3,6,7,2),2),Reg=c(rep(1,8),rep(2,8)),Dog=rep(c(16,9,13,12,14,11,10,15),2)))
playoffs_df$Yr <- yr
playoffs_df
})

the_po_df <- do.call(rbind,some_po_df)
teams_df$Lookup <- paste0(teams_df$Seed,'_',teams_df$Reg,'_',teams_df$Yr)

the_po_df[,c('FavID','FavID2')] <- teams_df[match(paste0(the_po_df$Fav,'_',the_po_df$Reg,'_',the_po_df$Yr),teams_df$Lookup),c('TeamID','School')]
the_po_df$Fav_elo <- NA
the_po_df[,c('DogID','DogID2')] <- teams_df[match(paste0(the_po_df$Dog,'_',the_po_df$Reg,'_',the_po_df$Yr),teams_df$Lookup),c('TeamID','School')]
the_po_df$Dog_elo <- NA
the_po_df$WeekID <- paste0(the_po_df$Yr,'_11')
the_po_df$DogID[which(the_po_df$WeekID=='2007_11' & the_po_df$Reg==17 & the_po_df$Dog==8)] <- '812'
the_po_df$DogID2[which(the_po_df$WeekID=='2007_11' & the_po_df$Reg==17 & the_po_df$Dog==8)] <- 'Kirtland'
the_po_df$Yr <- NULL


dog <- pred_all$Win[match(paste0(the_po_df$WeekID,'_',the_po_df$DogID),pred_all$GameID)]
fav <- pred_all$Win[match(paste0(the_po_df$WeekID,'_',the_po_df$FavID),pred_all$GameID)]
dog+fav

adv_mx <- ifelse(matrix(fav==1,142,3),as.matrix(the_po_df[,c('FavID','FavID2','Fav_elo')]),as.matrix(the_po_df[,c('DogID','DogID2','Dog_elo')]))
adv_mx2 <- data.frame('Fav'=NA,'Reg'=NA,'Dog'=NA,cbind(adv_mx[seq(1,142,2),],adv_mx[seq(2,142,2),]),stringsAsFactors=F)
names(adv_mx2)[4:9] <- c('FavID','FavID2','Fav_elo','DogID','DogID2','Dog_elo')
year <- matrix(as.numeric(substr(the_po_df$WeekID,1,4)),2)[1,]
adv_mx2$WeekID <- paste0(year,'_15')

##final four re-seed
fin_4 <- cbind(matrix(adv_mx2$FavID,ncol=2,byrow=T),matrix(adv_mx2$DogID,ncol=2,byrow=T))
year4 <- matrix(as.numeric(substr(the_po_df$WeekID,1,4)),4)[1,]
year2 <- matrix(as.numeric(substr(the_po_df$WeekID,1,4)),2)[1,]

tm_4 <- matrix(fin_4,4,byrow=T)
opp_4 <- matrix(pred_all$Opp_ID[match(paste0(year4,'_14_',fin_4),pred_all$GameID)],4,byrow=T)
adv_mx2$FavID <- c(apply(ifelse(tm_4>opp_4,tm_4,opp_4),2,function(x) unique(x)))
adv_mx2$DogID <- pred_all$Opp_ID[match(paste0(year2,'_14_',adv_mx2$FavID),pred_all$GameID)]
adv_mx2$FavID2 <- teams_df$School[match(adv_mx2$FavID,teams_df$TeamID)]
adv_mx2$DogID2 <- teams_df$School[match(adv_mx2$DogID,teams_df$TeamID)]
##


#the_final_po_df <- the_po_df
the_po_df <- adv_mx2
the_final_po_df <- rbind(the_final_po_df,the_po_df)

write.csv(the_final_po_df,'output/Playoffs Bracket.csv',row.names=F)






###
all_Fan50 <- lapply(dir('Fan50/Teams',full.name=T), function(z) {

the_tree <- htmlTreeParse(z, useInternal=T)
text_array <- sapply(xpathSApply(the_tree, '//text()'),function(y) xmlValue(y, trim=T))
headers <- sapply(xpathSApply(the_tree, '//b/text()'),function(y) xmlValue(y, trim=T))
header_gaps <- c(0,match(headers,text_array),length(text_array))
section <- as.character(cut(1:length(text_array),breaks=header_gaps,labels=c('Begin',headers),right=F))
sched <- text_array[which(section=="Schedule and results")[-c(1)]]

#find rows where Drew made a pick
#also find rows where he did not and get first row of each
pick_rows <- which(regexpr(', pick',sched)==1)
non_OH <- which(is.na(match(1:length(sched),unlist(lapply(pick_rows,function(x) (x-2):x)))))
non_OH_top <- c()
non_OH_name <- c()
if (length(non_OH) > 0) {
non_OH_name <- substr(sched[non_OH],sapply(gregexpr(' ',sched[non_OH]),function(x) x[7])+1,nchar(sched[non_OH]))
non_OH_top <- substr(sched[non_OH],1,sapply(gregexpr(' ',sched[non_OH]),function(x) x[7])-1)
}
top_line <- sort(c(pick_rows-2,non_OH))
non_OH_wk <- match(non_OH,top_line)

#put together reg szn
reg_szn <- cbind(sched[top_line],sched[top_line+1],sched[top_line+2])
reg_szn[non_OH_wk,1] <- non_OH_top
reg_szn[non_OH_wk,2] <- non_OH_name
reg_szn[non_OH_wk,3] <- NA


#missing weeks
weeks_played <- unlist(lapply(strsplit(reg_szn[,1],' '),function(x) x[3]))
reg_szn <- reg_szn[match(1:10,as.numeric(substr(weeks_played,3,nchar(weeks_played)-1))),]

hist_arr <- text_array[which(section=='Weekly ranking & projection history')[-c(1)]]
ply_arr <- matrix(NA,3,5)
if (length(which(substr(section,nchar(section)-7,nchar(section))=='playoffs'))>0) {
my_PO <- matrix(text_array[which(substr(section,nchar(section)-7,nchar(section))=='playoffs')[-c(1)]],3)
ply_arr[,1:ncol(my_PO)] <- my_PO
}
if (length(which(substr(section,nchar(section)-9,nchar(section))=='tournament'))>0) {
my_PO <- matrix(text_array[which(substr(section,nchar(section)-9,nchar(section))=='tournament')[-c(1)]],3)
ply_arr[,4:(ncol(my_PO)+3)] <- my_PO
}

JoeE_ID <- substr(xpathSApply(the_tree, '//@href')[5],59,63)
Fan50_ID <- substr(z,17,19)
cbind(JoeE_ID,Fan50_ID,text_array[7],text_array[length(text_array)],rbind(reg_szn,t(ply_arr)),rev(hist_arr)[1:15],rev(hist_arr)[2:16])
})

all_Fan50 <- do.call(rbind,all_Fan50)

#parse out some things
team_name <- substr(all_Fan50[,3],unlist(lapply(gregexpr(' ',all_Fan50[,3]),function(x) x[1]))+1,unlist(gregexpr('\\(',all_Fan50[,3]))-2)
opp_name <- substr(all_Fan50[,6],1,unlist(lapply(gregexpr('\\(',all_Fan50[,6]),function(x) x[1]))-2)
drew_pick <- substr(all_Fan50[,7],9,9)
drew_pick[which(drew_pick=='e')] <- substr(all_Fan50[which(drew_pick=='e'),7],30,30)
winner_loser <- substr(all_Fan50[,5],unlist(gregexpr(')',all_Fan50[,5]))+2,unlist(gregexpr(')',all_Fan50[,5]))+2)
drew_wfore <- as.numeric(substr(all_Fan50[,7],nchar(all_Fan50[,7])-3,nchar(all_Fan50[,7])-2))/100
drew_fore <- ifelse(drew_pick=='L',1-drew_wfore,drew_wfore)
week <- as.numeric(substr(all_Fan50[,9],2,unlist(gregexpr(':',all_Fan50[,9]))-1))
date <- as.Date(paste0(substr(all_Fan50[,5],1,unlist(lapply(gregexpr('\\(',all_Fan50[,5]),function(x) x[1]))-2),' 2017'),format='%b %d %Y')


end2017_rating <- as.numeric(substr(all_Fan50[,3],unlist(lapply(gregexpr(')',all_Fan50[,3]),function(x) x[1]))+2,nchar(all_Fan50[,3])))
end2016_rating <- as.numeric(substr(all_Fan50[,4],unlist(lapply(gregexpr('year ',all_Fan50[,4]),function(x) x[1]))+5,unlist(lapply(gregexpr('\\(',all_Fan50[,4]),function(x) x[1]))-2))


#take a look a drew's forecasts
drew_df <- data.frame(week,team_name,opp_name,date,
			drew=drew_pick,drew_fore,actual=winner_loser,
			JoeE_ID=all_Fan50[,1],Fan50_ID=all_Fan50[,2],TeamWk=paste0(all_Fan50[,1],'_',week),
			stringsAsFactors=F)
drew_df <- drew_df[which(!is.na(drew_df$drew)),]
drew_df_single <- drew_df[which(drew_df$actual=='W'),]

drew_df$tm_date <- paste0(drew_df$JoeE_ID,'_',drew_df$date)

drew_win_loss <- t(sapply(1:15, function(w) rev(table(drew_df_single$drew[which(drew_df_single$week==w)]))))
cbind(drew_win_loss,apply(drew_win_loss,1,function(x) x[1]/sum(x)))
drew_bin <- ifelse(drew_df$actual=='W',1,0)

sum(drew_win_loss[,1])/sum(drew_win_loss)


calib <- aggregate(data=drew_df,drew_bin~drew_fore,FUN=mean)
-mean(log(drew_df_single$drew_fore))

plot(calib,type='l',col='blue')
lines(cbind(calib[,1],calib[,1]),type='l',col='red')
lines(my_calib,type='l',col='green')


#head(drew_df_single[order(drew_df_single$drew_fore),])
head(all_Fan50)
head(drew_df_single)



###pull team sched on JoeEitel
###9015 & 9057 were missing in 2010 to 2011
files <- dir('JoeEitel/TeamSeason',full.name=T) 

files1 <- files[as.numeric(substr(files,26,29))>=2013]
files2 <- files[as.numeric(substr(files,26,29))< 2013]

JE_sched1 <- lapply(files1, function(z) {
the_tree <- htmlTreeParse(z, useInternal=T)
sched <- sapply(xpathSApply(the_tree, '//table/tbody/tr/td'),function(y) xmlValue(y, trim=T))
opp_ID <- substr(xpathSApply(the_tree, '//table/tbody/tr/td[3]//@href'),18,nchar(xpathSApply(the_tree, '//table/tbody/tr/td[3]//@href'))-10)

if (tail(sched,1)=='# - Ohio playoff game') {sched <- sched[1:(length(sched)-1)]}
if (tail(sched,1)=='* - game does not count in OHSAA rankings') {sched <- sched[1:(length(sched)-1)]}
cbind(substr(z,26,29),substr(z,38,nchar(z)-4),opp_ID,t(matrix(sched,7)),1:(length(sched)/7))
})

JE_sched2 <- lapply(files2, function(z) {
the_tree <- htmlTreeParse(z, useInternal=T)
sched <- sapply(xpathSApply(the_tree, '//table/tr[2]/td[1]/table/tr/td'),function(y) xmlValue(y, trim=T))[-c(1:2)]
opp_ID <- substr(xpathSApply(the_tree, '//table/tr[2]/td[1]/table/tr/td[3]//@href'),18,nchar(xpathSApply(the_tree, '//table/tr[2]/td[1]/table/tr/td[3]//@href'))-10)

if (tail(sched,1)=='# - Ohio playoff game') {sched <- sched[1:(length(sched)-1)]}
if (tail(sched,1)=='* - game does not count in OHSAA rankings') {sched <- sched[1:(length(sched)-1)]}
cbind(substr(z,26,29),substr(z,38,nchar(z)-4),opp_ID,t(matrix(sched,7)),1:(length(sched)/7))
})

JE_sched <- rbind(do.call(rbind,JE_sched1),do.call(rbind,JE_sched2))
row.names(JE_sched) <- NULL

#create df with all games from JoeEitel
JE_games <- data.frame(Season=JE_sched[,1],Week=JE_sched[,11],Tm_ID=JE_sched[,2],Opp_ID=JE_sched[,3],Loc=JE_sched[,5],Result=JE_sched[,8],Notes=JE_sched[,10],stringsAsFactors=F)
tm_row <- match(paste0(JE_games$Season,'_',JE_games$Tm_ID),teams_df$YrID)
opp_row <- match(paste0(JE_games$Season,'_',JE_games$Opp_ID),teams_df$YrID)

JE_games$Date <- as.Date(paste0(JE_sched[,4],'/',JE_sched[,1]),format='%m/%d/%Y')
JE_games$Playoff <- ifelse(substr(JE_sched[,6],1,1)=='#',1,0)
JE_games$Tm_score <- as.numeric(sapply(strsplit(JE_sched[,9],'-'),function(x) x[1]))
JE_games$Opp_score <- as.numeric(sapply(strsplit(JE_sched[,9],'-'),function(x) x[2]))
JE_games$Opp_Div <- teams_df$Div[opp_row]
JE_games$Score_diff <- JE_games$Tm_score - JE_games$Opp_score
JE_games$Win <- ifelse(substr(JE_games$Result,1,1)=='W',1,0)

JE_games$Cal_Week <- format(JE_games$Date-1,'%W')
tm_cal_wk <- paste0(JE_games$Season,'_',JE_games$Cal_Week,'_',JE_games$Tm_ID)
mult_games_wk <- names(which(table(tm_cal_wk)>1))
JE_games$Multi_Week <- NA
JE_games$Multi_Week[matrix(which(tm_cal_wk %in% mult_games_wk),2)[1,]] <- 1

season_begin <- aggregate(Cal_Week~Season,JE_games,FUN=min)
JE_games$Week <- as.numeric(JE_games$Cal_Week)-as.numeric(season_begin$Cal_Week[match(JE_games$Season,season_begin$Season)])+1

JE_games$Win[JE_games$Result=='T'] <- .5
JE_games$Excl_Harbin <- ifelse(substr(JE_sched[,8],nchar(JE_sched[,8]),nchar(JE_sched[,8]))=='*',1,0)

JE_games$Excl_Elo <- 0
JE_games$Excl_Elo[which(JE_games$Notes %in% c('suspended','cancelled','forfeit','double forfeit'))] <- 1

JE_games$Opp_Reg <- substr(JE_sched[,7],regexpr(':',JE_sched[,7])+1,nchar(JE_sched[,7])-1)
JE_games$nonOHSAA <- ifelse(JE_games$Opp_Reg=='' | JE_games$Opp_Reg==0, 1, 0)
JE_games$nonOHSAA_div <- ifelse(JE_games$nonOHSAA==1,substr(JE_sched[,7],2,2),NA)
#JE_games$Excl_Elo[which(JE_games$nonOHSAA==1)] <- 1

#JE_games[which(JE_sched[,7]=='' & JE_games$Excl_Harbin==0),]

write.csv(JE_games,'output/JoeEitel GameLog.csv',row.names=F)


###for teams where no Div is given
###this only would happen if div was not ultimatley needed
teams_df <- read.csv('output/JoeEitel Teams.csv',stringsAsFactors=F)
JE_games <- read.csv('output/JoeEitel GameLog.csv',stringsAsFactors=F)
div_not_marked <- JE_games[which(is.na(JE_games$nonOHSAA_div) & JE_games$nonOHSAA==1 & JE_games$Excl_Harbin==0),c('Season','Tm_ID')]
div_to_mark <- teams_df$Div[match(paste0(div_not_marked$Season,'_',div_not_marked$Tm_ID),teams_df$YrID)]
JE_games$nonOHSAA_div[which(is.na(JE_games$nonOHSAA_div) & JE_games$nonOHSAA==1 & JE_games$Excl_Harbin==0)] <- div_to_mark


JE_games_OH <- read.csv('output/JoeEitel GameLog.csv',stringsAsFactors=F)

opp_id <- paste0(JE_games_OH$Season,'_',JE_games_OH$Opp_ID)
nonOH_id <- unique(opp_id[which(JE_games_OH$nonOHSAA==1)])
nonOH_div <- sapply(nonOH_id, function(x) JE_games_OH$nonOHSAA_div[which(opp_id==x)[1]])


###pull in nonOHSAA team sched on JoeEitel
files <- dir('JoeEitel/TeamSeason_nonOH',full.name=T) 

files1 <- files[as.numeric(substr(files,32,35))>=2013]
files2 <- files[as.numeric(substr(files,32,35))< 2013]

JE_sched1 <- lapply(files1, function(z) {
the_tree <- htmlTreeParse(z, useInternal=T)
sched <- sapply(xpathSApply(the_tree, '//table/tbody/tr/td'),function(y) xmlValue(y, trim=T))
opp_ID <- substr(xpathSApply(the_tree, '//table/tbody/tr/td[3]//@href'),18,nchar(xpathSApply(the_tree, '//table/tbody/tr/td[3]//@href'))-10)

if (tail(sched,1)=='# - Ohio playoff game') {sched <- sched[1:(length(sched)-1)]}
if (tail(sched,1)=='* - game does not count in OHSAA rankings') {sched <- sched[1:(length(sched)-1)]}
cbind(substr(z,32,35),substr(z,44,nchar(z)-4),opp_ID,t(matrix(sched,7)),1:(length(sched)/7))
})

JE_sched2 <- lapply(files2, function(z) {
the_tree <- htmlTreeParse(z, useInternal=T)
sched <- sapply(xpathSApply(the_tree, '//table/tr[2]/td[1]/table/tr/td'),function(y) xmlValue(y, trim=T))[-c(1:2)]
opp_ID <- substr(xpathSApply(the_tree, '//table/tr[2]/td[1]/table/tr/td[3]//@href'),18,nchar(xpathSApply(the_tree, '//table/tr[2]/td[1]/table/tr/td[3]//@href'))-10)

if (tail(sched,1)=='# - Ohio playoff game') {sched <- sched[1:(length(sched)-1)]}
if (tail(sched,1)=='* - game does not count in OHSAA rankings') {sched <- sched[1:(length(sched)-1)]}
cbind(substr(z,32,35),substr(z,44,nchar(z)-4),opp_ID,t(matrix(sched,7)),1:(length(sched)/7))
})

JE_sched <- rbind(do.call(rbind,JE_sched1),do.call(rbind,JE_sched2))
row.names(JE_sched) <- NULL


#create df with all games from JoeEitel
JE_games <- data.frame(Season=JE_sched[,1],Week=JE_sched[,11],Tm_ID=JE_sched[,2],Opp_ID=JE_sched[,3],Loc=JE_sched[,5],Result=JE_sched[,8],Notes=JE_sched[,10],stringsAsFactors=F)
tm_row <- match(paste0(JE_games$Season,'_',JE_games$Tm_ID),names(nonOH_div))

JE_games$Date <- as.Date(paste0(JE_sched[,4],'/',JE_sched[,1]),format='%m/%d/%Y')
JE_games$Playoff <- 0
JE_games$Tm_score <- as.numeric(sapply(strsplit(JE_sched[,9],'-'),function(x) x[1]))
JE_games$Opp_score <- as.numeric(sapply(strsplit(JE_sched[,9],'-'),function(x) x[2]))
JE_games$Opp_Div <- nonOH_div[paste0(JE_games$Season,'_',JE_games$Tm_ID)]
JE_games$Score_diff <- JE_games$Tm_score - JE_games$Opp_score
JE_games$Win <- ifelse(substr(JE_games$Result,1,1)=='W',1,0)

JE_games$Cal_Week <- format(JE_games$Date-1,'%W')
tm_cal_wk <- paste0(JE_games$Season,'_',JE_games$Cal_Week,'_',JE_games$Tm_ID)
mult_games_wk <- names(which(table(tm_cal_wk)>1))
JE_games$Multi_Week <- NA
JE_games$Multi_Week[matrix(which(tm_cal_wk %in% mult_games_wk),2)[1,]] <- 1

season_begin <- aggregate(Cal_Week~Season,JE_games_OH,FUN=min)
JE_games$Week <- as.numeric(JE_games$Cal_Week)-as.numeric(season_begin$Cal_Week[match(JE_games$Season,season_begin$Season)])+1

JE_games$Win[JE_games$Result=='T'] <- .5
JE_games$Excl_Harbin <- ifelse(substr(JE_sched[,8],nchar(JE_sched[,8]),nchar(JE_sched[,8]))=='*',1,0)
JE_games$Excl_Harbin[which(JE_games$Week > 10)] <- 1

JE_games$Excl_Elo <- 0
JE_games$Excl_Elo[which(JE_games$Notes %in% c('suspended','cancelled','forfeit','double forfeit'))] <- 1

JE_games$Opp_Reg <- substr(JE_sched[,7],regexpr(':',JE_sched[,7])+1,nchar(JE_sched[,7])-1)
JE_games$nonOHSAA <- ifelse(JE_games$Opp_Reg=='' | JE_games$Opp_Reg==0, 1, 0)
JE_games$nonOHSAA_div <- ifelse(JE_games$nonOHSAA==1,substr(JE_sched[,7],2,2),NA)
JE_games$nonOHSAA_div[which(JE_games$nonOHSAA_div=='')] <- JE_games$Opp_Div[which(JE_games$nonOHSAA_div=='')]

JE_games[,c('Tm_lon','Tm_lat','Opp_lon','Opp_lat','trav_est')] <- NA

head(JE_games)

write.csv(JE_games,'output/JoeEitel GameLog NON_OH.csv',row.names=F)
#write.csv(data.frame(nonOH_div),'output/nonOH_div.csv')
#######

###adding names for non_OH teams
JE_tms1 <- sapply(files1, function(z) {
the_tree <- htmlTreeParse(z, useInternal=T)
sapply(xpathSApply(the_tree, '//body/div[1]//text()'),function(y) xmlValue(y, trim=T))[1:2]
})

JE_tms2 <- sapply(files2, function(z) {
the_tree <- htmlTreeParse(z, useInternal=T)
sapply(xpathSApply(the_tree, '//table[1]//text()'),function(y) xmlValue(y, trim=T))[1:2]
})

JE_tms <- cbind(JE_tms1,JE_tms2)
nonOH_tms <- data.frame(Season=substr(colnames(JE_tms),32,35),TeamID=substr(colnames(JE_tms),44,nchar(colnames(JE_tms))-4),stringsAsFactors=F)
nonOH_tms$School <- JE_tms[1,]
nonOH_tms$From <- JE_tms[2,]
nonOH_tms$State <- substr(nonOH_tms$From,nchar(nonOH_tms$From)-1,nchar(nonOH_tms$From))


ST <- nonOH_tms$State[match(names(nonOH_div),paste0(nonOH_tms$Season,'_',nonOH_tms$TeamID))]
nonOH_div <- data.frame(X=names(nonOH_div),nonOH_div,ST,stringsAsFactors=F)
nonOH_div$School <- nonOH_tms$School[match(nonOH_div$X,paste0(nonOH_tms$Season,'_',nonOH_tms$TeamID))]
write.csv(nonOH_div,'output/nonOH_div.csv',row.names=F)
###



#####re-pull non OH teams, missing some
head(JE_games_OH)
Non_OH <- unique(JE_games_OH[which(JE_games_OH$nonOHSAA==1),c('Season','Opp_ID')])
write.table(paste0('http://www.joeeitel.com/hsfoot/teams.jsp?year=',Non_OH$Season,'&teamID=',Non_OH$Opp_ID),'JoeEitel/TeamSeason_nonOH2.txt',quote=F,col.names=F,row.names=F)
#####

#####updating with more accurrate location
OHSAA_df <- read.csv('output/OHSAA pages.csv',stringsAsFactors=F,names=T)
row.names(OHSAA_df) <- OHSAA_df$X
OHSAA_df$X <- NULL

JE_games[,c('Tm_lon','Tm_lat')] <- OHSAA_df[paste0(JE_games$Tm_ID),c('lon','lat')]
JE_games[,c('Opp_lon','Opp_lat')] <- OHSAA_df[paste0(JE_games$Opp_ID),c('lon','lat')]
JE_games$trav_est <- sqrt((JE_games$Tm_lon-JE_games$Opp_lon)^2+(JE_games$Tm_lat-JE_games$Opp_lat)^2)*69
write.csv(JE_games,'output/JoeEitel GameLog.csv',row.names=F)

head(JE_games)


head(OHSAA_df)









#########
##get max preps conf
#setwd('C:/Users/A097092/Desktop/Extra/HS Football/MaxPreps/2018 Conf')

team_links <- lapply(dir(), function(x) {
the_tree <- htmlTreeParse(x, useInternal=T)
short_name <- sapply(xpathSApply(the_tree, '//table/tbody/tr/th'),function(y) xmlValue(y, trim=T))
link <- xpathSApply(the_tree, '//table/tbody/tr/th/a/@href')
data.frame(Conf=x,Team=short_name,link,stringsAsFactors=F)
})

team_links <- do.call(rbind,team_links)

#multiple conf
#lapply(names(which(table(team_links$link)!=1)),function(x) team_links[which(team_links$link==x),])

setwd('C:/Users/A097092/Desktop/Extra/HS Football/MaxPreps')
new_sc <- team_links[which(substr(team_links$link,2,6)=='local'),]
write.table(new_sc, 'new_schools.txt',row.names=F,col.names=F,sep=';',quote=F)

all_sc_url <- unique(team_links$link[which(substr(team_links$link,2,6)!='local')])
all_sc_url <- substr(all_sc_url,15,regexpr('/football-',all_sc_url)-1)
write.table(all_sc_url, 'use_schools.txt',row.names=F,col.names=F,sep=';',quote=F)
###

##get school page
setwd('C:/Users/A097092/Desktop/Extra/HS Football/MaxPreps/School Page')

team_info <- lapply(dir(), function(x) {
the_tree <- htmlTreeParse(x, useInternal=T)
page_links <- xpathSApply(the_tree, '//dl//@href')
leagues <- unique(page_links[which(substr(page_links,2,7)=='league')])
other_info <- sapply(xpathSApply(the_tree, '//dd//text()')[1:14],function(y) xmlValue(y, trim=T))
list(leagues,other_info)
})

quick_tm <- t(sapply(team_info,function(x) x[[2]]))
t(quick_tm)

tm_conf <- sapply(team_info,function(x) x[[1]])
dir()[which(sapply(tm_conf,length)!=1)]







########add city loc to JEgames
teams_df <- read.csv('output/JoeEitel Teams.csv',stringsAsFactors=F)
JE_games <- read.csv('output/JoeEitel GameLog.csv',stringsAsFactors=F)
team_loc <- read.csv('output/city loc.csv',stringsAsFactors=F)

teams_df[,c('lon','lat')] <- team_loc[match(teams_df$City,team_loc$City),c('lon','lat')]

JE_games[,c('Tm_lon','Tm_lat')] <- teams_df[match(JE_games$Tm_ID,teams_df$TeamID),c('lon','lat')]
JE_games[,c('Opp_lon','Opp_lat')] <- teams_df[match(JE_games$Opp_ID,teams_df$TeamID),c('lon','lat')]
JE_games$trav_est <- sqrt((JE_games$Tm_lon-JE_games$Opp_lon)^2+(JE_games$Tm_lat-JE_games$Opp_lat)^2)*69
JE_games$trav_est[which(JE_games$Loc=='A')] <- -JE_games$trav_est[which(JE_games$Loc=='A')]
JE_games$trav_est[which(JE_games$Loc=='N')] <- 0

write.csv(JE_games,'output/JoeEitel GameLog.csv',row.names=F)

head(JE_games)

summary(JE_games$trav_est)

teams_df[teams_df$TeamID==490,]






#######helmet lookup
recent_helmets <- sapply(dir('OH Helmets/main',full.names=T), function(x) {
#x <- dir('OH Helmets/main',full.names=T)[2]
the_tree <- htmlTreeParse(x, useInternal=T)
c(substr(x,17,nchar(x)-4),xpathSApply(the_tree, '//img/@src')[2:3])})

write.csv(t(recent_helmets),'output/helmet link2.csv',row.names=F,)

missing_h <- recent_helmets[1,which(is.na(recent_helmets[3,]))]

doubles <- names(table(recent_helmets[3,]))[which(table(recent_helmets[3,])>=2)]
doubles <- recent_helmets[1,which(recent_helmets[3,] %in% doubles)]

write.csv(rbind(cbind(ID=doubles,Type='need_full'),cbind(ID=missing_h,Type='missing')),'output/helmet missing.csv',row.names=F)

