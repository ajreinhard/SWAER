library(XML)
setwd('C:/Users/A097092/Desktop/Extra/HS Football')
bracket <- read.csv('output/Playoffs Bracket.csv',stringsAsFactors=F)
my_playoffs <- read.csv('output/playoffs.csv',stringsAsFactors=F)
my_seeding <- read.csv('output/seeding.csv',stringsAsFactors=F)
my_crystal <- read.csv('output/crystal.csv',stringsAsFactors=F)

my_playoffs <- read.csv('output/playoffs2.csv',stringsAsFactors=F)
my_seeding <- read.csv('output/seeding2.csv',stringsAsFactors=F)


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

head(all_Fan50)

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

tm_curr <- as.numeric(substr(all_Fan50[,8],regexpr(':',all_Fan50[,8])+2,regexpr(' \\(',all_Fan50[,8])))
 
init_pull <- gsub('%','',trimws(substr(all_Fan50[,8],regexpr(')',all_Fan50[,8])+2,regexpr(')',all_Fan50[,8])+4)))
init_pull[which(init_pull=='out')] <- 0
init_pull[which(init_pull=='in')] <- 100
init_pull <- as.numeric(init_pull)/100

home_gm <- gsub('%','',trimws(substr(all_Fan50[,8],regexpr('home',all_Fan50[,8])-4,regexpr('home',all_Fan50[,8])-2)))
home_gm[which(home_gm=='no')] <- 0
home_gm[which(home_gm=='ith')] <- 100
home_gm <- as.numeric(home_gm)/100

home_gm <- gsub('%','',trimws(substr(all_Fan50[,8],regexpr('home',all_Fan50[,8])-4,regexpr('home',all_Fan50[,8])-2)))

proj_seed <- as.numeric(substr(all_Fan50[,8],sapply(gregexpr('#',all_Fan50[,8]),function(x) x[3])+1,sapply(gregexpr('#',all_Fan50[,8]),function(x) x[3])+1))


#take a look a drew's forecasts
drew_df <- data.frame(week,team_name,opp_name,date,
			drew=drew_pick,drew_fore,actual=winner_loser,
			JoeE_ID=all_Fan50[,1],Fan50_ID=all_Fan50[,2],TeamWk=paste0(all_Fan50[,1],'_',week),
			drew_rating=tm_curr, drew_PO=init_pull, drew_hmPO=home_gm, drew_seed=proj_seed,
			stringsAsFactors=F)

drew_df$made_playoffs <- ifelse(!is.na(match(drew_df$JoeE_ID,unique(unlist(bracket[which(bracket$Season==2017),c('FavID','DogID')])))),1,0)
drew_df$home_playoff <- ifelse(!is.na(match(drew_df$JoeE_ID,bracket$FavID[which(bracket$Season==2017 & bracket$Week==11)])),1,0)


####compare median proj
my_crystal$ID <- paste0(my_crystal$Season,'_',my_crystal$TeamID,'_',my_crystal$Week+1)
drew_df$SWAER_seed <- my_crystal$proj_seed[match(paste0('2017_',drew_df$TeamWk),my_crystal$ID)]

drew_df$SWAER_picked_in <- ifelse(drew_df$SWAER_seed<=8 & drew_df$made_playoffs==1,1,0)
drew_df$drew_picked_in <- ifelse(!is.na(drew_df$drew_seed) & drew_df$made_playoffs==1,1,0)

aggregate(cbind(drew_picked_in,SWAER_picked_in)~week,drew_df,sum)
drew_df[which(drew_df$made_playoffs==1 & drew_df$week==1 & drew_df$SWAER_picked_in==1 & is.na(drew_df$drew_picked_in)),]


####home playoff log loss
my_seeding$ID <- paste0(my_seeding$Season,'_',my_seeding$X,'_',my_seeding$Week)
drew_df$SWAER_hmPO <- my_seeding$home[match(paste0('2017_',drew_df$TeamWk),my_seeding$ID)]

drew_df$hm_drew_log_loss <- -log(ifelse(drew_df$home_playoff==1,drew_df$drew_hmPO,1-drew_df$drew_hmPO))
drew_df$hm_SWAER_log_loss <- -log(ifelse(drew_df$home_playoff==1,drew_df$SWAER_hmPO,1-drew_df$SWAER_hmPO))
drew_df$SWAER_hmPO[which(drew_df$hm_SWAER_log_loss==Inf)] <- .99

hm_po_compare_by_wk <- aggregate(cbind(hm_drew_log_loss,hm_SWAER_log_loss)~week,drew_df,mean)

plot(hm_po_compare_by_wk$week,hm_po_compare_by_wk$hm_drew_log_loss,col='blue',type='l')
lines(hm_po_compare_by_wk$week,hm_po_compare_by_wk$hm_SWAER_log_loss,col='green',type='l')
####

####playoff log loss
my_playoffs$ID <- paste0(my_playoffs$Season,'_',my_playoffs$X,'_',my_playoffs$Week)
drew_df$SWAER_PO <- 1-my_playoffs$X0[match(paste0('2017_',drew_df$TeamWk),my_playoffs$ID)]

drew_df$drew_log_loss <- -log(ifelse(drew_df$made_playoffs==1,drew_df$drew_PO,1-drew_df$drew_PO))
drew_df$SWAER_log_loss <- -log(ifelse(drew_df$made_playoffs==1,drew_df$SWAER_PO,1-drew_df$SWAER_PO))
drew_df$SWAER_PO[which(drew_df$SWAER_log_loss==Inf & drew_df$SWAER_PO==1)] <- .999
drew_df$SWAER_PO[which(drew_df$SWAER_log_loss==Inf & drew_df$SWAER_PO==0)] <- .001

po_compare_by_wk <- aggregate(cbind(drew_log_loss,SWAER_log_loss)~week,drew_df,mean)

plot(po_compare_by_wk$week,po_compare_by_wk$drew_log_loss,col='blue',type='l')
lines(po_compare_by_wk$week,po_compare_by_wk$SWAER_log_loss,col='green',type='l')
####

head(drew_df)
po_compare_by_wk2 <- po_compare_by_wk
hm_po_compare_by_wk2 <- hm_po_compare_by_wk


1/(1+10^(--100/400))

(-100/20.7)/qnorm(.36)




####some old stuff

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
