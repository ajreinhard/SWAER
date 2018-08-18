library(png)
setwd('C:/Users/Owner/Documents/GitHub/SWAER')
playoff_res_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/playoffs.csv',stringsAsFactors=F)
crystal_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/crystal.csv',stringsAsFactors=F)
rank_df <- read.csv('C:/Users/Owner/Desktop/SWAER/output/hist rankings.csv',stringsAsFactors=F)
OHSAA_df <- read.csv('data sets/OHSAA ALL.csv',stringsAsFactors=F)

yr <- 2009
wkst <- 11

wk_cryst <- crystal_df[which(crystal_df$Season==yr & crystal_df$Week==wkst-1),]
wk_cryst <- wk_cryst[which(wk_cryst$proj_seed<=8),]

po_sim <- playoff_res_df[match(paste0(wk_cryst$TeamID,'_',wkst,'_',yr),paste0(playoff_res_df$X,'_',playoff_res_df$Week,'_',playoff_res_df$Season)),paste0('X',1:6)]
rank_mat <- rank_df[match(paste0(wk_cryst$TeamID,'_',wkst,'_',yr),paste0(rank_df$Tm_ID,'_',rank_df$Week+1,'_',rank_df$Season)),]

po_sim <- cbind(rank_mat,po_sim)
po_sim <- cbind(wk_cryst,po_sim)
po_sim$reg_quad <- paste0(po_sim$Reg,'_',ifelse(po_sim$proj_seed>=5,9-po_sim$proj_seed,po_sim$proj_seed))
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

po_sim$reg_semi <- paste0(po_sim$Reg,'_',ifelse(is.na(match(po_sim$proj_seed,c(1,4,5,8))),1,2))
placed_quads <- aggregate(placement~reg_semi,po_sim,max)
po_sim$Rnd3[which(po_sim$reg_semi %in% placed_quads[,1])] <- 0
reg_semi_order <- unlist(sapply(1:28, function(x) rank(-po_sim[which(po_sim$Reg==x),'Rnd3'],ties.method='random')))
po_sim$placement[which(reg_semi_order==1)] <- 3

reg_quads <- aggregate(placement~reg_quad,po_sim,max)
po_sim$Rnd2[which(po_sim$reg_quad %in% reg_quads[,1])] <- 0
reg_quad_order <- unlist(sapply(unique(po_sim$reg_quad), function(x) rank(-po_sim[which(po_sim$reg_quad==x),'Rnd2'],ties.method='random')))
po_sim$placement[which(reg_quad_order==1 & po_sim$Rnd2!=0)] <- 2


po_sim[,c('lon','lat','team')] <- OHSAA_df[match(po_sim$TeamID,OHSAA_df$OHSAA.ID),c('lon','lat','Best.Name')]
reg_midp <- aggregate(cbind(lon,lat)~Reg,po_sim,mean,subset= placement>=3)
r_trav_est <- function(r1,r2) sqrt(sum(((reg_midp[r1,2:3]-reg_midp[r2,2:3]) * c(52,69))^2))


trav_order <- function(div) {
reg1 <- (div-1)*4
final_v <- c(r_trav_est(reg1+1,reg1+2) + r_trav_est(reg1+3,reg1+4),r_trav_est(reg1+1,reg1+3) + r_trav_est(reg1+2,reg1+4),r_trav_est(reg1+1,reg1+4) + r_trav_est(reg1+2,reg1+3))
names(final_v) <- c(paste0(reg1+1,'v',reg1+2),paste0(reg1+1,'v',reg1+3),paste0(reg1+1,'v',reg1+4))
sort(final_v)
}


####begin bracket creation
png('bracket2017_pre3.png',width=1600, height=900)
my_div <- 3

reg_in_champ <- po_sim$Reg[which(po_sim$placement>=5 & po_sim$Div==my_div)]
reg_out_champ <- po_sim$Reg[which(po_sim$placement==4 & po_sim$Div==my_div)]
use_mapping <- ifelse(which(names(trav_order(my_div)) %in% c(paste0(reg_in_champ[1],'v',reg_in_champ[2]),paste0(reg_out_champ[1],'v',reg_out_champ[2])))==1,names(trav_order(my_div)[2]),names(trav_order(my_div)[1]))
reg_side <- strsplit(use_mapping,'v')

place_ord <- expand.grid(Seed=c(1,8,4,5,6,3,7,2),Reg=((my_div-1)*4+1):((my_div-1)*4+4))
place_ord$RegSeed <- paste0(place_ord$Reg,'_',place_ord$Seed)
place_ord$mat_row <- match(place_ord$RegSeed,paste0(po_sim$Reg,'_',po_sim$proj_seed))
place_ord$school <- paste0('#',po_sim$DivRank[place_ord$mat_row],' ',po_sim$team[place_ord$mat_row])
place_ord$TeamID <- po_sim$TeamID[place_ord$mat_row]
place_ord$placement <- po_sim$placement[place_ord$mat_row]
place_ord$half <- 1
place_ord$half[which(place_ord$Reg==reg_side[[1]][1] | place_ord$Reg==reg_side[[1]][2])] <- 2
place_ord$Helm <- paste0(place_ord$TeamID,ifelse(place_ord$half==1,'_L','_R'))
txt_size <- 1.1

####create a full bracket df
#dev.new(width=1600, height=900)
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

text(rep(3,16),seq(46.2,1,-3),place_ord$school[which(place_ord$half==1)],adj=c(0,0),cex=txt_size)
text(rep(13,8),seq(44.7,1,-6),place_ord$school[which(place_ord$half==1 & place_ord$placement>=2)],adj=c(0,0),cex=txt_size)
text(rep(23,4),seq(41.7,1,-12),place_ord$school[which(place_ord$half==1 & place_ord$placement>=3)],adj=c(0,0),cex=txt_size)
text(rep(33,2),seq(35.2,1,-24),place_ord$school[which(place_ord$half==1 & place_ord$placement>=4)],adj=c(0,0),cex=txt_size)
text(41,30.2,place_ord$school[which(place_ord$half==1 & place_ord$placement>=5)],adj=c(0,0),cex=txt_size)

for (k in 1:16) rasterImage(readPNG(paste0('helmets/',place_ord$Helm[which(place_ord$half==1)],'.png')[k]),0,seq(46.2,0,-3)[k],2.6,seq(48.2,2,-3)[k])
for (k in 1:8) rasterImage(readPNG(paste0('helmets/',place_ord$Helm[which(place_ord$half==1 & place_ord$placement>=2)],'.png')[k]),10,seq(44.7,0,-6)[k],12.6,seq(46.7,2,-6)[k])
for (k in 1:4) rasterImage(readPNG(paste0('helmets/',place_ord$Helm[which(place_ord$half==1 & place_ord$placement>=3)],'.png')[k]),20,seq(41.7,0,-12)[k],22.6,seq(43.7,2,-12)[k])
for (k in 1:2) rasterImage(readPNG(paste0('helmets/',place_ord$Helm[which(place_ord$half==1 & place_ord$placement>=4)],'.png')[k]),30,seq(35.2,0,-24)[k],32.6,seq(37.2,2,-24)[k])

text(rep(91,16),seq(46.2,1,-3),place_ord$school[which(place_ord$half==2)],adj=c(1,0),cex=txt_size)
text(rep(81,8),seq(44.7,1,-6),place_ord$school[which(place_ord$half==2 & place_ord$placement>=2)],adj=c(1,0),cex=txt_size)
text(rep(71,4),seq(41.7,1,-12),place_ord$school[which(place_ord$half==2 & place_ord$placement>=3)],adj=c(1,0),cex=txt_size)
text(rep(61,2),seq(35.2,1,-24),place_ord$school[which(place_ord$half==2 & place_ord$placement>=4)],adj=c(1,0),cex=txt_size)
text(53,16.2,place_ord$school[which(place_ord$half==2 & place_ord$placement>=5)],adj=c(1,0),cex=txt_size)

for (k in 1:16) rasterImage(readPNG(paste0('helmets/',place_ord$Helm[which(place_ord$half==2)],'.png')[k]),91.4,seq(46.2,0,-3)[k],94,seq(48.2,2,-3)[k])
for (k in 1:8) rasterImage(readPNG(paste0('helmets/',place_ord$Helm[which(place_ord$half==2 & place_ord$placement>=2)],'.png')[k]),81.4,seq(44.7,0,-6)[k],84,seq(46.7,2,-6)[k])
for (k in 1:4) rasterImage(readPNG(paste0('helmets/',place_ord$Helm[which(place_ord$half==2 & place_ord$placement>=3)],'.png')[k]),71.4,seq(41.7,0,-12)[k],74,seq(43.7,2,-12)[k])
for (k in 1:2) rasterImage(readPNG(paste0('helmets/',place_ord$Helm[which(place_ord$half==2 & place_ord$placement>=4)],'.png')[k]),61.4,seq(35.2,0,-24)[k],64,seq(37.2,2,-24)[k])

arrows(41,24,53,24,length=0)
text(47,25,place_ord$school[which(place_ord$placement>=6)],cex=txt_size)
dev.off()




