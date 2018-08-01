setwd('C:/Users/A097092/Desktop/Extra/HS Football')
library(maps)
library(ggplot2)
library(ggmap)


library(maptools)
###map to US house dist
OHSAA_df <- read.csv('output/OHSAA ALL.csv',stringsAsFactors=F)

oh_house <- 'Election/shape/EXPORT AMENDED HOUSE-0928-1322_shp'
us_house <- 'Election/shape/OHCD_2011_REVISED-HB369-AS-PASSED-DEC14_SHP.shp'
oh_senate <- 'Election/shape/senate_amended_0930-2200_shp'

shp_file <- readShapePoly(oh_senate)
gg_mapping <- fortify(shp_file)

grp_by <- gg_mapping$group
long <- unlist(lapply(unique(grp_by), function(x) c(NA,gg_mapping$long[which(grp_by==x)])))[c(-1)]
lat <- unlist(lapply(unique(grp_by), function(x) c(NA,gg_mapping$lat[which(grp_by==x)])))[c(-1)]
dist <- paste0('OH Senate ',ceiling(as.numeric(as.character(unique(grp_by)))))
rng <- c(-84.8,-80.5,38.4,42)
final_map <- list(x=long,y=lat,range=rng,names=dist)
fill_map <- map(final_map,fill=T,plot=F)
oh_senate_loc <- map.where(fill_map,OHSAA_df$lon,OHSAA_df$lat)

fill_map <- map('county',region=c('ohio'),fill=T,plot=F)
oh_county <- map.where(fill_map,OHSAA_df$lon,OHSAA_df$lat)
oh_county <- substr(oh_county,6,nchar(oh_county))
OHSAA_df$County <- oh_county

OHSAA_df[which(!is.na(OHSAA_df$Div.2017) & is.na(oh_senate_loc)),c('lon','lat')]

OHSAA_df$usHOUSE <- us_house_loc
OHSAA_df$ohHOUSE <- oh_house_loc
OHSAA_df$ohSENATE <- oh_senate_loc

write.csv(OHSAA_df,'output/OHSAA ALL.csv',row.names=F)


map(final_map)
points(


###

data(us.cities)
oh.cities <- us.cities[which(us.cities$country.etc=='OH'),]
head(oh.cities)

###get lon lat of all schools, loop back for those missed
teams <- read.csv('output/OHSAA Pages.csv',stringsAsFactors=F)

school_lon_lat <- geocode(teams$AddressFull)

missing <- which(is.na(school_lon_lat$lon))
go_get <- geocode(teams$AddressFull[missing])
school_lon_lat[missing,] <- go_get

write.csv(cbind(teams,school_lon_lat),'output/OHSAA Pages.csv')

###


my_cities <- data.frame(name=unique(teams$City),county.etc='OH',pop=0,lat=city_lon_lat$lat,long=city_lon_lat$lon,capital=0,stringsAsFactors=F)
map('county',region=c('ohio,mercer'))
map.cities(my_cities,label=T)


map('county',region=c('ohio'))
map.cities(oh.cities,label=F)
 
####look up county
cites_lon_lat <- read.csv('output/city loc.csv')
names(cites_lon_lat)[1] <- 'City'
m = map('county','ohio',plot=F,fill=T)
cites_lon_lat$county <- map.where(m,cites_lon_lat$lon,cites_lon_lat$lat)
cites_lon_lat$county <- ifelse(is.na(cites_lon_lat$county),map.where(m,cites_lon_lat$lon,cites_lon_lat$lat+.2),cites_lon_lat$county)
write.csv(cites_lon_lat,'output/city loc.csv',row.names=F)


###identify OB cities
map('county','ohio',fill=F)
points(cites_lon_lat$lon,cites_lon_lat$lat)
points(cites_lon_lat[is.na(cites_lon_lat$county),c('lon','lat')],col='red')

###begin heat map
dots <- expand.grid(lon=seq(-80.5,-84.8,-.05),lat=seq(42.3,38.4,-.05))

city_lon <- matrix(cites_lon_lat$lon,length(cites_lon_lat$lon),length(dots$lon))
dot_lon <- matrix(dots$lon,length(cites_lon_lat$lon),length(dots$lon),byrow=T)
city_lat <- matrix(cites_lon_lat$lat,length(cites_lon_lat$lat),length(dots$lat))
dot_lat <- matrix(dots$lat,length(cites_lon_lat$lat),length(dots$lat),,byrow=T)
miles_from_dot <- sqrt((city_lon-dot_lon)^2 + (city_lat-dot_lat)^2) * 69

avg_div <- aggregate(Current~Div,data=best_att,mean,na.rm=T)[,2]
best_att$Current_by_div <- (best_att$Current-avg_div[best_att$Div])/17.3
best_att$Current_Sp_all <- (best_att$Current-1500)/17.3
best_att$Current_Sp_D5 <- (best_att$Current-mean(best_att$Current[which(best_att$Div==5)]))/17.3

city_avg <- aggregate(Current_by_div~City,data=best_att,mean,na.rm=T)
cites_lon_lat$Avg_Rtg <- city_avg$Current[match(cites_lon_lat$City,city_avg$City)]
city_avg_mx <- matrix(cites_lon_lat$Avg_Rtg,length(cites_lon_lat$lat),length(dots$lat))
city_tms <- aggregate(School~City,data=best_att,length)
cites_lon_lat$Tm_cnt <- city_tms$School[match(cites_lon_lat$City,city_tms$City)]
dots_density <- apply(ifelse(miles_from_dot < 10, 1, 0),2,sum)
dots_avg <- apply(ifelse(miles_from_dot < 10, city_avg_mx, NA),2,mean,na.rm=T)
summary(dots_avg)

city_markers <- rbind(cites_lon_lat[order(-cites_lon_lat$Avg_Rtg)[1:25],c('City','lon','lat')],cites_lon_lat[order(-cites_lon_lat$Tm_cnt)[1:25],c('City','lon','lat')])

dots_avg <- apply(cbind(dots_avg,rep(40,length(dots_avg))),1,min)
dots_avg <- apply(cbind(dots_avg,rep(-40,length(dots_avg))),1,max)

oh <- map_data('county','ohio')
border_states <- map('state',c('kentucky','west virginia','michigan','pennsylvania','indiana'),fill=T,plot=F)
g_lakes <- map('world','Great Lakes',fill=T,plot=F)

ggplot(data=dots) + coord_fixed(xlim=c(-84.8,-80.5),ylim=c(38.4,42.3),ratio=1.3) + theme_nothing(legend=T) +
stat_summary_2d(aes(x=lon,y=lat,z=dots_avg), fun = mean, geom='tile', binwidth=.05,na.rm=T) +
geom_polygon(data=border_states, aes(x=long,y=lat,fill=region,group=group), color='white', fill='white') +
geom_polygon(data=g_lakes, aes(x=long,y=lat,fill=region,group=group), color='white', fill='white') +
geom_polygon(data=oh, aes(x=long,y=lat,fill=region,group=group), color='grey40', fill=NA) +
scale_fill_gradient2(low='blue',mid='grey95',high='red',midpoint=0,name='Spread') +
geom_text(aes(x=-82.7,y=42.2,label='Average result of every school within 10 miles'),size=6) +
geom_text(aes(x=-82.7,y=42,label='vs an average school in their own division'),size=6) +
geom_text(aes(x=-81,y=39,label='*After Wk 15, 2017'),size=4) +
geom_text(data=city_markers,aes(x=lon,y=lat,label=City),size=2,check_overlap=T,fontface='bold')


###end heat map
options(scipen=999)



ggplot(data=cites_lon_lat) + coord_fixed(1.3) +
stat_summary_2d(aes(x=lon,y=lat,z = Avg_Rtg), fun = mean, geom='hex', binwidth=.15,na.rm=T) +
geom_polygon(data=oh, aes(x=long,y=lat,fill=region,group=group), color='black', fill=NA)

scale_fill_gradientn(limits=c(1000,2000), breaks=c(1200,1400,1600,1800), colours=rainbow(3))

geom_point(aes(x=lon,y=lat,color=Avg_Rtg))

?geom_density_2d



map_data('state')

my_map, aes(x=x, y=y)
geom_density_2d() +

smooth.map(oh, my_map)
oh



data(state, package = "datasets")

data(votes.repub)
z = votes.repub[, "1900"]
m = map("state", fill = TRUE, plot = FALSE)
# use a small span to fill in, but not smooth, the data
# increase the resolution to get better results
fit = 
smooth.map(m, z, span = 1/100, merge = TRUE, ave = TRUE)
mat = tapply(fit$z, fit[1:2], mean)
gray.colors <- function(n) gray(rev(0:(n - 1))/n)
par(bg = "blue")
filled.contour(mat, color.palette = gray.colors, nlev = 32, asp = 1)
str(mat)

# another way to visualize:
image(mat, col = gray.colors(100))

###get center of county
min_pt <- aggregate(cbind(long,lat)~subregion,oh,FUN=min)
max_pt <- aggregate(cbind(long,lat)~subregion,oh,FUN=max)
center_pt <- (max_pt[,2:3]+min_pt[,2:3])/2
write.csv(data.frame(county=max_pt[,1],center_pt),'output/county_center.csv',row.names=F)

points(center_pt)
str(oh)

max(oh$lat)
min(oh$lat)


map('county','ohio')

nrow(dots)
points(dots)

###shape file json from 2015?
###https://opendata.arcgis.com/datasets/fca3c03921ee437da4dda8f1079936ca_0.geojson


R_MAP_DATA_DIR

map.where(m,rev(my_cities[which(my_cities$name=="St Marys"),c('lat','long')]))


county.fips

unique(teams$School)

geocode('2323 17th St NW, Canton, OH 44708')
geocode('Canton McKinley High School',override_limit=T)

?geocode
