##==========================1. data preparing============================
#join data
a<- read.csv ("https://www.dropbox.com/s/tef137xkcbcap0h/atlas.csv?raw=1")
b<- read.csv ("https://www.dropbox.com/s/766yez9h07ljner/2011price.csv?raw=1")
d<- read.csv ("https://www.dropbox.com/s/b7r399catw38c73/income.csv?raw=1")

library(plyr)
colnames(b)[1]<-"Codes"
colnames(d)[1]<-"Codes"
a<- join(a, b, by="Codes")
a<- join(a,d, by="Codes")

lsoa_attr <- a[,-c(11,13)]


#define hot market
lsoa_attr$pr_m_2011 <- as.numeric(as.character(lsoa_attr$price_m_2011))
lsoa_attr$ch_price <-lsoa_attr$pr_m_2011-lsoa_attr$price_m_2001
lsoa_attr$hot_market <-ifelse(lsoa_attr$ch_price>111500, 1, 0)
  
#define lowincome group: 1 indicate low income group
lsoa_attr$low_income <-ifelse(lsoa_attr$Median_2011<=0.9*median(lsoa_attr$Median_2011), 1,0)
length(which(lsoa_attr$low_income==1)) #353 lsoa are recognised as low income group. 
#define affordability: affordability = income/housingprice; 1 indicate could afford
lsoa_attr$affor <- lsoa_attr$Median_2011/lsoa_attr$pr_m_2011
#1 indicates the housing price increasing rate is below median speed
lsoa_attr$if_affor <-ifelse(lsoa_attr$pr_m_2011<=median(sort (lsoa_attr$pr_m_2011, decreasing=FALSE)), 1,0)

#define education(1 is low educated), renter (1 meanes lots of renters) and nonwhite (1 means lots of non-white)
lsoa_attr$if_edu <-ifelse(lsoa_attr$qua4_2011<= median(lsoa_attr$qua4_2011), 1,0)
lsoa_attr$if_rent <-ifelse(lsoa_attr$sp_rented2011 > median (lsoa_attr$sp_rented2011), 1,0)
lsoa_attr$if_nonwhite <-ifelse(lsoa_attr$non_white > median (lsoa_attr$non_white), 1,0)

#define vulnerable group, vul=1 means vulnerable

lsoa_attr$vul1 <- ifelse(lsoa_attr$if_affor==1 & lsoa_attr$if_edu==1& lsoa_attr$if_rent==1,1,0)
lsoa_attr$vul2 <- ifelse(lsoa_attr$if_affor==1 & lsoa_attr$if_edu==1& lsoa_attr$if_nonwhite==1,1,0)

lsoa_attr$vul <- ifelse((lsoa_attr$vul1 + lsoa_attr$vul2) >0, 1,0)
length(which(lsoa_attr$vul==1)) #777 lsoa are recognised as vulnerable groups

lsoa_attr$ch_income <- lsoa_attr$Median_2011- lsoa_attr$Median_2001
lsoa_attr$if_incomeinc <-ifelse(lsoa_attr$ch_income > median(lsoa_attr$ch_income), 1,0)
View(lsoa_attr)
##==========================2. plot regular map ============================
library (rgdal)
library(tmap)
library(tmaptools)
library(shinyjs)
library(sf)
#import the shp of london lsoa
london <- readOGR(dsn="//ad.ucl.ac.uk/home2/ucqbyz2/DesktopSettings/Desktop/EPA/30052019/rawdata",layer="gl_lsoa")
#join the attribute data to shp
london <- append_data(london, lsoa_attr, key.shp="code", key.data="Codes")
# plot the household income
#tmaptools::palette_explorer()
#tmap_mode("view")

tm_shape(london) +
  tm_fill(c( "Median_2011","pr_m_2011", "sp_rented2011", "non_white","qua4_2011", "affor"),
           style="jenks",
           palette=list("Reds", "YlOrRd","PuRd" , "YlGn","GnBu","BuPu"),n=7,
           auto.palette.mapping=FALSE,
           title="Legend", alpha=0.9)+
  tm_facets(sync = TRUE, ncol = 2)+
  tm_layout(frame=NA,
           title = c("Household_Income", "Housing_price","%Renters", "%Non_white","%High_Education", "Affordability"), title.size = 2, main.title.position =c("left","top"),
            legend.position = c("right","bottom"),legend.title.size = 1.8)


##==========================3. plot cartogram map ============================
library(cartogram)
# Create cartogram based on population
# calculate a poverty rate
#construct a cartogram using pop2011
lon_carto <- cartogram_cont (london,"pop_2011" , itermax=5)
tm_shape(lon_carto) + tm_fill("affor", title = "affordability")

tm_shape(lon_carto) +
  tm_fill(c( "Median_2011","pr_m_2011", "sp_rented2011", "non_white","qua4_2011", "affor"),
          style="jenks",
          palette=list("Reds", "YlOrRd","PuRd" , "YlGn","GnBu","BuPu"),n=7,
          auto.palette.mapping=FALSE,
          title="Legend", alpha=0.9)+
  tm_facets(sync = TRUE, ncol = 2)+
  tm_layout(frame=NA,
            title = c("Household_Income", "Housing_price","%Renters", "%Non_white","%High_Education", "Affordability"), title.size = 2, main.title.position =c("left","top"),
            legend.position = c("right","bottom"),legend.title.size = 1.8)
#cartograms with the boundaries
tm_shape(lon_carto) +
  tm_polygons(c( "Median_2011","pr_m_2011", "sp_rented2011", "non_white","qua4_2011", "affor"),
          style="jenks",
          palette=list("Reds", "YlOrRd","PuRd" , "YlGn","GnBu","BuPu"),n=7,
          auto.palette.mapping=FALSE,
          title="Legend", alpha=0.9)+
  tm_facets(sync = TRUE, ncol = 2)+
  tm_layout(frame=NA,
            title = c("Household_Income", "Housing_price","%Renters", "%Non_white","%High_Education", "Affordability"), title.size = 2, main.title.position =c("left","top"),
            legend.position = c("right","bottom"),legend.title.size = 1.8)

##==========================4.catogrise the gentrification groups and plot  ============================


#tm_shape(london)+tm_fill("Median_2011",style="jenks", palette=c("black","purple4","maroon3","darkorange2","gold1"), n=7, auto.palette.mapping=FALSE)

# define moderate and rich regions
shp.mr <- london[which(london$Median_2011>0.94*median (london$Median_2011)),]
# define low-income households regions
shp.lowincome <- london[which(london$Median_2011<=0.94*median (london$Median_2011)),]

# define ongoing gentrified regions
shp.on_gentrified <- shp.lowincome[which(shp.lowincome$hot_market==1 & shp.lowincome$if_incomeinc==1 ),]
sum(shp.on_gentrified$pop_2011)/sum(london$pop_2011) 
paste( round (sum(shp.on_gentrified$pop_2011)/sum(london$pop_2011)*100,2), "% is experiencing gentrification")
# define ongoing displacement regions
shp.dis<- shp.lowincome[which(shp.lowincome$pop_2011<=shp.lowincome$pop_2001 & shp.lowincome$if_incomeinc==0 & shp.lowincome$hot_market==0),]
sum(shp.dis$pop_2011)/sum(london$pop_2011)
paste( round (sum(shp.dis$pop_2011)/sum(london$pop_2011)*100,2), "% is experiencing displacement")
# definea risk gentrified regions
shp.risk <- shp.lowincome [which(shp.lowincome$hot_market==1 & shp.lowincome$vul==1),]
sum(shp.risk$pop_2011)/sum(london$pop_2011)
paste( round (sum(shp.risk$pop_2011)/sum(london$pop_2011)*100,2), "% is at risk of gentrification")

# plot the categories 
tm_shape(london)+
  tm_fill(col=NA)+
  tm_shape(shp.mr)+
  tm_fill(col="grey17", auto.palette.mapping=FALSE)+
  tm_shape(shp.lowincome)+
  tm_fill(col="purple4" )+
  tm_shape(shp.on_gentrified)+
  tm_fill(col="maroon3")+
  tm_shape(shp.dis)+
  tm_fill(col="darkorange2")+
  tm_shape(shp.risk)+
  tm_fill(col="gold1")+ 
  tm_compass(type = "arrow", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 100, 200), size = 1)
  
##now plot the cartogram version typology map


c_shp.mr <- lon_carto[which(lon_carto$Median_2011>0.94*median (lon_carto$Median_2011)),]
# define low-income households regions
c_shp.lowincome <- lon_carto[which(lon_carto$Median_2011<=0.94*median (lon_carto$Median_2011)),]
plot(c_shp.lowincome)
# define ongoing gentrified regions
c_shp.on_gentrified <- c_shp.lowincome[which(c_shp.lowincome$hot_market==1 & c_shp.lowincome$if_incomeinc==1 ),]
sum(shp.on_gentrified$pop_2011)/sum(london$pop_2011) 
paste( round (sum(shp.on_gentrified$pop_2011)/sum(london$pop_2011)*100,2), "% is experiencing gentrification")
# define ongoing displacement regions
c_shp.dis<- c_shp.lowincome[which(c_shp.lowincome$pop_2011<=c_shp.lowincome$pop_2001 & c_shp.lowincome$if_incomeinc==0 & c_shp.lowincome$hot_market==0),]
sum(shp.dis$pop_2011)/sum(london$pop_2011)
paste( round (sum(shp.dis$pop_2011)/sum(london$pop_2011)*100,2), "% is experiencing displacement")
# definea risk gentrified regions
c_shp.risk <- c_shp.lowincome [which(c_shp.lowincome$hot_market==1 & c_shp.lowincome$vul==1),]
sum(shp.risk$pop_2011)/sum(london$pop_2011)
paste( round (sum(shp.risk$pop_2011)/sum(london$pop_2011)*100,2), "% is at risk of gentrification")


# plot the cartogram version pf categories 
tm_shape(lon_carto)+
  tm_fill(col=NA)+
  tm_shape(c_shp.mr)+
  tm_fill(col="grey17", auto.palette.mapping=FALSE)+
  tm_shape(c_shp.lowincome)+
  tm_fill(col="purple4" )+
  tm_shape(c_shp.on_gentrified)+
  tm_fill(col="maroon3")+
  tm_shape(c_shp.dis)+
  tm_fill(col="darkorange2")+
  tm_shape(c_shp.risk)+
  tm_fill(col="gold1")+ 
  tm_compass(type = "arrow", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 5000, 10000), size = .01)+
  tm_layout(frame=NA,
            legend.position = c("right","bottom"),legend.title.size = 1.8)


