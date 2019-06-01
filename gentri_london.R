##==========================1. data preparing============================
#join data
a<- read.csv ("https://raw.githubusercontent.com/JessicavanJohn/Gentrification_London/master/data/atlas.csv?token=AMGYEP7P7LS4THNB6X5ECUK46B7DC")
b<- read.csv ("https://raw.githubusercontent.com/JessicavanJohn/Gentrification_London/master/data/2011price.csv?token=AMGYEP4WTP6R7GIT5WUJHFS46B7AE")
d<- read.csv ("https://raw.githubusercontent.com/JessicavanJohn/Gentrification_London/master/data/income.csv?token=AMGYEP4AIJ3YPPMCB2GQBQS46B7FM")

library(plyr)
colnames(b)[1]<-"Codes"
colnames(d)[1]<-"Codes"
a<- join(a, b, by="Codes")
a<- join(a,d, by="Codes")

lsoa_attr <- a[,-c(11,13)]
View(lsoa_attr)

#define hot market
lsoa_attr$pr_m_2011 <- as.numeric(as.character(lsoa_attr$price_m_2011))
lsoa_attr$ch_price <-lsoa_attr$pr_m_2011-lsoa_attr$price_m_2001
lsoa_attr$hot_market <-ifelse(lsoa_attr$ch_price>111500, 1, 0)

#define lowincome group: 1 indicate low income group
lsoa_attr$low_income <-ifelse(lsoa_attr$Median_2011<=0.8*median(lsoa_attr$Median_2011), 1,0)
length(which(lsoa_attr$low_income==1)) #353 lsoa are recognised as low income group. 
#define affordability: affordability = income/housingprice; 1 indicate could afford
lsoa_attr$affor <- lsoa_attr$Median_2011/lsoa_attr$pr_m_2011
#1 indicates the housing price increasing rate is below median speed
lsoa_attr$if_affor <-ifelse(lsoa_attr$pr_m_2011<=0.8*median(sort (lsoa_attr$pr_m_2011, decreasing=FALSE)), 1,0)

#define education(1 is low educated), renter (1 meanes lots of renters) and nonwhite (1 means lots of non-white)
lsoa_attr$if_edu <-ifelse(lsoa_attr$qua4_2011<= median(lsoa_attr$qua4_2011), 1,0)
lsoa_attr$if_rent <-ifelse(lsoa_attr$sp_rented2011 > median (lsoa_attr$sp_rented2011), 1,0)
lsoa_attr$if_nonwhite <-ifelse(lsoa_attr$non_white > median (lsoa_attr$non_white), 1,0)

#define vulnerable group, vul=1 means vulnerable

lsoa_attr$vul1 <- ifelse(lsoa_attr$if_affor==1 & lsoa_attr$if_edu==1& lsoa_attr$if_rent==1,1,0)
lsoa_attr$vul2 <- ifelse(lsoa_attr$if_affor==1 & lsoa_attr$if_edu==1& lsoa_attr$if_nonwhite==1,1,0)

lsoa_attr$vul <- ifelse((lsoa_attr$vul1 + lsoa_attr$vul2) >0, 1,0)
length(which(lsoa_attr$vul==1)) #777 lsoa are recognised as vulnerable groups
#write.csv (lsoa_attr,"//ad.ucl.ac.uk/home2/ucqbyz2/DesktopSettings/Desktop/EPA/30052019/attr.csv")

##==========================2. plot map ============================
library (rgdal)
library(tmap)
library(tmaptools)
library(shinyjs)
library(sf)

#import the shp of london lsoa
london <- readOGR(dsn="~/Desktop/shp",layer="shp")
london@data <- london@data[,-c(3:27)]
View(london@data)
#join the attribute data to shp

london <- append_data(london, lsoa_attr, key.shp="code", key.data="Codes")
# plot the household income
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
