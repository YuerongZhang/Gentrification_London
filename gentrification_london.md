London gentrification
================

# London gentrification

\#\#==========================1. data
preparing============================

``` r
#join data
a<- read.csv ("https://www.dropbox.com/s/tef137xkcbcap0h/atlas.csv?raw=1")
b<- read.csv ("https://www.dropbox.com/s/766yez9h07ljner/2011price.csv?raw=1")
d<- read.csv ("https://www.dropbox.com/s/b7r399catw38c73/income.csv?raw=1")
```

``` r
library(plyr)
```

    ## Warning: package 'plyr' was built under R version 3.5.3

``` r
colnames(b)[1]<-"Codes"
colnames(d)[1]<-"Codes"
a<- join(a, b, by="Codes")
a<- join(a,d, by="Codes")
lsoa_attr <- a[,-c(11,13)]
```

``` r
head(lsoa_attr)
```

    ##       Codes       Names pop_2001 pop_2011 non_white qua4_2011
    ## 1 E01000907 Camden 001A     1519     1431     26.50      44.2
    ## 2 E01000908 Camden 001B     1547     1564     27.58      50.4
    ## 3 E01000909 Camden 001C     1600     1602     15.92      61.9
    ## 4 E01000912 Camden 001D     1738     1589     22.66      47.2
    ## 5 E01000913 Camden 001E     1617     1695     11.81      65.2
    ## 6 E01000893 Camden 002A     1568     1563     11.88      72.6
    ##   s_rented_2011 p_rented_2011 sp_rented2011 price_m_2011 price_m_2001
    ## 1          54.4          13.7          68.1    415000.00       114000
    ## 2          52.6          14.0          66.6    282500.00       114000
    ## 3          18.6          18.8          37.4    435000.00       114000
    ## 4          51.3          15.0          66.3    500000.00       114000
    ## 5           1.9          19.4          21.3   1000000.00       114000
    ## 6           4.5          34.1          38.6    740000.00       113375
    ##   Median_2001 Median_2011
    ## 1       27790       39200
    ## 2       26920       40040
    ## 3       34810       52000
    ## 4       28840       40870
    ## 5       37310       56610
    ## 6       39740       58080

``` r
#define hot market
lsoa_attr$pr_m_2011 <- as.numeric(as.character(lsoa_attr$price_m_2011))
```

    ## Warning: NAs introduced by coercion

``` r
lsoa_attr$ch_price <-lsoa_attr$pr_m_2011-lsoa_attr$price_m_2001
lsoa_attr$hot_market <-ifelse(lsoa_attr$ch_price>111500, 1, 0)
```

``` r
#define lowincome group: 1 indicate low income group
lsoa_attr$low_income <-ifelse(lsoa_attr$Median_2011<=0.8*median(lsoa_attr$Median_2011), 1,0)
length(which(lsoa_attr$low_income==1)) #353 lsoa are recognised as low income group. 
```

    ## [1] 353

``` r
#define affordability: affordability = income/housingprice; 1 indicate could afford
lsoa_attr$affor <- lsoa_attr$Median_2011/lsoa_attr$pr_m_2011
#1 indicates the housing price increasing rate is below median speed
lsoa_attr$if_affor <-ifelse(lsoa_attr$pr_m_2011<=0.8*median(sort (lsoa_attr$pr_m_2011, decreasing=FALSE)), 1,0)
```

``` r
#define education(1 is low educated), renter (1 meanes lots of renters) and nonwhite (1 means lots of non-white)
lsoa_attr$if_edu <-ifelse(lsoa_attr$qua4_2011<= median(lsoa_attr$qua4_2011), 1,0)
lsoa_attr$if_rent <-ifelse(lsoa_attr$sp_rented2011 > median (lsoa_attr$sp_rented2011), 1,0)
lsoa_attr$if_nonwhite <-ifelse(lsoa_attr$non_white > median (lsoa_attr$non_white), 1,0)
```

``` r
#define vulnerable group, vul=1 means vulnerable

lsoa_attr$vul1 <- ifelse(lsoa_attr$if_affor==1 & lsoa_attr$if_edu==1& lsoa_attr$if_rent==1,1,0)
lsoa_attr$vul2 <- ifelse(lsoa_attr$if_affor==1 & lsoa_attr$if_edu==1& lsoa_attr$if_nonwhite==1,1,0)

lsoa_attr$vul <- ifelse((lsoa_attr$vul1 + lsoa_attr$vul2) >0, 1,0)
length(which(lsoa_attr$vul==1)) #777 lsoa are recognised as vulnerable groups
```

    ## [1] 777

``` r
#write.csv (lsoa_attr,"//ad.ucl.ac.uk/home2/ucqbyz2/DesktopSettings/Desktop/EPA/30052019/attr.csv")
```

\#\#=====plot map=======

``` r
library (rgdal)
```

    ## Warning: package 'rgdal' was built under R version 3.5.3

    ## Loading required package: sp

    ## Warning: package 'sp' was built under R version 3.5.3

    ## rgdal: version: 1.4-3, (SVN revision 828)
    ##  Geospatial Data Abstraction Library extensions to R successfully loaded
    ##  Loaded GDAL runtime: GDAL 2.2.3, released 2017/11/20
    ##  Path to GDAL shared files: N:/Development/R 3.51/Packages/rgdal/gdal
    ##  GDAL binary built with GEOS: TRUE 
    ##  Loaded PROJ.4 runtime: Rel. 4.9.3, 15 August 2016, [PJ_VERSION: 493]
    ##  Path to PROJ.4 shared files: N:/Development/R 3.51/Packages/rgdal/proj
    ##  Linking to sp version: 1.3-1

``` r
library(tmap)
```

    ## Warning: package 'tmap' was built under R version 3.5.3

``` r
library(tmaptools)
```

    ## Warning: package 'tmaptools' was built under R version 3.5.3

``` r
library(shinyjs)
```

    ## Warning: package 'shinyjs' was built under R version 3.5.3

    ## 
    ## Attaching package: 'shinyjs'

    ## The following object is masked from 'package:sp':
    ## 
    ##     show

    ## The following objects are masked from 'package:methods':
    ## 
    ##     removeClass, show

``` r
library(sf)
```

    ## Warning: package 'sf' was built under R version 3.5.3

    ## Linking to GEOS 3.6.1, GDAL 2.2.3, PROJ 4.9.3

``` r
#import the shp of london lsoa
london <- readOGR(dsn="//ad.ucl.ac.uk/home2/ucqbyz2/DesktopSettings/Desktop/EPA/30052019/rawdata",layer="gl_lsoa")
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "\\ad.ucl.ac.uk\home2\ucqbyz2\DesktopSettings\Desktop\EPA\30052019\rawdata", layer: "gl_lsoa"
    ## with 4835 features
    ## It has 2 fields

``` r
#join the attribute data to shp
london <- append_data(london, lsoa_attr, key.shp="code", key.data="Codes")
```

    ## Keys match perfectly.

``` r
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
```

    ## Warning: The argument auto.palette.mapping is deprecated. Please use
    ## midpoint for numeric data and stretch.palette for categorical data to
    ## control the palette mapping.

    ## Use "fisher" instead of "jenks" for larger data sets
    ## Use "fisher" instead of "jenks" for larger data sets

    ## Some legend labels were too wide. These labels have been resized to 0.37, 0.37. Increase legend.width (argument of tm_layout) to make the legend wider and therefore the labels larger.

![](gentrification_london_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
