#this is the script to describe the classes of the regions
#plans to take in the data set
install.packages("tmap")
install.packages("leaflet")
install.packages("stringdist")
library("tmap")
library("leaflet")
library("tmaptools")
library("stringdist")
library(raster)
#a function that takes in present and past which are vectors of the dataframe and calculates the absolute change of students
#that come to Williams from each state/country
#absolutechange <- function(present , past){
#  present-past
#}

#List of colors for convenience(lightestshade to darkest shade) Can use this for the created palette
green1 <- "#dbefd3"
green2 <- "#c1eeb4"
green3 <- "#9de686"
green4 <- "#78dd54"
green5 <- "#4fc431"
green6 <- "#47a426"
green7 <- "#37801d"
green8 <- "#265b12"
green9 <- "#1b4109"
green10 <- "#081f02"
red1 <- "#ffcacb"
red2 <- "#ff7f80"
red3 <- "#ff4c4e"
red4 <- "#ff3235"
red5 <- "#ff5656"
red6 <- "#f90204"
red7 <- "#e60000"
red8 <- "#cd0002"
red9 <- "#b20001"
red10 <- "#800000"

#creating a copy matrix of a 2 by 207 matrix and filling in the first columnn with the countries
copy <- as.data.frame(matrix(0, ncol = 1, nrow =207))
copy$V1 <- totalframe14$State.Countries
colnames(copy)[1] <- "NAME"


#fills in the second column of the matrix with corresponding colors based on the absolutechange to each corresponding country
# a function that assigns colors based on the absolute change from the newer year subtracted by the older year
#color <- function(x){
  #fills in the second column of the matrix with corresponding colors based on the absolutechange to each corresponding country
  # a function that assigns colors based on the absolute change from the newer year subtracted by the older year
# if(x == 0) y <- NA
# if(x>0 && x<=5)  y <- green1
# if(x>5 && x<=10) y <- green2
# if(x>10 && x<=15) y <- green3
# if(x>15 && x<=20) y <- green4
# if(x>20 && x<=25) y <- green5
# if(x>25 && x<=30) y <- green6
# if(x>30 && x<=35) y <- green7
# if(x>35 && x<=40) y <- green8
# if(x>40 && x<=45) y <- green9
# if(x>45) y <- green10
# if(x>=-5 && x<0) y<- red1
# if(x>=-10 && x<(-5)) y<- red2
# if(x>=-15 && x<(-10)) y<- red3
# if(x>=-20 && x<(-15)) y<- red4
# if(x>=-25 && x<(-20)) y<- red5
# if(x>=-30 && x<(-25)) y<- red6
# if(x>=-35 && x<(-30)) y<- red7
# if(x>=-40 && x<(-35)) y<- red8
# if(x>=-45 && x<(-40)) y<- red9
# if(x<=-45) y<- red10
# return(y)
#}
#Initiates a basic data frame for every year
year2000.2002 <- copy
year2001.2003 <- copy
year2002.2004 <- copy
year2003.2005 <- copy
year2004.2006 <- copy
year2005.2007 <- copy
year2006.2008 <- copy
year2007.2009 <- copy
year2008.2010 <- copy
year2009.2011 <- copy
year2010.2012 <- copy
year2011.2013 <- copy
year2012.2014 <- copy
year2013.2015 <- copy
year2014.2016 <- copy

#Adds the absolutechange colomn to each data frame
year2000.2002$Change <- c(yearsdata$Students.Attending.in.2001.2002- yearsdata$Students.Attending.in.2000.2001)
year2001.2003$Change <- c(yearsdata$Students.Attending.in.2002.2003- yearsdata$Students.Attending.in.2001.2002)
year2002.2004$Change <- c(yearsdata$Students.Attending.in.2003.2004- yearsdata$Students.Attending.in.2002.2003)
year2003.2005$Change <- c(yearsdata$Students.Attending.in.2004.2005- yearsdata$Students.Attending.in.2003.2004)
year2004.2006$Change <- c(yearsdata$Students.Attending.in.2005.2006- yearsdata$Students.Attending.in.2004.2005)
year2005.2007$Change <- c(yearsdata$Students.Attending.in.2006.2007- yearsdata$Students.Attending.in.2005.2006)
year2006.2008$Change <- c(yearsdata$Students.Attending.in.2007.2008- yearsdata$Students.Attending.in.2006.2007)
year2007.2009$Change <- c(yearsdata$Students.Attending.in.2008.2009- yearsdata$Students.Attending.in.2007.2008)
year2008.2010$Change <- c(yearsdata$Students.Attending.in.2009.2010- yearsdata$Students.Attending.in.2008.2009)
year2009.2011$Change <- c(yearsdata$Students.Attending.in.2010.2011- yearsdata$Students.Attending.in.2009.2010)
year2010.2012$Change <- c(yearsdata$Students.Attending.in.2011.2012- yearsdata$Students.Attending.in.2010.2011)
year2011.2013$Change <- c(yearsdata$Students.Attending.in.2012.2013- yearsdata$Students.Attending.in.2011.2012)
year2012.2014$Change <- c(yearsdata$Students.Attending.in.2013.2014- yearsdata$Students.Attending.in.2012.2013)
year2013.2015$Change <- c(yearsdata$Students.Attending.in.2014.2015- yearsdata$Students.Attending.in.2013.2014)
year2014.2016$Change <- c(yearsdata$Students.Attending.in.2015.2016- yearsdata$Students.Attending.in.2014.2015)

#Reading in the Nationfile and intiating shpfiles for each year
nationshapefile <- "~/HutchinHill/cb_2015_us_state_20m/cb_2015_us_state_20m.shp"
nationgeo2000.2002<- read_shape(file = nationshapefile)
nationgeo2001.2003<- read_shape(file = nationshapefile)
nationgeo2002.2004<- read_shape(file = nationshapefile)
nationgeo2003.2005<- read_shape(file = nationshapefile)
nationgeo2004.2006<- read_shape(file = nationshapefile)
nationgeo2005.2007<- read_shape(file = nationshapefile)
nationgeo2006.2008<- read_shape(file = nationshapefile)
nationgeo2007.2009<- read_shape(file = nationshapefile)
nationgeo2008.2010<- read_shape(file = nationshapefile)
nationgeo2009.2011<- read_shape(file = nationshapefile)
nationgeo2010.2012<- read_shape(file = nationshapefile)
nationgeo2011.2013<- read_shape(file = nationshapefile)
nationgeo2012.2014<- read_shape(file = nationshapefile)
nationgeo2013.2015<- read_shape(file = nationshapefile)
nationgeo2014.2016<- read_shape(file = nationshapefile)

#Tests to Merge
#cool <- match(year2000.2002$NAME, nationgeo$NAME)
#merge(nationgeo@data, year2000.2002, bx.x = "NAME", by.y="Change", all.x = TRUE)
#am <- amatch(nationgeo@data$NAME, year2000.2002$NAME, maxDist = 3)
#totalmap <- data.frame()
#for (i in 1:dim(nationgeo@data)[1]) {
#  totalmap<-rbind(totalmap,data.frame(nationgeo@data[i,],year2000.2002[am[i],]))
#}
#totalmap

#Corrects the Districtof Columbia issue
year2000.2002$NAME <- as.character(year2000.2002$NAME)
year2000.2002[year2000.2002$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2001.2003$NAME <- as.character(year2001.2003$NAME)
year2001.2003[year2001.2003$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2002.2004$NAME <- as.character(year2001.2003$NAME)
year2002.2004[year2001.2003$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2003.2005$NAME <- as.character(year2001.2003$NAME)
year2003.2005[year2001.2003$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2004.2006$NAME <- as.character(year2001.2003$NAME)
year2004.2006[year2001.2003$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2005.2007$NAME <- as.character(year2001.2003$NAME)
year2005.2007[year2001.2003$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2006.2008$NAME <- as.character(year2001.2003$NAME)
year2006.2008[year2001.2003$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2007.2009$NAME <- as.character(year2001.2003$NAME)
year2007.2009[year2001.2003$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2008.2010$NAME <- as.character(year2001.2003$NAME)
year2008.2010[year2001.2003$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2009.2011$NAME <- as.character(year2001.2003$NAME)
year2009.2011[year2001.2003$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2010.2012$NAME <- as.character(year2001.2003$NAME)
year2010.2012[year2001.2003$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2011.2013$NAME <- as.character(year2001.2003$NAME)
year2011.2013[year2001.2003$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2012.2014$NAME <- as.character(year2001.2003$NAME)
year2012.2014[year2001.2003$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2013.2015$NAME <- as.character(year2001.2003$NAME)
year2013.2015[year2001.2003$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2014.2016$NAME <- as.character(year2001.2003$NAME)
year2014.2016[year2001.2003$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"

#Working code for merging spacial object with year2000.2002
nationgeo@data <- data.frame(nationgeo@data, year2000.2002[match(nationgeo@data[,"NAME"], year2000.2002[,"NAME"]),])
tmap::qtm(nationgeo,"Change")

#Creating maps for every single year
nationgeo2000.2002@data<- data.frame(nationgeo2000.2002@data, year2000.2002[match(nationgeo2000.2002@data[,"NAME"], year2000.2002[,"NAME"]),])
nationgeo2001.2003@data<- data.frame(nationgeo2001.2003@data, year2001.2003[match(nationgeo@data[,"NAME"], year2001.2003[,"NAME"]),])
nationgeo2002.2004@data<- data.frame(nationgeo2002.2004@data, year2002.2004[match(nationgeo@data[,"NAME"], year2000.2002[,"NAME"]),])
nationgeo2003.2005@data<- data.frame(nationgeo2003.2005@data, year2000.2002[match(nationgeo@data[,"NAME"], year2000.2002[,"NAME"]),])
nationgeo2004.2006@data<- data.frame(nationgeo2004.2006@data, year2000.2002[match(nationgeo@data[,"NAME"], year2000.2002[,"NAME"]),])
nationgeo2005.2007@data<- data.frame(nationgeo2005.2007@data, year2000.2002[match(nationgeo@data[,"NAME"], year2000.2002[,"NAME"]),])
nationgeo2006.2008@data<- data.frame(nationgeo2006.2008@data, year2000.2002[match(nationgeo@data[,"NAME"], year2000.2002[,"NAME"]),])
nationgeo2007.2009@data<- data.frame(nationgeo2007.2003@data, year2000.2002[match(nationgeo@data[,"NAME"], year2000.2002[,"NAME"]),])
nationgeo2008.2010@data<- data.frame(nationgeo2008.2003@data, year2000.2002[match(nationgeo@data[,"NAME"], year2000.2002[,"NAME"]),])
nationgeo2009.2011@data<- data.frame(nationgeo2009.2003@data, year2000.2002[match(nationgeo@data[,"NAME"], year2000.2002[,"NAME"]),])
nationgeo2010.2012@data<- data.frame(nationgeo2010.2003@data, year2000.2002[match(nationgeo@data[,"NAME"], year2000.2002[,"NAME"]),])
nationgeo2011.2013@data<- data.frame(nationgeo2011.2003@data, year2000.2002[match(nationgeo@data[,"NAME"], year2000.2002[,"NAME"]),])
nationgeo2012.2014@data<- data.frame(nationgeo2012.2003@data, year2000.2002[match(nationgeo@data[,"NAME"], year2000.2002[,"NAME"]),])
nationgeo2013.2015@data<- data.frame(nationgeo2013.2003@data, year2000.2002[match(nationgeo@data[,"NAME"], year2000.2002[,"NAME"]),])
nationgeo2014.2016@data<- data.frame(nationgeo2014.2003@data, year2000.2002[match(nationgeo@data[,"NAME"], year2000.2002[,"NAME"]),])


#Code for the interative map
tmap_mode("view")
usmap <- tm_shape(nationgeo)+ tm_polygons("Change", palette = "Blues" , contrast=.7, id="name", title="Change in Students") + tm_style_gray() + tm_format_World()
usmap

#Attempts to make a working code that matches other word
#year2000.2002$match[!is.na(i)] <- nationgeo@data$NAME[i[!is.na(i)]]
#nationgeo@data <- data.frame(nationgeo@data, year2000.2002[match(nationgeo@data[,"NAME"], year2000.2002[,"match"]),])
