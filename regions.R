#this is the script to describe the classes of the regions
#plans to take in the data set
install.packages("tmap")
install.packages("leaflet")
library("tmap")
library("leaflet")

#a function that takes in present and past which are vectors of the dataframe and calculates the absolute change of students
#that come to Williams from each state/country
#absolutechange <- function(present , past){
#  present-past
#}

#List of colors for convenience(lightestshade to darkest shade)
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
copy <- as.data.frame(matrix(0, ncol = 2, nrow =207))
copy$V1 <- totalframe14$State.Countries
colnames(copy)[1] <- "States.Countries"
colnames(copy)[2] <- "Color"

#fills in the second column of the matrix with corresponding colors based on the absolutechange to each corresponding country
# a function that assigns colors based on the absolute change from the newer year subtracted by the older year
color <- function(x){
  #fills in the second column of the matrix with corresponding colors based on the absolutechange to each corresponding country
  # a function that assigns colors based on the absolute change from the newer year subtracted by the older year
  if(x == 0) y <- NA
  if(x>0 && x<=5)  y <- green1
  if(x>5 && x<=10) y <- green2
  if(x>10 && x<=15) y <- green3
  if(x>15 && x<=20) y <- green4
  if(x>20 && x<=25) y <- green5
  if(x>25 && x<=30) y <- green6
  if(x>30 && x<=35) y <- green7
  if(x>35 && x<=40) y <- green8
  if(x>40 && x<=45) y <- green9
  if(x>45) y <- green10
  if(x>=-5 && x<0) y<- red1
  if(x>=-10 && x<(-5)) y<- red2
  if(x>=-15 && x<(-10)) y<- red3
  if(x>=-20 && x<(-15)) y<- red4
  if(x>=-25 && x<(-20)) y<- red5
  if(x>=-30 && x<(-25)) y<- red6
  if(x>=-35 && x<(-30)) y<- red7
  if(x>=-40 && x<(-35)) y<- red8
  if(x>=-45 && x<(-40)) y<- red9
  if(x<=-45) y<- red10
  return(y)
}

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



year2000.2002 <- sapply(yearsdata$Students.Attending.in.2001.2002- yearsdata$Students.Attending.in.2000.2001,color)
year2001.2003 <- sapply(yearsdata$Students.Attending.in.2002.2003- yearsdata$Students.Attending.in.2001.2002,color)
year2002.2004 <- sapply(yearsdata$Students.Attending.in.2003.2004- yearsdata$Students.Attending.in.2002.2003,color)
year2003.2005 <- sapply(yearsdata$Students.Attending.in.2004.2005- yearsdata$Students.Attending.in.2003.2004,color)
year2004.2006 <- sapply(yearsdata$Students.Attending.in.2005.2006- yearsdata$Students.Attending.in.2004.2005,color)
year2005.2007 <- sapply(yearsdata$Students.Attending.in.2006.2007- yearsdata$Students.Attending.in.2005.2006,color)
year2006.2008 <- sapply(yearsdata$Students.Attending.in.2007.2008- yearsdata$Students.Attending.in.2006.2007,color)
year2007.2009 <- sapply(yearsdata$Students.Attending.in.2008.2009- yearsdata$Students.Attending.in.2007.2008,color)
year2008.2010 <- sapply(yearsdata$Students.Attending.in.2009.2010- yearsdata$Students.Attending.in.2008.2009,color)
year2009.2011 <- sapply(yearsdata$Students.Attending.in.2010.2011- yearsdata$Students.Attending.in.2009.2010,color)
year2010.2012 <- sapply(yearsdata$Students.Attending.in.2011.2012- yearsdata$Students.Attending.in.2010.2011,color)
year2011.2013 <- sapply(yearsdata$Students.Attending.in.2012.2013- yearsdata$Students.Attending.in.2011.2012,color)
year2012.2014 <- sapply(yearsdata$Students.Attending.in.2013.2014- yearsdata$Students.Attending.in.2012.2013,color)
year2013.2015 <- sapply(yearsdata$Students.Attending.in.2014.2015- yearsdata$Students.Attending.in.2013.2014,color)
year2014.2016 <- sapply(yearsdata$Students.Attending.in.2015.2016- yearsdata$Students.Attending.in.2014.2015,color)


copy$Color <- sapply(x , color)

usshapefile <- "data/cb_2014_us_county_5m/cb_2014_us_county_5m.shp"
usgeo <- read_shape(file=usshapefile)

