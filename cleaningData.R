#Reads in all the data in a clean format
#We will read each 2000-2001 data as the year2000 for simpicities sake

readYears("~/HutchinHill/2000-2001.rtf", "2000.2001_Clean")
readYears("~/HutchinHill/2001-2002.rtf", "2001.2002_Clean")
readYears("~/HutchinHill/2002-2003.rtf", "2002.2003_Clean")
readYears("~/HutchinHill/2003-2004.rtf", "2003.2004_Clean")
readYears("~/HutchinHill/2004-2005.rtf", "2004.2005_Clean")
readYears("~/HutchinHill/2005-2006.rtf", "2005.2006_Clean")
readYears("~/HutchinHill/2006-2007.rtf", "2006.2007_Clean")
readYears("~/HutchinHill/2007-2008.rtf", "2007.2008_Clean")
readYears("~/HutchinHill/2008-2009.rtf", "2008.2009_Clean")
readYears("~/HutchinHill/2009-2010.rtf", "2009.2010_Clean")
readYears("~/HutchinHill/2010-2011.rtf", "2010.2011_Clean")
readYears("~/HutchinHill/2011-2012.rtf", "2011.2012_Clean")
readYears("~/HutchinHill/2012-2013.rtf", "2012.2013_Clean")
readYears("~/HutchinHill/2013-2014.rtf", "2013.2014_Clean")
readYears("~/HutchinHill/2014-2015.rtf", "2014.2015_Clean")
readYears("~/HutchinHill/2015-2016.rtf", "2015.2016_Clean")


#Creates dataframes for all of the data
`2000` <- read.csv("~/HutchinHill/2000.2001_Clean", header=FALSE)
`2001` <- read.csv("~/HutchinHill/2001.2002_Clean", header=FALSE)
`2002` <- read.csv("~/HutchinHill/2002.2003_Clean", header=FALSE)
`2003` <- read.csv("~/HutchinHill/2003.2004_Clean", header=FALSE)
`2004` <- read.csv("~/HutchinHill/2004.2005_Clean", header=FALSE)
`2005` <- read.csv("~/HutchinHill/2005.2006_Clean", header=FALSE)
`2006` <- read.csv("~/HutchinHill/2006.2007_Clean", header=FALSE)
`2007` <- read.csv("~/HutchinHill/2007.2008_Clean", header=FALSE)
`2008` <- read.csv("~/HutchinHill/2008.2009_Clean", header=FALSE)
`2009` <- read.csv("~/HutchinHill/2009.2010_Clean", header=FALSE)
`2010` <- read.csv("~/HutchinHill/2010.2011_Clean", header=FALSE)
`2011` <- read.csv("~/HutchinHill/2011.2012_Clean", header=FALSE)
`2012` <- read.csv("~/HutchinHill/2012.2013_Clean", header=FALSE)
`2013` <- read.csv("~/HutchinHill/2013.2014_Clean", header=FALSE)
`2014` <- read.csv("~/HutchinHill/2014.2015_Clean", header=FALSE)
`2015` <- read.csv("~/HutchinHill/2015.2016_Clean", header=FALSE)

#Renames all of the columns
newname <- c("State/Countries", "Number of Students")
names(`2000`) <- c("State/Countries", "Students Attending in 2000-2001")
names(`2001`) <- c("State/Countries", "Students Attending in 2001-2002")
names(`2002`) <- c("State/Countries", "Students Attending in 2002-2003")
names(`2003`) <- c("State/Countries", "Students Attending in 2003-2004")
names(`2004`) <- c("State/Countries", "Students Attending in 2004-2005")
names(`2005`) <- c("State/Countries", "Students Attending in 2005-2006")
names(`2006`) <- c("State/Countries", "Students Attending in 2006-2007")
names(`2007`) <- c("State/Countries", "Students Attending in 2007-2008")
names(`2008`) <- c("State/Countries", "Students Attending in 2008-2009")
names(`2009`) <- c("State/Countries", "Students Attending in 2009-2010")
names(`2010`) <- c("State/Countries", "Students Attending in 2010-2011")
names(`2011`) <- c("State/Countries", "Students Attending in 2011-2012")
names(`2012`) <- c("State/Countries", "Students Attending in 2012-2013")
names(`2013`) <- c("State/Countries", "Students Attending in 2013-2014")
names(`2014`) <- c("State/Countries", "Students Attending in 2014-2015")
names(`2015`) <- c("State/Countries", "Students Attending in 2015-2016")

#Creating the data frames for all of these lists
year2000 <-data.frame(`2000`)
year2001 <-data.frame(`2001`)
year2002 <-data.frame(`2002`)
year2003 <-data.frame(`2003`)
year2004 <-data.frame(`2004`)
year2005 <-data.frame(`2005`)
year2006 <-data.frame(`2006`)
year2007 <-data.frame(`2007`)
year2008 <-data.frame(`2008`)
year2009 <-data.frame(`2009`)
year2010 <-data.frame(`2010`)
year2011 <-data.frame(`2011`)
year2012 <-data.frame(`2012`)
year2013 <-data.frame(`2013`)
year2014 <-data.frame(`2014`)
year2015 <-data.frame(`2015`)

#adding a column specifying the year
#year2000$Year <- 2000
#year2001$Year <- 2001
#year2002$Year <- 2002
#year2003$Year <- 2003
#year2004$Year <- 2004
#year2005$Year <- 2005
#year2006$Year <- 2006
#year2007$Year <- 2007
#year2008$Year <- 2008
#year2009$Year <- 2009
#year2010$Year <- 2010
#year2011$Year <- 2011
#year2012$Year <- 2012
#year2013$Year <- 2013
#year2014$Year <- 2014
#year2015$Year <- 2015

#Creates a total data frame by states and countries

totalframe<- merge(year2000, year2001, by= c("State.Countries"), all = TRUE)
totalframe<- merge(totalframe, year2002, by= c("State.Countries"), all = TRUE)
totalframe<- merge(totalframe, year2003, by= c("State.Countries"), all = TRUE)
totalframe<- merge(totalframe, year2004, by= c("State.Countries"), all = TRUE)
totalframe<- merge(totalframe, year2005, by= c("State.Countries"), all = TRUE)
totalframe<- merge(totalframe, year2006, by= c("State.Countries"), all = TRUE)
totalframe<- merge(totalframe, year2007, by= c("State.Countries"), all = TRUE)
totalframe<- merge(totalframe, year2008, by= c("State.Countries"), all = TRUE)
totalframe<- merge(totalframe, year2009, by= c("State.Countries"), all = TRUE)
totalframe<- merge(totalframe, year2010, by= c("State.Countries"), all = TRUE)
totalframe<- merge(totalframe, year2011, by= c("State.Countries"), all = TRUE)
totalframe<- merge(totalframe, year2012, by= c("State.Countries"), all = TRUE)
totalframe<- merge(totalframe, year2013, by= c("State.Countries"), all = TRUE)
totalframe<- merge(totalframe, year2014, by= c("State.Countries"), all = TRUE)
totalframe<- merge(totalframe, year2015, by= c("State.Countries"), all = TRUE)



