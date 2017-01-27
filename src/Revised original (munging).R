# Session to draw land value graph(s)
# See http://www.designandanalytics.com/american-beards-over-time-time-series-in-R-part-1  & part 2 for some fundamentals
# Initialize the data as a data frame

# No need for these when using the ProjectTemplate
#landdata-fhfa-national-2016q1.df <- read.csv(file = "~/OneDrive/Documents/Research-King Mac/Political Economy/Land Rent in the Circuit of Capital/Data Analysis/landdata-fhfa-national-2016q1.csv", header = TRUE, sep=",")
#landdata_fhfa_national_2016q1.df <- read.csv(file = "~/OneDrive/Documents/Research-King Mac/Political Economy/Land Rent in the Circuit of Capital/Data Analysis/landdata-fhfa-national-2016q1.csv", header = TRUE, sep=",")
#landdata_fhfa_national_2016q1.df <- read.csv(file = "~/OneDrive/Documents/Research-King Mac/Political Economy/Land Rent in the Circuit of Capital/Data Analysis/landdata-fhfa-national-2016q1.csv", header = TRUE, sep=",")
#landdata_fhfa_national_2016q1.df <- read.csv(file = "landdata-fhfa-national-2016q1.csv", header = TRUE, sep=",")
#head(landdata_fhfa_national_2016q1.df)

# These have to be run (only?) once.
install.packages("XML") # Need to read .zoo, to store time series
install.packages("xts") # Need to store time series
install.packages("tis") # Need for recession shading
install.packages("gridExtra") # For caption
install.packages("Hmisc") # For variable labels

# Now the real work begins
library('ProjectTemplate');load.project(); # Do the project's magic

# ProjectTemplate should take care of these. CHECK HOW.
library(XML) # To read .zoo.
library(xts) # To store time series
library(tis) # For recession shading
library(ggplot2) # For finished (artsy) plotting
library(gridExtra) # For captions
library(Hmisc) # For variable labels

# MUNGING BEGINS HERE
# Now make the original data frame into an xts time series
landdata_fhfa_national_2016q1.xts <- as.xts(landdata_fhfa_national_2016q1.df) # NEED TO CHANGE TO CSW

#head(landdata_fhfa_national_2016q1.df) # SHOULDN'T HAVE TO CHECK
# Test to see if xts allows yearqtr
# junk <- as.yearqtr(landdata_fhfa_national_2016q1.df$OBS)
# junk
# It works!

# Now replace the original OBS variable, which has date info. (e.g., 1975Q1), and convert it to
# xts yearqtr data.
landdata_fhfa_national_2016q1.df$OBS <- as.yearqtr(landdata_fhfa_national_2016q1.df$OBS)


# Now try making it into an xts time series
#landdata_fhfa_national_2016q1.xts <- as.xts(landdata_fhfa_national_2016q1.df)
#as.yearqtr(landdata_fhfa_national_2016q1.df$OBS)
#dates <- as.Date(as.yearqtr(landdata_fhfa_national_2016q1.df$OBS))
#class(dates)
#dates
#landdata_fhfa_national_2016q1.df$OBS <- dates
#head(landdata_fhfa_national_2016q1.df)
## Now try to make the xts object
#landdata_fhfa_national_2016q1.xts <- as.xts(landdata_fhfa_national_2016q1.df)
#class(landdata_fhfa_national_2016q1.df$OBS)
#as.POSIXlt(landdata_fhfa_national_2016q1.df$OBS)
#? as.xts
#str(landdata_fhfa_national_2016q1.df)
#landdata_fhfa_national_2016q1.xts <- xts(landdata_fhfa_national_2016q1.df[,-1], landdata_fhfa_national_2016q1.df[,1])
#head(landdata_fhfa_national_2016q1.xts)

# MUNGING REALLY BEGINS HERE
# That did it! (Thank you https://rstudio-pubs-static.s3.amazonaws.com/40873_5fbe3860854a47c38a58aabd01f9cf9d.html)
# OK, Let's try all that again
# Read in the data
landdata_fhfa_national_2016q1.df <- read.csv(file = "landdata-fhfa-national-2016q1.csv", header = TRUE, sep=",")
# Get the first column into dates
# class(landdata_fhfa_national_2016q1.df$OBS)
#dates <- as.Date(landdata_fhfa_national_2016q1.df$OBS)
dates <- as.Date(as.yearqtr(landdata_fhfa_national_2016q1.df$OBS))
#class(dates)
#head(dates)
landdata_fhfa_national_2016q1.xts <- xts(dates, landdata_fhfa_national_2016q1.df[,1])
landdata_fhfa_national_2016q1.df$OBS <- dates
landdata_fhfa_national_2016q1.xts <- xts(landdata_fhfa_national_2016q1.df[,-1], landdata_fhfa_national_2016q1.df[,1])
#class(landdata_fhfa_national_2016q1.xts)
#head(landdata_fhfa_national_2016q1.xts)

# Now add variable labels
var.labels = c(LAND_NOM="Aggregate market value of residential land", MKVAL_NOM="Aggregate market value of homes", STRUC_NOM="Aggregate replacement cost of residential structures",
LAND_PI="Price index for residential land", MKVAL_PI="Price index for homes", STRUC_PI="Price index for residential structures", CONS_PI="Price index for consumption")
# Use the new function here


#var.labels
#class(var.labels)
#var.labels$LAND_NOM
#var.labels[LAND_NOM]
#LAND_NOM
#var.labels["LAND_NOM"]
#junk <- var.labels["LAND_NOM"]
#junk
#as.data.frame(var.labesl)
#as.data.frame(var.labels)
#as.data.frame(var.labels[,2])
#as.data.frame(var.labels[,2])
#as.data.frame(var.labels[,1])
#as.data.frame(var.labels[2])
#as.data.frame(var.labels[,1])
#as.data.frame(var.labels[,-1])
#colnames(landdata_fhfa_national_2016q1.xts)
#dim(var.labels)
#class(var.labels)
#var.labels[1]
#var.labels <- data.frame()
#var.labels

# colnames(var.labels)<- colnames(landdata_fhfa_national_2016q1.df)
# #colnames(landdata_fhfa_national_2016q1.df)
# var.labels = rbind("Aggregate market value of residential land", "Aggregate market value of homes", "Aggregate replacement cost of residential structures",
# "Price index for residential land", "Price index for homes", "Price index for residential structures", "Price index for consumption")
# #var.labels
# #var.labels <-data.frame()
# var.labels = cbind("Aggregate market value of residential land", "Aggregate market value of homes", "Aggregate replacement cost of residential structures",
# "Price index for residential land", "Price index for homes", "Price index for residential structures", "Price index for consumption")
# #var.labels
# colnames(landdata_fhfa_national_2016q1.df[2:7])
# colnames(landdata_fhfa_national_2016q1.df[1:7])
# colnames(var.labels)<-colnames(landdata_fhfa_national_2016q1.df[2:7])
# dim(var.labels)
# var.labels
# dim(landdata_fhfa_national_2016q1.df)
# colnames(var.labels)<-colnames(landdata_fhfa_national_2016q1.df[2:7])
# cns <- colnames(landdata_fhfa_national_2016q1.df[2:7])
# dim(cns)
# class(cns)
# as.vector(cns)
# colnames(landdata_fhfa_national_2016q1.xts)
# dim(colnames(landdata_fhfa_national_2016q1.xts))
# length(colnames(landdata_fhfa_national_2016q1.xts))
# dim(var.labels)
# colnames(var.labels)<-colnames(landdata_fhfa_national_2016q1.xts)
# var.labels
# names(landdata_fhfa_national_2016q1.xts)
# #head(landdata_fhfa_national_2016q1.xts)
# #class(landdata_fhfa_national_2016q1.df[,1])
# #class(landdata_fhfa_national_2016q1.df[,2:7])
# #class(landdata_fhfa_national_2016q1.df[,2])
# #class(landdata_fhfa_national_2016q1.df[,3])
# #class(landdata_fhfa_national_2016q1.df)
# #landdata_fhfa_national_2016q1.df



#PLOTTING BEGINS HERE
plot1 <- ggplot(landdata_fhfa_national_2016q1.df,aes(x=OBS,y=LAND_PI))
print(plot1)
plot1 <- ggplot(landdata_fhfa_national_2016q1.df,aes(x=Quarter,y="Land Price Index"))
print(plot1)
plot1 <- ggplot(landdata_fhfa_national_2016q1.df,aes(x=OBS, y=LAND_PI), xlab("Quarter"), ylab(var.labels$LAND_PI))
print(plot1)
print(plot1)
plot1 <- ggplot(landdata_fhfa_national_2016q1.df,aes(x=OBS, y=LAND_PI)) + theme_bw + xlab("Quarter") + ylab(var.labels$LAND_PI))
plot1 <- ggplot(landdata_fhfa_national_2016q1.df,aes(x=OBS, y=LAND_PI)) +
theme_bw() +
xlab("Quarter")+
ylab(var.labels$LAND_PI)
var.labels$LAND_PI
class(var.labels)
var.labels
var.labels <- as.data.frame(var.labels)
class(var.labels)
plot1 <- ggplot(landdata_fhfa_national_2016q1.df,aes(x=OBS, y=LAND_PI)) +
theme_bw() +
xlab("Quarter")+
ylab(var.labels$LAND_PI)
print(plot1)
# Regression bars
# Recession bars
recessions <- nberShade(plot1, fill = "#C1C1FF", xrange = c("1974-10-01", "2016-04-01"), openShade = TRUE)
print(recessions)
recessions <- nberShade(plot1, fill = "#C1C1FF", openShade = TRUE)
print(recessions)
landdata_fhfa_national_2016q1.df
class(landdata_fhfa_national_2016q1.df$OBS)
qplot(OBS,LAND_PI, data=landdata_fhfa_national_2016q1.df,geom="line")
qplot(OBS,LAND_PI, data=landdata_fhfa_national_2016q1.df,geom="line",xlab="Quarter",ylab="Index",main="Housing and Land Price Indices")
qplot(OBS,LAND_PI, data=landdata_fhfa_national_2016q1.df,geom="line",xlab="Quarter",ylab="Index",main="Housing and Land Price Indices",theme_bw())
qplot(OBS,LAND_PI, data=landdata_fhfa_national_2016q1.df,geom="line",xlab="Quarter",ylab="Index",main="Housing and Land Price Indices")+theme_bw()
qplot(OBS,LAND_PI, data=landdata_fhfa_national_2016q1.df,geom="line",xlab="Quarter",ylab="Index",main="Housing and Land Price Indices, 1975 - 2016")+theme_bw()
land_PI_plot <- qplot(OBS,LAND_PI, data=landdata_fhfa_national_2016q1.df,geom="line",xlab="Quarter",ylab="Index",main="Housing and Land Price Indices, 1975 - 2016")+theme_bw()
print(land_PI_plot)
summary(landdata_fhfa_national_2016q1.df)
describe(landdata_fhfa_national_2016q1.df)
str(landdata_fhfa_national_2016q1.df)
recessions
str(recessions)
recessions.plot <- nberShade(as.data.frame(plot1), fill = "#C1C1FF", openShade = TRUE)
str(landdata_fhfa_national_2016q1.xts)
str(landdata_fhfa_national_2016q1.df)
recessions.plot <- nberShade(landdata_fhfa_national_2016q1.df, fill = "#C1C1FF", openShade = TRUE)
plot1 <- ggplot(landdata_fhfa_national_2016q1.df,aes(x=OBS, y=LAND_PI)) +
theme_bw()+
ylim(c(0,5))+
geom_line(aes(y=LAND_PI), size=0.5, linetype=2) +
xlab("Quarter") +
ylab(var.labels$LAND_PI) +
opts(title="Price Index of Residential Land, 1975 - 2016")
plot1 <- ggplot(landdata_fhfa_national_2016q1.df,aes(x=OBS, y=LAND_PI)) +
theme_bw()+
ylim(c(0,5))+
geom_line(aes(y=LAND_PI), size=0.5, linetype=2) +
xlab("Quarter") +
ylab(var.labels$LAND_PI)+
opts(title="Price Index of Residential Land, 1975 - 2016")
library(timeDate)
install.packages(timeDate)
install.packages("timeDate")
library(timeDate)
plot1 <- ggplot(landdata_fhfa_national_2016q1.df,aes(x=OBS, y=LAND_PI)) +
theme_bw() +
ylim(c(0,10))+
plot1 <- ggplot(landdata_fhfa_national_2016q1.df,aes(x=OBS, y=LAND_PI)) +
theme_bw() +
ylim(c(0,10))+
geom_line(aes(y=LAND_PI), size=0.5, linetype=2) +
xlab("Quarter") +
ylab("Land Price Index") +
opts(title="Price Index of Residential Land, 1976 - 2012")
str(landdata_fhfa_national_2016q1.df)
nberDates()
nberShade()
library(tis)
nberDates
str(nberDates)
junk <- nberDates()
str(junk)
nberShade()
land_PI_plot
nberShade()
nberShade(land_PI_plot)
nberShade(land_PI_plot, xrange = c("1975-01-01", "2017-01-01"))
tail(landdata_fhfa_national_2016q1.df)
x <- nberDates()
x
library(help="tis")
ip <- as.data.frame(installed.packages()[,c(1,3:4)])
rownames(ip)<-NULL
ip<-ip[is.na(ip$Priority),1:2,drop=FALSE]
print(ip,row.names = FALSE)

# CHECK IF WE NEED ALL THESE AND WHY
install.packages(c("cluster", "codetools", "colorspace", "foreign", "ggplot2", "irlba", "lattice", "Matrix", "mgcv", "Rcpp", "reshape2", "scales", "stringi", "survival", "XML"))

savehistory("~/OneDrive for Marsh@uri.edu/OneDrive/Documents/Research-King Mac/Political Economy/Land Rent in the Circuit of Capital/Data Analysis/Land Values.Rhistory")