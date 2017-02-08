# Session to draw land value graph(s)
# See http://www.designandanalytics.com/american-beards-over-time-time-series-in-R-part-1  & part 2 for some fundamentals
<<<<<<< HEAD


### Initialize the data as a data frame
#   ProjectTemplate will automatically load files in the data directory into data frames.
library("ProjectTemplate"); load.project(); # Let ProjectTemplate set things up for us.

# Now the real work begins

# ProjectTemplate should take care of these.
# library(XML) # To read .zoo.
# library(xts) # To store time series
# library(tis) # For recession shading
# library(ggplot2) # For finished (artsy) plotting
# library(gridExtra) # For captions
# library(ggfortify) # For plotting time series
# library(tis) # For plotting NBER recession bars
# library(scales) # For formatting various axes in various scales
# library(TTR) # Useful functions for analyzing time-series financial data.

### MUNGING BEGINS HERE ###
# Reference: https://rstudio-pubs-static.s3.amazonaws.com/40873_5fbe3860854a47c38a58aabd01f9cf9d.html

## Land & Housing Data
# Source: http://datatoolkits.lincolninst.edu/subcenters/land-values/price-and-quantity.asp
# Make the original data frame into an xts time series. Xts requires a date variable.
# So convert the first column (variable) to a yearqtr object.
# NOTE: This only works with the CSW and FHFA files. The Historical file is annual and therefore requires as.year
#   instead of as.yearqtr.
CSW.national.2016q1.xts <- xts(CSW.national.2016q1[,-1], order.by = as.yearqtr(CSW.national.2016q1[,1]))

# Add variable labels
xtsAttributes(CSW.national.2016q1.xts) <- list(
  LAND_NOM="Aggregate market value of residential land",
  MKVAL_NOM="Aggregate market value of homes",
  STRUC_NOM="Aggregate replacement cost of residential structures",
  LAND_PI="Price index for residential land",
  MKVAL_PI="Price index for homes",
  STRUC_PI="Price index for residential structures",
  CONS_PI="Price index for consumption"
)

## Compensation Data
# Source: https://fred.stlouisfed.org/series/A576RC1 from FRED.
# Strategy: Original data are monthly, so (1) convert to .xts object and make them quarterly.
#   Once that's done, (2) convert back to a data frame, and then convert that back to an .xts object
#   indexed as yearqtr so its compatible with the housing & land data. Finally, (3) extract just the
#   years for which the two datasets overlap and (4) create a comparable index for compensation

## Step 1. Convert compensation data to quarterly, using the mean of the months in each quarter. 
monthly.compensation.xts <- as.xts(A576RC1[,-1], order.by = as.Date(A576RC1[,1]))
# Now convert it from monthly to quarterly, using the monthly average over the quarter
quarter.end <- endpoints(monthly.compensation.xts, on="quarters")
temp.xts <- period.apply(compensation.xts, INDEX = quarter.end, FUN = mean)

# Step 2: Convert to data frame and then to .xts object in order to make the time unit yearqtr.
temp.df <- data.frame(date=index(temp.xts), coredata(temp.xts))
temp2.xts <- as.xts(temp.df[, !(names(temp.df) %in% "date")],
                    dateFormat = "yearqtr", order.by = as.yearqtr(temp.df[,"date"]))

# Step 3: Exctract years overlapping with housing/land data
quarterly.compensation.xts <- temp2.xts[index(CSW.national.2016q1.xts)]

# Step 4: Make a price index set at 1975:1 = 1.
colnames(quarterly.compensation.xts) = "A576RC1"
quarterly.compensation.xts$COMP_PI <- quarterly.compensation.xts$A576RC1/rep_len(
  quarterly.compensation.xts[as.yearqtr("2000 Q2"),1], length(quarterly.compensation.xts$A576RC1))

# Finally, add some variable labels
xtsAttributes(quarterly.compensation.xts) <- list(
  A576RC1="Compensation of employees, received: wage and salary disbursements",
  COMP_PI="Price index for employee compensation"
)

#### PLOTTING BEGINS HERE ####

#### base.plot: Create a base plot, which will be used as a starting point for all of them.
# Variables to be used throughout
base.breaks <- c("Land","Structures","Market", "Compensation", "Consumer") # Standard breaks for legends
base.colors <- c("Land"="#4daf4a", "Structures"="#ff7f00",
                 "Market"="#e41a1c", "Consumer"="#377eb8", "Compensation" = "#984ea3") # Standard colors for the different series.
base.linetypes <- c("Land"="solid", "Structures"="longdash",
                    "Market"="twodash", "Consumer"="dashed", "Compensation"= "dotdash")
base.labels <- c("Residential land", "Residential structures", "Homes",
                    "Wage & salary\ncompensation", "Consumer prices")

base.plot <- ggplot(data=CSW.national.2016q1.xts, # The data to be used
                    aes(x=as.Date(index(CSW.national.2016q1.xts)))) + # The horizontal variable
  xlab("Year") + # Label the x axis
  # And format it
  scale_x_date(labels = date_format("%Y"),
               breaks = date_breaks("2 years"), 
               limits = c(as.Date(index(CSW.national.2016q1.xts)[1]),Sys.Date()))
  ## Add recession bars and resave the plot so we have it for later.
  base.plot <- nberShade(base.plot, fill="#a6cee3")
  base.plot <- base.plot + theme(
    legend.justification = c(1,0),
    legend.position = c(1,0),
    legend.title = element_blank()
    )


### PI.plot: A line chart of price indices
legend.title <- "Index of:" # Make life a bit easier by storing the legend title as a string
PI.plot <- base.plot + # Start the plot
  # CONVERT THESE TWO STATEMENTS INTO FUNCTIONS TO ENSURE CONSISTENCY
  ## Coordinate colors of the geometric objects and the legend keys
  scale_color_manual(legend.title,
                     breaks = base.breaks,
                     values=base.colors,
                     labels=base.labels,
                     guide = guide_legend()
  ) +
  ## Coordinate line styles
  scale_linetype_manual(legend.title,
                        breaks = base.breaks,
                        values = base.linetypes,
                        labels = base.labels,
                        guide = guide_legend()
  ) +
  
## Label the ordinate axis
  ylab("Index of (2000:Q2 = 100)") + # Label the ordinate axis

## Map the data to the geometric objects:
  geom_line(aes(y=LAND_PI*100, color="Land", linetype="Land")) +
  geom_line(aes(y=STRUC_PI*100, color="Structures", linetype="Structures")) +
  geom_line(aes(y=MKVAL_PI*100, color="Market", linetype="Market")) +
  geom_line(aes(y=CONS_PI*100, color="Consumer", linetype="Consumer")) +
  geom_line(data=quarterly.compensation.xts, aes(y=COMP_PI*100, color="Compensation", linetype="Compensation"))

PI.plot # Comment/uncomment for testing


### MV.plot: An area plot of market values
legend.title <- "Aggregate\nMarket Value of:"
MV.plot <- base.plot + # Start the plot

## Label the ordinate axis
  ylab("Aggregate Market Value (US$B)") +
  scale_y_continuous(labels=scales::dollar,
                     expand = c(0,0,0.1,0)) + # Label the ordinate axis
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = base.breaks,
                     values = base.colors,
                     labels = base.labels,
                     guide = guide_legend()
  ) +
  ## Colors of areas and the legend keys
  scale_fill_manual(legend.title,
                    breaks = base.breaks,
                    values = base.colors,
                    labels = base.labels,
                    guide = guide_legend()
  ) +
  ## Line styles
  scale_linetype_manual(legend.title,
                        breaks = base.breaks,
                        values = base.linetypes,
                        labels = base.labels,
                        guide = guide_legend()
  ) +
  
  # Expand the plotted area
  # scale_x_date(expand = c(0,0)) +

## Map the data to the geometric objects:
  geom_area(aes(y=MKVAL_NOM, fill = "Market", color="black")) +
  geom_area(aes(y=STRUC_NOM, fill="Structures", color="black")) +
  geom_area(aes(y=LAND_NOM, fill="Land", color="black"))

MV.plot <- MV.plot + theme(
  legend.justification = c(1,0),
  legend.position = c(0.931,0.0)
)

MV.plot # Comment/uncomment for testing


### MV.plot2: An area plot of market values, using sum of land & structures
legend.title <- "Aggregate\nMarket Value of:"
MV.plot2 <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Aggregate Market Value (US$B)") +
  scale_y_continuous(labels=scales::dollar) + # Label the ordinate axis
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = base.breaks,
                     values = base.colors,
                     labels = base.labels,
                     guide = guide_legend()
  ) +
  ## Colors of areas and the legend keys
  scale_fill_manual(legend.title,
                    breaks = base.breaks,
                    values = base.colors,
                    labels = base.labels,
                    guide = guide_legend()
  ) +
  ## Line styles
  scale_linetype_manual(legend.title,
                        breaks = base.breaks,
                        values = base.linetypes,
                        labels = base.labels,
                        guide = guide_legend()
  ) +
  
  ## Map the data to the geometric objects:
  geom_area(aes(y=LAND_NOM + STRUC_NOM, fill="Market", color="black")) +
  geom_area(aes(y=(LAND_NOM), fill="Land", color="black"))

MV.plot2 # Comment/uncomment for testing


### PIpct.plot: Plot the price indices as a percent of the consumer price index.
legend.title <- "Percent of\nConsumer Price index:"

PIpct.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Index as Percent of Consumer Price Index (2000:Q2 = 100") +
  scale_y_continuous(labels=percent_format()) + # Label the ordinate axis
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = base.breaks,
                     values = base.colors,
                     labels = base.labels,
                     guide = guide_legend()
  ) +
  ## Colors of areas and the legend keys
  scale_fill_manual(legend.title,
                    breaks = base.breaks,
                    values = base.colors,
                    labels = base.labels,
                    guide = guide_legend()
  ) +
  ## Line styles
  scale_linetype_manual(legend.title,
                        breaks = base.breaks,
                        values = base.linetypes,
                        labels = base.labels,
                        guide = guide_legend()
  ) +
  
  ## Map the data to the geometric objects:
  geom_line(aes(y=(MKVAL_PI/CONS_PI), linetype = "Market", color="Market")) +
  geom_line(aes(y=(STRUC_PI/CONS_PI), linetype="Structures", color="Structures")) +
  geom_line(aes(y=(LAND_PI/CONS_PI), linetype="Land", color="Land")) +
  geom_line(aes(y=(CONS_PI/CONS_PI), linetype="Consumer", color="Consumer")) +
  geom_line(aes(y=quarterly.compensation.xts$COMP_PI/CONS_PI, color="Compensation", linetype="Compensation"))

PIpct.plot # Comment/uncomment for testing


### MVpct.plot: Plot aggregate land values as percent of aggregate Home value

MVpct.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Percent (%)") +
  scale_y_continuous(labels=percent_format()) + # Label the ordinate axis
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = base.breaks,
                     values = base.colors,
                     labels = base.labels
  ) +
  
  ## Map the data to the geometric objects:
  geom_line(aes(y=(LAND_NOM/MKVAL_NOM), linetype="Land", color="Land"))

MVpct.plot + theme(legend.position = "none")# Comment/uncomment for testing


### ROC.plot: Plot the rate of change in the variable, as a percentage.
legend.title <- "Rates of Change\n(Year over Year)::"

ROC.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Percent Change from Year Earlier") +
  scale_y_continuous(labels=percent_format()) + # Label the ordinate axis
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = base.breaks,
                     values = base.colors,
                     labels = base.labels,
                     guide = guide_legend()
  ) +
  ## Colors of areas and the legend keys
  scale_fill_manual(legend.title,
                    breaks = base.breaks,
                    values = base.colors,
                    labels = base.labels,
                    guide = guide_legend()
  ) +
  ## Line styles
  scale_linetype_manual(legend.title,
                        breaks = base.breaks,
                        values = base.linetypes,
                        labels = base.labels,
                        guide = guide_legend()
  ) +
  
  # ## Map the data to the geometric objects:
  
  geom_line(aes(y=(ROC(MKVAL_NOM, 4)), linetype = "Market", color="Market")) +
  geom_line(aes(y=(ROC(STRUC_NOM, 4)), linetype="Structures", color="Structures")) +
  geom_line(aes(y=(ROC(LAND_NOM, 4)), linetype="Land", color="Land")) +
  geom_line(aes(y=(ROC(CONS_PI, 4)), linetype="Consumer", color="Consumer")) +
  geom_line(data=quarterly.compensation.xts, aes(y=ROC(A576RC1,4),
            color="Compensation", linetype="Compensation"))

ROC.plot # Comment/uncomment for testing


### ROC2.plot: Acceleration in the Rates of Change, Year over Year.
legend.title <- "Acceleration Rates of Change\n(Year over Year)::"

ROC2.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Percent (%)") +
  scale_y_continuous(labels=percent_format()) + # Label the ordinate axis
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = base.breaks,
                     values = base.colors,
                     labels = base.labels,
                     guide = guide_legend()
  ) +
  ## Colors of areas and the legend keys
  scale_fill_manual(legend.title,
                    breaks = base.breaks,
                    values = base.colors,
                    labels = base.labels,
                    guide = guide_legend()
  ) +
  ## Line styles
  scale_linetype_manual(legend.title,
                        breaks = base.breaks,
                        values = base.linetypes,
                        labels = base.labels,
                        guide = guide_legend()
  ) +
  
  # ## Map the data to the geometric objects:
  
  geom_line(aes(y=(ROC(ROC(MKVAL_NOM, 4),4)), linetype = "Market", color="Market")) +
  geom_line(aes(y=(ROC(ROC(STRUC_NOM, 4),4)), linetype="Structures", color="Structures")) +
  geom_line(aes(y=(ROC(ROC(LAND_NOM, 4), 4)), linetype="Land", color="Land"))

ROC2.plot # Comment/uncomment for testing


### RelROC.plot: Plot the ratio of the rate of change in the variable to the rate of change in home prices, as a percentage.
legend.title <- "Relative Rates of Change\n(Year over Year)::"

RelROC.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Percent (%)") +
  scale_y_continuous(labels=percent_format()) + # Label the ordinate axis
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = base.breaks,
                     values = base.colors,
                     labels = base.labels,
                     guide = guide_legend()
  ) +
  ## Colors of areas and the legend keys
  scale_fill_manual(legend.title,
                    breaks = base.breaks,
                    values = base.colors,
                    labels = base.labels,
                    guide = guide_legend()
  ) +
  ## Line styles
  scale_linetype_manual(legend.title,
                        breaks = base.breaks,
                        values = base.linetypes,
                        labels = base.labels,
                        guide = guide_legend()
  ) +
  
  # ## Map the data to the geometric objects:

  geom_line(aes(y=(ROC(MKVAL_NOM, 4)/ROC(MKVAL_NOM, 4)), linetype = "Market", color="Market")) +
  geom_line(aes(y=(ROC(STRUC_NOM, 4)/ROC(MKVAL_NOM, 4)), linetype="Structures", color="Structures")) +
  geom_line(aes(y=(ROC(LAND_NOM, 4)/ROC(MKVAL_NOM, 4)), linetype="Land", color="Land"))

RelROC.plot # Comment/uncomment for testing


### RelROC2.plot: Plot the ratio of the rate of change in the variable to the rate of change in home prices, as a percentage.
legend.title <- "Relative Rates of\nAcceleration\n(Year over Year):"

RelROC2.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Percent (%)") +
  scale_y_continuous(labels=percent_format()) + # Label the ordinate axis
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = base.breaks,
                     values = base.colors,
                     labels = base.labels,
                     guide = guide_legend()
  ) +
  ## Colors of areas and the legend keys
  scale_fill_manual(legend.title,
                    breaks = base.breaks,
                    values = base.colors,
                    labels = base.labels,
                    guide = guide_legend()
  ) +
  ## Line styles
  scale_linetype_manual(legend.title,
                        breaks = base.breaks,
                        values = base.linetypes,
                        labels = base.labels,
                        guide = guide_legend()
  ) +
  
  # ## Map the data to the geometric objects:
  
  geom_line(aes(y=(ROC(ROC(MKVAL_NOM, 4), 4)/ROC(ROC(MKVAL_NOM, 4), 4)), linetype = "Market", color="Market")) +
  geom_line(aes(y=(ROC(ROC(STRUC_NOM, 4), 4)/ROC(ROC(MKVAL_NOM, 4), 4)), linetype="Structures", color="Structures")) +
  geom_line(aes(y=(ROC(ROC(LAND_NOM, 4), 4)/ROC(ROC(MKVAL_NOM, 4), 4)), linetype="Land", color="Land"))

RelROC2.plot # Comment/uncomment for testing


# CHECK IF WE NEED ALL THESE AND WHY
# install.packages(c("cluster", "codetools", "colorspace", "foreign", "irlba", "lattice", "Matrix", "mgcv", "Rcpp", "reshape2", "scales", "stringi", "survival", "XML"))

# savehistory("~/OneDrive for Marsh@uri.edu/OneDrive/Documents/Research-King Mac/Political Economy/Land Rent in the Circuit of Capital/Data Analysis/Land Values.Rhistory")
=======
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

# Now the real work begins
library('ProjectTemplate');load.project(); # Do the project's magic

# ProjectTemplate should take care of these. CHECK HOW.
library(XML) # To read .zoo.
library(xts) # To store time series
library(tis) # For recession shading
library(ggplot2) # For finished (artsy) plotting
library(gridExtra) # For captions

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

colnames(var.labels)<- colnames(landdata_fhfa_national_2016q1.df)
#colnames(landdata_fhfa_national_2016q1.df)
var.labels = rbind("Aggregate market value of residential land", "Aggregate market value of homes", "Aggregate replacement cost of residential structures",
"Price index for residential land", "Price index for homes", "Price index for residential structures", "Price index for consumption")
#var.labels
#var.labels <-data.frame()
var.labels = cbind("Aggregate market value of residential land", "Aggregate market value of homes", "Aggregate replacement cost of residential structures",
"Price index for residential land", "Price index for homes", "Price index for residential structures", "Price index for consumption")
#var.labels
colnames(landdata_fhfa_national_2016q1.df[2:7])
colnames(landdata_fhfa_national_2016q1.df[1:7])
colnames(var.labels)<-colnames(landdata_fhfa_national_2016q1.df[2:7])
dim(var.labels)
var.labels
dim(landdata_fhfa_national_2016q1.df)
colnames(var.labels)<-colnames(landdata_fhfa_national_2016q1.df[2:7])
cns <- colnames(landdata_fhfa_national_2016q1.df[2:7])
dim(cns)
class(cns)
as.vector(cns)
colnames(landdata_fhfa_national_2016q1.xts)
dim(colnames(landdata_fhfa_national_2016q1.xts))
length(colnames(landdata_fhfa_national_2016q1.xts))
dim(var.labels)
colnames(var.labels)<-colnames(landdata_fhfa_national_2016q1.xts)
var.labels
names(landdata_fhfa_national_2016q1.xts)
#head(landdata_fhfa_national_2016q1.xts)
#class(landdata_fhfa_national_2016q1.df[,1])
#class(landdata_fhfa_national_2016q1.df[,2:7])
#class(landdata_fhfa_national_2016q1.df[,2])
#class(landdata_fhfa_national_2016q1.df[,3])
#class(landdata_fhfa_national_2016q1.df)
#landdata_fhfa_national_2016q1.df



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
, 1975 - 2016
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
install.packages(c("colorspace", "ggplot2", "irlba", "Rcpp", "reshape2", "scales", "stringi", "XML"))
install.packages(c("colorspace", "ggplot2", "irlba", "Rcpp",
install.packages(c("colorspace", "ggplot2", "irlba", "Rcpp", "reshape2", "scales", "stringi", "XML"))
install.packages(c("colorspace", "ggplot2", "irlba", "Rcpp",
"reshape2", "scales", "stringi", "XML"))
install.packages(c("colorspace", "ggplot2", "irlba", "Rcpp",
install.packages(c("cluster", "codetools", "colorspace", "foreign", "ggplot2", "irlba", "lattice", "Matrix", "mgcv", "Rcpp", "reshape2", "scales", "stringi", "survival", "XML"))
install.packages(c("cluster", "codetools", "colorspace", "foreign",
install.packages(c("cluster", "codetools", "colorspace", "foreign", "ggplot2", "irlba", "lattice", "Matrix", "mgcv", "Rcpp", "reshape2", "scales", "stringi", "survival", "XML"))
install.packages(c("cluster", "codetools", "colorspace", "foreign",
install.packages(c("colorspace", "ggplot2", "irlba", "mgcv", "Rcpp", "reshape2", "scales", "stringi", "survival", "XML"))
install.packages(c("colorspace", "ggplot2", "irlba", "mgcv",
install.packages()
install.packages()[1:5,]
install.packages(base)[1:5,]
"base64"
installed.packages()
demo()
help.start()
install.packages(knitr)
library(knitr)
savehistory("~/OneDrive for Marsh@uri.edu/OneDrive/Documents/Research-King Mac/Political Economy/Land Rent in the Circuit of Capital/Data Analysis/Land Values.Rhistory")
>>>>>>> 0ff9b4e30d21b0867aa9f6861fae0b1a0e5c12c4
