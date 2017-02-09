# Session to draw land value graph(s)
# See http://www.designandanalytics.com/american-beards-over-time-time-series-in-R-part-1  & part 2 for some fundamentals.


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

### Theme selection & customization
original_theme <- theme_set(theme_gray()) # The grey theme seems closest to CMS specifications.
# Initialize current theme, with CMS specifications for 8 x 10 figure.
this_theme <- theme_set(theme_gray(base_size = 14, base_family = "Helvetica"))
theme_set(this_theme) 
this_theme <- theme_update(
  # CMS wants line weights of 1.5, but they're gross. Be more delicate.
  axis.line = element_line(size=0.5),
  axis.ticks = element_line(size=0.5),
  # Position the legend left-justified and in the lower right corner of the plot
  legend.justification = c(1,0),
  legend.position = c(1,0.01),
  
  # Titles go in the figure captions
  legend.title = element_blank()
  )

# CMS wants line size to be 1.5 for 8 x 10, but this seems too thick. Use 1.0.
update_geom_defaults("line", aes(size = 1.0))

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
  
  # scale_x_date(expand = c(0,0)) +
  
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
  theme(axis.title.y = element_blank()) +
  scale_y_continuous(labels=percent_format()) + # Label the ordinate axis
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = base.breaks,
                     values = base.colors,
                     labels = base.labels
  ) +
  
  ## Map the data to the geometric objects:
  geom_line(aes(y=(LAND_NOM/MKVAL_NOM), linetype="Land", color="Land"))

MVpct.plot <- MVpct.plot + theme(legend.position = "none")
MVpct.plot # Comment/uncomment for testing


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
