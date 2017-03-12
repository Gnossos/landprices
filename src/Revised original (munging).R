# Session to draw land value graph(s)
# See http://www.designandanalytics.com/american-beards-over-time-time-series-in-R-part-1  & part 2 for some fundamentals.
# Also see http://projecttemplate.net/getting_started.html on using the ProjectTemplate system

# Startup
setwd("~/OneDrive/Documents/Research-King Mac/Data Analysis/landprices")
#   ProjectTemplate will automatically load files in the data directory into data frames.
library("ProjectTemplate"); load.project(); # Let ProjectTemplate set things up for us.


#### PLOTTING BEGINS HERE ####

#### Theme selection & customization ####
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
  legend.position = c(0.25,.75),
  legend.position = c(1,0.01),
  
  # Titles go in the figure captions
  legend.title = element_blank()
  )

# CMS wants line size to be 1.5 for 8 x 10, but this seems too thick. Use 1.0.
update_geom_defaults("line", aes(size = 1.0))

#### base.plot: Create a base plot, which will be used as a starting point for all of them. ####
# Variables to be used throughout
base.breaks <- c("Land","Structures","Market", "Compensation", "Consumer") # Standard breaks for legends
base.colors <- c("Land"="#4daf4a", "Structures"="#ff7f00",
                 "Market"="#e41a1c", "Consumer"="#377eb8", "Compensation" = "#984ea3") # Standard colors for the different series.
base.linetypes <- c("Land"="solid", "Structures"="longdash",
                    "Market"="twodash", "Consumer"="dashed", "Compensation"= "dotdash")
base.labels <- c("Residential land", "Residential structures", "Dwellings",
                    "Wage & salary\ncompensation", "Consumer prices")

# The base plot itself
base.plot <- ggplot(data=CSW.national.2016q1.xts, aes(x=as.Date(index(CSW.national.2016q1.xts)))) # Establish the base plot

# Label the x axis
base.plot <- base.plot + xlab("Year") + # Label the x axis
  # And format it
  scale_x_date(labels = date_format("%Y"),
               breaks = date_breaks("2 years"), 
<<<<<<< HEAD
               limits = c(as.Date(index(first(CSW.national.2016q1.xts))),Sys.Date())
  )
=======
               limits = c(as.Date(index(CSW.national.2016q1.xts)[1]),Sys.Date()))
  ## Add recession bars and resave the plot so we have it for later.
  base.plot <- nberShade(base.plot, fill="#a6cee3")
>>>>>>> origin/master

  ## Add recession bars and resave the plot so we have it for later.
  base.plot
#  nberShade(base.plot, fill="#a6cee3")
  base.plot <- last_plot()

  
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
  ylab("Index of (1975:Q1 = 100)") + # Label the ordinate axis
  
  scale_y_continuous(labels=scales::comma) + # Format the ordinate axis labels

## Map the data to the geometric objects:
  geom_line(aes(y=LAND_index75, color="Land", linetype="Land")) +
  geom_line(aes(y=STRUC_index75, color="Structures", linetype="Structures")) +
  geom_line(aes(y=MKVAL_index75, color="Market", linetype="Market")) +
  geom_line(aes(y=CONS_index75, color="Consumer", linetype="Consumer")) +
  geom_line(data=wascur.fred.xts, aes(y=compensation_index75, color="Compensation", linetype="Compensation")) +
  theme(legend.position = c(.58,.45))

PI.plot # Comment/uncomment for testing
nberShade(PI.plot, fill="#a6cee3") + scale_y_continuous(labels=comma)
PI.plot <- last_plot()


### MV.plot: An area plot of market values
legend.title <- "Aggregate\nMarket Value of:"
MV.plot <- base.plot + # Start the plot

## Label the ordinate axis
  ylab("Aggregate Market Value (US$B)") +
  
  # scale_x_date(expand = c(0,0)) +
  
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

MV.plot # Comment/uncomment for testing
nberShade(MV.plot, fill="#a6cee3") + scale_y_continuous(labels=scales::dollar)
#                                                        expand = c(0,0,0.1,0)) # Label the ordinate axis
MV.plot <- last_plot()


### MV.plot2: An area plot of market values, using sum of land & structures
legend.title <- "Aggregate\nMarket Value of:"
MV.plot2 <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Aggregate Market Value (US$B)") +
  
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
nberShade(MV.plot2, fill="#a6cee3") + scale_y_continuous(labels=scales::dollar)

MV.plot2 <- last_plot()


### PIpct.plot: Plot the price indices as a percent of the consumer price index.
legend.title <- "Percent of\nConsumer Price index:"

PIpct.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Index as Percent of Consumer Price Index (1975:1 = 100") +
  
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
  geom_line(aes(y=(MKVAL_index75/CONS_index75), linetype = "Market", color="Market")) +
  geom_line(aes(y=(STRUC_index75/CONS_index75), linetype="Structures", color="Structures")) +
  geom_line(aes(y=(LAND_index75/CONS_index75), linetype="Land", color="Land")) +
  geom_line(aes(y=wascur.fred.xts$compensation_index75/CONS_index75,
                color="Compensation", linetype="Compensation")) +
  theme(legend.position = c(.58,.45))

PIpct.plot # Comment/uncomment for testing
nberShade(PIpct.plot, fill="#a6cee3") + scale_y_continuous(labels=percent_format()) # Label the ordinate axis
PIpct.plot <- last_plot()


### MVpct.plot: Plot aggregate land values as percent of aggregate Home value
MVpct.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  theme(axis.title.y = element_blank()) +
<<<<<<< HEAD
=======
  scale_y_continuous(labels=percent_format()) + # Label the ordinate axis
>>>>>>> origin/master
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = base.breaks,
                     values = base.colors,
                     labels = base.labels
  ) +
  
  ## Map the data to the geometric objects:
  geom_line(aes(y=(LAND_NOM/MKVAL_NOM), linetype="Land", color="Land"))

MVpct.plot <- MVpct.plot + theme(legend.position = "none")
<<<<<<< HEAD
nberShade(MVpct.plot, fill="#a6cee3") + scale_y_continuous(labels=percent_format()) # Label the ordinate axis
MVpct.plot <- last_plot() # Comment/uncomment for testing
=======
MVpct.plot # Comment/uncomment for testing
>>>>>>> origin/master


### ROC.plot: Plot the rate of change in the variable, as a percentage.
legend.title <- "Rates of Change\n(Year over Year)::"

ROC.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Percent Change in Total Value From Year Earlier") +
  
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
  
  # Draw a horizontal line at zero
  geom_hline(aes(yintercept=0)) +
  # ## Map the data to the geometric objects:
  geom_line(aes(y=(ROC(MKVAL_NOM, 4)), linetype = "Market", color="Market")) +
  geom_line(aes(y=(ROC(STRUC_NOM, 4)), linetype="Structures", color="Structures")) +
  geom_line(aes(y=(ROC(LAND_NOM, 4)), linetype="Land", color="Land")) +
  geom_line(data=wascur.fred.xts, aes(y=ROC(compensation,4),
            color="Compensation", linetype="Compensation")) +
  theme(legend.position = c(0.99,0.01))

ROC.plot # Comment/uncomment for testing
nberShade(ROC.plot, fill="#a6cee3") + scale_y_continuous(labels=percent_format()) # Label the ordinate axis
ROC.plot <- last_plot()


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
  geom_line(aes(y=(ROC(ROC(LAND_NOM, 4), 4)), linetype="Land", color="Land")) +
  geom_line(data=wascur.fred.xts, aes(y=(ROC(ROC(compensation,4),4)),
        linetype="Compensation", color="Compensation")) +
  theme(legend.position = c(0.15,0.75))

ROC2.plot # Comment/uncomment for testing
nberShade(ROC2.plot, fill="#a6cee3") + scale_y_continuous(labels=percent_format()) # Label the ordinate axis
ROC2.plot <- last_plot()


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
  geom_line(aes(y=(ROC(LAND_NOM, 4)/ROC(MKVAL_NOM, 4)), linetype="Land", color="Land")) +
  geom_line(data=wascur.fred.xts, aes(y=(ROC(compensation,4))/ROC(CSW.national.2016q1.xts$MKVAL_NOM,4),
                                      linetype="Compensation", color="Compensation")) +
  theme(legend.position = c(.15,.75))

RelROC.plot # Comment/uncomment for testing
nberShade(RelROC.plot, fill="#a6cee3") + scale_y_continuous(labels=percent_format()) # Label the ordinate axis
RelROC.plot <- last_plot()


### RelROC2.plot: Plot the ratio of the rate of change in the variable to the rate of change in home prices, as a percentage.
legend.title <- "Relative Rates of\nAcceleration\n(Year over Year):"

RelROC2.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Percent (%)") +
  
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
  geom_line(aes(y=(ROC(ROC(LAND_NOM, 4), 4)/ROC(ROC(MKVAL_NOM, 4), 4)), linetype="Land", color="Land")) +
  geom_line(data=wascur.fred.xts, aes(y=(ROC(ROC(compensation,4),4)/ROC(ROC(CSW.national.2016q1.xts$MKVAL_NOM,4),4)),
                                      linetype="Compensation", color="Compensation")) +
  theme(legend.position = c(.98,.01))

RelROC2.plot # Comment/uncomment for testing
nberShade(RelROC2.plot, fill="#a6cee3") + scale_y_continuous(labels=percent_format()) # Label the ordinate axis
RelROC2.plot <- last_plot()


### aggregates.plot: Plot aggregate land values, compensation, etc.
legend.title <- "Aggregate\nMarket Value of:"
aggregate.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Aggregate Market Value (US$B)") +
  scale_y_continuous(labels=scales::dollar) + # Label the ordinate axis
  
  ## Colors of lines and the legend keys
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
  
  ## Map the data to the geometric objects:
  geom_line(aes(y=LAND_NOM, color="Land", linetype="Land")) +
  geom_line(aes(y=STRUC_NOM, color="Structures", linetype="Structures")) +
  geom_line(aes(y=MKVAL_NOM, color="Market", linetype="Market")) +
  geom_line(data=wascur.fred.xts, aes(y=compensation,
          color="Compensation", linetype="Compensation")) +
  theme(legend.position = c(.98,.01))

<<<<<<< HEAD
aggregate.plot # Comment/uncomment for testing
nberShade(aggregate.plot, fill="#a6cee3") + scale_y_continuous(labels=percent_format()) # Label the ordinate axis
aggregate.plot <- last_plot()


### COMPpct.plot: Plot aggregate housing values as % of compensation
legend.title <- "Aggregate\nMarket Value of:"
COMPpct.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Value as Percent of Compensation (1975:1 = 100") +
  
  ## Colors of lines and the legend keys
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
  
  ## Map the data to the geometric objects:
  geom_line(aes(y=(LAND_NOM/wascur.fred.xts$compensation),
                color="Land", linetype="Land")) +
  geom_line(aes(y=(STRUC_NOM/wascur.fred.xts$compensation),
            color="Structures", linetype="Structures")) +
  geom_line(aes(y=(MKVAL_NOM/wascur.fred.xts$compensation),
            color="Market", linetype="Market")) +
  theme(legend.position = c(.15,.75))

COMPpct.plot # Comment/uncomment for testing
nberShade(COMPpct.plot, fill="#a6cee3") + scale_y_continuous(labels=percent_format()) # Label the ordinate axis
COMPpct.plot <- last_plot()


#### Per-capita plots
# The base plot itself
percapita.base.plot <- ggplot(data=percapita.xts, aes(x=as.Date(index(percapita.xts)))) # Establish the base plot

# Label the x axis
percapita.base.plot <- percapita.base.plot + xlab("Year") + # Label the x axis
  # And format it
  scale_x_date(labels = date_format("%Y"),
               breaks = date_breaks("2 years"), 
               limits = c(as.Date(index(first(percapita.xts))),Sys.Date())
  )

### aggregatePC.plot: Plot aggregate land values, compensation, etc. per capita
legend.title <- "Aggregate\nMarket Value Per Capita of:"
aggregatePC.plot <- percapita.base.plot + # Start the plot
  ## Label the ordinate axis
  ylab("Aggregate Market Value (US$B) Per Capita") +
  
  ## Colors of lines and the legend keys
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
  
  ## Map the data to the geometric objects:
  geom_line(aes(y=1e9*LAND_PC, color="Land", linetype="Land")) +
  geom_line(aes(y=1e9*STRUC_PC, color="Structures", linetype="Structures")) +
  geom_line(aes(y=1e9*MKVAL_PC, color="Market", linetype="Market")) +
  geom_line(data=1e9*percapita.xts, aes(y=compensation_PC,
                                    color="Compensation", linetype="Compensation")) +
  theme(legend.position = c(.99,.01))

aggregatePC.plot # Comment/uncomment for testing
nberShade(aggregatePC.plot, fill="#a6cee3") +  scale_y_continuous(labels=scales::dollar) # Label the ordinate axis
aggregatePC.plot <- last_plot()


### PC_index75.plot - Plot index numbers for all four series
PC_index75.plot <- percapita.base.plot + # Start the plot
  ## Label the ordinate axis
  ylab("Index (1975:1 = 100)") +
  
  ## Colors of lines and the legend keys
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
  
  scale_y_continuous(labels=scales::comma) + # Label the ordinate axis
  
  ## Map the data to the geometric objects:
  geom_line(aes(y=LAND_PC_index75, color="Land", linetype="Land")) +
  geom_line(aes(y=STRUC_PC_index75, color="Structures", linetype="Structures")) +
  geom_line(aes(y=MKVAL_PC_index75, color="Market", linetype="Market")) +
  geom_line(data=percapita.xts, aes(y=compensation_PC_index75,
                                    color="Compensation", linetype="Compensation")) +
  theme(legend.position = c(.99,.01))

  PC_index75.plot # Comment/uncomment for testing
  nberShade(PC_index75.plot, fill="#a6cee3") + scale_y_continuous(labels=comma) # Label the ordinate axis
  PC_index75.plot <- last_plot()
=======
# savehistory("~/OneDrive for Marsh@uri.edu/OneDrive/Documents/Research-King Mac/Political Economy/Land Rent in the Circuit of Capital/Data Analysis/Land Values.Rhistory")
>>>>>>> origin/master
