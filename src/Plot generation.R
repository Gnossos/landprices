# Session to draw land value graph(s)
# See http://www.designandanalytics.com/american-beards-over-time-time-series-in-R-part-1  & part 2 for some fundamentals.
# Also see http://projecttemplate.net/getting_started.html on using the ProjectTemplate system

# Startup
setwd("~/OneDrive/Documents/Research-King Mac/Data Analysis/landprices")
#   ProjectTemplate will automatically load files in the data directory into data frames.
library("ProjectTemplate"); load.project(); # Let ProjectTemplate set things up for us.

#### PRELIMINARIES ####
# Theme selection & customization ####
original_theme <- theme_set(theme_gray()) # The grey theme seems closest to CMS specifications.
# Initialize current theme, with CMS specifications for 8 x 10 figure.
this_theme <- theme_set(theme_gray(landprices_size = 14, landprices_family = "Helvetica"))
theme_set(this_theme) 
this_theme <- theme_update(
  # CMS wants line weights of 1.5, but they're gross. Be more delicate.
  axis.line = element_line(size=0.5),
  axis.ticks = element_line(size=0.5),
  # Position the legend left-justified and by default located in the lower right corner of the plot
  legend.justification = c(1,0),
  legend.position = c(0.25,.75),
  
  # Titles go in the figure captions
  legend.title = element_blank()
  )

# Look & Feel: Standardize aesthetic definitions of breaks, colors, line types, and labels. ====
# CMS wants line size to be 1.5 for 8 x 10, but this seems too thick. Use 1.0.
update_geom_defaults("line", aes(size = 1.0))
# Variables to be used throughout
require(RColorBrewer)
landprices_breaks <- c("Land","Structures","Market", "Compensation", "Consumer") # Standard breaks for legends
landprices_colors <- c( # Standard colors for the different series.
  "Land"= brewer.pal(8, "Dark2")[5],
  "Structures" = brewer.pal(8, "Dark2")[2],
  "Market" = brewer.pal(8, "Dark2")[7], 
  "Consumer" = brewer.pal(8, "Dark2") [3], 
  "Compensation" = brewer.pal(8, "Dark2")[1]
  ) 
landprices_linetypes <- c( # Line type associated with the different kinds of series
  "Land"="solid",
  "Structures"="longdash",
  "Market"="twodash",
  "Consumer"="dashed",
  "Compensation"= "dotdash",
  "Trend" = "8323"
  )
landprices_labels <- c(
  "Residential land",
  "Residential structures", 
  "Homes",
  "Wage & salary\ncompensation", 
  "Consumer prices"
  )

# base.plot: Create a base plot, which will be used as a starting point for all of them. ====
default_data.xts <- CSW.national.xts

# base.plot <- ggplot(data = fortify(CSW.national.xts, melt = TRUE),
#                     aes(x = Index, y = Value),
#                     CSW.national.xts) # Establish the plot based on original data
               
CSW.national.df <- data.frame(date = as.Date(index(CSW.national.xts)), coredata(CSW.national.xts))

base.plot <- ggplot(CSW.national.df, aes())

# Label the x axis
base.plot <- base.plot + xlab("") # Don't label the x axis -- Decided not to label it. It's redundant.
  # And format it
base.plot <- base.plot + scale_x_date(
  labels = date_format("%Y"),
  breaks = date_breaks("2 years"), 
  limits = c(as.Date(index(first(CSW.national.xts))),Sys.Date())
  )

  base.plot <- base.plot + ylab("Aggregate Market Value (US$B)")

# scale_x_date(expand = c(0,0))

  ## Colors of lines and the legend keys
  base.plot <- base.plot + scale_color_manual(
    breaks = landprices_breaks,
    values = landprices_colors,
    labels = landprices_labels,
    guide = guide_legend()
  )
  ## Colors of areas and the legend keys
  base.plot <- base.plot + scale_fill_manual(
    breaks = landprices_breaks,
    values = landprices_colors,
    labels = landprices_labels,
    guide = guide_legend()
  )
  
  ## Line styles
  base.plot <- base.plot + scale_linetype_manual(
    breaks = landprices_breaks,
    values = landprices_linetypes,
    labels = landprices_labels,
    guide = guide_legend()
  )

  base.plot # Comment out so as not to draw.
  

  # END OF PRELIMINARIES

# NOMINAL VALUE PLOTS ======================================================
# Uses current dollars
  
# Nominal Value Area Plots -------------------------------------------------
# 
# The aesthetics of the filled areas
  fill_layers <- c(
    geom_area(aes(x= date, y=MKVAL, fill = "Market", color="black")),
    geom_area(aes(x= date, y=LAND + STRUC, fill="Structures", color="black")), # To stack them
    geom_area(aes(x= date, y=LAND, fill="Land", color="black")),
    geom_area(aes(x= date, y=STRUC, fill="Structures", color="black"))
  )

### NOM_plot2: Area plot of market values using only Residential Land and Home values ====
legend.title <- "Aggregate\nMarket Value of:"
  
NOM_plot2 <- base.plot # Start the plot
  NOM_plot2 <- NOM_plot2 + 
    scale_x_date(
      expand = c(0,0),
      labels = date_format("%Y"),
      breaks = date_breaks("2 years") 
    )
  
  ## Map the data to the geometric objects:
  NOM_plot2 <- NOM_plot2 + fill_layers[1] + fill_layers[3]
  NOM_plot2 <- NOM_plot2 + recession_bars()
  NOM_plot2 <- NOM_plot2 + scale_y_continuous(labels = comma) + 
    theme(legend.position = c(0.995, 0.05))
NOM_plot2

### NOM_plot3: Area plot of market values, using sum of land & structures ====
NOM_plot3 <- NOM_plot2

  ## Map the data to the geometric objects:
  NOM_plot3$layers <- NULL
  NOM_plot3 <- NOM_plot3 + fill_layers[1] + fill_layers[4] + fill_layers[3]
  NOM_plot3 <- NOM_plot3 + recession_bars()
NOM_plot3


### NOM_pct_plot: Plot aggregate land values as percent of aggregate Home value ====

# These things are tenative
formula = y ~ x
xx <- seq(from = 0, to = ((nrow(CSW.national.df)-1)/4), by = 0.25)
fit <- lm((100 * CSW.national.df$LAND/CSW.national.df$MKVAL) ~ xx)


NOM_pct_plot <- base.plot # Start the plot
  
  ## Reset vertical axis labels
  NOM_pct_plot <- NOM_pct_plot + 
    ylab("Aggregate Land Value Relative to House Value") +
    scale_y_continuous(labels = percent_format())
  
  ## Map the data and a trend line to the geometric objects:
  NOM_pct_plot <- NOM_pct_plot + geom_line(aes(x = date, y = (LAND/MKVAL), linetype = "Land", color="Land"))
  NOM_pct_plot <- NOM_pct_plot + 
    geom_smooth(aes(x = date, y=(LAND/MKVAL), color = "Land", linetype = "Trend"),
          method = "lm", formula = formula, se = FALSE, size = 0.5)
  NOM_pct_plot <- NOM_pct_plot +
    stat_poly_eq(
      aes(x = date, y = (LAND/MKVAL), label = ..eq.label..),
      label.x.npc = .95, label.y.npc = .85,
      formula = formula,
      # color = landprices_colors$Land,
      parse = TRUE)
  NOM_pct_plot <- NOM_pct_plot + recession_bars()
    
  NOM_pct_plot <- NOM_pct_plot + scale_linetype_manual(values = landprices_linetypes, guide = "none")
  NOM_pct_plot <- NOM_pct_plot + theme(legend.position = "none") # Have to add this here because nberShade forces legends.
  NOM_pct_plot # Comment this out if not wanted



# END OF MARKET VALUE PLOTS
 
 







# # PLOTS OF PRICE INDICES --------------------------------------------------
### PI.plot: A line chart of housing price indices ====
legend.title <- "Index of:" # Make life a bit easier by storing the legend title as a string
PI.plot <- base.plot + # Start the plot
  # CONVERT THESE TWO STATEMENTS INTO FUNCTIONS TO ENSURE CONSISTENCY
  ## Coordinate colors of the geometric objects and the legend keys
  scale_color_manual(legend.title,
                     breaks = landprices_breaks,
                     values=landprices_colors,
                     labels=landprices_labels,
                     guide = guide_legend()
  ) +
  ## Coordinate line styles
  scale_linetype_manual(legend.title,
                        breaks = landprices_breaks,
                        values = landprices_linetypes,
                        labels = landprices_labels,
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

### PIpct.plot: Plot the price indices as a percent of the consumer price index. ====
legend.title <- "Percent of\nConsumer Price index:"

PIpct.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Index as Percent of Consumer Price Index (1975:1 = 100") +
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = landprices_breaks,
                     values = landprices_colors,
                     labels = landprices_labels,
                     guide = guide_legend()
  ) +
  ## Colors of areas and the legend keys
  scale_fill_manual(legend.title,
                    breaks = landprices_breaks,
                    values = landprices_colors,
                    labels = landprices_labels,
                    guide = guide_legend()
  ) +
  ## Line styles
  scale_linetype_manual(legend.title,
                        breaks = landprices_breaks,
                        values = landprices_linetypes,
                        labels = landprices_labels,
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


# END OF PLOTS OF PRICE INDICES



# RATES OF CHANGE ---------------------------------------------------------

### ROC.plot: Plot the rate of change in the variable, as a percentage. ====
legend.title <- "Rates of Change\n(Year over Year)::"

ROC.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Percent Change in Total Value From Year Earlier") +
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = landprices_breaks,
                     values = landprices_colors,
                     labels = landprices_labels,
                     guide = guide_legend()
  ) +
  ## Colors of areas and the legend keys
  scale_fill_manual(legend.title,
                    breaks = landprices_breaks,
                    values = landprices_colors,
                    labels = landprices_labels,
                    guide = guide_legend()
  ) +
  ## Line styles
  scale_linetype_manual(legend.title,
                        breaks = landprices_breaks,
                        values = landprices_linetypes,
                        labels = landprices_labels,
                        guide = guide_legend()
  ) +
  
  # Draw a horizontal line at zero
  geom_hline(aes(yintercept=0)) +
  # ## Map the data to the geometric objects:
  geom_line(aes(y=(ROC(MKVAL, 4)), linetype = "Market", color="Market")) +
  geom_line(aes(y=(ROC(STRUC, 4)), linetype="Structures", color="Structures")) +
  geom_line(aes(y=(ROC(LAND, 4)), linetype="Land", color="Land")) +
  geom_line(data=wascur.fred.xts, aes(y=ROC(compensation,4),
            color="Compensation", linetype="Compensation")) +
  theme(legend.position = c(0.99,0.01))

ROC.plot # Comment/uncomment for testing
nberShade(ROC.plot, fill="#a6cee3") + scale_y_continuous(labels=percent_format()) # Label the ordinate axis
ROC.plot <- last_plot()


### ROC2.plot: Acceleration in the Rates of Change, Year over Year. ====
legend.title <- "Acceleration Rates of Change\n(Year over Year)::"

ROC2.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Percent (%)") +
  scale_y_continuous(labels=percent_format()) + # Label the ordinate axis
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = landprices_breaks,
                     values = landprices_colors,
                     labels = landprices_labels,
                     guide = guide_legend()
  ) +
  ## Colors of areas and the legend keys
  scale_fill_manual(legend.title,
                    breaks = landprices_breaks,
                    values = landprices_colors,
                    labels = landprices_labels,
                    guide = guide_legend()
  ) +
  ## Line styles
  scale_linetype_manual(legend.title,
                        breaks = landprices_breaks,
                        values = landprices_linetypes,
                        labels = landprices_labels,
                        guide = guide_legend()
  ) +
  
  # ## Map the data to the geometric objects:
  
  geom_line(aes(y=(ROC(ROC(MKVAL, 4),4)), linetype = "Market", color="Market")) +
  geom_line(aes(y=(ROC(ROC(STRUC, 4),4)), linetype="Structures", color="Structures")) +
  geom_line(aes(y=(ROC(ROC(LAND, 4), 4)), linetype="Land", color="Land")) +
  geom_line(data=wascur.fred.xts, aes(y=(ROC(ROC(compensation,4),4)),
        linetype="Compensation", color="Compensation")) +
  theme(legend.position = c(0.15,0.75))

ROC2.plot # Comment/uncomment for testing
nberShade(ROC2.plot, fill="#a6cee3") + scale_y_continuous(labels=percent_format()) # Label the ordinate axis
ROC2.plot <- last_plot()


### RelROC.plot: Plot the ratio of the rate of change in the variable to the rate of change in home prices, as a percentage. ====
legend.title <- "Relative Rates of Change\n(Year over Year)::"

RelROC.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Percent (%)") +
  scale_y_continuous(labels=percent_format()) + # Label the ordinate axis
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = landprices_breaks,
                     values = landprices_colors,
                     labels = landprices_labels,
                     guide = guide_legend()
  ) +
  ## Colors of areas and the legend keys
  scale_fill_manual(legend.title,
                    breaks = landprices_breaks,
                    values = landprices_colors,
                    labels = landprices_labels,
                    guide = guide_legend()
  ) +
  ## Line styles
  scale_linetype_manual(legend.title,
                        breaks = landprices_breaks,
                        values = landprices_linetypes,
                        labels = landprices_labels,
                        guide = guide_legend()
  ) +
  
  # ## Map the data to the geometric objects:

  geom_line(aes(y=(ROC(MKVAL, 4)/ROC(MKVAL, 4)), linetype = "Market", color="Market")) +
  geom_line(aes(y=(ROC(STRUC, 4)/ROC(MKVAL, 4)), linetype="Structures", color="Structures")) +
  geom_line(aes(y=(ROC(LAND, 4)/ROC(MKVAL, 4)), linetype="Land", color="Land")) +
  geom_line(data=wascur.fred.xts, aes(y=(ROC(compensation,4))/ROC(CSW.national.xts$MKVAL,4),
                                      linetype="Compensation", color="Compensation")) +
  theme(legend.position = c(.15,.75))

RelROC.plot # Comment/uncomment for testing
nberShade(RelROC.plot, fill="#a6cee3") + scale_y_continuous(labels=percent_format()) # Label the ordinate axis
RelROC.plot <- last_plot()


### RelROC2.plot: Plot the ratio of the rate of change in the variable to the rate of change in home prices, as a percentage. ====
legend.title <- "Relative Rates of\nAcceleration\n(Year over Year):"

RelROC2.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Percent (%)") +
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = landprices_breaks,
                     values = landprices_colors,
                     labels = landprices_labels,
                     guide = guide_legend()
  ) +
  ## Colors of areas and the legend keys
  scale_fill_manual(legend.title,
                    breaks = landprices_breaks,
                    values = landprices_colors,
                    labels = landprices_labels,
                    guide = guide_legend()
  ) +
  ## Line styles
  scale_linetype_manual(legend.title,
                        breaks = landprices_breaks,
                        values = landprices_linetypes,
                        labels = landprices_labels,
                        guide = guide_legend()
  ) +
  
  # ## Map the data to the geometric objects:
  
  geom_line(aes(y=(ROC(ROC(MKVAL, 4), 4)/ROC(ROC(MKVAL, 4), 4)), linetype = "Market", color="Market")) +
  geom_line(aes(y=(ROC(ROC(STRUC, 4), 4)/ROC(ROC(MKVAL, 4), 4)), linetype="Structures", color="Structures")) +
  geom_line(aes(y=(ROC(ROC(LAND, 4), 4)/ROC(ROC(MKVAL, 4), 4)), linetype="Land", color="Land")) +
  geom_line(data=wascur.fred.xts, aes(y=(ROC(ROC(compensation,4),4)/ROC(ROC(CSW.national.xts$MKVAL,4),4)),
                                      linetype="Compensation", color="Compensation")) +
  theme(legend.position = c(.98,.01))

RelROC2.plot # Comment/uncomment for testing
nberShade(RelROC2.plot, fill="#a6cee3") + scale_y_continuous(labels=percent_format()) # Label the ordinate axis
RelROC2.plot <- last_plot()


# END OF RATES OF CHANGE



# AGGREGATE PLOTS ---------------------------------------------------------
## This may be redundant or belong near Market Value Plots

### aggregates.plot: Plot aggregate land values, compensation, etc. ====
legend.title <- "Aggregate\nMarket Value of:"
aggregate.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Aggregate Market Value (US$B)") +
  scale_y_continuous(labels=scales::dollar) + # Label the ordinate axis
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = landprices_breaks,
                     values=landprices_colors,
                     labels=landprices_labels,
                     guide = guide_legend()
  ) +
  ## Coordinate line styles
  scale_linetype_manual(legend.title,
                        breaks = landprices_breaks,
                        values = landprices_linetypes,
                        labels = landprices_labels,
                        guide = guide_legend()
  ) +
  
  ## Map the data to the geometric objects:
  geom_line(aes(y=LAND, color="Land", linetype="Land")) +
  geom_line(aes(y=STRUC, color="Structures", linetype="Structures")) +
  geom_line(aes(y=MKVAL, color="Market", linetype="Market")) +
  geom_line(data=wascur.fred.xts, aes(y=compensation,
          color="Compensation", linetype="Compensation")) +
  theme(legend.position = c(.98,.01))

aggregate.plot # Comment/uncomment for testing
nberShade(aggregate.plot, fill="#a6cee3") + scale_y_continuous(labels=percent_format()) # Label the ordinate axis
aggregate.plot <- last_plot()




# RATIOS AND PERCENTAGE PLOTS ---------------------------------------------
### COMPpct.plot: Plot aggregate housing values as % of compensation ====
legend.title <- "Aggregate\nMarket Value of:"
COMPpct.plot <- base.plot + # Start the plot
  
  ## Label the ordinate axis
  ylab("Value as Percent of Compensation (1975:1 = 100") +
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = landprices_breaks,
                     values=landprices_colors,
                     labels=landprices_labels,
                     guide = guide_legend()
  ) +
  ## Coordinate line styles
  scale_linetype_manual(legend.title,
                        breaks = landprices_breaks,
                        values = landprices_linetypes,
                        labels = landprices_labels,
                        guide = guide_legend()
  ) +
  
  ## Map the data to the geometric objects:
  geom_line(aes(y=(LAND/wascur.fred.xts$compensation),
                color="Land", linetype="Land")) +
  geom_line(aes(y=(STRUC/wascur.fred.xts$compensation),
            color="Structures", linetype="Structures")) +
  geom_line(aes(y=(MKVAL/wascur.fred.xts$compensation),
            color="Market", linetype="Market")) +
  theme(legend.position = c(.15,.75))

COMPpct.plot # Comment/uncomment for testing
nberShade(COMPpct.plot, fill="#a6cee3") + scale_y_continuous(labels=percent_format()) # Label the ordinate axis
COMPpct.plot <- last_plot()

# END OF RATIOS AND PERCENTAGE PLOTS





### PER CAPITA PLOTS --------------------------------------------------------
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
                     breaks = landprices_breaks,
                     values=landprices_colors,
                     labels=landprices_labels,
                     guide = guide_legend()
  ) +
  ## Coordinate line styles
  scale_linetype_manual(legend.title,
                        breaks = landprices_breaks,
                        values = landprices_linetypes,
                        labels = landprices_labels,
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


### PC_index75.plot - Plot index numbers for all four series ====
PC_index75.plot <- percapita.base.plot + # Start the plot
  ## Label the ordinate axis
  ylab("Index (1975:1 = 100)") +
  
  ## Colors of lines and the legend keys
  scale_color_manual(legend.title,
                     breaks = landprices_breaks,
                     values=landprices_colors,
                     labels=landprices_labels,
                     guide = guide_legend()
  ) +
  ## Coordinate line styles
  scale_linetype_manual(legend.title,
                        breaks = landprices_breaks,
                        values = landprices_linetypes,
                        labels = landprices_labels,
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

# END OF PER CAPITA PLOTS  
  