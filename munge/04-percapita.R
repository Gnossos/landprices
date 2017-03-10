### 04-percapita.xts

as.percapita.xts <- function(
  x,                                                                      # An xts object to be converted to per capita data
  popn = WAP.fred.xts[,"WAP"],                                            # A numeric vectors. The population base; will be the denominator in the calculations
  object.label = paste(label(x, self = TRUE), ", per capita", sep = ""),  # A revised label for the new xts object
  currency = "US$"                                                        # Currency units for monetary values in x. Not sure this function works for non-monetary values yet.
  )
  
  {
  # Object-wide constants and variables
  # Use magnitudes to adjust magnitde from macro to per capita
  magnitudes <- c(1e6, 1e9, 1e12)           # Millions and Billions and Trillions
  names(magnitudes) <- c("M", "B", "T")
  # currencies <- c("$", "€", "£") # This is here, but nothing is implemented with it.
  currency.symbol <- str_sub(currency, start = -1) # Extract the currency symbol from currency
  
  # Process units
  unlisted <- unlist(units(x)) #
  deblanked.units <- strsplit(unlisted, " ") #
  monetary.units <- str_extract(deblanked.units, "[A-Z,a-z]*\\$[A-Z]") #
  currency.pos <- str_locate(unlisted, str_c("\\", currency.symbol)) #
  revised.units <- str_c(str_sub(unlisted, end = currency.pos[,1]), str_sub(unlisted, start = currency.pos[,2] + 2, end = -1L)) #
  demonetary.units <- strsplit(unlist(monetary.units), currency.symbol, fixed = TRUE) #
  multipliers <- magnitudes[unlist(lapply(demonetary.units, last))] #
  
  row.usable <- as.yearqtr(intersect(index(x), index(popn)))
  if (length(row.usable) != length(x))
    warning("The number of rows in the original object, x, is greater than the number of usable rows in the popn vector.")
  if (length(row.usable) != length(popn))
    warning("The number of rows in the population vector, popn, is greater than the number of usable rows in the original object, x.")
  new.xts <- xts(x[row.usable,]/popn[row.usable], order.by = row.usable)
  new.xts <- sweep(new.xts, 2, multipliers, "*")
  
  names(new.xts) <- paste(names(x),"pc", sep = "_")
  label(new.xts) <- setNames(label(x, units = FALSE), names(new.xts))
  label(new.xts, self = TRUE) <- object.label
  units(new.xts) <- setNames(paste(revised.units, "per capita"), names(new.xts))
  
  return(new.xts)
}

CSW.national.realDollars.percapita.xts <- as.percapita.xts(CSW.national.realDollars.xts,
      object.label = "Per-capita levels of land, dwelling, and structure prices and compensation")
CSW.national.realDollars.percapita.indices.xts <- redate_base_xts(CSW.national.realDollars.percapita.xts)

wascur.fred.realDollars.percapita.xts <- as.percapita.xts((wascur.fred.realDollars.xts))
wascur.fred.realDollars.percapita.indices.xts <- redate_base_xts(wascur.fred.realDollars.percapita.xts)