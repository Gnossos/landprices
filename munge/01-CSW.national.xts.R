# 01-CSW.national.xts - Convert downloaded land-price csv file to xts time series.

## Land & Housing Data
#
# General guidance: # Reference: https://rstudio-pubs-static.s3.amazonaws.com/40873_5fbe3860854a47c38a58aabd01f9cf9d.html
#
# Specific information about the data:
# Source: http://datatoolkits.lincolninst.edu/subcenters/land-values/price-and-quantity.asp
# Make the original data frame into an xts time series.
# Xts requires a date variable. So convert the first column (variable) to a yearqtr object.
# NOTE: The default only works with the CSW and FHFA files. The Historical file is annual and therefore requires as.year
#   instead of as.yearqtr.

landdata_to_xts <- function(landdata.df,dateclass = "yearqtr", label="")
  #
  # Convert initial data frame to .xts and add columns with indexes based on the first year of the data.
  # landdata.df - initial data frame (required)
  # dateclass - "yearqtr" (date class of the original data)
{
  # Attribute list (for later use)
  var.labels.list <- list(
    LAND_NOM="Aggregate market value of residential land ($B)",
    MKVAL_NOM="Aggregate market value of dwellings ($B)",
    STRUC_NOM="Aggregate replacement cost of residential structures ($B)",
    LAND_PI="Price index for residential land (2000 Q2 = 100.0)",
    MKVAL_PI="Price index for dwellings (2000 Q2 = 100.0)",
    STRUC_PI="Price index for residential structures (2000 Q2 = 100.0)",
    CONS_PI="Price index for consumption (2000 Q2 = 100.0)"
    )
  
  landdata.xts <- xts(landdata.df[,-1], order.by = eval(call(paste("as.",dateclass,sep=""),landdata.df[,1]))) # Convert df to xts
  landdata.xts <- cbind(landdata.xts[,1:3],100*landdata.xts[,4:7]) # Multiply index numbers by 100, so 100 is base year.
  #label(landdata.xts) <- var.labels.list
  
  landdata.xts <- merge(landdata.xts,redate_base_xts(landdata.xts),join = "inner")
  if (length(label) != 1L)
    stop("label = option sets the single label for the entire xts object and therefore must have length equal to 1")
  # xtsAttributes(landdata.xts) <- list(label = label) # CHECK THIS
  label(landdata.xts, self = TRUE) <- label
  
  return(landdata.xts)
}

extended_name <- function(original,suffix=".xts")
  # Extends the original name by adding the suffix. E.g., from "original" to "original.xts"
  # Can be used by caller as:
  #     assign(extend_name(original),<value>), which will assign the value to <original><suffix>
{
  return(paste(deparse(substitute(original)),suffix,sep=""))
}

assign(extended_name(CSW.national),landdata_to_xts(CSW.national.2016q1,label="Case-Schiller-Weiss-based price index: aggregate land data, quarterly, 1975:1-2016:1"))
