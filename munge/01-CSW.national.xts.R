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

landdata_to_xts <- function(landdata.df,dateclass = "yearqtr", label="", varLabels="")
  #
  # Convert initial data frame to .xts  ## DELETE THIS ->and add columns with indexes based on the first year of the data.
  # landdata.df - initial data frame (required)
  # dateclass - "yearqtr" (date class of the original data)
  # label - a label for the entire object
  # varLabels - variable labels for the new xts object; class of the varLabels object must be legal for the label() function.
{
  landdata.xts <- xts(landdata.df[,-1], order.by = eval(call(paste("as.",dateclass,sep=""),landdata.df[,1]))) # Convert df to xts
  
  # Addition: Change the variable names to omit the _NOM suffix
  names(landdata.xts) <- unlist(lapply(strsplit(names(landdata.xts), "_NOM"), first))
  
  if (length(varLabels) > 0)
    label(landdata.xts) <- varLabels
  
  # landdata.xts <- merge(landdata.xts,redate_base_xts(landdata.xts),join = "inner")
  if (length(label) != 1L)
    stop("label = option sets the single label for the entire xts object and therefore must have length equal to 1")
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

# Label the variables
labels <- list(
  LAND="Aggregate market value of residential land",
  MKVAL="Aggregate market value of dwellings",
  STRUC="Aggregate replacement cost of residential structures",
  LAND_PI="Price index for residential land",
  MKVAL_PI="Price index for dwellings",
  STRUC_PI="Price index for residential structures",
  CONS_PI="Price index for consumption"
)

# Will create the CSW.national.xts object.
assign(extended_name(CSW.national),landdata_to_xts(CSW.national.2016q1, varLabels = labels,
           label="Case-Schiller-Weiss-based price index: aggregate land data, quarterly, 1975:1-2016:1"))
  
# Create a corresponding list of units
unitText <- append(append(rep("US$B",3),rep("2000Q2 = 100", 4)), rep("1975Q1 = 100", 7))
units(CSW.national.xts) <- setNames(as.list(unitText),names(CSW.national.xts))
