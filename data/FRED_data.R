### Access FRED data:
#       Quarterly, not seasonally adjusted working age population (LFWA64TTUSQ647N)
#       Compensation of Employees: Wages and Salary Accruals (WASCUR)
#       CPI: All Items Consumer Price Index for All Urban Consumers (CPI-U)

#library(fredr) # These library calls should be unnecessary with ProjectTemplate
fredr_key("d9898b700d6b107ec1c7f890d09bd6ec")
#library(xts)

### This is the workhorse function call
FRED.as.xts <- function(
  series_id = NULL, # FRED series id (required)
  # selection = index(CSW.national.xts), # An expression used to subset the FRED series. If FALSE, the entire series is returned.
  label = NULL, # A label for the entire object
  name = NULL, # A string used to name the variable in the series. Warning if not specified. Do not include a base suffix: see original.base below. Subsequent versions should allow lists & vectors of names.
  units = NULL, # A string used to label the units of the variable in the series
  make_index = TRUE, # Make an index number from the original series and merge it with the original
  mult = 100, # Multiply the index by mult. Setting it to 1 prevents any rescaling.
  var_labels = NULL, # A named list of labels for the returned xts object. See `label<-.xts`
  original_base = NULL # If the original series is an index number, indicate it by specifying original base as a character string. It will be appened to the name.
)
{
  if (is.null(series_id))
    stop("No series_id was given, so I don't know what series to get.")
  
  new.xts <- as.xts(fredr_series(series_id = series_id))
  # suppressWarnings(                          # Need to prevent a warning message
  #   if (selection != FALSE)                  # But see http://romainfrancois.blog.free.fr/index.php?post/2009/05/20/Disable-specific-warnings for a method
  #     new.xts <- as.xts(new.xts[selection,])   # to specifically suppress the "In if (selection != FALSE)" warning message.
  # )                  
  
  if (is.null(name))
    warning("No name was given for the retrieved series.")
  else
    names(new.xts) <- name # Use this name until redated.xts is done.
  
  if (make_index) {
    redated.new.xts <- redate_base_xts(new.xts)
    if (!is.null(original_base) && !is.null(name)) {
      name <- paste(name, "_base", original_base, sep = "")
      names(new.xts) <- name
    }
    new.xts <- merge(new.xts, mult * redated.new.xts)
  }
  
  if (!is.null(label))
    label(new.xts, self = TRUE) <- label
  
  if (!is.null(var_labels))
    if (!is.null(original_base))
      names(var_labels)[1] <- name
    label(new.xts) <- var_labels
  
  # Add units
  if (make_index) {
    if (!is.null(units)) {
      units_list <- as.list(units)
      names(units_list) <- name
      units(new.xts) <- append(units_list, units(redated.new.xts))
    }
    else
      units(new.xts) <- units(redated.new.xts)
  }
  return(new.xts)
}


# Working age population (LFWA64TTUSQ647N), quarterly,
# not seasonally adjusted (seasonally adjusted not available for all years)
# Working Age Population: Aged 15-64: All Persons for the United States
WAP.fred.xts <- FRED.as.xts(
  "LFWA64TTUSQ647N", 
  label = "Working Age Population: Aged 15-64: All Persons for the United States (FRED series LFWA64TTUSQ647N)",
  name = "WAP",
  units = "Persons, Quarterly, Not Seasonally Adjusted",
  var_labels = list(
    WAP = "Working Age Population: Aged 15-64: All Persons for the United States",
    WAP_base75 = "Index of working-age population"
    )
  )


# Compensation of Employees: Wages and Salary Accruals (WASCUR), seasonally adjusted
# Note: FRED series A576RC1 is for disbursements, whereas WASCUR includes earnings, whether
# disbursed or not.

wascur.fred.xts <- FRED.as.xts(
  "WASCUR",
  label = "Compensation of Employees: Wages and Salary Accruals (FRED series WASCUR)",
  name = "compensation",
  units = "US$B, Quarterly, Seasonally Adjusted Annual Rate",
  var_labels = (list
    (
      compensation ="Compensation of employees: wage and salary accruals",
      compensation_base75 = "Price index for employee compensation"
    )
  )
)

# CPI: All Items Consumer Price Index for All Urban Consumers (CPI-U)
cpi_u.fred.xts <- FRED.as.xts(
  "CPIAUCSL",
  label = "Consumer Price Index for All Urban Consumers (CPI-U)",
  name = "CPI_U",
  units = "1982-1984 = 100",
  var_labels = (list
                (
                  CPI_U = "Consumer Price Index for All Urban Consumers",
                  CPI_U_base75 = "Consumer Price Index for All Urban Consumers"
                )
  ),
  original_base = "82_84"
)


