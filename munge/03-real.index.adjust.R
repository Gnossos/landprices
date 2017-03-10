# 02-real.index.adjust.R

getSuffixes <- function(x, delim = "_") {
  # Returns suffixes from the names(x), delimited by delim.
  
  # x             Named data object
  # delim = "_"   Character string used to delimit the suffix
  
  suffixes <- unlist(lapply(strsplit(names(x), split = delim),last))
}

apply.index <- function(
  # Applies <index> to <x>, premultiplying it by <mult>.
  # Returns an xts object containing the indexed amounts.
  # Automatically adds labeling unless Labels  = FALSE
  x,                                                                # Original xts object
  index.numbers = cpi_u.fred.xts,                                   # (Price) index object by which to adjust the values in x
  suffix = "AUTO",                                                  # A character string to be appended to the names of the variables (e.g., "_2020")
                                                                    # Suffix code was: suffix = paste("_",format(first(index(x))
  delim = "_",                                                      # Character string used to delimit suffixes
  mult = 100,                                                       # A multiplier used to multiply the resulting index
  labels = TRUE,                                                    # Add variable labels to the new .xts object (see labeling.R)
  units = "AUTO",                                                   # Units to append to variable labels (see labeling.R).
                                                                    #     If not specified, automatically constructed from units(x) and units(index.numbers). Use units = NULL to omit.
                                                                    #     If overridden, use a list of units, with names matching those of the final object. Override at your own risk!
  ... )
  {
  # Initialize common variables
  mx <- mult*x
  original.names <- names(x)
  
  if (suffix == "AUTO")
    suffixes <- paste("_", getSuffixes(index.numbers, delim = delim), sep = "") # Automatically construct suffixes from index.number names.
  else 
    suffixes <- suffix        # (Heroic?) assumption that suffix is a vector of legitimate suffixes. No cosmetics added here.
  
  if (labels) {
    original.labels <- label(x, units = FALSE)
    original.units <- units(x)
  }
  
  indexed.xts <- xts(order.by = index(x))
  working.xtsAttributes <- list()
  
  for (i in seq_along(index.numbers[1,])) {
    tmp.xts <- as.xts(sweep(mx, 1, index.numbers[,i], "/"))
    if (length(suffixes) > 1)
      names(tmp.xts) <- paste(original.names,suffixes[i], sep="")
    else
      names(tmp.xts) <- paste(original.names, suffixes, sep="")
    
    if (labels) {
       working.xtsAttributes$var.labels <- append(working.xtsAttributes$var.labels, setNames(original.labels,names(tmp.xts)))
      if (units == "AUTO") {
        index.number.units <- data.frame(lapply(units(index.numbers), strsplit, split = " ="))[1,]  # Extract the base years
        combined.units <- paste(index.number.units[,i], original.units)                             # Combine the original units with units from index.numbers
        working.xtsAttributes$unit.labels <- as.list(append(working.xtsAttributes$unit.labels, setNames(combined.units, names(tmp.xts)))) # And save them for the xts object
      }
      else if (!is.null(units))
        working.xtsAttributes$unit.labels <- as.list(append(working.xtsAttributes$unit.labels, setNames(units, names(tmp.xts))))
    }
    indexed.xts <- merge(indexed.xts, tmp.xts)
  }
  
  xtsAttributes(indexed.xts) <- working.xtsAttributes
  
  if (labels)
    label(indexed.xts, self = TRUE) <- paste(label(x, self = TRUE), ", adjusted by the index:", deparse(substitute(index.numbers)), sep = "")
  
  return(indexed.xts)
}


CSW.national.realDollars.xts <- apply.index(CSW.national.xts[,1:3], delim = "base")
wascur.fred.realDollars.xts <- apply.index(wascur.fred.xts[,1], delim = "base")



