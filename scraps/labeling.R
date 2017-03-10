# Labeling routines for .xts.
# Make S3 class because xts does too. Both should probably be converted to S4.


###  Xts methods
#   Xts replacement method. This does not currently have value labels implemented.
#   self = TRUE labels the entire object (e.g., data label)
#   self = FALSE labels var.labels

`label<-.xts` <- 
  function (x, self = FALSE, by.name = FALSE, ..., value)
  {
    if (!is.xts(x)) {
      stop("x must be an xts object")
    }
    if (missing(self) && ( is.list(value) || is.vector(value))) {
      self <- FALSE
    }
    if (self) { # Label the entire object
      # Mimic label<-.default code here
      if (is.list(value)) {
        stop("Cannot assign a list to label an entire xts object.")
      }
      if (length(value) != 1L) {
        stop("value must be character vector of length 1.")
      }
      xtsAttributes(x) <- llist(label=value) # Add the label to the xtsAttributes
    }
    else { # Label the variables in the xts object
      xtsAttributes(x)$var.labels <- vector("list",ncol(x)) # Create the list of var.labels
      names(xtsAttributes(x)$var.labels) <- names(x) # and name it.
      
      # If either by.name is selected or the value is a list <= the number of variables, use by.name.
      if (by.name || (is.list(value) && (length(value) <= length(xtsAttributes(x)$var.labels)))) { # This can be used to set a subset of the variable labels
        xtsAttributes(x)$var.labels <- modifyList(xtsAttributes(x)$var.labels,value)
      }
      else {
        if (length(value) != ncol(x)) # if value is a vector, length must = number of columns in the xts object
          stop("value must have the same length as the xts object. To assign a single label to the object, use self = TRUE.")
        # If value is a vector, then each item becames the var.label for the corresponding variable.
          for (i in seq(ncol(x)))
            xtsAttributes(x)[["var.labels"]][i] <- value[i]
        }
    }
    # Make the object labelled class and return it
    if (!("labelled" %in% class(x))) {
      class(x) <- c("labelled", class(x))
    }
    return(x)
  }
  
#   Xts object method
label.xts <-
function (x, default = NULL, self = FALSE, units = TRUE, ...) 
{
  if (self) {
      return(
        xtsAttributes(x)$label
        )
  }
  else {
    if (length(default) > 0 && length(default) != ncol(x)) {
      stop("length of default must be same as x")
    }
    else if (length(default) == 0) {
      default <- list(default)
    }
    
    # THESE TWO STATEMENTS ARE PROBLEMATIC BECAUSE THEY'RE TAKING THE ENTIRE LABEL LIST INSTEAD OF THE NUMBER PASSED. THE ADDED COLUMN DIMENSIONS ARE A KLUDGE FIX.
    var.labels <- xtsAttributes(x)$var.labels [names(x)]
    var.units <- xtsAttributes(x)$var.units [names(x)]
    if (ncol(x) > 1)
      linechar = "\n"
    else
      linechar = ""
    
    label.list <- list()
    if (length(var.labels)) {
      for (n in names(var.labels)) # [1:ncol(x)]) # THE COLUMN DIMENSION KLUDGE AGAIN
        if (n %in% names(x)) # [1:ncol(x)])
          if (!units || is.null(var.units[[n]]))
            label.list[[n]] <- var.labels[[n]]
          else
            label.list[[n]] <- paste(var.labels[[n]], " [", var.units[[n]], "]", sep="")
    }
  }
  return(label.list)
}

# Subsetting for labelled xts objects
`[.labelled` <- function(x, i) {
  
  # browser()
  if (is.xts(x)) { # If it's an .xts object, select the proper subsets of variable and units labels.
    new.x <- unclass(x)
    if (!is.null(xtsAttributes(x)$var.labels))
      xtsAttributes(new.x)$var.list <- xtsAttributes(x)$var.labels [i]
    if (!is.null(xtsAttributes(x)$var.units))
      xtsAttributes(new.x)$var.units <- xtsAttributes(x)$var.units [i]
    NextMethod()
    return(new.x[i])
  }
  else UseMethod(`[`)
  
}

# units.labels.xts methods
`units.labels<-.xts` <- 
  function (x, self = FALSE, by.name = FALSE, ..., value) 
  {
    if (!is.xts(x)) {
      stop("x must be an xts object")
    }
    if (missing(self) && ( is.list(value) || is.vector(value))) {
      self <- FALSE
    }
    if (self) { # Add units to the entire object
      # Mimic label<-.default code here
      if (is.list(value)) {
        stop("Cannot assign a list to units of an entire xts object.")
      }
      if (length(value) != 1L) {
        stop("value must be character vector of length 1.")
      }
      xtsAttributes(x) <- list(units=value) # Add the label to the xtsAttributes
    }
    else { # Label the variables in the xts object
      xtsAttributes(x)$var.units <- vector("list",ncol(x)) # Create the list of units.labels
      names(xtsAttributes(x)$var.units) <- names(x) # and name it.
      
      # If either by.name is selected or the value is a list <= the number of variables, use by.name.
      if (by.name || (is.list(value) && (length(value) <= length(xtsAttributes(x)$var.units)))) { # This can be used to set a subset of the units labels
        xtsAttributes(x)$var.units <- modifyList(xtsAttributes(x)$var.units,value)
      }
      else {
        if (length(value) != ncol(x)) # if value is a vector, length must = number of columns in the xts object
          stop("value vector must be the same length as x")
        # If value is a vector, then each item becames the units.label for the corresponding variable.
        for (i in seq(ncol(x)))
          xtsAttributes(x)[["units.labels"]][i] <- value[i]
      }
    }
    # Make the object labelled class and return it
    if (!("labelled" %in% class(x))) {
      class(x) <- c("labelled", class(x))
    }
    return(x)
  }

#   units.labels.xts object method
units.labels.xts <-
  function (x, default = NULL, self = FALSE, ...) 
  {
    if (self) {
      return(xtsAttributes(x)$units)
    }
    else {
      if (length(default) > 0 && length(default) != ncol(x)) {
        stop("length of default must be same as x")
      }
      else if (length(default) == 0) {
        default <- list(default)
      }
      units <- xtsAttributes(x) [["units.labels"]]
      return(units)
    }
  }


# print.labelled -- modified from original to handle xts
print.labelled <-
function (x, self=FALSE, labels = TRUE, ...) 
{
  x.orig <- x # Save a copy of the original
  if(is.xts(x)) { # This is added for xts
    if (labels)
      if (self) {
          label(x, self = TRUE)
          label(x, self = TRUE) <- NULL
        }
    else {
        label(x)
        xtsAttributes(x)$var.labels <- modifyList(xtsAttributes(x),list(var.labels = NULL), keep.null = TRUE) # NEED TO ADD THIS FUNCTION TO label() itself - remove label
        if (length(xtsAttributes(x)$var.units))
         xtsAttributes(x) <- modifyList(xtsAttributes(x),list(units.labels = NULL), keep.null = TRUE)
    }
  }
  else { # This is original
    # x.orig <- x # Moved to common area
    u <- attr(x, "units", exact = TRUE)
    if (length(u)) 
      attr(x, "units") <- NULL
    cat(attr(x, "label", exact = TRUE), if (length(u)) 
      paste("[", u, "]", sep = ""), "\n")
    attr(x, "label") <- NULL
  }
  
  # Moved this up here because the rest is common
    class(x) <-
      if (length(class(x)) == 1 && class(x) == "labelled") 
        NULL
      else class(x)[class(x) != "labelled"]
    if (!length(attr(x, "class"))) 
      attr(x, "class") <- NULL
    NextMethod("print")
    invisible(x.orig)
}


# as.labelled.xts.data.frame -- For .xts objects, make a data frame with appropriately labelled columns
as.labelled.xts.data.frame <- function(x, ...)
{
  if (!is.xts(x))
    stop("x must be an xts object")
  x.orig <- x
  class(x) <- "xts"
  x.df <- as.data.frame(x)
  if (!(is.null(xtsAttributes(x)$label))) label(x.df, self = TRUE) <- label(x, self = TRUE)
  x.df <- label.cols(x.df, labels =  xtsAttributes(x)$var.labels)
  x.df <- label.cols(x.df, as.units = TRUE, labels = xtsAttriutes(x)$var.units)
  row.names(x.df) <- index(x)
  class(x.df) <- c("labelled", class(x.df))
  x.df
}


# label.cols -- label data frame columns
label.cols <- function(x, as.units = FALSE, ...)
{
  if (!(exists("labels")))
    stop("To label columns, you must explicitly pass labels as 'labels =.'")
  if (is.list(labels))
    values <- (labels)
  else
    values <- list(labels)
  
  if (!all(names(values) %in% names(x)))
    stop("Some variables are not found in x.")
  if (as.units)
    for (v in names(values)) label(as.vector(x[v])) <- values[[v]]
  else
    for (v in names(values)) label(as.vector(x[v])) <- values[[v]]
  x
}


# print.xts <-
# function (x, ...) 
# {
#   x.orig <- x
#   x <- as.data.frame(x)
#   row.names(x) <- index(x.orig)
#   
#   class(x) <-
#     if (length(class(x)) == 1 && class(x) == "labelled") 
#       NULL
#     else 
#       class(x)[class(x) != "xts"]
#   if (!length(attr(x, "class"))) 
#     attr(x, "class") <- NULL
#   NextMethod("print")
#   invisible(x.orig)
# }





### Other labelling-specific functions

## Units
`units<-` <-
function (x, ..., value) 
  UseMethod("units<-")

`units<-.default` <-
function (x, ..., value) 
{
  attr(x, "units") <- value
  x
}

units <-
function (x, ...) 
  UseMethod("units")

units.default <-
function (x, none = "", ...) 
{
  lab <- attr(x, "units")
  if (is.null(lab)) 
    lab <- attr(attr(x, "tspar"), "units")
  if (is.null(lab)) 
    lab <- none
  lab
}


### Utility Functions ###





# llist - list while preserving names or labels of component variables.
llist <-
function (..., labels = TRUE) 
{
  dotlist <- list(...)
  lname <- names(dotlist)
  name <- vname <- as.character(sys.call())[-1]
  for (i in 1:length(dotlist)) {
    vname[i] <- if (length(lname) && lname[i] != "") 
      lname[i]
    else name[i]
    lab <- vname[i]
    if (labels) {
      lab <- attr(dotlist[[i]], "label", exact = TRUE)
      if (length(lab) == 0) 
        lab <- vname[i]
    }
    label(dotlist[[i]]) <- lab
  }
  names(dotlist) <- vname[1:length(dotlist)]
  dotlist
}

# labelPlotmath
labelPlotmath <-
function (label, units = NULL, plotmath = TRUE, html = FALSE, 
          grid = FALSE, chexpr = FALSE) 
{
  if (!length(label)) 
    label <- ""
  if (!length(units) || (length(units) == 1 && is.na(units))) 
    units <- ""
  if (html) 
    return(markupSpecs$html$varlabel(label, units))
  if (!plotmath) 
    return(markupSpecs$plain$varlabel(label, units))
  g <- function(x, y = NULL, xstyle = NULL, ystyle = NULL) {
    h <- function(w, style = NULL) if (length(style)) 
      sprintf("%s(%s)", style, w)
    else w
    tryparse <- function(z, original, chexpr) {
      p <- try(parse(text = z), silent = TRUE)
      if (is.character(p)) 
        original
      else if (chexpr) 
        sprintf("expression(%s)", z)
      else p
    }
    if (!length(y)) 
      return(tryparse(h(plotmathTranslate(x), xstyle), 
                      x, chexpr))
    w <- paste("list(", h(plotmathTranslate(x), xstyle), 
               ",", h(plotmathTranslate(y), ystyle), ")", sep = "")
    tryparse(w, paste(x, y), chexpr)
  }
  if (units == "") 
    g(label)
  else if (label == "") 
    g(units)
  else g(label, units, ystyle = "scriptstyle")
}
