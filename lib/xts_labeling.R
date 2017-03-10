# Labeling routines for .xts.
# Make S3 class because xts does too. Both should probably be converted to S4.


###  Xts methods
#   Xts replacement method. This does not currently have value labels implemented.
#   self = TRUE labels the entire object (e.g., data label)
#   self = FALSE labels var.labels

# Subsetting for labelled xts objects
`[.xts` <- function(x, i, j, ...) {
  # If x is an .xts object, select the proper subsets of variable and units labels.
  # N.B. This does not depend on being a member of class "labelled."
  # It only looks at what var.labels and units are stored in the object.

  if (is.xts(x)) {
    original.class <- oldClass(x)
    if (!is.null(xtsAttributes(x)$var.labels))
      var.labels <- xtsAttributes(x)$var.labels [j]
    if (!is.null(xtsAttributes(x)$unit.labels))
      unit.labels <- xtsAttributes(x)$unit.labels [j]
    x <- NextMethod("[", drop = FALSE)
    class(x) <- original.class
    if (exists("var.labels"))
      xtsAttributes(x)$var.labels <- var.labels
    if (exists("unit.labels"))
      xtsAttributes(x)$unit.labels <- unit.labels
    return(x)
  }
  NextMethod("[")
}

# Subsetting for assignment to labelled xts objects
`[<-.xts` <- function(x, i, j, ..., value) {
  # Usage x[i,j] <- value.
  # If value is xts, copy labels [,j]
  # If value is matrix or vector, don't do anything with labels (use labels(x) <- to change them)
  
    if (!is.xts(x))
      stop("Target object must be of class xts.")
    if (is.xts(value)){
      if (ncol(value) != j)
        stop("Right-hand side (value) must have same number of columns as reconfigured left hand side (target x)")
      
      if (is.null(xtsAttributes(x)$var.labels))
        xtsAttributes(x)$var.labels <- xtsAttributes(value)$var.labels
      else
        xtsAttributes(x)$var.labels <- modifyList(xtsAttributes(x)$var.labels,xtsAttributes(value)$var.labels)
      
      if (is.null(xtsAttributes(x)$unit.labels))
        xtsAttributes(x)$unit.labels <- xtsAttributes((value)$unit.labels)
      else
        xtsAttributes(x)$unit.labels <- modifyList(xtsAttributes(x)$unit.labels,xtsAttributes(value)$unit.labels)
    }
  NextMethod()
}

makeLabelingList <- function(target,labels, newNames = FALSE, ...){
  # Utility function to construct a list to use for internal labeling
  # Returns the labeling list
  
  if (is.list(labels)) # If labels is already a list, use it.
    labelingList <- labels
  else        # Otherwise, create the list and take its names from the appropriate labels
    labelingList <- as.list(labels)
  
  if (newNames) { # newNames gets names from labels
    if (is.null(names(labels)))
      stop("newNames is TRUE, which means names should come from the labels, but the labels has no names.")
    if (length(names(labels)) > length(names(target)))
      warning(paste("The labels source has more names than the target. Because newNames = ",
                    newNames, ", the extra labels will be added to the target."))
    if (length(setdiff(names(labels),names(target))) > 0)
      warning(paste("These label names are missing from the target:",
                    paste(setdiff(names(labels), names(target)), collapse=", ", sep=" ")    ))
    labelingList <- labels
    }
  else {
    # if (length(labels) > length(target))
    #   stop(paste("labels has more elements than target, and newNames = ", newNames, ". So I can't add them.", sep=""))
    missingNames <- setdiff(names(labels),names(target))
    missingNames <- paste(missingNames, collapse = ", ", sep = "")
    if (length(missingNames)) {
      warning(paste("These names in the labels -- ", missingNames, "-- are missing from the target. But I can't add them to the target because newNames = ", newNames, ".",
                    " I will ignore them instead.", sep=""))
      labelingList <- labels[intersect(names(target),names(labels))]
    }
  }
  return(labelingList)
}

## Assign lables
`label<-.xts` <- 
  function (x, self = FALSE, by.name = FALSE, ..., value)
  {
    if (!is.xts(x))
      stop("x must be an xts object")
    if (missing(self))
      self <- FALSE
    
    # Handle self option
    if (self)
      if (length(value) != 1L)
        stop("value must be character vector of length 1.")
      else
        xtsAttributes(x) <- llist(label=value) # Add the label to the xtsAttributes

    else { # Label the variables in the xts object
      xtsAttributes(x)$var.labels <- vector("list",ncol(x)) # Create the list of var.labels
      names(xtsAttributes(x)$var.labels) <- names(x) # and name it.
      
      # If either by.name is selected or the value is a list, use by.name.
      if (by.name || (is.list(value))) { 
        xtsAttributes(x)$var.labels <- modifyList(xtsAttributes(x)$var.labels,value)
      }
      else {
        if (length(value) != ncol(x)) # if value is a vector, length must = number of columns in the xts object
          stop("Vector value must have the same length as the xts object. To assign a single label to the object, use self = TRUE.")
        # If value is a vector, then each item becames the var.label for the corresponding variable.
        #  for (i in seq(ncol(x)))
        #    xtsAttributes(x)$var.labels[i] <- value[i]
        labels <- as.list(value)
        names(labels) <- names(xtsAttributes(x)$var.labels)
        xtsAttributes(x)$var.labels <- modifyList(xtsAttributes(x)$var.labels,labels)
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
    
    var.labels <- xtsAttributes(x)$var.labels
    var.units <- xtsAttributes(x)$unit.labels
    
    label.list <- list()
    if (length(var.labels)) {
      label.list <- var.labels
      for (n in names(var.labels))
        if (n %in% names(x))
          if (units && !is.null(var.units[[n]]))
            label.list[[n]] <- paste(label.list[[n]], " [", var.units[[n]], "]", sep="")
    }
  }
  return(label.list)
}

## Assign units
`units<-.xts` <- 
  function (x, self = FALSE, by.name = FALSE, ..., value)
  {
    if (!is.xts(x))
      stop("x must be an xts object")
    if (missing(self))
      self <- FALSE
    
    # Handle self option
    if (self)
      if (length(value) != 1L)
        stop("value must be character vector of length 1.")
    else
      xtsAttributes(x) <- llist(units=value) # Add the label to the xtsAttributes
    
    else { # Add units to the variables in the xts object
      xtsAttributes(x)$unit.labels <- vector("list",ncol(x)) # Create the list of unit.labels
      names(xtsAttributes(x)$unit.labels) <- names(x) # and name it.
      
      # If either by.name is selected or the value is a list, use by.name.
      if (by.name || (is.list(value))) { 
        xtsAttributes(x)$unit.labels <- modifyList(xtsAttributes(x)$unit.labels,value)
      }
      else {
        if (length(value) != ncol(x)) # if value is a vector, length must = number of columns in the xts object
          stop("Vector value must have the same length as the xts object. To assign a single label to the object, use self = TRUE.")
        # If value is a vector, then each item becames the var.label for the corresponding variable.
        #  for (i in seq(ncol(x)))
        #    xtsAttributes(x)$var.labels[i] <- value[i]
        units <- as.list(value)
        names(units) <- names(xtsAttributes(x)$unit.labels)
        xtsAttributes(x)$unit.labels <- modifyList(xtsAttributes(x)$unit.labels,units)
      }
    }
    # Make the object labelled class and return it
    if (!("labelled" %in% class(x))) {
      class(x) <- c("labelled", class(x))
    }
    return(x)
  }

#   Xts object method
units.xts <-
  function (x, default = NULL, self = FALSE, ...) 
  {
    if (self) {
      return(
        xtsAttributes(x)$units
      )
    }
    else {
      if (length(default) > 0 && length(default) != ncol(x)) {
        stop("length of default must be same as x")
      }
      else if (length(default) == 0) {
        default <- list(default)
      }
      return(xtsAttributes(x)$unit.labels)
    }
  }
