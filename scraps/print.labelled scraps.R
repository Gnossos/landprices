# Stuff from print.labelled that we may not need.

var.labels <- xtsAttributes(x.orig)$var.labels
u <- xtsAttributes(x.orig)$unit.labels # Only use object-level units as default
for (n in names(u))
  units(x[,n]) <- u[[n]]
if (self) {
  label(x, self = TRUE) <- label(x.orig, self = TRUE) # MAY BE UNNECESSARY IF DONE IN as.labelled.xts.data.frame
  u.default <- units(x.orig, self = TRUE)
  for (n in names(x))
    if (is.null(units(x[,n])))
      units(x[,n]) <- u.default
}
else { # not self, so don't use object-level at all
  for (n in names(x))
    label(x) <- 
      
      u <- xtsAttributes(x)$units
  else
    u <- unlist(xtsAttributes(x)$units.labels)
  x <- as.labelled.xts.data.frame(x) # Convert x to a labelled data frame
}