redate_base_xts <- function(original.xts, base=index(first(original.xts)), mult = 100,
                      dateclass="yearqtr", name="base", format = "%y")
  # Takes an array from an .xts object and resets their bases to the new base date. Returns them as their own .xts object
  # original.xts - required
  # newbase date = index(first(original.xts))
  # dateclass = "yearqtr" (character string)
  # name = "_base" -- returned column name will be <name><format>
  # format = "%y" (2-digiit year without century; see format() for other options)
{
  # Change the base of each column specified by original.xts
  
  if (!is.Date(base)) {
    base <- as.Date(base)
    if (!is.Date(base))
        warning("The specified base is not a date, and I can't coerce it into being one. This may cause an error.")
  }
  
  redated.xts <- scale(mult * original.xts, center = FALSE, scale = (original.xts[base,]))

  # Add names to redated.xts
  if (length(format) == 0)
    names(redated.xts) <- sapply(names(original.xts), paste, "_", name, sep = "")
  else
    names(redated.xts) <- sapply(names(original.xts), paste, "_", name, format(base, format=format), sep="")
  
  # Create and apply units for the rescaled base.
  unitsList <- as.list(replicate(ncol(original.xts),paste(base, "= 100")))
  names(unitsList) <- names(redated.xts)
  units(redated.xts) <- unitsList
  
  if (!is.null(xtsAttributes(original.xts)$var.labels)) # If the original object has var.labels, use them.
    label(redated.xts) <- str_c("Index number of ", str_to_lower(unlist(label(original.xts, units = FALSE))))

  return(redated.xts)
}
