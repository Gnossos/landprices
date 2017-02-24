redate_base_xts <- function(original.xts,newbase=index(first(original.xts)),dateclass="yearqtr", name="_base", format = "%y")
  # Takes one of more vectors from an .xts object and resets their bases to the new base date. Returns them as their own .xts object
  # original.xts - required
  # newbase date = index(first(original.xts))
  # dateclass = "yearqtr" (character string)
  # name = "_base" -- returned column name will be <name><format>
  # format = "%y" (2-digiit year without century; see format() for other options)
{
  # Get variable labels if there are any
  if (!(is.null(xtsAttributes(original.xts)$var.labels))){
    var.labels <- xtsAttributes((original.xts)$var.labels)  
  }
  
  # Change the base of each column specified by original.xts
  redated.xts <- xts(order.by = index(original.xts)) # Start with an empty xts matrix
  for(i in 1:ncol(original.xts)){
    redated.xts <- merge(redated.xts,original.xts[,i]/rep_len(original.xts[
      eval(call(paste("as.",dateclass,sep=""),newbase)),i],nrow(original.xts))) # Make a redated xts object with entries divided by the new base.
  }
  
  # Add variable labels

    # Add names to redated.xts
    if (length(format) == 0){
      names(redated.xts)[i] <- paste(names(original.xts[,i]),name,sep="") # If format is "", don't use format() because it will add the entire base date to the name.
    } else {
      names(redated.xts)[i] <- paste(names(original.xts[,i]),name,format(newbase, format=format),sep="") # But do use it if there's something there.
    }
    
    # Reconstruct variable label
    if (is.list(xtsAttributes(original.xts)[["var.labels"]])) {
      var.labels.list <- xtsAttributes(original.xts)[["var.labels"]]
      for(label in var.labels.list){
        
      }
    }
  return(redated.xts)
}
