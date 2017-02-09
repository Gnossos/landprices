# Variable Labels
# Version 0.1
# Current version assigns value labels to a data frame
# Eventually want it S3, with methods to assign and retrieve variable labels
var.labels <- function(label.list,df){
  labels(df) = lapply(names(label.list),
                      function(x)label(dataframe[,x])=label.list[x])
  df
}