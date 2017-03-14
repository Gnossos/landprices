# recession_bars ====
# We may want to change the original FRED object from .xts
 
getSymbols("USREC",src="FRED")
start <- index(usrec.fred.xts[which(diff(as.vector(USREC$USREC)) == 1)])
end <- index(usrec.fred.xts[which(diff(as.vector(USREC$USREC)) == -1)-1])
recession.df <- data.frame(start = start, end = end)
recession.df <- subset(recession.df, start >= min(as.Date(index(CSW.national.xts))))

recession_bars <- function(x = recession.df, fill = "#a6cee3", alpha = 0.5) {
  geom_rect(
    data = x,
    aes(xmin = start, xmax = end, ymin = 0, ymax = Inf),
    fill = fill, alpha = alpha
  )
}  