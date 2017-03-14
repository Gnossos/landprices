library(quantmod)
# getSymbols("USREC",src="FRED")
# getSymbols("UNRATE", src="FRED")
# unrate.df <- data.frame(date= index(UNRATE),UNRATE$UNRATE)
CSW.national.df <- data.frame(date = as.Date(index(CSW.national.xts)), coredata(CSW.national.xts))

start <- index(USREC[which(diff(as.vector(usrec.fred.xts$USREC)) == 1)])
end   <- index(USREC[which(diff(as.vector(usrec.fred.xts$USREC)) == -1)-1])

recession.df <- data.frame(start=start, end=end)
recession.df <- subset(recession.df, start >= min(CSW.national.df$date))

ggplot(data=CSW.national.df)+
  geom_line(aes(x=date,y=MKVAL)) +
    # fill_layers[1] + fill_layers[3] +
    geom_rect(data=recession.df,
            aes(xmin=start,xmax=end, ymin=0,ymax=max(CSW.national.df$MKVAL)), 
            fill="#a6cee3", alpha=0.5)

# Real stuff is here

start <- index(usrec.fred.xts[which(diff(as.vector(usrec.fred.xts$USREC)) == 1)])
end <- index(usrec.fred.xts[which(diff(as.vector(usrec.fred.xts$USREC)) == -1)-1])

working.xts <- CSW.national.xts
index(working.xts) <- as.Date(index(working.xts))
tidy(working.xts) %>% ggplot(aes(x = index, y = MKVAL)) + geom_line()

test.plot <- base.plot
test.plot <- test.plot + geom_line(aes(x = as.Date(index(CSW.national.xts)), y = CSW.national.xts$MKVAL))
test.plot
