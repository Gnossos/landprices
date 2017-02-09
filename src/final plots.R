# Create plot files for Figure 1.

# First do them individually.
MVpct.plot
ggsave(file="graphs/MVpct.pdf", height=8, width=10, units = "in")
PIpct.plot
ggsave(file="graphs/PIpct.pdf", height = 8, width = 10, units = "in")
ROC.plot
ggsave(file="graphs/ROC.pdf", height = 8, width = 10, units = "in")
PC_index75.plot
ggsave(file="graphs/PC_index75.pdf", height = 8, width = 10, units = "in")

# Now try to get the entire figure
pdf("graphs/twobytwo.pdf", width = 8, height = 6) # Don't know about width & height
grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))

vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(MVpct.plot, vp = vplayout(1,1))
print(PIpct.plot, vp = vplayout(1,2))
print(ROC.plot, vp = vplayout(2,1))
print(PC_index75.plot, vp = vplayout(2,2))
dev.off()