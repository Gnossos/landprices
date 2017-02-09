# Create plot files for Figure 1.

# First do them individually.
MVpct.plot
ggsave(file="graphs/MVpct.pdf", height=8, width=10, units = "in")
PI.plot
ggsave(file="graphs/PI.pdf", height = 8, width = 10, units = "in")
ROC.plot
ggsave(file="graphs/ROC.pdf", height = 8, width = 10, units = "in")
MV.plot
ggsave(file="graphs/MV.pdf", height = 8, width = 10, units = "in")

# Now try to get the entire figure
pdf("graphs/twobytwo.pdf", width = 8, height = 6) # Don't know about width & height
grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))

vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(MVpct.plot, vp = vplayout(1,1))
print(PI.plot, vp = vplayout(1,2))
print(ROC.plot, vp = vplayout(2,1))
print(MV.plot, vp = vplayout(2,2))
dev.off()