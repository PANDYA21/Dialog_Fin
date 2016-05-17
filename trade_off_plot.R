# the trade-off plot
# Bhaumik Pandya

Sys.setlocale(, "en_US")
x.axis <- c(0,0.5,1)
y.axis <- c(0,1,0)
gg.fin <- ggplot() + geom_point(data = data.frame(xx = x.axis, 
                                                  yy = y.axis),
                                mapping = aes(x = xx, y = yy), colour = "blue", shape=21,
                                size = 7, fill = "white", stroke = 2)

for(ii in 1:length(norm.head[1:5])){
  ax1 <- norm.head[ii]
  ax2 <- norm.weight[ii]
  ax3 <- norm.string[ii]

  x.dat <- c(ax1*0.5, 1-(ax2*0.5), 0.5)
  # x.axis <- c(x.axis, x.dat)
  y.dat <- c(ax1*0.5, ax2*0.5, 1-(ax3*0.5))
  # y.axis <- c(y.axis, y.dat)

  gg.fin <- gg.fin + geom_point(data = data.frame(xx = sum(x.dat)/3, 
                                                  yy = sum(y.dat)/3), 
                                mapping = aes(x = xx, y = yy, colour = rownames(fin.df)[5+ii]), size = 5)
  gg.fin <- gg.fin + geom_point(data = data.frame(xx = sum(c(0,0.5,1))/3, 
                                                  yy = sum(c(0,1,0))/3),
                                mapping = aes(x = xx, y = yy), colour = "RED", size = 5)
}

gg.fin + theme_bw() +theme(axis.line = element_blank(),
               axis.ticks = element_blank(),
               axis.text = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank() )+ #, panel.background = element_blank()) +
  xlab("") + ylab("") +
  theme(
    panel.background = element_rect(fill = "#ecf0f5",colour = "#ecf0f5"), # or theme_blank() for transperency
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = "#ecf0f5",colour = "#ecf0f5"),
    legend.background = element_rect(fill = "#ecf0f5",colour = "#ecf0f5")
  )
