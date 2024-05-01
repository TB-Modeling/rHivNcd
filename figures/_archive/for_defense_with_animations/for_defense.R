## WITH ANIMATIONS FOR SLIDES ## 

## Figure 3A: HIV treatment coverage 
jpeg(file=paste0("plots/for_defense/Fig3A.1.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.hiv.coverage, aes(color=intervention, y=value/100, x=year)) + 
  geom_line() + 
  scale_color_manual(values=c(cols[1],rep(clear.col,6))) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        panel.grid.major.x = element_line(color = "grey", linetype="dotted"),
        legend.position = "none",
        text = element_text(size = 15))+
  ylab(NULL) + 
  scale_y_continuous(labels = scales::percent,name = NULL, limits = c(NA,NA))
dev.off()

jpeg(file=paste0("plots/for_defense/Fig3A.2.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.hiv.coverage, aes(color=intervention, y=value/100, x=year)) + 
  geom_line() + 
  scale_color_manual(values=c(cols[1:4],rep(clear.col,3))) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        panel.grid.major.x = element_line(color = "grey", linetype="dotted"),
        legend.position = "none",
        text = element_text(size = 15))+
  ylab(NULL) + 
  scale_y_continuous(labels = scales::percent,name = NULL, limits = c(NA,NA))
dev.off()

jpeg(file=paste0("plots/for_defense/Fig3D.1.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.ncd.coverage, aes(color=intervention, y=value/100, x=year)) + 
  geom_line() + ylim(0,NA) +
  scale_color_manual(values=c(cols[1],rep(clear.col,6))) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        panel.grid.major.x = element_line(color = "grey", linetype="dotted"),
        legend.position = "none",
        text = element_text(size = 15))+
  ylab(NULL)+ 
  scale_y_continuous(labels = scales::percent,name = NULL)
dev.off()

jpeg(file=paste0("plots/for_defense/Fig3D.2.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.ncd.coverage, aes(color=intervention, y=value/100, x=year)) + 
  geom_line() + ylim(0,NA) +
  scale_color_manual(values=c(cols[1:4],rep(clear.col,3))) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        panel.grid.major.x = element_line(color = "grey", linetype="dotted"),
        legend.position = "none",
        text = element_text(size = 15))+
  ylab(NULL)+ 
  scale_y_continuous(labels = scales::percent,name = NULL)
dev.off()
