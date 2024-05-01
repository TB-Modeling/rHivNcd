source("results/setup_paper_figures.R")
library(scales)
library(RColorBrewer)
cols = hue_pal()(7)
WIDTH = 1500
HEIGHT = 1000
RES = 300

labels = c("scen_1" = interventions.full.names[[1]][1],
          "scen_2" = interventions.full.names[[1]][2],
          "scen_3" = interventions.full.names[[1]][3],
          "scen_4" = interventions.full.names[[1]][4],
          "scen_5" = interventions.full.names[[1]][5],
          "scen_6" = interventions.full.names[[1]][6],
          "scen_7" = interventions.full.names[[1]][7])

labels[7] = "(7) Intensive NCD care + comprehensive\n HIV care @ community"
clear.col = rgb(0, 0, 1, alpha = 0)

model.labels = c("1" = "Compartmental HIV model",
                 "2" = "Individual-based NCD simulation")

# Use one of these to save the intervention label
if(1==2){
  ggplot(df.hiv.inc.per.pop, aes(x=intervention, y=value, fill=intervention)) + 
    stat_summary(fun.data = quantiles_95, geom="boxplot")  + 
    theme(legend.position = "bottom") +
    scale_fill_manual(labels = labels, 
                      values=cols)
  
  ggplot(df.hiv.inc.per.pop, aes(x=intervention, y=value, color=intervention)) + 
    stat_summary(fun.data = quantiles_95, geom="errorbar",width=0.1,size=0.5) + 
    stat_summary(fun="median", geom="point",size=3,pch=19)+
    theme(legend.position = "bottom") +
    scale_color_manual(labels = labels, 
                       values=cols)
  
  ggplot(df.relative.redux.events.per.pop, aes(x=intervention, y=value, color=intervention)) + 
    stat_summary(fun.data = quantiles_95, geom="errorbar",width=0.1,size=0.5) + 
    stat_summary(fun="median", geom="point",size=3,pch=19)+
    #theme(legend.position = "bottom") +
    guides(fill=guide_legend(ncol=2)) +
    scale_color_manual(labels = labels[2:7], 
                       values=cols[2:7]) 
}


# Setting up plotting data.frames
df.hiv.coverage = reshape2::melt(hiv.treatment.coverage.median)
df.ncd.coverage = reshape2::melt(ncd.treatment.coverage.median)
df.hiv.inc.per.pop = reshape2::melt(hiv.inc.cumulative.per.pop)
df.cvd.events.per.pop = reshape2::melt(cvd.events.cumulative.per.pop)
df.hiv.deaths.per.pop = reshape2::melt(hiv.deaths.cumulative.per.pop)
df.cvd.deaths.per.pop = reshape2::melt(cvd.deaths.cumulative.per.pop)
df.relative.redux.hiv.events.per.pop = reshape2::melt((relative.redux.hiv.events.per.pop[,-1]/100))
df.relative.redux.hiv.deaths.per.pop = reshape2::melt((relative.redux.hiv.deaths.per.pop[,-1]/100))
df.relative.redux.events.per.pop = reshape2::melt((relative.redux.events.per.pop[,-1]/100))
df.relative.redux.deaths.per.pop = reshape2::melt((relative.redux.deaths.per.pop[,-1]/100))
df.reduction.in.events.per.pop.per.trt = reshape2::melt(reduction.in.events.per.pop.per.trt[,-1])
df.reduction.in.deaths.per.pop.per.trt = reshape2::melt(reduction.in.deaths.per.pop.per.trt[,-1])

df.reduction.in.events.per.pop.per.trt.clinic = reshape2::melt(reduction.in.events.per.pop.per.trt[,2:4])
df.reduction.in.deaths.per.pop.per.trt.clinic = reshape2::melt(reduction.in.deaths.per.pop.per.trt[,2:4])

df.reduction.in.events.per.pop.per.trt.community = reshape2::melt(reduction.in.events.per.pop.per.trt[,5:7])
df.reduction.in.deaths.per.pop.per.trt.community = reshape2::melt(reduction.in.deaths.per.pop.per.trt[,5:7])

## Figure 1 is the model figure 

## Figure 2: Calibration to HIV model – population size by HIV 
jpeg(file=paste0("figures/Fig2.jpeg"), width = 2500,height = 1500,res=200)
simplot(khm.simset.full[[1]],ncd.simset[[1]],data.type = "population",facet.by = "hiv.status",scale.population = T,
        years = as.character(2015:2040),Fig.1 = T) +
  scale_color_manual(name = "Model", 
                        values=alpha(c(cols[1],cols[5]),.2)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "bottom") + 
  labs(title = element_blank(),
      subtitle = element_blank()) 
dev.off()

jpeg(file=paste0("figures/Fig2A.jpeg"), width = 2500,height = 1500,res=200)
simplot(khm.simset.full[[1]],ncd.simset[[1]],data.type = "hiv.prevalence",scale.population = T,
        years = as.character(2015:2040),Fig.1 = T) +
  scale_color_manual(name = "Model", 
                     values=alpha(c(cols[1],cols[5]),.2)) +
  theme_bw() +
  theme(text = element_text(size = 40),
        legend.position = "none") + 
  labs(title = element_blank(),
       subtitle = element_blank()) 
dev.off()

jpeg(file=paste0("figures/Fig2B.jpeg"), width = 2500,height = 1500,res=200)
simplot(khm.simset.full[[1]],ncd.simset[[1]],data.type = "hiv.incidence",scale.population = T,
        years = as.character(2015:2040),Fig.1 = T) +
  scale_color_manual(name = "Model", 
                     values=alpha(c(cols[1],cols[5]),.6)) +
  theme_bw() +
  theme(text = element_text(size = 40),
        legend.position = "none") + 
  labs(title = element_blank(),
       subtitle = element_blank()) 
dev.off()


jpeg(file=paste0("figures/FigS1.jpeg"), width = 2500,height = 1500,res=200)
simplot(khm.simset.full[[1]],ncd.simset[[1]],data.type = "population",facet.by = "age",scale.population = T,
        years = as.character(2015:2040)) +
  scale_color_manual(name = "Model", 
                     values=alpha(c(cols[1],cols[5]),.2)) +
  theme_bw() +
  theme(text = element_text(size = 15),
        legend.position = "bottom",
        axis.text.x = element_text(size = 10)) + 
  labs(title = element_blank(),
       subtitle = element_blank()) 
dev.off()

## Figure 3: NCD burden over time, no interventions (by disease/comorbidity type)
hyp.prev = simplot.ncd.prevalence.baseline(ncd.simset[[1]], data.type = "hyp.prev", 
                                           combine.comorbidity = T,years = as.character(2015:2040),for.paper = T,no.title = T) 
diab.prev = simplot.ncd.prevalence.baseline(ncd.simset[[1]], data.type = "diab.prev", 
                                            combine.comorbidity = T,years = as.character(2015:2040),for.paper = T, no.title = T)
diab.hyp.prev = simplot.ncd.prevalence.baseline(ncd.simset[[1]], data.type = "diab.hyp.prev", 
                                                combine.comorbidity = F,years = as.character(2015:2040),for.paper = T, no.title = T) 

jpeg(file=paste0("figures/Fig3.jpeg"), width = 2000,height = 750,res=200)
grid.arrange(hyp.prev, diab.prev, diab.hyp.prev, ncol=3)
dev.off()

## Figure 3B
for(i in 1:nrow(df.ncd.burden.by.age)){
  df.ncd.burden.by.age$plot.label[i] = paste0(format(round(df.ncd.burden.by.age$value[i]),big.mark=",")," (",
                                              round(all.proportions[i]),"%)")
}

jpeg(file=paste0("figures/Fig3B.jpeg"), width = 2000,height = 1000,res=200)
ggplot(data = df.ncd.burden.by.age,                         
       aes(x = age,
           y = value,
           fill = ncd)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  geom_text(aes(label=ifelse((ncd=="NCD negative" | ncd=="Hypertension"),plot.label,"")), position=position_stack(vjust = 0.5))+
  facet_grid(~ year) + 
  theme_bw()+
  theme(legend.position = "bottom",
        #panel.background = element_blank(),
        text = element_text(size = 15)) +
  xlab("Age")+
  scale_fill_manual(name = NULL,
                    values = rev(brewer.pal(4,"Set3")),
                    labels = c("Comorbid hypertension and diabetes","Diabetes","Hypertension","No hypertension or diabetes"),
                    guide = guide_legend(reverse = T))+
  scale_y_continuous(labels = function(x){format(x,big.mark=",",scientific = FALSE)},name = NULL, limits = c(0,NA))
dev.off()

## Figure 4A: HIV treatment coverage 
jpeg(file=paste0("figures/Fig4A.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.hiv.coverage, aes(color=intervention, y=value/100, x=year)) + 
  geom_line() + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        panel.grid.major.x = element_line(color = "grey", linetype="dotted"),
        legend.position = "none",
        text = element_text(size = 15))+
  ylab(NULL) + 
  scale_y_continuous(labels = scales::percent,name = NULL, limits = c(NA,NA))
dev.off()

## Figure 4B (version 1): Cumulative HIV inc, per 100,000 pop 
jpeg(file=paste0("figures/Fig4B_numbers.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.hiv.inc.per.pop, aes(x=intervention, y=value, color=intervention)) + 
  stat_summary(fun.data = quantiles_95, geom="errorbar",width=0.1,size=0.5) + 
  stat_summary(fun="median", geom="point",size=3,pch=19)+
  guides(color="none") + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15)) + 
  xlab(NULL)+
  ylab(NULL) 
dev.off()

## Figure 4B (version 2): % Reduction in HIV inc, relative to baseline
jpeg(file=paste0("figures/Fig4B_percent_redux.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.relative.redux.hiv.events.per.pop, aes(x=intervention, y=value, color=intervention)) + 
  stat_summary(fun.data = quantiles_95, geom="errorbar",width=0.1,size=0.5) + 
  stat_summary(fun="median", geom="point",size=3,pch=19)+
  guides(color="none") + 
  scale_y_continuous(labels = scales::percent,name = NULL) +
  scale_color_manual(values=cols[2:7]) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15)) + 
  xlab(NULL)+
  ylab(NULL)
dev.off()

## Figure 4C (version 1): Cumulative HIV deaths, per 100,000 pop
jpeg(file=paste0("figures/Fig4C_numbers.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.hiv.deaths.per.pop, aes(x=intervention, y=value, color=intervention)) + 
  stat_summary(fun.data = quantiles_95, geom="errorbar",width=0.1,size=0.5) + 
  stat_summary(fun="median", geom="point",size=3,pch=19)+
  guides(color="none") + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15)) + 
  xlab(NULL)+
  ylab(NULL)
dev.off()

## Figure 4C (version 2): % Reduction in HIV deaths, relative to baseline
jpeg(file=paste0("figures/Fig4C_percent_redux.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.relative.redux.hiv.deaths.per.pop, aes(x=intervention, y=value, color=intervention)) + 
  stat_summary(fun.data = quantiles_95, geom="errorbar",width=0.1,size=0.5) + 
  stat_summary(fun="median", geom="point",size=3,pch=19)+
  guides(color="none") + 
  scale_y_continuous(labels = scales::percent,name = NULL) +
  scale_color_manual(values=cols[2:7]) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15)) + 
  xlab(NULL)+
  ylab(NULL)
dev.off()

## Figure 4D: NCD treatment coverage 
jpeg(file=paste0("figures/Fig4D.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.ncd.coverage, aes(color=intervention, y=value/100, x=year)) + 
  geom_line() + ylim(0,NA) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        panel.grid.major.x = element_line(color = "grey", linetype="dotted"),
        legend.position = "none",
        text = element_text(size = 15))+
  ylab(NULL)+ 
  scale_y_continuous(labels = scales::percent,name = NULL)
dev.off()


## Figure 4E (version 1): Cumulative CVD events, per 100,000 pop
jpeg(file=paste0("figures/Fig4E_numbers.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.cvd.events.per.pop, aes(x=intervention, y=value, color=intervention)) + 
  stat_summary(fun.data = quantiles_95, geom="errorbar",width=0.1,size=0.5) + 
  stat_summary(fun="median", geom="point",size=3,pch=19)+
  guides(color="none") + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15)) + 
  xlab(NULL)+
  ylab(NULL)
dev.off()

## Figure 4E (version 2, same as 5A): % Reduction in CVD events, relative to baseline
jpeg(file=paste0("figures/Fig4E_percent_redux.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.relative.redux.events.per.pop, aes(x=intervention, y=value, color=intervention)) + 
  stat_summary(fun.data = quantiles_95, geom="errorbar",width=0.1,size=0.5) + 
  stat_summary(fun="median", geom="point",size=3,pch=19)+
  guides(color="none") + 
  scale_y_continuous(labels = scales::percent,name = NULL) +
  scale_color_manual(values=cols[2:7]) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15)) + 
  xlab(NULL)+
  ylab(NULL)
dev.off()

## Figure 4F (version 1): Cumulative CVD deaths, per 100,000 pop
jpeg(file=paste0("figures/Fig4F_numbers.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.cvd.deaths.per.pop, aes(x=intervention, y=value, color=intervention)) + 
  stat_summary(fun.data = quantiles_95, geom="errorbar",width=0.1,size=0.5) + 
  stat_summary(fun="median", geom="point",size=3,pch=19)+
  guides(color="none") + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15)) + 
  xlab(NULL)+
  ylab(NULL)
dev.off()

## Figure 4F (version 2): % Reduction in CVD deaths, relative to baseline
jpeg(file=paste0("figures/Fig4F_percent_redux.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.relative.redux.deaths.per.pop, aes(x=intervention, y=value, color=intervention)) + 
  stat_summary(fun.data = quantiles_95, geom="errorbar",width=0.1,size=0.5) + 
  stat_summary(fun="median", geom="point",size=3,pch=19)+
  guides(color="none") + 
  scale_y_continuous(labels = scales::percent,name = NULL) +
  scale_color_manual(values=cols[2:7]) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15)) + 
  xlab(NULL)+
  ylab(NULL)
dev.off()


## Figure 5C - Reduction in CVD events, relative to baseline, per 1k treated
jpeg(file=paste0("figures/Fig5C.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.reduction.in.events.per.pop.per.trt, aes(x=intervention, y=value, color=intervention)) + 
  stat_summary(fun.data = quantiles_95, geom="errorbar",width=0.1,size=0.5) + 
  #stat_summary(fun="median", geom="point",size=10,pch=4)+
  stat_summary(fun="median", geom="point",size=3,pch=19)+
  #stat_summary(fun="median", geom="point",size=3,pch=1,color="black")+
  scale_color_manual(values=cols[2:7]) +
  guides(color="none") + 
  # labs(title = paste0("Reduction in CVD deaths, relative to baseline, per 1k treated, 2023-2040")) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15)) + 
  xlab(NULL)+
  ylab(NULL)
dev.off()


## Figure 5D - Reduction in CVD deaths, relative to baseline, per 1k treated
jpeg(file=paste0("figures/Fig5D.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.reduction.in.deaths.per.pop.per.trt, aes(x=intervention, y=value, color=intervention)) + 
  stat_summary(fun.data = quantiles_95, geom="errorbar",width=.1,size=0.5) + 
  stat_summary(fun="median", geom="point",size=3,pch=19)+
  scale_color_manual(values=cols[2:7]) +
  guides(color="none") + 
  # labs(title = paste0("Reduction in CVD deaths, relative to baseline, per 1k treated, 2023-2040")) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15)) + 
  xlab(NULL)+
  ylab(NULL)
dev.off()
