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

model.labels = c("1" = "Compartmental HIV model",
                 "2" = "Individual-based NCD simulation")

# Use this to save the intervention label
ggplot(df.hiv.inc.per.pop, aes(x=intervention, y=value, fill=intervention)) + 
  stat_summary(fun.data = quantiles_95, geom="boxplot")  + 
  theme(legend.position = "bottom") +
  scale_fill_manual(labels = labels, 
                    values=cols)

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


## Figure 1: Calibration to HIV model – population size by HIV 
jpeg(file=paste0("plots/for_paper/Fig1.jpeg"), width = 2500,height = 1500,res=200)
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

jpeg(file=paste0("plots/for_paper/Fig1A.jpeg"), width = 2500,height = 1500,res=200)
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

jpeg(file=paste0("plots/for_paper/Fig1B.jpeg"), width = 2500,height = 1500,res=200)
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


jpeg(file=paste0("plots/for_paper/FigS1.jpeg"), width = 2500,height = 1500,res=200)
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

## Figure 2: NCD burden over time, no interventions (by disease/comorbidity type)
hyp.prev = simplot.ncd.prevalence.baseline(ncd.simset[[1]], data.type = "hyp.prev", 
                                           combine.comorbidity = T,years = as.character(2015:2040),for.paper = T,no.title = T) 
diab.prev = simplot.ncd.prevalence.baseline(ncd.simset[[1]], data.type = "diab.prev", 
                                            combine.comorbidity = T,years = as.character(2015:2040),for.paper = T, no.title = T)
diab.hyp.prev = simplot.ncd.prevalence.baseline(ncd.simset[[1]], data.type = "diab.hyp.prev", 
                                                combine.comorbidity = F,years = as.character(2015:2040),for.paper = T, no.title = T) 

jpeg(file=paste0("plots/for_paper/Fig2.jpeg"), width = 2000,height = 750,res=200)
grid.arrange(hyp.prev, diab.prev, diab.hyp.prev, ncol=3)
dev.off()

## Figure 2B
for(i in 1:nrow(df.ncd.burden.by.age)){
  df.ncd.burden.by.age$plot.label[i] = paste0(format(round(df.ncd.burden.by.age$value[i]),big.mark=",")," (",
                                              round(all.proportions[i]),"%)")
}

jpeg(file=paste0("plots/for_paper/Fig2B.jpeg"), width = 2000,height = 1000,res=200)
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

## Figure 3A: HIV treatment coverage 
jpeg(file=paste0("plots/for_paper/Fig3A.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
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

## Figure 3B: Cumulative HIV inc, per 100,000 pop 
jpeg(file=paste0("plots/for_paper/Fig3B.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.hiv.inc.per.pop, aes(x=intervention, y=value, fill=intervention)) + 
  guides(fill="none") + 
  stat_summary(fun.data = quantiles_95, geom="boxplot")  + 
  # labs(title = paste0("Cumulative HIV infections (per ",PER.POPULATION.SIZE," population size), 2023-2040")) + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15)) + 
  xlab(NULL)+
  ylab(NULL)
dev.off()


## Figure 3C: Cumulative HIV deaths, per 100,000 pop
jpeg(file=paste0("plots/for_paper/Fig3C.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.hiv.deaths.per.pop, aes(x=intervention, y=value, fill=intervention)) + 
  guides(fill="none") + 
  stat_summary(fun.data = quantiles_95, geom="boxplot") +
  # labs(title = paste0("Cumulative HIV deaths (per ",PER.POPULATION.SIZE," population size), 2023-2040")) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15)) + 
  xlab(NULL)+
  ylab(NULL)
dev.off()

## Figure 3D: NCD treatment coverage 
jpeg(file=paste0("plots/for_paper/Fig3D.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
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


## Figure 3E: Cumulative CVD events, per 100,000 pop
jpeg(file=paste0("plots/for_paper/Fig3E.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.cvd.events.per.pop, aes(x=intervention, y=value, fill=intervention)) + 
  guides(fill="none") + 
  stat_summary(fun.data = quantiles_95, geom="boxplot") +
  # labs(title = paste0("Cumulative CVD events (per ",PER.POPULATION.SIZE," population size), 2023-2040")) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15)) + 
  xlab(NULL)+
  ylab(NULL)
dev.off()


## Figure 3F: Cumulative CVD deaths, per 100,000 pop
jpeg(file=paste0("plots/for_paper/Fig3F.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.cvd.deaths.per.pop, aes(x=intervention, y=value, fill=intervention)) + 
  guides(fill="none") + 
  stat_summary(fun.data = quantiles_95, geom="boxplot") +
  # labs(title = paste0("Cumulative CVD deaths (per ",PER.POPULATION.SIZE," population size), 2023-2040")) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15)) + 
  xlab(NULL)+
  ylab(NULL)
dev.off()


## Figure 4A - % Reduction in CVD events, relative to baseline - NEW
jpeg(file=paste0("plots/for_paper/Fig4A.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.relative.redux.events.per.pop, aes(x=intervention, y=value, fill=intervention)) + 
  guides(fill="none") + 
  scale_y_continuous(labels = scales::percent,name = NULL) +
  stat_summary(fun.data = quantiles_95, geom="boxplot") + 
  scale_fill_manual(values=cols[2:7]) +
  # labs(title = paste0("Percent reduction in CVD events, relative to baseline, 2023-2040")) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15)) + 
  xlab(NULL)+
  ylab(NULL)
dev.off()

## Figure 4B - % Reduction in CVD events, relative to baseline - NEW
jpeg(file=paste0("plots/for_paper/Fig4B.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.relative.redux.deaths.per.pop, aes(x=intervention, y=value, fill=intervention)) + 
  guides(fill="none") + 
  scale_y_continuous(labels = scales::percent,name = NULL) +
  stat_summary(fun.data = quantiles_95, geom="boxplot") + 
  scale_fill_manual(values=cols[2:7]) +
  # labs(title = paste0("Percent reduction in CVD deaths, relative to baseline, 2023-2040")) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15)) + 
  xlab(NULL)+
  ylab(NULL)
dev.off()

## Figure 4C - Reduction in CVD events, relative to baseline, per 1k treated
jpeg(file=paste0("plots/for_paper/Fig4C.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.reduction.in.events.per.pop.per.trt, aes(x=intervention, y=value, fill=intervention)) + 
  guides(fill="none") + 
  stat_summary(fun.data = quantiles_95, geom="boxplot") + 
  scale_fill_manual(values=cols[2:7]) +
  # labs(title = paste0("Reduction in CVD events, relative to baseline, per 1k treated, 2023-2040")) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype="dotted"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 15)) + 
  xlab(NULL)+
  ylab(NULL)
dev.off()

## Figure 4D - Reduction in CVD deaths, relative to baseline, per 1k treated
jpeg(file=paste0("plots/for_paper/Fig4D.jpeg"), width = WIDTH,height = HEIGHT,res=RES)
ggplot(df.reduction.in.deaths.per.pop.per.trt, aes(x=intervention, y=value, fill=intervention)) + 
  guides(fill="none") + 
  stat_summary(fun.data = quantiles_95, geom="boxplot") + 
  scale_fill_manual(values=cols[2:7]) +
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



## OPTION TO SPLIT OUT CLINIC/COMMUNITY (but I actually prefer keeping them on the same scale?):
## Figure 4C - Reduction in CVD events, relative to baseline, per 1k treated, clinic
ggplot(df.reduction.in.events.per.pop.per.trt.clinic, aes(x=intervention, y=value, fill=intervention)) + 
  guides(fill="none") + 
  stat_summary(fun.data = quantiles_95, geom="boxplot") + 
  scale_fill_manual(values=cols[2:4]) +
  labs(title = paste0("Reduction in CVD events (clinic-based interventions), relative to baseline, per 1k treated, 2023-2040"))

## Figure 4D - Reduction in CVD deaths, relative to baseline, per 1k treated, clinic
ggplot(df.reduction.in.deaths.per.pop.per.trt.clinic, aes(x=intervention, y=value, fill=intervention)) + 
  guides(fill="none") + 
  stat_summary(fun.data = quantiles_95, geom="boxplot") + 
  scale_fill_manual(values=cols[2:4]) +
  labs(title = paste0("Reduction in CVD deaths (clinic-based interventions), relative to baseline, per 1k treated, 2023-2040"))

## Figure 4E - Reduction in CVD events, relative to baseline, per 1k treated, community
ggplot(df.reduction.in.events.per.pop.per.trt.community, aes(x=intervention, y=value, fill=intervention)) + 
  guides(fill="none") + 
  stat_summary(fun.data = quantiles_95, geom="boxplot") + 
  scale_fill_manual(values=cols[5:7]) +
  labs(title = paste0("Reduction in CVD events (community-based interventions), relative to baseline, per 1k treated, 2023-2040"))

## Figure 4F - Reduction in CVD deaths, relative to baseline, per 1k treated, community
ggplot(df.reduction.in.deaths.per.pop.per.trt.community, aes(x=intervention, y=value, fill=intervention)) + 
  guides(fill="none") + 
  stat_summary(fun.data = quantiles_95, geom="boxplot") + 
  scale_fill_manual(values=cols[5:7]) +
  labs(title = paste0("Reduction in CVD deaths, relative to baseline, per 1k treated, 2023-2040"))


if(1==2){
  ## REMOVING THESE FIGURES - I think it's fine just in the table 
  ## Figure 3C: Person-time on treatment
  trt.df = reshape2::melt(number.treated.per.pop)
  jpeg(file=paste0("plots/for_paper/Fig4A.jpeg"), width = 1000,height = 500,res=200)
  ggplot(trt.df, aes(x=intervention, y=value, fill=intervention)) + 
    guides(fill="none") + 
    stat_summary(fun.data = quantiles_95, geom="boxplot") + ylim(0,NA) 
  dev.off()
}



## OLD PLOTS (NOT STANDARDIZED TO PER.X.POPULATION)
if(1==2)
{
  
  ## Figure 3C: Cumulative HIV inc
  hiv.inc.df = reshape2::melt(hiv.inc.cumulative)
  jpeg(file=paste0("plots/for_paper/Fig3C.jpeg"), width = 1000,height = 500,res=200)
  ggplot(hiv.inc.df, aes(x=intervention, y=value, fill=intervention)) + 
    guides(fill="none") + 
    stat_summary(fun.data = quantiles_95, geom="boxplot") + ylim(0,NA) # + labs(title = "Cumulative HIV infections, 2023-2040")
  dev.off()
  
  ## Figure 3D: Cumulative CVD events
  cvd.events.df = reshape2::melt(cvd.events.cumulative)
  jpeg(file=paste0("plots/for_paper/Fig3D.jpeg"), width = 1000,height = 500,res=200)
  ggplot(cvd.events.df, aes(x=intervention, y=value, fill=intervention)) + 
    guides(fill="none") + 
    stat_summary(fun.data = quantiles_95, geom="boxplot") + ylim(10000,NA) # + labs(title = "Cumulative CVD events, 2023-2040")
  dev.off()
  
  ## Figure 3E: Cumulative HIV deaths
  hiv.deaths.df = reshape2::melt(hiv.deaths.cumulative)
  jpeg(file=paste0("plots/for_paper/Fig3E.jpeg"), width = 1000,height = 500,res=200)
  ggplot(hiv.deaths.df, aes(x=intervention, y=value, fill=intervention)) + 
    guides(fill="none") + 
    stat_summary(fun.data = quantiles_95, geom="boxplot") + ylim(0,NA) # + labs(title = "Cumulative HIV deaths, 2023-2040")
  dev.off()
  
  ## Figure 3F: Cumulative CVD deaths
  cvd.deaths.df = reshape2::melt(cvd.deaths.cumulative)
  jpeg(file=paste0("plots/for_paper/Fig3F.jpeg"), width = 1000,height = 500,res=200)
  ggplot(cvd.deaths.df, aes(x=intervention, y=value, fill=intervention)) + 
    guides(fill="none") + 
    stat_summary(fun.data = quantiles_95, geom="boxplot") + ylim(5000,NA) # + labs(title = "Cumulative CVD deaths, 2023-2040")
  dev.off()
  
  ## Figure 4B: Reduction in events per number treated
  redux.events.per.trt.df = reshape2::melt(reduction.in.events.per.trt)
  jpeg(file=paste0("plots/for_paper/Fig4B.jpeg"), width = 1000,height = 500,res=200)
  ggplot(redux.events.per.trt.df, aes(x=intervention, y=value, fill=intervention)) + 
    guides(fill="none") + 
    stat_summary(fun.data = quantiles_95, geom="boxplot") #+ labs(title = "Reduction in cumulative CVD events per 10,000 treated, 2023-2040")
  dev.off()
  
  ## Figure 4C: Reduction in deaths per number treated
  redux.deaths.per.trt.df = reshape2::melt(reduction.in.deaths.per.trt)
  jpeg(file=paste0("plots/for_paper/Fig4C.jpeg"), width = 1000,height = 500,res=200)
  ggplot(redux.deaths.per.trt.df, aes(x=intervention, y=value, fill=intervention)) + 
    guides(fill="none") + 
    stat_summary(fun.data = quantiles_95, geom="boxplot") #+ labs(title = "Reduction in cumulative CVD deaths per 10,000 treated, 2023-2040")
  dev.off()
  
  
  
  }