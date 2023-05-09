source("globalEnvironment.R")
source("person.R")
source("population.R")
source("rHelperFunctions.R")
source("rCoreFunctions.R")
source("plots.R")
source("postProcessingFunctions.R") # moved all functions to this file so I can just source it 
source("plots_simplified.R")

START.YEAR=2015
INT.START.YEAR=2023
INT.END.YEAR=2030
END.YEAR=2040 #you can alternatively set this to 2040
n.trt.years = (END.YEAR+1)-INT.START.YEAR

SCENARIOS = c(1:7)
HIV.SCENARIOS = c("noint", # NCD scen 1
                  "noint", # NCD scen 2
                  "retsupp",  # NCD scen 3
                  "retsupp",  # NCD scen 4
                  "noint",  # NCD scen 5
                  "tsteng",  # NCD scen 6
                  "comp"  # NCD scen 7
)
REPLICATIONS = c(1:100) 
n.reps=length(REPLICATIONS)
OUTPUTS.DIR = "~/Library/CloudStorage/OneDrive-JohnsHopkins/MELISSA/Model/rHivNcd/outputs/0509/"

ncd.simset=read.ncd.simset()
khm.simset.full = read.khm.simset.full()

results.array.cumulative = generate.cumulative.events.results.array(ncd.simset,n.reps=n.reps,
                                                                    years=as.character(c(INT.START.YEAR:END.YEAR)))
results.array.annual = generate.annual.events.results.array(ncd.simset,n.reps=n.reps,
                                                            years = as.character(c(START.YEAR:(END.YEAR)))) 


## Figures for paper 
reps = REPLICATIONS
interventions = paste0("scen_",SCENARIOS)
years = as.character(c(2022:2040))

## Figure 1: Calibration to HIV model – population size by HIV 
jpeg(file=paste0("plots/for_paper/Fig1.jpeg"), width = 2500,height = 1500,res=200)
simplot(khm.simset.full[[1]],ncd.simset[[1]],data.type = "population",facet.by = "hiv.status",scale.population = T,
        years = as.character(2015:2040))
dev.off()

## Figure 2: NCD burden over time, no interventions (by disease/comorbidity type); 
hyp.prev = simplot.ncd.prevalence.baseline(ncd.simset[[1]], data.type = "hyp.prev", combine.comorbidity = T,years = as.character(2015:2040))
diab.prev = simplot.ncd.prevalence.baseline(ncd.simset[[1]], data.type = "diab.prev", combine.comorbidity = T,years = as.character(2015:2040))
diab.hyp.prev = simplot.ncd.prevalence.baseline(ncd.simset[[1]], data.type = "diab.hyp.prev", combine.comorbidity = F,years = as.character(2015:2040))
  
jpeg(file=paste0("plots/for_paper/Fig2.jpeg"), width = 2500,height = 1000,res=200)
grid.arrange(hyp.prev, diab.prev, diab.hyp.prev, ncol=3)  
dev.off()


## Figure 3: 
# (A) Increase in number of people on HIV treatment
# (B) increase in number of people on NCD treatment
# (C) cumulative number of HIV incidence
# (D) cumulative number of CVD events (can combine mi and stroke inc)
# (E) cumulative HIV deaths
# (F) cumulative CVD deaths

## (A) HIV trt coverage
hiv.on.treatment.annual = apply(results.array.annual[,,4,,,"n.state.sizes",,],c("year","rep","intervention"),sum)
hiv.pop.annual = apply(results.array.annual[,,-1,,,"n.state.sizes",,],c("year","rep","intervention"),sum)

hiv.treatment.coverage = (hiv.on.treatment.annual/hiv.pop.annual)*100
hiv.treatment.coverage.median = apply(hiv.treatment.coverage,c("year","intervention"),median)

hiv.treatment.coverage.median = hiv.treatment.coverage.median[years,]
dimnames(hiv.treatment.coverage.median)[2] = list(intervention=interventions)
df.hiv.coverage = reshape2::melt(hiv.treatment.coverage.median)

jpeg(file=paste0("plots/for_paper/Fig3A.jpeg"), width = 1000,height = 500,res=200)
ggplot(df.hiv.coverage, aes(color=intervention, y=value, x=year)) + 
  geom_line() + ylim(0,NA) 
dev.off()



## (B) NCD trt coverage
ncd.on.treatment.annual = apply(results.array.annual[,,,c(5:7),,"n.state.sizes",,],c("year","rep","intervention"),sum)
ncd.pop.annual = apply(results.array.annual[,,,-1,,"n.state.sizes",,],c("year","rep","intervention"),sum)

ncd.treatment.coverage = (ncd.on.treatment.annual/ncd.pop.annual)*100
ncd.treatment.coverage.median = apply(ncd.treatment.coverage,c("year","intervention"),median)

ncd.treatment.coverage.median = ncd.treatment.coverage.median[years,]
dimnames(ncd.treatment.coverage.median)[2] = list(intervention=interventions)
df.ncd.coverage = reshape2::melt(ncd.treatment.coverage.median)

jpeg(file=paste0("plots/for_paper/Fig3B.jpeg"), width = 1000,height = 500,res=200)
ggplot(df.ncd.coverage, aes(color=intervention, y=value, x=year)) + 
  geom_line() + ylim(0,NA)
dev.off()


## (C) HIV inc
jpeg(file=paste0("plots/for_paper/Fig3C.jpeg"), width = 1000,height = 500,res=200)
plot.cumulative.outcome.boxplot(ncd.simset[[1]],ncd.simset[[2]],ncd.simset[[3]],
                                ncd.simset[[4]],ncd.simset[[5]],ncd.simset[[6]],ncd.simset[[7]],
                                data.types=c("n.hiv.inc"),dimension="total",n.reps = n.reps) 
dev.off()


## (D) CVD inc
jpeg(file=paste0("plots/for_paper/Fig3D.jpeg"), width = 1000,height = 500,res=200)
plot.cumulative.outcome.boxplot(ncd.simset[[1]],ncd.simset[[2]],ncd.simset[[3]],
                                ncd.simset[[4]],ncd.simset[[5]],ncd.simset[[6]],ncd.simset[[7]],
                                data.types=c("n.cvd.events"),dimension="total",n.reps = n.reps) +ylim(10000,NA)

dev.off()


## (E) HIV deaths
jpeg(file=paste0("plots/for_paper/Fig3E.jpeg"), width = 1000,height = 500,res=200)
plot.cumulative.outcome.boxplot(ncd.simset[[1]],ncd.simset[[2]],ncd.simset[[3]],
                                ncd.simset[[4]],ncd.simset[[5]],ncd.simset[[6]],ncd.simset[[7]],
                                data.types=c("n.deaths.hiv"),dimension="total",n.reps = n.reps) 
dev.off()

## (F) CVD deaths
jpeg(file=paste0("plots/for_paper/Fig3F.jpeg"), width = 1000,height = 500,res=200)
plot.cumulative.outcome.boxplot(ncd.simset[[1]],ncd.simset[[2]],ncd.simset[[3]],
                                ncd.simset[[4]],ncd.simset[[5]],ncd.simset[[6]],ncd.simset[[7]],
                                data.types=c("n.deaths.cvd"),dimension="total",n.reps = n.reps) + ylim(5000,NA)
dev.off()

# plot.cumulative.outcome.boxplot(ncd.simset[[5]],ncd.simset[[6]],ncd.simset[[7]],
#                                 data.types=c("n.deaths.cvd"),dimension="total") + ylim(5000,NA)


## Figure 4: Reduction per # on treatment 
mi.events.cumulative = apply(results.array.cumulative[,,,,"n.mi.inc",,],c("rep","intervention"),sum)
stroke.events.cumulative = apply(results.array.cumulative[,,,,"n.stroke.inc",,],c("rep","intervention"),sum)
cvd.events.cumulative = mi.events.cumulative + stroke.events.cumulative


reduction.in.cumulative.events.2 = cvd.events.cumulative[,1]-cvd.events.cumulative[,2]
reduction.in.cumulative.events.3 = cvd.events.cumulative[,1]-cvd.events.cumulative[,3]
reduction.in.cumulative.events.4 = cvd.events.cumulative[,1]-cvd.events.cumulative[,4]
reduction.in.cumulative.events.5 = cvd.events.cumulative[,1]-cvd.events.cumulative[,5]
reduction.in.cumulative.events.6 = cvd.events.cumulative[,1]-cvd.events.cumulative[,6]
reduction.in.cumulative.events.7 = cvd.events.cumulative[,1]-cvd.events.cumulative[,7]


reduction.dim.names = list(rep=reps,
                           intervention=interventions[-1])


reduction.in.cumulative.events = array(c(reduction.in.cumulative.events.2,
                                         reduction.in.cumulative.events.3,
                                         reduction.in.cumulative.events.4,
                                         reduction.in.cumulative.events.5,
                                         reduction.in.cumulative.events.6,
                                         reduction.in.cumulative.events.7),
                                       dim = sapply(reduction.dim.names,length),
                                       dimnames = reduction.dim.names)

cvd.deaths.cumulative = apply(results.array.cumulative[,,,,"n.deaths.cvd",,],c("rep","intervention"),sum)


reduction.in.cumulative.deaths.2 = cvd.deaths.cumulative[,1]-cvd.deaths.cumulative[,2]
reduction.in.cumulative.deaths.3 = cvd.deaths.cumulative[,1]-cvd.deaths.cumulative[,3]
reduction.in.cumulative.deaths.4 = cvd.deaths.cumulative[,1]-cvd.deaths.cumulative[,4]
reduction.in.cumulative.deaths.5 = cvd.deaths.cumulative[,1]-cvd.deaths.cumulative[,5]
reduction.in.cumulative.deaths.6 = cvd.deaths.cumulative[,1]-cvd.deaths.cumulative[,6]
reduction.in.cumulative.deaths.7 = cvd.deaths.cumulative[,1]-cvd.deaths.cumulative[,7]

reduction.in.cumulative.deaths = array(c(reduction.in.cumulative.deaths.2,
                                         reduction.in.cumulative.deaths.3,
                                         reduction.in.cumulative.deaths.4,
                                         reduction.in.cumulative.deaths.5,
                                         reduction.in.cumulative.deaths.6,
                                         reduction.in.cumulative.deaths.7),
                                       dim = sapply(reduction.dim.names,length),
                                       dimnames = reduction.dim.names)


## (A) - person-time on treatment - cumulative/# years (8)
ncd.on.treatment.cumulative = apply(ncd.on.treatment.annual,c("rep","intervention"),sum)
dimnames(ncd.on.treatment.cumulative)[2] = list(intervention=interventions)
#ncd.on.treatment.cumulative=ncd.on.treatment.cumulative[,c(1,5:7)]

trt.df = reshape2::melt(ncd.on.treatment.cumulative[,-1]/n.trt.years)

jpeg(file=paste0("plots/for_paper/Fig4A.jpeg"), width = 1000,height = 500,res=200)
ggplot(trt.df, aes(x=intervention, y=value, fill=intervention)) + 
  geom_boxplot()+ ylim(0,NA)
dev.off()

# jpeg(file=paste0("plots/for_paper/Fig4A.2.jpeg"), width = 1000,height = 500,res=200)
# ggplot(trt.df, aes(x=intervention, y=value, fill=intervention)) + 
#   geom_boxplot()+ ylim(20000,NA)
# dev.off()



## (B) - reduction in events 
reduction.in.events.per.trt = (reduction.in.cumulative.events/(ncd.on.treatment.cumulative[,-1]/n.trt.years))*10000 # per 10,000?
# reduction.in.events.per.trt = (reduction.in.cumulative.events/(ncd.on.treatment.annual["2030",,-1]))*10000
#reduction.in.events.per.trt=reduction.in.events.per.trt[,1:3]
# reduction.in.events.per.trt=reduction.in.events.per.trt[,4:6]
redux.events.df = reshape2::melt(reduction.in.events.per.trt)


jpeg(file=paste0("plots/for_paper/Fig4B.jpeg"), width = 1000,height = 500,res=200)
ggplot(redux.events.df, aes(x=intervention, y=value, fill=intervention)) + 
  geom_boxplot() #+ ylim(0,NA)
dev.off()

# jpeg(file=paste0("plots/for_paper/Fig4B.2.jpeg"), width = 1000,height = 500,res=200)
# ggplot(redux.events.df, aes(x=intervention, y=value, fill=intervention)) + 
#   geom_boxplot() #+ ylim(0,NA)
# dev.off()



## (C) - reduction in deaths
reduction.in.deaths.per.trt = (reduction.in.cumulative.deaths/(ncd.on.treatment.cumulative[,-1]/n.trt.years))*10000
#reduction.in.deaths.per.trt=reduction.in.deaths.per.trt[,1:3]
#reduction.in.deaths.per.trt=reduction.in.deaths.per.trt[,4:6]
redux.deaths.df = reshape2::melt(reduction.in.deaths.per.trt)

jpeg(file=paste0("plots/for_paper/Fig4C.jpeg"), width = 1000,height = 500,res=200)
ggplot(redux.deaths.df, aes(x=intervention, y=value, fill=intervention)) + 
  geom_boxplot() #+ ylim(0,NA)
dev.off()

# jpeg(file=paste0("plots/for_paper/Fig4C.2.jpeg"), width = 1000,height = 500,res=200)
# ggplot(redux.deaths.df, aes(x=intervention, y=value, fill=intervention)) +
#   geom_boxplot() #+ ylim(0,NA)
# dev.off()

## VALUES FOR FIGURE 3 ## 
# HIV inc
cumulative.hiv.inc = apply(results.array.cumulative[,,,,"n.hiv.inc",,],c("rep","intervention"),sum)
cumulative.hiv.inc = apply(cumulative.hiv.inc,c("intervention"),median)
dim(cumulative.hiv.inc) = length(interventions)
dimnames(cumulative.hiv.inc) = list(intervention = interventions)

# CVD inc
cumulative.cvd.inc = apply(results.array.cumulative[,,,,c("n.mi.inc","n.stroke.inc"),,],c("rep","intervention"),sum)
cumulative.cvd.inc = apply(cumulative.cvd.inc,c("intervention"),median)
dim(cumulative.cvd.inc) = length(interventions)
dimnames(cumulative.cvd.inc) = list(intervention = interventions)
cumulative.cvd.inc # this matches the median in the plots 

# HIV deaths
cumulative.hiv.deaths = apply(results.array.cumulative[,,,,c("n.deaths.hiv"),,],c("rep","intervention"),sum)
cumulative.hiv.deaths = apply(cumulative.hiv.deaths,c("intervention"),median)
dim(cumulative.hiv.deaths) = length(interventions)
dimnames(cumulative.hiv.deaths) = list(intervention = interventions)

# CVD deaths
cumulative.cvd.deaths = apply(results.array.cumulative[,,,,c("n.deaths.cvd"),,],c("rep","intervention"),sum)
cumulative.cvd.deaths = apply(cumulative.cvd.deaths,c("intervention"),median)
dim(cumulative.cvd.deaths) = length(interventions)
dimnames(cumulative.cvd.deaths) = list(intervention = interventions)





## OTHER DEBUGGING/PLOTTING 
apply(reduction.in.cumulative.events,2,median) # the jump in event reduction from 3-4 is decent
apply(reduction.in.cumulative.deaths,2,median) # the jump in event reduction from 3-4 is decent
apply((ncd.on.treatment.cumulative[,-1]/n.trt.years),2,median) # but you are also treating a lot more people

10000*apply(reduction.in.cumulative.events,2,median)/apply((ncd.on.treatment.cumulative[,-1]/n.trt.years),2,median)

apply(reduction.in.events.per.trt,2,median)
apply(reduction.in.deaths.per.trt,2,median)

apply(cvd.events.cumulative,2,median)
cumulative.cvd.inc
apply((ncd.on.treatment.cumulative[,-1]/n.trt.years),2,median)

