source("globalEnvironment.R")
source("person.R")
source("population.R")
source("rHelperFunctions.R")
source("rCoreFunctions.R")
source("plots.R")
source("results/postProcessingFunctions.R") # moved all functions to this file so I can just source it 
source("results/plots_simplified.R")

library(scales)
cols = hue_pal()(2)

START.YEAR=2015
INT.START.YEAR=2023
INT.END.YEAR=2030
END.YEAR=2040 #you can alternatively set this to 2040
n.trt.years = (END.YEAR+1)-INT.START.YEAR
PER.POPULATION.SIZE = 100000

SA.SCENARIOS = c(1:26)
REPS.FOR.SA = c(1:50)
n.reps.for.sa = length(REPS.FOR.SA)
SA.DIR = "~/Library/CloudStorage/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rockfish_outputs_sa/"

# if running for the first time - otherwise, load the saved results arrays 
if(1==2){
  sa.simset = read.ncd.simset.sa() # list with two elements (2 ncd scenarios); each ncd scenario has 26 sa scenarios; each has 10 reps 
  sa.simset.baseline=sa.simset[[1]]
  sa.simset.scen.7=sa.simset[[2]]
  
  sa.base.results.array.cumulative = generate.cumulative.events.results.array(sa.simset.baseline,n.reps=n.reps.for.sa,
                                                                              years=as.character(c(INT.START.YEAR:END.YEAR)))
  sa.7.results.array.cumulative = generate.cumulative.events.results.array(sa.simset.scen.7,n.reps=n.reps.for.sa,
                                                                           years=as.character(c(INT.START.YEAR:END.YEAR)))
  
  sa.base.results.array.annual = generate.annual.events.results.array(sa.simset.baseline,n.reps=n.reps.for.sa,
                                                                      years = as.character(c(START.YEAR:(END.YEAR)))) 
  sa.7.results.array.annual = generate.annual.events.results.array(sa.simset.scen.7,n.reps=n.reps.for.sa,
                                                                   years = as.character(c(START.YEAR:(END.YEAR)))) 
  
  save(sa.base.results.array.cumulative, file = paste0("outputs/sa.base.results.cumulative_",Sys.Date(),".Rdata"))
  save(sa.base.results.array.annual, file = paste0("outputs/sa.base.results.annual_",Sys.Date(),".Rdata"))
  
  save(sa.7.results.array.cumulative, file = paste0("outputs/sa.7.results.cumulative_",Sys.Date(),".Rdata"))
  save(sa.7.results.array.annual, file = paste0("outputs/sa.7.results.annual_",Sys.Date(),".Rdata"))
}

if(1==2){
  load("~/Library/CloudStorage/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/outputs/sa.base.results.annual_2023-05-13.Rdata")
  load("~/Library/CloudStorage/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/outputs/sa.base.results.cumulative_2023-05-13.Rdata")
  load("~/Library/CloudStorage/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/outputs/sa.7.results.annual_2023-05-13.Rdata") 
  load("~/Library/CloudStorage/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rHivNcd/outputs/sa.7.results.cumulative_2023-05-13.Rdata")
}

# Get total population sizes to standardize it
total.pop.annual.sa.base = apply(sa.base.results.array.annual[,,,,as.character(c(INT.START.YEAR:END.YEAR)),"n.state.sizes",,],c("year","rep","intervention"),sum)
total.pop.cumulative.sa.base = apply(total.pop.annual.sa.base,c("rep","intervention"),sum)
total.pop.cumulative.sa.base = total.pop.cumulative.sa.base/n.trt.years

total.pop.annual.sa.7 = apply(sa.7.results.array.annual[,,,,as.character(c(INT.START.YEAR:END.YEAR)),"n.state.sizes",,],c("year","rep","intervention"),sum)
total.pop.cumulative.sa.7 = apply(total.pop.annual.sa.7,c("rep","intervention"),sum)
total.pop.cumulative.sa.7 = total.pop.cumulative.sa.7/n.trt.years

# Get cumulative deaths, standardize 
cumulative.cvd.deaths.base = apply(sa.base.results.array.cumulative[,,,,c("n.deaths.cvd"),,],c("rep","intervention"),sum)
cumulative.cvd.deaths.base.per.pop = (cumulative.cvd.deaths.base/total.pop.cumulative.sa.base)*PER.POPULATION.SIZE

cumulative.cvd.deaths.7 = apply(sa.7.results.array.cumulative[,,,,c("n.deaths.cvd"),,],c("rep","intervention"),sum)
cumulative.cvd.deaths.7.per.pop = (cumulative.cvd.deaths.7/total.pop.cumulative.sa.7)*PER.POPULATION.SIZE

sa.redux.cvd.deaths.per.pop = cumulative.cvd.deaths.base.per.pop-cumulative.cvd.deaths.7.per.pop
sa.relative.redux.deaths.per.pop = (sa.redux.cvd.deaths.per.pop/cumulative.cvd.deaths.base.per.pop)*100

sa.relative.redux.deaths.per.pop.summary = apply(sa.relative.redux.deaths.per.pop,c("intervention"),median)
dim(sa.relative.redux.deaths.per.pop.summary) = length(SA.SCENARIOS)
dimnames(sa.relative.redux.deaths.per.pop.summary) = list(sa.scenario = SA.SCENARIOS)
sa.relative.redux.deaths.per.pop.summary

sa.relative.redux.deaths.per.pop.change = ((sa.relative.redux.deaths.per.pop - relative.redux.deaths.per.pop.summary[2,7])/relative.redux.deaths.per.pop.summary[2,7])

sa.relative.redux.deaths.per.pop.change.quantiles = apply(sa.relative.redux.deaths.per.pop.change,c("intervention"),quantile,probs=c(0.025,.25,.5,.75,.975))

sa.results = cbind(reshape2::melt(sa.relative.redux.deaths.per.pop.change.quantiles["50%",]),
                   data.frame(lower.2=as.numeric(sa.relative.redux.deaths.per.pop.change.quantiles["2.5%",]),
                              lower.1=as.numeric(sa.relative.redux.deaths.per.pop.change.quantiles["25%",]),
                              upper.1=as.numeric(sa.relative.redux.deaths.per.pop.change.quantiles["75%",]),
                              upper.2=as.numeric(sa.relative.redux.deaths.per.pop.change.quantiles["97.5%",])))

sa.results$subset = c("high","high",rep(c("low","high"),12))
sa.results$parameter = sapply(saScenarios, function(scen){scen$param})
parameter.names = c("Relative risk of NCDs,\n HIV-pos v neg","Annual increase in\n NCD prevalence",
                    rep(c("CVD event risk multiplier","Relative risk of CVD events,\n HIV-pos v neg",
                          "Relative risk of CVD events,\n history of events",
                          "First event MI, male","First event MI, female",
                          "CVD mortality multiplier","Odds ratio of mortality,\n recurrent stroke",
                          "Relative risk of mortality,\n recurrent MI",
                          "CVD event risk,\n on hypertension treatment",
                          "CVD death risk,\n on hypertension treatment",
                          "CVD event risk,\n on diabetes treatment",
                          "CVD death risk,\n on diabetes treatment"),each=2))
sa.results$parameter.name = parameter.names
sa.results = rbind(sa.results,data.frame(value=NA, lower.2 = NA, lower.1 = NA, upper.1 = NA, upper.2 = NA, subset = "low",
                                         parameter = NA, parameter.name = c("Relative risk of NCDs,\n HIV-pos v neg","Annual increase in\n NCD prevalence")))

sa.results$parameter.name=factor(sa.results$parameter.name,levels=rev(unique(parameter.names)))
sa.results$subset = factor(sa.results$subset, levels=c("low","high"))

jpeg(file=paste0("plots/for_paper/Fig5.jpeg"), width = 2500,height = 1500,res=200)
ggplot(sa.results) + geom_boxplot(aes(y=parameter.name,xmiddle=value,xlower = lower.1,xupper = upper.1, xmin = lower.2, xmax = upper.2, 
                                      fill=subset), stat="identity", position="dodge") +
  geom_vline(xintercept = 0, linetype="dashed") + 
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        legend.justification = "right",
        axis.title.y = element_blank(),
        text = element_text(size = 15)) +
  scale_x_continuous(labels = scales::percent) + 
  labs(x = "Percent change in projected reductions in CVD deaths under scenario 7 relative to no intervention") + 
  scale_fill_manual(name = element_blank(),
                    labels = c("low" = "Simulations with the lower sensitivity value",
                               "high" = "Simulations with the upper sensitivity value"),
                    values = cols) 
# scale_y_discrete(labels = function(x) str_wrap(x, width = 10))
dev.off()

sa.results.summary = data.frame(id = sapply(saScenarios, function(scen){scen$id}),
                                param = sapply(saScenarios, function(scen){scen$param}),
                                newVal = sapply(saScenarios, function(scen){scen$newVal}),
                                original.rel.redux.deaths = rep(relative.redux.deaths.per.pop.summary[2,7],26),
                                new.rel.redux.deaths = sa.relative.redux.deaths.per.pop.summary)

sa.results.summary$change = sa.results.summary$new.rel.redux.deaths - sa.results.summary$original.rel.redux.deaths
sa.results.summary$percent.change = sa.results.summary$change/sa.results.summary$original.rel.redux.deaths

range(sa.results.summary$change/sa.results.summary$original.rel.redux.deaths)
