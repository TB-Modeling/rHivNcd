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


SA.SCENARIOS = c(1:26)
REPS.FOR.SA = c(1:10)
n.reps.for.sa = length(REPS.FOR.SA)
SA.DIR = "~/Library/CloudStorage/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/rockfish_outputs_sa/"

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



cumulative.cvd.deaths.base = apply(sa.base.results.array.cumulative[,,,,c("n.deaths.cvd"),,],c("rep","intervention"),sum)
cumulative.cvd.deaths.7 = apply(sa.7.results.array.cumulative[,,,,c("n.deaths.cvd"),,],c("rep","intervention"),sum)
sa.reduction.in.cumulative.cvd.deaths = cumulative.cvd.deaths.base-cumulative.cvd.deaths.7


sa.reduction.in.cumulative.cvd.deaths = apply(sa.reduction.in.cumulative.cvd.deaths,c("intervention"),median)
dim(sa.reduction.in.cumulative.cvd.deaths) = length(SA.SCENARIOS)
dimnames(sa.reduction.in.cumulative.cvd.deaths) = list(sa.scenario = SA.SCENARIOS)
sa.reduction.in.cumulative.cvd.deaths

sa.results = data.frame(id = sapply(saScenarios, function(scen){scen$id}),
                        param = sapply(saScenarios, function(scen){scen$param}),
                        newVal = sapply(saScenarios, function(scen){scen$newVal}),
                        original.redux.cumulative.cvd.deaths = rep(redux.deaths.summary[6],26),
                        new.redux.cumulative.cvd.deaths = sa.reduction.in.cumulative.cvd.deaths)

sa.results$change = sa.results$new.redux.cumulative.cvd.deaths - sa.results$original.redux.cumulative.cvd.deaths
