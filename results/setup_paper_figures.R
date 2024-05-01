source("globalEnvironment.R")
source("person.R")
source("population.R")
source("rHelperFunctions.R")
source("rCoreFunctions.R")
source("plots.R")
source("results/postProcessingFunctions.R") 
source("results/plots_simplified.R")

START.YEAR=2015
INT.START.YEAR=2023
INT.END.YEAR=2030
END.YEAR=2040 
n.trt.years = (END.YEAR+1)-INT.START.YEAR

PER.POPULATION.SIZE = 100000

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
#OUTPUTS.DIR = "outputs/"

interventions = paste0("scen_",SCENARIOS)
interventions.full.names = list(interventions = c("(1) No intervention","(2) Basic NCD care @HIV clinics",
                                                  "(3) Basic NCD care + HIV retention @HIV clinics","(4) Intensive NCD care + HIV retention @HIV clinics",
                                                  "(5) Basic NCD care @ community","(6) Basic NCD care + HIV care @ community",
                                                  "(7) Intensive NCD care + comprehensive HIV care @ community"))
years = as.character(c(2022:2040))


# if I need to read in new Rockfish outputs
if(1==2){
  ncd.simset=read.ncd.simset()
  khm.simset.full = read.khm.simset.full()
  
  results.array.cumulative = generate.cumulative.events.results.array(ncd.simset,n.reps=n.reps,
                                                                      years=as.character(c(INT.START.YEAR:END.YEAR)))
  results.array.annual = generate.annual.events.results.array(ncd.simset,n.reps=n.reps,
                                                              years = as.character(c(START.YEAR:(END.YEAR)))) 
}

# if I have already saved results
if(1==1){
  # load("outputs/ncd.simset_2024-05-01.Rdata")
  # load("outputs/ncd.results.array.cumulative_2024-05-01.Rdata")
  # load("outputs/ncd.results.array.annual_2024-05-01.Rdata")
  
  load("outputs/outputs_backup/ncd.simset_2023-05-09.Rdata")
  load("outputs/outputs_backup/ncd.results.array.cumulative_2023-05-09.Rdata")
  load("outputs/outputs_backup/ncd.results.array.annual_2023-05-09.Rdata")
  
  khm.simset.full = read.khm.simset.full()
  
}

quantiles_95 = function(x) {
  r <- quantile(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

table.dim.names = list(quantiles = c("2.5%","50%","97.5%"),
                       intervention = interventions)


## NCD burden by age over time ##
{
  # first 6 age groups, 0-30
  age.0.to.30.neg = apply(results.array.annual[1:6,,,c("NCD.NEG"),c("2023","2040"),"n.state.sizes",,1],c("year","rep"),sum) 
  age.0.to.30.hyp = apply(results.array.annual[1:6,,,c("NCD.HYP","NCD.HYP.TRT"),c("2023","2040"),"n.state.sizes",,1],c("year","rep"),sum)
  age.0.to.30.diab = apply(results.array.annual[1:6,,,c("NCD.DIAB","NCD.DIAB.TRT"),c("2023","2040"),"n.state.sizes",,1],c("year","rep"),sum) 
  age.0.to.30.comorbid = apply(results.array.annual[1:6,,,c("NCD.DIAB_HYP","NCD.DIAB_HYP.TRT"),c("2023","2040"),"n.state.sizes",,1],c("year","rep"),sum) 
  
  # 30-50
  age.30.to.50.neg = apply(results.array.annual[7:10,,,c("NCD.NEG"),c("2023","2040"),"n.state.sizes",,1],c("year","rep"),sum) 
  age.30.to.50.hyp = apply(results.array.annual[7:10,,,c("NCD.HYP","NCD.HYP.TRT"),c("2023","2040"),"n.state.sizes",,1],c("year","rep"),sum)
  age.30.to.50.diab = apply(results.array.annual[7:10,,,c("NCD.DIAB","NCD.DIAB.TRT"),c("2023","2040"),"n.state.sizes",,1],c("year","rep"),sum) 
  age.30.to.50.comorbid = apply(results.array.annual[7:10,,,c("NCD.DIAB_HYP","NCD.DIAB_HYP.TRT"),c("2023","2040"),"n.state.sizes",,1],c("year","rep"),sum) 
  
  # 50+
  age.50.plus.neg = apply(results.array.annual[11:17,,,c("NCD.NEG"),c("2023","2040"),"n.state.sizes",,1],c("year","rep"),sum) 
  age.50.plus.hyp = apply(results.array.annual[11:17,,,c("NCD.HYP","NCD.HYP.TRT"),c("2023","2040"),"n.state.sizes",,1],c("year","rep"),sum)
  age.50.plus.diab = apply(results.array.annual[11:17,,,c("NCD.DIAB","NCD.DIAB.TRT"),c("2023","2040"),"n.state.sizes",,1],c("year","rep"),sum) 
  age.50.plus.comorbid = apply(results.array.annual[11:17,,,c("NCD.DIAB_HYP","NCD.DIAB_HYP.TRT"),c("2023","2040"),"n.state.sizes",,1],c("year","rep"),sum) 
  
  dim.names.burden = list(year = c("2023","2040"),
                          ncd = c("Comorbid hypertension and diabetes","Diabetes","Hypertension","NCD negative"),
                          age = c("0-30","30-50","50+"))
  
  # Median of reps 
  age.0.to.30.neg = apply(age.0.to.30.neg,"year",median)
  age.0.to.30.hyp = apply(age.0.to.30.hyp,"year",median)
  age.0.to.30.diab = apply(age.0.to.30.diab,"year",median)
  age.0.to.30.comorbid = apply(age.0.to.30.comorbid,"year",median)
  
  age.0.to.30.burden = array(c(age.0.to.30.comorbid,age.0.to.30.diab,age.0.to.30.hyp,age.0.to.30.neg),
                             dim = sapply(dim.names.burden[-3],length),
                             dimnames = dim.names.burden[-3])
  
  age.30.to.50.neg = apply(age.30.to.50.neg,"year",median)
  age.30.to.50.hyp = apply(age.30.to.50.hyp,"year",median)
  age.30.to.50.diab = apply(age.30.to.50.diab,"year",median)
  age.30.to.50.comorbid = apply(age.30.to.50.comorbid,"year",median)
  
  age.30.to.50.burden = array(c(age.30.to.50.comorbid,age.30.to.50.diab,age.30.to.50.hyp,age.30.to.50.neg),
                             dim = sapply(dim.names.burden[-3],length),
                             dimnames = dim.names.burden[-3])
  
  age.50.plus.neg = apply(age.50.plus.neg,"year",median)
  age.50.plus.hyp = apply(age.50.plus.hyp,"year",median)
  age.50.plus.diab = apply(age.50.plus.diab,"year",median)
  age.50.plus.comorbid = apply(age.50.plus.comorbid,"year",median)
  
  age.50.plus.burden = array(c(age.50.plus.comorbid,age.50.plus.diab,age.50.plus.hyp,age.50.plus.neg),
                              dim = sapply(dim.names.burden[-3],length),
                              dimnames = dim.names.burden[-3])
  
  ncd.burden.by.age = array(c(age.0.to.30.burden,age.30.to.50.burden,age.50.plus.burden),
                            dim = sapply(dim.names.burden,length),
                            dimnames = dim.names.burden)
  
  # FOR DESCRIPTION IN THE TEXT
  age.0.to.30.proportions = (age.0.to.30.burden/rowSums(age.0.to.30.burden))*100
  age.30.to.50.proportions = (age.30.to.50.burden/rowSums(age.30.to.50.burden))*100
  age.50.plus.proportions = (age.50.plus.burden/rowSums(age.50.plus.burden))*100
  all.proportions = c(age.0.to.30.proportions,age.30.to.50.proportions,age.50.plus.proportions)
  
  df.ncd.burden.by.age = reshape2::melt(ncd.burden.by.age)
}

## HIV TREATMENT COVERAGE 
{
  hiv.on.treatment.annual = apply(results.array.annual[,,4,,,"n.state.sizes",,],c("year","rep","intervention"),sum)
  hiv.pop.annual = apply(results.array.annual[,,-1,,,"n.state.sizes",,],c("year","rep","intervention"),sum)
  
  hiv.treatment.coverage = (hiv.on.treatment.annual/hiv.pop.annual)*100
  hiv.treatment.coverage.median = apply(hiv.treatment.coverage,c("year","intervention"),median)
  
  hiv.on.treatment.cumulative = apply(hiv.on.treatment.annual,c("rep","intervention"),sum)
  dimnames(hiv.on.treatment.cumulative)[2] = list(intervention=interventions)
  
  hiv.treatment.coverage.median = hiv.treatment.coverage.median[years,]
  dimnames(hiv.treatment.coverage.median)[2] = list(intervention=interventions)
}



## NCD TREATMENT COVERAGE
{
  ncd.on.treatment.annual = apply(results.array.annual[,,,c(5:7),,"n.state.sizes",,],c("year","rep","intervention"),sum)
  ncd.pop.annual = apply(results.array.annual[,,,-1,,"n.state.sizes",,],c("year","rep","intervention"),sum)
  
  ncd.treatment.coverage = (ncd.on.treatment.annual/ncd.pop.annual)*100
  ncd.treatment.coverage.median = apply(ncd.treatment.coverage,c("year","intervention"),median)
  ncd.treatment.coverage.median = ncd.treatment.coverage.median[years,]
  dimnames(ncd.treatment.coverage.median)[2] = list(intervention=interventions)
  
  hiv.ncd.on.treatment.annual = apply(results.array.annual[,,4,-1,,"n.state.sizes",,],c("year","rep","intervention"),sum)
  ncd.treatment.coverage.among.hiv = (ncd.on.treatment.annual/hiv.ncd.on.treatment.annual)*100 # to get coverage among HIV population (eligible population for scen 2-4)
  ncd.treatment.coverage.among.hiv.median = apply(ncd.treatment.coverage.among.hiv,c("year","intervention"),median)
  ncd.treatment.coverage.among.hiv.median = ncd.treatment.coverage.among.hiv.median[years,]
  dimnames(ncd.treatment.coverage.among.hiv.median)[2] = list(intervention=interventions)
  
  ncd.on.treatment.cumulative = apply(ncd.on.treatment.annual,c("rep","intervention"),sum)
  dimnames(ncd.on.treatment.cumulative)[2] = list(intervention=interventions)
  
  number.treated.summary = apply((ncd.on.treatment.cumulative/n.trt.years),2,quantile,probs=c(0.025,.5,.975))
  dim(number.treated.summary) = sapply(table.dim.names,length)
  dimnames(number.treated.summary) = table.dim.names
}

## CUMULATIVE HIV EVENTS & DEATHS
{
  # Events
  hiv.inc.cumulative = apply(results.array.cumulative[,,,,"n.hiv.inc",,],c("rep","intervention"),sum)
  dimnames(hiv.inc.cumulative)[[2]] = interventions
  
  hiv.inc.cumulative.summary = apply(hiv.inc.cumulative,c("intervention"),quantile,probs=c(0.025,.5,.975))
  dim(hiv.inc.cumulative.summary) = sapply(table.dim.names,length)
  dimnames(hiv.inc.cumulative.summary) = table.dim.names
  
  # Deaths
  hiv.deaths.cumulative = apply(results.array.cumulative[,,,,c("n.deaths.hiv"),,],c("rep","intervention"),sum)
  dimnames(hiv.deaths.cumulative)[[2]] = interventions
  
  hiv.deaths.cumulative.summary = apply(hiv.deaths.cumulative,c("intervention"),quantile,probs=c(0.025,.5,.975))
  dim(hiv.deaths.cumulative.summary) = sapply(table.dim.names,length)
  dimnames(hiv.deaths.cumulative.summary) = table.dim.names
  
}

## CUMULATIVE CVD EVENTS & DEATHS
{
  # Events
  mi.events.cumulative = apply(results.array.cumulative[,,,,"n.mi.inc",,],c("rep","intervention"),sum)
  stroke.events.cumulative = apply(results.array.cumulative[,,,,"n.stroke.inc",,],c("rep","intervention"),sum)
  cvd.events.cumulative = mi.events.cumulative + stroke.events.cumulative
  dimnames(cvd.events.cumulative)[[2]] = interventions
  
  cvd.events.cumulative.summary = apply(cvd.events.cumulative,c("intervention"),quantile,probs=c(0.025,.5,.975))
  dim(cvd.events.cumulative.summary) = sapply(table.dim.names,length)
  dimnames(cvd.events.cumulative.summary) = table.dim.names
  
  # Deaths
  cvd.deaths.cumulative = apply(results.array.cumulative[,,,,"n.deaths.cvd",,],c("rep","intervention"),sum)
  dimnames(cvd.deaths.cumulative)[[2]] = interventions
  
  cvd.deaths.cumulative.summary = apply(cvd.deaths.cumulative,c("intervention"),median)
  dim(cvd.deaths.cumulative.summary) = length(interventions)
  dimnames(cvd.deaths.cumulative.summary) = list(intervention = interventions)
}

## CUMULATIVE HIV EVENTS & DEATHS, per XX population 
{
  total.pop.annual = apply(results.array.annual[,,,,as.character(c(INT.START.YEAR:END.YEAR)),"n.state.sizes",,],c("year","rep","intervention"),sum)
  total.pop.cumulative = apply(total.pop.annual,c("rep","intervention"),sum)
  total.pop.cumulative = total.pop.cumulative/n.trt.years
  total.pop.cumulative.summary = apply(total.pop.cumulative,c("intervention"),quantile,probs=c(0.025,.5,.975))
  
  hiv.inc.cumulative.per.pop = (hiv.inc.cumulative/total.pop.cumulative)*PER.POPULATION.SIZE
  hiv.inc.cumulative.per.pop.summary = apply(hiv.inc.cumulative.per.pop,c("intervention"),quantile,probs=c(0.025,.5,.975))
  
  hiv.deaths.cumulative.per.pop = (hiv.deaths.cumulative/total.pop.cumulative)*PER.POPULATION.SIZE
  hiv.deaths.cumulative.per.pop.summary = apply(hiv.deaths.cumulative.per.pop,c("intervention"),quantile,probs=c(0.025,.5,.975))
}


## CUMULATIVE CVD EVENTS & DEATHS, per XX population 
{
  cvd.events.cumulative.per.pop = (cvd.events.cumulative/total.pop.cumulative)*PER.POPULATION.SIZE
  cvd.events.cumulative.per.pop.summary = apply(cvd.events.cumulative.per.pop,c("intervention"),quantile,probs=c(0.025,.5,.975))
  
  cvd.deaths.cumulative.per.pop = (cvd.deaths.cumulative/total.pop.cumulative)*PER.POPULATION.SIZE
  cvd.deaths.cumulative.per.pop.summary = apply(cvd.deaths.cumulative.per.pop,c("intervention"),quantile,probs=c(0.025,.5,.975))
}


## ABSOLUTE REDUCTION IN CUMULATIVE EVENTS & DEATHS 
{
  redux.dim.names = list(rep=REPLICATIONS,
                         intervention=interventions)
  
  # HIV Events
  reduction.in.cumulative.hiv.events.2 = hiv.inc.cumulative[,1]-hiv.inc.cumulative[,2]
  reduction.in.cumulative.hiv.events.3 = hiv.inc.cumulative[,1]-hiv.inc.cumulative[,3]
  reduction.in.cumulative.hiv.events.4 = hiv.inc.cumulative[,1]-hiv.inc.cumulative[,4]
  reduction.in.cumulative.hiv.events.5 = hiv.inc.cumulative[,1]-hiv.inc.cumulative[,5]
  reduction.in.cumulative.hiv.events.6 = hiv.inc.cumulative[,1]-hiv.inc.cumulative[,6]
  reduction.in.cumulative.hiv.events.7 = hiv.inc.cumulative[,1]-hiv.inc.cumulative[,7]
  reduction.in.cumulative.hiv.events = array(c(rep(0,100),
                                           reduction.in.cumulative.hiv.events.2,
                                           reduction.in.cumulative.hiv.events.3,
                                           reduction.in.cumulative.hiv.events.4,
                                           reduction.in.cumulative.hiv.events.5,
                                           reduction.in.cumulative.hiv.events.6,
                                           reduction.in.cumulative.hiv.events.7),
                                         dim = sapply(redux.dim.names,length),
                                         dimnames = redux.dim.names)
  
  redux.hiv.events.summary = apply(reduction.in.cumulative.hiv.events,2,quantile,probs = c(.025,.5,.975))
  
  # HIV Deaths
  reduction.in.cumulative.hiv.deaths.2 = hiv.deaths.cumulative[,1]-hiv.deaths.cumulative[,2]
  reduction.in.cumulative.hiv.deaths.3 = hiv.deaths.cumulative[,1]-hiv.deaths.cumulative[,3]
  reduction.in.cumulative.hiv.deaths.4 = hiv.deaths.cumulative[,1]-hiv.deaths.cumulative[,4]
  reduction.in.cumulative.hiv.deaths.5 = hiv.deaths.cumulative[,1]-hiv.deaths.cumulative[,5]
  reduction.in.cumulative.hiv.deaths.6 = hiv.deaths.cumulative[,1]-hiv.deaths.cumulative[,6]
  reduction.in.cumulative.hiv.deaths.7 = hiv.deaths.cumulative[,1]-hiv.deaths.cumulative[,7]
  reduction.in.cumulative.hiv.deaths = array(c(rep(0,100),
                                           reduction.in.cumulative.hiv.deaths.2,
                                           reduction.in.cumulative.hiv.deaths.3,
                                           reduction.in.cumulative.hiv.deaths.4,
                                           reduction.in.cumulative.hiv.deaths.5,
                                           reduction.in.cumulative.hiv.deaths.6,
                                           reduction.in.cumulative.hiv.deaths.7),
                                         dim = sapply(redux.dim.names,length),
                                         dimnames = redux.dim.names)
  
  redux.hiv.deaths.summary = apply(reduction.in.cumulative.hiv.deaths,2,quantile,probs = c(.025,.5,.975))
  
  
  
  
  # Events
  reduction.in.cumulative.events.2 = cvd.events.cumulative[,1]-cvd.events.cumulative[,2]
  reduction.in.cumulative.events.3 = cvd.events.cumulative[,1]-cvd.events.cumulative[,3]
  reduction.in.cumulative.events.4 = cvd.events.cumulative[,1]-cvd.events.cumulative[,4]
  reduction.in.cumulative.events.5 = cvd.events.cumulative[,1]-cvd.events.cumulative[,5]
  reduction.in.cumulative.events.6 = cvd.events.cumulative[,1]-cvd.events.cumulative[,6]
  reduction.in.cumulative.events.7 = cvd.events.cumulative[,1]-cvd.events.cumulative[,7]
  reduction.in.cumulative.events = array(c(rep(0,100),
                                           reduction.in.cumulative.events.2,
                                           reduction.in.cumulative.events.3,
                                           reduction.in.cumulative.events.4,
                                           reduction.in.cumulative.events.5,
                                           reduction.in.cumulative.events.6,
                                           reduction.in.cumulative.events.7),
                                         dim = sapply(redux.dim.names,length),
                                         dimnames = redux.dim.names)
  
  redux.events.summary = apply(reduction.in.cumulative.events,2,quantile,probs = c(.025,.5,.975))
  
  # Deaths
  reduction.in.cumulative.deaths.2 = cvd.deaths.cumulative[,1]-cvd.deaths.cumulative[,2]
  reduction.in.cumulative.deaths.3 = cvd.deaths.cumulative[,1]-cvd.deaths.cumulative[,3]
  reduction.in.cumulative.deaths.4 = cvd.deaths.cumulative[,1]-cvd.deaths.cumulative[,4]
  reduction.in.cumulative.deaths.5 = cvd.deaths.cumulative[,1]-cvd.deaths.cumulative[,5]
  reduction.in.cumulative.deaths.6 = cvd.deaths.cumulative[,1]-cvd.deaths.cumulative[,6]
  reduction.in.cumulative.deaths.7 = cvd.deaths.cumulative[,1]-cvd.deaths.cumulative[,7]
  reduction.in.cumulative.deaths = array(c(rep(0,100),
                                           reduction.in.cumulative.deaths.2,
                                           reduction.in.cumulative.deaths.3,
                                           reduction.in.cumulative.deaths.4,
                                           reduction.in.cumulative.deaths.5,
                                           reduction.in.cumulative.deaths.6,
                                           reduction.in.cumulative.deaths.7),
                                         dim = sapply(redux.dim.names,length),
                                         dimnames = redux.dim.names)
  
  redux.deaths.summary = apply(reduction.in.cumulative.deaths,2,quantile,probs = c(.025,.5,.975))
  
  
}

## ABSOLUTE REDUCTION IN CUMULATIVE EVENTS & DEATHS, per XX population 
{
  redux.hiv.events.per.pop = (reduction.in.cumulative.hiv.events/total.pop.cumulative)*PER.POPULATION.SIZE
  redux.hiv.events.per.pop.summary = apply(redux.hiv.events.per.pop,c("intervention"),quantile,probs=c(0.025,.5,.975))
  
  redux.hiv.deaths.per.pop = (reduction.in.cumulative.hiv.deaths/total.pop.cumulative)*PER.POPULATION.SIZE
  redux.hiv.deaths.per.pop.summary = apply(redux.hiv.deaths.per.pop,c("intervention"),quantile,probs=c(0.025,.5,.975))
  
  redux.events.per.pop = (reduction.in.cumulative.events/total.pop.cumulative)*PER.POPULATION.SIZE
  redux.events.per.pop.summary = apply(redux.events.per.pop,c("intervention"),quantile,probs=c(0.025,.5,.975))
  
  redux.deaths.per.pop = (reduction.in.cumulative.deaths/total.pop.cumulative)*PER.POPULATION.SIZE
  redux.deaths.per.pop.summary = apply(redux.deaths.per.pop,c("intervention"),quantile,probs=c(0.025,.5,.975))
  
  
}


## RELATIVE REDUCTION IN CUMULATIVE EVENTS & DEATHS
# ((events.baseline - events.scenario)/events.baseline)*100
{
  # Events 
  relative.redux.events = (reduction.in.cumulative.events/cvd.events.cumulative[,1])*100
  relative.redux.events.summary = apply(relative.redux.events,2,quantile,probs = c(.025,.5,.975))
  
  # Deaths
  relative.redux.deaths = (reduction.in.cumulative.deaths/cvd.deaths.cumulative[,1])*100
  relative.redux.deaths.summary = apply(relative.redux.deaths,2,quantile,probs = c(.025,.5,.975))
  
}

## RELATIVE REDUCTION IN CUMULATIVE EVENTS & DEATHS, per XX population
{
  # HIV Events 
  relative.redux.hiv.events.per.pop = (redux.hiv.events.per.pop/hiv.inc.cumulative.per.pop[,1])*100
  relative.redux.hiv.events.per.pop.summary = apply(relative.redux.hiv.events.per.pop,2,quantile,probs = c(.025,.5,.975))
  
  # HIV Deaths
  relative.redux.hiv.deaths.per.pop = (redux.hiv.deaths.per.pop/hiv.deaths.cumulative.per.pop[,1])*100
  relative.redux.hiv.deaths.per.pop.summary = apply(relative.redux.hiv.deaths.per.pop,2,quantile,probs = c(.025,.5,.975))
  
  # Events 
  relative.redux.events.per.pop = (redux.events.per.pop/cvd.events.cumulative.per.pop[,1])*100
  relative.redux.events.per.pop.summary = apply(relative.redux.events.per.pop,2,quantile,probs = c(.025,.5,.975))
  
  # Deaths
  relative.redux.deaths.per.pop = (redux.deaths.per.pop/cvd.deaths.cumulative.per.pop[,1])*100
  relative.redux.deaths.per.pop.summary = apply(relative.redux.deaths.per.pop,2,quantile,probs = c(.025,.5,.975))
  
}


## REDUCTION IN EVENTS & DEATHS PER 10,000 TREATED
{
  reduction.in.events.per.trt = (reduction.in.cumulative.events/(ncd.on.treatment.cumulative/n.trt.years))*10000 
  reduction.in.events.per.trt[,1]=0
  reduction.in.events.per.trt.summary = apply(reduction.in.events.per.trt,2,quantile,probs = c(.025,.5,.975))
  
  reduction.in.deaths.per.trt = (reduction.in.cumulative.deaths/(ncd.on.treatment.cumulative/n.trt.years))*10000
  reduction.in.deaths.per.trt[,1]=0
  reduction.in.deaths.per.trt.summary = apply(reduction.in.deaths.per.trt,2,quantile,probs = c(.025,.5,.975))
}


## NUMBER TREATED PER XX POPULATION
{
  number.treated.per.pop = ((ncd.on.treatment.cumulative/n.trt.years)/total.pop.cumulative)*PER.POPULATION.SIZE
  
  number.treated.per.pop.summary = apply(number.treated.per.pop,2,quantile,probs=c(0.025,.5,.975))
  dim(number.treated.per.pop.summary) = sapply(table.dim.names,length)
  dimnames(number.treated.per.pop.summary) = table.dim.names
  
  
  hiv.number.treated.per.pop = ((hiv.on.treatment.cumulative/n.trt.years)/total.pop.cumulative)*PER.POPULATION.SIZE
  
  hiv.number.treated.per.pop.summary = apply(hiv.number.treated.per.pop,2,quantile,probs=c(0.025,.5,.975))
  dim(hiv.number.treated.per.pop.summary) = sapply(table.dim.names,length)
  dimnames(hiv.number.treated.per.pop.summary) = table.dim.names
  
}

## REDUCTION IN EVENTS & DEATHS (per XX population) PER POP TREATED (per XX population)
{
  PER.N.TREATED = 1000
  
  reduction.in.events.per.pop.per.trt = (redux.events.per.pop/number.treated.per.pop)*PER.N.TREATED
  reduction.in.events.per.pop.per.trt[,1]=0
  reduction.in.events.per.pop.per.trt.summary = apply(reduction.in.events.per.pop.per.trt,2,quantile,probs = c(.025,.5,.975))
  
  reduction.in.deaths.per.pop.per.trt = (redux.deaths.per.pop/number.treated.per.pop)*PER.N.TREATED
  reduction.in.deaths.per.pop.per.trt[,1]=0
  reduction.in.deaths.per.pop.per.trt.summary = apply(reduction.in.deaths.per.pop.per.trt,2,quantile,probs = c(.025,.5,.975))
}



##--------------------##
##-- SUMMARY TABLE  --##
##--------------------##

# Scaled to PER.POPULATION.SIZE value:
treated.tab = c(paste0(round(number.treated.per.pop.summary[2,])," [",
                       round(number.treated.per.pop.summary[1,]),"-",
                       round(number.treated.per.pop.summary[3,]),"]"))

hiv.treated.tab = c(paste0(round(hiv.number.treated.per.pop.summary[2,])," [",
                       round(hiv.number.treated.per.pop.summary[1,]),"-",
                       round(hiv.number.treated.per.pop.summary[3,]),"]"))

hiv.inc.tab = c(paste0(round(hiv.inc.cumulative.per.pop.summary[2,])," [",
                       round(hiv.inc.cumulative.per.pop.summary[1,]),"-",
                       round(hiv.inc.cumulative.per.pop.summary[3,]),"]"))

hiv.deaths.tab = c(paste0(round(hiv.deaths.cumulative.per.pop.summary[2,])," [",
                       round(hiv.deaths.cumulative.per.pop.summary[1,]),"-",
                       round(hiv.deaths.cumulative.per.pop.summary[3,]),"]"))

cvd.events.tab = c(paste0(round(cvd.events.cumulative.per.pop.summary[2,])," [",
                          round(cvd.events.cumulative.per.pop.summary[1,]),"-",
                          round(cvd.events.cumulative.per.pop.summary[3,]),"]"))

cvd.deaths.tab = c(paste0(round(cvd.deaths.cumulative.per.pop.summary[2,])," [",
                          round(cvd.deaths.cumulative.per.pop.summary[1,]),"-",
                          round(cvd.deaths.cumulative.per.pop.summary[3,]),"]"))

redux.hiv.events.tab = c(paste0(round(redux.hiv.events.per.pop.summary[2,])," [",
                            round(redux.hiv.events.per.pop.summary[1,]),"-",
                            round(redux.hiv.events.per.pop.summary[3,]),"]"))

redux.hiv.deaths.tab = c(paste0(round(redux.hiv.deaths.per.pop.summary[2,])," [",
                            round(redux.hiv.deaths.per.pop.summary[1,]),"-",
                            round(redux.hiv.deaths.per.pop.summary[3,]),"]"))

redux.events.tab = c(paste0(round(redux.events.per.pop.summary[2,])," [",
                          round(redux.events.per.pop.summary[1,]),"-",
                          round(redux.events.per.pop.summary[3,]),"]"))

redux.deaths.tab = c(paste0(round(redux.deaths.per.pop.summary[2,])," [",
                            round(redux.deaths.per.pop.summary[1,]),"-",
                            round(redux.deaths.per.pop.summary[3,]),"]"))

rel.redux.hiv.events.tab = c(paste0(round(relative.redux.hiv.events.per.pop.summary[2,],1),"% [",
                                round(relative.redux.hiv.events.per.pop.summary[1,],1),"-",
                                round(relative.redux.hiv.events.per.pop.summary[3,],1),"]"))

rel.redux.hiv.deaths.tab = c(paste0(round(relative.redux.hiv.deaths.per.pop.summary[2,],1),"% [",
                                round(relative.redux.hiv.deaths.per.pop.summary[1,],1),"-",
                                round(relative.redux.hiv.deaths.per.pop.summary[3,],1),"]"))

rel.redux.events.tab = c(paste0(round(relative.redux.events.per.pop.summary[2,],1),"% [",
                                round(relative.redux.events.per.pop.summary[1,],1),"-",
                                round(relative.redux.events.per.pop.summary[3,],1),"]"))

rel.redux.deaths.tab = c(paste0(round(relative.redux.deaths.per.pop.summary[2,],1),"% [",
                            round(relative.redux.deaths.per.pop.summary[1,],1),"-",
                            round(relative.redux.deaths.per.pop.summary[3,],1),"]"))

rel.redux.events.per.trt.tab = c(paste0(round(reduction.in.events.per.pop.per.trt.summary[2,],1)," [",
                                round(reduction.in.events.per.pop.per.trt.summary[1,],1),"-",
                                round(reduction.in.events.per.pop.per.trt.summary[3,],1),"]"))

rel.redux.deaths.per.trt.tab = c(paste0(round(reduction.in.deaths.per.pop.per.trt.summary[2,],1)," [",
                                round(reduction.in.deaths.per.pop.per.trt.summary[1,],1),"-",
                                round(reduction.in.deaths.per.pop.per.trt.summary[3,],1),"]"))

full.tab = rbind(treated.tab,
                 hiv.treated.tab,
                 hiv.inc.tab,
                 hiv.deaths.tab,
                 cvd.events.tab,
                 cvd.deaths.tab,
                 redux.hiv.events.tab,
                 redux.hiv.deaths.tab,
                 redux.events.tab,
                 redux.deaths.tab,
                 rel.redux.hiv.events.tab,
                 rel.redux.hiv.deaths.tab,
                 rel.redux.events.tab,
                 rel.redux.deaths.tab,
                 rel.redux.events.per.trt.tab,
                 rel.redux.deaths.per.trt.tab)

dim.names.tab = list(outcomes = c("n.treated.per.X","n.hiv.treated.per.X","n.hiv.inc.per.X","n.hiv.deaths.per.X",
                                  "n.cvd.events.per.X","n.cvd.deaths.per.X",
                                  "redux.hiv.events.per.X","redux.hiv.deaths.per.X",
                                  "redux.cvd.events.per.X","redux.cvd.deaths.per.X",
                                  "rel.redux.hiv.events.per.X","rel.redux.hiv.deaths.per.X",
                                  "rel.redux.cvd.events.per.X","rel.redux.cvd.deaths.per.X",
                                  "redux.cvd.events.per.X.per.1k.trt","redux.cvd.deaths.per.X.per.1k.trt"),
                     interventions = interventions.full.names$interventions)

dim(full.tab) = sapply(dim.names.tab,length)
dimnames(full.tab) = dim.names.tab

write.csv(full.tab, file="results/full_results_table_with_hiv_v2.csv")

{
  hiv.inc.cumulative.per.pop.summary
  hiv.deaths.cumulative.per.pop.summary
  cvd.events.cumulative.per.pop.summary
  cvd.deaths.cumulative.per.pop.summary
  
  redux.events.per.pop.summary
  redux.deaths.per.pop.summary
  
  relative.redux.events.per.pop.summary # these end up being the exact same as above because it's relative to baseline
  relative.redux.deaths.per.pop.summary 
  
  reduction.in.events.per.pop.per.trt.summary
  reduction.in.deaths.per.pop.per.trt.summary
}



{
  # Actual model values (not scaled to per 100,000 pop): 
  hiv.inc.cumulative.summary # HIV incidence 
  hiv.deaths.cumulative.summary # HIV deaths
  cvd.events.cumulative.summary # CVD events 
  cvd.deaths.cumulative.summary # CVD deaths
  
  redux.events.summary # Reduction in CVD events 
  redux.deaths.summary # Reduction in CVD deaths
  
  relative.redux.events.summary # Reduction in CVD events, relative to baseline
  relative.redux.deaths.summary # Reduction in CVD deaths, relative to baseline 
}
