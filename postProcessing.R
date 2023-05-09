source("plots.R")
source("globalEnvironment.R")
source("rHelperFunctions.R")
source("postProcessingFunctions.R") # moved all functions to this file so I can just source it 

START.YEAR=2015
INT.START.YEAR=2023
INT.END.YEAR=2030
END.YEAR=2030 #you can alternatively set this to 2040

SCENARIOS = c(1:7)
HIV.SCENARIOS = c("noint", # NCD scen 1
                  "noint", # NCD scen 2
                  "retsupp",  # NCD scen 3
                  "retsupp",  # NCD scen 4
                  "noint",  # NCD scen 5
                  "tsteng",  # NCD scen 6
                  "comp"  # NCD scen 7
                     )
REPLICATIONS = c(1:36)
n.reps=36
OUTPUTS.DIR = "outputs/outputs-05-08/"

ncd.simset=read.ncd.simset()
#save(ncd.simset, file = paste0("outputs/ncd.simset_",Sys.Date(),"_small.Rdata"))
# khm.simset=read.khm.simset()
khm.simset.full = read.khm.simset.full()

#comparing scenario 3 and 4
#numbers screened is about the same
apply(ncd.simset[[3]][[1]]$n.ncd.screened,"year",sum)[10:17]/apply(ncd.simset[[4]][[1]]$n.ncd.screened,"year",sum)[10:17]
#in scenario 3 we diagnose more people with hypertension and treat more people too
apply(ncd.simset[[3]][[1]]$n.hyp.diag,"year",sum)[10:17]/apply(ncd.simset[[4]][[1]]$n.hyp.diag,"year",sum)[10:17]
apply(ncd.simset[[3]][[1]]$n.hyp.trt,"year",sum)[10:17]/apply(ncd.simset[[4]][[1]]$n.hyp.trt,"year",sum)[10:17]

#total person time on hyp trt
apply(ncd.simset[[3]][[1]]$n.hyp.trt,"year",sum)[10:17] #starting trt
apply(ncd.simset[[3]][[1]]$n.state.sizes,c("ncd.status","year"),sum)[6,][10:17]


#total person time on hyp trt
apply(ncd.simset[[4]][[1]]$n.hyp.trt,"year",sum)[10:17] #starting trt
apply(ncd.simset[[4]][[1]]$n.state.sizes,c("ncd.status","year"),sum)[6,][10:17] #on trt


apply(ncd.simset[[6]][[1]]$n.hyp.diag,"year",sum)[10:17]/apply(ncd.simset[[6]][[1]]$n.ncd.screened,"year",sum)[10:17]
apply(ncd.simset[[7]][[1]]$n.hyp.diag,"year",sum)[10:17]/apply(ncd.simset[[7]][[1]]$n.ncd.screened,"year",sum)[10:17]


#pcoverage is set properly? 
apply(ncd.simset[[3]][[1]]$n.state.sizes,c("hiv.status","year"),sum)[4,][10:17] #number engaged
apply(ncd.simset[[3]][[1]]$n.ncd.screened,"year",sum)[10:17] #screened

#########################################################################################################
# intervention.names = c("1-baseline",
#                        "2-ncd screening at hiv clinic",
#                        "3-ncd care co-located with hiv",
#                        "4-hiv/ncd community screening",
#                        "5-hiv/ncd community screening and co-location")
# names(ncd.simset) = intervention.names

results.array.cumulative = generate.cumulative.events.results.array(ncd.simset,n.reps=n.reps)
# sum(res[,,,,"n.hyp.trt","1","7"])
# sum(apply(ncd.simset[[7]][[1]]$n.hyp.trt,"year",sum))
save(results.array.cumulative, file = paste0("outputs/ncd.results.array.cumulative_",Sys.Date(),".Rdata"))

results.array.annual = generate.annual.events.results.array(ncd.simset,n.reps=n.reps)
res=results.array.annual;dimnames(res)
save(results.array.annual, file = paste0("outputs/ncd.results.array.annual_",Sys.Date(),".Rdata"))


# example of how to use results.array
{
  cumulative.cvd.deaths.by.age = apply(results.array.cumulative[,,,,"n.deaths.cvd",,],c("age","rep","intervention"),sum)
  cumulative.cvd.deaths.by.age = apply(cumulative.cvd.deaths.by.age,c("age","intervention"),median)
  
  cumulative.cvd.deaths.by.sex = apply(results.array.cumulative[,,,,"n.deaths.cvd",,],c("sex","rep","intervention"),sum)
  cumulative.cvd.deaths.by.sex = apply(cumulative.cvd.deaths.by.sex,c("sex","intervention"),median)
  
  # can compare cumulative cvd.deaths.by.sex values with plot to double-check: 
  plot.cumulative.outcome.boxplot(ncd.simset[[1]],ncd.simset[[2]],ncd.simset[[3]],ncd.simset[[4]],ncd.simset[[5]],
                                  data.types=c("n.deaths.cvd"),dimension="sex") 
}



# PLOTTING
{ 
  par(mfrow=c(1,1))
  khm.simset=khm.simset.full
  # POPULATION
  simplot(
    khm.simset[[1]],
    # khm.simset.full[[1]][[1]],
    ncd.simset[[1]],
    data.type = "population",
    facet.by = "age",
    scale.population = T, scale.to.year = "2015",years = as.character(c(START.YEAR:END.YEAR)))

  
  ########## HIV CASCADE 
  simplot(
    khm.simset[[1]],
    ncd.simset[[1]],
    data.type = "population",
    facet.by = "hiv.status",
    scale.population = T)
  
  #by age and hiv 
  #'@MS: can you generate this plot?
  simplot(
    khm.simset[[1]],
    ncd.simset[[1]],
    data.type = "population",
    facet.by = c("age","hiv.status"),
    scale.population = T)
  
  
  ########## HIV POPULATION BY AGE OVER TIME  
  simplot(
    khm.simset[[1]],
    ncd.simset[[1]],
    data.type = "hiv.prevalence",
    facet.by = "age",
    scale.population = T)
  
  simplot(
    khm.simset[[1]],
    ncd.simset[[1]],
    data.type = "hiv.prevalence",
    facet.by =c("age","sex"),
    scale.population = T)
  
  #'@MS: this one fails
  simplot(
    khm.simset[[1]],
    ncd.simset[[1]],
    data.type = "hiv.incidence",
    facet.by = "age",
    scale.population = T)
  
  ########## NCD PREVALENCE - new option to combine comorbidity 
  simplot(ncd.simset[[1]],
          data.type = "hyp.prev",
          facet.by = c("ncd.status"),
          scale.population = F,
          combine.comorbidity = F,
          show.treated = T
          #view.as.rate = T, per.X.population = 1
          )
  simplot(ncd.simset[[1]],
          data.type = "diab.prev", # "hyp.prev",
          facet.by = c("ncd.status"),
          scale.population = F,
          combine.comorbidity = F,#'@MS: I dont think that the comorbidity is added
          show.treated = T
          # view.as.rate = T, per.X.population = 1 
          #'@MS: when I add the rate the plot fails
  )
  
  simplot(ncd.simset[[1]],
          # ncd.simset[[2]],
          # ncd.simset[[3]],
          # ncd.simset[[4]],
          ncd.simset[[5]],
          ncd.simset[[6]],
          ncd.simset[[7]],
          data.type = "hyp.prev",
          scale.population = F,
          combine.comorbidity = T,
          show.treated = T,
          facet.by = "ncd.status"
  )
  
  simplot(ncd.simset[[1]],
          ncd.simset[[5]],
          data.type = "diab.prev",
          scale.population = F,
          combine.comorbidity = F,
          show.treated = T,
          facet.by = "ncd.status"
  )
  
  simplot(ncd.simset[[1]],
          data.type = "diab.prev",
          facet.by = c("age"),
          scale.population = F,
          combine.comorbidity = T,
          view.as.rate = T, per.X.population = 1
  )
  
  # LOOKING AT NCD TREATMENT STATUS OVER TIME 
  apply(ncd.simset[[1]][[1]]$stats$n.state.sizes,c(4:5),sum)
  apply(ncd.simset[[5]][[1]]$stats$n.state.sizes,c(4:5),sum)
  
  # CVD EVENTS
  simplot(ncd.simset[[1]],
          ncd.simset[[5]],
          data.type = "stroke.inc",scale.population=F, view.as.rate = F)
  
  simplot(ncd.simset[[1]],
          ncd.simset[[5]],
          data.type = "mi.inc",scale.population = F, view.as.rate = F)
  
  simplot(ncd.simset[[1]],
          #ncd.simset[[2]],
          ncd.simset[[3]],
          #ncd.simset[[4]],
          # ncd.simset[[5]],
          data.type = "cvd.mortality",scale.population=F, view.as.rate = F,
          facet.by = "hiv.status")
  
  simplot(khm.simset[[1]],khm.simset[[2]],khm.simset[[3]],khm.simset[[4]],khm.simset[[5]],
          data.type = "hiv.incidence",scale.population = F)
}

# CUMULATIVE RESULTS 
{ 
  
  plot.cumulative.outcome.boxplot(ncd.simset[[1]],ncd.simset[[2]],ncd.simset[[3]],ncd.simset[[4]],ncd.simset[[5]],
                                  data.types=c("n.deaths.cvd","n.stroke.inc"),dimension="sex")
  
  plot.cumulative.outcome.boxplot(ncd.simset[[1]],ncd.simset[[2]],ncd.simset[[3]],ncd.simset[[4]],ncd.simset[[5]],
                                  data.types=c("n.deaths.cvd","n.stroke.inc","n.mi.inc"),dimension="sex")
  
  plot.cumulative.outcome.boxplot(ncd.simset[[1]],ncd.simset[[5]],
                                  data.types=c("n.deaths.cvd"),dimension="age")
  
  
  plot.cumulative.outcome(ncd.simset[[1]],ncd.simset[[5]], data.type = "n.stroke.inc", dimension="age")
  plot.cumulative.outcome(ncd.simset[[1]],ncd.simset[[5]], data.type = "n.deaths.cvd", dimension="age")
  plot.cumulative.outcome(ncd.simset[[1]],ncd.simset[[5]], data.type = "n.mi.inc", dimension="age")
  plot.cumulative.outcome(ncd.simset[[1]],ncd.simset[[5]], data.type = "n.mi.inc", dimension="sex")
  plot.cumulative.outcome(ncd.simset[[1]],ncd.simset[[5]], data.type = "n.mi.inc", dimension="ncd.status")
  plot.cumulative.outcome(ncd.simset[[1]],ncd.simset[[5]], data.type = "n.mi.inc", dimension="hiv.status")
  plot.cumulative.outcome(ncd.simset[[1]],ncd.simset[[3]], data.type = "n.deaths.cvd", dimension="hiv.status")
  
}

