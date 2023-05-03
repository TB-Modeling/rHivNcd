# 
source("plots.R")
# source("globalEnvironment.R")
# source("rHelperFunctions.R")
# # moved all functions to this file so I can just source it 
source("postProcessingFunctions.R")

 # setwd("outputs/outputs-0503//")
#' SCENARIOS = c(1:5)
#' REPLICATIONS = c(1:30)
#' 
#' ncd.simset[[1]][[1]]$stats$pop.size
#' # # Reading populations back into simset objects
#' 
#'   ncd.simset=vector("list",length(SCENARIOS))
#'   invisible(lapply(SCENARIOS, function(scenario){
#'     temp.simset.ncd = vector("list",length(REPLICATIONS))
#'     
#'     invisible(lapply(REPLICATIONS,function(rep){
#'       # pop<-readRDS(paste0("outputs/popList-s",scenario,"-rep",rep))
#'       pop<-readRDS(paste0("popList-s",scenario,"-rep",rep))
#'       
#'       print(paste0("reading outputs/popList-s",scenario,"-rep",rep, " for the ncd model"))
#'       temp.simset.ncd[[rep]] <<- pop
#'       
#'       return(temp.simset.ncd)
#'     }))
#'     # print(paste(length(temp.simset.ncd),"reps read"))
#'     ncd.simset[[scenario]]<<-temp.simset.ncd # if still using scenario 0; make this scenario + 1
#'     return(ncd.simset)
#'   }))
#'   print(paste(length(ncd.simset)," ncd outputs read with",length(ncd.simset[[1]]),"reps"))
#'   
#' 
#'   #'@MS: since all 5 scenarios are using the same seeds, they're using the same khm pops, 
#'   #'you can read khm pops for one scenario and copy it for other scenarios
#'   khm.simset=vector("list",length(SCENARIOS))
#'   invisible(lapply(SCENARIOS, function(scenario){
#'     temp.simset.khm = vector("list",length(REPLICATIONS))
#'     invisible(lapply(REPLICATIONS,function(rep){
#'       pop<-readRDS(paste0("popList-s",scenario,"-rep",rep))
#'       print(paste0("reading outputs/popList-s",scenario,"-rep",rep, " for the hiv model"))
#'       temp.simset.khm[[rep]] <<- pop$params$khm
#'       print(pop$params$khm.id)
#'       return(temp.simset.khm)
#'     }))
#' 
#'     khm.simset[[scenario]]<<-temp.simset.khm # if still using scenario 0; make this scenario + 1
#'     return(khm.simset)
#'   }))
#'   print(paste(length(khm.simset)," khm outputs read with",length(khm.simset[[1]]),"reps"))
#'   
#'   # invisible(lapply(SCENARIOS, function(scenario){
#'   #   lapply(REPLICATIONS,function(rep){
#'   #     print(paste(scenario,"--",rep,"--",khm.simset[[scenario]][[rep]]$id))
#'   #   })}))
#'     

SCENARIOS = c(5)
REPLICATIONS = c(1)

ncd.simset=read.ncd.simset()
khm.simset=read.khm.simset()
khm.simset.full = read.khm.simset.full()

# PLOTTING
{ 
  par(mfrow=c(1,1))
  
  # POPULATION
  simplot(
    khm.simset[[1]],
    # khm.simset.full[[1]][[1]],
    ncd.simset[[1]],
    data.type = "population",
    facet.by = "age",
    scale.population = T, scale.to.year = "2015",years = as.character(c(2015:2030)))

  
  # HIV CASCADE 
  simplot(
    khm.simset[[1]],
    ncd.simset[[1]],
    data.type = "population",
    facet.by = "hiv.status",
    scale.population = T)
  
  # HIV POPULATION BY AGE OVER TIME 
  simplot(
    khm.simset[[1]],
    ncd.simset[[1]],
    data.type = "hiv.prevalence",
    facet.by = "age",
    scale.population = T)
  
  simplot(
    khm.simset[[1]],
    ncd.simset[[1]],
    data.type = "hiv.incidence",
    facet.by = "age",
    scale.population = T)
  

  # NCD PREVALENCE - new option to combine comorbidity 
  simplot(ncd.simset[[1]],
          data.type = "hyp.prev",
          # facet.by = c("ncd.status"),
          scale.population = F,
          combine.comorbidity = F,
          show.treated = T
          #view.as.rate = T, per.X.population = 1
          )
  
  simplot(ncd.simset[[1]],
          ncd.simset[[2]],
          ncd.simset[[3]],
          ncd.simset[[4]],
          ncd.simset[[5]],
          data.type = "hyp.prev",
          scale.population = F,
          combine.comorbidity = F,
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

  plot.cumulative.outcome(ncd.simset[[1]],ncd.simset[[5]], data.type = "n.stroke.inc", dimension="age")
  plot.cumulative.outcome(ncd.simset[[1]],ncd.simset[[5]], data.type = "n.deaths.cvd", dimension="age")
  plot.cumulative.outcome(ncd.simset[[1]],ncd.simset[[5]], data.type = "n.mi.inc", dimension="age")
  plot.cumulative.outcome(ncd.simset[[1]],ncd.simset[[5]], data.type = "n.mi.inc", dimension="sex")
  plot.cumulative.outcome(ncd.simset[[1]],ncd.simset[[5]], data.type = "n.mi.inc", dimension="ncd.status")
  plot.cumulative.outcome(ncd.simset[[1]],ncd.simset[[5]], data.type = "n.mi.inc", dimension="hiv.status")
  plot.cumulative.outcome(ncd.simset[[1]],ncd.simset[[3]], data.type = "n.mi.inc", dimension="hiv.status")
  
  # checking that these both work the way I think they do - they are equivalent when I use mean instead of median 
  cumulative.stroke = generate.cumulative.results(simset = ncd.simset[[1]], data.type = "n.stroke.inc", reps=30)
  annual.stroke =  generate.annual.results(simset = ncd.simset[[1]], data.type = "n.stroke.inc", reps=30)
  
  a = cumulative.stroke[,1,1,3]
  b = apply(annual.stroke[,1,1,3,],c(1),sum)
  
  table(a==b)
  cbind(a,b)
  
  qplot(x = c(DIM.NAMES.AGE),y=a)
  qplot(x = c(DIM.NAMES.AGE),y=b)
  
  sum(cumulative.stroke)
  sum(annual.stroke)

  # first rep only
  apply(ncd.simset[[1]][[1]]$stats$n.stroke.inc,5,sum)
  apply(ncd.simset[[2]][[1]]$stats$n.stroke.inc,5,sum)
  apply(ncd.simset[[3]][[1]]$stats$n.stroke.inc,5,sum)
  apply(ncd.simset[[4]][[1]]$stats$n.stroke.inc,5,sum)
  apply(ncd.simset[[5]][[1]]$stats$n.stroke.inc,5,sum)
  
  apply(ncd.simset[[1]][[1]]$stats$n.mi.inc,5,sum)
  apply(ncd.simset[[2]][[1]]$stats$n.mi.inc,5,sum)
  apply(ncd.simset[[3]][[1]]$stats$n.mi.inc,5,sum)
  apply(ncd.simset[[4]][[1]]$stats$n.mi.inc,5,sum)
  apply(ncd.simset[[5]][[1]]$stats$n.mi.inc,5,sum)
}

