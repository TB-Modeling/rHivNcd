SCENARIOS = c(1:5)
REPLICATIONS = c(1:30)

par(mfrow=c(1,1))


# Functions: 
# read.ncd.simset()
# read.khm.simset()
# read.khm.simset.full()
# generate.annual.results(simset,data.type,reps,summary.statistic="mean")
# generate.cumulative.results(simset,data.type,reps,summary.statistic="mean")
# plot.cumulative.outcome(simset.1,simset.2,data.type,dimension)



# NCD SIMSET: List of length (5), where each element is a scenario; 
# each scenario is a list of length (reps), where each element is a single sim
read.ncd.simset = function(){
  ncd.simset=vector("list",length(SCENARIOS))
  invisible(lapply(SCENARIOS, function(scenario){
    temp.simset.ncd = vector("list",length(REPLICATIONS))
    
    invisible(lapply(REPLICATIONS,function(rep){
      pop<-readRDS(paste0("outputs/popList-s",scenario,"-rep",rep))
      print(paste0("reading outputs/popList-s",scenario,"-rep",rep, " for the ncd model"))
      temp.simset.ncd[[rep]] <<- pop
      
      return(temp.simset.ncd)
    }))

    ncd.simset[[scenario]]<<-temp.simset.ncd # if still using scenario 0; make this scenario + 1
    return(ncd.simset)
  }))
  
  ncd.simset
}

# KHM SIMSET: List of length (5), where each element is a scenario; 
# each scenario is a list of length (reps), where each element is a single sim (that was sampled for the corresponding ncd sim)
read.khm.simset = function(){
  khm.simset=vector("list",length(SCENARIOS))
  invisible(lapply(SCENARIOS, function(scenario){
    temp.simset.khm = vector("list",length(REPLICATIONS))
    
    invisible(lapply(REPLICATIONS,function(rep){
      pop<-readRDS(paste0("outputs/popList-s",scenario,"-rep",rep))
      # print(paste0("reading outputs/popList-s",scenario,"-rep",rep, " for the hiv model"))
      temp.simset.khm[[rep]] <<- pop$params$khm
      
      return(temp.simset.khm)
    }))
    
    khm.simset[[scenario]]<<-temp.simset.khm # if still using scenario 0; make this scenario + 1
    return(khm.simset)
  }))
  
  for(i in 1:length(SCENARIOS)){
    class(khm.simset[[i]]) = "khm_simulation_output"
  }
  
  khm.simset
}

# KHM SIMSET FULL: List of length (5), where each element is a scenario; 
# each scenario is a list of length (reps), where each element is a simset; 
# each simset is a list of length (n.sims), where each element is a single sim 
read.khm.simset.full = function(){
  khm.simset.full=vector("list",length(SCENARIOS))
  invisible(lapply(SCENARIOS, function(scenario){
    temp.simset.khm.full = vector("list",length(REPLICATIONS))
    
    invisible(lapply(REPLICATIONS,function(rep){
      pop<-readRDS(paste0("outputs/popList-s",scenario,"-rep",rep))
      # print(paste0("reading outputs/popList-s",scenario,"-rep",rep, " for the hiv model"))
      temp.simset.khm.full[[rep]] <<- pop$params$khm.full
      
      return(temp.simset.khm.full)
    }))
    
    khm.simset.full[[scenario]]<<-temp.simset.khm.full # if still using scenario 0; make this scenario + 1
    return(khm.simset.full)
  }))
  
  for(i in 1:length(SCENARIOS)){
    class(khm.simset.full[[i]]) = "khm_simulation_output"
  }
  
  khm.simset.full
}



generate.annual.results = function(simset,
                                   data.type,
                                   reps,
                                   summary.statistic="mean"){
  
  simset = simset[1:reps]
  
  dim.names = list(age=DIM.NAMES.AGE,
                   sex=DIM.NAMES.SEX,
                   hiv.status=DIM.NAMES.HIV,
                   ncd.status=DIM.NAMES.NCD,
                   years=as.character(c(2014:2031)),
                   rep=c(1:reps))
  
  data = sapply(simset, function(rep){
    rep$stats[[data.type]]
  })
  
  dim(data) = sapply(dim.names,length)
  dimnames(data) = dim.names
  
  summary.result = apply(data,c("age","sex","hiv.status","ncd.status","years"), summary.statistic)
  
  summary.result
}




generate.cumulative.results = function(simset,
                                       data.type,
                                       reps,
                                       summary.statistic="mean"){
  
  simset = simset[1:reps]
  
  dim.names = list(age=DIM.NAMES.AGE,
                   sex=DIM.NAMES.SEX,
                   hiv.status=DIM.NAMES.HIV,
                   ncd.status=DIM.NAMES.NCD,
                   rep=c(1:reps))
  
  cumulative.data = sapply(simset, function(rep){
    x = apply(rep$stats[[data.type]], c(1:4), sum) # sum over years 
  })
  
  dim(cumulative.data) = sapply(dim.names,length)
  dimnames(cumulative.data) = dim.names
  
  summary.result = apply(cumulative.data,c("age","sex","hiv.status","ncd.status"), summary.statistic)
  
  summary.result
  
}

plot.cumulative.outcome = function(simset.1,
                                   simset.2,
                                   data.type,
                                   dimension){
  
  cumulative.outcome.1 = generate.cumulative.results(simset = simset.1, data.type = data.type, reps=30)
  cumulative.outcome.2 = generate.cumulative.results(simset = simset.2, data.type = data.type, reps=30)
  
  cumulative.outcome.by.dimension.1 = apply(cumulative.outcome.1,dimension,sum)
  cumulative.outcome.by.dimension.2 = apply(cumulative.outcome.2,dimension,sum)
  
  full.dimnames = list(age=DIM.NAMES.AGE,
                       sex=DIM.NAMES.SEX,
                       hiv.status=DIM.NAMES.HIV,
                       ncd.status=DIM.NAMES.NCD,
                       years=as.character(c(2014:2031)))
  
  dim.names = list(dim=full.dimnames[[dimension]],
                   scenario=c("scen_a","scen_b"))
  
  cumulative.outcome.by.dimension.comparison = array(c(cumulative.outcome.by.dimension.1,cumulative.outcome.by.dimension.2),
                                                     dim = sapply(dim.names,length),
                                                     dimnames= dim.names)
  
  df = reshape2::melt(cumulative.outcome.by.dimension.comparison)
  
  plot = ggplot(data = df,aes(x=dim,y=value, fill=scenario)) + geom_bar(stat="identity",position = "dodge")
  
  print(plot)
  
}
