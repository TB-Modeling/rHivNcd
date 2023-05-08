# Functions: 
# 1. read.ncd.simset()
# 2. read.khm.simset()
# 3. read.khm.simset.full()

# 4. generate.annual.results(simset,data.type,n.reps,summary.statistic="median")
# 5. generate.cumulative.results(simset,data.type,n.reps,summary.statistic="median")
# 6. plot.cumulative.outcome(simset.1,simset.2,data.type,dimension)

# 7. plot.cumulative.outcome.boxplot(..., # pass multiple simsets
#                                    data.types, # CAN PASS MULTIPLE DATA TYPES 
#                                    dimension,
#                                    facet.by=NULL,
#                                    n.reps=30)

# 8. generate.events.results.array(simset.list, # e.g., ncd.simset (which is a list of simsets, as read in using read.ncd.simset())
#                                  n.reps,
#                                  ages = DIM.NAMES.AGE,
#                                  sexes = DIM.NAMES.SEX,
#                                  hiv.status = DIM.NAMES.HIV,
#                                  ncd.status = DIM.NAMES.NCD,
#                                  outcomes = c("n.mi.inc","n.stroke.inc", "n.deaths.hiv", "n.deaths.cvd","n.deaths.non.hiv"))


# NCD SIMSET: List of length (7), where each element is a scenario; 
# each scenario is a list of length (reps), where each element is a single sim
read.ncd.simset = function(){
  
  ncd.simset=vector("list",length(SCENARIOS))
  invisible(lapply(SCENARIOS, function(scenario){
    temp.simset.ncd = vector("list",length(REPLICATIONS))
    
    invisible(lapply(REPLICATIONS,function(rep){
      # change this to read in only the stats list
      files=list.files(OUTPUTS.DIR)
      file = files[endsWith(files,paste0("ncd",scenario,"-rep",rep))]
      
      stats<-readRDS(paste0(OUTPUTS.DIR,file))
      
      print(paste0("reading ",OUTPUTS.DIR,"popStats-",scenario,"-rep",rep, " for the ncd model"))
      temp.simset.ncd[[rep]] <<- stats
      
      return(temp.simset.ncd)
    }))

    ncd.simset[[scenario]]<<-temp.simset.ncd # if still using scenario 0; make this scenario + 1
    rm(temp.simset.ncd)
    return(ncd.simset)
  }))
  
  ncd.simset
}

# KHM SIMSET: List of length (7), where each element is a scenario; 
# each scenario is a list of length (reps), where each element is a single sim (that was sampled for the corresponding ncd sim)
read.khm.simset = function(){
  khm.simset=vector("list",length(SCENARIOS))
  invisible(lapply(SCENARIOS, function(scenario){
    temp.simset.khm = vector("list",length(REPLICATIONS))
    
    invisible(lapply(REPLICATIONS,function(rep){
      pop<-readRDS(paste0(OUTPUTS.DIR,"popList-s",scenario,"-rep",rep))
      print(paste0("reading ",OUTPUTS.DIR,"popList-s",scenario,"-rep",rep, " for the hiv model"))
      temp.simset.khm[[rep]] <<- pop$params$khm
      
      return(temp.simset.khm)
    }))
    
    khm.simset[[scenario]]<<-temp.simset.khm # if still using scenario 0; make this scenario + 1
    rm(temp.simset.khm)
    return(khm.simset)
  }))
  
  for(i in 1:length(SCENARIOS)){
    class(khm.simset[[i]]) = "khm_simulation_output"
  }
  
  khm.simset
}

# KHM SIMSET FULL: List of length (7), where each element is a scenario; 
# each scenario is a list of length (reps), where each element is a simset; 
# each simset is a list of length (n.sims), where each element is a single sim 
read.khm.simset.full = function(){
  khm.simset.full=vector("list",length(SCENARIOS))
  invisible(lapply(SCENARIOS, function(scenario){
    
    hiv.scenario=HIV.SCENARIOS[scenario] # extract the HIV scenario name 
    temp.simset.khm.full = list()
    
    load(paste0("data/hiv_simset_",hiv.scenario,".RData"))
    print(paste0("reading data/hiv_simset_",hiv.scenario,".RData"))
    temp.simset.khm.full = khm.full
    khm.simset.full[[scenario]]<<-temp.simset.khm.full 
    return(khm.simset.full)    

  }))
  
  for(i in 1:length(SCENARIOS)){
    class(khm.simset.full[[i]]) = "khm_simulation_output"
  }
  
  khm.simset.full
}



generate.annual.results = function(simset,
                                   data.type,
                                   n.reps,
                                   years=as.character(c(2014:2031)),
                                   summary.statistic="median"){
  
  simset = simset[1:n.reps]
  
  dim.names = list(age=DIM.NAMES.AGE,
                   sex=DIM.NAMES.SEX,
                   hiv.status=DIM.NAMES.HIV,
                   ncd.status=DIM.NAMES.NCD,
                   years=years,
                   rep=c(1:n.reps))
  
  data = sapply(simset, function(rep){
    rep[[data.type]]
  })
  
  dim(data) = sapply(dim.names,length)
  dimnames(data) = dim.names
  
  summary.result = apply(data,c("age","sex","hiv.status","ncd.status","years"), summary.statistic)
  
  summary.result
}

generate.cumulative.results = function(simset,
                                       data.type,
                                       years=as.character(c(2023:2030)),
                                       n.reps,
                                       summary.statistic="median"){
  
  simset = simset[1:n.reps]
  
  dim.names = list(age=DIM.NAMES.AGE,
                   sex=DIM.NAMES.SEX,
                   hiv.status=DIM.NAMES.HIV,
                   ncd.status=DIM.NAMES.NCD,
                   rep=c(1:n.reps))
  
  cumulative.data = sapply(simset, function(rep){
    x = apply(rep[[data.type]][,,,,years], c(1:4), sum) # sum over years 
  })
  
  dim(cumulative.data) = sapply(dim.names,length)
  dimnames(cumulative.data) = dim.names
  
  if(summary.statistic=="median and CI"){
    summary.result = apply(cumulative.data,c("age","sex","hiv.status","ncd.status"), quantile,c(.025,.5,.975))
  } else 
    summary.result = apply(cumulative.data,c("age","sex","hiv.status","ncd.status"), summary.statistic)
  
  summary.result
  
}

plot.cumulative.outcome = function(simset.1,
                                   simset.2,
                                   years=as.character(c(2023:2030)),
                                   data.type,
                                   dimension){
  
  cumulative.outcome.1 = generate.cumulative.results(simset = simset.1, data.type = data.type, n.reps=30)
  cumulative.outcome.2 = generate.cumulative.results(simset = simset.2, data.type = data.type, n.reps=30)
  
  cumulative.outcome.by.dimension.1 = apply(cumulative.outcome.1,dimension,sum)
  cumulative.outcome.by.dimension.2 = apply(cumulative.outcome.2,dimension,sum)
  
  full.dimnames = list(age=DIM.NAMES.AGE,
                       sex=DIM.NAMES.SEX,
                       hiv.status=DIM.NAMES.HIV,
                       ncd.status=DIM.NAMES.NCD,
                       years=years)
  
  dim.names = list(dim=full.dimnames[[dimension]],
                   scenario=c("scen_a","scen_b"))
  
  cumulative.outcome.by.dimension.comparison = array(c(cumulative.outcome.by.dimension.1,cumulative.outcome.by.dimension.2),
                                                     dim = sapply(dim.names,length),
                                                     dimnames= dim.names)
  
  df = reshape2::melt(cumulative.outcome.by.dimension.comparison)
  
  plot = ggplot(data = df,aes(x=dim,y=value, fill=scenario)) + geom_bar(stat="identity",position = "dodge")
  
  print(plot)
  
}


plot.cumulative.outcome.boxplot = function(...,
                                           data.types,
                                           years=as.character(c(2023:2030)),
                                           dimension,
                                           facet.by=NULL,
                                           n.reps=100){
  simsets = list(...)
  
  dim.names = list(age=DIM.NAMES.AGE,
                   sex=DIM.NAMES.SEX,
                   hiv.status=DIM.NAMES.HIV,
                   ncd.status=DIM.NAMES.NCD,
                   rep=c(1:n.reps))
  
  df.sim = NULL
  
  for(i in 1:length(simsets)){
    simset = simsets[[i]]
    
    # scenario.number = as.numeric(simset[[1]][["ncd.id"]])

    if("n.cvd.events" %in% data.types){
      
      data.1 = sapply(simset, function(rep){x = apply(rep[["n.mi.inc"]][,,,,years], c(1:4), sum)})
      data.2 = sapply(simset, function(rep){x = apply(rep[["n.stroke.inc"]][,,,,years], c(1:4), sum)})
      data = data.1+data.2
      dim(data) = sapply(dim.names,length)
      dimnames(data)=dim.names
      
      if(dimension=="total"){
        data.by.dim = apply(data,c("rep"),sum)
        dim.names.new = list(dim="total",
                             rep=dim.names[["rep"]])
        
      } else{
        data.by.dim = apply(data,c(dimension,"rep"),sum)
        dim.names.new = list(dim=dim.names[[dimension]],
                             rep=dim.names[["rep"]])
      }
      
      dim(data.by.dim)=sapply(dim.names.new,length)
      dimnames(data.by.dim) = dim.names.new
      
      one.df = reshape2::melt(data.by.dim) 
      one.df$scenario.id = i
      one.df$data.type = "n.cvd.events"
      df.sim = rbind(df.sim, one.df)  
      
    } else {
      for(d in data.types){
        
        data = sapply(simset, function(rep){x = apply(rep[[d]][,,,,years], c(1:4), sum)})
        dim(data) = sapply(dim.names,length)
        dimnames(data) = dim.names
        
        if(dimension=="total"){
          data.by.dim = apply(data,c("rep"),sum)
          dim.names.new = list(dim="total",
                               rep=dim.names[["rep"]])
          
        } else{
          data.by.dim = apply(data,c(dimension,"rep"),sum)
          dim.names.new = list(dim=dim.names[[dimension]],
                               rep=dim.names[["rep"]])
        }
        
        dim(data.by.dim)=sapply(dim.names.new,length)
        dimnames(data.by.dim) = dim.names.new
        
        one.df = reshape2::melt(data.by.dim) 
        one.df$scenario.id = i
        one.df$data.type = d
        df.sim = rbind(df.sim, one.df)   
        
      }
      
    }
      
        
    
    
  }
  
  df.sim$scenario.id = as.character(df.sim$scenario.id)
  
  
  facet_string = '~data.type'
  if(length(facet.by)>0){
    facet_string = paste0("~",paste0(facet.by,collapse = '+'))
  }
  facet_formula = as.formula(facet_string)
  
  plot = ggplot(df.sim, aes(x=dim, y=value, fill=scenario.id)) + 
    geom_boxplot()+ 
    facet_wrap(facet_formula, scales = "free_y") + 
    ylim(0,NA)

  print(plot)
  
}


generate.cumulative.events.results.array = function(simset.list,
                                         n.reps,
                                         years=as.character(c(2023:2040)),
                                         ages = DIM.NAMES.AGE,
                                         sexes = DIM.NAMES.SEX,
                                         hiv.status = DIM.NAMES.HIV,
                                         ncd.status = DIM.NAMES.NCD,
                                         outcomes = c("n.hiv.eng","n.hyp.trt","n.diab.trt","n.diab.hyp.trt",
                                                      "n.hiv.inc","n.mi.inc","n.stroke.inc",
                                                      "n.deaths.hiv", "n.deaths.cvd","n.deaths.non.hiv")
){
  reps = c(1:n.reps)
  simset.list = simset.list
  interventions = c(1:length(simset.list))
  
  full.dim.names = list(age = ages,
                        sex = sexes,
                        hiv.status = hiv.status,
                        ncd.status = ncd.status,
                        outcome = outcomes,
                        rep = reps,
                        intervention = interventions)
  
  rv = sapply(simset.list, function(simset){
    sapply(reps, function(rep){
      sapply(outcomes, function(x){
        
        apply(simset[[rep]][[x]][,,,,years], c(1:4), sum)
        
      })
    })
  })
  
  dim(rv) = sapply(full.dim.names, length)
  dimnames(rv) = full.dim.names
  
  rv
}

generate.annual.events.results.array = function(simset.list,
                                                n.reps,
                                                ages = DIM.NAMES.AGE,
                                                sexes = DIM.NAMES.SEX,
                                                hiv.status = DIM.NAMES.HIV,
                                                ncd.status = DIM.NAMES.NCD,
                                                years = DIM.NAMES.YEAR,
                                                outcomes = c("n.state.sizes",
                                                             "n.hiv.eng","n.hyp.trt","n.diab.trt","n.diab.hyp.trt",
                                                             "n.hiv.inc","n.mi.inc","n.stroke.inc",
                                                             "n.deaths.hiv", "n.deaths.cvd","n.deaths.non.hiv")
){
  reps = c(1:n.reps)
  simset.list = simset.list
  interventions = c(1:length(simset.list))
  
  full.dim.names = list(age = ages,
                        sex = sexes,
                        hiv.status = hiv.status,
                        ncd.status = ncd.status,
                        year = years,
                        outcome = outcomes,
                        rep = reps,
                        intervention = interventions)
  
  rv = sapply(simset.list, function(simset){
    sapply(reps, function(rep){
      sapply(outcomes, function(x){
        
        simset[[rep]][[x]][ages,sexes,hiv.status,ncd.status,as.character(years)]
        
      })
    })
  })
  
  dim(rv) = sapply(full.dim.names, length)
  dimnames(rv) = full.dim.names
  
  rv
}


