# need a standard set of plots to review the population
# please add notes/documentation to plots.R

#
#  R HIVNCD 2022
#  Driver.R class
#  
#####################################
# list.of.packages <- c("ggplot2", "R6","Rcpp")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

library(R6)
library(data.table)
# library(Rcpp)
# library(ggplot2)
# library(data.table)

#######################################################
print("Sourcing dependencies")
{
  source("globalEnvironment.R")
  source("person.R")
  source("population.R")
  source("rHelperFunctions.R")
  source("rCoreFunctions.R")
  # source("plots.R")
}
#######################################################
# LIST OF NCD SCENARIOS
ncdScenarios=list(
  list(id=1,pCoverage=0.0,pNcdTrtInitiation=0.0,pDropOut=0.00),
  list(id=2,pCoverage=0.1,pNcdTrtInitiation=0.8,pDropOut=0.05),
  list(id=3,pCoverage=0.1,pNcdTrtInitiation=0.9,pDropOut=0.00),
  list(id=4,pCoverage=0.1,pNcdTrtInitiation=0.8,pDropOut=0.05),
  list(id=5,pCoverage=0.1,pNcdTrtInitiation=0.9,pDropOut=0.00)
)
#######################################################
# MULTI REPS
if (1==2){
  vReps=1:10 #reps
  vNcdScenarios=1:5 #scenarios
  print("running models sequentially ....")
  nReps=length(vReps)
  nNcdScenarios=length(vNcdScenarios)
  
  lapply(vReps,function(rep){
    lapply(vNcdScenarios,function(ncdId){
      set.seed(rep)
      print(paste("replication ",rep," scenario", ncdScenarios[[ncdId]]$id, "starting..."))
      
      # create pop at the end of 2014; set up hiv/ncd states; records stats and increament the year to 2015
      pop<-initialize.simulation(id = rep,
                                 n = POP.SIZE,
                                 ncdScenario = ncdScenarios[[ncdId]]$id,
                                 saScenario = 0)
      #run sims
      while(pop$params$CYNOW<= 2030)
        run.one.year.int(pop,
                         scenario =ncdScenarios[[ncdId]]$id,
                         int.start.year = 2023,
                         int.end.year = 2030,
                         pCoverage = ncdScenarios[[ncdId]]$pCoverage,
                         pNcdTrtInitiation = ncdScenarios[[ncdId]]$pNcdTrtInitiation,
                         pDropOut=ncdScenarios[[ncdId]]$pDropOut
        )
      
      #saving population
      res=list(stats=pop$stats,
               params=pop$params)
      saveRDS(res,file = paste0("outputs/popList-ncdScenario",ncdScenarios[[ncdId]]$id,"-rep",rep),compress = T)
       
    })
  })
}

# # #######################################################
# # SINGLE RUN ON ROCKFISH
if (1==1) {
  vReps=1:10 #reps
  vNcdScenarios=1:5 #scenarios
  #############
  print("running models sequentially ....")
  nReps=length(vReps)
  nNcdScenarios=length(vNcdScenarios)
  
  print(paste("running models parallel with ",nReps,"reps and",nNcdScenarios,"ncdScenarios"))
  
  args = commandArgs(trailingOnly=TRUE)
  x=as.numeric(args[1])
  rep=floor((x-1)/(nNcdScenarios))+1
  ncdId= (x-1)%%nNcdScenarios+1
  
  # for (x in c(1:150)){
  #   rep=floor((x-1)/(nNcdScenarios))+1
  #   scenarioId= (x-1)%%nNcdScenarios+1
  #   print(paste("x=",x,"rep=",rep,"scenario=",scenarioId))
  # }
  
  # create pop at the end of 2014; set up hiv/ncd states; records stats and increament the year to 2015
  set.seed(rep)
  start_time <- Sys.time()
  print(paste("replication ",rep," scenario", ncdScenarios[[ncdId]]$id, "starting..."))
  
  # create pop at the end of 2014; set up hiv/ncd states; records stats and increament the year to 2015
  pop<-initialize.simulation(id = rep,
                             n = POP.SIZE,
                             ncdScenario = ncdScenarios[[ncdId]]$id,
                             saScenario = 0)
  #run sims
  while(pop$params$CYNOW<= 2030)
    run.one.year.int(pop,
                     ncdScenario = ncdScenarios[[ncdId]]$id,
                     int.start.year = 2023,
                     int.end.year = 2030,
                     pCoverage = ncdScenarios[[ncdId]]$pCoverage,
                     pNcdTrtInitiation = ncdScenarios[[ncdId]]$pNcdTrtInitiation,
                     pDropOut=ncdScenarios[[ncdId]]$pDropOut
    )
  
  #saving population
  res=list(stats=pop$stats,
           params=pop$params)
  saveRDS(res,file = paste0("outputs/pop-ncdScenario",ncdScenarios[[ncdId]]$id,"-rep",rep),compress = T)
  
  # saving time
  end_time <- Sys.time()
  session_time=hms_span(start_time,end_time)
  txt=paste("rep= ",rep," ncdScenario=", ncdScenarios[[ncdId]]$id," >> session time ",session_time)
  print(txt)
  write.table(x = txt,file = "outputs/out-sessionTime.txt",col.names = F,row.names = F,append = T)
}
