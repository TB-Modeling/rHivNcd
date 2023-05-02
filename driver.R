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
#function to return elapse run time for the simulation
hms_span <- function(start, end) {
  dsec <- as.numeric(difftime(end, start, unit = "secs"))
  hours <- floor(dsec / 3600)
  minutes <- floor((dsec - 3600 * hours) / 60)
  seconds <- dsec - 3600*hours - 60*minutes
  paste0(
    sapply(c(hours, minutes, seconds), function(x) {
      formatC(x, width = 2, format = "d", flag = "0")
    }), collapse = ":")
}
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

scenarios=list(
  list(id=1,pCoverage=0.0,pNcdTrtInitiation=0.0,pDropOut=0.00),
  list(id=2,pCoverage=0.1,pNcdTrtInitiation=0.8,pDropOut=0.05),
  list(id=3,pCoverage=0.1,pNcdTrtInitiation=0.9,pDropOut=0.00),
  list(id=4,pCoverage=0.1,pNcdTrtInitiation=0.8,pDropOut=0.05),
  list(id=5,pCoverage=0.1,pNcdTrtInitiation=0.9,pDropOut=0.00)
)
#######################################################
# MULTI REPS
if (1==2){
  print("running models sequentially ....")
  lapply(c(1:1),function(rep){
    lapply(c(1:5),function(id){
      start_time <- Sys.time()
      set.seed(rep)
      scenario=scenarios[[id]]$id
      print(paste("replication ",rep," scenario", scenario, "starting..."))
      
      # create pop at the end of 2014; set up hiv/ncd states; records stats and increament the year to 2015
      pop<-initialize.simulation(id = rep,
                                 n = POP.SIZE,
                                 scenario=scenario)
      #run sims
      while(pop$params$CYNOW<= 2030)
        run.one.year.int(pop,
                         scenario =scenarios[[id]]$id,
                         int.start.year = 2023,
                         int.end.year = 2030,
                         pCoverage = scenarios[[id]]$pCoverage,
                         pNcdTrtInitiation = scenarios[[id]]$pNcdTrtInitiation,
                         pDropOut=scenarios[[id]]$pDropOut
        )
      
      #saving population
      res=list(stats=pop$stats,
               params=pop$params)
      saveRDS(res,file = paste0("outputs/popList-s",scenario,"-rep",rep),compress = T)
      # saving time
      end_time <- Sys.time()
      session_time=end_time - start_time
      txt=paste("Model ",rep," >> session time ",session_time)
      write.table(x = txt,file = "outputs/out-sessionTime.txt",col.names = F,row.names = F,append = T)
    })
  })
}

# # #######################################################
# # SINGLE RUN ON ROCKFISH
if (1==1) {
  print("running models parallel ....")
  
  args = commandArgs(trailingOnly=TRUE)
  R=10 #reps
  S=5 #scenarios
  
  x=as.numeric(args[1])
  rep=floor((x-1)/(S))+1
  scenarioId= (x-1)%%5+1
  
  # for (x in c(1:50)){
  #   rep=floor((x-1)/(S))+1
  #   scenarioId= (x-1)%%5+1
  #   print(paste("x=",x,"rep=",rep,"scenario=",scenarioId))
  # }
  
  # create pop at the end of 2014; set up hiv/ncd states; records stats and increament the year to 2015
  set.seed(rep)
  scenario=scenarios[[scenarioId]]$id
  print(paste("replication ",rep," scenario", scenario, "starting..."))
  start_time <- Sys.time()
  pop<-initialize.simulation(id = rep,
                             n = POP.SIZE,
                             scenario=scenario)
  #run sims
  while(pop$params$CYNOW<= 2030)
    run.one.year.int(pop,
                     scenario =scenario,
                     int.start.year = 2023,
                     int.end.year = 2030,
                     pCoverage = scenarios[[scenarioId]]$pCoverage,
                     pNcdTrtInitiation = scenarios[[scenarioId]]$pNcdTrtInitiation,
                     pDropOut=scenarios[[scenarioId]]$pDropOut
    )
  
  #saving population
  res=list(stats=pop$stats,
           params=pop$params)
  saveRDS(res,file = paste0("outputs/popList-s",scenario,"-rep",rep),compress = T)
  # saving time
  end_time <- Sys.time()
  session_time=end_time - start_time
  txt=paste("Model ",rep," >> session time ",session_time)
  write.table(x = txt,file = "outputs/out-sessionTime.txt",col.names = F,row.names = F,append = T)
}
