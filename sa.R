


#######################################################
# LIST OF NCD SCENARIOS
ncdScenarios=list(
  list(id=1,pCoverage=0.0,pNcdTrtInitiation=0.0,pDropOut=0.00),
  list(id=2,pCoverage=0.1,pNcdTrtInitiation=0.8,pDropOut=0.05),
  list(id=3,pCoverage=0.1,pNcdTrtInitiation=0.9,pDropOut=0.00),
  list(id=4,pCoverage=0.1,pNcdTrtInitiation=0.8,pDropOut=0.05),
  list(id=5,pCoverage=0.1,pNcdTrtInitiation=0.9,pDropOut=0.00)
)
# LIST OF SENSITIVITY ANALYSIS SCENARIOS
# EACH LINE REPRESENTS A NEW ONE-WAY VARIATION IN A PARAMETER VALUE
saScenarios=list(
  list(id=1, param="relative.ncd.risk.by.hiv", newVal=2), #baseline=1
  list(id=2, param="annual.growth.ncd.prev",  newVal=1.05),#baseline=1
  list(id=3, param="cvd.risk.multiplier.hiv",  newVal=2)#baseline=1.5
  # ....
)
#######################################################

library(R6)
library(data.table)
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

if (1==1) {
  args = commandArgs(trailingOnly=TRUE)
  x=as.numeric(args[1])
  
  vReps=c(1:2) #vector of reps 
  vSaScenarios=c(1:length(saScenarios)) #vector of sa scenario ids
  #'@MS: should we run each sa scenario for both ncd scenarios 1 (no int) and 3 & 5 (intervention)?
  vNcdScenarios=c(1,3,5) #specific NCD scenarios to run
  # filter ncd scenarios to only keep the ones that we need
  ncdScenarios=ncdScenarios[vNcdScenarios]
  
  nSaScenaios=length(vSaScenarios)
  nNcdScenarios=length(vNcdScenarios)
  rep=floor((x-1)/(nSaScenaios+nNcdScenarios))+1
  ncdId= floor((x-1)/nSaScenaios)%%nNcdScenarios+1
  saId= (x-1)%%(nSaScenaios)+1
  
  print(paste("running SA models in parallel with",length(vReps),"reps,",nSaScenaios," saScenarios,",nNcdScenarios,"ncdScenarios"))
  #  for (x in c(1:60)){
  #   rep=floor((x-1)/(nSaScenaios+nNcdScenarios))+1
  #   ncdId= floor((x-1)/nSaScenaios)%%nNcdScenarios+1
  #   saId= (x-1)%%(nSaScenaios)+1
  #   print(paste("x=",x,"rep=",rep,"ncd=",ncdId,"sa=",saId))
  # }
  
 # run a single SA scenario
  set.seed(rep)
  print(paste("node=",x,"running rep= ",rep," for SA-scenario=", saScenarios[[saId]]$id, " and ncd-scenario=",ncdScenarios[[ncdId]]$id,"starting..."))
  start_time <- Sys.time()
  
  #change param value
  print(paste("changing param: ",saScenarios[[saId]]$param," to new value of",saScenarios[[saId]]$newVal))
  SA.PARAMS[saScenarios[[saId]]$param]=saScenarios[[saId]]$newVal
  
  # start the population
  pop<-initialize.simulation(id = rep,
                            n = POP.SIZE,
                            ncdScenario=ncdScenarios[[ncdId]]$id,
                            saScenario=saScenarios[[saId]]$id)
 
  
  #run the model with new value
  while(pop$params$CYNOW<= 2030)
      run.one.year.int(pop,
                       ncdScenario  =ncdScenarios[[ncdId]]$id,
                     int.start.year = 2023,
                     int.end.year = 2030,
                     pCoverage = ncdScenarios[[ncdId]]$pCoverage,
                     pNcdTrtInitiation = ncdScenarios[[ncdId]]$pNcdTrtInitiation,
                     pDropOut=ncdScenarios[[ncdId]]$pDropOut
    )
  
  #saving population
  res=list(stats=pop$stats,
           params=pop$params)
  saveRDS(res,file = paste0("outputs/pop-saScenario",saScenarios[[saId]]$id,"-ncdScenario",ncdScenarios[[ncdId]]$id,"-rep",rep),compress = T)
  # saving time
  end_time <- Sys.time()
  session_time=hms_span(start_time,end_time)
  txt=paste("rep= ",rep," for SA-scenario=", saScenarios[[saId]]$id, " and ncd-scenario=",ncdScenarios[[ncdId]]$id,">>> session time ",session_time)
  print(txt)
  write.table(x = txt,file = "outputs/out-sessionTime.txt",col.names = F,row.names = F,append = T)
}
