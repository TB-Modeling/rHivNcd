


#######################################################
# LIST OF NCD SCENARIOS
pMonthlyCoverage=0.1/12#assuming 10% annual coverage 

ncdScenarios=list(
  list(id=1,pCoverage=0.0,pNcdTrtInitiation=0.0,pDropOut=0.00),
  list(id=2,pCoverage= pMonthlyCoverage,pNcdTrtInitiation=0.6,pDropOut=0.10/12), # basic NCD package (HIV clinic)
  list(id=3,pCoverage=pMonthlyCoverage,pNcdTrtInitiation=0.6,pDropOut=0.10/12), # basic NCD package + HIV retention/suppression (HIV clinic)
  list(id=4,pCoverage=pMonthlyCoverage,pNcdTrtInitiation=0.8,pDropOut=0.05/12), # intensive NCD + HIV retention/suppression  (HIV clinic)
  
  list(id=5,pCoverage=pMonthlyCoverage,pNcdTrtInitiation=0.6,pDropOut=0.10/12), # basic NCD package (community)
  list(id=6,pCoverage=pMonthlyCoverage,pNcdTrtInitiation=0.6,pDropOut=0.10/12), # basic NCD package + HIV testing/engagement (community)
  list(id=7,pCoverage=pMonthlyCoverage,pNcdTrtInitiation=0.8,pDropOut=0.05/12) # intensive NCD + comprehensive HIV [t/e/r/s] (community)
)

# LIST OF SENSITIVITY ANALYSIS SCENARIOS
# EACH LINE REPRESENTS A NEW ONE-WAY VARIATION IN A PARAMETER VALUE
saScenarios=list(
  list(id=1, param="relative.ncd.risk.by.hiv", newVal=2), #baseline=1
  
  list(id=2, param="annual.growth.ncd.prev",  newVal=1.03),#baseline=1
  
  list(id=3, param="cvd.risk.multiplier",  newVal=0.75),#baseline=1
  list(id=4, param="cvd.risk.multiplier",  newVal=1.25),#baseline=1
  
  list(id=5, param="cvd.risk.multiplier.hiv",  newVal=1),#baseline=1.5
  list(id=6, param="cvd.risk.multiplier.hiv",  newVal=2.02),#baseline=1.5
  
  list(id=7, param="recurrent.cvd.event.risk.multiplier",  newVal=1.5),#baseline=2
  list(id=8, param="recurrent.cvd.event.risk.multiplier",  newVal=2.5),#baseline=2
  
  list(id=9, param="prob.first.cvd.event.mi.male",  newVal=0.45),#baseline=0.6
  list(id=10, param="prob.first.cvd.event.mi.male",  newVal=0.75),#baseline=0.6
  
  list(id=11, param="prob.first.cvd.event.mi.female",  newVal=0.45),#baseline=0.6
  list(id=12, param="prob.first.cvd.event.mi.female",  newVal=0.75),#baseline=0.6
  
  list(id=13, param="cvd.mortality.multiplier",  newVal=0.75),#baseline=1
  list(id=14, param="cvd.mortality.multiplier",  newVal=1.25),#baseline=1
  
  list(id=15, param="recur.stroke.mort.OR.multiplier",  newVal=1.70),#baseline=2.53
  list(id=16, param="recur.stroke.mort.OR.multiplier",  newVal=3.76),#baseline=2.53
  
  list(id=17, param="recur.mi.mortality.multiplier",  newVal=1.4),#baseline=1.86
  list(id=18, param="recur.mi.mortality.multiplier",  newVal=2.3),#baseline=1.86
  
  list(id=19, param="red.cvd.event.hyp.trt",  newVal=0.60),#baseline=0.70
  list(id=20, param="red.cvd.event.hyp.trt",  newVal=0.85),#baseline=0.70
  
  list(id=21, param="red.cvd.death.hyp.trt",  newVal=0.64),#baseline=0.74
  list(id=22, param="red.cvd.death.hyp.trt",  newVal=0.85),#baseline=0.74
  
  list(id=23, param="red.cvd.event.diab.trt",  newVal=0.53),#baseline=0.68
  list(id=24, param="red.cvd.event.diab.trt",  newVal=0.87),#baseline=0.68
  
  list(id=25, param="red.cvd.death.diab.trt",  newVal=0.37),#baseline=0.58
  list(id=26, param="red.cvd.death.diab.trt",  newVal=0.91) #baseline=0.58
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

{
  vReps=c(1:100) #vector of reps 
  vNcdScenarios=c(1,7) #specific NCD scenarios to run
  vSaScenarios=c(1:length(saScenarios)) #vector of sa scenario ids
  #
  nSaScenaios=length(vSaScenarios)
  nNcdScenarios=length(vNcdScenarios)
  nReps=length(vReps)
  print(paste("Required number of nodes is:", nSaScenaios*nNcdScenarios*nReps))
}
#######################################################

if (1==1) {
  args = commandArgs(trailingOnly=TRUE)
  x=as.numeric(args[1])
  
  # filter ncd scenarios to only keep the ones that we need
  ncdScenarios=ncdScenarios[vNcdScenarios]
  
  print(paste("running SA models in parallel with",length(vReps),"reps,",nSaScenaios," saScenarios,",nNcdScenarios,"ncdScenarios"))
  
  # for (x in c(1:156)){
  #   rep=floor((x-1)/(nSaScenaios*nNcdScenarios))+1
  #   ncdId= floor((x-1)/nSaScenaios)%%nNcdScenarios+1
  #   saId= (x-1)%%(nSaScenaios)+1
  #   # print(paste("x=",x,"rep=",rep,"ncd=",ncdId,"sa=",saId))
  #   print(paste0("outputs-sa/pop-stats-saScenario",saScenarios[[saId]]$id,"-ncdScenario",ncdScenarios[[ncdId]]$id,"-rep",rep))
  #    }
  
  
  rep=floor((x-1)/(nSaScenaios*nNcdScenarios))+1
  ncdId= floor((x-1)/nSaScenaios)%%nNcdScenarios+1
  saId= (x-1)%%(nSaScenaios)+1
  
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
                             rep=rep,
                             ncdScenario=ncdScenarios[[ncdId]]$id,
                             saScenario=saScenarios[[saId]]$id)
  
  
  #run the model with new value
  while(pop$params$CYNOW<= END.YEAR)
    run.one.year.int(pop,
                     ncdScenario  =ncdScenarios[[ncdId]]$id,
                     int.start.year = 2023,
                     int.end.year = 2030,
                     pCoverage = ncdScenarios[[ncdId]]$pCoverage,
                     pNcdTrtInitiation = ncdScenarios[[ncdId]]$pNcdTrtInitiation,
                     pDropOut=ncdScenarios[[ncdId]]$pDropOut
    )
  
  #saving population stat and param files seperately
  saveRDS(pop$stats,file = paste0("outputs-sa/popStats-node",x,"-sa",saScenarios[[saId]]$id,"-ncd",ncdScenarios[[ncdId]]$id,"-rep",rep),compress = T)
  saveRDS(pop$params,file = paste0("outputs-sa/popParams-node",x,"-sa",saScenarios[[saId]]$id,"-ncd",ncdScenarios[[ncdId]]$id,"-rep",rep),compress = T)
  
  # saving execution time
  end_time <- Sys.time()
  session_time=hms_span(start_time,end_time)
  txt=paste("rep= ",rep," for SA-scenario=", saScenarios[[saId]]$id, " and ncd-scenario=",ncdScenarios[[ncdId]]$id,">>> session time ",session_time)
  print(txt)
  write.table(x = txt,file = "outputs-sa/out-sessionTime.txt",col.names = F,row.names = F,append = T)
}
