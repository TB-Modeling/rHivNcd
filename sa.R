# #NCD EVENT RISKS
# #relative risk of ncd incidence by hiv status (>1 relative to hiv.neg) (can be a single value or an array)
# MP$relative.ncd.risk.by.hiv=1 
# #annual growth in age/sex-specific prev of ncds relative to baseline (>1)
# MP$annual.growth.ncd.prev=1
# 
# 
# #CVD EVENT RISKS
# #modeling increased cvd risk by HIV state
# MP$cvd.risk.multiplier.hiv=1.5
# #applying a single multiplier for sensitivity analysis
# MP$cvd.risk.multiplier=1
# # risk of recurrent event here to 2x original risk; able to change in sensitivity analysis
# MP$recurrent.cvd.event.risk.multiplier=2
# #probability that the first CVD event is mi (vs stroke)
# MP$prob.first.cvd.event.mi.male = 0.6 
# MP$prob.first.cvd.event.mi.female = 0.6 
# 
# #CVD DEATHS
# #applying a single multiplier for sensitivity analysis
# MP$cvd.mortality.multiplier=1
# #recurrent events
# recur.stroke.mort.OR.multiplier=2.53 # this is an ODDS RATIO (relative to current probability), so have to convert to odds and then back to probability (in returnCvdMortality function)
# recur.mi.mortality.multiplier=1.856 
# 
# #INTERVENTION IMPACT
# MP$red.cvd.event.hyp.trt= (1-0.3)
# MP$red.cvd.death.hyp.trt= (1-0.26)
# MP$red.cvd.event.diab.trt= (1-0.32)
# MP$red.cvd.death.diab.trt= (1-0.42)
#######################################################
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
  ##
  pop<-initialize.simulation(id = rep,
                           n = POP.SIZE,
                           scenario=ncdScenarios[[ncdId]]$id)
  #change param value
  print(paste("changing param: ",saScenarios[[saId]]$param," to new value"))
  pop$params[saScenarios[[saId]]$param]=saScenarios[[saId]]$newVal
  
  #run the model with new value
  # while(pop$params$CYNOW<= 2030)
  #   run.one.year.int(pop,
  #                    scenario =ncdScenarios[[ncdId]]$id,
  #                    int.start.year = 2023,
  #                    int.end.year = 2030,
  #                    pCoverage = ncdScenarios[[ncdId]]$pCoverage,
  #                    pNcdTrtInitiation = ncdScenarios[[ncdId]]$pNcdTrtInitiation,
  #                    pDropOut=ncdScenarios[[ncdId]]$pDropOut
  #   )
  
  #saving population
  res=list(stats=pop$stats,
           params=pop$params)
  saveRDS(res,file = paste0("outputs/pop-saScenario",saScenarios[[saId]]$saId,"-ncdScenario",ncdScenarios[[ncdId]]$id,"-rep",rep),compress = T)
  # saving time
  end_time <- Sys.time()
  session_time=hms_span(start_time,end_time)
  txt=paste("rep= ",rep," for SA-scenario=", saScenarios[[saId]]$id, " and ncd-scenario=",ncdScenarios[[ncdId]]$id,">>> session time ",session_time)
  write.table(x = txt,file = "outputs/out-sessionTime.txt",col.names = F,row.names = F,append = T)
}
