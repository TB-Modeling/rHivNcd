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
  list(saId=1, param="relative.ncd.risk.by.hiv", newVal=2),
  list(saId=2, param="annual.growth.ncd.prev",  newVal=2),
  list(saId=3, param="cvd.risk.multiplier.hiv",  newVal=2)
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
  print("running SA models in parallel ....")
  R=10 #reps
  S=length(saScenarios)
  
  ncdScenarioId=5 #specific NCD scenario to run
  
  args = commandArgs(trailingOnly=TRUE)
  x=as.numeric(args[1])
  rep=floor((x-1)/(S))+1
  saId= (x-1)%%S+1
  
  # for (x in c(1:30)){
  #   rep=floor((x-1)/(S))+1
  #   scenarioId= (x-1)%%S+1
  #   print(paste("x=",x,"rep=",rep,"scenario=",scenarioId))
  # }
  
 # run a single SA scenario
  set.seed(rep)
  sa=saScenarios[[saId]]
  print(paste("node=",x,"running rep= ",rep," for SA-scenario=", sa$saId, " and ncd-scenario=",ncdScenarioId,"starting..."))
  start_time <- Sys.time()
  ##
  pop<-initialize.simulation(id = rep,
                           n = POP.SIZE,
                           scenario=ncdScenarios[[ncdScenarioId]]$id)
  #change param value
  pop$params[sa$param]=sa$newVal
  #run the model with new value
  while(pop$params$CYNOW<= 2030)
    run.one.year.int(pop,
                     scenario =ncdScenarios[[ncdScenarioId]]$id,
                     int.start.year = 2023,
                     int.end.year = 2030,
                     pCoverage = ncdScenarios[[ncdScenarioId]]$pCoverage,
                     pNcdTrtInitiation = ncdScenarios[[ncdScenarioId]]$pNcdTrtInitiation,
                     pDropOut=ncdScenarios[[ncdScenarioId]]$pDropOut
    )
  
  #saving population
  res=list(stats=pop$stats,
           params=pop$params)
  saveRDS(res,file = paste0("outputs/popList-sa",sa$saId,"-ncdScenario",ncdScenarios[[ncdScenarioId]]$id,"-rep",rep),compress = T)
  # saving time
  end_time <- Sys.time()
  session_time=end_time - start_time
  txt=paste("Model ",rep," >> session time ",session_time)
  write.table(x = txt,file = "outputs/out-sessionTime.txt",col.names = F,row.names = F,append = T)
}
