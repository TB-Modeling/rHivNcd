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
# LIST OF NCD SCENARIOS
#coverage and dropouts should be specified as monthly values - but treatment initiation and adherence don't have to be? 
pMonthlyCoverage=0.1/12#assuming 10% annual coverage 

# NEW NCD SCENARIOS
baselineValues = list(
  coverage = .07, # Hickey et al, 2021; Table 1: 7% self-reported baseline HTN treatment; this is post-dropout 
  treatment = 1, 
  adherence = 0.40, # Hickey et al, 2021: ~40% of non-intervention group had controlled hypertension ("Among those engaged in care, 56% of intervention group participants and 43% of control group participants had controlled hypertension at year 3")
  dropout = NULL
    # 0.80/12 # Hickey et al, 2021: ~20% of control group who linked to care attended 1 visit per year for each of 3 years of follow up
                    # Or do we just model drop-outs to maintain 7% on treatment? --> INCREASE UPTAKE TO MAKE SURE TREATMENT COVERAGE = 7%
                    # I.e., there are new diagnoses each year, some start on treatment, model the equivalent # dropping out 
)

#cbind(sapply(ncdScenarios, function(x){x$pNcdTrtInitiation})) # example code 
ncdScenarios = list(
  "baseline" = list(id = 1, # baseline 
                    location = "community", # this is new
                    alias = "baseline", # this is new
                    pCoverage = baselineValues$coverage, 
                    pNcdTrtInitiation = baselineValues$treatment, 
                        # previously: "combination of uptake and adherence", NOW: just uptake (model adherence separately)
                        # but for baseline, this is set to 1 because we are combining coverage and treatment (7%)
                    pNcdTrtAdherence = baselineValues$adherence, # this is new
                    pDropOut = baselineValues$dropout,
                    hivScenario = "noint" # this is new
  ),
  "Scen.1a" = list(id = 2, # AMPATH
                   location = "clinic",
                   alias = "clinic.AMPATH",
                   pCoverage = pMonthlyCoverage, # need to decide on this 
                   pNcdTrtInitiation = 0.35, # Hickey et al, 2021: 35% of control group linked to care
                   pNcdTrtAdherence = baselineValues$adherence + 0.10, # 10% increase in adherence (--> 50%)
                   pDropOut = (0.80/12) - (0.1/12), # 10% increase in retention (--> 70% dropout)
                   hivScenario = "noint"
  ),
  "Scen.1b" = list(id = 3, # SEARCH telehealth (clinic)
                   location = "clinic",
                   alias = "clinic.telehealth",
                   pCoverage = pMonthlyCoverage, 
                   pNcdTrtInitiation = 0.75, # SEARCH telehealth: among those eligible, % linked and randomized 
                   pNcdTrtAdherence = baselineValues$adherence + 0.40, # 40% increase in adherence (--> 80%) 
                   pDropOut = (0.80/12) - (0.3/12), # 30% increase in retention (--> 50% dropout)
                   hivScenario = "noint"
  ),
  "Scen.1c" = list(id = 4, # hypothetical, max NCD and HIV
                   location = "clinic",
                   alias = "clinic.max",
                   pCoverage = pMonthlyCoverage, 
                   pNcdTrtInitiation = 0.75,
                   pNcdTrtAdherence = 1, # baselineValues$adherence + 0.75, # this is >100%
                   pDropOut = (0.80/12) - (0.75/12), # 75% increase in retention (--> 5% dropout)
                   hivScenario = "comp" # this should be 90/90/90 targets - check HIV model scenarios ("comp" = comprehensive)
  ),
  "Scen.2a" = list(id = 5, # SEARCH telehealth (community)
                   location = "community",
                   alias = "comm.telehealth",
                   pCoverage = pMonthlyCoverage, 
                   pNcdTrtInitiation = 0.75,
                   pNcdTrtAdherence = baselineValues$adherence + 0.40,
                   pDropOut = (0.80/12) - (0.3/12),
                   hivScenario = "noint"
  ),
  "Scen.2b" = list(id = 6, # SEARCH telehealth (community) + HIV screening
                   location = "community",
                   alias = "comm.telehealth.hiv.screening",
                   pCoverage = pMonthlyCoverage, 
                   pNcdTrtInitiation = 0.75,
                   pNcdTrtAdherence = baselineValues$adherence + 0.40,
                   pDropOut = (0.80/12) - (0.3/12),
                   hivScenario = "tsteng" # this should be screening & linkage - check HIV model scenarios ("tsteng" = testing and engagement)
  ),
  "Scen.3a" = list(id = 7, # SEARCH telehealth (clinic + community) + HIV screening
                   location = c("clinic","community"),
                   alias = "comm.clinic.telehealth.hiv.screening",
                   pCoverage = pMonthlyCoverage, 
                   pNcdTrtInitiation = 0.75,
                   pNcdTrtAdherence = baselineValues$adherence + 0.40,
                   pDropOut = (0.80/12) - (0.3/12),
                   hivScenario = "tsteng"
  ),
  "Scen.3b" = list(id = 8, # SEARCH telehealth (clinic + community) + HIV 90/90/90
                   location = c("clinic","community"),
                   alias = "comm.clinic.max",
                   pCoverage = pMonthlyCoverage, 
                   pNcdTrtInitiation = 0.75,
                   pNcdTrtAdherence = baselineValues$adherence + 0.40,
                   pDropOut = (0.80/12) - (0.3/12),
                   hivScenario = "comp"
  )
)

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

# # #######################################################
# # # SINGLE RUN ON ROCKFISH
if (1==2) {
  vReps=1:100 #reps
  vNcdScenarios=1:7 #scenarios
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
                             rep=rep,
                             ncdScenario = ncdScenarios[[ncdId]]$id,
                             saScenario = 0)
  #run sims
  while(pop$params$CYNOW<= END.YEAR)
    run.one.year.int(pop,
                     ncdScenario = ncdScenarios[[ncdId]]$id,
                     int.start.year = 2023,
                     int.end.year = 2030,
                     pCoverage = ncdScenarios[[ncdId]]$pCoverage,
                     pNcdTrtInitiation = ncdScenarios[[ncdId]]$pNcdTrtInitiation,
                     pNcdTrtAdherence = ncdScenarios[[ncdId]]$pNcdTrtAdherence,
                     pDropOut=ncdScenarios[[ncdId]]$pDropOut
    )

  #saving population stat and param files separately
  saveRDS(pop$stats,file = paste0("outputs/popStats-node",x,"-ncd",ncdScenarios[[ncdId]]$id,"-rep",rep),compress = T)
  saveRDS(pop$params,file = paste0("outputs/popParams-node",x,"-ncd",ncdScenarios[[ncdId]]$id,"-rep",rep),compress = T)

  # saving time
  end_time <- Sys.time()
  session_time=hms_span(start_time,end_time)
  txt=paste("rep= ",rep," ncdScenario=", ncdScenarios[[ncdId]]$id," >> session time ",session_time)
  print(txt)
  write.table(x = txt,file = "outputs/out-sessionTime.txt",col.names = F,row.names = F,append = T)
}


# #######################################################
# MULTI REPS
if (1==1){
  vReps=1:1 #reps
  vNcdScenarios=1:8 #scenarios
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
                                 rep=rep,
                                 ncdScenario = ncdScenarios[[ncdId]]$id,
                                 saScenario = 0)
      #run sims

      while(pop$params$CYNOW<= END.YEAR)
        run.one.year.int(pop,
                         ncdScenario =ncdScenarios[[ncdId]]$id,
                         int.start.year = 2023,
                         int.end.year = 2030,
                         pCoverage = ncdScenarios[[ncdId]]$pCoverage,
                         pNcdTrtInitiation = ncdScenarios[[ncdId]]$pNcdTrtInitiation,
                         pNcdTrtAdherence = ncdScenarios[[ncdId]]$pNcdTrtAdherence,
                         pDropOut=ncdScenarios[[ncdId]]$pDropOut
        )

      #saving population stat and param files separately
      x=0
      saveRDS(pop$stats,file = paste0("outputs/popStats-node",x,"-ncd",ncdScenarios[[ncdId]]$id,"-rep",rep),compress = T)
      saveRDS(pop$params,file = paste0("outputs/popParams-node",x,"-ncd",ncdScenarios[[ncdId]]$id,"-rep",rep),compress = T)

    })
  })
}

