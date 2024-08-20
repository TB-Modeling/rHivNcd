#  R HIVNCD 2022
#  Driver.R class
#  
#####################################
# list.of.packages <- c("ggplot2", "R6","Rcpp")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

library(R6)
library(data.table)
library(ggplot2)
library(boot)
# library(pryr)
suppressPackageStartupMessages(library(pryr))


#######################################################
{
  source("globalEnvironment.R")
  source("person.R")
  source("population.R")
  source("rHelperFunctions.R")
  source("rCoreFunctions.R")
}
# # #######################################################
# # # SINGLE RUN ON ROCKFISH
# # #######################################################
#receiving the input from command line and printing it
if(1==1){
  # Making sure that the output folder exists (it's ignored by git)
  output_folder <- "outputs"
  # Check if the folder exists
  if (!dir.exists(output_folder)) {
    # Create the folder if it does not exist
    dir.create(output_folder, recursive = TRUE)
  }
  #####
  args <- commandArgs(trailingOnly = TRUE)
  # Check if the correct number of arguments is provided
  if (length(args) != 1) {
    stop("Please provide exactly one argument")
  }
  # Convert the argument to a numeric value
  input_value <- as.numeric(args[1])
  # Ensure the argument is a valid number
  if (is.na(input_value)) {
    stop("The provided argument is not a valid number")
  }
  # Print the input value to verify
  print(paste("Input value is:", input_value))
  
  #print(as.numeric(args[1]))
  test.calibration = calibrate.baseline.single.rep(replication.id = as.numeric(args[1]))
  #save(test.calibration, file = paste0("outputs/test.calibration_",Sys.Date(),".RData"))
  save(test.calibration,file = paste0("outputs/test.calibration_",as.numeric(args[1]),".RData")) # to save on Rockfish? 
}




# # #######################################################
# # # SINGLE RUN ON LOCAL COMPUTER
# # #######################################################
#receiving the input from command line and printing it
if(1==2){
  #make sure the output folder exists, and if not, create it
  output_folder <- "~/Downloads/outputs"
  # Check if the folder exists
  if (!dir.exists(output_folder)) {
    # Create the folder if it does not exist
    dir.create(output_folder, recursive = TRUE)
  }
  
  test.calibration = calibrate.baseline.single.rep(replication.id = 1)
  save(test.calibration,file = paste0(output_folder,"/test.calibration_",1,".RData"))  
}

# # # #######################################################
# # # # # PROFILE
# library(profvis)
# profvis({
#   #make sure the output folder exists, and if not, create it
#   output_folder <- "~/Downloads/outputs"
#   # Check if the folder exists
#   if (!dir.exists(output_folder)) {
#     # Create the folder if it does not exist
#     dir.create(output_folder, recursive = TRUE)
#   }
#   ###
#   set.seed(1)
#   test.calibration = calibrate.baseline.single.rep(replication.id = 1)
#   save(test.calibration,file = paste0(output_folder,"/test.calibration_",1,".RData"))
# })

 
# # # #######################################################
# # # # SINGLE RUN LOCALLY
# if (1==1) {
#   vReps=1:2 #reps
#   vNcdScenarios=1#scenarios
#   #############
#   print("running models sequentially ....")
#   nReps=length(vReps)
#   nNcdScenarios=length(vNcdScenarios)
#   
#   print(paste("running models parallel with ",nReps,"reps and",nNcdScenarios,"ncdScenarios"))
#   
#   args = commandArgs(trailingOnly=TRUE)
#   x=as.numeric(args[1])
#   rep=floor((x-1)/(nNcdScenarios))+1
#   ncdId= (x-1)%%nNcdScenarios+1
#   
#   # for (x in c(1:150)){
#   #   rep=floor((x-1)/(nNcdScenarios))+1
#   #   scenarioId= (x-1)%%nNcdScenarios+1
#   #   print(paste("x=",x,"rep=",rep,"scenario=",scenarioId))
#   # }
#   
#   # create pop at the end of 2014; set up hiv/ncd states; records stats and increament the year to 2015
#   set.seed(rep)
#   start_time <- Sys.time()
#   print(paste("replication ",rep," scenario", ncdScenarios[[ncdId]]$id, "starting..."))
#   
#   # create pop at the end of 2014; set up hiv/ncd states; records stats and increament the year to 2015
#   pop<-initialize.simulation(id = rep,
#                              n = POP.SIZE,
#                              rep=rep,
#                              ncdScenario = ncdScenarios[[ncdId]]$id,
#                              saScenario = 0)
#   #run sims
#   while(pop$params$CYNOW<= END.YEAR)
#     run.one.year.int(pop,
#                      ncdScenario = ncdScenarios[[ncdId]]$id,
#                      int.start.year = 2023,
#                      int.end.year = 2030,
#                      pCoverage = ncdScenarios[[ncdId]]$pCoverage,
#                      pNcdTrtInitiation = ncdScenarios[[ncdId]]$pNcdTrtInitiation,
#                      pNcdTrtAdherence = ncdScenarios[[ncdId]]$pNcdTrtAdherence,
#                      pDropOut=ncdScenarios[[ncdId]]$pDropOut
#     )
#   
#   #saving population stat and param files separately
#   saveRDS(pop$stats,file = paste0("outputs/popStats-node",x,"-ncd",ncdScenarios[[ncdId]]$id,"-rep",rep),compress = T)
#   saveRDS(pop$params,file = paste0("outputs/popParams-node",x,"-ncd",ncdScenarios[[ncdId]]$id,"-rep",rep),compress = T)
#   
#   # saving time
#   end_time <- Sys.time()
#   session_time=hms_span(start_time,end_time)
#   txt=paste("rep= ",rep," ncdScenario=", ncdScenarios[[ncdId]]$id," >> session time ",session_time)
#   print(txt)
#   write.table(x = txt,file = "outputs/out-sessionTime.txt",col.names = F,row.names = F,append = T)
# }
# 
# 
# # #######################################################
# # MULTI REPS
# if (1==2){
#   vReps=1:1 #reps
#   vNcdScenarios=1#:8 #scenarios
#   print("running models sequentially ....")
#   nReps=length(vReps)
#   
#   lapply(vReps,function(rep){
#     lapply(vNcdScenarios,function(ncdId){
#       set.seed(rep)
#       print(paste("replication ",rep," scenario", ncdScenarios[[ncdId]]$id, "starting..."))
#       
#       # create pop at the end of 2014; set up hiv/ncd states; set up ncd treatment; records stats and increment the year to 2015
#       pop<-initialize.simulation(id = rep,
#                                  n = POP.SIZE,
#                                  rep=rep,
#                                  ncdScenario = ncdScenarios[[ncdId]]$id,
#                                  saScenario = 0) 
#       
#       while(pop$params$CYNOW<= END.YEAR){
#         run.one.year.baseline(pop,
#                               p.monthly.baseline.enrollment=(.1/12), # sample this during calibration 
#                               p.monthly.baseline.dropout=(.1/12) # sample this during calibration
#         ) 
#         
#       }
#       
#       # run.one.year.int(pop,
#       #                  ncdScenario =ncdScenarios[[ncdId]]$id,
#       #                  int.start.year = 2023,
#       #                  int.end.year = 2030,
#       #                  pCoverage = ncdScenarios[[ncdId]]$pCoverage,
#       #                  pNcdTrtInitiation = ncdScenarios[[ncdId]]$pNcdTrtInitiation,
#       #                  pNcdTrtAdherence = ncdScenarios[[ncdId]]$pNcdTrtAdherence,
#       #                  pDropOut=ncdScenarios[[ncdId]]$pDropOut
#       # )
#       
#       #saving population stat and param files separately
#       x=0
#       saveRDS(pop$stats,file = paste0("outputs/popStats-node",x,"-ncd",ncdScenarios[[ncdId]]$id,"-rep",rep),compress = T)
#       saveRDS(pop$params,file = paste0("outputs/popParams-node",x,"-ncd",ncdScenarios[[ncdId]]$id,"-rep",rep),compress = T)
#       
#     })
#   })
# }
# 
# # 
# # if(1==2){
# #   # reps where the mean coverage = 3.5-14% (7/2 - 7*2)
# #   # reps.to.include = dimnames(
# #   #   test.calibration$coverage[(apply(test.calibration$coverage,"rep",mean)>.035) & 
# #   #                               (apply(test.calibration$coverage,"rep",mean)<.14),])$rep
# #   
# #   # in 2015 specifically 
# #   reps.to.include = dimnames(
# #     test.calibration$coverage[(test.calibration$coverage[,"2015"]>.035) & 
# #                                 (test.calibration$coverage[,"2015"]<.14),])$rep
# #   
# #   #x = cbind(test.calibration$coverage[,"2015"],exp(test.calibration$log.lik))
# #   # apply(test.calibration$coverage,"rep",mean)
# #   # mean(test.calibration$coverage)
# #   # mean(test.calibration$inputs[reps.to.include,"enrollment"])
# #   # mean(test.calibration$inputs[reps.to.include,"dropout"])
# #   
# #   ggplot() +
# #     geom_line(data = reshape2::melt(test.calibration$coverage[reps.to.include,]),
# #               aes(x=year,y=value,color=as.character(rep)))
# # }