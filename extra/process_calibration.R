N.REPS = 24
CALIBRATION.DIR = "calibration/calibration_08_19"

read.calibration.results = function(n.reps,
                                    calibration.dir){
  
  dim.names = list(replication.id = 1:n.reps,
                   value = c("enrollment","dropout","ratio",
                             "coverage.2015-2020","exp(log.lik)",
                             "khm.id","seed","pop.2015"))
  
  rv.array = array(NA,
                   dim = sapply(dim.names,length),
                   dimnames = dim.names)
  
  for(rep in 1:n.reps){
    load(paste0(calibration.dir,"/test.calibration_",rep,".RData"))
    rv.array[rep,] = test.calibration$summary
    
    
  }
  
  rv.array
  
}

x = read.calibration.results(n.reps = N.REPS,calibration.dir = CALIBRATION.DIR)
