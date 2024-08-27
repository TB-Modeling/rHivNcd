N.REPS.500k = 6
CALIBRATION.DIR.500k = "calibration/calibration_08_23"

N.REPS.100k = 24
CALIBRATION.DIR.100k = "calibration/calibration_08_19"

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

test.500k = read.calibration.results(n.reps = N.REPS.500k,calibration.dir = CALIBRATION.DIR.500k)
test.100k = read.calibration.results(n.reps = N.REPS.100k,calibration.dir = CALIBRATION.DIR.100k)

x = c()
for(i in 1:6){
  x[i] = abs(test.100k[i,"coverage.2015-2020"] - test.500k[i,"coverage.2015-2020"])/test.500k[i,"coverage.2015-2020"]
}
x

