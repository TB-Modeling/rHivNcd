N.REPS.500k = 6
CALIBRATION.DIR.500k = "calibration/calibration_08_23"

N.REPS.100k = 24
CALIBRATION.DIR.100k = "calibration/calibration_08_19"

N.REPS = 1008
CALIBRATION.DIR = "calibration/calibration_08_28"

read.calibration.results = function(n.reps,
                                    calibration.dir){
  
  dim.names = list(replication.id = 1:n.reps,
                   value = c("enrollment","dropout",
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

output = read.calibration.results(n.reps = N.REPS,calibration.dir = CALIBRATION.DIR)
# need to sample from this based on log.likelihood

resampled.enrollment = sample(output[,"enrollment"],1000,prob = output[,"exp(log.lik)"],replace = T)
quantile(resampled.enrollment)

resampled.ratio = sample(output[,"ratio"],1000,prob = output[,"exp(log.lik)"],replace = T)
quantile(resampled.ratio)
quantile(output[,"ratio"])

mean(output[output[,"ratio"]>10,"coverage.2015-2020"])

mean(output[,"coverage.2015-2020"])

qplot(output[,"coverage.2015-2020"])

# sample the parameter set based on likelihood
# 50% of weight across 50 simulations or 90% of weight across 500 simulations - rule of thumb 
# might not get to this in one generation 

output.sorted = output[order(output[,"exp(log.lik)"],decreasing = T),]
cdf = cumsum(output.sorted[,"exp(log.lik)"])
output.sorted = cbind(output.sorted,cdf)

percent = output.sorted[,"exp(log.lik)"]/sum(output.sorted[,"exp(log.lik)"])
cdf.percent = cumsum(percent)
output.sorted = cbind(output.sorted,cdf.percent)
plot(output.sorted[,"cdf"])
plot(output.sorted[,"cdf.percent"])
mean(output.sorted[,"cdf.percent"]<.9) # 8 simulations (0.8%) make up 90% of the likelihood...
