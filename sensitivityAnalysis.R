#NCD EVENT RISKS
#relative risk of ncd incidence by hiv status (>1 relative to hiv.neg) (can be a single value or an array)
MP$relative.ncd.risk.by.hiv=1 
#annual growth in age/sex-specific prev of ncds relative to baseline (>1)
MP$annual.growth.ncd.prev=1


#CVD EVENT RISKS
#modeling increased cvd risk by HIV state
MP$cvd.risk.multiplier.hiv=1.5
#applying a single multiplier for sensitivity analysis
MP$cvd.risk.multiplier=1
# risk of recurrent event here to 2x original risk; able to change in sensitivity analysis
MP$recurrent.cvd.event.risk.multiplier=2
#probability that the first CVD event is mi (vs stroke)
MP$prob.first.cvd.event.mi.male = 0.6 
MP$prob.first.cvd.event.mi.female = 0.6 

#CVD DEATHS
#applying a single multiplier for sensitivity analysis
MP$cvd.mortality.multiplier=1
#recurrent events
recur.stroke.mort.OR.multiplier=2.53 # this is an ODDS RATIO (relative to current probability), so have to convert to odds and then back to probability (in returnCvdMortality function)
recur.mi.mortality.multiplier=1.856 

#INTERVENTION IMPACT
MP$red.cvd.event.hyp.trt= (1-0.3)
MP$red.cvd.death.hyp.trt= (1-0.26)
MP$red.cvd.event.diab.trt= (1-0.32)
MP$red.cvd.death.diab.trt= (1-0.42)

th=0.25
saVars=list{
  c("relative.ncd.risk.by.hiv", 1, 1*(1-th), 1*(1+th)), ### strictly increasing? 
  c("annual.growth.ncd.prev", 1, 1*(1-th), 1*(1+th)), ### strictly increasing? 
  c("cvd.risk.multiplier.hiv", 1.5, 1, 2)),
....
}
  