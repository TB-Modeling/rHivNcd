#
#  R HIVNCD 2022
#  GlobalEnvironment.R
#  
#####################################
# GLOBAL PARAMETERS ARE CONSTANT. THEY ARE VISIBLE TO ALL CLASSES AND FUNCTIONS AND DONT CHANGE

DEBUGMODE=T
DEBUG=T

ANNUAL.TIMESTEPS=12 #how many timepsteps in a year?
INITIAL.YEAR=2014 #simulation starts
END.YEAR=2040 #simulation ends

#
AGE.INTERVAL=5
MIN.AGE=0
MAX.AGE=85*12

POP.SIZE= 100000 # 1000
#
FEMALE=1
MALE=2
#
HIV.NEG=1
HIV.UNDIAG=2 #undiagnosed
HIV.UNENG=3  #diagnosed but not on trt 
HIV.ENG=4   #on trt & suppressed
#
NCD.NEG=1 #no diabetes or hypertension
NCD.DIAB=2 #diabetic
NCD.HYP=3 #hypertensive
NCD.DIAB_HYP=4 #
NCD.DIAB.TRT=5 #diabetic on treatment
NCD.HYP.TRT=6 #hypertensive on treatment
NCD.DIAB_HYP.TRT=7 #diab&hyp on treatment
NCD.DIAB.TRT.ADH=8 #diabetic on treatment & adherent
NCD.HYP.TRT.ADH=9 #hypertensive on treatment & adherent
NCD.DIAB_HYP.TRT.ADH=10 #diab&hyp on treatment & adherent
#
DEATH.NATURAL=1
DEATH.HIV=2
DEATH.STROKE=3
DEATH.MI=4

DIM.NAMES.SEX=c("FEMALE","MALE")
DIM.NAMES.AGE=c("0-4","5-9","10-14","15-19", "20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59",
                "60-64","65-69","70-74","75-79","80-85")
DIM.NAMES.HIV=c("HIV.NEG","HIV.UNDIAG","HIV.UNENG", "HIV.ENG")
DIM.NAMES.NCD=c("NCD.NEG","NCD.DIAB","NCD.HYP","NCD.DIAB_HYP",
                "NCD.DIAB.TRT","NCD.HYP.TRT","NCD.DIAB_HYP.TRT",
                "NCD.DIAB.TRT.ADH","NCD.HYP.TRT.ADH","NCD.DIAB_HYP.TRT.ADH")
DIM.NAMES.YEAR=c(INITIAL.YEAR:END.YEAR)

DIM.NAMES.AGE.SEX.NCD = list(age = DIM.NAMES.AGE,
                             sex = DIM.NAMES.SEX,
                             ncd = DIM.NAMES.NCD)
DIM.NAMES.AGE.SEX.NCD.HIV = c(DIM.NAMES.AGE.SEX.NCD,
                              list(hiv = DIM.NAMES.HIV))
# this is the order used in state.sizes (hiv then ncd)
DIM.NAMES.AGE.SEX.HIV.NCD = list(age = DIM.NAMES.AGE,
                                 sex = DIM.NAMES.SEX,
                                 hiv = DIM.NAMES.HIV,
                                 ncd = DIM.NAMES.NCD)

DIM.SEX=length(DIM.NAMES.SEX)
DIM.AGE=length(DIM.NAMES.AGE)
DIM.HIV=length(DIM.NAMES.HIV)
DIM.NCD=length(DIM.NAMES.NCD)
DIM.YEAR=length(DIM.NAMES.YEAR)

# NEW NCD SCENARIOS 
BASELINE.VALUES = list(
  coverage = .07, # Hickey et al, 2021; Table 1: 7% self-reported baseline HTN treatment; this is post-dropout 
  treatment = 1, 
  adherence = 0.40, # Hickey et al, 2021: ~40% of non-intervention group had controlled hypertension ("Among those engaged in care, 56% of intervention group participants and 43% of control group participants had controlled hypertension at year 3")
  dropout = NULL # at baseline, model dropouts to maintain 7% on treatment
  # 0.80/12 # Hickey et al, 2021: ~20% of control group who linked to care attended 1 visit per year for each of 3 years of follow up
)

pMonthlyCoverage=0.1/12 #assuming 10% annual coverage 

# LIST OF NCD SCENARIOS
ncdScenarios = list(
  "baseline" = list(id = 1, # baseline 
                    location = "community", # this is new
                    alias = "baseline", # this is new
                    pCoverage = BASELINE.VALUES$coverage, 
                    pNcdTrtInitiation = BASELINE.VALUES$treatment, 
                    # previously: "combination of uptake and adherence", NOW: just uptake (model adherence separately)
                    # but for baseline, this is set to 1 because we are combining coverage and treatment (7%)
                    pNcdTrtAdherence = BASELINE.VALUES$adherence, # this is new
                    pDropOut = BASELINE.VALUES$dropout,
                    hivScenario = "noint" # this is new
  ),
  "Scen.1a" = list(id = 2, # AMPATH
                   location = "clinic",
                   alias = "clinic.AMPATH",
                   pCoverage = pMonthlyCoverage, # need to decide on this 
                   pNcdTrtInitiation = 0.35, # Hickey et al, 2021: 35% of control group linked to care
                   pNcdTrtAdherence = BASELINE.VALUES$adherence + 0.10, # 10% increase in adherence (--> 50%)
                   pDropOut = (0.80/12) - (0.1/12), # 10% increase in retention (--> 70% dropout)
                   hivScenario = "noint"
  ),
  "Scen.1b" = list(id = 3, # SEARCH telehealth (clinic)
                   location = "clinic",
                   alias = "clinic.telehealth",
                   pCoverage = pMonthlyCoverage, 
                   pNcdTrtInitiation = 0.75, # SEARCH telehealth: among those eligible, % linked and randomized 
                   pNcdTrtAdherence = BASELINE.VALUES$adherence + 0.40, # 40% increase in adherence (--> 80%) 
                   pDropOut = (0.80/12) - (0.3/12), # 30% increase in retention (--> 50% dropout)
                   hivScenario = "noint"
  ),
  "Scen.1c" = list(id = 4, # hypothetical, max NCD and HIV
                   location = "clinic",
                   alias = "clinic.max",
                   pCoverage = pMonthlyCoverage, 
                   pNcdTrtInitiation = 0.75,
                   pNcdTrtAdherence = 1, # BASELINE.VALUES$adherence + 0.75, # this is >100%
                   pDropOut = (0.80/12) - (0.75/12), # 75% increase in retention (--> 5% dropout)
                   hivScenario = "comp" # this should be 90/90/90 targets - check HIV model scenarios ("comp" = comprehensive)
  ),
  "Scen.2a" = list(id = 5, # SEARCH telehealth (community)
                   location = "community",
                   alias = "comm.telehealth",
                   pCoverage = pMonthlyCoverage, 
                   pNcdTrtInitiation = 0.75,
                   pNcdTrtAdherence = BASELINE.VALUES$adherence + 0.40,
                   pDropOut = (0.80/12) - (0.3/12),
                   hivScenario = "noint"
  ),
  "Scen.2b" = list(id = 6, # SEARCH telehealth (community) + HIV screening
                   location = "community",
                   alias = "comm.telehealth.hiv.screening",
                   pCoverage = pMonthlyCoverage, 
                   pNcdTrtInitiation = 0.75,
                   pNcdTrtAdherence = BASELINE.VALUES$adherence + 0.40,
                   pDropOut = (0.80/12) - (0.3/12),
                   hivScenario = "tsteng" # this should be screening & linkage - check HIV model scenarios ("tsteng" = testing and engagement)
  ),
  "Scen.3a" = list(id = 7, # SEARCH telehealth (clinic + community) + HIV screening
                   location = c("clinic","community"),
                   alias = "comm.clinic.telehealth.hiv.screening",
                   pCoverage = pMonthlyCoverage, 
                   pNcdTrtInitiation = 0.75,
                   pNcdTrtAdherence = BASELINE.VALUES$adherence + 0.40,
                   pDropOut = (0.80/12) - (0.3/12),
                   hivScenario = "tsteng"
  ),
  "Scen.3b" = list(id = 8, # SEARCH telehealth (clinic + community) + HIV 90/90/90
                   location = c("clinic","community"),
                   alias = "comm.clinic.max",
                   pCoverage = pMonthlyCoverage, 
                   pNcdTrtInitiation = 0.75,
                   pNcdTrtAdherence = BASELINE.VALUES$adherence + 0.40,
                   pDropOut = (0.80/12) - (0.3/12),
                   hivScenario = "comp"
  )
)
#cbind(sapply(ncdScenarios, function(x){x$pNcdTrtInitiation})) # example code 

# global list that is used to generate pop$param
# we have to define parameters used in sensitivity analysis outside of MP first, to change their value before MP is built
SA.PARAMS=list(
  #NCD EVENT RISKS############
  #relative risk of ncd incidence by hiv status (>1 relative to hiv.neg) (can be a single value or an array)
  relative.ncd.risk.by.hiv=1,
  #annual growth in age/sex-specific prev of ncds relative to baseline (>1)
  annual.growth.ncd.prev=1,
  
  #CVD EVENT RISKS############
  #modeling increased cvd risk by HIV state
  cvd.risk.multiplier.hiv=1.5,
  #applying a single multiplier for sensitivity analysis
  cvd.risk.multiplier=1,
  # risk of recurrent event here to 2x original risk; able to change in sensitivity analysis
  recurrent.cvd.event.risk.multiplier=2,
  #probability that the first CVD event is mi (vs stroke)
  prob.first.cvd.event.mi.male = 0.6,
  prob.first.cvd.event.mi.female = 0.6,
  
  #CVD DEATHS############
  #applying a single multiplier for sensitivity analysis
  cvd.mortality.multiplier=1,
  #recurrent events
  recur.stroke.mort.OR.multiplier=2.53, # this is an ODDS RATIO (relative to current probability), so have to convert to odds and then back to probability (in returnCvdMortality function)
  recur.mi.mortality.multiplier=1.856,
  
  #INTERVENTION IMPACT############
  red.cvd.event.hyp.trt= (1-0.3),
  red.cvd.death.hyp.trt= (1-0.26),
  red.cvd.event.diab.trt= (1-0.32),
  red.cvd.death.diab.trt= (1-0.42)
)




################################################################################################################
# MODEL PARAMETERS (MP) HOUSES ALL PARAMETERS THAT MAY BE CHANGED IN SENSITIVITY ANALYSIS. THEY'RE CREATED ONCE FOR EACH POPULATION
generate.new.modelParameter<-function(rep=0, #@MS: Isnt this the replication ID? 
                                      ncdScenario=0,
                                      saScenario=0){
  #variables
  MP<-list(
    REP=rep,
    NCD.SCENARIO=ncdScenario,
    SA.SCENARIO=saScenario,
    TNOW=1, #current timestep
    YNOW=1, #variable showing current year
    CYNOW=INITIAL.YEAR, #calendar year (we start one year earlier, so that we save the initial population state before simulation begins)
    LAST.PERSON.ID=0)
  
  ########################################################
  #1- load HIV data  
  load(paste0("data/hiv_simset_",ncdScenarios[[MP$NCD.SCENARIO]]$hivScenario,".RData")) # loads khm.full object
  print(paste0("reading data/hiv_simset_",ncdScenarios[[MP$NCD.SCENARIO]]$hivScenario))
  
  # MP$khm.full=khm.full # leaving full simset in here for plotting purposes
  # class(MP$khm.full) = "khm_simulation_output"
  #sample one random khm model:
  x=sample(1:length(khm.full),1)
  print(paste("KHM model ",x," was sampled"))
  MP$khm.id=x #khm id that was sampled for this run
  khm = khm.full[[x]]# randomly sample one hiv sim from the length of n.hiv.sims
  khm.hivPrev2015 = khm$population["2015",,,]
  MP$khm=khm
  MP$khm.hivPrev2015=khm.hivPrev2015
  #
  # Making sure the KHM timeline agrees with the NCD model:
  if(!as.numeric(dimnames(khm$incidence)[[1]][[1]])==INITIAL.YEAR)
    stop("Error: KHM starting year is different from NCD model")
  
  n=length(dimnames(khm$incidence)[[1]])
  if(as.numeric(dimnames(khm$incidence)[[1]][[n]])< END.YEAR)
    stop(paste0("Error: KHM END year (",as.numeric(dimnames(khm$incidence)[[1]][[n]]),") is smaller than NCD model (",END.YEAR,")"))
  
  ########################################################
  #2- load STEP dataset to generate the initial population by age, sex and ncd state
  step.dataset = fread("data/stepSimPop2015",header=T)
  step.dataset$agegroup=ceiling((step.dataset$age+1)/AGE.INTERVAL)
  MP$step.dataset=step.dataset
  
  ########################################################
  #3- read target NCD sizes and compute the target proportions based on 2015 STEP dataset
  D<-read.csv("data/ncd.state.sizes.2015.csv",header = T)[,2:9]
  target.ncd.sizes<-array(0,
                          dim=sapply(DIM.NAMES.AGE.SEX.NCD,length),
                          dimnames = DIM.NAMES.AGE.SEX.NCD)
  invisible(lapply(1:4,function(i){
    target.ncd.sizes[,,i]<<-array(unlist(D[,(i*2-1):(i*2)]),dim = c(DIM.AGE,DIM.SEX) )}))
  MP$target.ncd.sizes= target.ncd.sizes
  
  #target ncd proportions in each age/sex subgroup
  target.ncd.props<-target.ncd.sizes
  invisible(sapply(1:length(DIM.NAMES.SEX), function(sex){
    sapply(1:length(DIM.NAMES.AGE), function(age){
      target.ncd.props[age,sex,]<<-target.ncd.props[age,sex,]/sum(target.ncd.sizes[age,sex,]) # double assignment goes back to the most recent value of D in the upper environment
    })}))
  target.ncd.props[is.na(target.ncd.props)]<-0
  #add the HIV dimension:
  q=array(rep(target.ncd.props,4),
          dim = sapply(DIM.NAMES.AGE.SEX.NCD.HIV,length),
          dimnames = DIM.NAMES.AGE.SEX.NCD.HIV)
  q<-aperm(q,c(1,2,4,3)) #reorder dimensions
  MP$target.ncd.props=q 
  
  ###
  
  #initial ncd dist by age and sex #'@MS: for future follow up
  # ncd.dist.age.sex<-target.ncd.sizes
  # invisible(sapply(1:length(DIM.NAMES.NCD), function(ncd){
  #   ncd.dist.age.sex[,,ncd]<<-ncd.dist.age.sex[,,ncd]/sum(target.ncd.sizes[,,ncd]) # double assignment goes back to the most recent value of D in the upper environment
  # }))
  # ncd.dist.age.sex[is.na(ncd.dist.age.sex)]<-0
  # MP$ncd.dist.age.sex=ncd.dist.age.sex
  
  #relative risk of ncd incidence by hiv status (>1 relative to hiv.neg) (can be a single value or an array)
  MP$relative.ncd.risk.by.hiv=SA.PARAMS$relative.ncd.risk.by.hiv 
  #annual growth in age/sex-specific prev of ncds relative to baseline (>1)
  MP$annual.growth.ncd.prev=SA.PARAMS$annual.growth.ncd.prev
  
  ########################################################
  #4-load pooled 10-year CVD risk by age/sex/ncd category
  load('data/10.year.cvd.risk.by.age.sex.ncd.Rdata') #this dataset excludes HIV dimension and is only reported for 4 ncd states without trt
  q=pooled.risk.by.age.sex.ncd
  
  x=array(0,
          dim=sapply(DIM.NAMES.AGE.SEX.NCD,length),
          dimnames = DIM.NAMES.AGE.SEX.NCD)
  # inset q into appropriate location by age sex and ncd state
  x[dimnames(q)[[1]],dimnames(q)[[2]],dimnames(q)[[3]]]<-q
  # copy the same values for ncd states with trt, same for adherence
  x[dimnames(q)[[1]],dimnames(q)[[2]],c("NCD.DIAB.TRT","NCD.HYP.TRT","NCD.DIAB_HYP.TRT")]<-q[,,c("NCD.DIAB","NCD.HYP","NCD.DIAB_HYP")]
  x[dimnames(q)[[1]],dimnames(q)[[2]],c("NCD.DIAB.TRT.ADH","NCD.HYP.TRT.ADH","NCD.DIAB_HYP.TRT.ADH")]<-q[,,c("NCD.DIAB","NCD.HYP","NCD.DIAB_HYP")]
  #adding HIV dimension
  x<-array(rep(x,4),
           dim=sapply(DIM.NAMES.AGE.SEX.NCD.HIV,length),
           dimnames = DIM.NAMES.AGE.SEX.NCD.HIV)
  x<-aperm(x,c(1,2,4,3))
  # dim(x)
  
  #modeling increased cvd risk by HIV state
  MP$cvd.risk.multiplier.hiv=SA.PARAMS$cvd.risk.multiplier.hiv
  x[,,-1,]= MP$cvd.risk.multiplier.hiv*x[,,-1,]
  
  #applying values for 40-44 to younger agegroups and from 70-74 to older agegroups
  MP$cvd.risk.multiplier.15.to.29 = 1/50 
  MP$cvd.risk.multiplier.30.to.39 = 1/6 
  for(i in 1:3) x[c(DIM.NAMES.AGE[i]),,,]=0 # no risk below 15 
  for(i in 4:6) x[c(DIM.NAMES.AGE[i]),,,]=x["40-44",,,]*MP$cvd.risk.multiplier.15.to.29 # 15-19, 20-24, 25-29
  for(i in 7:8) x[c(DIM.NAMES.AGE[i]),,,]=x["40-44",,,]*MP$cvd.risk.multiplier.30.to.39 # 30-34, 35-39
  for(i in 16:17) x[c(DIM.NAMES.AGE[i]),,,]=x["70-74",,,]
  
  #applying a single multiplier for sensitivity analysis
  MP$cvd.risk.multiplier=SA.PARAMS$cvd.risk.multiplier
  x=x* MP$cvd.risk.multiplier
  
  # annual risk computed from an exponential decay
  annual.cvd.risk.by.age.sex=-((log(1- x/100 ))/10) # not included in MP since we only need monthly values 
  #assuming geometric distribution of risk over time
  MP$monthly.cvd.risk.by.age.sex=(1-(1-annual.cvd.risk.by.age.sex)^(1/12))
  
  # risk of recurrent event here to 2x original risk; able to change in sensitivity analysis
  MP$recurrent.cvd.event.risk.multiplier=SA.PARAMS$recurrent.cvd.event.risk.multiplier
  #probability that the first CVD event is mi (vs stroke)
  MP$prob.first.cvd.event.mi.male = SA.PARAMS$prob.first.cvd.event.mi.male 
  MP$prob.first.cvd.event.mi.female = SA.PARAMS$prob.first.cvd.event.mi.female
  
  ########################################################
  #5-load CVD mortality data
  load("data/monthly.stroke.mortality.Rdata")
  load("data/monthly.mi.mortality.Rdata")
  MP$first.stroke.monthly.mortality = stroke.monthly.mortality #first time stroke mortality
  MP$first.mi.monthly.mortality = mi.monthly.mortality
  
  #applying a single multiplier for sensitivity analysis
  MP$cvd.mortality.multiplier=SA.PARAMS$cvd.mortality.multiplier
  stroke.monthly.mortality=stroke.monthly.mortality*MP$cvd.mortality.multiplier
  mi.monthly.mortality=mi.monthly.mortality*MP$cvd.mortality.multiplier
  
  #recurrent events
  recur.stroke.mort.OR.multiplier=SA.PARAMS$recur.stroke.mort.OR.multiplier # this is an ODDS RATIO (relative to current probability), so have to convert to odds and then back to probability (in returnCvdMortality function)
  #adjusted OR:
  x=stroke.monthly.mortality/(1-stroke.monthly.mortality) * recur.stroke.mort.OR.multiplier
  MP$rec.stroke.monthly.mortality= x/ (1+x) #back to prob
  
  recur.mi.mortality.multiplier=SA.PARAMS$recur.mi.mortality.multiplier
  MP$rec.mi.monthly.mortality= mi.monthly.mortality * recur.mi.mortality.multiplier
  
  ########################################################
  #6- Reduction in CVD risk by trt
  MP$red.cvd.event.hyp.trt= SA.PARAMS$red.cvd.event.hyp.trt
  MP$red.cvd.death.hyp.trt= SA.PARAMS$red.cvd.death.hyp.trt
  MP$red.cvd.event.diab.trt= SA.PARAMS$red.cvd.event.diab.trt
  MP$red.cvd.death.diab.trt= SA.PARAMS$red.cvd.death.diab.trt
  MP$red.cvd.event.diabHyp.trt= min(MP$red.cvd.event.hyp.trt,MP$red.cvd.event.diab.trt) #assumption
  MP$red.cvd.death.diabHyp.trt= min(MP$red.cvd.death.hyp.trt,MP$red.cvd.death.diab.trt)
  
  return(MP)
}

################################################################################################################
generate.new.stat<-function(rep=0,
                            ncdScenario=0,
                            saScenario=0){
  
  #global statistics
  DIM.N=END.YEAR-INITIAL.YEAR+1
  DIM.NAMES.N=c(INITIAL.YEAR:(END.YEAR))
  
  #temporary empty arrays to initialize stats
  #1D
  v1temp=rep(0,DIM.N,
             dim=DIM.N,
             dimnames = list(year=DIM.NAMES.N))
  
  
  #5D
  v5temp=array(rep(0,DIM.AGE*DIM.SEX*DIM.HIV*DIM.NCD*DIM.N),  
               dim = c(DIM.AGE,
                       DIM.SEX,
                       DIM.HIV,
                       DIM.NCD,
                       DIM.N),
               dimnames=list(age = DIM.NAMES.AGE,
                             sex = DIM.NAMES.SEX,
                             hiv.status = DIM.NAMES.HIV,
                             ncd.status = DIM.NAMES.NCD,
                             year = DIM.NAMES.N))
  
  
  stats<-list(
    rep.id=rep,
    ncd.id=ncdScenario,
    sa.id=saScenario,
    
    #1D arrays for entire population over time
    pop.size=v1temp,
    n.births=v1temp,
    n.births.non.hiv=v1temp,
    n.births.hiv=v1temp,
    annual.ncd.trt.coverage = v1temp,
    
    
    #5D arrays [age, sex, hiv, ncd, year]
    # counting events modeled (incidence, getting diagnosed, etc)
    n.hiv.inc=v5temp, 
    n.hiv.diag=v5temp, 
    n.hiv.eng=v5temp, 
    n.hiv.uneng=v5temp, 
    
    # ncd incidence
    n.diab.hyp.inc=v5temp,
    n.diab.inc=v5temp,
    n.hyp.inc=v5temp,
    
    #cvd incidence
    n.mi.inc=v5temp,
    n.stroke.inc=v5temp,
    
    ########### intervention stats ##########
    # hiv additional diagnosis
    # n.hiv.diag.int=v5temp,
    # hiv additional treatment
    # n.hiv.trt.int=v5temp,
    
    n.ncd.screened=v5temp,
    
    # ncd new diagnosis
    n.diab.diag=v5temp,
    n.hyp.diag=v5temp,
    n.diab.hyp.diag=v5temp,
    
    # ncd treatment initiation
    n.diab.trt=v5temp,
    n.hyp.trt=v5temp,
    n.diab.hyp.trt=v5temp,
    
    # ncd treatment ADHERENCE
    n.diab.trt.adherence=v5temp,
    n.hyp.trt.adherence=v5temp,
    n.diab.hyp.trt.adherence=v5temp,
    
    #ncd trt dropouts
    n.diab.trt.dropout=v5temp,
    n.hyp.trt.dropout=v5temp,
    n.diab.hyp.trt.dropout=v5temp,
    
    
    ## STATE SIZES ##
    n.state.sizes=v5temp,
    
    n.deaths.hiv=v5temp,
    n.deaths.cvd=v5temp,
    n.deaths.ageout=v5temp,
    n.deaths.non.hiv=v5temp
  )
  return(stats)
}



# Code to extract the initial NCD prevalences from the STEP survey
# Saved externally; read into the model everytime (so we no longer need this code; but keeping for now)
# #add agegroups
# step.dataset$agegroup[step.dataset$agegroup<1]<-1
# step.dataset$agegroup[step.dataset$agegroup>DIM.AGE]<-GP$DIM.AGE
# #add ncd state
# step.dataset$ncdstate=step.dataset$diabetes + 2*step.dataset$hypertension + 1 
# #loop through and count the state sizes
# ncd.state.sizes<-array(0,
#                        dim=c(GP$DIM.AGE,GP$DIM.SEX,GP$DIM.NCD),
#                        dimnames = list(GP$DIM.NAMES.AGE,GP$DIM.NAMES.SEX,GP$DIM.NAMES.NCD))
# invisible(lapply(1:nrow(step.dataset),function(x){
#   p<-step.dataset[x,]
#   ncd.state.sizes[p$agegroup,
#                   p$sex,
#                   p$ncdstate] <<- ncd.state.sizes[   p$agegroup,
#                                                      p$sex,
#                                                      p$ncdstate] +1
#   }))
# write.csv(ncd.state.sizes,file = "ncd.state.sizes.2015.csv")






cat("Sourced globalEnvironment.R .... \n")
