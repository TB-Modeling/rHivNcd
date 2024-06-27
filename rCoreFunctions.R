#
#  R HIVNCD 2022
#  rCoreFunctions.R class
#  
#####################################
print("Sourcing rCoreFunctions.R ... ")

##-----------------------------##
##-- MAIN CALIBRATE FUNCTION --##
##-----------------------------##
calibrate.baseline <- function(n.reps){
  
  # Set up arrays for storing things
  dim.names.inputs = list(rep = c(1:n.reps),
                          inputs = c("enrollment","dropout","ratio"))
  inputs = array(NA,
                 dim = sapply(dim.names.inputs,length),
                 dimnames = dim.names.inputs)
  
  dim.names.coverage = list(rep = c(1:n.reps),
                            year = (c(INITIAL.YEAR:END.YEAR)))
  coverage = array(NA,
                   dim = sapply(dim.names.coverage,length),
                   dimnames = dim.names.coverage)
  
  khm.ids = c()
  seeds = c()
  
  for(rep in (1:n.reps)){
    print(paste("replication ",rep," starting..."))
    
    # SAMPLE AND STORE INPUT VALUES 
    p.monthly.baseline.enrollment = rlnorm(1, meanlog=log(0.008/12), sdlog=log(4)/2) 
    dropout.to.enrollment.ratio = rlnorm(1, meanlog=0, sdlog=log(4)/2) 
    p.monthly.baseline.dropout = p.monthly.baseline.enrollment * dropout.to.enrollment.ratio
    # p.monthly.baseline.dropout = rlnorm(1, meanlog=log(0.03/12), sdlog=log(4)/2)
    # log(x)/2 --> can be off by a factor of x (i.e., CI on log scale goes from (1/x)*mean to x*mean)
    
    inputs[rep,"enrollment"] = p.monthly.baseline.enrollment
    inputs[rep,"dropout"] = p.monthly.baseline.dropout
    inputs[rep,"ratio"] = dropout.to.enrollment.ratio
    
    
    # INITIALIZE AND RUN SIM 
    set.seed(rep)
    seeds[rep] = rep
    pop<-initialize.simulation(id = rep,
                               n = POP.SIZE,
                               rep=rep,
                               ncdScenario = 1, # hard-coding for baseline 
                               saScenario = 0) 
    
    khm.ids[rep] = pop$params$khm.id
    
    while(pop$params$CYNOW<= END.YEAR){
      run.one.year.baseline(pop,
                            p.monthly.baseline.enrollment=p.monthly.baseline.enrollment, # sampled above
                            p.monthly.baseline.dropout=p.monthly.baseline.dropout # sampled above
      ) 
      
    }
    
    # STORE COVERAGE 
    coverage[rep,] = pop$stats$annual.ncd.trt.coverage
    
    
    #saving population stat and param files separately
    # x=0
    # saveRDS(pop$stats,file = paste0("outputs/popStats-node",x,"-ncd",ncdScenarios[[ncdId]]$id,"-rep",rep),compress = T)
    # saveRDS(pop$params,file = paste0("outputs/popParams-node",x,"-ncd",ncdScenarios[[ncdId]]$id,"-rep",rep),compress = T)
    
  }
  
  rv = list()
  rv$coverage = coverage[,-1]
  rv$inputs = inputs
  rv$khm.ids = khm.ids
  rv$seeds = seeds
  
  rv
  
}

##------------------------------##
##-- MAIN INITIALIZE FUNCTION --##
##------------------------------##
initialize.simulation <- function( id=0,
                                   n=0 ,
                                   rep=0, #replication id
                                   ncdScenario=0, #ncd scenario
                                   saScenario=0 #sa scenario
){
  # 1- create an empty population
  pop<-POPULATION$new(id = id,
                      members = list(),
                      params = generate.new.modelParameter(rep,ncdScenario,saScenario),
                      stats =  generate.new.stat(rep,ncdScenario,saScenario))
  
  # 2- create member list of persons for this population 
  #subset the first n row to create n persons
  sim.pop=pop$params$step.dataset
  if (n>nrow(sim.pop)) stop ("Requested size of initial population is greater than simpop data in 2015")
  sim.pop=sim.pop[1:n,]
  #set up attributes for each row
  sim.pop$age=sim.pop$age * 12 +sample((0:11),size = n,replace = T)#monthly age values
  sim.pop$age[sim.pop$age>MAX.AGE] = MAX.AGE
  sim.pop$sex[sim.pop$sex=="MALE"] = MALE 
  sim.pop$sex[sim.pop$sex=="FEMALE"] = FEMALE
  sim.pop$ncd[sim.pop$hypertension==0 & sim.pop$diabetes==0] = NCD.NEG #1, neither 
  sim.pop$ncd[sim.pop$hypertension==0 & sim.pop$diabetes==1] = NCD.DIAB #2, diabetic
  sim.pop$ncd[sim.pop$hypertension==1 & sim.pop$diabetes==0] = NCD.HYP #3, hypertensive
  sim.pop$ncd[sim.pop$hypertension==1 & sim.pop$diabetes==1] = NCD.DIAB_HYP #4, both
  #vector of attributes for agents
  vIds = c(1:n) #@MS: the person id is a unique number that we use to differentiate people. 
  # we need to start this from 0 and add it over time. 
  # otherwise, the future values may overlap with ones you're reading from the step dataset sim.pop$X
  vAges = sim.pop$age
  vSexes = as.numeric(sim.pop$sex)
  vNcdState = sim.pop$ncd
  vHivState = rep(HIV.NEG,n)
  vtDiabInc= c(sim.pop$ncd%in%c(NCD.DIAB))*0
  vtHypInc= c(sim.pop$ncd%in%c(NCD.HYP))*0
  vtDiabHypInc= c(sim.pop$ncd%in%c(NCD.DIAB_HYP))*0
  tborn=pop$params$TNOW
  memberList = (mapply(PERSON$new,
                       tborn,
                       vIds,
                       vSexes,
                       vAges,
                       vHivState,
                       vNcdState,
                       vtDiabInc,
                       vtHypInc,
                       vtDiabHypInc
  ))
  memberList = unlist(memberList)
  pop$members<-memberList
  pop$stats$ncd.id<-id
  # browser()
  # pop$greet()
  #
  pop<-invisible(set.initial.hiv.status(pop ))
  pop<-invisible(set.cvd.risk(pop))
  pop<-invisible(initialize.ncd.treatment(pop)) # make sure this doesn't need to be before cvd risk
  ## MS ^ rename these arguments to be similar
  pop$record.annual.stats()
  pop$increaseYear() 
  #
  pop
}

##--------------------------------##
##-- MAIN BASELINE RUN FUNCTION --##
##--------------------------------##
# model one simulated year under baseline treatment (from Jan 1st to Dec 31st)
run.one.year.baseline <-function(pop,
                                 p.monthly.baseline.enrollment, # sample this during calibration 
                                 p.monthly.baseline.dropout # sample this during calibration  
){ 
  #### AT YEAR's BEGINNING: computing event probabilities from KHM
  khm<-compute.khm.probabilities(pop)
  
  ##### AT EACH TIMESTEP WITHIN THE YEAR:
  for(i in (1:ANNUAL.TIMESTEPS)){
    # print(pop$params$TNOW)
    
    # 1- modeling cvd events based on current cvd annual risk    # counting new events: marking those who will die after the event
    pop<-model.cvd.events(pop)
    
    # 2- modeling HIV transitions based on khm outputs
    pop<-model.hiv.transitions(pop,
                               khm$khm.prob.hiv.inc,
                               khm$khm.prob.hiv.eng,
                               khm$khm.prob.hiv.diseng,
                               khm$khm.prob.hiv.diag)
    
    # 3- modeling & removing HIV and CVD deaths
    pop<-model.hiv.cvd.deaths(pop,
                              khm$khm.monthly.hiv.mort,
                              khm$khm.monthly.non.hiv.mort)
    
    # MODEL BASELINE ------
    pop<-model.ncd.baseline(pop,
                            p.monthly.baseline.enrollment,
                            p.monthly.baseline.dropout)
    
    
    #4- MODEL AGING --------
    pop$modelAging()
    
    #5- MODEL BIRTHS -------
    {
      n.births.non.hiv=khm$n.births.non.hiv.per.month[i]
      n.births.hiv=khm$n.births.hiv.per.month[i]
      
      if (n.births.non.hiv>0){
        vIds = c((pop$params$LAST.PERSON.ID+1): (pop$params$LAST.PERSON.ID+n.births.non.hiv))
        pop$params$LAST.PERSON.ID=pop$params$LAST.PERSON.ID+n.births.non.hiv
        vSexes = sample(c(MALE,FEMALE),n.births.non.hiv,prob = c(.5,.5),replace = T) # still 50/50 male/female
        memberListNew = (mapply(PERSON$new, pop$params$TNOW,
                                vIds,
                                vSexes,
                                0,#age
                                HIV.NEG,
                                NCD.NEG)) 
        pop$addMembers(unlist(memberListNew))
        # record stats:
        pop$stats$n.births.non.hiv[pop$params$YNOW]=n.births.non.hiv
      }
      if (n.births.hiv>0){
        vIds = c((pop$params$LAST.PERSON.ID+1): (pop$params$LAST.PERSON.ID+n.births.hiv))
        pop$params$LAST.PERSON.ID=pop$params$LAST.PERSON.ID+n.births.hiv
        vSexes = sample(c(MALE,FEMALE),n.births.hiv,prob = c(.5,.5),replace = T)
        memberListNew = (mapply(PERSON$new,pop$params$TNOW,
                                vIds,
                                vSexes,
                                0,#age
                                HIV.UNDIAG,
                                NCD.NEG))  
        pop$addMembers(memberListNew)
        # record stats:
        pop$stats$n.births.hiv[pop$params$YNOW]=n.births.hiv
        nMaleNewborns=sum(vSexes==MALE)
        pop$stats$n.hiv.inc["0-4","MALE","HIV.NEG", "NCD.NEG",pop$params$YNOW]=pop$stats$n.hiv.inc["0-4","MALE","HIV.NEG","NCD.NEG",pop$params$YNOW] + nMaleNewborns
        pop$stats$n.hiv.inc["0-4","FEMALE","HIV.NEG","NCD.NEG",pop$params$YNOW]=pop$stats$n.hiv.inc["0-4","FEMALE","HIV.NEG","NCD.NEG",pop$params$YNOW] + (n.births.hiv-nMaleNewborns)
      }
      pop$stats$n.births[pop$params$YNOW]=n.births.non.hiv + n.births.hiv
    }
    
    pop$increaseMonth()
  }
  
  #### AT YEAR's END:
  ## --MODEL Deaths due to aging out --------
  pop$model.aging.out()
  
  ## -- UPDATE NCD STATES & CVD RISKS FOR NEXT YEAR --------
  pop<-update.ncd.states(pop)
  pop<-invisible(set.cvd.risk(pop))
  ##############################################
  
  # END OF YEAR----
  if (DEBUGMODE) cat("End of year: ",pop$params$CYNOW," --------------------------- \n")
  # Record annual statistics --------
  pop$record.annual.stats()
  pop$record.annual.ncd.trt.coverage()
  #Increment the clock
  pop$increaseYear()
  pop
}

##------------------------------------##
##-- MAIN INTERVENTION RUN FUNCTION --##
##------------------------------------##
# model one simulated year with intervention (from Jan 1st to Dec 31st)
run.one.year.int<-function(pop,
                           ncdScenario, 
                           int.start.year, # intervention start year
                           int.end.year,#intervention last year
                           pCoverage, # prop of community screened for ncd
                           pNcdTrtInitiation, # prop of ncd diag starting successful trt
                           pNcdTrtAdherence, # prop of ncd trt adhering to medication 
                           pDropOut=0, #prob of monthly dropout 
                           bAllowReenrollment=TRUE #allowing by default
){ 
  #### AT YEAR's BEGINNING: computing event probabilities from KHM
  khm<-compute.khm.probabilities(pop)
  
  ##### AT EACH TIMESTEP WITHIN THE YEAR:
  for(i in (1:ANNUAL.TIMESTEPS)){
    # print(pop$params$TNOW)
    
    # 1- modeling cvd events based on current cvd annual risk    # counting new events: marking those who will die after the event
    pop<-model.cvd.events(pop)
    
    # 2- modeling HIV transitions based on khm outputs
    pop<-model.hiv.transitions(pop,
                               khm$khm.prob.hiv.inc,
                               khm$khm.prob.hiv.eng,
                               khm$khm.prob.hiv.diseng,
                               khm$khm.prob.hiv.diag)
    
    # 3- modeling & removing HIV and CVD deaths
    pop<-model.hiv.cvd.deaths(pop,
                              khm$khm.monthly.hiv.mort,
                              khm$khm.monthly.non.hiv.mort)
    
    # MODEL INTERVENTION ------
    pop<-model.ncd.intervention(pop = pop,
                                ncdScenario = ncdScenario,
                                int.start.year=int.start.year,
                                int.end.year=int.end.year,
                                pCoverage = pCoverage,
                                pNcdTrtInitiation = pNcdTrtInitiation,
                                pNcdTrtAdherence = pNcdTrtAdherence,
                                pDropOut=pDropOut,
                                bAllowReenrollment=bAllowReenrollment)
    
    
    #4- MODEL AGING --------
    pop$modelAging()
    
    #5- MODEL BIRTHS -------
    {
      n.births.non.hiv=khm$n.births.non.hiv.per.month[i]
      n.births.hiv=khm$n.births.hiv.per.month[i]
      
      if (n.births.non.hiv>0){
        vIds = c((pop$params$LAST.PERSON.ID+1): (pop$params$LAST.PERSON.ID+n.births.non.hiv))
        pop$params$LAST.PERSON.ID=pop$params$LAST.PERSON.ID+n.births.non.hiv
        vSexes = sample(c(MALE,FEMALE),n.births.non.hiv,prob = c(.5,.5),replace = T) # still 50/50 male/female
        memberListNew = (mapply(PERSON$new, pop$params$TNOW,
                                vIds,
                                vSexes,
                                0,#age
                                HIV.NEG,
                                NCD.NEG)) 
        pop$addMembers(unlist(memberListNew))
        # record stats:
        pop$stats$n.births.non.hiv[pop$params$YNOW]=n.births.non.hiv
      }
      if (n.births.hiv>0){
        vIds = c((pop$params$LAST.PERSON.ID+1): (pop$params$LAST.PERSON.ID+n.births.hiv))
        pop$params$LAST.PERSON.ID=pop$params$LAST.PERSON.ID+n.births.hiv
        vSexes = sample(c(MALE,FEMALE),n.births.hiv,prob = c(.5,.5),replace = T)
        memberListNew = (mapply(PERSON$new,pop$params$TNOW,
                                vIds,
                                vSexes,
                                0,#age
                                HIV.UNDIAG,
                                NCD.NEG))  
        pop$addMembers(memberListNew)
        # record stats:
        pop$stats$n.births.hiv[pop$params$YNOW]=n.births.hiv
        nMaleNewborns=sum(vSexes==MALE)
        pop$stats$n.hiv.inc["0-4","MALE","HIV.NEG", "NCD.NEG",pop$params$YNOW]=pop$stats$n.hiv.inc["0-4","MALE","HIV.NEG","NCD.NEG",pop$params$YNOW] + nMaleNewborns
        pop$stats$n.hiv.inc["0-4","FEMALE","HIV.NEG","NCD.NEG",pop$params$YNOW]=pop$stats$n.hiv.inc["0-4","FEMALE","HIV.NEG","NCD.NEG",pop$params$YNOW] + (n.births.hiv-nMaleNewborns)
      }
      pop$stats$n.births[pop$params$YNOW]=n.births.non.hiv + n.births.hiv
    }
    
    pop$increaseMonth()
  }
  
  #### AT YEAR's END:
  ## --MODEL Deaths due to aging out --------
  pop$model.aging.out()
  
  ## -- UPDATE NCD STATES & CVD RISKS FOR NEXT YEAR --------
  pop<-update.ncd.states(pop)
  pop<-invisible(set.cvd.risk(pop))
  ##############################################
  
  # END OF YEAR----
  if (DEBUGMODE) cat("End of year: ",pop$params$CYNOW," --------------------------- \n")
  # Record annual statistics --------
  pop$record.annual.stats()
  pop$record.annual.ncd.trt.coverage()
  #Increment the clock
  pop$increaseYear()
  pop
}


##---------------------------------##
##-- HELPER INITIALIZE FUNCTIONS --##
##---------------------------------##

# sets the initial HIV status (based on HIV state prevalence in year 2015) 
set.initial.hiv.status = function(pop){
  #get hiv state proportions by age and sex
  hiv.probs = transform.hiv.data.1d.to.3d(pop$params$khm.hivPrev2015)
  # dim(hiv.probs)
  
  invisible(lapply(pop$members,function(p){
    p.probs = hiv.probs[,p$agegroup,p$sex]
    # break to ensure that sum of p.probs==1
    if(round(sum(p.probs),0)!=1){
      stop(paste0("Error: p.probs for: age ",dimnames(hiv.probs)[[2]][p$agegroup],
                  ", sex ",dimnames(hiv.probs)[[3]][p$sex], " does not sum to 1"))
    }
    #randomly assign HIV state based on probs
    rand.hiv.state = sample(x = c(1:length(p.probs)), size = 1, prob = p.probs)
    p$hivState=rand.hiv.state
  }))
  
  if(DEBUGMODE) cat("Initial HIV status set \n")
  pop
}

# sets the monthly CVD risk 
set.cvd.risk = function(pop){
  invisible(lapply(pop$members,function(p){
    # for whatever age group they are in, access the 10-year risk for the previous age group 
    p.agegroup=pmax(1, p$agegroup-1 )# for the youngest age group, just make this 1, not 0
    
    p$monthlyCvdRisk= pop$params$monthly.cvd.risk.by.age.sex[p$agegroup,p$sex,p$hivState,p$ncdState]
  }))
  pop
}

# sets initial ncd treatment coverage (7%)
initialize.ncd.treatment = function(pop){
  baselineNcdIds=c() # vector of Ids for those eligible for baseline treatment
  invisible(lapply(c(1:length(pop$members)),function(x){
    p=pop$members[[x]]
    if (p$ncdState > 1){ # "NCD.DIAB","NCD.HYP","NCD.DIAB_HYP"
      baselineNcdIds<<-cbind(baselineNcdIds,x)
    }}))
  
  nToScreen.baseline = round((BASELINE.VALUES$coverage * length(baselineNcdIds))) # BASELINE.VALUES$coverage is 7%; so 7% of people with NCDs are treated
  selectedIds.baseline=sample(size = nToScreen.baseline,x = baselineNcdIds,replace = F ) #choose a random set from baseline with NCDs 
  
  invisible(lapply(selectedIds.baseline,function(x){
    p=pop$members[[x]]
    tnow=pop$params$TNOW
    
    # NCD screening & treatment initiation (combined at baseline), and treatment adherence 
    if(p$ncdState==NCD.DIAB){
      pop$record.diab.diag(p$agegroup,p$sex,p$hivState,p$ncdState)
      p$model.diab.diag(tnow)
      
      if(runif(1) < BASELINE.VALUES$adherence){
        pop$record.diab.trt.adherence(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$start.diab.trt.adherence(tnow)
      } else {
        pop$record.diab.trt(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$start.diab.trt(tnow)
      }
    }
    
    if(p$ncdState==NCD.HYP){
      pop$record.hyp.diag(p$agegroup,p$sex,p$hivState,p$ncdState)
      p$model.hyp.diag(tnow)
      
      if(runif(1) < BASELINE.VALUES$adherence){
        pop$record.hyp.trt.adherence(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$start.hyp.trt.adherence(tnow)
      } else {
        pop$record.hyp.trt(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$start.hyp.trt(tnow)  
      }
    }
    
    if(p$ncdState==NCD.DIAB_HYP){
      pop$record.diab.hyp.diag(p$agegroup,p$sex,p$hivState,p$ncdState)
      p$model.diab.hyp.diag(tnow)
      
      if(runif(1) < BASELINE.VALUES$adherence){
        pop$record.diab.hyp.trt.adherence(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$start.diab.hyp.trt.adherence(tnow)
      } else {
        pop$record.diab.hyp.trt(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$start.diab.hyp.trt(tnow)
      }
    }
  }))
  pop
}


##--------------------------##
##-- HELPER RUN FUNCTIONS --##
##--------------------------##

# baseline scenario function
model.ncd.baseline = function(pop,
                              p.monthly.baseline.enrollment,
                              p.monthly.baseline.dropout){
  
  # model drop outs
  invisible(lapply(pop$members,function(p){
    if(p$ncdState>4) {
      if (runif(1)<p.monthly.baseline.dropout){
        #record stats
        if (p$ncdState %in% c(NCD.DIAB.TRT,NCD.DIAB.TRT.ADH)) pop$record.diab.trt.dropout(p$agegroup,p$sex,p$hivState,p$ncdState)
        if (p$ncdState %in% c(NCD.HYP.TRT,NCD.HYP.TRT.ADH)) pop$record.hyp.trt.dropout(p$agegroup,p$sex,p$hivState,p$ncdState)
        if (p$ncdState %in% c(NCD.DIAB_HYP.TRT,NCD.DIAB_HYP.TRT.ADH)) pop$record.diab.hyp.trt.dropout(p$agegroup,p$sex,p$hivState,p$ncdState)
        #model dropout
        p$model.ncd.trt.dropout()
      }}
    
  }))
  
  # loop over population, new enrollments (screening & treatment combined at baseline; treatment adherence)
  invisible(lapply(pop$members,function(p){
    tnow=pop$params$TNOW
    if(runif(1) < p.monthly.baseline.enrollment){ 
      if(p$ncdState==NCD.DIAB){
        #pop$record.diab.diag(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$model.diab.diag(tnow)
        
        if(runif(1) < BASELINE.VALUES$adherence){
          pop$record.diab.trt.adherence(p$agegroup,p$sex,p$hivState,p$ncdState)
          p$start.diab.trt.adherence(tnow)
        } else {
          pop$record.diab.trt(p$agegroup,p$sex,p$hivState,p$ncdState)
          p$start.diab.trt(tnow)
        }
      }
      
      if(p$ncdState==NCD.HYP){
        #pop$record.hyp.diag(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$model.hyp.diag(tnow)
        
        if(runif(1) < BASELINE.VALUES$adherence){
          pop$record.hyp.trt.adherence(p$agegroup,p$sex,p$hivState,p$ncdState)
          p$start.hyp.trt.adherence(tnow)
        } else {
          pop$record.hyp.trt(p$agegroup,p$sex,p$hivState,p$ncdState)
          p$start.hyp.trt(tnow)  
        }
      }
      
      if(p$ncdState==NCD.DIAB_HYP){
        #pop$record.diab.hyp.diag(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$model.diab.hyp.diag(tnow)
        
        if(runif(1) < BASELINE.VALUES$adherence){
          pop$record.diab.hyp.trt.adherence(p$agegroup,p$sex,p$hivState,p$ncdState)
          p$start.diab.hyp.trt.adherence(tnow)
        } else {
          pop$record.diab.hyp.trt(p$agegroup,p$sex,p$hivState,p$ncdState)
          p$start.diab.hyp.trt(tnow)
        }
      }
    }
  })) 
 
  pop
}

# intervention scenario function 
model.ncd.intervention<-function(pop,
                                 ncdScenario=0,
                                 int.start.year, # intervention start year
                                 int.end.year,#intervention last year
                                 pCoverage=0, # prop of community screened for ncd
                                 pNcdTrtInitiation=0, # ONLY represents treatment uptake (not necessarily adherent)
                                 pNcdTrtAdherence=0, # represents medication adherence among those initiated on treatment 
                                 pDropOut=0, #prob of ncd trt discontinuation
                                 bAllowReenrollment=TRUE #allowing by default
){
  
  if(ncdScenarios[[ncdScenario]]$id!=1){ 
    #has the intervention begun?
    if(pop$params$CYNOW >= int.start.year) {
      
      # model drop outs
      invisible(lapply(pop$members,function(p){
        if(p$ncdState>4) {
          if (runif(1)<pDropOut){
            #record stats
            if (p$ncdState %in% c(NCD.DIAB.TRT,NCD.DIAB.TRT.ADH)) pop$record.diab.trt.dropout(p$agegroup,p$sex,p$hivState,p$ncdState)
            if (p$ncdState %in% c(NCD.HYP.TRT,NCD.HYP.TRT.ADH)) pop$record.hyp.trt.dropout(p$agegroup,p$sex,p$hivState,p$ncdState)
            if (p$ncdState %in% c(NCD.DIAB_HYP.TRT,NCD.DIAB_HYP.TRT.ADH)) pop$record.diab.hyp.trt.dropout(p$agegroup,p$sex,p$hivState,p$ncdState)
            #model dropout
            p$model.ncd.trt.dropout()
          }}
        
      }))
      
      # model new enrollments
      if(pop$params$CYNOW <= int.end.year){
        selectedIds.clinic=NULL
        selectedIds.community=NULL
        
        # intervention focused at HIV clinic (among HIV.ENG)
        if ("clinic" %in% ncdScenarios[[ncdScenario]]$location) { 
          
          vIds=c() # vector of Ids for those eligible to receive the intervention 
          invisible(lapply(c(1:length(pop$members)),function(x){
            p=pop$members[[x]]
            if (p$hivState==HIV.ENG){
              vIds<<-cbind(vIds,x)
            }}))
          
          nToScreen.clinic = round(pCoverage * length(vIds)) #estimate the number of people that will be screened
          selectedIds.clinic=sample(size = nToScreen.clinic,x = vIds,replace = F ) #choose a random set from the clinic
        }
        
        #intervention focused at the community level
        else { 
          nToScreen.community = round(pCoverage * length(pop$members)) #estimate the number of people that will be screened
          selectedIds.community=sample(size = nToScreen.community,x = length(pop$members),replace = F ) #choose a random set from the community
        }
        
        selectedIds = unique(c(selectedIds.clinic,selectedIds.community))
        
        #loop over those selected
        invisible(lapply(selectedIds,function(x){
          p=pop$members[[x]]
          #should exclude those previously screened, with existing diag, or currently on trt?
          # if (bAllowReenrollment)  continue #'@MS: we are not using this parameter for now
          
          pop$record.ncd.screening(p$agegroup,p$sex,p$hivState,p$ncdState)
          tnow=pop$params$TNOW
          
          # NCD screening, treatment initiation, and treatment adherence 
          if(p$ncdState==NCD.DIAB){
            pop$record.diab.diag(p$agegroup,p$sex,p$hivState,p$ncdState)
            p$model.diab.diag(tnow)
            
            if(runif(1) < pNcdTrtInitiation){
              if(runif(1) < pNcdTrtAdherence){
                pop$record.diab.trt.adherence(p$agegroup,p$sex,p$hivState,p$ncdState)
                p$start.diab.trt.adherence(tnow)
              } else {
                pop$record.diab.trt(p$agegroup,p$sex,p$hivState,p$ncdState)
                p$start.diab.trt(tnow)
              }
            }}
          
          if(p$ncdState==NCD.HYP){
            pop$record.hyp.diag(p$agegroup,p$sex,p$hivState,p$ncdState)
            p$model.hyp.diag(tnow)
            
            if(runif(1) < pNcdTrtInitiation){
              if(runif(1) < pNcdTrtAdherence){
                pop$record.hyp.trt.adherence(p$agegroup,p$sex,p$hivState,p$ncdState)
                p$start.hyp.trt.adherence(tnow)
              } else {
                pop$record.hyp.trt(p$agegroup,p$sex,p$hivState,p$ncdState)
                p$start.hyp.trt(tnow)  
              }
            }}
          
          if(p$ncdState==NCD.DIAB_HYP){
            pop$record.diab.hyp.diag(p$agegroup,p$sex,p$hivState,p$ncdState)
            p$model.diab.hyp.diag(tnow)
            
            if(runif(1) < pNcdTrtInitiation){
              if(runif(1) < pNcdTrtAdherence){
                pop$record.diab.hyp.trt.adherence(p$agegroup,p$sex,p$hivState,p$ncdState)
                p$start.diab.hyp.trt.adherence(tnow)
              } else {
                pop$record.diab.hyp.trt(p$agegroup,p$sex,p$hivState,p$ncdState)
                p$start.diab.hyp.trt(tnow)
              }
            }}
        }))
      }
    }
  }
  
  pop
}

# compute event probabilities from KHM
compute.khm.probabilities = function(pop){
  khm=pop$params$khm
  rv = list() # initialize list we are going to return 
  
  ###computing prob of death
  { 
    ##Probability of HIV mortality (estimated via # events/ elig pop) --------
    n.hiv.pos = apply(khm$population[as.character(pop$params$CYNOW-1),-1,,],c(2:3),sum) # extract all but hiv.negative, sum over hiv states
    target.hiv.mort = khm$hiv.mortality[as.character(pop$params$CYNOW),,] # pull out current year; dimensions are year, age, sex
    n.hiv.pos.now = apply(khm$population[as.character(pop$params$CYNOW),-1,,],c(2:3),sum)
    #we are using average to approximate the integral
    # prob.hiv.mort=target.hiv.mort/((n.hiv.pos + n.hiv.pos.now)/2) 
    prob.hiv.mort=target.hiv.mort/(n.hiv.pos.now+target.hiv.mort)
    if(sum(prob.hiv.mort>1)>1)
      stop(paste("Error: probability of prob.hiv.mort >1 in year ",pop$params$CYNOW))
    prob.hiv.mort[prob.hiv.mort==Inf]<-0
    khm.annual.hiv.mort=prob.hiv.mort
    khm.monthly.hiv.mort=(1-(1-prob.hiv.mort)^(1/12))
    rv$khm.monthly.hiv.mort = khm.monthly.hiv.mort # add to list we are going to return 
    
    ##Probability of non.HIV mortality (estimated via # events/ elig pop) --------
    n.pop = apply(khm$population[as.character(pop$params$CYNOW-1),,,],c(2:3),sum) # extract all population, sum over hiv states
    target.non.hiv.mort = khm$non.hiv.mortality[as.character(pop$params$CYNOW),,] # pull out current year; dimensions are year, age, sex
    n.pop.now = apply(khm$population[as.character(pop$params$CYNOW),,,],c(2:3),sum)
    #we are using average to approximate the integral
    # prob.non.hiv.mort=target.non.hiv.mort/((n.pop + n.pop.now)/2) # denominator is now population as well as aging in - aging out 
    prob.non.hiv.mort=target.non.hiv.mort/( n.pop.now+target.non.hiv.mort)
    if(sum(prob.non.hiv.mort>1)>1){
      browser()
      stop(paste("Error: probability of prob.non.hiv.mort >1 in year ",pop$params$CYNOW))
    }
    prob.non.hiv.mort[prob.non.hiv.mort==Inf]<-0
    khm.annual.non.hiv.mort=prob.non.hiv.mort
    khm.monthly.non.hiv.mort=(1-(1-prob.non.hiv.mort)^(1/12))
    rv$khm.monthly.non.hiv.mort = khm.monthly.non.hiv.mort
    
  }
  ###computing number of births (and distributing them over 12 months)
  {
    # non-HIV births
    absolute.n.births.non.hiv=khm$target.parameters$non.hiv.births[as.character(pop$params$CYNOW)] # total non HIV births from HIV model
    non.hiv.births.scalar = absolute.n.births.non.hiv/sum(khm$population[as.character(pop$params$CYNOW),,,]) # scaled to pop size from HIV model
    n.births.non.hiv = round(non.hiv.births.scalar*length(pop$members),0) # re-scaled to pop size from NCD model
    #distribute births over months
    n.births.non.hiv.per.month=rep(0,ANNUAL.TIMESTEPS)
    if(n.births.non.hiv>0)
      n.births.non.hiv.per.month=as.vector(tabulate(sample(size = n.births.non.hiv,x = c(1:ANNUAL.TIMESTEPS),replace = T),nbins = 12))
    rv$n.births.non.hiv.per.month = n.births.non.hiv.per.month
    
    # HIV births - putting into undiagnosed for now
    absolute.n.births.hiv=khm$target.parameters$hiv.births[as.character(pop$params$CYNOW)] # total HIV births from HIV model
    hiv.births.scalar = absolute.n.births.hiv/sum(khm$population[as.character(pop$params$CYNOW),-1,,]) # scaled to HIV pop size from HIV model
    n.births.hiv = round(hiv.births.scalar*sum(pop$return.state.size.distribution()[,,-1,,]),0) # re-scaled to HIV pop size from NCD mode
    #distribute births over months
    n.births.hiv.per.month=rep(0,ANNUAL.TIMESTEPS)
    if(n.births.hiv>0)
      n.births.hiv.per.month=as.vector(tabulate(sample(size = n.births.hiv,x = c(1:ANNUAL.TIMESTEPS),replace = T),nbins = 12))
    rv$n.births.hiv.per.month = n.births.hiv.per.month
    
  }
  ###computing care cascade prob of events
  {
    ##Probability of incidence (estimated via # events/ elig pop) --------
    n.hiv.neg = khm$population[as.character(pop$params$CYNOW-1),"HIV.NEG",,]
    target.inc = khm$incidence[as.character(pop$params$CYNOW),,] # pull out current year; dimensions are year, age, sex
    n.hiv.neg.now = khm$population[as.character(pop$params$CYNOW),"HIV.NEG",,]
    delta = n.hiv.neg.now - n.hiv.neg # change in hiv negative population (aging in - aging out - deaths - hiv incidence)
    delta = delta + target.inc
    #'@MS: maybe change later?
    prob.inc=target.inc/(n.hiv.neg + delta)
    if(sum(prob.inc>1)>1)
      stop(paste("Error: probability of prob.inc >1 in year ",pop$params$CYNOW))
    prob.inc[prob.inc==Inf]<-0
    khm.prob.hiv.inc=(1-(1-prob.inc)^(1/12))
    rv$khm.prob.hiv.inc = khm.prob.hiv.inc
    
    ##Probability of engagement (direct input to HIV model) --------
    prob.eng=khm$target.parameters$prob.eng[as.character(pop$params$CYNOW),,]
    if(sum(prob.eng>1)>1)
      stop(paste("Error: probability of engagement >1 in year ",pop$params$CYNOW))
    prob.eng[prob.eng==Inf]<-0
    khm.prob.hiv.eng=(1-(1-prob.eng)^(1/12))
    rv$khm.prob.hiv.eng = khm.prob.hiv.eng
    
    ##Probability of disengagement (direct input to HIV model) --------
    prob.diseng=khm$target.parameters$prob.diseng[as.character(pop$params$CYNOW),,]
    if(sum(prob.diseng>1)>1)
      stop(paste("Error: probability of disengagement >1 in year ",pop$params$CYNOW))
    prob.diseng[prob.diseng==Inf]<-0
    khm.prob.hiv.diseng=(1-(1-prob.diseng)^(1/12))
    rv$khm.prob.hiv.diseng = khm.prob.hiv.diseng
    
    ##Probability of diagnosis (direct input to HIV model) --------
    prob.diag = khm$target.parameters$prob.diag[as.character(pop$params$CYNOW),,]    
    if(sum(prob.diag>1)>1)
      stop(paste("Error: probability of prob.diag >1 in year ",pop$params$CYNOW))
    prob.diag[prob.diag==Inf]<-0
    khm.prob.hiv.diag=(1-(1-prob.diag)^(1/12))
    rv$khm.prob.hiv.diag = khm.prob.hiv.diag
  }
  
  rv
  
}

# models the CVD events
model.cvd.events<-function(pop){
  invisible(lapply(pop$members,function(p){
    p.cvd.risk = p$return.cvd.risk(pop$params) # this function evaluates whether they have history of cvd events and returns appropriate risk 
    
    if(runif(1) < p.cvd.risk){ # evaluate if they have a cvd event 
      # evaluate whether this should be a stroke event or mi event (assign default male probability, change to female if sex is female)
      prob.mi=pop$params$prob.first.cvd.event.mi.male
      if(p$sex==FEMALE) prob.mi=pop$params$prob.first.cvd.event.mi.female
      
      if(runif(1) < prob.mi){ # mi event
        pop$record.mi.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$model.mi.event(pop$params$TNOW)
      } else{ # stroke event
        pop$record.stroke.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$model.stroke.event(pop$params$TNOW)
      }}}))
  pop
}

# models the HIV transitions 
model.hiv.transitions<-function(pop,
                                prob.inc,
                                prob.eng,
                                prob.diseng,
                                prob.diag){
  invisible(lapply(pop$members,function(p){
    #1-ENGAGEMENT
    if (p$hivState == HIV.UNENG) {
      if (runif(1) < prob.eng[p$agegroup,p$sex]){
        pop$record.hiv.eng(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$model.hiv.eng(pop$params$TNOW)
      }
    }else{      #2- DISENGAGEMENT
      if (p$hivState== HIV.ENG) {
        if (runif(1)<prob.diseng[p$agegroup,p$sex]){
          pop$record.hiv.uneng(p$agegroup,p$sex,p$hivState,p$ncdState)
          p$model.hiv.uneng(pop$params$TNOW)
        }
      }else{        #3- DIAGNOSIS
        if (p$hivState== HIV.UNDIAG) {
          if (runif(1)<prob.diag[p$agegroup,p$sex]){
            pop$record.hiv.diag(p$agegroup,p$sex,p$hivState,p$ncdState)
            p$model.hiv.diag(pop$params$TNOW)
          }
        }else{          #4- INCIDENCE
          if (p$hivState== HIV.NEG) {
            if (runif(1)<prob.inc[p$agegroup,p$sex]){
              pop$record.hiv.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
              p$model.hiv.inc(pop$params$TNOW)
            }
          }else {
            browser()
            stop(paste("Error: Person ",x," hivState is ",p$hivState," and it didnt meet any criteria"))
          }}}}}))
  pop
}

# removes the HIV & CVD deaths
model.hiv.cvd.deaths<-function(pop,
                               prob.hiv.mort,
                               prob.non.hiv.mort){
  
  invisible(lapply(pop$members,function(p){
    #model CVD death
    if(runif(1) < p$return.cvd.mortality(pop$params))
      p$bMarkedDead.cvd=T
    
    if (p$bMarkedDead.cvd==FALSE){
      # HIV MORTALITY
      if(p$hivState!=HIV.NEG){ # all HIV positive
        if(runif(1)<prob.hiv.mort[p$agegroup,p$sex])
          p$bMarkedDead.hiv=T
      }
      # NON.HIV MORTALITY 
      if(runif(1)< prob.non.hiv.mort[p$agegroup,p$sex])
        p$bMarkedDead.non.hiv=T
    }}))
  
  # removing deaths & saving stats
  { 
    death.ids=NULL
    n.cvd.deaths=0
    n.hiv.deaths=0
    n.nonHiv.deaths=0
    lapply(1:length(pop$members),function(x){
      p=pop$members[[x]]
      if (p$bMarkedDead.cvd==TRUE) {
        n.cvd.deaths<<-n.cvd.deaths+1
        death.ids<<-c(death.ids,x)
        
        pop$record.deaths.cvd(p$agegroup,p$sex,p$hivState,p$ncdState)
      }else{
        if (p$bMarkedDead.hiv ==TRUE) {
          n.hiv.deaths<<-n.hiv.deaths+1
          death.ids<<-c(death.ids,x)
          pop$record.deaths.hiv(p$agegroup,p$sex,p$hivState,p$ncdState)
        }else{
          if (p$bMarkedDead.non.hiv ==TRUE) {
            n.nonHiv.deaths<<-n.nonHiv.deaths+1
            death.ids<<-c(death.ids,x)
            pop$record.deaths.non.hiv(p$agegroup,p$sex,p$hivState,p$ncdState)
            
          }}}})
    
    #removing dead people:
    if(!is.null(death.ids))
      pop$members<-pop$members[-death.ids]
    
    
  }
  pop
}

# update NCD state (based on hiv and annual population growth)
update.ncd.states<-function(pop){ 
  #new parameters to incorporate:
  #1-additional risk of ncds by hiv status (can be a single value or different based on engagement)
  # MP$relative.ncd.risk.by.hiv=1 
  #2-annual growth in age/sex-specific prev of ncds
  # MP$annual.growth.ncd.prev=1
  
  #we are merging those on ncd trt with those who are untreated for the purpose of modeling new events
  # therefore reducing ncd dimension from 7 to 4
  dim.ncd.states=4 # we have 4 eligible ncd states that we are using to model ncd events 
  
  # TARGET PREV OF NCDs BASED ON 2015, HIV STATUS AND ANNUAL GROWTH
  target.ncd.props=pop$params$target.ncd.props 
  
  #1-multiply by HIV coef
  target.ncd.props[,,c("HIV.UNDIAG", "HIV.UNENG" , "HIV.ENG"),]=pop$params$relative.ncd.risk.by.hiv* target.ncd.props[,,c("HIV.UNDIAG", "HIV.UNENG" , "HIV.ENG"),]
  
  #2-multiply by rhe annual growth factor:
  target.ncd.props= pop$params$annual.growth.ncd.prev^(pop$params$CYNOW-INITIAL.YEAR)  * target.ncd.props
  
  #removing trt states for ncds
  target.ncd.props<-target.ncd.props[,,,c(1:4)]
  dimnames(target.ncd.props)
  #'@MS: we will return to this to figure out a way to distribute risk by age/sex dist
  
  
  
  # DIAB HYP #######################################
  # CURRENT NCD state sizes & prop
  current.ncd.states = filter.5D.stats.by.field(pop$return.state.size.distribution(),
                                                years = as.character(pop$params$CYNOW),
                                                keep.dimensions = c('age','sex','hiv.status','ncd.status','year'))[,,,,1]#to remove year dimension
  #merging NCD trt into untreated states for the purpose of modeling new events
  current.ncd.states[,,,"NCD.DIAB"]<-current.ncd.states[,,,"NCD.DIAB"]+current.ncd.states[,,,"NCD.DIAB.TRT"]+current.ncd.states[,,,"NCD.DIAB.TRT.ADH"]
  current.ncd.states[,,,"NCD.HYP"]<-current.ncd.states[,,,"NCD.HYP"]+current.ncd.states[,,,"NCD.HYP.TRT"]+current.ncd.states[,,,"NCD.HYP.TRT.ADH"]
  current.ncd.states[,,,"NCD.DIAB_HYP"]<-current.ncd.states[,,,"NCD.DIAB_HYP"]+current.ncd.states[,,,"NCD.DIAB_HYP.TRT"]+current.ncd.states[,,,"NCD.DIAB_HYP.TRT.ADH"]
  current.ncd.states<-current.ncd.states[,,,c(1:4)]
  
  current.ncd.props<-return.prop.sex.age(vFreq = current.ncd.states)
  # dimnames(current.ncd.props)
  # dimnames(current.ncd.props);dimnames(current.ncd.states)
  
  diff.props =  target.ncd.props-current.ncd.props
  
  # ADDITIONAL Transitions required to reach the target proportions in our current population:
  trans.freq=diff.props
  
  invisible(lapply(1:DIM.AGE, function(ag){
    lapply(1:DIM.SEX, function(sex){
      lapply(1:DIM.HIV, function(hiv){
        lapply(1:dim.ncd.states, function(ncd){
          trans.freq[ag,sex,hiv,ncd]<<-diff.props[ag,sex,hiv,ncd]*sum(current.ncd.states[ag,sex,hiv,]) # the required number of new transitions
        })})})}))
  trans.freq[trans.freq<0]<-0
  
  #PROBABILITY Of transition to DH for those in D or H state
  trans.prob.diab.hyp=
    trans.freq[,,,"NCD.DIAB_HYP"]/(current.ncd.states[,,,"NCD.DIAB"]+current.ncd.states[,,,"NCD.HYP"] )
  #'@MS: should we allow those on trt to develop multimorbidity? what would be their new trt status?
  #'I think that we can assume if someone is on "effective" trt for one ncd, they will receive a timely diagnosis 
  #'for other ncd as well and we can  move them from diab.trt for example to diab.hyp.trt?
  #'
  # trans.prob.diab.hyp=
  #   trans.freq[,,,"NCD.DIAB_HYP"]/(current.ncd.states[,,,"NCD.DIAB"]+current.ncd.states[,,,"NCD.HYP"]+
  #                                    current.ncd.states[,,,"NCD.DIAB.TRT"]+current.ncd.states[,,,"NCD.HYP.TRT"])
  trans.prob.diab.hyp[is.na(trans.prob.diab.hyp) | trans.prob.diab.hyp==Inf]<-0
  
  # TRANSITION to DH from D or H
  invisible(lapply(pop$members,function(p){
    #if(p$ncdState==NCD.DIAB || p$ncdState==NCD.HYP ||p$ncdState==NCD.DIAB.TRT || p$ncdState==NCD.HYP.TRT){
    if(p$ncdState %in% c("NCD.DIAB","NCD.HYP","NCD.DIAB.TRT","NCD.HYP.TRT","NCD.DIAB.TRT.ADH","NCD.HYP.TRT.ADH")){
      if(runif(1) < trans.prob.diab.hyp[p$agegroup,p$sex,p$hivState]){
        pop$record.diab.hyp.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$model.diab.hyp.inc(pop$params$TNOW)
        #print("new incident diab hyp case")
      }}}))
  
  # DIAB #######################################
  current.ncd.states = filter.5D.stats.by.field(pop$return.state.size.distribution(),
                                                years = as.character(pop$params$CYNOW),
                                                keep.dimensions = c('age','sex','hiv.status','ncd.status','year'))[,,,,1]#to remove year dimension
  #merging NCD trt into untreated states for the purpose of modeling new events
  current.ncd.states[,,,"NCD.DIAB"]<-current.ncd.states[,,,"NCD.DIAB"]+current.ncd.states[,,,"NCD.DIAB.TRT"]+current.ncd.states[,,,"NCD.DIAB.TRT.ADH"]
  current.ncd.states[,,,"NCD.HYP"]<-current.ncd.states[,,,"NCD.HYP"]+current.ncd.states[,,,"NCD.HYP.TRT"]+current.ncd.states[,,,"NCD.HYP.TRT.ADH"]
  current.ncd.states[,,,"NCD.DIAB_HYP"]<-current.ncd.states[,,,"NCD.DIAB_HYP"]+current.ncd.states[,,,"NCD.DIAB_HYP.TRT"]+current.ncd.states[,,,"NCD.DIAB_HYP.TRT.ADH"]
  current.ncd.states<-current.ncd.states[,,,c(1:4)]
  
  current.ncd.props<-return.prop.sex.age(vFreq = current.ncd.states)
  
  diff.props =  target.ncd.props-current.ncd.props
  
  # ADDITIONAL Transitions required to reach the target proportions in our current population:
  trans.freq=diff.props
  invisible(lapply(1:DIM.AGE, function(ag){
    lapply(1:DIM.SEX, function(sex){
      lapply(1:DIM.HIV, function(hiv){
        lapply(1:dim.ncd.states, function(ncd){
          trans.freq[ag,sex,hiv,ncd]<<-diff.props[ag,sex,hiv,ncd]*sum(current.ncd.states[ag,sex,hiv,]) # the required number of new transitions
        })})})}))
  trans.freq[trans.freq<0]<-0
  
  #PROBABILITY Of transition to DIAB for those in NCD.NEG
  trans.prob.diab= trans.freq[,,,"NCD.DIAB"]/(current.ncd.states[,,,"NCD.NEG"])
  trans.prob.diab[is.na(trans.prob.diab) | trans.prob.diab==Inf]<-0
  
  # TRANSITION to D from neg
  invisible(lapply(c(1:length(pop$members)),function(x){
    p=pop$members[[x]]
    if(p$ncdState==NCD.NEG){
      if(runif(1)<trans.prob.diab[p$agegroup,p$sex,p$hivState]){
        pop$record.diab.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$model.diab.inc(pop$params$TNOW)
        #print("new incident diab case")
      }}}))
  
  # HYP #############################################################################
  current.ncd.states = filter.5D.stats.by.field(pop$return.state.size.distribution(),
                                                years = as.character(pop$params$CYNOW),
                                                keep.dimensions = c('age','sex','hiv.status','ncd.status','year'))[,,,,1]#to remove year dimension
  #merging NCD trt into untreated states for the purpose of modeling new events
  current.ncd.states[,,,"NCD.DIAB"]<-current.ncd.states[,,,"NCD.DIAB"]+current.ncd.states[,,,"NCD.DIAB.TRT"]+current.ncd.states[,,,"NCD.DIAB.TRT.ADH"]
  current.ncd.states[,,,"NCD.HYP"]<-current.ncd.states[,,,"NCD.HYP"]+current.ncd.states[,,,"NCD.HYP.TRT"]+current.ncd.states[,,,"NCD.HYP.TRT.ADH"]
  current.ncd.states[,,,"NCD.DIAB_HYP"]<-current.ncd.states[,,,"NCD.DIAB_HYP"]+current.ncd.states[,,,"NCD.DIAB_HYP.TRT"]+current.ncd.states[,,,"NCD.DIAB_HYP.TRT.ADH"]
  current.ncd.states<-current.ncd.states[,,,c(1:4)]
  
  current.ncd.props<-return.prop.sex.age(vFreq = current.ncd.states)
  
  diff.props =  target.ncd.props-current.ncd.props
  
  # ADDITIONAL Transitions required to reach the target proportions in our current population:
  trans.freq=diff.props
  invisible(lapply(1:DIM.AGE, function(ag){
    lapply(1:DIM.SEX, function(sex){
      lapply(1:DIM.HIV, function(hiv){
        lapply(1:dim.ncd.states, function(ncd){
          trans.freq[ag,sex,hiv,ncd]<<-diff.props[ag,sex,hiv,ncd]*sum(current.ncd.states[ag,sex,hiv,]) # the required number of new transitions
        })})})}))
  trans.freq[trans.freq<0]<-0
  
  #PROBABILITY Of transition to HYP for those in NCD.NEG
  trans.prob.hyp= trans.freq[,,,"NCD.HYP"]/(current.ncd.states[,,,"NCD.NEG"])
  trans.prob.hyp[is.na(trans.prob.hyp) | trans.prob.hyp==Inf]<-0
  
  # TRANSITION to H from neg
  invisible(lapply(c(1:length(pop$members)),function(x){
    p=pop$members[[x]]
    if(p$ncdState==NCD.NEG) {
      if(runif(1)< trans.prob.hyp[p$agegroup,p$sex,p$hivState]){
        pop$record.hyp.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$model.hyp.inc(pop$params$TNOW)
        #print("new incident hyp case")
      }}}))
  
  pop
}




## Old tuning code
if(1==2){
  
  # stats aren't recorded until the end of the year, need to pull it here to get monthly values (can't do pop$stats$n.state.sizes)
  n.state.sizes = pop$return.state.size.distribution() # monthly state sizes
  
  # re-calculate coverage
  n.ncd = sum(n.state.sizes[,,,c("NCD.DIAB","NCD.HYP","NCD.DIAB_HYP"),
                            as.character(pop$params$CYNOW)]) # total number of people with NCDs
  n.ncd.trt = sum(n.state.sizes[,,,c("NCD.DIAB.TRT","NCD.HYP.TRT","NCD.DIAB_HYP.TRT",
                                     "NCD.DIAB.TRT.ADH","NCD.HYP.TRT.ADH","NCD.DIAB_HYP.TRT.ADH"),
                                as.character(pop$params$CYNOW)]) # total number on treatment (adherent or not)
  n.ncd.untreated = n.ncd - n.ncd.trt # number not on treatment 
  
  current.ncd.trt.coverage = n.ncd.trt/n.ncd
  target.ncd.trt.coverage = pCoverage # 7% 
  
  # IF CURRENT TREATMENT GREATER THAN TARGET, model DROPOUTS with probability of the difference between the two 
  if(current.ncd.trt.coverage>target.ncd.trt.coverage){
    
    p.dropout.total.ncd = current.ncd.trt.coverage-target.ncd.trt.coverage # % of those with NCDs needed to drop out 
    dropout.freq = (p.dropout.total.ncd*n.ncd) # need this number to drop out PER MONTH 
    p.dropout.treated.ncd = dropout.freq/n.ncd.trt # need this % of those on treatment to drop out PER MONTH
    
    print(paste0(dropout.freq," needed to drop out monthly to maintain 7% baseline coverage"))
    invisible(lapply(pop$members,function(p){
      if(p$ncdState>4) { # only those on treatment 
        if (runif(1)<(p.dropout.treated.ncd)){
          #record stats
          if (p$ncdState %in% c(NCD.DIAB.TRT,NCD.DIAB.TRT.ADH)) pop$record.diab.trt.dropout(p$agegroup,p$sex,p$hivState,p$ncdState)
          if (p$ncdState %in% c(NCD.HYP.TRT,NCD.HYP.TRT.ADH)) pop$record.hyp.trt.dropout(p$agegroup,p$sex,p$hivState,p$ncdState)
          if (p$ncdState %in% c(NCD.DIAB_HYP.TRT,NCD.DIAB_HYP.TRT.ADH)) pop$record.diab.hyp.trt.dropout(p$agegroup,p$sex,p$hivState,p$ncdState)
          #model dropout
          p$model.ncd.trt.dropout()
        }}}))
    
  }
  
  # IF CURRENT TREATMENT LESS THAN TARGET, model NEW ENROLLMENTS with probability of the difference between the two 
  if(current.ncd.trt.coverage<target.ncd.trt.coverage){
    
    p.enroll.total.ncd = target.ncd.trt.coverage-current.ncd.trt.coverage # % of those with NCDs needed to enroll 
    enroll.freq = (p.enroll.total.ncd*n.ncd) # need this number to enroll PER MONTH
    p.enroll.untreated.ncd = enroll.freq/n.ncd.untreated # need this % of those NOT on treatment to enroll
    
    print(paste0(enroll.freq," needed to enroll monthly to maintain 7% baseline coverage"))
    invisible(lapply(pop$members,function(p){
      
      tnow=pop$params$TNOW
      
      # NCD screening & treatment initiation (combined at baseline), and treatment adherence 
      if(runif(1) < p.enroll.untreated.ncd){ 
        if(p$ncdState==NCD.DIAB){
          pop$record.diab.diag(p$agegroup,p$sex,p$hivState,p$ncdState)
          p$model.diab.diag(tnow)
          
          if(runif(1) < pNcdTrtAdherence){
            pop$record.diab.trt.adherence(p$agegroup,p$sex,p$hivState,p$ncdState)
            p$start.diab.trt.adherence(tnow)
          } else {
            pop$record.diab.trt(p$agegroup,p$sex,p$hivState,p$ncdState)
            p$start.diab.trt(tnow)
          }
        }
        
        if(p$ncdState==NCD.HYP){
          pop$record.hyp.diag(p$agegroup,p$sex,p$hivState,p$ncdState)
          p$model.hyp.diag(tnow)
          
          if(runif(1) < pNcdTrtAdherence){
            pop$record.hyp.trt.adherence(p$agegroup,p$sex,p$hivState,p$ncdState)
            p$start.hyp.trt.adherence(tnow)
          } else {
            pop$record.hyp.trt(p$agegroup,p$sex,p$hivState,p$ncdState)
            p$start.hyp.trt(tnow)  
          }
        }
        
        if(p$ncdState==NCD.DIAB_HYP){
          pop$record.diab.hyp.diag(p$agegroup,p$sex,p$hivState,p$ncdState)
          p$model.diab.hyp.diag(tnow)
          
          if(runif(1) < pNcdTrtAdherence){
            pop$record.diab.hyp.trt.adherence(p$agegroup,p$sex,p$hivState,p$ncdState)
            p$start.diab.hyp.trt.adherence(tnow)
          } else {
            pop$record.diab.hyp.trt(p$agegroup,p$sex,p$hivState,p$ncdState)
            p$start.diab.hyp.trt(tnow)
          }
        }
      }
    })) 
  } 
}
