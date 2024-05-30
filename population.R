#
#  R HIVNCD 2022
#  Population class
#  
#####################################
print("Sourcing Population.R ... ")

# The R6 library has a lightweight class
# library(R6)

POPULATION<-R6Class("POPULATION",
                    public=list(
                      id=NULL,
                      members=NULL,
                      params=NULL, #model parameters
                      stats=NULL, #model statistics
                      ###
                      initialize=function(id=NULL,members=list(),params=list(),stats=list()){
                        self$id<-id
                        self$members<-members
                        self$params<-params
                        self$stats<-stats #generates a new general statistics
                      },
                      greet=function(){
                        cat("Pop id=",self$id,"created with ",length(self$members)," members.", 
                            "It has ",length(self$params), " parameters.", 
                            "Sum of all stats is ", sum(unlist(self$stats)),"\n")
                      },
                      increaseYear=function(){
                        self$params$YNOW<-self$params$YNOW+1
                        self$params$CYNOW<-self$params$CYNOW+1
                      },
                      increaseMonth=function(){
                        self$params$TNOW<-self$params$TNOW+1
                      },
                      addMembers=function(memberListNew=list()){
                        self$members<-c(self$members,memberListNew)
                      },
                      modelAging=function(){
                        invisible(lapply(self$members,function(p) {p$incAge}))
                      },
                      model.aging.out=function(){
                        vdead<-lapply(self$members,function(p) {
                          res=0; 
                          if (p$age>=MAX.AGE){ 
                            res=1;
                            self$record.deaths.ageout(p$agegroup,p$sex,p$hivState,p$ncdState)
                            }
                          return(res)
                        })
                        self$members <- self$members[!unlist(vdead)] #remove dead people
                        self$stats$n.deaths.ageout[self$params$YNOW]=sum(unlist(vdead))
                      },
                      ###
                      return.state.size.distribution=function(){
                        n=length(self$members)
                        state.sizes<-array(0,
                                           c(sapply(DIM.NAMES.AGE.SEX.HIV.NCD,length),1),
                                           dimnames = c(DIM.NAMES.AGE.SEX.HIV.NCD,
                                                        list(year = as.character(self$params$CYNOW))))
                        invisible(lapply(self$members,function(p){
                          state.sizes[  p$agegroup,
                                        p$sex, 
                                        p$hivState,
                                        p$ncdState,1] <<- state.sizes[  p$agegroup,
                                                                        p$sex, 
                                                                        p$hivState, 
                                                                        p$ncdState,1] +1}))
                        state.sizes
                      },
                      ###
                      record.annual.stats=function(){
                        ynow=self$params$YNOW
                        self$stats$pop.size[ynow] <-length(self$members) #population size
                        self$stats$n.state.sizes[,,,,ynow] <- self$return.state.size.distribution() #state sizes
                      },
                      
                      # return NCD treatment coverage (for baseline calibration)
                      return.ncd.trt.coverage = function(){
                        ynow=self$params$YNOW
                        current.state.sizes = self$stats$n.state.sizes[,,,,ynow]
                        
                        n.ncd = sum(current.state.sizes[,,,c("NCD.DIAB","NCD.HYP","NCD.DIAB_HYP")]) 
                        n.ncd.trt = sum(current.state.sizes[,,,c("NCD.DIAB.TRT","NCD.HYP.TRT","NCD.DIAB_HYP.TRT",
                                                           "NCD.DIAB.TRT.ADH","NCD.HYP.TRT.ADH","NCD.DIAB_HYP.TRT.ADH")]) 
                        
                        ncd.trt.coverage = n.ncd.trt/n.ncd
                        self$stats$ncd.trt.coverage[ynow] = ncd.trt.coverage
                      },
                      
                      #record HIV events
                      record.hiv.inc=function(age,sex,hiv,ncd){
                        self$stats$n.hiv.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hiv.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.hiv.diag=function(age,sex,hiv,ncd){
                        self$stats$n.hiv.diag[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hiv.diag[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.hiv.eng=function(age,sex,hiv,ncd){
                        self$stats$n.hiv.eng[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hiv.eng[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.hiv.uneng=function(age,sex,hiv,ncd){
                        self$stats$n.hiv.uneng[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hiv.uneng[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      
                      #record NCD events
                      record.diab.hyp.inc=function(age,sex,hiv,ncd){
                        self$stats$n.diab.hyp.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.diab.hyp.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.diab.inc=function(age,sex,hiv,ncd){
                        self$stats$n.diab.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.diab.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.hyp.inc=function(age,sex,hiv,ncd){
                        self$stats$n.hyp.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hyp.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      
                      #record CVD events
                      record.mi.inc=function(age,sex,hiv,ncd){
                        self$stats$n.mi.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.mi.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.stroke.inc=function(age,sex,hiv,ncd){
                        self$stats$n.stroke.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.stroke.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      
                      ############### INTERVENTION ##################
                      # record HIV new diagnosis and treatment initiation through intervention
                      # record.hiv.diag=function(age,sex,hiv,ncd){
                      #   self$stats$n.hiv.diag[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                      #     self$stats$n.hiv.diag[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      # },
                      # record.hiv.trt=function(age,sex,hiv,ncd){
                      #   self$stats$n.hiv.trt[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                      #     self$stats$n.hiv.trt[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      # },
                      # 
                      #record NCD screening through intervention
                      record.ncd.screening=function(age,sex,hiv,ncd){
                        self$stats$n.ncd.screened[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.ncd.screened[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      #record NCD diagnosis through intervention
                      record.diab.diag=function(age,sex,hiv,ncd){
                        self$stats$n.diab.diag[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.diab.diag[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.hyp.diag=function(age,sex,hiv,ncd){
                        self$stats$n.hyp.diag[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hyp.diag[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.diab.hyp.diag=function(age,sex,hiv,ncd){
                        self$stats$n.diab.hyp.diag[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.diab.hyp.diag[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      #record NCD treatment initiation through intervention
                      record.diab.trt=function(age,sex,hiv,ncd){
                        self$stats$n.diab.trt[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.diab.trt[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.hyp.trt=function(age,sex,hiv,ncd){
                        self$stats$n.hyp.trt[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hyp.trt[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.diab.hyp.trt=function(age,sex,hiv,ncd){
                        self$stats$n.diab.hyp.trt[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.diab.hyp.trt[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      #record NCD treatment ADHERENCE
                      record.diab.trt.adherence=function(age,sex,hiv,ncd){
                        self$stats$n.diab.trt.adherence[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.diab.trt.adherence[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.hyp.trt.adherence=function(age,sex,hiv,ncd){
                        self$stats$n.hyp.trt.adherence[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hyp.trt.adherence[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.diab.hyp.trt.adherence=function(age,sex,hiv,ncd){
                        self$stats$n.diab.hyp.trt.adherence[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.diab.hyp.trt.adherence[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      # record ncd trt dropouts
                      record.diab.trt.dropout=function(age,sex,hiv,ncd){
                        self$stats$n.diab.trt.dropout[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.diab.trt.dropout[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.hyp.trt.dropout=function(age,sex,hiv,ncd){
                        self$stats$n.hyp.trt.dropout[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hyp.trt.dropout[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.diab.hyp.trt.dropout=function(age,sex,hiv,ncd){
                        self$stats$n.diab.hyp.trt.dropout[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.diab.hyp.trt.dropout[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      
                      #record deaths
                      record.deaths.hiv=function(age,sex,hiv,ncd){
                        self$stats$n.deaths.hiv[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.deaths.hiv[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.deaths.cvd=function(age,sex,hiv,ncd){
                        self$stats$n.deaths.cvd[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.deaths.cvd[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.deaths.ageout=function(age,sex,hiv,ncd){
                        self$stats$n.deaths.ageout[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.deaths.ageout[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.deaths.non.hiv=function(age,sex,hiv,ncd){
                        self$stats$n.deaths.non.hiv[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.deaths.non.hiv[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      }
                      
                    )
                    
)

