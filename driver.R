# need a standard set of plots to review the population
# please add notes/documentation to plots.R

#
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
#function to return elapse run time for the simulation
hms_span <- function(start, end) {
  dsec <- as.numeric(difftime(end, start, unit = "secs"))
  hours <- floor(dsec / 3600)
  minutes <- floor((dsec - 3600 * hours) / 60)
  seconds <- dsec - 3600*hours - 60*minutes
  paste0(
    sapply(c(hours, minutes, seconds), function(x) {
      formatC(x, width = 2, format = "d", flag = "0")
    }), collapse = ":")
}
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

scenarios=list(
  list(id=1,pCoverage=0.0,pNcdTrtInitiation=0.0,pDropOut=0.00),
  list(id=2,pCoverage=0.1,pNcdTrtInitiation=0.8,pDropOut=0.05),
  list(id=3,pCoverage=0.1,pNcdTrtInitiation=0.9,pDropOut=0.00),
  list(id=4,pCoverage=0.1,pNcdTrtInitiation=0.8,pDropOut=0.05),
  list(id=5,pCoverage=0.1,pNcdTrtInitiation=0.9,pDropOut=0.00)
)
#######################################################
# MULTI REPS
if (1==2){
  print("running models sequentially ....")
  lapply(c(1:1),function(rep){
    lapply(c(1:5),function(id){
      start_time <- Sys.time()
      set.seed(rep)
      scenario=scenarios[[id]]$id
      print(paste("replication ",rep," scenario", scenario, "starting..."))
      
      # create pop at the end of 2014; set up hiv/ncd states; records stats and increament the year to 2015
      pop<-initialize.simulation(id = rep,
                                 n = POP.SIZE,
                                 scenario=scenario)
      #run sims
      while(pop$params$CYNOW<= 2030)
        run.one.year.int(pop,
                         scenario =scenarios[[id]]$id,
                         int.start.year = 2023,
                         int.end.year = 2030,
                         pCoverage = scenarios[[id]]$pCoverage,
                         pNcdTrtInitiation = scenarios[[id]]$pNcdTrtInitiation,
                         pDropOut=scenarios[[id]]$pDropOut
        )
      
      #saving population
      res=list(stats=pop$stats,
               params=pop$params)
      saveRDS(res,file = paste0("outputs/popList-s",scenario,"-rep",rep),compress = T)
      # saving time
      end_time <- Sys.time()
      session_time=end_time - start_time
      txt=paste("Model ",rep," >> session time ",session_time)
      write.table(x = txt,file = "outputs/out-sessionTime.txt",col.names = F,row.names = F,append = T)
    })
  })
}

# # #######################################################
# # SINGLE RUN ON ROCKFISH
if (1==1) {
  print("running models parallel ....")
  
  args = commandArgs(trailingOnly=TRUE)
  R=10 #reps
  S=5 #scenarios
  
  x=as.numeric(args[1])
  rep=floor((x-1)/(S))+1
  scenarioId= (x-1)%%5+1
  
  # for (x in c(1:50)){
  #   rep=floor((x-1)/(S))+1
  #   scenarioId= (x-1)%%5+1
  #   print(paste("x=",x,"rep=",rep,"scenario=",scenarioId))
  # }
  
  # create pop at the end of 2014; set up hiv/ncd states; records stats and increament the year to 2015
  set.seed(rep)
  scenario=scenarios[[scenarioId]]$id
  print(paste("replication ",rep," scenario", scenario, "starting..."))
  start_time <- Sys.time()
  pop<-initialize.simulation(id = rep,
                             n = POP.SIZE,
                             scenario=scenario)
  #run sims
  while(pop$params$CYNOW<= 2030)
    run.one.year.int(pop,
                     scenario =scenario,
                     int.start.year = 2023,
                     int.end.year = 2030,
                     pCoverage = scenarios[[scenarioId]]$pCoverage,
                     pNcdTrtInitiation = scenarios[[scenarioId]]$pNcdTrtInitiation,
                     pDropOut=scenarios[[scenarioId]]$pDropOut
    )
  
  #saving population
  res=list(stats=pop$stats,
           params=pop$params)
  saveRDS(res,file = paste0("outputs/popList-s",scenario,"-rep",rep),compress = T)
  # saving time
  end_time <- Sys.time()
  session_time=end_time - start_time
  txt=paste("Model ",rep," >> session time ",session_time)
  write.table(x = txt,file = "outputs/out-sessionTime.txt",col.names = F,row.names = F,append = T)
}

# # Reading populations back into a simset object
{
  # scenario 0
  SCENARIOS = c(0:4)
  REPLICATIONS = c(0:2)
  
  ncd.simset=vector("list",5)
  invisible(lapply(c(0:4), function(scenario){
    temp.simset = vector("list",2)
    
    invisible(lapply(c(1:2),function(rep){
      pop<-readRDS(paste0("outputs/popList-s",scenario,"-rep",rep))
      temp.simset[[rep]] <<- pop
      
      return(temp.simset)
    }))
    
    ncd.simset[[scenario+1]]<<-temp.simset # MELISSA CHANGE THIS WHEN I RENAME KHM OBJECTS 
    return(ncd.simset)
  }))


  khm.simset.0 = ncd.simset.0[[1]]$params$khm.full
  khm.0.ids = sapply(ncd.simset.0,function(pop){pop$params$khm.id})
  khm.simset.0=khm.simset.0[khm.0.ids]
  class(khm.simset.0)="khm_simulation_output"

  # scenario 1
  ncd.simset.1=list()
  lapply(c(1:2),function(rep){
    pop<-readRDS(sprintf("outputs/popList-s1-rep%g",rep))
    ncd.simset.1[[sprintf("popList-s0-rep%g",rep)]]<<-pop
  })

  khm.simset.1 = ncd.simset.1[[1]]$params$khm.full
  khm.1.ids = sapply(ncd.simset.1,function(pop){pop$params$khm.id})
  khm.simset.1=khm.simset.1[khm.1.ids]
  class(khm.simset.1)="khm_simulation_output"

  # scenario 2
  ncd.simset.2=list()
  lapply(c(1:2),function(rep){
    pop<-readRDS(sprintf("outputs/popList-s2-rep%g",rep))
    ncd.simset.2[[sprintf("popList-s0-rep%g",rep)]]<<-pop
  })

  khm.simset.2 = ncd.simset.2[[1]]$params$khm.full
  khm.2.ids = sapply(ncd.simset.2,function(pop){pop$params$khm.id})
  khm.simset.2=khm.simset.2[khm.2.ids]
  class(khm.simset.2)="khm_simulation_output"

  # scenario 3
  ncd.simset.3=list()
  lapply(c(1:2),function(rep){
    pop<-readRDS(sprintf("outputs/popList-s3-rep%g",rep))
    ncd.simset.3[[sprintf("popList-s0-rep%g",rep)]]<<-pop
  })

  khm.simset.3 = ncd.simset.3[[1]]$params$khm.full
  khm.3.ids = sapply(ncd.simset.3,function(pop){pop$params$khm.id})
  khm.simset.3=khm.simset.3[khm.3.ids]
  class(khm.simset.3)="khm_simulation_output"

  # scenario 4
  ncd.simset.4=list()
  lapply(c(1:2),function(rep){
    pop<-readRDS(sprintf("outputs/popList-s4-rep%g",rep))
    ncd.simset.4[[sprintf("popList-s0-rep%g",rep)]]<<-pop
  })

  khm.simset.4 = ncd.simset.4[[1]]$params$khm.full
  khm.4.ids = sapply(ncd.simset.4,function(pop){pop$params$khm.id})
  khm.simset.4=khm.simset.4[khm.4.ids]
  class(khm.simset.4)="khm_simulation_output"

  ncd.simset = list()
  ncd.simset[[1]] = ncd.simset.0
  ncd.simset[[2]] = ncd.simset.1
  ncd.simset[[3]] = ncd.simset.2
  ncd.simset[[4]] = ncd.simset.3
  ncd.simset[[5]] = ncd.simset.4

  khm.simset = list()
  khm.simset[[1]] = khm.simset.0
  khm.simset[[2]] = khm.simset.1
  khm.simset[[3]] = khm.simset.2
  khm.simset[[5]] = khm.simset.4

  simsets = c(paste0("khm.simset.",rep(1:4)),paste0("ncd.simset.",rep(1:4)))

  simplot(khm.simset.0, khm.simset.1, khm.simset.2, khm.simset.3, khm.simset.4,
          # ncd.simset.0, ncd.simset.1, ncd.simset.2, ncd.simset.3, ncd.simset.4,
          data.type = "hiv.incidence",scale.population = T)

  simplot(ncd.simset.0, #ncd.simset.1, ncd.simset.2,
          ncd.simset.3, ncd.simset.4,
          data.type = "hyp.prev",scale.population = F)
}
{
  print(paste0(length(simset)," ncd populationd data is read"))
  ncd.simset = simset
  khm.simset = ncd.simset[[1]]$params$khm.full # HIV simset
  print(paste0(length(khm.simset)," khm populationd data is read"))

  # vector of sampled khm.ids
  khm.ids = sapply(ncd.simset,function(pop){pop$params$khm.id})
  khm.simset=khm.simset[khm.ids]
  # khm.simset=khm.simset[c(5,75)]
  class(khm.simset)="khm_simulation_output"
}
#' {
#'   #comparing ncd and khm population sizes
#'   simplot(khm.simset,ncd.simset,data.type = "population",scale.population = T)
#'   simplot(khm.simset,ncd.simset,data.type = "population",scale.population = T, facet.by = "age")
#'   simplot(khm.simset,ncd.simset,data.type = "population",scale.population = T, facet.by = c("age","sex"))
#'   # simplot(ncd.simset,data.type = "population",facet.by = "age")
#'   simplot(khm.simset,ncd.simset,data.type = "population",scale.population = T, facet.by = "sex")
#'   #' @MS: simplot(khm.simset,ncd.simset,data.type = "population",scale.population = T, facet.by = c("age","sex")
#'   #'
#'   simplot(khm.simset,ncd.simset,data.type = "population",scale.population = T, facet.by = "hiv.status")
#' 
#'   # comparing deaths ???
#'   simplot(khm.simset,ncd.simset,data.type = "hiv.mortality",scale.population =T)
#'   simplot(khm.simset,ncd.simset,data.type = "hiv.mortality",scale.population = T,facet.by = "age")
#'   #'@MS
#'   # simplot(ncd.simset,data.type = "mortality",scale.population = F,facet.by = "age")
#'   # simplot(ncd.simset,data.type = "mortality",scale.population = F,facet.by = c("sex","age"))
#' 
#' }

#' #check NCD prevalence in 2015
#' {
#'   pop=simset$pop1
#'   ncd.states2015 = filter.5D.stats.by.field(pop$stats$n.state.sizes,
#'                                             years = as.character(2014),
#'                                             keep.dimensions = c('age','sex','ncd.status','year'))
#'   ncd.states2015=ncd.states2015[,,,1] #to remove year dimension
#'   ncd.props2015<-return.prop.sex.age(vFreq = ncd.states2015)
#' 
#'   par(mfrow=c(2,2))
#'   plot(pop$params$target.ncd.props[,"MALE","NCD.DIAB"],type="l",ylab="",main="diab.prev male",xlab="agegroups")
#'   lines(ncd.props2015[,"MALE","NCD.DIAB"],col="red")
#'   plot(pop$params$target.ncd.props[,"FEMALE","NCD.DIAB"],type="l",ylab="",main="diab.prev female",xlab="agegroups")
#'   lines(ncd.props2015[,"FEMALE","NCD.DIAB"],col="red")
#'   plot(pop$params$target.ncd.props[,"MALE","NCD.HYP"],ylim=c(0, 0.6), type="l",ylab="",main="hyp.prev male",xlab="agegroups")
#'   lines(ncd.props2015[,"MALE","NCD.HYP"],col="red")
#'   plot(pop$params$target.ncd.props[,"FEMALE","NCD.HYP"],ylim=c(0, 0.6), type="l",ylab="",main="hyp.prev female",xlab="agegroups")
#'   lines(ncd.props2015[,"FEMALE","NCD.HYP"],col="red")
#' }

# #######################################################
# 
# 
# # pop$stats$n.births
# # pop$stats$n.births.non.hiv
# # pop$stats$n.births.hiv
# # # #
# # pop$stats$n.deaths.ageout
# # pop$stats$n.deaths.hiv
# # pop$stats$n.deaths.non.hiv
# # pop$stats$n.deaths.cvd
# # #  NCD incidence
# filter.5D.stats.by.field(pop$stats$n.diab.inc, keep.dimensions = c('year'))
# filter.5D.stats.by.field(pop$stats$n.hyp.inc, keep.dimensions = c('year'))
# filter.5D.stats.by.field(pop$stats$n.diab.hyp.inc, keep.dimensions = c('year'))
# # # HIV events
# filter.5D.stats.by.field(pop$stats$n.hiv.inc, keep.dimensions = c('year',"age"))
# filter.5D.stats.by.field(pop$stats$n.diab.hyp.inc, keep.dimensions = c('year',"sex"))
# filter.5D.stats.by.field(pop$stats$n.diab.inc, keep.dimensions = c('year',"age","sex"))
# filter.5D.stats.by.field(pop$stats$n.hyp.inc, keep.dimensions = c('year',"age","sex"))
# # 
# filter.5D.stats.by.field(pop$stats$n.mi.inc, keep.dimensions = c('year'))
# filter.5D.stats.by.field(pop$stats$n.stroke.inc, keep.dimensions = c('year'))
# # 
# filter.5D.stats.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year'))
# filter.5D.stats.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year','hiv.status'))
# filter.5D.stats.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year','ncd.status'))

# ####################################################################################
# for(i in c(INITIAL.YEAR:END.YEAR)){
#   pop = run.one.year.for.ncd.test(pop)
# }
# ####################################################################################
# ####################################################################################
# # plot NCD prevalence at the population-level by year
# # props and freq
# ####################################################################################
# # simulated ncd prev by year
# {
#   sim.ncd.prev.size = filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('ncd.status','year'))
#   D<-lapply(1:DIM.YEAR,function(year){
#     return(sim.ncd.prev.size[,year]/sum(sim.ncd.prev.size[,year]))})
#   sim.ncd.prev.prp<-t(do.call(rbind,D))
#
#   x=pop$params$target.ncd.size
#   dim(x)=c(dim(x),1) #add year
#   dimnames(x)=list(
#     age = DIM.NAMES.AGE,
#     sex = DIM.NAMES.SEX,
#     ncd.status = DIM.NAMES.NCD,
#     year=as.character(2015)
#   )
#   target.ncd.prev.size=filter.4D.stats.by.field.ncd(x,
#                                                     years=as.character(2015),
#                                                     keep.dimensions = c('ncd.status','year'))
#   #prp of total population in 2015
#   target.ncd.prev.prp<-target.ncd.prev.size/sum(target.ncd.prev.size)
#
#   # eqivalent target sizes for our model
#   target.ncd.prev.simPop= round(target.ncd.prev.prp* POP.SIZE)
#
#   {  jpeg("ncdPrev_total.jpeg",width = 3000,height = 1500,res=300)
#     par(mfrow=c(2,4))
#     lapply(1:DIM.NCD,function(c){
#       sim=sim.ncd.prev.prp[c,]
#       target=target.ncd.prev.prp[c,]
#       plot(sim, ylim=c(min(sim,target,na.rm = T),max(sim,target,na.rm = T)),
#            main=DIM.NAMES.NCD[c],type="l",lwd=2,ylab="proportion")
#       abline(h=target,col="red",lwd=2)
#     })
#     lapply(1:DIM.NCD,function(c){
#       sim=sim.ncd.prev.size[c,]
#       target=target.ncd.prev.simPop[c,]
#       plot(sim, ylim=c(min(sim,target,na.rm = T),max(sim,target,na.rm = T)),
#            main=DIM.NAMES.NCD[c],type="l",lwd=2,ylab="Frequency")
#       abline(h=target,col="red",lwd=2)
#     })
#     dev.off()
#   }
# }
# ########################################################################################
# # plot NCD prevalence by age and sex over time
# {
#   #frequency distribution of NCD states by age and sex
#   sim.ncd.prev.size = filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c( 'age','sex' ,'ncd.status','year'))
#   # estimate ncd prevalence proportions in each age/sex strata
#   vFreq=sim.ncd.prev.size
#   vProp=vFreq
#   invisible(
#     sapply(1:length(DIM.NAMES.SEX), function(sex){
#       sapply(1:length(DIM.NAMES.AGE), function(age){
#         sapply(1:length(DIM.NAMES.YEAR), function(year){
#           vProp[age,sex,,year]<<- vProp[age,sex,,year]/sum(vFreq[age,sex,,year])
#         })
#       })
#     }))
#   vProp[vProp=="NaN"] = 0 # to remove NaN values that were introduced by dividing by 0
#   sim.ncd.prev.prp=vProp
#   dim(sim.ncd.prev.prp)
#
#   #target ncd prev proportions
#   target.ncd.prev.prp=pop$params$target.ncd.props
#
#   #estimate corresponding target frequencies in our population
#   target.ncd.prev.simPop=sim.ncd.prev.size
#   invisible(
#     lapply(1:DIM.AGE,function(age){
#       lapply(1:DIM.SEX,function(sex){
#         lapply(1:DIM.YEAR,function(year){
#           t=target.ncd.prev.prp[age,sex,]
#           popSize=sum(sim.ncd.prev.size[age,sex,,year])
#           #
#           target.ncd.prev.simPop[age,sex,,year]<<-round(t*popSize)
#         })  })}))
#
# }
#
# { #plot the ncd 'proportions' within each age/sex strata against target
#   jpeg("ncdPrevProp_ageSex.jpeg",width = 12000,height = 5000,res=300)
#   par(mfrow=c(8,17))
#   invisible(
#     lapply(1:DIM.SEX,function(sex){
#       lapply(1:DIM.NCD,function(ncd){
#         lapply(1:DIM.AGE,function(age){
#           sim=sim.ncd.prev.prp[age,sex,ncd,]
#           target=target.ncd.prev.prp[age,sex,ncd]
#           plot(sim, ylim=c(min(sim,target,na.rm = T),max(sim,target,na.rm = T)),lwd=2,
#                main=paste0(DIM.NAMES.NCD[ncd],"_",DIM.NAMES.SEX[sex],"_",DIM.NAMES.AGE[age]),
#                type="l",ylab="prop",xlab="")
#           abline(h=target,col="red",lwd=2)
#         })       })     })   )
#   dev.off()
# }
# {#plot the ncd 'frequencies' within each age/sex strata against target
#   jpeg("ncdPrevFreq_ageSex.jpeg",width = 12000,height = 6000,res=300)
#   par(mfrow=c(8,17))
#   invisible(
#     lapply(1:DIM.SEX,function(sex){
#       lapply(1:DIM.NCD,function(ncd){
#         lapply(1:DIM.AGE,function(age){
#           sim=sim.ncd.prev.size[age,sex,ncd,]
#           target=target.ncd.prev.simPop[age,sex,ncd,]
#           plot(sim, ylim=c(min(sim,target,na.rm = T),max(sim,target,na.rm = T)),lwd=4,
#                main=paste0(DIM.NAMES.NCD[ncd],"_",DIM.NAMES.SEX[sex],"_",DIM.NAMES.AGE[age]),
#                type="l",ylab="Freq",xlab="")
#           lines(target,col="green",lwd=4)
#         })       })     })   )
#   dev.off()
# }
# ########################################################################################
# # sum square error for sim freq vs target freq over year:
# # SSE= (sim.ncd.prev.size - target.ncd.prev.simPop)^2
#
# # SSE between proportions:
# target.prp<-array(rep(target.ncd.prev.prp, DIM.YEAR),dim=c(DIM.AGE,DIM.SEX,DIM.NCD,DIM.YEAR),dimnames = list(DIM.NAMES.AGE,DIM.NAMES.SEX,DIM.NAMES.NCD,DIM.NAMES.YEAR))
# SSE= (sim.ncd.prev.prp - target.prp)^2
#
# # for each NCD state in each YEAR: compute mean squared error accross all age/sex strata
# mse.ncd.year<-array(0,dim=c(DIM.NCD,DIM.YEAR),dimnames = list(DIM.NAMES.NCD,DIM.NAMES.YEAR))
# invisible(lapply(1:DIM.NCD,function(ncd){
#   lapply(1:DIM.YEAR,function(year){
#     mse.ncd.year[ncd,year] <<- mean(SSE[,,ncd,year])
#   })}))
#
#
# {  jpeg("mse_byNcdYear.jpeg",width = 1500,height = 1500,res=300)
#   par(mfrow=c(2,2))
#   lapply(1:DIM.NCD,function(c){
#     sim=mse.ncd.year[c,]
#     plot(sim,
#          main=DIM.NAMES.NCD[c],type="l",lwd=2,ylab="MSE")
#   })
# dev.off()
# }

#' 
#'      
#' # @MS:
#' #' @step.dataset has been randomized, correct?
#' 
#' 
#' #seems like lapply over the pop$member is the winner
#' system.time(
#'   invisible(sapply(pop$members,function(p){
#'     p.probs = hiv.probs[,p$agegroup,p$sex]
#'     if (p$hivState==HIV.NEG) p$bMarkedDead.hiv=T
#'   })), gcFirst = TRUE)
#' system.time(
#'   invisible(lapply(pop$members,function(p){
#'     p.probs = hiv.probs[,p$agegroup,p$sex]
#'     if (p$hivState==HIV.NEG) p$bMarkedDead.hiv=T
#'   })), gcFirst = TRUE)
#' 
#' system.time(
#'   invisible(lapply((1:length(pop$members)),function(x){
#'     p=pop$members[[x]]
#'     p.probs = hiv.probs[,p$agegroup,p$sex]
#'     if (p$hivState==HIV.NEG) p$bMarkedDead.hiv=T
#'   })), gcFirst = TRUE)
#' system.time(
#'   invisible(sapply((1:length(pop$members)),function(x){
#'     p=pop$members[[x]]
#'     p.probs = hiv.probs[,p$agegroup,p$sex]
#'     if (p$hivState==HIV.NEG) p$bMarkedDead.hiv=T
#'   })), gcFirst = TRUE)
#' 
#' 
#' #@MS
#' MP$annual.cvd.risk.by.age.sex=-((log(1- x/100 ))/10)
#' #assuming geometric distribution of risk over time
#' MP$monthly.cvd.risk.by.age.sex=(1-(1-MP$annual.cvd.risk.by.age.sex)^(1/12))
#' # why using differnt approaches to convert risk?
#' #why accessing previous agegroup?

