require(gridExtra)
if(1==2){
  hyp.prev <- simplot.ncd.prevalence.baseline(ncd.simset[[1]], data.type = "hyp.prev", combine.comorbidity = T)
  diab.prev <- simplot.ncd.prevalence.baseline(ncd.simset[[1]], data.type = "diab.prev", combine.comorbidity = T)
  diab.hyp.prev <- simplot.ncd.prevalence.baseline(ncd.simset[[1]], data.type = "diab.hyp.prev", combine.comorbidity = F)
  grid.arrange(hyp.prev, diab.prev, diab.hyp.prev, ncol=3)  
  
  hyp.prev.single <- simplot.ncd.prevalence.baseline(ncd.simset[[1]], data.type = "hyp.prev", combine.comorbidity = F)
  diab.prev.single <- simplot.ncd.prevalence.baseline(ncd.simset[[1]], data.type = "diab.prev", combine.comorbidity = F)
  diab.hyp.prev.single <- simplot.ncd.prevalence.baseline(ncd.simset[[1]], data.type = "diab.hyp.prev", combine.comorbidity = F)
  grid.arrange(hyp.prev.single, diab.prev.single, diab.hyp.prev, ncol=3)  
}


# 
# pdf("ncd.prev.pdf")
# grid.arrange(hyp.prev, diab.prev,diab.hyp.prev)
# dev.off()
# 
# ggsave("ncd.prev.pdf", arrangeGrob(hyp.prev, diab.prev,diab.hyp.prev))

simplot.ncd.prevalence.baseline = function(..., 
                                         years = as.character(2015:2030),
                                         data.type = c("hyp.prev"), 
                                         view.as.rate = T,
                                         per.X.population = 1,
                                         combine.comorbidity = F,
                                         # facet.by = NULL,
                                         # split.by = NULL,
                                         ages = DIM.NAMES.AGE, 
                                         sexes = DIM.NAMES.SEX,
                                         hiv.status = DIM.NAMES.HIV,
                                         ncd.status = DIM.NAMES.NCD
){
  
  sims = list(...)
  keep.dimensions = "year" # REMOVING FACET BY FOR THIS PLOT 
  # keep.dimensions = union('year',union(facet.by, split.by))
  
  ncd.data.types = list("population"="n.state.sizes",
                        "hiv.incidence"="n.hiv.inc",
                        "hiv.prevalence"="n.state.sizes",
                        "hyp.inc" = "n.hyp.inc",
                        "diab.inc" = "n.diab.inc",
                        "diab.hyp.inc" = "n.diab.hyp.inc",
                        "hyp.prev" = "n.state.sizes",
                        "diab.prev" = "n.state.sizes",
                        "diab.hyp.prev" = "n.state.sizes",
                        "mi.inc" = "n.mi.inc",
                        "stroke.inc" = "n.stroke.inc",
                        "hiv.mortality"="n.deaths.hiv",
                        "cvd.mortality"="n.deaths.cvd",
                        "non.hiv.mortality"="n.deaths.non.hiv")
  
  if(combine.comorbidity & !(data.type %in% c("hyp.prev","diab.prev")))
    stop("can only view combined comorbidity for one disease at a time")
  
  df.sim = NULL
  
  for(i in 1:length(sims)){
    sim = sims[[i]]
    
    for(j in 1:length(sim)){
      
      ncd.data.type.x = ncd.data.types[[data.type]]

      # PULL DIRECT DATA TYPE
      if(data.type %in% c("hiv.incidence","mi.inc","stroke.inc",
                          "hyp.inc","diab.inc","diab.hyp.inc",
                          "hiv.mortality","non.hiv.mortality","cvd.mortality")){
        value = filter.5D.stats.by.field(sim[[j]][[ncd.data.type.x]], 
                                         years = years,
                                         ages = ages, 
                                         sexes = sexes,
                                         hiv.status = hiv.status,
                                         ncd.status = ncd.status,
                                         keep.dimensions = keep.dimensions)
        
        # PULL STATE SIZES, WITH SOME MANIPULATIONS (E.G., ONLY HIV+, ONLY HYP+, ETC.)
      }
      if(data.type %in% c("population","hiv.prevalence","hyp.prev","diab.prev","diab.hyp.prev")){
        
        if(data.type=="population")
          ncd.status=ncd.status 
        if(data.type=="hiv.prevalence")
          hiv.status = DIM.NAMES.HIV[-1]
        
        # HYPERTENSION PREVALENCE 
        if(data.type=="hyp.prev"){
          ncd.status="NCD.HYP" 
          
          if(combine.comorbidity) 
            ncd.status=c("NCD.HYP","NCD.DIAB_HYP") # NO TRT: ONLY HYP AND DIAB_HYP
          
          # DIABETES PREVALENCE
        } 
        
        if(data.type=="diab.prev"){
          ncd.status="NCD.DIAB"
          
          if(combine.comorbidity)
            ncd.status=c("NCD.DIAB","NCD.DIAB_HYP")
          
          
          # COMBINED PREVALENCE 
        } 
        
        if(data.type=="diab.hyp.prev"){
          ncd.status="NCD.DIAB_HYP"
        }
        
        # note that ncd.status or hiv.status may have been overwritten from whatever was specified in the arguments
        value = filter.5D.stats.by.field(sim[[j]]$n.state.sizes, 
                                         years = years,
                                         ages = ages, 
                                         sexes = sexes,
                                         hiv.status = hiv.status,
                                         ncd.status = ncd.status,
                                         keep.dimensions = keep.dimensions)
        
      }
      
      if(view.as.rate){
        denominator = filter.5D.stats.by.field(sim[[j]]$n.state.sizes, 
                                               years = years,
                                               ages = ages, 
                                               sexes = sexes,
                                               hiv.status = DIM.NAMES.HIV,
                                               ncd.status = DIM.NAMES.NCD,
                                               keep.dimensions = keep.dimensions) # keeping this in, just in case I do age/sex 
        
        value = value/denominator
        value = value*per.X.population # set in arguments (right now set to 10,000)
        
      }
      
      
      
      # set up a dataframe with columns: year, value, sim id, data.type 
      one.df = reshape2::melt(value) 
      one.df$sim.id = i
      one.df$sim.number = j 
      # one.df$data.type = d
      
      df.sim = rbind(df.sim, one.df)   
      
    }
    
  }
  
  df.sim$sim.id = as.character(df.sim$sim.id)
  df.sim$group.id = paste0("sim ",df.sim$sim.id,"_",df.sim$sim.number)
  
  # for(s in split.by){
  #   df.sim$group.id = paste0(df.sim$group.id,", ",s,"=",df.sim[,s])
  # }
  
  # data.type label
  data.type.label = data.type
  if((data.type %in% c("hyp.prev","diab.prev")))
    data.type.label = paste0(data.type, " (no comorbidity)")
  if(combine.comorbidity)
    data.type.label = paste0(data.type, " (+ hyp/diab comorbidity)")  

  
  
  
  # sub title 
  sub.title.label = NULL
  if(view.as.rate)
    sub.title.label = paste0("per ",per.X.population," population")
  
  # setting up facet.by
  # if(length(facet.by)>0){
  #   facet_string = paste0("~",paste0(facet.by,collapse = '+'))
  #   facet_formula = as.formula(facet_string)
  #   plot = ggplot() + 
  #     geom_line(data = df.sim, aes(x = year, y = value, color = sim.id, group = group.id)) +
  #     facet_wrap(facet_formula, scales = "free_y") + 
  #     labs(title = paste0(data.type.label),
  #          subtitle = paste0(sub.title.label)
  #     )+
  #     ylim(0,NA)
  # } else{
    plot = ggplot() + 
      geom_line(data = df.sim, aes(x = year, y = value, color = sim.id, group = group.id)) +
      scale_y_continuous(labels = scales::percent,name = NULL,limits=c(0,NA)) + 
                         # ,limits=c(0,.3)
      labs(title = paste0(data.type.label))+
      theme(legend.position = "none"
            # panel.border = element_blank(), axis.line = element_line(color="gray")
      )
  # }
  
  suppressWarnings(print(plot))
  
}


# 
# if(percent){
#   ggplot(data = df,aes(x=age,y=value,fill=intervention)) + 
#     geom_bar(stat="identity",position = "dodge") + 
#     labs(title = paste0(outcome),
#          subtitle = paste0(sexes ,collapse=", "))+
#     scale_y_continuous(labels = scales::percent,name = NULL,limits=plot.limits) + 
#     theme(panel.background = element_blank(), legend.position = "bottom"
#           # panel.border = element_blank(), axis.line = element_line(color="gray")
#     ) + 
#     xlab("Age") + ylab(NULL)
