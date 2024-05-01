

# hiv.treatment.quantiles = apply(hiv.treatment,c("intervention"),quantile,c(0.025,.5,.975))
# hiv.treatment.median = hiv.treatment.quantiles[2,]








hiv.treatment = apply(results.array[,,,,"n.hiv.eng",,],c("rep","intervention"),sum)
hiv.treatment.inc = sapply(interventions, function(int){
  sapply(reps, function(rep){
    
    hiv.treatment[rep,int] - hiv.treatment[rep,1]  
  })
})
dimnames(hiv.treatment.inc) = list(rep=c(1:30),
                                   intervention=interventions)

hiv.treatment.inc=hiv.treatment.inc[,-1]
df = reshape2::melt(hiv.treatment.inc)
ggplot(df, aes(x=intervention,y=value, fill=intervention)) + geom_boxplot()


# this is newly on treatment 
hiv.treatment.annual = apply(results.array.annual[,,,,,"n.hiv.eng",,],c("year","rep","intervention"),sum)
hiv.treatment.annual.median = apply(hiv.treatment.annual,c("year","intervention"),median)

hiv.treatment.annual.median = hiv.treatment.annual.median[-1,]
dimnames(hiv.treatment.annual.median)[2] = list(intervention=interventions)

df.annual = reshape2::melt(hiv.treatment.annual.median)

# bar plot
ggplot(df.annual, aes(fill=intervention, y=value, x=year)) + 
  geom_bar(position="dodge", stat="identity")
