

paper.to.use = "Hertz" # or "Law

if(paper.to.use=="Law"){
  # Law et al 2002 MI mortality data 
  times = c(0, # baseline
            0.01, # before hospital
            0.1, # during hospital
            1, # one year after discharge
            5, # five years after
            10) # 10 years after
  mortality = c(0, # baseline
                0.23, # before hospital
                0.16, # during hospital
                (0.23+0.16+0.103), # one year after discharge; 0.493
                (0.23+0.16+0.28), # five years after; 0.67
                (0.23+0.16+0.45)) # 10 years after; 0.84
  
  df.mi.survival = data.frame(x=times,y=1-(mortality))
  
} else if (paper.to.use=="Hertz"){
  # Hertz et al
  times = c(0, # baseline
            0.1, # during hospital
            (3/12), # 3 months
            (6/12), # 6 months
            (9/12), # 9 months
            1) # 1 year
  
  mortality = c(0, # baseline
                0.349, # during hospital
                (0.487), # 3 months
                (0.527), # 6 months
                (0.554), # 9 months
                (0.599)) # 1 year
  
  times.2 = c(1,
              5,
              10)
  
  mortality.2 = c(0.599,
                  (0.23+0.16+0.28), # five years after; 0.67
                  (0.23+0.16+0.45))
  
  df.mi.survival = data.frame(x=c(times,5,10),
                              y=c(1-(mortality),(1-0.67),(1-0.84)))
}


survival = 1-mortality

# set up projection 
project.times = seq(0,10,by=(1/12))
max.cumulative.hazard = 1
cumulative.hazard = -log(survival) # this is a fact 

transformed.data = data.frame(y=log(pmax(0.0001,(1-(cumulative.hazard/max.cumulative.hazard)))),
                              x=times)

# +0 means don't fit an intercept
fit = glm(y~x+0, data = transformed.data,family=gaussian) # same as linear regression 
theta = exp(fit$coefficients[1])

# as above, h = k(1-(theta^t))
projected.cumulative.hazard = max.cumulative.hazard*(1-(theta^project.times))

projected.survival = data.frame(x=project.times,
                                y=exp(-projected.cumulative.hazard)) # h = -log(S) --> S = exp(-h)


monthly.mortality = projected.survival$y[-length(project.times)]-projected.survival$y[-1]
mi.monthly.mortality = array(monthly.mortality,
                                 dim=length(project.times[-1]),
                                 dimnames=list(month=1:120))


if(1==2){
  100*sum(mi.monthly.mortality[1:12]) # year 1: 48.77%
  100*sum(mi.monthly.mortality[13:24]) # year 2: 10.2%
  100*sum(mi.monthly.mortality[25:36]) # year 3: 2.9%
  100*sum(mi.monthly.mortality[37:48]) # year 4: 0.91%
  100*sum(mi.monthly.mortality[49:60]) # year 5: 0.30%
  100*sum(mi.monthly.mortality[1:60]) # --> at 5 years, cumulative is 63%
  100*sum(mi.monthly.mortality[1:120]) # --> at 10 years, cumulative is still 63%
}


ggplot() + 
  geom_line(data=projected.survival,aes(x,y)) + 
  geom_point(data=df.mi.survival,aes(x,y)) + 
  ylim(0,1)


## Use Hertz and then linearly project to year 10 using Law et al data for year 5 and year 10
survival.2 = 1-mortality.2
project.times.2 = seq(1,10,by=(1/12))
df.2 = data.frame(x=times.2,y=survival.2)

fit.test=glm(survival.2~times.2,data=df.2,family=gaussian)
projected.survival.2 = data.frame(x=project.times.2,
                                   y=((fit.test$coefficients[1]+fit.test$coefficients[2]*project.times.2)))

ggplot() + 
  geom_line(data=projected.survival[1:12,],aes(x,y)) + 
  geom_line(data=projected.survival.2,aes(x,y)) + 
  geom_point(data=df.mi.survival,aes(x,y)) + 
  ylim(0,1)

# Previous way of adding on years 1-10 from Law (fixed annual rate of 5.3%)
projected.survival.one.year = projected.survival[1:13,]

mi.annual.mortality = 0.053
mi.monthly.mortality.post.year.one = 0.053/12
years.2.to.10.monthly.times = project.times[14:length(project.times)]

projected.survival.post.one.year = projected.survival.one.year[13,2] - mi.monthly.mortality.post.year.one*c(1:length(years.2.to.10.monthly.times))
projected.survival.post.one.year = data.frame(x=years.2.to.10.monthly.times,
                                              y=projected.survival.post.one.year)

new.projected.survival = data.frame(x=project.times,
                                    y=c(projected.survival.one.year$y,projected.survival.post.one.year$y))

new.monthly.mortality = new.projected.survival$y[-length(project.times)]-new.projected.survival$y[-1]
new.mi.monthly.mortality = array(new.monthly.mortality,
                                 dim=length(project.times[-1]),
                                 dimnames=list(month=1:120))

qplot(project.times[-1],new.mi.monthly.mortality)

ggplot() + 
  geom_line(data=projected.survival.one.year,aes(x,y)) + 
  geom_line(data=projected.survival.post.one.year,aes(x,y)) + 
  geom_point(data=df.mi.survival,aes(x,y)) + 
  ylim(0,1)

# Hertz et al 
100*sum(new.mi.monthly.mortality[1:12]) # year 1: 60%
100*sum(new.mi.monthly.mortality[1:60]) # --> at 5 years, cumulative is 81%
100*sum(new.mi.monthly.mortality[1:102]) # --> reaches 100% at 8.5 years



mi.monthly.mortality = new.mi.monthly.mortality

save(mi.monthly.mortality,file="data/monthly.mi.mortality.Rdata")
