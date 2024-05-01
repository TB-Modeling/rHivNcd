source("plots.R")
source("globalEnvironment.R")
source("rHelperFunctions.R")
source("results/postProcessingFunctions.R") 

START.YEAR=2015
INT.START.YEAR=2023
INT.END.YEAR=2030
END.YEAR=2040 

SCENARIOS = c(1:7)
HIV.SCENARIOS = c("noint", # NCD scen 1
                  "noint", # NCD scen 2
                  "retsupp",  # NCD scen 3
                  "retsupp",  # NCD scen 4
                  "noint",  # NCD scen 5
                  "tsteng",  # NCD scen 6
                  "comp"  # NCD scen 7
                     )
REPLICATIONS = c(1:2)
n.reps=2
OUTPUTS.DIR = "outputs/"

ncd.simset=read.ncd.simset()
save(ncd.simset, file = paste0("outputs/ncd.simset_",Sys.Date(),".Rdata"))
# khm.simset=read.khm.simset()
khm.simset.full = read.khm.simset.full()

results.array.cumulative = generate.cumulative.events.results.array(ncd.simset,n.reps=n.reps)
save(results.array.cumulative, file = paste0("outputs/ncd.results.array.cumulative_",Sys.Date(),".Rdata"))

results.array.annual = generate.annual.events.results.array(ncd.simset,n.reps=n.reps)
save(results.array.annual, file = paste0("outputs/ncd.results.array.annual_",Sys.Date(),".Rdata"))


