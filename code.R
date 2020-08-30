# Replication file for: "Using R to Simulate a Pandemic"
# RPubs-link: https://rpubs.com/mstefan-rpubs/pandemic
# (c) Martin Stefan, August 2020

rm(list = ls())
graphics.off()
set.seed(1234)

# source functions
source("functions.R")

# run simulation: default values
runSim(n_people  = 300, 
       movement  = .05,
       infecDist = .05,
       deathProb = .02,
       recovTime =  20
       )

# run simulation: rapidly spreading disease
runSim(infecDist = .1)

# run simulation: strong social distancing
runSim(movement = .02)

# run simulation: high probability of death
runSim(deathProb = .1)

# run simulation: long recovery time
runSim(recovTime = 50)
