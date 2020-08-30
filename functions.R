# Replication file for: "Using R to Simulate a Pandemic"
# RPubs-link: https://rpubs.com/mstefan-rpubs/pandemic
# (c) Martin Stefan, August 2020

simPeople <- function(n) {
  locs <- matrix(runif(2*n,-1,1), ncol=2)
  status <- rep("healthy",n) # patient status
  t_infec <- rep(NA,n)       # time until immunity
  return(list(locs=locs, status=status, t_infec=t_infec))
}


move <- function(people, delta) {
  
  # update location
  eps <- matrix(rnorm(2*nrow(people$locs),0,delta), ncol=2)
  eps[people$status == "dead",] <- c(0,0)
  people$locs <- people$locs + eps
  
  # keep people inside the box
  people$locs[,1] <- people$locs[,1]+ifelse(people$locs[,1]<=-1,.1,0)
  people$locs[,2] <- people$locs[,2]+ifelse(people$locs[,2]<=-1,.1,0)
  people$locs[,1] <- people$locs[,1]+ifelse(people$locs[,1]>=1,-.1,0)
  people$locs[,2] <- people$locs[,2]+ifelse(people$locs[,2]>=1,-.1,0)
  
  # return
  return(people)
}


infect <- function(people, infecDist, recovTime) {
  
  # update status
  people$t_infec <- people$t_infec - 1
  people$status[people$t_infec == 0] <- "immune"
  people$t_infec[people$status == "immune"] <- NA
  
  # infect new people
  for(i in which(people$status == "infected")) {
    for(j in which(people$status == "healthy")) {
      if(dist(people$locs[c(i,j),]) < infecDist) {
        people$status[j] <- "infected"
        people$t_infec[j] <- recovTime
      }
    }
  }
  
  # return
  return(people)
}


survive <- function(foo, r) sample(c(T,F),1,prob=c(1-r, r))


kill <- function(people, deathProb) {
  infected <- which(people$status == "infected")
  deaths <- which(!sapply(infected,survive,r=deathProb))
  deaths <- infected[deaths]
  people$status[deaths] <- "dead"
  people$t_infec[deaths] <- NA
  return(people)
}


updateRecord <- function(people, record) {
  record <- rbind(record, rep(NA,4))
  record[nrow(record),1] <- length(which(people$status=="healthy"))
  record[nrow(record),2] <- length(which(people$status=="infected"))
  record[nrow(record),3] <- length(which(people$status=="immune"))
  record[nrow(record),4] <- length(which(people$status=="dead"))
  return(record)
}


plotPeople <- function(people, record) {
  
  par(mfrow=c(1,2))
  
  # determine colors
  palette <- c("grey","red","deepskyblue","black")
  colors <- rep(palette[1],length(people$status))
  colors[which(people$status == "infected")] <- palette[2]
  colors[which(people$status == "immune")] <- palette[3]
  colors[which(people$status == "dead")] <- palette[4]
  
  # plot movement of people
  plot(people$locs, bg=colors, pch=21,
       ylim=c(-1,1), xlim=c(-1,1),
       yaxt="n", xaxt="n", 
       ylab="", xlab="")
  
  # plot record (cumulative numbers)
  plot(record[,1], type="l", col=palette[1],
       ylim=c(0,length(people$status)),
       ylab="", xlab="")
  lines(record[,2], col=palette[2])
  lines(record[,3], col=palette[3])
  lines(record[,4], col=palette[4])
  legend("topright", cex=.75,
         legend = colnames(record), 
         lty=rep(1,4), col=palette)
  
  par(mfrow=c(1,1)) # back to default
  
}



runSim <- function(n_people  = 300, 
                   movement  = .05,
                   infecDist = .05,
                   deathProb = .02,
                   recovTime =  20
                   ) {
  
  # setup
  people <- simPeople(n=n_people)
  record <- matrix(NA,ncol=4,nrow=0)
  colnames(record) <- c("healthy","infected","immune","dead")
  
  # patient zero
  people$status[1] <- "infected" 
  people$t_infec[1] <- recovTime
  
  # run simulation
  while(T) {
    
    # update record and plot
    record <- updateRecord(people, record)
    plotPeople(people, record)
    Sys.sleep(.05)
    if(!any(people$status == "infected")) break()
    
    # move people, infect new people, kill infected people
    people <- move(people, movement)
    people <- infect(people, infecDist, recovTime)
    people <- kill(people, deathProb)
    
  }
  
  # return
  return(record[nrow(record),])
  
}