library(rjags)
library(runjags)
library(coda)


#Data
setwd("~/Downloads")
epl = read.csv("epl.csv",header = T)
data_list = list(hTeam = epl$HomeID,aTeam = epl$AwayID,H = epl$HomeGoals, A = epl$AwayGoals,
                 played = played,home_points = home_points,away_points = away_points)


#JAGS Model
modelString ="model{
for(i in 1:20){
  for(j in 1:20){
    LeagueRanks[i,j] = ifelse(LeagueRank[i]==j,1,0)
  }
}

LeagueRank = rank(-TotalPoints)

for(i in 1:20){
  TotalPoints[i] = sum(FinalPointsHome[i,]) -FinalPointsHome[i,i] + sum(FinalPointsAway[i,]) - FinalPointsAway[i,i]
}

for(i in 1:20){
  for(j in 1:20){
    FinalPointsHome[i,j] = ifelse(played[i,j]==1,home_points[i,j],SimPointsHome[i,j])
    FinalPointsAway[i,j] = ifelse(played[i,j]==1,away_points[i,j],SimPointsAway[i,j])
  }
}

for(i in 1:20){
  for(j in 1:20){
    SimPointsHome[i,j] = max(SimPointHome[i,j],SimPointsTie[i,j])
    SimPointsAway[i,j] = max(SimPointAway[i,j],SimPointsTie[i,j])
  }
}


for(i in 1:20){
  for(j in 1:20){
    SimPointHome[i,j] = ifelse(SimGoalsHome[i,j]>SimGoalsAway[i,j],3,0)
    SimPointAway[i,j] = ifelse(SimGoalsHome[i,j]<SimGoalsAway[i,j],3,0)
    SimPointsTie[i,j] = ifelse(SimGoalsHome[i,j]==SimGoalsAway[i,j],1,0)
  }
}
  
    
for(i in 1:20){
  for(j in 1:20){
    SimGoalsHome[i,j] ~ dnegbin(r/(lambda[hTeam[i],aTeam[i]]+r),r)
    SimGoalsAway[i,j] ~ dnegbin(r/(theta[hTeam[i],aTeam[i]]+r),r)
  }
}

  for(i in 1:146){
    H[i]~dnegbin(r/(lambda[hTeam[i],aTeam[i]]+r),r)
    
    A[i]~dnegbin(r/(theta[hTeam[i],aTeam[i]]+r),r)
  
  }
  
  for(i in 1:20){
    for(j in 1:20){
      lambda[i,j] = exp(mu + alpha[i] - delta[j] + gamma)
      theta[i,j] = exp(mu + alpha[j] - delta[i])
    }
  }
  
  for(i in 1:20){
      for(j in 1:20){
       ranks.a[i,j] = ifelse(rank.a[i] == j,1,0)
      }
    }

    for(i in 1:20){
      for(j in 1:20){
        ranks.d[i,j] = ifelse(rank.d[i] == j,1,0)
      }
    }

  rank.a = rank(-alpha)
  rank.d = rank(-delta)  
 
  alpha[20] = -sum(alpha[1:19])
  delta[20] = -sum(delta[1:19])
  for(i in 1:19){
    alpha[i]~dnorm(0,.01)
    delta[i]~dnorm(0,.01)
  }
  
  r~dgamma(.01,.01)
  mu~dnorm(0,.01)
  gamma~dnorm(0,.01)
}"

writeLines(modelString, con = "epl6.jags")
HPModel = jags.model(file = "epl6.jags",data = data_list)

update(HPModel,n.iter =2000)

sample64 = coda.samples(HPModel,n.iter = 20000, thin = 2, 
                        variable.names = c("alpha","delta","rank.a","rank.d","ranks.a","ranks.d"))
sampleMatrix64 = as.matrix(sample64)

sample65 = coda.samples(HPModel,n.iter = 20000, thin = 2, 
                      variable.names = c("TotalPoints","LeagueRank","LeagueRanks","SimPointsHome[14,13]","SimPointsAway[14,13]"))
sampleMatrix65 = as.matrix(sample65)
