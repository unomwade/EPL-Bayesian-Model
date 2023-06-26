library(rjags)
library(runjags)
library(coda)


#Data
epl = read.csv("epl.csv",header = T)
data_list = list(hTeam = epl$HomeID,aTeam = epl$AwayID,H = epl$HomeGoals, A = epl$AwayGoals)


#JAGS Model
modelString ="model{
  for(i in 1:146){
    H[i]~dpois(lambda[hTeam[i],aTeam[i]])
    
    A[i]~dpois(theta[hTeam[i],aTeam[i]])
    
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
  
  mu~dnorm(0,.01)
  gamma~dnorm(0,.01)
}"

writeLines(modelString, con = "epl.jags")
HPModel = jags.model(file = "epl.jags",data = data_list)

update(HPModel,n.iter =2000)

sample = coda.samples(HPModel,n.iter = 20000, thin = 2, variable.names = c("alpha","delta","rank.a","rank.d","ranks.a","ranks.d"))
sampleMatrix = as.matrix(sample)
