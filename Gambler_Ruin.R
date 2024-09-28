Gambling = function(a,m,p){
  stake=rep(NA,1e4)
  i = 1
  stake[1] = a
  while (stake[i] > 0 & stake[i] < m){
    outcome = sample(c(-1,1),1,prob=c(p,1-p))
    state_of_gambler = stake[i] + outcome
    stake[i+1] = state_of_gambler
    i = i+1
    if (length(stake) == i) stake <- c(stake, rep(NA_real_, 1e4))
  }
  return (stake[!is.na(stake)])
}


trials=1000

simulationlist=replicate(trials,Gambling(4,10,0.5))

simulationlist

number_of_ruins_in_simulation = 0

for (x in simulationlist){
  len=length(x)
  ruin=x[len]
  if (ruin == 0){
    number_of_ruins_in_simulation = number_of_ruins_in_simulation + 1
  }
}

number_of_ruins_in_simulation

LengthOfGame= function(SimList,trials){
  lengthsoftrials=c()
  for (l in 1:trials){
    lengthsoftrials=c(lengthsoftrials,length(SimList[[l]]))
  }
  return lengthsoftrials
}

LengthOfGame(simulationlist,trials)

