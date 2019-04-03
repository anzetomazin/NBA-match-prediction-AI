# shranim podatke iz datoteke "regular.txt"

regular <- read.table(file = "regular.txt", sep = ",", header = TRUE)

#####
# Izris pvoprečnega števila doseženih točk na tekmo v posamezni sezoni
#

avgs <- vector()
seasons <- c("2014-15", "2015-16", "2016-17")

for (s in seasons)
{
  games <- regular[regular$SEASON == s,]
  
  avgs <- c(avgs, mean(games$HPTS + games$APTS))
}

barplot(avgs, ylim=c(0, 250), main="Povprečo število točk za vasko sezono", names.arg=seasons)

#####
# Odstotki zmag na domačih tleh po sezonah
#

a <- vector()
seasons <- c("2014-15", "2015-16", "2016-17")

for (s in seasons)
{
  games <- regular[regular$SEASON == s,]
  
  wins <- ifelse(games$HPTS > games$APTS, T, F)
  
  a <- c(a, table(wins)["TRUE"] / length(wins))
}

barplot(a, ylim=c(0, 1), main="Odstotki zmag doma", names.arg=seasons)

#####
# Največje število točk po sezonah
#

a <- vector()
seasons <- c("2014-15", "2015-16", "2016-17")

for (s in seasons)
{
  games <- regular[regular$SEASON == s,]
  
  hmax <- max(games$HPTS)
  amax <- max(games$APTS)
  
  a <- c(a, max(c(hmax, amax)))
}

barplot(a, ylim=c(0, 170), main="Največje št. točk", names.arg=seasons)

#####
# Izris pvoprečnega števila doseženih točk domačih na tekmo v posamezni sezoni
#

avgs <- vector()
seasons <- c("2014-15", "2015-16", "2016-17")

for (s in seasons)
{
  games <- regular[regular$SEASON == s,]
  
  avgs <- c(avgs, mean(games$HPTS))
}

barplot(avgs, ylim=c(0, 150), main="Povprečo število točk za vasko sezono", names.arg=seasons)

#####
# Izris pvoprečnega števila doseženih točk gostujočih na tekmo v posamezni sezoni
#

avgs <- vector()
seasons <- c("2014-15", "2015-16", "2016-17")

for (s in seasons)
{
  games <- regular[regular$SEASON == s,]
  
  avgs <- c(avgs, mean(games$APTS))
}

barplot(avgs, ylim=c(0, 150), main="Povprečo število točk za vasko sezono", names.arg=seasons)

#####
# Izris pvoprečnega števila doseženih trojk na tekmo v posamezni sezoni
#

avgs <- vector()
seasons <- c("2014-15", "2015-16", "2016-17")

for (s in seasons)
{
  games <- regular[regular$SEASON == s,]
  
  avgs <- c(avgs, mean(games$A3PM + games$H3PM))
}

barplot(avgs, ylim=c(0, 25), main="Povprečo število trojk za vasko sezono", names.arg=seasons)

#####
# Najuspešnejše ekipe glede na razmerje zmag in porazov
#

teams <- vector()
ratings <- vector()
wins <- vector()

for(a in unique(regular$HOME)){
  
  teams <- c(teams, a)
}

for(t in teams){
  
  
  homeGames <- regular[regular$HOME == t,]
  homeWins <- c(homeGames$HPTS > homeGames$APTS)
  awayGames <- regular[regular$AWAY == t,]
  awayWins <- c(awayGames$APTS > awayGames$HPTS)
  
  wins <- c(homeWins, awayWins)
  ratings <- c(ratings, table(wins)["TRUE"] / (length(wins)))
}

barplot(width=c(1), space=0.5, cex.names = 0.75, xlim=c(0, 45), ylim=c(0, 1), col="black", ratings, main="Odstotki zmag", names.arg=teams)