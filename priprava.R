regular <- read.table(file = "regular.txt", sep = ",", header = TRUE)

seasons <- regular

seasons$SEASON <- NULL
seasons$DATE <- NULL

# Odstotki zadetih metov za 2 točki

seasons$H2PM <- seasons$H2PM / seasons$H2PA
seasons$H2PA <- NULL

seasons$A2PM <- seasons$A2PM / seasons$A2PA
seasons$A2PA <- NULL

# Odstotki zadetih metov za 3 točke

seasons$H3PM <- seasons$H3PM / seasons$H3PA
seasons$H3PA <- NULL

seasons$A3PM <- seasons$A3PM / seasons$A3PA
seasons$A3PA <- NULL

# Odstotki zadetih prostih metov

seasons$HFTM <- seasons$HFTM / seasons$HFTA
seasons$HFTA <- NULL

seasons$AFTM <- seasons$AFTM / seasons$AFTA
seasons$AFTA <- NULL

## Pripravi množice za klasifikacijo in regresijo sta v isti datoteki kot sami modeli