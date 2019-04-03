# REGRESIJA

rgames <- subset(regular, select=c("HOME", "HPTS", "AWAY", "APTS"))

rgames$HOMESD <- rgames$HPTS - rgames$APTS

rgames$HOMESD <- rgames$HOMESD

#####
#Priprava množice za regresijo
#

gamesReg <- data.frame(HOME = character(), HPTS = numeric(), H2PM = numeric(), H3PM = numeric(), HFTM = numeric(), HORB = numeric(), HDBR = numeric(), HAST = numeric(), HSTL = numeric(), HTOV = numeric(), HBLK = numeric(), HPF = numeric(), 
                      AWAY = character(), APTS = numeric(), A2PM = numeric(), A3PM = numeric(), AFTM = numeric(), AORB = numeric(), ADBR = numeric(), AAST = numeric(), ASTL = numeric(), ATOV = numeric(), ABLK = numeric(), APF = numeric(),
                      HOMESD = logical(), stringsAsFactors = FALSE)

for(row in 48:nrow(rgames)){
  
  home <- as.character(rgames[row, "HOME"])
  away <- as.character(rgames[row, "AWAY"])
  homesd <- rgames[row, "HOMESD"]
  
  aggHome <- aggregate(cbind(HPTS,H2PM,H3PM,HFTM,HORB,HDRB,HAST,HSTL,HTOV,HBLK,HPF) ~ HOME, data = subset(seasons[1:row-1,], HOME == home, select = c(HOME,HPTS,H2PM,H3PM,HFTM,HORB,HDRB,HAST,HSTL,HTOV,HBLK,HPF)), FUN = mean)
  
  aggAway <- aggregate(cbind(APTS,A2PM,A3PM,AFTM,AORB,ADRB,AAST,ASTL,ATOV,ABLK,APF) ~ AWAY, data = subset(seasons[1:row-1,], AWAY == away, select = c(AWAY,APTS,A2PM,A3PM,AFTM,AORB,ADRB,AAST,ASTL,ATOV,ABLK,APF)), FUN = mean)
  
  gamesReg[nrow(gamesReg)+1, ] <- c(home, aggHome[1,2:12], away, aggAway[1,2:12], homesd)
}

gamesReg$HOME <- NULL
gamesReg$AWAY <- NULL

learn <- gamesReg[1:as.integer(nrow(gamesReg)*0.7),]
test <- gamesReg[as.integer(nrow(gamesReg)*0.7):nrow(gamesReg),]

observed <- test$HOMESD


#####
# Mere za ocenjevanje ucenja v regresiji
#

mae <- function(observed, predicted)
{
  mean(abs(observed - predicted))
}

rmae <- function(observed, predicted, mean.val) 
{  
  sum(abs(observed - predicted)) / sum(abs(observed - mean.val))
}

mse <- function(observed, predicted)
{
  mean((observed - predicted)^2)
}

rmse <- function(observed, predicted, mean.val) 
{  
  sum((observed - predicted)^2)/sum((observed - mean.val)^2)
}

#####
# LINEARNI MODEL
#

lm.model <- lm(HOMESD ~ ., data = learn)
lm.model

predicted <- predict(lm.model, test)
mae(observed, predicted)
rmae(observed, predicted, mean(learn$HOMESD))

mse(observed, predicted)
rmse(observed, predicted, mean(learn$HOMESD))

#####
#DREVO
#

library(rpart)

rt.model <- rpart(HOMESD ~ ., learn)
predicted <- predict(rt.model, test)
rmae(observed, predicted, mean(learn$HOMESD))

plot(rt.model);text(rt.model, pretty = 0)


# nastavitve za gradnjo drevesa
rpart.control()

# zgradimo drevo z drugimi parametri
rt <- rpart(HOMESD ~ ., learn, minsplit = 100)
plot(rt);text(rt, pretty = 0)

# parameter cp kontrolira rezanje drevesa
rt.model <- rpart(HOMESD ~ ., learn, cp = 0)
plot(rt.model);text(rt.model, pretty = 0)


# izpisemo ocenjene napake drevesa za razlicne vrednosti parametra cp
printcp(rt.model)

# drevo porezemo z ustrezno vrednostjo cp, pri kateri je bila minimalna napaka
rt.model2 <- prune(rt.model, cp = 0.01)
plot(rt.model2);text(rt.model2, pretty = 0)
predicted <- predict(rt.model2, test)
mae(observed, predicted)
rmae(observed, predicted, mean(learn$HOMESD))
mse(observed, predicted)
rmse(observed, predicted, mean(learn$HOMESD))

#####
# nakljucni gozd
#

library(randomForest)

rf.model <- randomForest(HOMESD ~ ., learn)
predicted <- predict(rf.model, test)
mae(observed, predicted)
rmae(observed, predicted, mean(learn$HOMESD))
mse(observed, predicted)
rmse(observed, predicted, mean(learn$HOMESD))

#####
# Primer
#

attribsReg <- findAttribs("GSW", "LAL")
rf.model <- randomForest(HOMESD ~ ., learn)
predicted <- predict(rf.model, attribsReg)
predicted

#####
# svm
#

library(e1071)

svm.model <- svm(HOMESD ~ ., learn)
predicted <- predict(svm.model, test)
mae(observed, predicted)
rmae(observed, predicted, mean(learn$HOMESD))
mse(observed, predicted)
rmse(observed, predicted, mean(learn$HOMESD))

#####
# k-najblizjih sosedov
#
library(kknn)

knn.model <- kknn(HOMESD ~ ., learn, test, k = 100)
predicted <- fitted(knn.model)
mae(observed, predicted)
rmae(observed, predicted, mean(learn$HOMESD))

mse(observed, predicted)
rmse(observed, predicted, mean(learn$HOMESD))

#####
# nevronska mreža
#

library(nnet)

nn.model <- nnet(HOMESD ~ ., learn, size = 5, decay = 0.0001, maxit = 10000, linout = T)
predicted <- predict(nn.model, test)
mae(observed, predicted)
rmae(observed, predicted, mean(learn$HOMESD))

mse(observed, predicted)
rmse(observed, predicted, mean(learn$HOMESD))