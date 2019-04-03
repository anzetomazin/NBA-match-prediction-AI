
# Podatki o tekmah

games <- subset(regular, select=c("HOME", "HPTS", "AWAY", "APTS"))

games$HOMEWIN <- ifelse(games$HPTS > games$APTS, T, F)

games$HOMEWIN <- as.factor(games$HOMEWIN)

##### 
#Priprava učne množice za klasifikacijo
#

gamesClass <- data.frame(HOME = character(), HPTS = numeric(), H2PM = numeric(), H3PM = numeric(), HFTM = numeric(), HORB = numeric(), HDBR = numeric(), HAST = numeric(), HSTL = numeric(), HTOV = numeric(), HBLK = numeric(), HPF = numeric(), 
                         AWAY = character(), APTS = numeric(), A2PM = numeric(), A3PM = numeric(), AFTM = numeric(), AORB = numeric(), ADBR = numeric(), AAST = numeric(), ASTL = numeric(), ATOV = numeric(), ABLK = numeric(), APF = numeric(),
                         HOMEWIN = logical(), stringsAsFactors = FALSE)

for(row in 48:nrow(games)){
  
  home <- as.character(games[row, "HOME"])
  away <- as.character(games[row, "AWAY"])
  homeWin <- games[row, "HOMEWIN"]
  
  aggHome <- aggregate(cbind(HPTS,H2PM,H3PM,HFTM,HORB,HDRB,HAST,HSTL,HTOV,HBLK,HPF) ~ HOME, data = subset(seasons[1:row-1,], HOME == home, select = c(HOME,HPTS,H2PM,H3PM,HFTM,HORB,HDRB,HAST,HSTL,HTOV,HBLK,HPF)), FUN = mean)
  
  aggAway <- aggregate(cbind(APTS,A2PM,A3PM,AFTM,AORB,ADRB,AAST,ASTL,ATOV,ABLK,APF) ~ AWAY, data = subset(seasons[1:row-1,], AWAY == away, select = c(AWAY,APTS,A2PM,A3PM,AFTM,AORB,ADRB,AAST,ASTL,ATOV,ABLK,APF)), FUN = mean)
  
  gamesClass[nrow(gamesClass)+1, ] <- c(home, aggHome[1,2:12], away, aggAway[1,2:12], homeWin)
}

gamesClass$HOMEWIN <- as.factor(gamesClass$HOMEWIN)
gamesClass$HOME <- NULL
gamesClass$AWAY <- NULL

findAttribs <- function(h, a){
  gameAttribs <- data.frame(HOME = character(), HPTS = numeric(), H2PM = numeric(), H3PM = numeric(), HFTM = numeric(), HORB = numeric(), HDBR = numeric(), HAST = numeric(), HSTL = numeric(), HTOV = numeric(), HBLK = numeric(), HPF = numeric(), 
                            AWAY = character(), APTS = numeric(), A2PM = numeric(), A3PM = numeric(), AFTM = numeric(), AORB = numeric(), ADBR = numeric(), AAST = numeric(), ASTL = numeric(), ATOV = numeric(), ABLK = numeric(), APF = numeric(), 
                            stringsAsFactors = FALSE)
  
  aggHome <- aggregate(cbind(HPTS,H2PM,H3PM,HFTM,HORB,HDRB,HAST,HSTL,HTOV,HBLK,HPF) ~ HOME, data = subset(seasons, HOME == h, select = c(HOME,HPTS,H2PM,H3PM,HFTM,HORB,HDRB,HAST,HSTL,HTOV,HBLK,HPF)), FUN = mean)
    
  aggAway <- aggregate(cbind(APTS,A2PM,A3PM,AFTM,AORB,ADRB,AAST,ASTL,ATOV,ABLK,APF) ~ AWAY, data = subset(seasons, AWAY == a, select = c(AWAY,APTS,A2PM,A3PM,AFTM,AORB,ADRB,AAST,ASTL,ATOV,ABLK,APF)), FUN = mean)
    
  gameAttribs[nrow(gameAttribs)+1, ] <- c(aggHome[1,1:12], aggAway[1,1:12])
  
  gameAttribs$HOME <- NULL
  gameAttribs$AWAY <- NULL
  return(gameAttribs)
}

learn <- gamesClass[1:as.integer(nrow(gamesClass)*0.7),]
test <- gamesClass[as.integer(nrow(gamesClass)*0.7):nrow(gamesClass),]

observed <- test$HOMEWIN

# Klasifikacijska točnost

obsMat <- model.matrix(~HOMEWIN-1, test)

# Funkcija za izracun klasifikacijske tocnosti
CA <- function(observed, predicted)
{
  t <- table(observed, predicted)
  
  sum(diag(t)) / sum(t)
}

# Funkcija za izracun Brierjeve mere
brier.score <- function(observedMatrix, predictedMatrix)
{
  sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}

# metoda precnega preverjanja je implementirana v knjiznici "ipred"
library(ipred)

# pomozne funkcije, ki jih potrebujemo za izvedbo precnega preverjanja
mypredict.generic <- function(object, newdata){predict(object, newdata, type = "class")}
mymodel.coremodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}
mypredict.coremodel <- function(object, newdata) {pred <- predict(object, newdata)$class; destroyModels(object); pred}

#####
#Večinski klasifikator
#

which.max(table(learn$HOMEWIN))

majority.class <- names(which.max(table(learn$HOMEWIN)))

sum(test$HOMEWIN == majority.class) / length(test$HOMEWIN)

#####
#Odločitveno drevo (CORElearn)
#

library(CORElearn)
cm.dt <- CoreModel(HOMEWIN ~ ., data = learn, model="tree")
plot(cm.dt, learn)
predicted <- predict(cm.dt, test, type="class")
CA(observed, predicted)

predMat <- predict(cm.dt, test, type = "probability")
brier.score(obsMat, predMat)

errorest(HOMEWIN~., data=learn, model = mymodel.coremodel, predict = mypredict.coremodel, target.model="tree")

#####
#Odločitveno drevo (rpart)
#

library(rpart)
dt <- rpart(HOMEWIN ~ ., data = learn)
plot(dt);text(dt)
predicted <- predict(dt, test, type="class")
CA(observed, predicted)

predMat <- predict(dt, test, type = "prob")
brier.score(obsMat, predMat)

errorest(HOMEWIN~., data=learn, model = rpart, predict = mypredict.generic)

#####
# Konkreten primer
#

attribs <- findAttribs("PHI", "GSW")
library(rpart)
dt <- rpart(HOMEWIN ~ ., data = learn)
plot(dt);text(dt)
predicted <- predict(dt, attribs, type="class")
predicted # Zmagovalec: 1 -> AWAY, 2 -> HOME

#####
#Naivni Bayes
#

library(CORElearn)
cm.nb <- CoreModel(HOMEWIN ~ ., data = learn, model="bayes")
predicted <- predict(cm.nb, test, type="class")
CA(observed, predicted)

predMat <- predict(cm.nb, test, type = "probability")
brier.score(obsMat, predMat)

errorest(HOMEWIN~., data=learn, model = mymodel.coremodel, predict = mypredict.coremodel, target.model="bayes")

#####
#K-najbližnjih sosedov
#

library(CORElearn)
cm.knn <- CoreModel(HOMEWIN ~ ., data = learn, model="knn", kInNN = 9)
predicted <- predict(cm.knn, test, type="class")
CA(observed, predicted)

predMat <- predict(cm.knn, test, type = "probability")
brier.score(obsMat, predMat)

errorest(HOMEWIN~., data=learn, model = mymodel.coremodel, predict = mypredict.coremodel, target.model="knn")

#####
#Naključni gozd
#

library(CORElearn)
cm.rf <- CoreModel(HOMEWIN ~ ., data = learn, model="rf")
predicted <- predict(cm.rf, test, type="class")
CA(observed, predicted)

predMat <- predict(cm.rf, test, type = "probability")
brier.score(obsMat, predMat)

errorest(HOMEWIN~., data=learn, model = mymodel.coremodel, predict = mypredict.coremodel, target.model="rf")

#####
#SVM
#

library(e1071)

sm <- svm(HOMEWIN ~ ., data = learn)
predicted <- predict(sm, test, type="class")
CA(observed, predicted)

sm <- svm(HOMEWIN ~ ., learn, probability = T)
pred <- predict(sm, test, probability = T)
predMat <- attr(pred, "probabilities")

brier.score(obsMat, predMat[,c(2,1)])

errorest(HOMEWIN~., data=learn, model = svm, predict = mypredict.generic)

##### 
#Umetne nevronske mreže
#

library(nnet)

scale.data <- function(data)
{
  norm.data <- data
  
  for (i in 1:ncol(data))
  {
    if (!is.factor(data[,i]))
      norm.data[,i] <- scale(data[,i])
  }
  
  norm.data
}

norm.data <- scale.data(rbind(learn,test))
norm.learn <- norm.data[1:nrow(learn),]
norm.test <- norm.data[-(1:nrow(learn)),]

nn <- nnet(HOMEWIN ~ ., data = norm.learn, size = 5, decay = 0.0001, maxit = 10000)
predicted <- predict(nn, norm.test, type = "class")
CA(observed, predicted)

pm <- predict(nn, norm.test, type = "raw")
predMat <- cbind(1-pm, pm)
brier.score(obsMat, predMat)

mypredict.nnet <- function(object, newdata){as.factor(predict(object, newdata, type = "class"))}
errorest(HOMEWIN~., data=norm.learn, model = nnet, predict = mypredict.nnet, size = 5, decay = 0.0001, maxit = 10000)

#####
#Glasovanje
#

modelDT <- CoreModel(HOMEWIN ~ ., learn, model="tree")
modelNB <- CoreModel(HOMEWIN ~ ., learn, model="bayes")
modelKNN <- CoreModel(HOMEWIN ~ ., learn, model="knn", kInNN = 5)

predDT <- predict(modelDT, test, type = "class")
caDT <- CA(test$HOMEWIN, predDT)
caDT

predNB <- predict(modelNB, test, type="class")
caNB <- CA(test$HOMEWIN, predNB)
caNB

predKNN <- predict(modelKNN, test, type="class")
caKNN <- CA(test$HOMEWIN, predKNN)
caKNN

# zdruzimo napovedi posameznih modelov v en podatkovni okvir
pred <- data.frame(predDT, predNB, predKNN)
pred

# testni primer klasificiramo v razred z najvec glasovi
voting <- function(predictions)
{
  res <- vector()
  
  for (i in 1 : nrow(predictions))  	
  {
    vec <- unlist(predictions[i,])
    res[i] <- names(which.max(table(vec)))
  }
  
  factor(res, levels=levels(predictions[,1]))
}

predicted <- voting(pred)
CA(test$HOMEWIN, predicted)