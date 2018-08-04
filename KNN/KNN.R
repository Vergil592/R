# KNN

# KNN function
knn <- function(k,train,test,class){
  n <- nrow(test)
  predict <- rep(0,n)
  proba <- rep(0,n)
  
  train <- as.matrix(train)
  test <- as.matrix(test)
  
  ncl <- max(unclass(class))
  
  probs <- t(sapply(1:n,FUN = function(i){
    dist <- apply(train,1,FUN = function(x){sqrt(sum((x-test[i,])^2))})
    d_max <- sort(dist)[k] 
    temp <- as.data.frame(cbind(train,class,dist))
    dk <- temp[temp$dist <= d_max,]
    
    # Remarque : on peut aussi utiliser la fonction "order" qui renvoie les indices des éléments rangés par ordre croissant
    # Par exemple, on ferait :
    # indices_ordonnes <- order(dist)
    # class_kppv <- class[indices_ordonnes[1:k]]
    
    proba <- numeric()
    for (c in 1:ncl)
    {
      proba <- c(proba,mean(dk$class==c))
    }
    return(proba)}))
  
  classement <- apply(probs,1,FUN = function(x){y<-which(x==max(x));if(length(y)>1){y<-sample(y,1,replace = FALSE)};return(y)})
  
  predict <- cbind(test,probs,classement)
  
  return(predict)
}


# Exercice 2
wines <- read.csv("Wines.csv")

require(reshape2)
d_long <- melt(wines,id.vars=c("Classe"))

# Option: "facet_wrap" to crate grid for each boxplot
# Option: "scales=free"
require(ggplot2)
p <- ggplot(data=d_long,aes(x=as.factor(Classe),y=value,fill=as.factor(Classe))) + geom_boxplot() + facet_wrap(~variable,scales="free")
p + theme(legend.position = "none")

# Split Test and apprentissage
N <- nrow(wines)
ntrain <- floor(0.75*N)
ntest <- N - ntrain

indices <- sample(1:N,ntrain,replace = FALSE)

wines_train <- wines[indices,]
wines_test <- wines[-indices,]

# standardization
wines_train <- data.frame(Classe=wines_train$Classe,scale(wines_train[,-1]))
apply(wines_train,2,sd)
wines_test <- data.frame(Classe=wines_test$Classe,scale(wines_test[,-1]))

# weighted knn
distance <- function(x,y,nom,q=1){
  if (nom == "euclidienne")
  {
    distance <- sqrt(sum((x-y)^2))
  }else if (nom == "minkowski")
  {
    distance <- sum((x-y)^q)^(1/q)
  }else if (nom == "max")
  {
    distance <- max(abs(x-y))
  }else if (nom == "manhattan")
  {
    distance <- sum(abs(x-y))
  }else if (nom == "canberra")
  {
    distance <- sum( (abs(x-y))/(abs(x) + abs(y)) )
  }else
  {
    print("Nom de distance incorrect")
  }
}

noyau <- function(x,nom)
{
  if (nom == "rect"){noyau <- 0.5*(-1 <= x & x <= 1)}
  if (nom == "triang"){noyau <- (1-abs(x))*(-1 <= x & x <= 1)}
  if (nom == "epan"){noyau <- 0.75*(1-x^2)*(-1 <= x & x <= 1)}
  if (nom == "bipoids"){noyau <- (15/16)*(1-x^2)^2*(-1 <= x & x <= 1)}
  if (nom == "tripoids"){noyau <- (35/32)*(1-x^2)^3*(-1 <= x & x <= 1)}
  if (nom == "cos"){noyau <- (pi/4)*cos((pi/2)*x)*(-1 <= x & x <= 1)}
  if (nom == "gauss"){noyau <- dnorm(x)}
  if (nom == "inv"){noyau <- 1/abs(x)}
  if (nom == "exp"){noyau <- (1/2)*exp(-abs(x))}
  return(noyau)
}

knn_pond <- function(k,noyau_name,dist_name,train,test,class){
  n <- nrow(test)
  predict <- rep(0,n)
  proba <- rep(0,n)
  
  train <- as.matrix(train)
  test <- as.matrix(test)
  
  ncl <- max(unclass(class))
  
  affectation <- t(sapply(1:n,FUN = function(i){
    dist <- apply(train,1,FUN = function(x){sqrt(sum((x-test[i,])^2))})
    d_max <- sort(dist)[k] 
    temp <- as.data.frame(cbind(train,class,dist))
    dk <- temp[temp$dist <= d_max,]
    
    if(noyau_name %in% c("rect","triang","epan","bipoids","tripoids","cos")) # on doit renormaliser les distances
    {
      dk$dist <- dk$dist/temp$dist[k+1]
    }
    weight <- noyau(dk$dist,noyau_name)    
    
    proba <- numeric()
    for (c in 1:ncl)
    {
      proba <- c(proba,mean(dk$class==c))
    }
    return(proba)}))  
  
  classement <- apply(affectation,1,FUN = function(x){y<-which(x==max(x));if(length(y)>1){y<-sample(y,1,replace = FALSE)};return(y)})
  predict <- cbind(test,affectation,classement)
  
  return(predict)
}

# Apprentissage et test
x_train <- wines_train[,-1]
labels_train <- as.factor(wines_train$Classe)
x_test <- wines_test[,-1]
labels_test <- as.factor(wines_test$Classe)

res <- as.data.frame(knn_pond(k=10,noyau_name = "gauss",dist_name = "max",train = x_train,test=x_test,class = labels_train))
table(res$classement,labels_test)

# cross-validation
N <- nrow(wines_train)
K <- c(1,seq(5,130,5))
err <- rep(0,length(K))

for (k in 1:length(K))
{
  print(paste("k = ",K[k]))
  for (i in 1:N)
  {
    #print(paste("i = ",i))
    x_train <- wines_train[-i,-1]
    labels_train <- as.factor(wines_train[-i,1])
    x_test <- wines_train[i,-1]
    labels_test <- as.factor(wines_train[i,1])
    
    res <- as.data.frame(knn_pond(k=K[k],noyau_name = "gauss",dist_name = "max",train = x_train,test=x_test,class = labels_train))
    
    err[k] <- err[k] + mean(labels_test!=res$classement)
  }
  err[k] <- err[k]/N
}

# Plotting err in function of K (number of groups)
plot(K,err,type="l")
k_opt <- which(na.omit(err)==min(na.omit(err)))
