# AFD

library(MASS)

# Importing Dataset
insect <- read.table("insectes.txt")

summary(insect) 
# 21 obs in class A, 22 obs class in B et 31 class C
apply(insect,2,sd) # variable la plus dispersee : V1, la moins dispersee : V5

# Centers
g_classes <- sapply(c("A","B","C"),FUN = function(char){apply(insect[insect$V7==char,1:6],2,mean)})
g_tot <- apply(insect[,1:6],2,mean)
print("centers for each var : ")
g_tot


# With MASS package
require(MASS)
afd <- lda(V7~.-V7,data=insect,scores=TRUE)

# With ade4 package
# you have to select the number of axes
require(ade4)
afd2 <- discrimin(dudi.pca(insect[,1:6]),insect$V7)
plot(afd2)

afd2$eig

# Split in train-test
rows <- sample(1:74,30,replace = FALSE)
train <- insect[rows,]
test <- insect[-rows,]

afd_app <- lda(V7~.-V7,data=train,scores=TRUE)
class_test <- predict(afd_app,test)

table(class_test$class,test$V7)