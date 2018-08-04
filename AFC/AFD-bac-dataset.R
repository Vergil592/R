## AFD

require("MASS")
data3 <- read.csv("bacdata.csv",header=TRUE, sep='\t')
dim(data3)

n <-sum(data3)

D1 = diag( rowSums(data3) )
D2 = diag( colSums(data3) )

M <- solve(D2)*n

VM = t(data3) %*% solve(D1) %*% as.matrix(data3) %*% solve(D2)

decompo_VM <- eigen(VM)

# Plot 
afc.data3 <- corresp(data3,nf=2)
plot(afc.data3)