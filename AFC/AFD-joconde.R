## AFD

img <- readJPEG("joconde.jpg")
plot(1:2,type="n")
rasterImage(img, 1.2,1.1,1.8,1.9)

img1 <- img[,,1]
img2 <- img[,,2]
img3 <- img[,,3]


reconstIMG=img
reconstIMG[,,1] = reconstimg1
reconstIMG[,,2] = reconstimg2
reconstIMG[,,3] = reconstimg3


n <-sum(img[,,1])

for (i in 1:3){ # i=1
  D1 = diag(rowSums(img[,,i]))
  D2 = diag(colSums(img[,,i]))
  M <- solve(D2)*n
  
  ## On regarde les vals prpres de VM
  VM = t(img[,,i]) %*% solve(D1) %*% as.matrix(img[,,i]) %*% solve(D2)
  decompo_VM <- eigen(VM)
  
  ##faire function regle Kaiser pour nbre vals propres
  nb_val_prop = 9
  
  for(k in 1:nb_val_prop){
    ## Ck_l coord dans nouvelle base
    Ck_l <- solve(D1) %*% img[,,i] %*% M %*% decompo_VM$vectors[,k]
    reconstIMG[,,i] <- reconstIMG[,,i] + t( decompo_VM$vectors[,k] %*% t(Ck_l) )
  }
}



plot(1:2,type="n")
reconstIMG[reconstIMG<0] = 0
reconstIMG[reconstIMG>1] = 1
rasterImage(reconstIMG, 1.2,1.1,1.8,1.9)




decompo_VM <- eigen(VM)
## Regle Kaiser = 40 vals propres
##On garde  vals prorpres

decompo_VM$values[2:40]


C1_l <- solve(D1) %*% img[,,i] %*% M %*% decompo_VM$vectors[,1]
C2_l <- solve(D1) %*% img[,,i] %*% M %*% decompo_VM$vectors[,2]


##Formule pour compo principales (en ligne)



reconstIMG[reconstIMG<0]=0
reconstIMG[reconstIMG>1]=1
for(i in 2:9){
  reconstimg1 <- reconstimg1 + t( pca.img1$rotation[,i] %*% t(pca.img1$x[,i]) )
  reconstimg2 <- reconstimg2 + t( pca.img2$rotation[,i] %*% t(pca.img2$x[,i]) )
  reconstimg3 <- reconstimg3 + t( pca.img3$rotation[,i] %*% t(pca.img3$x[,i]) )
}
reconstIMG=img
reconstIMG[,,1] = reconstimg1
reconstIMG[,,2] = reconstimg2
reconstIMG[,,3] = reconstimg3

