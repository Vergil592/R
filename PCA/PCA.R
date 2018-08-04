## PCA

require(jpeg)
img <- readJPEG("joconde.jpg")

# Plot image
plot(1:2,type="n", asp = 1)
rasterImage(img,1.2,1.27,1.75,1.73)

img1 <- img[,,1]
img2 <- img[,,2]
img3 <- img[,,3]

pca.img1 <- prcomp(img1, retx = TRUE)
pca.img2 <- prcomp(img2, retx = TRUE)
pca.img3 <- prcomp(img3, retx = TRUE)

# Eigen values foreach color images
par(mfrow=c(2,2))
plot(pca.img1)
plot(pca.img2)
plot(pca.img3)
par(mfrow=c(1,1))

summary(pca.img1)

eig_numb <- 23

reconstimg1 = pca.img1$rotation[,1] %*% t(pca.img1$x[,1])
reconstimg2 = pca.img2$rotation[,1] %*% t(pca.img2$x[,1])
reconstimg3 = pca.img3$rotation[,1] %*% t(pca.img3$x[,1])
for(i in 1:eig_numb){
  reconstimg1 <- reconstimg1 + pca.img1$rotation[,i] %*% t(pca.img1$x[,i])
  reconstimg2 <- reconstimg2 + pca.img2$rotation[,i] %*% t(pca.img2$x[,i])
  reconstimg3 <- reconstimg3 + pca.img3$rotation[,i] %*% t(pca.img3$x[,i])
}
reconstimg1 <- t(reconstimg1)
reconstimg2 <- t(reconstimg2)
reconstimg3 <- t(reconstimg3)

reconstIMG=img
reconstIMG[,,1] = reconstimg1
reconstIMG[,,2] = reconstimg2
reconstIMG[,,3] = reconstimg3

# Image matrix must have values between 0 and 1
reconstIMG[reconstIMG<0]=0
reconstIMG[reconstIMG>1]=1

# Image with PCA
plot(1:2,type="n", asp = 1)
rasterImage(reconstIMG,1.2,1.27,1.75,1.73)
