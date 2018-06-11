# Partitionnement spectral
require(igraph)
require(ggplot2)

# IRM
require(jpeg)
img <- readJPEG("irm_small.jpeg")

# Matrice de similaritee
# On transforme d'abord l'image, stockee sous forme de matrice, en vecteur
img_vect <- as.vector(img)
d <- as.matrix(dist(img_vect))

# On choisit sigma=0.5 mais on peut tester d'autres valeurs
W <- exp(-d^2/(2*0.5^2))

# Graphe du epsilon-voisinage
seuil <- quantile(W,0.75)
W_eps <- W
W_eps[W<seuil] <- 0
W_eps[W>=seuil] <- 1

# matrice des degree
Deg_eps <- apply(W_eps, 1, sum)
# degree moyen
mean(Deg_eps) 

# Matrice Laplacienne normalisee symetrique
Lapl_eps <- diag(length(img_vect)) - diag(1/sqrt(Deg_eps)) %*% W_eps %*%  diag(1/sqrt(Deg_eps))

# Spectre
spec_eps <- eigen(Lapl_eps,symmetric=TRUE)

# Plot
n <- length(spec_eps$values)
ggplot(data=data.frame(values=spec_eps$values[(n-9):n],order=10:1),aes(x=order,y=values)) + geom_point() + ggtitle("Graphe du epsilon voisinage")
# Sur le graphe precedent, 
# On voit un saut entre la 3e et la 4e valeur propre, on retient donc 3 classes

# Matrice des 3 premiers vecteurs propres
U_eps <- spec_eps$vectors[,(n-2):n]

# Normalisation par lignes
row_norm <- function(v){
  return(v/sqrt(sum(v^2)))
}

V_eps <- t(apply(U_eps,1,row_norm))

# k-means sur les lignes 
clust_eps <- kmeans(V_eps,3)

mat_clust <- matrix(clust_eps$cluster,nr=nrow(img),byrow = F)

# Plots
image(img,col=grey((0:100)/100)) # image originale
image(mat_clust) # clusters

# Pour superposer les clusters les uns apres les autres
cl1 <- mat_clust
cl1[cl1!=1] <- NA
cl2 <- mat_clust
cl2[cl2!=2] <- NA
cl3 <- mat_clust
cl3[cl3!=3] <- NA

image(img,col=grey((0:100)/100))
image(cl1,col="blue",add=T)
image(cl2,col="orange",add=T)
image(cl3,col="yellow",add=T)
