source("Function.R")

## Variables to test the algorithm

# Number of simulation
Nsim = 100

# DImension of the Markov Chain
d = 5

N = 1000
X0 = sample(seq(1,d),1,prob=rep(1/d,d))

vect_d = c(5,10)
vect_N = c(500, 1000, 5000, 10000)

# KNN Matrix
P_voisin = diag(1/3,d)
P_voisin[1,d] = 1/3
P_voisin[d,1] = 1/3
for(k in 2:d-1){
  P_voisin[k-1,k]=1/3
  P_voisin[k,k-1]=1/3
  P_voisin[k+1,k]=1/3
  P_voisin[k,k+1]=1/3
}
P = list(UNIF=matrix(1/d,ncol = d,nrow=d,byrow = TRUE), PPV=P_voisin)


# Test
X = Simul_Markov(N, P$PPV, X0)
P_chap = Estimation_matrice(d, X)

# Error
Calcul_Erreur(P$PPV, P_chap)
Calcul_Erreur_Tpsexec_Nsim(Nsim, N, P$PPV, X0)

# Array of Error
Tableau_Erreur_Tpsexec_Nsim(Nsim, vect_N, P, X0)

tab_final <- Resultats(vect_d, vect_N ,Nsim)


Graphe_tab_final(tab_final,vect_N)
