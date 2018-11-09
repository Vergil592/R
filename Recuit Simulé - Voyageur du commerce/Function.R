Simul_Markov <- function(N, P, X0){
  E = seq(1,dim(P)[1],1)
  X = c()
  X[1] = X0
  for(i in 2:(N-1)){
    X[i] = sample(E,1,prob=P[X[i-1],])
  }
  return(X)
}

Estimation_matrice <- function(d, X){
  P_chap = matrix(0,ncol=d,nrow =d)
  for(x in 1:d){
    for (y in 1:d){
      # for (k in 1:(length(X)-1)){
      #   P_chap[x,y] = P_chap[x,y] + ((X[k+1]==y)*(X[k]==x))
      # }
      P_chap[x,y] = sum( (X[2:length(X)]==y) * (X[1:length(X)-1]==x) )
    }
    sum_x = sum(X==x)
    ## Si la chaine ne contient pas la valeur x
    if(sum_x == 0){
      P_chap[x,] = 0
    }else{
      P_chap[x,] = P_chap[x,] / sum(X==x)
    }
  }
  return(P_chap)
}

# Calcul Erreur entre P et P_chap
Calcul_Erreur <- function(P, P_chap){
  return(max(abs(P-P_chap)))
}

# Calcul erreur moyenne pour Nsim simulations et temps execution
## Returne un vecteur de la forme c(erreur_moy, tps_calcul)
Calcul_Erreur_Tpsexec_Nsim <- function(Nsim, N, P, X0){
  erreur = 0
  tpsexec = proc.time()[[3]]
  for(i in 1:Nsim){
    X = Simul_Markov(N, P, X0)
    P_chap = Estimation_matrice(dim(P)[1], X)
    erreur = erreur + Calcul_Erreur(P, P_chap)
  }
  tpsexec = proc.time()[[3]] - tpsexec
  erreur = erreur / Nsim
  return( c(erreur, tpsexec) )
}

# Tableau erreur et tps_exec pour une dimension d fixe
## Vect_N = vecteur contenant les les differentes tailles N
Tableau_Erreur_Tpsexec_Nsim <- function(Nsim, vect_N, P, X0){
  test = data.frame(N500=c(1,2,3,4), N1000=c(5,6,7,8), N5000=c(9,10,11,12), N10k=c(13,14,15,16), row.names=c("P$UNIF - Err moy","P$UNIF - Tps exec","P$PPV - Err moy","P$PPV - Tps exec"))
  for(i in 1:length(vect_N)){
    test[1:2,i] = Calcul_Erreur_Tpsexec_Nsim(Nsim, vect_N[i], P$UNIF, X0)
    test[3:4,i] = Calcul_Erreur_Tpsexec_Nsim(Nsim, vect_N[i], P$PPV, X0)
  }
  names(test) = c("N=500","N=1000","N=5000","N=1000")
  return(test)
}

# Renvoie le tableau final
## vect_d = vecteur contenant les les differentes dimmensions d
### Retourne une liste ou chaque elements de la liste correspond a une dimension
#### Un element de la liste contient le tableau (erreur_moy,tps_exec) pour les differents N
Resultats <- function(vect_d, vect_N ,Nsim){
  result = list()
  for(i in 1:length(vect_d)){
    X0 = sample(seq(1,vect_d[i]),1,prob=rep(1/vect_d[i],vect_d[i]))
    # On creer la matrice P_UNIF et P_PPV
    P_voisin = diag(1/3,vect_d[i])
    P_voisin[1,vect_d[i]] = 1/3
    P_voisin[vect_d[i],1] = 1/3
    for(k in 2:(vect_d[i]-1)){
      P_voisin[k-1,k]=1/3
      P_voisin[k,k-1]=1/3
      P_voisin[k+1,k]=1/3
      P_voisin[k,k+1]=1/3
    }
    P = list(UNIF=matrix(1/vect_d[i],nrow=vect_d[i],ncol = vect_d[i]), PPV=P_voisin)
    ##On met les resultats dans une liste pour chaque d
    result[[i]] = Tableau_Erreur_Tpsexec_Nsim(Nsim, vect_N, P, X0)
  }
  return(result)
}

# Graphioque comparatif
Graphe_tab_final <- function(tab_final, vect_N){
  par(mfrow=c(2,2))
  ## pour err_moye P_UNIF et P_PPV  d=5
  plot(vect_N, tab_final[[1]][1,],main = "Erreur moyenne pour P$UNIF et P$PPV, d=5" , col="red",type = "l", ylab = "erreur moyenne")
  lines(vect_N, tab_final[[1]][3,], col="blue")
  ## pour tps_exec P_UNIF et P_PPV  d=5
  plot(vect_N, tab_final[[1]][2,],main = "Temps execution pour P$UNIF et P$PPV, d=5" ,col="red",type = "l", ylab = "temps execution")
  lines(vect_N, tab_final[[1]][4,], col="blue")
  
  ## pour err_moye P_UNIF et P_PPV  d=10
  plot(vect_N, tab_final[[2]][1,],main = "Erreur moyenne pour P$UNIF et P$PPV, d=10",col="red",type = "l", ylab = "erreur moyenne")
  lines(vect_N, tab_final[[2]][3,], col="blue")
  ## pour tps_exec P_UNIF et P_PPV  d=10
  plot(vect_N, tab_final[[2]][2,],main = "Temps execution pour P$UNIF et P$PPV, d=10",col="red",type = "l", ylab = "temps execution")
  lines(vect_N, tab_final[[2]][4,], col="blue")
  
  par(mfrow=c(1,1))
}