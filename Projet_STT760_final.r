############################### Données ########################################
#variables
dimnames <- list(c(), c("v1", "v2", "v3", "v4", "v5", "v6",
                        "v7","v8","v9","v10"))
#Simulation des donn�es du premier mod�le  
student<- function(rows){ # returns an example data frame with rows numbers of observations
  cols <- 10
  coh <- c(1,0)
  niv <- c(1,0)
  diff <- c(1,0) 
  eetl <- c(1,0)
  bo <- c(1,0)
  intell <- c(1,0)
  noteetmt <- c(1,0)
  #dimnames <- list(c(), c("v1", "v2", "v3", "v4", "v5", "v6",
  #                        "v7","v8","v9","v10"))
  studentMatrix <- matrix(, rows, cols, dimnames = dimnames)
  student <- data.frame(studentMatrix, check.names = FALSE)
  student$v9 <- sample(coh, rows, replace = TRUE, prob = c(0.85, 0.15))
  student$v2 <- sample(intell, rows, replace = TRUE, prob = c(0.6, 0.4))
  student$v10 <- sample(noteetmt, rows, replace = TRUE, prob = c(0.3, 0.7))
  student$v7 <- sample(niv, rows, replace = TRUE, prob = c(0.95, 0.05))
  n <- 1
  for (i in student$v2) {
    #v4
    if ( student$v10[n]==noteetmt[1] ) 
      student$v4[n] <- sample(noteetmt, 1, replace = TRUE, prob = c(0.4, 0.6))
    if ( student$v10[n]==noteetmt[2] ) 
      student$v4[n] <- sample(noteetmt, 1, replace = TRUE, prob = c(0.2, 0.8))
    
    #v1
    if ( student$v9[n] == noteetmt[1] && student$v7[n] == niv[1] )
      student$v1[n] <- sample(diff, 1, replace = TRUE,prob = c(0.45, 0.55)) #
    if ( student$v9[n] == noteetmt[1] && student$v7[n] == niv[2] )
      student$v1[n] <- sample(diff, 1, replace = TRUE,prob = c(0.6, 0.4)) #
    if ( student$v9[n] == noteetmt[2] && student$v7[n] == niv[1] )
      student$v1[n] <- sample(diff, 1, replace = TRUE,prob = c(0.2, 0.8)) #
    if ( student$v9[n] == noteetmt[2] && student$v7[n] == niv[2] )
      student$v1[n] <- sample(diff, 1, replace = TRUE,prob = c(0.15, 0.85)) #
    
    #v3
    if ( student$v1[n] == diff[1] && i == intell[1])
      student$v3[n] <- sample(noteetmt, 1, replace = TRUE,prob = c(0.85, 0.15))
    if ( student$v1[n] == diff[1] && i == intell[2])
      student$v3[n] <- sample(noteetmt, 1, replace = TRUE,prob = c(0.95, 0.05))
    if ( student$v1[n] == diff[2] && i == intell[1])
      student$v3[n] <- sample(noteetmt, 1, replace = TRUE,prob = c(0.2, 0.8))
    if ( student$v1[n] == diff[2] && i == intell[2])
      student$v3[n] <- sample(noteetmt, 1, replace = TRUE,prob = c(0.5, 0.5))
    
    #v5
    
    if (student$v7[n] == niv[1] )
      student$v5[n] <- sample(eetl, 1, replace = TRUE,prob = c(1, 0))
    
    if ( student$v7[n] == niv[2] )
      student$v5[n] <- sample(eetl, 1, replace = TRUE,prob = c(0.95, 0.05))

    #v6
    if ( student$v4[n] == noteetmt[1] )
      student$v6[n] <- sample(eetl, 1, replace = TRUE,prob = c(0.95,0.05))
    if ( student$v4[n] == noteetmt[2] )
      student$v6[n] <- sample(eetl, 1, replace = TRUE,prob = c(0.9, 0.1))
  
    #v8
    if ( student$v3[n] == noteetmt[1] && student$v6[n] == eetl[1] )
      student$v8[n] <- sample(bo, 1, replace = TRUE,prob = c(1,0))
    if ( student$v3[n] == noteetmt[1] && student$v6[n] == eetl[2] )
      student$v8[n] <- sample(bo, 1, replace = TRUE,prob = c(0.75, 0.25))
    if ( student$v3[n] == noteetmt[2] && student$v6[n] == eetl[1] )
      student$v8[n] <- sample(bo, 1, replace = TRUE,prob = c(0.9, 0.1))
    if ( student$v3[n] == noteetmt[2] && student$v6[n] == eetl[2] )
      student$v8[n] <- sample(bo, 1, replace = TRUE,prob = c(0.4, 0.6))
    
    n <- n+1
  }
  return (student)
}
#Simulation des donn�es du deuxi�me mod�le  
student1<- function(rows){ # returns an example data frame with rows numbers of observations
  cols <- 10
  coh <- c(1,0)
  niv <- c(1,0)
  diff <- c(1,0) 
  eetl <- c(1,0)
  bo <- c(1,0)
  intell <- c(1,0)
  noteetmt <- c(1,0)
  #dimnames <- list(c(), c("v1", "v2", "v3", "v4", "v5", "v6",
  #                        "v7","v8","v9","v10"))
  studentMatrix <- matrix(, rows, cols, dimnames = dimnames)
  student1 <- data.frame(studentMatrix, check.names = FALSE)
  student1$v9 <- sample(coh, rows, replace = TRUE, prob = c(0.85, 0.15))
  student1$v2 <- sample(intell, rows, replace = TRUE, prob = c(0.6, 0.4))
  student1$v10 <- sample(noteetmt, rows, replace = TRUE, prob = c(0.3, 0.7))
  student1$v7 <- sample(niv, rows, replace = TRUE, prob = c(0.95, 0.05))
  n <- 1
  for (i in student1$v2) {
    #v4
    if (i == intell[1] && student1$v10[n]==noteetmt[1] ) 
      student1$v4[n] <- sample(noteetmt, 1, replace = TRUE, prob = c(0.4, 0.6))
    if (i == intell[1] && student1$v10[n]==noteetmt[2] ) 
      student1$v4[n] <- sample(noteetmt, 1, replace = TRUE, prob = c(0.2, 0.8))
    if (i == intell[2] && student1$v10[n]==noteetmt[1] ) 
      student1$v4[n] <- sample(noteetmt, 1, replace = TRUE, prob = c(0.75, 0.25))
    if (i == intell[2] && student1$v10[n]==noteetmt[2] ) 
      student1$v4[n] <- sample(noteetmt, 1, replace = TRUE, prob = c(0.4, 0.6))
    #v1
    if ( student1$v4[n] == noteetmt[1] && student1$v7[n] == niv[1] )
      student1$v1[n] <- sample(diff, 1, replace = TRUE,prob = c(0.45, 0.55)) #
    if ( student1$v4[n] == noteetmt[1] && student1$v7[n] == niv[2] )
      student1$v1[n] <- sample(diff, 1, replace = TRUE,prob = c(0.6, 0.4)) #
    if ( student1$v4[n] == noteetmt[2] && student1$v7[n] == niv[1] )
      student1$v1[n] <- sample(diff, 1, replace = TRUE,prob = c(0.2, 0.8)) #
    if ( student1$v4[n] == noteetmt[2] && student1$v7[n] == niv[2] )
      student1$v1[n] <- sample(diff, 1, replace = TRUE,prob = c(0.15, 0.85)) #
    
    #v3
    if ( student1$v1[n] == diff[1] && i == intell[1])
      student1$v3[n] <- sample(noteetmt, 1, replace = TRUE,prob = c(0.85, 0.15))
    if ( student1$v1[n] == diff[1] && i == intell[2])
      student1$v3[n] <- sample(noteetmt, 1, replace = TRUE,prob = c(0.95, 0.05))
    if ( student1$v1[n] == diff[2] && i == intell[1])
      student1$v3[n] <- sample(noteetmt, 1, replace = TRUE,prob = c(0.2, 0.8))
    if ( student1$v1[n] == diff[2] && i == intell[2])
      student1$v3[n] <- sample(noteetmt, 1, replace = TRUE,prob = c(0.5, 0.5))
    
    
    #v5
    
    if (student1$v7[n] == niv[1] && student1$v4[n] == noteetmt[1] && student1$v3[n] == noteetmt[1])
      student1$v5[n] <- sample(eetl, 1, replace = TRUE,prob = c(1, 0))
    if (student1$v7[n] == niv[1] && student1$v4[n] == noteetmt[1] && student1$v3[n] == noteetmt[2])
      student1$v5[n] <- sample(eetl, 1, replace = TRUE,prob = c(0.75, 0.25))
    if (student1$v7[n] == niv[1] && student1$v4[n] == noteetmt[2] && student1$v3[n] == noteetmt[1])
      student1$v5[n] <- sample(eetl, 1, replace = TRUE,prob = c(0.8, 0.2))
    if (student1$v7[n] == niv[1] && student1$v4[n] == noteetmt[2] && student1$v3[n] == noteetmt[2])
      student1$v5[n] <- sample(eetl, 1, replace = TRUE,prob = c(0.5, 0.5))
    
    if ( student1$v7[n] == niv[2] && student1$v4[n] == noteetmt[1] 
         && student1$v3[n] ==noteetmt[1])
      student1$v5[n] <- sample(eetl, 1, replace = TRUE,prob = c(0.95, 0.05))
    if ( student1$v7[n] == niv[2] && student1$v4[n] == noteetmt[1] && student1$v3[n] == noteetmt[2])
      student1$v5[n] <- sample(eetl, 1, replace = TRUE,prob = c(0.5, 0.5))
    if ( student1$v7[n] == niv[2] && student1$v4[n] == noteetmt[2] && student1$v3[n] == noteetmt[1])
      student1$v5[n] <- sample(eetl, 1, replace = TRUE,prob = c(0.6, 0.4))
    if ( student1$v7[n] == niv[2] && student1$v4[n] == noteetmt[2] && student1$v3[n] == noteetmt[2])
      student1$v5[n] <- sample(eetl, 1, replace = TRUE,prob = c(0.1, 0.9))
    
    
    #v6
    if ( student1$v4[n] == noteetmt[1] && student1$v5[n] == eetl[1] )
      student1$v6[n] <- sample(eetl, 1, replace = TRUE,prob = c(0.95,0.05))
    if ( student1$v4[n] == noteetmt[1] && student1$v5[n] == eetl[2] )
      student1$v6[n] <- sample(eetl, 1, replace = TRUE,prob = c(0.75, 0.25))
    if ( student1$v4[n] == noteetmt[2] && student1$v5[n] == eetl[1] )
      student1$v6[n] <- sample(eetl, 1, replace = TRUE,prob = c(0.9, 0.1))
    if (  student1$v4[n] == noteetmt[2] && student1$v5[n] == eetl[2] )
      student1$v6[n] <- sample(eetl, 1, replace = TRUE,prob = c(0.65, 0.35))
    
    
    #v8
    if ( student1$v3[n] == noteetmt[1] && student1$v6[n] == eetl[1] )
      student1$v8[n] <- sample(bo, 1, replace = TRUE,prob = c(1,0))
    if ( student1$v3[n] == noteetmt[1] && student1$v6[n] == eetl[2] )
      student1$v8[n] <- sample(bo, 1, replace = TRUE,prob = c(0.75, 0.25))
    if ( student1$v3[n] == noteetmt[2] && student1$v6[n] == eetl[1] )
      student1$v8[n] <- sample(bo, 1, replace = TRUE,prob = c(0.9, 0.1))
    if ( student1$v3[n] == noteetmt[2] && student1$v6[n] == eetl[2] )
      student1$v8[n] <- sample(bo, 1, replace = TRUE,prob = c(0.4, 0.6))
    
    n <- n+1
  }
  return (student1)
}

#utilisation du premier mod�le 
data4 <- student(100)

#utilisation du deuxi�me mod�le  
#data4 <- student1(100)

############################### CACHE ##########################################
# Nous créons le cache qui contiendra toutes les configurations de parents 
# jugées optimales. Le but est de réduire l'espace de recherche pour l'ago B&B.
# Nous appelons la librairie hash pour créer des dictionnaires 
library(hash)
# Nous créons une fonction génératrice de score BIC local 
# Arguments: noeud 'i' et ensemble de parents 'Pi'
si <- function(i, Pi){ 
  # initialisation 
  score <-0 
  # pour chaque parent 'k' dans Pi
  for (k in Pi){
    # update le score local avec la fonction BIC 
    # on utilise lm pour le fit des données simulées au noeud 'i' conditionnel au parent 'k'  
    score <- score + BIC(lm(data4[[dimnames[[2]][[i]]]]~data4[[dimnames[[2]][[k]]]]))
  }
  # retourne le score BIC local
  return (score) 
}
# On initialise le terme de pénalité pour le nombre parents (pour éviter la 
# surparamétrisation)
# Le nombre de valeurs par variable binaire ici est 2  
ri <- 2
# Le terme de pénalité est donné par la cardinalité des parents 'Pi' du noeud 'i'
ti <- function(Pi){ 
  a = (ri-1)*ri^length(Pi) 
  return(a) 
}
# On définit le théorème #4 sur deux ensembles de parents du noeud 'i', 
# soit 'Pi' inclu dans 'Pi_prime', les arguments de la fonction
th4 <- function(i, Pi, Pi_prime){
  # on applique le théorème #4 comme défini dans le papier
  if (ti(Pi_prime)+si(i,Pi)>0){
    # on garde 'Pi_prime' comme ensemble optimal
    keep <- FALSE
  } else {
    # ou non
    keep <- TRUE
  }
  # retourne FAUX (VRAI) pour garder 'Pi_prime' dans le cache
  return(keep)
}
# On définit une fonction pour générer toutes les combinaisons du vector 'x' 
# de longueur 'k'. On l'appelle pour initialiser le cache plus bas.  
enum.choose <- function(x, k) {
  if(k > length(x)) stop('k > length(x)')
  if(choose(length(x), k)==1){
    list(as.vector(combn(x, k)))
  } else {
    cbn <- combn(x, k)
    lapply(seq(ncol(cbn)), function(i) cbn[,i])
  }
}
# On définit une fonction qui génère tous les superensembles de parents 
# de l'ensemble de parents 'Pi' de taille length(Pi)+1
generate.configurations<-function(Pi, noeuds){
  # on utilise une variable non inclue dans 'Pi'
  noeuds.test <- noeuds[!noeuds%in%Pi]
  # on initialise la liste de superensembles
  Pi.supersets <- c()
  # pour chaque noeud test
  for (noeud in noeuds.test){
    # on crée un superensemble de 'Pi'
    Pi.supersets <- append(Pi.supersets,list(sort(c(Pi,c(noeud)))))
  }
  # on retourne la liste des superensembles
  return(Pi.supersets)
}
# On définit une fonction interne qui détermine si on garde l'ensemble de parents
# dans le cache ou non selon le lemme #1 et le théorème #4.
# Les arguments sont le noeud 'i', toutes les variables 'noeuds', 
# la liste des sous-ensembles de parents de taille fixe init'
internal.optimal.sets <- function(i, noeuds, init){
  # on initialise la liste des superensembles de parents que l'on garde
  keep <- c() 
  # que l'on rejette
  reject <- c()
  # pour chaque sous-ensemble 'Pi' dans 'init'
  for (Pi in init){
    # on générer des super ensembles de 'Pi' (avec un parent de plus) 
    Pi.supersets <- generate.configurations(Pi, noeuds)
    # pour chacun des super ensembles de 'Pi'
    for (Pi_prime in Pi.supersets){
      # on vérifie le théorème #4
      if (th4(i, Pi, Pi_prime)){ 
        # si on garde Pi_prime, on vérifie le lemme #1
        if (si(i,Pi_prime)>=si(i,Pi)){
          # si on garde et que Pi_prime n'est pas dans keep déjà
          if (!list(Pi_prime)%in%keep){ 
            # alors on ajoute Pi_prime pour garder plus tard
            keep <- append(keep, list(Pi_prime)) 
          }
          } else {
            # si le lemme #1 n'est pas satisfait alors on rejette 'Pi_prime'
            # si 'Pi_prime' n'est pas déjà rejetté
            if (!list(Pi_prime)%in%reject){
              reject <- append(reject, list(Pi_prime)) 
            }
          }
        } else {
        # si le théroème #4 n'est pas satisfait alors on rejette 'Pi_prime' 
        # si 'Pi_prime' n'est pas déjà rejetté 
        if (!list(Pi_prime)%in%reject){
          reject <- append(reject, list(Pi_prime)) 
        }
      }
    }
  }
  # on retoune les super ensembles à garder 
  return(keep[!keep%in%reject])  
}
# On définit une fonction récursive pour trouver les ensembles de parents 
# optimaux à storer dans le cache, pour le noeud 'i' avec les variables 'noeuds' 
recursive.optimal.sets <- function(i, noeuds){
  # on initialise la liste des ensembles de parents de 'i': chaque ensemble ne 
  # contient qu'un seul parent 
  init <- enum.choose(noeuds, 1)
  keep <- init
  # pour chaque longueur des ensembles de parents possibles 'k'
  # (la longueur max est le nombre de variables - 1)
  for (k in 1:(length(noeuds)-1)){
    # pour re-calcule l'ensemble des parents optimaux  
    init <- internal.optimal.sets(i, noeuds, init) 
    keep <- append(keep, init)
  }
  return(keep)
}
# On définit maintenant le cache
# On définit le nombre de variables dans le réseau 
num_var <- length(dimnames[[2]])
# On définit les indexes des noeuds
noeuds <- 1:num_var
# On initialise le cache
cache <- c()
# On initialise un dictionaire hash qu'on va redéfinir à chaque noeud
cache_i <- hash()
# Pour chacun des noeuds dans le graphe 
for (n in 1:num_var){
  # on trouve les ensembles de parents optimaux
  parents <- noeuds[!noeuds%in%c(n)]
  sets <- recursive.optimal.sets(n, parents)
  # pour chaque ensemble de parents optimal, on intialise une liste de dictionnaires
  # chacun de ces dictionnaires auront les clés "parents" de l'ensemble et "score" 
  cache_i_set <- c()
  scores <- c()
  # pour chacun des ensembles de parents optimaux
  for (i in 1:length(sets)){
    cache_i <- hash()
    # ajouter les parents de l'ensemble
    cache_i[["parents"]] <- sets[[i]] 
    # ajouter le score BIC local associé à l'ensemble
    cache_i[["score"]] <- si(i, sets[[i]]) 
    # stocker dans le cache
    cache_i_set[[i]] <- cache_i
    scores <- append(scores, cache_i[["score"]])
  }
  # on ordonne le cache, pour chauqe noeud on ordonne les ensembles de parents 
  # en ordre décroissant selon le score, i.e. le premier élément a le meilleur 
  # score 
  ordering <- order(scores, decreasing = TRUE)
  cache_i_set_ordered <- c()
  for (i in 1:length(sets)){
    cache_i_set_ordered[[i]] <- cache_i_set[[ordering[[i]]]]
  }
  cache[[n]] <- cache_i_set_ordered 
}
################################# B&B ##########################################
# Nous appellons la libraire igraph 
library(igraph)
# On définissons une fonction pour faire un graphe à partir d'un dictionnaire 
# où est storé l'ensemble des parents du noeud (comme dans le cache)
# prend en argument le # du set de parents pour chaque noeud dans le cache 
# storé dans le vecteurs 'indexes' (de longueur n pour n noeuds)
get.graph <-function(indexes){
  # on initialise le score global du graphe
  s <- 0
  # on initialise les arrêtes du graphe
  edges <- c()
  # pour chaque noeud dans le graphe
  for (i in 1:num_var){
    # on va chercher les parents dans le cache
    parents <- cache[[i]][[indexes[[i]]]][["parents"]]
    # pour chaque parent
    for (j in parents){
      # on crée une arrête, on l'ajoute à la liste
      edges <- append(edges, c(j,i))
    }
    # on update le score global avec le score local du noeud selon ses parents
    s <- s + cache[[i]][[indexes[[i]]]][["score"]]
  }
  # on initialise le graphe et on ajoute les arretes 
  g <- make_empty_graph(n = num_var) %>%
  add_edges(edges) 
  # on retoune un couple graphe et score global
  return(list(g,s))
}
# Voici un exemple (le graph initial optimal)
G<-get.graph(c(1,1,1,1,1,1,1,1,1,1))
plot(G[[1]]) # pour dessiner le graphe initial avec le score le plus élévé!  
print(G[[2]])
# On définit une fonction qui vérifier la présence de cycles dans le graphe 'g'
FindCycles <- function(g) {
  # on initialise les cycles
  Cycles <- NULL
  # pour chaque noeud
  for(v1 in V(g)){
    # on vérfie s'il y a des parents (mode "in"), sinon pas de cycles et on passe 
    if(degree(g, v1, mode="in") == 0) { next }
    # on vérifie les enfants du noeud
    GoodNeighbors = neighbors(g, v1, mode="out")
    GoodNeighbors = GoodNeighbors[GoodNeighbors > v1]
    # Pour chaque enfant
    for(v2 in GoodNeighbors){
      # on appelle all_simple_paths pour trouver des chemins 
      TempCyc = lapply(all_simple_paths(g, v2,v1, mode="out"), function(p) c(v1,p))
      TempCyc = TempCyc[which(sapply(TempCyc, length) > 1)]
      TempCyc = TempCyc[sapply(TempCyc, min) == sapply(TempCyc, `[`, 1)]
      Cycles  = c(Cycles, TempCyc)
    }
  }
  Cycles
}
# On définit une fonction qui génère les sous-graphes d'un graphe cyclique avec leurs scores
# prend en argument le graphe G et son score global s
make_Sub_Acyclic_Graph <- function(G, s){ 
  # vérifie s'il y a des cycles, si non alors retoune NULL
  if(length(FindCycles(G))==0){
    return(NULL)
  } else {
    # on intialise la liste de sous-graphes
    list_Sub_Graph <- c()
    # on trouve les cycles
    cycle <-FindCycles(G)
    # on prend qu'un cycle à la fois dans l'ago B&B, donc le premier 
    for (j in 1:(length(cycle[[1]])-1)){
      # on choisit l'arrête # 'j' qui part du noeud #j au noeud #j+1 dans le cycle 
      noeud <- cycle[[1]][[j+1]]
      parent <- cycle[[1]][[j]]
      G_prime <-  G
      # on garde en mémoire l'ancien ensemble de parents pour recalculer le score
      old_set <- incident(G, noeud, mode=c("in"))
      # o définit un nouveau graphe sans l'arrete "j" 
      G_second <- delete.edges(graph = G_prime, E(G_prime, P = c(parent,noeud)))
      # on cherche l'ensemble de parents optimal dans le cache qui ne contient pas 
      # l'arrête j et a le score maximal pour redéfinir G_second avec le meilleur
      # score possible, puisque l'arrête "j" n'affecte que les parents du noeud #j+1
      # alors on fouille le cache que pour ce noeud
      i <- 1
      while (i<=length(cache[[noeud]])){
        if (!parent%in%cache[[noeud]][[i]][["parents"]]){
          new_set <- cache[[noeud]][[i]][["parents"]]
          break
        } else {
          i <- i + 1
        }
      }
      # on update le score global du sous-graphe avec le nouvel ensemble de parents
      s_second <- s - si(noeud, old_set) + si(noeud, new_set)
      # on ajoute le sous-graphe à la liste
      list_Sub_Graph[[j]]<-list(G_second,s_second)
    }   
    # on retourne la liste de sous-graphes disjoints
    return(list_Sub_Graph) 
  }
}
## On définit l'algo B&B qui prend en entrée le doublet G = (graphe optimal, score global) 
Branch_And_Bound <- function(G) {
  # initialiser l'itérateur à 0
  i <- 0
  # initaliser le score optimal - inf
  s_best <- -Inf
  # initialiser bottom <- 3
  bottom <- 3
  # initialiser la liste Q avec G
  Q <- c()
  Q <- append(Q, list(G))
  # while loop tant que Q est pas vide
  while(length(Q)!=0) {
    # updater l'itérateur
    i<- i+1
    # if mod(i,bottom)==0 prendre le dernier element de Q sinon le premier
    if(i%%bottom==0){
      chosenGraph <- Q[[length(Q)]]
      Q <- Q[-c(length(Q))]
    } else {
      chosenGraph <- Q[[1]]
      Q <- Q[-c(1)]
    }
    # appeler make_Sub_Acyclic_Graph pour  vérifier si le graphe est acyclique
    listSubGragh <- make_Sub_Acyclic_Graph(chosenGraph[[1]],chosenGraph[[2]])
    # si NULL, comparer avec le score optimal actuel pour décider s'il s'agit 
    # du nouveau graphe optimal
    if(length(listSubGragh)==0){ 
      if (chosenGraph[[2]] > s_best){ 
        s_best <- chosenGraph[[2]]
        G <- chosenGraph[[1]]
      } 
    } else {  
      # sinon ajouter les sous-graphs à Q
      Q <- append(Q, listSubGragh)
    }
  }
  # retourner le graphe optimal avec son score
  return(chosenGraph)
}
# Exemple en utilisant le graphe optimal G définit dans l'exemple précédent
g  <- Branch_And_Bound(G)
plot(g[[1]]) # pour dessiner le graphe optimal de l'algo B&B!
print(g[[2]])
