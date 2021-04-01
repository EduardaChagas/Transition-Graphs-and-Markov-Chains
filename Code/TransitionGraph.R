################################################################################
# Author: Eduarda Chagas
# Date : Mar 18 2020
# Contact: eduarda.chagas@dcc.ufmg.br
################################################################################

# Packages and sources ---------------------------------------------------------
if(!require(gtools)){
  install.packages("gtools")
  require(gtools)
} 

# Bandt-Pompe symbolization Functions ------------------------------------------

# Auxiliar function
define.symbols <- function(dimension){
  d = c(1:dimension)
  symbol = matrix(unlist(permutations(n = dimension, r = dimension)),nrow = factorial(dimension),ncol = dimension,byrow = FALSE)
  symbol = symbol - 1
  symbol
}

# Auxiliar function
FP <- function(n, dimension, delay){
  dyn.load("FormationPatterns.so")
  p <- .Call("FormationPatterns", n, dimension, delay)
  p = t(p) + 1
  return(p)
}

# Auxiliar function
formationPattern <- function(serie, dimension, delay, option){
  i = 1
  n = length(serie)
  p_patterns = elements = index2 = matrix(nrow=n,ncol=dimension)
  index = c(0:(dimension-1))
  
  index2 = FP(length(serie), dimension, delay)
  
  while((i + ((dimension-1)*delay)) <= n){ 
    elements[i,] = serie[index2[i,]]
    p_patterns[i,] = index[order(elements[i,])]
    i = i + 1
  }
  
  if(option == 0){
    p_patterns = na.omit(p_patterns)
    return(p_patterns[1:dim(p_patterns)[1],])
  }else if(option == 1){
    elements = na.omit(elements)
    return(elements[1:dim(elements)[1],])    
  }else{
    index2 = na.omit(index2)
    return(index2[1:dim(index2)[1],])    
  }
}

# Transition Graph Functions ---------------------------------------------------

# Auxiliar function
pattern.wedding <- function(patterns){
  m = dim(patterns)[1]
  D = dim(patterns)[2]
  symbols = define.symbols(D)
  wedding = rep(0, m)
  for(i in 1:m){
    e = 0
    j = 1
    stop = F
    while(j <= factorial(D) && stop == F){
      if(sum(symbols[j,] == patterns[i,]) == D){
        wedding[i] = j
        stop = T
      }
      j = j + 1
    }
  }
  return(wedding)
}

TG <- function(series, dimension, delay){
  patterns = formationPattern(series, dimension, delay, 0)
  wedding = pattern.wedding(patterns)
  size = length(wedding)
  
  dyn.load("TransitionGraph.so")
  probability <- .Call("TransitionGraph", wedding, dimension, size)
  
  probability = matrix(probability, nrow = factorial(dimension), ncol = factorial(dimension))
  
  return(probability)
}

transition.graph.weight <- function(series, dimension, delay){
  
  graph = matrix(0, nrow = factorial(dimension), ncol = factorial(dimension))
  patterns = formationPattern(series, dimension, delay, 0)
  elements = formationPattern(series, dimension, delay, 1)
  wedding = pattern.wedding(patterns)
  m = length(wedding)
  weight.total = 0
  
  for(i in 1:(m-1)){
    weight.i1 = (max(elements[i,]) - min(elements[i,]))
    weight.i2 = (max(elements[i+1,]) - min(elements[i+1,]))
    graph[wedding[i],wedding[i+1]] = graph[wedding[i],wedding[i+1]] + abs(weight.i1 - weight.i2)
    weight.total = weight.total + abs(weight.i1 - weight.i2)
  }
  graph = graph/weight.total
  return(graph)
}