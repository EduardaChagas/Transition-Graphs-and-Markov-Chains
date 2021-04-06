################################################################################
# Author: Eduarda Chagas
# Date : Mar 21 2020
# Contact: eduarda.chagas@dcc.ufmg.br
################################################################################

# Packages and sources ---------------------------------------------------------
if(!require(Metrics)){
  install.packages("Metrics")
  require(Metrics)
} 
if(!require(markovchain)){
  install.packages("markovchain")
  require(markovchain)
} 
if(!require(gtools)){
  install.packages("gtools")
  require(gtools)
} 
if(!require(TSdist)){
  install.packages("TSdist")
  require(TSdist)
} 

# Generating AR sequences ------------------------------------------------------
AR <- function(n, phi, y0){
  x = c(y0, rep(0,n))
  for(i in c(2:(n+1))){
    error = rnorm(1,mean = 0, sd = 1)
    x[i] = (phi * x[i-1]) + error
  }
  return(x[2:(n+1)])
}

generate.AR <- function(N.sequences, N, N.test){
  s = matrix(nrow = N.sequences, ncol = N + N.test)
  for(i in 1:N.sequences){
    cat('Generating AR ', i, ' of ', N.sequences, '\n')
    s[i,] = AR(n = N + N.test, phi = 0.3, y0 = 0.1)
  }
  return(s)
}

# Obtain the reduced matrix ----------------------------------------------------
get.matrix.reduced <- function(transition.matrix){
  N = dim(transition.matrix)[1]
  is.blank = rep(FALSE, N)
  
  for(i in 1:N){
    if(sum(transition.matrix[i,]) != 0)
      is.blank[i] = TRUE
  }
  
  transition.matrix = transition.matrix[is.blank, ]
  transition.matrix = transition.matrix[, is.blank]
  
  return(transition.matrix)
}

# Calculating the estimators ---------------------------------------------------
parameters.rnorm <- function(ts, dimension = 3, delay = 1){
  patterns = formationPattern(serie = ts, dimension = 3, delay = 1, option = 0) 
  wedding = pattern.wedding(patterns)
  
  elements = formationPattern(serie = ts, dimension = 3, delay = 1, option = 1)
  
  metrics.elements = matrix(nrow = factorial(dimension), ncol = 2)
    
  for(i in 1:factorial(dimension)){
    is.wedding = (wedding == i)
    elements.pattern = elements[is.wedding,]
    metrics.elements[i, 1] = mean(elements.pattern[,dimension])
    metrics.elements[i, 2] = sd(elements.pattern[,dimension])
  }
  return(metrics.elements)
}

parameters.mvnorm <- function(ts, dimension = 3, delay = 1){
  patterns = formationPattern(serie = ts, dimension = 3, delay = 1, option = 0) 
  wedding = pattern.wedding(patterns)
  
  elements = formationPattern(serie = ts, dimension = 3, delay = 1, option = 1)
  
  metrics.elements = array(list(), factorial(dimension))
  
  for(i in 1:factorial(dimension)){
    is.wedding = (wedding == i)
    elements.pattern = elements[is.wedding,]
    metrics.elements[[i]] = mvnorm.mle(as.matrix(elements.pattern))
  }
  return(metrics.elements)
}

parameters.estimators.by.rnorm <- function(ts, next.pattern, dimension = 3, delay = 1){
  N = length(ts)
  new.ts = rep(0, N + 1)
  new.ts[1:N] = ts
  
  metrics.elements = parameters.rnorm(ts)
  
  new.ts[N + 1] = rnorm(1, mean = metrics.elements[next.pattern, 1], sd = metrics.elements[next.pattern, 2])
  return(new.ts)
}

parameters.estimators.by.mvnorm <- function(ts, next.pattern, dimension = 3, delay = 1){
  N = length(ts)
  new.ts = rep(0, N + 1)
  new.ts[1:N] = ts
  
  metrics.elements = parameters.mvnorm(ts)[[next.pattern]]
  
  new.ts[N + 1] = MASS::mvrnorm(1, mu = metrics.elements$mu, Sigma = metrics.elements$sigma)
  return(new.ts)
}

# Generation of the patterns samples elements ----------------------------------

generate.sequence <- function(ts, n.new.elements, generator, dimension = 3, delay = 1){
  new_ts = ts
  
  for(i in 1:n.new.elements){
    transition.matrix = TG(new_ts, dimension, delay)
    
    names.matrix = c("1","2","3","4","5","6")
    rownames(transition.matrix) = names.matrix
    colnames(transition.matrix) = names.matrix
    
    transition.matrix = get.matrix.reduced(transition.matrix)
    
    N = dim(transition.matrix)[1]
    for(i in c(1:N)){
      transition.matrix[i,] = transition.matrix[i,]/(sum(transition.matrix[i,]))
    }
    
    markov2 = new('markovchain',
                  transitionMatrix = transition.matrix, 
                  states = rownames(transition.matrix))
    #plot(markov2)
    #steadyStates(markov2)
    
    next.pattern =  rmarkovchain(n = 1, object = markov2)
    
    if(generator == 1)
      new_ts = parameters.estimators.by.rnorm(new_ts, as.integer(next.pattern))
    else 
      new_ts = parameters.estimators.by.mvnorm(new_ts, as.integer(next.pattern))
  }
  return(new_ts)
}

generate.sequence.watg <- function(ts, n.new.elements, generator, dimension = 3, delay = 1){
  new_ts = ts
  
  for(i in 1:n.new.elements){
    transition.matrix = transition.graph.weight(new_ts, dimension, delay)
    
    names.matrix = c("1","2","3","4","5","6")
    rownames(transition.matrix) = names.matrix
    colnames(transition.matrix) = names.matrix
    
    transition.matrix = get.matrix.reduced(transition.matrix)
    
    N = dim(transition.matrix)[1]
    for(i in c(1:N)){
      transition.matrix[i,] = transition.matrix[i,]/(sum(transition.matrix[i,]))
    }
    
    markov2 = new('markovchain',
                  transitionMatrix = transition.matrix, 
                  states = rownames(transition.matrix))
    #plot(markov2)
    #steadyStates(markov2)
    
    next.pattern =  rmarkovchain(n = 1, object = markov2)
    
    if(generator == 1)
      new_ts = parameters.estimators.by.rnorm(new_ts, as.integer(next.pattern))
    else 
      new_ts = parameters.estimators.by.mvnorm(new_ts, as.integer(next.pattern))
  }
  return(new_ts)
}

generate.n.sequences <- function(ts, N, N.test){
  N.sequences = dim(ts)[1]
  s = matrix(nrow = N.sequences, ncol = N)
  for(i in 1:N.sequences){
    cat('Generating sequence ', i, ' of ', N.sequences, '\n')
    s[i, ] = generate.sequence(ts[i, ], N.test)
  }
  return(s)
}

generate.pattern <- function(ts, n.new.elements, dimension = 3, delay = 1){
  for(i in 1:n.new.elements){
    transition.matrix = TG(ts, dimension, delay)
    
    names.matrix = c("1","2","3","4","5","6")
    rownames(transition.matrix) = names.matrix
    colnames(transition.matrix) = names.matrix
    
    transition.matrix = get.matrix.reduced(transition.matrix)
    
    N = dim(transition.matrix)[1]
    for(i in c(1:N)){
      transition.matrix[i,] = transition.matrix[i,]/(sum(transition.matrix[i,]))
    }
    
    markov2 = new('markovchain',
                  transitionMatrix = transition.matrix, 
                  states = rownames(transition.matrix))
    #plot(markov2)
    #steadyStates(markov2)
    
    next.pattern =  rmarkovchain(n = 1, object = markov2)
    ts = parameters.estimators.by.patterns(ts, as.integer(next.pattern))
  }
  return(ts)
}

patterns.hist <- function(ts, symbol_index, dimension = 3, delay = 1){
  patterns = formationPattern(serie = ts, dimension = 3, delay = 1, option = 0) 
  wedding = pattern.wedding(patterns)
  
  elements = formationPattern(serie = ts, dimension = 3, delay = 1, option = 1)
  
  is.wedding = (wedding == symbol_index)
  elements.pattern = elements[is.wedding,]
  return(elements.pattern)
}
