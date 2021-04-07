if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
} 
if(!require(ggthemes)){
  install.packages("ggthemes")
  require(ggthemes)
} 
if(!require(doParallel)){
  install.packages("doParallel")
  require(doParallel)
} 
if(!require(Rfast)){
  install.packages("Rfast")
  require(Rfast)
} 
source("GenerateSequencies.R")
source("TransitionGraph.R")

set.seed(456)
N.sequences = 50

ts.10k = read.csv('../Data/ts_10k.csv')
ts.100k = read.csv('../Data/ts_100k.csv')

new.ts.10k = matrix(nrow = 50, ncol = 10000)
for(i in 1:N.sequences){
  cat('Generating sequence 10k', i, ' of ', 50, '\n')
  new.ts.10k[i, ] = generate.sequence(unlist(ts.10k[i, 1:9900, 1]), n.new.elements = 100, generator = 2)
}
write.csv(new.ts.10k, '../Data/new_ts_mvnorm_10k.csv')

new.ts.100k = matrix(nrow = 50, ncol = 100000)
for(i in 1:N.sequences){
  cat('Generating sequence 100k', i, ' of ', 50, '\n')
  new.ts.100k[i, ] = generate.sequence(unlist(ts.100k[i, 1:99900, 1]), n.new.elements = 100, generator = 2)
}
write.csv(new.ts.100k, '../Data/new_ts_mvnorm_100k.csv')
