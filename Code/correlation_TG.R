if(!require(TSdist)){
  install.packages("TSdist")
  require(TSdist)
} 
if(!require(TSclust)){
  install.packages("TSclust")
  require(TSclust)
} 

cort.dist <- function(x, y){
  len.x = length(x)
  len.y = length(y)
  cort = sum((x[2:len.x] - x[1:(len.x-1)]) * (y[2:len.y] - y[1:(len.y-1)]))/(sqrt(sum((x[2:len.x] - x[1:(len.x-1)])^2)) * sqrt(sum((y[2:len.y] - y[1:(len.y-1)])^2)))
  return(cort)
}

#ts.10k = read.csv('../Data/ts_10k.csv')
new.ts.10k = read.csv('../Data/watg_mean_10k.csv')

#ts.100k = read.csv('../Data/ts_100k.csv')
new.ts.100k = read.csv('../Data/watg_mean_100k.csv')

N_init_10k = (10000 - 100) + 1
N_final_10k = 10000 + 1

N_init_100k = (100000 - 100) + 1
N_final_100k = 100000 + 1

N_seq = 50

distance.measures.10k = data.frame(ACF = numeric(length = N_seq),
                                   DTW = numeric(length = N_seq),
                                   CORT = numeric(length = N_seq),
                                   BP = numeric(length = N_seq))
  
distance.measures.100k = data.frame(ACF = numeric(length = N_seq),
                                DTW = numeric(length = N_seq),
                                CORT = numeric(length = N_seq),
                                BP = numeric(length = N_seq))

for(i in 1:N_seq){
  cat('Corr 10k number ', i, ' of ', N_seq, '\n')
  distance.measures.10k$ACF[i] = ACFDistance(x = unlist(ts.10k[i, N_init_10k:N_final_10k]), y = unlist(new.ts.10k[i, N_init_10k:N_final_10k]))
  distance.measures.10k$DTW[i] = DTWDistance(x = unlist(ts.10k[i, N_init_10k:N_final_10k]), y = unlist(new.ts.10k[i, N_init_10k:N_final_10k]))
  distance.measures.10k$CORT[i] = cort.dist(x = unlist(ts.10k[i, N_init_10k:N_final_10k]), y = unlist(new.ts.10k[i, N_init_10k:N_final_10k]))
  distance.measures.10k$BP[i] = PDCDistance(x = unlist(ts.10k[i, N_init_10k:N_final_10k]), y = unlist(new.ts.10k[i, N_init_10k:N_final_10k]))
}
write.csv(distance.measures.10k, 'Distance_watg_mean_10k.csv')

for(i in 1:N_seq){
  cat('Corr 100k number ', i, ' of ', N_seq, '\n')
  distance.measures.100k$ACF[i] = ACFDistance(x = unlist(ts.100k[i, N_init_100k:N_final_100k, 1]), y = unlist(new.ts.100k[i, N_init_100k:N_final_100k, 1]))
  distance.measures.100k$DTW[i] = DTWDistance(x = unlist(ts.100k[i, N_init_100k:N_final_100k, 1]), y = unlist(new.ts.100k[i, N_init_100k:N_final_100k, 1]))
  distance.measures.100k$CORT[i] = cort.dist(x = unlist(ts.100k[i, N_init_100k:N_final_100k, 1]), y = unlist(new.ts.100k[i, N_init_100k:N_final_100k, 1]))
  distance.measures.100k$BP[i] = PDCDistance(x = unlist(ts.100k[i, N_init_100k:N_final_100k, 1]), y = unlist(new.ts.100k[i, N_init_100k:N_final_100k, 1]))
}
write.csv(distance.measures.100k, 'Distance_watg_mean_100k.csv')
