if(!require(TSdist)){
  install.packages("TSdist")
  require(TSdist)
} 

ts.10k = read.csv('../Data/ts_10k.csv')
new.ts.10k = read.csv('../Data/watg_new_ts_10k.csv')

ts.100k = read.csv('../Data/ts_100k.csv')
new.ts.100k = read.csv('../Data/watg_new_ts_100k.csv')

N_init_10k = (10000 - 100) + 1
N_final_10k = 10000 + 1

N_init_100k = (100000 - 100) + 1
N_final_100k = 100000 + 1

N_seq = 50

ccor.distance.10k = ccor.distance.100k = rep(0, N_seq)

for(i in 1:N_seq){
  cat('Corr 10k number ', i, ' of ', N_seq, '\n')
  ccor.distance.10k[i] = CCorDistance(x = unlist(ts.10k[i, N_init_10k:N_final_10k]), y = unlist(new.ts.10k[i, N_init_10k:N_final_10k]))
}
write.csv(ccor.distance.10k, 'Ccor_Distance_watg_10k.csv')

for(i in 1:N_seq){
  cat('Corr 100k number ', i, ' of ', N_seq, '\n')
  ccor.distance.100k[i] = CCorDistance(unlist(ts.100k[i, N_init_100k:N_final_100k, 1]), unlist(new.ts.100k[i, N_init_100k:N_final_100k, 1]))
}
write.csv(ccor.distance.100k, 'Ccor_Distance_watg_100k.csv')
