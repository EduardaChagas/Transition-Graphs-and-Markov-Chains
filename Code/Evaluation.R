evaluation.string <- function(df, title.df, string){
  string = paste0(string, title.df)
  string = paste0(string, 'ACF - mean:', round(mean(df$ACF), 3), ' sd: ', round(sd(df$ACF), 3), ' \n ')
  string = paste0(string, 'DTW - mean:', round(mean(df$DTW), 3), ' sd: ', round(sd(df$DTW), 3), ' \n ')
  string = paste0(string, 'CORT - mean:', round(mean(df$CORT), 3), ' sd: ', round(sd(df$CORT), 3), ' \n ')
  string = paste0(string, 'BP - mean:', round(mean(df$BP), 3), ' sd: ', round(sd(df$BP), 3), ' \n ')
  return(string)
}

watg.mvnorm.10k = read.csv('../Data/Distance_watg_mvnorm_10k.csv')
watg.mvnorm.100k = read.csv('../Data/Distance_watg_mvnorm_100k.csv')
watg.mean.10k = read.csv('../Data/Distance_watg_mean_10k.csv')
watg.mean.100k = read.csv('../Data/Distance_watg_mean_100k.csv')
tg.mvnorm.10k = read.csv('../Data/Distance_tg_mvnorm_10k.csv')
tg.mean.10k = read.csv('../Data/Distance_tg_mean_10k.csv')
tg.mvnorm.100k = read.csv('../Data/Distance_tg_mvnorm_100k.csv')
tg.mean.100k = read.csv('../Data/Distance_tg_mean_100k.csv')

title.names = c('WATG mvnorm 10k \n ', 'WATG mean 10k \n ', 'WATG mvnorm 100k \n ', 'WATG mean 100k \n ', 'TG mvnorm 10k \n ', 'TG mean 10k \n ', 'TG mvnorm 100k \n ', 'TG mean 100k \n ')

results = ''
results = evaluation.string(watg.mvnorm.10k, title.names[1], results)
results = evaluation.string(watg.mean.10k, title.names[2], results)
results = evaluation.string(watg.mvnorm.100k, title.names[3], results)
results = evaluation.string(watg.mean.100k, title.names[4], results)
results = evaluation.string(tg.mvnorm.10k, title.names[5], results)
results = evaluation.string(tg.mean.10k, title.names[6], results)
results = evaluation.string(tg.mvnorm.100k, title.names[7], results)
results = evaluation.string(tg.mean.100k, title.names[8], results)


#ACF
#By length
t.test(watg.mvnorm.10k$ACF, watg.mvnorm.100k$ACF)$p.value
t.test(watg.mean.10k$ACF, watg.mean.100k$ACF)$p.value
t.test(tg.mvnorm.10k$ACF, tg.mvnorm.100k$ACF)$p.value
t.test(tg.mean.10k$ACF, tg.mean.100k$ACF)$p.value

#By algorithm
t.test(tg.mvnorm.10k$ACF, watg.mvnorm.10k$ACF)$p.value
t.test(tg.mean.10k$ACF, watg.mean.10k$ACF)$p.value
t.test(tg.mvnorm.100k$ACF, watg.mvnorm.100k$ACF)$p.value
t.test(tg.mean.100k$ACF, watg.mean.100k$ACF)$p.value

#By sampling
t.test(tg.mvnorm.10k$ACF, tg.mean.10k$ACF)$p.value
t.test(tg.mvnorm.100k$ACF, tg.mean.100k$ACF)$p.value
t.test(watg.mvnorm.10k$ACF, watg.mean.10k$ACF)$p.value
t.test(watg.mvnorm.100k$ACF, watg.mean.100k$ACF)$p.value

#DTW
#By length
t.test(watg.mvnorm.10k$DTW, watg.mvnorm.100k$DTW)$p.value
t.test(watg.mean.10k$DTW, watg.mean.100k$DTW)$p.value
t.test(tg.mvnorm.10k$DTW, tg.mvnorm.100k$DTW)$p.value
t.test(tg.mean.10k$DTW, tg.mean.100k$DTW)$p.value

#By algorithm
t.test(tg.mvnorm.10k$DTW, watg.mvnorm.10k$DTW)$p.value
t.test(tg.mean.10k$DTW, watg.mean.10k$DTW)$p.value
t.test(tg.mvnorm.100k$DTW, watg.mvnorm.100k$DTW)$p.value
t.test(tg.mean.100k$DTW, watg.mean.100k$DTW)$p.value

#By sampling
t.test(tg.mvnorm.10k$DTW, tg.mean.10k$DTW)$p.value
t.test(tg.mvnorm.100k$DTW, tg.mean.100k$DTW)$p.value
t.test(watg.mvnorm.10k$DTW, watg.mean.10k$DTW)$p.value
t.test(watg.mvnorm.100k$DTW, watg.mean.100k$DTW)$p.value

#CORT
#By length
t.test(watg.mvnorm.10k$CORT, watg.mvnorm.100k$CORT)$p.value # Diferente estatísticamente
t.test(watg.mean.10k$CORT, watg.mean.100k$CORT)$p.value
t.test(tg.mvnorm.10k$CORT, tg.mvnorm.100k$CORT)$p.value
t.test(tg.mean.10k$CORT, tg.mean.100k$CORT)$p.value

#By algorithm
t.test(tg.mvnorm.10k$CORT, watg.mvnorm.10k$CORT)$p.value
t.test(tg.mean.10k$CORT, watg.mean.10k$CORT)$p.value
t.test(tg.mvnorm.100k$CORT, watg.mvnorm.100k$CORT)$p.value
t.test(tg.mean.100k$CORT, watg.mean.100k$CORT)$p.value

#By sampling
t.test(tg.mvnorm.10k$CORT, tg.mean.10k$CORT)$p.value
t.test(tg.mvnorm.100k$CORT, tg.mean.100k$CORT)$p.value
t.test(watg.mvnorm.10k$CORT, watg.mean.10k$CORT)$p.value
t.test(watg.mvnorm.100k$CORT, watg.mean.100k$CORT)$p.value

#BP
#By length
t.test(watg.mvnorm.10k$BP, watg.mvnorm.100k$BP)$p.value
t.test(watg.mean.10k$BP, watg.mean.100k$BP)$p.value
t.test(tg.mvnorm.10k$BP, tg.mvnorm.100k$BP)$p.value
t.test(tg.mean.10k$BP, tg.mean.100k$BP)$p.value

#By algorithm
t.test(tg.mvnorm.10k$BP, watg.mvnorm.10k$BP)$p.value
t.test(tg.mean.10k$BP, watg.mean.10k$BP)$p.value
t.test(tg.mvnorm.100k$BP, watg.mvnorm.100k$BP)$p.value
t.test(tg.mean.100k$BP, watg.mean.100k$BP)$p.value

#By sampling
t.test(tg.mvnorm.10k$BP, tg.mean.10k$BP)$p.value
t.test(tg.mvnorm.100k$BP, tg.mean.100k$BP)$p.value
t.test(watg.mvnorm.10k$BP, watg.mean.10k$BP)$p.value
t.test(watg.mvnorm.100k$BP, watg.mean.100k$BP)$p.value






