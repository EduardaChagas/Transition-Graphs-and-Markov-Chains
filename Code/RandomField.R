################################################################################
# Author: Eduarda Chagas
# Date : Mar 18 2020
# Contact: eduarda.chagas@dcc.ufmg.br
################################################################################

# Packages and sources ---------------------------------------------------------
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
} 
if(!require(ggthemes)){
  install.packages("ggthemes")
  require(ggthemes)
} 
source("GenerateSequencies.R")
source("TransitionGraph.R")

# Simulate the autoregressive AR(1) process ------------------------------------

# Function of generation of autoregressive sequences ---------------------------

set.seed(456)

AR <- function(n, phi, y0){
  x = c(y0, rep(0,n))
  for(i in c(2:(n+1))){
    error = rnorm(1,mean = 0, sd = 1)
    x[i] = (phi * x[i-1]) + error
  }
  return(x[2:(n+1)])
}
# simulate AR(1) ---------------------------------------------------------------
N = 1000
N.test = 40

ts = data.frame(series = AR(n = N + N.test, phi = 0.3, y0 = 0.1),
                index = 1:(N + N.test))
#verifyMarkovProperty(tseries)

new.ts = data.frame(series = generation.sequence(ts$series[1:N], N.test),
                    index = 1:(N + N.test))

N.initial = 1
ggplot() +
  geom_line(data = ts[N.initial:(N + N.test),], aes(x = index, y = series, color = "blue")) +
  geom_line(data = new.ts[N.initial:(N + N.test),], aes(x = index, y = series, color = "red"))  + 
  theme_few()

N.initial = 990
ggplot() +
  geom_line(data = ts[N.initial:(N + N.test),], aes(x = index, y = series, color = "blue")) +
  geom_line(data = new.ts[N.initial:(N + N.test),], aes(x = index, y = series, color = "red"))  + 
  theme_few()

N.initial = 990
ts.pattern = pattern.wedding(formationPattern(ts$series, dimension = 3, delay = 1, 0))
new.ts.pattern = pattern.wedding(formationPattern(new.ts$series, dimension = 3, delay = 1, 0))

df.ts.pattern = data.frame(patterns = ts.pattern,
                index = 1:length(ts.pattern),
                type = rep(1, length(ts.pattern)))

df.new.ts.pattern = data.frame(patterns = new.ts.pattern,
                    index = 1:length(new.ts.pattern),
                    type = rep(2, length(new.ts.pattern)))

df.ts.pattern = df.ts.pattern[N.initial:length(ts.pattern),]
df.new.ts.pattern = df.new.ts.pattern[N.initial:length(new.ts.pattern),]

df.pattern = data.frame(patterns = c(df.ts.pattern$patterns, df.new.ts.pattern$patterns),
                        index = c(df.ts.pattern$index, df.new.ts.pattern$index),
                        type = c(df.ts.pattern$type, df.new.ts.pattern$type))

Sequence = c('Original', 'Synthetic')[df.pattern$type]
ggplot(data = df.pattern) +
  geom_line(aes(x = index, y = patterns, color = Sequence)) + 
  facet_grid(rows = vars(type)) + 
  theme_few()

######################################## Plot the series ##############################################

#Assuming normally distributed errors, 95% prediction intervals are given by

mean_cl_quantile <- function(x, q = c(0.1, 0.9), na.rm = TRUE){
  dat <- data.frame(y = mean(x, na.rm = na.rm),
                    ymin = quantile(x, probs = q[1], na.rm = na.rm),
                    ymax = quantile(x, probs = q[2], na.rm = na.rm))
  return(dat)
}

ggplot(dt,aes(Time, Value)) +
  stat_summary(geom = "line", fun.y = mean) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_quantile, alpha = 0.3)


# Markov Matrix ----------------------------------------------------------------


#sample from regMC
N = 500
synthetic.ts = rmarkovchain(n = N, object = markov2)
synthetic.ts = as.data.frame(synthetic.ts, stringsAsFactors = FALSE)
synthetic.ts$index = 1:N
synthetic.ts$synthetic.ts = as.numeric(synthetic.ts$synthetic.ts)

ggplot(synthetic.ts, aes(index, synthetic.ts)) + 
  geom_line(colour = "dark red") +
  xlab("time") +
  ylab("state") +
  ggtitle("Random Markov Chain")


patterns = formationPattern(ts, dimension, delay, 0)
wedding <- pattern.wedding(patterns)

qplot(x = c(1:length(wedding)), y = wedding, geom = "line", main = "phi = 0.3", xlab = "time", ylab = "patterns") +
  theme_few(base_size = 14, base_family = "serif")  + theme(plot.title = element_text(hjust=0.5)) + 
  scale_colour_few("Dark")
