rm(list = ls())

require('lda')
require('penalized')

countvalid <- function(a, e, s) {
  valid <- 0
  for (a in alpha) { for (e in eta) { for (s in sig) {
    if (a >= e) {
      valid <- valid + 1
    }
                    } } }
  return(valid)
}

# Load data
data = read.csv('data/enroll_data.csv', header=FALSE, sep='\t', as.is=TRUE)
names(data) <- c('Course', 'Demand', 'Comments')
data <- data[data$Comments != "",]
#Normalize reviews
data$Demand <- log(data$Demand)
data$Demand <- (data$Demand - mean(data$Demand))/sd(data$Demand)
# Prep for topic modeling
docs <- as.list(data$Comments)
corp <- lexicalize(docs)
vocab <- corp$vocab
# Split training and test sets
samp <- sample(1:length(docs), size=length(docs)*2/3, replace=FALSE)

train.docs <- corp$documents[samp]
test.docs <- corp$documents[-samp]

train.dem <- data$Demand[samp]
test.dem <- data$Demand[-samp]

# Parameter grids
alpha <- c(10, 1, 0.1, 0.01)
eta <- c(1, 0.1, 0.01)
n.topics <- seq(2, 16, by=2)
sig <- c(0.5,5)

# Iterations
e.its <- 10
m.its <- 10

# Store RMSE
t.perf <- list()
valid <- countvalid(alpha, eta, sig)

perf <- vector('numeric', valid*length(n.topics))

# GO!
i <- 1
for (t in n.topics) {
  p <- sample(c(-1,1), t, replace=TRUE)
  t.perf[[paste(t)]] <- vector('numeric', valid)
  j <- 1
  for (a in alpha) {
    for (e in eta) {
      for (s in sig) {
        if (e > a) {
          next
        }
        # Learn SLDA model via E-M algo
        f <- slda.em(documents=train.docs,
                     K=t,
                     vocab=vocab,
                     num.e.iterations=e.its,
                     num.m.iterations=m.its,
                     alpha=a, eta=e,
                     annotations=train.dem,
                     params=p,
                     variance=0.5,
                     logistic=FALSE,
                     regularise=TRUE,
                     lambda=s,
                     method='sLDA'
                     )
        y <- slda.predict(documents=test.docs,
                          topics=f$topics,
                          model=f$model,
                          alpha=a, eta=e)
        rmse <- sqrt(mean((y-test.dem)^2))
        m <- paste("N=", t, "     a=", a, "     e=", e, "     s=", s, "     rmse=", rmse)
        cat(paste("***", i, "***", m, "***\n"))
        t.perf[[paste(t)]][j] <- rmse
        j <- j+1
        i <- i+1
      }
    }
  }
}

require('ggplot2')
require('reshape2')

# Prep plotting
df <- data.frame(trial=1:valid)
for (t in n.topics) {
  df[[paste("err.", t, sep="")]] <- t.perf[[paste(t)]]
}

df.long <- melt(df, id.vars='trial')
ggplot(df.long, aes(x=trial, y=value, color=variable)) + geom_line() + geom_point(size=3)

model <- slda.em(documents=corp$documents,
                 K=12,
                 vocab=vocab,
                 num.e.iterations=e.its,
                 num.m.iterations=m.its,
                 alpha=10, eta=1,
                 annotations=data$Demand,
                 params=sample(c(-1,1), 12, replace=TRUE),
                 variance=0.5,
                 logistic=FALSE,
                 regularise=TRUE,
                 lambda=5,
                 method='sLDA'
                 )
top.topic.words(model$topics, num.words=15, by.score=TRUE)
model$coefs
