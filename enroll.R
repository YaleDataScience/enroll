### @author: henryre ###

rm(list = ls())

require('lda')
require('penalized')

# Find number of valid parameter sets
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
data <- data[data$Demand > 50, ]
# Normalize reviews
data$Demand <- log(data$Demand)
# Prep for topic modeling
docs <- as.list(data$Comments)
source('nlexicalize.R')
corp <- nlexicalize(docs, n=3)
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
sig <- c(1,5)

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
        # Skip unusual parameter sets
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
        # Predict on training set
        y <- slda.predict(documents=test.docs,
                          topics=f$topics,
                          model=f$model,
                          alpha=a, eta=e)
        # Compute RMSE
        rmse <- sqrt(mean((y-test.dem)^2))
        # Print message
        m <- paste("N=", t, "     a=", a, "     e=", e, "     s=", s, "     rmse=", rmse)
        cat(paste("***", i, "***", m, "***\n"))
        # Add RMSE to list
        t.perf[[paste(t)]][j] <- rmse
        # Increment
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
# Melt data frame and plot
df.long <- melt(df, id.vars='trial')
ggplot(df.long, aes(x=trial, y=value, color=variable)) + geom_line() + geom_point(size=3)
# Learn final model
model <- slda.em(documents=corp$documents,
                 K=12,
                 vocab=vocab,
                 num.e.iterations=50,
                 num.m.iterations=50,
                 alpha=10, eta=1,
                 annotations=data$Demand,
                 params=sample(c(-1,1), 12, replace=TRUE),
                 variance=0.5,
                 logistic=FALSE,
                 regularise=TRUE,
                 lambda=5,
                 method='sLDA'
                 )
# Print top terms for each topic and topic coefficients
top.topic.words(model$topics, num.words=25, by.score=TRUE)
model$coefs

require('wordcloud')
require('RColorBrewer')
# Convert weights to normalized scores (see top.topic.words())
topic.scores <- function(topics) {
  normalized.topics <- topics/(rowSums(topics) + 1e-05)
  scores <- apply(normalized.topics, 2, function(x) x *
                    (log(x + 1e-05) - sum(log(x + 1e-05))/length(x)))
  return(scores)
}
# Convert
scores <- topic.scores(model$topics)
# Generate word clouds
for (i in 1:nrow(scores)) {
  cloudy <- sort(scores[i,], decreasing=T)[1:25]
  print(wordcloud(names(cloudy), freq=cloudy,
                 scale=c(3,.10), min.freq=1, rot.per=0.15, random.order=F,
                 random.color=T, colors=brewer.pal(9,'Set1')[1:5]))
  invisible(readline(prompt="Press [enter] to continue"))
}
