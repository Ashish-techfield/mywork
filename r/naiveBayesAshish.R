require(ramify)
require(emdbook)
require(mvtnorm)
require(MASS)
require(R6)
require(hash)

NaiveBayesClassifier <- R6Class("NaiveBayesClassifier",
                            list(
                              mu     = hash(),
                              sigma  = hash(),
                              priors = hash(),
                              K      = 0,
                              P      = 0,
                              
                              fit = function(X, y) {
                              
                                self$K = unique(y)
                                
                                for(k in self$K){
                                  
                                  self$mu[[toString(k)]]     = colMeans(X[y==k,])
                                  self$sigma[[toString(k)]]  = cov(X[y==k,])
                                  self$priors[[toString(k)]] = length(X[y==k,])/length(X)
                                }
                                
                              },
                              
                              predict = function(X){
                                self$P = matrix(0, nrow = length(X), ncol = length(self$K))
                                
                                for(k in self$K){
                                  self$P[,k+1] = dmvnorm(X,self$mu[[toString(k)]], self$sigma[[toString(k)]], log=TRUE) + log(self$priors[[toString(k)]])
                                }
                                argmax(P ,row = TRUE)-1 
                              }
                              
                            )
                            
)   

##Load Data
df <-read.csv("C:/Users/Aashish/techfield/week3_4/week4hw/exNB.csv", header = FALSE)
X<-df[,ncol(df)*-1]
y<-df[,ncol(df)]  

#initialize r6 class
nb <- NaiveBayesClassfier$new() #creates a new instance of linearRegression model $new

#fitting the model and predicting
nb$fit(X,y)
y_hat <- nb$predict(X)

##accuracy function
accuracy <- function(y,y_hat){
  (sum(c(y==y_hat))/length(y))*100
}

#Print out accuracy
print(paste("Accuracy:", acc=accuracy(y,y_hat),"%"))
