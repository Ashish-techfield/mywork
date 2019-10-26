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
                              
                              fit = function(X, y, Naive = TRUE) {
                              
                                self$K = unique(y)
                                
                                for(k in self$K){
                                  
                                  self$mu[[toString(k)]]     = colMeans(X[y==k,])
                                  self$sigma[[toString(k)]]  = cov(X[y==k,])
                                  self$priors[[toString(k)]] = length(X[y==k,])/length(X)
                                  
                                  if(Naive == TRUE){self$sigma[[toString(k)]]  = diag(diag(cov(X[y==k,])))
                                  }
                                  else{
                                    self$sigma[[toString(k)]]  = cov(X[y==k,])
                                  }
                                  
                                }
                                
                              },
                              
                              predict = function(X, dist=dmvnorm){
                                self$P = matrix(0, nrow = length(X), ncol = length(self$K))
                                
                                
                                
                                
                                if(dist == dmvnorm){
                                for(k in self$K){
                                  self$P[,k+1] = dist(X,self$mu[[toString(k)]], self$sigma[[toString(k)]], log=TRUE) + log(self$priors[[toString(k)]])
                                }
                                }
                                else if (dist == rmvnorm){
                                  for(k in self$K){
                                    self$P[,k+1] = dist(nrow(X),self$mu[[toString(k)]], self$sigma[[toString(k)]], log=TRUE) + log(self$priors[[toString(k)]])
                                  }
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
