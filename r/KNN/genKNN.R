library(matrixStats)
library(questionr)
require(R6)



uniform_weights=function(dists, epsilon){
  K = len(dists)
  return(array([1/K]*K))
}
  
reciprocal_weights=function(dists, epsilon = 1e-2){
  (1/(sqrt(dist) + epsilon))
  }
  

gauss_weights=function(dists){
  ed = np.exp(dist)
  ed/sum(ed)
  
}
  
accuracy=function(y,y_hat){
  mean(y == y_hat)
  
}
  
R2= function(y, y_hat){
  1 - np.sum((y - y_hat)**2)/np.sum((y - y.mean())**2)
}


KNNClassifier = R6Class("KNNClassifier",
                        list(
                          X = 0,
                          y = 0,
                          fit = function(X,y){
                            self$X = X
                            self$y = y
                            #invisible(self)
                          },
                          
                          predict = function(X, K=5 ,p = 2, weight_func = gauss_weights, mode = TRUE, epsilon = 1e-3){    
                            N = nrow(X)
                            
                            y_hat = list()
                            
                            for(i in 1:N){
                              dist = rowSums(self$X - X[i,])^2
                              idx = order(dist)[1:K]
                              gamma <- weight_func(dist[idx], epsilon)
                              
                              if(mode ==TRUE){
                                y_hat[i] <- as.integer((gamma%*%self$y[idx])/sum(gamma))
                              }
                              y_hat[i] <- NA
                            }
                            return(y_hat)
                          }
                        )
)



