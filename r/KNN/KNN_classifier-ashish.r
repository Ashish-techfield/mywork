install.packages("plyr")
require(R6)
require(plyr)

KNNClassifier = R6Class("KNNClassifier",
                        list(
                          X = 0,
                          y = 0,
                          fit = function(X,y){
                            self$X = X
                            self$y = y
                            #invisible(self)
                          },
                          
                          predict = function(X, K=5,epsilon = 1e-2){    
                            N = nrow(X)
                            y_hat = list()
                            
                            for(i in 1:N){
                              distance = rowSums(self$X - X[i, ])^2
                              idx = order(distance)[1:K]
                              gamma = 1/(sqrt(distance[idx])+epsilon)
                              y_hat[i] =  
                                }
                            y_hat
                          }
                        )
                      )


