# Distance functions 
# 

a<-matrix(,3,2)
b<-matrix(,3,2)

a[1,1]=4
a[2,1]=5

a[1,2]=2
a[2,2]=7

b[1,1]=5
b[2,1]=5

b[1,2]=1
b[2,2]=3



a <- c(4,5)
b <- c(5,5)
a <- c(2,7)
b <- c(1,3)

c <- (a-b)
k <- colSums(abs(a-b)/(abs(a)+abs(b)))


x1 <- c(1:4)
t(x1)

x2 <- c(2:5)
x3 <- c(11:6)

X <- rbind(x1,x2,x3)

cov(X,X)

nrow(X)

# Set the directory for testing
# setwd("PhD/courses/data-mining/")

# General point distance function
myDistance <- function(x, y, metric="Euclidean", IC=NULL){
  
  ## Start by checking if the inputs are of same size
  dims <- length(x)
  if(length(y) != dims){
    stop("ERROR! x and y of different length! ")
  }
  
  ## Then check if valid metric has been chosen
  valid.metrics <- c("Euclidean", "Manhattan", "Chebyshev", "Canberra", "Mahalanobis")
  if( !(metric %in% valid.metrics ) ){
    stop("ERROR! Non-valid metric chosen")
  }
  
  ## Mahalanobis metric
  if(metric=="Mahalanobis"){
    if(nrow(IC) != dims | ncol(IC) != dims ){
        stop("ERROR! When using Mahalanobis metric, insert a valide inverse of the covariance matrix IC!")
    }   
      return( sqrt( t((x-y)) %*% IC %*% (x-y) ) )
  }
  
  
  ## Euclidean metric
  if(metric=="Euclidean"){
    return( sqrt( sum((x-y)*(x-y)) ) )
  }
  
  ## Manhattan metric
  if(metric=="Manhattan"){
    return(  sum( abs((x-y)) )  )
  }
  
  ## Chebyshev metric
  if(metric=="Chebyshev"){
    return(  max( abs((x-y)) )  )
  }
  
  ## Canberra metric
  if(metric=="Canberra"){
      return( sum( abs( (x-y) ) / ( abs(x)+abs(y) ) )  )
  }
  
}
