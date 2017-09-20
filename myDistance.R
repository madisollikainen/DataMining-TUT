# Distance functions 
# 

a <- c(1,2,3,4,5)
b <- c(5,5,5,5,5)
c <- (a-b)*(a-b)
k <- sum((a-b)*(a-b))


# Set the directory for testing
# setwd("PhD/courses/data-mining/")

# General distance function
# 
#
myDistance <- function(x, y, metric="Euclidean"){
  
  ## Start by checking if the inputs are of same size
  dims <- length(x)
  if(length(y) != dims){
    stop("ERROR! x and y of different length! ")
  }
  
  ## Then check if valid metric has been chosen
  valid.metrics <- c("Euclidean", "Manhattan", "Chebyshev")
  if( !(metric %in% valid.metrics ) ){
    stop("ERROR! Non-valid metric chosen")
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
  
}

