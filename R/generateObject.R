
#' The generateObject method
#' @description generateObject() is a method created for HillClimbing class which gives 
#' the object the random starting allocation and randomized coefficients of the profit function
#' @param object: an object of HillClimbing class
#' @return object: an object of HillClimbing class
#' @docType methods
#' @rdname generateObject_method
#' @export

setGeneric(
  name="generateObject", 
  def=function(object){standardGeneric("generateObject")
  }
)

setMethod(
  f="generateObject",
  signature="HillClimbing", 
  function(object){
    # n is the number of inputs
    n <- object@input
    
    # We start by generating a random budget allocation and use it as the 
    # starting point for local economic hillclimbing
    
    # initiate all inputs of the allocation to be 0
    object@allocation <- rep(0,n)
    # randomize the allocation of each dollar of the total budget
    for (i in 1:object@budget){
      # get a random integer number from 1 to n
      random <- sample(1:n,1,replace=T)
      # increase the corresponding component by 1
      object@allocation[random] <- object@allocation[random]+1 
    }
    
    # we randomize all coefficients of the profit function here
    coef1 <- object@coef1
    coef2 <- object@coef2
    coef2 <- matrix(0,nrow=n,ncol=n)
    # randomize linear coefficients
    coef1 <- runif(n,object@linearConstraint[1], object@linearConstraint[2])
    # randomize quadratic coefficients(square coefficients and cross product coefficients)
    for(i in 1:n){
      for (j in i:n){
        # randomize square coefficients
        if(i==j){
          coef2[i,j] <- runif(1,object@squareConstraint[1],object@squareConstraint[2])
        }
        # randomize cross product coefficients
        else {
          coef2[i,j] <- runif(1,object@crossproductConstraint[1],object@crossproductConstraint[2])/2
          coef2[j,i] <- runif(1,object@crossproductConstraint[1],object@crossproductConstraint[2])/2
          
        }
      }
    }
    # store the randomized coefficients in the object
    object@coef1 <-coef1
    object@coef2 <- coef2
    return(object)
  }
)
