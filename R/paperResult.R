
#' The paperResult method
#' @description paperResult() is a method created for HillClimbing
#' class which reproduces results from the three tables in the paper 
#' @param object: an object of HillClimbing class
#' @return result: the mean normalized profit over 1000 economic landscapes
#' @docType methods
#' @rdname paperResult_method
#' @export

# method to return table with the mean normalized profits for all three methods 
# and for all connections from 1 to 5
setGeneric(
  name="paperResult",
  def=function(object){standardGeneric("paperResult")
  }
)
setMethod(
  f="paperResult", 
  signature="HillClimbing", 
  def=function(object){
    # get an object with a starting allocation and coefficients for the profit function
    object <- generateObject(object)
    # calculate the profit for the starting allocation
    profit.norm <- profitFunction(object)
    # t is the number of times we want to use the according strategy
    t <- 1000
    # Result is a vector used to store profit levels in different landscapes
    Result <- NULL
    for (i in 1:t){
      Result[i] <- climbingStrategy(object)
      # the mean profit for all normalized landscape profits
      result <- mean(Result)
    }
    return(result)
  }
)
