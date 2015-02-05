
#' The profitFunction method
#' @description profitFunction() is a method created for HillClimbing
#' class which calculates the normalized profit for any given allocation.
#' @param object: an object of "HillClimbing" class
#' @return profit.norm: the normalized profit for the allocation
#' @docType methods
#' @rdname profitFunction_method
#' @export

setGeneric(
  name="profitFunction",
  def=function(object){standardGeneric("profitFunction")
  }
)
setMethod(
  f="profitFunction", 
  signature="HillClimbing", 
  def=function(object){  
    allocation <- object@allocation
    coef1 <- object@coef1
    coef2 <- object@coef2
    # calculate the profit of the budget allocation (linear terms+ quadratic terms)
    profit <- coef1%*%allocation+allocation%*%(coef2%*%allocation)
    # normalize the calculated profit and return it as a vector
    profit.norm <- as.vector((100*profit/(object@budget+object@budget^2)))
    return(profit.norm)
  }
)