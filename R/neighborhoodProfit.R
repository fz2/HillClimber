
#' The neighborhoodProfit method
#' @description neighborhoodProfit() is a method created for
#' "HillClimbing" class which calculates normalized profits for all neighbors
#' @param object: an object of HillClimbing class
#' @return neighbors.profit: a vector which contains normalized profits for all neighbors
#' @docType methods
#' @rdname selectNeighborhood_method
#' @export

setGeneric(
  name="neighborhoodProfit", 
  def=function(object){standardGeneric("neighborhoodProfit")
  }
)

setMethod(
  f="neighborhoodProfit",
  signature="HillClimbing", 
  function(object){
    neighbors <- findNeighborhood(object)
    # define a vector neighbors.iniprofit to store initial neighborhood profit values
    neighbors.iniprofit <- rep(0,ncol(neighbors))
    for (k in 1:ncol(neighbors)){ 
      coef1 <- object@coef1
      coef2 <- object@coef2
      # calculate neighborhood profit levels
      neighbors.iniprofit[k] <- (coef1)%*%neighbors[,k]+neighbors[,k]%*%(coef2%*%neighbors[,k])  
      # neighbors.profit is a vector used to store normalized profit values of all neighbors
      neighbors.profit <- 100*neighbors.iniprofit/(object@budget + object@budget^2)
    }
    return(neighbors.profit)
  }
)



