
#' The climbingStrategy method
#' @description climbingStrategy() is a method created for HillClimbing
#' class which uses three different strategies SA,MA and LA to find the 
#' maximum profit on a local economic landscape
#' @param object: an object of HillClimbing class
#' @return landscape.max: the maximum profit for the local landscape
#' @docType methods
#' @rdname climbingStrategy_method
#' @export

setGeneric(
  name = "climbingStrategy", 
  def = function(object){standardGeneric("climbingStrategy")}
)
setMethod(
  f="climbingStrategy", 
  signature="HillClimbing", 
  def=function(object){
    # find all neighbors of the starting allocation
    neighbors <- findNeighborhood(object)
    # find the original profit level
    profit.norm <- profitFunction(object)
    # create a vector select to store the subset of neighborhood profits
    select <- NULL
    # create a slot to store current profit level, initially it is the original profit level
    current.profit <- profit.norm
    # find all neighborhood profits
    neighbors.profit <- neighborhoodProfit(object)
    # define a slot to store the maximum landscape profit
    landscape.max <- 0
    # loop ends when there is no neighborhood profit higher than the current profit level
    while(length(neighbors.profit[which(neighbors.profit>profit.norm)])!=0) { 
      # select the subset of neighborhood profits higher than the current profit level 
      select <- neighbors.profit[which(neighbors.profit>profit.norm)]  
      # the Steepest Ascent strategy
      if(object@approach =="SA"){
        # k is the index of the allocation which we must move on to
        k <- which(neighbors.profit==max(select))
        current.profit <- neighbors.profit[k]
      }
      # the Median Ascent strategy
      else if(object@approach == "MA"){
        # medianIndex is the index for the median of select
        medianIndex <- which.min(abs(select-median(select)))
        k <- which(neighbors.profit==select[medianIndex])
        current.profit <- neighbors.profit[k]
      }
      # the Least Ascent strategy    
      else if(object@approach == "LA"){
        k <- which(neighbors.profit == min(select)) 
        current.profit <- neighbors.profit[k]
      }
      # move from the starting allocation to its neighbor given different approaches
      object@allocation <- neighbors[,k]  
      # calculate again new profit levels for the new allocation and its neighbors
      neighbors <- findNeighborhood(object)
      profit.norm <- profitFunction(object)
      neighbors.profit <- neighborhoodProfit(object)
    }
    landscape.max <- current.profit
    return(landscape.max)
  }
)