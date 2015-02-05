
#' The findNeighborhood method
#' @description findNeighborhood() is a method created for HillClimbing
#' class which finds all neighbors of a given allocation in the landscape
#' @param object: an object of HillClimbing class
#' @return neighbors: a matrix which contains all neighbors of the initial allocation
#' @docType methods
#' @rdname findNeighborhood_method
#' @export

setGeneric(
  name = "findNeighborhood", 
  def = function(object){standardGeneric("findNeighborhood")}
)
setMethod(
  f="findNeighborhood", 
  signature="HillClimbing", 
  def=function(object){
    n <- object@input
    
    # define a n by n adjacency matrix to represent connection status between inputs
    # 1 represents connected, 0 represents unconnected
    
    # initialize a matrix connections
    connections <- matrix(0,nrow=n,ncol=n)
    for (i in 1:n){
      # define a counter c
      c <- 0
      while (c < object@connection){
        # randomize an integer from 1 to n for index j 
        j <- sample(1:n,1,replace=T)
        # if the input is itself or if two inputs are already connected, move to find the next connection  
        while((i==j)|(connections[i,j]==1)){
          j <- sample(1:n,1,replace=T)
        }
        connections[i,j]=1
        # increase the counter number by 1
        c <- c+1  
      }
    }  
    # define neighborhood as a matrix and put each neighbor into a column of the matrix
    # initially define the number of columns to be 1000 as we don't know the number of neighbors
    neighbors <- matrix(0,nrow = n, ncol = 1000)
    # k is the index for the column of the matrix
    k <- 1
    for (i in 1:n){
      for (j in 1:n){
        # put the original allocation into a vector newNeighbor
        newNeighbor <- object@allocation
        # if there exists a connection between input Xi and input Xj, transfer one dollar from Xi to Xj
        if (connections[i,j] == 1 && newNeighbor[i]>0){    
          newNeighbor[i] <- newNeighbor[i]-1
          newNeighbor[j] <- newNeighbor[j]+1
          # store all neighbors in the matrix neighbors
          neighbors[, k] <- newNeighbor
          k <- k + 1    
        }
      }
    }
    # find out columns of zeros
    for(m in 1:1000){
      if(sum(neighbors[, m]) == 0) {
        break
      }
    }
    # delete columns of zeros from the neighborhood matrix
    neighbors <- neighbors[,1:(m-1)]
    # return the matrix with each neighbor stored in a separate column
    return(neighbors)
  }
)       

