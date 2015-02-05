
#' The HillClimbing class
#' @description The HillClimbing class is a class which contains slots used by 
#' different functions in the package
#' @slot approach: three simple strategies for hillclimbing firms, "SA", "MA" or "LA"
#' @slot budget: the amount of firm's total budget
#' @slot input: the number of total inputs
#' @slot connection: the number of connections per input
#' @slot allocation: a vector which contains a set of inputs subject to the budget constraint
#' @slot linearConstraint: a vector which gives constraints for linear coefficients
#' @slot squareConstraint: a vector which gives constraints for square coefficients
#' @slot crossproductConstraint: a vector which gives constraints for cross product coefficients
#' @slot coef1: a vector which stores all linear coefficients of the profit function
#' @slot coef2: a matrix which stores all quadratic coefficients
#' (both square and cross product coefficients) of the profit function
#' @docType class
#' @rdname HillClimbing_class
#' @author Fan Zhang 
#' @export

setClass(
  "HillClimbing",
  representation = representation(
    approach = "character",
    budget = "numeric", 
    input = "numeric", 
    connection = "numeric",
    allocation="vector",
    linearConstraint = "vector",
    squareConstraint = "vector",
    crossproductConstraint = "vector",
    coef1 = "vector",
    coef2="matrix"
  )
)



