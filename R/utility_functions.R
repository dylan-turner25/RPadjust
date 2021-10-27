#' Returns CRRA utility 
#'
#' @param x numeric value representing wealth
#' @param r numeric value representing the coefficient of relative risk aversion
#'
#' @return Returns a numeric value representing the level of utility
#' @export
#'
#' @examples 
#' crra_u(100,.2)
crra_u = function(x,r){
  if(r != 1){
    utility = (x^(1-r)) / (1-r)
    return(utility)
  }
  if(r == 1){
    return(log(x))
  }
}
