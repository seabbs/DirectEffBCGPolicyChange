#' Pretty format after checking a result is smaller than a limit
#'
#' @param x Numeric or character effect size estimate
#' @param digits Numeric defaults to 2. The number of digits to round to.
#' @param ... Additional arguements to pass to \code{OR_inf_rep}.
#'
#' @return A prettty effect estimate bounded by some maximum size.
#' @export
#' @importFrom prettypublisher pretty_round
#'
#' @examples
#' 
pretty_round_inf = function(x, digits = 2, ...) {
  x <- OR_inf_rep(x,  ...)
  x <- pretty_round(x, digits = digits)
  return(x)
}


