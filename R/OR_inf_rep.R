#' Inf Replacement for Large Odds Ratios
#'
#' @param OR Numeric, or character odds ratio
#' @param MaxSize Numeric, maximum size for odds ratio. Defaults to 
#' 100000
#'
#' @return A bounded effect estimate or Inf.
#' @export
#'
#' @examples
#' 
OR_inf_rep = function(OR, MaxSize = 100000) {
  OR <- as.numeric(OR)
  OR <- ifelse(OR > MaxSize, Inf, OR)
  return(OR)
}














