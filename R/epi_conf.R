#' Estimate Incidence Rates per 100,000
#'
#' @param Cases Numeric vector of cases
#' @param Population Numeric vector of populations
#'
#' @return Estimated incidence rates with confidence intervals.
#' @export
#'
#' @importFrom epiR epi.conf
#' @examples
#' 
epi_conf = function(Cases, Population) {
  epi.conf(matrix(c(sum(Cases, na.rm = TRUE), 
                    sum(Population, na.rm = TRUE)), 
                  ncol = 2, nrow = 1), ctype = "inc.rate",
           method = "exact", N = 100000)
}