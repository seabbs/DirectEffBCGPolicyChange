#' Format estimate and standard deviation
#' 
#' @param Estimate Numeric, the effect estimate. 
#' @param SE Numeric, the standard error
#' @param Digits Numeric, the number of digits to round to.
#' @param Scientific Logical, defaults to \code{FALSE}. Should the results be
#' displayed in scientific format.
#'
#' @return A format effect estimate with standard errors rounded to the specified digits.
#' @export
#' @examples
#' 
format_est_sd = function(Estimate, SE, Digits, Scientific = FALSE) {
  
  SE <-  SE %>%
    signif(digits = Digits) %>% 
    format(scientific = Scientific, digits = Digits)
  
  Estimate <- Estimate %>%
    signif(digits = Digits) %>% 
    format(scientific = Scientific, digits = Digits)
  
  EstSd <- paste0(Estimate, ' (', SE, ')') 
  
  return(EstSd)
}