
#' Detect Warnings and Store as a List
#' @param ListFits List of fitted model objects
#'
#' @return A list of model warnings
#' @export
#' @importFrom dplyr nth
#' @importFrom purrr map transpose quietly
#' @examples
#' 
store_warnings = function(ListFits) {
  ListFits %>% 
    map(quietly(function(x) summary(x))) %>%
    transpose %>% 
    nth(n = 3) 
}