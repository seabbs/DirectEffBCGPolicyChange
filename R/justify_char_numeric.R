#' Function to build justification framework for pander
#'
#' @param ColNum Numeric, the total number of columns
#' @param Character Numeric vector, the numeric position of character columns.
#' @param Numeric Numeric vector, the numeric position of numeric columns.
#'
#' @return A character string specifying the justification for a pander table.
#' @export
#'
#' @examples
#' 
justify_char_numeric = function(ColNum = NULL, Character = NULL, Numeric = NULL) {
  
  JustPos <- seq(1, ColNum)
  
  JustPos <- JustPos %>% 
    replace(JustPos %in% Character, 'l') %>%
    replace(JustPos %in% Numeric, 'r') %>% 
    replace(. %in% seq(1, ColNum), 'c') %>% 
    paste0(collapse  = '')
  
  return(JustPos)
}