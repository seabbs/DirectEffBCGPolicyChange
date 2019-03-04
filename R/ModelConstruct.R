#' Specify the Model Formula using Population as an Offset and Cases as the Outcome
#'
#' @param Preds A character vector containing variable names.
#'
#' @return A character string containing the model formula
#' @export
#'
#' @examples
#' 
ModelConstruct <- function(Preds) {
  paste0('Cases ~ ', paste(Preds, collapse = ' + '), ' + offset(log(Population))')
}