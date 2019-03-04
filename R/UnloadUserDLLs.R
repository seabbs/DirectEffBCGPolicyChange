#' Unload Named DLLs
#'
#' @param VectorDLLNames A character vetor of DLL names.
#' @param verbose Logical defautls to \code{TRUE}. Should progress messages be printed.
#'
#' @return This functions returns \code{NULL}
#' @export
#' @importFrom purrr map
#' @examples
#' 
UnloadUserDLLs = function(VectorDLLNames, verbose = TRUE) {
  
  DLLSummary <- getLoadedDLLs()
  
  VectorDLLNames %>% 
    map(~DLLSummary[[.]][['path']]) %>%
    map(dyn.unload)
  
  if (verbose & length(VectorDLLNames) != 0) {
    
    message('A total of ', VectorDLLNames %>% 
              length, ' DLL have been unloaded. These were ',
            paste(VectorDLLNames, collapse = ', ') ,
            '. This may restrict the future use of linked objects.')
  }
  
}