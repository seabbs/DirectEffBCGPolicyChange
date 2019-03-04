#' Function to print the number of stan models loaded as DLLs and return their names
#'
#' @param verbose Logical, defaults to \code{TRUE}. If \code{TRUE} the number of loaded stan
#' models are printed.
#' @param StanFileNames A character string identifying the Stan file names, defaults to file.
#' @param MaxDLLs Numeric, DLL cap. The maximum defaults to 100.
#'
#' @return A character vector of the currently loaded DLLs.
#' @export
#' @importFrom purrr map_lgl
#' @examples
#' 
capture_stan_DLLs = function(verbose = TRUE, StanFileNames = 'file', MaxDLLs = 100) {
  
  ## Capture DLLs
  DLLs <- getLoadedDLLs()
  
  StanFile <- grepl(StanFileNames, DLLs)
  
  ## Summmary of DLL breakdown
  TotalStanDLLs <- StanFile %>% sum
  
  TotalNonStanDLLs <- StanFile %>% map_lgl(~!.) %>% sum
  
  ## Message summarising DLLs
  if (verbose) {
    message('There are currently ', TotalStanDLLs, ' stan models loaded as DLLs, with ', TotalNonStanDLLs, ' DLLs loaded that are not stan models. There are ', MaxDLLs - TotalStanDLLs - TotalNonStanDLLs, ' DLLs available before the DLL cap is reached')
  }
  
  StanFileNames <- names(DLLs)[StanFile]
  
  return(StanFileNames)
}