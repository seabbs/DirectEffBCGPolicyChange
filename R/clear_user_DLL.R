#' Clear Stan DLLs
#'
#' @description A wrapper function around \code{capture_stan_DLLS} and \code{UnloadUserDLLs}.
#' Identifies the loaded Stan DLLs and unloads them.
#' @inheritParams capture_stan_DLLs 
#'
#' @seealso capture_stan_DLLs UnloadUserDLLs
#' @return This functions returns \code{NULL}
#' @export
#'
#' @examples
#' 
clear_user_DLL = function(verbose = TRUE) {
  
  tmp <- capture_stan_DLLs(verbose)
  
  UnloadUserDLLs(tmp, verbose)
}