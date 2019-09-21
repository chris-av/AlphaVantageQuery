#' Initialize API Query
#'
#' @param x
#'
#' @return sets an environmental variable that safely stores your API key. Should not appear in your environment.
#' @export
#'
#' @examples
alpha_vantage_auth = function(x) {
  Sys.setenv("auth_key" = as.character(x))
}
