#' The B.int function
#'
#' @param x  A variable for integraton.
#'
#' @param ddata A data frame contains the posterior means and SDs for all the groups.
#'
#' @param cdata A data frame contains the posterior means and SDs for all the groups.
#'
#' @param constant  A constant to be added to the Beta distribution to reflect the appropriate prior. For example, constant=1 reflects uniform (0,1), and constant=0.5 reflects Jeffery's prior.
#'
#' @examples
#'
#' # This is the numerical integration function. Do not need to be run independently.
#'
#' @export


##############################
# The integration function for Beta distributions

B.int = function(x, ddata, cdata, constant)
{for (i in 1:length(cdata))
{pi=stats::pbeta(x, sum(c(cdata, ddata))-cdata[i]+constant, cdata[i]+constant)
if (i==1) {pp=pi} else {pp=pp*pi}
}
  di=stats::dbeta(x, sum(c(cdata, ddata))-ddata+constant, ddata+constant)
  pp*di
}
