#' The FNN.out function
#'
#' @param data  A data frame that contain the covariates to be randomized.
#'
#' @param i The index of the subject to be randomized.
#'
#' @param covj  The vector of covariate names, for which the biasing randomization probability will be calculated.
#'
#' @param ratio  The assignment ratio for groups listed in group.level.
#'
#' @param weights The weights of the covariates.
#'
#' @param group.level A list of all potential groups.
#'
#' @export

# The function for calculating biasing randomization probability
FNN.out=function(data, i, covj, ratio, weights, group.level)
{ink=0
if (covj=="group") {tg=t(table(data[,"group"]))} else
{tg= table(data[1:i,covj], factor(data$group[1:i], levels = group.level)) }
if (all(abs(tg[,1]-tg[,2])<1)) {ink=1}

gi=data[i, covj]
frq0=frq=t(tg[row.names(tg)==gi,])

if (all(frq==0)) {} else {frq=frq  /ratio 
frq=frq*sum(frq0)/sum(frq)}
g.out=postP.Bfun(frq)
g.out[[2]]=g.out[[2]]^weights

return (list(g.out, ink))
}
