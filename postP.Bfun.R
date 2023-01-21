#' The postP.Bfun function
#'
#' @param frq  The observed frquency of each group.
#'
#' @examples
#'
#' postP.Bfun(c(20,30,50))
#'
#' @export


postP.Bfun=function(frq)
{if (all(frq==0)) {frq=1+frq}
  for (i in 1:length(frq))
  {data2=c(frq,frq)
  ddata=data2[i]+1; cdata=data2[(i+1):(i+length(frq)-1)]
  Pi=stats::integrate(B.int, ddata, cdata, constant=0.5, lower=0, upper=1, rel.tol = .Machine$double.eps^.5)$value
  if (i==1) {pout=Pi} else {pout=c(pout, Pi)}
  }
  allP=pout/sum(pout)
  if (all(allP==0)) {allP=1+allP}
  whtrt= names(as.data.frame(frq))[which(allP==max(allP))]
  whtrt=sample(whtrt,1)
  #  return(list(whtrt, allP/sum(allP)))
  return(allP/sum(allP))
}
