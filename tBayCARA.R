#' The FUN.BCAR function
#'
#' @param data  A data frame that contain the covariates to be randomized.
#'
#' @param group.var The name of the grouping variable. If none, then skip.The default is group.
#'
#' @param categorical.covariates  A list of categorical covariates to be balanced.
#'
#' @param continuous.covariates A list of continuous covariates to be balanced.
#'
#' @param group.level A list of all potential groups.
#'
#' @param ratio The assignment ratio for groups listed in group.level.
#'
#' @param categorical.weights A list of weighting factors for the categorical covariates.
#'
#' @param continuous.weights  A list of weighting factors for the continuous covariates.
#'
#' @param start.number  From which subject that the continuous covariates will be balanced.
#'
#' @param num.categories  The number of categories for the categorical variable that a continuous covariate will be converted to. The defalut is 4.
#'
#' @param pct.deterministic  The percentage of subjects will be assigned to a group in a more deterministic manner.
#'
#' @param planned.sample.size  The planned sample size for the study.
#' @param transformation.parameter  Whether a square root transformation is used. If 1, then no transformation is appliced; if 1/2, then a square root transformation is applied.
#'
#' @examples
#' data = data.frame("gender" = sample(c("female", "male"), 100, TRUE, c(.5, .5)), "age" = sample(c("<=50", ">50"), 100, TRUE, c(.5, .5)),  stringsAsFactors = TRUE)
#' data$continuous1=rnorm(nrow(data))
#' data$continuous2=rnorm(nrow(data))
#'
#' data$group=FUN.BCAR(data, categorical.covariates = c("gender", "age"), continuous.covariates = c("continuous1", "continuous2"), group.level=c("A", "B", "C"))
#' table(data$gender, data$group)
#' table(data$age, data$group)
#' table(data$group)
#' boxplot(data$continuous1~data$group)
#'
#' @export

F.BayCARA=function(data, group.var, categorical.covariates, continuous.covariates, group.level, ratio, categorical.weights, continuous.weights, start.number, num.categories, pct.deterministic, planned.sample.size, pRA, transformation.parameter)
{ if (missing(planned.sample.size)) {planned.sample.size=nrow(data)
print("Please provide the planned sample size for this study by adding, for example, 'planned.sample.size=100' as the last component of the FUN.BCAR function. If not, then the randomization could be more deterministic.")}
  if (missing(num.categories)) {num.categories=3}
  if (missing(pct.deterministic)) {pct.deterministic=0.05}
  if (missing(start.number)) {start.number=15}
  if (missing(group.level)) {group.level=c("A", "B")}
  contin=cat.biasing=cont.biasing=1
  if (missing(continuous.covariates)) {contin=0} else {qk=rep(num.categories+1, length(continuous.covariates))}
  if (missing(ratio)) {ratio=rep(1, length(group.level))}
  if (missing(categorical.weights)&!missing(categorical.covariates)) {categorical.weights=rep(1, length(categorical.covariates))}
  if (missing(continuous.weights)&!missing(continuous.covariates)) {continuous.weights=rep(1, length(continuous.covariates))}
  start.seq=rep(sample(group.level, length(group.level), replace = FALSE),1)
  #  if (missing(group.var)) {data$group=NA}
  
  if (!missing(group.var)) {if (any(names(data)%in%group.var))
  {data$group=data[, group.var]}}
  
  #The number of subjects that have already been randomized;
  
  n.already.randomized=sum(!is.na(data$group))
  #print(n.already.randomized)
  # The randomization starts from the first subject that has not been randomized;
  start=n.already.randomized+1
  if (start<=nrow(data))
  {existing.groups=unique(data$group[1:n.already.randomized])
  non.levels=group.level[which(!group.level%in%existing.groups)]
  # The codes immediately generate permutated block randomization for the run-in period;
  if (length(non.levels)!=0)
  {n.permutation=start:min(nrow(data),(start-1+length(non.levels)))
  data$group[n.permutation]=c(non.levels, start.seq)[1:length(n.permutation)]}
  
  # Once the run-in period allocations are defined, re-define the first subject to start BayCAR;
  start=sum(!is.na(data$group))+1
  
  if (start<=nrow(data))
  {
    if (missing(categorical.covariates)&(contin==0)) {data$group[start:start.number]=sample(group.level, start.number-start+1, replace = TRUE)}
    else
    {#data$group[start:nrow(data)]=NA
      #      for (i in start:nrow(data))
      #  {
      
      ii=start
      if (!missing(categorical.covariates))
        # calculating biasing probability for each of the categorical covariates
      {for (j in 1:length(categorical.covariates))
      {cat.biasing.prob.out=FUN.AdaptP(data, ii, categorical.covariates[j], ratio, categorical.weights[j])
      cat.biasing.prob.out=cat.biasing.prob.out/sum(cat.biasing.prob.out)
      if (j==1) {cat.biasing=cat.biasing.prob.out} else
      {cat.biasing=cat.biasing*cat.biasing.prob.out}
      }
      }
      
      # For continuous covariates, we do not consider them until the number of randomized subjects
      # has reached start.number, which is preset arbitrarily;
      data$cont.to.cat=NA
      if (contin&(ii>start.number))
      {
        for (k in 1:length(continuous.covariates))
        {cont=data[1:ii,continuous.covariates[k]]
        ncc=quantile(cont, seq(0,1, length.out=qk[k]))
        ncc=ncc[-c(1, length(ncc))]
        data$cont.to.cat[1:ii]=1
        for (l in 1:length(ncc))
        {data$cont.to.cat[1:ii][cont>ncc[l]]=(l+1)
        }
        cont.biasing.prob.out=FUN.AdaptP(data, ii, "cont.to.cat", ratio, continuous.weights[k])*pRA
        
        cont.biasing.prob.out=cont.biasing.prob.out/sum(cont.biasing.prob.out)
        # if (length(unique(cont.biasing.prob.out))) {qk[k]=qk[k]+1; k=k-1}
        if (k==1) {cont.biasing=cont.biasing.prob.out} else
        {cont.biasing=cont.biasing*cont.biasing.prob.out}
        }}
      
      PPc= (cat.biasing*cont.biasing) 
      
      PPP=PPc^transformation.parameter*pRA
      
      PPP=PPP/sum(PPP)
      nc= floor((1-pct.deterministic) *planned.sample.size)
      if (ii> nc)
      {pwhtrt=sample(group.level, 1, prob=PPP^(seq(2, 9, length.out = nrow(data)-nc)[ii-nc]))} else
      {pwhtrt=sample(group.level, 1, prob=PPP)}
      
    }
  }
  }
  return(list(sample(pwhtrt,1), PPP))
}



###################################################################################3
#################################################################################
#' The FNN.out function
#'
#' @param data  A data frame that contain the covariates to be randomized.
#'
#' @param i The index of the subject to be randomized.
#'
#' @param covj  The name of the covariate, for which is biasing randomization probability will be calculated.
#'
#' @param ratio  The assignment ratio for groups listed in group.level.
#'
#' @param weights The weights of the covariate.
#'
#' @param group.level A list of all potential groups.
#'
#' @export

# The function for calculating biasing randomization probability
FUN.AdaptP=function(data, i, covj, ratio, weights, group.level)
{ink=0
if (covj=="group") {tg=t(table(data[,"group"]))} else
{tg= table(data[1:i,covj], factor(data$group[1:i], levels = group.level)) }

if (all(abs(tg[,1]-tg[,2])<1)) {ink=1}

gi=data[i, covj]
frq0=frq=t(tg[row.names(tg)==gi,])

if (all(frq==0)) {} else {frq=frq  /ratio    #*sum(ratio)/min(ratio)# *100
frq=frq*sum(frq0)/sum(frq)}
g.out=postP.Bfun(frq)
g.out=g.out^weights

return (g.out)
}


#################################################################################
###############################################################################



B.int = function(x, ddata, cdata, c)
{for (i in 1:length(cdata))
{pi=pbeta(x, sum(c(cdata, ddata))-cdata[i]+c, cdata[i]+c)
if (i==1) {pp=pi} else {pp=pp*pi}
}
  di=dbeta(x, sum(c(cdata, ddata))-ddata+c, ddata+c)
  pp*di
}

postP.Bfun=function(frq)
{if (all(frq==0)) {frq=1+frq}
  for (i in 1:length(frq))
  {data2=c(frq,frq)
  ddata=data2[i]+1; cdata=data2[(i+1):(i+length(frq)-1)] 
  Pi=integrate(B.int, ddata, cdata, c=0.5, lower=0, upper=1, rel.tol = .Machine$double.eps^.5)$value
  if (i==1) {pout=Pi} else {pout=c(pout, Pi)}
  }
  allP=pout/sum(pout)
  if (all(allP==0)) {allP=1+allP}
  whtrt= names(as.data.frame(frq))[which(allP==max(allP))]
  whtrt=sample(whtrt,1)
  #  return(list(whtrt, allP/sum(allP)))
  return(allP/sum(allP))
}

#postP.Bfun(c(5,6,7))
##############################################################################
##############################################################################
 

postP.Tfun = function(tdata, premu, presize, p3, p4, transformation.parameter) {
  if (missing(transformation.parameter)) {transformation.parameter=1/2}
  # Get unique treatment groups and their counts
  ntrt <- sort(unique(tdata$trt))
  nntrt <- length(ntrt)
  
  # If prior parameters are scalars, repeat them for all groups
  if (length(premu) == 1) premu <- rep(premu, nntrt)
  if (length(presize) == 1) presize <- rep(presize, nntrt)
  if (length(p3) == 1) p3 <- rep(p3, nntrt)
  if (length(p4) == 1) p4 <- rep(p4, nntrt)
  
  # Compute posterior means and standard deviations for each group
  df = post_means = post_scale = ybar = numeric(nntrt) 
  sntrt = sort(ntrt)
  
  for (i in 1:nntrt) {
    group_data <- tdata$y[tdata$trt == sntrt[i]]
    n_i <- length(group_data)
    ybar[i] = ybar_i <- mean(group_data, na.rm=TRUE)
    sig2_i <- var(group_data, na.rm=TRUE)
    
    # Degrees of freedom for t-distribution
    df[i] = 2 * (p3[i] + n_i / 2)
    
    # Posterior mean (location parameter)
    post_means[i] = (premu[i] * presize[i] + n_i * ybar_i) / (presize[i] + n_i)
    
    # Posterior scale parameter (accounts for sample variance and prior contribution)
    post_scale[i] = sqrt(((1 / 2) * (n_i - 1) * sig2_i + (presize[i] * n_i * (ybar_i - premu[i])^2) / 
                            (2 * (presize[i] + n_i))) / ((p3[i] + n_i / 2) * (presize[i] + n_i)))
  }
  
  post_means[is.na(post_means)]=mean(post_means, na.rm=TRUE)
  post_scale[is.na(post_scale)]=mean(post_scale, na.rm=TRUE)
  
  
  # Loop to compute posterior probability for each group
  for (j in 1:length(sntrt)) {
    # Identify the target group (T in your case)
    target_idx <- which(ntrt == sntrt[j])  # Change if needed
    target_mean <- post_means[target_idx]
    target_scale <- post_scale[target_idx]
    
    # Compute the probability that T has the highest mean
    integrand <- function(x) {
      prob <- 1
      for (i in 1:nntrt) {
        if (i != target_idx) {
          a <- (target_mean - post_means[i]) / post_scale[i]
          b <- target_scale / post_scale[i]
          prob <- prob * pt(a + b * x, df[i])
        }
      }
      prob * dt(x, df[target_idx])  # Multiply by standard t-distribution density
    }
    
    # Integrate over all possible x (standard normal)
    Pi <- integrate(integrand, lower = -Inf, upper = Inf)$value
    if (j == 1) {
      outp = c(sntrt[j], Pi)
    } else {
      outp = rbind(outp, c(sntrt[j], Pi))
    }
  }
  
  outp=as.data.frame(outp)
  doutp = as.data.frame(cbind(outp, post_means, post_scale, df, ybar))
  names(doutp) = c("Group", "PosteriorP", "Post_mean", "Post_scale", "df", "ybar")
  doutp$PosteriorP=as.numeric(doutp$PosteriorP)
 
  doutp$PosteriorP=doutp$PosteriorP^transformation.parameter
  
  doutp$PosteriorP= doutp$PosteriorP/sum(doutp$PosteriorP) 
  row.names(doutp) = doutp$Group
  
  #print(doutp[,2])
  return(doutp)
}



