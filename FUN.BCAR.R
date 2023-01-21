#' The FUN.BCAR function
#'
#' @param data  A data frame that contains the covariates to be randomized.
#'
#' @param group.var The name of the grouping variable. The default is group.
#'
#' @param categorical.covariates  A list of categorical covariates to be balanced.
#'
#' @param continuous.covariates A list of continuous covariates to be balanced.
#'
#' @param group.level A list of all potential group ids.
#'
#' @param ratio The assignment ratio for groups listed in group.level.
#'
#' @param categorical.weights A list of weighting factors for the categorical covariates.
#'
#' @param continuous.weights  A list of weighting factors for the continuous covariates.
#'
#' @param start.number  From which subject that the continuous covariates will be balanced. The default is 15.
#'
#' @param num.categories  The number of categories preset for the categorical variable that a continuous covariate will be converted to. The defalut is 4.
#'
#' @param pct.deterministic  The percentage of subjects that will be assigned to a group in a more deterministic manner.
#'
#' @param planned.sample.size  The planned sample size for the study.
#'
#' @examples
#' data = data.frame("gender" = sample(c("female", "male"), 100, TRUE, c(.5, .5)),
#'                   "age" = sample(c("<=50", ">50"), 100, TRUE, c(.5, .5)))
#' data$continuous1=rnorm(nrow(data))
#' data$continuous2=rnorm(nrow(data))
#' 
#' data1=data[c(1,2),]
#' data1$group=FUN.BCAR(data1, group.var="group", categorical.covariates=c("gender", "age"),
#' continuous.covariates=c("continuous1", "continuous2"), group.level=c("A", "B", "C"))
#' 
#' data2=data[-c(1,2),]
#' data2$group=NA
#' datanew=rbind(data1, data2)
#' datanew$group=FUN.BCAR(datanew, group.var="group", categorical.covariates=c("gender", "age"),
#' continuous.covariates=c("continuous1", "continuous2"), group.level=c("A", "B", "C"), 
#' planned.sample.size=100)
#' 
#' #table(datanew$gender, datanew$group)
#' #table(datanew$age, datanew$group)
#' #table(datanew$group)
#' #boxplot(datanew$continuous1~datanew$group)
#'
#' @export

FUN.BCAR= function (data, group.var, categorical.covariates, continuous.covariates,
                    group.level, ratio, categorical.weights, continuous.weights,
                    start.number, num.categories, pct.deterministic, planned.sample.size)
{
  if (missing(planned.sample.size)) {
    planned.sample.size = nrow(data)
    print("Please provide the planned sample size for this study, e.g., 'planned.sample.size=100'. If not, then the randomization could be more deterministic.")
  }
  if (missing(num.categories)) {
    num.categories = 4
  }
  if (missing(pct.deterministic)) {
    pct.deterministic = 0.05
  }
  if (missing(start.number)) {
    start.number = 15
  }
  if (missing(group.level)) {
    group.level = c("A", "B")
  }
  contin = PP = cPP = 1
  if (missing(continuous.covariates)) {
    contin = 0
  }
  else {
    qk = rep(num.categories + 1, length(continuous.covariates))
  }
  if (missing(ratio)) {
    ratio = rep(1, length(group.level))
  }
  if (missing(categorical.weights) & !missing(categorical.covariates)) {
    categorical.weights = rep(1, length(categorical.covariates))
  }
  if (missing(continuous.weights) & !missing(continuous.covariates)) {
    continuous.weights = rep(1, length(continuous.covariates))
  }
  start.seq = rep(sample(group.level, length(group.level),
                         replace = FALSE), 5)
  if (missing(group.var)) {print(paste("Please provide the name of the treatment group variable! It is preferable <case sensitive> to use: group.var='group'."))
   } else {
  if (!missing(group.var)) {
    if (any(names(data) %in% group.var)) {
      data$group = as.character(data[, group.var])
     } else {data$group=NA}
  }
  non.missing = sum(!is.na(data$group))
  if (non.missing==nrow(data)) {non.missing=sum( data$group!="")}

  start = non.missing + 1
  if (start==1) {print("FYI: No subject has been assigned a group prior to running this application.")} else
  {print(paste0("FYI: ", start-1, " subject(s) has(ve) been assigned to a group prior to running this application."))}
    if (start <= nrow(data)) {
    existing.groups = unique(data$group[1:non.missing])
    non.levels = group.level[which(!group.level %in% existing.groups)]
    non.levels=sample(non.levels)
    if (length(non.levels) != 0) {
      n.permutation = start:min(nrow(data), (start - 1 +
                                               length(non.levels)))
      data$group[n.permutation] = c(non.levels, start.seq)[1:length(n.permutation)]
    }
    non.missing = sum(!is.na(data$group))
    if (non.missing==nrow(data)) {non.missing=sum( data$group!="")}

    start = non.missing + 1
    if (start <= nrow(data)) {
      if (missing(categorical.covariates)) {#not necessary
        data$group[start:start.number] = sample(group.level,
                                                start.number - start + 1, replace = TRUE)
      }
      data$group[start:nrow(data)] = NA
      for (i in start:nrow(data)) {
        if (!missing(categorical.covariates)) {
          for (j in 1:length(categorical.covariates)) {
            NN.out = FNN.out(data, i, categorical.covariates[j],
                             ratio, categorical.weights[j])
            NN.out[[1]] = NN.out[[1]]/sum(NN.out[[1]])
            if (j == 1) {
              PP = NN.out[[1]]
            }
            else {
              PP = PP * NN.out[[1]]
            }
          }
        }
        if (contin & i > start.number) {
          for (k in 1:length(continuous.covariates)) {
            cont = data[1:i, continuous.covariates[k]]
            ncc = stats::quantile(cont, seq(0, 1, length.out = qk[k]))
            ncc = ncc[-c(1, length(ncc))]
            data$ccont = 1
            for (l in 1:length(ncc)) {
              data$ccont[cont > ncc[l]] = (l + 1)
            }
            CNN.out = FNN.out(data, i, "ccont",
                              ratio, continuous.weights[k])
            CNN.out[[1]] = CNN.out[[1]]/sum(CNN.out[[1]])
            if (CNN.out[[2]] == 1) {
              qk[k] = qk[k] + 1
            }
            if (k == 1) {
              cPP = CNN.out[[1]]
            }
            else {
              cPP = cPP * CNN.out[[1]]
            }
          }
        }
        PPP = PP * cPP
        PPP = PPP/sum(PPP)
        nc = floor((1 - pct.deterministic) * planned.sample.size)
        if (i > nc) {
          pwhtrt = sample(group.level, 1, prob = PPP^(seq(2,
                                                          9, length.out = nrow(data) - nc)[i - nc]))
        }
        else {
          pwhtrt = sample(group.level, 1, prob = PPP)
        }
        data$group[i] = sample(pwhtrt, 1)
      }
     }
    }
   }
  return(data$group)
}

