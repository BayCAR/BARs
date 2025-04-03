# Load necessary library
library(mvtnorm)

# Define sample size for each group and the number of groups
n = nX = nY = nZ = nT = 100  # Sample size for each group (X, Y, Z, T)

# Define the variance (standard deviation squared) for each group
sigX2x = sigY2x = sigZ2x = sigT2x = 12^2  # Variance for each group

# Define the sample means for each group
ryX = 82; ryY = 88; ryZ = 88; ryT = 90  # Sample means for each group

# Generate random data for each group
tdata = rbind(data.frame(y = rnorm(n, ryX, sqrt(sigX2x)), trt = "X"),
              data.frame(y = rnorm(n, ryY, sqrt(sigY2x)), trt = "Y"),
              data.frame(y = rnorm(n, ryZ, sqrt(sigZ2x)), trt = "Z"),
              data.frame(y = rnorm(n, ryT, sqrt(sigT2x)), trt = "T"))

# Define prior mean and variance
mu0 = 86; sig02 = 50^2  # Prior mean and variance

# Calculate sample means and variances for each group
ybarX = mean(tdata$y[tdata$trt == "X"])
sigX2 = var(tdata$y[tdata$trt == "X"])
ybarY = mean(tdata$y[tdata$trt == "Y"])
sigY2 = var(tdata$y[tdata$trt == "Y"])
ybarZ = mean(tdata$y[tdata$trt == "Z"])
sigZ2 = var(tdata$y[tdata$trt == "Z"])
ybarT = mean(tdata$y[tdata$trt == "T"])
sigT2 = var(tdata$y[tdata$trt == "T"])

# Compute the posterior mean and standard deviation for each group
meanX = (n * ybarX / sigX2 + mu0 / sig02) / (n / sigX2 + 1 / sig02)
sdX = sqrt(1 / (n / sigX2 + 1 / sig02))

meanY = (n * ybarY / sigY2 + mu0 / sig02) / (n / sigY2 + 1 / sig02)
sdY = sqrt(1 / (n / sigY2 + 1 / sig02))

meanZ = (n * ybarZ / sigZ2 + mu0 / sig02) / (n / sigZ2 + 1 / sig02)
sdZ = sqrt(1 / (n / sigZ2 + 1 / sig02))

meanT = (n * ybarT / sigT2 + mu0 / sig02) / (n / sigT2 + 1 / sig02)
sdT = sqrt(1 / (n / sigT2 + 1 / sig02))

# Standardize the differences between group T and the other groups (X, Y, Z)
a = (meanT - meanX) / sdX  # Standardized difference between T and X
b = sdT / sdX  # Ratio of standard deviations (T vs. X)
c = (meanT - meanY) / sdY  # Standardized difference between T and Y
d = sdT / sdY  # Ratio of standard deviations (T vs. Y)
e = (meanT - meanZ) / sdZ  # Standardized difference between T and Z
f = sdT / sdZ  # Ratio of standard deviations (T vs. Z)

# Define the integrand for the posterior probability computation
integrand = function(x, a, b, c, d, e, f) {
  # Multiply the normal CDFs for each pair of groups, weighted by the normal PDF
  pnorm(a + b * x) * pnorm(c + d * x) * pnorm(e + f * x) * dnorm(x)
}

# Compute the integral to estimate the posterior probability that group T has the highest mean
P = integrate(integrand, lower = -Inf, upper = Inf, a, b, c, d, e, f)$value

# Output the estimated posterior probability
P

#####################################################################
#####################################################################
#####################################################################

# Now we use generic code to do the calculation assuming variance known
postP.Nfun = function(tdata, premu, presd) {
  # Get unique treatment groups and their counts
  ntrt <- sort(unique(tdata$trt))
  nntrt <- length(ntrt)
  
  # If prior parameters are scalars, repeat them for all groups
  if (length(premu) == 1) premu <- rep(premu, nntrt)
  if (length(presd) == 1) presd <- rep(presd, nntrt)
  
  # Compute posterior means and standard deviations for each group
  post_means <- numeric(nntrt)
  post_sds <- numeric(nntrt)
  ybar <- numeric(nntrt)
  sntrt = sort(ntrt)
  
  for (i in 1:nntrt) {
    group_data <- tdata$y[tdata$trt == sntrt[i]]
    n_i <- length(group_data)
    ybar[i] = ybar_i <- mean(group_data)
    sig2_i <- var(group_data)
    
    # Posterior mean and SD (same as Option 1)
    post_means[i] <- (n_i * ybar_i / sig2_i + premu[i] / presd[i]^2) / 
      (n_i / sig2_i + 1 / presd[i]^2)
    post_sds[i] <- sqrt(1 / (n_i / sig2_i + 1 / presd[i]^2))
  }
  
  # Loop to compute posterior probability for each group
  for (j in 1:length(sntrt)) {
    # Identify the target group (T in your case)
    target_idx <- which(ntrt == sntrt[j])  # Change if needed
    target_mean <- post_means[target_idx]
    target_sd <- post_sds[target_idx]
    
    # Compute the probability that T has the highest mean
    integrand <- function(x) {
      # For each non-target group, compute P(T > group_i | x)
      prob <- 1
      for (i in 1:nntrt) {
        if (i != target_idx) {
          a <- (target_mean - post_means[i]) / post_sds[i]
          b <- target_sd / post_sds[i]
          prob <- prob * pnorm(a + b * x)
        }
      }
      prob * dnorm(x)  # Multiply by standard normal density
    }
    
    # Integrate over all possible x (standard normal)
    Pi <- integrate(integrand, lower = -Inf, upper = Inf)$value
    if (j == 1) {
      outp = c(sntrt[j], Pi)
    } else {
      outp = rbind(outp, c(sntrt[j], Pi))
    }
  }
  
  doutp = data.frame(outp, post_means, ybar)
  names(doutp) = c("Group", "PosteriorP", "Post_mean", "ybar")
  row.names(doutp) = doutp$Group
  return(doutp)
}

# Define prior parameters and compute posterior probabilities assuming known variance
premu = mu0
presd = sqrt(sig02)

postP.Nfun(tdata, premu, presd)

#####################################################################
#####################################################################
#####################################################################

# Now we use generic code to do the calculation assuming variance unknown
postP.Tfun = function(tdata, premu, presize, p3, p4) {
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
    ybar[i] = ybar_i <- mean(group_data)
    sig2_i <- var(group_data)
    
    # Degrees of freedom for t-distribution
    df[i] = 2 * (p3[i] + n_i / 2)
    
    # Posterior mean (location parameter)
    post_means[i] = (premu[i] * presize[i] + n_i * ybar_i) / (presize[i] + n_i)
    
    # Posterior scale parameter (accounts for sample variance and prior contribution)
    post_scale[i] = sqrt(((1 / 2) * (n_i - 1) * sig2_i + (presize[i] * n_i * (ybar_i - premu[i])^2) / 
                            (2 * (presize[i] + n_i))) / ((p3[i] + n_i / 2) * (presize[i] + n_i)))
  }
  
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
  
  doutp = data.frame(outp, post_means, post_scale, df, ybar)
  names(doutp) = c("Group", "PosteriorP", "Post_mean", "Post_scale", "df", "ybar")
  row.names(doutp) = doutp$Group
  return(doutp)
}

# Define prior parameters and compute posterior probabilities assuming unknown variance
premu = mu0  # Prior mean
presize = 1  # Prior sample size equivalent (informative if larger)
p3 = 1   # Shape parameter for Gamma prior (affects variance estimation)
p4 = 50^2  # Scale parameter for Gamma prior (prior variance)

postP.Tfun(tdata, premu, presize, p3, p4)
