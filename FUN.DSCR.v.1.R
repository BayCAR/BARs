library('dplyr')
library('tidyr')
library('ggplot2')

FUN.Beta.est <- function(data, kluster, nstart = 25) {
  # Input validation
  if (!all(c("id", "x", "y") %in% names(data))) {
    stop("Data must contain 'id', 'x', and 'y' columns")
  }
  if (kluster < 1) stop("Number of clusters must be >= 1")
  
  # Reshape data for k-means
  outx <- as.data.frame(data) %>% 
    pivot_wider(names_from = x, values_from = y)
  
  row.names(outx) <- outx$id
  touts <- as.data.frame(outx[,-1])  
  
  # K-means with multiple starts
  kmeans_result <- kmeans(touts, centers = kluster, nstart = nstart)
  
  # Prepare data for regression
  data$kmean_cluster <- rep(as.vector(kmeans_result$cluster), 
                           each = length(unique(data$x)))
  data$x2 <- data$x^2
  data$x3 <- data$x^3
  
  # Fit polynomial regression for each cluster
  Beta <- matrix(NA, nrow = 4, ncol = kluster)
  rownames(Beta) <- c("(Intercept)", "x", "x2", "x3")
  
  for (k in 1:kluster) {
    data_k <- data[data$kmean_cluster == k,]
    if (nrow(data_k) > 0) {
      lm_fit <- try(lm(y ~ x + x2 + x3, data = data_k))
      if (!inherits(lm_fit, "try-error")) {
        Beta[,k] <- coef(lm_fit)
      } else {
        Beta[,k] <- rnorm(4, sd = 0.1) # Small random values if lm fails
      }
    } else {
      Beta[,k] <- rnorm(4, sd = 0.1) # Small random values if no data
    }
  }
  
  return(list(kmean_cluster = data$kmean_cluster, 
              Beta = Beta,
              kmeans_result = kmeans_result))
}

FUN.Beta.est <- function(data, kluster, nstart = 25) {
  # Input validation
  if (!all(c("id", "x", "y") %in% names(data))) {
    stop("Data must contain 'id', 'x', and 'y' columns")
  }
  
  # Create subject-level features (no longer assuming same x values)
  subject_features <- data %>%
    group_by(id) %>%
    summarize(
      mean_y = mean(y),
      sd_y = sd(y),
      slope = ifelse(n() > 1, coef(lm(y ~ x))[2], 0),
      .groups = 'drop'
    ) %>%
    select(-id) %>%
    as.matrix()
  
  # K-means on subject features
  kmeans_result <- kmeans(subject_features, centers = kluster, nstart = nstart)
  
  # Prepare data for regression
  data$kmean_cluster <- rep(kmeans_result$cluster, table(data$id))
  data$x2 <- data$x^2
  data$x3 <- data$x^3
  
  # Fit polynomial regression for each cluster
  Beta <- matrix(NA, nrow = 4, ncol = kluster)
  rownames(Beta) <- c("(Intercept)", "x", "x2", "x3")
  
  for (k in 1:kluster) {
    data_k <- data[data$kmean_cluster == k,]
    if (nrow(data_k) > 0) {
      lm_fit <- try(lm(y ~ x + x2 + x3, data = data_k))
      if (!inherits(lm_fit, "try-error")) {
        Beta[,k] <- coef(lm_fit)
      } else {
        Beta[,k] <- rnorm(4, sd = 0.1)
      }
    } else {
      Beta[,k] <- rnorm(4, sd = 0.1)
    }
  }
  
  return(list(kmean_cluster = data$kmean_cluster, 
              Beta = Beta,
              kmeans_result = kmeans_result))
}

FUN.Beta.est <- function(data, kluster, nstart = 25) {
  # Input validation
  if (!all(c("id", "x", "y") %in% names(data))) {
    stop("Data must contain 'id', 'x', and 'y' columns")
  }
  
  # Convert to data.frame if tibble
  data <- as.data.frame(data)
  
  # Create subject-level features (handles varying x-values)
  features <- data %>%
    group_by(id) %>%
    summarize(
      obs_count = n(),
      mean_y = mean(y, na.rm = TRUE),
      sd_y = sd(y, na.rm = TRUE),
      slope = ifelse(obs_count > 1, 
                     coef(lm(y ~ x, na.action = na.omit))[2], 
                     0),
      .groups = 'drop'
    ) %>%
    mutate(across(everything(), ~replace(., is.na(.) | is.infinite(.), 0))) %>%
    select(-id, -obs_count) %>%
    as.matrix()
  
  # Robust k-means with error handling
  kmeans_result <- tryCatch({
    set.seed(123) # for reproducibility
    kmeans(features, centers = kluster, nstart = nstart)
  }, error = function(e) {
    # Fallback to random centers if k-means fails
    centers <- features[sample(nrow(features), kluster), , drop = FALSE]
    kmeans(features, centers = centers)
  })
  
  # Prepare data for regression
  data$kmean_cluster <- rep(kmeans_result$cluster, table(data$id))
data$x2 <- data$x^2
data$x3 <- data$x^3

# Robust regression fitting
Beta <- matrix(NA, nrow = 4, ncol = kluster)
rownames(Beta) <- c("(Intercept)", "x", "x2", "x3")

for (k in 1:kluster) {
  data_k <- data[data$kmean_cluster == k, ]
  if (nrow(data_k) > 0) {
    Beta[, k] <- tryCatch({
      coef(lm(y ~ x + x2 + x3, data = data_k, na.action = na.omit))
    }, error = function(e) {
      # Fallback initialization if regression fails
      c(mean(data_k$y, na.rm = TRUE), runif(3, -0.5, 0.5))
    })
  } else {
    Beta[, k] <- c(mean(data$y, na.rm = TRUE), runif(3, -0.5, 0.5))
  }
}

return(list(
  kmean_cluster = data$kmean_cluster,
  Beta = Beta,
  kmeans_result = kmeans_result
))
}

# Helper functions
create_design_matrix <- function(data, contcov, catcov) {
  # Base polynomial terms
       
  x_vars <- c("int", "x", "x2", "x3")
  X <- as.matrix(data[, x_vars])
  X <- model.matrix(~ x + x2 + x3, data = data)
  colnames(X) <- x_vars
  
  # Add continuous covariates
  if (!is.null(contcov)) {
    X <- cbind(X, as.matrix(data[, contcov, drop = FALSE]))
  }
  
  # Add categorical covariates (as dummy variables)
  if (!is.null(catcov)) {
    for (cat in catcov) {
      if (is.factor(data[[cat]])) {
        contrasts(data[[cat]]) <- contr.treatment(levels(data[[cat]]), 
                                                 base = 1)
      }
      mm <- model.matrix(as.formula(paste("~", cat)), data)[,-1, drop = FALSE]
      X <- cbind(X, mm)
    }
  }
  
### Add interactions
#  if (!is.null(contcov)) {
#    for (var in c("x", "x2", "x3")) {
#      for (cont in contcov) {
#        X <- cbind(X, data[[var]] * data[[cont]])
#        colnames(X)[ncol(X)] <- paste0(var, "_", cont)
#      }
#    }
#  }
  
#  if (!is.null(catcov)) {
#    for (var in c("x", "x2", "x3")) {
#      for (cat in catcov) {
#        if (is.factor(data[[cat]])) {
#          for (lev in levels(data[[cat]])[-1]) {
#            X <- cbind(X, data[[var]] * (data[[cat]] == lev))
#            colnames(X)[ncol(X)] <- paste0(var, "_", cat, lev)
#          }
#        } else {
#          X <- cbind(X, data[[var]] * data[[cat]])
#          colnames(X)[ncol(X)] <- paste0(var, "_", cat)
#        }
#      }
#    }
#  }
  
  return(X)
}

  

 

calculate_responsibilities <- function(X, y, Beta, sdd, pii, ids, data) {
  unique_ids <- unique(ids)
  n_subjects <- length(unique_ids)
  kluster <- ncol(Beta)
  
  # Create subject-level responsibility matrix
  subject_T <- matrix(NA, n_subjects, kluster)
  
  for (k in 1:kluster) {
    for (i in 1:n_subjects) {
      subject_rows <- which(ids == unique_ids[i])
      residuals <- y[subject_rows] - X[subject_rows, ] %*% Beta[,k]
      subject_T[i,k] <- log(pii[k]) + 
        sum(dnorm(residuals, mean=0, sd=sdd[k], log=TRUE))
    }
  }
  
  # Apply softmax normalization by subject
  subject_T <- exp(subject_T - apply(subject_T, 1, max))
  subject_T <- subject_T / rowSums(subject_T)
  
  # Expand to observation-level matrix
  T <- subject_T[match(ids, unique_ids), ]
  
  return(list(T=T, subject_T=subject_T))
}

calculate_responsibilities <- function(X, y, Beta, sdd, pii, ids) {
  unique_ids <- unique(ids)
  n_subjects <- length(unique_ids)
  kluster <- ncol(Beta)
  subject_T <- matrix(NA, n_subjects, kluster)
  
  for (k in 1:kluster) {
    for (i in 1:n_subjects) {
      subject_rows <- which(ids == unique_ids[i])
      x_subject <- X[subject_rows, , drop = FALSE]
      y_subject <- y[subject_rows]
      
      # Handle cases where design matrix might be singular
      if (nrow(x_subject) >= ncol(x_subject)) {
        residuals <- y_subject - x_subject %*% Beta[,k]
        subject_T[i,k] <- log(pii[k]) + 
          sum(dnorm(residuals, mean = 0, sd = sdd[k], log = TRUE))
      } else {
        # Fallback for subjects with too few observations
        subject_T[i,k] <- log(pii[k]) + 
          sum(dnorm(y_subject, mean = mean(y_subject), 
                    sd = sdd[k], log = TRUE))
      }
    }
  }
  
  # Softmax normalization
  subject_T <- exp(subject_T - apply(subject_T, 1, max))
  subject_T <- subject_T / rowSums(subject_T)
  
  # Expand to observation-level
  T <- subject_T[match(ids, unique_ids), ]
  
  return(list(T = T, subject_T = subject_T))
}

FUN.cr <- function(data, kluster, Beta, sdd, contcov, catcov, 
                   max.iter = 100, threshold = 1e-6, 
                   equal.variance = TRUE, verbose = TRUE) {
  
  # Input validation
  if (!all(c("id", "x", "y") %in% names(data))) {
    stop("Data must contain 'id', 'x', and 'y' columns")
  }
  if (missing(Beta)) {
    stop("Please provide initial Beta values from FUN.Beta.est")
  }
  
  # Prepare data
  ids <- unique(data$id)
  n_subjects <- length(ids)
  data$int <- 1
  data$x2 <- data$x^2
  data$x3 <- data$x^3
  
  # Handle missing covariates
  if (missing(contcov)) contcov <- NULL
  if (missing(catcov)) catcov <- NULL
  
  # Create design matrix with interactions
  X <- create_design_matrix(data, contcov, catcov)

  y <- data$y
  
  # Initialize parameters
  if (missing(sdd)) sdd <- rep(sd(y), kluster)
  pii <- rep(1/kluster, kluster)
  
  # Pad Beta if needed
  if (nrow(Beta) < ncol(X)) {
    n_missing <- ncol(X) - nrow(Beta)
    Beta_add <- matrix(rnorm(n_missing * kluster, sd = 0.1), nrow = n_missing)
    Beta <- rbind(Beta, Beta_add)
  }
  
  # EM algorithm
  llk_history <- numeric(max.iter)
  converged <- FALSE
  
  for (iter in 1:max.iter) {
    # E-step: Calculate subject-level responsibilities
    responsibility <- calculate_responsibilities(X, y, Beta, sdd, pii, data$id)
    T <- responsibility$T
    subject_T <- responsibility$subject_T
    
    # M-step: Update parameters
    pii_new <- colMeans(subject_T)
    
    # Update Beta and sdd for each cluster
    for (k in 1:kluster) {
      # Weighted regression using subject weights
      subject_weights <- sqrt(subject_T[,k])
      weights <- subject_weights[match(data$id, ids)]
      X_weighted <- X * weights
      y_weighted <- y * weights
      
      Beta[,k] <- tryCatch(
        {
          qr.coef(qr(X_weighted), y_weighted)
        },
        error = function(e) {
          Beta[,k] + rnorm(ncol(X), sd = 0.01)
        }
      )
      
      # Update variance
      residuals <- y - X %*% Beta[,k]
      sdd[k] <- sqrt(sum(T[,k] * residuals^2) / sum(T[,k]))
    }
    
    if (equal.variance) {
      sdd <- rep(mean(sdd, na.rm = TRUE), kluster)
    }
    
    # Calculate log-likelihood (subject-level)
    llk <- 0
    for (k in 1:kluster) {
      for (i in 1:n_subjects) {
        subject_rows <- which(data$id == ids[i])
        residuals <- y[subject_rows] - X[subject_rows, ] %*% Beta[,k]
        llk <- llk + subject_T[i,k] * (log(pii_new[k]) + 
                                         sum(dnorm(residuals, mean = 0, sd = sdd[k], log = TRUE)))
      }
    }
    llk_history[iter] <- llk
    
    if (verbose) {
      cat(sprintf("Iter %d: log-likelihood = %.3f\n", iter, llk))
    }
    
    # Check convergence
    if (iter > 1 && abs(llk_history[iter] - llk_history[iter-1]) < threshold) {
      converged <- TRUE
      break
    }
    
    pii <- pii_new
  }
  
  
  marg_llk <- marginal_loglik(X, y, Beta, sdd, pii, data$id)
  
  # Calculate information criteria
  n_params <- kluster * ncol(X) + ifelse(equal.variance, 1, kluster)
  aic <- -2 * marg_llk + 2 * n_params
  bic <- -2 * marg_llk + log(n_subjects) * n_params 
  
  # Get subject-level cluster assignments
  subject_assign <- apply(subject_T, 1, which.max)
  cluster_assign <- subject_assign[match(data$id, ids)]
  
  return(list(
    ids = ids,
    T = T,
    subject_T = subject_T,
    Beta = Beta,
    sdd = sdd,
    pii = pii,
    llk = llk,                   # Complete data log-likelihood
    marg_llk = marg_llk,         # Marginal log-likelihood
    aic = aic,
    bic = bic,
    n_clusters = kluster,
    cluster_assign = cluster_assign,
    converged = converged,
    iterations = iter
  ))
}

# Calculate marginal log-likelihood for AIC/BIC
marginal_loglik <- function(X, y, Beta, sdd, pii, ids) {
  unique_ids <- unique(ids)
  n_subjects <- length(unique_ids)
  kluster <- ncol(Beta)
  marg_llk <- 0
  
  for (i in 1:n_subjects) {
    subject_rows <- which(ids == unique_ids[i])
    subject_llk <- 0
    
    for (k in 1:kluster) {
      residuals <- y[subject_rows] - X[subject_rows, ] %*% Beta[,k]
      subject_llk <- subject_llk + 
        pii[k] * exp(sum(dnorm(residuals, mean = 0, sd = sdd[k], log = TRUE)))
    }
    marg_llk <- marg_llk + log(subject_llk)
  }
  return(marg_llk)
}


FUN.select_clusters <- function(data, max_clusters = 5, verbose = TRUE, ...) {
  # Convert data to data.frame if it's a tibble
  data <- as.data.frame(data)
  
  results <- list()
  aic_values <- numeric(max_clusters)
  bic_values <- numeric(max_clusters)
  marg_llk_values <- numeric(max_clusters)
  
  for (k in 1:max_clusters) {
    if (verbose) message("Testing k = ", k)
    
    # Get initial estimates
    init <- FUN.Beta.est(data, kluster = k)
    
    # Run clustering regression with verbose parameter passed through
    cr_result <- tryCatch({
      FUN.cr(data, 
             kluster = k, 
             Beta = init$Beta, 
             verbose = verbose,
             ...)
    }, error = function(e) {
      if (verbose) message("Error with k=", k, ": ", e$message)
      return(NULL)
    })
    
    if (!is.null(cr_result)) {
      # Store results
      results[[k]] <- cr_result
      aic_values[k] <- cr_result$aic
      bic_values[k] <- cr_result$bic
      marg_llk_values[k] <- cr_result$marg_llk
      if (verbose) message("Completed k = ", k, " | BIC = ", round(cr_result$bic, 2))
    } else {
      aic_values[k] <- NA
      bic_values[k] <- NA
      marg_llk_values[k] <- NA
    }
  }
  
  # Find optimal number based on BIC
  optimal_k <- which.min(bic_values)
  
  if (verbose) {
    message("\nCluster selection results:")
    print(data.frame(
      Clusters = 1:max_clusters,
      BIC = round(bic_values, 2),
      AIC = round(aic_values, 2),
      marg_LLK = round(marg_llk_values, 2)
    ))
    message("\nOptimal number of clusters (by BIC): ", optimal_k)
  }
  
  return(list(
    results = results,
    aic_values = aic_values,
    bic_values = bic_values,
    marg_llk_values = marg_llk_values,
    optimal_k = optimal_k
  ))
}
