library(limosa.beta)
library(future)
plan(multisession, workers = 6)
library(listenv)


N <- 500 ## number of repetitions
boot_samples <- 2500 
alpha <- 0.1
grid_specs <- list(num_x = 200, num_y = 200, num_z = 200)



sensana_bootstrap <- function(sa) {
  pir_est <- pir(sa, grid_specs = grid_specs)
  
  sensint_obj <- sensint(sa, alpha = alpha,
                         boot_procedure = c("perc", "basic", "bca"),
                         boot_samples = boot_samples,
                         grid_specs = grid_specs)
  
  ## The following depends on the order of boot procedures!
  df <- sensint_obj$sensint
  percent_usual <- c(df[1,1], df[1,2])
  basic_usual <- c(df[3,1], df[3,2])
  bca_usual <- c(df[5,1], df[5,2])
  percent_con <- c(df[2,1], df[2,2])
  basic_con <- c(df[4,1], df[4,2])
  bca_con <- c(df[6,1], df[6,2])
  
  list(sensints = c(percent_usual, basic_usual, bca_usual,
                    percent_con, basic_con, bca_con),
       n_empty = sensint_obj$n_empty,
       t = sensint_obj$boot_obj$t,
       pir = pir_est,
       boot_obj = sensint_obj$boot_obj)
}



sensana_ch <- function(sa, data) {
  ret <- feasible_grid(sa$y, sa$d, sa$xt, sa$xp, sa$z, sa$bounds,
                       grid_specs = grid_specs, full_grid = TRUE)
  a_seq <- ret$a_seq
  b_mat <- ret$b_mat
  a_mat <- matrix(rep(a_seq, dim(b_mat)[2]),
                  length(a_seq), dim(b_mat)[2])
  
  lm_short <- stats::lm("y ~ d + x -1", data = data[,c("y", "d", "x")])
  df <- lm_short$df.residual
  se <- summary(lm_short)$coefficients[,2][["d"]]
  
  c1 <- qt(1 - alpha / 2, df = df - 1) / sqrt(df - 1)
  c2 <- se * sqrt(df)
  beta <- as.vector(lm_short$coefficients["d"])
  
  if (all(is.na(b_mat))) {
    ret <- c(NA, NA)
  } else {
  lower <- beta - (b_mat * a_mat / sqrt(1 - a_mat^2) +
                     c1 *sqrt((1 - b_mat^2)/(1 - a_mat^2))) * c2
  upper <- beta - (b_mat * a_mat / sqrt(1 - a_mat^2) -
                     c1 *sqrt((1 - b_mat^2)/(1 - a_mat^2))) * c2
  
  ret <- c(min(lower, na.rm = TRUE), max(upper, na.rm = TRUE))
  }
  ret
}



oracle_bootstrap <- function(data) {
  boot_fun <- function(data, indices) {
    full_model <- lm(y ~ x + d + u - 1, data = data[indices, ])
    as.vector(coef(full_model)["d"])
  }
  
  boot_res <- boot::boot(data = data,
                         statistic = boot_fun,
                         R = boot_samples)
  boot_confint <- boot::boot.ci(boot_res,
                                conf = 1 - alpha,
                                type = c("perc", "basic", "bca"))
  c(boot_confint[["percent"]][c(4,5)],
    boot_confint[["basic"]][c(4,5)],
    boot_confint[["bca"]][c(4,5)])
}


generate_data <- function(n) {
  res <- listenv()
  set.seed(2024)
  
  for (i in 1:N) {
    res[[i]] %<-% {
      u <- rnorm(n)
      x <- rnorm(n)
      d <- x + u + rnorm(n)
      y <- d + 2*x + u + rnorm(n) ## beta = 1
      data <- data.frame(y = y, d = d, x = x, u = u)
      ## Sensitivity model
      sa <- sensana(y = y, d = d, dep_x = NULL, indep_x = "x",
                    x = data.frame(x = x), intercept = FALSE)
      
      sa <- add_bound(sa, "UD", "comparative", b = 1, I = NULL, J = "x")
      sa <- add_bound(sa, "UY", "comparative", b = 4/9, I = NULL, J = "x")
  
      seed_before <- .Random.seed
      ## Our sensitivity interval
      sensint_our <- sensana_bootstrap(sa)
      ## Naive sensitivity interval
      sensint_ch <- sensana_ch(sa, data)
      
      ## Oracle confidence intervals
      set.seed(seed_before)
      confint_oracle_boot <- oracle_bootstrap(data)
      full_model <- lm(y ~ x + d + u - 1, data)
      confint_oracle <- confint(full_model, "d", 1-alpha)[c(1, 2)]
      
      list(stat = c(sensint_our$sensints, sensint_ch,
                    confint_oracle_boot, confint_oracle,
                    sensint_our$n_empty, sensint_our$pir),
           boot_sample = sensint_our$t,
           boot_obj = sensint_our$boot_obj)
    } %seed% TRUE
  }
  
  name <- paste0("generated-data/sim-reg-data-", n, ".rds")
  saveRDS(as.list(res), file = name)
}


generate_data(200)
generate_data(500)
generate_data(1000)
generate_data(2000)