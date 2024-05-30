library(limosa.beta)
library(ivmodel)
library(future)
plan(multisession, workers = 5)
library(listenv)
library(ggplot2)

alpha <- 0.05
boot_samples <- 3500

data(card.data)

y <- card.data[, "lwage"]
d <- card.data[, "educ"]
z <- card.data[, "nearc4"]
x <- card.data[, c("exper", "expersq", "black", "south", "smsa")]


estimators <- c("OLS-adj", "TSLS")
bound_combinations <- c("UD/UY", "UZ/ZY", "UZ/ZY/UD",
                        "UZ/ZY/UY", "UZ/ZY/UD/UY")

grid_specs <- list(num_x = 200, num_y = 200, num_z = 200)

df <- data.frame(matrix(ncol = 5, nrow = 0))
estim <- c(estimators, bound_combinations)
point <- c(rep(TRUE, 2), rep(FALSE, 5))


## Point-identified
sa <- sensana(y = y, d = d, indep_x = c("black", "south"),
              dep_x = c("exper", "expersq", "smsa"), x = x, z = z, alpha = alpha)
beta_l <- c(sa$beta_ols, sa$beta_iv)
beta_u <- c(sa$beta_ols, sa$beta_iv)
int_l <- c(sa$confint_ols[1], sa$confint_iv[1])
int_u <- c(sa$confint_ols[2], sa$confint_iv[2])


## Partially identified
res <- listenv()
set.seed(2024)
for (i in seq_along(bound_combinations)) {
res[[i]] %<-% {
    bc <- bound_combinations[i]
    
    sa <- sensana(y = y, d = d, indep_x = c("black", "south"),
                  dep_x = c("exper", "expersq", "smsa"), x = x, z = z)
    
    if (bc %in% c("UZ/ZY", "UZ/ZY/UY")) {
      sa <- add_bound(sa, "UD", "direct", lb = -0.98, ub = 0.98)
    }
    
    if (bc != "UD/UY") {
      sa <- add_bound(sa, "ZU", "comparative", b = 0.5, J = "black")
      sa <- add_bound(sa, "ZY", "comparative", b = 0.1, J = "black")
    }
    
    if (bc %in% c("UD/UY", "UZ/ZY/UY", "UZ/ZY/UD/UY")) {
      sa <- add_bound(sa, "UY", "comparative-d", b = 5, I = "south", J = "black")
    }
    
    if (bc %in% c("UD/UY", "UZ/ZY/UD", "UZ/ZY/UD/UY")) {#
      sa <- add_bound(sa, "UD", "comparative", b = 4, I = "south", J = "black")
    }
    
    pir_est <- pir(sa, grid_specs = grid_specs)
    sensint_obj <- sensint(sa, alpha = alpha, boot_procedure = "bca",
                           boot_samples = boot_samples, grid_specs = grid_specs)
    
    res[[i]] <- c(pir_est[1], pir_est[2],
                  sensint_obj$sensint[2, 1], sensint_obj$sensint[2, 2])
  } %seed% TRUE
}

res <- do.call(rbind, as.list(res))

beta_l <- c(beta_l, res[, 1])
beta_u <- c(beta_u, res[, 2])
int_l <- c(int_l, res[, 3])
int_u <- c(int_u, res[, 4])


df <- data.frame(est = estim, point = point,
                 beta_l = beta_l, beta_u = beta_u,
                 int_l = int_l, int_u = int_u)
saveRDS(df, file = "generated-data/sensint-comparison.rds")
## df <- readRDS(file = "generated-data/sensint-comparison.rds")

names <- c("OLS", "TSLS",
           "(B1), (B2)", "(B3), (B4)",
           "(B1), (B3), (B4)", "(B2), (B3), (B4)",
           "(B1) - (B4)")
df$est <- factor(df$est, levels = estim)

pl <- ggplot(data = df, aes(x = est)) +
  geom_errorbar(aes(ymin = int_l, ymax = int_u), width = 0.35,#0.4
                size = 1) + #1.3
  geom_point(data = subset(df, point), aes(y = beta_l),
             size = 3, col = "#619CFF") + ##4
  geom_errorbar(data = subset(df, !point),
                aes(ymin = beta_l, ymax = beta_u), width = 0.2,
                col = "#619CFF", size = 1) + ## 0.25, 1.3
  scale_x_discrete(labels = names) +
  scale_y_continuous() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 16, angle = 45,
                                   hjust = 1))
print(pl)
ggsave("generated-graphics/sensint-comparison.pdf")