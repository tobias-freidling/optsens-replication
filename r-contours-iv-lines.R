library(limosa.beta)
library(ivmodel)
library(ggplot2)
library(ggrepel)

data(card.data)

y <- card.data[, "lwage"]
d <- card.data[, "educ"]
z <- card.data[, "nearc4"]
x <- card.data[, c("exper", "expersq", "black", "south", "smsa")]

rv <- c(0.01, 0.02, 0.03, 0.04)
grid_specs <- list(num_x = 300, num_y = 300, num_z = 300)
dfp <- data.frame(matrix(nrow = 0, ncol = 9))
iv_lines_list <- NULL

for (i in seq_along(rv)) {
  sa <- sensana(y = y, d = d, indep_x = c("black", "south"),
                dep_x = c("exper", "expersq", "smsa"), x = x, z = z)
  sa <- add_bound(sa, "UD", "direct", lb = -0.99, ub = 0.99)
  sa <- add_bound(sa, "UY", "comparative-d", b = 5, I = "south", J = "black")
  sa <- add_bound(sa, "ZU", "direct", lb = -rv[i], ub = rv[i])
  sa <- add_bound(sa, "ZY", "direct", lb = -rv[i], ub = rv[i])
  
  data <- r_contours_data(sa, comparison_ind = NULL,
                          mult_r2 = TRUE, comparison = "comp",
                          iv_lines = TRUE, grid_specs = grid_specs,
                          print_warning = FALSE, eps = 0.001)
  temp <- cbind(data$df_plot, data.frame(p = rep(i, 300^2)))
  dfp <- rbind(dfp, temp)
  iv_lines_list <- data$iv_lines_list
}

saveRDS(list(dfp = dfp, iv_lines_list = iv_lines_list),
        "generated-data/r-contours-iv-lines.rds")
## plot_list <- readRDS("generated-data/r-contours-iv-lines.rds")
## list2env(plot_list, environment())


xlab <- expression("R"["D~U|X,Z"])
ylab <- expression("R"["Y~U|X,Z,D"])

make_breaks <- function(range, binwidth) {
  signif(pretty(range, 8), 4)
}

pl <- ggplot(subset(dfp, feasible)) +
  geom_rect(aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = val, colour = val),
            alpha = 1, na.rm = TRUE) +
  scale_fill_steps(breaks = make_breaks,
                   aesthetics = c("fill", "colour"),
                   name = "lower point of 95%\nconfidence interval")

## list2env(plot_list$iv_lines_list, environment())
## c is negative for this dataset
if (iv_lines_list$c < 0) {
  pl <- pl +
    stat_function(fun = iv_lines_list$fun,
                  xlim = c(-1, iv_lines_list$fun_inv(1)),
                  size = 0.7,
                  col = "red") +
    stat_function(fun = iv_lines_list$fun,
                  xlim = c(iv_lines_list$fun_inv(-1), 1),
                  size = 0.7,
                  col = "red")
}


custom <- function(string) {
  ret <- c()
  for (s in string) {
    if (s == "1") {
      ret <- c(ret, expression("R"["Z~U|X"]~", R"["Y~Z|X,U,D"] %in%" [-0.01 , 0.01]"))
    } else if (s == "2") {
      ret <- c(ret, expression("R"["Z~U|X"]~", R"["Y~Z|X,U,D"] %in%" [-0.02 , 0.02]"))
    } else if (s == "3") {
      ret <- c(ret, expression("R"["Z~U|X"]~", R"["Y~Z|X,U,D"] %in%" [-0.03 , 0.03]"))
    } else {
      ret <- c(ret, expression("R"["Z~U|X"]~", R"["Y~Z|X,U,D"] %in%" [-0.04 , 0.04]"))
    }
  }
  ret
}

pl <- pl +
  labs(x = xlab, y = ylab, z = "lower point of confidence interval") +
  facet_wrap(~p, labeller = as_labeller(custom,
                                        default = label_parsed)) +
  xlim(-1, 1) +
  ylim(-0.6, 1) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 11),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1.8, 'cm'),
        panel.spacing = unit(2, "lines"))

print(pl)
ggsave("generated-graphics/r-contours-iv-lines.pdf",
       width = 8, height = 6)