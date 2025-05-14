library(optsens)
library(ivmodel)
library(ggplot2)
library(ggrepel)

data(card.data)

y <- card.data[, "lwage"]
d <- card.data[, "educ"]
z <- card.data[, "nearc4"]
x <- card.data[, c("exper", "expersq", "black", "south", "smsa")]

rv <- c(0.01, 0.02, 0.03, 0.04)
grid_specs <- list(N1 = 300, N2 = 300, N5 = 300)
comparison_ind <- list(black = c(1, 2, 5), south = c(1, 2, 5))
dfp <- data.frame(matrix(nrow = 0, ncol = 9))
ivl <- NULL

for (i in seq_along(rv)) {
  sa <- sensana(y = y, d = d, indep_x = c("black", "south"),
                dep_x = c("exper", "expersq", "smsa"), x = x, z = z)
  sa <- add_bound(sa, arrow = "UD", kind = "direct", lb = -0.99, ub = 0.99)
  sa <- add_bound(sa, arrow = "UY", kind = "comparative-d", b = 5, I = "south", J = "black")
  sa <- add_bound(sa, arrow = "ZU", kind = "direct", lb = -rv[i], ub = rv[i])
  sa <- add_bound(sa, arrow = "ZY", kind = "direct", lb = -rv[i], ub = rv[i])
  
  data <- r_contours_data(sa, comparison_ind = NULL, comparison = "comp",
                          iv_lines = TRUE, grid_specs = grid_specs,
                          eps = 0.001)
  
  temp <- cbind(data$df_plot, data.frame(p = rep(i, 300^2)))
  dfp <- rbind(dfp, temp)
  ivl <- data$ivl
}

saveRDS(list(dfp = dfp, ivl = ivl),
        "generated-data/r-contours-iv-lines.rds")
plot_list <- readRDS("generated-data/r-contours-iv-lines.rds")
list2env(plot_list, environment())


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
  scale_fill_steps(##low = "#5A5A5A", high = "#F5F5F5",
                   breaks = make_breaks,
                   aesthetics = c("fill", "colour"))

## list2env(plot_list$ivl, environment())
## c is negative for this dataset
if (ivl$c < 0) {
  pl <- pl +
    stat_function(fun = ivl$fun,
                  xlim = c(-1, ivl$fun_inv(1)),
                  size = 0.7,
                  col = "red") +
    stat_function(fun = ivl$fun,
                  xlim = c(ivl$fun_inv(-1), 1),
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
  theme(strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.8, 'cm'),
        panel.spacing = unit(2, "lines"))

print(pl)
ggsave("generated-graphics/r-contours-iv-lines.pdf",
       width = 10, height = 7.5)
