library(optsens)
library(ivmodel)
library(ggplot2)
library(ggrepel)

data(card.data)

y <- card.data[, "lwage"]
d <- card.data[, "educ"]
z <- card.data[, "nearc4"]
x <- card.data[, c("exper", "expersq", "black", "south", "smsa")]

sa <- sensana(y = y, d = d, indep_x = c("black", "south"),
              dep_x = c("exper", "expersq", "smsa"), x = x, z = z)
sa <- add_bound(sa, arrow = "UD", kind = "direct", lb = -0.75, ub = 0.75)
sa <- add_bound(sa, arrow = "UY", kind = "direct", lb = -0.75, ub = 0.75)


comparison_ind <- list(black = c(1, 2, 5), south = c(1, 2, 5))
grid_specs <- list(N1 = 400, N2 = 400, N5 = 400)


data_inf <- r_contours_data(sa, comparison_ind, comparison = "informal",
                            iv_lines = FALSE, grid_specs = grid_specs,
                            eps = 0.001)
data_comp <- r_contours_data(sa, comparison_ind, comparison = "comp",
                             iv_lines = FALSE, grid_specs = grid_specs,
                             eps = 0.001)
data_comp_d <- r_contours_data(sa, comparison_ind, comparison = "comp-d",
                               iv_lines = FALSE, grid_specs = grid_specs,
                               eps = 0.001)

saveRDS(list(data_inf = data_inf,
             data_comp = data_comp,
             data_comp_d = data_comp_d),
        "generated-data/r-contours-comparison-points.rds")
data_list <- readRDS("generated-data/r-contours-comparison-points.rds")
list2env(data_list, environment())


make_breaks <- function(range, binwidth) {
  signif(pretty(range, 10), 4)
}
type <- rep("inf", 6)
df_cp_inf <- cbind(data_inf$df_cp, type)
type <- rep("comp", 6)
df_cp_comp <- cbind(data_comp$df_cp, type)
type <- rep("comp-d", 6)
df_cp_comp_d <- cbind(data_comp_d$df_cp, type)
df_cp <- rbind(df_cp_inf, df_cp_comp, df_cp_comp_d)

pl <- ggplot(subset(data_comp$df_plot, feasible)) +
  geom_rect(aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = val, colour = val),
            alpha = 1, na.rm = TRUE) +
  scale_fill_steps(low = "#0072B2", high = "#F0E442",##low = "#5A5A5A", high = "#F5F5F5",
                   breaks = make_breaks,
                   aesthetics = c("fill", "colour"))

pl <- pl +
  geom_point(data = data_comp$df_cp,
             mapping = aes(x = cp_x, y = cp_y),
             fill = "#E69F00", col = "black",
             stroke = 0.5, shape = 21,
             show.legend = FALSE, size = 3,
             alpha = 0.8) +
  geom_point(data = data_comp_d$df_cp,
             mapping = aes(x = cp_x, y = cp_y),
             col = "black", shape = 8,
             show.legend = FALSE, size = 3,
             alpha = 0.5) +
  geom_point(data = data_inf$df_cp,
             mapping = aes(x = cp_x, y = cp_y),
             fill = "#009E73", col = "black",
             stroke = 0.5, shape = 24,
             show.legend = FALSE, size = 3,
             alpha = 0.6) +
  geom_text_repel(data = data_inf$df_cp,
                  mapping = aes(x = cp_x, y = cp_y,
                                label = cp_label),
                  col = "black", size = 5)

xlab <- expression("R"["D~U|X,Z"])
ylab <- expression("R"["Y~U|X,Z,D"])

pl <- pl +
  labs(x = xlab, y = ylab) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.8, 'cm'),
        legend.title = element_blank())

print(pl)
ggsave("generated-graphics/r-contours-comparison-points.pdf",
       width = 9, height = 6)
