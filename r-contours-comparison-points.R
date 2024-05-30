library(limosa.beta)
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
sa <- add_bound(sa, "UD", "direct", lb = -0.75, ub = 0.75)
sa <- add_bound(sa, "UY", "direct", lb = -0.75, ub = 0.75)


comparison_ind <- list(black = c(1, 2, 5), south = c(1, 2, 5))
grid_specs <- list(num_x = 400, num_y = 400, num_z = 100)

data_inf <- r_contours_data(sa, comparison_ind, mult_r2 = TRUE,
                            comparison = "informal", iv_lines = FALSE,
                            grid_specs = grid_specs,
                            print_warning = FALSE, eps = 0.001)
data_comp <- r_contours_data(sa, comparison_ind, mult_r2 = TRUE,
                             comparison = "comp", iv_lines = FALSE,
                             grid_specs = grid_specs,
                             print_warning = FALSE, eps = 0.001)
data_comp_d <- r_contours_data(sa, comparison_ind, mult_r2 = TRUE,
                               comparison = "comp-d", iv_lines = FALSE,
                               grid_specs = grid_specs,
                               print_warning = FALSE, eps = 0.001)

saveRDS(list(data_inf = data_inf,
             data_comp = data_comp,
             data_comp_d = data_comp_d),
        "generated-data/r-contours-comparison-points.rds")
## data_list <- readRDS("generated-data/r-contours-comparison-points.rds")
## list2env(data_list, environment())


make_breaks <- function(range, binwidth) {
  signif(pretty(range, 15), 4)
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
  scale_fill_steps2(midpoint = 0,
                    low = scales::muted("blue"),
                    high = scales::muted("red"),
                    breaks = make_breaks,
                    aesthetics = c("fill", "colour"))

pl <- pl +
  geom_point(data = data_comp$df_cp,
             mapping = aes(x = cp_x, y = cp_y),
             col = "#000000", shape = 16,
             show.legend = FALSE, size = 2.5,
             alpha = 0.7) +
  geom_point(data = data_comp_d$df_cp,
             mapping = aes(x = cp_x, y = cp_y),
             col = "#0b5394", shape = 8,
             show.legend = FALSE, size = 2.5,
             alpha = 0.7) +
  geom_point(data = data_inf$df_cp,
             mapping = aes(x = cp_x, y = cp_y),
             col = "#009E73", shape = 17,
             show.legend = FALSE, size = 2.5,
             alpha = 0.7) +
  geom_text_repel(data = data_inf$df_cp,
                  mapping = aes(x = cp_x, y = cp_y,
                                label = cp_label),
                  col = "black", size = 4)

xlab <- expression("R"["D~U|X,Z"])
ylab <- expression("R"["Y~U|X,Z,D"])

pl <- pl +
  labs(x = xlab, y = ylab) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        title = element_text(size = 14),
        legend.text = element_text(size = 13),
        legend.key.size = unit(1.8, 'cm'),
        legend.title = element_blank())

print(pl)
ggsave("generated-graphics/r-contours-comparison-points.pdf",
       width = 9.5, height = 6)
