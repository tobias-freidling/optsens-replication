library(optsens)
library(ivmodel)
library(ggplot2)
library(ggrepel)
library(metR)

data(card.data)

y <- card.data[, "lwage"]
d <- card.data[, "educ"]
z <- card.data[, "nearc4"]
x <- card.data[, c("exper", "expersq", "black", "south", "smsa")]

sa <- sensana(y = y, d = d, indep_x = c("black", "south"),
              dep_x = c("exper", "expersq", "smsa"), x = x, z = z)
sa <- add_bound(sa, arrow = "UD", kind = "comparative", b = 4,
                I = "south", J = "black", name = "bud")
sa <- add_bound(sa, arrow = "UY", kind = "comparative-d", b = 5,
                I = "south", J = "black", name = "buy")
sa <- add_bound(sa, arrow = "ZU", kind = "comparative", b = 0.5, J = "black", name = "bzu")
sa <- add_bound(sa, arrow = "ZY", kind = "comparative", b = 0.1, J = "black", name = "bzy")


data <- b_contours_data(sa, pir_lower = TRUE,
                        bound1 = "bud", range1 = c(0.1, 8),
                        bound2 = "bzy", range2 = c(0.01, 0.12),
                        grid_specs_b = list(N1b = 30, N2b = 30),
                        grid_specs = list(N1 = 200, N2 = 200, N5 = 200),
                        eps = 0.001)

saveRDS(data, file = "generated-data/b-contours-b1-b4.rds")
## data <- readRDS("generated-data/b-contours-b1-b4.rds")


text_point <- paste0("(", 4, ", ", 0.1, ")")
make_breaks <- function(range, binwidth) {
  signif(pretty(range, 15), 4)
}
make_breaks_ex <- function(range, binwidth) {
  b <- make_breaks(range, binwidth)
  b[b != 0]
}

pl <- ggplot(data, aes(x, y, na.rm=TRUE)) +
  geom_contour_fill(aes(z = z, fill = after_stat(level)),
                    breaks = make_breaks,
                    show.legend = FALSE) +
  geom_contour2(aes(z = z, label = after_stat(level)),
                breaks = make_breaks_ex,
                col = "black",
                label_size = 4,
                size = 0.25) +
  scale_fill_discretised(low = "#5A5A5A", high = "#F5F5F5") +
  ## scale_fill_divergent_discretised(midpoint = 0) +
  geom_point(data = data.frame(x = 4, y = 0.1),
             size = 2.5,
             mapping = aes(x, y), col = "black") +
  geom_text_repel(data = data.frame(x = 4,
                                    y = 0.1,
                                    label = text_point),
                  mapping = aes(x, y, label = label),
                  col = "black",
                  size = 4) +
  labs(x = expression("b"["UD"]),
       y = expression("b"["ZY"])) +
  scale_y_continuous(breaks = seq(0, 0.12, by = 0.02)) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        title = element_text(size = 14))

print(pl)
ggsave("generated-graphics/b-contours-b1-b4.pdf", width = 7, height = 6)