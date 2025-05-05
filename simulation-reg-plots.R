library(ggplot2)

boot_samples <- 2500
sample_sizes <- c(200, 500, 1000, 2000)
df <- data.frame()

for (n in sample_sizes) {
  name <- paste0("generated-data/sim-reg-data-", n, ".rds")
  res_temp <- readRDS(file = name)
  res <- do.call(rbind, lapply(res_temp, function(l) l[[1]]))
  boot_mat <- do.call(rbind, lapply(res_temp, function(l) l$boot_sample))
  N <- dim(res)[1]
  temp_df <- data.frame(x = c(res[,24], res[,25],
                              boot_mat[,1], boot_mat[,2]),
                        label = c(rep("PIR lower", N),
                                  rep("PIR upper", N),
                                  rep("PIR lower - Boot",
                                      N*boot_samples),
                                  rep("PIR upper - Boot",
                                      N*boot_samples)))
  temp_df$n <- n
  df <- rbind(df, temp_df)
}


##############
## Density plot: bootstrap vs. real distribution
##############
g1 <- ggplot(df, aes(x = x, linetype = label)) + ## color = label
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "gray") +
  geom_vline(xintercept = (3 + sqrt(3)) / 2,
             linetype = "dashed",
             color = "gray") +
  geom_line(aes(color = label), stat = "density", size = 0.7) +
  scale_linetype_manual(values = c("PIR lower" = "solid",
                                   "PIR lower - Boot" = "solid",
                                   "PIR upper" = "31",
                                   "PIR upper - Boot" = "31")) +
  scale_color_manual(values = c("PIR lower" = "black",
                                "PIR lower - Boot" = "darkgray",
                                "PIR upper" = "black",
                                "PIR upper - Boot" = "darkgray")) +
  xlim(0, 3.5) +
  facet_wrap(~n,
             labeller = function(s) label_both(s, sep = " = ")) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 11),
        axis.text = element_text(size = 14),
        axis.title = element_blank(),
        title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.key.size = unit(1.0, 'cm'))

print(g1)
ggsave("generated-graphics/simulation-reg-dist.pdf")




############
## QQ plots
############

## subsample data frame that ggplot can handle it
set.seed(2024)
df_red <- df
df_red$rand_num <- 0
df_red["rand_num"][df_red["label"] == "PIR lower - Boot"] <- runif(500 * 2500)
df_red["rand_num"][df_red["label"] == "PIR upper - Boot"] <- runif(500 * 2500)


print_qq_plot <- function(label_name, threshold = 0.01) {
  g2 <- ggplot(subset(df_red, label == label_name & rand_num <= threshold),
               aes(sample = x)) +
    stat_qq(size = 0.5, alpha = 0.3) +
    stat_qq_line(colour = "red") +
    facet_wrap(~n, labeller = function(s) label_both(s, sep = " = "),
               scales = "free") +
    theme_bw() +
    theme(strip.text.x = element_text(size = 11),
          axis.text = element_text(size = 14),
          axis.title = element_blank(),
          title = element_text(size = 14),
          legend.title = element_blank(),
          legend.text = element_text(size = 11),
          legend.key.size = unit(1.0, 'cm'))
  g2
}

print_qq_plot("PIR lower")
print_qq_plot("PIR lower - Boot")
print_qq_plot("PIR upper")
print_qq_plot("PIR upper - Boot")