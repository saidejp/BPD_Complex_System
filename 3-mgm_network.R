# Mixed Graphical Model: Empirical analysis --------
# by: Said Jiménez 
# load("mgm_network.RData")

pacman::p_load(tidyverse, qgraph, bootnet, mgm)


data <- read_csv("complete_data.csv")


dat_mat <-
  data %>% 
  select(-EXP) %>% 
  mutate(
    across(
      .cols = everything(), 
      .fns = function(col) {
        ifelse(is.na(col), median(col, na.rm = T), col)
      }
    )
  ) %>% 
  as.matrix()


p <- ncol(dat_mat)


set.seed(2022)

fit_mgm <- estimateNetwork(
  data =  dat_mat, 
  default = "mgm", 
  type = c("c" , rep("g", p - 1)),
  level = c(2, rep(1, p - 1)),
  criterion = "EBIC"
)


plot(fit_mgm)

predictability <- predict(fit_mgm$results, data = dat_mat)
errors <- predictability$errors$R2
errors[1] <- predictability$errors$CC[1]


# Figure 6 in paper 
plot(fit_mgm, 
     layout = "spring",
     pie = errors, 
     label.font = 2,
     pieColor = c("#CBD5E8", rep("#7570B3", 12)),
     shape = c("square", rep("circle", 12)),
     color = c("#7570B3", rep("#CBD5E8", 12)),
     label.color = c("white", rep("black", 12)),
     legend.cex = 0.5,
     nodeNames = c("skills training", 
                   "fear of abandonmnent", 
                   "shifting opinion of others",
                   "changes in self-image",
                   "mood swings",
                   "feeling paranoid",
                   "feeling angry",
                   "feeling emptiness", 
                   "feeling suicidal",
                   "keep someone from leaving",
                   "self-harm/suicide attempt", 
                   "impulsive behavior",
                   "temper outburst"),
     filetype = "png", 
     filename = "mgm_net"
     )


# Metrics and network stability

# Centrality 
measures <- c("Strength", "Closeness", "Betweenness")
centralityPlot(fit_mgm, 
               include = measures, 
               labels = colnames(dat_mat))


color <- RColorBrewer::brewer.pal(3, name = "Set1")[2]
cent_plot <- centralityPlot(fit_mgm,  
                            include = measures, 
                            labels = colnames(dat_mat), scale = "z-scores")

cent_plot$layers[[1]] <- geom_path(col = color)
cent_plot$layers[[2]] <- geom_point(col = color)

fig_cent <- 
  cent_plot +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"), 
        panel.grid.major = element_blank())

fig_cent

#ggsave(fig_cent, filename = "centrality_fig.png", device = "png", dpi = 400, 
#       units = "cm", height = 10, width = 15)


# Bootstrapping
boot_mgm <- bootnet(fit_mgm, nBoots = 2500, nCores = 4)
plot(boot_mgm, labels = T, order = "sample")

summary(boot_mgm) %>% 
  filter(node1 == "Treat", CIupper < 0)

summary(boot_mgm) %>% 
  #filter(node1 == "Treat") %>% 
  select(type, node1, node2, mean, sd, CIlower, CIupper) %>% 
  mutate(across(.cols = where(is.numeric), .fns = round, 2)) %>% 
  filter(CIlower > 0) %>% View()

summary(boot_mgm) %>% 
  #filter(node1 == "Treat") %>% 
  select(type, node1, node2, mean, sd, CIlower, CIupper) %>% 
  mutate(across(.cols = where(is.numeric), .fns = round, 2)) %>% 
  writexl::write_xlsx("table_edge_boot.xlsx")

boot_mgm_case <- bootnet(fit_mgm, 
                         nBoots = 2500,
                         nCores = 4, 
                         type = "case",
                         statistics = c("edge", "strength", 
                                        "closeness", "betweenness"))

plot(boot_mgm_case, statistics = c("strength", 
                                   "closeness", "betweenness"))
corStability(boot_mgm_case)


differenceTest(boot_mgm, 8, 4, "strength")
differenceTest(boot_mgm, 5, 4, "strength")

differenceTest(boot_mgm, 5, 7, "strength")
differenceTest(boot_mgm, 5, 13, "strength")
differenceTest(boot_mgm, 4, 13, "strength")

plot(boot_mgm, "edge", plot = "difference", 
     onlyNonZero = TRUE, order = "sample")

