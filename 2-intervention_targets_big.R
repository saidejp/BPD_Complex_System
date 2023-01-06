# Simulation 3
# by: Said Jiménez 

# Here I perform the intervention to specific nodes by using the nodeIdentifyR method proposed by Lunansky 2022.

pacman::p_load(tidyverse, IsingSampler, IsingFit, nodeIdentifyR, qgraph, bootnet)

load("dataverse_files/vonKlipstein2021.RData")

data <- 
  shareD %>% 
  filter(time == 0) %>% 
  tibble()

items <- 
  data %>% 
  select(BPDSI1.1:BPDSI9.8) 

items_bin <- 
  items %>% 
  mutate(
    across(
      .cols = everything(),
      .fns = ~ ifelse(is.na(.x), median(.x, na.rm = T), .x)
    )
  ) %>% 
  mutate(
    across(
      .cols = everything(),
      .fns = ~ ifelse(.x >= mean(.x), 1, 0)
    )
  ) 



fit_net <- estimateNetwork(items_bin, default = "IsingFit")  

node_names <- c(
  "1: abandonment",
  "2: interpersonal relationships",
  "3: identity",
  "4: impulsivity",
  "5: parasuicidal behavior",
  "6: affective instability",
  "7: emptiness",
  "8: outbursts of anger",
  "9: dissociation/paranoid ideation"
)

group_labels <- 
  tibble(items = colnames(items_bin)) %>% 
  separate(col = items, into = c("symptom", "sub"), remove = F) %>% 
  mutate(symptom = factor(symptom, 
                          levels = paste0("BPDSI", 1:9),
                          labels = node_names)) %>% 
  pull(symptom)

items_names <- str_remove(colnames(items), pattern = "BPDSI")

color_nodes <- RColorBrewer::brewer.pal(n = 9, name = "Set3")

qgraph(
  fit_net$graph, 
  layout = "spring", 
  groups = group_labels, 
  color = color_nodes,
  labels = items_names,
  legend.cex = 0.6,
  label.font = 2,
  filetype = "png", 
  filename = "bpd_ising_net"
  )

centralityPlot(fit_net, 
               scale = "z-scores")



sim_resp <- 
  simulateResponses(fit_net$results$weiadj, 
                    thresholds = fit_net$results$thresholds, 
                    perturbation_type = "alleviating",
                    amount_of_SDs_perturbation = 1)

# Calcula la suma de los puntajes de los 5000 sujetos simulados dado que se disminuye 
# el threshold de cada síntoma (ítem) por un factor de 1 desviación estándar 

names(sim_resp) <- str_remove(names(sim_resp), pattern = "BPDSI")

plot1 <- 
  calculateSumScores(sim_resp) %>% 
  prepareDFforPlottingAndANOVA() %>% 
  plotSumScores(perturbation_type = "alleviating")

plot_a <-  
  plot1$sumScoresPlot + 
  labs(tag = "A", title = "Alleviating") +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(size = 9, angle = 90, hjust = 1), 
        plot.title = element_text(hjust = .5, face = "bold"))


color <- RColorBrewer::brewer.pal(3, name = "Set1")[c(1, 2)]

plot_a$layers[[1]] <- geom_line(color = color[2])
plot_a$layers[[2]] <- geom_point(color = color[2])
plot_a$layers[[3]] <- geom_errorbar(aes(ymin = ciLower, ymax = ciUpper), 
                                    width = 0.15, color = color[2])

plot_a

sim_resp_agg <- 
  simulateResponses(fit_net$results$weiadj, 
                    thresholds = fit_net$results$thresholds, 
                    perturbation_type = "aggravating",
                    amount_of_SDs_perturbation = 1)

names(sim_resp_agg) <- str_remove(names(sim_resp_agg), pattern = "BPDSI")


plot2 <- 
  calculateSumScores(sim_resp_agg) %>% 
  prepareDFforPlottingAndANOVA() %>% 
  plotSumScores(perturbation_type = "aggravating")

plot2$sumScoresPlot$layers[[1]] <- geom_line(color = color[1], lty = 2)
plot2$sumScoresPlot$layers[[2]] <- geom_point(color = color[1])
plot2$sumScoresPlot$layers[[3]] <- geom_errorbar(aes(ymin = ciLower, ymax = ciUpper), 
                                                 width = 0.15, color = color[1])

plot_b <- 
  plot2$sumScoresPlot +
  labs(tag = "B", title = "Aggravating") +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(size = 9, angle = 90, hjust = 1),
        plot.title = element_text(hjust = .5, face = "bold"))

plot_b

alleviating <- 
  calculateSumScores(sim_resp) %>% 
  map_df(mean) %>% 
  pivot_longer(
    cols = 2:last_col(),
    names_to = "Symptom", 
    values_to = "Sum"
  ) %>% 
  mutate(
    NIRA = abs(original -  Sum), 
    thresholds = fit_net$results$thresholds
  )



aggravating <- 
  calculateSumScores(sim_resp_agg) %>% 
  map_df(mean) %>% 
  pivot_longer(
    cols = 2:last_col(),
    names_to = "Symptom", 
    values_to = "Sum"
  ) %>% 
  mutate(
    NIRA = abs(original -  Sum), 
    thresholds = fit_net$results$thresholds
  )


df_interventions <-
  alleviating %>% 
  bind_rows(aggravating, .id = "Intervention") %>% 
  mutate(Intervention = factor(Intervention, 
                               levels = 1:2, 
                               labels = c("Alleviating", "Aggravating"))) 

df_interventions %>% 
  group_by(Intervention) %>% 
  summarize(cor = cor(NIRA, thresholds))

theme_set(theme_bw(base_size = 14))


fig_nira_thr <- 
  ggplot(df_interventions, aes(x = thresholds, y = NIRA, col = Intervention)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_brewer(type = "qual", palette = 6,  direction = -1) +
  labs(x = "Thresholds") +
  facet_wrap(~Intervention) +
  theme(legend.position = "none")

fig_nira_thr

strength <- 
  centralityTable(fit_net$results$weiadj) %>% 
  filter(measure == "Strength") %>% 
  pull(value)

df_interventions <- 
  df_interventions %>% 
  mutate(strength = rep(strength, 2))




# The most effective targets according to NIRA (los que tienen mayor impacto en la puntuación de la red al intervenirse) are related to but may differ from the most central nodes. 

df_interventions %>% 
  group_by(Intervention) %>% 
  summarize(cor = cor(strength, NIRA))

fig_str_nira <- 
  df_interventions %>% 
  ggplot(aes(x = NIRA, y = strength, col = Intervention)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(y = "Strength") +
  scale_color_brewer(type = "qual", palette = 6, direction = -1) +
  facet_wrap(~Intervention, scales = "free_x") +
  theme(legend.position = "none")

fig_str_nira

fig_nira <- cowplot::plot_grid(fig_nira_thr, fig_str_nira, ncol = 1)
fig_nira

ggsave(plot = fig_nira, filename = "fig_nira.png", device = "png", 
       dpi = 350, units = "cm", height = 15, width = 20)





plot_c <- 
  plot1$plottedInformation %>% 
  bind_rows(plot2$plottedInformation, .id = "Intervention") %>% 
  tibble() %>% 
  mutate(
    Intervention = factor(Intervention,
                          levels = c(1, 2),
                          labels =  c("Alleviating", "Aggravating")), 
    thresholdIteration = factor(thresholdIteration, 
                                levels = plot1$plottedInformation$thresholdIteration) 
  ) %>% 
  ggplot(aes(x = thresholdIteration, y = meanSumscore, color = Intervention)) +
  geom_line(aes(group = Intervention, lty = Intervention), show.legend = F) +
  geom_errorbar(aes(ymin = ciLower, ymax = ciUpper), width = 0.1) +
  geom_point() +
  scale_color_manual(values = rev(color)) +
  labs(
    x = "Symptom of which the threshold is altered", 
    y = "Sum score", 
    col = "Intervention:", 
    tag = "C"
  ) +
  theme_bw(base_family = "Times") +
  theme(axis.text.x = element_text(size = 6, angle = 90))



plot_c

figura <- 
  cowplot::plot_grid(
    plot_a, plot_b, ncol = 2
  ) %>% 
  cowplot::plot_grid(
    plot_c, ncol = 1
  )

figura

fig_interventions <- 
  cowplot::plot_grid(
  plot_a, 
  plot_b, 
  ncol = 1
)

fig_interventions

ggsave("fig_interventions.png", plot = fig_interventions, device = "png", dpi = 350, 
       units = "cm", height = 22, width = 25)



# Comparison between node parameters ------------------------------------------------


t_str <- 
  df_interventions %>% 
  filter(Intervention == "Alleviating") %>% 
  select(Symptom, Strength = strength) %>% 
  arrange(desc(Strength))

t_thr <- 
  df_interventions %>% 
  filter(Intervention == "Alleviating") %>% 
  select(Symptom, Thresholds = thresholds) %>% 
  arrange(desc(Thresholds))

t_all <- 
  df_interventions %>% 
  filter(Intervention == "Alleviating") %>% 
  select(Symptom, NIRA) %>% 
  arrange(desc(NIRA))

t_agg <- 
  df_interventions %>% 
  filter(Intervention == "Aggravating") %>% 
  select(Symptom, NIRA) %>% 
  arrange(desc(NIRA))


bind_cols(t_str, t_thr, t_all, t_agg) %>% 
  mutate(across(.cols = where(is.numeric), .fns = round, 2))



# Prevalence symptoms -----------------------------------------------------


prop_all <- 
  sim_resp %>% 
  map(colMeans) %>% 
  bind_rows() %>% 
  select(2:last_col()) %>% 
  as.matrix() %>% 
  diag()


prop_agg <- 
  sim_resp_agg %>% 
  map(colMeans) %>% 
  bind_rows() %>% 
  select(2:last_col()) %>% 
  as.matrix() %>% 
  diag()

tibble(
  #Symptoms = symptoms, 
  Node = colnames(items_bin), 
  `Raw data` = colMeans(items_bin),
  `Baseline model` = colMeans(sim_resp_agg[[1]]),
  `Alleviating intervention` = prop_all,
  `Aggravating intervention` = prop_agg
) %>% 
  mutate(across(.cols = where(is.numeric), .fns = round, 2))


# Stability------------

# Checking edge-weight accuracy 
boot1_pre <- bootnet(fit_net, nBoots = 4000, nCores = 4)
plot(boot1_pre, labels = T, order = "sample")

# Edges greater to zero
summary(boot1_pre) %>% 
  filter(CIlower > 0) 


# Stability of central indices using case-dropping bootstrap
boot2_pre <- bootnet(fit_net, 
                     nBoots = 4000,
                     nCores = 4, 
                     type = "case", 
                     statistics = c("edge", 
                                    "strength", 
                                    "closeness", 
                                    "betweenness"))

plot(boot2_pre) 
corStability(boot2_pre)
