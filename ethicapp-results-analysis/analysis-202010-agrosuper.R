source("analysis_rand_hetero.R")

data_hetero_202010 <- read_excel("./data-ethics-202010/202010-ethics-1-hetero-agrosuper.xlsx", "Selection")
data_hetero_202010 <- as.data.frame(data_hetero_202010)

data_random_202010 <- read_excel("./data-ethics-202010/202010-ethics-2-random-agrosuper.xlsx", "Selection")
data_random_202010 <- as.data.frame(data_random_202010)

global_file_prefix <- "./plots/202010-agrosuper-rand-hetero"

plot_builder <- build_ind_cmp_density

length(unique(data_random_202010$user_id)) # 32
length(unique(data_hetero_202010$user_id)) # 34

# Evolución del deltas de puntaje entre fases en DSs a nivel
# individual entre fases, 1-2, 2-3, 1-3.
for (i in 1:3) {
  pt1 <- plot_builder(data_random_202010, data_hetero_202010, i, 1, 2)
  pt2 <- plot_builder(data_random_202010, data_hetero_202010, i, 2, 3)
  pt3 <- plot_builder(data_random_202010, data_hetero_202010, i, 1, 3)
  plot_grid(pt1, pt2, pt3)
  ggsave(paste0(global_file_prefix, paste0("-ind-score-deltas-ds", paste0(i, ".png"))), 
         width = 22, height = 16, units = "cm", device = "png", dpi = 300)  
}

# Progreso de los puntajes individuales de las escalas DS en las tres fases
for (i in 1:3) {
  pt1 <- plot_builder(data_random_202010, data_hetero_202010, i, 1, 2, delta = FALSE, phase_arg = 1)
  pt2 <- plot_builder(data_random_202010, data_hetero_202010, i, 2, 3, delta = FALSE, phase_arg = 1)
  pt3 <- plot_builder(data_random_202010, data_hetero_202010, i, 1, 3, delta = FALSE, phase_arg = 2)
  plot_grid(pt1, pt2, pt3)
  ggsave(paste0(global_file_prefix, paste0("-ind-score-ds", paste0(i, ".png"))), 
         width = 22, height = 16, units = "cm", device = "png", dpi = 300)  
}

plot_builder <- build_group_cmp_density

# Diferencias del coeficiente de variación en grupos por DS y entre fases.
for(i in 1:3) {
  pt1 <- plot_builder(data_random_202010, data_hetero_202010, i, 1, 2)
  pt2 <- plot_builder(data_random_202010, data_hetero_202010, i, 2, 3)
  pt3 <- plot_builder(data_random_202010, data_hetero_202010, i, 1, 3)
  
  plot_grid(pt1, pt2, pt3)
  ggsave(paste0(global_file_prefix, paste0("-group-cvardelta-ds", paste0(i, ".png"))), 
         width = 20, height = 16, units = "cm", device = "png", dpi = 300)
}

# Diferencias del coeficiente de variación en grupos por DS y entre fases
# 1-2, 2-3, 1-3.
for (i in 1:3) {
  pt1 <- plot_builder(data_random_202010, data_hetero_202010, i, 1, 2, delta = FALSE, phase_arg = 1)
  pt2 <- plot_builder(data_random_202010, data_hetero_202010, i, 1, 2, delta = FALSE, phase_arg = 2)
  pt3 <- plot_builder(data_random_202010, data_hetero_202010, i, 1, 3, delta = FALSE, phase_arg = 2)
  
  plot_grid(pt1, pt2, pt3)
  ggsave(paste0(global_file_prefix, paste0("-group-cvar-ds", paste0(i, ".png"))), 
         width = 20, height = 16, units = "cm", device = "png", dpi = 300)
}
