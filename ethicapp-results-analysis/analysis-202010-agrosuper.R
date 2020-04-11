source("analysis_rand_hetero.R")

data_random_201920 <- read_excel("./data-ethics-201920/201920-ethics-1-random-agrosuper.xlsx", "Selection")
data_random_201920 <- as.data.frame(data_random_201920)

data_hetero_202010 <- read_excel("./data-ethics-202010/202010-ethics-1-hetero-agrosuper.xlsx", "Selection")
data_hetero_202010 <- as.data.frame(data_hetero_202010)

data_random_202010 <- read_excel("./data-ethics-202010/202010-ethics-2-random-agrosuper.xlsx", "Selection")
data_random_202010 <- as.data.frame(data_random_202010)

build_individual_comparative_analytics_charts <- function(df_series1, df_series2, 
                                                          series_labels, plot_builder,
                                                          output_prefix,
                                                          chart_width,
                                                          chart_height) {
  # Evolución del deltas de puntaje entre fases en DSs a nivel
  # individual entre fases, 1-2, 2-3, 1-3.
  for (i in 1:3) {
    pt1 <- plot_builder(df_series1, df_series2, series_labels, i, 1, 2)
    pt2 <- plot_builder(df_series1, df_series2, series_labels, i, 2, 3)
    pt3 <- plot_builder(df_series1, df_series2, series_labels, i, 1, 3)
    plot_grid(pt1, pt2, pt3)
    ggsave(paste0(output_prefix, paste0("-ind-score-deltas-ds", paste0(i, ".png"))), 
           width = chart_width, height = chart_height, units = "cm", device = "png", dpi = 300)  
  }
  
  # Progreso de los puntajes individuales de las escalas DS en las tres fases
  for (i in 1:3) {
    pt1 <- plot_builder(df_series1, df_series2, series_labels, i, 1, 2, delta = FALSE, phase_arg = 1)
    pt2 <- plot_builder(df_series1, df_series2, series_labels, i, 2, 3, delta = FALSE, phase_arg = 1)
    pt3 <- plot_builder(df_series1, df_series2, series_labels, i, 1, 3, delta = FALSE, phase_arg = 2)
    plot_grid(pt1, pt2, pt3)
    ggsave(paste0(output_prefix, paste0("-ind-score-ds", paste0(i, ".png"))), 
           width = chart_width, height = chart_height, units = "cm", device = "png", dpi = 300)  
  }
}

build_group_comparative_analytics_charts <- function(df_series1, df_series2, 
                                                     series_labels, plot_builder,
                                                     output_prefix,
                                                     chart_width,
                                                     chart_height) {
  # Diferencias del coeficiente de variación en grupos por DS y entre fases.
  for(i in 1:3) {
    pt1 <- plot_builder(df_series1, df_series2, series_labels, i, 1, 2)
    pt2 <- plot_builder(df_series1, df_series2, series_labels, i, 2, 3)
    pt3 <- plot_builder(df_series1, df_series2, series_labels, i, 1, 3)
    
    plot_grid(pt1, pt2, pt3)
    ggsave(paste0(output_prefix, paste0("-group-cvardelta-ds", paste0(i, ".png"))), 
           width = chart_width, height = chart_height, units = "cm", device = "png", dpi = 300)
  }
  
  # Diferencias del coeficiente de variación en grupos por DS y entre fases
  # 1-2, 2-3, 1-3.
  for (i in 1:3) {
    pt1 <- plot_builder(df_series1, df_series2, series_labels, i, 1, 2, delta = FALSE, phase_arg = 1)
    pt2 <- plot_builder(df_series1, df_series2, series_labels, i, 1, 2, delta = FALSE, phase_arg = 2)
    pt3 <- plot_builder(df_series1, df_series2, series_labels, i, 1, 3, delta = FALSE, phase_arg = 2)
    
    plot_grid(pt1, pt2, pt3)
    ggsave(paste0(output_prefix, paste0("-group-cvar-ds", paste0(i, ".png"))), 
           width = chart_width, height = chart_height, units = "cm", device = "png", dpi = 300)
  }
}

###############################################################################
## Analisis 202010 - Grupos Random y Heterogeneos
###############################################################################

output_prefix <- "./plots/202010-agrosuper-rand-hetero"
series_labels <- c('Random', 'Heterogeneous')

plot_builder <- build_ind_cmp_density
build_individual_comparative_analytics_charts(data_random_202010, 
                                              data_hetero_202010, 
                                              series_labels,
                                              plot_builder,
                                              output_prefix,
                                              chart_width = 22,
                                              chart_height = 16)

plot_builder <- build_group_cmp_density
build_group_comparative_analytics_charts(data_random_202010, 
                                         data_hetero_202010, 
                                         series_labels,
                                         plot_builder,
                                         output_prefix,
                                         chart_width = 22,
                                         chart_height = 16)

###############################################################################
## Analisis 201920 /202010 - Grupos Random
###############################################################################

output_prefix <- "./plots/201920-202010-agrosuper-rand-rand"
series_labels <- c('201920 Random', '202010 Random')

plot_builder <- build_ind_cmp_density
build_individual_comparative_analytics_charts(data_random_201920, 
                                              data_random_202010, 
                                              series_labels,
                                              plot_builder,
                                              output_prefix,
                                              chart_width = 24,
                                              chart_height = 16)

plot_builder <- build_group_cmp_density
build_group_comparative_analytics_charts(data_random_201920, 
                                         data_random_202010, 
                                         series_labels,
                                         plot_builder,
                                         output_prefix,
                                         chart_width = 24,
                                         chart_height = 16)
