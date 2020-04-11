if(!require(readxl)){install.packages("readxl")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(cowplot)){install.packages("cowplot")}
if(!require(Hmisc)){install.packages("Hmisc")}
if(!require(reshape2)){install.packages("reshape2")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(xtable)){install.packages("xtable")}
if(!require(stats)){install.packages("stats")}
if(!require(scales)){install.packages("scales")}
if(!require(sqldf)){install.packages("sqldf")}

# Retorna para cada individuo la diferencia de puntajes entre dos fases sucesivas.
# df: data frame con los datos
# item: el diferencial semantico (comúnmente 1, 2 ó 3) 
# ph1: la primera fase a considerar (1, 2 ó 3)
# ph2: la segunda fase a considerar (1, 2 ó 3)
get_deltas <- function(df, item, ph1, ph2) {
  query = paste("select d1.sel - d2.sel as delta, d1.sel as ph1_sel, d2.sel as ph2_sel from (select user_id, sel from df where iteration =", ph1)
  query = paste(query, "and df.df = ")
  query = paste(query, item)
  query = paste(query, ") as d1, (select user_id, sel from df where iteration =")
  query = paste(query, ph2)
  query = paste(query, "and df.df =")
  query = paste(query, item)
  query = paste(query, ") as d2 where d1.user_id = d2.user_id")
  return(sqldf(query))
}

# Retorna para cada individuo la diferencia de puntajes entre dos fases sucesivas.
# df_series1: data frame con data de primera serie
# df_series2: data frame con data de segunda serie
# item: el diferencial semantico (comúnmente 1, 2 ó 3) 
# ph1: la primera fase a considerar (1, 2 ó 3)
# ph2: la segunda fase a considerar (1, 2 ó 3)
build_ind_cmp_hist <- function(df_series1, df_series2, series_labels, item, ph1, ph2) {
  df_series1 <- get_deltas(df_series1, item, ph1, ph2)
  df_series1$type <- 0
  df_series2 <- get_deltas(df_series2, item, ph1, ph2)
  df_series2$type <- 1
  
  dt_cmp <- rbind.data.frame(df_series1, df_series2)
  dt_cmp$type <- factor(dt_cmp$type, levels = c(0,1), labels = series_labels)

  pt <- ggplot(dt_cmp, aes(x=delta, fill=type)) + 
    geom_histogram(position = 'identity', binwidth = 1, alpha = .7)

  return(pt)  
}

build_ind_cmp_density <- function(df_series1, df_series2, series_labels, 
                                  item, ph1, ph2, delta = TRUE, phase_arg = 1) {
  df_series1 <- get_deltas(df_series1, item, ph1, ph2)
  df_series1$type <- 0
  df_series2 <- get_deltas(df_series2, item, ph1, ph2)
  df_series2$type <- 1
  
  dt_cmp <- rbind.data.frame(df_series1, df_series2)
  dt_cmp$type <- factor(dt_cmp$type, levels = c(0,1), labels = series_labels)
  
  # build chart title
  if (delta) {
    chart_title <- paste0("Dist. of inter-phase score deltas\n in DS", item)
    chart_title <- paste(chart_title, paste("in Phases", paste0(ph1, paste0("-", ph2))))
    
    pt <- ggplot(dt_cmp, aes(x=delta)) + 
      geom_histogram(aes(y = ..density.., fill=type),
                     position = 'identity',
                     colour = "#CCCCCC",
                     binwidth = 1,
                     alpha = .5) +
      scale_color_manual("Grouping", values=c("#DB3A34", "#1B4965")) +
      scale_fill_manual("Grouping", values=c("#DB3A34", "#1B4965")) +
      geom_line(aes(colour=type), stat="density", size = 1.5) +
      labs(x="Score Delta", y="Density") + 
      theme(legend.position = 'bottom') +
      ggtitle(chart_title)
  }
  else {
    if (phase_arg == 1) {
      chart_title <- paste0("Distribution of individual DS", paste(item, "scores\n"))
      chart_title <- paste(chart_title, paste("in Phase", ph1))
      
      pt <- ggplot(dt_cmp, aes(x=ph1_sel)) + 
        geom_histogram(aes(y = ..density.., fill=type),
                       position = 'identity',
                       colour = "#CCCCCC",
                       binwidth = 1,
                       alpha = .5) +
        scale_color_manual("Grouping", values=c("#DB3A34", "#1B4965")) +
        scale_fill_manual("Grouping", values=c("#DB3A34", "#1B4965")) +
        geom_line(aes(colour=type), stat="density", size = 1.5) +
        theme(legend.position = 'bottom') + 
        labs(x="Score", y="Density") +
        ggtitle(chart_title)
    }
    else if (phase_arg == 2) {
      chart_title <- paste0("Distribution of individual DS", paste(item, "scores\n"))
      chart_title <- paste(chart_title, paste("in Phase", ph2))
      
      pt <- ggplot(dt_cmp, aes(x=ph2_sel)) + 
        geom_histogram(aes(y = ..density.., fill=type),
                       position = 'identity',
                       colour = "#CCCCCC",
                       binwidth = 1,
                       alpha = .5) +
        scale_color_manual("Grouping", values=c("#DB3A34", "#1B4965")) +
        scale_fill_manual("Grouping", values=c("#DB3A34", "#1B4965")) +
        geom_line(aes(colour=type), stat="density", size = 1.5) +
        theme(legend.position = 'bottom') + 
        labs(x="Score", y="Density") +
        ggtitle(chart_title)
    }
  }
  
  return(pt)  
}


# prefix: prefijo para el nombre de archivo de imagen a generar.
# builder: la función que genera el histograma
# df_rnd: data frame con los datos de grupos formados aleatoriamente
# df_ht: data frame con los datos de grupos formados heterogéneamente
# ph1: fase 1 a comparar
# ph2: fase 2 a comparar
save_ind_cmp_hist <- function(prefix, builder, df_rnd, df_ht, item, ph1, ph2) {
  hst <- builder(df_rnd, df_ht, item, ph1, ph2)

  fname <- paste0("./plots/", prefix)
  fname <- paste0(fname, item)
  fname <- paste0(fname, "-")
  fname <- paste0(fname, ph1)
  fname <- paste0(fname, "_")
  fname <- paste0(fname, ph2)
  fname <- paste0(fname, ".png")
  ggsave(plot = hst, fname, device = "png", dpi = 300)
}

build_group_cmp_hist <- function(df_series1, df_series2, series_labels, item, ph1, ph2) {
  df_series1 <- get_group_coef_var(df_series1, ph1, ph2)
  df_series1 <- df_series1[df_series1$df == item,] 
  df_series1$type <- 0
  
  df_series2 <- get_group_coef_var(df_series2, ph1, ph2)
  df_series2 <- df_series2[df_series2$df == item,] 
  df_series2$type <- 1
  
  dt_cmp <- rbind.data.frame(df_series1, df_series2)
  dt_cmp$type <- factor(dt_cmp$type, levels = c(0,1), labels = series_labels)
  
  pt <- ggplot(dt_cmp, aes(x=delta_vcoef, fill=type)) + 
     geom_histogram(position = 'identity', binwidth = .5, alpha = .7)
  
  return(pt)  
}

build_group_cmp_density <- function(df_series1, df_series2, series_labels, item, 
                                    ph1, ph2, delta = TRUE, phase_arg = 1) {
  df_series1 <- get_group_coef_var(df_series1, ph1, ph2)
  df_series1 <- df_series1[df_series1$df == item,] 
  df_series1$type <- 0
  
  df_series2 <- get_group_coef_var(df_series2, ph1, ph2)
  df_series2 <- df_series2[df_series2$df == item,] 
  df_series2$type <- 1
  
  dt_cmp <- rbind.data.frame(df_series1, df_series2)
  dt_cmp$type <- factor(dt_cmp$type, levels = c(0,1), labels = series_labels)

  pt <- NA
  
  if (delta) {
    # build chart title
    chart_title <- paste0("Dist. of deltas in group variation\n coefficients in DS", 
                          paste0(item, paste0(", Phases ", paste0(ph1, paste0("-", ph2)))))
  
    pt <- ggplot(dt_cmp, aes(x=delta_vcoef)) + 
      geom_histogram(aes(y = ..density.., fill=type),
                     position = 'identity',
                     colour = "#CCCCCC",
                     binwidth = .1,
                     alpha = .5) +
      scale_color_manual("Grouping", values=c("#DB3A34", "#1B4965")) +
      scale_fill_manual("Grouping", values=c("#DB3A34", "#1B4965")) +
      geom_line(aes(colour=type), stat="density", size = 1.5) +
      theme(legend.position = 'bottom') + 
      labs(x="VC Delta", y="Density") +
      ggtitle(chart_title)
    
    # pt <- ggplot(dt_cmp, aes(x=delta_vcoef, fill=type)) + 
    #   geom_density(alpha = .7) +
    #   theme(legend.position = 'bottom')    
  }
  else {
    if (phase_arg == 1) {
      # pt <- ggplot(dt_cmp, aes(x=ph1_vcoef, fill=type)) + 
      #   geom_density(alpha = .7) +
      #   theme(legend.position = 'bottom')    
      # build chart title
      chart_title <- paste0("Distribution of variation coefficient\n in groups, DS", 
                            paste0(item, paste0(", Phase", ph1)))
      
      pt <- ggplot(dt_cmp, aes(x=ph1_vcoef)) + 
        geom_histogram(aes(y = ..density.., fill=type),
                       position = 'identity',
                       colour = "#CCCCCC",
                       binwidth = .1,
                       alpha = .5) +
        scale_color_manual("Grouping", values=c("#DB3A34", "#1B4965")) +
        scale_fill_manual("Grouping", values=c("#DB3A34", "#1B4965")) +
        geom_line(aes(colour=type), stat="density", size = 1.5) +
        theme(legend.position = 'bottom') + 
        labs(x="Variation Coefficient", y="Density") +
        ggtitle(chart_title)      
    }
    else if (phase_arg == 2) {
      # pt <- ggplot(dt_cmp, aes(x=ph2_vcoef, fill=type)) + 
      #   geom_density(alpha = .7) +
      #   theme(legend.position = 'bottom')    
      # build chart title
      chart_title <- paste0("Distribution of variation coefficient\nin groups, DS", 
                            paste0(item, paste0(", Phase", ph2)))
      
      pt <- ggplot(dt_cmp, aes(x=ph2_vcoef)) + 
        geom_histogram(aes(y = ..density.., fill=type),
                       position = 'identity',
                       colour = "#CCCCCC",
                       binwidth = .1,
                       alpha = .5) +
        scale_color_manual("Grouping", values=c("#DB3A34", "#1B4965")) +
        scale_fill_manual("Grouping", values=c("#DB3A34", "#1B4965")) +
        geom_line(aes(colour=type), stat="density", size = 1.5) +
        theme(legend.position = 'bottom') + 
        labs(x="Variation Coefficient", y="Density") +
        ggtitle(chart_title)         
    }
  }

  return(pt)  
}

# df: data frame con datos originales
# item: diferencial semántico (1, 2, ó 3)
# ph1: Primera fase a considerar
# ph2: Segunda fase a considerar
# Retorna un dataframe que contiene para cada grupo la media de puntaje de la primera fase, 
# la media de la segunda, y la diferencia de las medias.
get_group_mean_deltas <- function(df, item, ph1, ph2) {
  query <- paste("select d1.team as team, d1.mean as ph1_mean, d2.mean as ph2_mean, d1.mean-d2.mean as mean_delta from 
                 (select team, avg(sel) as mean from df where df.df = ")
  query <- paste0(query, ph1)
  query <- paste(query, "group by team) as d1, (select team, avg(sel) as mean from df where df.df = ")
  query <- paste(query, ph2)
  query <- paste(query, "group by team) as d2 where d1.team = d2.team")
  print(query)
  return(sqldf(query))
}

# df: data frame con datos originales
# item: diferencial semántico (1, 2, ó 3)
# ph1: Primera fase a considerar
# ph2: Segunda fase a considerar
# Retorna un dataframe que contiene para cada grupo el coeficiente de variacion de la primera fase, 
# el coeficiente de variación de la segunda, y la diferencia del coeficiente de variación.
get_group_coef_var <- function(df, ph1, ph2) {
  query <- paste("select d1.team, d1.df, d1.vcoef as ph1_vcoef, d2.vcoef as ph2_vcoef, d1.vcoef - d2.vcoef as delta_vcoef from 
                 (select team, iteration, df.df, stdev(sel)/avg(sel) as vcoef from df where iteration =")
  query <- paste(query, ph1)
  query <- paste(query, "group by team, df.df order by team, df.df asc) as d1,
                        (select team, iteration, df.df, stdev(sel)/avg(sel) as vcoef from df where iteration =")
  query <- paste(query, ph2)
  query <- paste(query, "group by team, df.df order by team, df.df asc) as d2
                  where d1.team = d2.team and d1.df = d2.df")
  # debug
  print(query)
  
  result <- sqldf(query)
  return(sqldf(query))
}
