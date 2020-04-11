### Exploracion de variaciones inter-fase en primera medición (agosto 2019)
data_random_201920 <- read_excel("./data-ethics-201920/201920-ethics-1-random.xlsx", "Selection")
data_random_201920 <- as.data.frame(data_random_201920)

data_hetero_201920 <- read_excel("./data-ethics-201920/201920-ethics-1-hetero.xlsx", "Selection")
data_hetero_201920 <- as.data.frame(data_hetero_201920)

## Análisis con datos de estudio 2019-10
## Plots para cada item (DS), comparando diferencias individuales
## entre fases 1-2, y 2-3.
prefix <- "201920-08-agrosuper-ind-"

save_cmp_hist(prefix, build_cmp_hist, data_random_201920, data_hetero_201920, 1, 1, 2)
save_cmp_hist(prefix, build_cmp_hist, data_random_201920, data_hetero_201920, 1, 2, 3)

save_cmp_hist(prefix, build_cmp_hist, data_random_201920, data_hetero_201920, 2, 1, 2)
save_cmp_hist(prefix, build_cmp_hist, data_random_201920, data_hetero_201920, 2, 2, 3)

save_cmp_hist(prefix, build_cmp_hist, data_random_201920, data_hetero_201920, 3, 1, 2)
save_cmp_hist(prefix, build_cmp_hist, data_random_201920, data_hetero_201920, 3, 2, 3)

# En ambos casos delta 0 domina
gcv_rnd_12 <- get_group_coef_var(data_random_201920, 1, 2)
gcv_rnd_23 <- get_group_coef_var(data_random_201920, 2, 3)

gcv_ht_12 <- get_group_coef_var(data_hetero_201920, 1, 2)
gcv_ht_23 <- get_group_coef_var(data_hetero_201920, 2, 3)

summary(gcv_rnd_12[gcv_rnd_12$df == 1,]$delta_vcoef)
summary(gcv_rnd_23[gcv_rnd_23$df == 1,]$delta_vcoef)

summary(gcv_ht_12[gcv_ht_12$df == 1,]$delta_vcoef)
summary(gcv_ht_23[gcv_ht_23$df == 1,]$delta_vcoef)

summary(gcv_rnd_12[gcv_rnd_12$df == 2,]$delta_vcoef)
summary(gcv_rnd_23[gcv_rnd_23$df == 2,]$delta_vcoef)

summary(gcv_ht_12[gcv_ht_12$df == 2,]$delta_vcoef)
summary(gcv_ht_23[gcv_ht_23$df == 2,]$delta_vcoef)

summary(gcv_rnd_12[gcv_rnd_12$df == 3,]$delta_vcoef)
summary(gcv_rnd_23[gcv_rnd_23$df == 3,]$delta_vcoef)

summary(gcv_ht_12[gcv_ht_12$df == 3,]$delta_vcoef)
summary(gcv_ht_23[gcv_ht_23$df == 3,]$delta_vcoef)

## Análisis con datos de 2019
## Plots para cada item (DS), comparando diferencias grupales
## de coeficiente de variación entre fases 1-2, y 2-3.
prefix <- "201920-08-agrosuper-grp-"
save_cmp_hist(prefix, build_group_cmp_hist, data_random_201920, data_hetero_201920, 1, 1, 2)
save_cmp_hist(prefix, build_group_cmp_hist, data_random_201920, data_hetero_201920, 1, 2, 3)

save_cmp_hist(prefix, build_group_cmp_hist, data_random_201920, data_hetero_201920, 2, 1, 2)
save_cmp_hist(prefix, build_group_cmp_hist, data_random_201920, data_hetero_201920, 2, 2, 3)

save_cmp_hist(prefix, build_group_cmp_hist, data_random_201920, data_hetero_201920, 3, 1, 2)
save_cmp_hist(prefix, build_group_cmp_hist, data_random_201920, data_hetero_201920, 3, 2, 3)

### Exploracion de variaciones inter-fase en segunda medición (noviembre 2019)

data_hetero_201920_2 <- read_excel("./data-ethics-201920/201920-ethics-2-hetero.xlsx", "Selection")
data_hetero_201920_2 <- as.data.frame(data_hetero_201920_2)

# Analisis nivel individual
get_deltas(data_hetero_201920_2, 1, 1, 2)
get_deltas(data_hetero_201920_2, 1, 2, 3)

get_deltas(data_hetero_201920_2, 2, 1, 2)
get_deltas(data_hetero_201920_2, 2, 2, 3)

get_deltas(data_hetero_201920_2, 3, 1, 2)
get_deltas(data_hetero_201920_2, 3, 2, 3)

# Analisis de nivel grupal
get_group_coef_var(data_hetero_201920_2, 1, 2)
get_group_coef_var(data_hetero_201920_2, 2, 3)

### Exploracion de variaciones inter-fase en segunda medición (noviembre 2019)
data_hetero_201920_2 <- read_excel("./data-ethics-201920/201920-ethics-2-hetero.xlsx", "Selection")
data_hetero_201920_2 <- as.data.frame(data_hetero_201920_2)

# Analisis nivel individual
get_deltas(data_hetero_201920_2, 1, 1, 2)
get_deltas(data_hetero_201920_2, 1, 2, 3)

get_deltas(data_hetero_201920_2, 2, 1, 2)
get_deltas(data_hetero_201920_2, 2, 2, 3)

get_deltas(data_hetero_201920_2, 3, 1, 2)
get_deltas(data_hetero_201920_2, 3, 2, 3)

# Analisis de nivel grupal
get_group_coef_var(data_hetero_201920_2, 1, 2)
get_group_coef_var(data_hetero_201920_2, 2, 3)
