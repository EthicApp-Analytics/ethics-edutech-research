source("analysis_rand_hetero.R")

data_random_201920 <- read_excel("./data-ethics-201920/201920-ethics-1-random-agrosuper.xlsx", "Selection")
data_random_201920 <- as.data.frame(data_random_201920)

data_hetero_202010 <- read_excel("./data-ethics-202010/202010-ethics-1-hetero-agrosuper.xlsx", "Selection")
data_hetero_202010 <- as.data.frame(data_hetero_202010)

data_hetero_202010$name

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

data <- list(data_random_201920, data_random_202010, data_hetero_202010)
title_prefxs <- c("201920 RANDOM DS", "202010 RANDOM DS", "202010 HETERO DS")

table <- NA
for (i in 1:3) {
  table <- build_summary_table(table, data[[i]], title_prefxs[[i]], sel_col_extract)
}

write.csv(x = as.data.frame(table), file = "tables/data-ds-summary.csv")

## Deltas entre fases
table <- NA
table <- add_summary_table_row(table, data_random_201920, 1, 1, "201920 RANDOM DELTA DS1 P12", sel_delta_col_extract, phase2=2)
table <- add_summary_table_row(table, data_random_201920, 1, 2, "201920 RANDOM DELTA DS1 P23", sel_delta_col_extract, phase2=3)
table <- add_summary_table_row(table, data_random_201920, 1, 1, "201920 RANDOM DELTA DS1 P13", sel_delta_col_extract, phase2=3)

for (i in 1:3) {
  table <- build_summary_table(table, data[[i]], title_prefxs[[i]], sel_col_extract)
}

table

write.csv(x = as.data.frame(table), file = "tables/data-ds-summary.csv")

###############################################################################
if(!require(afe)){install.packages("afe", repos="http://R-Forge.R-project.org")}
if(!require(car)){install.packages("car")}
if(!require(psych)){install.packages("psych")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(phia)){install.packages("phia")}
if(!require(stargazer)){install.packages("stargazer")}
if(!require(readxl)){install.packages("readxl")}
if(!require(quanteda)){install.packages("quanteda")}
if(!require(stm)){install.packages("stm")}
if(!require(phia)){install.packages("phia")}
if(!require(sqldf)){install.packages("sqldf")}
if(!require(Hmisc)){install.packages("Hmisc")}
if(!require(ggcorrplot)){install.packages("ggcorrplot")}
if(!require(gridExtra)){install.packages("gridExtra")}


data_chat_202010 <- read_excel("./data-ethics-202010/202010-ethics-agrosuper-grupos.xlsx", "MANIFEST")
data_chat_202010 <- as.data.frame(data_chat_202010)

data_chat_202010$GENDER <- as.factor(data_chat_202010$GENDER)
data_chat_202010$ALG <- as.factor(data_chat_202010$ALG)

interaction.plot(x.factor     = data_chat_202010$ALG,
                 trace.factor = data_chat_202010$GENDER,
                 response     = data_chat_202010$N_MSG_CHAT,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o",
                 trace.label = "Gender",
                 xlab = "Grouping Method",
                 ylab = "Chat Messages per Student (Mean)")

model = lm(N_MSG_CHAT ~ ALG + GENDER + ALG:GENDER,
           data = data_chat_202010)
stargazer(model, title="Anova Table (Type II tests)", type = "html")

# phia interaction diagram with error
IM = interactionMeans(model)
plot(IM)

library(car)

anova_2 <- Anova(model,
      type = "II")

fulltb <- nice.anova(anova_2, es = NULL, correction = c("GG", "HF",
                                             "none"), sig.symbols = c(" +", " *", " **", " ***"),
           MSE = TRUE)
print.xtable(xtable(fulltb, caption = "ANOVA 2"), "html", include.rownames = FALSE, floating.environment = "sidewaystable")

##############
## Comparative Analysis: chat message length

chat_msg_202010_1_hetero <- read_excel("./data-ethics-202010/202010-ethics-1-hetero-agrosuper.xlsx", "Chat")
chat_msg_202010_1_random <- read_excel("./data-ethics-202010/202010-ethics-2-random-agrosuper.xlsx", "Chat")
colnames(chat_msg_202010_1_random)

chat_msg_202010_1_hetero_df_message <- chat_msg_202010_1_hetero[, c('df', 'message', 'team', 'user_id')]
chat_msg_202010_1_hetero_df_message$GROUPING <- 'HET'
chat_msg_202010_1_random_df_message <- chat_msg_202010_1_random[, c('df', 'message', 'team', 'user_id')]
chat_msg_202010_1_random_df_message$GROUPING <- 'RND'

chat_msg_202010_1_df_message <- chat_msg_202010_1_hetero_df_message
chat_msg_202010_1_df_message <- rbind.data.frame(chat_msg_202010_1_df_message, 
                                                 chat_msg_202010_1_random_df_message)

chat_msg_202010_1_df_message$message_corpus <- corpus(chat_msg_202010_1_df_message$message)
chat_msg_202010_1_df_message$ntokens <- ntoken(chat_msg_202010_1_df_message$message, remove_punct = TRUE)

chat_msg_202010_1_df_message$df <- factor(chat_msg_202010_1_df_message$df, levels = c(1,2,3), labels = c('CS1', 'CS2', 'CS3'))
chat_msg_202010_1_df_message$GROUPING <- as.factor(chat_msg_202010_1_df_message$GROUPING)

pt_css_chat_ntokens <- ggplot(data = chat_msg_202010_1_df_message) +
  geom_histogram(aes(x = ntokens, color = GROUPING, fill = GROUPING), binwidth=5) +
  #scale_x_continuous(labels = c(seq(1, 2, 3)), breaks = seq(1, 2, 3)) +
  #ggtitle("Number of tokens in chat messages per grouping method\nand case statement") + 
  theme_bw() +
  theme(legend.position="bottom",
        plot.title = element_text(size=24, margin=margin(0,0,20,0)),
        axis.title.x = element_text(margin=margin(20,0,0,0)),
        axis.title.y = element_text(margin=margin(0,20,0,0)),
        legend.title= element_text(size=18),
        legend.text= element_text(size=16),
        axis.text = element_text(size=18),
        axis.title = element_text(size=18),
        strip.text = element_text(size=20)) +
  xlab("Chat message length (number of tokens)") +
  ylab("Frequency") +
  labs(fill = "Grouping") + 
  guides(color=FALSE) +
  scale_fill_manual(labels=c('Heterogeneous', 'Random'), values=c('#bc38cf', '#3483eb')) +
  scale_color_manual(values=c('#760885', '#0b59bf')) +
  facet_wrap(. ~ df)

ggsave(plot = pt_css_chat_ntokens, file="./plots/202010-ntokens-df-grp.eps", 
       units = "cm", scale = 3, dpi = 300, width=10.0, height = 7)

#####
#### Descriptive Analysis

chat_msg_202010_1_hetero <- read_excel("./data-ethics-202010/202010-ethics-1-hetero-agrosuper.xlsx", "Chat")
chat_msg_202010_1_hetero$GROUPING <- 'HET'
chat_msg_202010_1_random <- read_excel("./data-ethics-202010/202010-ethics-2-random-agrosuper.xlsx", "Chat")
chat_msg_202010_1_random$GROUPING <- 'RND'
chat_msg_202010_1_complete <- chat_msg_202010_1_hetero
chat_msg_202010_1_complete <- rbind.data.frame(chat_msg_202010_1_complete, chat_msg_202010_1_random)

max(chat_msg_202010_1_hetero$team)- min(chat_msg_202010_1_hetero$team) + 1 # 11
max(chat_msg_202010_1_random$team)- min(chat_msg_202010_1_random$team) + 1 # 11

colnames(chat_msg_202010_1_complete)
sqldf("select GROUPING, count(df) from chat_msg_202010_1_complete where df = 1 group by GROUPING")
sqldf("select GROUPING, count(df) from chat_msg_202010_1_complete where df = 2 group by GROUPING")
sqldf("select GROUPING, count(df) from chat_msg_202010_1_complete where df = 3 group by GROUPING")

# Group messages; descriptive statistics on group level
grp_msg_ds_rnd_df1 <- sqldf("select team, count(message) as msgcnt from chat_msg_202010_1_complete where df = 1 and GROUPING='RND' group by team")
summary(grp_msg_ds_rnd_df1$msgcnt)
sd(grp_msg_ds_rnd_df1$msgcnt)

grp_msg_ds_het_df1 <- sqldf("select team, count(message) as msgcnt from chat_msg_202010_1_complete where df = 1 and GROUPING='HET' group by team")
summary(grp_msg_ds_het_df1$msgcnt)
sd(grp_msg_ds_het_df1$msgcnt)

grp_msg_ds_rnd_df2 <- sqldf("select team, count(message) as msgcnt from chat_msg_202010_1_complete where df = 2 and GROUPING='RND' group by team")
summary(grp_msg_ds_rnd_df2$msgcnt)
sd(grp_msg_ds_rnd_df2$msgcnt)

grp_msg_ds_het_df2 <- sqldf("select team, count(message) as msgcnt from chat_msg_202010_1_complete where df = 2 and GROUPING='HET' group by team")
summary(grp_msg_ds_het_df2$msgcnt)
sd(grp_msg_ds_het_df2$msgcnt)

grp_msg_ds_rnd_df3 <- sqldf("select team, count(message) as msgcnt from chat_msg_202010_1_complete where df = 3 and GROUPING='RND' group by team")
summary(grp_msg_ds_rnd_df3$msgcnt)
sd(grp_msg_ds_rnd_df3$msgcnt)

grp_msg_ds_het_df3 <- sqldf("select team, count(message) as msgcnt from chat_msg_202010_1_complete where df = 3 and GROUPING='HET' group by team")

summary(grp_msg_ds_het_df3$msgcnt)
sd(grp_msg_ds_het_df3$msgcnt)

# Group messages; descriptive statistics on individual level
colnames(chat_msg_202010_1_complete)
ind_msg_ds_rnd_df1 <- sqldf("select count(message) as msgcnt from chat_msg_202010_1_complete where df = 1 and GROUPING='RND' group by user_id")
summary(ind_msg_ds_rnd_df1$msgcnt)
sd(ind_msg_ds_rnd_df1$msgcnt)

ind_msg_ds_het_df1 <- sqldf("select team, count(message) as msgcnt from chat_msg_202010_1_complete where df = 1 and GROUPING='HET' group by user_id")
summary(ind_msg_ds_het_df1$msgcnt)
sd(ind_msg_ds_het_df1$msgcnt)

ind_msg_ds_rnd_df2 <- sqldf("select team, count(message) as msgcnt from chat_msg_202010_1_complete where df = 2 and GROUPING='RND' group by user_id")
summary(ind_msg_ds_rnd_df2$msgcnt)
sd(ind_msg_ds_rnd_df2$msgcnt)

ind_msg_ds_het_df2 <- sqldf("select team, count(message) as msgcnt from chat_msg_202010_1_complete where df = 2 and GROUPING='HET' group by user_id")
summary(ind_msg_ds_het_df2$msgcnt)
sd(ind_msg_ds_het_df2$msgcnt)

ind_msg_ds_rnd_df3 <- sqldf("select team, count(message) as msgcnt from chat_msg_202010_1_complete where df = 3 and GROUPING='RND' group by user_id")
summary(ind_msg_ds_rnd_df3$msgcnt)
sd(ind_msg_ds_rnd_df3$msgcnt)

ind_msg_ds_het_df3 <- sqldf("select team, count(message) as msgcnt from chat_msg_202010_1_complete where df = 3 and GROUPING='HET' group by user_id")
summary(ind_msg_ds_het_df3$msgcnt)
sd(ind_msg_ds_het_df3$msgcnt)

# Group deltas vs. message token correlations
data_202010_1_hetero <- read_excel("./data-ethics-202010/202010-ethics-1-hetero-agrosuper.xlsx", "Selection")
data_202010_2_random <- read_excel("./data-ethics-202010/202010-ethics-2-random-agrosuper.xlsx", "Selection")

data_202010_1_complete <- data_202010_1_hetero
data_202010_1_complete$GROUPING <- 'HET'
data_202010_2_random$GROUPING <- 'RND'

data_202010_1_complete <- rbind.data.frame(data_202010_1_complete, data_202010_2_random)

chat_msg_202010_1_hetero <- read_excel("./data-ethics-202010/202010-ethics-1-hetero-agrosuper.xlsx", "Chat")
chat_msg_202010_1_random <- read_excel("./data-ethics-202010/202010-ethics-2-random-agrosuper.xlsx", "Chat")

chat_msg_202010_1_complete <- chat_msg_202010_1_hetero
chat_msg_202010_1_complete <- rbind.data.frame(chat_msg_202010_1_complete, chat_msg_202010_1_random)
chat_msg_202010_1_complete$ntokens <- ntoken(chat_msg_202010_1_complete$message, remove_punct = TRUE)

chat_msg_202010_1_chat_quali <- read_excel("./data-ethics-202010/202010-ethics-agrosuper-chat-qualitative.xlsx", "Chat")

# CS1
grp_mean_delta_all_cs1 <- get_group_mean_deltas(data_202010_1_complete, 1, 2, 3)
grp_sum_delta_all_cs1 <- get_group_sum_deltas(data_202010_1_complete, 1, 2, 3)

# grupos 619 (het), {626, 627, 630, 632 (2), 633} (rand) perdieron integrantes en fase 3
grps_discarded <- c(619, 626, 627, 630, 632, 633)

# Consider all messages, regardless of grouping algorithm
grp_ans_chat_complete_cs1 <- sqldf("select 
          arg.team, arg.Argument_Count, chat.Average_Tokens, chat.Sum_Tokens, chat.Message_Count, sd.Delta_Avg 
        from 
          (select team, sum(ARG) as Argument_Count from chat_msg_202010_1_chat_quali where DS = 1 group by team) as arg,
          (select team, count(message) as Message_Count, avg(ntokens) as Average_Tokens, sum(ntokens) as Sum_Tokens from chat_msg_202010_1_complete where df = 1 group by team) as chat,
          (select team, mean_delta as Delta_Avg from grp_mean_delta_all_cs1) as sd
        where 
          arg.team = chat.team and chat.team = sd.team")

corr_data_cs1 <- as.matrix(
  grp_ans_chat_complete_cs1[, 
  #grp_ans_chat_complete_cs1[!(grp_ans_chat_complete_cs1$team %in% grps_discarded), 
                            c('Argument_Count', 'Sum_Tokens', 'Message_Count', 'Delta_Avg')])
corr_cs1 <- rcorr(corr_data_cs1)
corr_cs1$r
corr_cs1$P

corrplot_cs1 <- ggcorrplot(corr_cs1$r, 
           type = "lower", 
           lab = TRUE, 
           p.mat = corr_cs1$P,
           sig.level = 0.05) +
  #scale_x_discrete(label=function(x) abbreviate(x, minlength = 10)) +
  theme_classic() + 
  labs(x="CS1") +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, size = 10, vjust = 0.7, hjust=0.75),
        axis.text.y = element_text(size = 10))
  
# CS2
grp_mean_delta_all_cs2 <- get_group_mean_deltas(data_202010_1_complete, 2, 2, 3)
grp_sum_delta_all_cs2 <- get_group_sum_deltas(data_202010_1_complete, 2, 2, 3)

# Consider all messages, regardless of grouping algorithm
grp_ans_chat_complete_cs2 <- sqldf("select 
          arg.team, arg.Argument_Count, chat.Average_Tokens, chat.Sum_Tokens, chat.Message_Count, sd.Delta_Sum 
        from 
          (select team, sum(ARG) as Argument_Count from chat_msg_202010_1_chat_quali where DS = 2 group by team) as arg,
          (select team, count(message) as Message_Count, avg(ntokens) as Average_Tokens, sum(ntokens) as Sum_Tokens from chat_msg_202010_1_complete where df = 2 group by team) as chat,
          (select team, sumscore_delta as Delta_Sum from grp_sum_delta_all_cs2) as sd
        where 
          arg.team = chat.team and chat.team = sd.team")

colnames(grp_ans_chat_complete_cs2)
corr_data_cs2 <- as.matrix(
  grp_ans_chat_complete_cs2[!(grp_ans_chat_complete_cs1$team %in% grps_discarded), 
                            c('Argument_Count', 'Sum_Tokens', 'Message_Count', 'Delta_Sum')])
corr_cs2 <- rcorr(corr_data_cs2)
corr_cs2$r
corr_cs2$P

corrplot_cs2 <- ggcorrplot(corr_cs2$r, 
           type = "lower", 
           lab = TRUE, 
           p.mat = corr_cs2$P,
           sig.level = 0.05) +
  #scale_x_discrete(label=function(x) abbreviate(x, minlength = 10)) +
  theme_classic() + 
  labs(x="CS2") +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, size = 10, vjust = 0.7, hjust=0.75),
        axis.text.y = element_text(size = 10))

# CS3
grp_mean_delta_all_cs3 <- get_group_mean_deltas(data_202010_1_complete, 3, 2, 3)
grp_sum_delta_all_cs3 <- get_group_sum_deltas(data_202010_1_complete, 3, 2, 3)

# Consider all messages, regardless of grouping algorithm
grp_ans_chat_complete_cs3 <- sqldf("select 
          arg.team, arg.Argument_Count, chat.Average_Tokens, chat.Sum_Tokens, chat.Message_Count, sd.Delta_Sum 
        from 
          (select team, sum(ARG) as Argument_Count from chat_msg_202010_1_chat_quali where DS = 3 group by team) as arg,
          (select team, count(message) as Message_Count, avg(ntokens) as Average_Tokens, sum(ntokens) as Sum_Tokens from chat_msg_202010_1_complete where df = 3 group by team) as chat,
          (select team, sumscore_delta as Delta_Sum from grp_sum_delta_all_cs3) as sd
        where 
          arg.team = chat.team and chat.team = sd.team")

corr_data_cs3 <- as.matrix(
  grp_ans_chat_complete_cs1[!(grp_ans_chat_complete_cs3$team %in% grps_discarded), 
                            c('Argument_Count', 'Sum_Tokens', 'Message_Count', 'Delta_Sum')])
corr_cs3 <- rcorr(corr_data_cs3)
corr_cs3$r
corr_cs3$P

corrplot_cs3 <- ggcorrplot(corr_cs3$r, 
           type = "lower", 
           lab = TRUE, 
           p.mat = corr_cs3$P,
           sig.level = 0.05) +
  #scale_x_discrete(label=function(x) abbreviate(x, minlength = 10)) +
  theme_classic() + 
  labs(x="CS3") +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, size = 10, vjust = 0.7, hjust=0.75),
        axis.text.y = element_text(size = 10))

grid.arrange(corrplot_cs1, corrplot_cs2, corrplot_cs3, nrow = 1)
g <- arrangeGrob(corrplot_cs1, corrplot_cs2, corrplot_cs3, nrow=1) #generates g
ggsave(file="./plots/202010-cs-corrplots.eps", g,
       units = "cm", scale = 3, dpi = 300, width=12, height = 4) #saves g

####
grp_mean_delta_all_cs1 <- get_group_mean_deltas(data_202010_1_complete, 1, 2, 3)
grp_mean_delta_het_cs1 <- get_group_mean_deltas(data_202010_1_complete[data_202010_1_complete$GROUPING == "HET",], 1, 2, 3)
grp_mean_delta_rnd_cs1 <- get_group_mean_deltas(data_202010_1_complete[data_202010_1_complete$GROUPING == "RND",], 1, 2, 3)

grp_sum_delta_all_cs1 <- get_group_mean_deltas(data_202010_1_complete, 1, 2, 3)
grp_sum_delta_het_cs1 <- get_group_sum_deltas(data_202010_1_complete[data_202010_1_complete$GROUPING == "HET",], 1, 2, 3)
grp_sum_delta_rnd_cs1 <-  get_group_sum_deltas(data_202010_1_complete[data_202010_1_complete$GROUPING == "RND",], 1, 2, 3)

grp_mean_delta_het_cs2 <- get_group_mean_deltas(data_202010_1_complete[data_202010_1_complete$GROUPING == "HET",], 2, 2, 3)
grp_mean_delta_rnd_cs2 <- get_group_mean_deltas(data_202010_1_complete[data_202010_1_complete$GROUPING == "RND",], 2, 2, 3)

grp_sum_delta_all_cs2 <- get_group_mean_deltas(data_202010_1_complete, 2, 2, 3)
grp_sum_delta_het_cs2 <- get_group_sum_deltas(data_202010_1_complete[data_202010_1_complete$GROUPING == "HET",], 2, 2, 3)
grp_sum_delta_rnd_cs2 <-  get_group_sum_deltas(data_202010_1_complete[data_202010_1_complete$GROUPING == "RND",], 2, 2, 3)

# CS3
grp_mean_delta_all_cs3 <- get_group_mean_deltas(data_202010_1_complete, 3, 2, 3)
grp_mean_delta_het_cs3 <- get_group_mean_deltas(data_202010_1_complete[data_202010_1_complete$GROUPING == "HET",], 3, 2, 3)
grp_mean_delta_rnd_cs3 <- get_group_mean_deltas(data_202010_1_complete[data_202010_1_complete$GROUPING == "RND",], 3, 2, 3)

grp_sum_delta_all_cs3 <- get_group_mean_deltas(data_202010_1_complete, 3, 2, 3)
grp_sum_delta_het_cs3 <- get_group_sum_deltas(data_202010_1_complete[data_202010_1_complete$GROUPING == "HET",], 3, 2, 3)
grp_sum_delta_rnd_cs3 <-  get_group_sum_deltas(data_202010_1_complete[data_202010_1_complete$GROUPING == "RND",], 3, 2, 3)

#########################################################
## Individual deltas

data_hetero_202010_filt <- data_hetero_202010[!(data_hetero_202010$team %in% grps_discarded),]
data_random_202010_filt <- data_random_202010[!(data_random_202010$team %in% grps_discarded),]

nrow(data_hetero_202010_filt) # 262
nrow(data_random_202010_filt) # 148

ind_delta_het_1_23 <- get_deltas(data_hetero_202010_filt, 1, 2, 3)
ind_delta_het_1_23$Grouping = "HET"
ind_delta_het_1_23$CS = 1
ind_delta_rnd_1_23 <- get_deltas(data_random_202010_filt, 1, 2, 3)
ind_delta_rnd_1_23$Grouping = "RND"
ind_delta_rnd_1_23$CS = 1
ind_delta_het_2_23 <- get_deltas(data_hetero_202010_filt, 2, 2, 3)
ind_delta_het_2_23$Grouping = "HET"
ind_delta_het_2_23$CS = 2
ind_delta_rnd_2_23 <- get_deltas(data_random_202010_filt, 2, 2, 3)
ind_delta_rnd_2_23$Grouping = "RND"
ind_delta_rnd_2_23$CS = 2
ind_delta_het_3_23 <- get_deltas(data_hetero_202010_filt, 3, 2, 3)
ind_delta_het_3_23$Grouping = "HET"
ind_delta_het_3_23$CS = 3
ind_delta_rnd_3_23 <- get_deltas(data_random_202010_filt, 3, 2, 3)
ind_delta_rnd_3_23$Grouping = "RND"
ind_delta_rnd_3_23$CS = 3
ind_delta_complete_23 <- ind_delta_het_1_23
ind_delta_complete_23 <- rbind.data.frame(ind_delta_complete_23, ind_delta_rnd_1_23)
ind_delta_complete_23 <- rbind.data.frame(ind_delta_complete_23, ind_delta_het_2_23)
ind_delta_complete_23 <- rbind.data.frame(ind_delta_complete_23, ind_delta_rnd_2_23)
ind_delta_complete_23 <- rbind.data.frame(ind_delta_complete_23, ind_delta_het_3_23)
ind_delta_complete_23 <- rbind.data.frame(ind_delta_complete_23, ind_delta_rnd_3_23)
colnames(ind_delta_complete_23) <- c('Delta', 'AS Score', 'GD Score', 'Grouping', 'CS')

ggplot(data=ind_delta_complete_23) + 
  geom_histogram(binwidth = 1, aes(x=Delta)) + 
  facet_grid(CS ~ Grouping)

ggplot(data=ind_delta_complete_23[ind_delta_complete_23$Delta != 0,]) + 
  geom_histogram(binwidth = 1, aes(x=Delta)) + 
  facet_grid(CS ~ Grouping)

#########################################################
## Group delta sum

# grupo 619 perdio un integrante en fase 3
sqldf("select team, 2 as phase, count(team) from data_hetero_202010 where iteration = 2 and df = 1 group by team
      union 
      select team, 3 as phase, count(team) from data_hetero_202010 where iteration = 3 and df = 1 group by team")

# grupos 626, 627, 630, 632 (2), 633 perdieron integrantes en fase 3
# dificil controlar la situacion porque la actividad no se realizo presencial
sqldf("select team, 2 as phase, count(team) from data_random_202010 where iteration = 2 and df = 1 group by team
      union 
      select team, 3 as phase, count(team) from data_random_202010 where iteration = 3 and df = 1 group by team")

# TODO verificar que pasa con los mensajes de chat de los grupos que perdieron integrantes

grp_delta_het_1_23 <- get_group_sum_deltas(data_hetero_202010_filt, 1, 2, 3)
grp_delta_het_1_23$Grouping = "HET"
grp_delta_het_1_23$CS <- 1
grp_delta_het_1_23$team
grp_delta_rnd_1_23 <- get_group_sum_deltas(data_random_202010_filt, 1, 2, 3)
grp_delta_rnd_1_23$Grouping = "RND"
grp_delta_rnd_1_23$CS <- 1
grp_delta_rnd_1_23$team
grp_delta_het_2_23 <- get_group_sum_deltas(data_hetero_202010_filt, 2, 2, 3)
grp_delta_het_2_23$Grouping = "HET"
grp_delta_het_2_23$CS <- 2
grp_delta_rnd_2_23 <- get_group_sum_deltas(data_random_202010_filt, 2, 2, 3)
grp_delta_rnd_2_23$Grouping = "RND"
grp_delta_rnd_2_23$CS <- 2
grp_delta_het_3_23 <- get_group_sum_deltas(data_hetero_202010_filt, 3, 2, 3)
grp_delta_het_3_23$Grouping = "HET"
grp_delta_het_3_23$CS <- 3
grp_delta_rnd_3_23 <- get_group_sum_deltas(data_random_202010_filt, 3, 2, 3)
grp_delta_rnd_3_23$Grouping = "RND"
grp_delta_rnd_3_23$CS <- 3
grp_delta_complete_23 <- grp_delta_het_1_23
grp_delta_complete_23 <- rbind.data.frame(grp_delta_complete_23, grp_delta_rnd_1_23)
grp_delta_complete_23 <- rbind.data.frame(grp_delta_complete_23, grp_delta_het_2_23)
grp_delta_complete_23 <- rbind.data.frame(grp_delta_complete_23, grp_delta_rnd_2_23)
grp_delta_complete_23 <- rbind.data.frame(grp_delta_complete_23, grp_delta_het_3_23)
grp_delta_complete_23 <- rbind.data.frame(grp_delta_complete_23, grp_delta_rnd_3_23)
colnames(grp_delta_complete_23) <- c('Team','AS Score', 'GD Score', 'Delta', 'Grouping', 'CS')

grp_delta_complete_23$Team

ggplot(data=grp_delta_complete_23) + 
  geom_histogram(binwidth = 1, aes(x=Delta)) + 
  facet_grid(CS ~ Grouping)

grp_delta_complete_23[grp_delta_complete_23$Delta != 0,]

ggplot(data=grp_delta_complete_23[grp_delta_complete_23$Delta != 0,]) + 
  geom_histogram(binwidth = 1, aes(x=Delta)) + 
  facet_grid(CS ~ Grouping)
