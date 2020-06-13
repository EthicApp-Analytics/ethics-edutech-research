if(!require(readxl)){install.packages("readxl")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(cowplot)){install.packages("cowplot")}
if(!require(Hmisc)){install.packages("Hmisc")}
if(!require(reshape2)){install.packages("reshape2")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(xtable)){install.packages("xtable")}
if(!require(stats)){install.packages("stats")}
if(!require(scales)){install.packages("scales")}
if(!require(quanteda)){install.packages("quanteda")}

#data <- read_excel("./20190405-results.xlsx", "Selection")
data <- read_excel("./data-ethics-201920/201920-ethics-1-random.xlsx", "Selection")
data <- as.data.frame(data)
colnames(data)
unique(data$sel)

data$df <- factor(data$df)
data$iteration <- factor(data$iteration)

pt1 <- ggplot(data[data$df==1 & data$iteration == 1,]) + aes(x=`sel`) +
  geom_histogram(binwidth = 1, position = "identity", color="darkblue", fill="lightblue") +
  scale_x_discrete(limits=c(1:7)) +
    theme_bw() + 
  labs(x="IA Score", y="Count")
pt2 <- ggplot(data[data$df==1 & data$iteration == 2,]) + aes(x=`sel`) +
  geom_histogram(binwidth = 1, position = "identity", color="darkblue", fill="lightblue") +
  scale_x_discrete(limits=c(1:7)) +
  theme_bw() + 
  labs(x="AS Score", y="Count")
pt3 <- ggplot(data[data$df==1 & data$iteration == 3,]) + aes(x=`sel`) +
  geom_histogram(binwidth = 1, position = "identity", color="darkblue", fill="lightblue") +
  scale_x_discrete(limits=c(1:7)) +
  theme_bw() + 
  labs(x="GD Score", y="Count")
pt4 <- ggplot(data[data$df==2 & data$iteration == 1,]) + aes(x=`sel`) +
  geom_histogram(binwidth = 1, position = "identity", color="darkgreen", fill="lightgreen") +
  scale_x_discrete(limits=c(3:7)) +
  theme_bw() + 
  labs(x="IA Score", y="Count")
pt5 <- ggplot(data[data$df==2 & data$iteration == 2,]) + aes(x=`sel`) +
  geom_histogram(binwidth = 1, position = "identity", color="darkgreen", fill="lightgreen") +
  scale_x_discrete(limits=c(3:7)) +
  theme_bw() + 
  labs(x="AS Score", y="Count")
pt6 <- ggplot(data[data$df==2 & data$iteration == 3,]) + aes(x=`sel`) +
  geom_histogram(binwidth = 1, position = "identity", color="darkgreen", fill="lightgreen") +
  scale_x_discrete(limits=c(1:7)) +
  theme_bw() + 
  labs(x="GD Score", y="Count")
pt7 <- ggplot(data[data$df==3 & data$iteration == 1,]) + aes(x=`sel`) +
  geom_histogram(binwidth = 1, position = "identity", color="darkred", fill="pink") +
  scale_x_discrete(limits=c(1:7)) +
  theme_bw() + 
  labs(x="IA Score", y="Count")
pt8 <- ggplot(data[data$df==3 & data$iteration == 2,]) + aes(x=`sel`) +
  geom_histogram(binwidth = 1, position = "identity", color="darkred", fill="pink") +
  scale_x_discrete(limits=c(1:7)) +
  theme_bw() + 
  labs(x="AS Score", y="Count")
pt9 <- ggplot(data[data$df==3 & data$iteration == 3,]) + aes(x=`sel`) +
  geom_histogram(binwidth = 1, position = "identity", color="darkred", fill="pink") +
  scale_x_discrete(limits=c(1:7)) +
  theme_bw() + 
  labs(x="GD Score", y="Count")

grd <- plot_grid(pt1, pt2, pt3, 
                 pt4, pt5, pt6,
                 pt7, pt8, pt9,
                 nrow = 3)

title <- ggdraw() + 
  draw_label("Score distributions for each semantic differential\nscale in the phases of the trial activity")
grd_title <- plot_grid(title, grd, ncol = 1, rel_heights = c(0.1, 1))

ggsave(plot = grd_title, file="./plots/scores.eps", 
       units = "cm", scale = 3, dpi = 300, width=5.0, height = 5.0)

data_c <- dcast(data, user_id + df ~ iteration, value.var = "sel") 
colnames(data_c) <- c('user_id', 'df', 'score_1', 'score_2', 'score_3')
data_c$delta12 <- abs(data_c$score_2 - data_c$score_1)
data_c$delta23 <- abs(data_c$score_3 - data_c$score_2)
data_c$delta13 <- abs(data_c$score_3 - data_c$score_1)

pt10 <- ggplot(data_c[data_c$delta12 >= 1 & data_c$df == 1,]) + aes(x=`delta12`) +
  geom_histogram(binwidth = 1, position = "identity", color="darkblue", fill="lightblue") +
  scale_x_discrete(limits=c(1:6)) +
  theme_bw() + 
  labs(x="Diff. IA-AS", y="Count")

pt11 <- ggplot(data_c[data_c$delta23 >= 1 & data_c$df == 1,]) + aes(x=`delta23`) +
  geom_histogram(binwidth = 1, position = "identity", color="darkblue", fill="lightblue") +
  scale_x_discrete(limits=c(1:6)) +
  theme_bw() + 
  labs(x="Diff. AS-GD", y="Count")

pt12 <- ggplot(data_c[data_c$delta13 >= 1 & data_c$df == 1,]) + aes(x=`delta13`) +
  geom_histogram(binwidth = 1, position = "identity", color="darkblue", fill="lightblue") +
  scale_x_discrete(limits=c(1:6)) +
  theme_bw() + 
  labs(x="Diff. IA-GD", y="Count")

pt13 <- ggplot(data_c[data_c$delta12 >= 1 & data_c$df == 2,]) + aes(x=`delta12`) +
  geom_histogram(binwidth = 1, position = "identity", color="darkgreen", fill="lightgreen") +
  scale_x_discrete(limits=c(1:3)) +
  theme_bw() + 
  scale_x_discrete(limits=c("1","2")) +
  labs(x="Diff. IA-AS", y="Count")
  
pt14 <- ggplot(data_c[data_c$delta23 >= 1 & data_c$df == 2,]) + aes(x=`delta23`) +
  geom_histogram(binwidth = 1, position = "identity", color="darkgreen", fill="lightgreen") +
  scale_x_discrete(limits=c(1:3)) +
  theme_bw() + 
  labs(x="Diff. AS-GD", y="Count")

pt15 <- ggplot(data_c[data_c$delta13 >= 1 & data_c$df == 2,]) + aes(x=`delta13`) +
  geom_histogram(binwidth = 1, position = "identity", color="darkgreen", fill="lightgreen") +
  scale_x_discrete(limits=c(1:3)) +
  theme_bw() + 
  labs(x="Diff. IA-GD", y="Count")

pt16 <- ggplot(data_c[data_c$delta12 >= 1 & data_c$df == 3,]) + aes(x=`delta12`) +
  geom_histogram(binwidth = 1, position = "identity", fill="pink", color="darkred") +
  theme_bw() + 
  scale_x_discrete(limits=c(1:2)) +
  scale_y_continuous(breaks=c(0, 1,2)) +
  labs(x="Diff. IA-AS", y="Count")

pt17 <- ggplot(data_c[data_c$delta23 >= 1 & data_c$df == 3,]) + aes(x=`delta23`) +
  geom_histogram(binwidth = 1, position = "identity", fill="pink", color="darkred") +
  theme_bw() +
  scale_x_discrete(limits=c(1:2)) +
  labs(x="Diff. AS-GD", y="Count")

pt18 <- ggplot(data_c[data_c$delta13 >= 1 & data_c$df == 3,]) + aes(x=`delta13`) +
  geom_histogram(binwidth = 1, position = "identity", fill="pink", color="darkred") +
  theme_bw() + 
  scale_x_discrete(limits=c(1:2)) +
  labs(x="Diff. IA-GD", y="Count")

grd <- plot_grid(pt10, pt11, pt12, 
          pt13, pt14, pt15,
          pt16, pt17, pt18,
          nrow = 3)

title <- ggdraw() + 
  draw_label("Inter-phase score difference distributions for each\nsemantic differential scale in the trial activity")
grd_title <- plot_grid(title, grd, ncol = 1, rel_heights = c(0.1, 1))

ggsave(plot = grd_title, file="./plots/deltas.eps", 
       units = "cm", scale = 3, dpi = 300, width=5.0, height = 5.0)

kruskal.test(sel ~ iteration, data[data$df == 1, ])
kruskal.test(sel ~ iteration, data[data$df == 2, ])
kruskal.test(sel ~ iteration, data[data$df == 3, ])


#######

sus_data <- read_excel("./sus-results.xlsx", "SUS")
sus_data <- as.data.frame(sus_data)
colnames(sus_data)

mean(sus_data[,c(14)])
sd(sus_data[,c(14)])

score <- (sus_data$I1 - 1) + (5 - sus_data$I2) + (sus_data$I3 - 1) + (5 - sus_data$I4) + 
          (sus_data$I5 - 1) + (5 - sus_data$I6) + (sus_data$I7 - 1) + (5 - sus_data$I8) +
          (sus_data$I9 - 1) + (5 - sus_data$I10)

score <- score * 2.5
score <- as.data.frame(score)
colnames(score) <- c("SUS Score")

summary(score$`SUS Score`)

gbp <- ggplot(data = score, aes(x= factor(0), y=`SUS Score`)) +
  geom_boxplot() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  coord_flip() +
  ggtitle("Distribution of SUS scores")
gbp

boxplot(score)
mean(score) # 79.9
sd(score) # 14.3
length(score[score >= 85.58])
length(score[score >= 72.75 & score < 85.58])
15/35
10/35
9/35
length(score[score >= 52.01 & score < 72.75])

ggsave(plot = gbp, file="./plots/sus.eps", 
       units = "cm", scale = 3, dpi = 300, width=8.0, height = 2.3)


i11data <- as.data.frame(read_excel("./sus-results.xlsx", "I11_Results"))
i11data$`Sub Category` <- factor(i11data$`Sub Category`, levels=c(
  'Chat',
  'Text input',
  'Keep response',
  'State awareness',
  'Keyboard',
  'SDS',
  'PDF Viewer',
  'Icons',
  'Group',
  'Moderate chat'))

pt <- ggplot(data = i11data, aes(x=`Sub Category`, y=Frequency)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "pink", color= "red") +
  ggtitle("I11: Do you think something could improve in this application?") +
  labs(
    x = "Topics",
    y = "Frequency"
  ) +
  scale_y_continuous(breaks=pretty_breaks(n=5)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 15))
pt

ggsave(plot = pt, file="./plots/sus_i11.eps", 
       units = "cm", scale = 3, dpi = 300, width=6.0, height = 4.0)

i12data <- as.data.frame(read_excel("./sus-results.xlsx", "I12_Analysis"))
i12data$Category <- factor(i12data$Category, levels=c(
  'Anonymity',
  'Methodology',
  'Simplicity',
  'Chat',
  'Ease of use',
  'Design',
  'Idea exchange',
  'Features',
  'Reflection',
  'Interactivity',
  'Practicality',
  'Innovative'))

library(scales)
pt <- ggplot(data = i12data, aes(x=Category, y=Frequency)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "lightblue", color= "blue") +
  ggtitle("I12: What is the best thing you would highlight in this application?") +
  labs(
    x = "Topics",
    y = "Frequency"
  ) +
  scale_y_continuous(breaks=pretty_breaks(n=5)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 15))
pt

ggsave(plot = pt, file="./plots/sus_i12.eps", 
       units = "cm", scale = 3, dpi = 300, width=6.0, height = 4.0)

### Analisis de DS vs Chat
df_it2_avg_stdev_val <- sqldf("select df, team, team-547 as 'group', avg(sel) as 'avg_it2', stdev(sel) \
                              as 'stdev_it2', sum(sel) as 'sum_it2' from data where iteration = 2 group by df, team")
df_it2_avg_stdev_val$df <- as.numeric(df_it2_avg_stdev_val$df)

df_it3_avg_stdev_val <- sqldf("select df, team, team-547 as 'group', avg(sel) as 'avg_it3', stdev(sel) \
                              as 'stdev_it3', sum(sel) as 'sum_it3' from data where iteration = 3 group by df, team")
df_it3_avg_stdev_val$df <- as.numeric(df_it3_avg_stdev_val$df)

df_it2_it3_val <- inner_join(df_it2_avg_stdev_val, df_it3_avg_stdev_val, by.x = c("team", "df"), by.y = c("team", "df"))
df_it2_it3_val$sum_delta_23 <- abs(df_it2_it3_val$sum_it3 - df_it2_it3_val$sum_it2)
df_it2_it3_val$avg_delta_23 <- abs(df_it2_it3_val$avg_it3 - df_it2_it3_val$avg_it2)

### Analisis cruzado de chat y valores asignados a los DSs
chat_data <- read_excel("./20190405-results.xlsx", "Chat")
chat_data <- as.data.frame(chat_data)

chat_data$ntokens <- ntoken(chat_data$message)

#DS1
df_it3_avg_stdev_ntok_ds1 <- sqldf("select df, team, team-547 as 'group', sum(ntokens) as sum_ntokens_3, avg(ntokens) \
                                as 'avg_tokens_3', stdev(ntokens) \
                                as 'stdev_tokens_3' from chat_data \
                                where df = '1' \   
                                group by team")

df_chat_val_data_ds1 <- inner_join(df_it2_it3_val, df_it3_avg_stdev_ntok_ds1, by.x = c("team", "df"), by.y = c("team", "df"))

# matriz de correlacion para los atributos de discusion del df1; largo medio de comentarios, 
# stdev del largo de comentarios, suma de deltas entre fases 2 y 3
corr_ds1 <- rcorr(as.matrix(df_chat_val_data_ds1[, 
                                             c('sum_delta_23', 'avg_delta_23', 'sum_ntokens_3',
                                               'avg_tokens_3', 'stdev_tokens_3')]))
corr_ds1

out<- print(xtable(corr_ds1$r, 
             caption="Correlation Matrix (DS1)"), 
      "html", include.rownames=FALSE, caption.placement='top',
      html.table.attributes='align="left"')
writeLines(out, "./tables/ds1.html")

out<- print(xtable(corr_ds1$P, 
                   caption="Correlation significance Matrix (DS1)"), 
            "html", include.rownames=FALSE, caption.placement='top',
            html.table.attributes='align="left"')
writeLines(out, "./tables/ds1_p.html")

df_indcom_data_ds1 <- sqldf("select * from chat_data where df  = '1'")

sqldf("select * from df_chat_val_data_ds1 order by sum_ntokens_3 desc")

# lo mismo para DS2
df_it3_avg_stdev_ntok_ds2 <- sqldf("select df, team, team-547 as 'group', sum(ntokens) as sum_ntokens_3, avg(ntokens) \
                                   as 'avg_tokens_3', stdev(ntokens) \
                                   as 'stdev_tokens_3' from chat_data \
                                   where df = '2' \   
                                   group by team")

df_chat_val_data_ds2 <- inner_join(df_it2_it3_val, df_it3_avg_stdev_ntok_ds2, by.x = c("team", "df"), by.y = c("team", "df"))

# matriz de correlacion para los atributos de discusion del df1; largo medio de comentarios, 
# stdev del largo de comentarios, suma de deltas entre fases 2 y 3
corr_ds2 <- rcorr(as.matrix(df_chat_val_data_ds2[, 
                                                 c('sum_delta_23', 'avg_delta_23', 'sum_ntokens_3',
                                                   'avg_tokens_3', 'stdev_tokens_3')]))
corr_ds2

out<- print(xtable(corr_ds2$r, 
                   caption="Correlation Matrix (DS2)"), 
            "html", include.rownames=FALSE, caption.placement='top',
            html.table.attributes='align="left"')
writeLines(out, "./tables/ds2.html")

out<- print(xtable(corr_ds2$P, 
                   caption="Correlation significance Matrix (DS2)"), 
            "html", include.rownames=FALSE, caption.placement='top',
            html.table.attributes='align="left"')
writeLines(out, "./tables/ds2_p.html")

df_indcom_data_ds1 <- sqldf("select * from chat_data where df  = '1'")
hist(df_indcom_data_ds1$ntokens)

df_indcom_data_ds2 <- sqldf("select * from chat_data where df  = '2'")
hist(df_indcom_data_ds2$ntokens)


sqldf("select * from df_chat_val_data_ds2 order by sum_ntokens_3 desc")

# lo mismo para DS3
df_it3_avg_stdev_ntok_ds3 <- sqldf("select df, team, team-547 as 'group', sum(ntokens) as sum_ntokens_3, avg(ntokens) \
                                   as 'avg_tokens_3', stdev(ntokens) \
                                   as 'stdev_tokens_3' from chat_data \
                                   where df = 3 \   
                                   group by team")

df_chat_val_data_ds3 <- inner_join(df_it2_it3_val, df_it3_avg_stdev_ntok_ds3, by.x = c("team", "df"), by.y = c("team", "df"))

# matriz de correlacion para los atributos de discusion del df1; largo medio de comentarios, 
# stdev del largo de comentarios, suma de deltas entre fases 2 y 3
corr_ds3 <- rcorr(as.matrix(df_chat_val_data_ds3[, 
                                                 c('sum_delta_23', 'avg_delta_23', 'sum_ntokens_3',
                                                   'avg_tokens_3', 'stdev_tokens_3')]))
corr_ds3

out<- print(xtable(corr_ds3$r, 
                   caption="Correlation Matrix (DS3)"), 
            "html", include.rownames=FALSE, caption.placement='top',
            html.table.attributes='align="left"')
writeLines(out, "./tables/ds3.html")

out<- print(xtable(corr_ds3$P, 
                   caption="Correlation significance Matrix (DS3)"), 
            "html", include.rownames=FALSE, caption.placement='top',
            html.table.attributes='align="left"')
writeLines(out, "./tables/ds3_p.html")


sqldf("select * from df_it3_avg_stdev_ntok_ds3 order by sum_ntokens_3 desc")

df_indcom_data_ds3 <- sqldf("select * from chat_data where df  = '3'")
hist(df_indcom_data_ds3$ntokens)

ptds1 <- ggplot(df_indcom_data_ds1) + aes(x=`ntokens`) +
  geom_histogram(binwidth = 5, position = "identity", color="darkblue", fill="lightblue") +
  theme_bw() + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20)) + 
  scale_x_continuous(breaks=pretty_breaks(n=6)) + 
  labs(x="Token Count", y="Frequency")

ptds2 <- ggplot(df_indcom_data_ds2) + aes(x=`ntokens`) +
  geom_histogram(binwidth = 5, position = "identity", color="darkgreen", fill="lightgreen") +
  theme_bw() + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20)) + 
  scale_x_continuous(breaks=pretty_breaks(n=6)) + 
  labs(x="Token Count", y="Frequency")

ptds3 <- ggplot(df_indcom_data_ds3) + aes(x=`ntokens`) +
  geom_histogram(binwidth = 5, position = "identity", color="darkred", fill="pink") +
  theme_bw() + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20)) + 
  scale_x_continuous(breaks=pretty_breaks(n=6)) + 
  labs(x="Token Count", y="Frequency")

grd <- plot_grid(ptds1, ptds2, ptds3, 
                 nrow = 1)

title <- ggdraw() + 
  draw_label("Token count distributions in chat comments per semantic differential scale", size=24)
grd_title <- plot_grid(title, grd, ncol = 1, rel_heights = c(0.1, 1.2))

ggsave(plot = grd_title, file="./plots/distributions.eps", 
       units = "cm", scale = 3, dpi = 300, width=13.0, height = 4)



