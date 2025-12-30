
#survival 
library(dplyr)
df <- read.csv('data.csv')
df$sa[df$sa==0]<-NA
cleaned_df <- df %>%
  filter(!is.na(sa))
initial_counts <- cleaned_df %>%
  filter(day == 0) %>%
  group_by(temperature) %>%
  summarize(initial_count = n())
daily_counts <- cleaned_df %>%
  group_by(temperature, day) %>%
  summarize(daily_count = n())
merged_data <- left_join(daily_counts, initial_counts, by = "temperature")
merged_data <- merged_data %>%
  mutate(survival_rate = daily_count / initial_count)
output_file <- "merged_data.csv"
write.csv(merged_data, file = output_file, row.names = FALSE)
cat("Merged data exported to", output_file, "\n")
df <- read.csv('merged_data.csv')
df$initial_count<-df$initial_count
df$daily_count<-df$daily_count
merged_data <- merged_data %>%
  mutate(survival_rate = daily_count / initial_count)
output_file <- "merged_data.csv"
write.csv(merged_data, file = output_file, row.names = FALSE)

library(scales)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
data <- read.csv('merged_data.csv')
data$temperature <- as.factor(data$temperature)
g<-ggplot(data, aes(x = day, y = survival_rate,  shape = temperature)) +
  geom_line(aes (group=temperature),color= "black",position = position_dodge(1),size=1.5) +
  geom_point(position = position_dodge(1),size=5) +
  scale_y_continuous(breaks=seq(0,1, 0.1),limits = c(0,1) )+
  scale_x_continuous(breaks=seq(0,25, 5),limits = c(-.4,22) )+
  theme_classic() +
  theme(
    panel.grid.major = element_line(colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    axis.text = element_text(size = 25, colour = "black"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 40, r = 40, b = 40, l = 40, unit = 'pt'))+
  theme(
    axis.title.x = element_text(size = 20, vjust = -1.5),
    axis.title.y = element_text(size = 20, vjust = 2.5))+
  theme(axis.line.x=element_line(linetype = 1,colour = "black",size=1,lineend = 1))+
  theme(axis.line.y=element_line(linetype = 1,colour = "black",size=1,lineend = 1))
g
ggsave("survival.png", g, width = 10, height = 5, dpi = 1500)








#growth
library(ggplot2)
library(dplyr)
data <- read.csv('data.csv')
data$sa[data$sa==0]<-NA
data <- na.omit(data)
data$temperature <- as.numeric(data$temperature)
data$growth <- as.numeric(data$growth)
data$fv.fm <- as.numeric(data$fv.fm)
summary_data <- data %>%
  group_by(day, temperature) %>%
  summarise(
    mean_growth = mean(growth),
    sd_growth = sd(growth) )
custom_labels <- c(
  "4.8" = "4.8°C",
  "9.1" = "9.1°C",
  "11.7" = "11.7°C",
  "16" = "16.0°C",
  "20" = "20.0°C",
  "22.8" = "22.8°C"
)
p <- ggplot(summary_data, aes(x = day, y = mean_growth)) +
  geom_line(aes(group = temperature), color="black") +
  geom_errorbar(aes(ymin = mean_growth - sd_growth, ymax = mean_growth + sd_growth), width = 0.2, color="black") +
  geom_point(size = 3, color="black") +
  facet_wrap(~ temperature, scales = "fixed",labeller = labeller(temperature = custom_labels)) + 
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed",size=.5) +
  theme_bw() +
  labs(
    x = "Day",
    y = expression("Growth rate (cm"^2*" day"^-1*")")
  ) +
  theme(legend.position = "none")+
  theme(
    legend.position = "top",
    axis.title = element_text(size = 20),      
    axis.text = element_text(size = 18,colour="black"),
    strip.text = element_text(size = 18)  
  )
print(p)
ggsave("growth.png", p, width = 9, height = 6, dpi = 1500)










#box plot growth
library(ggplot2)
library(ggpubr) 
library(rstatix) 
library(tidyr)
library(segmented)
data <- read.csv('data.csv')
data$sa[data$sa==0]<-NA
data <- na.omit(data) 
data$temperature <- as.factor(data$temperature)
data$n <- as.numeric(data$n)
data$n <- as.numeric(data$n*0.1)
data$p <- as.numeric(data$p)
data$p <- as.numeric(data$p*0.1)
data$growth <- as.numeric(data$growth)
a <- ggplot(data, aes(x = temperature,y = growth,group=temperature,shape = factor(temperature))) +
  geom_boxplot(outlier.shape = 16,colour="black") +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed",size=.5)+
  theme_classic()+
  labs(
    x = "Temperature treatment (°C)",
    y = expression("Growth rate (cm"^2*" day"^-1*")")
  )+
  theme(
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.key.width = unit(55, "pt"))+
  theme(legend.position = "none")+
  theme(
    legend.position = "top",
    axis.title = element_text(size = 20),     
    axis.text = element_text(size = 18,colour="black"),
    strip.text = element_text(size = 18)   
  )
print(a)
ggsave("boxGR.png", a, width = 7, height = 4.5, dpi = 1500) 







#Fv/Fm
library(ggplot2)
library(dplyr)
data <- read.csv('data.csv')
data$sa[data$sa==0]<-NA
data <- na.omit(data)  # 去除所有含NA的行
data$temperature <- as.numeric(data$temperature)
data$growth <- as.numeric(data$growth)
data$fv.fm <- as.numeric(data$fv.fm)
# Calculate the mean and standard error for each day and temperature
summary_data <- data %>%
  group_by(day, temperature) %>%
  summarise(
    mean_fv.fm = mean(fv.fm),
    sd_fv.fm = sd(fv.fm) )
custom_labels <- c(
  "4.8" = "4.8°C",
  "9.1" = "9.1°C",
  "11.7" = "11.7°C",
  "16" = "16.0°C",
  "20" = "20.0°C",
  "22.8" = "22.8°C"
)
p <- ggplot(summary_data, aes(x = day, y = mean_fv.fm)) +
  geom_line(aes(group = temperature), color="black") +
  geom_errorbar(aes(ymin = mean_fv.fm - sd_fv.fm, ymax = mean_fv.fm + sd_fv.fm), width = 0.2, color="black") +
  geom_point(size = 3, color="black") +
  facet_wrap(~ temperature, scales = "fixed",labeller = labeller(temperature = custom_labels)) + 
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed",size=.5) +
  theme_bw() +
  labs(
    x = "Day",
    y = "Fv/Fm"
  ) +
  theme(legend.position = "none")+
  theme(
    legend.position = "top",
    axis.title = element_text(size = 20),      
    axis.text = element_text(size = 18,colour="black"),
    strip.text = element_text(size = 18)  
  )

print(p)
ggsave("fvfm.png", p, width = 9, height = 6, dpi = 1500) 











#np uptake rate
library(ggplot2)
library(dplyr)
data <- read.csv('data.csv')
data$sa[data$sa==0]<-NA
data <- na.omit(data)  
data$temperature <- as.numeric(data$temperature)
data$n <- as.numeric(data$n)
data$n <- as.numeric(data$n*0.1)
data$growth <- as.numeric(data$growth)
data$p <- as.numeric(data$p)
data$p <- as.numeric(data$p*0.1)
summary_data_n <- data %>%
  group_by(day, temperature) %>%
  summarise(
    mean_n = mean(n),
    se_n = sd(n)
  )
summary_data_p <- data %>%
  group_by(day, temperature) %>%
  summarise(
    mean_p = mean(p),
    se_p = sd(p) 
  )
custom_labels <- c(
  "4.8" = "4.8°C",
  "9.1" = "9.1°C",
  "11.7" = "11.7°C",
  "16" = "16.0°C",
  "20" = "20.0°C",
  "22.8" = "22.8°C"
)
y_limits_n <- range(c(summary_data_n$mean_n - summary_data_n$se_n, summary_data_n$mean_n + summary_data_n$se_n), na.rm = TRUE)
y_limits_p <- range(c(summary_data_p$mean_p - summary_data_p$se_p, summary_data_p$mean_p + summary_data_p$se_p), na.rm = TRUE)
scaling_factor <- max(y_limits_n) / max(y_limits_p)
summary_data_p <- summary_data_p %>%
  mutate(mean_p_scaled = mean_p * scaling_factor,
         se_p_scaled = se_p * scaling_factor)
p <- ggplot() +
  geom_line(data = summary_data_n, aes(x = day+0.2, y = mean_n, group = temperature, linetype = "N"), 
            color = "black") +
  geom_errorbar(data = summary_data_n, aes(x = day+0.2, ymin = mean_n - se_n, ymax = mean_n + se_n, group = temperature), 
                width = 0.2, color = "black") +
  geom_point(data = summary_data_n, aes(x = day+0.2, y = mean_n, shape = "N"), 
             size = 3, color = "black") +
  geom_line(data = summary_data_p, aes(x = day-0.2, y = mean_p_scaled, group = temperature, linetype = "P"), 
            color = "black") +
  geom_errorbar(data = summary_data_p, aes(x = day-0.2, ymin = mean_p_scaled - se_p_scaled, ymax = mean_p_scaled + se_p_scaled, group = temperature), 
                width = 0.2, color = "black") +
  geom_point(data = summary_data_p, aes(x = day-0.2, y = mean_p_scaled, shape = "P"), size = 3, 
             color = "black") +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed",size=.3) +
  facet_wrap(~ temperature, scales = "fixed",labeller = labeller(temperature = custom_labels)) +
  scale_y_continuous(
    name = expression("NO"[3]^"  -" * " uptake rate ("*mu*"mol cm"^-2*" day"^-1*")"),
    limits = y_limits_n,
    sec.axis = sec_axis(~ . / scaling_factor, name = expression("PO"[4]^"  3-" * " uptake rate ("*mu*"mol cm"^-2*" day"^-1*")"), labels = scales::comma)
  ) +
  scale_shape_manual(name = "Nutrients", values = c("N" = 16, "P" = 17)) +
  scale_linetype_manual(name = "Nutrients", values = c("N" = "solid", "P" = "dashed")) +
  theme_bw() +
  labs(
    x = "Day"
  ) +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 20),      
    axis.text = element_text(size = 18, colour = "black"),
    strip.text = element_text(size = 18)  
  )
print(p)
ggsave("np.png", p, width = 9, height = 6, dpi = 1500)










#box for np
library(ggplot2)
library(ggpubr) 
library(rstatix) 
library(tidyr)
library(segmented)
# 读取数据
data <- read.csv('data.csv')
data$sa[data$sa==0]<-NA
data <- na.omit(data)  
data$temperature <- as.factor(data$temperature)
data$n <- as.numeric(data$n)
data$n <- as.numeric(data$n*0.1)
data$p <- as.numeric(data$p)
data$p <- as.numeric(data$p*0.1)
data$growth <- as.numeric(data$growth)
a <- ggplot(data, aes(x = temperature,y = p,group=temperature,shape = factor(temperature))) +
  geom_boxplot(outlier.shape = 16,colour="black") +  
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed",size=.5)+
  theme_classic()+
  labs(
    x = "Temperature treatment (°C)",
    y = expression("PO"[4]^"  3-" * " uptake rate ("*mu*"mol cm"^-2*" day"^-1*")")
  )+
  theme(
    legend.position = "right",
    axis.title = element_text(size = 20),   
    axis.text = element_text(size = 18,colour="black"),
    strip.text = element_text(size = 18)
  )
print(a)
ggsave("boxnp.png", a, width = 9, height = 5, dpi = 1500) 












#box plot for np
library(ggplot2)
library(ggpubr)
library(rstatix) 
library(tidyr)
library(segmented)
data <- read.csv('data.csv')
data$sa[data$sa==0]<-NA
data <- na.omit(data) 
data$temperature <- as.factor(data$temperature)
data$n <- as.numeric(data$n)
data$n <- as.numeric(data$n*0.1)
data$p <- as.numeric(data$p)
data$p <- as.numeric(data$p*0.1)
data$growth <- as.numeric(data$growth)
a <- ggplot(data, aes(x = temperature,y = n,group=temperature,shape = factor(temperature))) +
  geom_boxplot(outlier.shape = 16,colour="black") +  
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed",size=.5)+
  theme_classic()+
  labs(
    x = "Temperature treatment (°C)",
    y = expression("NO"[3]^"  -" * " uptake rate ("*mu*"mol cm"^-2*" day"^-1*")")
  )+
  theme(
    legend.position = "right",
    axis.title = element_text(size = 20),     
    axis.text = element_text(size = 18,colour="black"),
    strip.text = element_text(size = 18) 
  )
print(a)
ggsave("boxn.png", a, width = 9, height = 5, dpi = 1500)  

















#relative plot
library(ggplot2)
library(dplyr)

data <- read.csv('data.csv')
data <- na.omit(data) 
data$temperature <- as.numeric(data$temperature)
data$n <- as.numeric(data$nt)
data$p <- as.numeric(data$pt)
data$growth <- as.numeric(data$growth)
x_range_n <- range(data$n, na.rm = TRUE)
x_range_p <- range(data$p, na.rm = TRUE)
y_range <- range(data$growth, na.rm = TRUE)
get_p_value <- function(df) {
  model <- lm(growth ~ n, data = df)
  summary(model)$coefficients[2, 4] 
}
summary(model)
get_lm_stats <- function(df) {
  model <- lm(growth ~ p, data = df)
  summary_model <- summary(model)
  r_squared <- summary_model$r.squared
  f_value <- summary_model$fstatistic[1]  
  p_value <- summary_model$coefficients[2, 4]  
  df1 <- summary_model$fstatistic[2]  
  df2 <- summary_model$fstatistic[3]  
  return(tibble(R2 = r_squared, F_value = f_value, df1 = df1, df2 = df2, p_value = p_value))
}
lm_results <- data %>%
  group_by(temperature) %>%
  summarise(get_lm_stats(cur_data()), .groups = "drop")
print(lm_results)
p_values_n <- data %>%
  group_by(temperature) %>%
  summarise(p_value = get_p_value(cur_data()))
plot_absolutegrowthrate_n_temp <- ggplot(data, aes(x = n, y = growth)) +
  geom_point(shape = 16, color = "black", size = 3) +
  geom_smooth(method = "lm", linetype = "solid", color = "black", se = FALSE) +  
  facet_wrap(~ temperature, scales = "fixed") +  
  scale_x_continuous(limits = x_range_n) + 
  scale_y_continuous(limits = y_range) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") + 
  theme_bw() +
  labs(
    x = "N Concentration",
    y = " Growth",
    title = "Relationship between  Growth and N Concentration"
  ) +
  theme(
    axis.title = element_text(size = 20),    
    axis.text = element_text(size = 18, colour = "black"),
    strip.text = element_text(size = 18),      
    plot.title = element_text(size = 22, face = "bold")
  ) +
  geom_text(data = p_values_n, aes(x = Inf, y = Inf, label = paste("p =", round(p_value, 3))),
            hjust = 1.1, vjust = 1.1, inherit.aes = FALSE, size = 4)
get_p_value <- function(df) {
  model <- lm(growth ~ p, data = df)
  summary(model)$coefficients[2, 4] 
}
p_values_p <- data %>%
  group_by(temperature) %>%
  summarise(p_value = get_p_value(cur_data()))
plot_absolutegrowthrate_p_temp <- ggplot(data, aes(x = p, y = growth)) +
  geom_point(shape = 17, color = "black", size = 3) +
  #geom_smooth(method = "lm", linetype = "solid", color = "black", se = FALSE) +
  facet_wrap(~ temperature, scales = "fixed") +  
  scale_x_continuous(limits = x_range_p) +  
  scale_y_continuous(limits = y_range) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") + 
  theme_bw() +
  labs(
    x = "P Concentration",
    y = "growthrate"
  ) +
  theme(
    axis.title = element_text(size = 20),      
    axis.text = element_text(size = 18, colour = "black"),
    strip.text = element_text(size = 18),     
    plot.title = element_text(size = 22, face = "bold")
  ) +
  geom_text(data = p_values_p, aes(x = Inf, y = Inf, label = paste("p =", round(p_value, 3))),
            hjust = 1.1, vjust = 1.1, inherit.aes = FALSE, size = 4)

print(plot_absolutegrowthrate_n_temp)
print(plot_absolutegrowthrate_p_temp)









library(lme4)
library(MuMIn)
library(lmerTest) 
library(emmeans)   
library(dplyr)
library(sjPlot)
library(flextable)
library(officer)
library(sjPlot)
df_raw <- read.csv("data.csv")
df_raw <- df_raw %>%
  filter(!is.na(sa))
df_raw$fv.fm <- as.numeric(df_raw$fv.fm)
df_raw$growth<- as.numeric(df_raw$growth)
df_raw$n <- as.numeric(df_raw$n)
df_raw$p <- as.numeric(df_raw$p)
df_bottle <- aggregate(p ~ flask + temperature + day, data = df_raw, FUN = mean)
df_bottle$flask       <- factor(df_bottle$flask)
df_bottle$temperature <- factor(df_bottle$temperature)
df_bottle$day         <- factor(df_bottle$day, ordered = TRUE)
mod <- lmer(p ~ temperature * day + (1 | flask), data = df_bottle)
summary(mod)  
anova(mod)    
aov_tab <- anova(mod) 
R2 <- r.squaredGLMM(mod)
AIC_val <- AIC(mod)
print(R2)
print(AIC_val)
tab_df <- as.data.frame(aov_tab)
tab_df$Parameter <- rownames(tab_df)
print(colnames(tab_df))
names(tab_df)[names(tab_df) == "F value"] <- "F_value"
colnames(tab_df) <- sub("F\\.value", "F_value", colnames(tab_df))
colnames(tab_df) <- sub("Pr\\(>F\\)", "p_value", colnames(tab_df))
colnames(tab_df) <- sub("^NumDF$", "numDF", colnames(tab_df))
colnames(tab_df) <- sub("^DenDF$", "denDF", colnames(tab_df))
out <- tab_df[, c("Parameter", "numDF", "denDF", "F_value", "p_value")]
fmt_p <- function(p) {
  ifelse(p < 0.001, formatC(p, format = "e", digits = 2),
         formatC(p, format = "f", digits = 3))
}
out$`F-value` <- round(out$F_value, 2)
out$`p-value` <- fmt_p(out$p_value)
out$Signif <- cut(tab_df$p_value,
                  breaks = c(-Inf, .001, .01, .05, Inf),
                  labels = c("***","**","*",""))
out$F_value <- NULL; out$p_value <- NULL
 ft <- flextable(out)
 ft <- autofit(ft)
 doc <- read_docx()
 doc <- body_add_par(doc, "Table S1. Linear mixed-effects model (ANOVA results).", style = "Normal")
 doc <- body_add_flextable(doc, ft)
 print(doc, target = "~/Desktop/LMM_anova_compactpt.docx")
 emm_T <- emmeans(mod, ~ temperature)
 tukey_T <- pairs(emm_T, adjust = "tukey")
 tukey_df <- as.data.frame(tukey_T)
 ci_df <- as.data.frame(confint(tukey_T))
 final_df <- tukey_df %>%
   left_join(ci_df[, c("contrast", "lower.CL", "upper.CL")], by = "contrast")
 final_df
 write.csv(final_df, "~/Desktop/LMM_tukey_results pt.csv", row.names = FALSE)
 res <- resid(mod)
 acf(res)
 acf_res <- acf(res, plot = FALSE)
 acf_res$acf[2]
 
 
 
 
 
 
 
 
 
 
 