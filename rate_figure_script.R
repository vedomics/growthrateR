
library("plyr")
library("dplyr")
library("reshape2")
library("gridExtra")
library("ggplot2")
library("ggrepel")
library("scales")
library("ggsci")
library("ggpubr")
library("stringr")

theme_set(theme_bw())


standard_error <- function(x) sd(x, na.rm = T) / sqrt(length(x))
'%!in%' = function(x,y)!('%in%'(x,y))

clinical_pals = c("48" ="#0073C2FF", "59" = "#CD534CFF", "61" = "#775F81" , "186" ="9CD08F")



all = read.csv("all_strain_GC.csv")

all_rates = read.csv("all_strain_rates.csv")


all_rates$Strain = sapply(all_rates$Strain, function(x) {substr(x, 2, nchar(x))})
all_rates$Strain = gsub("\\..*", "", all_rates$Strain)

all_rates$Strain = factor(all_rates$Strain, levels = c("48RI3", "59AI7", "45RD10", "45CD2", "48RD1", "48RI2", "061N5", "186RI6"))
all_rates$R_h = all_rates$R_est * 60

all_ratzes_sum = all_rates %>%
  group_by(Strain)%>%
  mutate(Rate_Avg_m = mean(R_est, na.rm = T)) %>%
  mutate(Rate_Avg_h = mean(R_h, na.rm = T)) 
# mutate(Rate_sem = standard_error(Rate))


ggplot(all_rates_sum, aes(x = Strain, y = R_h))+
  geom_point(alpha = 0.5)+
  geom_point(aes(y= Rate_Avg_h), shape = "-", size = 9, color = "red")+
  scale_x_discrete(labels = c("48RI3 \n Capsule Positive \n(Pt12)", "59AI7 Q199* \n Capsule Negative \n(Pt12)", "45RD10 \n Capsule Positive \n(Pt12)", "45CD2 Q191* \n Capsule Negative \n(Pt12)", "48RD1 V381E \n Altered CapD \n(Pt12)", "48RI2 Y596N \n Altered CapD \n(Pt12)", "061N5 \n Capsule Positive \n(Pt09)", "186RI6 N595S \n Altered CapD \n(Pt09)"))+
  geom_signif(comparisons = list(c("48RI3", "59AI7"), c("45RD10", "45CD2"), c("061N5", "186RI6"), c("48RI3", "48RD1"), c("48RI3", "48RI2")), step_increase = 0.2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(y = expression("Estimated Growth Rate Doublings"~hr^-1), x = "")
