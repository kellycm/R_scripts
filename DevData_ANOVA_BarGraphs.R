library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(rstatix)

dev.data <- read.csv("~/Desktop/DevData_OverlapAnalysis_forR_TemporalSelfvAGOthers.csv", header = TRUE, colClasses = c("factor", "numeric", "factor", "factor","numeric"))
summary(dev.data)

# Homotopicity Descriptive statistics
self.data <- dev.data %>%
  filter(Comparison==2)
summary(self.data)

self.data %>%
  group_by(AgeGroup) %>%
  get_summary_stats(Dice, Age, show = c("n","mean","sd","min","max","median"))
summary(self.data)

# One way ANOVA for Homotopicity
one.way <- aov(Dice ~ AgeGroup, data = self.data)
summary(one.way)

#https://www.r-bloggers.com/2017/06/add-p-values-and-significance-levels-to-ggplots/
#https://epirhandbook.com/en/ggplot-basics.html#ggplot-basics

# Plot bar graph of the homotopicity condition
homotop.data <- self.data
h <- ggplot(homotop.data,aes(x=AgeGroup,y=Dice,fill=AgeGroup)) +
  geom_bar(stat="summary", fun=mean) +
  geom_point(colour="black",fill="white",size=1.2) +
  geom_point(aes(colour=AgeGroup),size=0.8) +
  scale_y_continuous(limits=c(0,0.6),minor_breaks = 0.2) +
  labs(
    y="Dice Coefficient Mean Across Levels",
    title="Temporal ROI"
  ) +
  theme_light()+
  theme(
    plot.title = element_text(size = 14, color="black"),
    plot.title.position="plot",
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, color="black"),
    axis.title.y = element_text(size = 14, color="black"),
    axis.title.x = element_blank()
  ) +
  # frontal hline
 # geom_hline(yintercept=0.1809,colour="white",linetype="dashed")
 # temporal hline
  geom_hline(yintercept=0.1488,colour="white",linetype="dashed")
h

ggsave("~/Desktop/Thesis/Manuscripts/Weak Shadow Paper (Developmental Overlap Analysis)/Figures/Rplots/homotop_temporal.jpg")

# Others' RH Data Descriptive statistics
others.data <- dev.data %>%
  filter(Comparison!=1)
summary(others.data)

others.data %>%
  group_by(AgeGroup,Comparison) %>%
  get_summary_stats(Dice, Age, show = c("n","mean","sd","min","max","median"))

# Two way mixed ANOVA for Dice with Self vs. age-group others' RHs
two.way.rm.aov <- anova_test(
  data = others.data, dv = Dice, wid = SubID,
  between = AgeGroup, within = Comparison
)
get_anova_table(two.way.rm.aov)

# Plot bar graph of interleaved self and others conditions
o <- ggplot(others.data,aes(x=interaction(Comparison,AgeGroup),y=Dice,fill=interaction(Comparison,AgeGroup))) +
  geom_bar(stat="summary", fun=mean) +
  geom_line(aes(group=SubID),colour="light grey") +
  geom_point(colour="black",fill="white",size=1.2) +
  geom_point(aes(colour=interaction(Comparison,AgeGroup)),size=0.8) +
  scale_y_continuous(limits=c(0,0.6),minor_breaks = 0.2) +
  labs(
    y="Dice Coefficient Mean Across Levels",
    title="Temporal ROI"
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 14, color="black"),
    plot.title.position="plot",
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, color="black"),
    axis.title.y = element_text(size = 14, color="black"),
    axis.title.x = element_blank()
  ) +
  scale_fill_manual(
    values=c("#F8766D","#8C8C8C","#7CAE00","#8C8C8C","#00BFC4","#8C8C8C","#C77CFF","#8C8C8C")) +
  scale_color_manual(
    values=c("#F8766D","#8C8C8C","#7CAE00","#8C8C8C","#00BFC4","#8C8C8C","#C77CFF","#8C8C8C"))
o

ggsave("~/Desktop/Thesis/Manuscripts/Weak Shadow Paper (Developmental Overlap Analysis)/Figures/Rplots/selfothers_temporal.jpg")

