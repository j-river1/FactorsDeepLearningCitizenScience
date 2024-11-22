#Juan Camilo Rivera
#24 Oct 2021

library(tidyr)
library(dplyr)
library(e1071)
library(writexl)
library(ggplot2)
library(ggpmisc)
library(patchwork)
library(cowplot)
library(purrr)
library(ggplot2)
library(viridis)


rm(list=ls())



df_ <- read.csv("data/DataBase_2024-05-16_v49.csv")
colnames(df_)


p <- ggplot(df_, aes(x=errorYOLOv8Resta)) + geom_histogram( ) + theme_bw() + xlab("e(j)") +
  ylab("Frequency")





ggsave("results/Histogram_2024-11-21_v01.svg", plot = p, width = 8, height = 8, dpi = 300)







