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


# df_ <- read.csv("data/DataBase_2024-01-30_v21.csv")
# 
# df_ <- read.csv("data/DataBase_2024-04-05_v28.csv")
# 
# 
# df_$AbsoluteDiference <- abs((df_$rama_re.x - df_$CantidadCherriesVisual)/df_$rama_re.x)
# 
# 
# write.csv(df_, "data/DataBase_2024-04-05_v29.csv", row.names= F)
# 
# 
# summary(df_$AbsoluteDiference) 

df_ <- read.csv("data/DataBase_2024-04-05_v29.csv")
dim(df_)

q_95 <- quantile(df_$AbsoluteDiference, 0.95)
q_5 <- quantile(df_$AbsoluteDiference, 0.05)

# Select rows where values are outside the 95th and 5th percentiles
subset_df <- df_[df_$AbsoluteDiference < q_5 | df_$AbsoluteDiference > q_95, ]

dim(subset_df)
head(subset_df)

write.csv(subset_df, "data/Outliers_2024-04-05.csv")


df_$Estimation <- as.factor(df_$Estimation)

summary(as.factor(df_$fotografo))

total <- df_

get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

total <- total %>% filter (!is.na(yoloV8.x))

total <- total %>% group_by(variedad) %>% mutate(density= get_density(rama_re.x,yoloV8.x, n = 700))

total$fotografo_ <- as.factor(total$fotografo_)


max_value <- max(c(max(total$yoloV8.x), max(total$rama_re.x)))
unique_photographers <- unique(total$fotografo)
anonymous_names <- paste0("_", seq_along(unique_photographers))
dictionary <- data.frame(Photographer = unique_photographers, Anonymous_Photographer = anonymous_names)
dim(total)
total<- total %>% left_join(dictionary, by = c('fotografo' = 'Photographer'))
dim(total)

total$Anonymous_Photographer <- factor(total$Anonymous_Photographer, levels = unique(total$Anonymous_Photographer))


total$error_human_ <- ifelse(total$AbsoluteDiference < 0.5 ,"Mobile pictures following protocols and \n without typographical errors","Others")



plot_frame <- data.frame(error_human = c(total$error_human_, rep("Original data set of mobile pictures", nrow(total))),
                         yoloV8.x = c(total$yoloV8.x,total$yoloV8.x),
                         rama_re.x= c(total$rama_re.x, total$rama_re.x))



plot_frame <- plot_frame %>% filter (error_human != c("Others"))


plot_frame <- plot_frame %>% mutate(density= get_density(rama_re.x,yoloV8.x, n = 700))



plot <- ggplot(total, aes(x = yoloV8.x, y = rama_re.x)) +
  geom_point(alpha = 0.5,aes(color = density)) +
  scale_color_viridis() +
  # Add points with transparency       # Add 2D density contours
  labs(y = "Total number of coffee cherries manually \n counted on the branch", 
       x = "Total number of coffee cherries detected per picture \n by YOLO v8") +  # Customize labels
  facet_wrap(~Anonymous_Photographer) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 7) # Adjust the size of legend text
  ) +
  coord_fixed() +
  lims(x = c(0, max_value), y = c(0, max_value)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black")+
  stat_smooth(method = "lm", se = FALSE) +
  stat_poly_eq(label.y = "bottom", label.x = "right")# Adjust scales as needed


ggsave("results/FIG5_R2Enumerator_2024-05-29_v07.svg", plot = plot, width = 15, height = 10, dpi = 300)




plot <- ggplot(total, aes(x = yoloV8.x, y = rama_re.x)) +
  geom_point(alpha = 0.5,aes(color = density)) +
  scale_color_viridis() +
  # Add points with transparency       # Add 2D density contours
  labs(y = "Manual counting cherries\nmeasurement [the total of cherries]", 
       x = "Mobile pictures measurement\n [the total of cherries]") +  # Customize labels
  facet_wrap(~ScreenBehind) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 7) # Adjust the size of legend text
  ) +
  coord_fixed() +
  lims(x = c(0, max_value), y = c(0, max_value)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black")+
  stat_smooth(method = "lm", se = FALSE) +
  stat_poly_eq(label.y = "bottom", label.x = "right")# Adjust scales as needed



ggsave("results/ScreenBehind_2024-04-05_v01.svg", plot = plot, width = 15, height = 10, dpi = 300)




plot <- ggplot(total, aes(x = yoloV8.x, y = rama_re.x)) +
  geom_point(alpha = 0.5,aes(color = density)) +
  scale_color_viridis() +
  # Add points with transparency       # Add 2D density contours
  labs(y = "Manual counting cherries\nmeasurement [the total of cherries]", 
       x = "Mobile pictures measurement\n [the total of cherries]") +  # Customize labels
  facet_wrap(~AnglePicture) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 7) # Adjust the size of legend text
  ) +
  coord_fixed() +
  lims(x = c(0, max_value), y = c(0, max_value)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black")+
  stat_smooth(method = "lm", se = FALSE) +
  stat_poly_eq(label.y = "bottom", label.x = "right")#Adjust scales as needed


ggsave("results/AngleTakePicture_2024-04-05_v01.svg", plot = plot, width = 15, height = 10, dpi = 300)





plot <- ggplot(total, aes(x = yoloV8.x, y = rama_re.x)) +
  geom_point(alpha = 0.5,aes(color = density)) +
  scale_color_viridis() +
  # Add points with transparency       # Add 2D density contours
  labs(y = "Manual counting cherries\nmeasurement [the total of cherries]", 
       x = "Mobile pictures measurement\n [the total of cherries]") +  # Customize labels
  facet_wrap(~PortionBranch) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 7) # Adjust the size of legend text
  ) +
  coord_fixed() +
  lims(x = c(0, max_value), y = c(0, max_value)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black")+
  stat_smooth(method = "lm", se = FALSE) +
  stat_poly_eq(label.y = "bottom", label.x = "right")#Adjust scales as needed



ggsave("results/PortionBranch_2024-04-05_v01.svg", plot = plot, width = 15, height = 10, dpi = 300)



plot



