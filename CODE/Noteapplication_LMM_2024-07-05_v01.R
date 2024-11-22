

library(dplyr)
library(plyr)
library(ggplot2)
library(viridis)
library(lme4)
library(lmerTest)
library(partykit)
library(EnvStats)
library(performance)
library(DHARMa)
library(influence.ME)
library(sandwich)
library(ggResidpanel)
library(car)
library(patchwork) 
library(Matrix)
library(tidyr)
library(e1071)
library(writexl)
library(effectsize)
library(ggpmisc)
library(patchwork)
library(cowplot)
library(purrr)
library(emmeans)

library(viridis)

rm(list=ls())

#Juan Camilo Rivera
#Esta toda la infor en el archivo Modelos 2024-05-03
df_ <-  read.csv("data/DataBase_2024-05-16_v49.csv")
colnames(df_)

df_$BehindBranch_ <- as.factor(df_$BehindBranch_)
df_$Flash_ <- as.factor(df_$Flash_)
df_$AnglePicture_ <- as.factor(df_$AnglePicture_)
df_$Features_ <- as.factor(df_$Features_)
df_$LocationBranch_ <- as.factor(df_$LocationBranch_)
df_$ImageWidth <- as.numeric(df_$ImageWidth)
df_$ImageHeight <- as.numeric(df_$ImageHeight)
df_$XResolution <- as.numeric(df_$XResolution)
df_$YResolution <- as.numeric(df_$YResolution)
df_$ScreenWidth <- as.numeric(df_$ScreenWidth)
df_$ScreenHight <- as.numeric(df_$ScreenHight)
df_$ResolutionFrontCamerainMP_ <- as.numeric(df_$ResolutionFrontCamerainMP)
df_$longitude <- as.numeric(df_$longitude )
df_$latitude <- as.numeric(df_$latitude )
df_$Instructions_ <- as.factor(df_$Instructions_)
df_$Variedad_ <- as.factor(df_$Variedad_)
df_$Photographer  <- sub("Person", "", df_$Photographer )
df_$Photographer <- factor(df_$Photographer)





lmm <- lmerTest::lmer((as.numeric(errorYOLOv8Resta )) ~  Variedad_ + BehindBranch_ + LocationBranch_ + Instructions_ + 
                        XResolution + longitude + latitude + ImageWidth +  AnglePicture_ + YResolution + Flash_  + (1|Photographer) + ScreenWidth + ScreenHight+ ResolutionFrontCamerainMP_ + Features_, data = df_)

summary(lmm)

anova(lmm)

# Effectsize 08 Nov 2024. Uno de los comentarios del reviewers. 
# Calculate and print effect sizes
effect_sizes <- effectsize::eta_squared(lmm, partial = TRUE)
effect_sizes_rounded <- effect_sizes
effect_sizes_rounded[ , sapply(effect_sizes_rounded, is.numeric)] <- round(effect_sizes[ , sapply(effect_sizes, is.numeric)], 2)
print(effect_sizes_rounded)


r2(lmm)
plot(lmm)
AIC(lmm)
BIC(lmm)
logLik(lmm)


coefficients_table <- summary(lmm)$coefficients
coefficients_table
write.csv(coefficients_table, "results/resultados_fixedeffects_2024-07-07_v12.csv")

random_effects_table <- summary(lmm)$varcor
# write.csv(random_effects_table, "results/resultados_randomeffects_2024-07-07_v10.csv")
# 

random_effects <- ranef(lmm)
random_effects
write.csv(random_effects, "results/resultados_randomeffects_2024-07-07_v10.csv")


# Effectsize 08 Nov 2024. Uno de los comentarios del reviewers. 
# Por cada nivel 


emmeans_variedad <- emmeans(lmm, ~ Variedad_)
contrast_variedad <- contrast(emmeans_variedad, method = "pairwise")
summary(contrast_variedad)


effect_size_variedad <- eff_size(contrast_variedad, sigma = sigma(lmm), edf = df.residual(lmm))
summary(effect_size_variedad)




