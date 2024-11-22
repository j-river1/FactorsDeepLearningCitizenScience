#Juan Camilo Rivera
#1 Febrero 

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
library(ggpmisc)
library(patchwork)
library(cowplot)
library(purrr)

library(viridis)
library(svglite)

rm(list=ls())


# 
# df_ <-  read.csv("data/DataBase_2024-05-06_v40.csv")
# colnames(df_)
# 
# # 
# # df_ <- df_ %>% mutate(Estimation_YOLOv8_ = ifelse(errorYOLOv8Resta >0,"Underestimation","Overestimation"))
# # 
# # write.csv(df_, "data/DataBase_2024-05-06_v40.csv", row.names = F)
# 
# 
# # df_ <- df_ %>% mutate(Estimation_ErroDiferreYOLOv8 = ifelse(errorYOLOv8Resta >0,"Underestimation","Overestimation"))
# 
# # 
# # df_ %>% group_by(Estimation_ErroDiferreYOLOv8) %>% dplyr::summarize(n = n())
# 
# # df_ <- df_ %>%
# #   mutate(Estimation_V2 = ifelse(DiferenceHumanVisualRealcherries < 0,  'Overestimation', 'Underestimation'))
# # 
# # 
# # write.csv(df_, 'data/DataBase_2024-04-09_v35.csv', row.names = F)
# # 
# 
# 
# 
# df_$Make <- factor(df_$Make)
# df_$fotografo_anon <- factor(df_$fotografo_anon)
# df_$screen <- factor(df_$screen)
# df_$ImageWidth <- as.numeric(df_$ImageWidth)
# df_$ImageHeight <- as.numeric(df_$ImageHeight)
# df_$Flash <- factor(df_$Flash)
# df_$rama_.y <- factor(df_$rama_.y)
# df_$XResolution <- as.numeric(df_$XResolution)
# df_$yoloV8.x <- as.numeric(df_$yoloV8.x)
# df_$variedad <-factor(df_$variedad)
# df_$Estimation <- factor(df_$Estimation)
# df_$BrandCompany <- df_$Make
# df_$Photographer <- df_$fotografo_anon
# df_$Branch <- df_$rama_.y
# df_$AnglePicture <- as.factor(df_$AnglePicture)
# df_$PortionBranch <- as.factor(df_$PortionBranch)
# df_$ScreenBehind <- as.factor(df_$ScreenBehind)
# df_$Model<- as.factor(df_$Model)
# df_$Adherence_protocol <- as.factor(df_$Adherence_protocol)
# 
# # 
# # df_under <- df_ %>% filter(Estimation_V2 == "Underestimation") 
# df_under <- df_ %>% filter(Estimation_YOLOv8_ == "Underestimation") 
# df_under
# 
# 
# df_under$fotografo_anon <- sub("Person", "", df_under$fotografo_anon)
# df_under$fotografo_anon <- factor(df_under$fotografo_anon)
# df_under$Photographer <- df_under$fotografo_anon
# 
# 
# 
# # 
# # tree_model <- partykit::ctree( errorYOLOv8Resta ~ variedad + ScreenBehind + ImageWidth + ImageHeight +
# #                                  Flash  + PortionBranch + AnglePicture + Model + screen_width+screen_hight + ResolutionFrontCamera_MP +
# #                                  as.factor(XResolution) + as.factor(YResolution) + BrandCompany + Photographer + Adherence_protocol, data = df_)
# # 
# # partykit::varimp(tree_model)
# # 
# # svglite(filename='results/FIG8_underestimation_2024-05-03_v03.svg')
# # plot(tree_model, cex.lab = 0.05, cex.axis = 0.5, cex.main = 2)
# # dev.off()
# 
# 
# 
# 
# # df_under_scaled_ <- cbind(df_[, c("screen", "Flash", "variedad", "rama_pr", "fotografo_anon", "error",
# #                                   "XResolution", "rama_re.x", "Variedad_del_cultivo", "rama_.y", "Make", "yoloV8.x", "ImageWidth", "ImageHeight",
# #                                   "ScreenBehind", "PortionBranch", "AnglePicture", "CantidadCherriesVisual", "Model","Model_v2",
# #                                   "screen_width", "screen_hight", "ResolutionFrontCamera_MP", "DiferenceHumanVisualRealcherries",'rama_re.y', "DimPictureComplete")], df_under)
# # 
# 
# 
# # tree_model <- partykit::ctree( DiferenceHumanVisualRealcherries ~ variedad + ScreenBehind + ImageWidth + ImageHeight +
# #                                  Flash  +  X_GPS_latitude + X_GPS_longitude  + PortionBranch + AnglePicture + Model + screen_width+screen_hight + ResolutionFrontCamera_MP +
# #                                  XResolution + YResolution + BrandCompany + Photographer, data = df_under)
# # 
# # 
# 
# tree_model <- partykit::ctree( errorYOLOv8Resta ~ variedad + ScreenBehind + ImageWidth + ImageHeight +
#                                  Flash  + PortionBranch + AnglePicture + Model + screen_width+screen_hight + ResolutionFrontCamera_MP +
#                                  as.factor(XResolution) + as.factor(YResolution) + BrandCompany + Photographer + Adherence_protocol, data = df_under)
# 
# partykit::varimp(tree_model)
# 
# str(df_under$PortionBranch)
# levels(df_under$PortionBranch) <- c("all", "all_others", "partial", "partial_other")
# 
# levels(df_under$Adherence_protocol) <-  c("yes", "no")
# 
# 
# 
# 
# svglite(filename='results/FIG8_underestimation_2024-05-06_v06.svg')
# plot(tree_model, cex.lab = 0.02, cex.axis = 0.5, cex.main = 2, gp =gpar(fontsize = 9))
# dev.off()
# 
# 
# df_under$ErrorYOLOv8 <- as.numeric(df_under$ErrorYOLOv8 )
# 
# df_under$ErrorYOLOv8[is.na(df_under$ErrorYOLOv8)] <- 0 
# 
# 
# df_under <- df_under %>% filter(ErrorYOLOv8 >0)
# 
# # 
# # df_under <- df_under[is.na(df_under$ErrorYOLOv8), ]
# 
# 
# # df_under <- na.omit(df_under)
# dim(df_under)
# 
# 
# # 
# # str(df_under$PortionBranch)
# # levels(df_under$PortionBranch) <- c("all", "all_others", "partial", "partial_others")
# # 
# # 
# # tree_model <- partykit::ctree( ErrorYOLOv8 ~ variedad + ScreenBehind + ImageWidth + ImageHeight +
# #                                  Flash  + PortionBranch + AnglePicture + Model + screen_width+screen_hight + ResolutionFrontCamera_MP +
# #                                  as.factor(XResolution) + as.factor(YResolution) + BrandCompany + Photographer, data = df_under)
# # 
# # 
# # partykit::varimp(tree_model)
# # # 
# # table(predict(tree_model), df_under$DiferenceHumanVisualRealcherries)
# 
# 
# # svglite(filename='results/FIG8_underestimation_2024-04-16_v01.svg')
# # plot(tree_model, cex.lab = 0.05, cex.axis = 0.5, cex.main = 2)
# # dev.off()
# # 
# # df_under$Photographer <- df_over$fotografo_anon
# 
# 
# # df_under_scaled_ <- cbind(df_[, c("screen", "Flash", "variedad", "rama_pr", "fotografo_anon", "error",
# #                                   "XResolution", "rama_re.x", "Variedad_del_cultivo", "rama_.y", "Make", "yoloV8.x", "ImageWidth", "ImageHeight", 
# #                                   "ScreenBehind", "PortionBranch", "AnglePicture", "CantidadCherriesVisual", "Model","Model_v2", 
# #                                   "screen_width", "screen_hight", "ResolutionFrontCamera_MP", "DiferenceHumanVisualRealcherries",'rama_re.y', "DimPictureComplete")], df_under_scaled)
# # 
# 
# rm(list=ls())
# 
# df_ <-  read.csv("data/DataBase_2024-05-06_v40.csv")
# colnames(df_)
# 
# 
# # df_ <- df_ %>% mutate(Estimation_YOLOv8 = ifelse(ErrorYOLOv8 >0,"Underestimation","Overestimation"))
# # 
# # write.csv(df_, "data/DataBase_2024-04-16_v38.csv", row.names = F)
# 
# 
# df_ %>% group_by(Estimation_V2) %>% dplyr::summarize(n = n())
# 
# # df_ <- df_ %>%
# #   mutate(Estimation_V2 = ifelse(DiferenceHumanVisualRealcherries < 0,  'Overestimation', 'Underestimation'))
# # 
# # 
# # write.csv(df_, 'data/DataBase_2024-04-09_v35.csv', row.names = F)
# # 
# 
# 
# 
# df_$Make <- factor(df_$Make)
# df_$fotografo_anon <- factor(df_$fotografo_anon)
# df_$screen <- factor(df_$screen)
# df_$ImageWidth <- as.numeric(df_$ImageWidth)
# df_$ImageHeight <- as.numeric(df_$ImageHeight)
# df_$Flash <- factor(df_$Flash)
# df_$rama_.y <- factor(df_$rama_.y)
# df_$XResolution <- as.numeric(df_$XResolution)
# df_$yoloV8.x <- as.numeric(df_$yoloV8.x)
# df_$variedad <-factor(df_$variedad)
# df_$Estimation <- factor(df_$Estimation)
# df_$BrandCompany <- df_$Make
# df_$Photographer <- df_$fotografo_anon
# df_$Branch <- df_$rama_.y
# df_$AnglePicture <- as.factor(df_$AnglePicture)
# df_$PortionBranch <- as.factor(df_$PortionBranch)
# df_$ScreenBehind <- as.factor(df_$ScreenBehind)
# df_$Model<- as.factor(df_$Model)
# df_$Adherence_protocol <- as.factor(df_$Adherence_protocol)
# 
# 
# df_over <- df_ %>% filter(Estimation_YOLOv8 == "Overestimation") 
# df_over
# df_over$fotografo_anon <- sub("Person", "", df_over$fotografo_anon)
# df_over$fotografo_anon <- factor(df_over$fotografo_anon)
# df_over$Photographer <- df_over$fotografo_anon
# 
# 
# 
# # tree_model <- partykit::ctree( DiferenceHumanVisualRealcherries ~ variedad + ScreenBehind + ImageWidth + ImageHeight +
# #                                  Flash  +  X_GPS_latitude + X_GPS_longitude  + PortionBranch + AnglePicture + Model + screen_width+screen_hight + ResolutionFrontCamera_MP +
# #                                  XResolution + YResolution + BrandCompany + Photographer, data = df_over)
# 
# 
# 
# df_over$ErrorYOLOv8 <- as.numeric(df_over$ErrorYOLOv8 )
# 
# 
# 
# 
# df_over <- df_over[!is.na(df_over$ErrorYOLOv8), ]
# dim(df_over)
# # 
# # tree_model <- partykit::ctree( ErrorYOLOv8 ~ variedad + ScreenBehind + ImageWidth + ImageHeight +
# #                                  Flash  + PortionBranch + AnglePicture + Model + screen_width+screen_hight + ResolutionFrontCamera_MP +
# #                                  as.factor(XResolution) + as.factor(YResolution) + BrandCompany + Photographer, data = df_over)
# 
# 
# tree_model <- partykit::ctree( errorYOLOv8Resta ~ variedad + ScreenBehind + ImageWidth + ImageHeight +
#                                  Flash  + PortionBranch + AnglePicture + Model + screen_width+screen_hight + ResolutionFrontCamera_MP +
#                                  as.factor(XResolution) + as.factor(YResolution) + BrandCompany + Photographer + Adherence_protocol, data = df_over)
# 
# partykit::varimp(tree_model)
# 
# str(df_under$PortionBranch)
# levels(df_over$PortionBranch) <- c("all", "all_others", "partial", "partial_other")
# 
# levels(df_over$Adherence_protocol) <-  c("yes", "no")
# 
# 
# 
# svglite(filename='results/FIG8_overestimation_2024-05-06_v03.svg')
# plot(tree_model, cex.lab = 1, cex.axis = 0.5, cex.main = 2, gp =gpar(fontsize = 9))
# dev.off()
# 


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
library(ggpmisc)
library(patchwork)
library(cowplot)
library(purrr)

library(viridis)
library(svglite)

rm(list=ls())


df_ <-  read.csv("data/DataBase_2024-05-16_v49.csv")
colnames(df_)

# 
# lmm <- lmerTest::lmer((as.numeric(errorYOLOv8Resta )) ~  Variedad_ + BehindBranch_ + LocationBranch_ + Instructions_ + 
#                         XResolution + AnglePicture_ + YResolution + Flash_  + (1|Photographer) + ScreenWidth + ScreenHight+ ResolutionFrontCamerainMP_ + Features_, data = df_)
# 


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



tree_model <- partykit::ctree( as.numeric(errorYOLOv8Resta) ~ BehindBranch_  + Flash_  + 
                                 AnglePicture_ +  Features_ + LocationBranch_ + ImageWidth +
                                 ImageHeight + XResolution + YResolution + ScreenWidth + ScreenHight +
                                 ResolutionFrontCamerainMP_ + longitude + latitude + Instructions_ + 
                                 Instructions_ + Photographer, data = df_)

partykit::varimp(tree_model)

svglite(filename='results/FIG8_ALLCtree_2024-07-07_v07.svg')
plot(tree_model, cex.lab = 1, cex.axis = 0.5, cex.main = 2, gp =gpar(fontsize = 9))
dev.off()









