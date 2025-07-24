library(ggplot2)
library(dplyr)
library(reshape2)
library(foreign)
library(Hmisc)
library(lme4)
library(ggeffects)
library(DHARMa)
load('Roadkill_Data.RData')
Road.kill.data <- Roadkill.data
#################
glmm <- glmer.nb(RoadKill_s ~ Protected + roadTypeA2 +LCmax_8 + water_Dist +Pop_dist +TreeCover + DEM+Slope+ (1 | RoadID2), 
                 data = Road.kill.data)
options(width = 2000)
summary(glmm)
full_coef <- as.data.frame(coef(summary(glmm)))
simulationOutput <- simulateResiduals(fittedModel = glmm, n = 1000, plot = TRUE)
testDispersion(simulationOutput)
library(sjPlot)
######### interaction
p <- plot_model(glmm, type="pred", terms=c("roadTypeA2", "Protected"),
                axis.title="Predicted roadkill count (n/10 km)", 
                title="Interaction Effects in NB-GLMM",dodge = 0.5) +
  scale_color_manual(values=c("red", "blue"))+
  theme_bw()+theme(axis.text = element_text(color = "black"))+
  xlab('Road type')
p$data
ggsave(filename = 'Interaction.pdf',p,width = 6,height = 3.1)
##########
pred_road <- ggpredict(glmm, terms = "LCmax_8")
pred_road2 <- pred_road[-1,]
pred_road2 <- transform(pred_road2,LC = c('Forests','Shrublands','Savannas','Grasslands',
                                          'Wetlands','Croplands','Barren'))
p <- ggplot(pred_road2, aes(x = reorder(LC,-predicted), y = predicted, fill = x)) +
  geom_col(width = 0.7) + geom_point(size = 0.8)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2,size = 0.35) +
  labs(x = "", y = "Predicted roadkill count") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() + ggtitle("Across ecosystem types")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        axis.text = element_text(colour = "black"))+
  scale_fill_manual(values = c('#086a10','#c6b044','#fbff13','#b6ff05','#27ff8b','#c24f44','#f9ffa4'))+
  scale_y_continuous(breaks = seq(0,12.5,2.5))
p
ggsave(filename = 'Ecosystem_Roadkill.pdf',p,width = 4,height = 3.5)
################
