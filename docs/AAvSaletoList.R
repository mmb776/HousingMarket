library(HousingMarket)
library(tidyverse)
library(paletteer)
library(viridis)
library(paletteer)
HousingMarket$Month.of.Period.End <- ym(HousingMarket$Month.of.Period.End)


AAInteraction <-lm(Average.Sale.To.List ~`PopPct_BLACK OR AFRICAN AMERICAN ALONE`*Region, data = HousingMarket)
#summary(lm2)
#plot(lm2)
anova(AAInteraction)

HousingMarket <- HousingMarket %>%
  dplyr::select( -matches('fit'), -matches('lwr'), -matches('upr') ) %>%
  cbind( predict(AAInteraction, newdata=., interval='confidence') )

ggplot(HousingMarket , aes(x = `PopPct_BLACK OR AFRICAN AMERICAN ALONE` , y = Average.Sale.To.List , color = Region)) +
 
  geom_point(shape = 1,size = 2, alpha = 0.4) +
  geom_line( aes(y=fit),alpha = 1,linewidth = 1.5 ) +
  scale_color_viridis_d(option = "C",begin =0.1, end = 0.9)+
  labs(
    title = "Housing sale Discrepancy",
    x = "Percent of Afican American ",
    y = "Average Close Price to List Price Ratio",
    color = "Region"
  ) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", size = .5) +
  #scale_x_date(date_labels = " %Y", date_breaks = "1 year")+
  theme_classic()+
  theme(plot.title = element_text(size = 15,hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text( size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = "bold"),
        legend.position = c(0.8, 0.8),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

