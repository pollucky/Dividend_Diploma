library(Ecdat)
library(tidyverse)
library(remotes)
#remotes::install_github("R-CoderDotCom/ggbernie")
library(ggbernie)
library(gganimate)
library(viridis)
library(dplyr)

#таким образом, в выборке есть устойчивые незрелые компании на рассматриваемом промежутке времени
#устойчиво незрелые компании: 1, 2, 6, 7, 15, 22, 23, 27, 28, 
#устойчиво зрелые компании: 5, 8, 13, 14, 16, 18, 20, 24, 
#спорные: 4, 9, 12, 19, 21, 26, 29, 30

#возьмем по каждой компании: незрелые - 1, зрелые - 5, спорные - 4

data_animate <- DataSet2 %>% transmute(Number_Year, Number_Company, Dividends, DIV)
data_animate$Result <- ifelse(data_animate$Number_Company %in% c(1,2, 6, 7, 15, 22, 23, 27, 28), "Sustainable NonMature", ifelse(data_animate$Number_Company %in% c(5, 8, 13, 14, 16, 18, 20, 24), "Sustainable Mature", "Controversial"))

View(data_animate1)

data_animate1 <- data_animate %>% group_by(Number_Year, Result) %>% transmute(Number_Year, median_DIV = median(DIV), median_Dividends = median(Dividends), Result)

# Plot
Plot1 <- ggplot(data_animate1, aes(x = Number_Year,
                y = median_Dividends,
                group = Result,
                color = Result)) +
  geom_line(size = 2) +
  ggtitle("Динамика медианных значений совокупных дивидендных выплат по типам компаний") +
  theme_bw()  + xlab("Год") + labs("") + theme(legend.position = "bottom") + 
  ylab("Совокупные дивидендные выплаты") + scale_color_manual(labels = c("Спорные", "Устойчиво зрелые", "Устойчиво незрелые"), values=c("deeppink", "darkviolet", "darkslategray3")) +
  transition_reveal(Number_Year) 
library(gifski)
library(png)    
m <- animate(Plot1, duration = 5, fps = 20, width = 1000, height = 1000, renderer = gifski_renderer())
anim_save("median_Dividends_animate.gif",animation = m)

Plot2 <- ggplot(data_animate1, aes(x = Number_Year,
                                   y = median_DIV,
                                   group = Result,
                                   color = Result)) +
  geom_line(size = 2) +
  ggtitle("Динамика медианных значений дивиденда на одну акцию по типам компаний") +
  theme_bw()  + xlab("Год") + labs("") + theme(legend.position = "bottom") + 
  ylab("Дивиденд на одну акцию") + scale_color_manual(labels = c("Спорные", "Устойчиво зрелые", "Устойчиво незрелые"), values=c("deeppink", "darkviolet", "darkslategray3")) +
  transition_reveal(Number_Year) 
library(gifski)
library(png)    
m1 <- animate(Plot2, duration = 5, fps = 20, width = 1000, height = 1000, renderer = gifski_renderer())
anim_save("median_DIV_animate.gif",animation = m1)
