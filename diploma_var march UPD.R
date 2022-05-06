library(readxl)
DowJonesMarch <- read_excel("DowJones MARCH.xlsx")
names(DowJonesMarch)
library(Ecdat)
library(tidyverse)
library(ggstatsplot)
library(ggside)
library(lares)
library(remotes)
library(ggbernie)
library(rpart)
library(gganimate)
library(viridis)
library(dplyr)
library(tidyr)
library(corrplot)
library(gifski)
library(png) 
library("writexl")
library(dplyr)
library(forcats)
library(tidyquant)
library(lmtest)
library(corrr)
library(klaR)
library(lime)
library(pdp)
library(gbm)
library(MASS)
library(ggplot2)
library(car)
library(plotly)
library(factoextra)
library(gridExtra)
library("viridis")  
library(tseries)
library(RColorBrewer)
library(wesanderson)
library(PerformanceAnalytics)
library(forecast)
library(PortfolioAnalytics)
library(quantmod)
library(kohonen)
library(sandwich)
library(pscl)
library(ggcorrplot)
library(stringi) 
library(stargazer)
library(gapminder)
library(shiny)
library(dynlm)
library(dLagM)
library(qgraph)
library(FactoMineR)
library(flexclust)
library(gganimate)
library(devtools)
library(ggridges)
library(hrbrthemes)
library(caret)
library(AUC)
library(InformationValue)
library(klaR)
library(pROC)
library(randomForest)
library(randomForestExplainer)
library(RColorBrewer)
library(ggsci)
library(dismo)


#ограничим выборку по годам
y <- as.numeric(10:21)
DataSet <- dplyr::filter(DowJonesMarch, DowJonesMarch$Number_Year %in% y)
summary(DataSet)

#удалим компании AXP UN Equity (3), DOW UN Equity (10), GS UN Equity (11), JPM UN Equity (17), TRV UN Equity (25)
co <- as.numeric(c(1,2,4,5,6,7,8,9,12,13,14,15,16,18,19,20,21,22,23,24,26,27,28,29,30))
DataSet1 <- dplyr::filter(DataSet, DataSet$Number_Company %in% co)

#удалим переменные с большим количеством NA 
#список переменных #1 - IS_RESEARCH_AND_DEVELOPMENT, SG&A_AS_%_TOTAL_SALES, SHS_HLD_BY_CEO_AS_%_OF_OUTSTDG, BS_TOT_LOAN, IS_EXPORT_SALES, TOTAL_NUMBER_OF_SHIPMENTS, SHIPMENT_VOLUME, PCT_INDEPENDENT_DIRECTORS_SCORE, BOARD_REFRESHMENT_SCORE
#список переменных #2 - TOT_ANALYST_REC, BS_TOT_VAL_OF_SHARES_REPURCHASED, CUR_MKT_CAP, CB_RTG, 
names(DataSet)
DataSet2 <-  DataSet1[, -which(colMeans(!is.na(DataSet1)) != 1)]

colnames(DataSet2) <- c("Year", "ID Company", "Number_Year", "Number_Company", "Foundation_Year", 
                        "ROE", "Sales", "Net_Income", "DIV", "EQY_sh_out", "BS_LT_Borrow", "BS_Total_Assets", 
                        "ROCE", "RE_period", "Total_Equity", "ROA", "MB_ratio", "Cash", "RE_cum", 
                        "Tangible_Assets", "Tax_Rate", "Intangible_Assets", "Cash_Ratio", "Quick_Ratio", 
                        "ARtoSales", "Accounts_Payable", "EBITDA", "CF_DA", "BS_ST_Borrow", "BS_Cur_Liab", 
                        "CF_FCFF", "FCFE", "AGR", "WACC", "Beta", "PayoutRatio", "T12M_PayoutRatio", "AssetsToEquity", 
                        "Negative_CAPEX", "Risk_Bloomberg", "PE_Ratio", "Board_Size", "CAPEX", "Working_Capital", 
                        "Z_score", "EBIT", "BS_PPE", "Q_Ratio", "T12M_Tax_Rate", "T12M_Operating_Income", "CF_FCF",
                        "BS_Total_Liabilities", "BS_ST_Investments", "IS_Operating_Income", "Total_Common_Equity", "BS_Privelege_Equity", 
                        "Independent_Directors", "Shares_Directors", "RE")


#работаем с DataSet2

#проведем предварительное исследование по плательщикам
names(DataSet2)
corr4 <- round(cor(DataSet2[,c(7,8,9,20,27,43,46)]), 3)
ggcorrplot(corr4, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("#c4a123", "#fea7b0", "#c61c65"), 
           title="Коррелограмма ключевых показателей", 
           ggtheme=theme_bw)

Table14 <- DataSet2 %>% group_by(Number_Year) %>% summarise(Payers = sum(PayoutRatio>0), NonPayers=sum(PayoutRatio==0)) %>% 
                        pivot_longer(cols = c(Payers,NonPayers), names_to = "DividendIdentity", values_to = "Number")
Chart9 <- Table14 %>% ggplot(aes(x=Number_Year, y = Number, fill = DividendIdentity, label = Number)) + geom_col()+ 
  scale_fill_manual(labels = c("Неплательщики", "Плательщики"), values=c("darkred", "darkslateblue")) +
  theme_bw() +
  theme(legend.position = "bottom") + xlab("Год") + ylab("Количество") + labs(fill = "Тип отношения к дивидендам") + 
  geom_text(size = 3, position = position_stack(vjust = 0.5), col = "white")
Chart9

Table100 <- DataSet2 %>% dplyr::filter(DataSet2$PayoutRatio == 0)

#да, выборка несбалансированная. Попробуем всё равно ключевые показатели отразить. 
names(DataSet2)
DataSet2$PayOrNot <- ifelse(DataSet2$DIV > 0, 1, 0)
#медианная структура SALES_REV_TURN - EBITDA - NET_INCOME
names(DataSet2)
DataSet2$PayOrNot_character <- as.character(DataSet2$PayOrNot)
Table15 <- DataSet2 %>% group_by(Number_Year, PayOrNot_character) %>% summarise(Sales = median(Sales), EBITDA = median(EBITDA), Net_Income = median(Net_Income), Capitalization = median(Capitalization)) %>% ungroup

Table15$status <- ifelse(Table15$PayOrNot_character == "0", "NonPayers", "Payers")
Chart10 <- ggplot(Table15, aes(fill=status, y=Sales, x=Number_Year)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() + scale_fill_manual(labels = c("Неплательщики", "Плательщики"), values=c("#b4c5cc", "#182325")) + theme(legend.position = "right") + ylab("Выручка") + xlab("Год") + labs(fill = "Тип отношения к дивидендам")
Table15
Chart11 <- ggplot(Table15, aes(fill=status, y=EBITDA, x=Number_Year)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() + scale_fill_manual(labels = c("Неплательщики", "Плательщики"), values=c("#b4c5cc", "#182325")) + theme(legend.position = "right") + ylab("EBITDA") + xlab("Год") + labs(fill = "Тип отношения к дивидендам") + 
  geom_label(
    label="Из-за Apple", 
    x=11,
    y=17950,
    label.size = 0.35,
    color = "black",
    fill="#b4c5cc"
  )
#очень как-то странно, что в 2011 году у NonPayers было больше

Chart12 <- ggplot(Table15, aes(fill=status, y=Net_Income, x=Number_Year)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() + scale_fill_manual(labels = c("Неплательщики", "Плательщики"), values=c("#b4c5cc", "#182325")) + theme(legend.position = "right") + ylab("Чистая прибыль") + xlab("Год") + labs(fill = "Тип отношения к дивидендам")

grid.arrange(Chart10, Chart11, Chart12, nrow = 3)

DataSet2$Capitalization
names(Table15)
Table15
Chart59 <- ggplot(Table15, aes(fill=status, y=Capitalization, x=Number_Year)) + 
  geom_bar(position="dodge", stat="identity", color = "black") + theme_bw() + scale_fill_manual(labels = c("Неплательщики", "Плательщики"), values=c("#63719C", "#008AA4")) + theme(legend.position = "bottom") + ylab("Капитализация") + xlab("Год") + labs(fill = "Тип отношения к дивидендам") + 
  geom_label(
    label="Из-за Apple", 
    x=11,
    y=210000,
    label.size = 0.35,
    color = "white",
    fill="#63719C"
  )
Chart59

#медианная структура баланса BS_DISCLOSED_INTANGIBLES, TANGIBLE_ASSETS, BS_TOT_ASSET
names(DataSet2)

#соотношение Intangibles и Tangibles
Table16 <- DataSet2 %>% group_by(Number_Year, PayOrNot_character) %>% summarise(Intangible_Assets = median(Intangible_Assets), Tangible_Assets = median(Tangible_Assets)) %>% ungroup
Table16$status <- ifelse(Table16$PayOrNot_character == "0", "NonPayers", "Payers")
Chart13 <- ggplot(Table16, aes(fill=status, y=Tangible_Assets, x=Number_Year)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() + scale_fill_manual(values=c("#b4c5cc", "#182325")) + theme(legend.position = "right")

Table17 <- Table16 %>% pivot_longer(Intangible_Assets:Tangible_Assets, names_to = "type_of_assets", values_to = "valuation")
Chart60 <- ggplot(Table17,aes(x=status,y=valuation,fill=type_of_assets)) + theme_bw() + 
  geom_bar(stat="identity",position="stack") +
  facet_grid(~Number_Year) + theme(legend.position = "right", axis.text.x = element_text(angle=90, hjust=1), strip.background =element_rect(fill="#af969c")) + 
  scale_fill_manual(labels = c("Нематериальные активы", "Материальные активы"), values=c("#2a1c35", "#8f7aa1")) + xlab("") + ylab("Общая величина материальных и 
  нематериальных активов") + labs(fill = "Тип активов") + scale_y_continuous(labels = scales::comma) 

Chart61 <- ggplot(Table17,aes(x=status,y=valuation,fill=type_of_assets)) + theme_bw() + 
  geom_bar(stat="identity",position="fill") +
  facet_grid(~Number_Year) + theme(legend.position = "right", axis.text.x = element_text(angle=90, hjust=1), strip.background =element_rect(fill="#af969c")) + 
  scale_fill_manual(labels = c("Нематериальные активы", "Материальные активы"), values=c("#2a1c35", "#8f7aa1")) + xlab("") + ylab("Процентное соотношение 
  материальных и нематериальных 
  активов") + labs(fill = "Тип активов") 

grid.arrange(Chart60, Chart61)

#соотношение краткосрочных и долгосрочных заимствований
names(DataSet2)
Table18 <- DataSet2 %>% group_by(Number_Year, PayOrNot_character) %>% summarise(BS_ST_Borrow = median(BS_ST_Borrow), BS_LT_Borrow = median(BS_LT_Borrow)) %>% ungroup

Table18$status <- ifelse(Table18$PayOrNot_character == "0", "NonPayers", "Payers")
Table19 <- Table18 %>% pivot_longer(BS_ST_Borrow:BS_LT_Borrow, names_to = "type_of_borrowings", values_to = "valuation")
Chart62 <- ggplot(Table19,aes(x=status,y=valuation,fill=type_of_borrowings)) + theme_bw() + 
  geom_bar(stat="identity",position="stack") +
  facet_grid(~Number_Year) + theme(legend.position = "right", axis.text.x = element_text(angle=90, hjust=1), 
                                   strip.background =element_rect(fill="#182932"), 
                                   strip.text = element_text(colour = 'white')) + 
  scale_fill_manual(labels = c("LR заимствования", "SR заимствования"), values=c("#367d56", "#182932")) +xlab("")+ylab("Абсолютное соотношение 
  SR и LR заимствований") + 
  labs(fill = "Тип заимствований")

Chart63 <- ggplot(Table19,aes(x=status,y=valuation,fill=type_of_borrowings)) + theme_bw() + 
  geom_bar(stat="identity",position="fill") +
  facet_grid(~Number_Year) + theme(legend.position = "right", axis.text.x = element_text(angle=90, hjust=1), 
                                   strip.background =element_rect(fill="#182932"), 
                                   strip.text = element_text(colour = 'white')) + 
  scale_fill_manual(labels = c("LR заимствования", "SR заимствования"), values=c("#367d56", "#182932")) +xlab("")+ylab("% cоотношение 
  SR и LR заимствований") + 
  labs(fill = "Тип заимствований")

grid.arrange(Chart62,Chart63)

#сравним CASH_RATIO, QUICK_RATIO и BS_CASH_NEAR_CASH_ITEM + WORKING_CAPITAL
Table20 <- DataSet2 %>% group_by(Number_Year, PayOrNot_character) %>% transmute(Cash_Ratio = median(Cash_Ratio), Quick_Ratio = median(Quick_Ratio), Cash = median(Cash), Working_Capital = median(Working_Capital))
Table20$status <- ifelse(Table20$PayOrNot_character == "0", "NonPayers", "Payers")
Chart14 <- ggplot(data = Table20, aes(x = Number_Year, y = Cash_Ratio, group = status, color = status)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Неплательщики", "Плательщики"), values=c("#b71a3b", "#019697")) + theme(legend.position="bottom") + xlab("Год") +ylab("Коэффициент абсолютной 
ликвидности") + labs(fill = "Тип отношения к дивидендам")
Chart15 <- ggplot(data = Table20, aes(x = Number_Year, y = Quick_Ratio, group = status, color = status)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Неплательщики", "Плательщики"), values=c("#b71a3b", "#019697")) + theme(legend.position="bottom") + xlab("Год") + ylab("Коэффициент срочной 
ликвидности")+ labs(fill = "Тип отношения к дивидендам")
Chart16 <- ggplot(data = Table20, aes(x = Number_Year, y = Cash, group = status, color = status)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Неплательщики", "Плательщики"), values=c("#b71a3b", "#019697")) + theme(legend.position="bottom") + xlab("Год") + ylab("Денежные средства") + labs(fill = "Тип отношения к дивидендам")
Chart17 <- ggplot(data = Table20, aes(x = Number_Year, y = Working_Capital, group = status, color = status)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Неплательщики", "Плательщики"), values=c("#b71a3b", "#019697")) + theme(legend.position="bottom") + xlab("Год") + ylab("Оборотный капитал") + labs(fill = "Тип отношения к дивидендам")
grid.arrange(Chart14, Chart15, Chart16, Chart17, ncol = 2)

#новый график
Chart124
names(DataSet2)
data_Chart124 <- increase_data1 %>% pivot_longer(cols = c("Cash_Ratio", "Quick_Ratio"), values_to = "Ratios", names_to = "Names")

increase_data7 <- increase_data1
increase_data7$Number_Year <- as.character(increase_data7$Number_Year)
increase_data7$delta_Lintner <- as.character(increase_data7$delta_Lintner)
ggplot(na.omit(increase_data7)) + geom_bar(aes(x = Number_Year, y = delta_Lintner, fill = factor(Number_Cluster1), alpha = delta_Lintner), stat = "identity") + 
                                  facet_wrap(~Number_Cluster1)  +
                                  scale_fill_manual(values = c("#e691c7", "#5f73b3"), name = "Number_Cluster1") + 
                                  theme_bw() + theme(legend.position = "bottom")


#Стадия жизненного цикла по DeAngelo & DeAngelo & Stulz, 2006 (коэффициент заработанного капитала)
DataSet2$Stage_Indicator1 <- DataSet2$RE_period / DataSet2$Total_Equity
#Стадия жизненного цикла по Leary & Michaely, 2011 (размер и возраст компании)
DataSet2$Stage_Indicator2 <- ifelse(DataSet2$Foundation_Year > 0, 2022 - DataSet2$Foundation_Year, "-")


#создание датасета для графика
#отсечка зрелых / незрелых компаний по медиане!
DataSet2$Number_Cluster1 <- ifelse(DataSet2$Stage_Indicator1 > median(DataSet2$Stage_Indicator1), 1, 0) #по одному году или сразу все?
DataSet2$Number_Cluster2 <- ifelse(DataSet2$Stage_Indicator2 > median(DataSet2$Stage_Indicator2), 1, 0)
Table1 <- DataSet2 %>% group_by(Number_Year) %>% summarise(NonMature = sum(Number_Cluster1==0), Mature=sum(Number_Cluster1==1)) %>% pivot_longer(cols = c(NonMature,Mature), names_to = "Type", values_to = "Amount")
Chart1 <- Table1 %>% ggplot(aes(x=Number_Year, y = Amount)) + geom_point(aes(colour = Type))

Chart2 <- Table1 %>% ggplot(aes(x=Number_Year, y = Amount, fill = Type, label = Amount)) + geom_col(color = "black")+ 
  scale_fill_manual(labels = c("Зрелые", "Незрелые"), values=c("#441A05", "#FC8E01")) +
  theme_bw() +
  theme(legend.position = "bottom") + xlab("Год") + ylab("Количество") + labs(fill = "Критерий стадии жизненного цикла") + 
  geom_text(size = 3, position = position_stack(vjust = 0.5), col = "white")

Table5$Result
Table42 <- Table5 %>% group_by(Number_Year) %>% summarise(Controversial = sum(Result=="Controversial"), Mature=sum(Result=="Sustainable Mature"), NonMature = sum(Result == "Sustainable NonMature")) %>% pivot_longer(cols = c(Controversial, Mature, NonMature), names_to = "Type", values_to = "Amount")

Chart86 <- Table42 %>% ggplot(aes(x=Number_Year, y = Amount, fill = Type, label = Amount)) + geom_col(color = "black")+ 
  scale_fill_manual(labels = c("Спорные", "Устойчиво зрелые", "Устойчиво незрелые"), values=c("#ccaf9b", "#312921", "#873c1e")) +
  theme_bw() +
  theme(legend.position = "bottom") + xlab("Год") + ylab("Количество") + labs(fill = "Критерий стадии жизненного цикла") + 
  geom_text(size = 3, position = position_stack(vjust = 0.5), col = "white")

#построение компаний по второму критерию (срок функционирования)
Table2 <- DataSet2 %>% group_by(Number_Year) %>% summarise(NonMature = sum(Number_Cluster2==0), Mature=sum(Number_Cluster2==1)) %>% pivot_longer(cols = c(NonMature,Mature), names_to = "Type", values_to = "Amount")
Chart4 <- Table2 %>% ggplot(aes(x=Number_Year, y = Amount, fill = Type)) + geom_bar(stat='identity', col = "gray")+ 
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE, option = "A", begin = 0.3, end = 0.75) +
  theme_minimal() +
  theme(legend.position = "bottom")

names(DataSet2)
corr1 <- cor(DataSet2[,c(62,63,17)])
corrplot(corr1, method="color")
corrplot(corr1, method="number")
corr7 <- cor(DataSet2[,c(62,36)])
corrplot(corr7)
Stage_Indicator_upd <- DataSet2$RE / DataSet2$Total_Equity
corr10 <- cor(DataSet2[,c(61,62)])
cor(Stage_Indicator_upd, DataSet2$MB_ratio)

corr8 <- cor(testing[,c(36,62)])
corrplot(corr8)

#вспомогательные подсчеты
testing <- DataSet2
testing$Stage_Indicator1 <- DataSet2$RE_cum / DataSet2$Total_Equity
names(testing)
corr6 <- cor(testing[,c(62,63,17)])
corrplot(corr6, method="color")
corrplot(corr6, method="number")

#для отслеживания компаний, которые то зрелые, то незрелые
Table3 <- DataSet2%>% transmute(Number_Year = Number_Year, Number_Company, Number_Cluster1) 
Table3$Type <- ifelse(Table3$Number_Cluster1 == 1, "Mature", "NonMature")
for (i in 1:30){
  assign(paste0("TableCycle", i), dplyr::filter(Table3, Number_Company == i))
  assign(paste0("ChartCycle", i), ggplot(data = get(paste("TableCycle", i, sep="")), aes(x = Number_Year, y = Type))+geom_point(col = "darkblue")+theme_bw()+xlab("Год")+ylab("")+ggtitle(paste0("Компания № ", i)))
}

grid.arrange(ChartCycle1, ChartCycle2, ChartCycle4, ChartCycle5, ChartCycle6, ChartCycle7, ChartCycle8, ChartCycle9, ChartCycle12, ChartCycle13, ncol = 5)
grid.arrange(ChartCycle14, ChartCycle15, ChartCycle16, ChartCycle18, ChartCycle19, ChartCycle20, ChartCycle21, ChartCycle22, ChartCycle23, ChartCycle24, ncol = 5)
grid.arrange(ChartCycle26, ChartCycle27, ChartCycle28, ChartCycle29, ChartCycle30, ncol = 5)


#выделим компании, которые "устойчиво" зрелые и "устойчиво" незрелые
Table4 <- Table3 %>% group_by(Number_Company) %>% summarise(Final = sum(Number_Cluster1 == 1))
Table4$Result <- ifelse(Table4$Final == 12, "Sustainable Mature", ifelse(Table4$Final == 0, "Sustainable NonMature", "Controversial"))
Table5 <- merge(Table3, Table4)
table(Table5$Result)

#таким образом, в выборке есть устойчивые незрелые компании на рассматриваемом промежутке времени
#устойчиво незрелые компании: 1, 2, 6, 7, 15, 22, 23, 27, 28, 
#устойчиво зрелые компании: 5, 8, 13, 14, 16, 18, 20, 24, 
#спорные: 4, 9, 12, 19, 21, 26, 29, 30

#делили по критерию на основании медианы, теперь посмотрим на результаты посредством KNN
for (i in 1:12) {
  assign(paste0("Clusters_Data", i), dplyr::filter(DataSet2, Number_Year == i+9))
}

names(Clusters_Data1)
for(i in 1:12){
  model <- kmeans(get(paste("Clusters_Data", i, sep=""))[,c(62,17)], centers = 2, nstart = 10)
  model$cluster <- ifelse(model$cluster == which.min(model$centers[,2]), 1, 2)
  assign(paste0("Clusters_Data", i), cbind(get(paste("Clusters_Data", i, sep="")), model$cluster))
  assign(paste0("p", i),fviz_cluster(model, get(paste("Clusters_Data", i, sep=""))[,c(62,17)], geom = "point", palette = "Set1", ggtheme = theme_gray()))
}
names(Clusters_Data1)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, ncol = 4) #2 - более зрелая компания
Clusters_Data <- rbind(Clusters_Data1, Clusters_Data2, Clusters_Data3, Clusters_Data4, Clusters_Data5, Clusters_Data6, Clusters_Data7, Clusters_Data8, Clusters_Data9, Clusters_Data10, Clusters_Data11, Clusters_Data12)
Clusters_Data$clusters2 <- ifelse(Clusters_Data$`model$cluster`== 2, 1, 0)
#выделим компании, которые "устойчиво" зрелые и "устойчиво" незрелые
Table6 <- Clusters_Data %>% group_by(Number_Company)  %>% transmute(Number_Year = Number_Year, Number_Company = Number_Company, clusters2 = clusters2) %>% summarise(Final = sum(clusters2 == 1))
View(Table6) #есть пересечения

#попробуем сравнить результаты по KNN (Clusters_Data$clusters2) с медианой (Table5$Number_Cluster1)
names(Clusters_Data)
clust <- Clusters_Data[,c(3,4,68)]
Table7 <- merge(Table5, clust)
str(Table7)
#посмотрим на пересечения
View(Table7)
Table7$comparison <- ifelse(Table7$Number_Cluster1==Table7$clusters2, "coincidence", "NOcoincidence")
#посчитаем количество совпадений
table(Table7$comparison) #как-то 131 на 169
#не понятно...

#очень интересно посмотреть как отличаются ключевые показатели по зрелым, незрелым компаниям
#показатели: PayoutRatio, Net_Income, FCFE, CAPEX
#с дублями
names(Clusters_Data)
Table8 <- Clusters_Data %>% group_by(Number_Year, Number_Cluster1) %>% transmute(PayoutRatio = median(PayoutRatio), Net_Income = median(Net_Income), FCFEtoSales = median(FCFE/Sales), CAPEXtoSales = median(CAPEX/Sales), Sales = median(Sales), EBITDA = median(EBITDA), Net_Income = median(Net_Income),Tangible_assets =median(Tangible_Assets), Intangible_Assets=median(Intangible_Assets), 
                                                                                 BS_ST_Borrow=median(BS_ST_Borrow), BS_LT_Borrow = median(BS_LT_Borrow), Cash_Ratio = median(Cash_Ratio), Quick_Ratio = median(Quick_Ratio), Cash = median(Cash), Working_Capital = median(Working_Capital))

#без дублей
Table9 <- unique(Table8)
Table9$Number_Cluster1 <- as.character(Table9$Number_Cluster1)
Chart5 <- ggplot(data = Table9, aes(x = Number_Year, y = PayoutRatio, group = Number_Cluster1, color = Number_Cluster1)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Незрелые", "Зрелые"), values=c("darkslategray3", "darkviolet")) + theme(legend.position="bottom") + xlab("Год") + ylab("Коэффициент 
дивидендных выплат")
Chart6 <- ggplot(data = Table9, aes(x = Number_Year, y = Net_Income, group = Number_Cluster1, color = Number_Cluster1)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Незрелые", "Зрелые"), values=c("darkslategray3", "darkviolet")) + theme(legend.position="bottom") + xlab("Год") + ylab("Чистая прибыль")
Chart7 <- ggplot(data = Table9, aes(x = Number_Year, y = FCFEtoSales, group = Number_Cluster1, color = Number_Cluster1)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Незрелые", "Зрелые"), values=c("darkslategray3", "darkviolet")) + theme(legend.position="bottom") + xlab("Год") + ylab("FCFE/Выручка")
Chart8 <- ggplot(data = Table9, aes(x = Number_Year, y = CAPEXtoSales, group = Number_Cluster1, color = Number_Cluster1)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Незрелые", "Зрелые"), values=c("darkslategray3", "darkviolet")) + theme(legend.position="bottom") + xlab("Год")+ylab("CAPEX/Выручка")
grid.arrange(Chart5, Chart6, Chart7, Chart8, ncol = 2)

Chart86 <- ggplot(data = Table36, aes(x = Number_Year, y = PayoutRatio, group = Result, color = Result)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Спорные", "Устойчиво зрелые", "Устойчиво незрелые"), values=c("deeppink", "darkviolet", "darkslategray3")) + theme(legend.position="bottom") + xlab("Год") + ylab("Коэффициент 
дивидендных выплат")
Chart87 <- ggplot(data = Table36, aes(x = Number_Year, y = Net_Income, group = Result, color = Result)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Спорные", "Устойчиво зрелые", "Устойчиво незрелые"), values=c("deeppink", "darkviolet", "darkslategray3")) + theme(legend.position="bottom") + xlab("Год") + ylab("Чистая прибыль")
Chart88 <- ggplot(data = Table36, aes(x = Number_Year, y = FCFEtoSales, group = Result, color = Result)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Спорные", "Устойчиво зрелые", "Устойчиво незрелые"), values=c("deeppink", "darkviolet", "darkslategray3")) + theme(legend.position="bottom") + xlab("Год") + ylab("FCFE/Выручка")
Chart89 <- ggplot(data = Table36, aes(x = Number_Year, y = CAPEXtoSales, group = Result, color = Result)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Спорные", "Устойчиво зрелые", "Устойчиво незрелые"), values=c("deeppink", "darkviolet", "darkslategray3")) + theme(legend.position="bottom") + xlab("Год") + ylab("CAPEX/Выручка")
grid.arrange(Chart86, Chart87, Chart88, Chart89, ncol = 2)

Chart64 <-  ggplot(Table9, aes(fill=Number_Cluster1, y=Sales, x=Number_Year)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() + scale_fill_manual(labels = c("Незрелые", "Зрелые"), values=c("#b4c5cc", "#182325")) + theme(legend.position = "right") + ylab("Выручка") + xlab("Год") + labs(fill = "Критерий стадии жизненного цикла")

Chart65 <- ggplot(Table9, aes(fill=Number_Cluster1, y=EBITDA, x=Number_Year)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() + scale_fill_manual(labels = c("Незрелые", "Зрелые"), values=c("#b4c5cc", "#182325")) + theme(legend.position = "right") + ylab("EBITDA") + xlab("Год") + labs(fill = "Критерий стадии жизненного цикла")

Chart66 <- ggplot(Table9, aes(fill=Number_Cluster1, y=Net_Income, x=Number_Year)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() + scale_fill_manual(labels = c("Незрелые", "Зрелые"), values=c("#b4c5cc", "#182325")) + theme(legend.position = "right") + ylab("Чистая прибыль") + xlab("Год") + labs(fill = "Критерий стадии жизненного цикла")

grid.arrange(Chart64, Chart65, Chart66)
Table35 <- merge(Clusters_Data, Table13)
#для трех типов: Mature - NonMature - Controversial
Table36 <- Table35 %>% group_by(Number_Year, Result) %>% transmute(PayoutRatio = median(PayoutRatio), Net_Income = median(Net_Income), FCFEtoSales = median(FCFE/Sales), CAPEXtoSales = median(CAPEX/Sales), Sales = median(Sales), EBITDA = median(EBITDA), Net_Income = median(Net_Income),Tangible_assets =median(Tangible_Assets), Intangible_Assets=median(Intangible_Assets), 
                                                                                  BS_ST_Borrow=median(BS_ST_Borrow), BS_LT_Borrow = median(BS_LT_Borrow), Cash_Ratio = median(Cash_Ratio), Quick_Ratio = median(Quick_Ratio), Cash = median(Cash), Working_Capital = median(Working_Capital))
Chart67 <-  ggplot(Table36, aes(fill=Result, y=Sales, x=Number_Year)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() + scale_fill_manual(labels = c("Спорные", "Устойчиво зрелые", "Устойчиво незрелые"), values=c("#f4ece2", "#182325", "#b4c5cc")) + theme(legend.position = "right") + ylab("Выручка") + xlab("Год") + labs(fill = "Критерий стадии жизненного цикла")

Chart68 <- ggplot(Table36, aes(fill=Result, y=EBITDA, x=Number_Year)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() + scale_fill_manual(labels = c("Спорные", "Устойчиво зрелые", "Устойчиво незрелые"), values=c("#f4ece2", "#182325", "#b4c5cc")) + theme(legend.position = "right") + ylab("EBITDA") + xlab("Год") + labs(fill = "Критерий стадии жизненного цикла")

Chart69 <- ggplot(Table36, aes(fill=Result, y=Net_Income, x=Number_Year)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() + scale_fill_manual(labels = c("Спорные", "Устойчиво зрелые", "Устойчиво незрелые"), values=c("#f4ece2", "#182325", "#b4c5cc")) + theme(legend.position = "right") + ylab("Чистая прибыль") + xlab("Год") + labs(fill = "Критерий стадии жизненного цикла")

grid.arrange(Chart67, Chart68, Chart69)

#материальные - нематериальные активы 
names(Table38)

Table38 <- Table35 %>% group_by(Number_Year, Type) %>% transmute(PayoutRatio = median(PayoutRatio), Net_Income = median(Net_Income), FCFEtoSales = median(FCFE/Sales), CAPEXtoSales = median(CAPEX/Sales), Sales = median(Sales), EBITDA = median(EBITDA), Net_Income = median(Net_Income),Tangible_assets =median(Tangible_Assets), Intangible_Assets=median(Intangible_Assets), 
                                                                   BS_ST_Borrow=median(BS_ST_Borrow), BS_LT_Borrow = median(BS_LT_Borrow), Cash_Ratio = median(Cash_Ratio), Quick_Ratio = median(Quick_Ratio), Cash = median(Cash), Working_Capital = median(Working_Capital))

Table39 <- Table38 %>% pivot_longer(Intangible_Assets:Tangible_assets, names_to = "type_of_assets", values_to = "valuation")
Chart72 <- ggplot(Table39,aes(x=Type,y=valuation,fill=type_of_assets)) + theme_bw() +
  geom_bar(stat="identity",position="stack") +
  facet_grid(~Number_Year) + theme(legend.position = "right", axis.text.x = element_text(angle=90, hjust=1), strip.background =element_rect(fill="#af969c")) +
  scale_fill_manual(labels = c("Нематериальные активы", "Материальные активы"), values=c("#2a1c35", "#8f7aa1")) + xlab("") + ylab("Общая величина материальных и
нематериальных активов") + labs(fill = "Тип активов") + scale_y_continuous(labels = scales::comma)

Chart73 <- ggplot(Table39,aes(x=Type,y=valuation,fill=type_of_assets)) + theme_bw() +
  geom_bar(stat="identity",position="stack") +
  facet_grid(~Number_Year) + theme(legend.position = "right", axis.text.x = element_text(angle=90, hjust=1), strip.background =element_rect(fill="#af969c")) +
  scale_fill_manual(labels = c("Нематериальные активы", "Материальные активы"), values=c("#2a1c35", "#8f7aa1")) + xlab("") +  ylab("Процентное соотношение 
  материальных и нематериальных 
  активов") + labs(fill = "Тип активов")

grid.arrange(Chart72, Chart73)

Table37 <- Table36 %>% pivot_longer(Intangible_Assets:Tangible_assets, names_to = "type_of_assets", values_to = "valuation")
Chart70 <- ggplot(Table37,aes(x=Result,y=valuation,fill=type_of_assets)) + theme_bw() +
  geom_bar(stat="identity",position="stack") +
  facet_grid(~Number_Year) + theme(legend.position = "right", axis.text.x = element_text(angle=90, hjust=1), strip.background =element_rect(fill="#af969c")) +
  scale_fill_manual(labels = c("Нематериальные активы", "Материальные активы"), values=c("#2a1c35", "#8f7aa1")) + xlab("") + ylab("Общая величина 
материальных и
нематериальных активов") + labs(fill = "Тип активов") + scale_y_continuous(labels = scales::comma)

Chart71 <- ggplot(Table37,aes(x=Result,y=valuation,fill=type_of_assets)) + theme_bw() +
  geom_bar(stat="identity",position="stack") +
  facet_grid(~Number_Year) + theme(legend.position = "right", axis.text.x = element_text(angle=90, hjust=1), strip.background =element_rect(fill="#af969c")) +
  scale_fill_manual(labels = c("Нематериальные активы", "Материальные активы"), values=c("#2a1c35", "#8f7aa1")) + xlab("") + ylab("Процентное соотношение 
  материальных и нематериальных 
  активов") + labs(fill = "Тип активов") 

grid.arrange(Chart70, Chart71)

#соотношение краткосрочных и долгосрочных заимствований
#Non Mature - Mature
Table40 <- Table38 %>% pivot_longer(BS_ST_Borrow:BS_LT_Borrow, names_to = "type_of_borrowings", values_to = "valuation")
Chart74 <- ggplot(Table40,aes(x=Type,y=valuation,fill=type_of_borrowings)) + theme_bw() + 
  geom_bar(stat="identity",position="stack") +
  facet_grid(~Number_Year) + theme(legend.position = "right", axis.text.x = element_text(angle=90, hjust=1),
                                   strip.background =element_rect(fill="#182932"),
                                   strip.text = element_text(colour = 'white')) + scale_fill_manual(labels = c("LR заимствования", "SR заимствования"), values=c("#367d56", "#182932")) +xlab("")+ylab("Абсолютное соотношение 
  SR и LR заимствований") + 
  labs(fill = "Тип заимствований") + scale_y_continuous(labels = scales::comma)
Chart75 <- ggplot(Table40,aes(x=Type,y=valuation,fill=type_of_borrowings)) + theme_bw() + 
  geom_bar(stat="identity",position="fill") +
  facet_grid(~Number_Year) +  theme(legend.position = "right", axis.text.x = element_text(angle=90, hjust=1),
                                    strip.background =element_rect(fill="#182932"),
                                    strip.text = element_text(colour = 'white')) + scale_fill_manual(labels = c("LR заимствования", "SR заимствования"), values=c("#367d56", "#182932")) +xlab("")+ylab("% cоотношение 
  SR и LR заимствований") + 
  labs(fill = "Тип заимствований")

grid.arrange(Chart74, Chart75)

#Sustainable Mature - Sustainable NonMature - Controversial
Table41 <- Table36 %>% pivot_longer(BS_ST_Borrow:BS_LT_Borrow, names_to = "type_of_borrowings", values_to = "valuation")
Chart76 <- ggplot(Table41,aes(x=Result,y=valuation,fill=type_of_borrowings)) + theme_bw() + 
  geom_bar(stat="identity",position="stack") +
  facet_grid(~Number_Year) + theme(legend.position = "right", axis.text.x = element_text(angle=90, hjust=1),
                                   strip.background =element_rect(fill="#182932"),
                                   strip.text = element_text(colour = 'white')) + scale_fill_manual(labels = c("LR заимствования", "SR заимствования"), values=c("#367d56", "#182932")) +xlab("")+ylab("Абсолютное соотношение 
  SR и LR заимствований") + 
  labs(fill = "Тип заимствований") + scale_y_continuous(labels = scales::comma)
Chart77 <- ggplot(Table41,aes(x=Result,y=valuation,fill=type_of_borrowings)) + theme_bw() + 
  geom_bar(stat="identity",position="fill") +
  facet_grid(~Number_Year) + theme(legend.position = "right", axis.text.x = element_text(angle=90, hjust=1),
                                   strip.background =element_rect(fill="#182932"),
                                   strip.text = element_text(colour = 'white')) + 
  scale_fill_manual(labels = c("LR заимствования", "SR заимствования"), values=c("#367d56", "#182932")) +xlab("")+ylab("% cоотношение 
  SR и LR заимствований") + 
  labs(fill = "Тип заимствований")

grid.arrange(Chart76, Chart77)

#сравним CASH_RATIO, QUICK_RATIO и BS_CASH_NEAR_CASH_ITEM + WORKING_CAPITAL
#Non Mature - Mature
Table9
Chart78 <- ggplot(data = Table9, aes(x = Number_Year, y = Cash_Ratio, group = Number_Cluster1, color = Number_Cluster1)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Незрелые", "Зрелые"), values=c("#b71a3b", "#019697", "dodgerblue4")) + theme(legend.position="bottom") + xlab("Год") +ylab("Коэффициент абсолютной 
ликвидности") + labs(fill = "Критерий стадии жизненного цикла")
Chart79 <- ggplot(data = Table9, aes(x = Number_Year, y = Quick_Ratio, group = Number_Cluster1, color = Number_Cluster1)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Незрелые", "Зрелые"), values=c("#b71a3b", "#019697")) + theme(legend.position="bottom") + xlab("Год") + ylab("Коэффициент срочной 
ликвидности")+ labs(fill = "Критерий стадии жизненного цикла")
Chart80 <- ggplot(data = Table9, aes(x = Number_Year, y = Cash, group = Number_Cluster1, color = Number_Cluster1)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Незрелые", "Зрелые"),  values=c("#b71a3b", "#019697")) + theme(legend.position="bottom") + xlab("Год") + ylab("Денежные средства") + labs(fill = "Критерий стадии жизненного цикла")
Chart81 <- ggplot(data = Table9, aes(x = Number_Year, y = Working_Capital, group = Number_Cluster1, color = Number_Cluster1)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Незрелые", "Зрелые"), values=c("#b71a3b", "#019697")) + theme(legend.position="bottom") + xlab("Год") + ylab("Оборотный капитал") + labs(fill = "Критерий стадии жизненного цикла")
grid.arrange(Chart78, Chart79, Chart80, Chart81, ncol = 2)

Table36
Chart82 <- ggplot(data = Table36, aes(x = Number_Year, y = Cash_Ratio, group = Result, color = Result)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Спорные", "Устойчиво зрелые", "Устойчиво незрелые"), values=c("#b71a3b", "#019697", "darksalmon")) + theme(legend.position="bottom") + xlab("Год") +ylab("Коэффициент абсолютной 
ликвидности") + labs(fill = "Критерий стадии жизненного цикла")
Chart83 <- ggplot(data = Table36, aes(x = Number_Year, y = Quick_Ratio, group = Result, color = Result)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Спорные", "Устойчиво зрелые", "Устойчиво незрелые"), values=c("#b71a3b", "#019697", "darksalmon")) + theme(legend.position="bottom") + xlab("Год") + ylab("Коэффициент срочной 
ликвидности")+ labs(fill = "Критерий стадии жизненного цикла")
Chart84 <- ggplot(data = Table36, aes(x = Number_Year, y = Cash, group = Result, color = Result)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Спорные", "Устойчиво зрелые", "Устойчиво незрелые"), values=c("#b71a3b", "#019697", "darksalmon")) + theme(legend.position="bottom") + xlab("Год") + ylab("Денежные средства") + labs(fill = "Критерий стадии жизненного цикла")
Chart85 <- ggplot(data = Table36, aes(x = Number_Year, y = Working_Capital, group = Result, color = Result)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(labels = c("Спорные", "Устойчиво зрелые", "Устойчиво незрелые"), values=c("#b71a3b", "#019697", "darksalmon")) + theme(legend.position="bottom") + xlab("Год") + ylab("Оборотный капитал") + labs(fill = "Критерий стадии жизненного цикла")
grid.arrange(Chart82, Chart83, Chart84, Chart85, ncol = 2)
#§1 - модель из DeAngelo & DeAngelo & Stulz, 2006
SGR <- DataSet2 %>% group_by(Number_Company) %>% transmute(SGR = (Sales - lag(Sales))/lag(Sales)) %>% ungroup
?lag
lag_Stage_Indicator1 <- DataSet2 %>% group_by(Number_Company) %>% transmute(Number_Year, lag_Stage_Indicator1=dplyr::lag(Stage_Indicator1)) %>% ungroup


DataSet2
DataSet2$Capitalization <- DataSet2$Total_Equity * DataSet2$MB_ratio
Percentile <- DataSet2 %>% group_by(Number_Year) %>% transmute(Number_Company = Number_Company, Percentile = Capitalization / sum(Capitalization)) %>% ungroup
as.data.frame(quantile(Percentile$Percentile, probs = c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95)))

View(Percentile)
B2 <- DataSet2 %>% group_by(Number_Year) %>% summarise(B = sum(Capitalization))
str(DataSet2)

View(Percentile)


DeAngelo <- transmute(DataSet2, 
                        Number_Year = Number_Year, 
                        Number_Company = Number_Company, 
                        Stage_Indicator1 = Stage_Indicator1, 
                        REtoTA = RE_period / BS_Total_Assets , 
                        TEtoTA = Total_Equity / BS_Total_Assets , 
                        ROA = ROA, 
                        SGR = SGR$SGR, #из отдельного дата-сета
                        AGR = AGR, 
                        MB_ratio = MB_ratio, 
                        CashtoTA = Cash / BS_Total_Assets, 
                        DIV = DIV,
                        PayoutRatio = PayoutRatio, 
                        PayOrNot = PayOrNot, 
                        Percentile = Percentile$Percentile,
                        Capitalization = log(Capitalization)
)

DeAngelo5_lagSI <- transmute(DataSet2, 
                                         Number_Year = Number_Year, 
                                         Number_Company = Number_Company, 
                             lag_Stage_Indicator1 = lag_Stage_Indicator1$lag_Stage_Indicator1, 
                                         REtoTA = RE_period / BS_Total_Assets , 
                                         TEtoTA = Total_Equity / BS_Total_Assets , 
                                         ROA = ROA, 
                                         SGR = SGR$SGR, #из отдельного дата-сета
                                         AGR = AGR, 
                                         MB_ratio = MB_ratio, 
                                         CashtoTA = Cash / BS_Total_Assets, 
                                         DIV = DIV,
                                         PayoutRatio = PayoutRatio, 
                                         PayOrNot = PayOrNot, 
                                         Percentile = Percentile$Percentile,
                                         Capitalization = log(Capitalization)
)
View(DataSet2)
#с DIV
model1 <- lm(data = na.omit(DeAngelo), DIV~ . -PayoutRatio-PayOrNot-Number_Year-Number_Company-MB_ratio-Capitalization)
summary(model1)

#с лагом стадии жизненного цикла:
model114 <- lm(data = na.omit(DeAngelo5_lagSI), DIV~ . -PayoutRatio-PayOrNot-Number_Year-Number_Company-MB_ratio-Capitalization)
summary(model114)
bptest(model114) #есть гетероскедастичность

stepAIC(model114)
model115 <- update(model114, DIV ~ Percentile + AGR + SGR + lag_Stage_Indicator1 + REtoTA + TEtoTA)
summary(model115)

stargazer(model114, model115, type = 'html', out = "models_lag_SI.html", 
          df = FALSE, se = list(cse(model114), cse(model115)))
bptest(model115)



#мультиколлинеарность - благодаря этому удалили MBratio
vif(model1)
#нормальность остатков
plot(model1, which = 2, col = "darkblue", fill = "darkblue") #хвосты улетают
e <- resid(model1)
data_e <- data.frame(e)
ggplot(data = data_e, aes(e)) + geom_density(fill = "darkblue", alpha = 0.65)+
  xlab("Остатки") + ylab("Плотность распределения")+theme_bw()+ggtitle("Плотность распределения остатков регрессии (1)") + theme(plot.title = element_text(hjust
                                                                     = 0.5))
e1 <- resid(model2)
data_e1 <- data.frame(e1)

e2 <- resid(model3)
data_e2 <- data.frame(e2)

e3 <- resid(model5)
data_e3 <- data.frame(e3)

data_es <- data.frame(e, e1, e2)
colnames(data_es) <- c("model1", "model2", "model3")

data_es1 <- data_es %>% pivot_longer(cols = model1:model3, 
                                     names_to = "Model", 
                                     values_to = "Residuals")
ggplot(data = data_es1, aes(x = Residuals, fill = Model))  + geom_density()+ facet_grid(~Model) + 
  xlab("Остатки") + ylab("Плотность распределения")+theme_bw()+ggtitle("Плотность распределения остатков регрессии") + theme(strip.background =element_rect(fill="#182932"),
                                                                                                                             strip.text = element_text(colour = 'white'), 
                                                                                                                             legend.position = "bottom") + 
  scale_fill_manual(values = c("#010135", "#01528a", "#0096d3"), label = c("Модель (1)", "Модель (2)", "Модель (3)")) 

resettest(model1) #тест Рамсея не позволяет понять, степень какой переменной была пропущена
boxCox(model1)
bptest(model1)
model01 <- update(model1, log(DIV+1) ~ .) #дивиденды на акцию, лучше брать log, когда весь объем
plot(model1, which = 2, 
     main = "Распределение остатков регрессии",
     xlab = "Theoretical quantilies",
     pch=16, col="darkblue")
#гетероскедастичность 
plot(model1, which = 3, 
     main = "Визуальная диагностика наличия гетероскедастичности в данных",
     pch=16, col="darkblue") #кривая красная линия => Sd не постоянно
bptest(model1) #гетероскедастичность есть
#введем робастные ошибки
V_new <- vcovHC(model1, type = "HC0")
coeftest(model1, V_new) #SGR уже не значима стала
summary(model3)
qt(0.95, df = 275-6) #расчет табличной t-статистики
#посмотрим, какие переменные стоит удалить из model1 
drop1(model1, test = "none")
model2 <- update(model1, .~.-ROA)
summary(model2)
waldtest(model1, model2) #лучше model2
model3 <- update(model2, .~.-AGR)
summary(model3)
waldtest(model2, model3) #лучше model3
model4 <- update(model3, .~.-CashtoTA-Percentile)
summary(model4)
waldtest(model3, model4) #лучше model4
waldtest(model3,model1)

stepAIC(model1)
#Таким образом, финал - model4
summary(model4)
#посмотрим на влиятельные наблюдения с изначальной моделью
crPlots(model1)
influencePlot(model1)
influencePlot(model1, id.method="identify", main="Влиятельные наблюдения и выбросы",
              sub= "Диаметр круга пропорционален расстоянию Кука", col = "darkblue")

plot(cooks.distance(model1),type="h")
N <- which(cooks.distance(model1)>4/300)
N
Nm <- as.numeric(names(N))
model5 <- lm(data = na.omit(DeAngelo)[-Nm,], DIV ~ . -PayoutRatio-PayOrNot-Number_Year-Number_Company-MB_ratio-Capitalization-ROA-AGR)
summary(model5)
#R2 вырос, все знаки у переменных остались теми же 

cse <- function(model) {
  A <- sqrt(diag(vcovHC(model,type = "HC0")))
  return(A)
}

library(lares)
mplot_full(DeAngelo1$DIV, fitted(model3)) + theme_bw()
?mplot_full
#визуализация коэффициентов регрессии
color_model3 <- c("#100f16", "#33385a", "#6780b3", 
                  "#b5c6e8", "#1f4b8e", "#0f1225", 
                  "#496595")
set.seed(123)
theme_set(theme_bw())
V3 <- vcovHC(model3, type = "HC0")
ggcoefstats(model3, 
            title = "Результаты лучшей регрессии по модели жизненного цикла", 
            ylab = "Переменные", 
            xlab = "Оценка", 
            package = "RColorBrewer",
            palette = "Dark2", 
            vcov = V3
            )

dev.off()
Chart99
?plot
stargazer(model1, model2, model3, model4, model5, type = "html", df = FALSE, se = list(cse(model1), cse(model2), cse(model3), cse(model4), cse(model5)), out = "table7.html", title="Results DeAngelo")
bptest(model1)
bptest(model2)
bptest(model3)
bptest(model5)

#создание датасета с лагами
lag_Stage_Indicator1 <- DataSet2 %>% group_by(Number_Company) %>% transmute(Number_Year, lag_Stage_Indicator1=dplyr::lag(Stage_Indicator1)) %>% ungroup
lag_REtoTA <- DeAngelo %>% group_by(Number_Company) %>% transmute(Number_Year, lag_REtoTA=dplyr::lag(REtoTA)) %>% ungroup
lag_TEtoTA <- DeAngelo %>% group_by(Number_Company) %>% transmute(Number_Year, lag_TEtoTA=dplyr::lag(TEtoTA)) %>% ungroup
lag_ROA <- DataSet2 %>% group_by(Number_Company) %>% transmute(Number_Year, lag_ROA=dplyr::lag(ROA)) %>% ungroup
lag_SGR <- DeAngelo %>% group_by(Number_Company) %>% transmute(Number_Year, lag_SGR=dplyr::lag(SGR)) %>% ungroup
lag_AGR <- DataSet2 %>% group_by(Number_Company) %>% transmute(Number_Year, lag_AGR=dplyr::lag(AGR)) %>% ungroup
lag_CashtoTA <- DeAngelo %>% group_by(Number_Company) %>% transmute(Number_Year, lag_CashtoTA=dplyr::lag(CashtoTA)) %>% ungroup
lag_DeAngelo <- transmute(DataSet2, 
                          DIV = DIV, 
                          lag_Stage_Indicator1 = lag_Stage_Indicator1$lag_Stage_Indicator1, 
                          lag_REtoTA = lag_REtoTA$lag_REtoTA, 
                          lag_TEtoTA = lag_TEtoTA$lag_TEtoTA, 
                          lag_ROA = lag_ROA$lag_ROA, 
                          lag_SGR = lag_SGR$lag_SGR, 
                          lag_AGR = lag_AGR$lag_AGR, 
                          lag_CashtoTA = lag_CashtoTA$lag_CashtoTA)
model116 <- lm(data = na.omit(lag_DeAngelo), DIV ~ . )
summary(model116)

bptest(model116) #есть гетероскедастичность
stepAIC(model116)
model117 <- update(model116,DIV ~ lag_Stage_Indicator1 + lag_REtoTA + lag_TEtoTA + 
                     lag_SGR + lag_AGR + lag_CashtoTA)
summary(model117)

stargazer(model116, model117, type = "html", df = FALSE, se = list(cse(model116), cse(model117)), out = "lag.html", title="Results DeAngelo")


#фиксированные и случайные эффекты
names(DeAngelo1)
library(plm)
cycle_pooled <- plm(data = DeAngelo1,DIV~Stage_Indicator1+REtoTA+TEtoTA+ROA+SGR+AGR+CashtoTA+Percentile, model = "pooling",index = c("Number_Year", "Number_Company"))
summary(cycle_pooled)
bptest(cycle_pooled) #есть гетероскедастичность
cycle_pooled1 <- plm(data = DeAngelo1,DIV~Stage_Indicator1+REtoTA+TEtoTA+SGR+CashtoTA+Percentile, model = "pooling",index = c("Number_Year", "Number_Company"))
summary(cycle_pooled1)
bptest(cycle_pooled1) #есть гетероскедастичность

cycle_random <- plm(data = DeAngelo1,DIV~Stage_Indicator1+REtoTA+TEtoTA+ROA+SGR+AGR+CashtoTA+Percentile, model = "random",index = c("Number_Year", "Number_Company"))
summary(cycle_random)
bptest(cycle_random) #есть гетероскедастичность
cycle_random1 <- plm(data = DeAngelo1,DIV~Stage_Indicator1+REtoTA+TEtoTA+SGR+CashtoTA+Percentile, model = "random",index = c("Number_Year", "Number_Company"))
summary(cycle_random1)
bptest(cycle_random1) #есть гетероскедастичность

cycle_fix <- plm(data = DeAngelo1,DIV~Stage_Indicator1+REtoTA+TEtoTA+ROA+SGR+AGR+CashtoTA+Percentile, model = "within",index = c("Number_Year", "Number_Company"))
summary(cycle_fix)
bptest(cycle_fix) #есть гетероскедастичность
cycle_fix1 <- plm(data = DeAngelo1,DIV~Stage_Indicator1+REtoTA+TEtoTA+SGR+CashtoTA+Percentile, model = "within",index = c("Number_Year", "Number_Company"))
summary(cycle_fix1)
bptest(cycle_fix1) #есть гетероскедастичность

#без удаления
#Тестирование гипотез (первая модель это нулевая гипотеза):
#– фиксированные эффекты против обычной. Нулевая гипотеза: обычная модель лучше. Порядок важен, сначала записываем фиксированную модель. 
pFtest(cycle_fix, cycle_pooled) #лучше фиксированные эффекты
#фиксированные > пула

#– тест Хаусмана (RЕ против ФЕ). Нулевая гипотеза: случайные эффекты лучше
phtest(cycle_random, cycle_fix) #лучше фиксированные эффекты
#фиксированные > случайные

#– тест множителей Лагранжа (RЕ против обычной). Нулевая гипотеза: обычная регрессия лучше
plmtest(cycle_random, type = "bp") #лучше случайные эффекты 
#случайные > пула 

#с удалением
#– фиксированные эффекты против обычной. Нулевая гипотеза: обычная модель лучше. Порядок важен, сначала записываем фиксированную модель. 
pFtest(cycle_fix1, cycle_pooled1) #лучше фиксированные эффекты
#фиксированные > пула

#– тест Хаусмана (RЕ против ФЕ). Нулевая гипотеза: случайные эффекты лучше
phtest(cycle_random1, cycle_fix1) #лучше фиксированные эффекты
#фиксированные > случайные

#– тест множителей Лагранжа (RЕ против обычной). Нулевая гипотеза: обычная регрессия лучше
plmtest(cycle_random1, type = "bp") #лучше случайные эффекты 
#случайные > пула 

stargazer(list(cycle_pooled, cycle_fix, cycle_random), column.labels = c("Pooling", "RE", "FE"), type = "html", df = FALSE, se = list(cse(cycle_pooled), cse(cycle_fix), cse(cycle_random)), out = "WithoutDelete.html")
stargazer(list(cycle_pooled1, cycle_fix1, cycle_random1), column.labels = c("Pooling", "RE", "FE"), type = "html", df = FALSE, se = list(cse(cycle_pooled1), cse(cycle_fix1), cse(cycle_random1)), out = "WithDelete.html")


model41 <- update(model3, .~.+Capitalization)
summary(model41)

model42 <- update(model41, Capitalization ~. + DIV-Capitalization)
summary(model42)

stargazer(model3, model5, model41, type = "html", df = FALSE, se = list(cse(model3), cse(model5), cse(model41), cse(model4)), out = "table11.html", title="Results DeAngelo")
cor(DeAngelo$Percentile, DeAngelo$Capitalization)

stargazer(model42, type = "html", df = FALSE, se = list(cse(model42)), out = "table12.html", title="Results DeAngelo")


#с PayOrNot - логит регрессия
model6 <- glm(PayOrNot ~ . -PayoutRatio-DIV-Number_Year-Number_Company-MB_ratio, data = na.omit(DeAngelo), family = binomial(link = "logit"))
summary(model6)
hitmiss(model6)

#Предупреждение – возникли подогнанные вероятности 0 или 1 - выборка не сбалансированная

#§2 - модель из Lintner, 1956
#посмотрим на дивидендные выплаты
#работаем с DataSet2 
for (i in 1:30){
  assign(paste0("TableLintner", i), dplyr::filter(DataSet2, Number_Company == i))
  assign(paste0("ChartLintner", i), ggplot(data = get(paste("TableLintner", i, sep="")), aes(x = Number_Year, y = PayoutRatio))+geom_point(col = "blue")+geom_line(col = "purple")+xlab("Год")+ylab("Коэффициент 
дивидендных выплат")+ggtitle(paste0("Компания № ", i)))
}

grid.arrange(ChartLintner1, ChartLintner2, ChartLintner4, ChartLintner5, ChartLintner6, ChartLintner7, ChartLintner8, ChartLintner9, ChartLintner12, ChartLintner13, ChartLintner14, ChartLintner15, ncol = 4)
grid.arrange(ChartLintner16, ChartLintner18, ChartLintner19, ChartLintner20, ChartLintner21, ChartLintner22, ChartLintner23, ChartLintner24, ChartLintner26, ChartLintner27, ChartLintner28, ChartLintner29, ncol = 4)
ggplot(data = TableLintner2, aes(x = Number_Year, y = Net_Income))+geom_point(col = "darkred")+geom_line(col = "darkblue")+xlab("Год")+ylab("Чистая прибыль")+ggtitle("Компания №2 AMGN")+theme_bw()
names(TableLintner2)

TableLintner2

#отдельно ChartCycle30
ChartLintner30
#существуют выбросы, когда в 2019 году платили из кумулятивной нераспределенной прибыли? 
#построим по медианному значению 
names(DataSet2)
Table10 <- DataSet2 %>% group_by(Number_Company) %>% summarise(PayOut = median(PayoutRatio), PayOut_mean = mean(PayoutRatio)) 
DataSet2 <- merge(DataSet2,Table10)
DataSet2$Dividends <- DataSet2$DIV*DataSet2$EQY_sh_out
DataSet2$TargetDividend <- DataSet2$PayOut*DataSet2$Net_Income / 100
DataSet2$TargetDividend_mean <- DataSet2$PayOut_mean*DataSet2$EQY_sh_out
Table11 <- DataSet2 %>% group_by(Number_Company) %>% mutate(diff_Dividends = Dividends - lag(Dividends), diff_regressor = TargetDividend-lag(Dividends)) %>% ungroup

model7 <- lm(data = na.omit(Table11), diff_Dividends ~ diff_regressor)
summary(model7)
round(model7$coefficients, 6) #корректировка - 9%
bptest(model7)

stargazer(model7, type = "html", out = "table13.html", title="Results Lintner")

DataSet2$PayoutRatio * DataSet2$Net_Income / (100*DataSet2$EQY_sh_out)
DataSet2$DIV

#попробуем удалить влиятельные наблюдения
influencePlot(model7)
plot(cooks.distance(model7),type="h")
N <- which(cooks.distance(model7)>4/275)
N
M <- as.numeric(names(N))

#модель с удалением влиятельных наблюдений
model8 <- lm(data = na.omit(Table11)[-M,], diff_Dividends ~ diff_regressor)
summary(model8)
round(model8$coefficients, 6) #корректировка - 0.1%

stargazer(model7, model8, type = "html", out = "table13.html")

#попробуем по среднему значению построить
Table12 <- DataSet2 %>% group_by(Number_Company) %>% mutate(diff_Dividends = Dividends - lag(Dividends), diff_regressor = TargetDividend_mean-lag(Dividends)) %>% ungroup
model9 <- lm(data = na.omit(Table12), diff_Dividends ~ diff_regressor)
summary(model9) #как-то не очень, уж лучше медиану оставить!

#§3 - модель Fama, French 
#скачаю акции компаний
AAPL <- getSymbols("AAPL", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
AMGN <- getSymbols("AMGN", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
BA <- getSymbols("BA", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
CAT <- getSymbols("CAT", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
CRM <- getSymbols("CRM", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
CSCO <- getSymbols("CSCO", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
CVX <- getSymbols("CVX", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
DIS <- getSymbols("DIS", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
HD <- getSymbols("HD", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
HON <- getSymbols("HON", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
IBM <- getSymbols("IBM", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
INTC <- getSymbols("INTC", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
JNJ <- getSymbols("JNJ", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
KO <- getSymbols("KO", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
MCD <- getSymbols("MCD", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
MMM <- getSymbols("MMM", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
MRK <- getSymbols("MRK", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
MSFT <- getSymbols("MSFT", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
NKE <- getSymbols("NKE", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
PG <- getSymbols("PG", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
UNH <- getSymbols("UNH", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
V <- getSymbols("V", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
VZ <- getSymbols("VZ", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
WBA <- getSymbols("WBA", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
WMT <- getSymbols("WMT", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE)
market <- getSymbols("^GSPC", from = "2010-01-01", to = "2021-12-31", auto.assign = FALSE) #индекс S&P 500

sort(unique(DataSet2$`ID Company`))
sort(unique(DataSet2$Number_Company))
names_data <- data.frame(unique(DataSet2$Number_Company), unique(DataSet2$`ID Company`))
shares_data <- merge(AAPL$AAPL.Close, AMGN$AMGN.Close, BA$BA.Close, CAT$CAT.Close, CRM$CRM.Close, 
                     CSCO$CSCO.Close, CVX$CVX.Close, DIS$DIS.Close, HD$HD.Close, HON$HON.Close, 
                     IBM$IBM.Close, INTC$INTC.Close, JNJ$JNJ.Close, KO$KO.Close, MCD$MCD.Close, MMM$MMM.Close, MRK$MRK.Close, 
                     MSFT$MSFT.Close, NKE$NKE.Close, PG$PG.Close, UNH$UNH.Close, V$V.Close, 
                     VZ$VZ.Close, WBA$WBA.Close, WMT$WMT.Close)
return <- Return.calculate(shares_data)
return <- na.omit(return)
colnames(return) <- sort(unique(DataSet2$`ID Company`))
return_market <- Return.calculate(na.omit(market$GSPC.Close))

for (i in 1:25){
  assign(paste0("return", i), auto.arima(return[,i]))
}


p_value <- NULL
#результаты теста Дики-Фулера
for (i in 1:25){
  assign(paste0("DF.test", i), adf.test(return[,i]))
  p_value[i] <- get(paste("DF.test", i, sep=""))$p.value
}

p_value
DF.test25
names_data$ID

#поработаем с Table5
colnames(names_data) <- c("Number_Company", "ID_Company")
Table13 <- merge(Table5, names_data)
table(Table13$Result)
Mature_Companies <- Table13 %>% dplyr::filter(Result == "Sustainable Mature")
NonMature_Companies <- Table13 %>% dplyr::filter(Result == "Sustainable NonMature")
Controversial_Companies <- Table13 %>% dplyr::filter(Result == "Controversial")

return_assets1 <- return[, colnames(return) %in% as.character(Mature_Companies$ID_Company)]
return_assets1 <- na.omit(return_assets1)
return_assets2 <- return[, colnames(return) %in% as.character(NonMature_Companies$ID_Company)]
return_assets2 <- na.omit(return_assets2)
return_assets3 <- return[, colnames(return) %in% as.character(Controversial_Companies$ID_Company)]
return_assets3 <- na.omit(return_assets3)

#построение портфелей
#для Mature компаний
#портфель с одинаковыми весами активов

returns_port1 <- Return.portfolio(R = return_assets1, weights = c(rep(1/dim(return_assets1)[2],dim(return_assets1)[2])))
Return.annualized(returns_port1, geometric=FALSE) #9.50% (было для BS_PURE_RETAINED_EARNINGS) // 14.35% (для BS_RETAIN_EARN)

#для NonMature компаний
#портфель с одинаковыми весами активов
str(return_assets2)
returns_port2 <- Return.portfolio(R = return_assets2, weights = c(rep(1/dim(return_assets2)[2],dim(return_assets2)[2])))
Return.annualized(returns_port2, geometric=FALSE) #20.02% // 16.67%
table(Table13$Result)

#для Controversial компаний
#портфель с одинаковыми весами активов
returns_port3 <- Return.portfolio(R = return_assets3, weights = c(rep(1/dim(return_assets3)[2],dim(return_assets3)[2])))
Return.annualized(returns_port3, geometric=FALSE) #16.02% // 17.14%

#если вложить 1000, как будут вести себя портфели? 
#Sustainable Mature Companies
investments <- 1000
K1 <- exp(returns_port1)
plot(K1)
Price1 <- NULL
Price1[1] <- investments*K1[1]
for(i in 2:length(K1)) {
  Price1[i] <- Price1[i-1]*K1[i]
}
ts.plot(Price1)

plot(Price1, type = "l", xlab="Количество дней", ylab="Стоимость портфеля", col = "darkblue", main = "Изменение стоимости портфеля 
устойчиво зрелых компаний при вложении в портфель 1000$") 
names(data_sum3)
Price1
Price1[3019]/Price1[1] #за 10 лет увеличение в 3.12 раза // 4.83

#Sustainable NonMature Companies
K2 <- exp(returns_port2)
plot(K2)
Price2 <- NULL
Price2[1] <- investments*K2[1]
for(i in 2:length(K2)) {
  Price2[i] <- Price2[i-1]*K2[i]
}
ts.plot(Price2)
plot(Price2, type = "l", xlab="Количество дней", ylab="Стоимость портфеля", col = "darkblue", main = "Изменение стоимости портфеля 
устойчиво незрелых компаний при вложении в портфель 1000$") 
Price2[3019]/Price2[1] #за 10 лет увеличение в 9 раз! // 6.27

#Controversial Companies
K3 <- exp(returns_port3)
plot(K3)
Price3 <- NULL
Price3[1] <- investments*K3[1]
for(i in 2:length(K3)) {
  Price3[i] <- Price3[i-1]*K3[i]
}
ts.plot(Price3)
plot(Price3, type = "l", xlab="Количество дней", ylab="Стоимость портфеля", col = "darkblue", main = "Изменение стоимости портфеля 
спорных компаний при вложении в портфель 1000$") 
Price3[3019]/Price3[1] #за 10 лет увеличение в 7.39 раз! // 6.56

#построим график по-красивее
Price1
Price2
Price3

change_portfolio <- data.frame(Price1, Price2, Price3)
colnames(change_portfolio) <- c("Sustainable Mature", "Sustainable NonMature", "Controversial")
change_portfolio$days <- c(1:3019)
View(change_portfolio)

change_portfolio1 <- change_portfolio %>% pivot_longer(cols = "Sustainable Mature":"Controversial", 
                                                       names_to = "Type", 
                                                       values_to = "Price")

Chart129 <- ggplot(data = change_portfolio1, aes(x = days, y = Price, color = Type)) + theme_bw() + geom_line(size = 1) + theme(legend.position = "none") + 
  scale_color_manual(values = c("#a16db7", "#ccb4c3", "#5d3277"), 
                     labels = c("Спорные", "Устойчиво зрелые", "Устойчиво незрелые")) + 
  ggtitle("Изменение стоимости портфеля при вложении в портфель $1,000 
в зависимости от принадлежности компании к определенной стадии жизненного цикла", subtitle = "В квадратных скобках указан темп роста портфеля на горизонте 2010-2021 гг.") + ylab("Стоимость портфеля") + xlab("Количество дней") + scale_y_continuous(labels = scales::dollar_format())

Chart129
Price1[3019]

Chart129 +  annotate(geom = "label", x = 3220, y=3200, label = "Устойчиво 
зрелые 
[3.13x]", fill="#ccb4c3") + annotate(geom = "label", x = 3220, y=11000, label = "Устойчиво 
незрелые 
[11.08x]", fill="#5d3277", color = "white") + annotate(geom = "label", x = 3220, y=7400, label = "Спорные
[7.39x]", fill="#a16db7", color = "white") 

#как отличаются беты у трех типов компаний
#источник для rf - https://home.treasury.gov/resource-center/data-chart-center/interest-rates/TextView?type=daily_treasury_yield_curve&field_tdr_date_value_month=202203
#возьму на 23/03/2022 - 2.32%
rf <- 0.0232
beta1 <- CAPM.beta(returns_port1, return_market-rf/365)
beta2 <- CAPM.beta(returns_port2, return_market-rf/365)
beta3 <- CAPM.beta(returns_port3, return_market-rf/365)

beta1 ; beta2 ; beta3 #beta1 - Mature; beta2 - NonMature; beta3 - Controversial

#посмотрим, есть ли различия
t.test(returns_port1, returns_port2) #0 входит => различий нет? 
t.test(returns_port2, returns_port3)
t.test(returns_port1, returns_port3)
var.test(returns_port1, returns_port2) #1 не входит => дисперсии разные
var.test(returns_port1, returns_port3)
var.test(returns_port2, returns_port3)

return_portfolio_cycle <- data.frame(returns_port1, returns_port2, returns_port3)
colnames(return_portfolio_cycle) <- c("Устойчиво зрелые", "Устойчиво незрелые", "Спорные")
return_portfolio_cycle1 <- return_portfolio_cycle %>% pivot_longer(cols = "Устойчиво зрелые":"Спорные", 
                                                                   names_to = "Type", 
                                                                   values_to = "Value")
ggbetweenstats(
  data = return_portfolio_cycle1, 
  x     = Type,
  y     = Value,
  title = "Результаты теста Геймса-Ховелла", 
  caption = "Тест используется в случае наличия 3 и больше независимых выборок", 
  xlab = "Портфели", 
  ylab = "Доходность", 
  p.adjust.method = "none",
  bf.message = FALSE, 
  pairwise.display = "all") + scale_color_manual(values = c("#a16db7", "#ccb4c3", "#5d3277", "#e1ebec", "#aacfe2"))


#если дисперсии разные, то посчитаем их (в годовом выражении)
sd.table <- data.frame(sd.annualized(returns_port1, geometric=FALSE) , 
                       sd.annualized(returns_port2, geometric=FALSE) , 
                       sd.annualized(returns_port3, geometric=FALSE)) 
colnames(sd.table) <- c("Mature", "NonMature", "Transition")
sd.table1 <- as.data.frame(t(sd.table))
colnames(sd.table1) <- c("AnnualizedStandardDeviation")
sd.table1$Status <- c("Mature", "NonMature", "Transition")
plot(sd.table1$`AnnualizedStandardDeviation`)

#по var тесту стандартное отклонение весомо отличается, поэтому проанализируем его
ggplot() +geom_col(data=sd.table1, aes(x = Status, y=AnnualizedStandardDeviation, fill = Status)) + theme_bw() +
  scale_fill_manual(values=c("midnightblue", "lightsteelblue3", "mediumorchid4")) + theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank()) + ggtitle("Стандартное отклонение разных групп компаний")

#соединим несколько характеристик по портфелям вместе (beta, return и sd)
features <- c(rep("beta" , 3) , rep("sd" , 3) , rep("return" , 3))
condition <- rep(c("Mature" , "NonMature" , "Transition") , 3)
value <- abs(rnorm(9 , 0 , 15))
data <- data.frame(features,condition,value)
names(sd.table1)
sd.table2 <- sd.table1
sd.table2$ID <- c(rep("sd",3))

betas <- c(beta1, beta2, beta3)
status <- rep(c("Mature" , "NonMature" , "Transition") , 1)
id <- rep(c("beta"),3)
beta.table <- data.frame(betas, status, id)

returns <- c(Return.annualized(returns_port1, geometric=FALSE), Return.annualized(returns_port2, geometric=FALSE), Return.annualized(returns_port3, geometric=FALSE))
id1 <- rep(c("return"),3)
return.table <- data.frame(returns, status, id1)

colnames(sd.table2) <- c("values", "status", "id")
colnames(beta.table) <- c("values", "status", "id")
colnames(return.table) <- c("values", "status", "id")
data_values <- rbind(sd.table2, beta.table, return.table)

# строим график
ggplot(data_values, aes(fill=status, y=values, x=status, label = round(values,2))) + 
  geom_bar(position="dodge", stat="identity", color = "black")  +
  ggtitle("Характеристики портфелей") +
  facet_wrap(~id) +
  theme_bw() +
  theme(legend.position="none", 
        strip.background = element_rect(fill = "gray7"), 
        strip.text = element_text(colour = 'white')) +
  xlab("") + scale_fill_manual(values=c("midnightblue", "lightsteelblue3", "mediumorchid4")) + ylab("") + geom_text(size = 3, position = position_stack(vjust = 0.5), col = "white") + 
  scale_x_discrete(labels = c('Устойчиво зрелые','Устойчиво незрелые','Спорные'))

#отдельно беты со стандартными отклонениями
model12 <- lm(returns_port1 - rf/365 ~ I(return_market[-1,]-rf/365))
summary(model12)
#сверка
CAPM.beta(returns_port1, return_market-rf/365)

model13 <- lm(returns_port2 - rf/365 ~ I(return_market[-1,]-rf/365))
summary(model13)
#сверка
CAPM.beta(returns_port2, return_market-rf/365)

model14 <- lm(returns_port3 - rf/365 ~ I(return_market[-1,]-rf/365))
summary(model14)
#сверка
CAPM.beta(returns_port3, return_market-rf/365)
summary(model12)
sigma_model12 <- summary(model12)$coef[2,2]
sigma_model13 <- summary(model13)$coef[2,2]
sigma_model14 <- summary(model14)$coef[2,2]

sigmas <- c(sigma_model12, sigma_model13, sigma_model14)
beta.table1 <- data.frame(betas, sigmas)
beta.table1$status <- c("Mature", "NonMature", "Transition")

ggplot(beta.table1) +
  geom_bar( aes(x=status, y=betas, fill=status), stat="identity", alpha=0.7, color = "black") +
  geom_errorbar( aes(x=status, ymin=betas-sigmas, ymax=betas+sigmas), width=0.4, colour="darkslategray1", alpha=0.9, size=1.3) + xlab("") + ylab("Беты компаний") + 
  scale_fill_manual(values=c("midnightblue", "lightsteelblue3", "mediumorchid4")) + 
  scale_x_discrete(labels = c('Устойчиво зрелые','Устойчиво незрелые','Спорные')) +
  theme_bw() + theme(legend.position="none")

#ну беты не сильно разброшены

#Fama - French
names(return)

#взяла последний год (21)
names(DataSet2)
max(FamaFrench1$BM_ratio)
min(FamaFrench1$BM_ratio)
round(quantile(FamaFrench1$BM_ratio, 0.7),3)
round(quantile(FamaFrench1$BM_ratio, 0.3),3)
FamaFrench <- DataSet2 %>% dplyr::filter(Number_Year == 21) %>% transmute(Number_Company, Capitalization, 1/MB_ratio, PayoutRatio)
words <- c(names_data$ID_Company)
ID <- stri_extract_first(words, regex="\\w+")
names_data$ID <- ID
FamaFrench1 <- merge(FamaFrench, names_data, by ="Number_Company") #соединила с ID_Company
View(FamaFrench1)
colnames(FamaFrench1) <- c("Number_Company", "Capitalization", "BM_ratio", "PayoutRatio" , "ID_Company", "ID")
ggplot(data = FamaFrench1, aes(x = Capitalization, y = BM_ratio)) + geom_point(size = 1, shape = 10, colour = "purple") + 
  geom_text(aes(label=ID),hjust=0, vjust=0, size = 3, colour = "black") + 
  geom_hline(yintercept=quantile(FamaFrench1$BM_ratio, 0.7), color = "midnightblue", size = 1) + 
  geom_hline(yintercept=quantile(FamaFrench1$BM_ratio, 0.3), color = "turquoise3", size = 1) + 
  geom_vline(xintercept=median(FamaFrench1$Capitalization), color = "violetred3", size = 1) + theme_bw() + 
  xlab("Капитализация компании") + ylab("Соотношение Book-to-Market") + 
  annotate("text", x = 1500000, y=0.4, label = "Big High", color="darkblue") + 
  annotate("text", x = 60000, y=0.4, label = "Small High",  color="darkblue") + 
  annotate("text", x = 1500000, y=0.15, label = "Big Neutral", color="darkblue") + 
  annotate("text", x = 60000, y=0.15, label = "Small 
Neutral",  color="darkblue") + 
  annotate("text", x = 1500000, y=0.0, label = "Big Low", color="darkblue") + 
  annotate("text", x = 60000, y=0.0, label = "Small Low",  color="darkblue") + 
  ggtitle("Распределение компаний по параметрам из модели Фамы-Френча")


#построим 3D график

names(FamaFrench1)
Chart88 <- plot_ly(FamaFrench1, x = ~Capitalization, y = ~BM_ratio, z = ~PayoutRatio, color = ~criteria_Dividend, colors = c('#04323a', "#d1dcde", '#0b8eab'))
Chart88 <- Chart88 %>% add_markers()
Chart88 <- Chart88 %>% layout(title = 'Распределение параметров Фамы-Френч', 
                                  scene = list(xaxis = list(title = 'Капитализация компании'),
                                   yaxis = list(title = 'Соотношение Book-to-Market'),
                                   zaxis = list(title = 'Коэффициент дивидендных выплат')), 
                                   legend = list(title=list(text='<b>Классификация размера 
дивидендных выплат </b>')))
Chart88

saving <- gapminder %>% plot_ly(FamaFrench1, x = ~Capitalization, y = ~BM_ratio, z = ~PayoutRatio, color = ~criteria_Dividend, colors = c('#04323a', "#d1dcde", '#0b8eab')) %>% add_markers()  %>%
  layout(title = 'Распределение параметров Фамы-Френч', 
        scene = list(xaxis = list(title = 'Капитализация компании'),
                     yaxis = list(title = 'Соотношение Book-to-Market'),
                     zaxis = list(title = 'Коэффициент дивидендных выплат')), 
        legend = list(title=list(text='<b>Классификация размера 
дивидендных выплат </b>')))
library(yaml)
ui <- fluidPage(
  plotlyOutput("graph")
)

server <- function(input, output, session) {
  N <- 100
  Capitalization <- FamaFrench1$Capitalization
  BM_ratio <- FamaFrench1$BM_ratio
  PayoutRatio <- FamaFrench1$PayoutRatio
  luci.frame <- data.frame(Capitalization, BM_ratio, PayoutRatio)
  
  mySequence <- seq(0, 100, by = 0.03)
  
  cam.zoom = 2
  # ver.angle = 0
  
  output$graph <- renderPlotly({
    plot_ly(
      type = "scatter3d",
      mode = "markers",
      data = luci.frame,
      x = ~ Capitalization,
      y = ~ BM_ratio,
      z = ~ PayoutRatio, 
      color = ~FamaFrench1$criteria_Dividend, 
      colors = c('#04323a', "#d1dcde", '#0b8eab')
    ) %>%
      layout(title = 'Распределение параметров Фамы-Френч', 
             legend = list(title=list(text='<b>Классификация размера 
дивидендных выплат </b>')),
             scene = list(camera = list(eye = list(x = cos(mySequence[1]) * cam.zoom,
                                                   y = sin(mySequence[1]) * cam.zoom,
                                                   z = 0.3)
                                        )
                          )
             )
    })
  
  count <- reactiveVal(1L)

  observe({
    invalidateLater(100)
    plotlyProxyInvoke(plotlyProxy("graph"), "relayout", list(scene = list(camera = list(
      eye = list(
        x = cos(mySequence[isolate(count())]) * cam.zoom,
        y = sin(mySequence[isolate(count())]) * cam.zoom,
        z = 0.3
      ),
      center = list(x = 0,
                    y = 0,
                    z = 0)
    ))))
    
    isolate(count(count()+1))
    
    if(count() > length(mySequence)){
      count(1L)  
    }
  })
}

shinyApp(ui, server)




#разбиваем капитализацию по медиане
FamaFrench1$criteria_capitalization <- ifelse(FamaFrench1$Capitalization>median(FamaFrench1$Capitalization), "Big", "Small")
table(FamaFrench1$criteria_capitalization) #12 больших; 13 маленьких
quantile(FamaFrench1$BM_ratio, seq(0,1, by = 0.1))
quantile(FamaFrench1$BM_ratio, 0.3)
quantile(FamaFrench1$BM_ratio, 0.7)
quantile(FamaFrench1$BM_ratio, 1)

FamaFrench1$criteria_BMratio <- ifelse(FamaFrench1$BM_ratio>quantile(FamaFrench1$BM_ratio, 0.7), "High", 
                                         ifelse(FamaFrench1$BM_ratio>quantile(FamaFrench1$BM_ratio, 0.3), "Neutral", "Low"))

FamaFrench1$group <- paste0(FamaFrench1$criteria_capitalization, " " , FamaFrench1$criteria_BMratio) 
table(FamaFrench1$group)
View(FamaFrench1)
data_SmallLow <- dplyr::filter(FamaFrench1, FamaFrench1$group == "Small Low")
data_SmallNeutral <- filter(FamaFrench1, FamaFrench1$group == "Small Neutral")
data_SmallHigh <- filter(FamaFrench1, FamaFrench1$group == "Small High")
data_BigLow <- filter(FamaFrench1, FamaFrench1$group == "Big Low")
data_BigNeutral <- filter(FamaFrench1, FamaFrench1$group == "Big Neutral")
data_BigHigh <- filter(FamaFrench1, FamaFrench1$group == "Big High")

#построение портфелей по ФФ

#1- SmallLow

return_assets_SmallLow <- return[, colnames(return) %in% data_SmallLow$ID_Company]
returns_port4 <- Return.portfolio(R = return_assets_SmallLow, weights = c(rep(1/dim(return_assets_SmallLow)[2],dim(return_assets_SmallLow)[2])))
round(Return.annualized(returns_port4),3)
round(sd.annualized(returns_port4),3)

#2 - SmallNeutral
return_assets_SmallNeutral <- return[, colnames(return) %in% data_SmallNeutral$ID_Company]
returns_port5 <- Return.portfolio(R = return_assets_SmallNeutral, weights = c(rep(1/dim(return_assets_SmallNeutral)[2],dim(return_assets_SmallNeutral)[2])))
round(Return.annualized(returns_port5),3)
round(sd.annualized(returns_port5),3)

#3 - SmallHigh
return_assets_SmallHigh <- return[, colnames(return) %in% data_SmallHigh$ID_Company]
returns_port6 <- Return.portfolio(R = return_assets_SmallHigh, weights = c(rep(1/dim(return_assets_SmallHigh)[2],dim(return_assets_SmallHigh)[2])))
round(Return.annualized(returns_port6),3)
round(sd.annualized(returns_port6),3)

#4 - BigLow
return_assets_BigLow <- return[, colnames(return) %in% data_BigLow$ID_Company]
returns_port7 <- Return.portfolio(R = return_assets_BigLow, weights = c(rep(1/dim(return_assets_BigLow)[2],dim(return_assets_BigLow)[2])))
round(Return.annualized(returns_port7),3)
round(sd.annualized(returns_port7),3)

#5 - BigNeutral
return_assets_BigNeutral <- return[, colnames(return) %in% data_BigNeutral$ID_Company]
returns_port8 <- Return.portfolio(R = return_assets_BigNeutral, weights = c(rep(1/dim(return_assets_BigNeutral)[2],dim(return_assets_BigNeutral)[2])))
round(Return.annualized(returns_port8),3)
round(sd.annualized(returns_port8),3)

#6 - BigHigh
return_assets_BigHigh <- return[, colnames(return) %in% data_BigHigh$ID_Company]
returns_port9 <- Return.portfolio(R = return_assets_BigHigh, weights = c(rep(1/dim(return_assets_BigHigh)[2],dim(return_assets_BigHigh)[2])))
round(Return.annualized(returns_port9),3)
round(sd.annualized(returns_port9),3)

FF_return_portfolio <- data.frame(returns_port4, returns_port5, returns_port6, returns_port7, returns_port8, returns_port9)
colnames(FF_return_portfolio) <- c("SmallLow", "SmallNeutral", "SmallHigh", "BigLow", "BigNeutral", "BigHigh")
t.test(FF_return_portfolio$SmallLow, FF_return_portfolio$BigHigh) #0 входит...
var.test(FF_return_portfolio$SmallLow, FF_return_portfolio$BigHigh) #дисперсии разные!!! 

t.test(FF_return_portfolio$SmallHigh, FF_return_portfolio$BigHigh) #0 входит...
var.test(FF_return_portfolio$SmallHigh, FF_return_portfolio$BigHigh) #дисперсии разные!!! 

t.test(FF_return_portfolio$SmallLow, FF_return_portfolio$BigLow) #0 входит...
var.test(FF_return_portfolio$SmallLow, FF_return_portfolio$BigLow) #дисперсии разные!!! 
str(FF_return_portfolio)
FF_return_portfolio1 <- FF_return_portfolio %>% pivot_longer(cols = SmallLow:BigHigh, names_to = "Type", values_to = "Amount")
FF_return_portfolio2 <- FF_return_portfolio1 %>% filter(Type %in% c("SmallLow", "BigHigh", "SmallHigh", "BigLow"))
set.seed(123)
str(FF_return_portfolio2)
FF_return_portfolio2 <- as.data.frame(FF_return_portfolio2)
FF_return_portfolio2$Type <- as.factor(FF_return_portfolio2$Type)
ggbetweenstats(
  data = FF_return_portfolio2, 
  x     = Type,
  y     = Amount,
  title = "Результаты теста Геймса-Ховелла", 
  caption = "Тест используется в случае наличия 3 и больше независимых выборок", 
  xlab = "Портфели", 
  ylab = "Доходность", 
  p.adjust.method = "none",
  bf.message = FALSE, 
  pairwise.display = "all") + scale_color_manual(values = c("#3b6ba5", "#72a5d3", "#b1d3e3", "#e1ebec", "#aacfe2"))

table(FF_return_portfolio2$Type)

#строим регрессию с SMB и HML
data_SMB_Big <- filter(FamaFrench1, FamaFrench1$criteria_capitalization == "Big")
data_SMB_Small <- filter(FamaFrench1, FamaFrench1$criteria_capitalization == "Small")

data_HML_High <- filter(FamaFrench1, FamaFrench1$criteria_BMratio == "High")
data_HML_Low <- filter(FamaFrench1, FamaFrench1$criteria_BMratio == "Low")

#7 - Big
return_assets_Big <- return[, colnames(return) %in% data_SMB_Big$ID_Company]
returns_port10 <- Return.portfolio(R = return_assets_Big, weights = c(rep(1/dim(return_assets_Big)[2],dim(return_assets_Big)[2])))

#8 - Small
return_assets_Small <- return[, colnames(return) %in% data_SMB_Small$ID_Company]
returns_port11 <- Return.portfolio(R = return_assets_Small, weights = c(rep(1/dim(return_assets_Small)[2],dim(return_assets_Small)[2])))

#9 - High
return_assets_High <- return[, colnames(return) %in% data_HML_High$ID_Company]
returns_port12 <- Return.portfolio(R = return_assets_High, weights = c(rep(1/dim(return_assets_High)[2],dim(return_assets_High)[2])))

#10 - Low
return_assets_Low <- return[, colnames(return) %in% data_HML_Low$ID_Company]
returns_port13 <- Return.portfolio(R = return_assets_Low, weights = c(rep(1/dim(return_assets_Low)[2],dim(return_assets_Low)[2])))

FamaFrench1$criteria_Dividend <- ifelse(FamaFrench1$PayoutRatio>quantile(FamaFrench1$PayoutRatio, 0.7), "High Dividend", 
                                       ifelse(FamaFrench1$PayoutRatio>quantile(FamaFrench1$PayoutRatio, 0.3), "Neutral Dividend", "Low Dividend"))
names(FamaFrench1)[6] <- "ID"
data_DVD_High <- dplyr::filter(FamaFrench1, FamaFrench1$criteria_Dividend == "High Dividend")
data_DVD_Low <- dplyr::filter(FamaFrench1, FamaFrench1$criteria_Dividend == "Low Dividend")

data_DVD_High$ID
data_DVD_Low$ID

FamaFrench1$group2 <- paste0(FamaFrench1$criteria_capitalization, " " , FamaFrench1$criteria_Dividend) 
table(FamaFrench1$group2)
data_BigHighDiv <- dplyr::filter(FamaFrench1, FamaFrench1$group2 == "Big High Dividend")
data_BigLowDiv <- filter(FamaFrench1, FamaFrench1$group2 == "Big Low Dividend")
data_BigNeutralDiv <- filter(FamaFrench1, FamaFrench1$group2 == "Big Neutral Dividend")
data_SmallHighDiv <- filter(FamaFrench1, FamaFrench1$group2 == "Small High Dividend")
data_SmallLowDiv <- filter(FamaFrench1, FamaFrench1$group2 == "Small Low Dividend")
data_SmallNeutralDiv <- filter(FamaFrench1, FamaFrench1$group2 == "Small Neutral Dividend")

return_assets_Div_High <- return[, colnames(return) %in% data_DVD_High$ID_Company]
returns_port14 <- Return.portfolio(R = return_assets_Div_High, weights = c(rep(1/dim(return_assets_Div_High)[2],dim(return_assets_Div_High)[2])))

return_assets_Div_Low <- return[, colnames(return) %in% data_DVD_Low$ID_Company]
returns_port15 <- Return.portfolio(R = return_assets_Div_Low, weights = c(rep(1/dim(return_assets_Div_Low)[2],dim(return_assets_Div_Low)[2])))

return_assets_Div_SmallNeutral <- return[, colnames(return) %in% data_SmallNeutralDiv$ID_Company]
returns_port16 <- Return.portfolio(R = return_assets_Div_SmallNeutral, weights = c(rep(1/dim(return_assets_Div_SmallNeutral)[2],dim(return_assets_Div_SmallNeutral)[2])))

return_assets_Div_BigNeutral <- return[, colnames(return) %in% data_BigNeutralDiv$ID_Company]
returns_port17 <- Return.portfolio(R = return_assets_Div_BigNeutral, weights = c(rep(1/dim(return_assets_Div_BigNeutral)[2],dim(return_assets_Div_BigNeutral)[2])))

t.test(returns_port16, returns_port17)
var.test(returns_port16, returns_port17)

return_assets_Div_BigHigh <- return[, colnames(return) %in% data_BigHighDiv$ID_Company]
returns_port18 <- Return.portfolio(R = return_assets_Div_BigHigh, weights = c(rep(1/dim(return_assets_Div_BigHigh)[2],dim(return_assets_Div_BigHigh)[2])))

return_assets_Div_BigLow <- return[, colnames(return) %in% data_BigLowDiv$ID_Company]
returns_port19 <- Return.portfolio(R = return_assets_Div_BigLow, weights = c(rep(1/dim(return_assets_Div_BigLow)[2],dim(return_assets_Div_BigLow)[2])))

t.test(returns_port18, returns_port19)
var.test(returns_port18, returns_port19)

return_assets_Div_SmallHigh <- return[, colnames(return) %in% data_SmallHighDiv$ID_Company]
returns_port20 <- Return.portfolio(R = return_assets_Div_SmallHigh, weights = c(rep(1/dim(return_assets_Div_SmallHigh)[2],dim(return_assets_Div_SmallHigh)[2])))

return_assets_Div_SmallLow <- return[, colnames(return) %in% data_SmallLowDiv$ID_Company]
returns_port21 <- Return.portfolio(R = return_assets_Div_SmallLow, weights = c(rep(1/dim(return_assets_Div_SmallLow)[2],dim(return_assets_Div_SmallLow)[2])))

t.test(returns_port20, returns_port21)
var.test(returns_port20, returns_port21)

data_div <- c("Small Neutral Dividend", "Big Neutral Dividend", 
              "Big High Dividend", "Big Low Dividend", 
              "Small High Dividend", "Small Low Dividend")

data_div_total <- data.frame(returns_port16, returns_port17, returns_port18, 
                             returns_port19, returns_port20, returns_port21)

colnames(data_div_total) <- data_div
names(data_div_total1)
data_div_total1 <- data_div_total %>% pivot_longer(cols = "Small Neutral Dividend":"Small Low Dividend", 
                                                   values_to = "Amount", names_to = "Type")

ggbetweenstats(
  data = data_div_total1, 
  x     = Type,
  y     = Amount,
  title = "Результаты теста Геймса-Ховелла для для портфелей с дифференциацией 
по капитализации и коэффициенту дивидендных выплат", 
  caption = "Тест используется в случае наличия 3 и больше независимых выборок", 
  xlab = "Портфели", 
  ylab = "Доходность", 
  p.adjust.method = "none",
  bf.message = FALSE, 
  pairwise.display = "all") + scale_color_manual(values = c("#3A3920", "#807622", "#EAA724", "#D25B01", 
                                                            "#5B1C02", "#9f3a06"))


ggplot(data = FamaFrench1, aes(x = Capitalization, y = PayoutRatio)) + geom_point(size = 1, shape = 10, colour = "purple") + 
  geom_text(aes(label=ID),hjust=0, vjust=0, size = 3, colour = "black") + 
  geom_hline(yintercept=quantile(FamaFrench1$PayoutRatio, 0.7), color = "midnightblue", size = 1) + 
  geom_hline(yintercept=quantile(FamaFrench1$PayoutRatio, 0.3), color = "turquoise3", size = 1) + 
  geom_vline(xintercept=median(FamaFrench1$Capitalization), color = "violetred3", size = 1) + theme_bw() + 
  xlab("Капитализация компании") + ylab("Коэффициент дивидендных выплат") + 
  annotate("text", x = 1500000, y=100, label = "Big High
Dividend", color="darkblue") + 
  annotate("text", x = 60000, y=100, label = "Small High
Dividend",  color="darkblue") + 
  annotate("text", x = 1500000, y=50, label = "Big Neutral
Dividend", color="darkblue") + 
  annotate("text", x = 60000, y=50, label = "Small 
Neutral Dividend",  color="darkblue") + 
  annotate("text", x = 1500000, y=18, label = "Big Low
Dividend", color="darkblue") + 
  annotate("text", x = 60000, y=18, label = "Small Low
Dividend",  color="darkblue") + 
  ggtitle("Распределение компаний по капитализации и 
коэффициенту дивидендных выплат с учетом квантиля и медианы")



SMB <- as.data.frame(returns_port11 - returns_port10)
HML <- as.data.frame(returns_port12 - returns_port13)
DVD <- as.data.frame(returns_port14-returns_port15) #высокие минус низкие дивидендные выплаты
ERP <- as.data.frame(return_market-c(rep(rf, dim(return_market)[1])))
dim(return_market)[1]
testdrive <- cbind(SMB, HML, DVD, return)
colnames(testdrive)[c(1:3)] <- c("SMB", "HML", "DVD")
risk_free <- as.data.frame(c(rep(rf, dim(return_market)[1])))
model10 <- lm(data = testdrive, testdrive$`AAPL UW Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model10)
#дивиденды отрицательно влияют на доходность акций??? (случаются гэпы!)

model11 <- lm(data = testdrive, testdrive$`AMGN UW Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model11) #зависит от акций компании... 

model18 <- lm(data = testdrive, testdrive$`BA UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model18)

model19 <- lm(data = testdrive, testdrive$`CAT UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model19)

model20 <- lm(data = testdrive, testdrive$`CRM UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model20)

model21 <- lm(data = testdrive, testdrive$`CSCO UW Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model21)

model22 <- lm(data = testdrive, testdrive$`CVX UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model22)

model23 <- lm(data = testdrive, testdrive$`DIS UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model23)

model24 <- lm(data = testdrive, testdrive$`HD UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model24)

model25 <- lm(data = testdrive, testdrive$`HON UW Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model25)

model26 <- lm(data = testdrive, testdrive$`IBM UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model26)

model27 <- lm(data = testdrive, testdrive$`INTC UW Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model27)

model28 <- lm(data = testdrive, testdrive$`JNJ UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model28)

model29 <- lm(data = testdrive, testdrive$`KO UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model29)

model30 <- lm(data = testdrive, testdrive$`MCD UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model30)

model31 <- lm(data = testdrive, testdrive$`MMM UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model31)

model32 <- lm(data = testdrive, testdrive$`MRK UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model32)

model33 <- lm(data = testdrive, testdrive$`MSFT UW Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model33)

model34 <- lm(data = testdrive, testdrive$`NKE UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model34)

model35 <- lm(data = testdrive, testdrive$`PG UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model35)

model36 <- lm(data = testdrive, testdrive$`UNH UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model36)

model37 <- lm(data = testdrive, testdrive$`V UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model37)

model38 <- lm(data = testdrive, testdrive$`VZ UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model38)

model39 <- lm(data = testdrive, testdrive$`WBA UW Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model39)

model40 <- lm(data = testdrive, testdrive$`WMT UN Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365)+SMB + HML+DVD)
summary(model40)

coef(model40)

stargazer(model10, model11, model18, model19, model20, summary=TRUE, type = "html", title="Results", align=TRUE, out = "table1.html")
stargazer(model21, model22, model23, model24, model25, summary=TRUE, type = "html", title="Results", align=TRUE, out = "table2.html")
stargazer(model26, model27, model28, model29, model30, summary=TRUE, type = "html", title="Results", align=TRUE, out = "table3.html")
stargazer(model31, model32, model33, model34, model35, summary=TRUE, type = "html", title="Results", align=TRUE, out = "table4.html")
stargazer(model36, model37, model38, model39, model40, summary=TRUE, type = "html", title="Results", align=TRUE, out = "table5.html")

corr5 <- cor(testdrive[,c(1:3)])
corrplot(corr5) #нет мультиколлинеарности в модели

#§4 - Dividend-to-Sales в качестве регрессанта
data_DivToSales <- DataSet2 %>% transmute(AGR = AGR, 
                                          DivToSales = Dividends/Sales, 
                                          MB_ratio = MB_ratio, 
                                          ROS = IS_Operating_Income/Sales, 
                                          SGR = SGR$SGR)
data_DivToSales1 <- na.omit(data_DivToSales)

model15 <- lm(data = data_DivToSales1, DivToSales ~ .)
summary(model15) #все сошлось с точки зрения подтверждения результатов предыдущих исследователей

vif(model15)
plot(model15, which = 2,
     main = "Распределение остатков регрессии",
     xlab = "Theoretical quantilies",
     pch=16, col="darkblue")
bptest(model15)
resettest(model15)
crPlots(model15)

stepAIC(model15)
names(data_DivToSales1)
median(data_DivToSales1$DivToSales)

model41 <- update(model15, .~.-MB_ratio)
summary(model41)
bptest(model41)


stargazer(model15, model41, type = "html", df = FALSE, se = list(cse(model15), cse(model41)), out = "table8.html", title="Results DivToSales")
resettest(model15)
crPlots(model15)
model110 <- update(model15, .~.+I(ROS^3))
resettest(model110)
crPlots(model110)

V41 <- vcovHC(model41, type = "HC0")

ggcoefstats(model41, 
            title = "Результаты лучшей регрессии по модели с регрессантом DivToSales", 
            ylab = "Переменные", 
            xlab = "Оценка", 
            package = "RColorBrewer",
            palette = "Dark2", 
            vcov = V41
)


#§5 - Dividend-to-Earnings в качестве регрессанта
names(DataSet2)
data_DivToEarnings <- DataSet2 %>% transmute(PayoutRatio = PayoutRatio, 
                                             AGR = AGR, 
                                             Independent_Directors = Independent_Directors, 
                                             CAPEX = CAPEX, 
                                             Cash = Cash, 
                                             DE_ratio = (BS_LT_Borrow+BS_ST_Borrow)/Capitalization, 
                                             Stage_Indicator1 = Stage_Indicator1, 
                                             Stage_Indicator2 = Stage_Indicator2, 
                                             Firm_size = log(BS_Total_Assets), 
                                             CF_FCFF = CF_FCFF, 
                                             MB_ratio = MB_ratio, 
                                             ROA = ROA, 
                                             ROS = IS_Operating_Income/Sales, 
                                             SGR = SGR$SGR, 
                                             Tangible_Assets = Tangible_Assets, 
                                             Tax_Rate = Tax_Rate)

data_DivToEarnings1 <- na.omit(data_DivToEarnings)

model16 <- lm(data = data_DivToEarnings1, PayoutRatio ~ .-MB_ratio) #знак согласуется в том, что значимо
summary(model16)
vif(model16) #благодаря этому поняла, что надо удалить MBratio

stepAIC(model16) #если всё удалю, то останутся переменные: Stage_Indicator2, Firm_size, ROA, AGR, Tax_Rate

model17 <- update(model16, .~.-Independent_Directors - DE_ratio - ROS - CAPEX - Cash - Tangible_Assets - CF_FCFF - Stage_Indicator1 - SGR)
summary(model17)
bptest(model16)

model113 <- update(model16, log(PayoutRatio+1) ~ .)

plot(model16, which = 2)
plot(model113, which = 2)
stargazer(model16, model17, type = "html", df = FALSE, se = list(cse(model16), cse(model17)), out = "table9.html", title="Results PayoutRatio")
model17
min(data_DivToEarnings1$AGR)
min(data_DivToEarnings1$Stage_Indicator2)
min(data_DivToEarnings1$Firm_size)
data_DivToEarnings1$Tax_Rate

bptest(model16)
bptest(model17)
V17 <- vcovHC(model17, type = "HC0")

ggcoefstats(model17, 
            title = "Результаты лучшей регрессии по модели с регрессантом PayoutRatio", 
            ylab = "Переменные", 
            xlab = "Оценка", 
            package = "RColorBrewer",
            palette = "Dark2"
)

#§6 - Dividend-to-Total Assets в качестве регрессанта - Michaely & Roberts, 2012
data_DivToTA <- DataSet2 %>% transmute(DivToTA = Dividends/BS_Total_Assets, 
                                             DebtToAssets=(BS_LT_Borrow+BS_ST_Borrow)/BS_Total_Assets,
                                             Firm_size = log(BS_Total_Assets), 
                                             SGR = SGR$SGR, 
                                             ProfToAssets = Net_Income/BS_Total_Assets)

data_DivToTA1 <- na.omit(data_DivToTA)
model18 <- lm(data = data_DivToTA1, DivToTA ~ .) #не согласуется знак в DebtToAssets
summary(model18)
bptest(model18)
vif(model18)
plot(model18, which = 2)

model43 <- update(model18, .~.-Firm_size)
summary(model43)
stepAIC(model18)

vif(model18)

stargazer(model18, model43, type = "html", df = FALSE, out = "table10.html", title="Results PayoutRatio")
bptest(model18)
resettest(model43)
resettest(model18)
bptest(model43)
V43 <- vcovHC(model43, type = "HC0")
ggcoefstats(model43, 
            title = "Результаты лучшей регрессии по модели с регрессантом DivToTA", 
            ylab = "Переменные", 
            xlab = "Оценка", 
            package = "RColorBrewer",
            palette = "Dark2", 
            vcov = V43
)

#§7 - CAR
#вспомним графики
grid.arrange(ChartLintner1, ChartLintner2, ChartLintner4, ChartLintner5, ChartLintner6, ChartLintner7, ChartLintner8, ChartLintner9, ChartLintner12, ChartLintner13, ChartLintner14, ChartLintner15, ncol = 4)
grid.arrange(ChartLintner16, ChartLintner18, ChartLintner19, ChartLintner20, ChartLintner21, ChartLintner22, ChartLintner23, ChartLintner24, ChartLintner26, ChartLintner27, ChartLintner28, ChartLintner29, ncol = 4)
#отдельно ChartCycle30
ChartLintner30

#попытки идентификации аутлайеров
ggplot(TableLintner2) +
  aes(x = "", y = PayoutRatio) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_bw() + ylab("Коэффициент дивидендных выплат") + xlab("Компания №2 AMGN")

boxplot.stats(TableLintner2$PayoutRatio)$out
TableLintner2[which(TableLintner2$PayoutRatio==boxplot.stats(TableLintner2$PayoutRatio)$out),]

for (i in 1:30){
  assign(paste0("use",i),boxplot.stats(get(paste("TableLintner", i, sep=""))$PayoutRatio)$out)
  assign(paste0("TableCAR",i), get(paste("TableLintner", i, sep=""))[which(get(paste("TableLintner", i, sep=""))$PayoutRatio==get(paste("use",i,sep=""))),])
}

#посмотрим на TableCAR, которые ненулевые и будем их расширять
#TableCAR2, TableCAR5, TableCAR7, TableCAR9, TableCAR13, TableCAR16, TableCAR18
#TableCAR21, TableCAR22, TableCAR23, TableCAR24, TableCAR28, TableCAR29, TableCAR30

TableCAR <- rbind(TableCAR2, TableCAR5, TableCAR7, TableCAR9, TableCAR13, TableCAR16, TableCAR18, 
                  TableCAR21, TableCAR22, TableCAR23, TableCAR24, TableCAR28, TableCAR29, TableCAR30)
View(TableCAR)
TableLintner2[which(TableLintner2$Number_Year==c(18,19)),]

for (i in 1:30) {
  assign(paste0("Year",i),get(paste("TableCAR",i,sep=""))$Number_Year)
  assign(paste0("TableCAR_UPD",i),get(paste("TableLintner",i,sep=""))[which(get(paste("TableLintner",i,sep=""))$Number_Year %in% c(get(paste("Year",i,sep=""))+2,
                                                                                                                                   get(paste("Year",i,sep=""))+1,
                                                                                                                                   get(paste("Year",i,sep="")),
                                                                                                                                   get(paste("Year",i,sep=""))-1, 
                                                                                                                                   get(paste("Year",i,sep=""))-2)),])
}

View(TableCAR_UPD2)
TableCAR_UPD21 #тут было два выброса, удалим до одного выброса
TableCAR_UPD21_new <- dplyr::filter(TableCAR_UPD21, TableCAR_UPD21$Number_Year %in% c(15,16,17,18,19))

TableCAR_UPD <- rbind(TableCAR_UPD2,TableCAR_UPD5,TableCAR_UPD7,TableCAR_UPD9,TableCAR_UPD13,
                      TableCAR_UPD16,TableCAR_UPD18,TableCAR_UPD21_new,TableCAR_UPD22,TableCAR_UPD23,
                      TableCAR_UPD24,TableCAR_UPD28,TableCAR_UPD29,TableCAR_UPD30)
#найдем компании, которые представлены не 5-ю годами
View(TableCAR_UPD)
names(TableCAR_UPD)
Table21 <- unique(TableCAR_UPD %>% group_by(Number_Company) %>% transmute(Number_Company, NumberYears = sum(Number_Company>0)))
#удалим компании 29 и 9 из рассмотрения

TableCAR_total <- dplyr::filter(TableCAR_UPD, !(TableCAR_UPD$Number_Company %in% c(9,29)))

#Как в Grullon & Michaely & Swaminathan, 2002 рассмотрим медианные PayOut по выбросу, до выброса и после
#в 0 году
median(TableCAR0$PayoutRatio)
#найдем нулевой год в каждой выборке компаний
Table22 <- TableCAR_total %>% group_by(Number_Company) %>% summarise(Years = mean(Number_Year))
TableCAR0 <- dplyr::filter(TableCAR, !(TableCAR$Number_Company %in% c(9,29)))
Table23 <- TableCAR_total %>% group_by(Number_Company) %>% summarise(Years = mean(Number_Year)+1)
Table24 <- TableCAR_total %>% group_by(Number_Company) %>% summarise(Years = mean(Number_Year)+2)
Table25 <- TableCAR_total %>% group_by(Number_Company) %>% summarise(Years = mean(Number_Year)-1)
Table26 <- TableCAR_total %>% group_by(Number_Company) %>% summarise(Years = mean(Number_Year)-2)

View(Table23)
str(Table22)
Table22$Year_0 <- rep(c("0"), 12)
Table22$Concatenate <- paste(Table22$Number_Company, Table22$Years, sep=" ")
Table23$Year_plus1 <- rep(c("1"), 12)
Table23$Concatenate <- paste(Table23$Number_Company, Table23$Years, sep=" ")
Table24$Year_plus2 <- rep(c("2"),12)
Table24$Concatenate <- paste(Table24$Number_Company, Table24$Years, sep=" ")
Table25$Year_minus1 <- rep(c("-1"),12)
Table25$Concatenate <- paste(Table25$Number_Company, Table25$Years, sep=" ")
Table26$Year_minus2 <- rep(c("-2"),12)
Table26$Concatenate <- paste(Table26$Number_Company, Table26$Years, sep=" ")

TableCAR_total$Concatenate <- paste(TableCAR_total$Number_Company, TableCAR_total$Number_Year, sep=" ")

#тут произошла идентификация лет
TableCAR_0_total <- merge(Table22, TableCAR_total, by = "Concatenate")
TableCAR_plus1_total <- merge(Table23, TableCAR_total, by = "Concatenate")
TableCAR_plus2_total <- merge(Table24, TableCAR_total, by = "Concatenate")
TableCAR_minus1_total <- merge(Table25, TableCAR_total, by = "Concatenate")

TableCAR_minus2_total <- merge(Table26, TableCAR_total, by = "Concatenate")

#в рассмотрении осталось 12 компаний AMGN, CAT, CSCO, HON, JNJ, KO, MRK, MSFT, NKE, PG, VZ, WMT

return_CAR_regressor <- data.frame(yearlyReturn(AMGN$AMGN.Close-market$GSPC.Close), 
                         yearlyReturn(CAT$CAT.Close-market$GSPC.Close), 
                         yearlyReturn(CSCO$CSCO.Close-market$GSPC.Close), 
                         yearlyReturn(HON$HON.Close-market$GSPC.Close), 
                         yearlyReturn(JNJ$JNJ.Close-market$GSPC.Close), 
                         yearlyReturn(KO$KO.Close-market$GSPC.Close), 
                         yearlyReturn(MRK$MRK.Close-market$GSPC.Close), 
                         yearlyReturn(MSFT$MSFT.Close-market$GSPC.Close), 
                         yearlyReturn(NKE$NKE.Close-market$GSPC.Close), 
                         yearlyReturn(PG$PG.Close-market$GSPC.Close), 
                         yearlyReturn(VZ$VZ.Close-market$GSPC.Close), 
                         yearlyReturn(WMT$WMT.Close-market$GSPC.Close)
)

return_CAR <- data.frame(yearlyReturn(AMGN$AMGN.Close), 
                         yearlyReturn(CAT$CAT.Close), 
                         yearlyReturn(CSCO$CSCO.Close), 
                         yearlyReturn(HON$HON.Close), 
                         yearlyReturn(JNJ$JNJ.Close), 
                         yearlyReturn(KO$KO.Close), 
                         yearlyReturn(MRK$MRK.Close), 
                         yearlyReturn(MSFT$MSFT.Close), 
                         yearlyReturn(NKE$NKE.Close), 
                         yearlyReturn(PG$PG.Close), 
                         yearlyReturn(VZ$VZ.Close), 
                         yearlyReturn(WMT$WMT.Close)
)



return_CAR$Number_Year <- c(10:21)
return_CAR_regressor$Number_Year <- c(10:21)
colnames(return_CAR) <- c("AMGN", "CAT", "CSCO", "HON", "JNJ", "KO", "MRK", "MSFT", "NKE", "PG", "VZ", "WMT", "Number_Year")
colnames(return_CAR_regressor) <- c("AMGN", "CAT", "CSCO", "HON", "JNJ", "KO", "MRK", "MSFT", "NKE", "PG", "VZ", "WMT", "Number_Year")

return_CAR1 <- return_CAR_regressor %>% pivot_longer(AMGN:WMT, names_to = "ID", values_to = "returns")
return_CAR2 <- merge(return_CAR1, names_data, by = "ID")
return_CAR2$Concatenate <- paste(return_CAR2$Number_Company, return_CAR2$Number_Year, sep=" ")

#готовые штуки - return_CAR2 и TableCAR_0_total , TableCAR_plus1_total , TableCAR_plus2_total , TableCAR_minus1_total , TableCAR_minus2_total 
TableCAR_0return_total <- merge(return_CAR2, TableCAR_0_total, by = "Concatenate")

TableCAR_plus1return_total <- merge(return_CAR2, TableCAR_plus1_total, by = "Concatenate")
TableCAR_plus2return_total <- merge(return_CAR2, TableCAR_plus2_total, by = "Concatenate")
TableCAR_minus1return_total <- merge(return_CAR2, TableCAR_minus1_total, by = "Concatenate")
TableCAR_minus2return_total <- merge(return_CAR2, TableCAR_minus2_total, by = "Concatenate")

delta_returns_1 <- TableCAR_plus1return_total$returns - TableCAR_minus1return_total$returns
delta_returns_2 <- TableCAR_plus2return_total$returns - TableCAR_minus2return_total$returns

model52 <- lm(TableCAR_0return_total$returns ~ delta_returns_1 + delta_returns_2)
summary(model52)

model53 <- lm(TableCAR_0return_total$returns ~ delta_returns_2)
summary(model53)

bptest(model53)

model57 <- lm(delta_returns_2 ~ TableCAR_0return_total$returns + delta_returns_1)
summary(model57)
market_annual <- yearlyReturn(market$GSPC.Close)
market_annual$Years <- c(10:21)
TableCAR_0return_total2 <- merge(TableCAR_0return_total, market_annual, by = "Years")

model56 <- lm(TableCAR_0return_total2$returns-TableCAR_0return_total2$yearly.returns ~ delta_returns_1 + delta_returns_2)
summary(model56)
#cor(TableCAR_0return_total2$returns, TableCAR_0return_total2$yearly.returns)

stargazer(model57, type = "html", df = FALSE, out = "table11.html", title="Results CAR")

#прикольные, но не понятно зачем нужные штуки
barChart(to.monthly(AAPL),up.col='white',dn.col='blue') 
chartSeries(to.weekly(AAPL),up.col='white',dn.col='blue')

#новый регрессант - это избыточная доходность вокруг даты 
delta_returns_1 <- TableCAR_plus1return_total$returns - TableCAR_minus1return_total$returns
deltaDIV0 <- TableCAR_0return_total$DIV - TableCAR_minus1return_total$DIV
deltaDIV2 <- (TableCAR_plus2return_total$DIV - TableCAR_minus2return_total$DIV)/2

model54 <- lm(delta_returns_1 ~ deltaDIV0 + deltaDIV2)
summary(model54) #модель не очень - уравнение не значимо


#подумать как идентифицировать года +2, +1, -1, -2

#беты для CAPM

ID <- c("AAPL", "AMGN", "BA", "CAT", "CRM", "CSCO", "CVX", "DIS", "HD", "HON", "IBM", 
              "INTC", "JNJ", "KO", "MCD", "MMM", "MRK", "MSFT", "NKE", "PG", "UNH", "V", "VZ", "WBA", "WMT", "market")

beta1 <- CAPM.beta(returns_port1, return_market-rf/365)
shares_data_upd <- merge(AAPL$AAPL.Close, AMGN$AMGN.Close, BA$BA.Close, CAT$CAT.Close, CRM$CRM.Close, 
                     CSCO$CSCO.Close, CVX$CVX.Close, DIS$DIS.Close, HD$HD.Close, HON$HON.Close, 
                     IBM$IBM.Close, INTC$INTC.Close, JNJ$JNJ.Close, KO$KO.Close, MCD$MCD.Close, MMM$MMM.Close, MRK$MRK.Close, 
                     MSFT$MSFT.Close, NKE$NKE.Close, PG$PG.Close, UNH$UNH.Close, V$V.Close, 
                     VZ$VZ.Close, WBA$WBA.Close, WMT$WMT.Close, market$GSPC.Close)
return_upd <- Return.calculate(shares_data_upd)
return_upd1 <- na.omit(return_upd)
str(return_upd)
risk_free_upd <- rep(rf, dim(return_upd1)[1])
for (i in 1:26) {
  assign(paste0("beta", i), CAPM.beta(return_upd1[,i], return_upd1[,26]-risk_free_upd/365))
  assign(paste0("return_CAPM", i), unique(Return.annualized(get(paste("beta", i, sep=""))*(return_upd1[,26]-risk_free_upd/365)+risk_free_upd/365)))
  assign(paste0("return_current", i), Return.annualized(return_upd1[,i]))
  assign(paste0("alpha", i), get(paste("return_current", i, sep = ""))-get(paste("return_CAPM", i, sep = "")))
}
beta <- c(beta1, beta2, beta3, beta4, beta5, beta6, beta7, beta8, beta9, beta10, beta11, beta12, 
          beta13, beta14, beta15, beta16, beta17, beta18, beta19, beta20, beta21, beta22, beta23, 
          beta24, beta25, beta26)
return_CAPM <- c(return_CAPM1, return_CAPM2, return_CAPM3, return_CAPM4, return_CAPM5, return_CAPM6, return_CAPM7, 
                 return_CAPM8, return_CAPM9, return_CAPM10, return_CAPM11, return_CAPM12, return_CAPM13, return_CAPM14, 
                 return_CAPM15, return_CAPM16, return_CAPM17, return_CAPM18, return_CAPM19, return_CAPM20, return_CAPM21, 
                 return_CAPM22, return_CAPM23, return_CAPM24, return_CAPM25, return_CAPM26)
return_current <- c(return_current1, return_current2, return_current3, return_current4, return_current5, return_current6, return_current7, 
                    return_current8, return_current9, return_current10, return_current11, return_current12, return_current13, return_current14, 
                    return_current15, return_current16, return_current17, return_current18, return_current19, return_current20, return_current21, 
                    return_current22, return_current23, return_current24, return_current25, return_current26)

CAPM_model <- data.frame(as.data.frame(ID), as.data.frame(beta), as.data.frame(return_CAPM), as.data.frame(return_current))
CAPM_model$alpha <- CAPM_model$return_current - CAPM_model$return_CAPM

names(Table13)
names(names_data)
CAPM_model1 <- merge(CAPM_model, names_data, by = "ID")
CAPM_model2 <- merge(CAPM_model1, Table13, by = "ID_Company")
#построим график 

Chart126 <- ggplot(data = CAPM_model2, aes(x = beta, y = return_current, color = Result)) + geom_point(aes(size = 5)) + theme_bw() + theme(legend.position = "bottom") + 
  scale_color_manual(labels = c("Спорные", "Устойчиво зрелые", "Устойчиво незрелые"), values = c("#fa5c18", "#ffd881", "#7189bf")) + 
  geom_label(aes(label = ID)) + ylab("Годовая доходность в долях") + xlab("Бета-коэффициент акций компании") + 
  ggtitle("Карта доходности и бета-коэффициента по компаниям DowJones") + labs(subtitle="Посчитано по 2010-2021 гг.")
library(ggrepel)
Chart126 + geom_line(data=as.data.frame(CAPM_model2),aes(x=beta,y=return_CAPM), size = 3, color = "darkblue", label = "SML") 

#проверка подсчетов - всё ок!
model110 <- lm(data = testdrive, testdrive$`AAPL UW Equity`-I(rf/365) ~ I(return_market[-1,]-rf/365))
summary(model110)
beta1

#построим альфы
Chart127 <- ggplot(data = CAPM_model2, aes(x = return_current, y = alpha, color = Result)) + geom_rect(data=CAPM_model2, inherit.aes=FALSE, aes(xmin=min(CAPM_model2$return_current), xmax=max(CAPM_model2$return_current), ymin=0,
                                                                                                                                                ymax=max(CAPM_model2$alpha)), fill = "lightblue", alpha=0.1) +  geom_point(aes(size = 5)) + theme_bw() + theme(legend.position = "bottom") + 
  scale_color_manual(labels = c("Спорные", "Устойчиво зрелые", "Устойчиво незрелые"), values = c("#fa5c18", "#ffd881", "#7189bf")) + 
  geom_label(aes(label = ID)) + ylab("Недооцененность / Переоцененность") + xlab("Годовая доходность в долях") + 
  ggtitle("Карта текущей доходности и характеристик оценки по компаниям DowJones") + labs(subtitle="Посчитано по 2010-2021 гг.; недооцененость / переоцененость по модели CAPM") + 
  annotate("text", x = 0.15, y=0.1, label = "Недооцененные 
компании по CAPM") + 
  annotate("text", x = 0.15, y=-0.05, label = "Переоцененные 
компании по CAPM")
Chart127

CAPM_model2$valuation <- ifelse(CAPM_model2$alpha > 0, "Недооценена", "Переоценена")
View(CAPM_model2)
seq(1,300,12)
CAPM_model3 <- CAPM_model2[c(seq(1,300,12)),]
CAPM_model3$tech <- rep(1,25)
names(CAPM_model3)
CAPM_model4 <- CAPM_model3 %>% group_by(Result, valuation) %>% summarise(sum = sum(tech))
ggplot(data = CAPM_model4, aes(x = Result, y = sum)) + geom_col(aes(fill = valuation), color = "#1D323F")+ theme_bw() + ylab("Количество компаний") + theme(legend.position = "bottom") + xlab("") + scale_fill_manual(values = c("#2D5D62", "#77A8A3")) +
  scale_x_discrete(labels=c("Sustainable NonMature" = "Устойчиво незрелые", "Sustainable Mature" = "Устойчиво зрелые",
                            "Controversial" = "Спорные")) + geom_label(aes(group = valuation, label=sum), fill = "white", position = position_stack(vjust = 0.5)) + ggtitle("Распределение компаний по характеристикам жизненного цикла и CAPM") +coord_flip()
CAPM_model4
#§8 - Построение по всем статьям 
Data_comb <- transmute(DataSet2, 
                      Stage_Indicator1 = Stage_Indicator1, 
                      REtoTA = RE_period / BS_Total_Assets , 
                      TEtoTA = Total_Equity / BS_Total_Assets , 
                      ROA = ROA, 
                      SGR = SGR$SGR, #из отдельного дата-сета
                      AGR = AGR, 
                      CashtoTA = Cash / BS_Total_Assets, 
                      DIV = DIV,
                      PayoutRatio = PayoutRatio, 
                      Percentile = Percentile$Percentile, 
                      ROS = IS_Operating_Income/Sales, 
                      Firm_size = log(BS_Total_Assets), 
                      Tax_Rate = Tax_Rate, 
                      DivToTA = Dividends/BS_Total_Assets, 
                      DebtToAssets=(BS_LT_Borrow+BS_ST_Borrow)/BS_Total_Assets, 
                      ProfToAssets = Net_Income/BS_Total_Assets, 
                      DivToSales = Dividends/Sales
                      
)

Data_comb1 <- na.omit(Data_comb)
model44 <- lm(data = Data_comb1, DIV ~ . - DivToSales - PayoutRatio - DivToTA-ProfToAssets) #ROA и ProfToAssets - большая мультиколлинеарность
summary(model44)
vif(model44)

model45 <- lm(data = Data_comb1, DivToSales ~ . - DIV - PayoutRatio - DivToTA-ProfToAssets) #ROA и ProfToAssets - большая мультиколлинеарность
summary(model45)

model46 <- lm(data = Data_comb1, PayoutRatio ~ . - DIV - DivToSales - DivToTA-ProfToAssets) #ROA и ProfToAssets - большая мультиколлинеарность
summary(model46)

model47 <- lm(data = Data_comb1, DivToTA ~ . - DIV - DivToSales - PayoutRatio-ProfToAssets) #ROA и ProfToAssets - большая мультиколлинеарность
summary(model47)

stargazer(model44, model45, model46, model47, type = "html", df = FALSE, se = list(cse(model44), cse(model45), cse(model46), cse(model47)), out = "table11.html", title="Results Total")

bptest(model44)
bptest(model45)
bptest(model46)
bptest(model47)

stepAIC(model44)
stepAIC(model45)
stepAIC(model46)
stepAIC(model47)
model48 <- update(model44, .~.-ROA -DebtToAssets - SGR - CashtoTA - AGR - Tax_Rate )
model49 <- update(model45, .~.-Firm_size-Stage_Indicator1-Percentile-CashtoTA)
model50 <- update(model46, .~.-TEtoTA-Stage_Indicator1-CashtoTA-Percentile-SGR-DebtToAssets-REtoTA)
model51 <- lm(DivToTA ~ ROS+SGR+AGR+DebtToAssets+ROA+REtoTA, data = Data_comb1)

bptest(model48)
bptest(model49)
bptest(model50)
bptest(model51)

stargazer(model48, model49, model50, model51, type = "html", df = FALSE, se = list(cse(model48), cse(model49), cse(model50), cse(model51)), out = "table12.html", title="Results Total")
stargazer(model48, model49, model50, model51, type = "html", df = FALSE, out = "table12_1.html", title="Results Total")

#9 - Модель распределенных лагов
names(DataSet2)
dr <- DataSet2$DIV
dr1 <- ts(dr, start = c(2000,1), frequency = 12)
View(dr1)
mod_dyn <- dynlm(dr1~ L(dr1, 1:2))
adf.test(dr)
summary(mod_dyn)

mr <- DataSet2$PayoutRatio
adf.test(mr)

for (i in 1:30){
  assign(paste0("AR_Table", i), dplyr::filter(DataSet2, Number_Company == i))
  assign(paste0("AR_Table_DIV", i), dplyr::select(get(paste("AR_Table", i, sep="")),DIV))
}

AR_Table_DIV_TS1 <- ts(AR_Table_DIV1, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS2 <- ts(AR_Table_DIV2, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS3 <- ts(rep(0,12), start = c(2010,1), frequency = 1)
AR_Table_DIV_TS4 <- ts(AR_Table_DIV4, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS5 <- ts(AR_Table_DIV5, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS6 <- ts(AR_Table_DIV6, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS7 <- ts(AR_Table_DIV7, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS8 <- ts(AR_Table_DIV8, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS9 <- ts(AR_Table_DIV9, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS10 <- ts(rep(0,12), start = c(2010,1), frequency = 1)
AR_Table_DIV_TS11 <- ts(rep(0,12), start = c(2010,1), frequency = 1)
AR_Table_DIV_TS12 <- ts(AR_Table_DIV12, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS13 <- ts(AR_Table_DIV13, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS14 <- ts(AR_Table_DIV14, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS15 <- ts(AR_Table_DIV15, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS16 <- ts(AR_Table_DIV16, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS17 <- ts(rep(0,12), start = c(2010,1), frequency = 1)
AR_Table_DIV_TS18 <- ts(AR_Table_DIV18, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS19 <- ts(AR_Table_DIV19, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS20 <- ts(AR_Table_DIV20, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS21 <- ts(AR_Table_DIV21, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS22 <- ts(AR_Table_DIV22, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS23 <- ts(AR_Table_DIV23, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS24 <- ts(AR_Table_DIV24, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS25 <- ts(rep(0,12), start = c(2010,1), frequency = 1)
AR_Table_DIV_TS26 <- ts(AR_Table_DIV26, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS27 <- ts(AR_Table_DIV27, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS28 <- ts(AR_Table_DIV28, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS29 <- ts(AR_Table_DIV29, start = c(2010,1), frequency = 1)
AR_Table_DIV_TS30 <- ts(AR_Table_DIV30, start = c(2010,1), frequency = 1)

for (i in 1:30){
  assign(paste0("mod_dyn", i), dynlm(get(paste("AR_Table_DIV_TS", i, sep=""))~ L(get(paste("AR_Table_DIV_TS", i, sep="")), 1:2)))
}

summary(mod_dyn5) #не везде есть значимое влияние первого лага: mod_dyn4

stargazer(mod_dyn1, mod_dyn2, mod_dyn4, mod_dyn5, mod_dyn7, type = "html", out = "dyn1.html")
stargazer(mod_dyn8, mod_dyn9, mod_dyn12, mod_dyn13, mod_dyn14, mod_dyn15, mod_dyn16, type = "html", out = "dyn2.html")
stargazer(mod_dyn18, mod_dyn19, mod_dyn20, mod_dyn21, mod_dyn22, mod_dyn23, type = "html", out = "dyn3.html")
stargazer(mod_dyn24, mod_dyn26, mod_dyn27, mod_dyn28, mod_dyn29, mod_dyn30, type = "html", out = "dyn4.html")

stargazer(mod_dyn18, mod_dyn19, mod_dyn20, mod_dyn21, mod_dyn22, mod_dyn23, mod_dyn24, mod_dyn26, mod_dyn27, mod_dyn28, mod_dyn29, mod_dyn30, type = "html", out = "dyn.total.html")

for (i in 1:30){
  assign(paste0("AR_Table_PR", i), dplyr::select(get(paste("AR_Table", i, sep="")),PayoutRatio))
}

AR_Table_PR_TS1 <- ts(AR_Table_PR1, start = c(2010,1), frequency = 1)
AR_Table_PR_TS2 <- ts(AR_Table_PR2, start = c(2010,1), frequency = 1)
AR_Table_PR_TS3 <- ts(rep(0,12), start = c(2010,1), frequency = 1)
AR_Table_PR_TS4 <- ts(AR_Table_PR4, start = c(2010,1), frequency = 1)
AR_Table_PR_TS5 <- ts(AR_Table_PR5, start = c(2010,1), frequency = 1)
AR_Table_PR_TS6 <- ts(AR_Table_PR6, start = c(2010,1), frequency = 1)
AR_Table_PR_TS7 <- ts(AR_Table_PR7, start = c(2010,1), frequency = 1)
AR_Table_PR_TS8 <- ts(AR_Table_PR8, start = c(2010,1), frequency = 1)
AR_Table_PR_TS9 <- ts(AR_Table_PR9, start = c(2010,1), frequency = 1)
AR_Table_PR_TS10 <- ts(rep(0,12), start = c(2010,1), frequency = 1)
AR_Table_PR_TS11 <- ts(rep(0,12), start = c(2010,1), frequency = 1)
AR_Table_PR_TS12 <- ts(AR_Table_PR12, start = c(2010,1), frequency = 1)
AR_Table_PR_TS13 <- ts(AR_Table_PR13, start = c(2010,1), frequency = 1)
AR_Table_PR_TS14 <- ts(AR_Table_PR14, start = c(2010,1), frequency = 1)
AR_Table_PR_TS15 <- ts(AR_Table_PR15, start = c(2010,1), frequency = 1)
AR_Table_PR_TS16 <- ts(AR_Table_PR16, start = c(2010,1), frequency = 1)
AR_Table_PR_TS17 <- ts(rep(0,12), start = c(2010,1), frequency = 1)
AR_Table_PR_TS18 <- ts(AR_Table_PR18, start = c(2010,1), frequency = 1)
AR_Table_PR_TS19 <- ts(AR_Table_PR19, start = c(2010,1), frequency = 1)
AR_Table_PR_TS20 <- ts(AR_Table_PR20, start = c(2010,1), frequency = 1)
AR_Table_PR_TS21 <- ts(AR_Table_PR21, start = c(2010,1), frequency = 1)
AR_Table_PR_TS22 <- ts(AR_Table_PR22, start = c(2010,1), frequency = 1)
AR_Table_PR_TS23 <- ts(AR_Table_PR23, start = c(2010,1), frequency = 1)
AR_Table_PR_TS24 <- ts(AR_Table_PR24, start = c(2010,1), frequency = 1)
AR_Table_PR_TS25 <- ts(rep(0,12), start = c(2010,1), frequency = 1)
AR_Table_PR_TS26 <- ts(AR_Table_PR26, start = c(2010,1), frequency = 1)
AR_Table_PR_TS27 <- ts(AR_Table_PR27, start = c(2010,1), frequency = 1)
AR_Table_PR_TS28 <- ts(AR_Table_PR28, start = c(2010,1), frequency = 1)
AR_Table_PR_TS29 <- ts(AR_Table_PR29, start = c(2010,1), frequency = 1)
AR_Table_PR_TS30 <- ts(AR_Table_PR30, start = c(2010,1), frequency = 1)

for (i in 1:30){
  assign(paste0("mod_dyn_PR", i), dynlm(get(paste("AR_Table_PR_TS", i, sep=""))~ L(get(paste("AR_Table_PR_TS", i, sep="")), 1:2)))
}

stargazer(mod_dyn_PR1, mod_dyn_PR2, mod_dyn_PR4, mod_dyn_PR5, mod_dyn_PR7, type = "html", out = "dyn_PR1.html")
stargazer(mod_dyn_PR8, mod_dyn_PR9, mod_dyn_PR12, mod_dyn_PR13, mod_dyn_PR14, mod_dyn_PR15, mod_dyn_PR16, type = "html", out = "dyn_PR2.html")
stargazer(mod_dyn_PR18, mod_dyn_PR19, mod_dyn_PR20, mod_dyn_PR21, mod_dyn_PR22, mod_dyn_PR23, type = "html", out = "dyn_PR3.html")
stargazer(mod_dyn_PR24, mod_dyn_PR26, mod_dyn_PR27, mod_dyn_PR28, mod_dyn_PR29, mod_dyn_PR30, type = "html", out = "dyn_PR4.html")

for (i in 1:30){
  assign(paste0("AR_Table_Cycle", i), dplyr::select(get(paste("AR_Table", i, sep="")),DIV, Stage_Indicator1))
}

AR_Table_Cycle_TS1 <- ts(AR_Table_Cycle1, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS2 <- ts(AR_Table_Cycle2, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS3 <- ts(rep(0,12), start = c(2010,1), frequency = 1)
AR_Table_Cycle_TS4 <- ts(AR_Table_Cycle4, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS5 <- ts(AR_Table_Cycle5, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS6 <- ts(AR_Table_Cycle6, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS7 <- ts(AR_Table_Cycle7, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS8 <- ts(AR_Table_Cycle8, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS9 <- ts(AR_Table_Cycle9, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS10 <- ts(rep(0,12), start = c(2010,1), frequency = 1)
AR_Table_Cycle_TS11 <- ts(rep(0,12), start = c(2010,1), frequency = 1)
AR_Table_Cycle_TS12 <- ts(AR_Table_Cycle12, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS13 <- ts(AR_Table_Cycle13, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS14 <- ts(AR_Table_Cycle14, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS15 <- ts(AR_Table_Cycle15, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS16 <- ts(AR_Table_Cycle16, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS17 <- ts(rep(0,12), start = c(2010,1), frequency = 1)
AR_Table_Cycle_TS18 <- ts(AR_Table_Cycle18, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS19 <- ts(AR_Table_Cycle19, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS20 <- ts(AR_Table_Cycle20, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS21 <- ts(AR_Table_Cycle21, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS22 <- ts(AR_Table_Cycle22, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS23 <- ts(AR_Table_Cycle23, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS24 <- ts(AR_Table_Cycle24, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS25 <- ts(rep(0,12), start = c(2010,1), frequency = 1)
AR_Table_Cycle_TS26 <- ts(AR_Table_Cycle26, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS27 <- ts(AR_Table_Cycle27, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS28 <- ts(AR_Table_Cycle28, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS29 <- ts(AR_Table_Cycle29, start = c(2010,1), frequency = 1)[,2]
AR_Table_Cycle_TS30 <- ts(AR_Table_Cycle30, start = c(2010,1), frequency = 1)[,2]

AR_Table_Cycle_TS30

for (i in 1:30){
  assign(paste0("mod_dyn_Cycle", i), dynlm(get(paste("AR_Table_DIV_TS", i, sep=""))~ L(get(paste("AR_Table_Cycle_TS", i, sep="")), 0:2)))
}

stargazer(mod_dyn_Cycle1, mod_dyn_Cycle2, mod_dyn_Cycle4, mod_dyn_Cycle5, mod_dyn_Cycle7, type = "html", out = "dyn_Cycle1.html")
stargazer(mod_dyn_Cycle8, mod_dyn_Cycle9, mod_dyn_Cycle12, mod_dyn_Cycle13, mod_dyn_Cycle14, mod_dyn_Cycle15, mod_dyn_Cycle16, type = "html", out = "dyn_Cycle2.html")
stargazer(mod_dyn_Cycle18, mod_dyn_Cycle19, mod_dyn_Cycle20, mod_dyn_Cycle21, mod_dyn_Cycle22, mod_dyn_Cycle23, type = "html", out = "dyn_Cycle3.html")
stargazer(mod_dyn_Cycle24, mod_dyn_Cycle26, mod_dyn_Cycle27, mod_dyn_Cycle28, mod_dyn_Cycle29, mod_dyn_Cycle30, type = "html", out = "dyn_Cycle4.html")


#поиск оптимального числа лагов
str(AR_Table_DIV_TS1)


#10 - Метод главных компонент
#найдем сильно зависимые переменные - иначе PCA не сработает
?qgraph
names(DataSet2)
#обозначим ключевые переменные для метода главных компонент
PCA_mod <- DataSet2[,c(6,7,8,9,11,12,14,15,16,17,18,20,28,29,31,32,33,34,36,41,42,43,44,45,57)]
str(PCA_mod)
qgraph(cor(as.matrix(PCA_mod)))
qgraph(cor(as.matrix(PCA_mod)), minimum = 0.5, cut = 0.7, vsize = 10)
qgraph(cor(as.matrix(PCA_mod)), theme = "Borkulo", minimum = 0.4, cut = 0.7, vsize = 10)

#удалим переменные: BS_ST_Borrow (14), PE_ratio (20), AGR (17), PayoutRatio (19), WACC (18), Working_Capital (23)
PCA_mod1 <- PCA_mod[,-c(14,20,17,19,18,23)]
qgraph(cor(as.matrix(PCA_mod1)), theme = "Borkulo", minimum = 0.4, cut = 0.7, vsize = 10)

#начинаем строить
PCA_mod2 <- PCA(PCA_mod1)
#две компоненты передают 51% информации, хорошо переданы такие переменные, как Z-score, ROA, Board_Size, DIV

#достанем матрицу нагрузок - то есть то, как переменные задаютсяя через главные компоненты
View(PCA_mod2$var$coord) #каждое число в строчке отражает корреляцию с главной компонентой

#график каменистой осыпи - впринципе можно использовать 2/3 кластера 
fviz_eig(PCA_mod2)

#для интепретации знака визуализируем матрицу нагрузок
corrplot(PCA_mod2$var$coord, is.corr=FALSE) #возьму три компоненты

#для интерпретации переменных в главных компонентах 
corrplot(PCA_mod2$var$cos2, is.corr=FALSE)

#первые компании - генерирующие большие денежные потоки, при этом ещё активно реинвестирующие в себя. Но опять же возникает вопрос. Компании, которые зрелые просто по объему могут больше тратить на поддержание => стандартизируем показатели денежного потока на выручку
#Capex / Sales, CF_DA / Sales, CF_FCFF / Sales, FCFE / Sales
PCA_mod3 <- mutate(PCA_mod1, CapextoSales = CAPEX / Sales, DAtoSales = CF_DA / Sales, 
                                    FCFFtoSales = CF_FCFF / Sales, FCFEtoSales = FCFE / Sales)
View(PCA_mod3)
names(PCA_mod3)
PCA_mod4 <- PCA_mod3[,-c(13,14,15,17)]

PCA_mod5 <- PCA(PCA_mod4) #компоненты стали меньше инфо передавать - 40%
#для интерпретации переменных в главных компонентах 
corrplot(PCA_mod5$var$cos2, is.corr=FALSE)

#как-то больше нравилось PCA_mod2
#сравниваем наблюдения
fviz_pca_ind(PCA_mod2, repel = TRUE, pointsize = "cos2")
#какая-то дичь, надо сравнивать всё-таки по компаниям, а не по наблюдениям 

names(DataSet2)
Mmm <- ModelForPCA[,-1]
qgraph(cor(as.matrix(Mmm)), theme = "Borkulo", minimum = 0.1, cut = 0.7, vsize = 10)
cor(Mmm$FCFEtoSales, Mmm$Cash_Ratio)
ModelForPCA <- DataSet2 %>% group_by(Number_Company) %>% summarise(DIV = median(DIV), 
                                                                   ROA = median(ROA), 
                                                                   FCFEtoSales = median(FCFE/Sales), 
                                                                   AssetsToEquity = median(AssetsToEquity), 
                                                                   CAPEXtoSales = median(CAPEX/Sales), 
                                                                   Stage_Indicator1 = median(Stage_Indicator1), 
                                                                   Board_Size = median(Board_Size), 
                                                                   Independent_Directors = median(Independent_Directors), 
                                                                   Cash_Ratio = median(Cash_Ratio)
)
names(ModelForPCA)
names(names_data)
ModelForPCA1 <- merge(names_data[,c(1,3)], ModelForPCA, by = "Number_Company")
rownames(ModelForPCA1) <- names_data[,3]
?PCA
modPCA <- PCA(ModelForPCA1[,-c(1,2)])#передача 49% информации
stargazer(modPCA$var$coord, summary = FALSE, type = "html", out = "table15.html")
fviz_eig(modPCA, barfill = "darkblue", xlab = "Номер компоненты", ylab = "Процент объясняемой информации", main = "График каменистой осыпи") + theme_bw() #остановилась бы на трех компонентах

corrplot(modPCA$var$coord, is.corr=FALSE)
corrplot(modPCA$var$cos2, is.corr=FALSE)

fviz_pca_ind(modPCA, repel = TRUE, col.ind = "cos2", pointsize = "cos2") + theme_bw() +
  scale_color_gradient2(low="#fa448c", mid="#43b5a0",
                        high="#491d88", midpoint=0.2)

modPCA_clust  <- HCPC(modPCA)
#строим дендрограмму
fviz_dend(modPCA_clust, rect_border = c("#E7DED7","#B6C6D5"), rect = TRUE, rect_fill = TRUE, cex = 0.8, palette = c("#664845", "#003986"))
library(ape)
hcpcTree<-modPCA_clust$call$t$tree
apeTree<-as.phylo(hcpcTree)
colors <- c("red", "blue", "green", "black")
plot(apeTree, type = "fan", label.offset = 0.2, cex = 0.7, tip.color = colors) 

#визуализируем кластеры 
fviz_cluster(modPCA_clust, repel = TRUE) + theme_bw() + scale_color_manual(values = c("#8e064e", "#172b69")) + scale_fill_manual(values = c("#8e064e", "#172b69"))
modPCA_clust$desc.var

stargazer(modPCA_clust$desc.var$quanti$`1`, type = "html", summary = FALSE, out = "table17.html")
stargazer(modPCA_clust$desc.var$quanti$`2`, type = "html", summary = FALSE, out = "table18.html")

modPCA_clust1 <- hclust(dist(scale(ModelForPCA)))
flexclust::barchart(modPCA_clust1, ModelForPCA)

#красивые графики. Исходный датасет - DataSet2

data_for_graphs <- DataSet2
View(DataSet2)
data_for_graphs$Year <- rep(c(2010:2021), 25)

data_for_graphs1 <- merge(names_data, data_for_graphs, by = "Number_Company")


Chart90 <- ggplot(data_for_graphs1, aes(CF_FCFF, Sales, frame = Year, size = DIV, fill = ID, color = ID)) +
  theme_bw() + geom_text(aes(label = ID), check_overlap = TRUE) + xlab("FCFF") + ylab("Выручка") + labs(fill = "Компания") + scale_color_manual(values = c("darkblue", "aquamarine2", "azure3", "black", "blue2", 
                                                                                                                                                          "blueviolet", "brown2", "burlywood2", "chocolate1", "cornflowerblue",
                                                                                                                                                          "cyan2", "darkgoldenrod1", "darkmagenta", "firebrick1", "deeppink", 
                                                                                                                                                          "green2", "indianred2", "lightcoral", "mediumorchid2", "mistyrose1",                                                                                                                                                "mediumspringgreen", "orangered1", "purple4", "peru", "seagreen"))
ggplotly(Chart90)
names(data_for_graphs1)

#показатели P&L по компаниям
dataChart91 <- data_for_graphs1 %>% pivot_longer(cols = c("Sales", "EBITDA", "EBIT", "RE"), names_to = "PLstatement", values_to = "value")
Chart91 <- ggplot(dataChart91, aes(Year, value, fill = PLstatement)) +
  geom_bar(stat="identity", position=position_dodge()) + theme_bw() +
  facet_grid(~factor(Number_Company, levels = c(1,2,4,5,6,7)))

View(dataChart91)

DataSet2[1,]
for (i in 1:30){
  assign(paste0("dataChart", i), dplyr::filter(dataChart91, Number_Company == i))
  assign(paste0("Chart91_", i), ggplot(data = get(paste("dataChart", i, sep="")), aes(Year, value, fill = PLstatement))+
           xlab("") + ylab("") + theme(legend.position = "bottom") + labs(fill = "") + ggtitle(paste0("Компания № ", i)) + 
           geom_bar(stat="identity", position=position_dodge()) + theme_bw()+scale_fill_manual(values = c("#c6d3fa", "#7387c3", "#bcacdb", "#28398d")))
}

grid.arrange(Chart91_1, Chart91_2, Chart91_4, Chart91_5)
grid.arrange(Chart91_6, Chart91_7, Chart91_8, Chart91_9)
grid.arrange(Chart91_12, Chart91_13, Chart91_14, Chart91_15)
grid.arrange(Chart91_16, Chart91_18, Chart91_19, Chart91_20)
grid.arrange(Chart91_21, Chart91_22, Chart91_23, Chart91_24)
grid.arrange(Chart91_26, Chart91_27, Chart91_28, Chart91_29, Chart91_30)

names_data$ID
industry <- as.data.frame(c("Software", "Biotech", "Aircraft", "Machinery", "Software", "Communication", "Oil", "Leisure", "Retail", 
              "Conglomerates", "IT", "IT", "Pharmaceuticals", "Leisure", "Leisure", "Conglomerates", "Pharmaceuticals", 
              "Software", "Retail", "Retail", "Retail", "IT", "Communication", "Retail", "Retail"))

industry_match <- cbind(names_data$ID, industry)
colnames(industry_match) <- c("ID", "industry")
table(industry_match$industry)

data_for_graphs2 <- merge(industry_match, data_for_graphs1, by = "ID")

#разбивка по отраслям
Chart92 <- ggplot(data = industry_match, aes(x = industry, fill = industry)) + geom_bar(color = "black") + 
  theme_bw() + theme(legend.position = "bottom") + scale_fill_manual(values = c("#F0F0F8", "#B8E1D9", "#5EBAA6", "#237C6C", "#2D554A", "#314448", "#536d6c", "#7c9a92", "#c7d3bf", "#e0dab8", "#275362", "#407a61")) + ylab("Количество компаний в определенной отрасли") + labs(fill = "Отрасль") + xlab("Отрасль") + 
  ggtitle("Распределение компаний выборки по отраслям")
Chart92

#дивиденды по отраслям
View(data_for_graphs2)
View(dplyr::filter(data_for_graphs2, data_for_graphs2$DIV == 0))
data_for_graphs3 <- data_for_graphs2 %>% dplyr::filter(data_for_graphs2$Number_Year == 21) %>% group_by(industry) %>% transmute(DIV = median(DIV), Dividends = median(Dividends))
data_for_graphs3 <- as.data.frame(data_for_graphs3)
data_for_graphs4 <- unique(data_for_graphs3)
library(ggstatsplot)
paletter_vector <-
  paletteer::paletteer_d(
    palette = "palettetown::venusaur",
    n = nlevels(as.factor(data_for_graphs4$industry)),
    type = "discrete"
  )

g_1 <- ggdotplotstats(
  data       = data_for_graphs4,
  x          = DIV,
  y          = industry,
  title      = "Медианное значение дивиденда на одну акцию в 2021 году по отраслям",
  xlab       = "Дивиденд на одну акцию", 
  point.args = list(
    shape = 16,
    size = 5, 
    color = paletter_vector
  )
) + theme_bw()

g_2 <- ggdotplotstats(
  data       = data_for_graphs4,
  x          = Dividends,
  y          = industry,
  title      = "Медианное значение совокупных дивидендных выплат в 2021 году по отраслям",
  xlab       = "Совокупные дивидендные выплаты", 
  point.args = list(
    shape = 16,
    size = 5, 
    color = paletter_vector
  )
) + theme_bw()

grid.arrange(g_1, g_2, nrow = 2)

View(data_for_graphs3)

?ggdotplotstats
names(data_DE4)
Chart128 <- ggplot(data = na.omit(Damodaran), aes(x = IndustryDamodaran, fill = IndustryDamodaran)) + geom_bar(color = "black") + 
  theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = 90)) + ylab("Количество компаний в определенной отрасли") + labs(fill = "Отрасль") + xlab("Отрасль") + 
  ggtitle("Распределение компаний выборки по отраслям") + scale_fill_manual(values = c("#04BFBF", "#ACF0F2", "#0EEAFF", "#59D8E6", "#0092B2", "#00ABD8", "#04668C", "#0288D1", "#7ECEFD", 
                                                                                       "#0067A6", "#012840", "#3498DB", "#ADD5F7", "#00305A", "#7ABAF2", "#4192D9", "#004B8D", 
                                                                                       "#1976D2", "#7FB2F0", "#133463", "#002253", "#1B76FF", "#0D47A1")) 
Chart128

View(Damodaran)
graph_base_1 <- data_for_graphs2 %>% group_by(industry, Year) %>% summarise(PayoutRatio = median(PayoutRatio), DIV = median(DIV))
View(graph_base_1)
#не информативный график
Chart93 <- ggplot(graph_base_1[-c(37:48),], aes(x=industry, y = PayoutRatio, fill = industry)) + geom_violin() + 
  theme_bw() + theme(legend.position = "bottom") + scale_fill_manual(values = c("#F0F0F8", "#B8E1D9", "#5EBAA6", "#2D554A", "#314448", "#536d6c", "#7c9a92", "#c7d3bf", "#e0dab8", "#275362", "#407a61")) + ylab("Коэффициент дивидендных выплат") + labs(fill = "Отрасль") + xlab("")

Chart93

Chart100 <- ggplot(graph_base_1[-c(37:48),], aes(x=industry, y = DIV, fill = industry)) + geom_violin() + 
  theme_bw() + theme(legend.position = "bottom") + scale_fill_manual(values = c("#F0F0F8", "#B8E1D9", "#5EBAA6", "#2D554A", "#314448", "#536d6c", "#7c9a92", "#c7d3bf", "#e0dab8", "#275362", "#407a61")) + ylab("Дивиденд на акцию") + labs(fill = "Отрасль") + xlab("")

Chart100

#чем больше выручки, тем больше дивиденд на акцию? 

Chart94 <- ggplot(data_for_graphs2, aes(x=DIV, y=Sales)) + 
  geom_point(aes(col=ID, size=DIV)) + theme_bw() + geom_smooth(method="loess", se=F) + xlab("Дивиденд на одну акцию") + ylab("Выручка")

#чем больше чистой прибыли, тем больше дивиденд на акцию?
Chart95 <- ggplot(data_for_graphs2, aes(x=DIV, y=Net_Income)) + 
  geom_point(aes(col=ID, size=DIV)) + theme_bw() + geom_smooth(method="loess", se=F) + xlab("Дивиденд на одну акцию") + ylab("Чистая прибыль")
Chart95



#а если умножим на величину акций в обращении? 
Chart96 <- ggplot(data_for_graphs2, aes(x=Dividends, y=Net_Income)) + 
  geom_point(aes(col=ID, size=Dividends)) + theme_bw() + geom_smooth(method="loess", se=F) + xlab("Совокупные дивидендные выплаты") + ylab("Чистая прибыль")
Chart96

library(ggside)
ggscatterstats(
  data  = data_for_graphs2,
  y     = Dividends,
  x     = Net_Income,
  ylab  = "Совокупные дивидендные выплаты",
  xlab  = "Чистая прибыль",
  title = "Диаграмма рассеивания чистой прибыли и совокупных дивидендных выплат", 
  xfill = "#018abd", 
  yfill = "#97cbdc", 
  smooth.line.args = list(size = 1.5, color = "darkblue", method = "lm", formula = y ~ x,
                          na.rm = TRUE), 
  point.args = list(size = 3, alpha = 0.4, color = "#0093b7", stroke = 0, na.rm = TRUE)
)

ggscatterstats(
  data  = data_for_graphs2,
  y     = DIV,
  x     = Net_Income,
  ylab  = "Дивиденд на одну акцию",
  xlab  = "Чистая прибыль",
  title = "Диаграмма рассеивания чистой прибыли и дивиденда на одну акцию", 
  xfill = "#c1e3ff", 
  yfill = "#153f65", 
  smooth.line.args = list(size = 1.5, color = "darkblue", method = "lm", formula = y ~ x,
                          na.rm = TRUE), 
  point.args = list(size = 3, alpha = 0.4, color = "#70bdf2", stroke = 0, na.rm = TRUE)
)

#показатели баланса по компаниям 
names(data_for_graphs2)

#разброс медианного значения по компаниям за года 
graph_base_2 <- data_for_graphs2 %>% group_by(Number_Year) %>% summarise(FCFE=median(FCFE), FCFF=median(CF_FCFF)) %>% pivot_longer(cols = c("FCFE", "FCFF"), names_to = "ID_CF", values_to = "Amount")

ggplot(graph_base_2, aes(x=ID_CF, y = Amount, fill=ID_CF)) +
  geom_boxplot() + theme_bw() + scale_fill_manual(values = c("#b7cfd5", "#c7b0a3")) + xlab("") + ylab("Величина потока") + labs(fill = "")

Chart_m <- ggplot(graph_base_2, aes(x=ID_CF, y = Amount, fill=ID_CF)) +
  geom_boxplot() + theme_bw() + scale_fill_manual(values = c("#b7cfd5", "#c7b0a3")) + xlab("") + ylab("Величина потока") + labs(fill = "")

ggplotly(Chart_m)

graph_base_3 <- data_for_graphs2 %>% group_by(Number_Company) %>% summarise(FCFE=median(FCFE), FCFF=median(CF_FCFF)) %>% pivot_longer(cols = c("FCFE", "FCFF"), names_to = "ID_CF", values_to = "Amount")
Chart97 <- ggplot(graph_base_3, aes(x=ID_CF, y = Amount, fill=ID_CF)) +
  geom_boxplot() + theme_bw() + scale_fill_manual(values = c("#b7cfd5", "#c7b0a3")) + xlab("") + ylab("Величина потока") + labs(fill = "")

ggplotly(Chart97)

#2020 и 2021 года 
graph_base_4 <- data_for_graphs2 %>% dplyr::filter(Year == c("2019", "2020", "2021"))
graph_base_4$Year_ch <- as.character(graph_base_4$Year)
Chart98 <- ggplot(graph_base_4, aes(x=Year_ch, y = WACC, fill=Year_ch)) +
  geom_boxplot() + theme_bw() + scale_fill_manual(values = c("#705e6c", "#c0b6c1", "#c4a8a5")) + xlab("") + ylab("WACC") + labs(fill = "Год")

Chart98

#а на горизонте как? 
graph_base_5 <- data_for_graphs2
View(data_for_graphs2)
graph_base_5$Year_ch <- as.character(graph_base_5$Year)
Chart99 <- ggplot(graph_base_5, aes(x=Year_ch, y = WACC, fill=Year_ch)) +
  geom_boxplot() + theme_bw() + xlab("") + ylab("WACC") + labs(fill = "Год") + 
  scale_fill_manual(values = c("#f3f6fb", "#e8e7e5", "#c4b4a7", "#af8988", "#5a3f2e", 
                               "#483233", "#aa8c77", "#d3c9c2", "#eaeaec", "#5e2009", 
                               "#b26b4d", "#866a67"))

Chart99

summary(data_for_graphs2)

#распределение независимых директоров 

Chart101 <- ggplot(data = data_for_graphs2, aes(x=Independent_Directors)) +
  geom_density(fill="navyblue", color="black", alpha=0.8) + facet_grid(~Year) + theme_bw() + xlab("Независимые директора") + ylab("Плотность распределения") + 
  theme(strip.background = element_rect(fill = "royalblue4"), 
        strip.text = element_text(colour = 'white'))
Chart101
#по-другому изобразим

str(data_for_graphs2)
data_for_graphs2$Year_ch <- as.factor(data_for_graphs2$Year)

ggplot(data_for_graphs2, aes(x = Independent_Directors, y = Year_ch, fill = ..x..)) + stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, rel_min_height = 0.01, scale = 0.8) + scale_fill_viridis_c(name = "Независимые директора", direction = -1) + theme_bw() + xlab("") + ylab("")

#совмещение Total_Equity и Capitalization
graph_base_6 <- data_for_graphs2 %>% pivot_longer(cols = c("Total_Equity", "Capitalization"), names_to = "criteria", values_to = "Amount")
ggplot(graph_base_6, aes(Amount, fill = criteria)) + geom_density(alpha = 0.2) + theme_bw() + scale_fill_manual(values = c("darkblue", "#CF391B")) + facet_wrap(~Year, nrow = 3, dir = "v") + labs(fill = "Параметр") + ylab("Плотность распределения") + xlab("") + theme(legend.position = "bottom") 

ggplot(data_for_graphs2, aes(MB_ratio)) + geom_density(alpha = 0.2) + theme_bw() + scale_fill_manual(values = c("darkblue", "#CF391B")) + facet_wrap(~Year, nrow = 3, dir = "v") + labs(fill = "Параметр") + ylab("Плотность распределения") + xlab("Соотношение рыночной и балансовой стоимости") + theme(legend.position = "bottom") 
ggplot(data_for_graphs2, aes(MB_ratio)) + geom_density(alpha = 0.2) + theme_bw() + scale_fill_manual(values = c("darkblue", "#CF391B")) + facet_wrap(~ID, nrow = 5, dir = "v") + labs(fill = "Параметр") + ylab("Плотность распределения") + xlab("Соотношение рыночной и балансовой стоимости") + theme(legend.position = "bottom") 

#Правда ли что во время короны компании стали меньше реинвестировать? 

Chart102 <- ggplot(graph_base_5, aes(x=Year_ch, y = CAPEX, fill=Year_ch)) +
  geom_boxplot() + theme_bw() + xlab("") + ylab("CAPEX") + labs(fill = "Год") + 
  scale_fill_manual(values = c("#f3f6fb", "#e8e7e5", "#c4b4a7", "#af8988", "#5a3f2e", 
                               "#483233", "#aa8c77", "#d3c9c2", "#eaeaec", "#5e2009", 
                               "#b26b4d", "#866a67"))
Chart103 <- ggplot(graph_base_4, aes(x=Year_ch, y = CAPEX, fill=Year_ch)) +
  geom_boxplot() + theme_bw() + scale_fill_manual(values = c("#705e6c", "#c0b6c1", "#c4a8a5")) + xlab("") + ylab("CAPEX") + labs(fill = "Год")

#а платить дивиденды стали меньше? 
Chart104 <- ggplot(graph_base_5, aes(x=Year_ch, y = Dividends, fill=Year_ch)) +
  geom_boxplot() + theme_bw() + xlab("") + ylab("Совокупные дивидендные выплаты") + labs(fill = "Год") + 
  scale_fill_manual(values = c("#f3f6fb", "#e8e7e5", "#c4b4a7", "#af8988", "#5a3f2e", 
                               "#483233", "#aa8c77", "#d3c9c2", "#eaeaec", "#5e2009", 
                               "#b26b4d", "#866a67"))

Chart105 <- ggplot(graph_base_5, aes(x=Year_ch, y = DIV, fill=Year_ch)) +
  geom_boxplot() + theme_bw() + xlab("") + ylab("Дивиденд на одну акцию") + labs(fill = "Год") + 
  scale_fill_manual(values = c("#f3f6fb", "#e8e7e5", "#c4b4a7", "#af8988", "#5a3f2e", 
                               "#483233", "#aa8c77", "#d3c9c2", "#eaeaec", "#5e2009", 
                               "#b26b4d", "#866a67"))

Chart106 <- ggplot(data = data_for_graphs2, aes(x=DIV)) +
  geom_density(fill="turquoise4", color="black", alpha=0.8) + facet_grid(~Year) + theme_bw() + xlab("Дивиденд на одну акцию") + ylab("Плотность распределения") + 
  theme(strip.background = element_rect(fill = "turquoise4"), 
        strip.text = element_text(colour = 'white')) + ggtitle("Плотность распределения дивиденда на одну акцию на горизонте 2010-2021 гг.")
Chart106
ggplot(data_for_graphs2, aes(x = DIV, y = Year_ch, fill = ..x..)) + stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, rel_min_height = 0.01, scale = 0.8) + scale_fill_viridis_c(name = "Дивиденд на одну акцию", direction = -1) + theme_bw() + xlab("") + ylab("")

Chart107 <- ggplot(graph_base_5, aes(x=Year_ch, y = EQY_sh_out, fill=Year_ch)) +
  geom_boxplot() + theme_bw() + xlab("") + ylab("Количество акций в обращении") + labs(fill = "Год") + 
  scale_fill_manual(values = c("#f3f6fb", "#e8e7e5", "#c4b4a7", "#af8988", "#5a3f2e", 
                               "#483233", "#aa8c77", "#d3c9c2", "#eaeaec", "#5e2009", 
                               "#b26b4d", "#866a67"))

Chart108 <- ggplot(graph_base_5, aes(x=Number_Year, y = Dividends, fill = "lightpink4")) + geom_bar(position="dodge", stat="identity", color = "darkblue") + facet_wrap(~ID, nrow = 5, dir = "v") + theme_bw() + scale_fill_manual(values = c("#3b73ad")) + theme(legend.position = "none") + ylab("Совокупные дивидендные выплаты") + xlab("Год")

#Стали ли компании ближе к дефолту из-за короны? 
Chart109 <- ggplot(graph_base_5, aes(x=Number_Year, y = Z_score, fill = "#488E7B")) + geom_bar(position="dodge", stat="identity", color = "black") + facet_wrap(~ID, nrow = 5, dir = "v") + theme_bw() + theme(legend.position = "none", strip.background = element_rect(fill = "#9DACA1"), strip.text = element_text(colour = 'black')) + ylab("Z score") + xlab("Год") + scale_fill_manual(values = c("#488E7B"))
Chart109

#сопоставим 2019, 2020 и 2021 
graph_base_4
Chart110 <- ggplot(graph_base_4, aes(x=Year_ch, y = Z_score, fill=Year_ch)) +
  geom_boxplot() + theme_bw() + scale_fill_manual(values = c("#705e6c", "#c0b6c1", "#c4a8a5")) + xlab("") + ylab("Z score") + labs(fill = "Год")
Chart110

graph_base_4_1 <- dplyr::filter(graph_base_4, Year_ch == "2019")
graph_base_4_2 <- dplyr::filter(graph_base_4, Year_ch == "2020")
graph_base_4_3 <- dplyr::filter(graph_base_4, Year_ch == "2021")

t.test(graph_base_4_1$Z_score, graph_base_4_2$Z_score)
t.test(graph_base_4_2$Z_score, graph_base_4_3$Z_score)
t.test(graph_base_4_1$Z_score, graph_base_4_3$Z_score)

View(graph_base_4)
names(graph_base_4)

ggbetweenstats(
  data = graph_base_4, 
  x     = Year,
  y     = Z_score,
  title = "Результаты теста Геймса-Ховелла для параметра Z-score", 
  caption = "Тест используется в случае наличия 3 и больше независимых выборок", 
  xlab = "Годы", 
  ylab = "Параметр вероятности банкротства Z-score", 
  p.adjust.method = "none",
  bf.message = FALSE, 
  pairwise.display = "all") + scale_color_manual(values = c("#D8B6C1", "#CF766B", "#B7463C"))


var.test(graph_base_4_1$Z_score, graph_base_4_2$Z_score)
var.test(graph_base_4_2$Z_score, graph_base_4_3$Z_score)
var.test(graph_base_4_1$Z_score, graph_base_4_3$Z_score)

#ROCE 
Chart111 <- ggplot(graph_base_5, aes(x=Year_ch, y = ROCE, fill=Year_ch)) +
  geom_boxplot() + theme_bw() + scale_fill_manual(values = c("#f3f6fb", "#e8e7e5", "#c4b4a7", "#af8988", "#5a3f2e", 
                                                             "#483233", "#aa8c77", "#d3c9c2", "#eaeaec", "#5e2009", 
                                                             "#b26b4d", "#866a67")) + xlab("") + ylab("ROCE") + labs(fill = "Год")
Chart111

graph_base_5 %>% ggplot( aes(y=Year_ch, x=ROCE,  fill=Year_ch)) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("ROCE") +
  ylab("") + scale_fill_manual(values = c("#f3f6fb", "#e8e7e5", "#c4b4a7", "#af8988", "#5a3f2e", 
                                          "#483233", "#aa8c77", "#d3c9c2", "#eaeaec", "#5e2009", 
                                          "#b26b4d", "#866a67"))

graph_base_5 %>% ggplot(aes(x = ROCE, y = Year_ch, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  labs(title = 'ROCE компаний DowJones на горизонте 2010-2021') + ylab("") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) + scale_fill_viridis(name = "Величина", option = "A") + theme_bw()

#График Капитализация - Дивидендные выплаты
Chart112 <- ggplot(data = graph_base_5, aes(color = industry, x = Dividends, y = Capitalization)) + scale_color_manual(values = c("#22211B", "#693F2F", "#CF391B", "#D06F1D", 
                                                                                                                                                            "#3b73ad", "#c48d74", "#cb7749", "#362321", 
                                                                                                                                                            "#768fad", "#904214", "#c1846a", "#d8b5ab")) + theme_bw() + geom_point() + geom_smooth(method=lm, se=FALSE, col='darkblue', size=1) + xlab("Совокупные дивидендные выплаты") + ylab("Капитализация компании") + labs("Отрасль") + 
  theme(legend.position = "right") + ggtitle("Диаграмма рассеивания абсолютных величин дивидендных выплат и капитализации компании")

Chart112

Chart113 <- ggplot(data = graph_base_5, aes(color = industry, x = DIV, y = Capitalization)) + scale_color_manual(values = c("#22211B", "#693F2F", "#CF391B", "#D06F1D", 
                                                                                                                                  "#3b73ad", "#c48d74", "#cb7749", "#362321", 
                                                                                                                                  "#768fad", "#904214", "#c1846a", "#d8b5ab")) + theme_bw() + geom_point() + geom_smooth(method=lm, se=FALSE, col='darkblue', size=1)+ xlab("Дивиденд на одну акцию") + ylab("Капитализация компании") + labs("Отрасль") + 
  theme(legend.position = "right") + ggtitle("Диаграмма рассеивания относительных величин дивидендных выплат и капитализации компании")
Chart113

#Верно ли, что наиболее эффективные компании с точки зрения ROA платят больше дивидендов? 
Chart114 <- ggplot(data = graph_base_5, aes(color = industry, x = Dividends, y = ROA)) + scale_color_manual(values = c("#22211B", "#693F2F", "#CF391B", "#D06F1D", 
                                                                                                                                  "#3b73ad", "#c48d74", "#cb7749", "#362321", 
                                                                                                                                  "#768fad", "#904214", "#c1846a", "#d8b5ab")) + theme_bw() + geom_point() + geom_smooth(method=lm, se=FALSE, col='darkblue', size=1) + xlab("Совокупные дивидендные выплаты") + ylab("ROA") + labs("Отрасль")
Chart114
Chart115 <- ggplot(data = graph_base_5, aes(color = industry, x = DIV, y = ROA)) + scale_color_manual(values = c("#22211B", "#693F2F", "#CF391B", "#D06F1D", 
                                                                                                                            "#3b73ad", "#c48d74", "#cb7749", "#362321", 
                                                                                                                            "#768fad", "#904214", "#c1846a", "#d8b5ab")) + theme_bw() + geom_point() + geom_smooth(method=lm, se=FALSE, col='darkblue', size=1)+ xlab("Дивиденд на одну акцию") + ylab("ROA") + labs("Отрасль")
Chart115
names(graph_base_5)
corr9 <- cor(graph_base_5[,-c(1,2,3,4,5,6,7,8,42,43,64,68,69,70,71,72,73,74,75)])
#корреляции с DIV - выберем > 15%
corr_DIV <- as.data.frame(corr9[,4]) #влияет ROE 
colnames(corr_DIV) <- c("Correlation")
corr_DIV1 <- dplyr::filter(corr_DIV, abs(Correlation) > 0.15)
View(corr_DIV1)
stargazer(corr_DIV1, type = "html", out = "corr.html", summary = FALSE)

#D/E
graph_base_6 <- graph_base_5
graph_base_6$DtoE <- (graph_base_6$BS_ST_Borrow + graph_base_6$BS_LT_Borrow)/graph_base_6$Total_Equity

#изменение по компаниям 
Chart116 <- ggplot(graph_base_6, aes(x=Number_Year, y = DtoE, fill = "#488E7B")) + geom_bar(position="dodge", stat="identity", color = "black") + facet_wrap(~ID, nrow = 5, dir = "v") + theme_bw() + theme(legend.position = "none") + ylab("D/E") + xlab("Год") + scale_fill_manual(values = c("#488E7B"))
Chart116

View(graph_base_6)
graph_base_6_1 <- dplyr::filter(graph_base_6, graph_base_6$ID%in%c("BA", "HD", "MCD", "VZ"))
Chart117 <- ggplot(graph_base_6_1, aes(x=Number_Year, y = DtoE, fill = "#488E7B")) + geom_bar(position="dodge", stat="identity", color = "black") + facet_wrap(~ID, nrow = 2, dir = "v") + theme_bw() + theme(legend.position = "none") + ylab("D/E") + xlab("Год") + scale_fill_manual(values = c("#488E7B"))
Chart117

#самоорганизующиеся карты •	CAPEX •	DIV •	Independent_Directors •	Stage_Indicator1 •	BS_LT_Borrow
graph_base_7 <- dplyr::select(graph_base_6, CAPEX, DIV, Independent_Directors, Stage_Indicator1, BS_LT_Borrow)
graph_base_7_scale <- as.matrix(scale(graph_base_7))
som_grid <- somgrid(xdim = 4, ydim = 4, topo = "hexagonal")
som_grid_example <- somgrid(xdim = 3, ydim = 3, topo = "hexagonal")

som_model <- som(graph_base_7_scale, grid = som_grid, rlen = 100, keep.data = TRUE)
som_model_example <- som(graph_base_7_scale, grid = som_grid_example, rlen = 100, keep.data = TRUE)
plot(som_model, type = "counts") #нормально, укрупнять не надо
plot(som_model, type = "mapping") 
plot(som_model_example, type = "counts")

plot(som_model, type = "codes", main = "Эталонные представители узла")
#как выглядят кластеры?
plot(som_model, type = "code")
#достанем информацию о каждом кластере
Som_codes <- getCodes(som_model)
Som_codes <- as.data.frame(Som_codes)

#как распределены параметры?
par(mfrow = c(2,4))
for (i in 1:8) {
  assign(paste0("plot", i), plot(som_model, type = "property", property = Som_codes[,i], main = colnames(Som_codes)[i], palette = palette_new))
}

par(mfrow = c(1,1))
par(mar=c(5.1 ,4.1 ,4.1 ,2.1)) 


#добавим иерархическую кластеризацию
mydata <- as.matrix(Som_codes)

#используем иерархическую кластеризацию
som_cluster <- cutree(hclust(dist(mydata)), 3)

# Определяем палитру цветов
palette_new_2 <- function(n) {
  inferno(n)
}

# Показываем разными цветами кластеры узлов и переменные
plot(som_model, type = "codes", 
     bgcol = palette_new_2(3)[som_cluster], main = NA)
add.cluster.boundaries(som_model, som_cluster)

som_cluster1 <- cutree(hclust(dist(mydata)), 4)
palette_new_3 <- function(n) {
  magma(n)
}
plot(som_model, type = "codes", 
     bgcol = palette_new_3(4)[som_cluster1], main = NA)
add.cluster.boundaries(som_model, som_cluster1)

som_cluster2 <- cutree(hclust(dist(mydata)), 5)
palette_new_4 <- function(n) {
  plasma(n)
}
plot(som_model, type = "codes", 
     bgcol = palette_new_4(5)[som_cluster2], main = NA)
add.cluster.boundaries(som_model, som_cluster2)

#достаточно ли долго мы обучали модель?
par(mar=c(5.1 ,4.1 ,4.1 ,2.1)) 
plot(som_model, type = "changes")

#строим самоорганизующиеся карты по одному году

graph_base_8 <- dplyr::filter(graph_base_6, Number_Year == 21)
graph_base_9 <- dplyr::select(graph_base_8, CAPEX, DIV, Independent_Directors, Stage_Indicator1, BS_LT_Borrow)

graph_base_9_scale <- as.matrix(scale(graph_base_9))
som_grid1 <- somgrid(xdim = 2, ydim = 2, topo = "hexagonal")

som_model1 <- som(graph_base_9_scale, grid = som_grid1, rlen = 100, keep.data = TRUE)
plot(som_model1, type = "counts") #нормально, укрупнять не надо
plot(som_model1, type = "mapping") 

plot(som_model1, type = "codes", main = "Эталонные представители узла")
#как выглядят кластеры?
plot(som_model1, type = "code")
#достанем информацию о каждом кластере
Som_codes1 <- getCodes(som_model1)
Som_codes1 <- as.data.frame(Som_codes1)

#как распределены параметры?
par(mfrow = c(2,4))
for (i in 1:8) {
  assign(paste0("plot", i), plot(som_model1, type = "property", property = Som_codes1[,i], main = colnames(Som_codes1)[i], palette = palette_new))
}

par(mfrow = c(1,1))
par(mar=c(5.1 ,4.1 ,4.1 ,2.1)) 


#добавим иерархическую кластеризацию
mydata1 <- as.matrix(Som_codes1)

#используем иерархическую кластеризацию
som_cluster1 <- cutree(hclust(dist(mydata1)), 3)

# Определяем палитру цветов
palette_new_2 <- function(n) {
  inferno(n)
}

# Показываем разными цветами кластеры узлов и переменные
plot(som_model1, type = "codes", 
     bgcol = palette_new_2(3)[som_cluster1], main = NA)
add.cluster.boundaries(som_model1, som_cluster1)

som_cluster10 <- cutree(hclust(dist(mydata1)), 2)
palette_new_3 <- function(n) {
  magma(n)
}
plot(som_model1, type = "codes", 
     bgcol = palette_new_3(2)[som_cluster10], main = NA)
add.cluster.boundaries(som_model1, som_cluster10)


#достаточно ли долго мы обучали модель?
par(mar=c(5.1 ,4.1 ,4.1 ,2.1)) 
plot(som_model1, type = "changes")

#sum 
names(DataSet2)
data_sum <- DataSet2 %>% transmute(DIV, BS_LT_Borrow, CF_FCFF, 
                                   CAPEX, Stage_Indicator1, Z_score, DebtToEquity = (DataSet2$BS_LT_Borrow + DataSet2$BS_ST_Borrow)/DataSet2$Total_Equity, Independent_Directors, WACC, CAPEXtoSales = CAPEX/Sales)

model58 <- lm(data = data_sum, DIV ~ .-CAPEXtoSales)
summary(model58) #смущают знаки перед CAPEX
stepAIC(model58) #удалим CAPEX и Z_score

model61 <- lm(data = data_sum, DIV ~ .-CAPEX-CAPEXtoSales-Z_score)
summary(model61) 

#а если CAPEX к Sales? 
model59 <- lm(data = data_sum, DIV ~ .-CAPEX)
summary(model59) #а тут уже CAPEX норм влияет
stepAIC(model59)

model60 <- update(model59, .~.-Z_score-CAPEX)
summary(model60)


#тут фиксим - model61 и model60

#попробуем с другими регрессантами - прокси

data_sum1 <- DataSet2 %>% transmute(DIV, DivToSales = Dividends/Sales, Dividends = log(Dividends), DivToTA = Dividends/BS_Total_Assets, BS_LT_Borrow, CF_FCFF, 
                                   Stage_Indicator1, Z_score, DebtToEquity = (DataSet2$BS_LT_Borrow + DataSet2$BS_ST_Borrow)/DataSet2$Total_Equity, Independent_Directors, WACC, CAPEXtoSales = CAPEX/Sales)
data_sum1$Dividends <- ifelse(data_sum1$Dividends== "-Inf", 0, data_sum1$Dividends)
data_sum1$DivToTA <- ifelse(data_sum1$DivToTA== "-Inf", 0, data_sum1$DivToTA)
#график для прокси
names(data_sum3)
data_sum4 <- data_sum3 %>% group_by(Number_Year) %>% transmute(Number_Company, 
                                                               median_BS_LT_Borrow = median(BS_LT_Borrow), 
                                                               median_CF_FCFF = median(CF_FCFF), 
                                                               median_Stage_Indicator1 = median(Stage_Indicator1), 
                                                               median_DebtToEquity = median(DebtToEquity), 
                                                               median_Z_score = median(Z_score), 
                                                               median_Independent_Directors = median(Independent_Directors), 
                                                               median_WACC = median(WACC), 
                                                               median_CAPEXtoSales = median(CAPEXtoSales), 
                                                               median_DIV = median(DIV))
data_sum5 <- merge(data_sum4, data_sum3, by = c("Number_Company", "Number_Year"))
View(data_sum5)
data_sum5$BS_LT_Borrow_dummy <- ifelse(data_sum5$BS_LT_Borrow>data_sum5$median_BS_LT_Borrow, 1, 0)
data_sum5$CF_FCFF_dummy <- ifelse(data_sum5$CF_FCFF>data_sum5$median_CF_FCFF, 1, 0)
data_sum5$Stage_Indicator1_dummy <- ifelse(data_sum5$Stage_Indicator1>data_sum5$median_Stage_Indicator1, 1, 0)
data_sum5$DebtToEquity_dummy <- ifelse(data_sum5$DebtToEquity>data_sum5$median_DebtToEquity, 1, 0)
data_sum5$Z_score_dummy <- ifelse(data_sum5$Z_score>data_sum5$median_Z_score, 1, 0)
data_sum5$Independent_Directors_dummy <- ifelse(data_sum5$Independent_Directors>data_sum5$median_Independent_Directors, 1, 0)
data_sum5$WACC_dummy <- ifelse(data_sum5$WACC>data_sum5$median_WACC, 1, 0)
data_sum5$CAPEXtoSales_dummy <- ifelse(data_sum5$CAPEXtoSales>data_sum5$median_CAPEXtoSales, 1, 0)
data_sum5$DIV_dummy <- ifelse(data_sum5$DIV>data_sum5$median_DIV, 1, 0)
dummy <- data.frame(data_sum5$BS_LT_Borrow_dummy, data_sum5$CF_FCFF_dummy, data_sum5$Stage_Indicator1_dummy, data_sum5$DebtToEquity_dummy, 
                    data_sum5$Z_score_dummy, data_sum5$Independent_Directors_dummy, data_sum5$WACC_dummy, data_sum5$CAPEXtoSales_dummy, data_sum5$DIV_dummy)

colnames(dummy) <- c("BS_LT_Borrow", "CF_FCFF", "Stage_Indicator1", "DebtToEquity", 
                     "Z_score", "Independent_Directors", "WACC", "CAPEXtoSales", 'DIV')
N <- names(dummy)
count1 <- dummy %>% filter(Z_score == 1)
count1$sum <- count1$BS_LT_Borrow+count1$CF_FCFF+count1$Stage_Indicator1+count1$DebtToEquity+count1$Z_score+count1$Independent_Directors+count1$WACC+count1$CAPEXtoSales+count1$DIV
count2 <- count1 %>% filter(sum == 1)
count2
View(dummy)
library(ComplexUpset)
?upset
ComplexUpset::upset(dummy, N, name='N', 
      width_ratio = 0.4, n_intersections  = 5, set_sizes = FALSE, 
      base_annotations = list(
        'Intersection size'=intersection_size(
          fill='darkblue'
        )
      ), 
      matrix=(
        intersection_matrix(geom=geom_point(shape='circle filled', size=3))
        + scale_color_manual(values=c('DIV'='#693450', 'CAPEXtoSales'='#ca540c', 'WACC'='#ffef03',
                                      'Z_score'='#285678', 'DebtToEquity'='#5d3277', 'Stage_Indicator1'='#a16db7', 
                                      'CF_FCFF'='#426c79', 'BS_LT_Borrow'='#C711A4', 'Independent_Directors'='#F6386D'))
        ), 
      queries=list(upset_query(set='DIV', fill='#693450'),
                   upset_query(set='CAPEXtoSales', fill='#ca540c'),
                   upset_query(set='WACC', fill='#ffef03'), 
                   upset_query(set='Z_score', fill='#285678'), 
                   upset_query(set='DebtToEquity', fill='#5d3277'), 
                   upset_query(set='Stage_Indicator1', fill='#a16db7'), 
                   upset_query(set='CF_FCFF', fill='#426c79'), 
                   upset_query(set='BS_LT_Borrow', fill='#C711A4'), 
                   upset_query(set='Independent_Directors', fill='#F6386D')))
?upset
names(dummy)
UpSetR::upset(dummy, nsets = 9, order.by = c("freq"), 
              main.bar.color = "darkblue", point.size=5, 
              queries=list(
                upset_query(set='DIV', fill='#693450'),
                upset_query(set='CAPEXtoSales', fill='#ca540c'),
                upset_query(set='WACC', fill='#ffef03'), 
                upset_query(set='Z_score', fill='#285678'), 
                upset_query(set='DebtToEquity', fill='#5d3277'), 
                upset_query(set='Stage_Indicator1', fill='#a16db7'), 
                upset_query(set='CF_FCFF', fill='#426c79'), 
                upset_query(set='BS_LT_Borrow', fill='#C711A4'), 
                upset_query(set='Independent_Directors', fill='#F6386D')
              )
              )
?upset
queries=list(upset_query(set='DIV', fill='#693450'),
             upset_query(set='CAPEXtoSales', fill='#ca540c'),
             upset_query(set='WACC', fill='#ffef03'), 
             upset_query(set='Z_score', fill='#285678'), 
             upset_query(set='DebtToEquity', fill='#5d3277'), 
             upset_query(set='Stage_Indicator1', fill='#a16db7'), 
             upset_query(set='CF_FCFF', fill='#426c79'), 
             upset_query(set='BS_LT_Borrow', fill='#C711A4'), 
             upset_query(set='Independent_Directors', fill='#F6386D'))

upset(dummy, N, name='N', main.bar.color = "SteelBlue", sets.bar.color = "DarkCyan")
#убрали просто CAPEX, оставили нормированный CAPEX
model62 <- lm(data = data_sum1, DivToSales ~.-DIV-Dividends-DivToTA)
summary(model62)
stepAIC(model62)
model63 <- update(model62, .~.-BS_LT_Borrow-Stage_Indicator1-DebtToEquity-CF_FCFF)
summary(model63)

model64 <- lm(data = data_sum1, Dividends ~.-DivToSales-DIV-DivToTA)
summary(model64)
stepAIC(model64)

model65 <- update(model64, .~.-DebtToEquity-Z_score-Stage_Indicator1-BS_LT_Borrow-CAPEXtoSales)
summary(model65)

model66 <- lm(data = data_sum1, DivToTA ~.-DivToSales-DIV-Dividends)
summary(model66)
stepAIC(model66)

model67 <- update(model66, .~.-DebtToEquity-Stage_Indicator1-WACC-BS_LT_Borrow-Independent_Directors)
summary(model67)
bptest(model61)
bptest(model60)
bptest(model63)
bptest(model65)
bptest(model67)
stargazer(model60, model61, model63, model65, model67, summary = FALSE, type = "html", out = "proxy.html", se = list(cse(model60), cse(model61), cse(model63), cse(model65), cse(model67)))

#лаги для модели прокси
lag_BS_LT_Borrow <- DataSet2 %>% group_by(Number_Company) %>% transmute(Number_Year, lag_BS_LT_Borrow=dplyr::lag(BS_LT_Borrow)) %>% ungroup
lag_CF_FCFF <- DataSet2 %>% group_by(Number_Company) %>% transmute(Number_Year, lag_CF_FCFF=dplyr::lag(CF_FCFF)) %>% ungroup
lag_DebtToEquity <- data_sum2 %>% group_by(Number_Company) %>% transmute(lag_DebtToEquity=dplyr::lag(DebtToEquity)) %>% ungroup
lag_Z_score <- DataSet2 %>% group_by(Number_Company) %>% transmute(Number_Year, lag_Z_score=dplyr::lag(Z_score)) %>% ungroup
lag_Independent_Directors <- DataSet2 %>% group_by(Number_Company) %>% transmute(Number_Year, lag_Independent_Directors=dplyr::lag(Independent_Directors)) %>% ungroup
lag_WACC <- DataSet2 %>% group_by(Number_Company) %>% transmute(Number_Year, lag_WACC=dplyr::lag(WACC)) %>% ungroup
lag_CAPEXtoSales <- data_sum2 %>% group_by(Number_Company) %>% transmute(lag_CAPEXtoSales=dplyr::lag(CAPEXtoSales)) %>% ungroup

lag_proxy_SI <- DataSet2 %>% transmute(DIV, BS_LT_Borrow, CF_FCFF, 
                                       lag_Stage_Indicator1 = lag_Stage_Indicator1$lag_Stage_Indicator1, Z_score, DebtToEquity = (DataSet2$BS_LT_Borrow + DataSet2$BS_ST_Borrow)/DataSet2$Total_Equity, Independent_Directors, WACC, CAPEXtoSales = CAPEX/Sales)

model118 <- lm(DIV ~ ., data = na.omit(lag_proxy_SI))
summary(model118)
bptest(model118) #есть гетероскедастичность

stepAIC(model118)
model119 <- update(model118, DIV ~ BS_LT_Borrow + CF_FCFF + lag_Stage_Indicator1 + 
                 DebtToEquity + Independent_Directors + WACC + CAPEXtoSales)
summary(model119)
bptest(model119) #есть гетероскедастичность

stargazer(model118, model119, type = "html", df = FALSE, se = list(cse(model118), cse(model119)), out = "table_proxy.html")

lag_proxy <- DataSet2 %>% transmute(DIV, lag_BS_LT_Borrow = lag_BS_LT_Borrow$lag_BS_LT_Borrow, 
                                    lag_CF_FCFF = lag_CF_FCFF$lag_CF_FCFF, 
                                    lag_Stage_Indicator1 = lag_Stage_Indicator1$lag_Stage_Indicator1, 
                                    lag_DebtToEquity = lag_DebtToEquity$lag_DebtToEquity, 
                                    lag_Z_score = lag_Z_score$lag_Z_score, 
                                    lag_Independent_Directors = lag_Independent_Directors$lag_Independent_Directors, 
                                    lag_WACC = lag_WACC$lag_WACC, 
                                    lag_CAPEXtoSales = lag_CAPEXtoSales$lag_CAPEXtoSales
                                    )
model120 <- lm(data = na.omit(lag_proxy), DIV ~ .)
summary(model120)
bptest(model120) #есть гетеро
stepAIC(model120)

model121 <- update(model120, DIV ~ lag_BS_LT_Borrow + lag_CF_FCFF + lag_Stage_Indicator1 + 
                     lag_DebtToEquity + lag_Independent_Directors + lag_WACC + 
                     lag_CAPEXtoSales)
bptest(model121) #есть гетеро

stargazer(model120, model121, type = "html", df = FALSE, se = list(cse(model120), cse(model121)), out = "table_lag_proxy.html")



#фиксированные и случайные эффекты для модели с прокси-переменными
names(DataSet2)
data_sum3 <- DataSet2 %>% transmute(Number_Year, Number_Company, DIV, DivToSales = Dividends/Sales, Dividends = log(Dividends), DivToTA = Dividends/BS_Total_Assets, BS_LT_Borrow, CF_FCFF, 
                                    Stage_Indicator1, Z_score, DebtToEquity = (DataSet2$BS_LT_Borrow + DataSet2$BS_ST_Borrow)/DataSet2$Total_Equity, Independent_Directors, WACC, CAPEXtoSales = CAPEX/Sales)

proxy_pooled <- plm(data = data_sum3,DIV~BS_LT_Borrow+CF_FCFF+Stage_Indicator1+DebtToEquity+Z_score+Independent_Directors+WACC+CAPEXtoSales, model = "pooling",index = c("Number_Year", "Number_Company"))
proxy_random <- plm(data = data_sum3,DIV~BS_LT_Borrow+CF_FCFF+Stage_Indicator1+DebtToEquity+Z_score+Independent_Directors+WACC+CAPEXtoSales, model = "random",index = c("Number_Year", "Number_Company"))
proxy_fix <- plm(data = data_sum3,DIV~BS_LT_Borrow+CF_FCFF+Stage_Indicator1+DebtToEquity+Z_score+Independent_Directors+WACC+CAPEXtoSales, model = "within",index = c("Number_Year", "Number_Company"))
bptest(proxy_pooled) #есть гетероскедастичность

proxy_pooled1 <- plm(data = data_sum3,DIV~BS_LT_Borrow+CF_FCFF+Stage_Indicator1+DebtToEquity+Independent_Directors+WACC+CAPEXtoSales, model = "pooling",index = c("Number_Year", "Number_Company"))
proxy_random1 <- plm(data = data_sum3,DIV~BS_LT_Borrow+CF_FCFF+Stage_Indicator1+DebtToEquity+Independent_Directors+WACC+CAPEXtoSales, model = "random",index = c("Number_Year", "Number_Company"))
proxy_fix1 <- plm(data = data_sum3,DIV~BS_LT_Borrow+CF_FCFF+Stage_Indicator1+DebtToEquity+Independent_Directors+WACC+CAPEXtoSales, model = "within",index = c("Number_Year", "Number_Company"))

#Тестирование гипотез (первая модель это нулевая гипотеза):
#– фиксированные эффекты против обычной. Нулевая гипотеза: обычная модель лучше. Порядок важен, сначала записываем фиксированную модель. 
pFtest(proxy_fix, proxy_pooled) #лучше фиксированные эффекты
#фиксированные > пула

#– тест Хаусмана (RЕ против ФЕ). Нулевая гипотеза: случайные эффекты лучше
phtest(proxy_random, proxy_fix) #лучше фиксированные эффекты
#фиксированные > случайные

#– тест множителей Лагранжа (RЕ против обычной). Нулевая гипотеза: обычная регрессия лучше
plmtest(proxy_random, type = "bp") #лучше случайные эффекты 
#случайные > пула 

#с удалением
#Тестирование гипотез (первая модель это нулевая гипотеза):
#– фиксированные эффекты против обычной. Нулевая гипотеза: обычная модель лучше. Порядок важен, сначала записываем фиксированную модель. 
pFtest(proxy_fix1, proxy_pooled1) #лучше фиксированные эффекты
#фиксированные > пула

#– тест Хаусмана (RЕ против ФЕ). Нулевая гипотеза: случайные эффекты лучше
phtest(proxy_random1, proxy_fix1) #лучше фиксированные эффекты
#фиксированные > случайные

#– тест множителей Лагранжа (RЕ против обычной). Нулевая гипотеза: обычная регрессия лучше
plmtest(proxy_random1, type = "bp") #лучше случайные эффекты 
#случайные > пула 


stargazer(list(proxy_pooled, proxy_fix, proxy_random), column.labels = c("Pooling", "RE", "FE"), type = "html", df = FALSE, se = list(cse(proxy_pooled), cse(proxy_fix), cse(proxy_random)), out = "WithoutDeleteProxy.html")
stargazer(list(proxy_pooled1, proxy_fix1, proxy_random1), column.labels = c("Pooling", "RE", "FE"), type = "html", df = FALSE, se = list(cse(proxy_pooled1), cse(proxy_fix1), cse(proxy_random1)), out = "WithDeleteProxy.html")

V61 <- vcovHC(model61, type = "HC0")

ggcoef1 <- ggcoefstats(model61, 
            title = "Результаты лучшей регрессии по модели с прокси-переменными и регрессантом DIV", 
            ylab = "Переменные", 
            xlab = "Оценка", 
            package = "RColorBrewer",
            palette = "Dark2", 
            vcov = V61
)
V63 <- vcovHC(model63, type = "HC0")
ggcoef2 <- ggcoefstats(model63, 
                       title = "Результаты лучшей регрессии по модели с прокси-переменными и регрессантом DivToSales", 
                       ylab = "Переменные", 
                       xlab = "Оценка", 
                       package = "RColorBrewer",
                       palette = "Dark2", 
                       vcov = V63
)
V65 <- vcovHC(model65, type = "HC0")
ggcoef3 <- ggcoefstats(model65, 
                       title = "Результаты лучшей регрессии по модели с прокси-переменными и 
регрессантом логарифмом совокупных дивидендных выплат", 
                       ylab = "Переменные", 
                       xlab = "Оценка", 
                       package = "RColorBrewer",
                       palette = "Dark2", 
                       vcov = V65
)

V67 <- vcovHC(model67, type = "HC0")
ggcoef4 <- ggcoefstats(model67, 
                       title = "Результаты лучшей регрессии по модели с прокси-переменными и регрессантом DivToTA", 
                       ylab = "Переменные", 
                       xlab = "Оценка", 
                       package = "RColorBrewer",
                       palette = "Dark2", 
                       vcov = V67
)


cor(data_sum1$BS_LT_Borrow, data_sum1$DebtToEquity)
#совокупные дивидендные выплаты и DPS 
Chart90 <- ggplot(data_for_graphs1, aes(Dividends, frame = Year, size = DIV, fill = ID, color = ID)) +
  theme_bw() + geom_text(aes(label = ID), check_overlap = TRUE) + xlab("FCFF") + ylab("Выручка") + labs(fill = "Компания") + scale_color_manual(values = c("darkblue", "aquamarine2", "azure3", "black", "blue2", 
                                                                                                                                                           "blueviolet", "brown2", "burlywood2", "chocolate1", "cornflowerblue",
                                                                                                                                                           "cyan2", "darkgoldenrod1", "darkmagenta", "firebrick1", "deeppink", 
                                                                                                                                                           "green2", "indianred2", "lightcoral", "mediumorchid2", "mistyrose1",                                                                                                                                                "mediumspringgreen", "orangered1", "purple4", "peru", "seagreen"))
ggplotly(Chart90)
names(data_for_graphs3)
str(data_for_graphs3)
data_for_graphs3$mmm <- ifelse(data_for_graphs3$DivID == "DIV", "Дивиденд на одну акцию", "Совокупные дивидендные выплаты")
Chart119 <- ggplot(data_for_graphs3, aes(x = mmm, y = SUM, frame = Year)) + geom_boxplot(fill = "darkblue", color = "lightblue")+theme_bw() + scale_fill_manual(values = c("darkblue")) + scale_y_continuous(name = "", sec.axis = sec_axis(~ . /1000, name = expression("Дивиденд на одну акцию"))) + facet_wrap(~DivID, scales="free",ncol=2) + xlab("") + ylab("")
Chart119
ggplotly(Chart119)

#случайный лес + прокси
data_sum3 <- data_sum1[,-c(2,3,4)]
set.seed(123)
forest2 <- randomForest(DIV ~ ., data = data_sum3, localImp = TRUE)
forest2

min_depth_frame1 <- min_depth_distribution(forest2)
head(min_depth_frame1, n = 10)
#график распределения минимальной глубины для десяти лучших переменных
plot_min_depth_distribution(min_depth_frame1) + 
  xlab("Переменные") +
  ylab("Количество деревьев")+
  scale_fill_manual(values = c("#003f5c", "#2f4b7c", "#665191", "#a05195", "#d45087", "#f95d6a", "#ff7c43", "#ffa600", 
                               "#e45149", "#b6d2dd", "#259086", "#0a463c")) + ggtitle("Распределение минимальной глубины и среднего")
varImpPlot(forest2) #показывает степень важности переменных, лучше всего Impurity (загрязненность) сокращается, когда начинаю делить с той, а не с иной переменной
#самые нижние - самые не информативные, самые верхние - самые информативные
plot(forest2)#переобучения нет
importance_frame1 <- measure_importance(forest2)
importance_frame1
plot_multi_way_importance(importance_frame1, size_measure = "no_of_nodes")
plot_multi_way_importance(importance_frame1, x_measure = "mse_increase", y_measure = "node_purity_increase", size_measure = "p_value", no_of_labels = 5)
names(DeAngelo2)
plot_predict_interaction(forest2, data_sum2, "Stage_Indicator1", "BS_LT_Borrow")

names(data_sum2)
proxy_mean <- transmute(data_sum2, BS_LT_Borrow = mean(BS_LT_Borrow), Stage_Indicator1 = Stage_Indicator1, 
                        CF_FCFF = mean(CF_FCFF), Z_score = mean(Z_score), DebtToEquity = mean(DebtToEquity), Independent_Directors = mean(Independent_Directors), 
                        WACC = mean(WACC), CAPEXtoSales = mean(CAPEXtoSales))
Predict_values_proxy <- predict(forest2, newdata = proxy_mean)
proxy_mean1 <- mutate(proxy_mean, Predict_values_proxy)
ggplot(proxy_mean1, aes(x = Stage_Indicator1, y = Predict_values_proxy)) + geom_line(color = "darkblue", size = 1) + theme_bw() + xlab("Стадия жизненного цикла (Stage_Indicator1)") + ylab("Прогнозные значения дивиденда на одну акцию (DIV)") + ggtitle("Отражение зависимости между стадией жизненного цикла и 
дивидендом на одну акцию для среднего наблюдения по выборке")


#бустинг + прокси
set.seed(123)
names(data_sum3)
fitControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 10)
gbm_proxy <- train(DIV ~ ., data = data_sum3, method = 'gbm', verbose = FALSE)
gbm_proxy
gbm_proxy$bestTune
getTrainPerf(gbm_proxy)

ggplot(gbm_proxy) + geom_point(col = "darkblue", size = 0.5) + scale_color_manual(values = c("#550445", "#c96588", "#5f73b3")) + theme_bw() + theme(legend.position = "bottom")
#глубина - 3 лучшая

gbm.fit.final_proxy <- gbm(
  formula = DIV ~ .,
  data = data_sum3,
  n.trees = 150,
  interaction.depth = 3,
  shrinkage = 0.1,
  n.minobsinnode = 10,
  verbose = FALSE
)  

par(mar = c(5, 8, 1, 1))
summary_proxy <- as.data.frame(summary(
  gbm.fit.final_proxy, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
))
names(summary1) 
ggplot(summary_proxy, aes(x = reorder(var, rel.inf), y = rel.inf, fill = rel.inf)) + geom_col(color = "black") + coord_flip() + theme_bw() + ylab("Относительная важность переменной") + xlab("") + labs("Значения") +
  scale_fill_gradient2(low="#d2cdca", mid="#929fb0",
                       high="#4a5060", midpoint=20) + theme(legend.position = "bottom")

explainer_proxy <- lime::lime(
  x              = data_sum3, 
  model          = gbm_proxy, 
  bin_continuous = TRUE
)

explanation_proxy_all <- lime::explain(
  data_sum3, 
  explainer    = explainer_proxy, 
  n_labels     = 1, 
  n_features   = 3,
  kernel_width = 0.5
)
View(explanation_proxy_all)

explanation_proxy_all1 <- explanation_proxy_all %>% group_by(case, feature) %>% dplyr::arrange(-model_r2)
explanation_proxy_all1$prediction <- round(explanation_proxy_all1$prediction, 2)

plot_features(explanation_proxy_all1[1:12,]) +
  labs(title = "Важность переменных для конкретных случаев",
       subtitle = "Взяты 4 случая с большей объясняющей силой") +  scale_fill_manual(values = c("#4f6d9c", "#ced6e3")) + theme_minimal()+theme(legend.position = "bottom") + xlab("")
?plot_features
plot_explanations(explanation_proxy_all1[1:60,]) +
  labs(title = "Важность переменных для конкретных случаев",
       subtitle = "Взяты 20 случаев с большей объясняющей силой") + theme_bw()+ scale_fill_gradient2(low="#acbdd3", mid="#5d80b6",
                                                                                                     high="#101e5a", midpoint=-1) 
#прокси + бустинг + кросс-валидация 
fitControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 10)
inTraining_gbm_cv_proxy <- caret::createDataPartition(data_sum3$DIV, p=.75, list = FALSE) #разделим на 75% и 25% случайным образом
training_cv_proxy <- data_sum3[inTraining_gbm_cv_proxy,]
testing_cv_proxy <- data_sum3[-inTraining_gbm_cv_proxy,]
set.seed(123)
gbm_cv_proxy <- train(DIV ~ ., data = training_cv_proxy, method = 'gbm', trControl = fitControl, verbose = FALSE)
gbm_cv_proxy$bestTune
getTrainPerf(gbm_cv_proxy)

#введем сетку для перебора
gbmGrid <- expand.grid(interaction.depth = c(1,2,3), n.trees= (1:15)*50, shrinkage = 0.1, n.minobsinnode = 10)
set.seed(123)
gbm_cv_proxy1 <- train(DIV ~ ., data = training_cv_proxy, method = 'gbm', trControl = fitControl, verbose = FALSE, tuneGrid = gbmGrid)
getTrainPerf(gbm_cv_proxy1) #самая лучшая модель
gbm_cv_proxy1$bestTune
ggplot(gbm_cv_proxy1) + geom_point(col = "darkblue", size = 0.5) + scale_color_manual(values = c("#550445", "#c96588", "#5f73b3")) + theme_bw() + theme(legend.position = "bottom")

set.seed(123)
gbm.fit_cv_proxy <- gbm(
  formula = DIV ~ .,
  data = training_cv_proxy,
  n.trees = 750,
  interaction.depth = 3,
  shrinkage = 0.1,
  n.minobsinnode = 10,
  verbose = FALSE
)  

par(mar = c(5, 8, 1, 1))
summary_cv_proxy <- as.data.frame(summary(
  gbm.fit_cv_proxy, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
))

ggplot(summary_cv_proxy, aes(x = reorder(var, rel.inf), y = rel.inf, fill = rel.inf)) + geom_col(color = "black") + coord_flip() + theme_bw() + ylab("Относительная важность переменной") + xlab("") + labs("Значения") +
  scale_fill_gradient2(low="#d2cdca", mid="#929fb0",
                       high="#4a5060", midpoint=15) + theme(legend.position = "bottom")

ggplot(gbm_cv_proxy1, plotType = "level") + scale_fill_gradient2(low="#f7e8e3", mid="#005ec4",
                                                            high="#02062f", midpoint=1.05) + theme_bw() + theme(legend.position = "bottom") + ylab("Глубина дерева") + xlab("Количество деревьев")

gbm.fit_cv_proxy %>%
  pdp::partial(pred.var = "Stage_Indicator1", n.trees = gbm.fit_cv$n.trees) %>%
  autoplot(rug = TRUE, train = training_cv_proxy, col = "darkblue", size = 1) + theme_bw() + ylab("Прогноз дивиденда на одну акцию")

explainer_gbm_proxy <- lime::lime(
  x              = data_sum3, 
  model          = gbm_cv_proxy1, 
  bin_continuous = FALSE
)

explanation_gbm_proxy_cv_all <- lime::explain(
  data_sum3, 
  explainer    = explainer_gbm_proxy, 
  n_labels     = 1, 
  n_features   = 3,
  kernel_width = 0.5
)

explanation_gbm_proxy_cv_all1 <- explanation_gbm_proxy_cv_all %>% group_by(case, feature) %>% dplyr::arrange(-model_r2)
explanation_gbm_proxy_cv_all1$prediction <- round(explanation_gbm_proxy_cv_all1$prediction, 2)
View(explanation_gbm_cv_all1)

plot_features(explanation_gbm_proxy_cv_all1[1:12,]) +
  labs(title = "Важность переменных для конкретных случаев",
       subtitle = "Взяты 4 случая с большей объясняющей силой") +  scale_fill_manual(values = c("#4f6d9c", "#ced6e3")) + theme_minimal()+theme(legend.position = "bottom") + xlab("")

plot_explanations(explanation_gbm_proxy_cv_all1[1:60,]) +
  labs(title = "Важность переменных для конкретных случаев",
       subtitle = "Взяты 20 случаев с большей объясняющей силой") + theme_bw()+ scale_fill_gradient2(low="#acbdd3", mid="#5d80b6",
                                                                                                     high="#101e5a", midpoint=-1) 
#сравнение моделей: регрессия / случайный лес / бустинг 

getTrainPerf(gbm_proxy)[2]
getTrainPerf(gbm_cv_proxy1)[2]
last(forest2$rsq)
summary(model61)$adj.r.squared

x1 <- as.data.frame(c("Результаты лучшей регрессии", "Случайный лес", "Бустинг", "Бустинг и кросс-валидация"))
y1 <- t(as.data.frame(c(summary(model61)$r.squared, last(forest2$rsq), getTrainPerf(gbm_proxy)[2], getTrainPerf(gbm_cv_proxy1)[2])))
svod2 <- cbind(x1,y1)
svod2
colnames(svod2) <- c("Model", "Result")

ggplot(data = svod2[-3,], aes(x = reorder(Model, Result), y = Result, fill = Model)) + geom_col(color = "black") +theme_bw() + ggtitle("Сравнительный анализ методов для модели с прокси-переменными") + xlab("") + ylab("Значение коэффициента детерминации (Rsquared)") + theme(legend.position="none") + scale_fill_manual(values = c("#E5E5E5", "#2CA5A9", "#013F56")) + geom_label(aes(label=round(Result, 2)),check_overlap = TRUE, fill = "white")

#свод двух моделей
svod1$id <- rep("Модель жизненного цикла", 4)
svod2$id <- rep("Модель с прокси-переменными", 4)
svod3 <- rbind(svod1, svod2)
svod3

ggplot(svod3[-c(3,7),], aes(y=Result, x=reorder(Model, Result))) + 
  geom_bar(aes(fill = id), position="dodge", stat="identity", color = "black") + theme_bw() + scale_fill_manual(values = c("#acbdd3", "#101e5a")) + theme(legend.position = "bottom") + 
   ylab("Значение коэффициента детерминации (Rsquared)") + xlab("Модели") + 
  ggtitle("Сравнительный анализ методов") + geom_label(aes(group = id, label = round(Result, 2)),
                                                      position = position_dodge(width = 1)) 


#график структуры капитала - сравнение с Дамодараном
Damodaran <- read_excel("Damodaran.xlsx")
Damodaran1 <- na.omit(Damodaran)
colnames(Damodaran1)[1] <- "ID_Company"
colnames(Damodaran1)[4:7] <- c("18", "19", "20", "21")
Damodaran2 <- Damodaran1 %>% pivot_longer(cols = c("18", "19", "20", "21"), names_to = "Number_Year", values_to = "Market_DE")
sort(names(DataSet2))
data_DE <- DataSet2 %>% transmute(Number_Company, Number_Year, Year, DebtToEquity = (DataSet2$BS_LT_Borrow + DataSet2$BS_ST_Borrow)/DataSet2$Total_Equity)
names(DataSet2)
data_DE1 <- merge(names_data, data_DE, by = "Number_Company")
data_DE2 <- merge(data_DE1, Damodaran2, by = c("Number_Year","ID_Company"))
data_DE3 <- dplyr::filter(data_DE2, DebtToEquity > 0) #удалили отрицательные

#финальное значение D/E и Дамодарана - data_DE2 
data_DE3$Result <- -(data_DE3$Market_DE - data_DE3$DebtToEquity)
#на сколько в среднем по годам компании отличаются от целевой структуры капитала? 
data_DE3$Year_ch <- as.character(data_DE3$Year)
Chart120 <- ggplot(data_DE3, aes(x=Year_ch, y = Result, fill=Year_ch)) +
  geom_boxplot() + theme_bw() + scale_fill_manual(values = c("#BCC8B4", "#E7E1C4", "#F7B78C", "#EF7663")) + xlab("") + ylab("") + labs(fill = "Год") + ggtitle("Насколько есть отличия от целевой структуры капитала по отрасли")
Chart120

#ну прям есть выбросы...
which.min(data_DE3$Result)

#а если без выбросов? 
data_DE4 <- dplyr::filter(data_DE3, Result < 10)
Chart121 <- ggplot(data_DE4, aes(x=Year_ch, y = Result, fill=Year_ch)) +
  geom_boxplot() + theme_bw() + scale_fill_manual(values = c("#BCC8B4", "#E7E1C4", "#F7B78C", "#EF7663")) + xlab("") + ylab("") + labs(fill = "Год") + ggtitle("Насколько есть отличия от целевой структуры капитала по отрасли")
Chart121

#все компании отличаются от целевой структуры капитала, но разброс был шире в 2020 году

#а если по отраслям 
names(data_DE4)
Chart122 <- ggplot(data_DE4, aes(x=IndustryDamodaran, y = Result, fill=IndustryDamodaran)) +
  geom_boxplot() + theme_bw()+ xlab("") + ylab("") + labs(fill = "Отрасль") + ggtitle("Насколько есть отличия от целевой структуры капитала по отрасли") + theme(legend.position = "bottom", axis.text.x = element_text(color = "grey20", size = 4, hjust = .5, vjust = .5, face = "plain"))
Chart122

#изменчивость самого Дамодарана по отраслям
names(data_DE4)
Chart123 <- ggplot(data_DE4, aes(x=IndustryDamodaran, y = Market_DE, fill=IndustryDamodaran)) +
  geom_boxplot() + theme_bw()+ xlab("") + ylab("") + labs(fill = "Отрасль") + ggtitle("Целевая структура по отраслям Дамодаран") + theme(legend.position = "bottom", axis.text.x = element_text(size = 0))
Chart123

#вероятность увеличения дивиденда на акцию в следующем периоде 
increase_data <- DataSet2
delta <- DataSet2 %>% group_by(Number_Company) %>% transmute(Number_Year, delta = (DIV - lag(DIV))/lag(DIV)) %>% ungroup
delta$delta_result <- ifelse(delta$delta>0, 1, 0)
table(delta$delta_result) #выборка не сбалансированная... 8/254
#давайте учитывать коррекцию по модели Литнера: ~ 9.2%
delta$delta_Lintner <- ifelse(delta$delta>0.09, 1, 0)
table(delta$delta_Lintner) #50 на 50, норм
names(delta)
increase_data1 <- merge(increase_data, delta, by = c("Number_Company", "Number_Year"))
View(increase_data1)
increase_data2 <- na.omit(increase_data1)
increase_data3 <- dplyr::filter(increase_data2, delta != "Inf")

increase_data4 <- increase_data3 %>% transmute(delta_Lintner, BS_LT_Borrow, CF_FCFF, 
                            Stage_Indicator1, Z_score, Independent_Directors, WACC, CAPEXtoSales = CAPEX/Sales)

write_xlsx(increase_data4,"Keras1.xlsx")

model69 <- glm(data = increase_data4, delta_Lintner ~ .-delta_Lintner_ch, family = binomial(link = "logit"))
summary(model69)
lrtest(model69) 
vif(model69)
hitmiss(model69)
#если значимо, то модель лучше, чем модель с константой (т.е. они различаются)
#модель лучше, чем модель с константой

#Проверка необходимости удаления переменных через критерий Акаике и тест отношения правдоподобия:
drop1(model69, test = "Chi") #удалим Stage_Indicator1 , Z_score, Independent_Directors

model70 <- update(model69, .~.-Stage_Indicator1-Z_score-Independent_Directors)
summary(model70)

bptest(model70)
stargazer(model69, model70, type = "html", out = "logit.html")
lrtest(model70)
exp(coef(model70))
exp(0.01*coef(model70))

#прогноз для вероятности 
predict(model70, se = TRUE)

#вычисление доверительных интервалов
confint(model70)

#правильно классифицированных наблюдений по сравнению с моделью, содержащей только константу
hitmiss(model70)

#построим прогноз
Pr_model70 <- predict(model70)
Pr_model70 #это z; тут можно сранвить с нулем
Pr_prob_model70 <- predict(model70, type = "response")
Pr_prob_model70

#перейдем к классу
Pr_class_model70 <- ifelse(Pr_prob_model70 > 0.5, "1", "0")
Pr_class_model70 <- factor(Pr_class_model70, levels = c(1, 0))
increase_data4$delta_Lintner_ch <- factor(increase_data4$delta_Lintner, levels = c(1, 0))
?confusionMatrix
mplot_full(increase_data4$delta_Lintner, fitted(model70))
caret::confusionMatrix(Pr_class_model70, increase_data4$delta_Lintner_ch)

y <- increase_data4$delta_Lintner_ch
?plotROC

plotROC(actuals = y, predictedScores = Pr_model70)

#с ДеАнгело
names(increase_data3)
increase_data5 <- increase_data3[,c(1,2,3,74)]
DeAngelo1 <- na.omit(DeAngelo)
increase_data6 <- merge(increase_data5, DeAngelo1, by = c("Number_Company", "Number_Year"))                                               
#259 наблюдений осталось
names(increase_data6)

model71 <- glm(data = increase_data6, delta_Lintner ~ .-Number_Year-Number_Company-Year-PayOrNot-PayoutRatio-DIV-Capitalization, family = binomial(link = "logit"))
vif(model71)
summary(model71)
lrtest(model71) #модель лучше, чем модель с константой
hitmiss(model71)
#если значимо, то модель лучше, чем модель с константой (т.е. они различаются)
#модель лучше, чем модель с константой

write_xlsx(increase_data6, "Keras2.xlsx")
#Проверка необходимости удаления переменных через критерий Акаике и тест отношения правдоподобия:
drop1(model71, test = "Chi") #удалим TEtoTA, AGR, CashtoTA

model72 <- update(model71, .~.-TEtoTA-AGR-CashtoTA)
summary(model72)

stargazer(model71, model72, type = "html", out = "logit1.html")
bptest(model71)
bptest(model72)
round(exp(coef(model72)),3)
round(exp(0.01*coef(model72)),3)
lrtest(model72)
#прогноз для вероятности 
predict(model72, se = TRUE)

#вычисление доверительных интервалов
confint(model72)

#правильно классифицированных наблюдений по сравнению с моделью, содержащей только константу
hitmiss(model72)

#построим прогноз
Pr_model72 <- predict(model72)
Pr_model72 #это z; тут можно сранвить с нулем
Pr_prob_model72 <- predict(model72, type = "response")
Pr_prob_model72

#перейдем к классу
Pr_class_model72 <- ifelse(Pr_prob_model72 > 0.5, "1", "0")
Pr_class_model72 <- factor(Pr_class_model72, levels = c(1,0))
increase_data6$delta_Lintner_ch <- factor(increase_data6$delta_Lintner, levels = c(1,0))
caret::confusionMatrix(Pr_class_model72, increase_data6$delta_Lintner_ch)

y1 <- increase_data6$delta_Lintner_ch 
plotROC(actuals = y1, predictedScores = Pr_model72)

#логит-регрессия. Финальные версии - model70 (increase_data4), model72 (increase_data6)
#начнем с прокси
?createDataPartition
inTraining <- caret::createDataPartition(increase_data4$delta_Lintner, p=.75, list = FALSE) #разделим на 75% и 25% случайным образом
training <- increase_data4[inTraining,]
testing <- increase_data4[-inTraining,]
library(caret)

#10-кратная кроссвалидация
fitControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 10)
#бустинг
set.seed(123)
gbm_model70 <- train(delta_Lintner ~ BS_LT_Borrow + CF_FCFF + WACC + CAPEXtoSales, data = training, method = 'gbm', trControl = fitControl, verbose = FALSE)
gbm_model70
gbm_model70$bestTune
gbm_model70$results
getTrainPerf(gbm_model70)

#сетка для перебора
gbmGrid <- expand.grid(interaction.depth = c(1,2,3), n.trees= (1:15)*50, shrinkage = 0.1, n.minobsinnode = 10)
set.seed(123)
gbm_model70_2 <- train(delta_Lintner ~ BS_LT_Borrow + CF_FCFF + WACC + CAPEXtoSales, data = training, method = 'gbm', trControl = fitControl, verbose = FALSE, tuneGrid = gbmGrid)
getTrainPerf(gbm_model70_2) #самая лучшая модель
gbm_model70_2$bestTune
plot(gbm_model70_2, col = c("#1E1613", "#AB372E", "#D08821")) 
ggplot(gbm_model70_2) + geom_point(col = "darkblue", size = 0.5) + scale_color_manual(values = c("#550445", "#c96588", "#5f73b3")) + theme_bw() + theme(legend.position = "bottom")

set.seed(123)
gbm.fit.final <- gbm(
  formula = delta_Lintner ~ BS_LT_Borrow + CF_FCFF + WACC + CAPEXtoSales,
  data = training,
  n.trees = 50,
  interaction.depth = 1,
  shrinkage = 0.1,
  n.minobsinnode = 10,
  verbose = FALSE
)  

par(mar = c(5, 8, 1, 1))
summary1 <- as.data.frame(summary(
  gbm.fit.final, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
))


names(summary1) 
Chart124 <- summary1 %>% arrange(rel.inf) %>% ggplot(aes(x = var, y = rel.inf, fill = rel.inf)) + geom_col(color = "black") + coord_flip() + theme_bw() + ylab("Относительная важность переменной") + xlab("") + labs("Значения") +
  scale_fill_gradient2(low="#d2cdca", mid="#929fb0",
                        high="#4a5060", midpoint=25) + theme(legend.position = "bottom")

ggplot(summary1, aes(x = reorder(var, rel.inf), y = rel.inf, fill = rel.inf)) + geom_col(color = "black") + coord_flip() + theme_bw() + ylab("Относительная важность переменной") + xlab("") + labs("Значения") +
  scale_fill_gradient2(low="#d2cdca", mid="#929fb0",
                       high="#4a5060", midpoint=25) + theme(legend.position = "bottom")

gbm.fit.final %>%
  pdp::partial(pred.var = "CAPEXtoSales", n.trees = gbm.fit.final$n.trees) %>%
  autoplot(rug = TRUE, train = training, col = "darkblue", size = 1) + theme_bw() + ylab("Прогноз степени корректировки дивидендов больше, чем на 9%")

ggplot(gbm_model70_2, plotType = "level") + scale_fill_gradient2(low="#f7e8e3", mid="#005ec4",
                                                                 high="#02062f", midpoint=0.48) + theme_bw() + theme(legend.position = "bottom") + ylab("Глубина дерева") + xlab("Количество деревьев")

#ROC кривую нельзя задать в виде функции => строим много моделей и смотрим где площадь больше
#оценим модель через максимизацию площади под ROC кривой
set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
training1 <- training
training1$delta_Lintner <- ifelse(training1$delta_Lintner == "1", "X1", "X0")
set.seed(123)
gbm_model70_3 <- train(delta_Lintner ~ BS_LT_Borrow + CF_FCFF + WACC + CAPEXtoSales, data = training1, method = "gbm", trControl = fitControl1, verbose = FALSE, tuneGrid = gbmGrid, metric = "ROC")
gbm_model70_3$bestTune
getTrainPerf(gbm_model70_3)

gbm.fit.final1 <- gbm(
  formula = delta_Lintner ~ BS_LT_Borrow + CF_FCFF + WACC + CAPEXtoSales,
  data = training,
  n.trees = 50,
  interaction.depth = 1,
  shrinkage = 0.1,
  n.minobsinnode = 10,
  verbose = FALSE
)  

par(mar = c(5, 8, 1, 1))
summary(
  gbm.fit.final1, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

#случайный лес для тех же параметров
set.seed(123)
rf_model70 <- train(delta_Lintner ~ BS_LT_Borrow + CF_FCFF + WACC + CAPEXtoSales, tuneLength = 5, data = training1, trControl = fitControl1, metric = "ROC", method = "rf")
rf_model70
rf_model70$bestTune
caret::confusionMatrix(rf_model70)

#дискриминантный анализ
set.seed(123)
lda_model70 <- train(delta_Lintner ~ BS_LT_Borrow + CF_FCFF + WACC + CAPEXtoSales, data = training1, trControl = fitControl1, method = "lda", verbose = FALSE)
lda_model70

library(mvnormtest)
names(training1)
str(training1)
View(training1_11)
training1_11 <- training1
str(training1_11)
training1_11[,1] <- ifelse(training1_11[,1] == "X1", 1, 0)
#на выборке обучения
Shapiro <- t(training1_11[training1_11$delta_Lintner == 1, c(2:8)])
Shapiro1 <- t(training1_11[training1_11$delta_Lintner == 0, c(2:8)])
mshapiro.test(Shapiro)
mshapiro.test(Shapiro1)

library(heplots)
boxM(as.matrix(training1_11[, 2:8]), training1_11$delta_Lintner)

#на всей выборке
increase_data4$delta_Lintner
names(increase_data4)
Shapiro2 <- t(increase_data4[increase_data4$delta_Lintner == 1, c(2:8)])
Shapiro3 <- t(increase_data4[increase_data4$delta_Lintner == 0, c(2:8)])
mshapiro.test(Shapiro2)
mshapiro.test(Shapiro3)

boxM(as.matrix(increase_data4[, 2:8]), increase_data4$delta_Lintner)

#опорные вектора
set.seed(123)
svm_model70 <- train(delta_Lintner ~ BS_LT_Borrow + CF_FCFF + WACC + CAPEXtoSales, data = training1, trControl = fitControl1, method = "svmRadial", verbose = FALSE)
svm_model70

Comparison2 <- resamples(list(RF = rf_model70, SVM = svm_model70, LDA = lda_model70, GBM = gbm_model70_3))
bwplot(Comparison2,  col = "white", fill = "darkblue", par.settings = my.settings, par.strip.text=list(col="white", font=2))
dotplot(Comparison2, par.settings = my.settings, par.strip.text=list(col="white", font=2))

#переходим к модели жизненного цикла - model72 (increase_data6)
inTraining_1 <- createDataPartition(increase_data6$delta_Lintner, p=.75, list = FALSE) #разделим на 75% и 25% случайным образом
training_1 <- increase_data6[inTraining_1,]
testing_1 <- increase_data6[-inTraining_1,]
#10-кратная кроссвалидация
fitControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 10)
#бустинг
set.seed(123)
gbm_model72 <- train(delta_Lintner ~ Stage_Indicator1+REtoTA+ROA+SGR+MB_ratio+Percentile, data = training_1, method = 'gbm', trControl = fitControl, verbose = FALSE)
gbm_model72
gbm_model72$bestTune
gbm_model72$results
getTrainPerf(gbm_model72)

#сетка для перебора
set.seed(123)
gbm_model72_1 <- train(delta_Lintner ~ Stage_Indicator1+REtoTA+ROA+SGR+MB_ratio+Percentile, data = training_1, method = 'gbm', trControl = fitControl, verbose = FALSE, tuneGrid = gbmGrid)
getTrainPerf(gbm_model72_1) #самая лучшая модель
gbm_model72_1$bestTune
ggplot(gbm_model72_1) + geom_point(col = "darkblue", size = 0.5) + scale_color_manual(values = c("#550445", "#c96588", "#5f73b3")) + theme_bw() + theme(legend.position = "bottom")

gbm.fit.final2 <- gbm(
  formula = delta_Lintner ~ Stage_Indicator1+REtoTA+ROA+SGR+MB_ratio+Percentile,
  data = training_1,
  n.trees = 50,
  interaction.depth = 3,
  shrinkage = 0.1,
  n.minobsinnode = 10,
  verbose = FALSE
)  

par(mar = c(5, 8, 1, 1))
summary2 <- summary(
  gbm.fit.final2, 
  cBars = 20,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

summary1 <- as.data.frame(summary(
  gbm.fit.final, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
))

Chart125 <- summary2 %>% arrange(rel.inf) %>% ggplot(aes(x = reorder(var, rel.inf), y = rel.inf, fill = rel.inf)) + geom_col(color = "black") + coord_flip() + theme_bw() + ylab("Относительная важность переменной") + xlab("") + labs("Значения") +
  scale_fill_gradient2(low="#9dd3f7", mid="#bc6596",
                       high="#805ea4", midpoint=19) + theme(legend.position = "bottom")
Chart125

gbm.fit.final2 %>%
  pdp::partial(pred.var = "Stage_Indicator1", n.trees = gbm.fit.final2$n.trees) %>%
  autoplot(rug = TRUE, train = training_1, col = "darkblue", size = 1) + theme_bw() + xlim(0,10)

gbm.fit.final2 %>%
  pdp::partial(pred.var = "ROA", n.trees = gbm.fit.final2$n.trees) %>%
  autoplot(rug = TRUE, train = training, col = "darkblue", size = 1) + theme_bw()

ggplot(gbm_model72_1, plotType = "level") + scale_fill_gradient2(low="#f7e8e3", mid="#005ec4",
                                                                 high="#02062f", midpoint=0.445) + theme_bw() + theme(legend.position = "bottom") + ylab("Глубина дерева") + xlab("Количество деревьев")




#ROC кривую нельзя задать в виде функции => строим много моделей и смотрим где площадь больше
#оценим модель через максимизацию площади под ROC кривой
set.seed(123)
fitControl2 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
training_1_1 <- training_1
training_1_1$delta_Lintner <- ifelse(training_1_1$delta_Lintner == "1", "X1", "X0")
training_1_1
set.seed(123)
gbm_model72_2 <- train(delta_Lintner ~ Stage_Indicator1+REtoTA+ROA+SGR+MB_ratio+Percentile, data = training_1_1, method = "gbm", trControl = fitControl2, verbose = FALSE, tuneGrid = gbmGrid, metric = "ROC")
gbm_model72_2$bestTune
getTrainPerf(gbm_model72_2)

#финал - gbm_model_72_2

#случайный лес для тех же параметров
set.seed(123)
rf_model72 <- train(delta_Lintner ~ Stage_Indicator1+REtoTA+ROA+SGR+MB_ratio+Percentile, data = training_1_1, trControl = fitControl1, metric = "ROC", method = "rf")
rf_model72

training_1
training_1_1
testing_1

names(training_1_1)
train_rf <- training_1_1[,c(5,6,8,9,11,16)]
test_rf <- testing_1[,c(5,6,8,9,11,16)]
str(test_rf)
explainer_rf <- lime(train_rf, rf_model72)
explanation_rf <- explain(test_rf, explainer_rf, n_labels = 1, n_features = 4)
explanation_rf_1 <- explain(test_rf[c(1:4),], explainer_rf, n_labels = 1, n_features = 4)

plot_features(explanation_rf_1) +
  labs(title = "LIME Feature Importance Visualization",
       subtitle = "Hold Out (Test) Set, First 4 Cases Shown") + scale_fill_manual(values = c("#1d840e", "#ee192d")) + theme_bw() + theme(legend.position = "bottom") + xlab("")

plot_explanations(explanation_rf_1) +
  labs(title = "LIME Feature Importance Heatmap",
       subtitle = "Hold Out (Test) Set, First 4 Cases Shown") + scale_fill_gradient2(low="#acbdd3", mid="#5d80b6",
                                                                                        high="#101e5a", midpoint=0.1) + xlab("")
y_train_rf <- training_1[,4]
str(train_rf)
# Feature correlations to Churn
names(train_rf)
cor_train_rf<- cor(as.matrix(training_1[,c(4,5,6,8,9,11,16)]))
corrr_analysis <- as.data.frame(cor_train_rf[-1,1])
corrr_analysis$feature <- c("Stage_Indicator1", "REtoTA", "ROA", "SGR", "MB_ratio", "Percentile")
colnames(corrr_analysis) <- c("Churn", "feature")
corrr_analysis %>% ggplot(aes(x = Churn, y = fct_reorder(feature, desc(Churn)))) +
  geom_point() +
  geom_segment(aes(xend = 0, yend = feature), 
               color = palette_light()[[2]], 
               data = corrr_analysis %>% filter(Churn > 0)) +
  geom_point(color = palette_light()[[2]], 
             data = corrr_analysis %>% filter(Churn > 0)) +
  geom_segment(aes(xend = 0, yend = feature), 
               color = palette_light()[[1]], 
               data = corrr_analysis %>% filter(Churn < 0)) +
  geom_point(color = palette_light()[[1]], 
             data = corrr_analysis %>% filter(Churn < 0)) +
  geom_vline(xintercept = 0, color = palette_light()[[5]], size = 1, linetype = 2) +
  geom_vline(xintercept = -0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
  geom_vline(xintercept = 0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
  theme_tq() +
  labs(title = "Churn Correlation Analysis",
       subtitle = paste("Positive Correlations (contribute to churn),",
                        "Negative Correlations (prevent churn)"),
       y = "Feature Importance")


caret::confusionMatrix(rf_model72)

library(geomlime)

ggplot(DataSet2, aes(Net_Income, DIV)) +
  geom_pint(size = 3) + theme_bw()

ggplot(DataSet2, aes(Net_Income, DIV)) +
  geom_lime(size = 3) + theme_bw()
library(remotes)
remotes::install_github("R-CoderDotCom/ggdogs@main")
library(ggdogs)
Table16
ggplot(Table16, aes(Intangible_Assets, Tangible_Assets )) +
  geom_dog(size = 3) + theme_bw()

df <- data.frame(Table16$Intangible_Assets, Table16$Tangible_Assets,
                 dog = rep("pug", 24))

# Plot
colnames(df) <- c("Intangible_Assets", "Tangible_Assets", "dog")
ggplot(df, aes(x = Intangible_Assets, y = Tangible_Assets, dog = dog)) +
  geom_dog(size = 5) + theme_bw()

remotes::install_github("R-CoderDotCom/ggbernie")
library(ggbernie)
ggplot(Table16, aes(Intangible_Assets, Tangible_Assets )) +
  geom_bernie(bernie = "asking") + theme_bw()

#дискриминантный анализ
set.seed(123)

lda_model72 <- train(delta_Lintner ~ Stage_Indicator1+REtoTA+ROA+SGR+MB_ratio+Percentile, data = training_1_1, trControl = fitControl1, method = "lda", verbose = FALSE)
lda_model72

#опорные вектора
set.seed(123)
svm_model72 <- train(delta_Lintner ~ Stage_Indicator1+REtoTA+ROA+SGR+MB_ratio+Percentile, data = training_1_1, trControl = fitControl1, method = "svmRadial", verbose = FALSE)
svm_model72

Comparison3 <- resamples(list(RF = rf_model72, SVM = svm_model72, LDA = lda_model72, GBM = gbm_model72_2))
myColours <- brewer.pal(6,"Blues")

my.settings <- list(
  superpose.polygon=list(col=myColours[2:5], border="transparent"),
  strip.background=list(col=myColours[6]),
  strip.border=list(col="black")
)
bwplot(Comparison3,  col = "white", fill = "darkblue", par.settings = my.settings, par.strip.text=list(col="white", font=2))
?bwplot
ggplot(Comparison3)
dotplot(Comparison3, par.strip.text=list(col="white", font=2), par.settings = my.settings)

#поиск различий
summary(diff(Comparison2))
summary(diff(Comparison3))

Comparison4 <- resamples(list(RF1 = rf_model70, RF2 = rf_model72))
summary(diff(Comparison4))
bwplot(Comparison4,  col = "white", fill = "darkblue", par.settings = my.settings, par.strip.text=list(col="white", font=2))
dotplot(Comparison4, par.strip.text=list(col="white", font=2), par.settings = my.settings)

Comparison5 <- resamples(list(SVM1 = svm_model70, SVM2 = svm_model72))
summary(diff(Comparison5))
bwplot(Comparison5,  col = "white", fill = "darkblue", par.settings = my.settings, par.strip.text=list(col="white", font=2))
dotplot(Comparison5, par.strip.text=list(col="white", font=2), par.settings = my.settings)

Comparison6 <- resamples(list(LDA1 = lda_model70, LDA2 = lda_model72))
summary(diff(Comparison6))
bwplot(Comparison6,  col = "white", fill = "darkblue", par.settings = my.settings, par.strip.text=list(col="white", font=2))
dotplot(Comparison6, par.strip.text=list(col="white", font=2), par.settings = my.settings)

Comparison7 <- resamples(list(GBM1 = gbm_model70_3, GBM2 = gbm_model72_2))
summary(diff(Comparison7))
bwplot(Comparison7,  col = "white", fill = "darkblue", par.settings = my.settings, par.strip.text=list(col="white", font=2))
dotplot(Comparison7, par.strip.text=list(col="white", font=2), par.settings = my.settings)


#случайный лес 
#возьму модель жизненного цикла

#пример дерева
example <- rpart(DIV ~., data = DeAngelo2, control = rpart.control(minsplit = 10, minbucket = 25, maxdepth = 4)) 
library(rpart.plot)
rpart.plot(example, type = 2, extra = 1) 
names(DeAngelo1)
DeAngelo2 <- DeAngelo1[,-c(1,2,9,12,13,15)]
forest1 <- randomForest(DIV ~ ., data = DeAngelo2, localImp = TRUE)
forest1
min_depth_frame <- min_depth_distribution(forest1)
head(min_depth_frame, n = 10)
#график распределения минимальной глубины для десяти лучших переменных
plot_min_depth_distribution(min_depth_frame) + 
  xlab("Переменные") +
  ylab("Количество деревьев")+
  scale_fill_manual(values = c("#003f5c", "#2f4b7c", "#665191", "#a05195", "#d45087", "#f95d6a", "#ff7c43", "#ffa600", 
                                                                            "#e45149", "#b6d2dd", "#259086", "#0a463c"))
varImpPlot(forest1) #показывает степень важности переменных, лучше всего Impurity (загрязненность) сокращается, когда начинаю делить с той, а не с иной переменной
#самые нижние - самые не информативные, самые верхние - самые информативные
plot(forest1) #переобучения нет
importance_frame <- measure_importance(forest1)
importance_frame
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")
plot_multi_way_importance(importance_frame, x_measure = "mse_increase", y_measure = "node_purity_increase", size_measure = "p_value", no_of_labels = 5)
names(DeAngelo2)
plot_predict_interaction(forest1, DeAngelo2, "Stage_Indicator1", "TEtoTA")

vars <- important_variables(importance_frame, k = 5, measures = c("mean_min_depth", "no_of_trees"))
interactions_frame <- min_depth_interactions(forest1, vars)
plot_min_depth_interactions(interactions_frame)
explain_forest(forest1, interactions = TRUE, data = DeAngelo2)

names(DeAngelo2)
DeAngelo2_mean <- transmute(DeAngelo2, REtoTA = mean(REtoTA), Stage_Indicator1 = Stage_Indicator1, 
                            TEtoTA = mean(TEtoTA), ROA = mean(ROA), SGR = mean(SGR), AGR = mean(AGR), 
                            CashtoTA = mean(CashtoTA), Percentile = mean(Percentile))
Predict_values <- predict(forest1, newdata = DeAngelo2_mean)
DeAngelo2_mean1 <- mutate(DeAngelo2_mean, Predict_values)
ggplot(DeAngelo2_mean1, aes(x = Stage_Indicator1, y = Predict_values)) + geom_line(color = "darkblue", size = 1) + theme_bw() + xlab("Стадия жизненного цикла (Stage_Indicator1)") + ylab("Прогнозные значения дивиденда на одну акцию (DIV)") + ggtitle("Отражение зависимости между стадией жизненного цикла и 
дивидендом на одну акцию для среднего наблюдения по выборке")

DeAngelo2_mean2 <- transmute(DeAngelo2, REtoTA = mean(REtoTA), Stage_Indicator1 = mean(Stage_Indicator1), 
                            TEtoTA = TEtoTA, ROA = mean(ROA), SGR = mean(SGR), AGR = mean(AGR), 
                            CashtoTA = mean(CashtoTA), Percentile = mean(Percentile))
Predict_values1 <- predict(forest1, newdata = DeAngelo2_mean2)
DeAngelo2_mean3 <- mutate(DeAngelo2_mean2, Predict_values1)
ggplot(DeAngelo2_mean3, aes(x = TEtoTA, y = Predict_values1)) + geom_line(color = "darkblue", size = 1) + theme_bw() + xlab("Соотношение собственного капитала к совокупным активам (TEtoTA)") + ylab("Прогнозные значения дивиденда на одну акцию (DIV)") + ggtitle("Отражение зависимости между долей собственного капитала в валюте баланса и 
дивидендом на одну акцию для среднего наблюдения по выборке")

DeAngelo2_mean4 <- transmute(DeAngelo2, REtoTA = REtoTA, Stage_Indicator1 = mean(Stage_Indicator1), 
                             TEtoTA = mean(TEtoTA), ROA = mean(ROA), SGR = mean(SGR), AGR = mean(AGR), 
                             CashtoTA = mean(CashtoTA), Percentile = mean(Percentile))
Predict_values2 <- predict(forest1, newdata = DeAngelo2_mean4)
DeAngelo2_mean5 <- mutate(DeAngelo2_mean4, Predict_values2)
ggplot(DeAngelo2_mean5, aes(x = REtoTA, y = Predict_values2)) + geom_line(color = "darkblue", size = 1) + theme_bw() + xlab("Соотношение нераспределенной прибыли к совокупным активам (REtoTA)") + ylab("Прогнозные значения дивиденда на одну акцию (DIV)") + ggtitle("Отражение зависимости между долей нераспределенной прибыли в валюте баланса и 
дивидендом на одну акцию для среднего наблюдения по выборке")

#бустинг для жизненного цикла
set.seed(123)
fitControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 10)
gbm_cycle <- train(DIV ~ ., data = DeAngelo2, method = 'gbm', verbose = FALSE)
gbm_cycle
gbm_cycle$bestTune
getTrainPerf(gbm_cycle)

ggplot(gbm_cycle) + geom_point(col = "darkblue", size = 0.5) + scale_color_manual(values = c("#550445", "#c96588", "#5f73b3")) + theme_bw() + theme(legend.position = "bottom")
#глубина - 3 лучшая

gbm.fit.final_cycle <- gbm(
  formula = DIV ~ .,
  data = DeAngelo2,
  n.trees = 150,
  interaction.depth = 3,
  shrinkage = 0.1,
  n.minobsinnode = 10,
  verbose = FALSE
)  

par(mar = c(5, 8, 1, 1))
summary_cycle <- as.data.frame(summary(
  gbm.fit.final_cycle, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
))
names(summary1) 
ggplot(summary_cycle, aes(x = reorder(var, rel.inf), y = rel.inf, fill = rel.inf)) + geom_col(color = "black") + coord_flip() + theme_bw() + ylab("Относительная важность переменной") + xlab("") + labs("Значения") +
  scale_fill_gradient2(low="#d2cdca", mid="#929fb0",
                       high="#4a5060", midpoint=15) + theme(legend.position = "bottom")

explainer_cycle <- lime::lime(
  x              = DeAngelo2, 
  model          = gbm_cycle, 
  bin_continuous = FALSE
)

explanation_cycle_all <- lime::explain(
  DeAngelo2, 
  explainer    = explainer_cycle, 
  n_labels     = 1, 
  n_features   = 3,
  kernel_width = 0.5
)

explanation_cycle_all1 <- explanation_cycle_all %>% group_by(case, feature) %>% dplyr::arrange(-model_r2)
explanation_cycle_all1$prediction <- round(explanation_cycle_all1$prediction, 2)
View(explanation_cycle_all1)

plot_features(explanation_cycle_all1[1:12,]) +
  labs(title = "Важность переменных для конкретных случаев",
       subtitle = "Взяты 4 случая с большей объясняющей силой") +  scale_fill_manual(values = c("#4f6d9c", "#ced6e3")) + theme_minimal()+theme(legend.position = "bottom") + xlab("")

plot_explanations(explanation_cycle_all1[1:60,]) +
  labs(title = "Важность переменных для конкретных случаев",
       subtitle = "Взяты 20 случаев с большей объясняющей силой") + theme_bw()+ scale_fill_gradient2(low="#acbdd3", mid="#5d80b6",
                                                                                     high="#101e5a", midpoint=-1) 

names(DeAngelo2)
cor_gbm_cycle<- cor(as.matrix(DeAngelo2[,]))
corrr_gbm_analysis <- as.data.frame(cor_gbm_cycle[-8,8])
corrr_gbm_analysis$feature <- c("Stage_Indicator1", "REtoTA", "TEtoTA", "ROA", "SGR", "AGR", "CashtoTA", "Percentile")
colnames(corrr_gbm_analysis) <- c("DIV", "feature")
g1 <- corrr_gbm_analysis %>% ggplot(aes(x = DIV, y = fct_reorder(feature, desc(DIV)))) +
  geom_point() +
  geom_segment(aes(xend = 0, yend = feature), 
               color = palette_light()[[2]], 
               data = corrr_gbm_analysis %>% filter(DIV > 0)) +
  geom_point(color = palette_light()[[2]], 
             data = corrr_analysis %>% filter(DIV > 0)) +
  geom_segment(aes(xend = 0, yend = feature), 
               color = palette_light()[[1]], 
               data = corrr_gbm_analysis %>% filter(DIV < 0)) +
  geom_point(color = palette_light()[[1]], 
             data = corrr_gbm_analysis %>% filter(DIV < 0)) +
  geom_vline(xintercept = 0, color = palette_light()[[5]], size = 1, linetype = 2) +
  geom_vline(xintercept = -0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
  geom_vline(xintercept = 0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
  theme_tq() +
  labs(title = "Корреляционная матрица с дивидендом на акцию (DIV)",
       y = "Переменные")


#бустинг + кросс-валидация 
fitControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 10)
inTraining_gbm_cv <- caret::createDataPartition(DeAngelo2$DIV, p=.75, list = FALSE) #разделим на 75% и 25% случайным образом
training_cv <- DeAngelo2[inTraining_gbm_cv,]
testing_cv <- DeAngelo2[-inTraining_gbm_cv,]
set.seed(123)
names(training_cv)
gbm_cv <- train(DIV ~ ., data = training_cv, method = 'gbm', trControl = fitControl, verbose = FALSE)
gbm_cv$bestTune
gbm_cv$results
getTrainPerf(gbm_cv)

#введем сетку для перебора
gbmGrid <- expand.grid(interaction.depth = c(1,2,3), n.trees= (1:15)*50, shrinkage = 0.1, n.minobsinnode = 10)
set.seed(123)
gbm_cv_1 <- train(DIV ~ ., data = training_cv, method = 'gbm', trControl = fitControl, verbose = FALSE, tuneGrid = gbmGrid)
getTrainPerf(gbm_cv_1) #самая лучшая модель
gbm_cv_1$bestTune
ggplot(gbm_cv_1) + geom_point(col = "darkblue", size = 0.5) + scale_color_manual(values = c("#550445", "#c96588", "#5f73b3")) + theme_bw() + theme(legend.position = "bottom")

set.seed(123)
gbm.fit_cv <- gbm(
  formula = DIV ~ .,
  data = training_cv,
  n.trees = 300,
  interaction.depth = 3,
  shrinkage = 0.1,
  n.minobsinnode = 10,
  verbose = FALSE
)  

par(mar = c(5, 8, 1, 1))
summary_cv <- as.data.frame(summary(
  gbm.fit_cv, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
))

ggplot(summary_cv, aes(x = reorder(var, rel.inf), y = rel.inf, fill = rel.inf)) + geom_col(color = "black") + coord_flip() + theme_bw() + ylab("Относительная важность переменной") + xlab("") + labs("Значения") +
  scale_fill_gradient2(low="#d2cdca", mid="#929fb0",
                       high="#4a5060", midpoint=18) + theme(legend.position = "bottom")

ggplot(gbm_cv_1, plotType = "level") + scale_fill_gradient2(low="#f7e8e3", mid="#005ec4",
                                                                 high="#02062f", midpoint=1.32) + theme_bw() + theme(legend.position = "bottom") + ylab("Глубина дерева") + xlab("Количество деревьев")

gbm.fit_cv %>%
  pdp::partial(pred.var = "Stage_Indicator1", n.trees = gbm.fit_cv$n.trees) %>%
  autoplot(rug = TRUE, train = training_cv, col = "darkblue", size = 1) + theme_bw() + ylab("Прогноз дивиденда на одну акцию")

explainer_gbm_cv <- lime::lime(
  x              = DeAngelo2, 
  model          = gbm_cv_1, 
  bin_continuous = FALSE
)

explanation_gbm_cv_all <- lime::explain(
  DeAngelo2, 
  explainer    = explainer_gbm_cv, 
  n_labels     = 1, 
  n_features   = 3,
  kernel_width = 0.5
)

explanation_gbm_cv_all1 <- explanation_gbm_cv_all %>% group_by(case, feature) %>% dplyr::arrange(-model_r2)
explanation_gbm_cv_all1$prediction <- round(explanation_gbm_cv_all1$prediction, 2)
View(explanation_gbm_cv_all1)

plot_features(explanation_gbm_cv_all1[1:12,]) +
  labs(title = "Важность переменных для конкретных случаев",
       subtitle = "Взяты 4 случая с большей объясняющей силой") +  scale_fill_manual(values = c("#4f6d9c", "#ced6e3")) + theme_minimal()+theme(legend.position = "bottom") + xlab("")

plot_explanations(explanation_gbm_cv_all1[1:60,]) +
  labs(title = "Важность переменных для конкретных случаев",
       subtitle = "Взяты 20 случаев с большей объясняющей силой") + theme_bw()+ scale_fill_gradient2(low="#acbdd3", mid="#5d80b6",
                                                                                                     high="#101e5a", midpoint=-1) 
#сравнение моделей: регрессия / случайный лес / бустинг 

getTrainPerf(gbm_cv_1)[2]
getTrainPerf(gbm_cycle)[2]
last(forest1$rsq)
summary(model4)$adj.r.squared

x <- as.data.frame(c("Результаты лучшей регрессии", "Случайный лес", "Бустинг", "Бустинг и кросс-валидация"))
y <- t(as.data.frame(c(summary(model4)$r.squared, last(forest1$rsq), getTrainPerf(gbm_cycle)[2], getTrainPerf(gbm_cv_1)[2])))
svod1 <- cbind(x,y)
svod1
colnames(svod1) <- c("Model", "Result")

ggplot(data = svod1[-3,], aes(x = reorder(Model, Result), y = Result, fill = Model)) + geom_col(color = "black") +theme_bw() + ggtitle("Сравнительный анализ методов для модели жизненного цикла") + xlab("") + ylab("Значение коэффициента детерминации (Rsquared)") + theme(legend.position="none") + scale_fill_manual(values = c("#E5E5E5",  "#2CA5A9", "#013F56")) + geom_label(aes(label=round(Result, 2)),check_overlap = TRUE, fill = "white")

#прокси + Board_size
data_sum2 <- DataSet2 %>% transmute(Number_Company, DIV, DivToSales = Dividends/Sales, Dividends = log(Dividends), DivToTA = Dividends/BS_Total_Assets, BS_LT_Borrow, CF_FCFF, 
                                    Stage_Indicator1, Z_score, DebtToEquity = (DataSet2$BS_LT_Borrow + DataSet2$BS_ST_Borrow)/DataSet2$Total_Equity, Independent_Directors, WACC, CAPEXtoSales = CAPEX/Sales, Board_Size)
data_sum2$Dividends <- ifelse(data_sum2$Dividends== "-Inf", 0, data_sum2$Dividends)
data_sum2$DivToTA <- ifelse(data_sum2$DivToTA== "-Inf", 0, data_sum2$DivToTA)
names(data_sum2)
model73 <- lm(data = data_sum2, DIV ~ .-DivToSales-Dividends-DivToTA-Number_Company)
summary(model73)
bptest(model73) #есть гетероскедастичность
stepAIC(model73)
model74 <- update(model73, .~.-Z_score)
summary(model74)
bptest(model74) #есть гетероскедастичность

model75 <- lm(data = data_sum2, DivToSales ~ .-DIV-Dividends-DivToTA-Number_Company)
summary(model75)
stepAIC(model75)
model76 <- update(model75, .~.-BS_LT_Borrow -Stage_Indicator1-DebtToEquity)
summary(model76)
bptest(model76) #нет гетероскедастичности

model77 <- lm(data = data_sum2, Dividends ~ . - DivToSales-DIV-DivToTA-Number_Company)
summary(model77)
stepAIC(model77)
model78 <- update(model77, .~.-Z_score-Stage_Indicator1-DebtToEquity-BS_LT_Borrow)
summary(model78)
bptest(model78) #есть гетероскедастичность

stargazer(model73, model74, model76, model78, type = "html", df = FALSE, se = list(cse(model73), cse(model74), cse(model76), cse(model78)), out = "table_proxy.html")

cor(data_sum2$Independent_Directors, data_sum2$Board_Size)

data_sum2$Number_Company <- as.factor(data_sum2$Number_Company)
model79 <- lm(data = data_sum2, DIV ~ .-DivToSales-Dividends-DivToTA)
summary(model79)

#прокси - кросс-валидация
TR <- trainControl(method = "cv", number = 10) #10-кратная кросс-валидация
#делаем модель аналогичную модели 74
names(data_sum)
model92 <- train(data = data_sum, DIV ~ BS_LT_Borrow + CF_FCFF + Stage_Indicator1 + DebtToEquity + Independent_Directors + WACC, method = 'lm', trControl = TR)
model92

#модель по модели жизненного цикла 
names(DeAngelo1)
model93 <- train(data = DeAngelo1, DIV ~ Stage_Indicator1 + REtoTA + TEtoTA + SGR + CashtoTA + Percentile, method = 'lm', trControl = TR)
model93

Comparison <- resamples(list(model92, model93))
summary(Comparison)

bwplot(Comparison, col = "white", par.settings=list(box.rectangle=list(col="darkblue",fill = "darkblue"), box.umbrella=list(col="darkblue")))
dotplot(Comparison, metric = "Rsquared")  #не отличаются, так как доверительные интервалы пересекаются
xyplot(Comparison, metric = "Rsquared")
dotplot(Comparison, metric = "MAE") 
dotplot(Comparison, metric = "RMSE")
#добавим сравнение со случайным лесом
model94 <- train(data = data_sum, DIV ~ BS_LT_Borrow + CF_FCFF + Stage_Indicator1 + DebtToEquity + Independent_Directors + WACC, method = 'rf', trControl = TR)
model95 <- train(data = DeAngelo1, DIV ~ Stage_Indicator1 + REtoTA + TEtoTA + SGR + CashtoTA + Percentile, method = 'rf', trControl = TR)
Comparison1 <- resamples(list(model92, model93, model94, model95))
summary(Comparison1)
bwplot(Comparison1, col = "white", par.settings=list(box.rectangle=list(col="darkblue",fill = "darkblue"), box.umbrella=list(col="darkblue")))
dotplot(Comparison1, metric = "Rsquared") #модель 1 и модель 3 отличаются!
dotplot(Comparison1, metric = "MAE") 
dotplot(Comparison1, metric = "RMSE") 

#Гипотеза 8 - Величина CAPEX в среднем ниже в те годы, в которых дивиденды выплачиваются из кумулятивной нераспределенной прибыли.
hyp8 <- DataSet2 %>% transmute(DIV, DivToSales = Dividends/Sales, Dividends = log(Dividends), DivToTA = Dividends/BS_Total_Assets, BS_LT_Borrow, CF_FCFF, 
                               Stage_Indicator1, Z_score, DebtToEquity = (DataSet2$BS_LT_Borrow + DataSet2$BS_ST_Borrow)/DataSet2$Total_Equity, Independent_Directors, WACC, CAPEX, Number_Year, Number_Company, PayoutRatio, Net_Income)
hyp8$id_cum_pr <- ifelse(hyp8$PayoutRatio > 100, 1, ifelse(hyp8$Net_Income < 0, 1, 0))

model80 <- lm(data = hyp8, CAPEX ~ . + I(DIV*id_cum_pr)-id_cum_pr - DivToSales-Dividends-DivToTA-Number_Year-Number_Company-PayoutRatio-Net_Income)
summary(model80) #по обычной регрессии нет различий
stepAIC(model80)
vif(model80)
bptest(model80)
stargazer(model80, type = 'html', out = 'model80.html')
table(hyp8$id_cum_pr)

model81 <- lm(data = hyp8, Z_score ~ . + I(DIV*id_cum_pr)-id_cum_pr - DivToSales-Dividends-DivToTA-Number_Year-Number_Company-PayoutRatio-Net_Income)
summary(model81)
vif(model81)
stepAIC(model81)
model96 <- update(model81, .~.-DebtToEquity-Stage_Indicator1-Independent_Directors)
summary(model96)
bptest(model96)
stargazer(model81, model96, type = "html", out = "models.html")

#тестирование гипотезы 11 - нематериальные активы
names(DeAngelo)
Intang <- data.frame(DataSet2$Number_Year, DataSet2$Number_Company, DataSet2$Intangible_Assets)
colnames(Intang) <- c("Number_Year", "Number_Company", "Intangible_Assets")
DeAngelo4 <- as.data.frame(DeAngelo)
str(DeAngelo4)
str(Intang)
DeAngelo3 <- merge(DeAngelo4, Intang, by = c("Number_Year", "Number_Company"))
names(DeAngelo3)

hyp11 <- DataSet2 %>% transmute(Number_Year, Number_Company, DIV, DivToTA = Dividends/BS_Total_Assets, DivToSales = Dividends/Sales, Dividends = log(Dividends), 
                                Stage_Indicator1, REtoTA = RE_period / BS_Total_Assets , 
                                TEtoTA = Total_Equity / BS_Total_Assets , 
                                ROA = ROA, 
                                SGR = SGR$SGR, #из отдельного дата-сета
                                AGR = AGR, 
                                CashtoTA = Cash / BS_Total_Assets, 
                                Percentile = Percentile$Percentile, 
                                Intangible_Assets = Intangible_Assets)
hyp11_1 <- na.omit(hyp11)
model97 <- lm(data = hyp11_1, DIV ~ . -DivToTA-DivToSales-Dividends-Number_Year-Number_Company)
summary(model97)
stepAIC(model97)
model98 <- update(model97, .~.-CashtoTA-ROA-AGR)
summary(model98)
bptest(model98) #есть гетероскедастичность

model99 <- lm(data = hyp11_1, DivToSales ~ . - DIV-DivToTA-Dividends-Number_Year-Number_Company)
summary(model99)
stepAIC(model99)
model100 <- update(model99,.~.-Percentile-CashToTA-TEtoTA-REtoTA-CashtoTA)
summary(model100)
bptest(model100) #нет гетероскедастичности

hyp11_1$Dividends <- ifelse(hyp11_1$Dividends == "-Inf", 0, hyp11_1$Dividends)
model101 <- lm(data = hyp11_1, Dividends ~ . - DIV-DivToSales-DivToTA-Number_Year-Number_Company)
summary(model101)
stepAIC(model101)
model102 <- update(model101,.~.-TEtoTA)
summary(model102)
bptest(model102) #есть гетероскедастичность

stargazer(model98, model100, model102, type = "html", df = FALSE, se = list(cse(model98), cse(model100), cse(model102)), out = "intang.html")
stargazer(model98, model100, model102, type = "html", out = "intang1.html")

cor(DataSet2$CAPEX, DataSet2$Intangible_Assets)

#идентифицируем высокотехнологичные компании
hyp11_2 <- hyp11_1
hyp11_3 <- merge(hyp11_1 %>% group_by(Number_Year) %>% summarise(Intang_median = median(Intangible_Assets)), hyp11_2, by = "Number_Year")

hyp11_3$id_tech <- ifelse(hyp11_3$Intangible_Assets > hyp11_3$Intang_median, 1, 0)

names(hyp11_3)
model103 <- lm(data = hyp11_3[,-c(1,2,3,5,6,7)], DIV ~ .+I(Intangible_Assets*id_tech)-id_tech)
summary(model103)
stepAIC(model103)
model104 <- update(model103, .~.-ROA-CashtoTA-AGR)
summary(model104)
bptest(model104) #есть гетероскедастичность

model105 <- lm(data = hyp11_3[,-c(1,2,3,4,5,7)], DivToSales ~ .+I(Intangible_Assets*id_tech)-id_tech)
summary(model105)
stepAIC(model105)
model106 <- update(model105, .~.-Percentile-CashtoTA-TEtoTA-REtoTA)
summary(model106)
bptest(model106) #нет гетероскедастичности

model107 <- lm(data = hyp11_3[,-c(1,2,3,4,5,6)], Dividends ~ .+I(Intangible_Assets*id_tech)-id_tech)
summary(model107)
stepAIC(model107)
model108 <- update(model107, .~.-TEtoTA)
summary(model108)
bptest(model108) #есть гетероскедастичности

stargazer(model104, model106, model108, type = "html", df = FALSE, se = list(cse(model104), cse(model106), cse(model108)), out = "intang2.html")
stargazer(model104, model106, model108, type = "html", out = "intang3.html")

hyp11_4 <- hyp11_2
hyp11_4$id_tech <- ifelse(hyp11_4$Intangible_Assets > median(hyp11_4$Intangible_Assets), 1, 0)
names(hyp11_4)
model109 <- lm(data = hyp11_4[,-c(1,2,4,5,6)], DIV ~ .+I(Intangible_Assets*id_tech)-Intangible_Assets-id_tech)
summary(model109)

#Незрелые компании имеют структуру капитала, смещенную в сторону долгосрочных заимствований.
View(Clusters_Data)
#есть Number_Cluster1, который поделен по медианному значению Stage_Indicator1
hyp6 <- Clusters_Data %>% transmute(DIV, Dividends = DIV*EQY_sh_out, Dividends1 = log(Dividends), DivToSales = Dividends/Sales, Dividends = log(Dividends), DivToTA = Dividends/BS_Total_Assets, BS_LT_Borrow, CF_FCFF, 
                                    Stage_Indicator1, Z_score, DebtToEquity = (DataSet2$BS_LT_Borrow + DataSet2$BS_ST_Borrow)/DataSet2$Total_Equity, Independent_Directors, WACC, CAPEX, Number_Cluster1, part_of_LR = BS_LT_Borrow/(BS_LT_Borrow+BS_ST_Borrow))
model82 <- lm(data = hyp6, DIV ~ . - Dividends1-Dividends-DivToSales-BS_LT_Borrow-Stage_Indicator1-DivToTA)
summary(model82)

bptest(model82) #есть гетероскедастичность

stepAIC(model82)

model83 <- update(model82, .~.-DebtToEquity-CAPEX-CF_FCFF)
summary(model83)

cor(DataSet2$BS_LT_Borrow, DataSet2$Stage_Indicator1)
cor(DataSet2$BS_LT_Borrow, DataSet2$CAPEX)

hyp6$Dividends <- ifelse(hyp6$Dividends== "-Inf", 0, hyp6$Dividends)
hyp6$Dividends1 <- ifelse(hyp6$Dividends1== "-Inf", 0, hyp6$Dividends1)
hyp6$DivToTA <- ifelse(hyp6$DivToTA== "-Inf", 0, hyp6$DivToTA)

model84 <- lm(data = hyp6, Dividends1 ~ . - Dividends-DIV-DivToSales-BS_LT_Borrow-Stage_Indicator1-DivToTA)
summary(model84)
bptest(model84) #есть гетероскедастичность
stepAIC(model84)

model85 <- update(model84, .~.-DebtToEquity-Z_score)
summary(model85)

stargazer(model83, model85, type = "html", out = "hyp6.html", df = FALSE, se = list(cse(model83), cse(model84)))

model86 <- lm(data = hyp6, DIV ~ . - Dividends1-Dividends-DivToSales-BS_LT_Borrow-Stage_Indicator1-DivToTA-Number_Cluster1+I(Number_Cluster1*part_of_LR))
summary(model86)
stepAIC(model86)
model87 <- update(model86, .~.-CAPEX-DebtToEquity-CF_FCFF)
summary(model87) 
bptest(model87) #есть

model88 <- lm(data = hyp6, Dividends1 ~ . - Dividends-DIV-DivToSales-BS_LT_Borrow-Stage_Indicator1-DivToTA-Number_Cluster1+I(Number_Cluster1*part_of_LR))
summary(model88)
stepAIC(model88)
model89 <- update(model88, .~.-DebtToEquity-Z_score)
summary(model89)
bptest(model89) #есть

stargazer(model87, model89, type = "html", out = "hyp6-1.html", df = FALSE, se = list(cse(model87), cse(model89)))

model90 <- glm(data = hyp6, Number_Cluster1 ~ . -Dividends1- Dividends-DIV-DivToSales-BS_LT_Borrow-Stage_Indicator1-DivToTA, family = binomial(link = "logit"))
summary(model90)
stepAIC(model90)
model91 <- update(model90, .~.-DebtToEquity)
summary(model91)
bptest(model91) #гетероскедастичности на 1% уровне нет

stargazer(model91, type = "html", out = "hype-2.html")

#таким образом, в выборке есть устойчивые незрелые компании на рассматриваемом промежутке времени
#устойчиво незрелые компании: 1, 2, 6, 7, 15, 22, 23, 27, 28, 
#устойчиво зрелые компании: 5, 8, 13, 14, 16, 18, 20, 24, 
#спорные: 4, 9, 12, 19, 21, 26, 29, 30

#возьмем по каждой компании: незрелые - 1, зрелые - 5, спорные - 4

data_animate <- DataSet2 %>% transmute(Number_Year, Number_Company, Dividends, DIV)
data_animate$Result <- ifelse(data_animate$Number_Company %in% c(1,2, 6, 7, 15, 22, 23, 27, 28), "Sustainable NonMature", ifelse(data_animate$Number_Company %in% c(5, 8, 13, 14, 16, 18, 20, 24), "Sustainable Mature", "Controversial"))

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
m <- gganimate::animate(Plot1, duration = 5, fps = 20, width = 1000, height = 1000, renderer = gifski_renderer())
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

#новая модель Фама-Френч с ребалансировкой
View(FamaFrench)
for (i in 10:21){
  assign(paste0("FamaFrench_upd", i), dplyr::filter(DataSet2, Number_Year == i) %>% group_by(Number_Year) %>% transmute(Number_Company, Capitalization, BM_ratio = 1/MB_ratio, PayoutRatio))
  assign(paste0("criteria_BMratio", i), ifelse(get(paste("FamaFrench_upd", i, sep=""))$BM_ratio>quantile(get(paste("FamaFrench_upd", i, sep=""))$BM_ratio, 0.7), "High", 
         ifelse(get(paste("FamaFrench_upd", i, sep=""))$BM_ratio>quantile(get(paste("FamaFrench_upd", i, sep=""))$BM_ratio, 0.3), "Neutral", "Low")))
  assign(paste0("criteria_capitalization", i), ifelse(get(paste("FamaFrench_upd", i, sep=""))$Capitalization>median(get(paste("FamaFrench_upd", i, sep=""))$Capitalization), "Big", "Small"))
  assign(paste0("criteria_Dividend", i), ifelse(get(paste("FamaFrench_upd", i, sep=""))$PayoutRatio>quantile(get(paste("FamaFrench_upd", i, sep=""))$PayoutRatio, 0.7), "High Dividend", 
                                                ifelse(get(paste("FamaFrench_upd", i, sep=""))$PayoutRatio>quantile(get(paste("FamaFrench_upd", i, sep=""))$PayoutRatio, 0.3), "Neutral Dividend", "Low Dividend")))
  assign(paste0("svod_FF", i), cbind(get(paste("FamaFrench_upd", i, sep="")), 
                                     criteria_BMratio = get(paste("criteria_BMratio", i, sep="")), 
                                     criteria_capitalization = get(paste("criteria_capitalization", i, sep="")), 
                                     criteria_Dividend = get(paste("criteria_Dividend", i, sep=""))))
  assign(paste0("names",i), paste0(get(paste("svod_FF", i, sep=""))$criteria_capitalization, " " , get(paste("svod_FF", i, sep=""))$criteria_BMratio))
  assign(paste0("svod_FF_total", i), cbind(get(paste("svod_FF", i, sep="")), 
                                     names = get(paste("names", i, sep=""))))
  assign(paste0("svod_FF_tot", i), merge(get(paste("svod_FF_total", i, sep="")), names_data, by = "Number_Company"))
  
}

View(svod_FF_tot10)

return_FF <- return
names(names_data)
View(svod_FF_total10)


#%>% pivot_longer(cols = "AAPL UW Equity":"WMT UN Equity", names_to = "ID_Company", values_to = "return") %>% merge(names_data, by = "ID_Company")
return_FF_10 <- as.data.frame(window(return_FF, start = "2010-01-01", end = "2010-12-31"))
return_FF_11 <- as.data.frame(window(return_FF, start = "2011-01-01", end = "2011-12-31")) 
return_FF_12 <- as.data.frame(window(return_FF, start = "2012-01-01", end = "2012-12-31")) 
return_FF_13 <- as.data.frame(window(return_FF, start = "2013-01-01", end = "2013-12-31")) 
return_FF_14 <- as.data.frame(window(return_FF, start = "2014-01-01", end = "2014-12-31")) 
return_FF_15 <- as.data.frame(window(return_FF, start = "2015-01-01", end = "2015-12-31")) 
return_FF_16 <- as.data.frame(window(return_FF, start = "2016-01-01", end = "2016-12-31")) 
return_FF_17 <- as.data.frame(window(return_FF, start = "2017-01-01", end = "2017-12-31"))
return_FF_18 <- as.data.frame(window(return_FF, start = "2018-01-01", end = "2018-12-31"))
return_FF_19 <- as.data.frame(window(return_FF, start = "2019-01-01", end = "2019-12-31"))
return_FF_20 <- as.data.frame(window(return_FF, start = "2020-01-01", end = "2020-12-31")) 
return_FF_21 <- as.data.frame(window(return_FF, start = "2021-01-01", end = "2021-12-31")) 


for (i in 10:21) {
  assign(paste0("SmallHigh_SMB", i), dplyr::filter(get(paste("svod_FF_tot", i, sep="")), get(paste("svod_FF_tot", i, sep=""))$names == "Small High"))
  assign(paste0("SmallNeutral_SMB", i), dplyr::filter(get(paste("svod_FF_tot", i, sep="")), get(paste("svod_FF_tot", i, sep=""))$names == "Small Neutral"))
  assign(paste0("SmallLow_SMB", i), dplyr::filter(get(paste("svod_FF_tot", i, sep="")), get(paste("svod_FF_tot", i, sep=""))$names == "Small Low"))
  assign(paste0("BigHigh_SMB", i), dplyr::filter(get(paste("svod_FF_tot", i, sep="")), get(paste("svod_FF_tot", i, sep=""))$names == "Big High"))
  assign(paste0("BigNeutral_SMB", i), dplyr::filter(get(paste("svod_FF_tot", i, sep="")), get(paste("svod_FF_tot", i, sep=""))$names == "Big Neutral"))
  assign(paste0("BigLow_SMB", i), dplyr::filter(get(paste("svod_FF_tot", i, sep="")), get(paste("svod_FF_tot", i, sep=""))$names == "Big Low"))
  assign(paste0("HighDividend_DVD", i), dplyr::filter(get(paste("svod_FF_tot", i, sep="")), get(paste("svod_FF_tot", i, sep=""))$criteria_Dividend == "High Dividend"))
  assign(paste0("LowDividend_DVD", i), dplyr::filter(get(paste("svod_FF_tot", i, sep="")), get(paste("svod_FF_tot", i, sep=""))$criteria_Dividend == "Low Dividend"))
  assign(paste0("NeutralDividend_DVD", i), dplyr::filter(get(paste("svod_FF_tot", i, sep="")), get(paste("svod_FF_tot", i, sep=""))$criteria_Dividend == "Neutral Dividend"))
}

SmallNeutral_SMB10
LowDividend_DVD21
NeutralDividend_DVD21
View(return_FF_10)

for (i in 10:21) {
  assign(paste0("Small_High_Portfolio", i), get(paste("return_FF_", i, sep=""))[, colnames(get(paste("return_FF_", i, sep=""))) %in% get(paste("SmallHigh_SMB", i, sep=""))$ID_Company])
  assign(paste0("Small_Neutral_Portfolio", i), get(paste("return_FF_", i, sep=""))[, colnames(get(paste("return_FF_", i, sep=""))) %in% get(paste("SmallNeutral_SMB", i, sep=""))$ID_Company])
  assign(paste0("Small_Low_Portfolio", i), get(paste("return_FF_", i, sep=""))[, colnames(get(paste("return_FF_", i, sep=""))) %in% get(paste("SmallLow_SMB", i, sep=""))$ID_Company])
  assign(paste0("Big_High_Portfolio", i), get(paste("return_FF_", i, sep=""))[, colnames(get(paste("return_FF_", i, sep=""))) %in% get(paste("BigHigh_SMB", i, sep=""))$ID_Company])
  assign(paste0("Big_Neutral_Portfolio", i), get(paste("return_FF_", i, sep=""))[, colnames(get(paste("return_FF_", i, sep=""))) %in% get(paste("BigNeutral_SMB", i, sep=""))$ID_Company])
  assign(paste0("Big_Low_Portfolio", i), get(paste("return_FF_", i, sep=""))[, colnames(get(paste("return_FF_", i, sep=""))) %in% get(paste("BigLow_SMB", i, sep=""))$ID_Company])
  assign(paste0("HighDividend_Portfolio", i), get(paste("return_FF_", i, sep=""))[, colnames(get(paste("return_FF_", i, sep=""))) %in% get(paste("HighDividend_DVD", i, sep=""))$ID_Company])
  assign(paste0("LowDividend_Portfolio", i), get(paste("return_FF_", i, sep=""))[, colnames(get(paste("return_FF_", i, sep=""))) %in% get(paste("LowDividend_DVD", i, sep=""))$ID_Company])
}


#построение портфелей
for (i in 10:21) {
  assign(paste0("return_SmallHigh", i), Return.portfolio(R = get(paste("Small_High_Portfolio", i, sep="")), weights = c(rep(1/dim(get(paste("Small_High_Portfolio", i, sep="")))[2],dim(get(paste("Small_High_Portfolio", i, sep="")))[2]))))
  assign(paste0("return_SmallNeutral", i), Return.portfolio(R = get(paste("Small_Neutral_Portfolio", i, sep="")), weights = c(rep(1/dim(get(paste("Small_Neutral_Portfolio", i, sep="")))[2],dim(get(paste("Small_Neutral_Portfolio", i, sep="")))[2]))))
  assign(paste0("return_SmallLow", i), Return.portfolio(R = get(paste("Small_Low_Portfolio", i, sep="")), weights = c(rep(1/dim(get(paste("Small_Low_Portfolio", i, sep="")))[2],dim(get(paste("Small_Low_Portfolio", i, sep="")))[2]))))
  assign(paste0("return_BigHigh", i), Return.portfolio(R = get(paste("Big_High_Portfolio", i, sep="")), weights = c(rep(1/dim(get(paste("Big_High_Portfolio", i, sep="")))[2],dim(get(paste("Big_High_Portfolio", i, sep="")))[2]))))
  assign(paste0("return_BigNeutral", i), Return.portfolio(R = get(paste("Big_Neutral_Portfolio", i, sep="")), weights = c(rep(1/dim(get(paste("Big_Neutral_Portfolio", i, sep="")))[2],dim(get(paste("Big_Neutral_Portfolio", i, sep="")))[2]))))
  assign(paste0("return_BigLow", i), Return.portfolio(R = get(paste("Big_Low_Portfolio", i, sep="")), weights = c(rep(1/dim(get(paste("Big_Low_Portfolio", i, sep="")))[2],dim(get(paste("Big_Low_Portfolio", i, sep="")))[2]))))
  assign(paste0("return_HighDividend", i), Return.portfolio(R = get(paste("HighDividend_Portfolio", i, sep="")), weights = c(rep(1/dim(get(paste("HighDividend_Portfolio", i, sep="")))[2],dim(get(paste("HighDividend_Portfolio", i, sep="")))[2]))))
  assign(paste0("return_LowDividend", i), Return.portfolio(R = get(paste("LowDividend_Portfolio", i, sep="")), weights = c(rep(1/dim(get(paste("LowDividend_Portfolio", i, sep="")))[2],dim(get(paste("LowDividend_Portfolio", i, sep="")))[2]))))
}

#характеристики на примере 2021 года
Return.annualized(return_SmallHigh21, geometric=FALSE) 
sd.annualized(return_SmallHigh21, geometric=FALSE) 

Return.annualized(return_BigHigh21, geometric=FALSE) 
sd.annualized(return_BigHigh21, geometric=FALSE) 

Return.annualized(return_SmallNeutral21, geometric=FALSE) 
sd.annualized(return_SmallNeutral21, geometric=FALSE) 

Return.annualized(return_BigNeutral21, geometric=FALSE) 
sd.annualized(return_BigNeutral21, geometric=FALSE) 

Return.annualized(return_SmallLow21, geometric=FALSE) 
sd.annualized(return_SmallLow21, geometric=FALSE) 

Return.annualized(return_BigLow21, geometric=FALSE) 
sd.annualized(return_BigLow21, geometric=FALSE) 

#Small Low – Big High, Small High – Big High, Small Low – Big Low
t.test(return_SmallLow21, return_BigHigh21)
t.test(return_SmallHigh21, return_BigHigh21)
t.test(return_SmallLow21, return_BigLow21)

var.test(return_SmallLow21, return_BigHigh21)
var.test(return_SmallHigh21, return_BigHigh21)
var.test(return_SmallLow21, return_BigLow21)

#сравнение High Dividend - Low Dividend
t.test(return_HighDividend21, return_LowDividend21)
var.test(return_HighDividend21, return_LowDividend21)

GH_test_21 <- data.frame(BigHigh = return_BigHigh21, BigLow = return_BigLow21, SmallHigh = return_SmallHigh21, SmallLow = return_SmallLow21)
colnames(GH_test_21) <- c("BigHigh", "BigLow", "SmallHigh", "SmallLow")
GH_test_21_pl <- GH_test_21 %>% pivot_longer(cols = BigHigh:SmallLow, names_to = "Type", values_to ="return")
ggbetweenstats(
  data = GH_test_21_pl, 
  x     = Type,
  y     = return,
  title = "Результаты теста Геймса-Ховелла", 
  caption = "Тест используется в случае наличия 3 и больше независимых выборок", 
  xlab = "Портфели", 
  ylab = "Доходность", 
  p.adjust.method = "none",
  bf.message = FALSE, 
  pairwise.display = "all") + scale_color_manual(values = c("#3b6ba5", "#72a5d3", "#b1d3e3", "#e1ebec", "#aacfe2"))


#расчет SMB, HML, DVD
for (i in 10:21) {
  assign(paste0("SMB", i), 1/3*(get(paste("return_SmallHigh", i, sep = ""))+get(paste("return_SmallNeutral", i, sep = ""))+get(paste("return_SmallLow", i, sep = ""))-
                                  get(paste("return_BigHigh", i, sep = ""))-get(paste("return_BigNeutral", i, sep = ""))-get(paste("return_BigLow", i, sep = ""))))
  assign(paste0("HML", i), 1/2*(get(paste("return_SmallHigh", i, sep = ""))+get(paste("return_BigHigh", i, sep = ""))-
                                  get(paste("return_SmallLow", i, sep = ""))-get(paste("return_BigLow", i, sep = ""))))
  assign(paste0("DVD", i), get(paste("return_HighDividend", i, sep = ""))-get(paste("return_LowDividend", i, sep = "")))
  
}

SMB_new <- rbind(SMB10, SMB11, SMB12, SMB13, SMB14, SMB15, SMB16, SMB17, SMB18, SMB19, SMB20, SMB21)
colnames(SMB_new) <- "SMB"
HML_new <- rbind(HML10, HML11, HML12, HML13, HML14, HML15, HML16, HML17, HML18, HML19, HML20, HML21)
colnames(HML_new) <- "HML"
DVD_new <- rbind(DVD10, DVD11, DVD12, DVD13, DVD14, DVD15, DVD16, DVD17, DVD18, DVD19, DVD20, DVD21)
colnames(DVD_new) <- "DVD"


dim(testdrive)
for (i in 1:25) {
  assign(paste0("mod_FF", i), lm(testdrive[,i+3]-I(rf/365) ~ I(return_market[-1,]-rf/365) + SMB_new + HML_new + DVD_new))
}
round(mod_FF3$coefficients,3)
summary(mod_FF1) #дивиденд значим
summary(mod_FF2) #дивиденд не значим
summary(mod_FF3) #дивиденд значим
summary(mod_FF4) #дивиденд значим
summary(mod_FF5) #дивиденд значим
summary(mod_FF6) #дивиденд значим
summary(mod_FF7) #дивиденд значим
summary(mod_FF8) #дивиденд значим
summary(mod_FF9) #дивиденд значим
summary(mod_FF10) #дивиденд значим
summary(mod_FF11) #дивиденд значим
summary(mod_FF12) #дивиденд значим
summary(mod_FF13) #дивиденд значим
summary(mod_FF14) #дивиденд значим
summary(mod_FF15) #дивиденд значим
summary(mod_FF16) #дивиденд значим
summary(mod_FF17) #дивиденд значим
summary(mod_FF18) #дивиденд значим
summary(mod_FF19) #дивиденд значим
summary(mod_FF20) #дивиденд значим
summary(mod_FF21) #дивиденд значим
summary(mod_FF22) #дивиденд значим
summary(mod_FF23) #дивиденд значим
summary(mod_FF24) #дивиденд значим
summary(mod_FF25) #дивиденд значим

stargazer(mod_FF1, mod_FF2, mod_FF3, mod_FF4, mod_FF5, type = "html", out = "FF_new.html")

#построим оценки для всех и выгрузим
#первые 25 - это акции
K <- matrix(0, nrow = 25, ncol = 5)
t_value <- matrix(0, nrow = 25, ncol = 5)
Stars <- matrix(0, nrow = 25, ncol = 5)
q <- qt(0.975, 3015) #степени свободы - n-k: 3019-4=3015
R2 <- NULL


#запустим цикл
for(i in 1:25) {
  model_FF_cycle <- lm(testdrive[,i+3] -I(rf/365) ~  I(return_market[-1,]-rf/365) +
               SMB_new + HML_new + DVD_new)
  S <- summary(model_FF_cycle)
  K[i,] <- S$coefficients[,1]
  t_value[i,] <- S$coefficients[,3]
  Stars[i,] <- abs(S$coefficients[,3]) > q
  R2[i] <- S$r.squared
}

#сколько раз дивидендный фактор значим?
sum(Stars[,5]) #23 / 25 компаний
#значит, DVD фактор хорошо прогнозирует доходность
#сколько раз значима альфа?
sum(Stars[,1]) #3 / 25 компаний

#проделаем все то же самое без R2 и посмотрим на сколько он 
#меняется при добавлении ESG
K2 <- matrix(0, nrow = 25, ncol = 4)
t_value2 <- matrix(0, nrow = 25, ncol = 4)
Stars2 <- matrix(0, nrow = 25, ncol = 4)
q <- qt(0.975, 3015)
R2_2 <- NULL

#запустим цикл
for(i in 1:25) {
  model_FF_cycle1 <- lm(testdrive[,i+3] -I(rf/365) ~  I(return_market[-1,]-rf/365) +
                         SMB_new + HML_new)
  S <- summary(model_FF_cycle1)
  K2[i,] <- S$coefficients[,1]
  t_value2[i,] <- S$coefficients[,3]
  Stars2[i,] <- abs(S$coefficients[,3]) > q
  R2_2[i] <- S$r.squared
}

#изменен R2
R2_delta <- R2 - R2_2

#среднее изменение R2
mean(R2_delta) #3 процента

#изменение R2 в моделях, где DVD значим
R2_delta_star <- R2_delta*Stars[,5]
R2_delta_star <- R2_delta_star[R2_delta_star != 0]
mean(R2_delta_star) #3.3%

str(R2_delta_star)
R2_for_graph <- as.data.frame(R2_delta_star)
ggplot(data = R2_for_graph1, aes(x = reorder(ID, R2_delta_star), y = R2_delta_star)) + geom_point(color = "darkblue") + theme_bw() + xlab("Тикер компании") + ylab("Изменение коэффициента детерминации в случае 
добавления дивиденого параметра")
View(R2_for_graph1)

table(DataSet2$Number_Company)
R2_for_graph$Number_Company <- c(1,4,5,6,7,8,9,13,14,15,16,18,19,20,21,22,23,24,26,27,28,29,30)
names_data
R2_for_graph1 <- merge(R2_for_graph, names_data, by = "Number_Company")

ggdotplotstats(
  data = R2_for_graph1,
  x = R2_delta_star,
  y = ID,
  title = "Изменение коэффициента детерминации в случае добавления DVD в модель Фама-Френч",
  caption = "В случае если DVD был статистически значимым", 
  xlab = "Изменение коэффициента детерминации",
  point.args = list(
    shape = 16,
    size = 5, 
    color = "darkblue"
  )
) + theme_bw()

names_data

#выгрузим таблицу со значимыми коэффициентами (буду ставить нули там,
#где нет значимости)
Data_result <- data.frame(K*Stars, R2_delta = R2_delta*Stars[,5])
#первый столбец не выгружаю, там нули
Data_result <- Data_result[, -1]
colnames(Data_result) <- c("ERP", "SMB", "HML", "DVD", "delta in Rsquared")
rownames(Data_result) <-ID


#выгрузка
stargazer(Data_result, type = "html", 
          summary = FALSE, out = "Data_result.html")
