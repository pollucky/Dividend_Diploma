install.packages("keras")
library(keras)
keras::install_keras(tensorflow = "cpu")
library(ggplot2)

#Keras1 - прокси-переменные
#Keras2 - жизненный цикл

library(readxl)
Keras1 <- as.data.frame(read_excel("Keras1.xlsx"))
Keras2 <- as.data.frame(read_excel("Keras2.xlsx"))

#работаем с Keras1
names(Keras1)
#Количество входных нейронов равно количеству переменных - 4; 
#количество выходных нейронов равно зависимой переменной - 1 

model <- keras_model_sequential() %>% layer_dense(units = 4, activation = "relu",input_shape = 4) %>%  layer_dense(units = 4, activation = "relu") %>% layer_dense(units = 1, activation = "sigmoid")
model <- model %>% compile(optimizer = "rmsprop", loss = "binary_crossentropy", metrics = c("accuracy"))
data_x_Keras1 <- as.matrix(scale(Keras1[,c(2,3,7,8)]))
data_y_Keras1 <- as.matrix(Keras1$delta_Lintner)
Result <- model %>% fit(data_x_Keras1, data_y_Keras1, batch_size = 50, epochs = 250) 
str(Keras1)

plot(Result) + theme_bw() + ylab("") + xlab("Количество эпох") +geom_point(color = "darkblue", size = 0.1) + geom_smooth(color = "orange")
#вроде бы 250 эпох - норм
install.packages("remotes")
remotes::install_github("andrie/deepviz")
library(deepviz)
library(magrittr)

model %>% plot_model()
library(gamlss.add)
plot.nnet(model)


#нормируем
str(Keras1)
m <- colMeans(Keras1[,c(2,3,7,8)])
SD <- apply(Keras1[,c(2,3,7,8)], MARGIN = 2, sd)

train_data <- scale(Keras1[,c(2,3,7,8)], center = m, scale = SD)
train_targets <- Keras1[,1]
#тестовая выборка
set.seed(123)
n <- sample(1:dim(Keras1)[1], 0.25*259, FALSE) #отбор 25% наблюдений рандом
data_test <- Keras1[n,]
names(data_test)
data2_test <- dplyr::select(data_test, -delta_Lintner, -delta_Lintner_ch, -Stage_Indicator1, -Z_score, -Independent_Directors)
y_test <-  dplyr::select(data_test, delta_Lintner)
test_data <- scale(data2_test, center = m, scale = SD)
test_targets <- y_test

#тогда зададим функцию для построения модели
build_model <- function() {
  model <- keras_model_sequential() %>% layer_dense(units = 4, activation = "relu",
                                                    input_shape = 4) %>%
    layer_dense(units = 4, activation = "relu") %>% layer_dense(units = 1, activation = "sigmoid")
  model %>% compile(optimizer = "rmsprop", loss = "binary_crossentropy", metrics = "accuracy")
}

#Зададим основные параметры
k <- 10
indices <- sample(1:nrow(train_data)) 
folds <- cut(indices, breaks = k, labels = FALSE)
num_epochs <- 200
all_mse_histories <- NULL

#обучение
for(i in 1:k) {
  cat("processing fold #", i, "\n")
  val_indices <- which(folds == i)
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  model <- build_model()
  history <- model %>% fit(partial_train_data, partial_train_targets, epochs = num_epochs,
                           batch_size = 50, verbose = 0, 
                           validation_data = list(val_data, val_targets))
  mse_history <- history$metrics$val_loss
  all_mse_histories <- rbind(all_mse_histories, mse_history)
}

View(all_mse_histories)
#поиск оптимального числа эпох
mse_history <- colMeans(all_mse_histories)
x <- 1:length(mse_history)
qplot(x, mse_history, geom = "line") + geom_smooth(color = "darkblue") + theme_bw() + ylab("Ошибка модели") + xlab("Количество эпох")
D <- which.min(mse_history)
D

#обучение итого
model <- build_model()
history <- model %>% fit(train_data, train_targets, epochs = D,
                         batch_size = 50)
Pr <- predict(model, x = train_data)



#обычная регрессия
Data3 <- data.frame(train_data, train_targets)
names(Data3)
mod <- lm(mrall ~ ., data = Data3)
Pr2 <- predict(mod)
sum((Pr2 - train_targets)^2)
1 - sum((Pr2 - train_targets)^2)/sum((train_targets)^2)

#на другой выборке
Pr_test3 <- predict(mod, newdata =  data.frame(test_data))
Pr_test3 <- data.frame(Pr_test3)
1 - sum((Pr_test3 - test_targets)^2)/sum((test_targets)^2)

system.time ( 
  history <- fit (
    object           = model,             # => Our Model
    x                = as.matrix (train_data), #=> Matrix
    y                = train_targets,             #=> Numeric Vector 
    batch_size       = 50,     #=> #OfSamples/gradient update in each epoch
    epochs           = D,     #=> Control Training cycles
    validation_split = 0.25) ) #=> Include 25% data for 'Validation' Model

#минимальное различие между validation и training accuracy
print (history)
plot(history) + theme_bw() + ylab("") + xlab("Количество эпох") + geom_line(col = "grey", size = 0.05, alpha = 0.1) +scale_color_manual(values = c("#0390e8", "#255a62")) + theme(legend.position = "bottom")

#работа с Keras2
names(Keras2)
#Количество входных нейронов равно количеству переменных - 6; 
#количество выходных нейронов равно зависимой переменной - 1 

model1 <- keras_model_sequential() %>% layer_dense(units = 6, activation = "relu",input_shape = 6) %>%  layer_dense(units = 6, activation = "relu") %>% layer_dense(units = 1, activation = "sigmoid")
model1 <- model1 %>% compile(optimizer = "rmsprop", loss = "binary_crossentropy", metrics = c("accuracy"))
data_x_Keras2 <- as.matrix(scale(Keras2[,c(5,6,8,9,11,16)]))
data_y_Keras2 <- as.matrix(Keras2$delta_Lintner)
Result1 <- model1 %>% fit(data_x_Keras2, data_y_Keras2, batch_size = 50, epochs = 250) 

plot(Result1) + theme_bw() + ylab("") + xlab("Количество эпох") +geom_point(color = "darkblue", size = 0.1) + geom_smooth(color = "orange")
#вроде бы 250 эпох - норм


#нормируем
str(Keras1)
m1 <- colMeans(Keras2[,c(5,6,8,9,11,16)])
SD1 <- apply(Keras2[,c(5,6,8,9,11,16)], MARGIN = 2, sd)

train_data1 <- scale(Keras2[,c(5,6,8,9,11,16)], center = m1, scale = SD1)
names(Keras2)
train_targets1 <- Keras2[,4]
#тестовая выборка
set.seed(123)
n1 <- sample(1:dim(Keras2)[1], 0.25*259, FALSE) #отбор 25% наблюдений рандом
data_test1 <- Keras2[n,]
names(data_test1)
data2_test1 <- dplyr::select(data_test1, -delta_Lintner, -Number_Company, -Number_Year, -Year, -TEtoTA, -AGR, -CashtoTA, -DIV, -PayoutRatio, -PayOrNot, -Capitalization)
y_test1 <-  dplyr::select(data_test1, delta_Lintner)
test_data1 <- scale(data2_test1, center = m1, scale = SD1)
test_targets1 <- y_test1

#тогда зададим функцию для построения модели
build_model1 <- function() {
  model1 <- keras_model_sequential() %>% layer_dense(units = 6, activation = "relu",
                                                    input_shape = 6) %>%
    layer_dense(units = 6, activation = "relu") %>% layer_dense(units = 1, activation = "sigmoid")
  model1 %>% compile(optimizer = "rmsprop", loss = "binary_crossentropy", metrics = "accuracy")
}

#Зададим основные параметры
k <- 10
indices1 <- sample(1:nrow(train_data1)) 
folds1 <- cut(indices1, breaks = k, labels = FALSE)
num_epochs <- 200
all_mse_histories1 <- NULL

#обучение
for(i in 1:k) {
  cat("processing fold #", i, "\n")
  val_indices1 <- which(folds1 == i)
  val_data1 <- train_data1[val_indices,]
  val_targets1 <- train_targets1[val_indices]
  partial_train_data1 <- train_data1[-val_indices,]
  partial_train_targets1 <- train_targets1[-val_indices]
  model1 <- build_model1()
  history1 <- model1 %>% fit(partial_train_data1, partial_train_targets1, epochs = num_epochs,
                           batch_size = 50, verbose = 0, 
                           validation_data = list(val_data1, val_targets1))
  mse_history <- history1$metrics$val_loss
  all_mse_histories <- rbind(all_mse_histories, mse_history)
}

View(all_mse_histories)
#поиск оптимального числа эпох
mse_history <- colMeans(all_mse_histories)
x <- 1:length(mse_history)
qplot(x, mse_history, geom = "line") + geom_smooth(color = "darkblue") + theme_bw() + ylab("Ошибка модели") + xlab("Количество эпох")
D <- which.min(mse_history)
D

system.time ( 
  history <- fit (
    object           = model1,             # => Our Model
    x                = as.matrix (train_data1), #=> Matrix
    y                = train_targets1,             #=> Numeric Vector 
    batch_size       = 50,     #=> #OfSamples/gradient update in each epoch
    epochs           = D,     #=> Control Training cycles
    validation_split = 0.25) ) #=> Include 25% data for 'Validation' Model

#минимальное различие между validation и training accuracy
print (history)
plot(history) + theme_bw() + ylab("") + xlab("Количество эпох") + geom_point(col = "grey", size = 0.05, alpha = 0.1) +scale_color_manual(values = c("#0390e8", "#255a62")) + theme(legend.position = "bottom")




yhat_keras_prob_vec <- predict(object = model, x = as.matrix(train_data)) %>% as.vector()
yhat_keras_class_vec <- ifelse(yhat_keras_prob_vec<0.5, 0, 1)
class(model)
model_type(model)

library(forcats)
library(tibble)
estimates_keras_tbl <- tibble(
  truth  = as.factor(data_y_Keras1) %>% forcats::fct_recode (Positive = "1", Negative = "0"),
  estimate   = as.factor(yhat_keras_class_vec) %>% forcats::fct_recode (Positive = "1", Negative = "0"),
  class_prob = yhat_keras_prob_vec )

head(estimates_keras_tbl, 10)
library(yardstick)
estimates_keras_tbl %>% yardstick::conf_mat(truth, estimate)
estimates_keras_tbl %>% yardstick::metrics (truth, estimate)

library(lime)

# Setup lime::model_type() function for keras
class(model)
model_type.keras.engine.sequential.Sequential <- function(x, ...) {
  "classification"
}

predict_model.keras.models.Sequential <- function(x, newdata, type, ...) {
  pred <- predict(object = x, x = as.matrix(newdata))
  data.frame(Yes = pred, No = 1 - pred)
}
class(model)
predict_model.keras.models.Sequential(x = model, newdata = test_data, type = 'raw') %>% tibble::as_tibble()
predict_model(x = model, newdata = test_data, type = 'raw')

explainer <- lime::lime(
  x              = as.data.frame(data_x_Keras1), 
  model          = model
)
predict_model(model)
explanation <- lime::explain(
  as.data.frame(test_data), 
  explainer    = explainer, 
  n_labels     = 2, 
  n_features   = 4
)

dimnames(test_data)[[1]]

plot_features(explainer)
