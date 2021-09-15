x <- c(9.35, 16.72, 21.87, 27.74, 32.95, 38.97, 
       45.87, 52.32, 59.32, 65.57, 75.75)
y <- c(7.67, 11.83, 13.76, 15.48, 16.99, 17.74,
       18.41, 19.18, 19.88, 20.5, 21.34)
data <- data.frame(x,y)



#Принять предполагаемые значения и вписать в модель.
model <- nls(y ~ b1 * sqrt(x) + b0, start = list(b0 = 3 ,b1 = 2))
summary(model)
coef_model <- coef(model)
b1 <- coef_model[1]
b0 <- coef_model[2]
x <- seq(1,100)
plot(data,type="p", col="blue", xlab="переменная х", ylab="переменная у",
     asp=1, main="график нелинейной регрессии")
lines(2.283 * sqrt(x) + 2.590)
main = "Легенда"
location = "bottomright"
labels = c("Данные выборки", "Кривая регрессии")
colors = c("blue", "black")
legend(location, labels, title = main, fill=colors)

#диаграмма с новыми данными, подгоняя ее к прогнозу из 100 точек данных.
#new.data <- data.frame(x = seq(min(x),max(x),len = 100))
#(new.data$x,predict(model,newdata = new.data))
#plot(new.data)





#сумма квадратных остатков
print(sum(resid(model)^2))

#доверительные интервалы по выбранным значениям коэффициентов
print(confint(model))
