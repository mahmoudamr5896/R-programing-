# project 
data = read.csv(file.choose())
str(data)
plot(data)
model = lm(data = data,data$Id~data$SalePrice)
summary(model)
tab_model(model)

View(data)






















































