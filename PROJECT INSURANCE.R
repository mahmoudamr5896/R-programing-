data = read.csv(file.choose())
str(data)


##exploration data     plot
attach(data)
boxplot(data$bmi~data$sex)

boxplot(data$children~data$sex)

boxplot(data$charges~data$sex)
plot(sex~age)

data2 = data[data$age>=50,]

boxplot(data$bmi~data$smoker,
        ylab='BMI',xlab = 'smoker')
boxplot(data$charges~data$smoker,
        ylab='charges',xlab = 'smoker')

boxplot(data$charges~data$sex,
        ylab='BMI',xlab = 'smoker')


boxplot(data$bmi~data$sex,
        ylab='BMI',xlab = 'Gender')

hist(data$bmi,data$smoker)

barplot(table(data$bmi))
barplot(table(data$charges))
barplot(table(data$age))

plot(data$charges~data$age)
abline(lm(data$charges~data$age),col='red')
plot(data$bmi~data$age)

hist(data$charges)
hist(data$bmi)
hist(data$children)
hist(data$age)

qplot(bmi,charges,size= i(2),col=sex)

ggplot(data=data,aes(x=charges,y=bmi,col=sex)+geom_point(aes(size=age)))


##model
model = lm(data=data,data$age~data$bmi)
summary(model)


model%<%plot(model)

model2 = lm(data = data,data$age~data$bmi+data$charges+data$children)
summary(model2)


# model evluation
tab_model(model,model2)


















































































