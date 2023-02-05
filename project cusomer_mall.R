df = read.csv(file.choose())
View(df)
summary(df)
str(df)
plot(df$Annual.Income..k..~df$Spending.Score..1.100.+df$CustomerID)


model = lm(data=df,df$Annual.Income..k..~df$Spending.Score..1.100.)
summary(model)
tab_model(model)




