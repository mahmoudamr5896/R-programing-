df = read.csv('https://covid.ourworldindata.org/data/owid-covid-data.csv')
View(df)

Egy = df[df$location=='Egypt',]
View(Egy)
world = df[df$location=='World',]
Egy$new_cases
dff = read.csv(file.choose())
View(dff)
plot(dff$Deaths,dff$Population)
##    linear regression
model = lm(data= dff,Deaths~Population)
summary(model)

tab_model(model)

































