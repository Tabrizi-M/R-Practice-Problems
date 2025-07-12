### Solution to exercise 9.2 of the textbook

require(dobson)
require(ggplot2)

### preparation of data
insurance <- dobson::insurance
insurance$rates <- insurance$y/insurance$n

### plots of data
boxplot(insurance$rates ~ insurance$age)
boxplot(insurance$rates ~ insurance$district)
boxplot(insurance$rates ~ insurance$car)

### models
glm_car.age <- glm(y ~ car + age,
offset = log(n +1), family = "poisson", data=insurance)
#part c model - AGE and CAR treated as continuous variables.
glm_car.age.district <- glm(y ~ car + age + factor(district),
offset = log(n +1), family = "poisson", data=insurance)
glm_car.age.district.factor <-glm(y ~ factor(car) + factor(age) + factor(district),
offset = log(n +1), family = "poisson", data=insurance)
######################################
### adding interactions
add1(glm_car.age.district.factor, ~ .^2, test = "Chisq")
### stepwise comparison
search = step(glm_car.age.district.factor, ~.^2)
###################################### plots

### plot of results
plot( insurance$y, col="black", pch=21,
main="Comparison of GLM models", ylab = "fitted values")
# points(glm_car.age$fitted, col="red", pch=19)
points(glm_car.age.district.factor$fitted , col="blue", pch=19)
points(glm_car.age.district$fitted, col="red", pch=19) #part c model
