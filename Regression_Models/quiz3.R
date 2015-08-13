# Question 1-4
data(mtcars)
fit1 <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(fit1)

fit2 <- lm(mpg ~ factor(cyl), data = mtcars)
summary(fit2)

fit3 <- lm(mpg ~ factor(cyl) + wt + factor(cyl)*wt, data = mtcars)
summary(fit3)

anova(fit1, fit3)

fit4 <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
summary(fit1)
summary(fit4)

# Question 5-6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

fit5 <- lm(y ~ x)

hatvalues(fit5)
dfbetas(fit5)
