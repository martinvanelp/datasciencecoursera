# Question 3
data(mtcars)
fitcars <- lm(mpg ~ wt, data = mtcars)

sumCoef <- summary(fitcars)$coefficients
a <- sumCoef[1,1] + c(-1, 1) * qt(.975, df = fitcars$df) * sumCoef[1, 2]
b <- sumCoef[2,1] + c(-1, 1) * qt(.975, df = fitcars$df) * sumCoef[2, 2]
Mweight <- mean(mtcars$wt)

# FALSE!!! Should be 18.991
a + b * Mweight
sumCoef[1,1] + b * Mweight

# Question 5
X <- mtcars$wt
Y <- mtcars$mpg

data <- as.data.frame(cbind(X,Y))
head(data)

fit <- lm(Y ~ X, data = data)
summary(fit)

B0 <- coef(fit)[1]
B1 <- coef(fit)[1]
sigma <- sqrt(sum((Y - (B0 + B1*X))^2)/(n - 2))
ssx <- sum((X - mean(X))^2)
SEBeta1 <- sigma / sqrt(ssx)


# plot(data, xlim=c(2,8), ylim=c(1,9))
plot(data)
lines(X, coef(fit)[1] + coef(fit)[2]*X, col = "red", type = "l")
lines(X, coef(fit)[1] + coef(fit)[2]*X + 1.96*SEBeta1, X, col = "blue", type="l")
lines(X, coef(fit)[1] + coef(fit)[2]*X - 1.96*SEBeta1, col = "blue")
lines(X, predict(fit, new.data = sort(X), interval = "prediction")[,"upr"],col="pink")
lines(X, predict(fit, new.data = sort(X), interval = "prediction")[,"lwr"],col="pink")
lines(X, predict(fit, new.data = sort(X), interval = "confidence")[,"upr"],col="yellow")
lines(X, predict(fit, new.data = sort(X), interval = "confidence")[,"lwr"],col="yellow")
abline(v=mean(X))
abline(h=mean(Y))