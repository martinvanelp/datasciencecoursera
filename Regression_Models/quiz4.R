# Question 1-3
library(MASS)
data(shuttle)

fit1 <-  glm(use ~ wind, family = "binomial", data = shuttle)
summary(fit1)

exp(fit1$coefficients)

fit2 <-  glm(use ~ wind + magn, family = "binomial", data = shuttle)
summary(fit2)

exp(fit2$coefficients)

fit31 <- glm((use=='auto') ~ wind, data = shuttle)
fit32 <- glm(I(1 - (shuttle$use=='auto')) ~ wind, data = shuttle)

fit31
fit32

# Question 4
data("InsectSprays")
fit4 <- glm(count ~ spray, family = "poisson", data = InsectSprays)

summary(fit4)

1 / exp(fit4$coef[2])
    
# Question 5
count <- rep(c(1, 2, 3, 4, 5), 2)
x <- rep(c(0, 1), 5) 
t <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

fit51 <- glm(count ~ x + offset(t), family = poisson)

t2 <- log(10) + t
fit52<- glm(count ~ x + offset(t2), family = poisson)
  
all.equal(fit51$coef[1] - log(10), fit52$coef[1])

all.equal(fit51$coef[2], fit52$coef[2])

# Question 6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

knots <- 0; 
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot)) 
xMat <- cbind(1, x, splineTerms) 
yhat <- predict(lm(y ~ xMat - 1)) 
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2) 
lines(x, yhat, col = "red", lwd = 2)

fit6 <- lm(y ~ xMat - 1)

fit6$coef[3] + fit6$coef[2]