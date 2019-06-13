expit <- function(x) return(exp(x)/(1+exp(x)))
n <- 20000
A <- rbinom(n, size = 1, prob = 0.5)
X1 <- rnorm(n) + (2*A-1)/2
X2 <- rnorm(n, sd = 1) + 2*(2*A-1)

Y <- rbinom(n, size = 1, prob = expit(X1+X2))
data <- data.frame(cbind(X1, X2, Y))
glm( Y~., data = data, family = "binomial")$coefficients

# now assume that X1 is resolving
X2_tilde <- X2 + 2*2*(A == 0)
Y_tilde <- Y
Y_tilde[A == 0] <- rbinom(sum(A == 0), size = 1, prob = expit(X1[A == 0] + X2_tilde[A == 0]))

data_tilde <- data.frame(cbind(X1, X2_tilde, Y))
glm(Y ~ ., data = data_tilde, family = "binomial")$coefficients

data_tilde2 <- data.frame(cbind(X1, X2_tilde, Y_tilde))
glm(Y_tilde ~ ., data = data_tilde2, family = "binomial")$coefficients
