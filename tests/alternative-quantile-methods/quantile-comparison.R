n <- 5000

X <- replicate(5, rnorm(n))
beta <- c(1, -1, 0, 0, 0)
eps <- rnorm(n)
Y <- X %*% beta + eps
data <- data.frame(Y, X)

qrf <- ranger::ranger(Y ~ ., data = data, quantreg = TRUE, keep.inbag = TRUE)
forest.estimate <-

library(qrnn)
nnqr <- qrnn.fit(X, Y, tau = seq(0.01, 0.99, by = 0.01))
net.estimate <- sapply(1:nrow(data), function(x) ecdf(qrnn.predict(X, nnqr))(data$Y[x]))

library(quantreg)
qr <- rq(Y ~ ., data = data, tau = seq(0.01, 0.99, by = 0.01))
qr.estimate <- sapply(1:nrow(data), function(x) ecdf(predict(qr, newdata = data.frame(X)))(data$Y[x]))

qrs <- rqss(Y ~ ., data = data, tau = seq(0.01, 0.99, by = 0.01))
qrs.estimate <- sapply(1:nrow(data), function(x) ecdf(predict(qrs, newdata = data.frame(X)))(data$Y[x]))

par(mfrow = c(2, 2))

plot(pnorm(eps), forest.estimate, xlab = "True quantile", ylab = "Estimated quantile", main = "Method: Forests")
plot(pnorm(eps), net.estimate, xlab = "True quantile", ylab = "Estimated quantile", main = "Method: Neural Networks")
plot(pnorm(eps), qr.estimate, xlab = "True quantile", ylab = "Estimated quantile", main = "Method: Linear")
plot(pnorm(eps), qrs.estimate, xlab = "True quantile", ylab = "Estimated quantile", main = "Method: Splines")
