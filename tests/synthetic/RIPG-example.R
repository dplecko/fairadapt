u <- seq(0.001, 0.999, length.out = 1000)
logit <- function(x) {
  return(log(x/(1-x)))
}
expit <- function(x) {
  return(exp(x)/(1+exp(x)))
}
mc_expectation <- function(mu, dev, n = 10000L) {
  smp1 <- expit(rnorm(n, mean = mu, sd = dev))
  smp2 <- expit(rnorm(n, mean = -mu, sd = dev))
  return(mean(smp1) - mean(smp2))
}


mu <- 1/3
dev <- 0.1

mc_expectation(mu, dev)

res <- rbind(
  cbind(u, dnorm(logit(u), mean = -mu, sd = dev)/(u*(1-u)), 1),
  cbind(u, dnorm(logit(u), mean = mu, sd = dev)/(u*(1-u)), 2)
)
res <- data.frame(res)
names(res) <- c("prob", "density", "gender")
res$gender <- factor(res$gender)
levels(res$gender) <- c("Female", "Female")
ggplot(res, aes(x = prob, y = density, color = gender)) +
  geom_line(size = 2) +
  xlab("probability of positive outcome") + ylab("density") + ggtitle("Density of the probability of positive outcome", subtitle = waiver()) +
  theme_bw(25) + theme(legend.position = c(0.8, 0.65),
    legend.box.background = element_rect(colour = "black"))

