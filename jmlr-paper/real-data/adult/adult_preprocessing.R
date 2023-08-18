
root <- rprojroot::find_root(rprojroot::has_file("fairadapt.Rproj"))
require(readr)

adult1 <- read_csv(file.path(root, "jmlr-paper", "real-data", "adult",
                             "adult-data", "adult.data"),
                   col_names = FALSE)

adult2 <- read_csv(file.path(root, "jmlr-paper", "real-data", "adult",
                             "adult-data", "adult.test"),
                   col_names = FALSE)

adult <- rbind(adult1, adult2)
adult <- data.frame(adult)

colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'educatoin',
  'educatoin_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex',
  'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')
adult[, c("educatoin", "relationship", "fnlwgt",
          "capital_gain", "capital_loss")] <- NULL
factor.columns <- c("workclass", "marital_status", "occupation", "race", "sex",
                    "native_country", "income")

for(i in factor.columns) {

  adult[, i] <- as.factor(adult[, i])

}

# recode the factors appropriately
require(rockchalk)

# variable workclass
adult$workclass <- combineLevels(adult$workclass,
                                 levs = c("?", "Never-worked", "Without-pay"),
                                 newLabel = "Other/Unknown")

adult$workclass <- combineLevels(adult$workclass,
                                 levs = c("Self-emp-inc", "Self-emp-not-inc"),
                                 newLabel = "Self-Employed")

adult$workclass <- combineLevels(adult$workclass,
                                 levs = c("Federal-gov", "Local-gov", "State-gov"),
                                 newLabel = "Government")

# variable marital_status
adult$marital_status <- combineLevels(adult$marital_status,
                                      levs = c("Married-AF-spouse","Married-civ-spouse", "Married-spouse-absent"),
                                      newLabel = "Married")

adult$marital_status <- combineLevels(adult$marital_status,
                                      levs = c("Divorced", "Never-married", "Separated", "Widowed"),
                                      newLabel = "Not-Married")

# variable native_country
non.US <- setdiff(levels(adult$native_country), "United-States")
adult$native_country <- combineLevels(adult$native_country,
                                      levs = non.US,
                                      newLabel = "Not-United-States")

# variable race
adult$race <- combineLevels(adult$race,
                            levs = c("Amer-Indian-Eskimo", "Asian-Pac-Islander", "Other"),
                            newLabel = "Other")

# variable income
levels(adult$income) <- c("<=50K", "<=50K", ">50K", ">50K")

df <- expand.grid(levels(adult$race), levels(adult$sex))
df <- cbind(df, sapply(1:nrow(df), function(i) {

  100*sum(adult[, "race"] == df[i, 1] &
      adult[, "sex"] == df[i, 2]) / sum(adult[, "sex"] == df[i, 2])

}))

names(df) <- c("race", "sex", "prop")

require(ggplot2)

# race * sex distribution
p1 <- ggplot(data = df, aes(x = race, y = prop, fill = sex)) +
  geom_bar(stat="identity", position = "dodge", width=0.5) +
  theme_bw() + ggtitle("Race/Sex distribution") +
  xlab("race") + ylab("percentage") +
  theme(legend.position = "bottom")

# sex * age distribution
age.m <- data.frame(probmass = tabulate(adult$age[adult$sex == "Male"])[17:90] / sum(adult$sex == "Male"),
                    age = 17:90, sex = factor(rep("Male", 90-16)))

age.f <- data.frame(probmass = tabulate(adult$age[adult$sex == "Female"])[17:90] / sum(adult$sex == "Female"),
                    age = 17:90, sex = factor(rep("Female", 90-16)))

age <- rbind(age.m, age.f)

p2 <- ggplot(data = age, aes(x = age, y = probmass, color = sex)) +
  geom_line(size = 1) + geom_point(size = 2) +
  xlab("age [years]") +
  ylab("probability mass") +
  theme_bw() + ggtitle("Age distribution per sex")
  theme(legend.position = "bottom")

require(cowplot)

plot_grid(plot_grid(p1 + theme(legend.position = "none"),
    p2 + theme(legend.position = "none"),
    ncol = 2L, labels = c("A", "B")),
    get_legend(p1), nrow = 2L, rel_heights = c(1, 0.1))

ggsave(paste0(file.path(root, "..", "Article", "UCIplot"), ".png"),
       device = "png", width = 8, height = 4)

# save external data example
adult_save <- adult[sample(nrow(adult), 2000), ]
names(adult_save)[names(adult_save) == "educatoin_num"] <- "education_num"
saveRDS(adult_save, file = file.path(root, "inst", "extdata", "uci_adult.rds"))

# subsampling to remove the bias
adult <- adult[adult$race == "White", ]

set.seed(12345)
subset <- NULL
for(i in sort(unique(adult$age))) {

  fem <- which(adult$age == i & adult$sex == "Female")
  m <- length(fem)
  if(length(which(adult$age == i & adult$sex == "Male")) < m) next
  male <- sample(x = which(adult$age == i & adult$sex == "Male"), size = m)
  subset <- union(subset, male)
  subset <- union(subset, fem)
}

adult <- adult[subset, ]
write.csv(adult, file = file.path(root, "tests", "adult", "UCIAdult.csv"))
