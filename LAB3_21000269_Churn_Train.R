colSums(is.na(churn_Train))

#Data preparation task

ggplot(churn_Train, aes(Monthly.Charges)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  ggtitle("Variable distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

value_imputed <- data.frame(
  original = churn_Train$Monthly.Charges,
  imputed_zero = replace(churn_Train$Monthly.Charges,is.na(churn_Train$Monthly.Charges), 0),
  imputed_mean = replace(churn_Train$Monthly.Charges,is.na(churn_Train$Monthly.Charges), mean(churn_Train$Monthly.Charges, na.rm = TRUE)),
  imputed_median = replace(churn_Train$Monthly.Charges,is.na(churn_Train$Monthly.Charges), median(churn_Train$Monthly.Charges, na.rm = TRUE)))
value_imputed

#check histogram
h1 <- ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position =
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position =
                   "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()
h3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position =
                   "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()
h4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position =
                   "identity") +
  ggtitle("Median-imputed distribution") +
  theme_classic()
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

library(mice)
churn_numeric <- churn_Train %>% select(Senior.Citizen, Tenure, Monthly.Charges, Total.Charges)
md.pattern(churn_numeric)

mice_imputed <- data.frame(
  original = churn_Train$Monthly.Charges,
  imputed_pmm = complete(mice(churn_numeric, method = "pmm"))$Monthly.Charges,
  imputed_cart = complete(mice(churn_numeric, method = "cart"))$Monthly.Charges,
  imputed_lasso = complete(mice(churn_numeric, method = "lasso.norm"))$Monthly.Charges)
mice_imputed

#MICE histogram
h1 <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Pmm-imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("Cart-imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso-imputed distribution") +
  theme_classic()
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

#MissForest - ACRTIVITY 3
library(missForest)

missForest_imputed <- data.frame(
  original = churn_numeric$Monthly.Charges,
  imputed_missForest = missForest(churn_numeric)$ximp$Monthly.Charges
)
missForest_imputed

#missForest Histogram
h1 <- ggplot(missForest_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(missForest_imputed, aes(x = imputed_missForest)) +
  geom_histogram(fill = "#ad7345", color = "#000000", position = "identity") +
  ggtitle("missForest-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, nrow = 2, ncol = 1)

#ACTIVITY 4 - NORMALIZATION OF DATA (log) - scaling method
log_scale = log(as.data.frame(churn_Train$Monthly.Charges))

library(caret)
process <- preProcess(as.data.frame(churn_Train$Monthly.Charges),
                      method=c("range"))
norm_scale <- predict(process, as.data.frame(churn_Train$Monthly.Charges))

scale_data <- as.data.frame(scale(churn_Train$Monthly.Charges))

#ACTIVITY 5 - FEATURE ENCODING

#Label-Encoding
gender_encode <- ifelse(churn_Train$Gender == "male",1,0)
table(gender_encode)

#One hot-encoding
new_dat =
  data.frame(churn_Train$Monthly.Charges,churn_Train$Gender,churn_Train$Total.Charges)
summary(new_dat)

#Call caret and transform
library(caret)
dmy <- dummyVars(" ~ .", data = new_dat, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = new_dat))
glimpse(dat_transformed)

#Encoding Continuous (or Numeric) Variables
summary(new_dat$churn_Train.Monthly.Charges)

bins <- c(-Inf, 35.40, 89.85, Inf)
bin_names <- c("Low", "Mid50", "High")

new_dat$new_Monthly.Charges <- cut(new_dat$churn_Train.Monthly.Charges, breaks =
                          bins, labels = bin_names)

summary(new_dat$churn_Train.Monthly.Charges)
summary(new_dat$new_Monthly.Charges)

