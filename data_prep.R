test <- read.delim('test.txt')
test_2 <- read.delim('test_2.txt')
training <- read.delim('training.txt')
training <- training[,c(5,10:184)]
colnames(training)[1] <- 'class'
test <- test[1:35, c(3:178)]
colnames(test) <- colnames(training)
test_2 <- test_2[, c(3:178)]
colnames(test) <- colnames(training)
names(test_2)[1] <- 'class'
test$Max_val <- as.numeric(as.character((test$Max_val)))
test <- rbind(test, test_2)

library(dplyr)
library(forcats)

training <- training %>%
  mutate(class = recode(class, '0' = 'Type1',
                        '1' = 'Type2'))
rm(test_2)

