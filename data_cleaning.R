setwd("~/Documents/Syracuse/Courses/ist687_intro_data_science/Project/Resources/data")
# our labels dataset
file <- "scores.csv"

# save as a data frame 
scores_df <- read.csv(file)

# get a dataset with only ones with 13 days' observation
scores_df <- scores_df[scores_df$days == 13, ]
scores_df

# Switch directories 
setwd("~/Documents/Syracuse/Courses/ist687_intro_data_science/Project/Resources/data/condition")

# get all the files in the folder in one list 
files <- list.files(pattern = "\\.csv$")

# created vector with 4 columns
columns <- c("timestamp", "date", "activity", "number", "age", "edu", "gender", 
             "inpatient", "marriage", "work", "madrs_before", "madrs_after", "label") 

# create an empty dataframe
DF <-  data.frame(matrix(nrow = 0, ncol = length(columns)))

colnames(DF) <- columns                   
                  
#reading each file within the range and append them to create one file
for (f in files){
  
  df <- read.csv(f)      # read the file
  
  i <- which(f == files)
  # row number determined by file number
  df$number <- scores_df$number[i]
  
  df$age <- scores_df$age[i]
  
  df$edu <- scores_df$edu[i]
  
  df$gender <- scores_df$gender[i]
  
  df$inpatient <- scores_df$inpatient[i]
  
  df$marriage <- scores_df$marriage[i]
  
  df$work <- scores_df$work[i]
  
  df$madrs_before <- scores_df$madrs1[i]
  
  df$madrs_after <- scores_df$madrs2[i]
  
  df$label <- scores_df$afftype[i]
  
  DF <- rbind(DF, df)    # append the current file
}
#writing the appended file  
write.csv(DF, "condition_dataset.csv", row.names=FALSE, quote=FALSE)

DF <- read.csv("condition_dataset.csv")

summary(DF)

# remove NAs

DF <- na.omit(DF)

 # add labels instead of the numbers 

DF$label[DF$label == 1] <- "Bipolar II"

DF$label[DF$label == 2] <- "Unipolar" 

DF$label[DF$label == 3] <- "Bipolar I" 


# fixing the data types 
library(chron)
DF <- transform(DF, 
                date = as.Date(date),
                activity = as.integer(activity),
                number = as.integer(number),
                age = as.factor(age),
                edu = as.factor(edu),
                gender = as.factor(gender),
                inpatient = as.factor(inpatient),
                marriage = as.factor(marriage),
                work = as.factor(work),
                label = as.factor(label))

# Set seed and take random sample 

set.seed(180)

data <- sample_n(DF, 5000)
data <- na.omit(data)
summary(data)

# load Random Forest Classifier 
library(randomForest)
require(caTools)


# Random Forest Sampling

sample = sample.split(data$label, SplitRatio = .70)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)

rf <- randomForest(label ~ ., data=train)
conf <- rf$confusion
conf
importance(rf)

varImpPlot(rf)

data.rf <- randomForest(data[,-13], data[,13], na.action = na.omit)
data.rf$confusion
importance(data.rf)

varImpPlot(data.rf)

# Under Sampling 
tree_undersample <- rpart(label ~ ., method = "class", data = train)
plot(tree_undersample, uniform = T)
text(tree_undersample)

# Using only activity and other scores to predict Symptoms After
head(data)
test_acc <- data[, -8:-11]
test_acc <- test_acc[,-2:-3]
test_acc <- transform(test_acc, 
                            activity = as.integer(activity),
                            gender = as.factor(gender),
                            inpatient = as.factor(inpatient),
                            marriage = as.factor(marriage),
                            work = as.factor(work),
                            Symptoms_After = as.factor(Symptoms_After))

summary(test_acc)
test.rf <- randomForest(test_acc[,-6], test_acc[,6], na.action = na.omit)
test.rf$confusion
importance(test.rf)

varImpPlot(test.rf)

test_acc <- test_acc %>% mutate(work = case_when(work == 1 ~ "Studying/Working",
                                                           TRUE ~ "Unemployed/Sick Leave/Pension"))

ggplot(test_acc, aes(x=activity, y=work, color=Symptoms_After)) + 
  geom_point(size=4) +
  theme_ipsum() + labs(title = "Plot of Activity Level by Work",
                       subtitle = "Effect of Activity on Symptoms of Depression",
                       caption = "Data Source: Kaggle",
                       x = "Activity",
                       y = "Work") +
  theme(text = element_text(color = "#444444", family = 'Helvetica Neue')
        ,plot.title = element_text(size = 18, color = '#333333')
        ,plot.subtitle = element_text(size = 10)
        ,axis.title = element_text(size = 14, color = '#333333')
        ,axis.title.x = element_text(size = 10, vjust = .9, hjust = 0.5)
        ,axis.title.y = element_text(angle = 90, vjust = .9, hjust = 0.5)
  ) + scale_color_discrete(name = "Symptoms")



# load ggplot2
library(ggplot2)
library(hrbrthemes)

# Graphs based on the MADRS scores 

# Set seed and take random sample 

set.seed(180)

data <- sample_n(DF, 5000)
data <- na.omit(data)
summary(data)

# Symptoms before Study
data <- data %>% mutate(Symptoms_After = case_when(madrs_before < 7 ~ "Normal",
                                                    madrs_before < 19 ~ "Mild",
                                                    madrs_before < 34 ~ "Moderate",
                                                    TRUE ~ "Severe"))

ggplot(data, aes(x=activity, y=madrs_before, color=Symptoms_After)) + 
  geom_point(size=4) +
  theme_ipsum() + labs(title = "Plot of Symptoms Before Study",
                       subtitle = "Effect of Activity on Montgomery-Åsberg Depression Rating Scale (MADRS)",
                       caption = "Data Source: Kaggle",
                       x = "Activity",
                       y = "MADRS Score Before Study") +
  theme(text = element_text(color = "#444444", family = 'Helvetica Neue')
        ,plot.title = element_text(size = 18, color = '#333333')
        ,plot.subtitle = element_text(size = 10)
        ,axis.title = element_text(size = 14, color = '#333333')
        ,axis.title.x = element_text(size = 10, vjust = .9, hjust = 0.5)
        ,axis.title.y = element_text(angle = 90, vjust = .9, hjust = 0.5)
  ) + scale_color_discrete(name = "Symptoms")

# Symptoms After Study 

data <- data %>% mutate(Symptoms_After = case_when(madrs_after < 7 ~ "Normal",
                                                   madrs_after < 19 ~ "Mild",
                                                   madrs_after < 34 ~ "Moderate",
                                                   TRUE ~ "Severe"))

ggplot(data, aes(x=activity, y=madrs_before, color=Symptoms_After)) + 
  geom_point(size=4) +
  theme_ipsum() + labs(title = "Plot of Symptoms After Study",
                       subtitle = "Effect of Activity on Montgomery-Åsberg Depression Rating Scale (MADRS)",
                       caption = "Data Source: Kaggle",
                       x = "Activity",
                       y = "MADRS Score After Study") +
  theme(text = element_text(color = "#444444", family = 'Helvetica Neue')
        ,plot.title = element_text(size = 18, color = '#333333')
        ,plot.subtitle = element_text(size = 10)
        ,axis.title = element_text(size = 14, color = '#333333')
        ,axis.title.x = element_text(size = 10, vjust = .9, hjust = 0.5)
        ,axis.title.y = element_text(angle = 90, vjust = .9, hjust = 0.5)
  ) + scale_color_discrete(name = "Symptoms")



# Graphs based on the Affect Type 

ggplot(data, aes(x=activity, y=madrs_before, color=label)) + 
  geom_point(size=4) +
  theme_ipsum() + labs(title = "Plot of Affect Types Before Study",
                       subtitle = "Effect of Activity on Montgomery-Åsberg Depression Rating Scale (MADRS)",
                       caption = "Data Source: Kaggle",
                       x = "Activity",
                       y = "MADRS Score Before Study") +
  theme(text = element_text(color = "#444444", family = 'Helvetica Neue')
        ,plot.title = element_text(size = 18, color = '#333333')
        ,plot.subtitle = element_text(size = 10)
        ,axis.title = element_text(size = 14, color = '#333333')
        ,axis.title.x = element_text(size = 10, vjust = .9, hjust = 0.5)
        ,axis.title.y = element_text(angle = 90, vjust = .9, hjust = 0.5)
  ) + scale_color_discrete(name = "Affect Type")

ggplot(data, aes(x=activity, y=madrs_after, color=label)) + 
  geom_point(size=4) +
  theme_ipsum() + labs(title = "Plot of Affect Types After Study",
                       subtitle = "Effect of Activity on Montgomery-Åsberg Depression Rating Scale (MADRS)",
                       caption = "Data Source: Kaggle",
                       x = "Activity",
                       y = "MADRS Score After Study") +
  theme(text = element_text(color = "#444444", family = 'Helvetica Neue')
        ,plot.title = element_text(size = 18, color = '#333333')
        ,plot.subtitle = element_text(size = 10)
        ,axis.title = element_text(size = 14, color = '#333333')
        ,axis.title.x = element_text(size = 10, vjust = .9, hjust = 0.5)
        ,axis.title.y = element_text(angle = 90, vjust = .9, hjust = 0.5)
  ) + scale_color_discrete(name = "Affect Type")

ggplot(data, aes(x=activity, y=age, color=label)) + 
  geom_point(size=4) +
  theme_ipsum() + labs(title = "Plot of Activity Level by Age Group",
                       subtitle = "Effect of Activity on Affect Type",
                       caption = "Data Source: Kaggle",
                       x = "Activity",
                       y = "Age Group") +
  theme(text = element_text(color = "#444444", family = 'Helvetica Neue')
        ,plot.title = element_text(size = 18, color = '#333333')
        ,plot.subtitle = element_text(size = 10)
        ,axis.title = element_text(size = 14, color = '#333333')
        ,axis.title.x = element_text(size = 10, vjust = .9, hjust = 0.5)
        ,axis.title.y = element_text(angle = 90, vjust = .9, hjust = 0.5)
  ) + scale_color_discrete(name = "Affect Type")

# Load class package
library(class)

# Set seed and take random sample 

set.seed(180)

data <- sample_n(DF, 5000)
data <- na.omit(data)
summary(data)

# Begin testing K Nearest Neighbors 

data <- data[,c(3, 11,12,13)]
data <- na.omit(data)
head(data)
data <- transform(data, 
                        activity = as.numeric(activity),
                        madrs_before = as.numeric(madrs_before),
                        madrs_after = as.numeric(madrs_after),
                        label = as.factor(label))
str(data)

# split train and test
sample <- sample.split(data$label, SplitRatio = .7)
train.dep <- subset(data, sample == "TRUE")
test.dep  <- subset(data, sample == "FALSE")

# check dimensions
dim(train.dep)
dim(test.dep)

# Run KNN with 3 clusters 
knn.dep <- knn(train=train.dep, test=test.dep, cl=train.dep$label, k = 3)

# Find accuracy score 
ACC.dep <- 100 * sum(test.dep$label == knn.18) / NROW(test.dep$label)
ACC.dep

# ~ 72% 

library(caret)
library(e1071)
confusionMatrix(table(knn.18, test.dep$label))


##############
# Work in Progress Section 
##############

# Elbow curve

i = 1
k.optim = 1
bestk = 0 
acc_max = 0 

for (i in 1:28) {
  knn.mod <- knn(train=train.dep, test=test.dep, cl=train.dep$label, k = i)
  k.optim[i] <- 100 * sum(test.dep$label == knn.mod) / NROW(test.dep$label)
  i = k
  cat(k, "=", k.optim[i])
  
  if (k.optim[i] > acc_max) {
    bestk <- i 
    acc_max = k.optim[i]
  }
  
}

plot(k.optim, type="b", xlab="K- Value", ylab="Accuracy Level")
print("Optimal K")
knn.max <- knn(train=train.dep, test=test.dep, cl=train.dep$label, k = bestk)
acc_max <- 100 * sum(test.dep$label == knn.max) / NROW(test.dep$label)
cat("Best k is", bestk)
confusionMatrix(table(knn.max, test.dep$label))


# Use for Undersampling to get more balanced dataset
#options(repos = c(CRAN = 'http://cran.rstudio.com'))
#install.packages("DMwR")

#classTree <- SMOTE(label ~ ., data, perc.over = 600, perc.under = 100, learner = 'rpartXse', se=0.5)


# KMeans Clustering 

# Installing Packages
install.packages("ClusterR")
install.packages("cluster")
install.packages("VIM")

# Loading package
library(ClusterR)
library(cluster)

# VIM library for using 'aggr'
library(VIM)

# 'aggr' plots the amount of missing/imputed values in each column
aggr(df_norm)

# Fitting K-Means clustering Model 
# to training dataset
set.seed(240) # Setting seed
kmeans.re <- kmeans(df, centers = 6, nstart = 20)
kmeans.re

# Cluster identification for 
# each observation
kmeans.re$cluster

## Visualizing clusters
y_kmeans <- kmeans.re$cluster
clusplot(df[, c("X1", "X2")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster dep"),
         xlab = 'X1',
         ylab = 'X2')