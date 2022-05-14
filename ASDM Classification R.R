#Set working Directory

setwd("C:/Users/ogech/OneDrive/Desktop/ASDM Coursework - Classification")
getwd()

# install.packages("tidyr")
# install.packages("reshape2")
# install.packages("ggfortify")
# install.packages("pander")
# install.packages("GGally")
# install.packages("MASS")
# install.packages("caTools")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("caret")
# install.packages("caretEnsemble")
# install.packages("corrplot")
# install.packages("gridExtra")
# install.packages("pROC")
# install.packages("doMC")
# install.packages("tidyr")

library(reshape2)
library(ggfortify)
library(pander)
library(GGally)
library(MASS)
library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(corrplot)
library(gridExtra)
library(pROC)
library(doMC)
library(tidyr)

#Create the dataframe

cancer_dataframe<- read.csv("data.csv", header=TRUE)
str(cancer_dataframe)
summary(cancer_dataframe)
head(cancer_dataframe)

#Modify the diagnosis column: B to Benign and M to Malignant 

cancer_dataframe$diagnosis <- factor(cancer_dataframe$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))


#Checking the Missing Values in the columns
sapply(cancer_dataframe, function(y) sum(is.na(y)))

#checking the total count of B and M
ggplot(cancer_dataframe, aes(x=diagnosis)) + geom_text(stat='Count', vjust=-1, aes(label=..count..)) + geom_bar()


#Plotting the contents on the pie chart
lbls <- paste(names(table(cancer_dataframe$diagnosis)), round(prop.table(table(cancer_dataframe$diagnosis)) * 100, digits = 1), "%")

pie(table(cancer_dataframe$diagnosis), labels = lbls, main = "Diagnosis", col = c("violet", "grey"))
box()


#Checking the diagnosis percentage
percentage_split <- round(100*prop.table(table(cancer_dataframe$diagnosis)), 1)
percentage_split

####### Taking the diagnosis column as a factor #########
cancer_dataframe$diagnosis <- as.factor(cancer_dataframe$diagnosis)


#Removing the last column X
cancer_dataframe[,33] <- NULL

#Removing the first column id
cancer_dataframe[,1] <- NULL

prop.table(table(cancer_dataframe$diagnosis))

install.packages("corrgram")

#Plot the Mean variables, Worst variables and SE vaiables against Diagosis

# install.packages("ggstatsplot")
library(ggstatsplot)

# install.packages("ggcorrplot")
library(ggcorrplot)

# correlogram of _mean variables vs diagnosis
ggstatsplot::ggcorrmat(
  data = cancer_dataframe[,c(2:11)],
  type = "parametric", 
  colors = c("darkred", "white", "steelblue") 
)

# correlogram of _se variables vs diagnosis
ggstatsplot::ggcorrmat(
  data = cancer_dataframe[,c(12:21)],
  type = "parametric", 
  colors = c("darkred", "white", "steelblue") 
)

# correlogram of _worst variables vs diagnosis
ggstatsplot::ggcorrmat(
  data = cancer_dataframe[,c(22:31)],
  type = "parametric", 
  colors = c("darkred", "white", "steelblue") 
)


#Setting the Seed
set.seed(1234)

#Splitting the Data
cancer_dataframe_train <- cancer_dataframe[1:469, -1]
cancer_dataframe_test <- cancer_dataframe[470:569, -1]
cancer_dataframe_train_labels <- cancer_dataframe[1:469, 1]
cancer_dataframe_test_labels <- cancer_dataframe[470:569, 1]


#Scaling the Data separately
cancer_dataframe_z <- as.data.frame(scale(cancer_dataframe[-1]))
cancer_dataframe_train <- cancer_dataframe_z[1:469, ]
cancer_dataframe_test <- cancer_dataframe_z[470:569, ]


library(class)
library(gmodels)

cancer_dataframe_test_pred <- knn(train = cancer_dataframe_train, test = cancer_dataframe_test,
                                  cl = cancer_dataframe_train_labels, k = 18)

CrossTable(x = cancer_dataframe_test_labels, y = cancer_dataframe_test_pred,
           prop.chisq = FALSE)

accuracy_knn = (77+21)/100
accuracy_knn


library(e1071)

nb_classifier <- naiveBayes(cancer_dataframe_train, cancer_dataframe_train_labels, laplace = 1)
nb_eval_pred <- predict(nb_classifier,cancer_dataframe_test)

CrossTable(nb_eval_pred, cancer_dataframe_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))

accuracy_nb = (71+21)/100
accuracy_nb


#install.packages("C50")
library(C50)
dt_tree <- C5.0(cancer_dataframe_train, cancer_dataframe_train_labels, trials = 99)
dt_tree

dt_tree_pred <- predict(dt_tree, cancer_dataframe_test)
CrossTable(dt_tree_pred, dt_tree_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual class', 'predicted class'))

#Put the correct predicted numbers below to calculate accuracy
accuracy_dt = (76+24)/100
accuracy_dt


