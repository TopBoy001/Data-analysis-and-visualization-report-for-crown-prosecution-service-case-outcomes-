library(dplyr)
library(tidyr)
library(readr)
library(psych)
library(MASS)
library(tidymodels)
library(ggplot2)
library(plotly)
library(nnet)
library(tidyverse)
library(openxlsx)
library(lubridate)
library(forecast)
library(devtools)
library(visdat)
library(sf)
library(mapview)
library(ggpubr)
library(caTools)
library(ROCR)


# Set the path to folder
path_folder <- "C:/Users/FALCON AVIATION/Desktop/Data"

# List CSV files and create paths
data_paths <- list.files(path = folder, pattern = "*.csv", full.names = TRUE)

# Merge CSV files
cps <- data_paths %>% 
  map_df(~read_csv(.))

# Explore the merged data
glimpse(cps)
view(cps)

#Missing data
sum(is.na(cps))
vis_miss(cps)
vis_dat(cps)

#Explore Data
describe(cps)
View(cps)
str(cps)

# Check for missing values
sum(is.na(cps))

# Visualizing missing data
vis_miss(cps)
vis_dat(cps)


#clean dataset
# Convert percentage columns to numeric
cps <- cps %>% mutate(across(where(is.character), ~gsub("-", "", .)))
cps <- cps %>% mutate(across(where(is.character), ~gsub("%", "", .)))

# rename column
cps <- cps %>% rename(Area = '...1')
cps <- cps %>% rename(Hom_conv = 'Number of Homicide Convictions')
cps <- cps %>% rename(Pers_off = 'Number of Offences Against The Person Convictions')
cps <- cps %>% rename(Sex_off = 'Number of Sexual Offences Convictions')
cps <- cps %>% rename(Burg_conv = 'Number of Burglary Convictions')
cps <- cps %>% rename(Rob_conv = 'Number of Robbery Convictions')
cps <- cps %>% rename(Theft_conv = 'Number of Theft And Handling Convictions')
cps <- cps %>% rename(Frau_conv = 'Number of Fraud And Forgery Convictions')
cps <- cps %>% rename(Crim_dam = 'Number of Criminal Damage Convictions')
cps <- cps %>% rename(Drugs_off = 'Number of Drugs Offences Convictions')
cps <- cps %>% rename(Pub_conv = 'Number of Public Order Offences Convictions')
cps <- cps %>% rename(Oth_off = 'Number of All Other Offences (excluding Motoring) Convictions')
cps <- cps %>% rename(Mot_off = 'Number of Motoring Offences Convictions')
cps <- cps %>% rename(Adm_uns = 'Number of Admin Finalised Unsuccessful')



#Plotting Number of Homicide Convictions
hist(cps$Hom_conv, xlab = "Number of Homicide Convictions", ylab = "Count", main = 'Distribution of Homicide Convictions')

hist(cps$Pers_off, xlab = "Number of Person offence Convictions", ylab = "Count", main = 'Distribution of Person Offence Convictions')

hist(cps$Sex_off, xlab = "Number of Sexual Offence Convictions", ylab = "Count", main = 'Distribution of Sexual Offence Convictions')

hist(cps$Burg_conv, xlab = "Number of Burglary Convictions", ylab = "Count", main = 'Distribution of Burglary Convictions')

hist(cps$Rob_conv, xlab = "Number of Robbery Convictions", ylab = "Count", main = 'Distribution of Robbery Convictions')

hist(cps$Theft_conv, xlab = "Number of Theft Convictions", ylab = "Count", main = 'Distribution of Theft Convictions')

hist(cps$Frau_conv, xlab = "Number of Fraud Convictions", ylab = "Count", main = 'Distribution of Fraud Convictions')

hist(cps$Crim_dam, xlab = "Number of Criminal Damage Convictions", ylab = "Count", main = 'Distribution of Criminal Damage Convictions')

hist(cps$Drugs_off, xlab = "Number of Drugs Offence Convictions", ylab = "Count", main = 'Distribution of Drugs Offence Convictions')

hist(cps$Pub_conv, xlab = "Number of Public Disorder Convictions", ylab = "Count", main = 'Distribution of Public Disorder Convictions')

hist(cps$Oth_off, xlab = "Number of Other Offences Convictions", ylab = "Count", main = 'Distribution of Other Offences Convictions')

#Exploratory Analysis

# Barchart of homicide conviction rates in different areas
ggplot(cps, aes(x = Area, y = Hom_conv, fill = Area)) +
  geom_bar(stat = "identity") +
  theme_classic() +  
  labs(x = "Area", y = "Homicide number", title = "Homicides across areas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")  


# Barchart of Offences against Person conviction in different areas
ggplot(cps, aes(x = Area, y = Pers_off, fill = Area)) +
  geom_bar(stat = "identity") +
  theme_classic() +  
  labs(x = "Area", y = "Person Offences number", title = "Person Offences Conviction across areas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")  

# Barchart of Sexual Offence conviction in different areas
ggplot(cps, aes(x = Area, y = Sex_off, fill = Area)) +
  geom_bar(stat = "identity") +
  theme_classic() +  
  labs(x = "Area", y = "Sexual Offences number", title = "Sexual Offences Conviction across areas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")  

# Barchart of Burglary conviction in different areas
ggplot(cps, aes(x = Area, y = Burg_conv, fill = Area)) +
  geom_bar(stat = "identity") +
  theme_classic() +  
  labs(x = "Area", y = "Burglary number", title = "Burglary Offences Conviction across areas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")  

# Barchart of Robbery conviction in different areas
ggplot(cps, aes(x = Area, y = Rob_conv, fill = Area)) +
  geom_bar(stat = "identity") +
  theme_classic() +  
  labs(x = "Area", y = "Robbery number", title = "Robbery Conviction across areas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") 

# Barchart of Theft convictions in different areas
ggplot(cps, aes(x = Area, y = Theft_conv, fill = Area)) +
  geom_bar(stat = "identity") +
  theme_classic() +  
  labs(x = "Area", y = "Theft conviction number", title = "Theft Conviction across areas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") 

# Barchart of Fraud convictions in different areas
ggplot(cps, aes(x = Area, y = Frau_conv, fill = Area)) +
  geom_bar(stat = "identity") +
  theme_classic() +  
  labs(x = "Area", y = "Fraud conviction number", title = "Fraud Conviction across areas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") 

# Barchart of Criminal Damages Conviction in different areas
ggplot(cps, aes(x = Area, y = Crim_dam, fill = Area)) +
  geom_bar(stat = "identity") +
  theme_classic() +  
  labs(x = "Area", y = "Criminal Damage conviction number", title = "Criminal Damage Conviction across areas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") 

# Barchart of Drugs Offence Conviction in different areas
ggplot(cps, aes(x = Area, y = Drugs_off, fill = Area)) +
  geom_bar(stat = "identity") +
  theme_classic() +  
  labs(x = "Area", y = "Drugs Offence conviction number", title = "Drugs Offence Conviction across areas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") 

#correlation analysis
ggscatter(cps, x = "Hom_conv", y = "Rob_conv", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Homicide Convictions", ylab = "Robbery Convictions",
          color = "darkgreen")

res <- cor.test(cps$Hom_conv, cps$Rob_conv,
                method = 'pearson')

res


l_model <- lm(Hom_conv ~ Rob_conv + Sex_off, 
              data = cps)

summary(l_model)

#cluster model
numbers <- select(cps, Hom_conv, Rob_conv, Sex_off)
set.seed(42)
cluster.conviction <- kmeans(numbers[, 1:2], 2, nstart = 20)
cluster.conviction
table(cluster.conviction$cluster, numbers$Hom_conv)

cluster.conviction$cluster <- as.factor(cluster.conviction$cluster)

ggplot(numbers, aes(x=Hom_conv, y=Rob_conv, color=cluster.conviction$cluster)) +
  geom_point() +
  scale_color_manual(values=c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")) +
  ggtitle("Convictions cluster plot") +
  xlab("Homicide Convictions") +  
  ylab("Robbery Convictions")



#classification
numbers$clusters <- cluster.conviction$cluster
numbers_split <- sample.split(numbers, SplitRatio = 0)
train <- subset(numbers, numbers_split = TRUE)
test <- subset(numbers, numbers_split = FALSE)
log_model <- glm(clusters ~ Hom_conv + Rob_conv + Sex_off, 
                 cps = train)
summary(log_model)

#check accuracy and plot ROC-AUC curve
predictions <- predict(log_model, 
                       test, type = "response")

predictions <- ifelse(predictions > 0.5, 1, 0)

measure <- mean(predictions != test$clusters)
print(paste('Accuracy =', 1 - measure))

# ROC-AUC Curve
ROC <- prediction(predictions, test$clusters)
ROCPer <- performance(ROC, measure = "tpr",
                      x.measure = "fpr")

auc <- performance(ROC, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Plotting curve
plot(ROCPer)
plot(ROCPer, colorize = TRUE,
     print.cutoffs.at = seq(0.1, by = 0.1),
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)

#Hypothesis tests
first_z <- abs((mean(data$Percentage.of.Homicide.Convictions) - 63) / 5)
qnorm(first_z)

#second test
second_z <- abs((mean(data$Percentage.of.Sexual.Offences.Convictions) - 80) / 5)
pnorm(second_z)
