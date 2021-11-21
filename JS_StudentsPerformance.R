################################################################################
# Project title: 'Prediction of Secondary School Student Grades Using UCI Data '
################################################################################

# Create package to need for our report 
pkg<-c("tidyverse", "dslabs","dplyr","ggplot2","caret","data.table","lattice","readr","magrittr","skimr","Hmisc","psych","gridExtra","doBy", "corrplot", "pheatmap", "ROCR", "gplots", "irr", "janitor", "PerformanceAnalytics", "rpart", "rattle")

# Create package to need for installation
new_pkg<-pkg[!(pkg %in% rownames(installed.packages()))]

# if there are packages uninstalled, install the package at a time
if(length(new_pkg))install.packages(new_pkg, dependencies = TRUE)

# Road the package at a time
suppressMessages(sapply(pkg, require, character.only=TRUE)) # suppress the complicated messages.

#################
# Data collection
#################
# The dataset were imported from UCI Machine Learning Respository website: http://archive.ics.uci.edu/ml/datasets/Student+Performance#\n
# The file is located at: http://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip
# The file name is "student-mat.csv"
# Download the dataset in our local computer and load the data 
# An example of the path of downloaded and unzipped folder in a local computer: "C:/datascience/r/projects/performance/data/student/"
# Load the data and store "mat" objective
mat <- read.table("C:/datascience/r/projects/performance/data/student/student-mat.csv", sep=";",header=TRUE)

#######################
# 2.1. Data Preparation
#######################
#The "mat" dataset is ready for analysis. But we will modify it for specific uses by creating several datasets. We will use them accordingly in EDA and modelling.  Further we will clean and modify more in the "Data Pre-processing" section and other sections as necessary. 

# Create a new dataset called "new_mat" for changing numeric G1, G2 and G3 variables into binary variables. Let's define G3>= 8 as "1" meaning "pass" and otherwise, "0" meaning "fail". The new_mat dataset has a binary dependant variable whereas the mat dataset has a numeric dependant variable. We will use both accordingly.  
new_mat<-mat
new_mat$G3<-ifelse(new_mat$G3>=8, 1, 0)
new_mat$G1<-ifelse(new_mat$G1>=8, 1, 0)
new_mat$G2<-ifelse(new_mat$G2>=8, 1, 0)

# Create a factor dataset called "fnew_mat". 
fnew_mat<-new_mat
fnew_mat$school<-factor(mat$school, levels=c("GP","MS"))
fnew_mat$sex <- factor(mat$sex, levels=c("F","M"))
fnew_mat$age <- factor(mat$age, levels=c("15","16", "17","18","19","20","21","22"))
fnew_mat$address <- factor(mat$address, levels=c("U","R"))
fnew_mat$famsize <- factor(mat$famsize, levels=c("LE3","GT3"))
fnew_mat$Pstatus <- factor(mat$Pstatus, levels=c("T","A"))
fnew_mat$Medu <- factor(mat$Medu, levels=c("0","1", "2","3","4")) 
fnew_mat$Fedu <- factor(mat$Fedu, levels=c("0","1", "2","3","4")) 
fnew_mat$Mjob <- factor(mat$Mjob, levels=c("teacher","health","services","at_home","other"))
fnew_mat$Fjob <- factor(mat$Fjob, levels=c("teacher","health","services","at_home","other"))
fnew_mat$reason <- factor(mat$reason, levels=c("home","reputation","course","other"))
fnew_mat$guardian <- factor(mat$guardian, levels=c("mother","father","other"))
fnew_mat$traveltime <- factor(mat$traveltime, levels=c("1", "2","3","4")) 
fnew_mat$studytime <- factor(mat$studytime, levels=c("1", "2","3","4")) 
fnew_mat$failures <- factor(mat$failures, levels=c("0","1", "2","3","4")) 
fnew_mat$schoolsup <- factor(mat$schoolsup, levels=c("yes","no"))
fnew_mat$famsup <- factor(mat$famsup, levels=c("yes","no"))
fnew_mat$paid <- factor(mat$paid, levels=c("yes","no"))
fnew_mat$activities <- factor(mat$activities, levels=c("yes","no"))
fnew_mat$nursery <- factor(mat$nursery, levels=c("yes","no"))
fnew_mat$higher <- factor(mat$higher, levels=c("yes","no"))
fnew_mat$internet <- factor(mat$internet, levels=c("yes","no"))
fnew_mat$romantic <- factor(mat$romantic, levels=c("yes","no"))
fnew_mat$famrel <- factor(mat$famrel, levels=c("1", "2","3","4", "5")) 
fnew_mat$freetime <- factor(mat$freetime, levels=c("1", "2","3","4", "5")) 
fnew_mat$goout <- factor(mat$goout, levels=c("1", "2","3","4", "5")) 
fnew_mat$Dalc <- factor(mat$Dalc, levels=c("1", "2","3","4", "5")) 
fnew_mat$Walc <- factor(mat$Walc, levels=c("1", "2","3","4", "5")) 
fnew_mat$health <- factor(mat$health, levels=c("1", "2","3","4", "5")) 
fnew_mat$absences<-ifelse(new_mat$absence<2, 0, 
                          ifelse(new_mat$absences>=3 & new_mat$absences<10, 1, 
                                 ifelse(new_mat$absences >=10 & new_mat$absences <20, 2, 
                                        ifelse(new_mat$absences >=20 & new_mat$absences <40, 3, 4))))
fnew_mat$absences<-factor(new_mat$absences, levels=c("0", "1", "2", "3", "4"))
fnew_mat$G1<-factor(new_mat$G1, levels=c("0", "1"), labels=c("fail", "pass"))
fnew_mat$G2<-factor(new_mat$G2, levels=c("0", "1"), labels=c("fail", "pass"))
fnew_mat$G3<-factor(new_mat$G3, levels=c("0", "1"), labels=c("fail", "pass"))

# Create a binomial dataset where G3 is binomial dependent variable and others are factors.
bi<-fnew_mat
bi$G1<-new_mat$G1
bi$G1<-as.factor(new_mat$G1)
bi$G2<-new_mat$G2
bi$G2<-as.factor(new_mat$G2)
bi$G3<-new_mat$G3
bi$G3<-as.factor(new_mat$G3)

# Create a numeric dataset called "num" for analizing correlation among attributes
num<-dplyr::select(mat, age, Medu, Fedu, traveltime, studytime, failures, famrel, freetime, goout,Dalc, Walc,health,absences, G1, G2, G3)

# Create numeric dataset called "allnum" for all variables
allnum<-mat
allnum$school<-as.numeric(ifelse(fnew_mat$school=="GP", 0, 1)) 
allnum$sex<-as.numeric(ifelse(fnew_mat$sex=="F", 1, 0)) 
allnum$address<-as.numeric(ifelse(fnew_mat$address=="U", 1, 0)) 
allnum$famsize<-as.numeric(ifelse(fnew_mat$famsize=="LE3", 0, 1)) 
allnum$Pstatus<-as.numeric(ifelse(fnew_mat$Pstatus=="T", 1, 0)) 
allnum$Mjob<-as.numeric(ifelse(fnew_mat$Mjob=="teacher", 4, 
                               ifelse(fnew_mat$Mjob=="health", 3, 
                                      ifelse(fnew_mat$Mjob=="services", 2,
                                             ifelse(fnew_mat$Mjob=="at_home", 1, 0))))) 
allnum$Fjob<-as.numeric(ifelse(fnew_mat$Fjob=="teacher", 4, 
                               ifelse(fnew_mat$Fjob=="health", 3, 
                                      ifelse(fnew_mat$Fjob=="services", 2,
                                             ifelse(fnew_mat$Fjob=="at_home", 1, 0))))) 
allnum$reason<-as.numeric(ifelse(fnew_mat$reason=="home", 3, 
                                 ifelse(fnew_mat$reason=="reputation", 2, 
                                        ifelse(fnew_mat$reason=="course", 1, 0))))
allnum$guardian<-as.numeric(ifelse(fnew_mat$guardian=="mother", 2, 
                                   ifelse(fnew_mat$guardian=="father", 1, 0))) 
allnum$schoolsup<-as.numeric(ifelse(fnew_mat$schoolsup=="yes", 1, 0)) 
allnum$famsup<-as.numeric(ifelse(fnew_mat$famsup=="yes", 1, 0)) 
allnum$paid<-as.numeric(ifelse(fnew_mat$paid=="yes", 1, 0)) 
allnum$activities<-as.numeric(ifelse(fnew_mat$activities=="yes", 1, 0)) 
allnum$nursery<-as.numeric(ifelse(fnew_mat$nursery=="yes", 1, 0)) 
allnum$higher<-as.numeric(ifelse(fnew_mat$higher=="yes", 1, 0)) 
allnum$internet<-as.numeric(ifelse(fnew_mat$internet=="yes", 1, 0)) 
allnum$romantic<-as.numeric(ifelse(fnew_mat$romantic=="yes", 1, 0)) 

# Create a new dataset called "ex_mat, ex_num, ex_new_mat, ex_fnew_mat" for removing G1 and G2 because of too obviously decisive indicators of the dependent variable and becasue of a wider application in other cases where many schools might not have the results of the previous tests
ex_num<-num[, c(1:13,16)]
ex_new_mat<-new_mat[, c(1:30,33)]
ex_bi<-bi[, c(1:30,33)]

#################################
# EDA (Exploratory Data Analysis)
#################################

# 2.2.1. Overview of the Dataset
# Let's take a look an overall picture of the "mat" dataset through statistical analysis and get some insights from it.
# We will use different packages for statistical analysis because each package has the unique way with different statistical displays.  

# a. Statistical analysis  
str(mat)
# There are 395 observations of 33 variables. We can see the content of levels for character variable. 

# b. Statistical analysis  
Hmisc::describe(mat)
# We can clearly check the number of missing data, frequency and proportion of levels of each variables. We will analyze some variables in the end of this section.   

# c. Statistical analysis 
psych::describe(mat)
# The table above provides a very comprehensive list of the descriptive statistics for our dataset. As we are just going to scroll across the table, it shows us the variable number, "n" which indicates the number of observations for each variable, "mean" which indicates the average value, "standard deviation", "median", "trimmed", "mad" which means the mean absolute deviation, "max", "min" which means the minimum value, "range" which means between the two, "skew" means the measure of how skewed our dataset is, "kurtosis" and "se" which indicates the standard error. 
# This statistics are very useful for the numeric variables but not for the categorical ones which are denoted with asterisks in the table.  <br>

# d. Statistical analysis 
skimr::skim(mat)
# We can quickly assess our data features and types. It goes even deeper by each character variables arranged according to data types. The above two tibbles are grouped by variable types: character and numeric. It gives some information about the number of missing data, completed rate, distribution of the values. It also shows unique values for character variables, whereas it displays distribution such as mean, standard deviation, quantile and a little histogram for numeric variables. 

#################################
# Data Analysis and Visualization
#################################
# we will explore the data by asking and answering questions about variables and their relations to get some insights.  

# a. Show graphs for all variables.
# Let's explore overall tendency of each variable displaying multiple histograms
par(mfrow=c(3,3)) # Show 3*3 graphs on each pane
hist.data.frame(mat)
par(mfrow=c(1,1)) # Return the format of displaying graph showing 1*1 graph on each pane

# b. How many observations and variables are in the dataset?
dim(new_mat)
# There are 395 observations and 33 variables.  

# c. What proportion of individuals is passed?
mean(new_mat$G3)

# d. What is the name of columns of numeric variabls?
names(num) 

# e. Show the distribution of the dependent variable G3
mat$G3%>%qplot()
janitor::tabyl(mat$G3)%>%print # Show frequency and percentage in original G3 variable. 
tabyl(fnew_mat$G3) # Show frequency and percentage in binomial G3 variable.  
# There are around 18% students that failed the test. 

# f. Show analysis tibble for whether there is the difference in the characteristics of the numeric variable for G3.
num%>%select(age:absences, G3)%>% group_by(G3)%>%summarise_all(mean) 
# Medu, Fedu, studytime, failures, goout, Walc and absences are significant variables of which failure and absences are the most. 

# g. Show box plots analyzing whether there is the difference in the level of numeric variables for G3.
p1<-ggplot(data=mat, aes(x=G3, y=age, fill=G3)) + geom_boxplot()
p2<-ggplot(data=mat, aes(x=G3, y=Medu, fill=G3)) + geom_boxplot()
p3<-ggplot(data=mat, aes(x=G3, y=Fedu, fill=G3)) + geom_boxplot()
p4<-ggplot(data=mat, aes(x=G3, y=traveltime, fill=G3)) + geom_boxplot()
p5<-ggplot(data=mat, aes(x=G3, y=studytime, fill=G3)) + geom_boxplot()
p6<-ggplot(data=mat, aes(x=G3, y=failures, fill=G3)) + geom_boxplot()
p7<-ggplot(data=mat, aes(x=G3, y=famrel, fill=G3)) + geom_boxplot()
p8<-ggplot(data=mat, aes(x=G3, y=freetime, fill=G3)) + geom_boxplot()
p9<-ggplot(data=mat, aes(x=G3, y=goout, fill=G3)) + geom_boxplot()
p10<-ggplot(data=mat, aes(x=G3, y=Dalc, fill=G3)) + geom_boxplot()
p11<-ggplot(data=mat, aes(x=G3, y=Walc, fill=G3)) + geom_boxplot()
p12<-ggplot(data=mat, aes(x=G3, y=health, fill=G3)) + geom_boxplot()
p13<-ggplot(data=mat, aes(x=G3, y=absences, fill=G3)) + geom_boxplot()
p14<-ggplot(data=mat, aes(x=G3, y=school, fill=G3)) + geom_boxplot()
p15<-ggplot(data=mat, aes(x=G3, y=sex, fill=G3)) + geom_boxplot()
p16<-ggplot(data=mat, aes(x=G3, y=address, fill=G3)) + geom_boxplot()
p17<-ggplot(data=mat, aes(x=G3, y=famsize, fill=G3)) + geom_boxplot()
p18<-ggplot(data=mat, aes(x=G3, y=Pstatus, fill=G3)) + geom_boxplot()
p19<-ggplot(data=mat, aes(x=G3, y=Mjob, fill=G3)) + geom_boxplot()
p20<-ggplot(data=mat, aes(x=G3, y=Fjob, fill=G3)) + geom_boxplot()
p21<-ggplot(data=mat, aes(x=G3, y=guardian, fill=G3)) + geom_boxplot()
p22<-ggplot(data=mat, aes(x=G3, y=schoolsup, fill=G3)) + geom_boxplot()
p23<-ggplot(data=mat, aes(x=G3, y=famsup, fill=G3)) + geom_boxplot()
p24<-ggplot(data=mat, aes(x=G3, y=paid, fill=G3)) + geom_boxplot()
p25<-ggplot(data=mat, aes(x=G3, y=activities, fill=G3)) + geom_boxplot()
p26<-ggplot(data=mat, aes(x=G3, y=nursery, fill=G3)) + geom_boxplot()
p27<-ggplot(data=mat, aes(x=G3, y=higher, fill=G3)) + geom_boxplot()
p28<-ggplot(data=mat, aes(x=G3, y=internet, fill=G3)) + geom_boxplot()
p29<-ggplot(data=mat, aes(x=G3, y=romantic, fill=G3)) + geom_boxplot()

grid.arrange(p1,p2,p3, p4,p5, p6,p7, p8,p9, p10,p11, p12,p13, ncol=3)
# Fedu and failures seems significant variables for G3. 

# h. Show box plots analyzing whether there is the difference in the level of character variables for G3.
grid.arrange(p14,p15, p16,p17, p18,p19, p20,p21,p22,p23,p24,p25,p26,p27,p28,p29, ncol=3)
# school, sex, address, Mjob, Fjob, gaurdian, schoolsup, higher and internet are factors which affect on results of G3. <br>
  
#############
# Correlation 
#############
# Let's examine and analyse how our variables are correlated with each other.  

# i. Show correlation numbers in the numeric variables
round(cor(allnum),3)
# The correlation varies between 1(positive correlation) and -1(negative correlation). school,Pstatus, reason, guardian, famsup, activities, nursery, internet, famrel, freetime, absences, Dalc and Walc variables are less related with G3 compared to other variables.  

# j. Visualize the correlations
# It is difficult to see the correlation in numbers at once. Let's visualize it. 
pheatmap(cor(num))
# The correlation varies between 1 (positive) and -1 (negative). The dark red color indicates the high correlated positive relation, for example: G1 and G3; G2 and G3. The dark blue color indicates highly correlated negative relation, such as failures and study time. 
pheatmap(cor(allnum))

# k. Show a heatmap with correlation numbers in the numeric variables
corrplot.mixed(cor(num))
# Let's think more correlated variables except the above results of correlation from "num" objective
corrplot.mixed(cor(allnum))
# To sum up about correlated variables
# Fitst, direct influential variables whose absolute value of the relation with G3 is greater than 0.2 are sex, age, address, famsize, Medu, Fedu, Mjob, Fjob, traveltime, studytime, failures, schoolsup, paid, higher, romantic, goout, G1 and G2. They are more correlated with G3 compared to other variables. Whereas school, Pstatus, reason, guardian, famsup, activities, nursery, internet, famrel, freetime, absences, Dalc, Walc and health variables are less related with G3 compared to other variables. We will exclude G1 and G2 from now on. Because we want to know the reason behind poor results except the previous result of any performance. 
# Second, indirect influential variables whose absolute value of the correlation with the direct influential variables is greater than 0.2 are famsup, school, internet, Dalc, Walc, freetime and gauardian variables. They are more correlated with direct influential variables correlated with G3 directly compared to other variables. 
# Third, excluded variables are Pstatus, reason, famsup, activities, nursery, famrel, health and absences. This is because they seem less important variables in consideration with correlation with G3. 

# l. Create a dataset which is excluded less related variables
imp_num<-ex_num[, !(names(ex_num)%in% c("Pstatus", "reason", "famsup", "activities", "nursery", "famrel", "health", "absences"))]

# m. Show correlation table and graph at a time
chart.Correlation(num, Histogram=T)
# Although absences variable has shown in the above graph that it has * correlated with age, Medu, Dalc Walc variables, we will still exclude it because it is less than 0.2

# n. Show analysis of relation among decisive variables on G3
# The most three direct decisive variables are failure, Medu and studytime. And many variables are related with these decisive variables. Among them, We will examine the following: failure, age and sex(gender)
# The reason to choose it is that failure is the most decisive variable on G3. Age has correlation number with failure (0.24) and worth exploring. Sex(gender) has correlation with studytime(0.31) which has correlation with failure.

# n-1. Show G1+G2+G3, age and sex relation
G1=ggplot(data=mat,aes(x=age, y=G1+G2+G3, col=sex, shape=sex))+geom_point()+geom_smooth(method="lm",se=F)+facet_grid(~sex)
G1
# The girls' grades are consistent with their age whereas boys' grades are getting decreased with their age. Behind of the results might be explained with the following questions and codes. 

# n-2.Why boys are getting bad at their grades with their age? 
G1=ggplot(data=mat,aes(x=age, y=failures, col=sex, shape=sex))+geom_point()+geom_smooth(method="lm",se=F)+facet_grid(~sex)
G1
# As the students are getting older, they might experience failures which directly affect the results of grades. Thy boys might experience more failures than girls. Let's deep into the reason. 

# n-3. Which variable affect boys' failure? 
# Let's explore alcohol consumption because one of variables that related with failures are studytime which is also related Dalc and Walc. 
my_graph <- ggplot(mat, aes(x = Dalc + Walc, y = G1+G2+G3)) +
    geom_point(aes(color = sex)) +
    stat_smooth(method = "lm",
    se = FALSE,
    size = 3)
my_graph
# As the line explains that as alcohol consumption increases the grades decrease. Alcohol intake may lead to frequent confusion and an inability to remember, which results in poor performance. The higher amount of alcohol consumption is from boys. Boys's higher intake of alcohol than girls might affect on their poorer grades than girls.  

# n-4. Might a negatively correlated variable, traveltime affect on male's relatively poor results?
table(mat$traveltime)
travel=ggplot(data=mat, aes(x=traveltime, y=G1, col=sex))+geom_point()+geom_smooth(method="lm",se=F)+facet_grid(~sex)
travel
# As shown above, the negative impact of the traveltime could be largely seen in the boys' performance, the further the male student resides from the school, the less their performance would they get.
# Therefore, the difference between results of female and male would be explained with failures, alcohol consumption, traveltime. 

####################################################
# Data Pre-processing_Data Modification and Cleaning
####################################################
# Checking NA (Null Values)
# We have already checked the missing values in several statistic tables before. Let's one more check using a simple code. 
# Check missing data
sapply(mat, anyNA)
# There is no NA in all columns. 

# Checking Outliers
boxplot(num, main="Multiplot Visualization for numeric variables" )
# Absences variable has many outliers.

# Data Partition
# Let's split dataset into trainset and testset.
set.seed(3, sample.kind = "Rounding") 
partition <- createDataPartition(new_mat[,'G3'], times=1, p=0.80, list=FALSE)
new_mat_trainset<-new_mat[partition,]
new_mat_testset<-new_mat[-partition,]

partition <- createDataPartition(ex_new_mat[,'G3'], times=1, p=0.80, list=FALSE)
ex_new_mat_trainset<-ex_new_mat[partition,]
ex_new_mat_testset<-ex_new_mat[-partition,]

partition <- createDataPartition(ex_bi[,'G3'], times=1, p=0.80, list=FALSE)
ex_bi_trainset<-ex_bi[partition,]
ex_bi_testset<-ex_bi[-partition,]

###########
# Modelling
###########
  
# Model 1: Baseline prediction by randomly guessing the outcome
# The simplest prediction method is randomly guessing the outcome, whether that person passed or not by sampling from the vector c(0,1), without using additional predictors. These methods will help us determine whether our machine learning algorithm performs better than chance. How accurate are this method of guessing students' final test?
# Let's apply this method to different datasets.

# Modeling and Prediction
# set.seed(3)  
set.seed(1, sample.kind = "Rounding")

# Guess with equal probability of pass
guess <- sample(c(0,1), nrow(new_mat_trainset), replace = TRUE)
mean(guess == new_mat_testset$G3)

# set.seed(2)  
set.seed(2, sample.kind = "Rounding")
# guess with equal probability of pass with ex_new_mat dataset
guess <- sample(c(0,1), nrow(ex_new_mat_trainset), replace = TRUE)
mean(guess == ex_new_mat_testset$G3)

# set.seed(3)  
set.seed(3, sample.kind = "Rounding")
# guess with equal probability of pass with ex_bi dataset
guess <- sample(c(0,1), nrow(ex_bi_trainset), replace = TRUE)
guess_results<-mean(guess == ex_bi_testset$G3)
guess_results

# The best result of guessing method was when applying to binary dataset, ex_bi.

# Result Table
results <- tibble(Method = "Model 1: Guessing Model", 
                            Accuracy = guess_results)
results %>% knitr::kable()

# Model 2: Logistic Regression Model 

# Modeling
# Regression_glm
set.seed(3, sample.kind = "Rounding") # set.seed function is to ensure that the samples produced are reproducible
ex_new_mat_log_model<-glm(G3~., data=ex_new_mat_trainset, family=binomial)
ex_new_mat_log_model
summary(ex_new_mat_log_model)

# Prediction
predict_log1<-predict(ex_new_mat_log_model, newdata=ex_new_mat_testset, type="response")
predict_log1<-round(predict_log1)
log_results<-mean(predict_log1==ex_new_mat_testset$G3)
log_results

# Result Table
results <- bind_rows(results, 
                     tibble(Method = "Model 2: Logistic Regression Model", 
                            Accuracy = log_results))
results %>% knitr::kable()

# Model 3: Simplified Logistic Regression Model with Significant Variables
# Modeling
set.seed(3, sample.kind = "Rounding") # set.seed function is to ensure that the samples produced are reproducible
sig_log_model<-glm(G3~failures + sex + address + age + Medu + Fedu+ Mjob + Fjob + schoolsup + paid + higher + romantic + goout + studytime  + traveltime + famsize, data=ex_new_mat_trainset, family=binomial)
sig_log_model

summary(sig_log_model)

# Prediction
predict_log2<-predict(sig_log_model, newdata=ex_new_mat_testset, type="response")
predict_log2<-round(predict_log2)
sig_log_results<-mean(predict_log2==ex_new_mat_testset$G3)
sig_log_results

# Result Table
results <- bind_rows(results, 
                     tibble(Method = "Model 3: Simplified Logistic Regression Model with Significant Variables", 
                            Accuracy = sig_log_results))
results %>% knitr::kable()

# Model 4: Decision Tree
# Visualization
dtree<-rpart(G3~., data=ex_bi_trainset)
fancyRpartPlot(dtree, main="Model 4: Decision Tree", palettes=c("Blues", "Reds"))

# Modeling
# Cross validation of the model. 
# Let's do? cross validation of the model to assess how the results of a statistical analysis will generalize with the optimal cp.
set.seed(3, sample.kind = "Rounding")  #2)
tc <- trainControl(method = "cv",
                   number = 10)
cp_grid <- expand.grid(cp = seq(0, 0.03, 0.001))

dtree_cv <- train(G3~., 
                  na.action=na.omit,
                  data = ex_bi_trainset, 
                  method = "rpart", 
                  trControl = tc,
                  tuneGrid = cp_grid)
dtree_cv

# Prediction
# Test the model in the fnew_mat_testset With the optimal value for the model, cp=0.027.
set.seed(3, sample.kind = "Rounding")
cp_grid <- expand.grid(cp = 0.027)

dtree_cv <- train(G3~.,
                  data = ex_bi_testset, 
                  na.action=na.omit,
                  method = "rpart", 
                  trControl = tc,
                  tuneGrid = cp_grid)
dtree_cv

# Result
# What is the accuracy on the test set using the cross-validated DT model?
dt_pred <- predict(dtree_cv, ex_bi_testset)
dt_results<-mean(dt_pred == ex_bi_testset$G3) # accuracy of the KNN model on the test set
dt_results

# Result Table
results <- bind_rows(results, 
                     tibble(Method = "Model 4: Cross-validated Decison Tree Model", 
                            Accuracy = dt_results))
results %>% knitr::kable()

# 3.5. Model 5: KNN (K-Nearest Neighbors)
# Every student is stored. A new student is comapared to the stored students. Let's find the k most similar students. The most common grade of the k studetns is given to a new student. 
# Modeling
# Let's use caret to train a decision tree with the rpart method.
# The caret package performs cross validation for us and lets us train different algorithms using similart syntax. 

# Predict
#set.seed(6)
set.seed(6, sample.kind = "Rounding") # if using R 3.6 or later
train_knn_cv <- train(G3 ~ .,
                      method = "knn",
                      data = ex_bi_trainset, na.action=na.omit,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),# Try tuning with k=seq(3,51,2).
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))# 10-fold cross validation where each partition consists of 10% of the total. 

# Optimal value of k using cross-validation
train_knn_cv$bestTune # parameter that maximized the estimated accuracy
# The optimal value of k is 35. 
print(train_knn_cv)

# Results
# What is the accuracy on the test set using the cross-validated KNN model?
# Accuracy
knn_cv_pred <- predict(train_knn_cv, ex_bi_testset)
# mean(knn_cv_pred == fnew_mat_test_set$G3)
knn_cv_results<-mean(knn_cv_pred== ex_bi_testset$G3) # accuracy of the KNN model on the test set
knn_cv_results

# Visualization
ggplot(train_knn_cv)

# Results Table
results <- bind_rows(results, 
                     tibble(Method = "Model 5: Cross-validated KNN Model", 
                            Accuracy = knn_cv_results))
results %>% knitr::kable()

# Model 6. Random Forest Model
# Modeling
# Set the seed to 14. Use the caret train() function with the rf method to train a random forest. Test values of mtry = seq(1:7). Set ntree to 100.
# What mtry value maximizes accuracy?
#set.seed(14)  
set.seed(14, sample.kind = "Rounding")    # simulate R 3.5
train_rf <- train(G3 ~ .,
                  data = ex_bi_trainset,
                  na.action=na.omit,
                  method = "rf",
                  ntree = 100,
                  tuneGrid = data.frame(mtry = seq(1:7)))
train_rf$bestTune

# Visualization
# What is the best value for mtry? Lets try to find the optimal value for mtry parameter (Number of variables randomly sampled as candidates at each split).
plot(train_rf)
# From the graph above and from the results, we can conclude that optimal number of variables is 6
print(train_rf)

# Result
# What is the accuracy of the random forest model on the test set? 
rf_preds <- predict(train_rf, ex_bi_testset) 
rf_results<-mean(rf_preds==ex_bi_testset$G3)
rf_results

# Results Table
results <- bind_rows(results, 
                     tibble(Method = "Model 6: Random Forest Model", 
                            Accuracy = rf_results))
results %>% knitr::kable()
 
# What is the most important variable? 
varImp(train_rf)    # first row
# Some important variables in the above table are failures, paid and goout as the same as correlation graphs but we can see slightly different result. For example, we excluded absences because of relatively smaller correlation coefficient but we can see absences variable is one of the important variable in rf model. The most striking feature is romantic is the most important variable in rf model unlike correlation table.  

# 4. Results
# Now, let's see which method has performed the best among guessing, Regression, DT, KNN and RF.
# Show the final result
results 

