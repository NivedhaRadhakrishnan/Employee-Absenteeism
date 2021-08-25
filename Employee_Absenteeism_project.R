########################Employee Absenteeism R Code#############################
#Cleaning the environment
rm(list = ls())

#Setting the working directory
setwd("")

#Load libraries
libraries = c("psych","ggpubr","corrplot","usdm","caret","rpart","randomForest","rpart.plot")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

##Importing the csv file
#emp_abs_data_complete = read.csv(file = "Absenteeism_dataset.csv", header = T)
#Storing only predictor data
#emp_abs_data = emp_abs_data_complete[1:20]
emp_abs_data= read.csv(file = "Absenteeism_dataset.csv", header = T)

########################Exploratory data analysis###############################
#Observing sample data
head(emp_abs_data)

#Observing structure of the data
str(emp_abs_data)
#dimnames(emp_abs_data)

##Sorting the variables into numerical and categorical
numeric_all_variable_set = c("Transportation.expense","Distance.from.Residence.to.Work","Service.time","Age","Work.load.Average.day","Hit.target","Weight","Height","Body.mass.index","Absenteeism.time.in.hours")
categorical_pred_variable_set = c("ID","Reason.for.absence","Month.of.absence","Day.of.the.week","Seasons","Disciplinary.failure","Education","Son","Social.drinker","Social.smoker","Pet")
numeric_pred_variable_set = c("Transportation.expense","Distance.from.Residence.to.Work","Service.time","Age","Work.load.Average.day","Hit.target","Weight","Height","Body.mass.index")

#Parsing the datatype of categorical variables and assigning levels
for(i in categorical_pred_variable_set){
  emp_abs_data[,i] = as.factor(emp_abs_data[,i])
}

#verifying the structure of the categorical variables
str(emp_abs_data)

#Storing the numeric and categorical data separately
#Get the data with only numeric columns
numeric_index = sapply(emp_abs_data, is.numeric)
numeric_data = emp_abs_data[,numeric_index]

#Get the data with only factor columns
factor_data = emp_abs_data[,!numeric_index]

##Copy the dataset for reference/comparison
#df = emp_abs_data

##########################Missing value Analysis################################

###------------------------Observation---------------------------------------###
#Count of missing values in each variable
missing_values_df = data.frame(apply(emp_abs_data,2,function(x){sum(is.na(x))}))

#Creating a new column for missing value percentage
names(missing_values_df)[1] = "NA_count"
missing_values_df$NA_Percent = (missing_values_df$NA_count/nrow(emp_abs_data))*100

#Sorting missing values in decreasing order
missing_values_df = missing_values_df[order(-missing_values_df$NA_Percent),]

###------------------------Imputation----------------------------------------###
##Imputing 0s with NA for variables
emp_abs_data$Reason.for.absence[emp_abs_data$Reason.for.absence==0] = NA
emp_abs_data$Month.of.absence[emp_abs_data$Month.of.absence==0] = NA
emp_abs_data$Day.of.the.week[emp_abs_data$Day.of.the.week==0] = NA
emp_abs_data$Seasons[emp_abs_data$Seasons==0] = NA
emp_abs_data$Education[emp_abs_data$Education==0] = NA
emp_abs_data$ID[emp_abs_data$ID==0] = NA
emp_abs_data$Age[emp_abs_data$Age==0] = NA
emp_abs_data$Weight[emp_abs_data$Weight==0] = NA
emp_abs_data$Height[emp_abs_data$Height==0] = NA
emp_abs_data$Body.mass.index[emp_abs_data$Body.mass.index==0] = NA

##Imputing missing values with default values
#Reason for absence
#emp_abs_data$Reason.for.absence[emp_abs_data$Reason.for.absence==0] = 26
emp_abs_data$Reason.for.absence[is.na(emp_abs_data$Reason.for.absence)] = 27

#Parsing the column Work load average day into categorical value
emp_abs_data$Work.load.Average.day  = as.numeric(as.factor(as.character(emp_abs_data$Work.load.Average.day )))
##Imputing NA values with id wise mean of the column values for numeric columns
#Transportation expense
x = emp_abs_data$ID[is.na(emp_abs_data$Transportation.expense)]
for (i in x){
  emp_abs_data$Transportation.expense[is.na(emp_abs_data$Transportation.expense) & emp_abs_data$ID==i] = mean(emp_abs_data$Transportation.expense[emp_abs_data$ID==i],na.rm = T)
}

#Distance from Residence to Work
x = emp_abs_data$ID[is.na(emp_abs_data$Distance.from.Residence.to.Work)]
for (i in x){
  emp_abs_data$Distance.from.Residence.to.Work[is.na(emp_abs_data$Distance.from.Residence.to.Work) & emp_abs_data$ID==i] = mean(emp_abs_data$Distance.from.Residence.to.Work[emp_abs_data$ID==i],na.rm = T)
}

#Service time
x = emp_abs_data$ID[is.na(emp_abs_data$Service.time)]
for (i in x){
  emp_abs_data$Service.time[is.na(emp_abs_data$Service.time) & emp_abs_data$ID==i] = mean(emp_abs_data$Service.time[emp_abs_data$ID==i],na.rm = T)
}

#Age
x = emp_abs_data$ID[is.na(emp_abs_data$Age)]
for (i in x){
  emp_abs_data$Age[is.na(emp_abs_data$Age) & emp_abs_data$ID==i] = mean(emp_abs_data$Age[emp_abs_data$ID==i],na.rm = T)
}

#Weight
x = emp_abs_data$ID[is.na(emp_abs_data$Weight)]
for (i in x){
  emp_abs_data$Weight[is.na(emp_abs_data$Weight) & emp_abs_data$ID==i] = mean(emp_abs_data$Weight[emp_abs_data$ID==i],na.rm = T)
}

#Height
x = emp_abs_data$ID[is.na(emp_abs_data$Height)]
for (i in x){
  emp_abs_data$Height[is.na(emp_abs_data$Height) & emp_abs_data$ID==i] = mean(emp_abs_data$Height[emp_abs_data$ID==i],na.rm = T)
}

##Imputing NA values with id wise mode of the column values for categorical columns
#Defining mode function
getmode = function(x){
  unique_x = unique(x)
  mode_val = which.max(tabulate(match(x,unique(x))))
}

#Reason for absence
x = emp_abs_data$ID[is.na(emp_abs_data$Reason.for.absence)]
for (i in x){
  emp_abs_data$Reason.for.absence[is.na(emp_abs_data$Reason.for.absence) & emp_abs_data$ID==i] = getmode(emp_abs_data$Reason.for.absence[emp_abs_data$ID==i])
}

#Education
x = emp_abs_data$ID[is.na(emp_abs_data$Education)]
for (i in x){
  emp_abs_data$Education[is.na(emp_abs_data$Education) & emp_abs_data$ID==i] = getmode(emp_abs_data$Education[emp_abs_data$ID==i])
}

#Son
x = emp_abs_data$ID[is.na(emp_abs_data$Son)]
for (i in x){
  emp_abs_data$Son[is.na(emp_abs_data$Son) & emp_abs_data$ID==i] = getmode(emp_abs_data$Son[emp_abs_data$ID==i])
}

#Social drinker
x = emp_abs_data$ID[is.na(emp_abs_data$Social.drinker)]
for (i in x){
  emp_abs_data$Social.drinker[is.na(emp_abs_data$Social.drinker) & emp_abs_data$ID==i] = getmode(emp_abs_data$Social.drinker[emp_abs_data$ID==i])
}

#Social smoker
x = emp_abs_data$ID[is.na(emp_abs_data$Social.smoker)]
for (i in x){
  emp_abs_data$Social.smoker[is.na(emp_abs_data$Social.smoker) & emp_abs_data$ID==i] = getmode(emp_abs_data$Social.smoker[emp_abs_data$ID==i])
}

#Pet
x = emp_abs_data$ID[is.na(emp_abs_data$Pet)]
for (i in x){
  emp_abs_data$Pet[is.na(emp_abs_data$Pet) & emp_abs_data$ID==i] = getmode(emp_abs_data$Pet[emp_abs_data$ID==i])
}

##Imputing NA BMI values using height and weight
#Body mass index
x = emp_abs_data$ID[is.na(emp_abs_data$Body.mass.index)]
for (i in x){
  emp_abs_data$Body.mass.index[is.na(emp_abs_data$Body.mass.index) & emp_abs_data$ID==i] = round(((emp_abs_data$Weight[emp_abs_data$ID==i])*10000)/((emp_abs_data$Height[emp_abs_data$ID==i])**2))
}


#Imputing the remaining values using KNNimputation
# library(DMwR2)
# emp_abs_data = knnImputation(emp_abs_data, k = 10)
#emp_abs_data$Absenteeism.time.in.hours[1]


##Imputing remaining NA values with median for numerical data and mode for categorical data
#Numerical
#Absenteeism.time.in.hours
x = emp_abs_data$ID[is.na(emp_abs_data$Absenteeism.time.in.hours)]
for (i in x){
  emp_abs_data$Absenteeism.time.in.hours[is.na(emp_abs_data$Absenteeism.time.in.hours) & emp_abs_data$ID==i] = median(emp_abs_data$Absenteeism.time.in.hours,na.rm = T)
}
#Hit.target
x = emp_abs_data$ID[is.na(emp_abs_data$Hit.target)]
for (i in x){
  emp_abs_data$Hit.target[is.na(emp_abs_data$Hit.target) & emp_abs_data$ID==i] = median(emp_abs_data$Hit.target,na.rm = T)
}

#Categorical
#Disciplinary.failure
x = emp_abs_data$ID[is.na(emp_abs_data$Disciplinary.failure)]
for (i in x){
  emp_abs_data$Disciplinary.failure[is.na(emp_abs_data$Disciplinary.failure) & emp_abs_data$ID==i] = getmode(emp_abs_data$Disciplinary.failure)
}

#Reason.for.absence
x = emp_abs_data$ID[is.na(emp_abs_data$Reason.for.absence)]
for (i in x){
  emp_abs_data$Reason.for.absence[is.na(emp_abs_data$Reason.for.absence) & emp_abs_data$ID==i] = getmode(emp_abs_data$Reason.for.absence)
}

#Month.of.absence
x = emp_abs_data$ID[is.na(emp_abs_data$Month.of.absence)]
for (i in x){
  emp_abs_data$Month.of.absence[is.na(emp_abs_data$Month.of.absence) & emp_abs_data$ID==i] = getmode(emp_abs_data$Month.of.absence)
}

#Checking for missing values
sum(is.na(emp_abs_data))

##########################Analysis of data distribution using histogram#########
#Storing the numeric and categorical data separately
#Get the data with only numeric columns
numeric_index = sapply(emp_abs_data, is.numeric)
numeric_data = emp_abs_data[,numeric_index]

#Get the data with only factor columns
factor_data = emp_abs_data[,!numeric_index]
#Multiple histograms for numerical predictors
emp_data_plot_hist = numeric_data[1:9]
multi.hist(emp_data_plot_hist,dcol= c("blue","red"),dlty=c("solid", "solid"),bcol="linen")

#Multiple barplot
#Bar plot for categorically predictors
emp_data_plot_bar = factor_data[1:11]
gplot1 = ggplot(emp_data_plot_bar, aes(x = ID ) )+ geom_bar()
gplot2 = ggplot(emp_data_plot_bar, aes(x = Reason.for.absence ) )+ geom_bar()
gplot3 = ggplot(emp_data_plot_bar, aes(x = Month.of.absence ) )+ geom_bar()
gplot4 = ggplot(emp_data_plot_bar, aes(x = Day.of.the.week ) )+ geom_bar()
gplot5 = ggplot(emp_data_plot_bar, aes(x = Seasons ) )+ geom_bar()
gplot6 = ggplot(emp_data_plot_bar, aes(x = Disciplinary.failure ) )+ geom_bar()
gplot7 = ggplot(emp_data_plot_bar, aes(x = Education ) )+ geom_bar()
gplot8 = ggplot(emp_data_plot_bar, aes(x = Son ) )+ geom_bar()
gplot9 = ggplot(emp_data_plot_bar, aes(x = Social.drinker ) )+ geom_bar()
gplot10 = ggplot(emp_data_plot_bar, aes(x = Social.smoker ) )+ geom_bar()
gplot11 = ggplot(emp_data_plot_bar, aes(x = Pet ) )+ geom_bar()
ggarrange(gplot1,gplot2,gplot3,gplot4,gplot5,gplot6,gplot7,gplot8,gplot8,gplot9,gplot10,gplot11)

##########################Outlier Analysis######################################
#Outlier boxplot
for(i in 1:ncol(numeric_data)) {
  assign(paste0("box_plot",i), ggplot(data = emp_abs_data, aes_string(y = numeric_data[,i])) +
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour = "red", fill = "blue", outlier.size = 1) +
           labs(y = colnames(numeric_data[i])) +
           ggtitle(paste("Boxplot of: ",colnames(numeric_data[i]))))
}

#Arrange the plots in grids
gridExtra::grid.arrange(box_plot1,box_plot2,box_plot3,box_plot4,box_plot5,
                        box_plot6,box_plot7,box_plot8,box_plot9,box_plot10,ncol=5)

#Replacing all outliers with NA
for(i in numeric_pred_variable_set){
  val1 = emp_abs_data[,i][emp_abs_data[,i] %in% boxplot.stats(emp_abs_data[,i])$out]
  print(paste(i,length(val1)))
  emp_abs_data[,i][emp_abs_data[,i] %in% val1] = NA
}

#Checking for missing values
sum(is.na(emp_abs_data))

#Imputing data in columns with NA by median method
impute_median_mode = function(data_set){
  for(i in colnames(data_set)){
    if(sum(is.na(data_set[,i]))!=0){
      if(is.numeric(data_set[,i])){
        
        data_set[is.na(data_set[,i]),i] = median(data_set[,i],na.rm = TRUE)
      }
      
      
      else if(is.factor(data_set[,i])){
        
        data_set[is.na(data_set[,i]),i] = getmode(data_set[,i])
      }
      
    }
  }
  #print(data_set)
  return(data_set)
}
emp_abs_data = impute_median_mode(emp_abs_data)
#Checking for missing values
sum(is.na(emp_abs_data))

#########################Feature Selection######################################
correlation_matrix = cor(emp_abs_data[,numeric_pred_variable_set])
# dev.off()
corrplot(correlation_matrix,method = "number",type = 'lower')

#Checking multi-collinearity
#Using VIF technique for numerical data
vif(emp_abs_data[,numeric_pred_variable_set])
#Using ANOVA technique for categorical data
for(i in categorical_pred_variable_set){
  print(i)
  aov_summary = summary(aov(Absenteeism.time.in.hours~emp_abs_data[,i],data = emp_abs_data))
  print(aov_summary)
}

#Dimentionality reduction
emp_abs_data_all_columns = emp_abs_data
#From the observations obtained from from VIF and ANOVA test,we derive the following inference
#From VIF both weight and body mass index are above 5. Either of the predictor can be removed
#From ANOVA the following columns had p-value greater than the level of significance(ie.,0.05) and hence can be removed
emp_abs_data = subset(emp_abs_data, select = -(which(names(emp_abs_data) %in% c("Weight","Day.of.the.week","Seasons","Education","Social.smoker","Social.drinker"))))

###########################Feature Sampling#####################################
variable_set = c("Transportation.expense","Distance.from.Residence.to.Work","Service.time","Age","Work.load.Average.day","Hit.target","Height","Body.mass.index","Absenteeism.time.in.hours","ID","Reason.for.absence","Month.of.absence","Disciplinary.failure","Son","Pet")
#Parsing all the columns to numeric value
for(i in variable_set){
  emp_abs_data[,i] = as.numeric(emp_abs_data[,i])
}
#Separating dataset into test and train set
set.seed(123)
#Splitting the train and test set in 4:1 ratio (ie., 80% training data and 20% test data)
train_index = sample(1:nrow(emp_abs_data), 0.8*nrow(emp_abs_data))        
train = emp_abs_data[train_index,]
test = emp_abs_data[-train_index,]

############################## Model Development  ##############################

###----------------Multiple linear regression--------------------------------###
# RMSE    Rsquared         MAE 
# 15.79761152  0.01206002 14.8248566614 
#Train the model using training data
# Fitting Multiple Linear Regression to the Training set
regressor_mlr = lm(formula = Work.load.Average.day ~ .,data = train)
# Predicting the Test set results
y_pred_mlr = predict(regressor_mlr, newdata = test)
#Get the summary of the model
summary(regressor_mlr)
#Create dataframe for actual and predicted values
model_pred = data.frame("actual"=test, "model_pred"=y_pred_mlr)
head(model_pred)
#Calcuate MAE, RMSE, R-squared for testing data 
print(postResample(pred = y_pred_mlr, obs =test$Absenteeism.time.in.hours))
#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="red")
lines(y_pred_mlr,col="blue")

###--------------------Decision Tree-----------------------------------------###
# RMSE    Rsquared         MAE 
# 10.90598041  0.05805556  5.61130582 
#Build decsion tree using rpart
regressor_dt = rpart(Absenteeism.time.in.hours ~., data = train, method = "anova")

#Predict the test cases
y_pred_dt = predict(regressor_dt, newdata = test)

#Create data frame for actual and predicted values
model_pred = cbind(model_pred,y_pred_dt)
head(model_pred)

#Get the summary of the model
summary(regressor_dt)

#Calcuate MAE, RMSE, R-squared for testing data 
print(postResample(pred = y_pred_dt, obs = test$Absenteeism.time.in.hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="red")
lines(y_pred_dt,col="blue")

# Visualize the decision tree with rpart.plot
rpart.plot(regressor_dt, box.palette="RdBu", shadow.col="gray", nn=TRUE)

###--------------------Random forest-----------------------------------------###
# RMSE   Rsquared        MAE 
#9.97901412 0.06205605 5.10285898  
#Train the model using training data
regressor_rf = randomForest(Absenteeism.time.in.hours~., data = train, ntrees = 500)

#Predict the test cases
y_pred_rf = predict(regressor_rf,test)

#Create dataframe for actual and predicted values
model_pred = cbind(model_pred,y_pred_rf)
head(model_pred)

#Get the summary of the model
summary(regressor_rf)

#Calcuate MAE, RMSE, R-squared for testing data 
print(postResample(pred = y_pred_rf, obs = test$Absenteeism.time.in.hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="red")
lines(y_pred_rf,col="blue")

