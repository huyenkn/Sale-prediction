library(data.table) # used for reading and manipulation of data
library(dplyr, warn.conflicts = FALSE)      # used for data manipulation and joining 
library(ggplot2)    # used for ploting
library(caret)      # used for modeling
library(corrplot)   # used for making correlation plot
library(xgboost, warn.conflicts = FALSE)    # used for building XGBoost model
library(cowplot, warn.conflicts = FALSE)    # used for combining multiple plots 
library(lattice)
suppressPackageStartupMessages(library(ggplot2))
theme_set(theme_cowplot())
#options(device="quartz")
# quartz()
#rm(test_set, train_set, train, myfile) delete data in global environment
train = fread("/Users/khanhhuyen4523/Desktop/Data_science/sale-prediction/train_set.csv")
test = fread("/Users/khanhhuyen4523/Desktop/Data_science/sale-prediction/test_set.csv")
submission = fread("/Users/khanhhuyen4523/Desktop/Data_science/sale-prediction/sample_submission.csv") # fread() function of data.table package to read the datasets.
dim(train)
dim(test)
names(train)
names(test)
str(train)
A = test[,Item_Outlet_Sales := NA] 
combi = rbind(train, test) # combining train and test datasets 
# dim(combi)

p = ggplot(train) + 
	geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill = "darkgreen") +  
	xlab("Item_Outlet_Sales")
#p = ggplot(train) + 
#  geom_histogram(aes(Item_Outlet_Sales), binwidth = 100, fill = "darkgreen"): same above result
# ggsave("myplot.pdf") : important -- save plot in a pdf file.
p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue")
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue") 
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue") 
plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +   
geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")
###iris %>% head() %>% summary() is equivalent to summary(head(iris))
#stat = "identity", default: stat_count() must not be used with a y aesthetic,
#which makes the height of the bar proportion to the number of cases in each group,
#we don't need it now.

combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat" 
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat" 
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular" 

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +   
geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")


# plot for Item_Type 
#theme: rotate label to read easily.
#geom_label(label = Count): show number counted in each column.
#ggtitle: add title at the top
#vjust, hjust: not important

p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) +   
	geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +  xlab("") +  
	geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +  
	theme(axis.text.x = element_text(angle = 45, hjust = 1))+  
	ggtitle("Item_Type")

# plot for Outlet_Identifier 
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) +   
	geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +  
	geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +  
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot for Outlet_Size 
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) +   
	geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +  
	geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +  
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

#this is how you present 3 plots in one shot --> good one!
second_row = plot_grid(p5, p6, nrow = 1) 
plot_grid(p4, second_row, ncol = 1)

# plot for Outlet_Establishment_Year 
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) +   
	geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +  
	geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +  
	xlab("Outlet_Establishment_Year") +  theme(axis.text.x = element_text(size = 8.5))


# plot for Outlet_Type 
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) +   
	geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +  
	geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +  
	theme(axis.text.x = element_text(size = 8.5))

# ploting both plots together 
plot_grid(p7, p8, ncol = 2)


# extracting train data from the combined data
train = combi[1:nrow(train)] 

# Item_Weight vs Item_Outlet_Sales 
p9 = ggplot(train) +      
	geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +     
	theme(axis.title = element_text(size = 8.5))
#alpha = 0.3: make the plot blur... to show the density the data

# Item_Visibility vs Item_Outlet_Sales 
p10 = ggplot(train) +       
	geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +      
	theme(axis.title = element_text(size = 8.5))

# Item_MRP vs Item_Outlet_Sales 
p11 = ggplot(train) +       
	geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +      
	theme(axis.title = element_text(size = 8.5))

second_row_2 = plot_grid(p10, p11, ncol = 2) 
plot_grid(p9, second_row_2, nrow = 2)

# Item_Type vs Item_Outlet_Sales 
p12 = ggplot(train) +       
	geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +      
	theme(axis.text.x = element_text(angle = 45, hjust = 1), 
	axis.text = element_text(size = 6), axis.title = element_text(size = 8.5))

# Item_Fat_Content vs Item_Outlet_Sales 
p13 = ggplot(train) +       
	geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +      
	theme(axis.text.x = element_text(angle = 45, hjust = 1), 
	axis.text = element_text(size = 8), axis.title = element_text(size = 8.5))

# Outlet_Identifier vs Item_Outlet_Sales 
p14 = ggplot(train) +       
	geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +      
	theme(axis.text.x = element_text(angle = 45, hjust = 1),            
	axis.text = element_text(size = 8), axis.title = element_text(size = 8.5))

second_row_3 = plot_grid(p13, p14, ncol = 2) 
plot_grid(p12, second_row_3, ncol = 1)

ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")

p15 = ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta") 
p16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta") 
plot_grid(p15, p16, ncol = 1)

sum(is.na(combi$Item_Weight))

missing_index = which(is.na(combi$Item_Weight)) 
#which: make a list of all rows that have missing Item_Weight values
for(i in missing_index){
item = combi$Item_Identifier[i] 
combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T)
}
#in the Item_weight column, find all values that have Item_identifier == item (have the same identifier), 
#then calculate the mean of all values, and take the mean to replace all NA values)

sum(is.na(combi$Item_Weight))

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

zero_index = which(combi$Item_Visibility == 0) 
#length(which(combi$Item_Visibility == 0))
#897
#length(zero_index)

for(i in zero_index){
item = combi$Item_Identifier[i] 
combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)  
}

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

#Feature engineering
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", 
                   "Health and Hygiene", "Household", "Soft Drinks")

# create a new feature 'Item_Type_new' 
combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable", 
                        ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]
#1:10 %in% 3:7
#[1] FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE

table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))
combi[,Item_Category := substr(combi$Item_Identifier, 1, 2)]

sum(is.na(combi$Item_Fat_Content))
combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible" 

# years of operation for outlets 
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year] 
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year)
#combi$Outlet_Years <- NULL ->delete one column

# Price per unit weight 
combi[,price_per_unit_wt := Item_MRP/Item_Weight]

# creating new independent variable - Item_MRP_clusters 
combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st",  
                            ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",
                            ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]

combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0,
                          ifelse(Outlet_Size == "Medium", 1, 2))]

combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,
                                   ifelse(Outlet_Location_Type == "Tier 2", 1, 2))]

#combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL] : delete 2 columns

#One hot encoding for the categorical variable
ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T)
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))
combi = cbind(combi[,"Item_Identifier"], ohe_df)
#cbind: combine 2 datasets

#Data preprocessing
#Removing Skewness
C = combi[,Item_Visibility := log(Item_Visibility + 1)] # log + 1 to avoid division by zero 
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]

ggplot(combi) + geom_histogram(aes(C$Item_Visibility), binwidth = 0.005, fill = "blue") 

#Scaling numeric predictors
num_vars = which(sapply(combi, is.numeric)) # index of numeric features
num_vars_names = names(num_vars) 
#extract all numeric feature names of combi
combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F] 
#get all numeric features of combi except Item_Outlet_Sales
prep_num = preProcess(combi_numeric, method=c("center", "scale"))
combi_numeric_norm = predict(prep_num, combi_numeric)

combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables 
#In the code, the numeric variables are removed from the dataset 
#and scaled version of those numeric variables are appended to the same dataset.
combi = cbind(combi, combi_numeric_norm)

#Splitting the combined data combi back to train and test set.
train = combi[1:nrow(train)]
test = combi[(nrow(train) + 1):nrow(combi)] 

#Correlated Variables
#It is not desirable to have correlated features if we are using linear regressions.

cor_train = cor(train[,-c("Item_Identifier")])
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)

#Linear regression
linear_reg_mod = lm(Item_Outlet_Sales ~ ., data = train[,-c("Item_Identifier")])

# preparing dataframe for submission and writing it in a csv file 
submission$Item_Outlet_Sales = predict(linear_reg_mod, test[,-c("Item_Identifier")]) 
write.csv(submission, "Linear_Reg_submit1.csv", row.names = F)
# Leaderboard score: 1202.33

#Regularised regression models 
set.seed(1235)
my_control = trainControl(method="cv", number=5)
Grid = expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0002)) 
lasso_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], 
                             y = train$Item_Outlet_Sales, method='glmnet', 
                             trControl=my_control, tuneGrid = Grid)
submission$Item_Outlet_Sales = predict(lasso_linear_reg_mod, test[,-c("Item_Identifier")]) 
write.csv(submission, "lasso_linear_reg_submit.csv", row.names = F)

#Ridge Regression
set.seed(1236) 
my_control = trainControl(method="cv", number=5) 
Grid = expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0002)) 
ridge_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales,                       
                             method='glmnet', trControl= my_control, tuneGrid = Grid)
submission$Item_Outlet_Sales = predict(ridge_linear_reg_mod, test[,-c("Item_Identifier")]) 
write.csv(submission, "ridge_linear_reg_submit.csv", row.names = F)
#Leaderboard score: 1205.04
#RMSE: 1133.026

#Random forest:
set.seed(1237) 
my_control = trainControl(method="cv", number=5) # 5-fold CV 
tgrid = expand.grid(
  .mtry = c(3:10),
  .splitrule = "variance",
  .min.node.size = c(10,15,20))
rf_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
               y = train$Item_Outlet_Sales,
               method='ranger',
               trControl= my_control,
               tuneGrid = tgrid,
               num.trees = 400,
               importance = "permutation")

submission$Item_Outlet_Sales = predict(rf_mod, test[,-c("Item_Identifier")]) 
write.csv(submission, "randomforest_submit.csv", row.names = F)
#Leaderboard score: 1158.01
#RMSE: 1086.375
plot(rf_mod)
#Tuning parameter 'splitrule' was held constant at a value of variance
#RMSE was used to select the optimal model using the smallest value.
#The final values used for the model were mtry = 6, splitrule = variance
#and min.node.size = 20.

plot(varImp(rf_mod))

#XGBoost
param_list = list(objective = "reg:linear", eta=0.01, gamma = 1,
                  max_depth=6,subsample=0.8,colsample_bytree=0.5)

dtrain = xgb.DMatrix(data = as.matrix(train[,-c("Item_Identifier", "Item_Outlet_Sales")]), 
                     label= train$Item_Outlet_Sales) 
dtest = xgb.DMatrix(data = as.matrix(test[,-c("Item_Identifier")]))

set.seed(112) 
xgbcv = xgb.cv(params = param_list, data = dtrain,
                             nrounds = 1000, nfold = 5, print_every_n = 10, 
                             early_stopping_rounds = 30, maximize = F)

#As per the verbose above, we got the best validation/test score at the 418th iteration. 
#Hence, we will use nrounds = 418 for building the XGBoost model.

xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 418)

submission$Item_Outlet_Sales = predict(xgb_model, test[,-c("Item_Identifier")]) 
write.csv(submission, "XGboost_submit.csv", row.names = F)
#Leaderboard: 1153.35

#Variable Importance
var_imp = xgb.importance(feature_names = setdiff(names(train), 
                       c("Item_Identifier", "Item_Outlet_Sales")), model = xgb_model)

xgb.plot.importance(var_imp)


