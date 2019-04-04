library(caret)
kc_house_data <- read.csv("C:/Users/singh/OneDrive/Desktop/575/Project/kc_house_data.csv")
#View(kc_house_data)
head(kc_house_data)
#============Description of the Data set=================================================================
str(kc_house_data)
summary(kc_house_data)
sapply(kc_house_data,class)

#============Checking for NA values if any =============================================================
ncol(kc_house_data)
na_count <- (sapply(kc_house_data, function(x) sum(is.na(x))))
na_count <-as.data.frame(na_count)
na_count

#===========Excluding the id and date columns ==========================================================
HouseData <- kc_house_data[,3:21]
#HouseData$view<-as.factor(HouseData$view)
#HouseData$waterfront<-as.factor(HouseData$waterfront)
str(HouseData)
unique(HouseData$view)
#View(HouseData)
#Now our primary df is HouseData

#============From summary we see price,bedroom and sqft_lots showing skweness===========================
#============To check the skewness, we can use moment package and outlier will be considered outside (-2,2)===
#install.packages("moments")
library(moments)
apply(HouseData, 2, skewness, na.rm =TRUE)

#============Plot histograms to check for right/left skewness for only the ones that show abnormality==========
#par(mfrow=c(3,3))
p1 <- hist(HouseData$price,col="Green4")
p2 <- hist(HouseData$bedrooms,col="Red3")
p3 <- hist(HouseData$sqft_lot,col="Blue")
p4 <- hist(HouseData$sqft_lot15,col="Black")
p5 <- hist(HouseData$waterfront,col="Coral")
p6 <- hist(HouseData$yr_renovated,col="Pink3")





#===========Checking correlation between variables===========================================================
library(corrplot)
corrplot(cor(HouseData),type="upper",method="square",tl.col = "black",order = "hclust")

#==========Transform Variables ==============================================================================

HouseData$price <- log(HouseData$price)
hist(HouseData$price,col="Green4")

HouseData$sqft_lot <- log(HouseData$sqft_lot)
hist(HouseData$sqft_lot,col="Blue")

HouseData$sqft_lot15 <- log(HouseData$sqft_lot15)
hist(HouseData$sqft_lot15,col="Black")

HouseData$sqft <- HouseData$sqft_living+HouseData$sqft_above
hist(HouseData$sqft,col="Blue4")
HouseData$sqft <- log(HouseData$sqft)

which(HouseData$bedrooms==33)
HouseData <- HouseData[-15871,]
nrow(HouseData)
#================Partitioning the data================================================

set.seed(9876)
index <- sample(2,nrow(HouseData),replace = T,prob = c(0.67,0.34))
trainData <- HouseData[index==1,]
testData <- HouseData[index==2,]
nrow(trainData)
nrow(testData)
head(trainData)
#================ Linear Model ===========================================================

#Linear with all variables : All true, except bedroom
model_lm <- lm(price ~ bedrooms + bathrooms + sqft + sqft_lot 
               + floors + condition + grade + yr_built + zipcode 
               + lat + long + sqft_living15 + sqft_lot15,data = trainData)
summary(model_lm)
prediction_lm <- predict(model_lm, testData,type="response")
model_lm_output <- cbind(testData, prediction_lm)
RMSE_lm <- sqrt(mean((model_lm_output$price-model_lm_output$prediction_lm)^2))
RMSE_lm

#Linear with all the imp variables according to correlation matrix,but bedroom is still abnormal
model_lm <- lm(price ~ sqft+grade+sqft_living15+bedrooms+bathrooms,data = trainData)
summary(model_lm)
prediction_lm <- predict(model_lm, testData,type="response")
model_lm_output <- cbind(testData, prediction_lm)
RMSE_lm <- sqrt(mean((model_lm_output$price-model_lm_output$prediction_lm)^2))
RMSE_lm

#Linear with only bedroom : shows positive relation but RMSE more
model_lm <- lm(price ~ bedrooms,data = trainData)
summary(model_lm)
prediction_lm <- predict(model_lm, testData,type="response")
model_lm_output <- cbind(testData, prediction_lm)
RMSE_lm <- sqrt(mean((model_lm_output$price-model_lm_output$prediction_lm)^2))
RMSE_lm

#Linear with bedroom and sqft/grade/bathrooms
model_lm <- lm(price ~ bedrooms+grade+bathrooms+sqft_living15,data = trainData)
summary(model_lm)
prediction_lm <- predict(model_lm, testData,type="response")
model_lm_output <- cbind(testData, prediction_lm)
RMSE_lm <- sqrt(mean((model_lm_output$price-model_lm_output$prediction_lm)^2))
RMSE_lm

#Linear with all the imp variables according to correlation matrixnleaving bedroom
model_lm <- lm(price ~ sqft+grade+sqft_living15+bathrooms,data = trainData)
summary(model_lm)
prediction_lm <- predict(model_lm, testData,type="response")
model_lm_output <- cbind(testData, prediction_lm)
RMSE_lm <- sqrt(mean((model_lm_output$price-model_lm_output$prediction_lm)^2))
RMSE_lm

#========================Analysis by Decision Tree ================================================================

#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)


HouseData_rpart <- rpart(price ~ bedrooms + bathrooms + sqft + sqft_lot 
                         + floors + condition + grade + yr_built + zipcode 
                         + lat + long + sqft_living15 + sqft_lot15
                         ,data=trainData,parms = list(split="gini"),control = rpart.control(minbucket = 10,cp=cp1, maxdepth = 5))
rpart.plot(HouseData_rpart)
#printcp(HouseData_rpart)
#plotcp(HouseData_rpart)


cp_opt <- function(tree){
  min_split <- which.min(tree$cptable[,"xerror"])
  err_min <- tree$cptable[min_split,"xerror"]
  err_msd <- tree$cptable[min_split,"xstd"]
  err_opt <- err_min + err_msd   #optimal tree is smallest possible with xerror below this value
  
  for(i in 1:nrow(tree$cptable)){
    if(tree$cptable[i,"xerror"]<err_opt)
      return(tree$cptable[i,"CP"])
  }
}

cp1 <- cp_opt(HouseData_rpart)
cp1
HouseData_prune <- prune(HouseData_rpart,cp=cp1)
print(HouseData_prune)

#Test the model on test data
train_pred <-  predict(HouseData_rpart,newdata = trainData)
pred_err_train <- trainData$price - train_pred
pred_sqerr_train <- pred_err_train^2
mse_train <- mean(pred_sqerr_train)
mse_train

test_pred <- predict(HouseData_rpart, newdata = testData)
pred_err_test <- testData$price - test_pred
pred_sqerr_test <- pred_err_test^2
mse_test <- mean(pred_sqerr_test)
mse_test

#Plot Prediction Vs Observed
#install.packages("ggplot2")
library(ggplot2)

test_pred <- round(test_pred,digits = 2)
HouseData_eval <- data.frame(testData$price,test_pred)
colnames(HouseData_eval) <- c("Observed","Predicted")
View(HouseData_eval)
ggplot(data=HouseData_eval,aes(x=HouseData_eval$Predicted , y =HouseData_eval$Observed)) + geom_smooth(method = "lm") +
  geom_point() + ggtitle("Predicted vs Observed values, Method = Decision Tree") +
  labs(x="Predicted Median Values",y="Observed Median Values") + theme(plot.title = element_text(size = 12,hjust = 0.5))

#=======================================Analysis by Random forests ========================================================

#install.packages("randomForest")
library(randomForest)
set.seed(4356)
tree_rf <- price ~ bedrooms + bathrooms + sqft + sqft_lot + floors + condition + grade + yr_built + zipcode + lat + long + sqft_living15 + sqft_lot15
model_rf <- randomForest(tree_rf, data=HouseData, ntree=80,nodesize=10,proximity=T)# change ntree 
#and see how variable importance changes and write your observation
model_rf
plot(model_rf) 

varImpPlot(model_rf)  #For plotting the importance of Variables in the random Forest


#Calculate MSE============================================================================================================
pred_forest <- model_rf$predicted
head(pred_forest)
forest_eval <- cbind(HouseData,pred_forest)
forest_eval <- subset(forest_eval,select=c("price","pred_forest"))
View(forest_eval)

pred_err_frst <- forest_eval$price - forest_eval$pred_forest
pred_sqerr_frst <- pred_err_frst^2
mse_frst <- mean(pred_sqerr_frst)
mse_frst

#Plot Predictions vs Observations========================================================================================
colnames(forest_eval) <- c("Observed","Predicted")
ggplot(data=forest_eval,aes(x=forest_eval$Predicted , y =forest_eval$Observed)) + geom_smooth(method = "lm") +
  geom_point() + ggtitle("Predicted vs Observed values, Method = Random Forest") +
  labs(x="Predicted Median Values",y="Observed Median Values") + theme(plot.title = element_text(size = 12,hjust = 0.5))

#============PCA:particularly helpful in the case of "wide" datasets,where you have many variables for each sample======================================================================================
#Can enable us to identify groups of samples that are similar and work out which variables make one group different from another===

#We are practicing an unsupervised learning technique, hence response variable must be removed.

pca_data <- HouseData[c(-1,-4,-11)]
View(pca_data)
#Partition the data

set.seed(9876)
index <- sample(2,nrow(pca_data),replace = T,prob = c(0.7,0.3))
trainpca <- pca_data[index==1,]
testpca <- pca_data[index==2,]
nrow(trainpca)
nrow(testpca)

install.packages("factoextra")
library(factoextra)
res.pca <- prcomp(trainpca, scale = TRUE)
fviz_eig(res.pca)
#Graph of variables. Positive correlated variables point to the same side of the plot.
#Negative correlated variables point to opposite sides of the graph.
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}
# Compute Coordinates
#::::::::::::::::::::::::::::::::::::::::
loadings <- res.pca$rotation
sdev <- res.pca$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev)) 
head(var.coord[, 1:4])

# Compute Cos2
#::::::::::::::::::::::::::::::::::::::::
var.cos2 <- var.coord^2
head(var.cos2[, 1:4])

# Compute contributions: Contribution of a variable to the component
#::::::::::::::::::::::::::::::::::::::::
comp.cos2 <- apply(var.cos2, 2, sum)
contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2))
head(var.contrib[, 1:4])

#==============================Support Vector Regression=============================================
install.packages("e1071")
library(e1071)
model_svr <- svm(price ~ bedrooms+grade+bathrooms+sqft_living15,data = trainData,kernel="linear")
summary(model_svr)



