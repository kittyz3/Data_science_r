# import necessary libraries 
library(pROC)
library(MASS)
#import dataset 
breast_cancer <- read.table("breast-cancer-wisconsin.data", sep = ",")

# Understand overall structure of the dataset 
str(breast_cancer)
head(breast_cancer)

# assign column names to each of the factors
colnames(breast_cancer) <- c("code","clump","cell_size","cell_shape", "adhesion","epithelial","bare_nuclei","chromatin","nucleoli","mitosis","class")

# Find out which data points represent benign/malignant tumors and assign binary labels  
benign_ind <- which(breast_cancer$class==2)
malig_ind <- which(breast_cancer$class==4)
breast_cancer$class[benign_ind]=0
breast_cancer$class[malig_ind]=1

# Clean up the dataset
breast_cancer$bare_nuclei <- gsub("?",NA,breast_cancer$bare_nuclei,fixed=TRUE)
breast_cancer$bare_nuclei <- as.numeric(breast_cancer$bare_nuclei)
breast_cancer$class <- as.factor(breast_cancer$class)

# Use resampling methods (i.e. validation set approach) to split the dataset into training and testing sets 
set.seed(1)
train = sample(699,349)

# Do logistic regression on the training dataset 
glm.fit <- glm(class ~., data = breast_cancer,family = binomial,subset = train)
breast_cancer.test =breast_cancer[-train,]
summary(glm.fit)
coef(glm.fit)

# predict cancer diagnosis using model built from training data and assess performance
glm.pred <- predict(glm.fit,breast_cancer.test,type = "response") 
glm.pred[1:10]
contrasts(breast_cancer$class)
glm.predi = rep("malignant",350)
glm.predi[glm.pred<.5]="benign"
table(glm.predi,breast_cancer.test$class)
perform <- (215+115)/(215+115+9+11) #model correctly classified 94% of diagnosis

# Use LDA to perform classification on the dataset and only use factors that had significant p-values in 
# previous logistic regression model
lda.fit = lda(class~clump+cell_shape+bare_nuclei+chromatin, data = breast_cancer,subset = train)
lda.fit 
lda.pred=predict(lda.fit,breast_cancer.test)
table(lda.pred$class,breast_cancer.test$class)
lda_perform=(219+111)/(219+111+2+12) #model correctly classified 95% of diagnosis

# plot roc curves and visualize classifier performance for both LR and LDA model 
plot(roc(breast_cancer.test$class,glm.pred)) #ROC curve for lR model
plot(roc(breast_cancer.test$class,as.numeric(lda.pred$class))) #ROC curve for LDA model
