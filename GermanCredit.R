# Inteligent Systems: German Credit Homewok.
#
# Authors:  Miguel Avina
#           Arturo Perez
# Date:     05/11/2020
# -----------------------------------------------------------------------------


credit <- read.csv(file = "data/german.csv", header = FALSE, sep = ' ', 
                 col.names = c('CheckAcc', 'Duration', 'CreditHist', 'Purpose', 
                               'Amount', 'Savings', 'YearsEmploy', 'InstallRate',
                               'Sex', 'Debtors', 'Residence', 'Property', 'Age',
                               'OtherInstallments', 'Housing', 'ExistingCredits',
                               'Job', 'Dependents', 'Telephone', 'Foreign', 
                               'Result'))


library(dplyr)

credit$Result <- as.factor(credit$Result)

credit.numeric <- dplyr::select_if(credit, is.numeric)
credit.factors <- dplyr::select_if(credit, is.factor)

# credit.y <- credit.numeric$Result # Removing the Y variable
# credit.numeric <- select(credit.numeric, -Result)

# Preprocessing. 

# A partir del conjunto de datos inicial, realiza un preprocesamiento de los
# datos categóricos: verifica que cada variable tiene al menos el 10% de datos
# en cada uno de sus niveles, de no ser así, agrúpalos de la mejor manera
# para que se cumpla dicho criterio. Finalmente, asigna los nombres a la
# variable y los niveles resultantes, siguiendo la información de la página de
# la UCI.

# Combining levels of factors to get at least 10% of observations in each one
prop.table(table(credit.factors$CheckAcc))  # Do not need modification
credit.factors$CheckAcc <- factor(credit.factors$CheckAcc,
                             labels = c("Zero", "Less200", "Over200", "NoAccount"))

prop.table(table(credit.factors$CreditHist)) 
# Combining critical accounts+Delay in paying & no credits+all paid back
credit.factors$CreditHist <- factor(credit.factors$CreditHist, 
                                    labels=c("Good","Good","Good","Critical","Critical"))
prop.table(table(credit.factors$CreditHist)) 

prop.table(table(credit.factors$Purpose))  
# Here we will combine business and education as they can be taken as 'Invest'
# The rest of leves are combined under the 'Other' level
credit.factors$Purpose <- factor(credit.factors$Purpose, 
                                 labels = c("CarNew", "CarOld", "Other", "Other",
                                            "RadioTV", "Other", "Other", "Invest",
                                            "Other", "Invest"))
prop.table(table(credit.factors$Purpose))  

prop.table(table(credit.factors$Savings))
# Here, we combined savings above 500 in a single level
credit.factors$Savings <- factor(credit.factors$Savings,
                                 labels = c("Less100", "100-500", "Over500", "Over500", "Unknown"))
prop.table(table(credit.factors$Savings))

prop.table(table(credit.factors$YearsEmploy))
# Altough the unemployed level has 6.2%, it would not be logic to combine it to other level
# We will keep it this way meanwhile
credit.factors$YearsEmploy <- factor(credit.factors$YearsEmploy,
                                     labels = c("NoJob", "Less1Y", "1-4Y", "4-7Y", "Over7Y"))

prop.table(table(credit.factors$Sex))
# Combined male divorced/separated+male married/widowed
credit.factors$Sex <- factor(credit.factors$Sex,
                             labels = c("Male", "Female", "MaleSingle", "Male"))
prop.table(table(credit.factors$Sex))

prop.table(table(credit.factors$Debtors))
# Made 'None' and 'Yes' Levels. 
credit.factors$Debtors <- factor(credit.factors$Debtors, labels = c("None", "Yes", "Yes"))
prop.table(table(credit.factors$Debtors))

prop.table(table(credit.factors$Property)) # No change
credit.factors$Property <- factor(credit.factors$Property, 
                                           labels = c("RealState", "LifeIns", "CarOther", "Unknown"))

prop.table(table(credit.factors$OtherInstallments))
# Merged stores with bank
credit.factors$OtherInstallments <- factor(credit.factors$OtherInstallments, 
                                           labels = c("Yes", "Yes", "None"))
prop.table(table(credit.factors$OtherInstallments))

prop.table(table(credit.factors$Housing)) # No change
credit.factors$Housing <- factor(credit.factors$Housing, 
                                 labels = c("Rent", "Own", "Free"))

prop.table(table(credit.factors$Job)) 
# Merged unemployed/unskilled non-resident with unskilled resident
credit.factors$Job <- factor(credit.factors$Job, 
                             labels = c("Unskilled", "Unskilled", "Skilled", "HighRange"))
prop.table(table(credit.factors$Job)) 

prop.table(table(credit.factors$Telephone)) # No change
credit.factors$Telephone <- factor(credit.factors$Telephone, 
                             labels = c("None", "Yes"))

prop.table(table(credit.factors$Foreign)) 
# 96% in one level, maybe we must remove this factor
credit.factors$Foreign <- factor(credit.factors$Foreign, 
                                 labels = c("Yes", "None"))


credit.factors$Result <- factor(credit.factors$Result, labels = c("Good", "Bad"))

credit <- data.frame(credit.factors)  # Now we just add these variables to the whole df.

# Realiza un preprocesamiento análogo, ahora para las variables numéricas.
# En particular, para este ejercicio, aplicaremos el criterio de eliminar todos
# aquellos datos en los que alguna de las variables continuas tenga valores
# extremos menores o mayores a 2.5*IQR de Q1 y Q3, respectivamente. Es
# decir, Q1-2.5*IQR y Q3+2.5*IQR.

# Doing one explicit iteration with one numeric predictor
summary(credit.numeric$Duration)
hist(credit.numeric$Duration)
boxplot(credit.numeric$Duration, main="Meses del credito solicitado",xlab="Duration",ylab="meses")


ThresHigh <- as.numeric(quantile(credit.numeric$Duration, 0.75) + 2.5*IQR(credit.numeric$Duration))
ThresLow <- as.numeric(quantile(credit.numeric$Duration, 0.25) - 2.5*IQR(credit.numeric$Duration)) 

# Identificamos los datos de Duration que satisfacen este criterio:
u <- ifelse((credit.numeric$Duration > ThresHigh) | (credit.numeric$Duration < ThresLow), TRUE, FALSE ) 
sum(u) # podemos contabilizarlos.
sum(u)/length(u) # Proporci?n de outliers de V2.

# Podemos ahora eliminar todo el renglon del data.frame que contiene
# dichos outliers y obtener de nuevo el boxplot:

credit.numeric <- credit.numeric[!u, ]  
credit <- credit[!u, ]
boxplot(credit.numeric$Duration)

# Now we are going to iterate through all numeric variables in a loop

boxplot_descriptions <- c("Box1", "Box2", "Box3", "Box4", "Box5", "Box6", "Box7", "Box8")
y_labels <- c("meses", "$", "Percentage", "Years", "Years", "Credits", "Dependents")
box_index <- 0
for (predictor in colnames(credit.numeric)) {
  print(summary(credit.numeric[[predictor]]))
  boxplot(credit.numeric[[predictor]], main=boxplot_descriptions[box_index], xlab=predictor,ylab="meses")
  
  ThresHigh <- as.numeric(quantile(credit.numeric[[predictor]], 0.75) + 2.5*IQR(credit.numeric[[predictor]]))
  ThresLow <- as.numeric(quantile(credit.numeric[[predictor]], 0.25) - 2.5*IQR(credit.numeric[[predictor]])) 
  u <- ifelse((credit.numeric[[predictor]] > ThresHigh) | (credit.numeric[[predictor]] < ThresLow), TRUE, FALSE ) 
  print(predictor)
  print(sum(u)) # podemos contabilizarlos.
  credit.numeric <- credit.numeric[!u, ]  
  credit <- credit[!u, ]
  
}

# Printing boxplot after removing outliers
for (predictor in colnames(credit.numeric)) {

  boxplot(credit.numeric[[predictor]], main=boxplot_descriptions[box_index], xlab=predictor,ylab="meses")

}

credit <- data.frame(credit, credit.numeric)  # Merging into credit the numerical values

# Notes: Dependents can be removed since they are all 1, BUT maybe we shouldt remove the 2 as outlier

# Partitioning the data

# Dividing set in 80/20. Creating a random permutation
# WHY ARE WE CHOSING THIS???????????????????
set.seed(7)
train <- sample (1:nrow(credit), nrow(credit)*0.8)
credit.test <- credit[-train ,]
credit.train <- credit[train, ]

# Verifying uniformity of proportions in each subset:
prop.table(table(credit.test$Result))
prop.table(table(credit.train$Result))

# Creating decision tree with default values of rpart ---------------
library(rpart.plot)
fit <- rpart(formula = Result ~ ., data=credit.train, method='class')
rpart.plot(fit, extra = 106)  # 106:binary; 104:multiclass

# Predictions
prediction = predict(fit, credit.test, type='class')

# Generating confusion matrix and other stats
library(caret)
confusionMatrix(prediction, credit.test$Result, positive = NULL, dnn = c("Prediction", "Reference"))

# Creating decision tree using Gini index for binary splitting ---------------
fit <- rpart(formula = Result ~ ., data=credit.train, method='class', parms = list(split='gini'))
rpart.plot(fit, extra = 106)  # 106:binary; 104:multiclass
# We found out that the default valur of rpart for splitting is Gini

# Creating decision tree using Cross-Entropy for binary splitting ---------------
fit <- rpart(formula = Result ~ ., data=credit.train, method='class', parms = list(split='information'))
rpart.plot(fit, extra = 106)  # 106:binary; 104:multiclass
# We found out that the default valur of rpart for splitting is Gini
# Predictions
prediction = predict(fit, credit.test, type='class')
confusionMatrix(prediction, credit.test$Result, positive = NULL, dnn = c("Prediction", "Reference"))

