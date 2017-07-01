# @Encode: utf-8
# @Title: Logistic Regression
# @Date: July 01 2017
# @Description: 
# - Logistic regression algorithm by glm function.
# - Visualization the result of the model.


logisticRegression <- function(data, feature, label,trace=FALSE){
  # Extends 'glm' function
  # Parameters
    # data(matrix): The training data
    # label(num): The label of train data
    # trace(logical): output should be produced for each iterations
  # Return
    # model(list): A list contain the weight and bias of the logistic model
  
  # Data type transform
  data[,which(names(data)==label)] <- as.factor(data[,which(names(data)==label)])
  # Create a formula object
  featureFormula <- feature[1]
  if(length(feature)>=2){
    for(i in 2:length(feature)){
      featureFormula <- paste(featureFormula,feature[i], sep = "+")
    }
  }
  
  model <- glm(formula = as.formula(paste(label,"~",featureFormula,sep = "")),
               data = data,
               family = binomial(link = "logit"), # Logistic
               control = glm.control(trace = trace),
               intercept=TRUE)
  
  return(model)
}

createLogit <- function(weight, bias, start, end){
  # Generate data which obey the logistic function
  # The data is for plot a 'logistic' line
  x <- seq(from = (start)-0.7*start, to = end+0.7*end, by=0.001)
  y <- x*weight + bias
  y <- 1/(1+exp(-y))
  
  dataReturn <- data.frame("X"=x, "Y"=y)
  
  return(dataReturn)
}

#### Test strat ####
# Data
library(DAAG) # Contain data set anesthetic
dataTest <- anesthetic
# Modeling
modelTest <- logisticRegression(data=dataTest, label = "nomove", feature = "conc", trace = TRUE)
# Predict
dataTest$Predict <- predict(modelTest, dataTest, type = "response")
# Delete duplicate items
dataTestLine <- dataTest[!duplicated(dataTest$conc),]

# Visualization
# Visualization the 'logistic' line and the sample
# the points are sample
# the blue line is the result of logistic regression
dataLogit <- createLogit(weight = as.numeric(modelTest$coefficients[2]), bias = as.numeric(modelTest$coefficients[1]),
                         start = min(dataTest$conc), end = max(dataTest$conc))

p <- ggplot()+
      geom_point(data = dataTest, aes(x = conc, y = Predict), color="red", size= 3, alpha=0.4)+
      geom_line(data = dataLogit, aes(x = X, y = Y), size=1, color="blue", alpha = 0.5)+
      geom_hline(yintercept = 0.5, color = "red", alpha=0.7, size=1)+
      geom_hline(yintercept = 1, color= "black", size=1.5)+
      geom_hline(yintercept = 0, color= "black", size=1.5)+
      geom_label(x = "CONC", y = "Probability")
p
#### Test End ####


