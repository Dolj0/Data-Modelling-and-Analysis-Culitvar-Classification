



hyperParam <- function(inputdata){
  
  #Define return df
  returnDf <- data.frame(kernel=character(), gamma=double(), c=double(), acc=double(), stringsAsFactors = FALSE)
  
  
  #define possible hyper parameters
  kernels <- c("linear", "polynomial", "radial", "sigmoid")
  gammas <- logspace(-3, 2, 6)
  cs <- logspace(-3, 2, 6)
  
  #Create initial test/train split
  classIndex <- grep("Class", colnames(inputdata))
  shuffledData <- inputdata[sample(nrow(inputdata)),]
  folds <- cut(seq(1, nrow(inputdata)), breaks=5, labels=FALSE)
  testIndexes <- which(folds==1, arr.ind=TRUE)
  test <- inputdata[testIndexes,]
  train <- inputdata[-testIndexes,]
  
  for(kernel in kernels){
    storedAcc = -1000
    storedGamma = 0
    storedC = 0
    for(gamma in gammas){
      for(c in cs){
        acc=0
        svmfit = svm(Class~., data=train, kernel=kernel, cost=c, gamma=gamma, scale=FALSE)
        pred=predict(svmfit, test[,-classIndex], type="Class")
        acc = 100*mean(pred==test[,classIndex])
        
        if(acc > storedAcc){
          storedAcc = acc
          storedGamma = gamma
          storedC = c
        }
      }
    }
    newRow = c(kernel, storedGamma, storedC, acc)
    returnDf <- rbind(returnDf, newRow)
  }
  return(returnDf)
}


crossv <- function(inputdata, kernel, c, gamma){
  
  returnDf <- data.frame(fold=integer, c=double(), gamma=double(), Accuracy=double(), stringsAsFactors = FALSE)
  
  classIndex <- grep("Class", colnames(inputdata))
  shuffledData <- inputdata[sample(nrow(inputdata)),]
  folds <- cut(seq(1, nrow(inputdata)), breaks=10, labels=FALSE)
  
  for(i in 1:10){
    testIndexes <- which(folds==i, arr.ind=TRUE)
    testData <- inputdata[testIndexes,]
    trainData <- inputdata[-testIndexes,]
    
    svmfit = svm(Class~., data=trainData, kernel=kernel, cost=c, gamma=gamma, scale=FALSE)
    pred = predict(svmfit, testData[,-classIndex], type="Class")
    acc = 100*mean(pred==testData[,classIndex])
    
    newRow = c(i, c, gamma, acc)
    returnDf <- rbind(returnDf, newRow)
  }
  
  colnames(returnDf) <- c("Fold", "C", "Gamma", "Accuracy")
  return(returnDf)
  
}
