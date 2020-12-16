#Read the file
df=read.csv("data.csv")

#for stratified cross validation sorting  the class variable number of fold=5
dfn = df[order(df$EmpSatisfaction),]

n=nrow(df[])
f=(n/5)-1

#inserting the first element in each fold
i=1
df1=dfn[i,]
df2=dfn[i+1,]
df3=dfn[i+2,]
df4=dfn[i+3,]
df5=dfn[i+4,]

#inserting all other elements so that equal number of class varible present in each class
for (i in 1:f)
{ 
  j=i*5
  df1=rbind(dfn[j+1,],df1)
  df2=rbind(dfn[j+2,],df2)
  df3=rbind(dfn[j+3,],df3)
  df4=rbind(dfn[j+4,],df4)
  df5=rbind(dfn[j+5,],df5)
}

#final 5 folds  

fold1train=rbind(df2,df3,df4,df5)
fold1test=df1

fold2train=rbind(df1,df3,df4,df5)
fold2test=df2

fold3train=rbind(df1,df2,df4,df5)
fold3test=df3

fold4train=rbind(df1,df2,df3,df5)
fold4test=df4

fold5train=rbind(df1,df2,df3,df4)
fold5test=df5

# function of supervised learning method random forest used in the wrapper approach
RF=function(train,test)
{
  training_set=train
  n=ncol(training_set)
  #print(n)
  n1=nrow(test)
  #print(n1)
  
  training_set$EmpSatisfaction <- as.character(training_set$EmpSatisfaction)
  training_set$EmpSatisfaction <- as.factor(training_set$EmpSatisfaction)
  
  
  library(randomForest)
  set.seed(123)
  classifier = randomForest(x = training_set[-n],
                            y = training_set$EmpSatisfaction,
                            ntree = 500)
  
  y_pred = predict(classifier, newdata = test[-n])
  
  
  #calculate the confusion matrix
  cm = table(test[, n], y_pred)
  cm1=as.matrix(cm)
  
  #calling the accuracy function to know the accuracy
  acc1=accuracy(cm1,n1)
  return(acc1)
}

# function for getting the accuracy
accuracy=function(cm1,n1)
{
  b=0
  for(k in 1:4)
  {
    a=cm1[k,k]
    b=b+a
    
  }
  acc= b/n1
  return(acc)
  
}


#for the first attribute selection
b=0
for(i in 1:20)
{
  train1=fold1train[,c(i,21)]
  test1=fold1test[,c(i,21)]
  a1=RF(train1,test1)
  
  train2=fold2train[,c(i,21)]
  test2=fold2test[,c(i,21)]
  a2=RF(train2,test2)
  
  train3=fold3train[,c(i,21)]
  test3=fold3test[,c(i,21)]
  a3=RF(train3,test3)
  
  train4=fold4train[,c(i,21)]
  test4=fold4test[,c(i,21)]
  a4=RF(train4,test4)
  
  train5=fold5train[,c(i,21)]
  test5=fold5test[,c(i,21)]
  a5=RF(train5,test5)
  
  #avg of the 5 turns considering as the accuracy of the index
  a=(a1+a2+a3+a4+a5)/5
  print(paste0("index is: ",i))
  print(a)
  
  
  #replacing and checking if the feature gave maximum accuracy
  if(a>b)
  {
    b=a
    featureIndex=i
    
  }
}

print(b)
b1=b

#inserting the selected feature index
FeatureSet=c(featureIndex)


#looping till full feature list or till increase in accuracy

for(k in 2:20)
{
b=0
for(i in 1:20)
{
  #skipping the feature which are already selected
  if(any(i %in% FeatureSet))
  {
    print("skip")
  }
  else
  {
    # for each turn generating the test and train dataframe  
    # & performing random forest and getting accuracy
    
    t=subset(fold1train, select = FeatureSet)
    train1=fold1train[,c(i,21)]
    train1=cbind(t,train1)
    t1=subset(fold1test, select = FeatureSet)
    test1=fold1test[,c(i,21)]
    test1=cbind(t1,test1)
    a1=RF(train1,test1)
    
    t=subset(fold2train, select = FeatureSet)
    t1=subset(fold2test, select = FeatureSet)
    train2=fold2train[,c(i,21)]
    train2=cbind(t,train2)
    test2=fold2test[,c(i,21)]
    test2=cbind(t1,test2)
    a2=RF(train2,test2)
    
    t=subset(fold3train, select = FeatureSet)
    t1=subset(fold3test, select = FeatureSet)
    train3=fold3train[,c(i,21)]
    train3=cbind(t,train3)
    test3=fold3test[,c(i,21)]
    test3=cbind(t1,test3)
    a3=RF(train3,test3)
    
    t=subset(fold4train, select = FeatureSet)
    t1=subset(fold4test, select = FeatureSet)
    train4=fold4train[,c(i,21)]
    train4=cbind(t,train4)
    test4=fold4test[,c(i,21)]
    test4=cbind(t1,test4)
    a4=RF(train4,test4)
    
    t=subset(fold5train, select = FeatureSet)
    t1=subset(fold5test, select = FeatureSet)
    train5=fold5train[,c(i,21)]
    train5=cbind(t,train5)
    test5=fold5test[,c(i,21)]
    test5=cbind(t1,test5)
    a5=RF(train5,test5)
    
    #avg of the 5 turns considering as the accuracy of the feature
    a=(a1+a2+a3+a4+a5)/5
    #print(a)
    print(paste0("feature index is: ",i))
    print(a)
    
    
    #getting the best feature index in the iteration
    if(a>b)
    {
      b=a
      featureIndex=i
      #FeatureSet[k]=featureIndex
    }
    
  }
}
#adding the feature till accuracy increasing 
  if (b<b1)
  {
    break
  }
  else{
    b1=b
    FeatureSet[k]=featureIndex
  }
}



# getting the column label names from the indexes 
featurename=0
n3=length(FeatureSet)
for (m in 1:n3)
{
  inx=FeatureSet[m]
  
  featurename[m]=cbind(names(df[inx]))
  
}

#printing the important feature indexes and names
print("important feature index are:")
print(FeatureSet)
print("important feature names are:")
print(featurename)
