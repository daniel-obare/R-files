#normalization function
normalize=function(x) {
  num=x-min(x)
  den=max(x)-min(x)
  return(num/den)
}

#applying the function in dataframe irisas an example
norm=as.data.frame(lapply(dia[ ,c(1,5,6,7,8,9,10)], normalize))


#getting the accuracy of the model table
accuracy=function(x) {
  sum(diag(x)/(sum(rowSums(x))))*100
}

accuracy(tb)