
set.seed(1)

#a) randomly draw 500 datapoints
genData = function(size,p,sD1,sD2){
        Data = matrix(0,size,3)
        for(i in c(1:size)){
                y = rbinom(1,1,p)
                sD = sD1 
                if( 1 == y){
                        sD = sD2 
                }
                Data[i,1] = y
                Data[i,2:3] = rnorm(2,mean=0,sd=sD) 
        }
        return(Data)
}
twoDscatterplot = function(data,title,color){
        plot(pch=16,data[,2],data[,3],xlab="x1",ylab="x2", col=color[data[,1]+1],main=title)
}
#a) randomly draw 500 datapoints
trainData = genData(500,0.5,1,4)
#2D scatterplot with red points for class y=0, blue points for class y=1
pdf('./result_plot.pdf', height=5, width=10)
par(mfrow=c(1,2), cex=0.5, lwd=2)
color=c("red","blue")
twoDscatterplot(trainData, "trainData",color)

#b)randomly draw 2000 new datapoints for testing set
testData = genData(2000,0.5,1,4)
#2D scatterplot testing data set
twoDscatterplot(testData, "testData",color)

 #find the mode of the classes of values
findMode = function(vX){
        #uX = unique(vX) return(uX[which.max(tabulate(match(vX, uX)))]) for binary 
        #classifier and the size of vX is even, we can use median to find themode
        return(median(vX))
}

kNNClassifier = function(k, test, trainSet=trainData){
        size = dim(trainSet)[1]
        distanceM = matrix(0,size,2)
        for(i in c(1:size)){
                train = trainSet[i,2:3]
                distanceM[i,1] = sqrt(sum((test - train)^2))
                distanceM[i,2] = trainSet[i,1]
        }       
        distanceM = distanceM[order(distanceM[,1]),]
        return(findMode(distanceM[1:k,2]))
}
classifyDataSet = function(dataSet, kSet, trainSet=trainData){
        lenK = length(kSet)
        dataSize = dim(dataSet)[1]
        #save the classifier result for each k
        classMatrix = matrix(0,dataSize, lenK)
        for(i in c(1:lenK)){
                k = kSet[i]
                for(j in c(1:dataSize)){
                        classMatrix[j,i] = kNNClassifier(k, dataSet[j,2:3])
                }
        }
        return(classMatrix)
}
#caculate the percentage of mislassification
misClassification = function(cMatrix,dataSet){
        size = dim(dataSet)[1]
        lenK = dim(cMatrix)[2]
        vErr = vector(mode="integer",lenK)
        for(i in c(1:lenK)){
                #misclassification
               vErr[i] = sum(abs(cMatrix[,i] - dataSet[,1])) / size
        }
        return(vErr)
}
#set the kSet
kSet =c(1,3,5,7,9,13,17,21,25,33,41,49,57)
#c)kNN classify testing data
testClResult = classifyDataSet(testData, kSet)
testMisCl = misClassification(testClResult, testData)
#plot(kSet,testClResult,xlab="Degree of Freedom - N/k",ylab="Test Error",main="Misclassification curves")

#d)kNN classify train data
trainClResult = classifyDataSet(trainData, kSet)
trainMisCl = misClassification(trainClResult, trainData)
#plot(kSet,trainClResult,xlab="Degree of Freedom - N/k",ylab="Test Error",main="Misclassification curves")

#e)generate 10000 samples
test2Data=genData(10000,0.5,1,4)
optimalValue=sqrt(log(4)*32/15)
bErr = 0
for(i in c(1:10000)){
        if(test2Data[i,2]>= -optimalValue && test2Data[i,2]<= optimalValue && test2Data[i,3]>= -optimalValue && test2Data[i,3]<= optimalValue && test2Data[i,1] == 1){
                bErr = bErr + 1
        }
       if(test2Data[i,2]<= -optimalValue && test2Data[i,2]>= optimalValue && test2Data[i,3]<= -optimalValue && test2Data[i,3]>= optimalValue && test2Data[i,1] == 0){                
                bErr = bErr + 1
        }    
}
#calculate the bayesian error rate
bErrRate = bErr/10000
print(bErrRate)
par(mfrow=c(1,2),lwd=2,mar = c(5,5,5,2))
plot(ylab='test error',xlab='N/k',main='k',xaxt='n',ylim=c(0,0.18),testMisCl[length(testMisCl):1], col = 'orange', type = 'b', lty=2)
axis(3,c(1:length(kSet)),kSet[length(kSet):1])
axis(1,c(1:length(kSet)),round(500/kSet[length(kSet):1]))
grid(10,10,col='gray')
lines(trainMisCl[length(testMisCl):1], col = 'blue', type = 'b', lty=3)
abline(h=bErrRate, col = 'purple', lty = 4)
legend('bottomleft',pch=1, lty = c(2,3,4), col=c('orange','blue','purple'), c('test','train','Bayes'))

#
dev.off()
