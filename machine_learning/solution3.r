# the script suppose mnist directory in the same level of parent directory example:
# # # ----> ex3/
# # #       ----> solution3.r
# # # ----> mnist/
# # #       ----> sourcedir.R
# # #
# # # script can be called by source('solution3.r')
# # #
source('../mnist/sourcedir.R')
sourceDir('../mnist')
setwd('../mnist/')
#a)load 5,000 images and their associated labels to mnistdata
N <- 5000
mnistdata <- loadmnist( N )
#b)use the provided functions to plot a random sample of 100 handwritten digits
visual(mnistdata$X[1:100,])
#Show the corresponding labels
cat("the associated symbols:\n")
print(mnistdata$y[1:100])
cat("\n")
setwd('../ex3/')
#c)divide the data into two parts
train.X = mnistdata$X[1:2500,]
train.y = mnistdata$y[1:2500]
test.X = mnistdata$X[2501:5000,]
test.y = mnistdata$y[2501:5000]
cat("the 5,000 data have been divided into two sets.")
#d) compute a class prototype given by the mean of all the images in the training set
classPrototype = function(X,y)
{
        P = matrix(0, 10, dim(X)[2])
        num = vector(mode="integer", 10)
        for(i in c(1:dim(X)[1]))
        {
                d = y[i] + 1
                for(j in c(1:dim(X)[2]))
                {
                        if( X[i,j] != 0)
                        {
                                P[d,j] = P[d,j] + X[i,j]
                        }
                }
                num[d] = num[d] + 1
        }        
        
        for(i in c(1:dim(P)[1]))
        {
                for(j in c(1:dim(P)[2]))
                {
                        if( P[i,j] != 0)
                        {
                                P[i,j] = P[i,j] / num[i]
                        }
                }
       }
       return(P)
}
cat("compute a class prototype given by the mean of all the images in the training set:\n")
newP = classPrototype(train.X, train.y)
cat("plot the prototype.....\n")
dev.new()
visual(newP)
#e)compute the euclidean distance of the image to all 10 prototypes and classify the test image into the class for which the distance to the prototype is the smallest
testImage = function(image,newP,trainData=NULL,trainSymbol=NULL)
{
        head(image)
        standarl = trainData
        standarS = trainSymbol
        if(is.null(trainData)){
                standarl = newP
                standarS = c(0:(dim(newP)[1]-1))
        }
        miniDistance = sqrt(sum((image - standarl[1,])^2))
        nearestClass = standarS[1] 
        for(i in c(2:dim(standarl)[1]))
        {
                Distance = sqrt(sum((image - standarl[i,])^2))
                if(miniDistance > Distance)
                {
                        miniDistance = Distance
                        nearestClass = standarS[i] 
                }
        }
        return(nearestClass)
}
#get the confusion matrix according to classify the test image as the nearest neighbor in the prototype
pConfusionMatrix = function(X,y,newP)
{
        #caculate the confusion matrix
        cMatrix = matrix(0,dim(newP)[1],dim(newP)[1])
        for(i in c(1:dim(X)[1])){
                tClass = y[i]+1
                pClass = testImage(X[i,], newP)+1
                cMatrix[tClass,pClass] = cMatrix[tClass,pClass] + 1
        }
        return(cMatrix)
}
#g)get the confusion matrix by comparing the image with all the training image
confusionMatrix = function(tX,ty,X,y)
{
        cMatrix = matrix(0,10,10)
        for(i in c(1:dim(tX)[1]))
        {
                tClass = ty[i]+1
                pClass = testImage(tX[i,],NULL,X,y)+1
                cMatrix[tClass,pClass] = cMatrix[tClass,pClass] + 1
        }
        return(cMatrix)
}
#calculate the accuracy of a confusion matrix
accuracy = function(cMatrix)
{
        total = 0
        d = dim(cMatrix)[1]
        for(i in c(1:d))
        {
                total = total + cMatrix[i,i]
        }
        return(total/sum(cMatrix))
}
#g)compute the error rate of the property-based classifier
cat("The prototype-based confusion matrix:\n")
pMatrix = pConfusionMatrix(test.X,test.y,newP)
print(pMatrix)
pErrorRate = 1 - accuracy(pMatrix)
cat("The nearest neighbor confusion matrix:\n")
nMatrix = confusionMatrix(test.X,test.y,train.X,train.y)
print(nMatrix)
nErrorRate = 1 - accuracy(nMatrix)
cat('The error rate of prototype-based classifer\t vs the nearest neighbor classifier is:', pErrorRate, nErrorRate)

