#generate 30 points xi from he uniform distribution on the interval[-3,3]
set.seed(1)
genData = function(size){
        xy = matrix(0, size, 2)
        names(xy)=c("x","y")
        n = rnorm(size,0,0.4)
        xy[,1] = runif(size,-3,3)
        for(i in c(1:size)){
                xy[i,2]= 2 + xy[i,1] - 0.5 * xy[i,1]^2 + n[i]
        }
        return(xy)
}
#generate 30 pairs(xi,yi)
dataSet = genData(30)
#define expr for k order fit poly
kOrderFitExpr = function(x,w,k){
        t = x - x
        uw = unname(w)
        for(i in c(1:(k+1))){
               t = t + uw[i]*x^(i-1) 
        }
        return(t)
}
#define get the coefficients array for k-order fit polynomial fit
kOrderFitPoly = function(x,y,k){
        weight = c(0)
        if(0 == k){
                weight = lm(y~1)        
        }else if(1==k){
                weight = lm(y~I(x^1))
        }else if(2 == k){
                weight = lm(y~I(x^1)+I(x^2))
        }else if(3 == k){
                weight = lm(y~I(x^1)+I(x^2)+I(x^3))
        }else if(4 == k){
                weight = lm(y~I(x^1)+I(x^2)+I(x^3)+I(x^4))
        }else if(5 == k){
                weight = lm(y~I(x^1)+I(x^2)+I(x^3)+I(x^4)+I(x^5))
        }else if(6 == k){
                weight = lm(y~I(x^1)+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6))
        }else if(7 == k){
                weight = lm(y~I(x^1)+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7))
        }else if(8 == k){
                weight = lm(y~I(x^1)+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7)+I(x^8))
        }else if(9 == k){
                weight = lm(y~I(x^1)+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7)+I(x^8)+I(x^9))
        }else if(10 == k){
                weight = lm(y~I(x^1)+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7)+I(x^8)+I(x^9)+I(x^10))
        }
        w = unname(weight$coefficients)
        return(w)
}
#define function to draw the datapoints(xi,yi), calculate the fitted polynomial k-order, and plot the polynomial as a curve in the full interval[-3,3],overlayed on the scatterplot of the points
plotKOrderPolyResult = function(X,Y,k){
        plot(X,Y,main=paste(k,"-order Fit Result"),xlab="x",ylab="y")
        axis(1,c((-3):3))
        weight = kOrderFitPoly(X,Y,k)
        #draw the curve in the interval[-3,3] with weight vector
        curve(kOrderFitExpr(x,weight,k),-3,3,add=TRUE) 
}
#define function to calculate the coefficent of determination R2 for k-order fit polynomial
coDetermination = function(X,Y,k){
        RRn = 0
        RRd = 0
        xLen = length(X)
        meanY = mean(Y)
        weight = kOrderFitPoly(X,Y,k)
        for(i in c(1:xLen)){
                RRn = RRn + (Y[i] - kOrderFitExpr(X[i],weight,k))^2
                RRd = RRd + (Y[i] - meanY)^2  
        }
        return(1-(RRn/RRd))
}
kFoldCrossValidation = function(X,Y,k,kR){
        squareErr = vector(mode="integer",kR+1)
        xLen = length(X)
        dataSize = xLen/k 
        testXData = c(0)
        testYData = c(0)
        trainXData = c(0)
        trainYData = c(0)
        for(j in c(1:k)){
                if(1 == j){
                        testXData = X[1:dataSize]
                        testYData = Y[1:dataSize]  
                        trainXData = X[(dataSize+1):xLen]
                        trainYData = Y[(dataSize+1):xLen]
                }else if(k == j){
                        trainXData = X[1:((j-1)*dataSize)]
                        trainYData = Y[1:((j-1)*dataSize)]  
                        testYData = Y[((j-1)*dataSize+1):xLen]
                        testXData = X[((j-1)*dataSize+1):xLen]
                }else{
                        trainXData = X[c(1:((j-1)*dataSize),c(((j)*dataSize+1):xLen))]
                        trainYData = Y[c(1:((j-1)*dataSize),c(((j)*dataSize+1):xLen))]  
                        testYData = Y[((j-1)*dataSize+1):(j*dataSize)]
                        testXData = X[((j-1)*dataSize+1):(j*dataSize)]
                }
                #calculate the coeffient for 0~kR order 
                for(i in c(0:kR)){
                        #plotKOrderPolyResult(trainXData,trainYData,i)
                        weight = kOrderFitPoly(trainXData,trainYData,i)
                        
                        for(t in c(1:dataSize)){
                                distance = (testYData[t] -  kOrderFitExpr(testXData[t],weight,i))
                                squareErr[i+1] = squareErr[i+1] + distance^2
                        }
                }
        }
        smallK = sort(squareErr,index.return=TRUE)$ix[1]
        print(paste('The order fit poly has the smallest squareErr is',smallK-1,'with error',squareErr[smallK])[1])
        print('Its coeeficients are') 
        print(kOrderFitPoly(dataSet[,1],dataSet[,2],smallK-1)) 
        plot(c(0:kR),squareErr,main="Square Error with K", xlab="K", ylab="squareError",type="b")
        axis(1,c(0:kR))
}
pdf("./result_plot.pdf",width=24,height=18)
par(mfrow = c(3,4))
drawResult = function(x,y,k){
        for( i in c(0:k)){
                plotKOrderPolyResult(dataSet[,1],dataSet[,2],i)
        }
}
#a) draw the 0~10-order fit polynomial plot
drawResult(dataSet[,1],dataSet[,2],10)
#a) calculate the 0~10-order coeffient of determination R2 and plot
cD = vector(mode="integer",11)
for(i in c(0:10)){
        cD[i+1] = coDetermination(dataSet[,1],dataSet[,2],i)
}
largeCD = sort(cD,index.return=TRUE)$ix[11]
print(paste('The k-order fit polynomial with high coeeficient od determination R2 is',largeCD-1,'with',cD[largeCD]))
plot(c(0:10),cD,main="coeffient of determination R2 with K",xlab="K", ylab="coeffient of determination R2",type="b")
axis(1,c(0:10))
#b) 10-fold cross-validation and plot the square error result
kFoldCrossValidation(dataSet[,1],dataSet[,2],10,10)
dev.off()
