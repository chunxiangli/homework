#defin gendata function
source('../mnist/sourcedir.R')
sourceDir('../mnist')
setwd('../mnist/')
#a)load 5,000 images and their associated labels to mnistdata
N = 5000
mnistdata = loadmnist(N)
setwd('../ex6/')
color=c("red","blue","green","purple")
#pdf("./Rplots.pdf",width=24,height=18)
genData = function(size,sd){
        centerBase = matrix(c(-1,-1,1,1,-1,1,-1,1),4,2)
        print("The original centroids are")
        print(centerBase)
        baseC = c(0,0,0,0)
        x=1:4
        Data = matrix(0,size,2)
        aIndex=vector(mode="integer",size)
        for(i in c(1:size)){
                index = sample(x)[1]
                aIndex[i] = index
                baseC[index] = baseC[index]+1
                Data[i,1] = rnorm(1,centerBase[index,1],sd)
                Data[i,2] = rnorm(1,centerBase[index,2],sd)
        }
        plot(Data,main=paste("Original Data(sd=",sd,",size=",size,")"),cex=0.5,col=color[aIndex],pch=aIndex)
        return(Data)
}
#define K-means algorithm
kMean = function(data,k,maxStep=10,draw=TRUE){
        size = dim(data)[1]
        dSize = dim(data)[2]
        aIndex = vector(mode="integer",size)
        #1)random select k points as centeroid
        cClusterM=matrix(0,(maxStep*10+1)*k,dSize)
        cCluster = data[sample(1:size)[1:k],]
        cClusterM[1:k,]=cCluster
        if(draw){
                print("The sample centroids are")
                print(cCluster)
        }else{
                visual(cCluster)
        }
        #2)assigning each point to its closest centroid
        stop = FALSE
        #calculate the sse
        ssE = 0
        #recode the decreasing trend
        step = 0
        Step = 0
        while(!stop){
                ssENew = 0
                for(i in c(1:size)){
                        min = sum((data[i,]-cCluster[1,])^2)
                        aIndex[i] = 1
                        for(j in c(2:k)){
                                distance = sum((data[i,]-cCluster[j,])^2)
                                if(distance < min){
                                        min = distance 
                                        aIndex[i] = j
                                }
                        }
                        ssENew = ssENew + min
                }
                #calculate the new cluster centeroid
                sizeClusters = vector(mode="integer",k+1)
                sizeClusters[2:(k+1)] = tabulate(match(aIndex,c(1:k)))
                newClusterC = matrix(0,k,dSize) 
                for(iJ in c(1:size)){
                        cJ = aIndex[iJ]
                        newClusterC[cJ,] = newClusterC[cJ,] + data[iJ,] 
                }
                for(iJ in c(1:k)){
                        newClusterC[iJ,] = newClusterC[iJ,]/sizeClusters[iJ+1]
                }
                #check whether the centroid changes
                change = 0
                for( i in c(1:dSize)){
                        if(sum(newClusterC[,i] != cCluster[,i]) > 1){
                                change = change + 1 
                        }
                }
                if(0 == change){
                        stop = TRUE
                }else{ 
#                        if((ssE  - ssENew) > 0.1){
#                                step = step + 1
#                        }else{
#                                step = 0
#                         }
#                        if( maxStep <= step){
#                                print("keep decreasing")
#                                 stop = TRUE 
#                        }
                }
                ssE = ssENew
                Step = Step + 1
                if(draw){
                        plot(data,main=paste(Step,"-th iteration cluster result"),col=color[aIndex],pch=aIndex,cex=0.5)
                        points(cCluster,pch=11,cex=1,col='black')
                        centerX=matrix(as.numeric(cClusterM[c(1:(Step*k)),1]),k,Step*k)
                        centerY=matrix(as.numeric(cClusterM[c(1:(Step*k)),2]),k,Step*k)
                        for(ik in c(1:k))
                        {lines(centerX[ik,],centerY[ik,],col=color[ik],type = 'b',lwd=1,pch=16)}
                        print(paste("The",Step,"th iteration's SSE is"))
                        print(ssENew)
                }
                cCluster = newClusterC
                cClusterM[((Step*k)+1):((Step*k)+k),] = newClusterC
        }
        print(paste('Use ',Step,'iterations'))
        if(FALSE == draw){
                visual(cCluster)
        }else{
                print("The final centroids are")
                print(cCluster)
        }
        return(aIndex)
}
#a) generate 500 data for sd=0.5 and plot the data
pdf("./result_plot.pdf")
par(mfrow=c(2,2))
trainData = genData(500,0.5)
#b)1) apply the algorithm k=4 to the trainData and plot the final centroids on top of the datapoints
index=kMean(trainData,4)
#b)2) generate 600 datapoints with the same sd with a) and run the k-means again
trainMData = genData(600,0.5)
index=kMean(trainMData,4)
#c) generate 500 datapoints with the same function as a) with sd=0.1
trainCData = genData(500,0.1)
index=kMean(trainCData,4)


#d)
testMNIST = function(){
        kSet = c(2,4,10)
        for(i in c(1:length(kSet))){
                #cluster the mnist image data
                aIndex = kMean(mnistdata$X,kSet[i],draw=FALSE)
                #generate the digit*cluster matrix
                cMatrix = matrix(0,kSet[i],10)
                for(j in c(1:N)){
                        c = aIndex[j]
                        s = mnistdata$y[j]
                        cMatrix[c,s+1] = cMatrix[c,s+1]+1
                }
                print(paste(kSet[i],'-cluster result'))
                print(cMatrix)
        }
}
testMNIST()
kSet = c(2,4,10)
dev.off()
