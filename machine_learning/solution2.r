# the script suppose movielens directory in the same level of parent directory example:
# # ----> ex2/
# #       ----> solution2.r
# # ----> movielens/
# #       ----> sourcedir.R
# #
# # script can be called by source('solution2.r')
# #



source('../movielens/sourcedir.R')
sourceDir('../movielens')
setwd('../movielens/')
loadmovielens()
setwd('../ex2/')

cat(" Generate userid to itemid matrix \n")
item2user = matrix(0,length(userids),length(itemids))
for(i in c(1:dim(ratings)[1]))
{
        item2user[ratings[i,1],ratings[i,2]] = ratings[i,3]
}
item2user_binary = item2user
item2user_binary[item2user_binary!=0] = 1

itemid2jc = function(id1,id2,item2user_binary)
{
        regmat = cbind(item2user_binary[,id1],item2user_binary[,id2])
        nart <- ncol(regmat)
        jdist <- rep(0, nart * nart)
        dim(jdist) <- c(nart, nart)
        reg.col.sum <- apply(regmat, 2, sum)
        reg.aggrement <- t(regmat) %*% regmat
        jdist <- reg.aggrement/(reg.col.sum - t(t(reg.aggrement) - reg.col.sum))
        return(jdist[1,2])
}

itemname2itemid = function(name,items)
{
        for(i in c(1:dim(items)[1]))
        {
                if(name == sub(' \\(.*\\)','',items[i,2]))
                {return(i)}
        }
        warnings(' id not found')
        return(0)
}


itemid2itemname = function(id,items)
{
        for(i in c(1:dim(items)[1]))
        {
                if(id == items[i,1])
                {return(unlist(strsplit(items[i,2],' \\('))[1])}
        }
        warnings(' name not found')
        return('no name')
}

itemid2cor = function(id1,id2,item2user)
{
        regmat = cbind(item2user[,id1],item2user[,id2],item2user[,id1]*item2user[,id2])
		regmat = regmat[which(regmat[,3]!=0),1:2]
		if(length(regmat) < 3)
		{return(0)}else
		{return(cor(regmat,method = c("spearman"))[1,2])}
}

# b.
cat("b.\n")
 
# jaccard coefficient of 'Toy Story' and 'GoldenEye'
cat("Toy Story\tvs GoldenEye:\t", itemid2jc(itemname2itemid('Toy Story',items), itemname2itemid('GoldenEye',items), item2user_binary),"\n")

# three colors: blue and three colors: red
cat("Three Colors: Blue\tvs Three Colors: Red:\t", itemid2jc(itemname2itemid('Three Colors: Blue',items), itemname2itemid('Three Colors: Red',items), item2user_binary),"\n")

# 5best to taxi driver
top5HighestWithMovie = function(moviename,items,item2user_binary)
{
        desid = itemname2itemid(moviename,items)
        res = c()
        for(i in c(1:dim(item2user)[1]))
        {
                res = rbind(res, c(i, itemid2jc(desid, i, item2user_binary)))
        }
        res = res[order(res[,2], decreasing=TRUE),]
        for(i in c(1:10))
        {
                cat(i-1, ":\t", itemid2itemname(res[i,1],items), "\t", res[i,2], "\n")
        }
}
top5HighestWithMovie('Taxi Driver',items,item2user_binary)
cat("\n")
top5HighestWithMovie('Speed',items,item2user_binary)

# c.
cat("c.\n")

# correlation coefficient of 'Toy Story' and 'GoldenEye'
cat("Toy Story\tvs GoldenEye:\t", itemid2cor(itemname2itemid('Toy Story',items), itemname2itemid('GoldenEye',items), item2user),"\n")

# three colors: blue and three colors: red
cat("Three Colors: Blue\tvs Three Colors: Red:\t", itemid2cor(itemname2itemid('Three Colors: Blue',items), itemname2itemid('Three Colors: Red',items), item2user),"\n")

top5HighestWithMovie2 = function(moviename,items,item2user)
{
        desid = itemname2itemid(moviename,items)
        res = c()
        for(i in c(1:dim(item2user)[1]))
        {
                res = rbind(res, c(i, itemid2cor(desid, i, item2user)))
        }
        res = res[order(res[,2], decreasing=TRUE),]
        for(i in c(1:10))
        {
                cat(i-1, ":\t", itemid2itemname(res[i,1],items), "\t", res[i,2], "\n")
        }
}
top5HighestWithMovie2('Taxi Driver',items,item2user)
cat("\n")
top5HighestWithMovie2('Speed',items,item2user)
