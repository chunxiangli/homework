
#
#
# R script for supervised machine learning exercise one
#
#

#!/usr/bin/r

# initialize parameters
d <<- 600
n <<- 1000
v <<- 0.2
bt <<- 0.0001

# generate input matrix x
x = sample(c(1,-1), d*n, replace = TRUE, prob = c(0.5,0.5))
dim(x) = c(d,n)

# generate hypothesis function h, functions are indexed by parameter index
h = function(index, xt)
{
    return(xt[index])
}

# generate labeling function
y = function(xt)
{
    return(sample(c(xt[1], -xt[1]), 1, prob = c(1-v, v)))
}

# weighted majority voting algorithm
wm = function(x)
{
    W = c()
    loss.1 = c()
    loss.2 = c()
    w = rep(1, n)   # initialize w
    W = w
    for(t in c(1:d))
    {
        w.new = w
        xt = x[t,]
        yt = y(xt)
        P = c()
        M = c()
        Wp = 0
        Wm = 0
        for(index in c(1:n))
        {
            yhi = h(index,xt)
            if(yhi != yt)
            {w.new[index] = w.new[index] * bt}
            if(yhi == 1)
            {
                P = c(P, index)
                Wp = Wp + w[index]
            }
            else
            {
                M = c(M, index)
                Wm = Wm + w[index]
            }
        }
        if(Wm > Wp)
        {yh = -1}
        else
        {yh = 1}
        if(length(loss.1) != 0)
        {tmp = loss.1[length(loss.1)]}
        else
        {tmp = 0}
        loss.1 = c(loss.1, tmp+abs(yh-yt)/2)
        if(length(loss.2) != 0)
        {tmp = loss.2[length(loss.2)]}
        else
        {tmp = 0}
        loss.2 = c(loss.2, tmp+abs(x[t,1]-yh)/2)
        w = w.new
        W = rbind(W, w)
    }
    return(list(W,loss.1, loss.2))
}

# R U N
res = wm(x)
w = res[[1]]
loss.1 = res[[2]]
loss.2 = res[[3]]

# plot

#pdf(sprintf('./plots/3.%s.%s.%n.pdf', n, v, bt), height = 12, width =12)
par(mfrow = c(2,2))
v = w/apply(w,1,sum)

plot(log(v[,1]/v[,n]), col = 'red', type = 'l',xlab = 'n',ylab ='odd ratio', main = 'odd ratio of relevant weigh\nagainst irrelevant weight')
grid(10,10)
for(i in c((length(v[1,])-1):2))
{
    lines(log(v[,1]/v[,i]), col = 'red', type = 'l')
}

plot(v[,1], col = 'red', type = 'l', xlab = 'n', ylab = 'weight', main = 'relevant weight\nand irrelevant weights')
grid(10,10)
for(i in c(2:10))
{
    lines(v[,i], col = 'blue', type = 'l')
}
legend('topleft', col = c('red','blue'), lty = 1, c('relevant weight','irrelevant weight'))



#
#
# R script for supervised machine learning exercise one
#
#

#!/usr/bin/r

# initialize parameters
argv = commandArgs(trailingOnly = TRUE)
d = as.numeric(argv[1])
n = as.numeric(argv[2])
v = as.numeric(argv[3])
bt = as.numeric(argv[4])

#d <<- 600
#n <<- 100
#v <<- 0.2
#bt <<- 0.99

# generate input matrix x
x = sample(c(1,-1), d*n, replace = TRUE, prob = c(0.5,0.5))
dim(x) = c(d,n)

# generate hypothesis function h, functions are indexed by parameter index
h = function(index, xt)
{
    return(xt[index])
}

# generate labeling function
y = function(xt)
{
    return(sample(c(xt[1], -xt[1]), 1, prob = c(1-v, v)))
}

# weighted majority voting algorithm
wm = function(x)
{
    W = c()
    loss.1 = c()
    loss.2 = c()
    w = rep(1, n)   # initialize w
    W = w
    for(t in c(1:d))
    {
        w.new = w
        xt = x[t,]
        yt = y(xt)
        P = c()
        M = c()
        Wp = 0
        Wm = 0
        for(index in c(1:n))
        {
            yhi = h(index,xt)
            if(yhi != yt)
            {w.new[index] = w.new[index] * bt}
            if(yhi == 1)
            {
                P = c(P, index)
                Wp = Wp + w[index]
            }
            else
            {
                M = c(M, index)
                Wm = Wm + w[index]
            }
        }
        if(Wm > Wp)
        {yh = -1}
        else
        {yh = 1}
        if(length(loss.1) != 0)
        {tmp = loss.1[length(loss.1)]}
        else
        {tmp = 0}
        loss.1 = c(loss.1, tmp+abs(yh-yt)/2)
        if(length(loss.2) != 0)
        {tmp = loss.2[length(loss.2)]}
        else
        {tmp = 0}
        loss.2 = c(loss.2, tmp+abs(x[t,1]-yh)/2)
        w = w.new
        W = rbind(W, w)
    }
    return(list(W,loss.1, loss.2))
}

# R U N
res = wm(x)
w = res[[1]]
loss.1 = res[[2]]
loss.2 = res[[3]]

# plot

pdf(sprintf('./plots/3.%s.%s.%s.pdf', n, v, bt), height = 12, width =12)
par(mfrow = c(2,2))
v = w/apply(w,1,sum)

plot(log(v[,1]/v[,n]), col = 'red', type = 'l',xlab = 'n',ylab ='odd ratio', main = 'odd ratio of relevant weigh\nagainst irrelevant weight')
grid(10,10)
for(i in c((length(v[1,])-1):2))
{
    lines(log(v[,1]/v[,i]), col = 'red', type = 'l')
}

plot(v[,1], col = 'red', type = 'l', xlab = 'n', ylab = 'weight', main = 'relevant weight\nand irrelevant weights')
grid(10,10)
for(i in c(2:10))
{
    lines(v[,i], col = 'blue', type = 'l')
}
legend('topleft', col = c('red','blue'), lty = 1, c('relevant weight','irrelevant weight'))


plot(loss.1, col = 'red', type = 'l', xlab = 'n', ylab = '', main = 'cumulative loss 1')
grid(10,10)

plot(loss.2, col = 'red', type = 'l', xlab = 'n', ylab = '', main = 'cumulative loss 2')
grid(10,10)

dev.off()
