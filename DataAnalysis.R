http://www.itc.nl/~rossiter/teach/R/R_corregr.pdf
#####read data from ath.csv
ath=read.table("ath.csv",header=T)
str(ath)
names(ath)

#replace the NA element in a matrix with the mean of the observations 
removeNAs=function(group){
	gMean=mean(sapply(group, mean, na.rm=T))
	group[is.na(group)] = gMean
        return(group)
}

#a new variable for the sum of the component questions
#1)according the quesionaire, we know the question 38 covers the columns from 256 to 265
trust_sum = apply(removeNAs(ath[,256:265]), 1, sum) 
summary(trust_sum)
pdf("plot_result_for_trust_sum.pdf")
par(mfrow=c(2,2))
boxplot(trust_sum,notch=T, horizontal=T, main="Boxplot of trust_sum")

hist(trust_sum, freq=F, breaks=seq(10,50,by=1),main="The sum of trust")
lines(density(trust_sum), lwd=2)
lines(density(trust_sum,adj=.5), lwd=1,col="red")
lines(density(trust_sum,adj=2), lwd=1.5, col=4)

qqnorm(trust_sum, main="QQ plot for trust_sum vs Normal_distribution",ylab="trust_sum")
qqline(trust_sum, col=4)
dev.off()




#########How smoking and alcohol usage are associated with trust
##The answers to smoke questions are the columns 455 to 464.
#####deal jump questions for smoke, those non-smoke or don't drink alcohol persons, their daily usage of cigarettes or drinks should be 0 and if they answered the usage of one type of cigarette or drink, the usage of others should be zero.
#for these people have smoked for few years or they smoked recently, they should be smoker.
smoke=ath[,455:464]
summary(smoke)
length(smoke[smoke[,1]==1,1])
#for these 260 nonsmokers, their answers to question 71-74 should also be NA.
#while there are only 252 people didn't answer the question about when did they smoke last time. And only 213 gave no response to the question about "Do you smoke now".
smoke[smoke$smoke_ever==1&!is.na(smoke$smoke_current),4]
#it seems some of those "nonsmoker" smoke recently.

#through observations, I found that the missing values are caused by non-smokers or only answering the type of cigaratte they smoked daily. Therefore, these missing values can be replaced with 0.
smoke_usage=smoke[,5:8]
smoke_usage=replace(smoke_usage, is.na(smoke_usage), 0)
smoke_usage=apply(smoke_usage[,c(1,2,4)], 1, sum)+3*smoke_usage[,3]
cor(trust_sum, smoke_usage)#-0.1734913

#smoke$smoke_ever==2&!is.na(smoke$smoke_ever)&!is.na(smoke$smoke_years)
smoke_years=with(smoke, replace(smoke_years, is.na(smoke_years), 0))
cor(trust_sum, smoke_years)#-0.1683496

cor(smoke_years, smoke_usage)
#0.6946942
pdf("smoke_usage_vs_years.pdf")
plot(smoke_usage~smoke_years, title="The relationship between smoke usage and smoke years")
abline(h=mean(smoke_years), col="red")
abline(v=mean(smoke_usage), col="blue")
abline(lm(smoke_years~smoke_usage), col="green")
dev.off()

#categorize people into smoker or non_smoker group
smoke_ever = with(smoke, factor(smoke_ever, levels=c(1,2), label=c("non_smoker", "smoker")))
t.test(trust_sum[smoke_ever=="smoker"], trust_sum[smoke_ever=="non_smoker"],na.rm=T)
#the test p-value is smaller than 0.05, which shows the smokers have significant different trust values in comparison to these non-smokers
pdf("prop_nonsmoke_smoke.pdf")
plot(prop.table(table(trust_sum, smoke_ever),2)[,1:2], main="The proportaion between non_smoke and smoke")
dev.off()
chisq.test(trust_sum, smoke_ever)#0.2252


#how about the difference among those people used nicotine therapy or not
smoke_rep=with(smoke, factor(smoke_replacement,levels=c(1,2,3,4),label=c("never","past12","quit","other")))
cor(table(trust_sum, smoke_rep))
chisq.test(trust_su, smoke_rep)
#from the test, the other has low correlation with others.
t.test(trust_sum[smoke_rep=="never"], trust_sum[smoke_rep=="other"], na.rm=T) 
t.test(trust_sum[smoke_rep=="past12"], trust_sum[smoke_rep=="other"], na.rm=T)
t.test(trust_sum[smoke_rep=="quit"], trust_sum[smoke_rep=="other"], na.rm=T)
#the replacement is not the factor for the trust_sum
#Therefore, unitl now, there is vairance between smoker and non_smoker, and the trust_sum is correlated with smoker usage


###The answers to alcohol questions include the columns from 465 to 472
#complement the usage of alcohol
alcohol=ath[,465:472]
summary(alcohol)
length(alcohol[alcohol$alco_ever==1,1])

#make a variable for the usage of alcohol over last 7 days
alcohol_usage=ath[,469:472]
alcohol_usage=replace(alcohol_usage, is.na(alcohol_usage), 0)
alcohol_usage=apply(100.0/3*alcohol_usage[,1:2],1,sum)+12*alcohol_usage[,3]+4*alcohol_usage[,4]

#to check whether different amount alcohol usage will have the same trend in the trust to the goverment
cor(trust_su, alcohol_usage)
alco_amt=with(alcohol, replace(alco_amt, is.na(alco_amt), 0))
alco_amt=factor(alco_amt, levels=c(0,1,2,3,4,5), label=c("0", "1/2", "3/4", "5/6", "7/8/9", "10more"))
cor(table(trust_sum, alco_amt))#
chisq.test(trust_sum, alco_amt)#0.1238
cor(table(alcohol_usage, alco_amt))#the different amount has the same pattern of usage

#categorize people into drinking alcoholic beverage or not
alco_ever= with(alcohol, factor(alco_ever,levels=c(1,2),label=c("non_alco","alco")))
t.test(trust_sum[alco_ever=="alco"], trust_sum[alco_ever=="non_alco"], na.rm=T)
#the test value is 0.01491 and thus the persons who drink alcohol or not differ in their trust to the goverment.
#For more drink frequency
alco_freq=with(alcohol, factor(alco_freq, levels=c(1,2,3,4,5), labe=c("never", "montthlyLess", "2/4m", "2/3w", "4w")))
table(trust_sum, alco_freq)
cor(table(trust_sum, alco_freq))

#how does the trust_sum vary between alco and non_alco
trust_alco_prop=prop.table(table(trust_sum,alco_ever),2)
pdf("prop_nonalco_alco.pdf")
plot(trust_alco_prop[,1:2], main="The proportion between non_alco and alco")
dev.off()
#Then we should analysis how about the usage of alco
cor(trust_sum, alcohol_usage)#-0.1289264


# #the p-value for the above test is smaller than 2.2e-16 and the first one has a bigger mean vaule than the second one
#Furtherly, how about the difference among those people with different frequency to have six or more drinks on one accasion 
alco_more=with(alcohol, factor(alco_more, levels=c(1,2,3,4,5), label=c("never", "lessMonthly", "monthly", "weekly", "daily")))
cor(table(trust_sum, alco_more))

#From the correlation result, only these poeple choose daily are different from others.
alco_more[alco_more=="daily"]#there is no person choose daily option. Those person's trust are correlated. Therefore, it seems the different level won't have differ vastly in trust to the goverment.
t.test(trust_sum[alco_more=="never"], trust_sum[alco_more=="lessMonthly"])
#But all the p-values for the t.test are bigger than 0.05. We could not consider the factor 



 
trust_alcohol_smoke=cbind(trust_sum, alcohol_usage, smoke_usage)
pairs( ~ trust_sum + alcohol_usage + smoke_usage, data=trust_alchol_smoke)
cor(trust_alcohol_smoke)
summary(lm(trust_sum~alcohol_usage+smoke_usage))
lmc=lm(trust_sum~alcohol_usage)
lmr=lm(trust_sum~smoke_usage)
mrc=residuals(lmc)
mrs=residuals(lmr)
# diagnostic plots 
plot fitted value vs observed
plot fitted values vs residuals
compare models

#3)confounder
#check whether there is na value in gender
ath[is.na(ath[,"rg_age"]),"rg_age"]
cor(trust_sum, ath$rg_age)

# create the categorical age variable
min(ath$rg_age)
max(ath$rg_age)
age_10=with(ath, cut(rg_age,c(19,seq(20,90,10)),right=FALSE,include.lowest=TRUE))

#tablulate trust_sum and age to see how the trust_sum vs age
cor(table(trust_sum, age_10))
table(age_10, age_10)
age_2=with(ath, cut(rg_age,c(19,70,90),right=FALSE,include.lowest=TRUE))
cor(table(trust_sum, age_2))
t.test(trust_sum[ath$rg_age>=70],trust_sum[ath$rg_age<70])

#change re_gender to categorical gender variable
gender=with(ath, factor(rg_gender, levels=c(1,2), label=c("male", "female")))

#how does the trust_sum vary between men and women
cor(table(trust_sum, gender))
chisq.test(trust_sum, gender)
chisq.test(trust_sum, age_2)

#sleep
sleep=ath[,500:503]
#whether sleep at daytime
sleep_naps=with(sleep, factor(sleep_naps, levels=c(1,2), label=c("y","n")))
cor(table(trust_sum, sleep_naps))
t(trust_sum[sleep_naps=="y"], trust_sum[sleep_naps=="n"], na.rm=T)

sleep_freq=with(sleep, replace(sleep_freq, is.na(sleep_freq), mean(sleep_freq, na.rm=T)))
cor(trust_sum, sleep_freq)
cor(alcohol_usage, sleep_freq)
cor(smoke_usage, sleep_freq)

sleep_engh=with(sleep, factor(sleep_engh, levels=c(1,2,3,4), label=c("1","2","3","4")))
cor(table(trust_sum, sleep_engh))

