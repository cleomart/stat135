load("/Users/Leomart/Desktop/Stat135/KaiserBabies.rda")
plot(density(infants$bwt), xlab = "Birth Weight (oz)", main = "Male Babies, Oakland Kaiser 1960s")
plot(density(infants$bwt,bw=1), xlab = "Birth Weight (oz)", main = "Male Babies, Oakland Kaiser 1960s")
plot(density(infants$bwt,adjust=0.5), xlab = "Birth Weight (oz)", main = "Male Babies, Oakland Kaiser 1960s")
hist(infants$bwt)
hist(infants$bwt[!infants$smoke=="Now"],breaks=50,col=rgb(0,0,1,.3),
xlab="Birthweight (ounces)",main="Birthweight")
hist(infants$bwt[infants$smoke=="Now"],breaks=50,col=rgb(1,0,0,.3),add=T)
legend(50,40,legend=c("Non-smokers","Smokers"),
fill=c(rgb(0,0,1,.3),rgb(1,0,0,.3)))
mean(infants$bwt)
sd(infants$bwt)
summary(infants$bwt)
boxplot(infants$bwt)
qqnorm(infants$bwt)
qqline(infants$bwt)
qqnorm(infants$wt)
qqline(infants$wt)
X=runif(1000)
qqnorm(X)
qqline(X)
X=rexp(1000)
qqnorm(X)
qqline(X)
X=rnorm(1000)
qqnorm(X)
qqline(X)
set.seed(7)
mysample=sample(na.omit(infants$wt),10)
# Part 1
# 1a
true_average = mean(infants$bwt)
x_bar = mean(mysample)
estimated_se = sd(mysample) / sqrt(length(mysample))
# 95% CI Interval
interval = c(x_bar - 1.96*estimated_se, x_bar + 1.96*estimated_se)
interval
# 1b
# creates 1000 95% Confidence Interval
thousand_interval = c()
thousand_averages = c()
num_interval = 0
for (i in 1:1000)
{
sample = sample(na.omit(infants$wt),10)
std_error = sd(sample) / sqrt(length(sample))
thousand_averages = c(thousand_averages, mean(sample))
ci_interval = c(mean(sample) - 1.96*std_error, mean(sample) + 1.96*std_error )
thousand_interval = c(thousand_interval, ci_interval)
if(ci_interval[1] <= true_average & ci_interval[2] >= true_average){
num_interval = num_interval +1
}
}
# I expect 95% (950 intervals) of the intervals cover the true average
cat("I expect 95% (950 intervals) of the intervals cover the true average")
# The number of 95% CI that has true average is in num_interval
cat("The number of 95% CI that has true average is", num_interval)
# 1c
sd_averages = sd(thousand_averages)
sd_averages
estimated_se
cat('The SD of the sample averages, ' , sd_averages, ', is not very close to the estimated standard error, ',
estimated_se)
hist(thousand_averages)
qqnorm(thousand_averages)
qqline(thousand_averages)
cat('As we can see in the histogram, the shape looks very close to a bell shaped curve, normal distribution.
The qq plot shows that most of the points are in the line, so the sample average follows the normal curve pretty closely.
')
# Part 2
# 2a
bootStrap = function(mySample, popSize = NULL, B = 1000, repl = FALSE){
if (repl) {
# Bootstrap should be done the same way as original sample, usually without rep
return(replicate(B, mean(sample(mySample, length(mySample), TRUE))))
} else {
vals = sort(unique(mySample))
counts = table(mySample)
# makes the bootstrap pop as rounded version of sample, not quite right
bootPop = rep(vals, round(counts * popSize / length(mySample)))
return(list(bootPop,
bootSamps = replicate(B,mean(sample(bootPop, length(mySample), FALSE))))
)
}
}
bootstrap_averages =  bootStrap(mysample, 10, repl = TRUE)
bootstrap_averages
hist(bootstrap_averages)
abline(v=x_bar,col="red")
bootstrap_sd = sd(bootstrap_averages)
cat('The SD of the sample averages from using bootstrap, ' , bootstrap_sd, ', is very close to the estimated standard error, ',
estimated_se)
# 2b
quantile(bootstrap_averages, probs = c(0.025, 0.975))
ci_interval
cat("The 95% confidence interval from the bootstrap is closer to the 95% confidence interval of the bootstrap")
# Part 3
set.seed(7)
mysample=sample(na.omit(infants$wt),100)
mysample
# 1a
true_average = mean(infants$bwt)
x_bar = mean(mysample)
estimated_se = sd(mysample) / sqrt(length(mysample))
# 95% CI Interval
interval = c(x_bar - 1.96*estimated_se, x_bar + 1.96*estimated_se)
interval
# 1b
# creates 1000 95% Confidence Interval
thousand_interval = c()
thousand_averages = c()
num_interval = 0
for (i in 1:1000)
{
sample = sample(na.omit(infants$wt),100)
std_error = sd(sample) / sqrt(length(sample))
thousand_averages = c(thousand_averages, mean(sample))
ci_interval = c(mean(sample) - 1.96*std_error, mean(sample) + 1.96*std_error )
thousand_interval = c(thousand_interval, ci_interval)
if(ci_interval[1] <= true_average & ci_interval[2] >= true_average){
num_interval = num_interval +1
}
}
# I expect 95% (950 intervals) of the intervals cover the true average
cat("I expect 95% (950 intervals) of the intervals cover the true average")
# The number of 95% CI that has true average is in num_interval
cat("The number of 95% CI that has true average is", num_interval)
x_bar
thousand_averages
# 1c
sd_averages = sd(thousand_averages)
sd_averages
estimated_se
cat('The SD of the sample averages, ' , sd_averages, ', is very close to the estimated standard error, ',
estimated_se)
hist(thousand_averages)
qqnorm(thousand_averages)
qqline(thousand_averages)
cat('As we can see in the histogram, the shape looks very close to a bell shaped curve, normal distribution.
The qq plot shows that most of the points are in the line, so the sample average follows the normal curve pretty closely.')
# Part 2
# 2a
bootstrap_averages
hist(bootstrap_averages)
abline(v=x_bar,col="red")
bootstrap_sd = sd(bootstrap_averages)
cat('The SD of the sample averages from using bootstrap, ' , bootstrap_sd, ', is very close to the estimated standard error, ',
estimated_se)
# 2b
quantile(bootstrap_averages, probs = c(0.025, 0.975))
ci_interval
cat("The 95% confidence interval from the bootstrap is closer to the 95% confidence interval of the bootstrap")
