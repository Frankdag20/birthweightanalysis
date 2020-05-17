# Frank D'Agostino and Jothi Ramaswamy
rm(list = ls())
# Clear console

##################################################################
# Math 23c Term Project

# Based on our work, we believe we qualify for
# the following for additional points:
# 1. Data set with lots of columns (41 different variables)
# 3. One-page doc to discuss ethical issues (collection)
# 4. One-page doc to discuss ethical issues (conclusions)
# 5. Graphical display that is different (binned scatterplot)
# 6. Use of R functions for unique distribution (Poisson)
# 9. Demo of relationship that may be significant but was not (gender vs. birthweight)
# 10. Software-engineering designs (ChiSq functions, organization, etc.)
# 11. Nicely labeled graphics using ggplot (binned scatterplot)
# 14. Use of linear regression
# 19. Graphical display different from class scripts (binned scatterplot)
# 21. Use of quantiles to observe distributions
# 22. Team consists of exactly 2 members

# Our dataset has 50,000 rows and approximately 41 columns of different variables regarding
# birthweights and mother demographics, economiccircumstances, and health conditions. 
# We believe that our dataset qualifies as one with lots of columns, allowing for a 
# plethora of comparisons among different variables.

# We retrieved this dataset from the CDC in the National Vital Statistics System.
# We were made aware of this dataset through Economics 50, Using Big Data to Solve
# Economic and Social problems (a class we both took this semester).
dat <- read.csv(file.choose()); head(dat)
# Should be "natality.csv"

bw <- dat$birthweight
gender <- dat$baby_female
age <- dat$mom_age
gain <- dat$mom_wt_gain
smoke <- dat$mom_use_tobacco
live_birth <- dat$mom_prior_live
math_score <- dat$county_math_testscores
risk <- dat$mom_other_risk
edu <- dat$mom_yrs_educ
vis <- dat$mom_prenatal_visits

data <- data.frame(bw, gender, age, gain, smoke, live_birth, math_score, risk, edu, vis); head(data)

##############################################################

# Define having a low birthweight as having a birthweight below average
avg_bw <- mean(data$bw)
small_wt <- 1*(data$bw < avg_bw); head(small_wt)

data <- data.frame(bw, gender, age, gain, smoke, live_birth, math_score, risk, edu, vis, small_wt)

#barplot (birthweights in groups of 100)
barplot(table(data$bw - data$bw%%100), col = "lightcoral", xlab = "Rounded (down) birthweight", ylab = "Frequency")

#histogram and normal distribution overlay
subset <- data[which(data$age < 32 & data$age > 28),]
hist(subset$bw, col = "lightcoral", xlab = "Rounded (down) birthweight", ylab = "Frequency", breaks = "FD", probability = T)
curve(dnorm(x, mean(subset$bw), sd(subset$bw)/1.45), add = T, col = "deeppink4", lwd = 3)

#contingency table
table(data$gender, data$small_wt)

#demonstrates that most low birthweights are female and most high birthweights are male.
#demonstrates that most male birthweights are high and most female birthweights are low

#Could this demonstrate a significant relationship between gender and birthweight?
#Or is it just a relationship that might have been statistically significant
#but that turns out not to be so?

#calculate overall observed
observed <- mean(bw[gender==0]) - mean(bw[gender==1]); observed

#permutation test
N = 10^4
diffs <- numeric(N)

for (i in 1:N) {
  Gender <- sample(data$gender, 1000)
  MAvg<- sum(data$bw*(Gender==F))/sum(Gender==F)
  FAvg<- sum(data$bw*(Gender==T))/sum(Gender==T)
  diffs[i] <- MAvg - FAvg
}

#histogram of differences
hist(diffs, probability = T, col = "blue", main="Histogram of Differences Between Genders", ylab="Density", xlab="Differences")

#Display avg in distribution
abline(v = observed,col = "lightgreen", lwd=3)

#1 sided p-value
pvalue1t <- (sum(diffs >= observed)+1)/(N+1); pvalue1t

#2 sided p-value
pvalue2t <- 2*pvalue1t; pvalue2t

#The p-value is 0.67, which is greater than 0.5. This means that the probability of a
#difference this extreme is about 67%, which is fairly high. There is not sufficient
#evidence show that the birthweights across genders is different. We thought this was
#different initially, because the contingency table made low birthweights seem more common
#in females. However, we cannot prove that this is sufficiently different. This is an
#example of a relationship that might have been statistically significant but turns 
#out not to be so

#use linear regression to further visualize that there might not be a clear relationship
#between gender and birthweight
bw.lm <- lm(bw~age, data=data);bw.lm;
plot(age, bw)
curve(7.761*x + 3059.580,col = "cyan", lwd=3, add = T)
#as we can see, the line does not accurately model the data well, and there is a lot of error
#the data also seem not to look very linear in the plot.

#A lot of the analysis above demonstrates insufficient evidence for different relationship
#however, we are analyzing so much data here, comparable to a population of individuals,
#that we encounter a lot of noise that probably affects the trends that we are finding
#let's visualize this using a binned scatterplot. This essentially divided the data up into
#20 sections based on their x-values, then takes the average of the x-values and y-values
#in each section. The resulting plot is the plot of all these averages, and is meant to 
#demonstrate a trend in data that might not be too visible due to noise points
library(ggplot2)

#plot without the bins, clearly very noisy
plot(bw, math_score, col = "darkgreen",xlab="Birthweight", ylab="Math Score", main="Plot of Birthweight vs. Math Scores")

#binned scatter plot. Now it is much easier to see a general upward relationship in the
#data
(ggplot(dat, aes(x = birthweight, y = math_score)) + stat_summary_bin(fun.y='mean', bins=25, color='deeppink4', size=1, alpha=0.7, geom='point', na.rm = TRUE)+labs(y= "Math Scores", x = "Birthweight")+ggtitle("Binned Scatter Plot of Birthweight vs. Math Scores"))

##############################################################

# Let's run a permutation test where we only have a sample and know nothing
# about the population, looking at whether the mom smokes tobacco 
# in regards to baby birthweight
nonsmoke_avg <- sum(data$bw*(data$smoke==0))/sum(data$smoke==0); nonsmoke_avg
smoke_avg <- sum(data$bw*(data$smoke==1))/sum(data$smoke==1); smoke_avg
observed_smoke <- nonsmoke_avg - smoke_avg; observed_smoke

# Now replace nonsmokers with a random sample
smoker <- sample(data$smoke); head(smoker)

nonsmoke_avg <- sum(data$bw*(smoker == 0))/sum(smoker == 0); nonsmoke_avg
smoke_avg <- sum(data$bw*(smoker == 1))/sum(smoker == 1); smoke_avg
nonsmoke_avg - smoke_avg

# Repeat this 10000 times
N <- 10000
diffs_smoke <- numeric(N)
for (i in 1:N){
  # Permuted gender column
  smoker <- sample(data$smoke); smoker 
  nonsmoke_avg <- sum(data$bw*(smoker == 0))/sum(smoker == 0); nonsmoke_avg
  smoke_avg <- sum(data$bw*(smoker == 1))/sum(smoker == 1); smoke_avg
  # Same likelihood to be positive or negative
  diffs_smoke[i] <- nonsmoke_avg - smoke_avg
}

mean(diffs_smoke) # Theoretically should be close to zero
hist(diffs_smoke, breaks = "FD", col="purple", probability=TRUE, xlab="Differences in Smoking Mothers", ylab="Density", main="Histogram of Birthweight Differences in Mothers Who Smoke or Not")

# We can display the observed difference on the histogram
# It's not even on the histogram since it is so large
abline(v = observed_smoke, col = "green")

# Calculate the probability that a difference this large
# could have arose from a random sample
pvalue_smoke <- (sum(diffs_smoke >= observed_smoke)+1)/(N+1); pvalue_smoke
# P-value is 9.999e-05. Since the p=value is less than 0.05,
# we reject the null hypothesis since there is a small chance the permuted
# tests were as extreme as the observed differences. Therefore, the evidence
# is sufficient enough against the null hypothesis that the birthweight of
# moms who smoke tobacco vs. moms who do not smoke are the same. This means 
# there is a statistical significance that whether a mom smokes or not impacts birthweight. 
# The probability that a difference this large occurring is extremely unlikely,
# meaning that moms who smoke while their children are in the womb
# has a significant effect on decreasing their child's birthweight

# Now we can run a t-test on this data, by running it for birthweights
# for children with a mother who smoked vs mother who did not smoke
smoked <- subset(data, data$smoke == 1); head(smoked)
no_smoked <- subset(data, data$smoke == 0); head(no_smoked)
t.test(smoked$bw, no_smoked$bw, equal.values = FALSE)
# The built-in t-test for unequal variances gives p-value < 2.2e-16, 
# which means that our classical method agrees with our simulation method 
# of a permutation test. However, the t-test seems to give a much smaller 
# p-value in relation to the permutation test, meaning that the simulation
# methods may be more forgiving in regards to significance.

#####################################################################

# We want to compare birthweights to the normal distribution using quantiles
mu <- mean(data$bw)
sigma <- sd(data$bw)

N <- 2000; xbars <- numeric(N)
for (i in 1:N) xbars[i] <- mean(sample(data$bw,25))
hist(xbars, breaks = "FD", probability = TRUE, col="orange", xlab="Sample Means", main="Histogram of Sample Means")    # Seems normal
# For best fit, compute the mean and variance of the data
mu25 <- mean(xbars); mu25; mu    # It's the same
sig25 <- sd(xbars); sig25; sigma     # Smaller since we used a mean of 25 samples
curve(dnorm(x, mu25, sig25), add = TRUE, col = "red")

# We can compare quantiles
dec <- qnorm(seq(0.0, 1, by = 0.1), mu, sigma); dec # Deciles for normal distribution
# Compare with quantiles for the birthweights
bquant <- quantile(data$bw, seq(0.0, 1, by = 0.1), type = 2); bquant
# Now plot one against the other
plot(dec, bquant, xlim = c(2500, 4100), ylim = c(2500, 4100))
curve(x+1-1, 2500, 4100, add = TRUE)   # Not a great match to a straight line

# Try again, using the sample means instead of individual weights
dec <- qnorm(seq(0.0, 1, by = 0.1), mu25, sig25); dec   # Deciles for normal distribution
bquant <- quantile(xbars, seq(0.0, 1, by = 0.1), type = 2); bquant
# Plot one against the other
plot(dec, bquant, xlim = c(3100, 3500), ylim = c(3100, 3500))
curve(x+1-1, 3100, 3500, add = TRUE)   # Better match to a straight line

#This approach has been automated in R, using lots of quantiles
qqnorm(data$bw)    # Poor match to normal distribution
# Wait a bit, as it takes a while for it to load in the plot
qqnorm(xbars)    # Sample means are good match to normal distribution

###################################################################

# We can create a Chi-squared function for later use
ChiSq <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
}

# Create a table of live births
live_table_orig <- table(data$live_birth); live_table_orig

# We can now see if the number of live births given by a mother previous
# follows a Poisson distribution, since it would make sense for it to follow
# something like a Poisson distribution as the number of births drastically drops
# off beyond 0 to 1 previous births

# We can set lambda = 1.02608 as the mean
lambda <- mean(data$live_birth); lambda
live_table_orig

# We can group any births above 5 and then table it and label it accordingly
end <- sum(live_table_orig[10:15])
live_table <- c(live_table_orig[1:9], end)
names(live_table) <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9); live_table

# Calculate the expected value, and replace the fifth one
# to account for all the other values of live births >= 9
Expected<-50000*dpois(0:8, lambda); Expected[10]<-50000*(1-ppois(8, lambda))
Expected

# Calculate chi-squared value using our function
ChiLive <- ChiSq(live_table,Expected); ChiLive
# There are only three degrees of freedom
# Let's try our ChiSq function and the built-in pchisq function
Pvalue<- pchisq(ChiLive,3,lower.tail = FALSE); Pvalue  
# Since the p-value given is so small that it is practically 0,
# that means there is a significance and we must reject the null hypothesis
# that the distribution of previous live births given by the mothers
# does not follow a Poisson distribution

# We can now try a simulation method
# We simulate data with 50,000 sets each
length(data$live_birth)
N = 10^4-1; result <- numeric(N)
for (i in 1:N){
  expData <- rpois(50000,lambda) # We generate 50000 random samples
  Counts <- numeric(10)
  Counts[1] <- sum(expData ==0) 
  Counts[2] <- sum(expData ==1)
  Counts[3] <- sum(expData ==2)
  Counts[4] <- sum(expData ==3)
  Counts[5] <- sum(expData ==4)
  Counts[6] <- sum(expData ==5)
  Counts[7] <- sum(expData ==6)
  Counts[8] <- sum(expData ==7)
  Counts[9] <- sum(expData ==8)
  Counts[10] <- sum(expData >=9) # 9 or more previous live births
  result[i] <- ChiSq(Counts, Expected)
}

# Plot histogram
hist(result, breaks = "FD", col="yellow", probability = TRUE, xlab="Live Births Value", ylab="Density", main="Histogram of the Live Births Distribution")
curve(dchisq(x, df=3), col = "blue", add = TRUE)    # Not a great fit
abline(v = ChiLive, col = "red")

# Determine mean of how often simulated chi-squared is greater than theoretical chi-squared
mean(result >= ChiLive) 
# Value of 0 means that the simulated chi-squared value was always less than the
# theoretical, suggesting that the fit is not perfect for the Poisson distribution.
# This concurs with our previous theoretical finding, meaning the simulated and 
# classical methods both agree and produce similar results.

########################################################
