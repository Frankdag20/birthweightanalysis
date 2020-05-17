# Introduction

## Dataset
The dataset we obtained is regarding the births of 50,000 mothers in the United States. There are 41 different columns in the dataset that can be analyzed. The dataset was collected by the CDC and is available in the National Vital Statistics System. We were made aware of this dataset through our Economics 50 class.

## Analysis
The most important column in this dataset is the column representing the birthweight of each observation. In our analysis, we are trying to model the birthweight with different distributions and run different tests and other analyses (linear regression, different visualizations, etc) to demonstrate the relationship between birthweight and other characteristics (age, mother's math scores/education, baby gender, etc). One thing that we encountered in our analysis is the issue that some of our results are inconclusive and insignificant. We think this is because our dataset is so large that there are multiple noise points that are hurting the results of our analysis and making them yield insignificant conclusions. Many of the analyses were done with significant aid and inspiration from Paul's scripts and videos. We would like to thank him for all of this help throughout the semester and on this project.

# Code

```{r eval=TRUE}
#load data
dat <- read.csv("natality.csv")

#define having a low birthweight as having a birthweight below average
avg_bw <- mean(dat$birthweight)
small_wt <- 1*(dat$birthweight < avg_bw); head(small_wt)

#create columns from the dataset
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

#from these columns, create the dataframe that we will specifically be looking at
data <- data.frame(bw, gender, age, gain, smoke, live_birth, 
                   math_score, risk, edu, vis); head(data)
```

# Visualizations (barplot, histogram)
```{r eval=TRUE}

#Let's start off by creating some visualizations. Let's look at the distribution of
#birthweights through a barplot. Since there are many observations in this dataset,
#this is probably going to look really scattered and messy

barplot(bw, col = "lightcoral", xlab = "Observation", ylab = "Birthweight", main="Distribution of Birthweight")
```

```{r eval=TRUE}
# Now let's create a histogram. Take the specific portion of the dataset containing
# mothers between the ages of 28 and 32, and plot the distribution of their
# birthweights. We will also lay a normal distribution curve over it to see if it fits

subset <- dat[which(age < 32 & age > 28),]
hist(subset$birthweight, col = "lightcoral", xlab = "Birthweight", ylab = "Frequency", main="Histogram of Birthweights", breaks = "FD", probability = T)
curve(dnorm(x, mean(subset$birthweight), sd(subset$birthweight)/1.45), add = T, col = "deeppink4", lwd = 3) #the curve shown can potentially be a good fit
```

# Using Quantiles to Compare to Normal Distribution

```{r eval=FALSE}
# We want to compare birthweights to the normal distribution using quantiles
mu <- mean(data$bw)
sigma <- sd(data$bw)

N <- 2000; xbars <- numeric(N)
for (i in 1:N) xbars[i] <- mean(sample(data$bw,25))
hist(xbars, breaks = "FD", probability = TRUE, col="orange", xlab="Sample means", main="Histogram of Sample Means")    # Seems normal
# For best fit, compute the mean and variance of the data
mu25 <- mean(xbars); mu25; mu    # It's the same
sig25 <- sd(xbars); sig25; sigma     # Smaller since we used a mean of 25 samples
curve(dnorm(x, mu25, sig25), add = TRUE, col = "red")

# We can compare quantiles
dec <- qnorm(seq(0.0, 1, by = 0.1), mu, sigma); dec # Deciles for normal distribution
# Compare with quantiles for the baby birthweights
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
```

# Analysis

Clearly, the sample means from the birthweight dataset fit much better to the normal distribution. That means that while the individual birthweights do not necessarily follow a normal distribution, the sample means on average follow a normal distribution quite well.

# Contingency Table
```{r eval=TRUE}

#Let's look at a contingency table now between the baby's gender and whether the
#birthweight is below average
table(gender, small_wt)

```
  
# Analysis: 
This table demonstrates that most low birthweights are female and most high birthweights are male. It also demonstrates that most male birthweights are high and most female birthweights are low. Just by looking at this table, you might think that male births are related to higher weights and female births are related to lower weights. 

Could this demonstrate a significant relationship between gender and birthweight? Or is it just a relationship that might have been statistically significant but that turns out not to be so? Let's do a permutation test to find out

```{r eval=TRUE}

#calculate overall observed difference between male and female birthweights
observed <- mean(bw[gender==1]) - mean(bw[gender==0]); observed

#permutation test
N = 10^4
diffs <- numeric(N)

for (i in 1:N) {
  G <- sample(data$gender, 1000)
  MAvg<- sum(bw*(G==0))/sum(G==0)
  FAvg<- sum(bw*(G==1))/sum(G==1)
  diffs[i] <- MAvg - FAvg
}

#histogram of differences
hist(diffs, probability = T, col = "blue")

#Display avg in distribution
abline(v = observed,col = "lightgreen", lwd=3)

#1 sided p-value
pvalue1t <- (sum(diffs <= observed)+1)/(N+1); pvalue1t

#2 sided p-value
pvalue2t <- 2*pvalue1t; pvalue2t
```

The two sided p-value is greater than 0.05. This means that the probability of a difference this extreme is about 67%, which is fairly high. There is not sufficient evidence show that the birthweights across genders is different. We thought this was different initially, because the contingency table made low birthweights seem more common in females. However, we cannot prove that this is sufficiently different. This is an example of a relationship that might have been statistically significant but turns out not to be so.

Let's use linear regression to further demonstrate that there might not be a clear relationship between gender and birthweight

# Linear Regression

```{r eval=TRUE}

#create linear model with lm and overlay the curve of the predicted line over the
#plot of age and birthweight
bw.lm <- lm(bw~age, data =data); bw.lm;
plot(age, bw)
curve(7.761*x + 3059.580,col = "cyan", lwd=3, add = T)

```

As we can see, the line does not accurately model the data well, and there is a lot of error the data also seem not to look very linear in the plot.

# More visualizations

A lot of the analysis above demonstrates insufficient evidence for different relationship. However, we are analyzing so much data here, comparable to a population of individuals, that we encounter a lot of noise that probably affects the trends that we are finding let's visualize this using a binned scatterplot. This essentially divided the data up into 20 sections based on their x-values, then takes the average of the x-values and y-values in each section. The resulting plot is the plot of all these averages, and is meant to demonstrate a trend in data that might not be too visible due to noise points.

```{r eval=TRUE}

library(ggplot2)

#plot without the bins, clearly very noisy
plot(bw, math_score, col = "darkgreen", xlab="Birthweight", ylab="Math Score", main="Plot of Birthweight vs. Math Scores")
```

```{r eval=TRUE}
#binned scatter plot
(ggplot(dat, aes(x = bw, y = math_score)) + stat_summary_bin(fun.y='mean', bins=25,     color='deeppink4', size=1, alpha=0.7, geom='point', na.rm = TRUE)+labs(y= "Math Scores", x = "Birthweight")+ggtitle("Binned Scatter Plot of Birthweight vs. Math Scores"))

```

With the binned scatter plot, it is clearly much easier to see the trends in the data, compared to without the binned scatter plot. This demonstrates that our findings may be skewed by the effect of multiple noise points in our data.

As we can see, the line does not accurately model the data well, and there is a lot of error the data also seem not to look very linear in the plot.

# Permutation Test

```{r eval=TRUE}

# Let's run another permutation test where we only have a sample and know nothing
# about the population, looking at whether the mom smoking tobacco has an impact
# in regards to baby birthweight
nonsmoke_avg <- sum(data$bw*(data$smoke==0))/sum(data$smoke==0); nonsmoke_avg
smoke_avg <- sum(data$bw*(data$smoke==1))/sum(data$smoke==1); smoke_avg
observed_smoke <- nonsmoke_avg - smoke_avg; observed_smoke

# Permute different scenarios 10000 times
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

pvalue_smoke <- (sum(diffs_smoke >= observed_smoke)+1)/(N+1); pvalue_smoke
```

# Analysis

P-value is 9.999e-05. Since the p=value is less than 0.05, we reject the null hypothesis since there is a small chance the permuted tests were as extreme as the observed differences. Therefore, the evidence is sufficient enough against the null hypothesis that the birthweight of moms who smoke tobacco vs. moms who do not smoke are the same. This means there is a statistical significance that whether a mom smokes or not impacts birthweight. The probability that a difference this large occurring is extremely unlikely, meaning that moms who smoke while their children are in the womb has a significant effect on decreasing their child's birthweight.


# Compare simulation method (permutation test) with classical method (t-test)

```{r eval=TRUE}
# Now we can run a t-test on this data, by running it for birthweights
# for children with a mother who smoked vs mother who did not smoke
smoked <- subset(data, data$smoke == 1); head(smoked)
no_smoked <- subset(data, data$smoke == 0); head(no_smoked)
t.test(smoked$bw, no_smoked$bw, equal.values = FALSE)

```

# Analysis

The built-in t-test for unequal variances gives p-value < 2.2e-16, which means that our classical method agrees with our simulation method of a permutation test. However, the t-test seems to give a much smaller p-value in relation to the permutation test, meaning that the simulation methods may be more forgiving in regards to significance, and more lenient in terms of assigning significance.


# Using R functions for Poisson Distribution

```{r eval=TRUE}
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

```

# Analysis

We have shown that although it may seem that the number of previous live births had by a mother may follow a Poisson distribution, we have shown that this is actually not the case. We rejected the null hypothesis that the data followed a Poisson distribution as a result, using simulation and theoretical methods. We also created a chi-squared function for easier use.
