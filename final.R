setwd("~/Documents/Harvard/23c/23cfinal")
wine = read.csv("winemag-data-130k-v2.csv")
head(wine)

wine = subset(wine, !is.na(price))

points = wine$points
mu = mean(points)
sig = sd(points)

# Graphical displays:
# barplot of 10 most abundant provinces in sample with ggplot2

library(ggplot2)

ten_wine_table <- sort(table(wine$province), decreasing = TRUE)[1:10]
ten_wine <- subset(wine, province %in% as.vector(as.list(as.data.frame(ten_wine_table)["Var1"])$Var1)); ten_wine
ggplot(as.data.frame(ten_wine_table), aes(x = Var1, y = Freq)) +
  geom_bar(color = "white", fill = "steelblue", stat = "identity") +
  ylab("Frequency") + xlab("") + 
  labs(title = "Top 20 Regions", caption = "Data source: Wine Reviews Database") +
  theme(plot.title = element_text(face = "bold")) + coord_flip()

# violin plot of points in 10 most abundant provinces

five_wine_table <- sort(table(wine$province), decreasing = TRUE)[1:5]
five_wine <- subset(wine, province %in% as.vector(as.list(as.data.frame(five_wine_table)["Var1"])$Var1)); five_wine
ggplot(five_wine, aes(x = province, y = points, fill=province)) + 
  geom_violin(trim = FALSE) + 
  stat_summary(fun.y=median, geom="point", shape=23, size=2) +
  geom_boxplot(width=0.1) 

# histogram: done
hist(points, breaks=20, prob = TRUE, col = "cornflowerblue", xlab = "Points /100", main = "Histogram of Wine Ratings")

# pdf on hist: done
# conting table: done


# Analyses:

# Linear regression and scatter of price and points
# 

# Graphical displays:



##############################
# Permutation Test
# Testing the question: is wine from Southwest France statistically significantly
# different than the population of wines?
region = subset(wine, province == "Southwest France")

# The two have very close means, 88.60745 for SWF and 88.42188 for all wines
region_mean = mean(region$points); region_mean; mu

# However, the number of wines from SWF is large, 1503. Is that enough for
# such a small difference to be significant?
nrow(region)

n = numeric(10000)
for(i in 1:length(n)){
  s = sample(1:nrow(wine), nrow(region))
  n[i] = mean(wine$points[s]) - mu
}
hist(n, breaks = 40, prob = TRUE, main = "Difference of SW France and Population Means", xlab = )
abline(v = region_mean - mu)
abline(v = mu - region_mean)

p = mean(abs(n) > abs(region_mean - mu)); p

# p-value based on a distribution function

# By CLT, sample means should be normally distributed
sampdist_sd = sig/sqrt(nrow(region))
curve(dnorm(x, mean = 0, sd = sampdist_sd), add = TRUE)
p = pnorm(-abs(region_mean - mu), mean = 0, sd = sampdist_sd) + pnorm(abs(region_mean - mu), mean = 0, sd = sampdist_sd, lower.tail = FALSE); p

# The p-value calculations by the simulation method produced (on my specific run)
# a p-value of 0.0082, which is quite close to the classically-calculated p-value
# of 0.00938. Success!


# 95% Confidence interval of SWF points does not contain the population mean
z = abs(qnorm(0.025))
region_mean - z * sampdist_sd; region_mean + z * sampdist_sd 
mu


##############################
# Contingency Table
cali = wine$province == "California"
chard = wine$variety =="Chardonnay"
mean(cali)
mean(chard)
table(chard, cali)

#we do a simulation.
N <- 10000; TC <- numeric(N); 
for (i in 1:N){
  scramble <- sample(chard, length(chard), replace = FALSE) #deal the W-E hands
  TC[i] <- sum(cali&scramble)
}

hist(TC, breaks = 20)
sum(cali&chard) # off the charts, p ~=~ 0

fisher.test(chard,cali, alternative = "g") # p-value < 2.2 * 10 ^ (-16)
# Very significant!

#############################
# Linear regression



# The additional points:
# 2: long data: done
# 5: graphical display different from class: violin plot
# 8: not statistically significant, but is: done
# 14: linreg
# 16: correlation
# 17: chisq
# 18: samp dist
# 20: confidence interval
# 21: CIs
# 22: 2-person group: done




