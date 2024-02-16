#IMPORT DATASET

createsample('#######')
head(mysample)
save(mysample, file = "mysample.RData")
mysample['Breed']
table(mysample$Breed)

#LOAD NECESSARY LIBRARIES

library(dplyr)
library(tidyr)
library(ggplot2)
install.packages("MASS")
library(MASS)
citation("tidyr")

#DATA EXPLORATION

copy1 <- mysample
copy1[!is.na(copy1) , ]
nrow(copy1[(copy1$Rehomed == 99999 | is.na(copy1$Breed)), ])
copy1[(copy1$Rehomed == 99999),]
table(mysample$Breed)
head(copy1)

table(copy1$Reason)

#Number of null data in Breed -> 6
#Number of null data in Rehomed -> 9
#Total null data -> 15

initial_obs <- nrow(mysample)
final_obs <- nrow(copy1[(copy1$Rehomed == 99999 | is.na(copy1$Breed)), ])
percentage_removed <- (final_obs / initial_obs) * 100
percentage_removed
#percentage of removed data -> 9.74%

#REMOVE NULL DATA

df <- copy1[!(copy1$Rehomed == 99999 | is.na(copy1$Breed)), ]

#DATA SUMMARY

colnames(df)
unique(df['Age'])
table(df['Age'])
table(df["Reason"])
table(df['Returned'])
table(df['Health'])

df['Health Comp'] <- ifelse(df['Health'] <=50, 0 , 1)
table(df['Health Comp'])

#Rotweiller
table(rottweiler_data['Age'])
table(rottweiler_data["Reason"])
table(rottweiler_data['Returned'])

#Shih Tzu
table(shih_tzu_data['Age'])
table(shih_tzu_data["Reason"])
table(shih_tzu_data['Returned'])

#Terrier
table(terrier_data['Age'])
table(terrier_data["Reason"])
table(terrier_data['Returned'])

df["Age Num"] <- ifelse (df['Age'] == 'Fully grown', 1, 0)

#SUMMARY TABLE

cleaned_data <- df 

summary_table <- cleaned_data %>%
  group_by(Breed) %>%
  summarise(
    Mean_Health = mean(Health),
    Mean_First_Visit = mean(Visited),
    Mean_Rehoming_Time = mean(Rehomed),
    SD_Rehoming_Time = sd(Rehomed)
  )
print(summary_table)

# BOXPLOT

ggplot(cleaned_data, aes(x = Breed, y = Rehomed)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Comparison of Rehoming Time by Breed",
       x = "Breed",
       y = "Rehoming Time") +
  theme_minimal()

ggplot(sampled_data, aes(x = Breed, y = Visited)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Comparison of First Visited Time by Breed",
       x = "Breed",
       y = "First Visit Time") +
  theme_minimal()

unique(df['Age'])

#DATA SPLIT

rottweiler_data <- df[df$Breed == "Rottweiler", ]
shih_tzu_data <- df[df$Breed == "Shih Tzu", ]
terrier_data <- df[df$Breed == "West Highland White Terrier", ]

summary(rottweiler_data)
summary(shih_tzu_data)
summary(terrier_data)

#Anomaly in rot data - visited -> -1 count 1
#From summary, rottweiler has lower visited and rehomed time, and a higher mean health
#Lets try to do with sampled data
#visited and rehomed is linear. 
#Rotweiller < Shih Tzu < Terrier

#HIST OF DATA

par(mfrow = c(1,3))

hist(rottweiler_data$Rehomed,prob = T,col = 'lightblue1',xlab = 'Rehoming Time of Rottweilers',main = NULL)
lines(density(rottweiler_data$Rehomed),col = 'red',lwd = 1.5)
hist(shih_tzu_data$Rehomed,prob = T,col = 'lightblue1',xlab = 'Rehoming Time of Shih Tzu',main = NULL)
lines(density(shih_tzu_data$Rehomed),col = 'red',lwd = 1.5)
mtext("Histogram Plots for Rehoming Times of Each Dog Breed", side = 3, line = 0.5)
hist(terrier_data$Rehomed,prob = T,col = 'lightblue1',xlab = 'Rehoming Time of White Terriers',main = NULL)
lines(density(terrier_data$Rehomed),col = 'red',lwd = 1.5)

#EVALUATION USING SHAPIRO

shapiro.test(rottweiler_data$Rehomed)
shapiro.test(shih_tzu_data$Rehomed)
shapiro.test(terrier_data$Rehomed)

# Create Q-Q plot 
par(mfrow = c(2,3))

exponential_fit <- fitdistr(rottweiler_data$Rehomed, "exponential")
theoretical_quantiles <- qexp(ppoints(length(rottweiler_data$Rehomed)), rate = exponential_fit$estimate["rate"])

qqplot(rottweiler_data$Rehomed, theoretical_quantiles, main = "Rottweiler",xlab = 'Sample Quantiles', ylab = 'Theoretical Quantiles')
abline(0, 1, col = "red")

exponential_fit <- fitdistr(shih_tzu_data$Rehomed, "exponential")
theoretical_quantiles <- qexp(ppoints(length(shih_tzu_data$Rehomed)), rate = exponential_fit$estimate["rate"])

qqplot(shih_tzu_data$Rehomed, theoretical_quantiles, main = "Shih Tzu",xlab = 'Sample Quantiles', ylab = 'Theoretical Quantiles')
abline(0, 1, col = "red")
mtext("Q-Q Plots of Exponential Distribution", side = 3, line = 3)

exponential_fit <- fitdistr(terrier_data$Rehomed, "exponential")
theoretical_quantiles <- qexp(ppoints(length(terrier_data$Rehomed)), rate = exponential_fit$estimate["rate"])

qqplot(terrier_data$Rehomed, theoretical_quantiles, main = "White Terrier",xlab = 'Sample Quantiles', ylab = 'Theoretical Quantiles')
abline(0, 1, col = "red")

qqnorm(rottweiler_data$Rehomed,main = "Rottweiler")
qqline(rottweiler_data$Rehomed, col = "red")

qqnorm(shih_tzu_data$Rehomed,main = 'Shih Tzu')
qqline(shih_tzu_data$Rehomed, col = "red")
mtext("Q-Q Plots of Normal Distribution", side = 3, line = 3)

qqnorm(terrier_data$Rehomed,main = 'White Terrier')
qqline(terrier_data$Rehomed, col = "red")

#KS TEST FOR NORMALITY

ks.test(x = rottweiler_data$Rehomed, y = 'pnorm')
ks.test(x = shih_tzu_data$Rehomed, y = 'pnorm')
ks.test(x = terrier_data$Rehomed, y = 'pnorm')

#KS TEST FOR EXPONENTIALITY

ks.test(rottweiler_data$Rehomed, "pexp", rate = exponential_fit$estimate["rate"])
ks.test(shih_tzu_data$Rehomed, "pexp", rate = exponential_fit$estimate["rate"])
ks.test(terrier_data$Rehomed, "pexp", rate = exponential_fit$estimate["rate"])

#LOG TRANSFORMED DATA

log_rot <- log(rottweiler_data$Rehomed)
log_shih <- log(shih_tzu_data$Rehomed)
log_ter <- log(terrier_data$Rehomed)

sqrt_rehoming_time <- sqrt(rottweiler_data$Rehomed)

#HIST OF LOG TRANSFORMED DATA

hist(log_rot,prob = T,col = 'lightblue1',xlab = 'Rehoming Time of Rottweilers',cex = 1.5,main = NULL)
lines(density(log_rot),col = 'red',lwd = 1.5)
hist(log_shih,prob = T,col = 'lightblue1',xlab = 'Rehoming Time of Shih Tzu',main = NULL)
lines(density(log_shih),col = 'red',lwd = 1.5)
mtext("Histogram Plots for Rehoming Times After Transformation", side = 3, line = 0.5)
hist(log_ter,prob = T,col = 'lightblue1',xlab = 'Rehoming Time of White Terriers',main = NULL)
lines(density(log_ter),col = 'red',lwd = 1.5)

#QQ PLOT OF LOG TRANSFORMED DATA

par(mfrow = c(2,3))

qqnorm(log_rot,main = "Rottweiler")
qqline(log_rot, col = "red")

qqnorm(log_shih,main = 'Shih Tzu')
qqline(log_shih, col = "red")
mtext("Q-Q Plots of Normal Distribution", side = 3, line = 3)

qqnorm(log_ter,main = 'White Terrier')
qqline(log_ter, col = "red")

exponential_fit <- fitdistr(log_rot, "exponential")
theoretical_quantiles <- qexp(ppoints(length(log_rot)), rate = exponential_fit$estimate["rate"])

qqplot(log_rot, theoretical_quantiles, main = "Rottweiler",xlab = 'Sample Quantiles', ylab = 'Theoretical Quantiles')
abline(0, 1, col = "red")

exponential_fit <- fitdistr(log_shih, "exponential")
theoretical_quantiles <- qexp(ppoints(length(log_shih)), rate = exponential_fit$estimate["rate"])

qqplot(log_shih, theoretical_quantiles, main = "Shih Tzu",xlab = 'Sample Quantiles', ylab = 'Theoretical Quantiles')
abline(0, 1, col = "red")
mtext("Q-Q Plots of Exponential Distribution", side = 3, line = 3)

exponential_fit <- fitdistr(log_ter, "exponential")
theoretical_quantiles <- qexp(ppoints(length(log_ter)), rate = exponential_fit$estimate["rate"])

qqplot(log_ter, theoretical_quantiles, main = "White Terrier",xlab = 'Sample Quantiles', ylab = 'Theoretical Quantiles')
abline(0, 1, col = "red")


# KS TEST WITH LOG TRANSFORMED DATA

ks.test(log_rot,"pnorm")
ks.test(log_shih,"pnorm")
ks.test(log_ter,"pnorm")
ks.test(log_rot, "pexp", rate = exponential_fit$estimate["rate"])
ks.test(log_shih, "pexp", rate = exponential_fit$estimate["rate"])
ks.test(log_ter, "pexp", rate = exponential_fit$estimate["rate"])

#PARAMETERS OF DATA

nrow(rottweiler_data)
sd(rottweiler_data$Rehomed)
var(rottweiler_data$Rehomed)

nrow(shih_tzu_data)
sd(shih_tzu_data$Rehomed)
var(shih_tzu_data$Rehomed)

nrow(terrier_data)
sd(terrier_data$Rehomed)
var(terrier_data$Rehomed)

table(terrier_data['Age'])
table(df['Reason'])


##################################################################################################################################
lambda_hat <- 1/mean(rottweiler_data$Rehomed)

x<- terrier_data$Rehomed
plot(x, dnorm(x, mean = mean(terrier_data$Rehomed), sd = sd(terrier_data$Rehomed)), col = "blue")

hist(rottweiler_data$Rehomed, freq = FALSE)  
x <- seq(from = min(rottweiler_data$Rehomed), to = max(rottweiler_data$Rehomed), by = 0.1) 
lines(x, dnorm(x, mean = mean(rottweiler_data$Rehomed), sd = sd(rottweiler_data$Rehomed)), lwd = 1.5, col = "blue")

hist(shih_tzu_data$Rehomed, freq = FALSE)  
x <- seq(from = min(shih_tzu_data$Rehomed), to = max(shih_tzu_data$Rehomed), by = 0.1) 
lines(x, dnorm(x, mean = mean(shih_tzu_data$Rehomed), sd = sd(shih_tzu_data$Rehomed)), lwd = 1.5, col = "blue")

hist(terrier_data$Rehomed, freq = FALSE)  
x <- seq(from = min(terrier_data$Rehomed), to = max(terrier_data$Rehomed), by = 0.1) 
lines(x, dnorm(x, mean = mean(terrier_data$Rehomed), sd = sd(terrier_data$Rehomed)), lwd = 1.5, col = "blue")

#########################################################################################################################
# CONFIDENCE INTERVAL
# T-Test

X <- shih_tzu_data$Rehomed 
X_bar <- mean(X)  
S2 <- var(X)
n <- length(X)

int <- qt(p = 0.975, df = 92)
lower_value = (X_bar) - (int*(sqrt(S2/n))) 
upper_value = (X_bar) + (int*(sqrt(S2/n)))

cat(c(lower_value,upper_value))  

X <- terrier_data$Rehomed 
X_bar <- mean(X)  
S2 <- var(X)
n <- length(X)

int <- qt(p = 0.975, df = 92)
lower_value = (X_bar) - (int*(sqrt(S2/n))) 
upper_value = (X_bar) + (int*(sqrt(S2/n)))

cat(c(lower_value,upper_value))
  
#Z TEST FOR ROT

alpha <- 0.05
qnorm(p = 1 - alpha/2, mean = 0, sd = 1)

X <- rottweiler_data$Rehomed 
X_bar <- mean(X)  
S2 <- var(X)
n <- length(X)

lower_value = (X_bar) - (1.96*(sqrt(S2/n))) 
upper_value = (X_bar) + (1.96*(sqrt(S2/n)))
cat(c(lower_value,upper_value))

# T TESTS FOR SHIH TZU AND TERRIER
t.test(shih_tzu_data$Rehomed, alternative = "two.sided", mu = 27, conf.level = 0.95)
t.test(terrier_data$Rehomed, alternative = "two.sided", mu = 27, conf.level = 0.95)

# TWO SAMPLE T TEST
t.test(shih_tzu_data$Rehomed,rottweiler_data$Rehomed,alternative = "two.sided",conf.level = 0.95)
t.test(terrier_data$Rehomed,rottweiler_data$Rehomed,alternative = "two.sided",conf.level = 0.95)
t.test(terrier_data$Rehomed,shih_tzu_data$Rehomed,alternative = "two.sided",conf.level = 0.95)

# FOREST PLOT

analysis = c("Rottweiler", 
             "Shih Tzu", 
             "West Highland\n White Terrier")

estimate  =  c(10.505, 19.500, 20.227)             
upper     =  c(11.76704, 23.07847, 24.50961)
lower     =  c(9.243712, 15.92153, 15.94494)
pval      =  c(2.2e-16,0.0003755,0.004932)

par(mar = c(6,8,1,11))

plot(x = 0,                                 
     xlim = c(5, 30), ylim=c(0, 3),        
     type = "n", xaxt = "n", yaxt="n",       
     ann = FALSE,   
     bty="n")                                
axis(side = 1, cex.axis = 1)
mtext("Estimated Mean Rehoming Times of each dog Breeds", 
      side = 1, line = 4) 

for(i in c(5, 10, 15, 20, 25, 30)){
  
  lines(c(i, i), c(0, 5), lty = 2, col = "gray53")
  
}

verticalpos = 1:3

mtext(text = analysis,  at = verticalpos, 
      side = 2, line = 7, outer = FALSE, las = 1, adj = 0)

points(estimate, verticalpos, pch = 16)

for(i in 1:3 ){
  
  lines(c(lower[i], upper[i]), c(verticalpos[i], verticalpos[i]))
  
  lines(c(lower[i], lower[i]), c(verticalpos[i] + 0.2, verticalpos[i] - 0.2))
  
  lines(c(upper[i], upper[i]), c(verticalpos[i] + 0.2, verticalpos[i] - 0.2))
  
}

est <- formatC(estimate, format='f', digits = 0)
P <- formatC(pval , format = 'f', digits = 5) 
pval <- paste("p =", P) 
pval
P
L <- formatC(lower, format = 'f', digits = 0)
U <- formatC(upper, format = 'f', digits = 0)

interval <- paste("(", L, ", ", U, "),", sep = "")   # Type interval to check.

results <- paste(est, interval, pval)
mtext(text = results, at = verticalpos, 
      side = 4, line = 10, outer = FALSE, las = 1, adj = 1)



