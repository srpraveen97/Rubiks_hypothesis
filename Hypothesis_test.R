

library(ggplot2)

n_data <- read.csv("D:/Personal_projects/Rubik's_cube/N_data.csv")

x_n <- n_data$Best_time

mu0 <- 60

n <- length(x_n)
xbar_n <- mean(x_n)
s_n <- sqrt(var(x_n))

df <- n-1
alpha <- 0.05

xaxis <- seq(-5,5,length=10^4)
t_dist <- dt(x = seq(-5,5,length=10^4),df=df)
df_dist <- data.frame(xaxis,t_dist)


#------------------------------Two-sided Test----------------------------------#
 

T_stat <- (xbar_n - mu0)/(s_n/sqrt(n))

T_cr <- qt(1-alpha/2,df)
T_cl <- qt(alpha/2,df)

if (T_stat > T_cr | T_stat < T_cl) {
  print("Reject H0 at alpha = 0.05")
}

p_val_two <- 2*(1 - pt(T_stat,df))

sprintf("The p-value is %f.",p_val_two)

ci_2_ll <- xbar_n - T_cr*s_n/sqrt(n)
ci_2_ul <- xbar_n - T_cl*s_n/sqrt(n)

sprintf("The true mean lies in the interval [%.2f %.2f] with a 95 percent confidence level.",ci_2_ll,ci_2_ul)

p1 <- ggplot(data=df_dist,aes(xaxis,t_dist))

p1 + geom_area(data = subset(df_dist,xaxis<T_cl | xaxis>T_cr), fill = "lightgray") +
  geom_area(data = subset(df_dist,xaxis>T_cl & xaxis<T_cr), fill = "lightblue") +
  geom_line(color="black") 



#----------------------------Upper-tailed Test---------------------------------#

T_crit <- qt(1-alpha,df)

if (T_stat > T_crit) {
  print("Reject H0 at alpha = 0.05")
}

p_val_upper <- 1 - pt(T_stat,df)

ci_ul <- xbar_n - (T_crit*s_n)/(sqrt(n))

sprintf("The true mean lies in the interval [%.2f inf] with a 95 percent confidence level",ci_ul)


#--------------------------Inbuilt t.test function-----------------------------#


two_sided <- t.test(x = x_n,alternative = 'two.sided',mu = 60)
two_sided
ggttest(two_sided)
upper_tailed <- t.test(x = x_n, alternative = 'greater',mu = 60)
upper_tailed
ggttest(upper_tailed)


#-------------------------Proportion of Variance Test--------------------------#


p_data <- read.csv("D:/Personal_projects/Rubik's_cube/P_data.csv")

x_p <- p_data$Best_time

xbar_p <- mean(x_p)
s_p <- sqrt(var(x_p))

F_stat <- (s_n)^2/(s_p)^2

F_cr <- qf(alpha,df,df)

if (F_stat < F_cr) {
  print("Reject H0 at alpha = 0.05")
}



#---------------------------Difference in Mean Test----------------------------#

del0 <- 0
denom <- sqrt((s_n^2 + s_p^2)/n)

T_stat_diff <- ((xbar_n - xbar_p) - del0)/denom

df_np <- round((denom^4)/((s_n^4+s_p^4)/(n^2*(n+1))))

T_cr_diff <- qt(alpha,df_np)

if (T_stat_diff < T_cr_diff) {
  print("Reject H0 at alpha = 0.05")
}

ci_ll_diff <- (xbar_n - xbar_p) - T_cr_diff*denom

sprintf("The true difference in the mean lies in the interval [-inf %.2f] with 95 percent confidence level",ci_ll_diff)


#--------------------------Inbuilt t.test function-----------------------------#



