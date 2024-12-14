df <- read_excel("Wage by Gender in Common Jobs Air Force Discussion Forum Data.xlsx")
summary(df)
df$`ID Gender`<-as.factor(df$`ID Gender`)
df$`ID PUMS Occupation`<-as.factor(df$`ID PUMS Occupation`)
df$`Degree Level` <- vector(length = length(df$`Degree Type`))
for (i in 1:length(df$`Degree Type`)){
  if (df$`Degree Type`[i] == "High School Degree" | df$`Degree Type`[i] == "High School Degree and Certificate")
  {df$`Degree Level`[i] <- '1'}
  else if (df$`Degree Type`[i] == "Associate's Degree and Certificate" | df$`Degree Type`[i] == "Associate's Degree and/or Certificate")
  {df$`Degree Level`[i] <- '2'}
  else if (df$`Degree Type`[i] == "Bachelor's Degree" | df$`Degree Type`[i] == "Bachelor's Degree and Certification")
  {df$`Degree Level`[i] <- '3'}
  else 
  {df$`Degree Level`[i] <- '4'}
}
# create sample
df_sample<-df[sample(nrow(df),30),]
# calculate moving ranges and average
moving_ranges<-abs(diff(df_sample$`Average Wage`))
mr_bar<-mean(moving_ranges)
# add contstants from control chart table
d2<-1.128
d3<-0
d4<-3.267
# calculate sigma and mean
chart_sd<-mr_bar/d2
chart_mean<-mean(df_sample$`Average Wage`)
# calculate control limits
UCL_I<-chart_mean + (3* chart_sd)
LCL_I<-chart_mean - (3* chart_sd)
UCL_mr<-mr_bar*d4
LCL_mr<-mr_bar*d3
#calculate warning limits
UWL_I<-chart_mean + (2* chart_sd)
LWL_I<-chart_mean - (2* chart_sd)
d3_prime<-0
d4_prime<-2.223
UWL_mr<-mr_bar*d4_prime
LWL_mr<-mr_bar*d3_prime
# plot individuals chart
options(scipen = 999)
buffer<-25000
plot(df_sample$`Average Wage`, type = "o", xlab = " Enlisted Air Force Military-Individuals", ylab = "Salary", ylim=c(0, max(df_sample$`Average Wage`)+buffer))
abline(h=chart_mean, col='blue')
abline(h=UCL_I,col='red')
abline(h=LCL_I,col='red')
abline(h=LWL_I,col='orange')
abline(h=UWL_I,col='orange')
# plot for moving range
plot(moving_ranges, type = "o", xlab = "Points from Sample - Moving Range", ylab = "Ranges between points", ylim = c(0, max(moving_ranges)+buffer))
abline(h=mr_bar, col='blue')
abline(h=UCL_mr,col='red')
abline(h=LCL_mr,col='red')
abline(h=LWL_mr,col='orange')
abline(h=UWL_mr,col='orange')
# qcc tool
library(qcc)
qcc(df_sample$`Average Wage`,type="xbar.one")
