# df setup
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
df_sample<-df[sample(1:nrow(df),30),]
summary(df_sample$`Average Wage`)
summary(df$`Average Wage`)
df_mean<-mean(df$`Average Wage`)
df_sd<-sd(df$`Average Wage`)
sample_sd<-sd(df_sample$`Average Wage`)
sample_mean<-mean(df_sample$`Average Wage`)
# Part 1
pnorm(48010,48011,sd=sd(df$`Average Wage`))
plot_ly(x=df$`Average Wage`,type = "histogram")
cdf_df_aw<-ecdf(df$`Average Wage`)
cdf_df_aw(48010)
# part 2
round(length(df_sample$`Average Wage`[df_sample$`Average Wage` > sample_mean + 5000 ])/length(df_sample$`Average Wage`[df_sample$`Average Wage` > df_mean]),4)
bonus_prob <- round(length(df_sample$`Average Wage`[df_sample$`Average Wage` > sample_mean + 5000 ])/length(df_sample$`Average Wage`[df_sample$`Average Wage` > df_mean]),4)
# part 3
bach_prob <- round(length(df_sample$`Average Wage`[df_sample$`Average Wage` < 51000 & df_sample$`Average Wage` > 45000 & df_sample$`Degree Level` == 3])/length(df_sample$`Average Wage`[df_sample$`Average Wage` > 45000 & df_sample$`Degree Level` == 3]),4)
# part 4
# create table of quantile values
prompt_percentiles <- c(.25,.5,.75,.90,.95)
quan_table<-quantile(df_sample$`Average Wage`,probs = prompt_percentiles)
quan_table<-as.table(quan_table)
print(xtable(quan_table),type = "html")
quan_table<-unname(quan_table)
# create df for each quantile range
bottom_25<-df_sample[df_sample$`Average Wage`<=quan_table[1],]
middle_50<-df_sample[df_sample$`Average Wage`>quan_table[1] & df_sample$`Average Wage`<=quan_table[3],]
upper_25<-df_sample[df_sample$`Average Wage`>quan_table[3],]
upper_10<-df_sample[df_sample$`Average Wage`>quan_table[4],]
upper_5<-df_sample[df_sample$`Average Wage`>quan_table[5],]
# create tables
make_tables <- function(x){
     t_gender <- prop.table(table(x$Gender))
     t_occ <- prop.table(table(x$`PUMS Occupation`))
     t_deg <- prop.table(table(x$`Degree Level`))
     t_all <- merge(t_gender,t_occ, all=TRUE)
     t_all <- merge(t_all,t_deg,all=TRUE)
     return(t_all)
}
bottom_25_t <- make_tables(bottom_25)
middle_50_t <- make_tables(middle_50)
upper_25_t <- make_tables(upper_25)
upper_10_t <- make_tables(upper_10)
upper_5_t <- make_tables(upper_5)
colnames(bottom_25_t)[1]<-"Bottom 25"
colnames(middle_50_t)[1]<-"Middle 50"
colnames(upper_25_t)[1]<-"Upper 25"
colnames(upper_10_t)[1]<-"Upper 10"
colnames(upper_5_t)[1]<-"Upper 5"
# output html
library(data.table)
bottom_25_out <- as.data.table(bottom_25_t)
print(xtable(bottom_25_out), type="html")
middle_50_out <- as.data.table(middle_50_t)
print(xtable(middle_50_out), type="html")
upper_25_out <- as.data.table(upper_25_t)
print(xtable(upper_25_out), type="html")
upper_10_out <- as.data.table(upper_10_t)
print(xtable(upper_10_out), type="html")
upper_5_out <- as.data.table(upper_5_t)
print(xtable(upper_5_out), type="html")
# Part 5
curve(dnorm(x, sample_mean, sample_sd), from = sample_mean-4*sample_sd, to = sample_mean+4*sample_sd, ylab = "f(x)", xlab="Salary")
male_mean<-mean(df_sample$`Average Wage`[df_sample$`ID Gender`==1])
male_sd<-sd(df_sample$`Average Wage`[df_sample$`ID Gender`==1])
female_sd<-sd(df_sample$`Average Wage`[df_sample$`ID Gender`==2])
female_mean<-mean(df_sample$`Average Wage`[df_sample$`ID Gender`==2])
curve(dnorm(x, male_mean, male_sd), from = sample_mean-4*sample_sd, to = sample_mean+4*sample_sd, ylab = "f(x)", xlab="Salary", col='blue')
curve(dnorm(x, sample_mean, sample_sd), add = TRUE)
curve(dnorm(x, female_mean, female_sd), add = TRUE, col='red')