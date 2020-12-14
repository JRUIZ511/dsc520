#Assignment 3 Report
#Name: Ruiz, Joseph
#Date: 2020-12-13
library(ggplot2)
theme_set(theme_minimal())

#Set working directory to where data is saved for assignment 3 on my computer
setwd("C:/Users/Joseph Ruiz/Documents/GitHub/dsc520/assignments/assignment03")

#setup data to df
all_data_df <- read.csv("scores.csv",stringsAsFactors = FALSE)
head(all_data_df)
str(all_data_df)


#create regular only df
regular_df <- subset(all_data_df, Section=="Regular")
head(regular_df)
print(sum(regular_df$Count))
summary(regular_df,quantile.type = 1)



#create vector of all scores for the regular sections
total_reg_points <- c()
loop_count = 1
for(count in regular_df$Count){
  count_reg = count
  print(count_reg)
  
      while(count_reg > 0){
        total_reg_points <- append(total_reg_points,regular_df$Score[loop_count])
        count_reg = count_reg -1
      }
  loop_count = loop_count + 1
}
print(total_reg_points)
length(total_reg_points)
sum(regular_df$Count)
mean(total_reg_points)
max(total_reg_points)
ave_score_reg <- mean(total_reg_points)
med_reg <- median(total_reg_points)
summary(total_reg_points)
std_reg <- sd(total_reg_points)
print(std_reg)
iqr_reg<- IQR(total_reg_points, type = 1)



#create sport only df
sports_df <- subset(all_data_df, Section=="Sports")
head(sports_df)
print(sports_df)
summary(sports_df, quantile.type = 1)

#create vector of all scores for the sports sections
total_spt_points <- c()
loop_count_sport = 1
for(count in sports_df$Count){
  count_reg = count
  print(count_reg)
  
  while(count_reg > 0){
    total_spt_points <- append(total_spt_points,sports_df$Score[loop_count_sport])
    count_reg = count_reg -1
  }
  loop_count_sport = loop_count_sport + 1
}
print(total_spt_points)
length(total_spt_points)
sum(sports_df$Count)
mean(total_spt_points)
mean_spt <- mean(total_spt_points)
max(total_spt_points)
ave_score_spt <- mean(total_spt_points)
med_spt <- median(total_spt_points)
iqr_spt<- IQR(total_spt_points, type = 1)
summary(total_spt_points)

#normX_spt <- rnorm(length(total_spt_points),mean = mean_spt,sd = std_spt)
#normX_spt <- rnorm(total_spt_points)
print(normX_spt)

#comparing mean and median 
#reg
summary(total_reg_points)
sum(total_reg_points > ave_score_reg)
sum(total_reg_points > med_reg )
print(iqr_reg)
length(total_reg_points)
std_reg <- sd(total_reg_points)
print(std_reg)
ave_score_reg <- mean(total_reg_points)

#sport
summary(total_spt_points)
sum(total_spt_points > ave_score_spt)
sum(total_spt_points > med_spt)
print(iqr_spt)
length(total_spt_points)
std_spt <- sd(total_spt_points) 
print(std_spt)
mean_spt <- mean(total_spt_points)
#comparing mean and median agnaistsections

sum(total_reg_points > ave_score_spt)
sum(total_reg_points > med_spt )
sum(total_reg_points > med_reg)
sum(total_spt_points > ave_score_reg)
sum(total_spt_points > med_reg)


#graph regular
ggplot(, aes(Score, Count)) + geom_bar()
boxplot(total_reg_points, main = "Scores of Regular Section", horizontal = TRUE, notch = TRUE, xlab = "Total Score", ylab= "Students", col = "orange", border = "brown") + abline(v=mean(total_reg_points))
hist(total_reg_points, main = "Regular Section Histogram", ylab="Count", xlab = "Total Score") + abline(v=mean(total_reg_points), col= "red")


#graph sports
boxplot(total_spt_points, main = "Scores of Sports Section", horizontal = TRUE, notch = TRUE, xlab = "Total Score", ylab= "Students", col = "green", border = "brown") + abline(v=mean(total_spt_points))
hist(total_spt_points, main = "Sport Section Histogram", ylab="Count", xlab = "Total Score") + abline(v=mean(total_spt_points), col = 'red') 
#hist(normX_spt,col = "red")
#plot(total_spt_points, normX_spt, type = "n", xlab="Class Score", ylab="Probality of student score",
   #  main="Normal Distribution", axes=TRUE)    + lines(total_spt_points,normX_spt) + abline(v=mean(total_spt_points))
#ggplot(total_spt_points,aes(x=total_reg_points, y=normX_spt)) + geom_point()
#hist(normX_spt)

