################################################################
##BIA 5303 - Big Data 2 - Group Project
##Group 6 Members:
  #1) Daniel Arantes Ventura - N01468881
  #2) Rutuja Bhagatsing Kadam - N01468983
  #3) Mariana Reyes - N01468759
  #4) Maria Paula Rodriguez - N01469279
  #5) Mugdha Vivek Bhatwadekar- N01468894

# UCI Machine Learning Repository: Bank Marketing Data Set. (n.d.). 
             #Retrieved November 1, 2022, from https://archive.ics.uci.edu/ml/datasets/Bank+Marketing

################################################################

#Installing packages and loading libraries

install.packages("ggplot2")                           
library("ggplot2")
install.packages("reshape2")  
library("reshape2")
install.packages("dplyr")  
library("dplyr")
install.packages("readxl")
library("readxl")
library(tidyverse)
library("sqldf")
library("tidyr")
library("readxl")
library("scales")

#file read
bankdata = read.csv("C://Users//kadam//Documents//bank-full.csv", sep = ';')
bankdata

#1) Categorization of all customers according to the job profile

##Dataframe creation with job column and counting it
data_job <- as.data.frame(table(bankdata$job))
data_job1 = rename(data_job,job = Var1)
colnames(data_job1)

## Bar plot display

ggplot(data_job1, aes(x= job, y = Freq)) + 
  geom_bar(stat = "identity",color="black", fill="lightblue") +
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title = "Customer's Job Profile", x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90))

#2) Customer's count as per age group

## Categorizing age column into different age categories and storing it into new column
bankdata[bankdata$age > 10 & bankdata$age <= 20, "age_group"] <- "10-20"
bankdata[bankdata$age > 20 & bankdata$age <= 30, "age_group"] <- "20-30"
bankdata[bankdata$age > 30 & bankdata$age <= 40, "age_group"] <- "30-40"
bankdata[bankdata$age > 40 & bankdata$age <= 50, "age_group"] <- "40-50"
bankdata[bankdata$age > 50 & bankdata$age <= 60, "age_group"] <- "50-60"
bankdata[bankdata$age > 60 & bankdata$age <= 70, "age_group"] <- "60-70"
bankdata[bankdata$age > 70 & bankdata$age <= 80, "age_group"] <- "70-80"
bankdata[bankdata$age > 80 & bankdata$age <= 90, "age_group"] <- "80-90"
bankdata[bankdata$age > 90, "age_group"] <- "> 90"

## New dataframe creation for the count of customers as per age category
data_srz <- as.data.frame(table(bankdata$age_group))
data_srz1 = rename(data_srz,AgeGroup = Var1)
colnames(data_srz1)

## Bar plot to display Customer's count as per age group
ggplot(data_srz1, aes(x= AgeGroup, y = Freq, fill = AgeGroup )) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title = "Customer's Count as per Age Category", x = NULL, y = NULL)

#3) Housing Loan Taken/Not Taken as per Age Category 

## Categorizing age column into different age categories and storing it into new column
bankdata[bankdata$age > 0  & bankdata$age <= 30, "age_group1"] <- " 0-30"
bankdata[bankdata$age > 30 & bankdata$age <= 40, "age_group1"] <- "30-40"
bankdata[bankdata$age > 40 & bankdata$age <= 50, "age_group1"] <- "40-50"
bankdata[bankdata$age > 50 & bankdata$age <= 60, "age_group1"] <- "50-60"
bankdata[bankdata$age > 60, "age_group1"] <- "> 60"

## New dataframe creation for the count of customers for the loan
data_housing <- as.data.frame(table(bankdata$age_group1, bankdata$loan))
data_housing1 = rename(data_housing,Loan = Var2)

## Stacked bar plot to display Housing Loan Taken/Not Taken as per Age Category 
ggplot(data_housing1, aes(fill = Loan,
                          y = Freq,
                          x = Var1)) +
  geom_bar(position = "stack",
           stat="identity") +
  labs(title = "Housing Loan Taken/Not Taken as per Age Category", x = NULL, y = NULL) +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.6),size=3.5) 


#Renaming 'y' variable to 'Subscriber' for better understanding
# "y" variable (Subscriber)is the target variable.Has the client subscribed a term deposit? (binary: "yes","no")
bank_df1=rename(bankdata, Subscriber=y)
colnames(bank_df1)

#4) Subscription Rate per Job Title

##Using prop.table function to calculate the proportions.
job_subscriber <- bank_df1 %>%
  select(job,Subscriber)%>%
  table()%>%
  prop.table(margin=1L)

##Converting the previous "table" outcome to data frame
job_subscriber_df=as.data.frame(job_subscriber) 

##Adding a new column "labels" with the proportions (Freq) in a percentage format. 
job_subscriberdf1 <- job_subscriber_df %>%
  mutate(labels=scales::percent(Freq,accuracy=0.1)) 

##Filtering the data frame by customers that subscribed to the product.
job_plot = filter(job_subscriberdf1, Subscriber == "yes")
job_plot

##Bar plot Job 
##What type of job did the customers who best responded to the campaign have?

p1=ggplot(job_plot, aes(x= job, y = Freq  )) + geom_bar(fill="#0077b6",position ="stack",stat="identity") 
p1+theme(axis.text.x = element_text(angle=90,hjust=1 )) +
  labs(title = "Subscription Rate per Job type", x = "Job", y = "% Subscribers") +
  geom_text(aes(label = labels, vjust = 2))

#5) Subscription Rate per Education level

##Using prop.table function to calculate the proportions.
education <- bank_df1 %>%
  select(education,Subscriber)%>%
  table()%>%
  prop.table(margin=1L)

##Converting the previous "table" outcome to data frame
education_df=as.data.frame(education) 


##Adding a new column "edu_lab" with the proportions (Freq) in a percentage format.
education_df1 <- education_df %>%
  mutate(edu_lab=scales::percent(Freq,accuracy=0.1)) 


##Filtering the data frame by customers that subscribed to the product.
education_plot = filter(education_df1, Subscriber == "yes")
education_plot

##Pie chart 
## What was the education level for the customers who best responded to the campaign?

ggplot(education_plot, aes(x = "", y = Freq, fill = education)) +
  geom_col() +
  geom_label(aes(label = edu_lab),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void()

#6) Subscription Rate per Marital Status

##Using prop.table function to calculate the proportions.
marital <- bank_df1 %>%
  select(marital,Subscriber)%>%
  table()%>%
  prop.table(margin=1L)

##Converting the previous "table" outcome to data frame
marital_df=as.data.frame(marital) 

##Adding a new column "marital_lab" with the proportions (Freq) in a percentage format.
marital_df1 <- marital_df %>%
  mutate(marital_lab=scales::percent(Freq,accuracy=0.1)) 

##Filtering the data frame by customers that subscribed to the product.
marital_plot = filter(marital_df1, Subscriber == "yes")
marital_plot

##Bar plot
## What was the marital status for the customers who best responded to the campaign?
p2=ggplot(marital_plot, aes(x= marital, y = Freq  )) + geom_bar(fill="#0077b6",position ="stack",stat="identity") 
p2+theme(axis.text.x = element_text(angle=90,hjust=1 )) +
  labs(title = "Subscription Rate per Marital Status", x = "Marital Status", y = "% Subscribers") +
  geom_text(aes(label = marital_lab, vjust = 2))

#7) Subscription Rate per Age Groups

##Creating age groups.
bank_df1[bank_df1$age > 10 & bank_df1$age <= 20, "age_group"] <- "10-20"
bank_df1[bank_df1$age > 20 & bank_df1$age <= 30, "age_group"] <- "20-30"
bank_df1[bank_df1$age > 30 & bank_df1$age <= 40, "age_group"] <- "30-40"
bank_df1[bank_df1$age > 40 & bank_df1$age <= 50, "age_group"] <- "40-50"
bank_df1[bank_df1$age > 50 & bank_df1$age <= 60, "age_group"] <- "50-60"
bank_df1[bank_df1$age > 60 & bank_df1$age <= 70, "age_group"] <- "60-70"
bank_df1[bank_df1$age > 70 & bank_df1$age <= 80, "age_group"] <- "70-80"
bank_df1[bank_df1$age > 80 & bank_df1$age <= 90, "age_group"] <- "80-90"
bank_df1[bank_df1$age > 90, "age_group"] <- "90 +"

##Using prop.table function to calculate the proportions.
age_df <- bank_df1 %>%
  select(age_group,Subscriber)%>%
  table()%>%
  prop.table(margin=1L)

age_df

##Converting the previous "table" outcome to data frame
age_group_df=as.data.frame(age_df) 
age_group_df


##Adding a new column "age_lab1" with the proportions (Freq) in a percentage format.
age_group_df1 <- age_group_df %>%
  mutate(age_lab1=scales::percent(Freq,accuracy=0.1)) 


##Filtering the data frame by customers that subscribed to the product.
age_plot1 = filter(age_group_df1, Subscriber == "yes")
age_plot1

##Bar plot
## What was the age group with higher subscription rate ?
p3=ggplot(age_plot1, aes(x= age_group, y = Freq  )) + geom_bar(fill="#0077b6",position ="stack",stat="identity") 
p3+theme(axis.text.x = element_text(angle=90,hjust=1 )) +
  labs(title = "Subscription Rate per Age", x = "Age Group", y = "% Subscribers") +
  geom_text(aes(label = age_lab1, vjust = 2))

#8) Subscription Rate per Personal Loan variable 

##Using prop.table function to calculate the proportions.
personal <- bank_df1 %>%
  select(loan,Subscriber)%>%
  table()%>%
  prop.table(margin=1L)

##Converting the previous "table" outcome to data frame
personal_df=as.data.frame(personal) 
personal_df

##Adding a new column "personal_lab" with the proportions (Freq) in a percentage format.
personal_df1 <- personal_df %>%
  mutate(personal_lab=scales::percent(Freq,accuracy=0.1)) 

##Filtering the data frame by customers that subscribed to the product.
personal_plot = filter(personal_df1, Subscriber == "yes")
personal_plot

##Bar plot
##Was the subscription rate higher for customers that have personal loan ?
p4=ggplot(personal_plot, aes(x= loan, y = Freq  )) + geom_bar(fill="#0077b6",position ="stack",stat="identity") 
p4+theme(axis.text.x = element_text(angle=90,hjust=1 )) +
  labs(title = "Subscription Rate per Personal Loan", x = "Personal Loan", y = "% Subscribers") +
  geom_text(aes(label = personal_lab, vjust = 2))


## Analysis - Subscription Rate per Housing Loan variable 

#Using prop.table function to calculate the proportions.
housing <- bank_df1 %>%
  select(housing,Subscriber)%>%
  table()%>%
  prop.table(margin=1L)

#Converting the previous "table" outcome to data frame
housing_df=as.data.frame(housing) 
housing_df

#Adding a new column "housing_lab" with the proportions (Freq) in a percentage format.
housing_df1 <- housing_df %>%
  mutate(housing_lab=scales::percent(Freq,accuracy=0.1)) 

#Filtering the data frame by customers that subscribed to the product.
housing_plot = filter(housing_df1, Subscriber == "yes")
housing_plot

#Bar plot
# Was the subscription rate higher for customers that have housing loan ?
p5=ggplot(housing_plot, aes(x= housing, y = Freq)) + geom_bar(fill="#0077b6",position ="stack",stat="identity") 
p5+theme(axis.text.x = element_text(angle=90,hjust=1 )) +
  labs(title = "Subscription Rate per Housing Loan", x = "Housing Loan", y = "% Subscribers") +
  geom_text(aes(label =housing_lab, vjust = 2))

#9) Subscription Rate per Default variable 

##Using prop.table function to calculate the proportions.
default <- bank_df1 %>%
  select(default,Subscriber)%>%
  table()%>%
  prop.table(margin=1L)

##Converting the previous "table" outcome to data frame
default_df=as.data.frame(default) 
default_df

##Adding a new column "default_lab" with the proportions (Freq) in a percentage format.
default_df1 <- default_df %>%
  mutate(default_lab=scales::percent(Freq,accuracy=0.1)) 

##Filtering the data frame by customers that subscribed to the product.
default_plot = filter(default_df1, Subscriber == "yes")
default_plot

##Bar plot
## Was the subscription rate higher for customers that have credit default ?
p6=ggplot(default_plot, aes(x= default, y = Freq)) + geom_bar(fill="#0077b6",position ="stack",stat="identity") 
p6+theme(axis.text.x = element_text(angle=90,hjust=1 )) +
  labs(title = "Subscription Rate per Credit Default Customer", x = "Credit Default", y = "% Subscribers") +
  geom_text(aes(label =default_lab, vjust = 2))

#10) Conversion rate per MONTH

##Creating an aggregate table with subscription rate

month_cr <- bankdata %>%
  select(month,y)%>%
  table()%>%
  prop.table(margin=1L)

##Converting the table into a data frame so that it can be plotted with ggplot
month_cr_df=as.data.frame(month_cr) 
month_cr_df

##Converting the subscription rate to percentage
month_cr_df <- month_cr_df %>%
  mutate(labels=scales::percent(Freq,accuracy=0.1))
month_cr_df

##Creating a filter to select only the successful subscription rates where y = yes
month_plot = filter(month_cr_df, y == "yes")
month_plot

##Creating a bar plot of the % subscription rate per month

### Ordering the months in chronological order
month_plot <- month_plot %>%
  mutate(month = factor(month, levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")))

###Creating the bar plot using ggplot
p1=ggplot(month_plot, aes(x= month, y = Freq)) +
  geom_col(fill="#0077b6",position ="stack") 
p1+theme(axis.text.x = element_text(hjust=0.5 )) +
  labs(title = "Subscription Rate per Month", x = "Month", y = "% Subscribers") +
  geom_text(aes(label = labels, vjust = 2))

#11) Conversion rate per TYPE OF CONTACT

##Creating an aggregate table with subscription rate

contact_cr <- bankdata %>%
  select(contact,y)%>%
  table()%>%
  prop.table(margin=1L)

##Converting the table into a data frame so that it can be plotted with ggplot
contact_cr_df=as.data.frame(contact_cr) 
contact_cr_df

##Converting the subscription rate to percentage
contact_cr_df <- contact_cr_df %>%
  mutate(labels=scales::percent(Freq,accuracy=0.1))
contact_cr_df

##Creating a filter to select only the successful subscription rates where y = yes
contact_plot = filter(contact_cr_df, y == "yes")
contact_plot

##Creating a bar plot of the % subscription rate by contact form

p2=ggplot(contact_plot, aes(x= contact, y = Freq  )) +
  geom_col (fill="#0077b6",position ="stack") 
p2+theme(axis.text.x = element_text(hjust=0.5))
p2+theme(axis.title.x = element_text(vjust=-1))+
  labs(title = "Subscription Rate per Type of Contact", x = "Type of Contact", y = "% Subscribers") +
  geom_text(aes(label = labels, vjust = 2))

#12) Creating an aggregate table with subscription rate by CALL DURATION

duration_cr <- bankdata %>%
  select(duration,y)%>%
  table()%>%
  prop.table(margin=1L)

##Converting the table into a data frame so that it can be plotted with ggplot
duration_cr_df=as.data.frame(duration_cr) 
duration_cr_df

##Converting the subscription rate to percentage
duration_cr_df <- duration_cr_df %>%
  mutate(labels=scales::percent(Freq,accuracy=0.1))
duration_cr_df

##Creating a filter to select only the succesful subscription rates where y = yes
duration_plot = filter(duration_cr_df, y == "yes")
duration_plot

##Creating a plot of the % subscription rate by call duration

###Filtering out all points where subscription rate was 1 or 0 because they create noise in the visualization
dur=filter(duration_plot, Freq>0 & Freq<1)

###Transforming the duration column into numeric, it was factor by default
dur$duration <- as.numeric(as.character(dur$duration))
sapply(dur, class)

###Creating a scatter plot since duration and subscription rate are both numeric variables
p3=ggplot(dur, aes(x=duration, y=Freq)) + 
  geom_point(color="#0077b6")
p3 + scale_x_continuous(name="Duration", breaks=seq(0, 5000,100))+
  labs(title = "Subscription Rate per Call Duration", x = "Call Duration (min)", y = "% Subscribers")

##13) Creating an aggregate table with subscription rate by BALANCE



balance_cr <- bankdata %>%
  select(balance,y)%>%
  table()%>%
  prop.table(margin=1L)

##Converting the table into a data frame so that it can be plotted with ggplot
balance_cr_df=as.data.frame(balance_cr) 
balance_cr_df

##Converting the subscription rate to percentage
balance_cr_df <- balance_cr_df %>%
  mutate(labels=scales::percent(Freq,accuracy=0.1))
balance_cr_df

##Creating a filter to select only the successful subscription rates where y = yes
balance_plot = filter(balance_cr_df, y == "yes")
balance_plot

##Creating a plot of the % subscription rate by balance

###Filtering out all poitns where subscription rate was 1 or 0 because they create noise in the visualization
bal=filter(balance_plot, Freq>0 & Freq<1)

bal
###Transforming the balance column into numeric, it was factor by default
bal$balance <- as.numeric(as.character(bal$balance))
sapply(bal, class)

###Creating a scatter plot since balance and subscription rate are both numeric variables

bal=filter(bal,balance<6000 & balance >0)

p3=ggplot(bal, aes(x=balance, y=Freq)) + 
  geom_point(color="#0077b6")
p3 + scale_x_continuous(name="Balance ($)", breaks=seq(0,6000,1000))+
  labs(title = "Subscription Rate by Account Balance", x = "Balance ($)", y = "% Subscribers")


#14) Creating an aggregate table with subscription rate by NUMBER OF CONTACTS DURING CAMPAIGN

nocontacts_cr <- bankdata %>%
  select(campaign,y)%>%
  table()%>%
  prop.table(margin=1L)

##Converting the table into a data frame so that it can be plotted with ggplot
nocontacts_cr_df=as.data.frame(nocontacts_cr) 
nocontacts_cr_df

##Converting the subscription rate to percentage
nocontacts_cr_df <- nocontacts_cr_df %>%
  mutate(labels=scales::percent(Freq,accuracy=0.1))
nocontacts_cr_df

##Creating a filter to select only the succesful subscription rates where y = yes
nocontacts_plot = filter(nocontacts_cr_df, y == "yes")
nocontacts_plot

##Creating a plot of the % subscription rate by number of contacts


###Transforming the campaign column into numeric, it was factor by default
nocontacts_plot$campaign <- as.numeric(as.character(nocontacts_plot$campaign))
sapply(nocontacts_plot, class)

###Creating a scatter plot since campaign and subscription rate are both numeric variables
p5=ggplot(nocontacts_plot, aes(x=campaign, y=Freq)) + 
  geom_point(color="#0077b6")
p5 + scale_x_continuous(name="Number of Contacts", breaks=seq(0, 70,2))+
  labs(title = "Subscription Rate by Number of Contacts", x = "Number of contacts", y = "% Subscribers")

#15) Creating an aggregate table with subscription rate by NUMBER OF DAYS SINCE LAST CONTACTED

pdays_cr <- bankdata %>%
  select(pdays,y)%>%
  table()%>%
  prop.table(margin=1L)

##Converting the table into a data frame so that it can be plotted with ggplot
pdays_cr_df=as.data.frame(pdays_cr) 
pdays_cr_df

##Converting the subscription rate to percentage
pdays_cr_df <- pdays_cr_df %>%
  mutate(labels=scales::percent(Freq,accuracy=0.1))
pdays_cr_df

##Creating a filter to select only the succesful subscription rates where y = yes
pdays_plot = filter(pdays_cr_df, y == "yes")
pdays_plot

##Creating a plot of the % subscription rate by number of days since last contacted

###Transforming the pdays column into numeric, it was factor by default
pdays_plot$pdays <- as.numeric(as.character(pdays_plot$pdays))
sapply(pdays_plot, class)

###Creating a scatter plot since pdays and subscription rate are both numeric variables
pdays1=filter(pdays_plot, Freq>0 & Freq<1)
p6=ggplot(pdays1, aes(x=pdays, y=Freq)) + 
  geom_point(color="#0077b6")
p6 + scale_x_continuous(name="Number of Days since Last Campaign", breaks=seq(-1, 800,50))+
  labs(title = "Subscription Rate by Number of Days since Last Campaign", x = "NNumber of Days since Last Campaign", y = "% Subscribers")

###Creating a scatter plot for pdays and subscription rate, filtering pdays from -2 to 60
pdays2=filter(pdays_plot, Freq>0 & Freq<1 & pdays<60 & pdays >-2)
p7=ggplot(pdays2, aes(x=pdays, y=Freq)) + 
  geom_smooth(color="#0077b6")
p7 + scale_x_continuous(name="Number of Days since Last Campaign", breaks=seq(-1, 60,3))+
  labs(title = "Subscription Rate by Number of Days since Last Campaign", x = "Number of Days since Last Campaign", y = "% Subscribers")

#16) Creating an aggregate table with subscription rate by NUMBER OF CONTACTS BEFORE THE CAMPAIGN

previous_cr <- bankdata %>%
  select(previous,y)%>%
  table()%>%
  prop.table(margin=1L)


##Converting the table into a data frame so that it can be plotted with ggplot
previous_cr_df=as.data.frame(previous_cr) 
previous_cr_df

##Converting the subscription rate to percentage
previous_cr_df <- previous_cr_df %>%
  mutate(labels=scales::percent(Freq,accuracy=0.1))
previous_cr_df

##Creating a filter to select only the successful subscription rates where y = yes
previous_plot = filter(previous_cr_df, y == "yes")
previous_plot

##Creating a plot of the % subscription rate by number of days before the campaign

###Transforming the previous column into numeric, it was factor by default
previous_plot$previous <- as.numeric(as.character(previous_plot$previous))
sapply(previous_plot, class)

###Creating a scatter plot since previous and subscription rate are both numeric variables
previous1=filter(previous_plot, previous>0 & previous<60)
p8=ggplot(previous1, aes(x=previous, y=Freq)) + 
  geom_point(color="#0077b6")
p8 + scale_x_continuous(name="Number of Previous Contacts", breaks=seq(0, 60,5))+
  labs(title = "Subscription Rate by Number of Previous Contacts", x = "Number of Previous Contacts", y = "% Subscribers")

#17) Conversion rate per PREVIOUS SUCCESS

poutcome_cr <- bankdata %>%
  select(poutcome,y)%>%
  table()%>%
  prop.table(margin=1L)

##Converting the table into a data frame so that it can be plotted with ggplot
poutcome_cr_df=as.data.frame(poutcome_cr) 
poutcome_cr_df

##Converting the subscription rate to percentage
poutcome_cr_df <- poutcome_cr_df %>%
  mutate(labels=scales::percent(Freq,accuracy=0.1))
poutcome_cr_df

##Creating a filter to select only the successful subscription rates where y = yes
poutcome_plot = filter(poutcome_cr_df, y == "yes")
poutcome_plot

##Creating a bar plot of the % subscription rate by past outcome 

###Creating the bar plot using ggplot
p9=ggplot(poutcome_plot, aes(x= poutcome, y = Freq)) +
  geom_col(fill="#0077b6",position ="stack") 
p9+theme(axis.text.x = element_text(hjust=0.5 )) +
  labs(title = "Subscription Rate by Last Outcome", x = "Past Outcome", y = "% Subscribers") +
  geom_text(aes(label = labels, vjust = 2))

#18) Combining Client and Campaign data

##Creating an aggregate table with subscription rate

all_cr <- bankdata %>%
  select(poutcome,age, job, y)%>%
  table()%>%
  prop.table(margin=1L)

##Converting the table into a data frame so that it can be plotted with ggplot
all_cr_df=as.data.frame(all_cr) 
all_cr_df

##Converting the subscription rate to percentage
all_cr_df <- all_cr_df %>%
  mutate(labels=scales::percent(Freq,accuracy=0.1))
all_cr_df

##Creating a filter to select only the successful subscription rates where y = yes
all_plot = filter(all_cr_df, y == "yes")
all_plot

###Transforming the age column into numeric, it was factor by default
all_plot$age <- as.numeric(as.character(all_plot$age))
sapply(all_plot, class)

##Creating a bar plot of the % subscription rate by past outcome and occupation

##Creating the bar plot using ggplot
p10=ggplot(all_plot, aes(x= poutcome, y = Freq, fill = job)) +
  geom_col(position ="stack") 
p10+labs(title = "Subscription Rate by Last Outcome and Job", x = "Past Outcome", y = "% Subscribers")






