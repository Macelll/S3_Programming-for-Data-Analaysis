#Marcell Agung Wahyudi
#TP058650

#My Libraries
library(pacman)
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes,
               ggvis, httr, lubridate, plotly, rio, shiny,
               rmakrdown, stringr, tidyr, magrittr)
library(datasets)

################
#For testing purposes
write.csv(test, "C:\\Users\\ASUS ROG\\Documents\\College Stuff\\3rd Semester\\(PFDA) ProgrammingForDataAnalysis (R)\\Assignment\\test.csv")

#All Pre-Processing for Terminated Employees in one line
empUniqueTerminated <- read.csv("C:\\Users\\ASUS ROG\\Documents\\College Stuff\\3rd Semester\\(PFDA) ProgrammingForDataAnalysis (R)\\Assignment\\employee_attrition.csv") %>% filter(terminationdate_key != '1/1/1900') %>% distinct(EmployeeID, .keep_all = TRUE)
View(empUniqueTerminated)

#Data Import
empData <- read.csv("C:\\Users\\ASUS ROG\\Documents\\College Stuff\\3rd Semester\\(PFDA) ProgrammingForDataAnalysis (R)\\Assignment\\employee_attrition.csv")

View(empData)
summary(empData)

#Pre-Processing
#To answer the assignment questions, first we must find Staffs who have been terminated 
#We can do that by filtering out employees who have termination date of 1/1/1900 
#(termination date of 1/1/1900 means still in the company).
empTerminated <- empData %>% filter(terminationdate_key != '1/1/1900')
View(empTerminated)

#Removing duplicated Employee ID
empUnique <- empData %>% distinct(EmployeeID, .keep_all=TRUE)
View(empUnique)
summary(empUnique)

#Find only active employees
empUniqueActive <- empUnique %>% filter(terminationdate_key == '1/1/1900')
View(empUniqueActive)

#DataFrame for Unique Terminated Employees
empUniqueTerminated <- empTerminated %>% distinct(EmployeeID, .keep_all = TRUE)
View(empUniqueTerminated)

#For checking purposes*
write.csv(empUniqueTerminated, "C:\\Users\\ASUS ROG\\Documents\\College Stuff\\3rd Semester\\(PFDA) ProgrammingForDataAnalysis (R)\\Assignment\\employee_unique_terminated.csv", row.namews = FALSE)

#Q1. Why would staff leave the Company
#Analysis 1-1: Relationship terminated employees between position and attrition

ggplot(data = empUniqueTerminated,aes(y = department_name, fill = department_name)) + geom_bar()+
  labs(title="Barchart of Terminated Employees Department Name", x="Count", y="Department Name")

#From the bar chart, we can observe that the most terminated employees are from Departments of: 
#Meats, Produce, Customer Service, Dairy, and Bakery

#Filter out other departments,
DeptNames <- c("Bakery", "Produce", "Customer Service", "Dairy", "Meats")
empUniqueTerminated2 <- empUniqueTerminated %>% filter(department_name %in% DeptNames)
empUniqueTerminated2

ggplot(data = empUniqueTerminated2,aes(y = job_title, fill = job_title)) + geom_bar()+
  labs(title="Barchart of Terminated Employees Job Position", y="Job Position", x="Count")

#From the visualization, we can see that Cashier, Dairy Person, Meat Cutter, and Produce Clerk are the most dominant Job Positions where the employees got terminated.

#Analysis 1-2: Relationship between terminated employees age and attrition
#Bar chart for Job age:
ggplot(data = empUniqueTerminated,aes(x = age, fill = age)) + geom_bar()+
  labs(title="Barchart of Terminated Employees Age", x="Age", y="Count")

#From the Bar chart, it can be seen that most terminated employees are around 55+, which most likely be the retirement year for them.
#the 2nd highest are from people in their 20s till 30 years old, above it, many employees rarely gets terminated.

#Analysis 1-3: Relationship between length_of_service and attrition
ggplot(data = empUniqueTerminated,aes(x = length_of_service, fill = length_of_service)) + geom_bar()+
  labs(title="Barchart of Terminated Employees Length of Service", x="Length of Service", y="Count")
#From the Bar chart, it can be seen that the most terminated employees served at least 13 years of service, but surprisingly, the graph dips on the 14th and 15th year, one hypothesis is that the company might reward employees who have worked for 15 years for the company, thus motivating the employees to stay a bit longer..

#Analysis 1-4: Job positions of terminated employees age 20-25
empYoung <- empUniqueTerminated %>% filter(age > '19' & age < '26')

ggplot(empYoung, aes(x=job_title, y=age))+
  geom_violin()+  
  labs(title="Barchart of Terminated Employees age 20~25 Job Positions", 
                       x="Job Position", y="Age")
#From the visualization, it can be seen that most terminated employees age 20~25
#are terminated at 20 years old as a cashier, and 25 as a Baker, while Meat Cutter at 23

#Q2. Are there relationship between employees age and their job title
empAgeJobTitle <- empUniqueTerminated %>% select(age,job_title) %>% arrange(job_title)
View(empAgeJobTitle)
summary(empAgeJobTitle)
#From the dataframe above, we can find informatio such as the min age of a terminated employee worked as a certain job_title
#in this example, these are the min, max, and mean ages of employees worked as a baker
empBaker <- empAgeJobTitle %>% filter(job_title == 'Baker')
min(empBaker$age)
max(empBaker$age)
mean(empBaker$age)

#Q3. How many males and females resigns or retired from the company
empGenderTRD <- empUniqueTerminated %>% 
  filter(termreason_desc != 'Not Applicable')%>% 
  select(gender_full, termreason_desc) %>% 
  arrange(termreason_desc)
empGenderTRD

ggplot(empGenderTRD, aes(x=termreason_desc,fill=gender_full)) + 
    geom_bar(position="dodge") + 
  labs(title="Barchart of Relationship between Gender and Terminated Reason", 
       x="Terminated Reason", y="Frequency")
#From the visualization, it can be seen that way more employees retired from the company rather than resigned (this information is apart from the N/A data)

#Q4. At what age does employees resigns from the company.
#Code to filter only employees who resigns, then selects only age, termreason_desc, and gender_full attributes)
empAgeResignation <- empUniqueTerminated %>% 
  filter(termreason_desc == 'Resignaton')%>% 
  select(age, termreason_desc, gender_full)

View(empAgeResignation)

ggplot(empAgeResignation, aes(x=termreason_desc, y=age, fill=gender_full)) +
  geom_violin() +
  labs(title="Barchart of Relationship between Resignation and Age w/ Gender",
       x="Resignation", y="Age")
#From the visualization, it can be seen that most males and females resigns at the age of 20~30, and slowly decreases from then on

#Q5.Which job positions laid off the most employees
#Code to filter only laid off employees 
empLayoff <- empTerminated %>% filter(termreason_desc == 'Layoff')
View(empLayoff)

ggplot(empLayoff, aes(y=job_title, fill=gender_full)) +
  geom_bar() +
  labs(title="Barchart of Laid off employees' Job Titles", x="Frequency", y="Job Titles")

#Q6. Does attrition have a connection with the specific store?
#Analysis 6-1: Finding which store_name has the most employee termination
#Function to get Mode: (source: https://www.tutorialspoint.com/r/r_mean_median_mode.htm)
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getMode(empUniqueTerminated$store_name)  #Mode = 35

#Analysis 6-2: Finding most terminated job position in store 35.
empStore35 <- empUniqueTerminated %>% filter(store_name==(getMode(empUniqueTerminated$store_name)))
View(empStore35)

ggplot(empStore35, aes(y=job_title, fill=gender_full))+
  geom_bar()

#Analysis 6-3: Finding the age of the employees working as Meat Cutter and Dairy Person in Store 35
jobTitle35 <- empStore35 %>% filter(job_title=='Meat Cutter' | job_title=='Dairy Person')
View(jobTitle35)

ggplot(jobTitle35, aes(y=age, fill=job_title))+
  geom_bar()
