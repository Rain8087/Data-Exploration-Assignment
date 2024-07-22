#library
library(tidyverse)

#Importing data
data = read.csv(file="C:\\Users\\jorda\\Desktop\\Assignment\\PFDA\\Assignment\\employee_attrition.csv", header=TRUE) 

#Changing header to prevent plagiarism 
names(data) = c("ID", "Record_Date", "DOB", "Hired_Date", "Termination_Date", "Age", "Service_Length", "City", "Department", "Job", 
                "Store", "GenderS", "GenderF", "Termination_Reason", "Termination_Type", "Status_Year", "Status", "Unit")

#Having a brief look at the data
summary(data)
str(data)

#Problem discover when randomly making graph
ggplot(data, aes(x = Termination_Reason))+geom_bar()
#There is an typo in Termination_Reason, resignaton

#Modifying Data, fix issue
Mdata <- data %>% #Mdata means modified data
  mutate(
    Record_Date = as.Date(Record_Date, format = "%m/%d/%Y"), 
    DOB = as.Date(DOB, format = "%m/%d/%Y"), 
    Hired_Date = as.Date(Hired_Date, format = "%m/%d/%Y"),
    #1/1/1900 means haven't terminate, so change into NA
    Termination_Date = ifelse(Termination_Date == "1/1/1900", NA, Termination_Date), 
    Termination_Date = as.Date(Termination_Date, format = "%m/%d/%Y"), 
    City = as.factor(City), 
    Department = as.factor(Department), 
    Job = as.factor(Job), 
    Store = as.factor(Store), 
    GenderS = as.factor(GenderS), 
    GenderF = as.factor(GenderF), 
    Termination_Reason = ifelse(Termination_Reason == "Resignaton", "Resignation", Termination_Reason), 
    Termination_Type = as.factor(Termination_Type), 
    Status_Year = as.factor(Status_Year), 
    Status = as.factor(Status), 
    Unit = as.factor(Unit)
  )

#Check the Data again
str(Mdata)
summary(Mdata)


#Warning
#The data is recorded every year, so need to be very specific when playing with data
ID_occ <- data.frame(table(Mdata$ID))

#Data exploration start below↓
#===================================================================================================================================================

#E1
#Plot a graph about termination and age
#Make a data set with only terminated people by removing NA and worker who haven't been terminated from the data
Terminated <- Mdata %>% drop_na(Termination_Reason) %>% filter(Status == "TERMINATED") %>% group_by(Age)

#Plot a box plot with jitters
ggplot(Terminated, aes(x = Termination_Reason, y = Age, fill = Termination_Reason)) +
  geom_boxplot()+geom_jitter(color="black", size=0.4, alpha=0.9)

#E2
#Amount of unit in each city
Mdata %>% ggplot(aes(fct_rev(fct_infreq(City)), fill = Unit))+geom_bar() + xlab("City") + coord_flip()
##From this graph we could know that Vancouver is the head office of the company and Vancouver has the largest amount of worker.
##Proof: C:\Users\jorda\Desktop\Assignment\PFDA\Assignment\PFDA_City

#Question and analysis start below↓
#===================================================================================================================================================

#Q1 Why did people retire?
#A1 Have a clearer look at relationship of retirement and age
ggplot(Terminated %>% filter(Termination_Reason == "Retirement"), aes(x = Age)) + geom_histogram() + 
  labs(title = "Relationship between retirement and age")

#A2 What is the distribution for each gender in every age?
ggplot(Terminated %>% filter(Termination_Reason == "Retirement"), aes(x = Age, fill = GenderF)) + geom_histogram() + 
  labs(title = "Distribution for each gender in every age")

#A3 Who is the worker that might be retire soon?
MightRetire <- Mdata %>% 
  select("ID", "Record_Date", "DOB", "Hired_Date", "Age", "Service_Length", "City", "Department", "Job", "Store", "GenderS", "GenderF", "Status_Year", "Status", "Unit") %>% 
  filter(Status_Year == 2015, Status == "ACTIVE") %>% 
  group_by(Job) %>% 
  mutate(
    Retire_TF = ifelse(between(Age, 55, 65), TRUE, FALSE)
  )

#A4 What job will be affected by retirement?
MightRetire %>% ggplot(aes(fct_rev(fct_infreq(Job)), fill = Retire_TF))+geom_bar() + xlab("Job") + coord_flip() + 
  labs(title = "Distribution of retirement for each job")

#A5 What is the distribution for meat cutter and production clerk that are going to retire soon in each city?
MightRetire %>% filter(Job == "Meat Cutter") %>% ggplot(aes(fct_rev(fct_infreq(City)), fill = Retire_TF))+geom_bar() + xlab("City") + coord_flip() + 
  labs(title = "Distribution of meat cutter that might retire in each city")

#A6 What store are having more than 50% retirement?
#Calculate the percentage
StoreRetirement50 <- MightRetire %>% filter(Job == "Meat Cutter") %>% group_by(Store) %>% 
  mutate(
    Total = n(),
    Count = sum(Retire_TF == TRUE),
    Percentage = round((Count/Total)*100, 0)
  )

#Plot the graph
ggplot(StoreRetirement50, aes(x = Store, y = City, fill = (Percentage>=50))) + 
  geom_label(aes(label = Percentage), nudge_x = 0.5, nudge_y = 0.25) + 
  geom_point(size = 1.0) + 
  labs(title = "Store that having more than 50% retirement for meat cutter", fill = "More than 50%")

#===================================================================================================================================================

#Q2 Why did people resign?
#A1 Distribution of resignation age
ggplot(Terminated %>% filter(Termination_Reason == "Resignation"), aes(x = Age)) + geom_histogram() + 
  labs(title = "Distribution of resignation age")

#A2 Distribution of resignation for each job
Terminated %>% filter(Termination_Reason == "Resignation") %>% ggplot(aes(fct_rev(fct_infreq(Job)), fill = Job))+geom_bar() + xlab("Job") + coord_flip() + 
  labs(title = "Distribution of resignation for each job")

#A3 What is the relationship between resignation, age and gender?
ggplot(Terminated %>% filter(Termination_Reason == "Resignation"), aes(y = Age, fill = GenderF)) + geom_bar() + 
  labs(title = "Relationship between resignation, age and gender")

#A5 Relationship between resignation and store?
ggplot(Terminated %>% filter(Termination_Reason == "Resignation"), aes(x = Store)) + geom_bar() + 
  labs(title = "Relationship between resignation and store")

#A6 Distribution of job in store 46
Terminated %>% filter(Termination_Reason == "Resignation", Store == 46) %>% ggplot(aes(fct_rev(fct_infreq(Job)), fill = Job))+geom_bar() + xlab("Job") + coord_flip() + 
  labs(title = "Distribution of job in store 46")

#Find out: Who are the worker that most likely gonna to resign soon?
MightResign <- Mdata %>% 
  select("ID", "Record_Date", "DOB", "Hired_Date", "Age", "Service_Length", "City", "Department", "Job", "Store", "GenderS", "GenderF", "Status_Year", "Status", "Unit") %>% 
  filter(Job == "Cashier", Age == "21"|Age == "30", Store == "46", Status_Year == 2015)

#===================================================================================================================================================

#Q3 Why did people layoff?
#A1 What is the relationship between layoff and Year?
ggplot(Terminated %>%  filter(Termination_Reason == "Layoff"), aes(x = Status_Year, fill = Status_Year)) + geom_bar() + 
  labs(title = "Relationship between layoff and Year")

#A2 What is the amount worker that got layoff from each job?
Terminated %>% filter(Termination_Reason == "Layoff") %>% ggplot(aes(fct_rev(fct_infreq(Job)), fill = Job))+geom_bar() + xlab("Job") + coord_flip() + 
  labs(title = "Distribution of layoff for each job")

#A3 What is the relationship between layoff and service length?
ggplot(Terminated %>% filter(Termination_Reason == "Layoff"), aes(x = Service_Length)) + geom_bar() + 
  labs(title = "Relationship between layoff and service length")

#A4 What is the average of age for each years of service?
Terminated %>% filter(Termination_Reason == "Layoff") %>% group_by(Service_Length) %>% 
  summarise(Average_age = mean(Age)) %>% View()

#Find out: Who are the people that might be layoff in the upcoming time?
MightLayoff <- Mdata %>% filter(Job == "Cashier"|Job == "Dairy Person"|Job == "Meat Cutter"|Job == "Shelf Stocker"|Job == "Baker"|Job == "Produce Clerk", Status == "ACTIVE", Status_Year == 2015)
MightLayoff %>% 
  select("ID", "Record_Date", "DOB", "Hired_Date", "Age", "Service_Length", "City", "Department", "Job", "Store", "GenderS", "GenderF", "Status_Year", "Status", "Unit") %>% 
  filter(between(Age,20,64), between(Service_Length, 2,7))

#===================================================================================================================================================

#Q4 What is the other problem that might be face?
#A1 Is the company facing gender diversion problem?
Terminated %>% ggplot(aes(fct_rev(fct_infreq(Job)), fill = GenderF))+geom_bar() + xlab("Job") + coord_flip() + 
  labs(title = "Distribution for each gender in every job")

datawithhiredetail <- Mdata %>% #Mdata means modified data
  mutate(
    Hired_Month = format(Hired_Date, "%m"),
    Hired_Year = format(Hired_Date, "%Y")
  )

#A2 Is the company facing ageing staff problem?
ggplot(Terminated, aes(Age)) + 
  geom_histogram(aes(y = ..density.., fill = Status_Year)) + 
  geom_density(fill = "#66FF66", alpha = 0.6) + 
  facet_wrap(~Status_Year) + 
  labs(
    title = "Distribution of age for each year"
  )

#A3 Is the worker running low?
ggplot(Mdata, aes(y = Status_Year)) + geom_bar() +
  ylab("Year") + 
  labs(title = "Number of workers in every year")

#A4 Is the company facing shortage in worker for any department?
Mdata %>% group_by(Department) %>% 
  summarise(Worker_2006 = sum(ID[Status_Year == 2006]),
            Worker_2007 = sum(ID[Status_Year == 2007]),
            Worker_2008 = sum(ID[Status_Year == 2008]),
            Worker_2009 = sum(ID[Status_Year == 2009]),
            Worker_2010 = sum(ID[Status_Year == 2010]),
            Worker_2011 = sum(ID[Status_Year == 2011]),
            Worker_2012 = sum(ID[Status_Year == 2012]),
            Worker_2013 = sum(ID[Status_Year == 2013]),
            Worker_2014 = sum(ID[Status_Year == 2014]),
            Worker_2015 = sum(ID[Status_Year == 2015])) %>%
  View()

#Extra features start below↓
#===================================================================================================================================================

#Extra features 1: 
#fct_infreq()
#Without
MightRetire %>% filter(Job == "Meat Cutter") %>% ggplot(aes(y = City, fill = Retire_TF))+geom_bar() + 
  labs(title = "Distribution of meat cutter that might retire in each city")
#With
MightRetire %>% filter(Job == "Meat Cutter") %>% ggplot(aes(fct_infreq(City), fill = Retire_TF))+geom_bar() + xlab("City") + coord_flip() + 
  labs(title = "Distribution of meat cutter that might retire in each city")

#Extra features 2: 
#fct_rev()
#Without
MightRetire %>% filter(Job == "Meat Cutter") %>% ggplot(aes(fct_infreq(City), fill = Retire_TF))+geom_bar() + xlab("City") + coord_flip() + 
  labs(title = "Distribution of meat cutter that might retire in each city")
#With
MightRetire %>% filter(Job == "Meat Cutter") %>% ggplot(aes(fct_rev(fct_infreq(City)), fill = Retire_TF))+geom_bar() + xlab("City") + coord_flip() + 
  labs(title = "Distribution of meat cutter that might retire in each city")


