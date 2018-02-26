setwd("/home/karan/DataAnalytics/Divorce")
divorce <- read.csv("adult.csv", header=TRUE)
#For the map plot
adult<-read.csv("adult.csv",header=TRUE)

library(class)
library(kernlab)
library(ROCR)

#All textual columns are factors. Convert to characters to modify if necessary while cleaning.
divorce$workclass = as.character(divorce$workclass)
divorce$occupation = as.character(divorce$occupation)
divorce$native_country = as.character(divorce$native_country)
divorce$education = as.character(divorce$education)

table(divorce$workclass)
#remember to use spaces
divorce$workclass = gsub("^ Federal-gov"," Federal-Govt",divorce$workclass)
divorce$workclass = gsub("^ Local-gov"," Other-Govt",divorce$workclass)
divorce$workclass = gsub("^ State-gov"," Other-Govt",divorce$workclass)
divorce$workclass = gsub("^ Private"," Private",divorce$workclass)
divorce$workclass = gsub("^ Self-emp-inc"," Self-Employed",divorce$workclass)
divorce$workclass = gsub("^ Self-emp-not-inc"," Self-Employed",divorce$workclass)
divorce$workclass = gsub("^ Without-pay"," Not-Working",divorce$workclass)
divorce$workclass = gsub("^ Never-worked"," Not-Working",divorce$workclass)

table(divorce$occupation)
divorce$occupation = gsub("^ Adm-clerical"," Admin",divorce$occupation)
divorce$occupation = gsub("^ Armed-Forces"," Military",divorce$occupation)
divorce$occupation = gsub("^ Craft-repair"," Blue-Collar",divorce$occupation)
divorce$occupation = gsub("^ Exec-managerial"," White-Collar",divorce$occupation)
divorce$occupation = gsub("^ Farming-fishing"," Blue-Collar",divorce$occupation)
divorce$occupation = gsub("^ Handlers-cleaners"," Blue-Collar",divorce$occupation)
divorce$occupation = gsub("^ Machine-op-inspct"," Blue-Collar",divorce$occupation)
divorce$occupation = gsub("^ Other-service"," Service",divorce$occupation)
divorce$occupation = gsub("^ Priv-house-serv"," Service",divorce$occupation)
divorce$occupation = gsub("^ Prof-specialty"," Professional",divorce$occupation)
divorce$occupation = gsub("^ Protective-serv"," Other-Occupations",divorce$occupation)
divorce$occupation = gsub("^ Sales"," Sales",divorce$occupation)
divorce$occupation = gsub("^ Tech-support"," Other-Occupations",divorce$occupation)
divorce$occupation = gsub("^ Transport-moving"," Blue-Collar",divorce$occupation)

#grouped by geography, political organization or economic zones
#Euro_1 refers to affluent European countries
divorce$native_country[divorce$native_country==" Cambodia"] = " SE-Asia"
divorce$native_country[divorce$native_country==" Canada"] = " British-Commonwealth"    
divorce$native_country[divorce$native_country==" China"] = " China"       
divorce$native_country[divorce$native_country==" Columbia"] = " South-America"    
divorce$native_country[divorce$native_country==" Cuba"] = " Other"        
divorce$native_country[divorce$native_country==" Dominican-Republic"] = " Latin-America"
divorce$native_country[divorce$native_country==" Ecuador"] = " South-America"     
divorce$native_country[divorce$native_country==" El-Salvador"] = " South-America"
divorce$native_country[divorce$native_country==" England"] = " British-Commonwealth"
divorce$native_country[divorce$native_country==" France"] = " Euro_1"
divorce$native_country[divorce$native_country==" Germany"] = " Euro_1"
divorce$native_country[divorce$native_country==" Greece"] = " Euro_2"
divorce$native_country[divorce$native_country==" Guatemala"] = " Latin-America"
divorce$native_country[divorce$native_country==" Haiti"] = " Latin-America"
divorce$native_country[divorce$native_country==" Holand-Netherlands"] = " Euro_1"
divorce$native_country[divorce$native_country==" Honduras"] = " Latin-America"
divorce$native_country[divorce$native_country==" Hong"] = " China"
divorce$native_country[divorce$native_country==" Hungary"] = " Euro_2"
#divorce$native_country[divorce$native_country==" India"] = " British-Commonwealth"
#we keep India separate for analysis, do not group
divorce$native_country[divorce$native_country==" Iran"] = " Other"
divorce$native_country[divorce$native_country==" Ireland"] = " British-Commonwealth"
divorce$native_country[divorce$native_country==" Italy"] = " Euro_1"
divorce$native_country[divorce$native_country==" Jamaica"] = " Latin-America"
divorce$native_country[divorce$native_country==" Japan"] = " Other"
divorce$native_country[divorce$native_country==" Laos"] = " SE-Asia"
divorce$native_country[divorce$native_country==" Mexico"] = " Latin-America"
divorce$native_country[divorce$native_country==" Nicaragua"] = " Latin-America"
divorce$native_country[divorce$native_country==" Outlying-US(Guam-USVI-etc)"] = " Latin-America"
divorce$native_country[divorce$native_country==" Peru"] = " South-America"
divorce$native_country[divorce$native_country==" Philippines"] = " SE-Asia"
divorce$native_country[divorce$native_country==" Poland"] = " Euro_2"
divorce$native_country[divorce$native_country==" Portugal"] = " Euro_2"
divorce$native_country[divorce$native_country==" Puerto-Rico"] = " Latin-America"
divorce$native_country[divorce$native_country==" Scotland"] = " British-Commonwealth"
divorce$native_country[divorce$native_country==" South"] = " Euro_2"
divorce$native_country[divorce$native_country==" Taiwan"] = " China"
divorce$native_country[divorce$native_country==" Thailand"] = " SE-Asia"
divorce$native_country[divorce$native_country==" Trinadad&Tobago"] = " Latin-America"
divorce$native_country[divorce$native_country==" United-States"] = " United-States"
divorce$native_country[divorce$native_country==" Vietnam"] = " SE-Asia"
divorce$native_country[divorce$native_country==" Yugoslavia"] = " Euro_2"
table(divorce$native_country)

#can use education_num instead of education

is.na(divorce) = divorce=='?'
is.na(divorce) = divorce==' ?'
divorce = na.omit(divorce)


divorce$workclass = as.factor(divorce$workclass)
divorce$occupation = as.factor(divorce$occupation)
divorce$native_country = as.factor(divorce$native_country)
divorce$education = as.factor(divorce$education)

divorce$income = as.factor(ifelse(divorce$income==divorce$income[1],0,1))
#turning income into binary 0-1, so now 0 means <=50K

# divorce$age = scale(divorce$age)
# divorce$hours_per_week = scale(divorce$hours_per_week)

nrow(divorce) #32561
divorcees = subset(divorce, marital_status == " Divorced")
separated = subset(divorce, marital_status == " Separated")
nrow(divorcees) #4443, down to 4214 after removing NAs. We do not lose much
nrow(separated) #1025, down to 939 after removing NAs
divsep = subset(divorce, marital_status == " Separated" | marital_status == " Divorced")
nrow(divsep) #5468, down to 5153 after removing NAs

military = subset(divorce, occupation == " Military")
print(military)
#There are too few military members to draw any sort of conclusion, but we see that 6 out of 9 have never been married.

divorce_copy = divorce
str(divorce_copy)
#In this copy, we group divorced and separated into Divorced, and the others into Not-Divorced to make boxplots
#Two boxplots for every continuous predictor: one for Divorced and one for Not-Divorced
divorce_copy$marital_status = as.character(divorce_copy$marital_status)
divorce_copy$marital_status = gsub("^ Separated"," Divorced",divorce_copy$marital_status)
divorce_copy$marital_status = gsub("^ Married-AF-spouse"," Not-Divorced",divorce_copy$marital_status)
divorce_copy$marital_status = gsub("^ Married-civ-spouse"," Not-Divorced",divorce_copy$marital_status)
divorce_copy$marital_status = gsub("^ Never-married"," Not-Divorced",divorce_copy$marital_status)
divorce_copy$marital_status = gsub("^ Widowed"," Not-Divorced",divorce_copy$marital_status)
divorce_copy$marital_status = gsub("^ Married-spouse-absent"," Not-Divorced",divorce_copy$marital_status)
divorce_copy$marital_status = as.factor(divorce_copy$marital_status)


#CLEANING THE TEST DATA
#Importing the dataset first
test<-read.csv('adult_test.csv',header=TRUE)

test$workclass = as.character(test$workclass)
test$occupation = as.character(test$occupation)
test$native_country = as.character(test$native_country)
test$education = as.character(test$education)

test$workclass = gsub("^ Federal-gov"," Federal-Govt",test$workclass)
test$workclass = gsub("^ Local-gov"," Other-Govt",test$workclass)
test$workclass = gsub("^ State-gov"," Other-Govt",test$workclass)
test$workclass = gsub("^ Private"," Private",test$workclass)
test$workclass = gsub("^ Self-emp-inc"," Self-Employed",test$workclass)
test$workclass = gsub("^ Self-emp-not-inc"," Self-Employed",test$workclass)
test$workclass = gsub("^ Without-pay"," Not-Working",test$workclass)
test$workclass = gsub("^ Never-worked"," Not-Working",test$workclass)

test$occupation = gsub("^ Adm-clerical"," Admin",test$occupation)
test$occupation = gsub("^ Armed-Forces"," Military",test$occupation)
test$occupation = gsub("^ Craft-repair"," Blue-Collar",test$occupation)
test$occupation = gsub("^ Exec-managerial"," White-Collar",test$occupation)
test$occupation = gsub("^ Farming-fishing"," Blue-Collar",test$occupation)
test$occupation = gsub("^ Handlers-cleaners"," Blue-Collar",test$occupation)
test$occupation = gsub("^ Machine-op-inspct"," Blue-Collar",test$occupation)
test$occupation = gsub("^ Other-service"," Service",test$occupation)
test$occupation = gsub("^ Priv-house-serv"," Service",test$occupation)
test$occupation = gsub("^ Prof-specialty"," Professional",test$occupation)
test$occupation = gsub("^ Protective-serv"," Other-Occupations",test$occupation)
test$occupation = gsub("^ Sales"," Sales",test$occupation)
test$occupation = gsub("^ Tech-support"," Other-Occupations",test$occupation)
test$occupation = gsub("^ Transport-moving"," Blue-Collar",test$occupation)

test$native_country[test$native_country==" Cambodia"] = " SE-Asia"
test$native_country[test$native_country==" Canada"] = " British-Commonwealth"    
test$native_country[test$native_country==" China"] = " China"       
test$native_country[test$native_country==" Columbia"] = " South-America"    
test$native_country[test$native_country==" Cuba"] = " Other"        
test$native_country[test$native_country==" Dominican-Republic"] = " Latin-America"
test$native_country[test$native_country==" Ecuador"] = " South-America"     
test$native_country[test$native_country==" El-Salvador"] = " South-America"
test$native_country[test$native_country==" England"] = " British-Commonwealth"
test$native_country[test$native_country==" France"] = " Euro_1"
test$native_country[test$native_country==" Germany"] = " Euro_1"
test$native_country[test$native_country==" Greece"] = " Euro_2"
test$native_country[test$native_country==" Guatemala"] = " Latin-America"
test$native_country[test$native_country==" Haiti"] = " Latin-America"
test$native_country[test$native_country==" Holand-Netherlands"] = " Euro_1"
test$native_country[test$native_country==" Honduras"] = " Latin-America"
test$native_country[test$native_country==" Hong"] = " China"
test$native_country[test$native_country==" Hungary"] = " Euro_2"
#test$native_country[test$native_country==" India"] = " British-Commonwealth"
#we keep India separate for analysis, do not group
test$native_country[test$native_country==" Iran"] = " Other"
test$native_country[test$native_country==" Ireland"] = " British-Commonwealth"
test$native_country[test$native_country==" Italy"] = " Euro_1"
test$native_country[test$native_country==" Jamaica"] = " Latin-America"
test$native_country[test$native_country==" Japan"] = " Other"
test$native_country[test$native_country==" Laos"] = " SE-Asia"
test$native_country[test$native_country==" Mexico"] = " Latin-America"
test$native_country[test$native_country==" Nicaragua"] = " Latin-America"
test$native_country[test$native_country==" Outlying-US(Guam-USVI-etc)"] = " Latin-America"
test$native_country[test$native_country==" Peru"] = " South-America"
test$native_country[test$native_country==" Philippines"] = " SE-Asia"
test$native_country[test$native_country==" Poland"] = " Euro_2"
test$native_country[test$native_country==" Portugal"] = " Euro_2"
test$native_country[test$native_country==" Puerto-Rico"] = " Latin-America"
test$native_country[test$native_country==" Scotland"] = " British-Commonwealth"
test$native_country[test$native_country==" South"] = " Euro_2"
test$native_country[test$native_country==" Taiwan"] = " China"
test$native_country[test$native_country==" Thailand"] = " SE-Asia"
test$native_country[test$native_country==" Trinadad&Tobago"] = " Latin-America"
test$native_country[test$native_country==" United-States"] = " United-States"
test$native_country[test$native_country==" Vietnam"] = " SE-Asia"
test$native_country[test$native_country==" Yugoslavia"] = " Euro_2"

is.na(test) = test=='?'
is.na(test) = test==' ?'
test = na.omit(test)


test$workclass = as.factor(test$workclass)
test$occupation = as.factor(test$occupation)
test$native_country = as.factor(test$native_country)
test$education = as.factor(test$education)

test$income = as.factor(ifelse(test$income==test$income[1],0,1))
#turning income into binary 0-1, so now 0 means <=50K

test$marital_status = as.character(test$marital_status)
test$marital_status = gsub("^ Separated"," Divorced",test$marital_status)
test$marital_status = gsub("^ Married-AF-spouse"," Not-Divorced",test$marital_status)
test$marital_status = gsub("^ Married-civ-spouse"," Not-Divorced",test$marital_status)
test$marital_status = gsub("^ Never-married"," Not-Divorced",test$marital_status)
test$marital_status = gsub("^ Widowed"," Not-Divorced",test$marital_status)
test$marital_status = gsub("^ Married-spouse-absent"," Not-Divorced",test$marital_status)
test$marital_status = as.factor(test$marital_status)

#Ground Truth Variable
actual<-test$marital_status
test2 = test
#Testing dataset 
test<-test[,-c(6)]

library(plotly)
div_age = subset(divorce_copy, marital_status == " Divorced")$age
not_div_age = subset(divorce_copy, marital_status == " Not-Divorced")$age
vs_age <- plot_ly(y = ~div_age, type = "box", name = "Divorced") %>%
  add_trace(y = ~not_div_age, name = "Not divorced")
vs_age

div_hours = subset(divorce_copy, marital_status == " Divorced")$hours_per_week
not_div_hours = subset(divorce_copy, marital_status == " Not-Divorced")$hours_per_week
vs_hours <- plot_ly(y = ~div_hours, type = "box", name = "Divorced") %>%
  add_trace(y = ~not_div_hours, name = "Not divorced")
vs_hours

div_edu = subset(divorce_copy, marital_status == " Divorced")$education_num
not_div_edu = subset(divorce_copy, marital_status == " Not-Divorced")$education_num
vs_edu <- plot_ly(y = ~div_edu, type = "box", name = "Divorced") %>%
  add_trace(y = ~not_div_edu, name = "Not divorced")
vs_edu

#Corrplot for numeric attributes
correl = cor(divorce[,c("age", "hours_per_week", "education_num")])
library(corrplot)
corrplot(correl, method = "circle")
#We don't see much correlation at all between them


#exploring categorical data with table()
table(divorce_copy[,c("sex", "marital_status")])



#Plots
library(ggplot2)
library(scales)
#Barplot of divorcees by zone
ggplot(divorcees,aes(native_country))+geom_bar(aes(fill=native_country))+theme(axis.text.x = element_text(angle=90,hjust=1)) +labs(x="Native Zone",y="No.of divorcees",title="Number of divorcees by zone")+guides(fill=FALSE)+geom_text(stat="count",aes(label=..count..),vjust=-0.25)
#For countries other than the US, we see that Latin America and more affluent European countries (the "Euro 1" zone) have the most representatives. In Euro 1's case, this may suggest that divorce is more likely with a high income.
#However, these numbers must be compared with the total populations.


#Barplot of Marital Status in the United States
us<-divorce[which(divorce$native_country==" United-States"),]
ggplot(us,aes(x=marital_status))+geom_bar(aes(y=(..count..)/sum(..count..)*100,fill=marital_status))+theme(axis.text.x = element_text(angle=45,hjust=1))+labs(x="Marital Status",y='Percentage',title='Marital Status in the US')+geom_text(stat="count",aes(label=round((..count..)/sum(..count..)*100,digits=2),y=(..count..)/sum(..count..)*100),vjust=-0.25)+guides(fill=FALSE)
#There are 3 main categories. Interestingly, there are more than twice as many adults who have never married than those who have been divorced.

#Divorce rates by Zone
d<-as.vector(unique(divorce$native_country))
zonerate<-data.frame(zone=character(length(d)),rate=numeric(length(d)))
for(i in 1:length(d))
{
  levels(zonerate$zone)<-c(levels(zonerate$zone),d[i])
  zonerate$zone[i]<-toString(d[i])
  zonerate$rate[i]<-length(which(divorce$native_country==d[i] & divorce$marital_status==" Divorced"))/length(which(divorce$native_country==d[i]))
}
ggplot(zonerate,aes(x=zone,y=rate))+geom_bar(stat="identity",aes(fill=zone))+theme(axis.text.x=element_text(angle=90,hjust=1))+labs(x="Native Zone",y="Divorce Rate",title="Divorce Rate")+guides(fill=FALSE)
#Using a crude measure of divorce rate (as we cannot conduct a longitudinal study), we see that Euro 1 natives are more likely to be divorced
#than US natives. Though Latin America produced the second highest number of divorcees in a previous graph, we see that they have a low divorce rate (i.e. relative to population)


#Histogram for Ages by Gender CONVERTED TO DIVORCE
ggplot(divsep,aes(x=age,colour=sex))+geom_histogram(binwidth=2,aes(fill=sex),alpha=0.5,position="identity")+labs(x="Age",title="Histogram of Ages of Divorcees")
#We observe that there are more female divorcees in than male at virtually every age, with very large differences from ages 35-50.


#Pie chart for Gender in the dataset
gendnum<-rbind(length(which(divorce$sex==" Male")),length(which(divorce$sex==" Female")))
gendnum<-as.data.frame(cbind(c("Male","Female"),gendnum))
colnames(gendnum)<-c("Gender","Value")
plot_ly(gendnum,labels=~Gender,values=~Value,type='pie',textinfo='label+percent',insidetextfont=list(color='FFFFFF'),showlegend=FALSE) %>% layout(title="Males and Females",xaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE),yaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE))

#Pie chart for workclass (not divorced)
wcf<-as.vector(unique(subset(divorce_copy, marital_status == " Not-Divorced")$workclass))
wc<-data.frame(class=character(length(wcf)),count=integer(length(wcf)))
levels(wc$class)<-wcf
for(i in 1:length(wcf))
{
  wc$class[i]<-wcf[i]
  wc$count[i]<-length(which(divorce$workclass==wcf[i]))
}
plot_ly(wc,labels=~class,values=~count,insidetextfont=list(color='FFFFFF')) %>% add_pie(hole=0.5) %>% layout(title="Work class (not divorced)",showlegend=TRUE)

#Pie chart for workclass (divorced)
wcf<-as.vector(unique(divsep$workclass))
wc<-data.frame(class=character(length(wcf)),count=integer(length(wcf)))
levels(wc$class)<-wcf
for(i in 1:length(wcf))
{
  wc$class[i]<-wcf[i]
  wc$count[i]<-length(which(divorce$workclass==wcf[i]))
}
plot_ly(wc,labels=~class,values=~count,insidetextfont=list(color='FFFFFF')) %>% add_pie(hole=0.5) %>% layout(title="Work class (divorced)",showlegend=TRUE)
#There is almost no difference in the work distribution. We see that 0.1% more divorcees tend to be self-employed than non-divorcees.



#Scatterplot for Age CONVERTED TO DIVORCE
divsep %>% group_by(age) %>% summarise(count=n())%>%plot_ly(x=~age,y=~count,type='scatter',mode='markers') %>% layout(title='Age Distribution of divorcees',xaxis=list(title='Age'),yaxis=list(title='Count'))
#We have a symmetric distribution with a sharp peak at around 40 years of age. We see that the longer a marriage lasts after the partners cross 40, the lower the likelihood of divorce though this depends on the population.


#Stacked bar chart of Age vs Marital Status CONVERTED TO DIVORCE
bins<-c("17-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90")
avi<-data.frame(age=character(length(bins)),div=integer(length(bins)),notdiv=integer(length(bins)))
levels(avi$age)<-bins
lb<-11
ub<-20
for(i in 1:length(bins))
{
  avi$age[i]<-bins[i]
  avi$div[i]<-length(which(divorce_copy$marital_status==" Divorced" & divorce_copy$age%in%c(lb:ub)))
  avi$notdiv[i]<-length(which(divorce_copy$marital_status==" Not-Divorced" & divorce_copy$age%in%c(lb:ub)))             
  lb<-lb+10
  ub<-ub+10
}

plot_ly(avi,x=~age,y=~div,type='bar',name='Divorced') %>% add_trace(y=~notdiv,name='Not divorced') %>% layout(yaxis=list(title="Number of entries"),barmode='stack',xaxis=list(title="Age Group"),title="Age Group vs Marital Status")
#We see that the largest ratio of divorced:not divorced occurs in the 41-50 age bin. There seem to be no divorcees in the 81-90 bin and few beyond 61, confirming our earlier conjecture that a longer a marriage lasts after 40, the stronger it is.


#Stacked bar chart of Race vs Marital Status
bins<-c(" White", " Black", " Asian-Pac-Islander", " Amer-Indian-Eskimo", " Other")
avi<-data.frame(race=character(length(bins)),div=integer(length(bins)),notdiv=integer(length(bins)))
levels(avi$race)<-bins
for(i in 1:length(bins))
{
  avi$race[i]<-bins[i]
  avi$div[i]<-length(which(divorce_copy$marital_status==" Divorced" & divorce_copy$race == bins[i]))
  avi$notdiv[i]<-length(which(divorce_copy$marital_status==" Not-Divorced" & divorce_copy$race == bins[i]))             
}
plot_ly(avi,x=~race,y=~div,type='bar',name='Divorced') %>% add_trace(y=~notdiv,name='Not divorced') %>% layout(yaxis=list(title="Number of entries"),barmode='stack',xaxis=list(title="Race"),title="Race vs Marital Status")
#Black people seem to have the largest percentage of divorces. Taking a closer look:
table(subset(divorce_copy, race == " Black")$marital_status)
table(subset(divorce_copy, race == " White")$marital_status)
#24.5% of the Black population is divorced, compared to 16.4% of the White population.

#Grouped Bar Chart of Marital Status and WorkClass
wcm<-data.frame(class=character(length(wcf)),d=integer(length(wcf)),nd=integer(length(wcf)))
levels(wcm$class)<-wcf
for(i in 1:length(wcf))
{
  wcm$class[i]<-wcf[i]
  wcm$d[i]<-length(which(divorce_copy$workclass==wcf[i] & divorce_copy$marital_status==" Divorced"))
  wcm$nd[i]<-length(which(divorce_copy$workclass==wcf[i] & divorce_copy$marital_status==" Not-Divorced"))
}
wcm %>% plot_ly() %>% add_trace(x = ~class, y = ~d, type = 'bar',text='Divorced',name='Divorced',textposition = 'auto',marker = list(color = 'darkolivegreen3',line = list(color = 'white', width = 1.5))) %>% add_trace(x = ~class, y = ~nd,name='Not-Divorced',text='Not-Divorced',type = 'bar', textposition = 'auto',marker = list(color = 'deepskyblue1',line = list(color = 'white', width = 1.5))) %>% layout(title = "Divorce for Each Workclass Group",barmode = 'group',xaxis = list(title = ""),yaxis = list(title = ""))
#It looks roughly proportional for every work sector, leading us to believe that it is not a significant predictor.

#Donut chart for 0s and non 0s in capital_gain and capital_loss
cg<-data.frame(class=character(2),count=integer(2))
levels(cg$class)<-c('Zeroes','Non-Zeroes')
cg$class<-c('Zeroes','Non-Zeroes')
cg$count[1]<-length(which(divorce_copy$capital_gain==0))
cg$count[2]<-length(which(divorce_copy$capital_gain!=0))
plot_ly(cg,labels=~class,values=~count,insidetextfont=list(color='FFFFFF'))%>% add_pie(hole=0.5)%>% layout(title="Capital Gain Values",showlegend=TRUE)

cl<-data.frame(class=character(2),count=integer(2))
levels(cl$class)<-c('Zeroes','Non-Zeroes')
cl$class<-c('Zeroes','Non-Zeroes')
cl$count[1]<-length(which(divorce_copy$capital_loss==0))
cl$count[2]<-length(which(divorce_copy$capital_loss!=0))
plot_ly(cl,labels=~class,values=~count,insidetextfont=list(color='FFFFFF'))%>% add_pie(hole=0.5) %>% layout(title="Capital Loss Values",showlegend=TRUE)
#Based on the above plots, we can disregard capital_gain and capital_loss because most of the values are 0.


#Highest Level of Education vs Divorce
edu<-levels(divorce_copy$education)
led<-data.frame(ed=character(length(edu)),div=integer(length(edu)),ndiv=integer(length(edu)))
levels(led$ed)<-edu
for(i in 1:length(edu))
{
  led$ed[i]<-edu[i]
  led$div[i]<-length(which(divorce_copy$education==edu[i] & divorce_copy$marital_status==" Divorced"))
  led$ndiv[i]<-length(which(divorce_copy$education==edu[i] & divorce_copy$marital_status==" Not-Divorced"))
}
plot_ly(led,x=~ed,y=~div,type='bar',name="Divorced") %>% add_trace(y=~ndiv,name="Not-Divorced") %>% layout(yaxis=list(title="Number of Entries"),barmode='stack',title="Highest Level of Education vs Divorce")
#A drastic fall in the ratio of divorced:not divorced occurs at an education number of 13, which corresponds to a Bachelor's degree

#Workclass vs Divorce
wcl<-levels(divorce_copy$workclass)
wcd<-data.frame(wc=character(length(wcl)),div=integer(length(wcl)),ndiv=integer(length(wcl)))
levels(wcd$wc)<-wcl
for(i in 1:length(wcl))
{
  wcd$wc[i]<-wcl[i]
  wcd$div[i]<-length(which(divorce_copy$workclass==wcl[i] & divorce_copy$marital_status==" Divorced"))
  wcd$ndiv[i]<-length(which(divorce_copy$workclass==wcl[i] & divorce_copy$marital_status==" Not-Divorced"))
}
plot_ly(wcd,x=~wc,y=~div,type='bar',name='Divorced') %>% add_trace(y=~ndiv,name="Not-Divorced") %>% layout(yaxis=list(title="Number of Entries"),barmode='stack',xaxis=list(title="Workclass"),title="Workclass vs Divorce")
#The divorced:not divorced ratio appears to be roughly 1:5 across the board. There is no appreciable difference between work sectors.

#Occupation vs Divorce
oc<-levels(divorce_copy$occupation)
ocd<-data.frame(o=character(length(oc)),div=integer(length(oc)),ndiv=integer(length(oc)))
levels(ocd$o)<-oc
for(i in 1:length(oc))
{
  ocd$o[i]<-oc[i]
  ocd$div[i]<-length(which(divorce_copy$occupation==oc[i] & divorce_copy$marital_status==" Divorced"))
  ocd$ndiv[i]<-length(which(divorce_copy$occupation==oc[i] & divorce_copy$marital_status==" Not-Divorced"))
}
plot_ly(ocd,x=~o,y=~div,type='bar',name='Divorced') %>% add_trace(y=~ndiv,name="Not-Divorced") %>% layout(yaxis=list(title="Number of Entries"),barmode='stack',xaxis=list(title="Occupation"),title="Occupation and Divorce")
#Fairly uniform except for blue-collar workers who clearly have the lowest divorce rate. This graph contradicts reports that state a higher divorce rate for blue-collar workers.

#Hours per Week vs Divorce
h<-c("<40","40",">40")
hpd<-data.frame(class=character(length(h)),div=integer(length(h)),ndiv=integer(length(h)))
levels(hpd$class)<-h
hours<-40

hpd$class[1]<-h[1]
hpd$div[1]<-length(which(divorce_copy$hours_per_week<hours & divorce_copy$marital_status==" Divorced"))
hpd$ndiv[1]<-length(which(divorce_copy$hours_per_week<hours & divorce_copy$marital_status==" Not-Divorced"))

hpd$class[2]<-h[2]
hpd$div[2]<-length(which(divorce_copy$hours_per_week==hours & divorce_copy$marital_status==" Divorced"))
hpd$ndiv[2]<-length(which(divorce_copy$hours_per_week==hours & divorce_copy$marital_status==" Not-Divorced"))

hpd$class[3]<-h[3]
hpd$div[3]<-length(which(divorce_copy$hours_per_week>hours & divorce_copy$marital_status==" Divorced"))
hpd$ndiv[3]<-length(which(divorce_copy$hours_per_week>hours & divorce_copy$marital_status==" Not-Divorced"))

plot_ly(hpd,x=~class,y=~div,type='bar',name="Divorced") %>% add_trace(y=~ndiv,name="Not-Divorced") %>% layout(barmode='stack',yaxis=list(title="Number of Entries"),xaxis=list(title="Number of Hours"),title="Number of Hours Per Week and Divorce")
#We observe that divorce rates are the highest for those who work less than 40 hours a week and are close for those who work 40 or more hours.

#Income vs Divorce
inc<-c("<=50k", ">50k")
inco<-data.frame(i=character(length(inc)),div=integer(length(inc)),ndiv=integer(length(inc)))
levels(inco$i)<-inc
for(i in 1:length(inc))
{
  inco$i[i]<-inc[i]
  inco$div[i]<-length(which(divorce_copy$income==i-1 & divorce_copy$marital_status==" Divorced"))
  inco$ndiv[i]<-length(which(divorce_copy$income==i-1 & divorce_copy$marital_status==" Not-Divorced"))
}
plot_ly(inco,x=~i,y=~div,type='bar',name='Divorced') %>% add_trace(y=~ndiv,name="Not-Divorced") %>% layout(yaxis=list(title="Number of Entries"),barmode='stack',xaxis=list(title="Income"),title="Income and Divorce")
#Here, we see a much lower divorce rate for incomes above $50,000 a year.


#Are certain races more likely to be separated?
white = subset(divorce, race == " White")
black = subset(divorce, race == " Black")
nrow(subset(white, marital_status == " Divorced")) #3618
nrow(subset(white, marital_status == " Separated")) #658
nrow(subset(black, marital_status == " Divorced")) #452
nrow(subset(black, marital_status == " Separated")) #240
race_separate <- plot_ly(
  x = c("White Divorced", "White Separated", "Black Divorced", "Black Separated"),
  y = c(nrow(subset(white, marital_status == " Divorced")), nrow(subset(white, marital_status == " Separated")), nrow(subset(black, marital_status == " Divorced")),nrow(subset(black, marital_status == " Separated"))),
  name = "Race vs Separation and Divorce",
  type = "bar"
)
race_separate
#We see that many more Black people are separated as compared to White people. One possible reason for this is financial.
#Divorces are expensive and if you are separated, you are still liable for your spouse's debts. Also, a ten-year marriage (counting separation) qualifies divorcees for Social Security benefits.
#There are also health insurance and tax benefits.

#Are lower incomes more likely to be separated?
str(divorce)
less50 = subset(divorce, income == 0)
more50 = subset(divorce, income == 1)
nrow(less50) #22564
nrow(more50) #7508
nrow(subset(less50, marital_status == " Divorced")) #3762
nrow(subset(less50, marital_status == " Separated")) #873
nrow(subset(more50, marital_status == " Divorced")) #452
nrow(subset(more50, marital_status == " Separated")) #66
income_separate <- plot_ly(
  x = c("<=50k Divorced", "<=50k Separated", ">50k Divorced", ">50k Separated"),
  y = c(nrow(subset(less50, marital_status == " Divorced")), nrow(subset(less50, marital_status == " Separated")), nrow(subset(more50, marital_status == " Divorced")),nrow(subset(more50, marital_status == " Separated"))),
  name = "Income vs Separation and Divorce",
  type = "bar"
)
income_separate
#We calculate that with lower incomes, the ratio of separated:divorced is 232:1000 while it is 146:1000 for higher incomes.

#Now for Indians, out of curiosity.
indians = subset(divorce, native_country == " India")
#Every textual field in the dataset begins with a space.
nrow(indians) #100
print(indians)
sum(indians$fnlwgt) #16853909
sum(as.numeric(divorce$fnlwgt)) #6179373392
#If weight represents the number of similar people in America, there's a lot of overlap. Maybe it doesn't.
#Ignoring fnlwgt
table(indians$marital_status)
#2 divorced and 2 separated. We see that 4/100 Indians are either divorced or separated, which is a low rate.
print(subset(indians, marital_status == " Divorced" | marital_status == " Separated"))
#There isn't much we can conclude from this, but the results follow some of the patterns we have established.
#The ones with lower incomes are separated instead of divorced. The youngest one to be separated/divorced has an 11th grade education.





#MODELS
#Naive Bayes
#The training dataset
cat("Naive Bayesian Model 1")
training1<-divorce_copy
test1<-test
#Removing the column education_num because it conveys the same data that education column conveys
training1<-training1[,-c(5)]
#And hence from the testing dataset too
test1<-test1[,-c(5)]
NBmodel1<-naiveBayes(marital_status~.,data=training1)
predicted1<-predict(NBmodel1,test1)
pNB1<-match(predicted1,levels(predicted1))
pNB1<-prediction(pNB1,actual)
aucNB1<-performance(pNB1,'auc')
aucNB1<-aucNB1@y.values[[1]]
sprintf("Area Under the Curve Naive Bayesian  Model 1 is %f",aucNB1)
confMatrixNB1<-confusionMatrix(predicted1,actual,positive=' Divorced',dnn=c("Predicted","Ground Truth"))
confMatrixNB1$table
sprintf("Naive Bayesian Model 1 - Accuracy is %f%%", confMatrixNB1$overall[1]*100)
sprintf("Naive Bayesian Model 1 - F1 score is %f%%", confMatrixNB1$byClass[7]*100)

#Reducing the no. of dimensions and then trying
#By removing the capital_gain and capital_loss fields because they have a high percentage of zeroes
#and very few non zero values, so they do not really contribute anything new.
cat("Naive Bayesian Model 2")
training2<-divorce_copy
training2<-training2[,-c(5,11,12)]
#And hence in the testing dataset too
test2<-test
test2<-test2[,-c(5,9,10)]
NBmodel2<-naiveBayes(marital_status~.,data = training2)
predicted2<-predict(NBmodel2,test2)
pNB2<-match(predicted2,levels(predicted2))
pNB2<-prediction(pNB2,actual)
aucNB2<-performance(pNB2,'auc')
aucNB2<-aucNB2@y.values[[1]]
sprintf("Area Under the Curve for Naive Bayesian  Model 2 is %f",aucNB2)
confMatrixNB2<-confusionMatrix(predicted2,actual,positive=" Divorced",dnn=c("Predicted","Ground Truth"))
confMatrixNB2$table
sprintf("Naive Bayesian Model 2 - Accuracy is %f%%",confMatrixNB2$overall[1]*100)
sprintf("Naive Bayesian Model 2 - F1 score is %f%%",confMatrixNB2$byClass[7]*100)

#K Nearest Neighbours
#Since it needs to have only numerical data
#Converting all text data to numerical data in both training and testing datasets
cat("KNN Model 1")
training3<-divorce_copy[,-c(6)]
test3<-test

#Removing the education column 
training3<-training3[,-c(4)]
test3<-test3[,-c(4)]

training3$income<-as.integer(training3$income)
test3$income<-as.integer(test3$income)

normalize<-function(x){(x-min(x)/(max(x)-min(x)))}
#Normalizing all numerical data
for(i in 1:ncol(training3))
{
  if(!is.factor(training3[,i]))
    training3[,i]<-normalize(training3[,i])
}
for(i in 1:ncol(test3))
{
  if(!is.factor(test3[,i]))
    test3[,i]<-normalize(test3[,i])
}
#Converting all categorical data into numerical data
for(i in 1:ncol(training3))
{
  if(is.factor(training3[,i]))
  {
    training3[,i]<-match(training3[,i],levels(training3[,i]))
    training3[,i]<-as.integer(training3[,i])
  }
}
for(i in 1:ncol(test3))
{
  if(is.factor(test3[,i]))
  {
    test3[,i]<-match(test3[,i],levels(test3[,i]))
    test3[,i]<-as.numeric(test3[,i])
  }
}
cl<-as.vector(divorce_copy$marital_status)
knnpredicted<-knn(training3,test3,cl,k=3,prob=TRUE)
pKNN1<-match(knnpredicted,levels(knnpredicted))
pKNN1<-prediction(pKNN1,actual)
aucKNN1<-performance(pKNN1,'auc')
aucKNN1<-aucKNN1@y.values[[1]]
sprintf("Area Under the Curve for KNN Model 1 is %f",aucKNN1)
confMatrixKNN1<-confusionMatrix(knnpredicted,actual,positive=" Divorced",dnn=c("Predicted","Ground Truth"))
confMatrixKNN1$table
sprintf("KNN Model 1 - Accuracy is %f%%",confMatrixKNN1$overall[1]*100)
sprintf("KNN Model 1 - F1 score is %f%%",confMatrixKNN1$byClass[7]*100)

#Now removing capital_gain and capital_loss in both the data sets
cat("KNN Model 2")
training4<-training3[,-c(9,10)]
test4<-test3[,-c(9,10)]
knnpredicted2<-knn(training4,test4,cl,k=5,prob=TRUE)
pKNN2<-match(knnpredicted2,levels(knnpredicted2))
pKNN2<-prediction(pKNN2,actual)
aucKNN2<-performance(pKNN2,'auc')
aucKNN2<-aucKNN2@y.values[[1]]
sprintf("Area Under the Curve for KNN Model 2 is %f",aucKNN2)
confMatrixKNN2<-confusionMatrix(knnpredicted2,actual,positive=" Divorced",dnn=c("Predicted","Ground Truth"))
confMatrixKNN2$table
sprintf("KNN Model 2 - Accuracy is %f%%",confMatrixKNN2$overall[1]*100)
sprintf("KNN Model 2 - F1 score is %f%%",confMatrixKNN2$byClass[7]*100)

#SVM Gaussian Kernel
cat("Gaussian SVM")
marital_status<-divorce_copy$marital_status
training5<-cbind(training3,marital_status)
test5<-test3
svmgauss<-ksvm(marital_status~.,data=training5,kpar=list(sigma=0.50),kernel=rbfdot,C=2,cross=2)
svmprediction1<-predict(svmgauss,test3)
pSVMG<-match(svmprediction1,levels(svmprediction1))
pSVMG<-prediction(pSVMG,actual)
aucSVMG<-performance(pSVMG,'auc')
aucSVMG<-aucSVMG@y.values[[1]]
sprintf("Area Under the Curve for Gaussian SVM Model 1 is %f",aucSVMG)
confMatrixSVMG<-confusionMatrix(svmprediction1,actual,positive=" Divorced",dnn=c("Predicted","Ground-Truth"))
confMatrixSVMG$table
sprintf("Gaussian SVM - Accuracy is %f%%",confMatrixSVMG$overall[1]*100)
sprintf("Gaussian SVM - F1 score is %f%%",confMatrixSVMG$byClass[7]*100)

#Linear SVM
cat("Linear SVM")
training6<-training3[,-c(9,10)]
training6<-cbind(training6,marital_status)
test6<-test3[,-c(9,10)]
svmlinear<-svm(marital_status~.,data=training6,kernel="linear",cost=5,gamma=2)
svmprediction2<-predict(svmlinear,test6)
pSVML<-match(svmprediction2,levels(svmprediction2))
pSVML<-prediction(pSVML,actual)
aucSVML<-performance(pSVML,'auc')
aucSVML<-aucSVML@y.values[[1]]
sprintf("Area Under the Curve for Linear SVM Model 1 is %f",aucSVML)
confMatrixSVML<-confusionMatrix(svmprediction2,actual,positive=" Divorced",dnn=c("Predicted","Ground-Truth"))
confMatrixSVML$table
sprintf("Linear SVM - Accuracy is %f%%",confMatrixSVML$overall[1]*100)
sprintf("Linear SVM - F1 score is %f%%",confMatrixSVML$byClass[7]*100)

#Single RoC Plot
pNB1<-performance(pNB1,measure="tpr",x.measure="fpr")
plot(pNB1,col='red')

pNB2<-performance(pNB2,measure="tpr",x.measure="fpr")
plot(pNB2,add=TRUE,col='blue')

pKNN1<-performance(pKNN1,measure="tpr",x.measure="fpr")
plot(pKNN1,add=TRUE,col='green')
#title(main="RoC for KNN Model 1")

pKNN2<-performance(pKNN2,measure="tpr",x.measure="fpr")
plot(pKNN2,add=TRUE,col='yellow')
#title(main="RoC for KNN Model 2")

pSVMG<-performance(pSVMG,measure="tpr",x.measure="fpr")
plot(pSVMG,add=TRUE,col='violet')
#title(main="RoC for Gaussian SVM Model")

pSVML<-performance(pSVML,measure="tpr",x.measure="fpr")
plot(pSVML,add=TRUE,col='cyan')
#title(main="RoC for Linear SVM Model")

legend(0.7, 0.7, c("NB Model 1","NB Model 2", "KNN Model 1", "KNN Model 2","GSVM Model","LSVM Model"),lty=c(1,1), lwd=c(1.5,1.5),col=c("red", "blue", "green", "yellow","violet","cyan"),bty="n")

#The association rules
library(arules)
library(arulesViz)

for(i in 1:ncol(divorce_copy))
{
  divorce_copy[,i]<-as.factor(divorce_copy[,i])
}
rules<-apriori(divorce_copy,parameter = list(support=0.8,confidence=0.8,minlen=2))

#Visualizing them
plot(rules,method="graph")

#requirements for NBTree INCOMPLETE
library("rJava")
library("RWekajars")
library("RWeka")

WPM("refresh-cache")
WPM("package-info", "repository", "naiveBayesTree")
WPM("install-package", "naiveBayesTree")

WOW("weka/classifiers/trees/NBTree")

divorce_use = subset(divorce_copy,select = -c(fnlwgt, education, relationship, capital_gain, capital_loss))

test_use = subset(test2,select = -c(fnlwgt, education, relationship, capital_gain, capital_loss))

resultJ48 <- J48(as.factor(marital_status)~., divorce_use) 
dataTest.pred <- predict(resultJ48, newdata = test_use)
table(test_use$marital_status, dataTest.pred)
#                Divorced  Not-Divorced
# Divorced           797          1758
# Not-Divorced       546         11959
#Accuracy: 84.7%, but many from Divorced are classified into Not-Divorced

#LOGISTIC REGRESSION
library(caret)
library(ROSE)
table(divorce_use$marital_status)
#25009 not divorced and 5153 divorced

balanced_div <- ovun.sample(marital_status ~ ., data = divorce_use, method = "over", N = 25009*2)$data
table(balanced_div$marital_status)

model <- glm(marital_status ~ ., data=balanced_div, family=binomial(link='logit')) 
summary(model)
# Coefficients:
#                                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                  -1.404e+00  1.789e-01  -7.849 4.18e-15 ***
#   age                           4.419e-02  8.901e-04  49.650  < 2e-16 ***
#   workclass Not-Working        -1.332e+01  7.105e+01  -0.187 0.851278    
# workclass Other-Govt         -1.347e-01  6.403e-02  -2.104 0.035422 *  
#   workclass Private            -1.061e-01  5.863e-02  -1.810 0.070367 .  
# workclass Self-Employed      -3.995e-01  6.570e-02  -6.080 1.20e-09 ***
#   education_num                 1.622e-02  5.174e-03   3.135 0.001718 ** 
#   occupation Blue-Collar        5.116e-02  3.602e-02   1.420 0.155484    
# occupation Military          -1.190e+01  1.035e+02  -0.115 0.908501    
# occupation Other-Occupations  1.208e-01  5.264e-02   2.294 0.021776 *  
#   occupation Professional      -1.444e-01  4.210e-02  -3.430 0.000603 ***
#   occupation Sales             -2.075e-01  4.155e-02  -4.993 5.94e-07 ***
#   occupation Service           -1.336e-02  4.017e-02  -0.333 0.739473    
# occupation White-Collar       7.626e-02  4.050e-02   1.883 0.059661 .  
# race Asian-Pac-Islander      -6.889e-01  1.333e-01  -5.170 2.34e-07 ***
#   race Black                   -1.864e-01  1.011e-01  -1.844 0.065120 .  
# race Other                   -2.718e-01  1.489e-01  -1.826 0.067845 .  
# race White                   -4.118e-01  9.685e-02  -4.252 2.12e-05 ***
#   sex Male                     -1.435e+00  2.377e-02 -60.388  < 2e-16 ***
#   hours_per_week                2.365e-02  9.599e-04  24.638  < 2e-16 ***
#   native_country China         -6.295e-01  2.475e-01  -2.543 0.010996 *  
#   native_country Euro_1         2.339e-02  1.662e-01   0.141 0.888115    
# native_country Euro_2        -4.518e-01  1.776e-01  -2.544 0.010958 *  
#   native_country India         -8.447e-01  3.196e-01  -2.643 0.008208 ** 
#   native_country Latin-America -3.548e-01  1.325e-01  -2.678 0.007396 ** 
#   native_country Other          5.245e-01  1.688e-01   3.107 0.001891 ** 
#   native_country SE-Asia       -6.422e-01  1.919e-01  -3.346 0.000821 ***
#   native_country South-America -2.628e-01  1.690e-01  -1.555 0.119884    
# native_country United-States  1.371e-01  1.184e-01   1.158 0.246954    
# income1                      -1.309e+00  3.004e-02 -43.580  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 69340  on 50017  degrees of freedom
# Residual deviance: 58207  on 49988  degrees of freedom
# AIC: 58267


prediction <- predict(model, newdata = test_use, type = 'response')
str(prediction)
str(test_use)
table(test_use$marital_status, prediction > 0.5)
#               FALSE TRUE
# Divorced       796 1759
# Not-Divorced  8655 3850

#Accuracy: 69.1%

#Finding the relative importance of predictors
divorce_num = subset(divorce_use, select = c(age, hours_per_week, education_num, marital_status, income))
divorce_num$marital_status = ifelse(divorce_num$marital_status==divorce_num$marital_status[1],0,1)
library(relaimpo)
lmMod <- lm(marital_status ~ . , data = divorce_num)  # fit lm() model
relImportance <- calc.relimp(lmMod, type = "lmg", rela = TRUE)  # calculate relative importance scaled to 100
sort(relImportance$lmg, decreasing=TRUE)  
# income            age       education_num hours_per_week 
# 0.55197565     0.41778231     0.01539846     0.01484358 

#The above shows us that income and age are by far the most important deciding factors, followed by education and hours worked per week.
#Hence the people least likely to be divorced are those in certain age groups i.e. older than 61 or younger than 30 as shown by our graphs, and those with higher incomes.
#Followed by that in importance is education. We saw earlier that a Bachelor's degree or higher drastically brings down the chances of divorce. 
#This is arguably because they will be older and with better reasoning/decision-weighing skills after completing their education.





