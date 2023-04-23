############################################################
##Data Wrangling of Vulnificus Data from Ketty
##written by Andrea Ayala last change 1/5/23
##Reference sites:
##https://www.statmethods.net/management/merging.html
##https://www.statmethods.net/management/subset.html
##https://statsandr.com/blog/descriptive-statistics-in-r/
##https://www.marsja.se/how-to-rename-column-or-columns-in-r-with-dplyr/
##https://stackoverflow.com/questions/7531868/how-to-rename-a-single-column-in-a-data-frame
##http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
##http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
##https://stackoverflow.com/questions/41384075/r-calculate-and-interpret-odds-ratio-in-logistic-regression
##https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
##https://freyasystems.com/creating-a-dual-axis-plot-with-ggplot2/
##https://stackoverflow.com/questions/1296646/sort-order-data-frame-rows-by-multiple-columns
#####################################################################################

#Install packages

install.packages("googlesheets4")
install.packages("googledrive")
install.packages("gsheet")
install.packages("pastecs")

#Loading necessary packages
library(readr)
library(ggplot2)
library(googlesheets)
library(googlesheets4)
library(httpuv)
library(googledrive)
library(gsheet)
library(plyr)
library(pastecs)
library(dplyr)
library(data.table)
library(tidyr)
library(ggpubr)
library(TTAinterfaceTrendAnalysis)
library(stargazer)
library(RColorBrewer)
library(viridis)
library(glue)

rm(list=ls()) #this clears the workspace to make sure no leftover variables are left. Not strictly needed but often a good idea
#graphics.off(); #close all graphics windows, in case there are still some open from previous work performed

#Starting with descriptive statistics of cases

#Downloading the google sheet from the web - we have only one sheet, so we are going to end up with one dataframe

ss0<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1yVqSyVN1jFTDbqR5wbbQKVXcnpM6h6nXvZ2ucte2bzk/edit#gid=1082480308')

str(ss0) #shows the structure of the data

#Let's convert the variables into factors
ss0$Record<-as.factor(ss0$Record)
ss0$County<-as.factor(ss0$County)
ss0$Deaths.M1<-as.factor(ss0$Deaths.M1)
ss0$Month.M...4<-as.factor(ss0$Month.M...4)
ss0$Season.M...5<-as.factor(ss0$Season.M...5)
ss0$Year.M<-as.factor(ss0$Year.M)
ss0$Cases.M1<-as.numeric(ss0$Cases.M1)
ss0$Coastal<-as.factor(ss0$Coastal)
ss0$NOAA_Stat<-as.factor(ss0$NOAA_Stat)
ss0$WMA.M<-as.factor(ss0$WMA.M)
ss0$Month.M...11<-as.factor(ss0$Month.M...11)
ss0$Season.M...12<-as.factor(ss0$Season.M...12)

str(ss0)

#Since our data is representative of individual cases, we will need to aggregate the data to plot cases by county

ss_county_occurences<-table(unlist(ss0$County))
ss_county_occurences

#Now let's convert our table to a dataframe so we can plot the occurrences of this
ss_county_occurences1<- aggregate(ss0$Cases.M1, by=list(County=ss0$County), FUN=sum)

#Let's check out our data
head(ss_county_occurences1, 50)

#Let's convert the x variable to a factor so we can play with the levels
ss_county_occurences1$x<-as.factor(ss_county_occurences1$x)

levels(ss_county_occurences1$x) #it's in alphabetical order, but we want descending order

ss_county_occurences1<-ss_county_occurences1 %>% arrange(desc(x)) #this puts our counties into descending order
View(ss_county_occurences1)

#let's plot the total cases from the greatest to the least by running a factor() function. 
#This works only if your data is already in order from greatest to least of the total.

ss_county_occurences1$County <- factor(ss_county_occurences1$County, ss_county_occurences1$County)

#Let's plot this data


#figa<-ggplot(ss_county_occurences1, aes(x = County, y = x, fill = County)) +
#  geom_bar(stat = "identity") +
#  xlab("Florida Counties") +
#  ylab("Total V.vulnificus cases") +
#  ggtitle("Florida Counties V. vulnificus Occurrences") +
#  scale_fill_grey(start = 0, end = .9) +
#  theme_bw() +
#  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
#  theme(plot.title = element_text(hjust = 0.5))  #Success!
#figa



figa<-ggplot(ss_county_occurences1, aes(x = County, y = x, fill = County)) +
  geom_bar(stat = "identity") +
  xlab("Florida Counties") +
  ylab("Total V.vulnificus cases") +
  ggtitle("Florida Counties V. vulnificus Occurrences") +
  scale_fill_viridis_d()+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5))  #Success!
figa

#Now let's look at our data according to Month, followed by the other variables

#Since our data is representative of individual cases, we will need to aggregate the data to plot cases by Month

ss_month_occurrences<-table(unlist(ss0$Month.M...4))
ss_month_occurrences

#Now let's convert our table to a dataframe so we can plot the occurrences of this
ss_month_occurrences1<- aggregate(ss0$Cases.M1, by=list(Month.M...4=ss0$Month.M...4), FUN=sum)

#Let's check out our data
head(ss_month_occurrences1, 12)

#Let's convert the x variable to a factor so we can play with the levels
ss_month_occurrences1$x<-as.factor(ss_month_occurrences1$x)

levels(ss_month_occurrences1$x) #it's in alphabetical order, but we want descending order

ss_month_occurrences1<-ss_month_occurrences1 %>% arrange(desc(x)) #this puts our months into descending order
View(ss_month_occurrences1)

#let's plot the total cases from the greatest to the least by running a factor() function. 
#This works only if your data is already in order from greatest to least of the total.

ss_month_occurrences1$Month.M...4 <- factor(ss_month_occurrences1$Month.M...4, ss_month_occurrences1$Month.M...4)
View(ss_month_occurrences1)

#Let's go ahead and rename the columns so that they don't get confusing
#ss_month_occurrences1<- ss_month_occurrences1 %>% 
#  rename(
#    Month = Month.M...4,
#    Cases = x
#  )

# assigning new names to the columns of the data frame
colnames(ss_month_occurrences1) <- c('Month','Cases')

#Let's sort by month

ss_month_occurrences1$Month = factor(ss_month_occurrences1$Month, levels = month.name)

str(ss_month_occurrences1)

##################################Let's plot this data

figb<-ggplot(ss_month_occurrences1, aes(x = Month, y = Cases, fill = Month)) +
  geom_bar(stat = "identity") +
  xlab("Florida Counties") +
  ylab("Total V.vulnificus cases") +
  ggtitle("V. vulnificus Occurrences by Months in Florida") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5))  #Success!
figb

#Now let's look at our data according to season
#Since our data is representative of individual cases, we will need to aggregate the data to plot cases by Season

ss_season_occurrences<-table(unlist(ss0$Season.M...5))
ss_season_occurrences

#Now let's convert our table to a dataframe so we can plot the occurrences of this
ss_season_occurrences1<- aggregate(ss0$Cases.M1, by=list(Season.M...5=ss0$Season.M...5), FUN=sum)

#Let's check out our data
head(ss_season_occurrences, 4)
View(ss_season_occurrences)

#Let's convert the x variable to a factor so we can play with the levels
ss_season_occurrences1$x<-as.factor(ss_season_occurrences1$x)

levels(ss_season_occurrences1$Season.M...5) #it's in alphabetical order, but we want descending order

ss_season_occurrences1<-ss_season_occurrences1 %>% arrange(desc(x)) #this puts our months into descending order
View(ss_season_occurrences1)

#let's plot the total cases from the greatest to the least by running a factor() function. 
#This works only if your data is already in order from greatest to least of the total.

ss_season_occurrences1$Season.M...5 <- factor(ss_season_occurrences1$Season.M...5, ss_season_occurrences1$Season.M...5)
View(ss_season_occurrences1)

# assigning new names to the columns of the data frame
colnames(ss_season_occurrences1) <- c('Season','Cases')

#Let's sort by Season

ss_season_occurrences1$Season <- factor(ss_season_occurrences1$Season, levels = c("Winter", "Spring", "Summer", "Fall"))

str(ss_season_occurrences1)

#Replacing the values in ss_season_occurrences1 with actual data

ss_season_data_new <- ss_season_occurrences1 %>%                              
  mutate(Cases = replace(Cases, Cases == 4, 193))
ss_season_data_new

ss_season_data_new<-ss_season_data_new %>% 
  mutate(Cases = replace(Cases, Cases == 3, 173))
ss_season_data_new

ss_season_data_new<-ss_season_data_new %>% 
  mutate(Cases = replace(Cases, Cases == 2, 61))
ss_season_data_new

ss_season_data_new<-ss_season_data_new %>% 
  mutate(Cases = replace(Cases, Cases == 1, 21))
ss_season_data_new


#######Let's do an analysis of our seasonal data#####################################################################
View(ss_season_data_new)

shapiro.test(ss_season_data_new$Cases)

kruskal.test(Cases ~ Season, data = ss_season_data_new)

#Now lets do a chi-square test

Season.Chi<-c(21, 61, 173, 193)

chisq.test(Season.Chi, p = c(.25, .25, .25, .25))

####doing follow-up chi-square tests to determine where the significance is
Season.Chi_1<-c(21, 61)
Season.Chi_2<-c(21, 173)
Season.Chi_3<-c(21, 193)
Season.Chi_4<-c(61, 173)
Season.Chi_5<-c(61, 193)
Season.Chi_6<-c(173, 193)

chisq.test(Season.Chi_1)
chisq.test(Season.Chi_2)
chisq.test(Season.Chi_3)
chisq.test(Season.Chi_4)
chisq.test(Season.Chi_5)
chisq.test(Season.Chi_6)


#Let's plot this data

figc<-ggplot(ss_season_data_new, aes(x = Season, y = Cases, fill = Season)) +
  geom_bar(stat = "identity") +
  xlab("Season") +
  ylab("Total V.vulnificus cases") +
  ggtitle("V. vulnificus Occurrences by Season in Florida") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5))  #Success!
figc

#Now let's look at our data according to year
#Since our data is representative of individual cases, we will need to aggregate the data to plot cases by year

ss_yearly_occurrences<-table(unlist(ss0$Year.M))
ss_yearly_occurrences

#Now let's convert our table to a dataframe so we can plot the occurrences of this
ss_yearly_occurrences1<- aggregate(ss0$Cases.M1, by=list(Year.M=ss0$Year.M), FUN=sum)

#Let's check out our data
head(ss_yearly_occurrences, 13)

#Let's convert the x variable to a factor so we can play with the levels
ss_yearly_occurrences1$x<-as.factor(ss_yearly_occurrences1$x)

levels(ss_yearly_occurrences1$x) #it's in alphabetical order, but we want descending order

#let's plot the total cases from the greatest to the least by running a factor() function. 
#This works only if your data is already in order from greatest to least of the total.

#ss_season_occurrences1$Season.M...5 <- factor(ss_season_occurrences1$Season.M...5, ss_season_occurrences1$Season.M...5)
View(ss_yearly_occurrences1)

# assigning new names to the columns of the data frame
colnames(ss_yearly_occurrences1) <- c('Year','Cases')

str(ss_yearly_occurrences1)

#Let's plot this data

figd<-ggplot(ss_yearly_occurrences1, aes(x = Year, y = Cases, fill = Year)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Total V.vulnificus cases") +
  ggtitle("V. vulnificus Occurrences by Year in Florida") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5))  #Success!
figd

#############################edits from PLOS NTD

#Create a dataframe with percents, with 2008 = 100% of cases

df1<-data.frame(Year = c(2008:2020), Cases = c(15, 24, 32, 35, 26, 41, 30, 44, 48, 50, 41, 27, 35), Percents = c(100, 1.60, 213, 233, 173, 273, 200, 293, 320, 333, 273, 180, 233))
  

figee <- ggplot(df1)  + 
  geom_bar(aes(x=Year, y=Cases),stat="identity", fill="purple3",colour="#006000")+
  geom_line(aes(x=Year, y=0.15*Percents),stat="identity",color="yellow3",size=2)+
  labs(x="Year",y="Number of Cases")+
    scale_y_continuous(sec.axis=sec_axis(~.*6.7,name="Case Percentage Change (2008 = 100% of Reported Cases)"))
figee + theme(axis.title.y.right = element_text(vjust=3))

#+ theme(axis.title.y.right = element_text(vjust=2 or 3 or 4))

########################################arranging the data into a single plot

ggarrange(figa, figb, figc, figd, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)



#Starting with descriptive statistics of deaths

ss0deaths<-subset(ss0, Deaths.M1 == 1)
View(ss0deaths)

#Now let's look at our data according to Month, followed by the other variables

#Since our data is representative of individual cases, we will need to aggregate the data to plot cases by Month

ss_month_deaths<-table(unlist(ss0deaths$Month.M...4))
ss_month_deaths

#Let's compare with the occurrence of cases to calculate CFR
ss_month_occurrences<-table(unlist(ss0$Month.M...4))
ss_month_occurrences

#From this data, we are able to compute our case fatality rates by month

#Now let's look at our data according to Season, followed by the other variables

#Since our data is representative of individual cases, we will need to aggregate the data to plot cases by season

ss_season_deaths<-table(unlist(ss0deaths$Season.M...5))
ss_season_deaths

#Let's compare with the occurrence of cases to calculate CFR
ss_season_occurrences<-table(unlist(ss0$Season.M...5))
ss_season_occurrences

#From this data, we are able to compute our case fatality rates by year

#Since our data is representative of individual cases, we will need to aggregate the data to plot cases by year

ss_yearly_deaths<-table(unlist(ss0deaths$Year.M))
ss_yearly_deaths

#Let's compare with the occurrence of cases to calculate CFR
ss_yearly_occurrences

#From this data, we are able to compute our case fatality rates by season

#Let's identify which NOAA stations are associated with which counties
View(ss0)

NOAA_ST <- subset(ss0, select = c("County","NOAA_Stat"))
View(NOAA_ST)

NOAA_ST_county<-table(unlist(ss0$NOAA_Stat))
NOAA_ST_county
###################################################################################################################
###################################################################################################################
########################################Ecological Variables Analysis#############################################

#Downloading the google sheets from the web - we have five sheets, so we are going to end up with 5 dataframes
ss1<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1yVqSyVN1jFTDbqR5wbbQKVXcnpM6h6nXvZ2ucte2bzk/edit#gid=0')
ss2<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1yVqSyVN1jFTDbqR5wbbQKVXcnpM6h6nXvZ2ucte2bzk/edit#gid=1505837731')
ss3<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1yVqSyVN1jFTDbqR5wbbQKVXcnpM6h6nXvZ2ucte2bzk/edit#gid=1076198116')
ss4<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1yVqSyVN1jFTDbqR5wbbQKVXcnpM6h6nXvZ2ucte2bzk/edit#gid=1476997744')
ss5<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1yVqSyVN1jFTDbqR5wbbQKVXcnpM6h6nXvZ2ucte2bzk/edit#gid=2052167108')

#Merging the dataframes vertically

ss6<-rbind(ss1, ss2)
ss7<-rbind(ss3, ss4)
ss8<-rbind(ss5, ss6)
ss9<-rbind(ss7, ss8)

ss_final<-ss9 #renaming the dataframe
View(ss_final)

dim(ss_final) #viewing the dimensions

categories<-length(unique(ss_final$County.M)) #Get the number of individual counties represented
print(categories)

categories1 <- unique(ss_final$County.M) #Identifying each county represented
print(categories1)

#Let's drop the years that have no cases in our counties, using the variable Case.Y
ss_final <- subset(ss_final, Case.Y == "Yes") 

#The number of rows in ss_final went from >5,000 to 2711, excellent!
View(ss_final)

#######################Let's get some descriptive statistics for WSPD#############################################
###################################################################################################################

# using subset function, we drop all observations <9999 and select County.M through WSPD -
#this gives us all of our relevant WSPD data
ss_final_WSPD_all <- subset(ss_final, WSPD < 9999,
                  select=County.M:WSPD)

str(ss_final_WSPD_all) #Getting the structure of the data
View(ss_final_WSPD_all)

#Let's create a dataframe where we have the WSPD observations for months where there are no cases

ss_final_WSPD_no.cases<-subset(ss_final_WSPD_all, Case.M =="No")
View(ss_final_WSPD_no.cases)


#Now lets's create another dataframe where we have only the WSPD observations where there are cases

ss_final_WSPD_cases<-subset(ss_final_WSPD_all, Case.M =="Yes")
View(ss_final_WSPD_cases)


#Now lets's create another dataframe where we have only the WSPD observations where there are deaths

ss_final_WSPD_deaths<-subset(ss_final_WSPD_all, Death.M =="Yes")
View(ss_final_WSPD_deaths)


##Let's check out the descriptive statistics for WSPD for when there case months, cases, and deaths

#Doing the range of the data

range(ss_final_WSPD_no.cases$WSPD)

range(ss_final_WSPD_cases$WSPD)

range(ss_final_WSPD_deaths$WSPD)

#Now doing the standard deviation

sd(ss_final_WSPD_no.cases$WSPD)

sd(ss_final_WSPD_cases$WSPD)

sd(ss_final_WSPD_deaths$WSPD)

#Doing summary, which gives us the mean, median, and quartiles of the data

summary(ss_final_WSPD_no.cases$WSPD)

summary(ss_final_WSPD_cases$WSPD)

summary(ss_final_WSPD_deaths$WSPD)

#Using the pastecs package, we are going to get all the stats 

stat.desc(ss_final_WSPD_no.cases$WSPD)

stat.desc(ss_final_WSPD_cases$WSPD)

stat.desc(ss_final_WSPD_deaths$WSPD)

#Shapiro-Wilks test of Normality

shapiro.test(ss_final_WSPD_no.cases$WSPD)

shapiro.test(ss_final_WSPD_cases$WSPD)

shapiro.test(ss_final_WSPD_deaths$WSPD)

#We are going to have to plot our data and do non parametric testing, so we will need to merge our dataframes, and 
#reshape them

#Let's go back to our dataframe, ss_final_WSPD_all
View(ss_final_WSPD_all)

#Let's add a new column based on the values of Case.Y, Case.M, and Death.M
# R adding a column to dataframe based on values in other columns:
# Multiple conditions when adding new column to dataframe:

ss_final_col<-ss_final_WSPD_all %>% mutate(Group =
                     case_when(Case.M =="No" ~ "No_Case", 
                               Case.M == "Yes" & Death.M == "No" ~ "Case",
                               Case.M == "Yes" & Death.M == "Yes" ~ "Death"))
      
                                                 
View(ss_final_col)

str(ss_final_col)

#Let's convert some of the characters to factors so that we can do stats on them

ss_final_col$County.M<-as.factor(ss_final_col$County.M)
ss_final_col$NOAA_St<-as.factor(ss_final_col$NOAA_St )   
ss_final_col$Group<-as.factor(ss_final_col$Group)

str(ss_final_col)

#Let's double check the levels of each factor variable

levels(ss_final_col$County.M)
levels(ss_final_col$NOAA_St)
levels(ss_final_col$Group)

#Success!

#Let's order the 
ss_final_col$Group <- ordered(ss_final_col$Group,
                         levels = c("Death","Case","No_Case"))
View(ss_final_col)

#Let's do the summary statistics again

detach("package:plyr", unload = TRUE)
require(dplyr)

group_by(ss_final_col, Group) %>%
  summarise(
    count = n(),
    mean = mean(WSPD, na.rm = TRUE),
    sd = sd(WSPD, na.rm = TRUE),
    median = median(WSPD, na.rm = TRUE),
    IQR = IQR(WSPD, na.rm = TRUE)
  )

#Visualize WSPD data with ggpubr:
  # Box plots
  # ++++++++++++++++++++
  # Plot WSPD by group and color by group

  library("ggpubr")

figaa<-ggboxplot(ss_final_col, x = "Group", y = "WSPD", 
                 color = "Group", palette = c("black", "gray23", "grey39"),
                 order = c("Death","Case", "No_Case"),
                 ylab = "Windspeed in meters/second", xlab = "")

figaaa<-figaa + scale_x_discrete(breaks=c("Death","Case","No_Case"),
                                 labels=c("Deaths", "Cases", "No Cases"))


figaaaa<-figaaa + theme(legend.position="none")

figaaaa


#Now, we are going to run a one-way non-parametric version of the ANOVA, the Kruskal-Wallis test

kruskal.test(WSPD ~ Group, data = ss_final_col)

#Kruskal-Wallis rank sum test

#data:  WSPD by Group
#Kruskal-Wallis chi-squared = 5.8906, df = 2, p-value = 0.05259

#Not significant


#Since our data was not significant, let's put cases and deaths together.  Again, we need a new column

#Let's add a new column based on the values of Case.M
# R adding a column to dataframe based on values in other columns:
# Multiple conditions when adding new column to dataframe:

ss_final_col<-ss_final_WSPD_all %>% mutate(Group2 =
                                             case_when(Case.M =="No" ~ "No_Case", 
                                                       Case.M == "Yes" ~ "Case"))
                                                       
ss_final_col


ggboxplot(ss_final_col, x = "Group2", y = "WSPD", 
          color = "Group2", palette = c("#00AFBB", "#FC4E07"),
          order = c("Case","No_Case"),
          ylab = "Windspeed", xlab = "Vulnificus Incidence")

#Now, we are going to run a one-way non-parametric version of the ANOVA, the Kruskal-Wallis test

kruskal.test(WSPD ~ Group2, data = ss_final_col)

#Now we are going to run a Mann-Whitney U test

wilcox.test(WSPD ~ Group2, data=ss_final_col)

#Now we are going to run a logistic regression

ss_final_col<-ss_final_WSPD_all %>% mutate(Group3 =
                                             case_when(Case.M =="No" ~ 0, 
                                                       Case.M == "Yes" ~ 1))
ss_final_col

str(ss)

ss_final_col$Group3<-as.integer(ss_final_col$Group3) #Turning our column into an integer so R doesn't freak out

x<-glm(Group3 ~ WSPD, family = binomial, data = ss_final_col)
x
#Call:  glm(formula = Group3 ~ WSPD, family = binomial, data = ss_final_col)

#Coefficients:
#  (Intercept)         WSPD  
#-1.4341      -0.1094  

#Degrees of Freedom: 2408 Total (i.e. Null);  2407 Residual
#Null Deviance:	    1972 
#Residual Deviance: 1969 	AIC: 1973

#Getting the odds ratio
exp(coef(x))

#(Intercept)        WSPD 
#0.2383289   0.8963925 

##############################Let's do atmospheric temperature next, ATMP############################################################
###############################################################################################################################
#Going back to ss_final


#Case    0.72    -      
#No_Case 4.3e-12 < 2e-16View(ss_final)

# using subset function, we drop all observations <9999 and select County.M through ATMP -
#this gives us all of our relevant WSPD data
ss_final_ATMP_all <- subset(ss_final, ATMP < 9999,
                            select=County.M:ATMP)

str(ss_final_ATMP_all) #Getting the structure of the data
View(ss_final_ATMP_all)

#Let's drop WSPD

ss_final_ATMP_all<-subset(ss_final_ATMP_all, select=-c(WSPD))
ss_final_ATMP_all

#Let's create a dataframe where we have the ATMP observations for months where there are no cases

ss_final_ATMP_no.cases<-subset(ss_final_ATMP_all, Case.M =="No")
View(ss_final_ATMP_no.cases)


#Now lets's create another dataframe where we have only the ATMP observations where there are cases

ss_final_ATMP_cases<-subset(ss_final_ATMP_all, Case.M =="Yes")
View(ss_final_ATMP_cases)


#Now lets's create another dataframe where we have only the WSPD observations where there are deaths

ss_final_ATMP_deaths<-subset(ss_final_ATMP_all, Death.M =="Yes")
View(ss_final_ATMP_deaths)

#Using the pastecs package, we are going to get all the stats 

stat.desc(ss_final_ATMP_no.cases$ATMP)

stat.desc(ss_final_ATMP_cases$ATMP)

stat.desc(ss_final_ATMP_deaths$ATMP)

#Shapiro-Wilks test of Normality

shapiro.test(ss_final_ATMP_no.cases$ATMP)

shapiro.test(ss_final_ATMP_cases$ATMP)

shapiro.test(ss_final_ATMP_deaths$ATMP)

#Non-normal data = non parametric testing

#We are going to have to plot our data and do non parametric testing, so we will need to merge our dataframes, and 
#reshape them

#Let's go back to our dataframe, ss_final_ATMP_all
View(ss_final_ATMP_all)

#Let's add a new column based on the values of Case.Y, Case.M, and Death.M
# R adding a column to dataframe based on values in other columns:
# Multiple conditions when adding new column to dataframe:

ss_final_col_ATMP<-ss_final_ATMP_all %>% mutate(Group =
                                             case_when(Case.M =="No" ~ "No_Case", 
                                                       Case.M == "Yes" & Death.M == "No" ~ "Case",
                                                       Case.M == "Yes" & Death.M == "Yes" ~ "Death"))


View(ss_final_col_ATMP)

str(ss_final_col_ATMP)

#Let's convert some of the characters to factors so that we can do stats on them

ss_final_col_ATMP$County.M<-as.factor(ss_final_col_ATMP$County.M)
ss_final_col_ATMP$NOAA_St<-as.factor(ss_final_col_ATMP$NOAA_St )   
ss_final_col_ATMP$Group<-as.factor(ss_final_col_ATMP$Group)

str(ss_final_col_ATMP)

#Let's double check the levels of each factor variable

levels(ss_final_col_ATMP$County.M)
levels(ss_final_col_ATMP$NOAA_St)
levels(ss_final_col_ATMP$Group)

#Success!

#Let's order the 
ss_final_col_ATMP$Group <- ordered(ss_final_col_ATMP$Group,
                              levels = c("Death","Case","No_Case"))
View(ss_final_col_ATMP)

#Let's do the summary statistics again

detach("package:plyr", unload = TRUE)
require(dplyr)

group_by(ss_final_col_ATMP, Group) %>%
  summarise(
    count = n(),
    mean = mean(ATMP, na.rm = TRUE),
    sd = sd(ATMP, na.rm = TRUE),
    median = median(ATMP, na.rm = TRUE),
    IQR = IQR(ATMP, na.rm = TRUE)
  )

#Visualize WSPD data with ggpubr:
# Box plots
# ++++++++++++++++++++
# Plot WSPD by group and color by group

library("ggpubr")

figbb<-ggboxplot(ss_final_col_ATMP, x = "Group", y = "ATMP", 
          color = "Group", palette = c("black", "gray23", "grey39"),
          order = c("Death","Case","No_Case"),
          ylab = "Atmospheric Temperature in Celsius", xlab = expression(paste(italic("V.vulnificus"))))

figbbb<-figbb + scale_x_discrete(breaks=c("Death","Case","No_Case"),
                                 labels=c("Deaths", "Cases", "No Cases"))

figbbbb<-figbbb + theme(legend.position="none")

figbbbb




#Now, we are going to run a one-way non-parametric version of the ANOVA, the Kruskal-Wallis test

kruskal.test(ATMP ~ Group, data = ss_final_col_ATMP)

#Kruskal-Wallis rank sum test

#data:  ATMP by Group
#Kruskal-Wallis chi-squared = 181.32, df = 2, p-value < 2.2e-16

#Significant

pairwise.wilcox.test(ss_final_col_ATMP$ATMP, ss_final_col_ATMP$Group,
                     p.adjust.method = "BH")

#data:  ss_final_col_ATMP$ATMP and ss_final_col_ATMP$Group 

#########Death   Case   


#Since our data was not significant between deaths and cases, let's put cases and deaths together.  
#Again, we need a new column

#Let's add a new column based on the values of Case.M
# R adding a column to dataframe based on values in other columns:
# Multiple conditions when adding new column to dataframe:

View(ss_final_ATMP_all)

ss_final_col_ATMP<-ss_final_ATMP_all %>% mutate(Group2 =
                                             case_when(Case.M =="No" ~ "No_Case", 
                                                       Case.M == "Yes" ~ "Case"))

ss_final_col_ATMP

ggboxplot(ss_final_col_ATMP, x = "Group2", y = "ATMP", 
          color = "Group2", palette = c("#00AFBB", "#FC4E07"),
          order = c("Case","No_Case"),
          ylab = "Atmospheric Temperature", xlab = "Vulnificus Incidence")

#Now, we are going to run a one-way non-parametric version of the ANOVA, the Kruskal-Wallis test

kruskal.test(ATMP ~ Group2, data = ss_final_col_ATMP)

#Kruskal-Wallis rank sum test

#data:  ATMP by Group2
#Kruskal-Wallis chi-squared = 181.3, df = 1, p-value < 2.2e-16

#Now we are going to run a Mann-U Whitney test to see if there are differences

wilcox.test(ATMP ~ Group2, data=ss_final_col_ATMP) 

#Wilcoxon rank sum test with continuity correction

#data:  ATMP by Group2
#W = 508627, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0


#Now we are going to run a logistic regression

ss_final_col_ATMP<-ss_final_ATMP_all %>% mutate(Group3 =
                                             case_when(Case.M =="No" ~ 0, 
                                                       Case.M == "Yes" ~ 1))
ss_final_col_ATMP

str(ss_final_col_ATMP)

ss_final_col_ATMP$Group3<-as.integer(ss_final_col_ATMP$Group3) #Turning our column into an integer so R doesn't freak out

x1<-glm(Group3 ~ ATMP, family = binomial, data = ss_final_col_ATMP)
x1
#Call:  glm(formula = Group3 ~ ATMP, family = binomial, data = ss_final_col_ATMP)

#Coefficients:
#  (Intercept)         ATMP  
#-6.9386       0.2117  

#Degrees of Freedom: 2395 Total (i.e. Null);  2394 Residual
#Null Deviance:	    1957 
#Residual Deviance: 1755 	AIC: 1759

#Getting the odds ratio
exp(coef(x1))

#(Intercept)         ATMP 
#0.0009696247 1.2357890524 

###################################################Let's examine WTMP####################################################
########################################################################################################################
#Going back to ss_final

View(ss_final)

# using subset function, we drop all observations <9999 and select County.M through WTMP -
#this gives us all of our relevant WTMP data
ss_final_WTMP_all <- subset(ss_final, WTMP < 9999,
                            select=County.M:WTMP)

str(ss_final_WTMP_all) #Getting the structure of the data
View(ss_final_WTMP_all)

#Let's drop WSPD & ATMP

ss_final_WTMP_all<-subset(ss_final_WTMP_all, select=-c(WSPD, ATMP))
ss_final_WTMP_all

#Let's create a dataframe where we have the WTMP observations for months where there are no cases

ss_final_WTMP_no.cases<-subset(ss_final_WTMP_all, Case.M =="No")
View(ss_final_WTMP_no.cases)


#Now lets's create another dataframe where we have only the ATMP observations where there are cases

ss_final_WTMP_cases<-subset(ss_final_WTMP_all, Case.M =="Yes")
View(ss_final_WTMP_cases)


#Now lets's create another dataframe where we have only the WSPD observations where there are deaths

ss_final_WTMP_deaths<-subset(ss_final_WTMP_all, Death.M =="Yes")
View(ss_final_WTMP_deaths)


#Using the pastecs package, we are going to get all the stats 

stat.desc(ss_final_WTMP_no.cases$WTMP)

stat.desc(ss_final_WTMP_cases$WTMP)

stat.desc(ss_final_WTMP_deaths$WTMP)

#Shapiro-Wilks test of Normality

shapiro.test(ss_final_WTMP_no.cases$WTMP)

shapiro.test(ss_final_WTMP_cases$WTMP)

shapiro.test(ss_final_WTMP_deaths$WTMP)


#All of our variables failed Shapiro Wilks

#We are going to have to plot our data and do non parametric testing, so we will need to merge our dataframes, and 
#reshape them

#Let's go back to our dataframe, ss_final_ATMP_all
View(ss_final_WTMP_all)

#Let's add a new column based on the values of Case.Y, Case.M, and Death.M
# R adding a column to dataframe based on values in other columns:
# Multiple conditions when adding new column to dataframe:

ss_final_col_WTMP<-ss_final_WTMP_all %>% mutate(Group =
                                                  case_when(Case.M =="No" ~ "No_Case", 
                                                            Case.M == "Yes" & Death.M == "No" ~ "Case",
                                                            Case.M == "Yes" & Death.M == "Yes" ~ "Death"))


View(ss_final_col_WTMP)

str(ss_final_col_WTMP)

#Let's convert some of the characters to factors so that we can do stats on them

ss_final_col_WTMP$County.M<-as.factor(ss_final_col_WTMP$County.M)
ss_final_col_WTMP$NOAA_St<-as.factor(ss_final_col_WTMP$NOAA_St )   
ss_final_col_WTMP$Group<-as.factor(ss_final_col_WTMP$Group)

str(ss_final_col_WTMP)

#Let's double check the levels of each factor variable

levels(ss_final_col_WTMP$County.M)
levels(ss_final_col_WTMP$NOAA_St)
levels(ss_final_col_WTMP$Group)

#Success!

#Let's order the 
ss_final_col_WTMP$Group <- ordered(ss_final_col_WTMP$Group,
                                   levels = c("Death","Case","No_Case"))
View(ss_final_col_WTMP)

#Let's do the summary statistics again

detach("package:plyr", unload = TRUE)
require(dplyr)

group_by(ss_final_col_WTMP, Group) %>%
  summarise(
    count = n(),
    mean = mean(WTMP, na.rm = TRUE),
    sd = sd(WTMP, na.rm = TRUE),
    median = median(WTMP, na.rm = TRUE),
    IQR = IQR(WTMP, na.rm = TRUE)
  )

#Visualize WSPD data with ggpubr:
# Box plots
# ++++++++++++++++++++
# Plot WSPD by group and color by group

library("ggpubr")

figcc<-ggboxplot(ss_final_col_WTMP, x = "Group", y = "WTMP", 
          color = "Group", palette = c("black", "gray23", "grey39"),
          order = c("Death","Case","No_Case"),
          ylab = "Water Temperature in Celsius", xlab = expression(paste(italic("V.vulnificus"))))

figccc<-figcc + scale_x_discrete(breaks=c("Death","Case","No_Case"),
                                 labels=c("Deaths", "Cases", "No Cases"))

figcccc<-figccc + theme(legend.position="none")

figcccc


#Now, we are going to run a one-way non-parametric version of the ANOVA, the Kruskal-Wallis test

kruskal.test(WTMP ~ Group, data = ss_final_col_WTMP)

#	Kruskal-Wallis rank sum test

#data:  WTMP by Group
#Kruskal-Wallis chi-squared = 176.14, df = 2, p-value < 2.2e-16

#Significant

pairwise.wilcox.test(ss_final_col_WTMP$WTMP, ss_final_col_WTMP$Group,
                     p.adjust.method = "BH")

#Pairwise comparisons using Wilcoxon rank sum test with continuity correction 

#data:  ss_final_col_WTMP$WTMP and ss_final_col_WTMP$Group 

#Death   Case   
#Case    0.25    -      
#No_Case 1.1e-13 < 2e-16

#Since our data was not significant between deaths and cases, let's put cases and deaths together.  
#Again, we need a new column

#Let's add a new column based on the values of Case.M
# R adding a column to dataframe based on values in other columns:
# Multiple conditions when adding new column to dataframe:

View(ss_final_WTMP_all)

ss_final_col_WTMP<-ss_final_WTMP_all %>% mutate(Group2 =
                                                  case_when(Case.M =="No" ~ "No_Case", 
                                                            Case.M == "Yes" ~ "Case"))

ss_final_col_WTMP

ggboxplot(ss_final_col_WTMP, x = "Group2", y = "WTMP", 
          color = "Group2", palette = c("#00AFBB", "#FC4E07"),
          order = c("Case","No_Case"),
          ylab = "Water Temperature", xlab = "Vulnificus Incidence")

#Now, we are going to run a one-way non-parametric version of the ANOVA, the Kruskal-Wallis test

kruskal.test(WTMP ~ Group2, data = ss_final_col_WTMP)

#Kruskal-Wallis rank sum test

#data:  WTMP by Group2
#Kruskal-Wallis chi-squared = 175.46, df = 1, p-value < 2.2e-16

#Now we are going to run a Mann-U Whitney test to see if there are differences

wilcox.test(WTMP ~ Group2, data=ss_final_col_WTMP) 

#Wilcoxon rank sum test with continuity correction

#data:  WTMP by Group2
#W = 451022, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0


#Now we are going to run a logistic regression

ss_final_col_WTMP<-ss_final_WTMP_all %>% mutate(Group3 =
                                                  case_when(Case.M =="No" ~ 0, 
                                                            Case.M == "Yes" ~ 1))
ss_final_col_WTMP

str(ss_final_col_WTMP)

ss_final_col_WTMP$Group3<-as.integer(ss_final_col_WTMP$Group3) #Turning our column into an integer so R doesn't freak out

x3<-glm(Group3 ~ WTMP, family = binomial, data = ss_final_col_WTMP)
x3
#Call:  glm(formula = Group3 ~ WTMP, family = binomial, data = ss_final_col_WTMP)

#Coefficients:
#  (Intercept)         WTMP  
#-7.3386       0.2127  

#Degrees of Freedom: 2222 Total (i.e. Null);  2221 Residual
#Null Deviance:	    1853 
#Residual Deviance: 1653 	AIC: 1657

#Getting the odds ratio
exp(coef(x3))

#(Intercept)         WTMP 
#0.0006499416 1.2370338511

####################################################Now Analyzing PRES###########################################################
############################################################################################################################

#Going back to ss_final

View(ss_final)

# using subset function, we drop all observations <9999 and select County.M through PRES -
#this gives us all of our relevant PRES data
ss_final_PRES_all <- subset(ss_final, PRES < 9999,
                            select=County.M:PRES)

str(ss_final_PRES_all) #Getting the structure of the data
View(ss_final_PRES_all)

#Let's drop WSPD & ATMP & WTMP

ss_final_PRES_all<-subset(ss_final_PRES_all, select=-c(WSPD, ATMP, WTMP))
ss_final_PRES_all

#Let's create a dataframe where we have the WTMP observations for months where there are no cases

ss_final_PRES_no.cases<-subset(ss_final_PRES_all, Case.M =="No")
View(ss_final_PRES_no.cases)


#Now lets's create another dataframe where we have only the ATMP observations where there are cases

ss_final_PRES_cases<-subset(ss_final_PRES_all, Case.M =="Yes")
View(ss_final_PRES_cases)


#Now lets's create another dataframe where we have only the WSPD observations where there are deaths

ss_final_PRES_deaths<-subset(ss_final_PRES_all, Death.M =="Yes")
View(ss_final_PRES_deaths)


#Using the pastecs package, we are going to get all the stats 

stat.desc(ss_final_PRES_no.cases$PRES)

stat.desc(ss_final_PRES_cases$PRES)

stat.desc(ss_final_PRES_deaths$PRES)

#Shapiro-Wilks test of Normality

shapiro.test(ss_final_PRES_no.cases$PRES)

shapiro.test(ss_final_PRES_cases$PRES)

shapiro.test(ss_final_PRES_deaths$PRES)

#Some of our variables failed Shapiro Wilks

#We are going to have to plot our data and do non parametric testing, so we will need to merge our dataframes, and 
#reshape them

#Let's go back to our dataframe, ss_final_PRES_all
View(ss_final_PRES_all)

#Let's add a new column based on the values of Case.Y, Case.M, and Death.M
# R adding a column to dataframe based on values in other columns:
# Multiple conditions when adding new column to dataframe:

ss_final_col_PRES<-ss_final_PRES_all %>% mutate(Group =
                                                  case_when(Case.M =="No" ~ "No_Case", 
                                                            Case.M == "Yes" & Death.M == "No" ~ "Case",
                                                            Case.M == "Yes" & Death.M == "Yes" ~ "Death"))


View(ss_final_col_PRES)

str(ss_final_col_PRES)

#Let's convert some of the characters to factors so that we can do stats on them

ss_final_col_PRES$County.M<-as.factor(ss_final_col_PRES$County.M)
ss_final_col_PRES$NOAA_St<-as.factor(ss_final_col_PRES$NOAA_St )   
ss_final_col_PRES$Group<-as.factor(ss_final_col_PRES$Group)

str(ss_final_col_PRES)

#Let's double check the levels of each factor variable

levels(ss_final_col_PRES$County.M)
levels(ss_final_col_PRES$NOAA_St)
levels(ss_final_col_PRES$Group)

#Success!

#Let's order the 
ss_final_col_PRES$Group <- ordered(ss_final_col_PRES$Group,
                                   levels = c("Death","Case","No_Case"))
View(ss_final_col_PRES)

#Let's do the summary statistics again

detach("package:plyr", unload = TRUE)
require(dplyr)

group_by(ss_final_col_PRES, Group) %>%
  summarise(
    count = n(),
    mean = mean(PRES, na.rm = TRUE),
    sd = sd(PRES, na.rm = TRUE),
    median = median(PRES, na.rm = TRUE),
    IQR = IQR(PRES, na.rm = TRUE)
  )

#Visualize WSPD data with ggpubr:
# Box plots
# ++++++++++++++++++++
# Plot WSPD by group and color by group

library("ggpubr")


ss_final_col_PRES$Group<-as.factor(ss_final_col_PRES$Group)

figdd<-ggboxplot(ss_final_col_PRES, x = "Group", y = "PRES", 
          color = "Group", palette = c("black", "gray23", "grey39"),
          order = c("Death","Case", "No_Case"),
          ylab = "Atmospheric Pressure in hectopascals", xlab = "")

                   

figddd<-figdd + scale_x_discrete(breaks=c("Death","Case","No_Case"),
                                   labels=c("Deaths", "Cases", "No Cases"))

figdddd<-figddd + theme(legend.position="none")

figdddd

###########################################Let's put all these into a single panel###########################

ggarrange(figaaaa, figdddd, figcccc, figbbbb, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)





#Now, we are going to run a one-way non-parametric version of the ANOVA, the Kruskal-Wallis test

kruskal.test(PRES ~ Group, data = ss_final_col_PRES)

#	Kruskal-Wallis rank sum test

#data:  PRES by Group
#Kruskal-Wallis chi-squared = 88.619, df = 2, p-value < 2.2e-16

#Significant

pairwise.wilcox.test(ss_final_col_PRES$PRES, ss_final_col_PRES$Group,
                     p.adjust.method = "BH")

#Pairwise comparisons using Wilcoxon rank sum test with continuity correction 

#data:  ss_final_col_PRES$PRES and ss_final_col_PRES$Group 

#Death   Case   
#Case    0.8     -      
#  No_Case 2.4e-06 < 2e-16

#P value adjustment method: BH 

#Since our data was not significant between deaths and cases, let's put cases and deaths together.  
#Again, we need a new column

#Let's add a new column based on the values of Case.M
# R adding a column to dataframe based on values in other columns:
# Multiple conditions when adding new column to dataframe:

View(ss_final_PRES_all)

ss_final_col_PRES<-ss_final_PRES_all %>% mutate(Group2 =
                                                  case_when(Case.M =="No" ~ "No_Case", 
                                                            Case.M == "Yes" ~ "Case"))

ss_final_col_PRES

ggboxplot(ss_final_col_PRES, x = "Group2", y = "PRES", 
          color = "Group2", palette = c("#00AFBB", "#FC4E07"),
          order = c("Case","No_Case"),
          ylab = "Atmospheric Pressure", xlab = "Vulnificus Incidence")

#Now, we are going to run a one-way non-parametric version of the ANOVA, the Kruskal-Wallis test

kruskal.test(PRES ~ Group2, data = ss_final_col_PRES)

#	Kruskal-Wallis rank sum test

#data:  PRES by Group2
#Kruskal-Wallis chi-squared = 88.619, df = 1, p-value < 2.2e-16

#Now we are going to run a Mann-U Whitney test to see if there are differences

wilcox.test(PRES ~ Group2, data=ss_final_col_PRES) 

#Wilcoxon rank sum test with continuity correction

#data:  PRES by Group2
#W = 227799, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

#Now we are going to run a logistic regression

ss_final_col_PRES<-ss_final_PRES_all %>% mutate(Group3 =
                                                  case_when(Case.M =="No" ~ 0, 
                                                            Case.M == "Yes" ~ 1))
ss_final_col_PRES

str(ss_final_col_PRES)

ss_final_col_PRES$Group3<-as.integer(ss_final_col_PRES$Group3) #Turning our column into an integer so R doesn't freak out

x4<-glm(Group3 ~ PRES, family = binomial, data = ss_final_col_PRES)
x4
#Call:  glm(formula = Group3 ~ PRES, family = binomial, data = ss_final_col_PRES)

#Coefficients:
#  (Intercept)         PRES  
#285.1920      -0.2822  

#Degrees of Freedom: 2367 Total (i.e. Null);  2366 Residual
#Null Deviance:	    1912 
#Residual Deviance: 1807 	AIC: 1811

#Getting the odds ratio
exp(coef(x4))

#(Intercept)          PRES 
#7.199886e+123  7.541342e-01 

######################################################Let's do some logistic regression#########################################
##############################################################################################################################

View(ss_final)

# using subset function, we drop all observations <9999 and select County.M through Year -
#this gives us all of our relevant WSPD data
ss_final_all <- subset(ss_final, WSPD < 9999 & ATMP < 9999 & WTMP < 9999 & PRES < 9999)
View(ss_final_all)

str(ss_final_all)

ss_final_all<-ss_final_all %>% mutate(Group4 =
                                                  case_when(Case.M =="No" ~ "No_Case", 
                                                            Case.M == "Yes" ~ "Case"))

ss_final_all

ss_final_all<-ss_final_all %>% mutate(Group5 =
                                                  case_when(Case.M =="No" ~ 0, 
                                                            Case.M == "Yes" ~ 1))

ss_final_all

summary(ss_final_all)
str(ss_final_all)

#Convert characters to factors so we can do statistics
ss_final_all$County.M<-as.factor(ss_final_all$County.M)
ss_final_all$NOAA_St<-as.factor(ss_final_all$NOAA_St ) 
ss_final_all$Month.M<-as.factor(ss_final_all$Month.M)
ss_final_all$Season.M<-as.factor(ss_final_all$Season.M)
ss_final_all$Group4<-as.factor(ss_final_all$Group4)
ss_final_all$Group5<-as.factor(ss_final_all$Group5)

str(ss_final_all)

mylogit1 <- glm(Group5 ~ WSPD + ATMP + WTMP + PRES, data = ss_final_all, family = "binomial")
mylogit1
summary(mylogit1)

#to calculate McFadden's R2

nullmod1 <- glm(Group5 ~ 1, data = ss_final_all, family="binomial")
McF1<-1-logLik(mylogit1)/logLik(nullmod1)
print(McF1)


mylogit2 <- glm(Group5 ~ WSPD + ATMP + WTMP + PRES + County.M, data = ss_final_all, family = "binomial")
mylogit2
summary(mylogit2)

#to calculate McFadden's R2

nullmod2 <- glm(Group5 ~ 1, data = ss_final_all, family="binomial")
McF2<-1-logLik(mylogit2)/logLik(nullmod2)
print(McF2)


mylogit3 <- glm(Group5 ~ WSPD + ATMP + WTMP + PRES + NOAA_St, data = ss_final_all, family = "binomial")
mylogit3
summary(mylogit3)

#to calculate McFadden's R2

nullmod3 <- glm(Group5 ~ 1, data = ss_final_all, family="binomial")
McF3<-1-logLik(mylogit3)/logLik(nullmod3)
print(McF3)


mylogit4 <- glm(Group5 ~ WSPD + ATMP + WTMP + PRES + Month.M, data = ss_final_all, family = "binomial")
mylogit4
summary(mylogit4)

#to calculate McFadden's R2

nullmod4 <- glm(Group5 ~ 1, data = ss_final_all, family="binomial")
McF4<-1-logLik(mylogit4)/logLik(nullmod4)
print(McF4)

#####################################BEST MODEL##########################################################################

mylogit5 <- glm(Group5 ~ WSPD + ATMP + WTMP + PRES + Season.M, data = ss_final_all, family = "binomial")
mylogit5
summary(mylogit5)

nullmod5 <- glm(Group5 ~ 1, data = ss_final_all, family="binomial")
McF5<-1-logLik(mylogit5)/logLik(nullmod5)
print(McF5)


mylogit6 <- glm(Group5 ~ ATMP + WTMP + PRES + Season.M, data = ss_final_all, family = "binomial")
mylogit6
summary(mylogit6)

nullmod6 <- glm(Group5 ~ 1, data = ss_final_all, family="binomial")
McF6<-1-logLik(mylogit6)/logLik(nullmod6)
print(McF6)
#########################################################################################################################

########################################################################################################################
################################################Binary Correlations for Discussion####################################

##Starting with ss_final_all, from which all the 
View(ss_final_all)

# count number of occurrences in a column, cases versus no cases
table(ss_final_all$Group4)

#Let's take out the WTMP during the months of July, August, and September

WTMP_JULY <- subset(ss_final_all, Month.M == "July", select = c("WTMP"))
mean(WTMP_JULY$WTMP)

WTMP_AUGUST<- subset(ss_final_all, Month.M == "August", select = c("WTMP"))
mean(WTMP_AUGUST$WTMP)

WTMP_SEPTEMBER<- subset(ss_final_all, Month.M == "September", select = c("WTMP"))
mean(WTMP_SEPTEMBER$WTMP)

#####################################################################################################################################
###################################################################################################################################
######################################################################################################################################
####################################################Correlations for Ecological Variables###########################################


#WSPD
#ATMP
#WTMP
#PRES

#Spearman's Rank Correlation

#corr <- cor.test(x=cars$speed, y=cars$dist, method = 'spearman')

#Correlation for the variables, making sure the variables are paired. 

corr_WSPD_ATMP <- cor.test(x=ss_final_all$WSPD, y=ss_final_all$ATMP, method = 'spearman')
corr_WSPD_ATMP

corr_WSPD_WTMP <- cor.test(x=ss_final_all$WSPD, y=ss_final_all$WTMP, method = 'spearman')
corr_WSPD_WTMP

corr_WSPD_PRES <- cor.test(x=ss_final_all$WSPD, y=ss_final_all$PRES, method = 'spearman')
corr_WSPD_PRES

corr_ATMP_WTMP <- cor.test(x=ss_final_all$ATMP, y=ss_final_all$WTMP, method = 'spearman')
corr_ATMP_WTMP

corr_ATMP_PRES <- cor.test(x=ss_final_all$ATMP, y=ss_final_all$PRES, method = 'spearman')
corr_ATMP_PRES

corr_WTMP_PRES <- cor.test(x=ss_final_all$WTMP, y=ss_final_all$PRES, method = 'spearman')
corr_WTMP_PRES

####Correlations during periods with no cases

ss_final_all_no.cases<-subset(ss_final_all, Case.M == "No") #Times without cases

no_cases_corr_WSPD_ATMP <- cor.test(x=ss_final_all_no.cases$WSPD, y=ss_final_all_no.cases$ATMP, method = 'spearman')
no_cases_corr_WSPD_ATMP

no_cases_corr_WSPD_WTMP <- cor.test(x=ss_final_all_no.cases$WSPD, y=ss_final_all_no.cases$WTMP, method = 'spearman')
no_cases_corr_WSPD_WTMP

no_cases_corr_WSPD_PRES <- cor.test(x=ss_final_all_no.cases$WSPD, y=ss_final_all_no.cases$PRES, method = 'spearman')
no_cases_corr_WSPD_PRES

no_cases_corr_ATMP_WTMP <- cor.test(x=ss_final_all_no.cases$ATMP, y=ss_final_all_no.cases$WTMP, method = 'spearman')
no_cases_corr_ATMP_WTMP

no_cases_corr_ATMP_PRES <- cor.test(x=ss_final_all_no.cases$ATMP, y=ss_final_all_no.cases$PRES, method = 'spearman')
no_cases_corr_ATMP_PRES

no_cases_corr_WTMP_PRES <- cor.test(x=ss_final_all_no.cases$WTMP, y=ss_final_all_no.cases$PRES, method = 'spearman')
no_cases_corr_WTMP_PRES

####Correlations during periods with cases

ss_final_all_cases<-subset(ss_final_all, Case.M == "Yes") #Times with cases

cases_corr_WSPD_ATMP <- cor.test(x=ss_final_all_cases$WSPD, y=ss_final_all_cases$ATMP, method = 'spearman')
cases_corr_WSPD_ATMP

cases_corr_WSPD_WTMP <- cor.test(x=ss_final_all_cases$WSPD, y=ss_final_all_cases$WTMP, method = 'spearman')
cases_corr_WSPD_WTMP

cases_corr_WSPD_PRES <- cor.test(x=ss_final_all_cases$WSPD, y=ss_final_all_cases$PRES, method = 'spearman')
cases_corr_WSPD_PRES

cases_corr_ATMP_WTMP <- cor.test(x=ss_final_all_cases$ATMP, y=ss_final_all_cases$WTMP, method = 'spearman')
cases_corr_ATMP_WTMP

cases_corr_ATMP_PRES <- cor.test(x=ss_final_all_cases$ATMP, y=ss_final_all_cases$PRES, method = 'spearman')
cases_corr_ATMP_PRES

cases_corr_WTMP_PRES <- cor.test(x=ss_final_all_cases$WTMP, y=ss_final_all_cases$PRES, method = 'spearman')
cases_corr_WTMP_PRES

####Correlations during periods with deaths

ss_final_all_deaths<-subset(ss_final_all, Death.M == "Yes") #Times with Deaths

deaths_corr_WSPD_ATMP <- cor.test(x=ss_final_all_deaths$WSPD, y=ss_final_all_deaths$ATMP, method = 'spearman')
deaths_corr_WSPD_ATMP

deaths_corr_WSPD_WTMP <- cor.test(x=ss_final_all_deaths$WSPD, y=ss_final_all_deaths$WTMP, method = 'spearman')
deaths_corr_WSPD_WTMP

deaths_corr_WSPD_PRES <- cor.test(x=ss_final_all_deaths$WSPD, y=ss_final_all_deaths$PRES, method = 'spearman')
deaths_corr_WSPD_PRES

deaths_corr_ATMP_WTMP <- cor.test(x=ss_final_all_deaths$ATMP, y=ss_final_all_deaths$WTMP, method = 'spearman')
deaths_corr_ATMP_WTMP

deaths_corr_ATMP_PRES <- cor.test(x=ss_final_all_deaths$ATMP, y=ss_final_all_deaths$PRES, method = 'spearman')
deaths_corr_ATMP_PRES

deaths_corr_WTMP_PRES <- cor.test(x=ss_final_all_deaths$WTMP, y=ss_final_all_deaths$PRES, method = 'spearman')
deaths_corr_WTMP_PRES

#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#############################################Associations with Season###################################################

#Kruskal-Wallis test for all variables

kruskal.test(WSPD ~ Season.M, data = ss_final_all)

pairwise.wilcox.test(ss_final_all$WSPD, ss_final_all$Season.M,
                     p.adjust.method = "BH")

kruskal.test(ATMP ~ Season.M, data = ss_final_all)

pairwise.wilcox.test(ss_final_all$ATMP, ss_final_all$Season.M,
                     p.adjust.method = "BH")

kruskal.test(WTMP ~ Season.M, data = ss_final_all)

pairwise.wilcox.test(ss_final_all$WTMP, ss_final_all$Season.M,
                     p.adjust.method = "BH")

kruskal.test(PRES ~ Season.M, data = ss_final_all)

pairwise.wilcox.test(ss_final_all$PRES, ss_final_all$Season.M,
                     p.adjust.method = "BH")



