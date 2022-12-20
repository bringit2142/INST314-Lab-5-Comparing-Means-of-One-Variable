library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)


#Question 1
irisdataset <- read.csv("iristtest.csv")
view(irisdataset)

#Question 2
irisdataset %>%
  group_by(Species) %>%
  get_summary_stats(Petal.Length, type="mean_sd")

ggboxplot(irisdataset, x = 'Species', y = 'Petal.Length')

#Question 3
#Checking Assumptions
irisdataset %>%
  group_by(Species) %>%
  identify_outliers(Petal.Length)
#No extreme outliers in the dataset

irisdataset %>%
  group_by(Species) %>%
  shapiro_test(Petal.Length)
#Both groups are non-significant

ggqqplot(irisdataset, x = "Petal.Length", facet.by = "Species")
#Data points are normally distributed


TTestoutput <- irisdataset %>%
  t_test(Petal.Length ~ Species) %>%
  add_significance()
TTestoutput
#The p value <0.05, so there is a significant difference between the petal length of setosa
#and versicolor flowers.


#Question 4
micedataset <- read.csv("weightTtest.csv")
view(micedataset)


#Question 5
#Updating the dataset so that the 'before' and 'after' columns are in one single column 'status'
updatedmicedataset <- micedataset %>%
  gather(key = "status", value = "weight", before, after)
view(updatedmicedataset)

updatedmicedataset %>%
  group_by(status) %>%
  get_summary_stats(weight, type = "mean_sd")

#Create boxplot
ggboxplot(updatedmicedataset, x = 'status', y = 'weight')

#Paired plot graph
pairedplot <- ggpaired(updatedmicedataset, x = "status", y = "weight", 
                       order = c("before", "after"),
                       ylab = "Weight", xlab = "Status")
pairedplot


#Checking differences in each pair
micedataset <- micedataset %>% mutate(differences = before - after)
head(micedataset, 5)

micedataset %>% identify_outliers(differences)

micedataset %>% shapiro_test(differences)
#No outliers and the data is normally distributed


ggqqplot(micedataset, "differences")


#Question 6
PToutput <- updatedmicedataset %>%
  t_test(weight ~ status, paired = TRUE) %>%
  add_significance()
PToutput
#P-value is significantly smaller than .05, therefore there is a significant difference
#in the weight of the mice before and after the treatment.


#Question 7 
AnovaIris <- read.csv('iris.csv', stringsAsFactors=T)
view(AnovaIris)

AnovaIris %>% sample_n_by(Species,size = 1)
levels(AnovaIris$Species)

#Question 8
AnovaIris %>%
  group_by(Species) %>%
  get_summary_stats(Sepal.Length, type="mean_sd")

ggboxplot(AnovaIris,x="Species",y="Sepal.Length")

#Question 9
AnovaIris %>%
  group_by(Species) %>%
  identify_outliers(Sepal.Length)
#No extreme outliers

model <- lm(Sepal.Length ~ Species, data = AnovaIris)
ggqqplot(residuals(model))


AnovaIris %>%
  levene_test(Sepal.Length ~ Species)
#Data is significant, so data needs to be converted to logarithm

iris$Sepal.L.Log <- log(iris$Sepal.Length)
iris$Sepal.L.Log
#Petal.Length column converted to logarithm so that test can be carried out


pg.aov <- AnovaIris %>% anova_test(iris$Sepal.L.Log ~ Species)
pg.aov
#The p-value is significantly smaller than 0.05, so there is significant difference between
#the petal length of all 3 flowers

pg.pwc <- AnovaIris %>% tukey_hsd(Sepal.Length ~ Species)
pg.pwc
#Tukey's test suggest that there is a significant difference between each of the 3 petal lengths


#Question 10
MiceAnova <- read.csv('weightanova.csv')
view(MiceAnova)

#Shrink data into 2 columns, with each time being in one column, and 
#weight in another column
MiceAnova <- MiceAnova %>%
  gather(key = "time", value = "weight", t1, t2, t3) %>%
  convert_as_factor(id, time) # convert id and time into factor
view(MiceAnova)

#Question 11
MiceAnova %>%
  group_by(time) %>%
  get_summary_stats(weight, type = "mean_sd")

bxp <- ggboxplot(MiceAnova, x = "time", y = "weight", add = "point") 
bxp

#Question 12
#Checking Assumptions
MiceAnova %>%
  group_by(time) %>%
  identify_outliers(weight)
#No extreme outliers

MiceAnova %>%
  group_by(time) %>%
  shapiro_test(weight)
#All 3 time variables are >.05, which confirms normal distribution

ggqqplot(MiceAnova, "weight", facet.by = "time")

res.aov <- anova_test(data = MiceAnova, dv = weight, within = time, wid = id)
get_anova_table(res.aov)
#p-value is significantly smaller than 0.05, which indicates that there is a significant
#difference between the weights in each time frame.

bonf <- pairwise_t_test(data = MiceAnova, 
                        weight~time,
                        paired = T,
                        p.adjust.method = "bonferroni")
bonf
#This shows that the difference between weights is the greatest between t1 and t3,
#and by looking at the boxplots, it is clear that there is a significant weight
#difference between those 2 times.

rm(list=ls())
