
#Import the necessary libraries
library(readxl)
library(summarytools)
library(ggplot2)
library(psych)

#Read the dataset from the given excel
csibank <- read_excel("csibank.xlsx")
#Open the dataset 'page' for viewing
View(csibank)
#See if there are missing values in any column (especially in the blocks that will be averaged)
print(colSums(is.na(csibank)))

#Create the new variables that are the averages of the distinct variables of each context block
csibank$Image <- rowMeans(csibank[, c("imag1", "imag2", "imag3", "imag4", "imag5")])
csibank$Expectation <- rowMeans(csibank[, c("expe1", "expe2", "expe3", "expe4")])
csibank$Quality <- rowMeans(csibank[, c("qual1", "qual2", "qual3", "qual4")])
csibank$Value <- rowMeans(csibank[, c("val1", "val2", "val3", "val4")])
csibank$Satisfaction <- rowMeans(csibank[, c("sat1", "sat2", "sat3")])
csibank$Loyalty <- rowMeans(csibank[, c("loy1", "loy2", "loy3")])

str(csibank)
csibank$Gender <- factor(csibank$Gender)
csibank$Age <- factor(csibank$Age, ordered = TRUE, levels = c("<=25", "26-35", "36-45", "46-55", "56-65",">=66"))
csibank$Education <- factor(csibank$Education)
csibank$Occupation <- factor(csibank$Occupation)
csibank$Region <- factor(csibank$Region)

#Show the distributions of factor variables
freq(csibank$Gender)
ggplot(csibank, aes(x=Gender))+geom_bar(stat="count")+labs(x="Gender", y="Count")
freq(csibank$Age)
ggplot(csibank, aes(x=Age))+geom_bar(stat="count")+labs(x="Age", y="Count")
freq(csibank$Education)
ggplot(csibank, aes(x=Education))+geom_bar(stat="count")+labs(x="Education", y="Count")
freq(csibank$Occupation)
ggplot(csibank, aes(x=Occupation))+geom_bar(stat="count")+labs(x="Occupation", y="Count")
freq(csibank$Region)
ggplot(csibank, aes(x=Region))+geom_bar(stat="count")+labs(x="Region", y="Count")

freq(csibank$Image)
ggplot(csibank, aes(x=Image))+geom_histogram()
freq(csibank$Expectation)
ggplot(csibank, aes(x=Expectation))+geom_histogram()
freq(csibank$Quality)
ggplot(csibank, aes(x=Quality))+geom_histogram()
freq(csibank$Value)
ggplot(csibank, aes(x=Value))+geom_histogram()
freq(csibank$Satisfaction)
ggplot(csibank, aes(x=Satisfaction))+geom_histogram()

freq(csibank$Loyalty)
ggplot(csibank, aes(x=Loyalty))+geom_histogram()

describe(csibank[,c(1:5)])
describe(csibank[,c(29:34)])

ggplot(csibank, aes(y=Loyalty, x=Gender))+geom_boxplot()
ggplot(csibank, aes(y=Loyalty, x=Age))+geom_boxplot()
ggplot(csibank, aes(y=Loyalty, x=Education))+geom_boxplot()
ggplot(csibank, aes(y=Loyalty, x=Occupation))+geom_boxplot()
ggplot(csibank, aes(y=Loyalty, x=Region))+geom_boxplot()

describeBy(csibank$Loyalty, csibank$Gender)
describeBy(csibank$Loyalty, csibank$Age)
describeBy(csibank$Loyalty, csibank$Education)
describeBy(csibank$Loyalty, csibank$Occupation)
describeBy(csibank$Loyalty, csibank$Region)
