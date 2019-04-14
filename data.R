library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
# grade15a = read_excel('Grade Data\\LabReport1_Fall2015-A.xlsx')
# r = nrow(grade15a)
# c = ncol(grade15a)
# t()
# a = grade15a[9,3:28]
# b = as.data.frame(a)
# hist(b)
# typeof(b)
# c = t(b)
# hist(c)
# grade16a = read_excel('Grade Data\\LabReport1_Fall2016-A.xlsx')
# nrow(grade16a)
# ncol(grade16a)
# a = grade16a[9,3:36]
#boxplot

## survey

#get correct name and questions
s16post = read_csv('Survey Data\\Post-Survey Fall 2016.csv')
namecol = colnames(s16post)
questions = s16post[1,2:14]


#Set repeat
# a = str_split_fixed("Pre-Survey Fall 2016.csv", " ", n =3)
# 
# substrYear <- function(x){
#   substr(x, nchar(x)-7, nchar(x)-4)
# }
# 
# substrType <- function(x){
#   a = substr(x, 1, 4)
#   if (a == 'Post'){
#     ans = 'Post'
#   } else {
#     ans = 'Pre'
#   }
#   return(ans)
# }
################################# get final df##########################
finaldf = data.frame()
for (i in list.files('Survey Data')) {
  survey = read_csv(paste0('Survey Data/',i))
  t = survey[c(-1,-2),]
  t = t[complete.cases(t),]
  t = t[,2:14]
  colnames(t) = namecol[2:14]
  a = str_split_fixed(i,' ', n = 3)
  t['Term'] = paste(substr(a[3],1,4),a[2])
  t['Type'] = a[1]
  finaldf = rbind(finaldf,t)
}
data = as.data.frame(finaldf)
data$Term = as.factor(data$Term)
data$Type = as.factor(data$Type)
data$Q3 = NULL
# convert to factor
a = unique(data$Q2_2)
lvl = c(a[5],a[2],a[4],a[1],a[3])

for (i in 1:12) {
  data[,i] = as.factor(data[,i])
  levels(data[,i]) = lvl
}

data$Type = as.factor(data$Type)
levels(data$Type) = c('Pre-Survey', 'Post-Survey')

# wrap title
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}
wrapper(questions[3],70)

#######################################################################
# 
# ggplot(data,aes(x = Q2_4))+
#   geom_bar(aes(y = (..count..)/sum(..count..),fill = Type), position = "dodge")+
#   ggtitle(wrapper(questions[3],70))+
#   xlab("Answers")+
#   scale_y_continuous(labels = scales::percent, name = "Proportion")+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# ggplot(data,aes(eval(as.name("Q2_2"))))+
#   geom_bar(aes(y = (..count..)/sum(..count..),fill = Type), position = "dodge")+
#   title('questions[2]')+
#   scale_y_continuous(labels = scales::percent, name = "Proportion")+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
########################### Grade Data #######################################

# get data
grade_15A = read_xlsx('Grade Data\\LabReport1_Fall2015-A.xlsx')
grade_15A[,5] <- NULL
total_15A = as.numeric(grade_15A[9,3:27])

grade_16A = read_xlsx('Grade Data\\LabReport1_Fall2016-A.xlsx')
total_16A = as.numeric(grade_16A[9,3:27])

grade_15B = read_xlsx('Grade Data\\LabReport1_Fall2015-B.xlsx')
total_15B = as.numeric(grade_15B[9,3:27])

grade_16B = read_xlsx('Grade Data\\LabReport1_Fall2016-B.xlsx')
grade_16B[,5] <- NULL
total_16B = as.numeric(grade_16B[9,3:27])


df1 = data.frame(total_15A)
df1['term'] = '2015'
df1['class'] = 'A'
df2 = data.frame(total_16A)
df2['term'] = '2016'
df2['class'] = 'A'
df3 = data.frame(total_15B)
df3['term'] = '2015'
df3['class'] = 'B'
df4 = data.frame(total_16B)
df4['term'] = '2016'
df4['class'] = 'B'
colnames(df2) <- colnames(df1)
colnames(df3) <- colnames(df1)
colnames(df4) <- colnames(df1)
df = rbind(df1,df2,df3,df4)
colnames(df)<-c("grade","term",'class')
