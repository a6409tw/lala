rm(list=ls()) 
load(paste(getwd(),"/ eco100.rds",sep=""))
library(dplyr)
library(ggplot2)
economy100

economy100 %>% group_by(lala) %>%
  summarise(mean_grade.1=mean(gpa.1,na.rm=TRUE),
            mean_grade.2=mean(gpa.2,na.rm=TRUE),
            mean_grade.3=mean(gpa.3,na.rm=TRUE),
            mean_grade.4=mean(gpa.4,na.rm=TRUE)) %>% 
  gather(grade,mean_grade,-lala) -> mean.across.grades

mean.across.grades$lala<-as.logical(mean.across.grades$lala)
mean.across.grades %>% ggplot(aes(x=grade,y=mean_grade, fill=lala)) +
  geom_bar(stat="identity",position='dodge')

# #No pipeline
# group_by(economy100,lala)->economy100.2
# summarise(economy100.2,mean(gpa.1,na.rm=TRUE))
# 
# group_by(lala,economy100)
# 
# economy100 %>% group_by(lala,.)
# 
# lala<-filter(economy100,lala==1)
# notlala<-filter(economy100,lala!=1)
# x1<-mean(lala$gpa.1,na.rm = TRUE)
# x2<-mean(notlala$gpa.1,na.rm = TRUE)
# x3<-mean(lala$gpa.2,na.rm = TRUE)
# x4<-mean(notlala$gpa.2,na.rm = TRUE)
# x5<-mean(lala$gpa.3,na.rm = TRUE)
# x6<-mean(notlala$gpa.3,na.rm = TRUE)
# x7<-mean(lala$gpa.4,na.rm = TRUE)
# x8<-mean(notlala$gpa.4,na.rm = TRUE)
# ttt<-matrix(c(1,2,3,4,1,2,3,4,x1,x3,x5,x7,x2,x4,x6,x8,1,1,1,1,0,0,0,0), nrow =8 , ncol = 3)
# ttt<-as.data.frame(ttt)
# 
# colnames(ttt)<-c("grade","gpa","lala")
# ttt$lala<-as.logical(ttt$lala)
# ggplot(ttt, aes(x = grade, y = gpa, colour = lala)) + geom_line()
