rm(list=ls()) 
load("~/Dropbox/lala/ eco100.rds")

lala<-filter(economy100,lala==1)
notlala<-filter(economy100,lala!=1)
x1<-mean(lala$gpa.1,na.rm = TRUE)
x2<-mean(notlala$gpa.1,na.rm = TRUE)
x3<-mean(lala$gpa.2,na.rm = TRUE)
x4<-mean(notlala$gpa.2,na.rm = TRUE)
x5<-mean(lala$gpa.3,na.rm = TRUE)
x6<-mean(notlala$gpa.3,na.rm = TRUE)
x7<-mean(lala$gpa.4,na.rm = TRUE)
x8<-mean(notlala$gpa.4,na.rm = TRUE)
ttt<-matrix(c(1,2,3,4,1,2,3,4,x1,x3,x5,x7,x2,x4,x6,x8,1,1,1,1,0,0,0,0), nrow =8 , ncol = 3)
ttt<-as.data.frame(ttt)

colnames(ttt)<-c("grade","gpa","lala")
ttt$lala<-as.logical(ttt$lala)
ttt %>% ggplot(aes(x=grade,y=gpa,fill=lala)) + geom_line() ->p
p
