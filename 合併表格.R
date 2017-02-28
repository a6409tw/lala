#合併姓名啦啦隊表格以及學號啦啦隊表格
#比較101學年經濟系有參加啦啦隊以及沒參加啦啦隊的能力表現
rm(list=ls())  
library(tidyr)
library(dplyr)
working.path<-"~/Dropbox/lala"
library(readxl)
lalaexample<- read_excel("~/Dropbox/lala/lalaexample.xlsx")
idexample<- read_excel("~/Dropbox/lala/idexample.xlsx")
mergetry<-dplyr::full_join(idexample,lalaexample,"name")
head(mergetry)
mergetry11<-dplyr::select(mergetry,name,year.x,department.x,id,lala,ability)
attendlala<-dplyr::filter(mergetry11,lala==1)
donotattend<-dplyr::filter(mergetry11,is.na(lala))


