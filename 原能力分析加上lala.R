rm(list=ls()) 
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
#####################################################################
load("~/Dropbox/Data/ student ability.Rdata")

load("~/Dropbox/Data/registration.Rdata")
#載入啦啦隊名單
lala101<- read_excel("~/Dropbox/lala/lala101.xlsx")

regisdata2<-registration.data[,c('學號','10101中文姓名')]
colnames(regisdata2)<-c('ID','name')

mergeeco<-full_join(regisdata2,lala101,by='name')
eco101<-dplyr::filter(mergeeco,substring(mergeeco$ID,2,4)=="100")
lala.na.location<-which(is.na(eco101$lala)) #lala為NA的資料位置
eco101[lala.na.location,c('lala')]<-0

#樣本個數觀察
table(eco101$lala)

#合併能力與平圴成績
eco101<-left_join(eco101,irt.out2,by='ID')

#  觀察到有些沒lala的人，沒念完，所以無能力與成績資料，有可能在準備轉學。

eco101$lala<-as.logical(eco101$lala)
# 圖： 能力與lala
eco101 %>% ggplot(aes(x=lala,y=ab))+
  geom_boxplot()+
  ggtitle('有無參加lala學生能力分佈')+
  theme(text = element_text(family = 'STSongti-TC-Light'))+
  xlab('lala')+
  ylab('經濟專業能力')

# 圖： 平均成績與lala
eco101 %>% ggplot(aes(x=lala,y=cum.gpa))+
  geom_boxplot()+
  ggtitle('有無參加lala學生能力分佈')+
  theme(text = element_text(family = 'STSongti-TC-Light'))+
  xlab('lala')+
  ylab('平均成績')

# #留名字跟學號
# idandname<-dplyr::select(registration.data,ID,13, 24)
# colnames(idandname)<-c( 'ID',	'department',  'name')
# mergeeco<-dplyr::full_join(idandname,lala101,"name")
# 
# eco101<-dplyr::filter(mergeeco,substring(mergeeco$ID,2,4)==100)
# eco101<-dplyr::filter(eco101,department.x=='經濟學系')
# eco101<-dplyr::filter(eco101,ID!=410073195)
# eco101<-dplyr::filter(eco101,ID!=410073196)
# attend101eco<-dplyr::filter(eco101,lala==1)
# donotattendeco<-dplyr::filter(eco101,is.na(lala))
# attend101ecoID<-dplyr::select(attend101eco,ID)
# donotattendecoID<-dplyr::select(donotattendeco,ID)
# dodo<-dplyr::full_join(irt.out2,attend101eco,"ID")
# dodo<-dplyr::filter(dodo,lala==1)
# donot<-dplyr::full_join(irt.out2,donotattendeco,"ID")
# donot<-dplyr::filter(donot,substring(donot$ID,2,4)==100)
# donot<-dplyr::filter(donot,!is.na(ab))
# lalaornot<-dplyr::union(dodo, donot)
# lalaornot$lala[is.na(lalaornot$lala)]<-'no'
# lalaornot$lala[lalaornot$lala==1]<-'yes'
# lalaornot %>% ggplot(aes(x=lala,y=cum.gpa))+
#   geom_boxplot()+
#   ggtitle('有無參加lala學生能力分佈')+
#   theme(text = element_text(family = 'STSongti-TC-Light'))+
#   xlab('lala')+
#   ylab('經濟專業能力')
# 
# 
# #####################################################################
# # 分析家族能力影響
# library(stringr)
# irt.out2$family_ID<-str_sub(irt.out2$ID,-5,-1) # 一般經濟系學生為 73xxx，若轉系生可能非73而是保留原系號
# irt.out5<-dplyr::select(irt.out2,cohort,ab,family_ID)
# irt.out5
# irt.out5 %>% spread(cohort,ab) -> irt.out6
# 
# 
# 
# colnames(irt.out6)
# irt.out6 %>% ggplot(aes(x=`98`,y=`99`))+
#   geom_point()+stat_smooth(method = lm, se=FALSE)+
#   theme(text = element_text(family = 'STSongti-TC-Light'))+
#   xlab('98年入學的畢業能力')+
#   ylab('99年入學的畢業能力')
# irt.out6 %>% ggplot(aes(x=`99`,y=`100`))+
#   geom_point()+stat_smooth(method = lm, se=FALSE)+
#   theme(text = element_text(family = 'STSongti-TC-Light'))+
#   xlab('99年入學的畢業能力')+
#   ylab('100年入學的畢業能力')
# irt.out6 %>% ggplot(aes(x=`100`,y=`101`))+
#   geom_point()+stat_smooth(method = lm, se=FALSE)+
#   theme(text = element_text(family = 'STSongti-TC-Light'))+
#   xlab('100年入學的畢業能力')+
#   ylab('101年入學的畢業能力')
# 
# cor(irt.out6$`98`,irt.out6$`99`,use = 'pairwise')
# cor(irt.out6$`99`,irt.out6$`100`,use = 'pairwise')
# cor(irt.out6$`100`,irt.out6$`101`,use = 'pairwise')

