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
#換名稱
colnames(registration.data)<-c( 'ID',	  '2',	  '3',	  '4',	  '5',	  '6',	  '7',	  '8',	  '9',	  '10',	  '11',	  '12',	  '13',	  '14',	  '15',	  '16',	  '17',	  '18',	  '19',	  '20',	  '21',	  '22',	  '23',	  '24',	  '25',	  '26',	  '27',	  '28',	  '29',	  '30',	  '31',	  '32',	  '33',	  '34',	  '35',	  '36',	  '37',	  '38',	  '39',	  '40',	  '41',	  '42',	  '43',	  '44',	  '45',	  '46',	  '47',	  '48',	  '49',	  '50',	  '51',	  '52',	  '53',	  '54',	  '55',	  '56',	  '57',	  '58',	  '59',	  '60',	  '61',	  '62',	  '63',	  '64',	  '65',	  '66',	  '67',	  '68',	  '69',	  '70',	  '71',	  '72',	  '73',	  '74',	  '75',	  '76',	  '77',	  '78',	  '79',	  '80',	  '81',	  '82',	  '83',	  '84',	  '85',	  '86',	  '87',	  '88',	  '89',	  '90',	  '91',	  '92',	  '93',	  '94',	  '95',	  '96',	  '97',	  '98',	  '99',	  '100')
#留名字跟學號
idandname<-dplyr::select(registration.data,ID,13, 24)
colnames(idandname)<-c( 'ID',	'department',  'name')
mergeeco<-dplyr::full_join(idandname,lala101,"name")

eco101<-dplyr::filter(mergeeco,substring(mergeeco$ID,2,4)==100)
eco101<-dplyr::filter(eco101,department.x=='經濟學系')
eco101<-dplyr::filter(eco101,ID!=410073195)
eco101<-dplyr::filter(eco101,ID!=410073196)
attend101eco<-dplyr::filter(eco101,lala==1)
donotattendeco<-dplyr::filter(eco101,is.na(lala))
attend101ecoID<-dplyr::select(attend101eco,ID)
donotattendecoID<-dplyr::select(donotattendeco,ID)
dodo<-dplyr::full_join(irt.out2,attend101eco,"ID")
dodo<-dplyr::filter(dodo,lala==1)
donot<-dplyr::full_join(irt.out2,donotattendeco,"ID")
donot<-dplyr::filter(donot,substring(donot$ID,2,4)==100)
donot<-dplyr::filter(donot,!is.na(ab))
lalaornot<-dplyr::union(dodo, donot)
lalaornot$lala[is.na(lalaornot$lala)]<-'no'
lalaornot$lala[lalaornot$lala==1]<-'yes'
lalaornot %>% ggplot(aes(x=lala,y=cum.gpa))+
  geom_boxplot()+
  ggtitle('有無參加lala學生能力分佈')+
  theme(text = element_text(family = 'STSongti-TC-Light'))+
  xlab('lala')+
  ylab('經濟專業能力')


#####################################################################
# 分析家族能力影響
library(stringr)
irt.out2$family_ID<-str_sub(irt.out2$ID,-5,-1) # 一般經濟系學生為 73xxx，若轉系生可能非73而是保留原系號
irt.out5<-dplyr::select(irt.out2,cohort,ab,family_ID)
irt.out5
irt.out5 %>% spread(cohort,ab) -> irt.out6



colnames(irt.out6)
irt.out6 %>% ggplot(aes(x=`98`,y=`99`))+
  geom_point()+stat_smooth(method = lm, se=FALSE)+
  theme(text = element_text(family = 'STSongti-TC-Light'))+
  xlab('98年入學的畢業能力')+
  ylab('99年入學的畢業能力')
irt.out6 %>% ggplot(aes(x=`99`,y=`100`))+
  geom_point()+stat_smooth(method = lm, se=FALSE)+
  theme(text = element_text(family = 'STSongti-TC-Light'))+
  xlab('99年入學的畢業能力')+
  ylab('100年入學的畢業能力')
irt.out6 %>% ggplot(aes(x=`100`,y=`101`))+
  geom_point()+stat_smooth(method = lm, se=FALSE)+
  theme(text = element_text(family = 'STSongti-TC-Light'))+
  xlab('100年入學的畢業能力')+
  ylab('101年入學的畢業能力')

cor(irt.out6$`98`,irt.out6$`99`,use = 'pairwise')
cor(irt.out6$`99`,irt.out6$`100`,use = 'pairwise')
cor(irt.out6$`100`,irt.out6$`101`,use = 'pairwise')

