rm(list=ls()) 
working.path<-"~/Dropbox/李晨/Data/"
# load original data
load(paste(working.path,'data irt all.Rdata'))
# load ability data
load.file<-paste(working.path,'student ability.Rdata')
load(load.file)

colnames(raw.data)<-c('ID','year','semester','course_name','course_ID','teacher','teacher_ID','Required',
                      'credit','grade')
# 計算平均成績, 合併入
head(raw.data)
library(tidyr)
library(dplyr)
raw.data<-tbl_df(dataeconomy)
raw.data %>% group_by(ID) %>% dplyr::summarise(cum.gpa=weighted.mean(grade,credit)) -> cum.gpa.data
head(cum.gpa.data)

raw.data %>% group_by(ID) %>% filter(Required != '通') %>% 
  dplyr::summarise(cum.gpa.no.GE=weighted.mean(grade,credit)) -> cum.gpa.data2
head(cum.gpa.data2)

head(irt.out)
tbl_df(irt.out)
irt.out<-left_join(irt.out,cum.gpa.data,by='ID')
irt.out<-left_join(irt.out,cum.gpa.data2,by='ID')

# 繪製成績與能力相關圖
head(irt.out)
library(ggplot2)
library(showtext)
#showtext.begin()
irt.out %>% ggplot(aes(x=ab,y=cum.gpa)) +
  geom_point()+
  ggtitle('學生能力與累計平均成績:未排除通識科目')+
  theme(text = element_text(family = 'STSongti-TC-Light'))
#showtext.end()

irt.out %>% ggplot(aes(x=ab,y=cum.gpa.no.GE)) +
  geom_point()+
  ggtitle('學生能力與累計平均成績:排除通識科目')+
  theme(text = element_text(family = 'STSongti-TC-Light'))

irt.out %>% gather(type,gpa,3:4) -> irt.out2
library(dplyr)
library(tidyr)
irt.out2$type<-as.factor(irt.out2$type)
#irt.out2$type<-revalue(irt.out2$type,c('cum.gpa'='含通識','cum.gpa.no.GE'='不含通識'))

irt.out2 %>% ggplot(aes(x=gpa,y=ab)) +
  geom_point()+
  facet_grid(.~type)+
  ggtitle('學生能力與累計平均成績')+
  theme(text = element_text(family = 'STSongti-TC-Light'))+
  xlab('平均成績')+
  ylab('經濟專業能力')

# 計算能力與平均成績的相關係數
irt.out2 %>% group_by(type) %>% dplyr::summarise(corr=cor(ab,gpa))

# 分析不同年入學學生的能力分配
irt.out3<-tbl_df(irt.out2)
irt.out3


# 創造入學年份資料 cohort 
year.3digit<-as.numeric(substring(irt.out3$ID,2,4))
year.2digit<-as.numeric(substring(irt.out3$ID,2,3))
cohort<- (year.3digit<105)*year.3digit+(year.2digit>90)*year.2digit
irt.out3$cohort<-as.factor(cohort)
levels(irt.out3$cohort)
# irt.out3$cohort<-as.factor(substr(irt.out3$ID,1,4))
# levels(irt.out3$cohort)
#irt.out3$cohort<-revalue(irt.out3$cohort,c('4100'='100','4101'='101','4987'='98','4997'='99','4998'='others'))
table(irt.out3$cohort)

irt.out3 %>% filter(type=='cum.gpa.no.GE',cohort!='others') %>%
  ggplot(aes(ab,linetype=cohort))+
  geom_density()

irt.out3 %>% filter(type=='cum.gpa',cohort!='others') -> irt.out4
irt.out4$cohort<-ordered(as.numeric(as.character(irt.out4$cohort)))
levels(irt.out4$cohort)


irt.out4 %>% group_by(cohort) %>% 
  dplyr::summarise(median.ab=median(ab),
                   mean.ab=mean(ab),
                   std.ab=sd(ab))

irt.out4 %>% ggplot(aes(x=cohort,y=ab))+
  geom_boxplot()+
  ggtitle('不同學年入學學生能力分佈')+
  theme(text = element_text(family = 'STSongti-TC-Light'))+
  xlab('入學年')+
  ylab('經濟專業能力')
#####################################################################
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
dodo<-dplyr::full_join(irt.out4,attend101eco,"ID")
dodo<-dplyr::filter(dodo,lala==1)
donot<-dplyr::full_join(irt.out4,donotattendeco,"ID")
donot<-dplyr::filter(donot,substring(donot$ID,2,4)==100)
donot<-dplyr::filter(donot,!is.na(ab))
lalaornot<-dplyr::union(dodo, donot)
lalaornot$lala[is.na(lalaornot$lala)]<-'no'
lalaornot$lala[lalaornot$lala==1]<-'yes'
lalaornot %>% ggplot(aes(x=lala,y=ab))+
  geom_boxplot()+
  ggtitle('有無參加lala學生能力分佈')+
  theme(text = element_text(family = 'STSongti-TC-Light'))+
  xlab('lala')+
  ylab('經濟專業能力')


#####################################################################
# 分析家族能力影響
library(stringr)
irt.out4$family_ID<-str_sub(irt.out4$ID,-5,-1) # 一般經濟系學生為 73xxx，若轉系生可能非73而是保留原系號
irt.out5<-dplyr::select(irt.out4,cohort,ab,family_ID)
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

