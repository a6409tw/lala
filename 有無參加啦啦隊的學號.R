rm(list=ls())  
#載入入學資料
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
