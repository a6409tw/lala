rm(list=ls())
econ102_104<-read_excel("~/dropbox/lala/econ102_104.xlsx")

load("~/dropbox/Data/ student ability.Rdata")
load("~/Dropbox/Data/registration.Rdata")
regisdataecon<-registration.data[,c('學號','10402中文姓名')]
colnames(regisdataecon)<-c('ID','name')
merge1111<-full_join(regisdataecon,econ102_104,by='name')

dplyr::filter(merge1111,substring(merge1111$ID,2,4)!="100")->mergeecon
dplyr::filter(mergeecon,substring(mergeecon$ID,2,4)!="101")->mergeecon
dplyr::filter(mergeecon,substring(mergeecon$ID,2,3)!="98")->mergeecon
dplyr::filter(mergeecon,substring(mergeecon$ID,2,3)!="99")->mergeecon
eco101<-left_join(ec,irt.out2,by='ID')
econlala<-left_join(mergeecon,irt.out2,by='ID')

load("~/Dropbox/Data/transcript.Rdata")
econ.subject<-c("金融市場（一）",
                "金融市場（二）",
                "金融實務（一）",
                "金融實務（二）",
                "財務經濟學",
                "財務管理",
                "經濟結合之經濟學",
                "經濟發展",
                "經濟成長",
                "投資學",
                "保險學",
                "國際金融理論",
                "貨幣理論與政策",
                "期貨交易理論與實務",
                "企業管理",
                "合作經濟學",
                "公用事業經濟學",
                "法律經濟學",
                "租稅各論",
                "高科技產業分析",
                "產業經濟學",
                "創新經濟學",
                "管理經濟學",
                "網際網路經濟學",
                "醫療經濟學",
                "反托拉斯經濟學",
                "反托拉斯經濟學專題研究",
                "中小企業研究",
                "公營事業研究",
                "多國籍公司經濟學",
                "通訊傳播經濟分析",
                "競爭政策分析",
                "產業結構與政策",
                "國際貿易理論與政策",
                "國際貿易實務",
                "福利經濟",
                "管制經濟學",
                "土地經濟分析",
                "都市經濟學",
                "勞動經濟學",
                "資源經濟學",
                "環境經濟學",
                "人口經濟學",
                "家庭經濟學",
                "運動經濟學",
                "迴歸分析",
                "線性代數",
                "經濟資料視覺化處理",
                "作業研究",
                "計量經濟學",
                "高等統計學",
                "高等微積分",
                "經濟套裝軟體",
                "經濟數學",
                "賽局理論（一）",
                "賽局理論（二）",
                "一般均衡理論與應用",
                "成本效益分析",
                "行為經濟學",
                "資訊經濟學",
                "實驗經濟學",
                "應用數量方法",
                "統計軟體資料處理與應用",
                "個體經濟理論分析",
                "經濟理論分析",
                "動態經濟方法",
                "數理經濟學",
                "經濟閱讀",
                "經濟論壇（一）",
                "經濟論壇（二）",
                "經濟問題與政策（一）",
                "經濟問題與政策（二）",
                "經濟學原理",
                "個體經濟學",
                "公共經濟學",
                "統計學",
                "貨幣銀行學",
                "微積分",
                "總體經濟學",
                "國際經濟學",
                "經濟思想史")
library(dplyr)
transcript.data<-tbl_df(transcript.data)
transcript.data %>% filter(科目名稱 %in% econ.subject)->dataeconomy
x<-dataeconomy$學分數*dataeconomy$成績
mutate(dataeconomy,加權=x)->dataeconomy

gpa.track<-c()
for(i in 1:415){
  #print(i)
  #i<-1
  id.i<-econlala[i,c('ID')]
  # 抽出學號為id.i的學生成績單
  data.i <- filter(dataeconomy,學號==as.integer(id.i))

  gpa.i<- ( sum(data.i$加權)/sum(data.i$學分數))

gpa.track<-rbind(gpa.track,c(id.i,gpa.i))
  }


gpa.track2<-gpa.track
unlist(gpa.track2)->gpa.track2 #資料變成一串數字，非144x3的陣列
gpa.track2<-matrix(gpa.track2,415,2) #一串數字轉成144x3的矩陣
gpa.track2<-as.data.frame(gpa.track2) #矩陣改成data frame
colnames(gpa.track2)<-c('ID','gpa') 
table(gpa.track2[,2]) #查看狀態

gpa.102_104<-full_join(econlala,gpa.track2,by='ID')
gpa.102_104$lala[is.na(gpa.102_104$lala)]<-0
gpa.102_104$lala<-as.logical(gpa.102_104$lala)
gpa102<-dplyr::filter(gpa.102_104,substring(gpa.102_104$ID,2,4)=="102")
gpa103<-dplyr::filter(gpa.102_104,substring(gpa.102_104$ID,2,4)=="103")
gpa104<-dplyr::filter(gpa.102_104,substring(gpa.102_104$ID,2,4)=="104")
  
gpa102 %>% ggplot(aes(x=lala,y=gpa))+
  geom_boxplot()+
  ggtitle('102有無參加lala學生能力分佈')+
  theme(text = element_text(family = 'STSongti-TC-Light'))+
  xlab('lala')+
  ylab('平均成績')
gpa103 %>% ggplot(aes(x=lala,y=gpa))+
  geom_boxplot()+
  ggtitle('103有無參加lala學生能力分佈')+
  theme(text = element_text(family = 'STSongti-TC-Light'))+
  xlab('lala')+
  ylab('平均成績')
gpa104 %>% ggplot(aes(x=lala,y=gpa))+
  geom_boxplot()+
  ggtitle('104有無參加lala學生能力分佈')+
  theme(text = element_text(family = 'STSongti-TC-Light'))+
  xlab('lala')+
  ylab('平均成績')
