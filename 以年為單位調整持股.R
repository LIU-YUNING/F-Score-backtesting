######################只看年報#########################################
library(RMySQL)    
library(keras)
library(dplyr)
library(parallel)     
library(lubridate)
library(tidyquant)
library(tidyverse) 
library(reshape2)  
library(dplyr)
library(quantmod)
library(keras)
library(magick) 
library(ggplot2)
library(grDevices) 
library(graphics)
library(dtw)
library(matrixStats)

rm(list=ls(all=T));gc()
load("data.RData")
#load("data1.RData")
#load("data2.RData")

# setwd("C:/Users/tinar/OneDrive/桌面/R期末/B074030025_財管111_劉于寧")
# getwd()
options(scipen=200)#取消e顯示



# FRy<-read.table("FR.txt",header=T,sep=",")
colnames(FRy)<-c("code","name","yearmonth","cashflow","profit","debt","current_ratio","share","gross","ROA","asset_turnover")
FR_year<-unique(FRy$yearmonth)

# SP<-read.table("SP.txt",header=T,sep=";")
colnames(SP)<-c("code","name","date","close")
SP<-SP %>%
  na.omit()%>%
  mutate(
    code=as.numeric(code),
    name=str_c(code,name)
  )

# 1. 當年度 ROA > 0
# 2. 當年度的營業現金流(cashflow) > 0
# 3. 當年度的營業現金流大於淨利(profit)
# 4. 當年度長期負債(debt)金額小於上一年度
# 5. 當年度流動比例（current_ratio）大於上一年度
# 6. 上一年度沒有發行新股(share)
# 7. 當年度的總資產報酬率(ROA)大於上一個年度的總資產報酬率
# 8. 當前毛利率(gross)大於上一年度
# 9. 當前資產週轉率(asset_turnover)大於上一年度

fscore<-FRy %>%
  arrange(code,yearmonth) %>%
  group_by(code) %>%
  na.omit()%>%
  mutate(
    name=str_c(code,name),
    code=as.numeric(code),
    cashflow=as.numeric(cashflow),
    profit=as.numeric(profit),
    debt=as.numeric(debt),
    current_ratio=as.numeric(current_ratio),
    share=as.numeric(share),
    gross=as.numeric(gross),
    ROA=as.numeric(ROA),
    asset_turnover=as.numeric(asset_turnover),
    
    s1=ifelse(ROA>0,1,0),
    s2=ifelse(cashflow>0,1,0),
    s3=ifelse(cashflow>profit,1,0),
    s4=ifelse(debt<lag(debt),1,0),
    s5=ifelse(current_ratio>lag(current_ratio),1,0),
    s6=ifelse(share<=lag(share),1,0),
    s7=ifelse(ROA>lag(ROA),1,0),
    s8=ifelse(gross>lag(gross),1,0),
    s9=ifelse(asset_turnover>lag(asset_turnover),1,0),
    total=s1+s2+s3+s4+s5+s6+s7+s8+s9
  ) %>% na.omit()


summary1<-data.frame()
summary2<-data.frame()
summary3<-data.frame()







for(year in FR_year)
{
  if(year==201512){next}#2015只是用來得出2016年財報的f-score
  fr_year=floor(year/100)
  sp_year=fr_year+1#購買年分為財報年分+1
  
  sp_start_date=sp_year*10000+331#從3/31之後開始
  sp_end_date=(sp_year+1)*10000+331
  thisFR<-fscore %>%
    filter(yearmonth==year)
  thisSP3<-SP %>%
    filter(date>=sp_start_date,
           date<sp_end_date)
  
  sp_start_date=sp_year*10000+331-5#-5：策略一要算5MA
  sp_end_date=(sp_year+1)*10000+331
  thisSP12<-SP %>%
    filter(date>=sp_start_date,
           date<sp_end_date)
  
  
  for(score in c(0:9))
  {
    codelist<-thisFR %>%
      filter(total==score)%>%
      pull(code)
    if(length(codelist)==0)
    {next}
    #================= 策略一=================    
    target1<-thisSP12 %>%
      filter(code %in% codelist)%>%
      arrange(code,date) %>%
      group_by(name) %>%na.omit()%>%
      filter(n()>=5)%>%
      mutate(
        MA5=SMA(close,n=5),
        buy_signal=ifelse((lag(close)<=lag(MA5)&close>MA5),1,0),
        sell_signal=ifelse((lag(close)>=lag(MA5)&close<MA5),1,0)
      )%>%na.omit()
    
    strategy1<-target1 %>%
      filter(buy_signal==1|sell_signal==1) %>%
      mutate(
        Ret=ifelse((lag(buy_signal)==1)&(sell_signal==1),(close/lag(close)-1),NA),
        hold_day=ifelse((lag(buy_signal)==1)&(sell_signal==1),(as.numeric(as.Date(as.character(date),format="%Y%m%d")-as.Date(as.character(lag(date)),format="%Y%m%d") )),NA)
      ) %>%
      na.omit() %>%
      group_by()%>%#############
    summarise(
      year=floor(year/100),
      score=score,
      AvgRet=mean(Ret),
      Avg_Prob_win=mean(Ret>=0),
      Avg_hold_day=mean(hold_day),
      Annual_Ret=AvgRet*365/Avg_hold_day
    )
    
    
    #================= 策略二=================    
    thiscode<-unique(target1$name)
    trade_table<-data.frame(matrix(ncol=8)) #股票該年交易表
    colnames(trade_table)<-c("name","score","buy_date","sell_date","buy_price","sell_price","Ret","hold_day")
    for(i1 in thiscode)
    {
      
      
      
      target2<-target1%>%
        filter(name==i1) %>%
        na.omit()
      
      status=0#現在有無部位
      highest_price=0
      
      for(i in c(1:length(target2$name)))
      {
        
        if(status==0 & target2$buy_signal[i]==1)#手上無部位->進場
        {
          status=1
          buy_price=target2$close[i]
          highest_price=target2$close[i]
          buy_date=target2$date[i]
        }
        else if(status==1 & target2$close[i]>highest_price)
        {
          highest_price=target2$close[i]
        }
        else if(status==1 & target2$close[i]<=highest_price*0.9)
        {
          status=0
          highest_price=0
          Ret=(target2$close[i]/buy_price)-1
          sell_price=target2$close[i]
          sell_date=target2$date[i]
          nname=target2$name[i]
          hold_day=as.numeric(as.Date(as.character(sell_date),format="%Y%m%d")-as.Date(as.character(buy_date),format="%Y%m%d") )
          
          trade_table=rbind(trade_table,c(nname,score,buy_date,sell_date,buy_price,sell_price,Ret,hold_day))
        }
        
      }
      
      
      
    }
    
    strategy2<-trade_table %>%
      na.omit()%>%
      group_by()%>%
      mutate(
        hold_day=as.numeric(hold_day),
        Ret=as.numeric(Ret)
      )%>%
      summarise(
        year=floor(year/100),
        score=score,
        AvgRet=mean(Ret),
        Avg_Prob_win=mean(Ret>=0),
        Avg_hold_day=mean(hold_day),
        Annual_Ret=AvgRet*365/Avg_hold_day
      )%>%
      filter(row_number()==1)
    
    
    
    #================= 策略三=================  
    target3<-thisSP3 %>%
      filter(code %in% codelist)%>%
      arrange(code,date) %>%
      group_by(name)   
    
    strategy3<-target3 %>%
      filter(row_number()==1|row_number()==n()) %>%
      mutate(
        Ret=(close/lag(close)-1),
        hold_day=as.numeric(as.Date(as.character(date),format="%Y%m%d")-as.Date(as.character(lag(date)),format="%Y%m%d") )
      ) %>%
      na.omit() %>%
      group_by()%>%
      summarise(
        year=floor(year/100),
        score=score,
        AvgRet=mean(Ret),
        Avg_Prob_win=mean(Ret>=0),
        Avg_hold_day=mean(hold_day),
        Annual_Ret=AvgRet*365/Avg_hold_day
      )
    
    # Ret_table<-Ret_table %>% bind_rows(target)
    summary1=rbind(summary1,strategy1)
    summary2=rbind(summary2,strategy2)
    summary3=rbind(summary3,strategy3)  
  }
  
  cat(year)
}

ssummary1<-data.frame()
ssummary2<-data.frame()
ssummary3<-data.frame()

for(s in c(0:9))
{
  temp<-summary1%>%
    filter(score==s) %>%
    group_by() %>%
    summarise(
      score=s,
      AvgRet=mean(AvgRet),
      Avg_Prob_win=sum(Avg_Prob_win)/n(),
      Avg_hold_day=mean(Avg_hold_day),
      Annual_Ret=AvgRet*365/Avg_hold_day
    ) %>%
    filter(row_number()==1)
  ssummary1=rbind(ssummary1,temp) 
}

for(s in c(0:9))
{
  temp<-summary2%>%
    filter(score==s) %>%
    group_by() %>%
    summarise(
      score=s,
      AvgRet=mean(AvgRet),
      Avg_Prob_win=sum(Avg_Prob_win)/n(),
      Avg_hold_day=mean(Avg_hold_day),
      Annual_Ret=AvgRet*365/Avg_hold_day
    ) %>%
    filter(row_number()==1)
  ssummary2=rbind(ssummary2,temp) 
}

for(s in c(0:9))
{
  temp<-summary3%>%
    filter(score==s) %>%
    group_by() %>%
    summarise(
      score=s,
      AvgRet=mean(AvgRet),
      Avg_Prob_win=sum(Avg_Prob_win)/n(),
      Avg_hold_day=mean(Avg_hold_day),
      Annual_Ret=AvgRet*365/Avg_hold_day
    ) %>%
    filter(row_number()==1)
  ssummary3=rbind(ssummary3,temp) 
}

#summary3不同天是因為有些股票提早下市(2020則只有交易到2021年末)