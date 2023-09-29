library(tidyverse)
library(glmnetUtils)
library(glmnet)
library(feather)
library(doSNOW)
library(magrittr)
library(tcltk)


datasets<-"DataProcessing\\ProcessedData\\" %>% list.files(full.names=TRUE)

myCluster<-makeCluster(10,
                       outfile="")

registerDoSNOW(myCluster)

for(i in datasets){
  
  print(i)
  df<-i%>%
    read_feather%>%
    group_by(Index)%>%
    mutate(dt=c(0,diff(Duration)))%>%
    filter(State!=0,
           (max(SOC)-min(SOC))>0.3,
           max(dt)<=10*median(dt)
           )
  
  df%>%
    group_by(Index)%>%
    arrange(Time)%>%
    mutate(Duration=Duration/60/60,
           dt=c(0,diff(Time%>%as.numeric))/60/60,
           #PDis=cumsum(Power*(Power>0)*dt)-(Power*(Power>0)*dt)/2-first(Power*(Power>0))*dt/2,
           #PChg=cumsum(Power*(Power<0)*dt)-(Power*(Power<0)*dt)/2-first(Power*(Power<0))*dt/2,
           PDis=cumsum(Power*(Power>0)*dt),
           PChg=cumsum(Power*(Power<0)*dt),
           delSOC=SOC-first(SOC),
           AvgDelSOC=abs(last(SOC)-first(SOC))/n())->df2
  

    Indexes<-df2%>%
      group_by(Index)%>%
      summarise(Time=min(Time))%>%
      arrange(as.numeric(Time))
    
    Strings<-df2$String%>%unique%>%length
    
    pb <- txtProgressBar(max = nrow(Indexes), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    Predicted<<-foreach(i = (Strings*6+1):nrow(Indexes),
                        .combine='bind_rows',
                        .packages = c("tidyverse","glmnet","glmnetUtils","dplyr","lubridate"),
                        .export = c("df2","Indexes","glmalpha","glmlambda"),
                        .errorhandling="remove",
                        .options.snow=opts) %dopar% {
                          
                          train<-df2%>%
                            filter(Time<Indexes$Time[i])%>%
                            group_by(String)%>%
                            arrange(Time)
                          
                          test<-df2%>%
                            filter(Index==Indexes$Index[i])
                          
                          
                          if(nrow(train)>=15){
                            
                            
                            model<-lm(formula=delSOC~Duration+PDis+PChg+0,
                                                       data=train,
                                      weights = AvgDelSOC)
                            
                            
                            #write.csv("hello",paste0(i,"hello.csv"))
                            
                            test%>%
                              mutate(Predicted=predict(model,test))->Predictions1
                            
                          }else{
                            test%>%
                              mutate(Predicted=NA)->Predictions1
                          }
                          
                          Predictions1
                        }

  filename<-gsub("DataProcessing\\\\ProcessedData\\\\","",i)
  filename<-gsub(".feather","",filename)
  
  Predicted%>%
    write_feather(paste0("Models\\LinearUnfiltered\\",filename,"LinearPredictions.feather"))
}

stopCluster(myCluster)


LinearPredictionFiles<-list.files("Models\\LinearUnfiltered\\",full.names=T)

for(i in 1:length(LinearPredictionFiles)){
  
  df<-LinearPredictionFiles[i]%>%
    read_feather
  
  df%>%
    group_by(Index)%>%
    summarise(String=first(String),
              Start=first(Time),
              End=last(Time),
              RMSE=sqrt(mean((Predicted-delSOC)^2)),
              delSOC=max(SOC)-min(SOC))->Cycles
  
}

Cycles%>%ggplot(aes(x=Start,y=RMSE))+geom_point()