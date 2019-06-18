# ggplot2 library
library(ggplot2)

# # DATA
# set.seed(345)
# Sector <- rep(c("S01","S02","S03","S04","S05","S06","S07"),times=9)
# Year <- as.numeric(rep(c("1950","1960","1970","1980","1990","2000","2010"),each=7))
# Value <- runif(49, 10, 100)
# data <- data.frame(Sector,Year,Value)
# 
# my_fun=function(vec){ as.numeric(vec[3]) / sum(data$Value[data$Year==vec[2]]) *100 }
# data$prop=apply(data , 1 , my_fun)
# 
# ggplot(data, aes(x=Year, y=prop, fill=Sector)) + 
#     geom_area(alpha=0.6 , size=1, colour="black")
#############################################
library(dplyr)
dataFolder

fileList<-list.files(dataFolder, recursive = TRUE)
countList<-fileList[grepl("cohort_count_per_year.csv",fileList)]

for (file in countList){
    #file = countList[1]
    count<-read.csv(file.path(dataFolder, file), stringsAsFactors = F)
    #Ticagrelor: 874, Clopidogrel: 929
    tica <- count [count$cohortDefinitionId == 874,]
    clo <- count [count$cohortDefinitionId == 929,]
    colnames(tica)[grepl("personCount",colnames(tica))]<-"ticaPersonCount"
    colnames(tica)[grepl("cohortDateSum",colnames(tica))]<-"ticaDateSum"
    
    colnames(clo)[grepl("personCount",colnames(clo))]<-"cloPersonCount"
    colnames(clo)[grepl("cohortDateSum",colnames(clo))]<-"cloDateSum"
    
    mergedCount<-dplyr::full_join(tica,clo,by = "year") %>% arrange(year)
    
    mergedCount$ticaPersonCount[is.na(mergedCount$ticaPersonCount)]<-0
    mergedCount$cloPersonCount[is.na(mergedCount$cloPersonCount)]<-0
    mergedCount$ticagrelorProp <- mergedCount$ticaPersonCount/(mergedCount$ticaPersonCount+mergedCount$cloPersonCount)
    
    ggplot(mergedCount, aes(x=year, y=ticagrelorProp)) + 
        geom_area(alpha=0.6 , size=1, colour="black")+
        coord_cartesian(xlim = c(2010:2017), ylim = c(0:1))
}


