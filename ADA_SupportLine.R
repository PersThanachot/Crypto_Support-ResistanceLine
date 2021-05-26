library(tidyverse)
library(quantmod)

#Dowload Data
getSymbols("ADA-USD")
Data <- `ADA-USD` 

#Rename Column
names(Data)[1] <- "Open"
names(Data)[2]<- "High"
names(Data)[3]<- "Low" 
names(Data)[4] <- "Close"
names(Data)[5] <- "Volume"
names(Data)[6] <- "Adjusted"

# xts to data.frame
Data_NEW <- data.frame(print(Data[, c('Open',
                                            'High',
                                            'Low',
                                            'Close',
                                            'Volume',
                                            'Adjusted'
)]))
####################################################################################
#Column row to Real cOlumn
df <-  Data_NEW %>% rownames_to_column("date")
#Filter no >= 1240
df1 <- df %>% filter(date >= "2021-02-21")
####################################################################################
df2 <- df1
df2$No <- row.names(df2) 
# Charactor to Numeric
df2$No <- as.numeric(df2$No)
####################################################################################
#Scatter Plot
#ggplot() +
#  geom_point(aes(No,Low), col = "blue",alpha = 1, data = df2)

####################################################################################
df2$error = NA
df2$Support_cluster = NA
#df2$cluster2 = NA
  
AAA <- NULL

  for(i in 1:nrow(df2)){ # i = 1:n
    for(i in i){ # Start i[1],j[1:n] , i[2]:j[1:n] loop
      for(j in 1:nrow(df2)){
        output <- print((df2$Low[i] - df2$Low[j])^2)# distance keep in output
        
        AAA <- rbind(AAA,output) # combine output in AAA
        AAA <- data.frame(AAA) # Create Data Frame
        out <- as.data.frame(matrix(AAA$AAA[AAA$AAA!=""], ncol=nrow(df1), byrow=TRUE)) # 1 column to start_Day:today column
        diag(out)=max(Data_NEW$High,na.rm = TRUE)^2 # Change Diagonal = 999
        
        #df2$error[i] = min(out[i])
        #df2$cluster[i] = which(out[i]== min(out[i]))
       
      }
    }
  }

####################################################################################
#find dot nearest
for(i in 1:nrow(df2)){
df2$error[i] = min(out[i]) # i dont know.Why keep error?
df2$Support_cluster[i] = which(out[i]== min(out[i])) # column[n] find min
#df2$cluster2[i] = order(out[i])[2]
}
####################################################################################
# COlumn numeric to draw a line
df2$Support_line = NA 

for(i in 1:nrow(df2)){ # i = 1:n
  for(i in i){ # Start i[1],j[1:n] , i[2]:j[1:n] loop
    for(j in 1:nrow(df2)){
      if(df2$Support_cluster[i] == df2$No[j]){ # if cluster[1] == No[1,2,3,...,today] is TRUE/print Low is row TRUE
      df2$Support_line[i] = df2$Low[j] 
      }
    }  
  }
}
####################################################################################
# Column = group by & count
df2 <-  df2 %>% 
  group_by(df2$Support_cluster) %>% mutate(Support_count = n()) %>% ungroup() 
df2$`df2$Support_cluster` <- NULL
####################################################################################
df2$Support_Confirm <- ifelse(df2$Support_count >= 2, "let's go","Nope") # if else
####################################################################################
SupportLine = NA

for(i in 1:nrow(df2)){
  if(df2$Support_Confirm[i] == "let's go" & df2$Low[i] <= df2$Low[nrow(df2)]){
    SupportLine[i] = (c(df2$Support_line[i]))
  }
}

chartSeries(Data["2021-02-21/"]
            ,TA = NULL, theme = "black", up.col = "green", dn.col = "Orange"
            #,TA=NULL
            )

plot(addLines(h=c(SupportLine),col = "red"))

#plot(addLines(v=c(18,58,77),col = "blue"))

#plot(addLines(v=c(70,82),col = "blue"))