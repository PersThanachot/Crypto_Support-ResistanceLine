df2$R_cluster = NA
#df2$cluster2 = NA

AAA_R <- NULL

for(i in 1:nrow(df2)){ # i = 1:n
  for(i in i){ # Start i[1],j[1:n] , i[2]:j[1:n] loop
    for(j in 1:nrow(df2)){
      outputR <- print((df2$High[i] - df2$High[j])^2)# distance keep in output
      
      AAA_R <- rbind(AAA_R,outputR) # combine output in AAA
      AAA_R <- data.frame(AAA_R) # Create Data Frame
      outR <- as.data.frame(matrix(AAA_R$AAA_R[AAA_R$AAA_R!=""], ncol=nrow(df1), byrow=TRUE)) # 1 column to start_Day:today column
      diag(outR)=max(Data_NEW$High,na.rm = TRUE)^2 # Change Diagonal = 999
      
      #df2$error[i] = min(out[i])
      #df2$cluster[i] = which(out[i]== min(out[i]))
      
    }
  }
}

####################################################################################
#find dot nearest
for(i in 1:nrow(df2)){
  df2$R_cluster[i] = which(outR[i]== min(outR[i])) # column[n] find min
  #df2$cluster2[i] = order(out[i])[2]
}
####################################################################################
# COlumn numeric to draw a line
df2$R_line = NA 

for(i in 1:nrow(df2)){ # i = 1:n
  for(i in i){ # Start i[1],j[1:n] , i[2]:j[1:n] loop
    for(j in 1:nrow(df2)){
      if(df2$R_cluster[i] == df2$No[j]){ # if cluster[1] == No[1,2,3,...,today] is TRUE/print Low is row TRUE
        df2$R_line[i] = df2$High[j] 
      }
    }  
  }
}
####################################################################################
# Column = group by & count
df2 <-  df2 %>% 
  group_by(df2$R_cluster) %>% mutate(R_count = n()) %>% ungroup() 
df2$`df2$R_cluster` <- NULL
####################################################################################
df2$R_Confirm <- ifelse(df2$R_count >= 2, "let's go","Nope") # if else
####################################################################################
ResistanceLine = NA

for(i in 1:nrow(df2)){
  if(df2$R_Confirm[i] == "let's go" & df2$High[i] >= df2$High[nrow(df2)]){
    ResistanceLine[i] = (c(df2$R_line[i]))
  }
}


plot(addLines(h=c(ResistanceLine),col = "yellow"))


