library(vegan)
library(mdatools)
library(psych)
library(resample)
library(igraph)
library(reshape2)
library(qgraph)
library(psych)
library(corrr)
library(ggplot2)
library(dplyr)
library(GGally)
library(stats)
library(dgof)
setwd("C:/Users/idpdl/Desktop/Paper Jessica")

#Loading Data
X <- read.csv('Deidentified Data.csv',header = T, dec = '.')

#Descriptive Statistics Visualization
 
#Period-wise statistics
ggplot(X, aes(x=Period, y=Average, fill=Period)) +
   geom_boxplot()+ scale_fill_brewer(palette="RdBu")

##Skill-wise statistics]
ggplot(X, aes(x=Period, y=RC, fill=Period)) +
   geom_boxplot()+ scale_fill_brewer(palette="RdBu")
ggplot(X, aes(x=Period, y=QR, fill=Period)) +
   geom_boxplot()+ scale_fill_brewer(palette="RdBu")
ggplot(X, aes(x=Period, y=CS, fill=Period)) +
   geom_boxplot()+ scale_fill_brewer(palette="RdBu")
ggplot(X, aes(x=Period, y=EP, fill=Period)) +
   geom_boxplot()+ scale_fill_brewer(palette="RdBu")
ggplot(X, aes(x=Period, y=WC, fill=Period)) +
   geom_boxplot()+ scale_fill_brewer(palette="RdBu")

# Splitting Data
temp <- as.numeric(grepl("2020", X$Period))
X$COVID <- temp


   X_COVID <- X[X$COVID==1,]
   X_No_COVID <- X[X$COVID==0,] 
 
   temp1 <- as.data.frame(X_COVID[,3:7])
   temp2 <- as.data.frame(X_No_COVID[,3:7])
   
# Testing Normality
   NormalityTests_No_COVID <- as.data.frame(matrix(rep(0,length(temp1[1,])),1,5))
   colnames(NormalityTests_No_COVID) <- colnames(temp1)
   NormalityTests_COVID <- NormalityTests_No_COVID

   for (i in 1:length(temp1[1,])) {
     NormalityTests_No_COVID[1,i] <- shapiro.test(temp2[sample(length(temp2[,i]), 5000, replace = F),i])
     NormalityTests_COVID[1,i] <- shapiro.test(temp1[sample(length(temp1[,i]), 5000, replace = F),i])
   }

#Differential Analysis: Testing mean and std. dev. equality
   #Hereby we use the Welch's t-teste and the Fisher's F-test
   pvals_means <- as.data.frame(matrix(rep(0,length(temp1[1,])),1,5))
   colnames(pvals_means) <- colnames(temp1)
   pvals_std_devs <- pvals_means
   for (i in 1:length(temp1[1,])) {
     temp <- t.test(temp1[,i],temp2[,i],alternative="two.sided",var.equal=F)
     pvals_means[1,i] <- temp$p.value
     temp <- var.test(temp1[,i], temp2[,i], alternative = "two.sided")
     pvals_std_devs[1,i] <- temp$p.value
   }
   
# Correlation Structures  
   ggpairs(temp1) 
   ggpairs(temp2)
   
# Correlation Networks
   corMat=cor(X_No_COVID[,3:7])
   corMat2=cor(X_COVID[,3:7])
   
   CorMat_mod <- corMat
   CorMat_mod[upper.tri(CorMat_mod)] <- 2
   cor_df1 <- melt(CorMat_mod )
   cor_df1 <- filter(cor_df1, value != 2) %>% filter(Var1 != Var2)
   adj_list1 <- cor_df1 %>% filter(abs(value) > 0.01)
   names(adj_list1) <- c('from', 'to', 'weight')
   net1 <- graph_from_data_frame(adj_list1, directed = FALSE)
   Cols <- c("red","blue")
   E(net1)$color <- unlist(lapply(1:nrow(adj_list1), function(i){Cols[(adj_list1$weight[i]>0)+1]}))
   E(net1)$size <- adj_list1$weight
   set.seed(2)
   
   CorMat_mod2 <- corMat2
   CorMat_mod2[upper.tri(CorMat_mod2)] <- 2
   cor_df2 <- melt(CorMat_mod2)
   cor_df2 <- filter(cor_df2, value != 2) %>% filter(Var1 != Var2)
   adj_list2 <- cor_df2 %>% filter(abs(value) > 0.01)
   names(adj_list2) <- c('from', 'to', 'weight')
   net2 <- graph_from_data_frame(adj_list2, directed = FALSE)
   Cols <- c("red","blue")
   E(net2)$color <- unlist(lapply(1:nrow(adj_list2), function(i){Cols[(adj_list2$weight[i]>0)+1]}))
   E(net2)$size <- adj_list2$weight
   
   par(mfrow=c(1,2))

      set.seed(2)
   plot(net1, vertex.size=45, vertex.label = 
           c("RC","QR","CS","EP","WC"), 
        edge.width= 12*E(net1)$size)
   
   set.seed(2)
   plot(net2, vertex.size=45, vertex.label = 
           c("RC","QR","CS","EP","WC"),
        edge.width= 12*E(net2)$size)
   
   #Jennrich Test
   n1 <- dim(X_No_COVID)[1]
   n2 <- dim(X_COVID)[1]
   pval_Jennrich <- cortest.jennrich(corMat,corMat2,n1,n2)$prob