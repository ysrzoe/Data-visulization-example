df <- read.csv('loan_data_train.csv')
#删除有空值的行
df <- na.omit(df)
#保留数字部分
df$Loan.Length = gsub("[^0-9]", "",df$Loan.Length)
df$Interest.Rate = gsub("%$","",df$Interest.Rate)
df$Debt.To.Income.Ratio = gsub("%$", "",df$Debt.To.Income.Ratio)

#FICO.range取均值
library(dplyr)
library(tidyr)
df <- df %>%
  separate(FICO.Range, c("range1", "range2"), sep="-")
df$range1 = as.numeric(df$range1)
df$range2 = as.numeric(df$range2)
df$FICO <- (df$range1+df$range2)/2
df<-df[,-grep("range1|range2",colnames(df))]

#Employment.Lengh, n/a = 0, <=1 = 1,>=10 = 10
df$Employment.Length[df$Employment.Length=="n/a"]<-0
df$Employment.Length = gsub("[^0-9]", "",df$Employment.Length)

#更改数据格式
glimpse(df)
num_df <- df[,c('Amount.Requested','Amount.Funded.By.Investors',
                'Interest.Rate','Loan.Length','Debt.To.Income.Ratio',
                'Monthly.Income','Inquiries.in.the.Last.6.Months',
                'FICO','Open.CREDIT.Lines','Revolving.CREDIT.Balance')]
num_df=as.data.frame(lapply(num_df,as.numeric))
###################dummy variable###################################
library(nnet)
c <- class.ind(df$Loan.Purpose)
d <- class.ind(df$State)
e <- class.ind(df$Home.Ownership)
df_new <- cbind(num_df,c,d,e)
num_df <- cbind(num_df,df$Home.Ownership)
write.csv(num_df,"C:\\Users\\ysr\\Desktop\\5218\\num_df.csv", row.names = FALSE)
#####################Correlation Matrix#############################################
library(corrgram)
corrgram(num_df, order=TRUE,lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         col.regions = colorRampPalette(c("dark green", "white",  "orange")),
         main="Correlation Matrix")
#########################Circular barplot#######################################
cb <- cbind(num_df,df$State)
cb%>%
  group_by(df$State)%>%
  summarise(muIR=mean(Interest.Rate)) -> mean_interest

cb%>%
  group_by(df$State)%>%
  summarise(muMI=mean(Monthly.Income)) -> mean_income
df1 <- cbind(mean_interest$`df$State`,mean_interest$muIR,mean_income$muMI)
colnames(df1)<-c('state','muIR','muMI')
df1 <- df1[-c(1),]
df1 <- as.data.frame(df1)
df1$muIR <- as.numeric(df1$muIR)
df1$muMI <- as.numeric(df1$muMI)
df1 <- mutate(df1, muMI.divide.100 = muMI/100)
df1 <- mutate(df1, muMI = -muMI)

library(ggplot2)

df1 <- df1 %>% 
  mutate(TreeRank = rank(-muIR), PopRank = rank(-muMI.divide.100)) %>% 
  mutate(SqRank = (TreeRank^2)+(PopRank^2)/2) %>% 
  mutate(RankOrder = rank(SqRank))
RankOrder = rank(df1$muIR)

ggplot(df1, aes(x = reorder(state, RankOrder))) +
  geom_col(aes(y = muIR), fill = "#2A9D8F") +
  geom_col(aes(y = muMI.divide.100), fill = "#E9C46A") +
  geom_text(aes(y = 80, label = state)) +
  coord_polar()+
  scale_y_continuous(limits = c(-150, 130)) +#y轴的范围为-150到130
  theme_void()
 Home.Ownership = df$Home.Ownership
library(ggforce)
ggplot(num_df, aes(FICO, Interest.Rate,color = Home.Ownership)) +
  geom_point()+
  theme_bw() +
  theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour='black'))
################################################################################
num_df$difference <- num_df$Amount.Requested-num_df$Amount.Funded.By.Investors
ggplot(num_df, 
       aes(x = Debt.To.Income.Ratio, y = Interest.Rate, size = difference)) +
  geom_point(alpha = .5, 
             fill="#B5ECB4", 
             shape=21) +
  theme_bw() +
  theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour='black'))
