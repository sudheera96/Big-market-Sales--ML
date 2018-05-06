# Big-market-Sales--ML

install.packages("data.table")
library(data.table)
getwd()
setwd("C://Users//ADMIN//Desktop")
train=fread("Train_UWu5bXk.csv")
test=fread("Test_u94Q5KV.csv")
submission=fread("SampleSubmission_TmnO39y.csv")
dim(train)
dim(test)
names(train)
names(test)
str(train)
str(test)
test[,Item_Outlet_Sales :=NA]
combi=rbind(train,test)
View(combi)
dim(combi)
library(ggplot2)
ggplot(train)+geom_histogram(aes(train$Item_Outlet_Sales),binwidth = 100,fill='darkgreen')+
  xlab('Item_Outlet_Sales')
p1=ggplot(combi)+geom_histogram(aes(Item_Weight),binwidth = 0.5,fill='blue')
p2=ggplot(combi)+geom_histogram(aes(Item_Visibility),binwidth = 0.005,fill="blue")
p3=ggplot(combi)+geom_histogram(aes(Item_MRP),binwidth = 1,fill='blue')
install.packages('cowplot')
library(cowplot)
plot_grid(p1,p2,p3,nrow = 1)
library(dplyr)
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(count=n()))+
  geom_bar(aes(Item_Fat_Content,count),stat='identity',fill='coral1')
combi$Item_Fat_Content[combi$Item_Fat_Content=='LF']='Low Fat'
combi$Item_Fat_Content[combi$Item_Fat_Content=='low fat']='Low Fat'
combi$Item_Fat_Content[combi$Item_Fat_Content=='reg']='Regular'
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(count=n()))+
  geom_bar(aes(Item_Fat_Content,count),stat='identity',fill='coral1')

p4=ggplot(combi %>% group_by(Item_Type) %>% summarise(count=n()))+
  geom_bar(aes(Item_Type,count),stat = 'identity',fill='coral1')+
  xlab(" ")+
  geom_label(aes(Item_Type,count,label=count),vjust=0.5)+
  theme(axis.text.x = element_text(angle = 45,hjust=1))+
  ggtitle('Item_Type')

p5=ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(count=n()))+
  geom_bar(aes(Outlet_Identifier,count),stat = 'identity',fill='coral1')+
  geom_label(aes(Outlet_Identifier,count,label=count))+
  theme(axis.text.x = element_text(angle = 45,hjust=1))
p6=ggplot(combi %>% group_by(Outlet_Size) %>% summarise(count=n()))+
  geom_bar(aes(Outlet_Size,count),stat = 'identity',fill='coral1')+
  geom_label(aes(Outlet_Size,count,label=count))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))

second_row=plot_grid(p5,p6,nrow = 1)

plot_grid(p4,second_row,ncol = 1)

p7=ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(count =n()))+
  geom_bar(aes(Outlet_Establishment_Year,count),stat = 'identity',fill='coral1')+
  geom_label(aes(Outlet_Establishment_Year,count,label=count))

p8=ggplot(combi %>% group_by(Outlet_Type) %>% summarize(count=n()))+
  geom_bar(aes(Outlet_Type,count),stat = 'identity',fill='coral1')+
  geom_label(aes(Outlet_Type,count,label=count))

plot_grid(p7,p8,ncol = 2)

train=combi[1:nrow(train)]

p9=ggplot(train)+geom_point(aes(Item_Weight,Item_Outlet_Sales),colour='violet',alpha=0.8)
p10=ggplot(train)+geom_point(aes(Item_Visibility,Item_Outlet_Sales),colour='violet')
p11=ggplot(train)+geom_point(aes(Item_MRP,Item_Outlet_Sales),colour='violet')
second_row1=plot_grid(p10,p11,nrow = 1)
plot_grid(p9,second_row1,ncol = 1)

p12=ggplot(train)+geom_violin(aes(Item_Type,Item_Outlet_Sales),fill='magenta')+
  theme(axis.text.x = element_text(angle = 45,hjust=1))
p13=ggplot(train)+geom_violin(aes(Item_Fat_Content,Item_Outlet_Sales),fill='magenta')
p14=ggplot(train)+geom_violin(aes(Outlet_Identifier,Item_Outlet_Sales),fill='magenta')+
  theme(axis.text.x = element_text(angle = 45,hjust=1))
second_row2=plot_grid(p13,p14,nrow=1)
plot_grid(p12,second_row2,ncol = 1)

p15=ggplot(train)+geom_violin(aes(Outlet_Size,Item_Outlet_Sales),fill='magenta')
p16=ggplot(train)+geom_violin(aes(Outlet_Location_Type,Item_Outlet_Sales),fill='magenta')
p17=ggplot(train)+geom_violin(aes(Outlet_Type,Item_Outlet_Sales),fill='magenta')
second_row3=plot_grid(p15,p16,nrow = 1)
plot_grid(second_row3,p17,ncol = 1)

colSums(is.na(combi))

missing_index=which(is.na(combi$Item_Weight))
for (i in missing_index) {
  item=combi$Item_Identifier[i]
  combi$Item_Weight[i]=mean(combi$Item_Weight[combi$Item_Identifier==item],na.rm = TRUE)
  
}

Zero_index=which(combi$Item_Visibility==0)
for (i in Zero_index) {
  item=combi$Item_Identifier[i]
  combi$Item_Visibility[i]=mean(combi$Item_Visibility[combi$Item_Identifier==item],na.rm = TRUE)
  
}

perishable=c('Breads','Breakfast','Dairy','Fruits and Vegetables','Meat','Seafood')
non_perishable=c('Baking Goods','Canned','Frozen Foods','Hard Drinks','Health and Hygiene','Household','Soft Drinks')
combi[,Item_Type_new:=ifelse(Item_Type %in% perishable,'perishable',
                             ifelse(Item_Type %in% non_perishable,'non perishable','not sure'))]

table(combi$Item_Type,substr(combi$Item_Identifier,1,2))
combi[,item_category:=substr(combi$Item_Identifier,1,2)]

combi$Item_Fat_Content[combi$item_category=='NC']='Non Edible'

combi[,Outlet_years:=2013- Outlet_Establishment_Year]
combi$Outlet_Establishment_Year=as.factor(combi$Outlet_Establishment_Year)
combi[,price_per_unit_wt:= Item_MRP/Item_Weight]
combi[,Item_MRP_cluster:=ifelse(Item_MRP<69,'1st',
                                ifelse(Item_MRP>=69 & Item_MRP<136,'2nd',
                                ifelse(Item_MRP>=136 & Item_MRP<203,'3rd','4th')))]

combi[,Outlet_Size_num:=ifelse(Outlet_Size=='Small',0,
                               ifelse(Outlet_Size=='Medium',1,2))]
combi[,Outlet_Location_Type_num:=ifelse(Outlet_Location_Type=='Tier 3',0,
                                        ifelse(Outlet_Location_Type=='Tier 2',1,2))]
combi[,c('Outlet_Size','Outlet_Location_Type'):=NULL]

library(caret)
ohe=dummyVars('~.',data=combi[,-c('Item_Identifier','Outlet_Establishment_Year','Item_Type')],fullRank = TRUE)
ohe_df=data.table(predict(ohe,combi[,-c('Item_Identifier','Outlet_Establishment_Year','Item_Type')]))
combi=cbind(combi[,'Item_Identifier'],ohe_df)                  

combi[,Item_Visibility:=log(Item_Visibility+1)]
combi[,price_per_unit_wt:=log(price_per_unit_wt+1)]

num_vars=which(sapply(combi, is.numeric))
num_vars_names=names(num_vars)

combi_numeric=combi[,setdiff(num_vars,'Item_Outlet_Sales'),with=FALSE]
pre_num=preProcess(combi_numeric,method = c('center','scale'))
combi_numeric_norm=predict(pre_num,combi_numeric)

combi[,setdiff(num_vars_names,'Item_Outlet_Sales'):=NULL]
combi=cbind(combi,combi_numeric_norm)

train=combi[1:nrow(train)]
test=combi[(nrow(train)+1):nrow(combi)]
test[,Item_Outlet_Sales:=NULL]

cor_train=cor(train[,-c('Item_Identifier')])
install.packages('corrplot')
library(corrplot)
corrplot(cor_train,method = 'pie',type = 'lower',tl.cex = 0.9)

install.packages('glmnet')
library(glmnet)

set.seed(1234)
my_control=trainControl(method = 'cv',number = 5)
linear_reg_model=train(x=train[,-c('Item_Identifier','Item_Outlet_Sales')],y=train$Item_Outlet_Sales,
                       method='glmnet',trControl=my_control)
submission$Item_Outlet_Sales=predict(linear_reg_model,test[,-c('Item_Identifier')])
write.csv(submission,'Linear_Reg_submit.csv',row.names = FALSE)
summary(linear_reg_model)

plot(linear_reg_model)

set.seed(1235)
my_control = trainControl(method="cv", number=5)
Grid = expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0002))

lasso_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales,
                             method='glmnet', trControl= my_control, tuneGrid = Grid)
plot(lasso_linear_reg_mod)

set.seed(1236)
my_control = trainControl(method="cv", number=5)
Grid = expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0002))

ridge_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales,
                             method='glmnet', trControl= my_control, tuneGrid = Grid)
plot(ridge_linear_reg_mod)

install.packages('ranger')
library(ranger)
set.seed(1237)
my_control = trainControl(method="cv", number=5)
tgrid = expand.grid(
  .mtry = c(3:10),
  .splitrule = "variance",
  .min.node.size = c(10,15,20)
  )
rf_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], 
               y = train$Item_Outlet_Sales,
               method='ranger', 
               trControl= my_control, 
               tuneGrid = tgrid,
               num.trees = 400,
               importance = "permutation")
plot(rf_mod)

param_list = list(
  
  objective = "reg:linear",
  eta=0.01,
  gamma = 1,
  max_depth=6,
  subsample=0.8,
  colsample_bytree=0.5
)
install.packages('xgboost')
library(xgboost)

dtrain = xgb.DMatrix(data = as.matrix(train[,-c("Item_Identifier", "Item_Outlet_Sales")]), label= train$Item_Outlet_Sales)
dtest = xgb.DMatrix(data = as.matrix(test[,-c("Item_Identifier")]))

set.seed(112)
xgbcv = xgb.cv(params = param_list, 
               data = dtrain, 
               nrounds = 1000, 
               nfold = 5, 
               print_every_n = 10, 
               early_stopping_rounds = 30, 
               maximize = F)

xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 430)
var_imp = xgb.importance(feature_names = setdiff(names(train), c("Item_Identifier", "Item_Outlet_Sales")), 
                         model = xgb_model)
xgb.plot.importance(var_imp)
