library(ggplot2)
library(data.table)
library(scorecard)
library(ROCR)

breaks_adj = list(
  branch_region=c("1"),
  occupation_duration_total=c(1.3, 2, 2.4),
  occupation_duration_current=c(1.3, 2, 2.4),
  CollateralValueToDebt_Ratio=c(1.001, 1.04),#CollateralValueToDebt_Ratio=c(1.001, 1.04),
  ExpenseToTotalincome_Ratio=c(0.92, 0.95, 0.99),
  TotalincomeToDebt_Ratio=c(0.95),
  collateral_value=c(15.5,16.1,16.1,17.1),
  total_income=c(13.1, 13.35, 13.8),
  exposure=c(2,3),
  #NetIncomeToDebt_Ratio_Ratio=c(0.80, 0.85, 0.90),
  available_loan=c(15.3, 15.7, 16.2),
  gender=c("1"),
  age=c(24, 28, 34, 40, 50),
  sec_amount=c(15.7,17),
  total_household_expense=c(12, 12.55, 12.70),
  TotalincomePerTotalMember_Ratio=c(3,4,5.5),
  trans_score=c(-15, 49),#trans_score=c(-18, -15, 49),
  score_line=c(-4, -3, 0.8), 
  days_with_bank=c(0.6, 6.6, 7.6),
  total_overdue_days=c(0.1, 2.8, 3.5),
  dependent=c(1,2,3),
  urban_suburban=c( "0", "suburban", "urban1", "urban2"),
  area=c("0", "ger horoolol", "horoolol", "luxury")
)


setwd('/home/batbold/Codes/Consumer/')
dt_train=read.csv('data/consumer_without_dummy_outlier_included_train_rand100.csv')
dt_test=read.csv('data/consumer_without_dummy_outlier_included_test_rand100.csv')
#dt_train=read.csv('data/consumer_without_dummy_outlier_included_total.csv')

columns_final9<-c(
  'bad',
  'gender',
  'NetincomePerTotalMember_Ratio',
  'NetIncomeToDebt_Ratio_Ratio',
  'CollateralValueToDebt_Ratio',
  'branch_region',
  'trans_score',
  'score_line',
  'total_income',
  'occupation_duration_total',
  'exposure'
)

columns_test<-c(
  'bad',
  'branch_region',
  'occupation_duration_total',
  'occupation_duration_current',
  'trans_score',
  'CollateralValueToDebt_Ratio',
  'TotalincomeToDebt_Ratio',
  'collateral_value',
  'ExpenseToTotalincome_Ratio',
  'exposure',
  'total_income',
  'gender',
  'age',
  'TotalincomePerTotalMember_Ratio',
  'total_overdue_days'
)

columns_final9<-c(
  'bad',
  'occupation_duration_total',
  'branch_region',
  'days_with_bank',#'trans_score',
  'CollateralValueToDebt_Ratio',
  'TotalincomeToDebt_Ratio',
  'total_income',
  'exposure',
  'gender',
  'dependent'
)

columns_final8<-c(
  'bad',
  'occupation_duration_total',
  'branch_region',
  'days_with_bank',#'trans_score',
  'CollateralValueToDebt_Ratio',
  #'TotalincomeToDebt_Ratio',
  'total_income',
  'exposure',
  'gender',
  'dependent'
)

dt_test<-dt_test[columns_final8]
dt_train<-dt_train[columns_final8]

colnames(dt_train)[1]<-"y"
colnames(dt_test)[1]<-"y"

bins = woebin(dt_train, y="y",breaks_list=breaks_adj)
#bins = woebin(dt_train, "y")
#breaks_adj = woebin_adj(bins, dt_train, "y")

plotlist = woebin_plot(bins)
for (i in 1:length(plotlist)) {
  ggplot2::ggsave(
    paste0("images_bins/",names(plotlist[i]), ".png"), plotlist[[i]],
    width = 15, height = 9, units="cm" )
}

# converting train and test into woe values
train = woebin_ply(dt_train, bins)
test = woebin_ply(dt_test, bins)

#write.csv(train, file="data/train_woe.csv")

# glm ------
m1 = glm( y ~ ., family = "binomial", data = train)
summary(m1)

# predicted proability
train_pred = predict(m1, type='response', train)
test_pred = predict(m1, type='response', test)

# # ks & roc plot
png(filename="images/roc_train_final8.png")
train_perf=perf_eva(train$y, train_pred, title = "train")
dev.off()

png(filename="images/roc_test_final8.png")
test_perf=perf_eva(test$y, test_pred, title = "test")
dev.off()


#' # scorecard
card = scorecard(bins, m1, points0 = 600, odds0 = 1/19, pdo = 50)

columns_card<-c('variable','bin','count','count_distr','good',
                  'bad','badprob','woe','bin_iv','total_iv','points'
                  )

df_card<-setNames(data.frame(matrix(ncol =length(columns_card), nrow = 0)),columns_card)
basepoint_card_row<-c("basepoints",0,0,0,0,0,0,0,0,0,card[[1]]$points)
names(basepoint_card_row)<-columns_card 

df_card[1,]<-basepoint_card_row
  
for (i in 2:length(card)) {
  df_card_row=data.frame(card[[i]])
  df_card <- rbind(df_card,df_card_row) 
}

write.csv(df_card, file="data/scorecard_bins_8pars.csv")

# credit score, only_total_score = TRUE
train_score = scorecard_ply(dt_train, card,only_total_score = F)
test_score = scorecard_ply(dt_test, card,only_total_score = F)

write.csv(train_score, file="data/train_score_600_pdo50_final8_days_with_bank.csv")
write.csv(test_score, file="data/test_score_600_pdo50_final8_days_with_bank.csv")

# credit score, only_total_score = TRUE
train_score_good = scorecard_ply(dt_train[dt_train$y==0], card,only_total_score = F)
train_score_bad = scorecard_ply(dt_train[dt_train$y==1], card,only_total_score = F)

# credit score, only_total_score = TRUE
test_score_good = scorecard_ply(dt_test[dt_test$y==0], card,only_total_score = F)
test_score_bad = scorecard_ply(dt_test[dt_test$y==1], card,only_total_score = F)

write.csv(train_score_bad, file="data/train_score_bad_600_pdo50_final8_days_with_bank.csv")
write.csv(train_score_good, file="data/train_score_good_600_pdo50_final8_days_with_bank.csv")
write.csv(test_score_bad, file="data/test_score_bad_600_pdo50_final8_days_with_bank.csv")
write.csv(test_score_good, file="data/test_score_good_600_pdo50_final8_days_with_bank.csv")

# coeffs	
seed = 1
coef = data.frame(summary(m1)$coefficients)
coef$variable = row.names(coef)
row.names(coef)<-gsub("_woe$", "", row.names(coef))
coef$variable = row.names(coef)

row_result_auc<-c(seed,test_perf['AUC'],test_perf['KS'],test_perf['Gini'],
                  train_perf['AUC'],train_perf['KS'],train_perf['Gini'])

row_result_fit=c(coef[['Estimate']])
row_result<-c(row_result_auc,row_result_fit)

names(row_result) <- c('seed','AUC_test','KS_test','Gini_test',
                       'AUC_train','KS_train','Gini_train',
                       coef[['variable']])

#dt_one_row=setDT(as.list(row_result))[]
#df_result <- rbind(df_result, dt_one_row,fill=TRUE)
#df_result <- rbind(df_result, data.frame(row_result))
df_fit_result <- data.frame(row_result)

