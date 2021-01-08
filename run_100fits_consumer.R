run_100fits_consumer <- function(df_tmp) {
    
	library(ggplot2)
        library(data.table)
        library(scorecard)
        library(ROCR)

  breaks_adj_old = list(
        branch_region=c("1"),
        branch_cat=c("Highstat-Highrisk", "Highstat-Averisk%,%Lowstat"),
        address_proxy=c(0.9, 1.9),
        occupation_duration_total=c(1.3, 2, 2.4),
        CollateralValueToDebt_Ratio=c(1.001, 1.04), #CollateralValueToDebt_Ratio=c(1.001, 1.04, 1.11),
        ExpenseToTotalincome_Ratio=c(0.92, 0.95, 0.99),
        total_income=c(13.1, 13.35, 13.8),
        exposure=c(2,3),
        NetIncomeToDebt_Ratio_Ratio=c(0.80, 0.85, 0.90),#NetIncomeToDebt_Ratio_Ratio=c(0.75, 0.85, 0.95),
        available_loan=c(15.3, 15.7, 16.2),
        gender=c("1"),
        age=c(24, 28, 34, 40, 50),
        sec_amount=c(15.7,17),
        total_household.expense=c(12, 12.55, 12.70),
        NetincomePerTotalMember_Ratio=c(5,10),
        trans_score=c(-15, 49), #trans_score=c(-18, -15, 49),
        #score_line=c(-3, 0.8), # score_line=c(-4, -3, 0.8),
        days_with_bank=c(0.6, 6.6),#days_with_bank=c(0.6, 6.6, 7.6),
        dependent=c(1,2,3),
        urban_suburban=c( "0", "suburban", "urban1", "urban2"),
        area=c("0", "ger horoolol", "horoolol", "luxury")
  )

breaks_adj = list(
  branch_region=c("1"),
  branch_cat=c("Highstat-Highrisk", "Highstat-Averisk%,%Lowstat"),
  address_proxy=c(0.9, 1.9),
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

        bins = woebin(df_tmp, y="bad", breaks_list=breaks_adj, print_step=0)

  plotlist = woebin_plot(bins)
  for (i in 1:length(plotlist)) {
    ggplot2::ggsave(
    paste0("images_bins/",names(plotlist[i]), ".png"), plotlist[[i]],
    width = 15, height = 9, units="cm" )
  }

        seeds<-c( 135,  343,  382,  532,  551,  581,  773,  841,  846,  874,  948,
          1036, 1106, 1353, 1542, 1599, 1676, 1768, 1857, 1919, 1996, 2033,
          2218, 2263, 2327, 2347, 2397, 2569, 2592, 2595, 2733, 2748, 2756,
          2785, 2808, 2948, 3008, 3059, 3241, 3258, 3402, 3700, 3744, 3828,
          4008, 4110, 4175, 4506, 4562, 4650, 4658, 4687, 4824, 4978, 5271,
          5295, 5373, 5375, 5501, 5567, 5611, 5737, 5767, 5868, 6203, 6222,
          6289, 6348, 6395, 6416, 6467, 6503, 6517, 6586, 6726, 7115, 7210,
          7221, 7283, 7388, 7429, 7628, 7703, 7716, 7736, 7860, 8279, 8367,
          8459, 8670, 8767, 8915, 8998, 9061, 9151, 9423, 9483, 9636, 9665,
          9764)
    

	columns_result<-c('seed','AUC_test','KS_test','Gini_test',
                  	'AUC_train','KS_train','Gini_train','(Intercept)',
                  	colnames(df_tmp)[2:length(df_tmp)])

	df_result <- setNames(data.frame(matrix(ncol = length(columns_result), nrow = 0)), columns_result)
        
	for (seed in seeds) {
              dt_list = split_df(df_tmp, "bad", ratio = 0.8, seed=seed)
              dt_train = dt_list$train; dt_test = dt_list$test
  
              # converting train and test into woe values
              train = woebin_ply(dt_train, bins, print_step = 0)
              test = woebin_ply(dt_test, bins, print_step = 0)
              # glm ------
              m1 = glm( bad ~ ., family = "binomial", data = train)

              #summary(m1)
	      # coeffs	
	      coef = data.frame(summary(m1)$coefficients)
  	      coef$variable = row.names(coef)
              row.names(coef)<-gsub("_woe$", "", row.names(coef))
    	      coef$variable = row.names(coef)

              # predicted proability
              train_pred = predict(m1, type='response', train)
              test_pred = predict(m1, type='response', test)
 
              # ks & roc plot
              train_perf=perf_eva(train$bad, train_pred, title = "train",show_plot=FALSE)
              test_perf=perf_eva(test$bad, test_pred, title = "test",show_plot=FALSE)

              row_result_auc<-c(seed,test_perf['AUC'],test_perf['KS'],test_perf['Gini'],
    					train_perf['AUC'],train_perf['KS'],train_perf['Gini'])

  	      row_result_fit=c(coef[['Estimate']])
  	      row_result<-c(row_result_auc,row_result_fit)
              names(row_result) <- c('seed','AUC_test','KS_test','Gini_test',
                         	'AUC_train','KS_train','Gini_train',
                         	coef[['variable']]
                         )

	      #dt_one_row=setDT(as.list(row_result))[]
	      #df_result <- rbind(df_result, dt_one_row,fill=TRUE)
	      df_result <- rbind(df_result, data.frame(row_result))

        }
	
	#fwrite(df_result,filename)
	df_result	
}
