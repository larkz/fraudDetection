write.csv(sales_order_effAll_withLabel, file = "allCustomerSummary.csv")
write.csv(sales_order_summary_cat, file = "allCategorySummary.csv")
write.csv(sales_order_summary_cat_t1, file = "t1CategorySummary.csv")


getStemString <- function(str){
  if (is.na(str)){return("other")} else{
    if (as.character(str) != "null"){return(strsplit(as.character(str), "/")[[1]][1])}
    else {return("other")}
  }
}

sales_order_agg0$t1_category <- sapply(sales_order_agg0$category, getStemString)