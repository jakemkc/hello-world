

IAMR71FL_dta <- readRDS("/home/st320/IAMR71FL_dta.RDS")



dataPreparation <- function (dat, threshold) {
  
  cat(sprintf("Number of columns: %f\n", ncol(dat)))
  cat(sprintf("Threshold for keeping columns: %f\n", threshold))
  
  # get a count of values in each column
  t <- as.data.frame(apply(is.na(dat), 2, sum))
  t$variable <- rownames (t)
  colnames(t) <- c("value", "variable")
  
  # get the columns that have all values missing
  # for reporting only
  t_allMiss <- t %>% dplyr::filter(value == nrow(dat))
  cat(sprintf("%f columns have all values missing\n",nrow(t_allMiss)))
  
  # filter out the variables where threshold amount of data is missing
  # if threshold = 1 - then if all data is missing
  # if threshold = .9 - then if 90% of the data is missing
  print(t$variable)
  
  t <- t %>% dplyr::filter(value != threshold*nrow(dat))
  dat <- dat %>%
    dplyr::select(one_of(t$variable))
  
  # identify duplicate columns
  # one possible method- identify columns that have a perfect correlation
  # can only be done for numeric columns
  dat.num <- dat %>%
    dplyr::select(which(sapply(., is.numeric)))
  #               purrr::keep(is.numeric)
  rownames(dat.num) <- dat$mcaseid
  dat.num$mcaseid <- dat$mcaseid
  
  ## save the char data
  drops <- colnames(dat.num)
  mainCol <- colnames(dat)
  dat.char <- dat %>%
    dplyr::select(-which(colnames(dat) %in% as.vector(drops)), mcaseid)
  
  # remove columns with zero sd
  # they don't add any value
  t <- which(sapply(dat.num, sd) == 0, arr.ind = TRUE)
  dat.num <- dat.num[, -t]
  
  # for the remainder numeric columns
  # perform correlation (using pairwise.complete)
  dat.cor <- dat.num %>%
    dplyr::select(-mcaseid)
  # do we need pairwise complete obs here???
  cor.val <- cor(dat.cor)
  
  cor.val[upper.tri(cor.val)] <- 0
  diag(cor.val) <- 0
  a <- apply(cor.val,2,function(x) any(x > 0.99))
  
  cat(sprintf("Columns with high correlation: %s\n", as.vector(names(a)[which(a==TRUE)]) ))
  #cat(print("These columns have not been removed! Double check and execute: dat.t <- dat.num[,-which(a == TRUE)] \n"))
  
  # uncomment if want to remove highly correlated variables
  dat.t <- dat.num[,-which(a == TRUE)]
  
  ## join this back with the other data
  dat.new <- dplyr::inner_join(dat.char, dat.num, by="mcaseid")
  
  cat(sprintf("Returning. New dataset has Num Rows: %f, Num Cols: %f\n", nrow(dat.new), ncol(dat.new)))
  
  return(dat.new)
}

#IAMR71FL_dta_clean <- dataPreparation(IAMR71FL_dta, threshold = 0.9)

IAMR71FL_dta_clean_remove_high_corr <- dataPreparation(IAMR71FL_dta, threshold = 0.9)


saveRDS(IAMR71FL_dta_clean_remove_high_corr, "/home/st320/IAMR71FL_dta_clean_remove_high_corr.RDS")