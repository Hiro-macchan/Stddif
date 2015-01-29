#'Calculate Standardized Difference between two groups.(category)
#'
#'@param vec_tr treatment group category vector
#'@param vec_con control group category vector
#'@return standardized differnce
#'@export


fun_sef_cat <- function (vec_tr, vec_con) {
  mat_tr <- dummies::dummy(vec_tr)[,-ncol(dummies::dummy(vec_tr))]
  mat_con <- dummies::dummy(vec_con)[,-ncol(dummies::dummy(vec_con))]
  p_tr <- colMeans(mat_tr)
  p_con <- colMeans(mat_con)
  S <- (cov(mat_tr) + cov(mat_con))/2
  es <- sqrt(t(matrix(p_tr-p_con)) %*% MASS::ginv(S) %*% matrix(p_tr-p_con))
  return(es[,1])
}

#'Calculate Standardized Difference between two groups.(numeric)
#'
#'@param vec_tr treatment group numeric vector
#'@param vec_con control group numeric vector
#'@return standardized differnce
#'@export


fun_sef_num <- function (vec_tr, vec_con) {
  e_tr <- mean(vec_tr)
  e_con <- mean(vec_con)
  S <- (var(vec_tr) + var(vec_con))/2
  es <- sqrt((e_tr - e_con)^2/S)
  return(es)
}

#'Calculate Standardized Difference between two groups.(category)
#'
#'@param input_data input dataframe with 2 columns(treatmend and responce)
#'@param treat_vec_name treatment columns name
#'@return standardized differnce
#'@export

fun_sef <- function (input_data,treat_vec_name) {
  if(ncol(input_data) != 2){
    print("ERROR::Invarid column name!")
  }else{
    input_data <- as.data.frame(input_data)
    treat_vec <- input_data[,treat_vec_name]
    if(length(unique(treat_vec)) < 2){
      print("ERROR::Invarid treatment category!")
    }else{
      vec <- input_data[,!(colnames(input_data) %in% treat_vec_name)]
      if(is.numeric(vec)){
        vec_tr <- vec[treat_vec == unique(treat_vec)[1]]
        vec_con <- vec[treat_vec == unique(treat_vec)[2]]
        es <- fun_sef_num(vec_tr, vec_con)
      }else{
        if(length(unique(vec)) > 2){
          vec_tr <- vec[treat_vec == unique(treat_vec)[1]]
          vec_con <- vec[treat_vec == unique(treat_vec)[2]]
          es <- fun_sef_cat(vec_tr, vec_con)  
        }else{
          vec <- dummies::dummy(vec)[,1]
          vec_tr <- vec[treat_vec == unique(treat_vec)[1]]
          vec_con <- vec[treat_vec == unique(treat_vec)[2]]
          es <- fun_sef_num(vec_tr, vec_con)
        }
      }
      return(es)
    }
  }
}

#'Calculate Standardized Difference between two groups with dataframe.
#'
#'@param input_data input dataframe contain treatment variable and responce variables
#'@param treat_vec_name treatment columns name
#'@return standardized differnce vector
#'@export


fun_sef_df <- function (input_data, treat_vec_name) {
  input_data_res <- input_data[,!(colnames(input_data) %in% treat_vec_name)]
  if(!is.data.frame(input_data_res)){
    es <- fun_sef(input_data,treat_vec_name)
    names(es) <-  colnames(input_data)[!(colnames(input_data) %in% treat_vec_name)]
    return_vec <- es
  }else{
    val_names_vec <- colnames(input_data_res)
    return_vec <- vector(length = length(val_names_vec))
    for(i in 1:length(val_names_vec)){
      val_neme <- val_names_vec[i]
      input_data_tar <- input_data[,c(treat_vec_name,val_neme)]
      es <- fun_sef(input_data_tar,treat_vec_name)
      return_vec[i] <- es
    }
    names(return_vec) <- val_names_vec
  }
  return(return_vec)
}

#fun_sef_df(input_data,treat_vec_name)
