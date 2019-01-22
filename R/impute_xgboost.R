#' Fast imputation of missing values by extreme gradien boosting 
#' 
#' @importFrom stats var predict
#' @importFrom xgboost xgboost
#'
#' @description Uses the "xgboost" package to do fast missing value imputation by extreme gradien boosting.
#' Between the iterative model fitting, it offers the option of predictive mean matching. This firstly avoids imputation
#' with values not present in the original mat (like a value 0.3334 in a 0-1 coded variable). Secondly, predictive mean
#' matching tries to raise the variance in the resulting conditional distributions to a realistic level and, as such, 
#' allows to do multiple imputation when repeating the call to impute_xgboost(). The iterative chaining stops as soon as \code{max_iterations}
#' is reached or if the average out-of-bag estimate of performance stops improving. In the latter case, except for the first iteration,
#' the second last (i.e. best) imputed matrix is returned.
#' @param mat A \code{matrix} with missing values to impute.
#' @param max_iterations Maximum number of chaining iterations.
#' @param pmm_k Number of candidate non-missing values to sample from in the predictive mean matching step. 0 to avoid this step.
#' @param seed Integer seed to initialize the random generator.
#' @param verbose Controls how much info is printed to screen. 0 to print nothing. 1 (default) to print a "." per iteration and 
#'                standardized prediction error , 2 to print model convergences.
#' @param eta eta control the learning rate: scale the contribution of each tree by a factor of 0 < eta < 1 when it is added to the current approximation. Used to prevent overfitting by making the boosting process more conservative. Lower value for eta implies larger value for nrounds: low eta value means model more robust to overfitting but slower to compute. Default: 0.3
#' @param nrounds max number of boosting iterations.
#' @param max_depth maximum depth of a tree. Default: 6
#' @param objective specify the learning task and the corresponding learning objective, default 'reg:linear'
#' @param eval_metric evaluation metrics for validation data. Default 'rmse'
#' @param ... Arguments passed to \code{xgboost}. 
#' @return An imputed \code{matrix}.
#' @export
#'
#' @examples
#' mat = as.matrix(iris[,1:4]) 
#' mis_mat = generate_na(mat , 0.3)
#' imp_mat = impute_xgboost(mis_mat)
#' 


impute_xgboost = function(mat, 
                          max_iterations = 10L , 
                          seed = NULL , 
                          verbose = 1 , 
                          returnOOB = FALSE , 
                          pmm_k = 0 , 
                          nrounds = 40 ,
                          eta = 0.4 , 
                          max_depth = 6 , 
                          objective  = 'reg:linear' , 
                          eval_metric = 'rmse' , 
                          ...) {
  
  stopifnot(verbose %in% 0:3 )
  
  if (verbose > 0) {
    cat("\nMissing value imputation by extreme gradien boosting\n")
  }
  
  stopifnot(
    is.matrix(mat), dim(mat) >= 1L, 
            is.numeric(max_iterations), length(max_iterations) == 1L, max_iterations >= 1L,
            is.numeric(pmm_k), length(pmm_k) == 1L, pmm_k >= 0L
    )
  
  if (!is.null(seed)) {
    set.seed(seed)
  }  
  
  all_missing = function(vec){
    all(is.na(vec))
  }
  
  imputation_variables = apply(mat, 2, function(x) !all_missing(x) )
  
  if (verbose > 0 && length(imputation_variables) < ncol(mat)) {
    cat("\n  Variables ignored because all values missing: ")
    cat(setdiff(names(mat), imputation_variables), sep = ", ")
  }
  
  stopifnot(length(imputation_variables) > 1L)
  missingness_indicator_matrix = is.na(mat[, imputation_variables, drop = FALSE])
  count_sequence = sort(colSums(missingness_indicator_matrix))
  visit_sequence = names(count_sequence)[count_sequence > 0]
  
  if (!length(visit_sequence)) {
    return(mat)
  }
  
  verbose_digits = 4  # prediction of OOB prediction errors (if verbose = 2)
  iteration_count = 1L             # iterator
  prediction_error = rep(1, length(visit_sequence))
  names(prediction_error) = visit_sequence
  prediction_error_decreased = TRUE # prediction_error on OOB prediction error to keep iterating
  completed = setdiff(imputation_variables, visit_sequence)
  
  if (verbose >= 2) {
    cat("\n", abbreviate(visit_sequence, minlength = verbose_digits + 2), sep = "\t")
  }
  
  while (prediction_error_decreased && iteration_count <= max_iterations) {
    
    if (verbose > 0) { cat("\niter ", iteration_count , ":\t", sep = "") }
    
    last_mat = mat
    last_prediction_error = prediction_error
    
    for( vector_to_impute in visit_sequence ) {
      
      vector_to_impute_index = which(colnames(mat) == vector_to_impute)
      vector_to_impute_is_na = missingness_indicator_matrix[, vector_to_impute]
      
      if (length(completed) == 0L) {
        
        mat[[vector_to_impute]] = impute_univariate(mat[[vector_to_impute]])
        
      } else {
        
        fit = xgboost(
          data = mat[!vector_to_impute_is_na , -vector_to_impute_index] , 
          label = mat[!vector_to_impute_is_na , vector_to_impute_index] , 
          eta = eta , 
          max_depth = max_depth , 
          objective = objective , 
          eval_metric = eval_metric ,
          nrounds = nrounds ,
          verbose = max( 0 , verbose - 1 )  ,
          ... 
          )
        
        this_prediction_error = fit$evaluation_log[nrounds]$train_rmse
        pred = predict( fit , mat[vector_to_impute_is_na, -vector_to_impute_index])
        
        if(pmm_k  > 0){
          pred = pmm(xtrain = predict( fit , mat[ !vector_to_impute_is_na , -vector_to_impute_index]) , 
                     xtest = pred, 
                     ytrain = mat[,vector_to_impute][!vector_to_impute_is_na], 
                     k = pmm_k)
        } 
        
        mat[vector_to_impute_is_na, vector_to_impute] = pred
        prediction_error[[vector_to_impute]] = this_prediction_error / var(mat[,vector_to_impute][!vector_to_impute_is_na])
        
        if (is.nan(prediction_error[[vector_to_impute]])) {
          prediction_error[[vector_to_impute]] = 0
        }
      }
      
      completed = union(completed, vector_to_impute)
      
      if (verbose == 1) {
        cat(".")
      } else if (verbose >= 2) {
        cat(format(round(prediction_error[[vector_to_impute]], verbose_digits), nsmall = verbose_digits), "\t")
      }
    }
    
    if(verbose > 0) {
      cat('\n')
      print(prediction_error)
    }
    
    iteration_count= iteration_count + 1L
    prediction_error_decreased = mean(prediction_error) < mean(last_prediction_error)
  }
  
  if (verbose > 0) {
    cat("\n")
  }
  
  if (iteration_count== 2L || (iteration_count== max_iterations && prediction_error_decreased)) {
    return(mat)
  } 
  
  last_mat
}
