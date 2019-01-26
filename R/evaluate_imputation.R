#' Imputation Evaluation
#'
#' @description This function calculates for evaluation metrics between matrix with no missigness and matrix with imputed values. Four metrics will be returned: root mean square error, mean absolute error, coeficient of variation of root mean square error and mean absolute percentage error for each column in the matrices.
#'
#' @param original_matrix Original matrix
#' @param imputed_matrix Matrix for which missigness has been for example artificially created and then imputed
#'
#' @return matrix containing rmse, mae, rmse_cv and mape for each column
#' @export
#'
#' @examples
#' mat = as.matrix(iris[,1:4])
#' mis_mat = generate_na(mat , 0.3)
#' imp_mat = impute_xgboost(mis_mat)
#' evaluate_imputation(mat, imp_mat)
#'


evaluate_imputation = function(original_matrix , imputed_matrix){

  rmse = function(actual_values , predicted_values) {
    sqrt(mean( (actual_values - predicted_values) ^2))
  }

  mae = function(actual_values , predicted_values){
    mean(abs(actual_values - predicted_values))
  }

  mape = function(actual_values , predicted_values){
    mean( abs((actual_values-predicted_values)/actual_values) * 100 )
  }

  return_diagnostics = function(actual_values , predicted_values){

    this_rmse = rmse(actual_values , predicted_values)
    this_mae = mae(actual_values , predicted_values)
    this_rmse_cv = this_rmse / mean(abs(actual_values))
    this_mape = mape(actual_values , predicted_values)

    return(list(
      rmse = this_rmse , mae = this_mae , rmse_cv = this_rmse_cv , mape = this_mape
    ))
  }

  difference_indicator_matrix = (original_matrix - imputed_matrix) != 0

  diagnostics_matrix = sapply(1:ncol(difference_indicator_matrix), function(column_index){
    difference_index = which(difference_indicator_matrix[,column_index])
    return_diagnostics( original_matrix[difference_index,column_index] , imputed_matrix[difference_index,column_index]  )
  })
  diagnostics_matrix = t(as.matrix(diagnostics_matrix))
  rownames(diagnostics_matrix) = colnames(original_matrix)
  return(diagnostics_matrix)
}
