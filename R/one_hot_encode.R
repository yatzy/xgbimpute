#' One Hot Encode
#'
#' @description One hot encodes a data.frame or data.table to a matrix in efficient manner. Function also adds a couple of package specific attributes to returned matrix so one can pass the result throuh impute_xgboost-function to one_hot_decode-function which then knows how to reverse the one hot encoding. At this point only supported categorical column types are factor and character.
#'
#' @param dt a data.table or data.frame wanted to one hot encode
#'
#' @import data.table
#'
#' @return A matrix created from one hot encoded data.frame or data.table with special attributes for one_hot_decode-function
#' @export
#'
#' @examples
#'
#' mat = one_hot_encode(iris)
#' mis_mat = generate_na(mat)
#' imp_mat = impute_xgboost(mis_mat)
#'
#'

one_hot_encode = function( dt ){

  ########### sanity checks


  if( 'data.table' %ni% class( dt )  ){
    setDT(copy(dt))
  }

  original_class = class(dt)
  original_column_names = colnames(dt)
  original_column_classes = lapply(dt, class)

  categorical_classes = c('factor' , 'character')
  numeric_classes = c('numeric' , 'integer')
  valid_classes = c(categorical_classes , numeric_classes )


  is_column_class_supported = sapply(original_column_classes, function(column_class){
    any(valid_classes %in% column_class ) & ( 'ordered' %ni% column_class )
  })

  if(!all(is_column_class_supported)){
    stop('Unsupported column class in data.table. Valid classes are (', paste(valid_classes , collapse = ', '), '). Factors may not be ordered.' )
  }

  character_columns = sapply(original_column_classes, function( column_class ){
    'character' %in% column_class
  })

  if(any(character_columns)){
    dt[ , (character_columns) := lapply(.SD, as.factor), .SDcols = character_columns]
  }

  factor_columns = colnames( dt )[ which(sapply(dt, is.factor)) ]

  ########### one hot encode

  dt = as.matrix( one_hot_encode_raw(dt , factor_columns ) )
  attr(dt , 'xgbimpute_original_column_names') = original_column_names
  attr(dt , 'xgbimpute_original_column_classes') = unlist( unname(original_column_classes) )
  attr(dt , 'xgbimpute_original_class') = original_class

  return( dt )

}


