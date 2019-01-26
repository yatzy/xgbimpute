
one_hot_encode_raw = function(dt, cols ){

  # R CMD check
  .I = NULL
  ID = NULL
  setDT(dt)

  tempDT = dt[, cols, with=FALSE]
  tempDT[, ID := .I]
  setcolorder(tempDT, unique(c("ID", colnames(tempDT))))
  for(col in cols){
    set(tempDT, j=col, value=factor(paste(col, tempDT[[col]], sep="_"), levels=paste(col, levels(tempDT[[col]]), sep="_")))
  }

  newCols = dcast( melt(tempDT, id = 'ID', value.factor = T) , ID ~ value, drop = T, fun.aggregate = length )

  # Combine binarized columns with the original dataset
  newCols = cbind(dt, newCols[, !"ID"])
  newCols = newCols[, !cols, with=FALSE]

  return(newCols)
}
