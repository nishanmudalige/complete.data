missing.total = function(df){
  sum(is.na(df))
}

complete.total = function(df){
  sum(!is.na(df))
}

missing.and.complete = function(df, type = "number"){
  
  if(!is.data.frame(df)){
    warning("Object entered must be a data frame")
    return(invisible())
  }
  
  result = cbind( apply(df, 2, complete.total),
                  apply(df, 2, missing.total))
  
  if(type == "number"){
    
    result = cbind(result, nrow(df))
    colnames(result) = c("Complete", "Missing", "Total")
    return(result)
    
  } else if (type == "percent") {
    
    result[,1:2] = result[,1:2]/nrow(df)
    result = cbind(result, nrow(df))
    
    colnames(result) = c("Complete %", "Missing %", "Total")
    return(result)
    
  } else if (type == "both") {
    
    complete.p = result[,1]/nrow(df)
    missing.p = result[,2]/nrow(df)
    
    result = cbind(result, complete.p, missing.p, nrow(df))
    
    colnames(result) = c("Complete", "Missing", 
                         "Complete %", "Missing %", "Total")
    return(result)
    
  } else {
    warning("'Type' must be either 'number', 'percent' ot 'both'")
    return(invisible())
  }
}