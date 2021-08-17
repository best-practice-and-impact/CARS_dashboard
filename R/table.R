#'@title create table
#'
#'@description creates a frequency table from input column
#'
#'@param data column 
#'
#'@return table 
#'@export

create_table <- function(data){
  
  freq_table <- data.frame(table(data$Q1..Which.department.do.you.primarily.work.in.))
  
  colnames(freq_table) <- c("Department", "Count")
  
  return(freq_table)
}
