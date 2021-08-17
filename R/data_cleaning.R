#'@title remove low counts
#'
#'@description remove departments with counts below 5
#'
#'@return department list (character)
#'

remove_low_counts <- function(departments) {
  
  dep_counts <- table(departments)
  filter_list <- names(dep_counts[dep_counts >= 5])
  
  filtered_departments <- departments[departments %in% filter_list]
  
  return(filtered_departments)
}