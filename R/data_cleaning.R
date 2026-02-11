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


#' @title Filter data to divisons
#'
#' @description Take full data set and filter to only ONS divsions
#'
#' @param data full dataset
#'
#' @return filtered CARS dataset
#'
#' @export

filter_divisons <- function(data) {
  
  data$Q3..Which.Civil.Service.department.do.you.primarily.work.in.[data$X== "National Records of Scotland"] <- "National Records of Scotland"
  departments <- data$Q3..Which.Civil.Service.department.do.you.primarily.work.in.[3:length(data$Q3..Which.Civil.Service.department.do.you.primarily.work.in.)]
  
  return(departments)
}


