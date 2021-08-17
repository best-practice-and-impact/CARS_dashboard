#'@title Plot frequency graph
#'
#'@description Produce bar chart (plotly) for single factor frequency data. 
#'
#'@param table Frequency table (data frame). 2 columns - cateogry names and frequencies. 
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param bar_colour Colour name. Defaults to blue
#'@param n sample size
#'@param font_size minimum font size for the plot (numeric).
#'@param max_lines maximum number of lines. Int, defaults to 2/ See carsurvey::break_q_names()
#'@param ... additional plotly_ly arguments
#'
#'@return bar chart
#'
#'@export

plot_freqs <- function(table, xlab, ylab, bar_colour = "#004556", n, font_size = 12, break_q_names_col = NULL, max_lines = 2, ...) {
  
  # Validate table
  if (!is.data.frame(table)) {
    stop("Unexpected input - table is not a data.frame.")
  } else if (ncol(table) != 2) {
    stop("Unexpected input - table does not contain two columns.")
  } else if (!is.numeric(table[[2]])) {
    stop("Unexpected input - table column 2 is not numeric.")
  }
  
  # Validate labels
  if (!is.character(xlab) | !is.character(ylab) | length(xlab) > 1 | length(ylab) > 1) {
    stop("Unexpected input - labels should be single character strings.")
  }
  
  # Validate n
  if ((!is.numeric(n) & !is.character(n)) | length(n) > 1) {
    stop("Unexpected input - n is not a single number or string")
  }
  
  # Validate font size
  if (!is.numeric(font_size)) {
    stop("Unexpected input - font_size is not numeric.")
  }
  
  # Apply break_q_names to a column
  if(!is.null(break_q_names_col)) {
    # Coerce to character type
    table[[break_q_names_col]] <- as.character(table[[break_q_names_col]])
    
    table[[break_q_names_col]] <- break_q_names(table[[break_q_names_col]], max_lines = max_lines)
    
    table[[break_q_names_col]] <- factor(table[[break_q_names_col]], levels = table[[break_q_names_col]])
  }
  
  x <- list(
    title = xlab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )
  
  y <- list(
    title = ylab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )
  
  table <- dplyr::arrange(table, dplyr::desc(table[,1]))
  table[,1] <- factor(table[,1], levels = table[,1])
  x_vals <- table[[2]]
  y_vals <- table[[1]]
  x_axis <- y
  y_axis <- x
  
  ylab <- y_axis$title
  y_axis$title <- "" # Y axis title is created as a caption instead
  
  line <- list(
    type = "line",
    line = list(color = "#ff6900"), 
    x0 = 20, 
    y0 = -0.5,
    x1 = 20, 
    y1 = nrow(table) - 0.5
  )
  
  fig <- plotly::plot_ly(
    x = x_vals,
    y = y_vals,
    marker = list(color = bar_colour),
    type = "bar",
    orientation = "h",
    ...
  )
  
  fig <- plotly::config(fig, displayModeBar = F)
  fig <- plotly::layout(fig,
                        xaxis = x_axis, 
                        yaxis = y_axis,
                        margin = list(b = 100),
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                        annotations = list(x = 1, y = 0, text = paste0("Sample size = ", n), 
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size),
                        shapes = line)
  )
  
  return(fig)
  
}

#'@title add line break to question names 
#'
#'@description add line breaks to question names - split into two lines.
#'Line lengths are determined by the longest question.
#'
#'@param q_names question names - character
#'@param max_lines maximum number of lines. Int, defaults to 2
#'@export

break_q_names <- function(q_names, max_lines = 2) {
  
  # Validate input
  if (typeof(q_names) != "character") {
    stop("Unexpected input - q_names is not a character vector")
  } else if (!is.numeric(max_lines) | length(max_lines) > 1) {
    stop("Unexpected input - max_lines is not a single integer")
  }
  
  max_length = max(nchar(q_names)) / (max_lines-1)
  wrap_loc = ceiling(max_length / (2))
  
  regex <- paste0("(.{", wrap_loc, "}.?) (.?)")
  wrapped_strings <- gsub(regex , "\\1<br>\\2", q_names)
  
  return(wrapped_strings)
}