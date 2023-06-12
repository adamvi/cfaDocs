#' @title Get current counter
#' @description Returns the current counter number associated with a
#' table, figure, or equation
#' 
#' @param type Character string indicating either a \sQuote{table}, 
#'   \sQuote{figure}, or \sQuote{equation}.
#' @return Returns a (numeric) counter for the specified type.
#' @examples 
#' \dontrun{
#'   if(interactive()){
#'     getCounter("figure")
#'   }
#' }
#' @rdname getCounter
#' @export

getCounter <- function(type = c("table", "figure", "equation")) {
    if (tolower(type) == "table") return(getOption("table_number"))
    if (tolower(type) == "figure") return(getOption("fig_caption_no"))
    if (tolower(type) == "equation") return(getOption("equation_counter"))
}


#' @title Set counter numbers
#' @description Sets the initial table, figure and equation counter numbers.
#'  Can also be used to re-set counters (e.g. for an appendix). The function
#'  relies on \code{options("table_number")}, \code{options("fig_caption_no")}
#'  and \code{options("equation_counter")} as counters.
#' @param tbl.counter Numeric. Number to set to the present table number.
#'   Default is 0, meaning the next (first) table will be number 1.
#' @param fig.counter Numeric. Number to set to the present figure number.
#'   Default is 0, meaning the next (first) figure will be number 1.
#' @param eqn.counter Numeric. Number to set to the present equation number.
#'   Default is 0, meaning the next (first) equation will be number 1.
#' @return NULL. Sets options
#' @details DETAILS
#' @examples 
#' \dontrun{
#'   if(interactive()){
#'    setCounters()
#'   }
#' }
#' @rdname setCounters
#' @export

setCounters <- function(tbl.counter=0, fig.counter=0, eqn.counter=0) {
  options("table_number" = tbl.counter)
  options("fig_caption_no" = fig.counter)
  options("equation_counter" = eqn.counter)
}


#' @title Get a table number for reference
#' @description The function relies on \code{options("table_number")} in 
#'   order to keep track of the current number.
#' @param advance.counter Numeric. Number to be added to the present table
#'   number. Default is 0, giving the present table.
#' @return Returns a prior, current or subsequent table number.
#' @details Typically used for inline R code within a .Rmd document
#' @examples 
#' \dontrun{
#'  if(interactive()){
#'    options(table_number=1)
#'    tblNum()
#'  }
#' }
#' @rdname tblNum
#' @export

tblNum <- function(advance.counter = 0) {
  if (!is.null(getOption("table_num_str"))) {
    return(sprintf(getOption("table_num_str"), getOption("table_number")+advance.counter))
  }
  return(getOption("table_number")+advance.counter)
}

#' @title Get a figure number for reference
#' @description The function relies on \code{options("fig_caption_no")}
#'   in order to keep track of the current number.
#' @param advance.counter Numeric.  Number to be added to the present figure
#'   number.  Default is 0, giving the number for the last figure placed/plotted.
#' @return Returns a prior, current or subsequent figure number.
#' @details Typically used for inline R code within a .Rmd document
#' @examples 
#' \dontrun{
#'  if(interactive()){
#'   options(fig_caption_no=1)
#'   figNum()
#'  }
#' }
#' @rdname figNum
#' @export

figNum <- function(advance.counter = 0) {
  if (!is.null(getOption("fig_num_str"))) {
    return(sprintf(getOption("fig_num_str"), getOption("fig_caption_no")+advance.counter))
  }
  getOption("fig_caption_no")+advance.counter
}


#' @title Increase the table number counter
#' @description Bump the table number by one.  The function relies on
#'   \code{options("table_number")} in order to keep track of the last number.
#' @param advance.counter Numeric. Number to be added to the present table
#'   number. Default is 1, advancing table count by 1.
#' @return Next table number
#' @examples 
#' \dontrun{
#'  if(interactive()){
#'   options(table_number=1)
#'   getCounter("table")
#'   tblNumIncrement()
#'   getCounter("table")
#'  }
#' }
#' @rdname tblNumIncrement
#' @export

tblNumIncrement <- function(advance.counter=1) {
  options("table_number" = getCounter("table")+advance.counter)
  getCounter("table")
}


#' @title Increase the figure number counter
#' @description Bump the figure number by one.  The function relies on
#'   \code{options("fig_caption_no")} in order to keep track of the last number.
#' @param advance.counter Numeric. Number to be added to the present figure
#'   number. Default is 1, advancing figure count by 1.
#' @return Next figure number
#' @examples 
#' \dontrun{
#'  if(interactive()){
#'    options(fig_caption_no=1)
#'    getCounter("figure")
#'    figNumIncrement()
#'    getCounter("figure")
#'  }
#' }
#' @rdname figNumIncrement
#' @export

figNumIncrement <- function(advance.counter=1) {
  options("fig_caption_no" = getCounter("figure")+advance.counter)
  getCounter("figure")
}


#' @title Formated caption with table number
#' @description Returns a formatted markdown text string for the caption,
#'   which includes the table number. The function relies on
#'   \code{options("table_number")} in order to keep track of the current
#'   number.  Also increases the table number BEFORE processing the caption
#'   using \link{tblNumIncrement}.
#' @param caption.text Text string to be used for caption.  If NULL, no
#'   caption is returned, but the table counter is advanced.
#'   Default: NULL
#' @param advance.counter Numeric. Number to be added to the present table
#'   number.  Default is 1, advancing table count by 1.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#'  if(interactive()){
#'    options(table_number=1)
#'    tblCap("My table caption is great.")
#'    # Returns '**Table 2:**  My table caption is great.'
#'
#'    getCounter("table")
#'    getOption("table_number")
#'    # Returns '2'
#'  }
#' }
#' @rdname tblCap
#' @export

tblCap <- function(caption.text=NULL, advance.counter=1) {
  if (as.integer(advance.counter) != 0L) tblNumIncrement(advance.counter=advance.counter)
  if (is.null(caption.text))  return(NULL)
  if (!is.null(getOption("table_counter_str"))) {
    tmp.caption <- paste(getOption("table_counter_str"), caption.text)
    tmp.caption <- gsub("[%]s", getOption("table_number"), tmp.caption)
    return(tmp.caption)
  }
  return(paste0("**Table ", getOption("table_number"), ":** ", caption.text))
}


#' @title Increase the equation number counter
#' @description Bump the equation number by one.  The function relies on
#'   \code{options("equation_counter")} in order to keep track of the last number.
#' @param advance.counter Numeric. Number to be added to the present equation
#'   number. Default is 1, advancing equation count by 1.
#' @return Next equation number
#' @examples 
#' \dontrun{
#'  if(interactive()){
#'    options(equation_counter=1)
#'    getCounter("equation")
#'    eqnNumIncrement()
#'    getCounter("equation")
#'  }
#' }
#' @rdname eqnNumIncrement
#' @export

eqnNumIncrement <- function(advance.counter = 1) {
  options("equation_counter" = getCounter("equation")+advance.counter)
  getCounter("equation")
}


#' @title Get the equation number for reference
#' @description The function relies on \code{options("equation_counter")} in
#'   order to keep track of the current number.
#' @param advance.counter Numeric. Number to be added to the present table number.
#'   Default is 0, giving the present equation
#' @param eqn.name Character. Name of equation if needed for reference in text.
#'   Default: 't1'
#' @param em.space Numeric. Space before equation when rendered in PDF. Default: 150
#' @return Returns a prior, current or subsequent equation number.
#' @details Typically used for inline R code within a .Rmd document
#' @examples 
#' \dontrun{
#'  if(interactive()){
#'   options(equation_counter=1)
#'   eqnNum()
#'  }
#' }
#' @rdname eqnNum
#' @export

eqnNum <- function(advance.counter = 0, eqn.name = "t1", em.space = 150) {
  pos <- 1
  if (!is.null(eqn.name)) {
    assign(eqn.name, getOption('equation_counter')+1, envir = as.environment(pos))
  } else getOption('equation_counter')+1
  return(cat('\\hspace{', em.space, 'em} \\text{(', getOption('equation_counter')+advance.counter, ')}', sep=""))
}
