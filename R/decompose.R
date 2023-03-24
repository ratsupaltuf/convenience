#' Decomposition of Data using Between and Within Effects
#'
#' Decompose a dataset into between and within components of given variables based on ID column and year column.
#'
#' @param data Data to decompose (data.frame)
#' @param vars Variables to be decomposed (character vector)
#' @param idcol Column holding ID values (character)
#' @param yearcol Column holding year values (character)
#' @param suffix.within Suffix for within component names (character, default ".cwc")
#' @param suffix.between Suffix for between component names (character, default ".gm")
#' @param indicators Type of components to calculate (within, between, both, character, default "both")
#'
#' @return Original data with between and within components added (data.frame)
#'
#' @examples
#' decompose(mtcars, c("mpg", "wt"), "cyl", "vs", ".cwc", ".gm", "within")
#'
#' @importFrom purrr map2
#' @importFrom data.table setDT
#'
#' @export
decompose <- # function name

  function(data, # data to decompose
           vars, # variables to be decomposed
           idcol, # column holding ID values
           yearcol, # column holding year values
           suffix.within = ".cwc", # suffix for within component names
           suffix.between = ".gm", # suffix for between component names
           indicators = "both") { # type of components to calculate (within, between, both)


    selected_cols <- c(vars, idcol, yearcol) # selected columns to keep from data
    df_selected <- unique(data[, ..selected_cols]) # subset of data with only selected columns

    gms.names<- paste0(vars,suffix.between) # variable names for between components
    cwc.names<- paste0(vars,suffix.within) # variable names for within components

    if(!is.data.table(df_selected)) { # if subset is not a data table
      setDT(data, key=c(idcol,yearcol)) # convert data to data table
    }

    df_selected[, (gms.names):= lapply(.SD, mean, na.rm=T), .SDcols=vars, by=c(idcol)] # calculate between

    cwc.data<- map2(df_selected[,..vars], df_selected[, ..gms.names], \(x,y) x-y) %>%
      as.data.frame()

    names(cwc.data) <- cwc.names
    df_selected<- cbind(df_selected, cwc.data)

    selection <- names(df_selected)[!(names(df_selected) %in% vars)]
    if(indicators!="both") {
      if(indicators=="within") {
        selection <- names(df_selected)[!(names(df_selected) %in% c(vars, gms.names))]

      } else if(indicators=="between") {
        selection <- names(df_selected)[!(names(df_selected) %in% c(vars, cwc.names))]
      }

    }

    df_selected <-
      df_selected[, .SD, .SDcols = selection]
    merge(data, df_selected, all.x=T)
  }
