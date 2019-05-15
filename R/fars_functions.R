#' Import into R ".csv.bz2" Data from US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System
#'
#' The function loads into R bz2-compressed CSV for the year of interest. Canonical
#' filename can be conveniently constructed with the function \code{\link{make_filename}}.
#'
#' @param filename name (character string) of the file to be imported
#'
#' @return The function returns a tibble (a tidyverse version of the dataframe)
#' with the data of interest.
#'
#' @details The function uses \code{\link{readr::read_csv}} to import the file
#' and \code{\link{dplyr::tbl_df}} to construct the dataframe. It will not show
#' how it formatted each column nor its loading progress. If it fails to find a
#' specified file, it will throw an error, notifying the user that the file does
#' not exist. The function assumes that the file is in the current working directory.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#' fars_read(make_filename(2013))
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Infer Filename From Year
#'
#' The function generates the canonical filename for the year of interest.
#'
#' @param year A string of four numbers that denote the year of interest.
#'
#' @return This function returns a character string which corresponds to the
#' canonical name of a file that contains bz2-compressed comma-separated values
#' for the year of interest.
#'
#' @note Entered year is first coerced into integer and only then used to generate
#' a filename. Entering non-numbers will coerce entered string into NA.
#'
#' @examples
#' make_filename(2013)
#' make_filename("2013")
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Import Into R Several Years of Data, Retain Only Month and Year
#'
#' This function imports several years of data and retains from it only the month
#' and year columns.
#'
#' @param years an integer vector specifying which years should be imported into R
#'
#' @return The function returns a list of tibbles. Each of the tibbles has only
#' two columns, MONTH and year.
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @details Internally, the function uses \code{\link{make_filename}} function to
#' generate the filenames and then \code{\link{fars_read}} function to import the data.
#' If the data for the specified year are not available, the function will create a
#' list entry with NULL value.
#'
#' @examples
#' fars_read_years(c(2013, 2014))
#' fars_read_years(c(2013, 2017, 2014))
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarise Number of Accidents Per Months
#'
#' The function counts the number of accidents that occurred in each month of
#' the specified years.
#'
#' @inheritParams fars_read_years
#'
#' @return The function returns a tibble with the number of accidents that occurred
#' in a specific month of a specified year. The months (1-12) are shown in
#' rows and the years in columns.
#'
#' @details Internally, the function builds on the function \code{\link{fars_read_years}}
#' and summarises the data by counting the number of month-year occurrences.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom magrittr %>%
#' @importFrom tidyr spread
#'
#' @examples
#' fars_summarize_years(c(2013, 2014))
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plot Location of Motor Vehicles Traffic Crashes
#'
#' The function plots a map that shows where in the specified state the accidents
#' occurred during the specified year.
#'
#' @param state.num State number
#' @param year Year of interest
#'
#' @return The function plots a map of the state with points representing
#' locations at which the accidents occurred.
#'
#' @details Internally, the function uses \code{\link{make_filename}} and
#' \code{\link{fars_read}} functions to load the data, filters by state and plots
#' accidents using the \code{\link{map}} function of the maps package. Only one state
#' and one year can be specified in the function call. If the user enters wrong
#' state number, the function will abort, throwing out an error. If the user filters
#' out all of the data, the function will print an appropriate message.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' fars_map_state(10, 2014)
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
