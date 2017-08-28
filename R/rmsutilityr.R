## library(stringi, quietly = TRUE)
## library(lubridate, quietly = TRUE)
## library(xtable, quietly = TRUE)
## suppressPackageStartupMessages(library(XLConnect, quietly = TRUE))
#' Has the sideeffect of writing out a LaTeX title page compatible with memoir.
#'
#'#' Written by Josh O'Brien on stackoverflow on May 13 '15 at 21:42
#' @param path character vector of length one having the directory path to
#' where new version of the custom package exists.
#' @export
get_deps <- function(path) {
  dcf <- read.dcf(file.path(path, "DESCRIPTION"))
  jj <- intersect(c("Depends", "Imports", "Suggests"), colnames(dcf))
  val <- unlist(strsplit(dcf[, jj], ","), use.names = FALSE)
  val <- gsub("\\s.*", "", trimws(val))
  val[val != "R"]
}
#' Make package dependency list
#'
#' Gathers the dependencies from each source package and combines the lists
#' without duplication.
#'
#' @param source_names character vector containing the names of the custom
#' packages to be installed from source.
#' @param path character vector of length one having the path to R's
#' \code{library} directory, which contains the packages being updated from
#' source. This may or may or may not be the same as the system wide
#' \code{library} directory. It could be a user directory.
#' @export
make_package_dependency_list <- function(source_names, path) {
  dependencies <- character(0)
  for (name in source_names) {
    dependencies <- unique(c(dependencies, get_deps(paste0(path, name))))
  }
  dependencies
}
#' Remove these strings
#'
#' Modified from rmsutilityr::remove_strings() by R. Mark Sharp. The
#' modification was to remove a package dependency using the standard
#' relational opporator "==" instead of stri_detect_regex().
#' @param .str character vector that have tokens removed that match
#' tokens within the \code{expunge} vector.
#' @param expunge character vector of tokens to be removed from the
#' \code{.str} vector if present.
#' @param ignore_case boolean that determines whether or not case is ignored.
#' Defaults to FALSE.
#' @export
remove_these_str <- function(.str, expunge, ignore_case = FALSE) {
  if (ignore_case) {
    tmp_str <- tolower(.str)
    tmp_expunge <- tolower(expunge)
  }
  else {
    tmp_str <- .str
    tmp_expunge <- expunge
  }
  keep <- rep(TRUE, length(.str))
  for (exp_str in tmp_expunge) {
    keep <- !tmp_str == exp_str & keep
  }
  .str[keep]
}
#' Install R package from package source
#'
#' Takes a list of packages (\code{source_names}) and the path
#' (\code{source_path}) to their common location and
#' installs them into the \code{install_path} directory.
#'
#' @param source_names character vector having one source package name per
#' cell.
#' @param source_path character vector of length one having the directory
#' path of where the package sources (*.tar.gz) reside.
#' @param install_path character vector of length one having the directory
#' path of where packages are to be installed.
#' @export
install_from_source <- function(source_names, source_path, install_path) {
  for (source_name in source_names) {
    source <- max(list.files(path = source_path,
                             pattern = paste0(source_name, ".*.tar.gz")))
    install.packages(paste0(source_path, source), type = "source", repos = NULL,
                     lib = install_path)
  }
}
#' Assumes the presence of the LaTeX package \code{titling} with the new command
#' \code{\\subtitle}.
#' \\usepackage{titling}
#' \\newcommand{\\subtitle}[1]{%
#'   \\posttitle{%
#'     \\par\\end{center}
#'     \\begin{center}\\large#1\\end{center}
#'     \\vskip0.5em}%
#' }
#' Capitalization is not modified by this function.
#'
#' @param title_str character vector of length 1 having the text of the title
#' @param sub_title_str character vector of length 1 having the text of the
#' subtitle
#' @param author_str character vector of length 1 having the text describing the
#' author(s). Has default.
#' @export
make_knitr_header <- function(title_str = "", sub_title_str = "",
                              author_str = "Data Science Core") {
  cat(stri_c(
    "\\title{", title_str, "}\n",
    "\\subtitle{", sub_title_str, "}\n",
    "\\author{", author_str, "}\n",
    "\\date{\\today}\n",
    "\\begin{document}\n",
    "\\maketitle\n"))
}
#' Returns a string formated as a comment for HTML or LaTeX depending on
#' output format.
#'
#' @param  x character string with comment.
#' @param color color that font is to be
#' @export
colfmt = function(x, color = "red") {
  outputFormat = knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (is.null(outputFormat))
    outputFormat <- "latex"
  if (outputFormat == "latex")
    paste0("\\textcolor{", color, "}{\\emph{", x,"}}")
  else if (outputFormat == 'html')
    paste0("<font color='", color, "'>_", x, "_</font>")
  else
    x
}
#' Wrapper function around \code{colfmt()} to make the font red.
#'
#' In addition to adding color, underscores are prepended and
#' append for HTML and \code{\\emph{}} is used for LaTeX
#'
#' @param x character string with comment
#' @export
rcomment <- function(x) {
  colfmt(x, color = "red")
}
#' Wrapper function around \code{colfmt()} to make the font blue.
#'
#' In addition to adding color, underscores are prepended and
#' append for HTML and \code{\\emph{}} is used for LaTeX
#'
#' @param x character string with comment
#' @export
bcomment <- function(x) {
  colfmt(x, color = "blue")
}
#' Returns TRUE if the string is a valid date.
#'
#' @examples
#' is_valid_date_str(c("13-21-1995", "20-13-98", "5-28-1014",
#'   "1-21-15", "2-13-2098", "25-28-2014"), format = "%m-%d-%y")
#'
#' @param date_str character vector with 0 or more dates
#' @param format character vector of lenght one having the date format
#' descriptor.
#' @export
is_valid_date_str <- function(date_str, format = "%d-%m-%Y %H:%M:%S") {
  result <- as.logical(sapply(date_str, function(s) {
    d <- try(as.Date(s, format = format))
    !(class(d) == "try-error" || is.na(d))
  }))
  result
}

#' Returns a character vector with an file name having the date prepended.
#'
#' @param filename character vector with name to use in file name
#' @import lubridate
#' @export
get_dated_filename <- function(filename) {
  date_stamp <- stri_replace_all_fixed(
    stri_replace_all_fixed(as.character(now()), " ", "_"), ":", "_")
  stri_c(date_stamp, "_", filename)
}
#' Returns a character vector with an Excel complient name having the date prepended.
#'
#' @param data_set_name character vector with name to use in file name
#' @export
get_dated_excel_name <- function(data_set_name) {
  get_dated_filename(stri_c(data_set_name, ".xlsx"))
}
#' Returns a character vector made up of two vectors, where if both are is.na()
#' NA is returned, if one is.na() the other is returned and if neither are
#' is.na() then the first is returned.
#'
#' @param vec1 first character vector
#' @param vec2 second character vector
#' @export
combine_vectors <- function(vec1, vec2) {
  if (length(vec1) != length(vec2))
    stop("vectors must be the same length")
  combined <- character(length(vec1))
  both <- seq_along(vec1)[!is.na(vec1) & !is.na(vec2)]
  use_vec1 <- seq_along(vec1)[!is.na(vec1) & is.na(vec2)]
  use_vec2 <- seq_along(vec1)[is.na(vec1) & !is.na(vec2)]
  neither <- seq_along(vec1)[is.na(vec1) & is.na(vec2)]
  combined[c(both, use_vec1)] <- vec1[c(both, use_vec1)]
  combined[use_vec2] <- vec2[use_vec2]
  combined[neither] <- NA
  combined
}
#' Returns character vector of one containing the results of the Tukey
#' five number summary calculations made by \code{fivenum()}.
#'
#' @param dist vector of numbers that are the argument to \code{fivenum()}
#' @param digits integer values with the number of digits to be displayed.
#' @import stats
#' @import stringi
#' @export
latex_fivenum <- function(dist, digits = 3) {
  stri_c("{", vector2string(signif(fivenum(dist), digits), SS = "}{"), "}")
}

#' Takes a dataframe and trims all character columns of leading and trailing
#' blanks.
#'
#' @return dataframe with values in all character columns having been
#' trimmed on both sides.
#' @param .df dataframe to have columns trimmed
#' @param cols optional character vector arguement specifying which columns to
#' trim
#' @import stringi
#' @export
stri_trim_cols <- function(.df, cols = names(.df)) {
  for (col in cols) {
    .df[ , col] <- stri_trim_both(.df[ , col])
  }
  return(.df)
}
#' Returns dataframe with multiple sets of the original columns (cols) made
#' from the original dataframe columns.
#'
#' Thus, a 10 row 1 column dataframe becomes a 4 row 3 column dataframe with
#' 2 NA values added to fill out the end of the new dataframe.
#' This is used to reduce the length of very narrow dataframes.
#'
#' @param .df dataframe
#' @param cols number of set of columns to be made
#' @import stringi
#' @examples
#' df <- data.frame(a = 1:5, b = letters[1:5], c = LETTERS[1:5])
#' make_mult_col(df, 2)
#' @export
make_mult_col <- function(.df, cols = 1) {
  n_orig_col <- ncol(.df)
  rows <- ceiling(nrow(.df) / cols)
  new_df <- data.frame(col = rep(NA, rows))
  total <- nrow(.df)
  .df <- rbind(.df, rep(NA, (rows * cols * n_orig_col) - total))
  for (i in seq(1, rows * cols, by = rows)) {
    j <- i + rows - 1
    new_df <- cbind(new_df, .df[i:j, ])
  }
  new_df <- new_df[ , -1]
  names(new_df) <- stri_c(names(.df), rep(1:cols, each = n_orig_col))
  new_df
}
#' Returns the words that describe the integer values of the number provided
#' as an argument.
#'
#' @param x number
#' @export
numbers2words <- function(x) {
  ## From GitHubGistpsychemedia/numbers2words.R
  ## Function by John Fox found here:
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  helper <- function(x) {
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1)
      as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19)
        as.vector(teens[digits[1]])
    else
      trim(paste(tens[digits[2]],
                 Recall(as.numeric(digits[1]))))
    else if (nDigits == 3)
      trim(paste(ones[digits[3]], "hundred",
      # else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and",
                 Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes))
        stop(paste(x, "is too large!"))
      trim(paste(
        Recall(makeNumber(digits[nDigits:(3 * nSuffix + 1)])),
        suffixes[nSuffix], ",", Recall(makeNumber(digits[(3 * nSuffix):1]))
      ))
    }
  }
  trim <- function(text) {
    #Tidy leading/trailing whitespace, space before comma
    text <- gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,", ",", text)))
    #Clear any trailing " and"
    text <- gsub(" and$", "", text)
    #Clear any trailing comma
    gsub("\ *,$", "", text)
  }
  makeNumber <- function(...)
    as.numeric(paste(..., collapse = ""))
  #Disable scientific notation
  opts <- options(scipen = 100)
  on.exit(options(opts))
  ones <-
    c("",
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
      "nine")
  names(ones) <- 0:9
  teens <-
    c(
      "ten",
      "eleven",
      "twelve",
      "thirteen",
      "fourteen",
      "fifteen",
      "sixteen",
      " seventeen",
      "eighteen",
      "nineteen"
    )
  names(teens) <- 0:9
  tens <-
    c("twenty",
      "thirty",
      "forty",
      "fifty",
      "sixty",
      "seventy",
      "eighty",
      "ninety")
  names(tens) <- 2:9
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")
  if (length(x) > 1)
    return(trim(sapply(x, helper)))
  helper(x)
}
#' Returns the fractional age in years, months or days between the start_date
#' and the end_date.
#'
#' @param start_date a POSIXct or POSIXt object with the beginning date (such
#' as a birth date).
#' @param end_date  a POSIXct or POSIXt object with the ending date (such
#' as a death date).
#' @param unit one of c("year", "month", "week", "day") that become the
#' unit used as the devisor for the time interval.
#' @import lubridate
#' @export
get_age <- function(start_date, end_date, unit) {
  if (missing(unit)) {
    cat(stri_c("You must enter a 'unit' argument that is one of 'year', ",
               "'month', 'week', 'day'.\n"))
    return(invisible())
  }
  match.arg(unit, c("year", "month", "week", "day"))
  switch(unit,
         year = (end_date - start_date) / eyears(1),
         month = 12.0 * (end_date - start_date) / eyears(1),
         week = (end_date - start_date) / eweeks(1),
         day = (end_date - start_date) / edays(1))
}
#' Returns even numbers from a set of numbers
#'
#' @param x vector of numbers (only integers should be used)
#' @export
is_even <- function(x) x %% 2 == 0
#' Returns odd numbers from a set of numbers
#'
#' @param x vector of numbers (only integers should be used)
#' @export
is_odd <- function(x) x %% 2 == 1
#' Returns dataframe with ordered locations of the matching braces.
#'
#' @param txt character vector of length one having 0 or more matching braces.
#' @import stringi
#' @examples
#' library(rmsutilityr)
#' match_braces("{123{456{78}9}10}")
#' @export
match_braces <- function(txt) {
  txt <- txt[1] # just in the case of having more than one element
  left <- stri_locate_all_regex(txt, "\\{")[[1]][ , 1]
  right <- stri_locate_all_regex(txt, "\\}")[[1]][ , 2]
  len <- length(left)
  braces <- data.frame(left = rep(0, len), right = rep(0, len))
  for (i in seq_along(right)) {
    for (j in rev(seq_along(left))) {
      if (left[j] < right[i] & left[j] != 0) {
        braces$left[i] <- left[j]
        braces$right[i] <- right[i]
        left[j] <- 0
        break
      }
    }
  }
  braces[order(braces$left), ]
}

#' Returns a list containing two objects in the text of a character vector
#' of length one: (1) object = the first json object found and (2) remainder =
#' the remaining text.
#'
#'  Properly formed messages are assumed. Error checking is non-existent.
#' @param json_txt character vector of length one having one or more JSON
#' objects in character form.
#' @import stringi
#' @export
get_first_json_message <- function(json_txt) {
  len <- stri_length(json_txt)
  braces <- match_braces(json_txt)
  if (braces$right[1] + 1 > len) {
    remainder <- ""
  } else {
    remainder <- stri_trim_both(stri_sub(json_txt, braces$right[1] + 1))
  }
  list(object = stri_sub(json_txt, braces$left[1], to = braces$right[1]),
       remainder = remainder)
}
#' Returns list of lists made by call to fromJSON()
#'
#' @param json_txt character vector of length 1 having one or more
#' JSON objects in text form.
#' @import jsonlite
#' @import stringi
#' @export
get_json_list <- function (json_txt) {
  t_json_txt <- json_txt
  i <- 0
  json_list <- list()
  repeat{
    i <- i + 1
    message_remainder <- get_first_json_message(t_json_txt)
    json_list[i] <- list(fromJSON(message_remainder$object))
    if (message_remainder$remainder == "")
      break
    t_json_txt <- message_remainder$remainder
  }
  json_list
}
#' Returns a dataframe after empty columns have been removed.
#'
#' @param df dataframe
#' @export
remove_empty_columns <- function(df) {
  vec <- c()
  for (i in seq_along(names(df))) {
    if (sum(abs(!is.na(df[i]))) > 0) {
      vec <- c(vec, names(df[i]))
    }
  }
  subset(df, select = vec)
}

#' Returns a character vector with the remove_str pattern removed if it existed.

#' Warning: depricated; left in for compatibility. Use remove_strings() instead.
#' @param strings character vector
#' @param remove_str character vector of length one containing the regular
#' expression of the character string to be removed from the strings character
#' vector
#' @import stringi
#' @export
remove_string <- function(strings, remove_str) {
  warning(stri_c("Warning: depricated; left in for compatibility. ",
                  "Use remove_strings() instead."))
  strings[!stri_detect(strings, regex = remove_str)]
}
#' Returns a character vector with the string patterns within the expunge
#' character vector removed.
#'
#' Uses stri_detect_regex.
#' @examples
#' strngs <- c("abc", "d", "E", "Fh")
#' expunge <- c("abc", "D", "E")
#' remove_strings(strngs, expunge, ignore_case = TRUE)
#' remove_strings(strngs, expunge, ignore_case = FALSE)
#'
#' @param .str character vector
#' @param expunge character vector of strings to be removed
#' @param ignore_case logical indication whether or not to be case specific.
#' @import stringi
#' @export
remove_strings <- function(.str, expunge, ignore_case = FALSE) {
  if (ignore_case) {
    tmp_str <- tolower(.str)
    tmp_expunge <- tolower(expunge)
  } else {
    tmp_str <- .str
    tmp_expunge <- expunge
  }
  keep <- rep(TRUE, length(.str))
  for (exp_str in tmp_expunge) {
    keep <- !stri_detect_regex(tmp_str, exp_str) & keep
  }
  .str[keep]
}
#' Returns a character vector with the string pattern
#' within the expunge_str removed from names_str if they existed.
#' @param names_str character vector with names to be filtered
#' @param expunge_str character vector with names to be expunged
#' @param lower logical TRUE if names_str is to be turned to lower case
#' @export
expunge_names <- function (names_str, expunge_str = c(), lower = TRUE) {
  if (lower)
    names_str <- tolower(names_str)

  for (expunge_string in expunge_str) {
    names_str <- remove_string(names_str, expunge_string)
  }
  names_str
}

#' Returns the elapsed time since start_time.
#' @param start_time a POSIXct time object
#' @import stringi
#' @import lubridate
#' @export
get_elapsed_time_str <- function(start_time) {
  # To use: collect the start_time at the beginning of the script with
  # start_time <- proc.time()
  # At the end call this function using start_time as the sole argument
  # elapsed_time <- get_elapsed_time_str(start_time)
  total_seconds <- (proc.time()[[3]] - start_time[[3]])
  total_minutes <- total_seconds / 60
  hours <- floor(total_minutes / 60)
  minutes <- floor(total_minutes - hours * 60)
  seconds <- round(total_seconds - (hours * 3600) - (minutes * 60), 0)
  hours_str <- ifelse(hours > 0, stri_c(hours, " hours, "), "")
  minutes_str <- ifelse(minutes > 0, stri_c(minutes, " minutes and "), "")
  seconds_str <- stri_c(seconds, " seconds.")
  stri_c(hours_str, minutes_str, seconds_str)
}
#' Creates an Excel workbook with worksheets.
#'
#' @param file filename of workbook to be created
#' @param df_list list of data frames to be added as worksheets to workbook
#' @param sheetnames character vector of worksheet names
#' @param create Specifies if the file should be created if it does not
#' already exist (default is FALSE). Note that create = TRUE has
#' no effect if the specified file exists, i.e. an existing file is
#' loaded and not being recreated if create = TRUE.
#' @import XLConnect
#' @export
create_wkbk <- function(file, df_list, sheetnames, create = TRUE) {
  if (length(df_list) != length(sheetnames))
    stop("Number of dataframes does not match number of worksheet names")

  if (file.exists(file) & create)
    file.remove(file)

  wkbk <- loadWorkbook(filename = file, create = create)
  for (i in seq_along(df_list)) {
    sheetname <- sheetnames[i]
    df <- df_list[[i]]
    createSheet(wkbk, sheetname)
    writeWorksheet(wkbk, df, sheetname, startRow = 1, startCol = 1,
                   header = TRUE)
    setColumnWidth(wkbk, sheetname, column = 1:ncol(df), width = -1)
  }
  saveWorkbook(wkbk)
  wkbk
}

#' Runs debug once on a function that is in memory
#'
#' myDebug() is by Andrea Span
#' 
#' @param f name of function to be debuged.
#' @export
myDebug <-  function(f) {
  fname <- deparse(substitute(f))
  dump(fname, file = "tmp.R")
  source("tmp.R")
  do.call("debugonce", args = list(fname), envir = globalenv())
  invisible(NULL)
}
#' \code{captitalize_first_letter()} returns character string with first
#'  character of each word capitalized and
#' remaining characters either left alone or are forced to lower case.
#'
#' @examples
#' library(stringi, quietly = TRUE)
#' capitalize_first_letter(words = c("a", "vector of words",
#'                                  "TO BE CAPITALIZED."))
#' capitalize_first_letter(words = c("a", "vector of words",
#'                                  "TO BE CAPITALIZED."), lower = TRUE)
#' @param words character string with one or more words in each element.
#' @param lower logical indicating whether remaining characters will be
#' forced to lower case.
#'
#' @import stringi
#' @export
capitalize_first_letter <- function(words = "", lower = FALSE) {
  if (lower)
    words <- tolower(words)
  new <- sapply(words, function(word) {
    word <- unlist(stri_split(as.character(word[[1]]), fixed = " "))
    substring(word, 1, 1) <- toupper(substring(word, 1, 1))
    substring(word, 2) <- substring(word, 2)
    word <- stri_c(word, collapse = " ")
    word
  })
  names(new) <- NULL
  new
}

#' Replaces duplicate strings with empty strings.
#'
#' Replaces duplicate strings with empty strings
#' in a character vector.
#'
#' @examples
#' library(stringi)
#' dups_to_empty_str(c("A", "duplicated", "duplicated", "word"))
#' @param char_vec character vector
#' @import stringi
#' @export
dups_to_empty_str <- function(char_vec) {
  if (length(char_vec) > 1) {
    element <- char_vec[1]
    for (i in 2:length(char_vec)) {
      if (element == char_vec[i]) {
        char_vec[i] <- ""
      } else {
        element <- char_vec[i]
      }
    }
  }
  char_vec
}

#' Prints xtable with gray bar
#'
#' Prints xtable with longtable features
#' @param df data frame being printed
#' @param caption table caption. Default is ''.
#' @param label table label. Default is ''.
#' @param size latex font size to be used. Default is 'normalsize'
#' @param cont longtable continuation string. Default is 'Cont. ...'.
#' @param tabular.environment print.xtable tabular.environment. Default is
#' 'longtable'.
#' @param include.rownames logical. If TRUE the rows names is printed.
#' Default value is TRUE.
#' @param align	Character vector of length equal to the number of columns
#'  of the resulting table, indicating the alignment of the corresponding
#'  columns. Default is NULL
#'  Also, "|" may be used to produce vertical lines between columns in
#'  LaTeX tables, but these are effectively ignored when considering the
#'  required length of the supplied vector. If a character vector of length
#'  one is supplied, it is split as strsplit(align, "")[[1]] before processing.
#'  Since the row names are printed in the first column, the length of align
#'  is one greater than ncol(x) if x is a data.frame. Use "l", "r", and "c"
#'  to denote left, right, and center alignment, respectively. Use "p{3cm}" etc.
#'  for a LaTeX column of the specified width. For HTML output the "p" alignment
#'  is interpreted as "l", ignoring the width request.
#' @param floating If TRUE and type="latex", the resulting table will be a
#'  floating table (using, for example, \\begin{table} and \\end{table}).
#'  See floating.environment below. Default value is TRUE.
#' @param floating.environment  If floating=TRUE and type="latex",
#'  the resulting table uses the specified floating environment.
#'  Possible values include "table", "table*", and other floating
#'  environments defined in LaTeX packages. Default value is "table".
#' @param digits  Numeric vector of length equal to one (in which case
#'  it will be replicated as necessary) or to the number of columns of the
#'  resulting table or matrix of the same size as the resulting table
#'  indicating the number of digits to display in the corresponding columns.
#'  Since the row names are printed in the first column, the length of the
#'  vector digits or the number of columns of the matrix digits is one
#'  greater than ncol(x) if x is a data.frame. Default depends of class of x.
#'  If values of digits are negative, the corresponding values of x are
#'  displayed in scientific format with abs(digits) digits.
#' @param hlines.after When type="latex", a vector of numbers between -1 and
#'  "nrow(x)", inclusive, indicating the rows after which a horizontal line
#'  should appear. If NULL is used no lines are produced. Default value is
#'  c(-1,0,nrow(x)) which means draw a line before and after the columns
#'  names and at the end of the table. Repeated values are allowed.
#' @param type character vector of length one representing the type of table
#'  to produce. Possible values for type are "latex" or "html".
#'  Default value is "latex".
#' @param ... extra arguments to print()
#' @import stringi
#' @import xtable
#' @export
xt_print <- function(df, caption = "", label = "", size = "normalsize",
                     cont = "Cont. ...", tabular.environment = "longtable",
                     include.rownames = FALSE, align = NULL, floating = FALSE,
                     floating.environment = "table", digits = NULL,
                     hlines.after = c(-1, 0, nrow(df)), type = "latex", ...) {
  if (any(nrow(df) %in% c(0, 1))) {
    pos <- 1
  } else
    pos <- seq(1, nrow(df) - (nrow(df) %% 2), by = 2)

  for (i in seq_along(hlines.after)) {
    if (hlines.after[i] > 0) {
      if (any(pos == hlines.after[i]) & hlines.after[i] < nrow(df)) {
        pos <-
          c(pos[pos < hlines.after[i]],
            seq(hlines.after[i] + 1, nrow(df) - (nrow(df) %% 2), by = 2))
      }
    }
  }

  if (type == "latex") {
    tabular.environment <- "longtable"
    floating <- FALSE
    addtorow <- list()
    if (any(nrow(df) %in% c(0, 1))) {
      addtorow$pos <- list(0, 0) # gray bar
      addtorow$command  <- c(stri_c("\\hline \n",
                                    "\\endhead \n",
                                    "\\hline \n",
                                    "{\\footnotesize ", cont, "} \n",
                                    "\\endfoot \n"),
                             "\\endlastfoot \n")
    } else {
      addtorow$pos <- list(0, pos, 0) # gray bar
      addtorow$command  <- c(stri_c("\\hline \n",
                                    "\\endhead \n",
                                    "\\hline \n",
                                    "{\\footnotesize ", cont, "} \n",
                                    "\\endfoot \n"),
                             "\\rowcolor[gray]{.95} \n",
                             "\\endlastfoot \n")
    }
  } else {
    tabular.environment <- "tabular"
    addtorow <- NULL
    floating <- TRUE
  }
  print(xtable(df, caption = caption, label = label, align = align,
               digits = digits),
        tabular.environment = tabular.environment, table.placement = "",
        caption.placement = "bottom", floating = floating,
        include.rownames = include.rownames,
        hline.after = hlines.after,
        add.to.row = addtorow,
        size = size, floating.environment = floating.environment, type = type,
        ...)
}
#' Returns a format statement used by xtable based on centimeter column widths
#'
#' @param col_widths_txt character vector with the representations of the
#' column widths to be used in the format statement.
#' @import stringi
#' @export
get_col_width_fmt <- function(col_widths_txt) {
  col_width_fmt <- "p{0cm}"
  for (txt in col_widths_txt)
    col_width_fmt <- c(col_width_fmt, stri_c("p{", txt, "cm}"))
  stri_c(col_width_fmt, collapse = "")
}
#' Returns a character string of format statements split up from a format
#' statement based on the number of centimeters indicated by the parameter cm
#'
#' @param fmt source format statement
#' @param cm number of centimeters format statement are to fit within
#' @export
get_split_format <- function(fmt, cm = 15) {
  col_widths_txt <- unlist(stri_extract_all_regex(fmt,
                                                  pattern = "[0-9]+\\.*[0-9]*"))
  col_widths <- as.numeric(col_widths_txt)
  ncols <- length(col_widths)
  n_page_widths <- 100 # more than we need
  split_format <- character(n_page_widths)
  cols <- matrix(rep(c(0, 0), n_page_widths), ncol = 2)
  start <- 1
  ## Space between columns:
  ## To tweak the space between columns (LaTeX will by default choose very
  ## tight columns), one can alter the column separation:
  ## \setlength{\tabcolsep}{5pt}. The default value is 6pt.
  space_between <- 0.211666667 # 6pt in cm

  for (page in 1:n_page_widths) {
    for (col in start:ncols) {
      if (sum(c(col_widths[start:col], space_between * (col - start))) > cm) {
        col <- col - 1
        break
      }
    }
    split_format[page] <- get_col_width_fmt(col_widths_txt[start:col])
    cols[page, ] <- c(start, col)
    if (col < ncols) {
      start <- col + 1
    } else {
      break
    }
  }
  list(split_format = split_format[1:page],
       cols = as.matrix(cols[1:page, ], ncol = 2))
}
#' Returns a list of data frames split up for printing so that they do not
#' exceed the width (cm) specified.
#'
#' @param df source dataframe
#' @param fmt source format statement
#' @param cm number of centimeters format statement are to fit within
#' @export
get_split_table <- function(df, fmt, cm = 15) {
  split <- get_split_format(fmt, cm)
  split_df_list <- vector("list", length(split$split_format))
  for (page in seq_along(split$split_format)) {
    split_df_list[[page]] <-
      df[ , split$cols[page, 1]:split$cols[page, 2]]
  }
  split_df_list
}
#' Check to see of a package is installed
#'
#' This function takes a package name and sees if it is installed on the
#' present system.
#'
#' @param mypkg is a character variable containing a package name
#' @examples
#' not_installed("stringi")
#' @import stringi
#' @import utils
#' @export
not_installed <- function(mypkg) !is.element(mypkg, installed.packages()[ , 1])
#' Make a date character string given a date.
#'
#' This function takes a date and arguments about how to represent months
#' to form a character string that forms a date.
#'
#' @param .date a POSIXct or Date object to be translated
#' @param label (from ?lubridate:::month) logical. TRUE will display
#' the month as a character string such as "January."
#' FALSE will display the month as a number.
#' @param abbr (from ?lubridate:::month) logical. FALSE will display
#' the month as a character string #' label, such as #' "January".
#' TRUE will display an abbreviated version of the label,
#' such as "Jan". abbr is #' disregarded if label = FALSE.
#' @keywords date
#' @examples
#' library(stringi, quietly = TRUE)
#' library(lubridate, quietly = TRUE)
#' make_date_str(ymd("2010/3/21", quiet = TRUE))
#' make_date_str(ymd("2010/3/21", quiet = TRUE), abbr = TRUE)
#' @import stringi
#' @export
make_date_str <- function(.date, label = TRUE, abbr = TRUE) {
  stri_c(month(.date, label = label, abbr = abbr), " ",
        day(.date), ", ", year(.date))
}

#' Coverts values from a vector to a concatenation of substrings separated
#' by user-specified string separator.
#'
#' LIMITATION: The function interprets all substrings in the vector either
#' as of type 'int' or 'char'. A function that interprets the type of each
#' substring dynamically may one day be written by an R-guru.
#'
#' Franc Brglez, Wed Dec 9 15:43:59 EST 2009
#'
#' @param vector integer or character vector
#' @param SS User-specified string separator. (default value: SS = "', '").
#' @param type Tell function the class of the \code{vector}.
#' @examples
#' library(stringi, quietly = TRUE)
#' ## Here we convert a binary vector to a binary string representing an integer:
#' binV <- c(1, 0, 0, 1)
#' strS <- vector2string(binV, type = 'int')
#' ## Here we convert a binary vector to string representing a binary sequence:
#' binV <- c(1,0,0,1)
#' seqS <- vector2string(binV, SS=' ', type='char')
#' ## Here we convert a vector of substrings to colon-separated string:
#' subsV <- c('I', 'am', 'done')
#' strS <- vector2string(subsV, SS = ':', type = 'char')
#'
#' ## Making an SQL IS IN statement
#' ids <- c(' 12345', '4X3200', '1X2890')
#' stri_c("and master.id in ('", vector2string(ids, "', '"), "') ")
#' @import stringi
#' @export
vector2string <- function(vector=c(" 12345", "4X3200", "1X2890"),
                         SS = "', '", type = "char") {
  if (type == "int") {
    string <- stri_c(stri_split_fixed(stri_c(vector), " "), collapse = SS)
  } else if (length(vector) == 1) {
    return(vector)
  } else {
    n <- length(vector)
    nm1 <- n - 1
    string <- ""
    for (i in 1:nm1) {
      tmp <- noquote(vector[i])
      string <- stri_c(string, tmp, SS, sep = "")
    }
    tmp <- noquote(vector[n])
    string <- stri_c(string, tmp, sep = "")
  }
  return(string)
} # vector2string

#' Returns the Ids in right blank filled and upper case animal Ids.
#'
#' @param ids character vector of animal Ids that may need to be transformed
#' into the proper Id format.
#' @param upper logical indicator of whether ids are to be forced to upper case
#' @examples
#' new_ids <- blank_fill_ids(c("12345", "1X1234", "1234","2 3456"))
#' @import stringi
#' @export
blank_fill_ids <- function(ids, upper = TRUE) {
  if (upper)
    ids <- toupper(stri_trim_both(ids))
  if (class(ids) == "factor")
    ids <- as.character(ids)
  for (i in seq_along(ids)) {
    if (is.na(ids[i]))
      next

    if (stri_length(ids[i]) > 6 | stri_length(ids[i]) < 3)
      warning(stri_c("Id size out of range. i is ", i,
                    " stri_length(ids[[i]]) is ",
                    stri_length(ids[[i]]),
                    " and Id is ", ids[[i]]))
    else
      ids[[i]] <- sprintf("%6s", ids[[i]])
  }
  ids
}

#' Returns character vector where each element has had its character
#' order rerversed.
#'
#' @param x - Character vector
#' @examples
#' str_reverse('abc') # "cba"
#' str_reverse(c('abc', 'defgh', 'i', 'jklm')) # "cba" "hgfed" "i" "mlkj"
#' @import stringi
#' @export
str_reverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), stri_c,
                                  collapse = "")

#' Returns a one element character string with correct punctuation for
#' a list made up of the elements of the character vector argument.
#'
#' @param c_vector Character vector containing the list of words
#' to be put in a list.
#' @param conjunction The conjunction to be used as the connector.
#' This is usually `and' or `or' with `and' being the default.
#' @examples
#' get_and_or_list(c('Bob', 'John')) # "Bob and John"
#' get_and_or_list(c('Bob', 'John'), 'or') # "Bob or John"
#' get_and_or_list(c('Bob', 'John', 'Sam', 'Bill'), 'or')
#' # "Bob, John, Sam, or Bill"
#' @import stringi
#' @export
get_and_or_list <- function(c_vector, conjunction = "and") {
  len <- length(c_vector)
  c_str <- ""
  if (len == 1)
    c_str <- c_vector
  if (len == 2)
    c_str <- stri_c(c_vector[[1]], conjunction, c_vector[[2]], sep = " ")
  if (len > 2) {
    c_str <- stri_c(c(c_vector[1:(len - 1)], conjunction), sep = "",
                   collapse = ", ")
    c_str <- stri_c(c_str, " ", c_vector[[len]], sep = "", collapse = " ")
  }
  c_str
}

#' Detects regular expressions or fixed string values in character vectors.
#'
#' Returns a logical vector with results of stri_detect() for each pattern in
#' second parameters character vector.
#'
#' @usage str_detect_all(strings, patterns, type, ...)
#' @param strings input vector. This must be an atomic vector and will be
#' coerced to a character vector.
#' @param patterns patterns to look for, as defined by a POSIX regular
#' expression. See the \emph{Extended Regular Expressions} section of regex
#' for details. See fixed, ignore.case and perl for how to use other
#' types of matching: fixed, case insensitive and perl-compatible.
#' @param type character vector of length equal to \code{search} with either
#' \code{``fixed''} or \code{``regex''}.
#' @param ... further arguments for stri_detect_regex
#' @import stringi
#' @export
str_detect_all <- function(strings, patterns, type, ...) {
  sapply(seq_along(patterns), function(i) {
    if (type[i] == "regex") {
      any(stri_detect_regex(strings, patterns[i], ...))
    } else if (type[i] == "fixed") {
      any(stri_detect_fixed(strings, patterns[i], ...))
    } else {
      stop("type must be the same length as patterns and either 'regex' or
           'fixed'.")
    }
  })
}
#' Returns a logical vector with results of stri_detect() for each pattern in
#' second parameters character vector.
#'
#' @usage str_detect_regex_all(strings, patterns, ...)
#' @param strings input vector. This must be an atomic vector and will be
#' coerced to a character vector.
#' @param patterns patterns to look for, as defined by a POSIX regular
#' expression. See the \emph{Extended Regular Expressions} section of regex
#' for details. See fixed, ignore.case and perl for how to use other
#' types of matching: fixed, case insensitive and perl-compatible.
#' @param ... further arguments for stri_detect_regex
#' @import stringi
#' @export
str_detect_regex_all <- function(strings, patterns, ...) {
  sapply(patterns, function(pattern) {
    any(stri_detect_regex(strings, pattern, ...))})
}
#' Returns a logical vector with results of stri_detect() for each pattern in
#' second parameters character vector.
#'
#' @usage str_detect_fixed_all(strings, patterns, ...)
#' @param strings input vector. This must be an atomic vector and will be
#' coerced to a character vector.
#' @param patterns patterns to look for, as defined by a POSIX regular
#' expression. See fixed, ignore.case and perl sections
#' for details. See  \emph{Extended Regular Expressions} for how to use regular
#' expressions for matching.
#' @param ... further arguments for stri_detect_fixed
#' @import stringi
#' @export
str_detect_fixed_all <- function(strings, patterns, ...) {
  sapply(patterns, function(pattern) {
    any(stri_detect_fixed(strings, pattern, ...))})
}
#' Replaces string or NA in a character vector with either NA or
#' another string.
#'
#' This is to augment \code{stri_replace_all_fixed()}, which does
#' not allow \code{NA} as a pattern.
#'
#' @param x character vector
#' @param pattern string pattern to search for may be empty string or NA.
#' Comparison is done with "==". Case can be ignored.
#' @param replacement string to replace entire entity that has pattern.
#' @param fold logical value where if TRUE the vector \code{x} and
#' \code{pattern} are converted to lower case.
#' @import stringi
#' @export
str_or_na_replace <- function(x, pattern, replacement, fold = FALSE) {
  if (is.na(pattern)) {
    x[is.na(x)] <- replacement
  } else if (fold) {
    x[tolower(x) == tolower(pattern)] <- replacement
  } else {
    x[x == pattern] <- replacement
  }
  x
}
#' Convert underscore text strings such as \code{trans_col_select} to
#' \code{TransColSelect}.
#'
#' @param s character vector that may have tokens with embedded
#' underscores.
#' @import stringi
#' @export
underscoreToTitleCase <- function(s) {
  sapply(s, function(s1) {
    s2 <- stri_split_fixed(s1, pattern = "_")[[1]]
    stri_c(toupper(stri_sub(s2, 1, 1)),
           stri_sub(s2, 2), collapse = "")})
}
#' Convert underscore text strings such as \code{trans_col_select} to
#' \code{transColSelect}.
#'
#' @param s character vector that may have tokens with embedded
#' underscores.
#' @import stringi
#' @export
underscoreToCamelCase <- function(s) {
  sapply(s, function(s1) {
    s2 <- stri_split_fixed(s1, pattern = "_")[[1]]
    s3 <- stri_c(toupper(stri_sub(s2, 1, 1)),
                 stri_sub(s2, 2), collapse = "")
    stri_c(tolower(stri_sub(s3, 1, 1)),
           stri_sub(s3, 2), collapse = "")})
}
#' Are these files?
#'
#' If at least one of the character strings is a file it returns
#' \code{exist} as a list element set to \code{TRUE} and all of the
#' character strings that are file names are retained and the
#' character strings that are not file names are removed from the
#' \code{files} character vector in the returned list.
#' If none of the  character strings is a file it returns
#' \code{exist} as a list element set to \code{FALSE}
#' and all of the character strings are retained in the
#' \code{files} character vector in the returned list.
#' @param files character vector of file names or path.
#' @param no_files_stop logical vector of length one when set to
#' \code{TRUE} the routine calls \code{stop} if no files are found.
#' @import stringi
#' @import tools
is_file <- function(files, no_files_stop = FALSE) {
  files <- files[tolower(file_ext(files)) %in% c("r", "rmd", "rnw")]
  file_exist <- file.exists(files)
  if (any(file_exist)) {
    exist <- TRUE
    not_found <- files[!file_exist]
    if (length(not_found) > 0) {
      warning(stri_c("File(s) not found: ", get_and_or_list(not_found), "."))
      files <- files[file_exist]
    }
  } else if (no_files_stop) {
    stop("No files found.")
  } else {
    exist <- FALSE
  }
  list(exist = exist, files = files)
}
#' Creates a list object containing number of lines, blank lines,
#' bare comments, roxygen2 documentation, and code.
#'
#' @param files character vector of file names. The file names may contain the
#' path. Without a path in the file name and without a defined \code{path}
#' paramenter, the current directory is used.
#' @param path optional character vector of length one or of the same length as
#' \code{files}. If of length one, it is used for all files. If it is the
#' same length as files, each value is paired with its corresponding file to
#' make the fully qualified name.
#' @import stringi
#' @export
classify_code_lines <- function(files = ".", path = NULL) {
  if (files[1] == "." & length(files) == 1 & is.null(path)) {
    files <- list.files(path = ".", full.names = TRUE)
  }
  if (!is.null(path)) {
    files <- stri_c(path, "/", files)
    files <- stri_replace_all_fixed(files, pattern = "//", replacement = "/")
  }

  file_list <- is_file(files, no_files_stop = FALSE)
  if (!file_list$exist) { # May be path argument
    files <- list.files(path = files, full.names = TRUE)
    file_list <- is_file(files, no_files_stop = TRUE)
  }

  count <- 0
  code <- 0
  comments <- 0
  roxygen_comments <- 0
  blank_lines <- 0
  for (file in file_list$files) {
    lines <- readLines(file)
    count <- count + length(lines)
    for (line in lines) {
      if (stri_sub(line, 1, 2) == "#'") {
        roxygen_comments <- roxygen_comments + 1
        next
      }
      line <- stri_trim_both(line)
      if (stri_length(line) == 0) {
        blank_lines <- blank_lines + 1
        next
      }
      buf <- stri_split_boundaries(line, type = "word")
      if (stri_detect_fixed(buf[[1]][1], "#")) {
        comments <- comments + 1
        next
      } else {
        code <- code + 1
      }
    }
  }
  c(files = length(file_list$files), lines = count, code = code,
    blank_lines = blank_lines, roxygen_doc_lines = roxygen_comments,
    comments = comments)
}
