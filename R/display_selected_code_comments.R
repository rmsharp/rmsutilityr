#' Displays selected comments
#' 
#' @returns Dataframe of selected comments with the base file name, the 
#' comment label, the comment start line, and the comment text.
#' 
#' Internally uses the \code{list.files} function with the \code{path} and 
#' \code{pattern} arguments as defined in the call. Other arguments to
#' \code{list.files} are forced as follows:
#' \describe{
#' \item{all.files}{TRUE}
#' \item{full.names}{TRUE}
#' \item{recursive}{TRUE}
#' \item{ignore.case}{TRUE}
#' \item{include.dirs}{FALSE}
#' \item{no..}{FALSE}
#' }
#' The user is free to create the list of files anyway desired and provide them
#' to the \code{path} argument.
#' @param path a character vector of full path names; the default corresponds to 
#' the working directory, getwd(). Tilde expansion (see path.expand) is 
#' performed. Missing values will be ignored. 
#' Elements with a marked encoding will be converted to the native encoding 
#' (and if that fails, considered non-existent). Defaults to ".".
#' @param pattern an optional regular expression. Only file names which match the 
#' regular expression will be returned.
#' @param label Optional regex expression that can be used to limit the 
#' comments found by adding each element of the character vector in turn 
#' immediately after "<!--" in the regex expression. The resulting logical
#' vectors are OR'd together to combine their results.
#' @importFrom kableExtra kbl kable_styling column_spec
#' @export
display_selected_code_comments <-
  function(path = ".",
           pattern = NULL,
           label = "") {
    files <-
      list.files(
        path = path,
        pattern = pattern,
        all.files = TRUE,
        full.names = TRUE,
        recursive = TRUE,
        ignore.case = FALSE,
        include.dirs = FALSE,
        no.. = FALSE
      )
    html_comment_lines_and_labels <-
      get_html_comment_text_lines_and_labels_from_files(files, label = label)
    
    caption <-
        stri_c("Output of the ",
          "get\\_html\\_comment\\_text\\_lines\\_and\\_labels\\_from\\_files ",
          "function includes text of comments from selected ",
          "comment labels.")

    selected_code_comments <-
      html_comment_lines_and_labels[, c("file",
                                        "comment_label",
                                        "comment_start_line",
                                        "comment_text")]
    kbl(
      selected_code_comments,
      format = ifelse(knitr::is_latex_output(), "latex", "html"),
      booktabs = TRUE,
      caption = caption,
      row.names = FALSE,
      col.names = c("File", "Label", "Start", "Text"),
      longtable = TRUE
    ) %>%
      kable_styling(
        latex_options = c("repeat_header", "striped"),
        font_size = ifelse(knitr::is_latex_output(), 8, 12)
      ) %>%
      column_spec(1, width = "15em") %>%
      column_spec(2, width = "5em") %>%
      column_spec(3, width = "5em") %>%
      column_spec(4, width = "25em")
  }
