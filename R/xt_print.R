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
