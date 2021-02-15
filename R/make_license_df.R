#' Make dataframe with mapping of code license descriptions to more general license descriptions.
#' 
#' Needs to be manually updated base on descriptions observed.
#' 
#' @export
make_license_df <- function() {
  licenses <- c("AGPL-3", "Apache License (== 2.0)", "Apache License (== 2.0) | file LICENSE", 
                "Apache License (>= 2.0)", "Apache License 2.0", "Apache License 2.0 | file LICENSE",
                "Artistic-2",
                "BSD 2-clause License + file LICENSE", "BSD_2_clause + file LICENSE", 
                "BSD_3_clause + file LICENCE", "BSD_3_clause + file LICENSE", 
                "BSL", "BSL-1.0", "CC0", "file LICENSE", "GPL", "GPL (>= 2.0)", "GPL (>= 2)", 
                "GPL (>= 2) | file LICENCE", "GPL (>= 2) | file LICENSE", "GPL (>= 3)", 
                "GPL (>= 3) | file LICENCE", "GPL-2", "GPL-2 | file LICENSE", 
                "GPL-2 | GPL-3", "GPL-2 | LGPL-2.1 | MPL-1.1", "GPL-3", "GPL-3 | file LICENSE", 
                "LGPL", "LGPL (>= 2.1)", "LGPL (>= 2)", "LGPL-2", "LGPL-2.1", 
                "LGPL-3", "MIT + file LICENSE", "MIT + file LICENSE | Unlimited", 
                "MPL (>= 2)", "MPL-2.0 | file LICENSE", "Unlimited")
  license_lst <- list(`AGPL-3` = licenses[1],
                      `Apache-2` = licenses[2:6],
                      `Artistic-2` = licenses[7],
                      `BSD-2` = licenses[8:9],
                      `BSD-3` = licenses[10:11],
                      `BSL` = licenses[12:13],
                      `CC0` = licenses[14],
                      `NA` = licenses[15],
                      `GPL` = licenses[c(16, 23:24)],
                      `GPL >= 2` = licenses[c(17:20, 25)],
                      `GPL-3` = licenses[c(21, 22, 24, 27:28)],
                      `LGPL-2` = licenses[29:33],
                      `LGPL-3` = licenses[34],
                      `MIT` = licenses[35:36],
                      `MPL-2` = licenses[37:38],
                      `Unlimited` = licenses[39])
  license_df <- data.frame()
  for (i in seq_along(license_lst)) {
    lic <- license_lst[i]
    license_df <- rbind(license_df, data.frame(
      license = rep(names(lic), length(lic)), description = as.character(unlist(lic)),
      stringsAsFactors = FALSE))
  }
  license_df
}
