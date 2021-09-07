NEWS
================
R. Mark Sharp
2021-09-03

# rmsutilityr 1.1.1.9000 20210903

-   Added dependency on R (> 3.5.0) because of serialization of objects
    being used were created with formate 3.
-   Added logical parameter to to determine whether (, default) or not
    () to put a comma before the congunction. Does not break prior use
    of the function.

# rmsutilityr 1.1.0.9001 20210215

-   Added sufficient HTML comment management functions to allow current
    workflow.
-   Added initial draft of vignette illustrating use of comment
    management.

# rmsutilityr 1.1.0.9000 20210215

-   Added most of code of HTML comment management

# rmsutilityr 1.0.75 20190703

-   Added unit tests for vector2string, get_split_format,
    make_license_df, get_pkg_descriptions
-   Further enhanced efficiency of get_pkg_descriptions by reducing need
    to get data from CRAN. User can limit number of time needed to go to
    CRAN is once per instance per day.

# rmsutilityr 1.0.74 20190703

-   Updated get_pkg_descriptions to use CRAN database for dependency
    lookups

# rmsutilityr 1.0.70

-   Added get_pkg_list function to return a character vector of loaded
    packages.

# rmsutilityr 1.0.61 20181022

-   Added a `NEWS.md` file to track changes to the package.

-   Added unit test for is_valid_date_str when anytime was incorporated
    into function to handle more date formats.
