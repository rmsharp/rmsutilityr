NEWS
================
R. Mark Sharp
2021-02-16

# rmsutilityr 1.1.0.9001 20210215

-   Added sufficient HTML comment management functions to allow current
    workflow.
-   Added initial draft of vignette illustrating use of comment
    management.

# rmsutilityr 1.1.0.9000 20210215

-   Added most of code of HTML comment management

# rmsutilityr 1.0.75 20190703

-   Added unit tests for vector2string, get\_split\_format,
    make\_license\_df, get\_pkg\_descriptions
-   Further enhanced efficiency of get\_pkg\_descriptions by reducing
    need to get data from CRAN. User can limit number of time needed to
    go to CRAN is once per instance per day.

# rmsutilityr 1.0.74 20190703

-   Updated get\_pkg\_descriptions to use CRAN database for dependency
    lookups

# rmsutilityr 1.0.70

-   Added get\_pkg\_list function to return a character vector of loaded
    packages.

# rmsutilityr 1.0.61 20181022

-   Added a `NEWS.md` file to track changes to the package.

-   Added unit test for is\_valid\_date\_str when anytime was
    incorporated into function to handle more date formats.
