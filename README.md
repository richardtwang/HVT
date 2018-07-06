# HVT
My project concerning the Habitual Voter Theorem. Voter data supplied by the [North Carolina State Board of Elections](https://dl.ncsbe.gov/index.html?prefix=data/). US Census data provided by the [United States Census Bureau](https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml). Originally published on June 2017.

* The census folder contains census data matched by zip codes for each ncid (voter).
* The voter_data folder contains cleaned .xlsx files (100 total) containing voting information for all registered voters of North Carolina.
* The Interactive Model folder contains a macro-enabled .xlsx file representing the logistic regression output as a user interface (UI). instructions on using the UI can be found in Appendix B of the corresponding paper [here](https://rtwrtw8.github.io/papers/Modeling%20Voter%20Turnout.pdf).
* The R code used to run the regression model is located in the voter_code.R file.
