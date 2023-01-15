# example_rshiny

Repository for an examplatory RShiny application structured as follows:

- data - creating database
- sample - RShiny application
- apppackage - package

You can find the application [here](http://dominikzabinski.com:3838/sample/).

## data/

I'm using <code>{DBI}</code> package an .sqlite engine to create simple database. Database is based on single .csv file with names of musician/bands, with additional info about genre. 

Based on that I've created database structure, splitting data into separate tables for dictionaries. 

Based on the data I've randomized encounters (relationships?) that would help me in building a model for finding related/similar bands without filling database manually via app.

## apppackage/

This is the package that holds majority of important files behind RShiny app. Beside basic files/directories I've also included:

- inst directory to store .css and .js files
- testthat directory with some basic tests

I'm using <code>{roxygen2}</code> to document all the functions in the package. 

## sample/

All of the functions and modules are stored in a <code>{apppackage}</code>. The only .R files in this directory is an app.R file that:

- loads <code>{apppackage}</code>
- enables using staitc files from <code>{apppackage}</code> package
- defines final UI and server parts

## Justifying choices

For packages:

- <code>{shiny}</code>: for, well, shiny
- <code>{DBI}</code>: for connecting to database easy, simple, 
- <code>{shinyWidgets}</code> - pickerInput mostly (inline option enables making sentences with inputs in them which will make app easier to use),
- <code>{htmltools}</code> - for tagAppendAttributes
- <code>{data.table}</code> - for data wrangling
- <code>{ggplot2}</code> - for visualizations
