# A5 Report
This rubric is intended to help guide expectations and increase transparency. However, it **is not** necessarily fully exhaustive. Make sure to read the detailed explanation on Canvas, and follow the best practices described in the course book and in class.

For this assignment, you must use the appropriate `dplyr` functions to complete each task, returning the specified data type. Partial credit will be given where appropriate.

## Set up (**13 points**)
- Creates the file `analysis.R` (**2 points**)
- Creates the file `index.Rmd` (**2 points**)
- Suppress any undesired content from the report (e.g., warning messages) (**2 points**)
- Add a link the the hosted report in the `README.md` file (**2 points**)
- Create a second level header (`##`) for each section of the report (**5 points**)

## Summary Information (**22 points**)
- 2 points: Include a link to the original data source
- For each piece of summary information, you must:
    - Properly compute information in `analysis.R` (**1 point**)
    - Write a full sentence describing the piece of information (**1 point**)
    - Insert the value into a sentence by referencing a variable created in `analysis.R` (**1 point**)
    - Use a meaningful variable name (**1 point**)

## Summary Table (**14 points**)
- 3 points: properly computes aggregate information (e.g., groups by a selected column)
- 2 points: sorts the table by a column of interest
- 2 points: only include relevant columns
- 2 points: Columns names must be well formatted (e.g., not the variable names in your data frame)
- 2 points: Use an appropriate package to display the table (e.g., not just the raw data frame)
- 3 points: Describes important insights from the table (e.g., patterns, extreme value, etc.)

## Description of a particular incident (**10 points**)
- Includes the following (2 points each) by referencing a data value (not simply by typing it out) in a sentence
    - Date
    - Location
    - Number injured
    - Number killed
- Includes a link to an outside resource about the incident (2 points)

## Interactive Map (**18 points**)
- Introduce the purpose of the map in the report: 2 points
- Render an interactive map: 5 points
- Place a marker at each location point: 3 points
- Adjust the size of each marker based on a piece of data about the marker: 2 points
- Include hover or click information showing at least 3 pieces of data about the incident: 3 points
- Place a line break between each piece of information ("<br>"): 1 point
- Lists 2 insights (2 points)

## Plot of your choice (**15 points**)
Choice of plot is up to you, but it should be intentionally designed to answer a specific question about the dataset.
- 2 points: describe a specific question that you intend to answer with your chart
- 6 points: choose the optimal chart type and graphical encodings of your variables to answer your question of interest (see the Designing Data Visualizations chapter of the course book). A legend should be included if appropriate (e.g., if you use colors that aren't otherwise explicit).
- 1 point for a chart title
- 1 point for an x-axis label
- 1 point for an y-axis label
- 4 points for highlighting two insights revealed by the chart

## Critical reflection (**10 points**)
- 2 points for referring to a specific finding of your Report
- 2 points for referring to a specific concept from O'Neil
- 4 points for discussion both a benefit and and harm
- 2 points for clarity andconciseness of your writing

## Code clarity (**8 points**)
To earn full points, you must not have any `lintr()` errors (an _any_ `.R` file you create):
- -1 point for 1 - 2 `lintr` errors
- -2 points for 3 - 4 `lintr` errors
- -3 points for 5 - 6 `lintr` errors
- -4 points for 7 - 8 `lintr` errors
- -5 points for 9+ `lintr` errors

- 3 points for appropriate use of `dplyr` to perform data manipulation throughout
