# RiskMgmt_DSS
## Risk Management Course Project
### Decision Support System for Travel Destination
https://bohong.shinyapps.io/DesicionSupportSystem/

This System is coded by R Shiny Package which is a interactive visulization tool in R.

DSS is a decision support system, which contains databases, models and the user interface.

When you want to take a trip but have no idea where to go, this Desicion Support System can give you some idea and help you make the decision. You only need to answer several questions, after which you will get a list of top 5 recommended destinations.

Yet there is a prerequisite for this DSS: it is only applied to people who do not have very specific purpose or tendency on their travel destination.


## Code Readme

#### I. Structure

  A. Raw consequence table - Final Consequence Table.xlsx
  B. Derived table - contb.csv
  C. Normalized table - contb_norm.csv
  D. User Interface R code - ui.R
  E. Model building R code - server.R
#### II. What’s in ui.R

  A. Similar to Web design
    1. Input
    2. Output
#### III. What’s in server.R

  A. From line 1 to 380
    1. Define attributes’ variable
    2. Get the individual utility points based on user’s answer (two fixed and 3 derived)
  B. From line 381 to 785
    1. Build utility function
    2. Based on cont_norm.csv to calculate the utility value
    3. Get top5
  C. Define output from 786 to end
    1. Curve
    2. Text output
    3. Generate Map
    4. Generate Spider chart
