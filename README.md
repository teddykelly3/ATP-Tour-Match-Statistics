# Tennis Match Statistics Association with Match Outcomes

## Author
Teddy Kelly

## Overview
This project analyzes ATP tennis match statistics to identify which factors
most significantly determine match outcomes. Additionally, this project investigates
the role that the different court surfaces play in influencing the the importance of
certain match statistics on outcomes.

## Other Files
Math_Stats_Most_Associated_With_Match_Results_On_ATP_Tour.qmd
- R quarto file that contains all of the code used for the project.
  
Math_Stats_Most_Associated_With_Match_Results_On_ATP_Tour.pdf
- Paper containing abstract, introduction, literature review, methodology,
results, and conclusions

app.R
- R Shiny app that allows the user to compare logistic regression results between the
three surfaces for any combination of predictor variables specified by the user. The
app produces a table with the coefficient estimates for each selected independent variable.
Match stats with a coefficient estiamtes of higher magnitude have a stronger effect on
tennis match outcomes.

app1.R
- R Shiny app the allows the user to select any match statistic and visually compared the
mean values between match winners and losers across each surface. 


## Data
- Source: *ATP matches* dataset by Sijo Manikadan on Kaggle
- Time period: 1991–2022
- Surfaces: Hard, Clay, Grass

## Methods
- Logistic regression
- Feature standardization
- Surface-specific modeling

## Tools
- R Studio(tidyverse, ggplot2, glm, stargazer)

## Results
- Generating break points and break point conversion rate are among the strongest
predictors of match outcomes on the ATP Tour.
- Gaining a significant advantage over opponents in hitting aces is generally
more important on slower surfaces (clay) because aces are traditionally more rare
on clay. 
- Gaining a significant advantage over opponents in break points generated and
break point conversion rate is more important on clay since that is the
"recipe to success" on clay whereas breaking serve is not as important on
hard or grass
- Gaining a significant advantage over opponents in 1st serve percentage is more
important on grass and hard courts since faster serves are rewarded more so on
quicker surfaces than clay which is a slow surface.



