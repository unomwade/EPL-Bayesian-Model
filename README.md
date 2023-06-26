# EPL-Bayesian-Model

This is part of the final project for a class on Bayesian Statistics at UNO.
I created a model that determines the offensive and defensive strength of each team in the 2022 English Premier League.
I used data from the 2022 season up until the break for World Cup to create the model.
All calculations were done through the use of JAGS and result exploration was done in R Studio.

I then went further and used it to predict the number of goals scored by each team for each match for the rest of the season. 
These results were used to predict the overall standings at the end of league play as well as overall offensive and defensive strengths at the season's end.

epl.jags is the initial model that uses goals scored, goals allowed, league average goals, as well as the effect of each stadium's home field advantage. 

The model was created in 4, while the predictions were done in various manners in 5 and 6.
