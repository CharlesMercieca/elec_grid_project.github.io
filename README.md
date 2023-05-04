[![run belgian_grid_pred](https://github.com/CharlesMercieca/elec_grid_project.github.io/actions/workflows/actions.yaml/badge.svg)](https://github.com/CharlesMercieca/elec_grid_project.github.io/actions/workflows/actions.yaml)

## What is this

This is a quick and dirty experiment using Belgium's largest grid operator (Elia)'s [generous open data commitment](https://opendata.elia.be/pages/home/) to model the previous load and forecast ahead a 3 day horizon.

To check with the actual grid situation, [visit here](https://www.elia.be/en/grid-data) and scroll down to the grid load chart.

## How does this work?

Every day at midnight UTC _elec_grid_xgboost.r_ downloads the previous day's load from the elia website, as well as the temperatures in Brussels and Bruges from the openmeteo API.

A tidymodels lightgbm is fit and predicts 3 days ahead.

A number of different features are included in the final model, including holiday and weekend times, warming effects and recent loads.

A back fit on the past day is also run as a quick a validation step. This allows a Mean Absolute Error calculation: the average error expected, either + or - per 15 minute period.

The script is scheduled through a github action which then renders a flexdashboard dashboard accesible at https://charlesmercieca.github.io/elec_grid_project.github.io/

For some exploratory data analysis I sketched up while working on this model, see _EDA.html_ in this repo.
