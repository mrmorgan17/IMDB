# IMDB

This repository includes exploratory analysis and predictive modeling for the [IMDB Score Prediction](https://www.kaggle.com/c/imdbusmovies) Kaggle competition. All code was done using the R programming language.

The .R file `IMDBCleaning.R` includes the code for the methodology used for the data cleaning process. This R script showcases feature selection, feature engineering, mode, mean, and stochastic regression imputation, among other methods to create a clean dataset, `CleanedIMDBData.csv`.

The .R file `IMDB.R` includes the modeling done on the `CleanedIMDBData.csv` dataset. This R script created two different xgbTree models using the [caret](https://topepo.github.io/caret/) package and the [xgboost](https://www.rdocumentation.org/packages/xgboost/versions/1.1.1.1) package. Both models performed similarly, and due to randomness, sometimes the caret model's predictions, `caret-preds.csv` would outperform the xgboost model's preditions, `xgboost-preds.csv`.

A notebook was created on Kaggle for this competition as well and can be found [here](https://www.kaggle.com/matt4byu/imdb-score-prediction-analysis-with-caret-xgboost).

The goal of this competition was to use a variety of data about a movie (actors, director, facebook likes, budget, gross, etc) to predict what its score out of 10 on [IMDB](https://www.imdb.com) would be. This dataset, `IMDBTrain.csv` and `IMDBTest.csv` had information on approximately 3400 domestic (USA) movies. Since the end result was a score out of 10, this meant that the competition was a regression problem and not a classification problem. A unique aspect of this project was the depth of data cleaining that had to be done beforehand. Much of the basis for what I did in my data cleaning comes from Dr. Heaton's [IMDBDataCleaning.R](https://github.com/MJHeaton/IMDB/blob/master/IMDBDataCleaning.R) file. It gave me great new insights into using tidyverse and new methods of data cleaning!

The best model that was fit to the data was an xgbTree model (caret) or an xgboost model with a gbtree booster (xgboost). Models were fit using the [caret](https://topepo.github.io/caret/) package and the [xgboost](https://www.rdocumentation.org/packages/xgboost/versions/1.1.1.1) package. While the competition was only an in-class competition, one the private leaderboard, my best submission achieved an RMSE score of .66826 which placed me 1st among the 18 students who submitted predictions during the time that this Kaggle competition was active. 

Additional improvements could possibly be made in the areas of data cleaning as I am sure that there is more that could be done with this dataset. Also, it is possible that a different model besides the xgbTree models that I used could outperform my current score. Additionally, the model parameters I chose for my xgbTree models could possibly be improved as well.
