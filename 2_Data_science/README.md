Data Science: overview 
================
UWI-couRse
26/02/2021

### Data structure

#### Concepts and bounds

This section will focus on the different types of data and its properties. The type of data and aim of the use will determine the most suitable statistical tool or visualization approach. 

#### References

You can learn more about data science with R in [R for Data Science](https://r4ds.had.co.nz/) and in [Introduction to Data Science](https://rafalab.github.io/dsbook), both freely available bookdown ([see a collection of books here](https://www.bookdown.org/). Specific about tidy data you can find [here](https://rafalab.github.io/dsbook/tidyverse.html) or [here](https://moderndive.com/4-tidy.html).

#### Prerequesites

Have installed in your Rstudio the [tidyverse](https://www.tidyverse.org/) packages, `install.packages("tidyverse")`. 


### Data visualization

#### Concepts and bounds
Based on the data structure and type, this section will present some general principal and tricks using the most powerful plots for data analysis. Cool and informative plots and maps in your publications or presentations, make a lot of difference for the audiences and reviewers.

#### References
There are lot of materials in the web, but you can check some well organized bookdown such as [R Graphics Cookbook](https://r-graphics.org/) or [Fundamentals of Data Visualization](https://clauswilke.com/dataviz/).

#### Prerequesites
The `library("tidyverse")` and `install.packages("cowplot", "grid" .... should be include later)`


###	Exploratory Analysis

#### Concepts and bounds

Defined the data structure and visualizing the most important variables, it is time to find relationships and identify behaviors that help us to understand the underlying process we want to explore. 

#### References

These book chapters [here](https://r4ds.had.co.nz/exploratory-data-analysis.html), [here](https://www.datascienceatthecommandline.com/1e/chapter-7-exploring-data.html), [here](https://livebook.datascienceheroes.com/exploratory-data-analysis.html#correlation) or [here](https://rafalab.github.io/dsbook/robust-summaries.html) are a good start 

#### Prerequesites
`install.packages("cowplot", "correlograma", "lattice" .... should be include later)`


###	Modelling approaches


#### Concepts and bounds
We will have a overview of empirical modeling approaches for explaining, predicting or forecasting environmental and climatic variables according to the data structure and aim of the study.  

#### References
These book chapters [here](https://r4ds.had.co.nz/model-intro.html),
[here](https://moderndive.com/5-regression.html), [here](https://www.datascienceatthecommandline.com/1e/chapter-9-modeling-data.html), or[here](https://rafalab.github.io/dsbook/models.html)

#### Prerequesites
`install.packages("caret", "lm", .... should be include later)`