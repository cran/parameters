## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = '')
options(digits = 2)
knitr::opts_chunk$set(comment = "#>", tidy.opts = list(width.cutoff = 100))

if (!requireNamespace("broom", quietly = TRUE) ||
    !requireNamespace("gt", quietly = TRUE) ||
    !requireNamespace("magrittr", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
} else {
  library(parameters)
  library(broom)
  library(gt)
  library(magrittr)
  library(insight)
}

set.seed(333)

## -----------------------------------------------------------------------------
data(iris)
iris$Petlen <- cut(iris$Petal.Length, breaks = c(0, 3, 7))
model <- lm(Sepal.Width ~ poly(Sepal.Length, 2) + Species + Petlen, data = iris)

summary(model)

## -----------------------------------------------------------------------------
library(parameters)
format_parameters(model)

## -----------------------------------------------------------------------------
cat(format_parameters(model), sep = "\n")

## -----------------------------------------------------------------------------
colnames(model_parameters(model))

## -----------------------------------------------------------------------------
library(broom)
colnames(tidy(model))

## -----------------------------------------------------------------------------
library(insight)
library(magrittr)
model %>% 
  model_parameters() %>% 
  standardize_names() %>% 
  colnames()

## -----------------------------------------------------------------------------
model %>% 
  model_parameters() %>% 
  standardize_names(style = "broom") %>% 
  colnames()

## -----------------------------------------------------------------------------
cbind(summary(model)$coefficients, confint(model))

## -----------------------------------------------------------------------------
tidy(model, conf.int = TRUE)

## -----------------------------------------------------------------------------
model %>% 
  tidy(conf.int = TRUE) %>% 
  format_table()

## -----------------------------------------------------------------------------
model %>% 
  model_parameters() %>% 
  format_table()

## -----------------------------------------------------------------------------
data(mtcars)
cat(export_table(mtcars[1:8, 1:5]))

## -----------------------------------------------------------------------------
model %>% 
  tidy(conf.int = TRUE) %>% 
  format_table() %>% 
  export_table() %>% 
  cat()

## -----------------------------------------------------------------------------
model_parameters(model)

## -----------------------------------------------------------------------------
model %>% 
  tidy(conf.int = TRUE) %>% 
  # parenthesis look better in markdown-tables, so we use "brackets" here
  format_table(ci_brackets = c("(", ")")) %>% 
  export_table(format = "markdown", caption = "My Table", align = "lcccrr")

## -----------------------------------------------------------------------------
model_parameters(model) %>% print_md()

## -----------------------------------------------------------------------------
model_parameters(model) %>% print_html()

## -----------------------------------------------------------------------------
model_parameters(model) %>% display(format = "html")

