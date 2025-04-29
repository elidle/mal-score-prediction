# Imports
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(kableExtra)
library(GGally)
library(fastDummies)
library(caret)
library(ranger)
library(glmnet)
library(xgboost)
library(broom)
library(doParallel)
library(parallel)
library(plotly)

create_signif_plot <- function(model, title) {
  tidy_coef <- tidy(model$finalModel) |>
    arrange(desc(abs(estimate))) |>
    head(20) |>
    mutate(
    direction = ifelse(estimate > 0, "Positive", "Negative"),
    term = reorder(term, estimate),
    hover_text = paste(
      "Term:", term, "<br>",
      "Estimate:", round(estimate, 3), "<br>",
      "Direction:", direction, "<br>",
      "Std. Error:", round(std.error, 3), "<br>",
      "p-value:", round(p.value, 4)
    ))
      
  p <- ggplot(tidy_coef, aes(x = term, y = estimate, fill = estimate,
                             text = hover_text)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient2(
      high = "#d7191c",
      mid = "white",
      low = "#2c7bb6",
      midpoint = 0,
      name = "Effect Size"
    ) +
    coord_flip() +
    labs(
      title = title,
      x = NULL,
      y = "Coefficient Estimate"
    ) +
    theme_minimal() +
    theme(legend.position = "right")

  ggplotly(p, tooltip = "text") %>%
    layout(
      hoverlabel = list(
        bgcolor = "white",
        font = list(size = 12)
      ),
      title = list(
        text = title,
        size = 10,
        x = -1
      ),
      margin = list(l = 100)
    ) %>%
    style(hoverinfo = "text")
}

create_imp_plot <- function(model, low, high, title) {
  imp <- varImp(model, scale = FALSE)
  imp_data <- imp$importance |>
    tibble::rownames_to_column("Variable") |>
    arrange(desc(Overall)) |>
    head(20)
  
  p <- ggplot(imp_data, aes(x = reorder(Variable, Overall), y = Overall, 
                            fill = Overall, 
                            text = paste("Variable:", Variable, "<br>Importance:", Overall))) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_gradient(
      low = "#f0f9e8",
      high = "#238b45",
      name = "Importance"
    ) +
    coord_flip() +
    labs(
      title = title,
      x = NULL,
      y = "Importance Score"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(size = 12),
      axis.text.y = element_text(size = 9),
      legend.position = "right",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  ggplotly(p, tooltip = "text")
}

# For first time running: Save models to an .RData file. Upload to GitHub
# save(anime_data, anime_data_encoded, anime_data_filtered, lm_model, lm_model_filtered, glmnet_model, rf_model, xgb_model, file = "models.RData")

# For all subsequent time: Load models from .RData file for visualization
load("data/models.RData")