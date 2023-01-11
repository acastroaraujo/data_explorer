
# helper functions --------------------------------------------------------

wrap_fct_levels <- function(x, width = 15) {
  stopifnot(class(x) == "factor")
  levels(x) <- str_wrap(levels(x), width = 15)
  return(x)
}

table_plot_message <- function() {  ## not in use yet
  
  message("This table shows row percentages")
  message("Read: X% of «variable 1» belongs in «variable 2»")
  
}

# histogram ---------------------------------------------------------------

custom_histogram_numeric <- function(d, var1, type, survey_weights) {
  stopifnot(length(type) == 1)
  force(var1)
  survey_weights <- as.logical(survey_weights)
  
  if (isTRUE(survey_weights)) {
    out <- d |> 
      ggplot(aes({{var1}}, weight = WEIGHT)) + 
      geom_histogram(color = "white")
  } else {
    out <- d |> 
      ggplot(aes({{var1}})) + 
      geom_histogram(color = "white")
  }
  return(out)
}

# custom_histogram_numeric(dsub(), var1, type, input$survey_weights)

## not really a "histogram"
custom_histogram_categorical <- function(d, var1, type, survey_weights) {
  stopifnot(length(type) == 1)
  force(var1)
  survey_weights <- as.logical(survey_weights)
  
  if (isTRUE(survey_weights)) {
    d <- d |> 
      count({{var1}}, wt = WEIGHT) |> 
      mutate(prop = n / sum(n)) 
  } else {
    d <- d |> 
      count({{var1}}) |> 
      mutate(prop = n / sum(n)) 
  }
  
  d |> 
    ggplot(aes({{var1}}, prop)) + 
    geom_col(width = 1/2) +
    scale_y_continuous(label = scales::percent) +
    labs(x = as.character(var1)) + 
    coord_flip()
}


# table plot --------------------------------------------------------------

custom_table_plot_2 <- function(d, var1, var2, type, survey_weights) {
  stopifnot(length(type) == 2)
  force(var1); force(var2)
  survey_weights <- as.logical(survey_weights)
  
  if (isTRUE(survey_weights)) {
    d <- d |> 
      count({{var1}}, {{var2}}, wt = WEIGHT) |> 
      group_by({{var1}}) |> 
      mutate(prop = n / sum(n)) 
  } else {
    d <- d |> 
      count({{var1}}, {{var2}}) |> 
      group_by({{var1}}) |> 
      mutate(prop = n / sum(n)) 
  }
  
  d |> 
    ggplot(aes({{var1}}, {{var2}}, fill = prop)) + 
    geom_tile(color = "white", show.legend = FALSE) + 
    geom_text(aes(label = scales::percent(prop, accuracy = 0.2), color = after_scale(
      prismatic::best_contrast(fill, c("white", "black")))
    ), size = 3) + 
    scale_fill_distiller(direction = 1, palette = "Greys") +
    labs(caption = "\nThis table shows row percentages\n\nRead: X% of «variable 1» belongs in «variable 2»") +
    theme(panel.grid = element_blank())
  
}

custom_table_plot_3 <- function(d, var1, var2, var3, type, survey_weights) {
  stopifnot(length(type) == 3)
  force(var1); force(var2); force(var3)
  survey_weights <- as.logical(survey_weights)
  
  if (isTRUE(survey_weights)) {
    d <- d |> 
      count({{var1}}, {{var2}}, {{var3}}, wt = WEIGHT) |> 
      group_by({{var1}}, {{var3}}) |> 
      mutate(prop = n / sum(n)) 
  } else {
    d <- d |> 
      count({{var1}}, {{var2}}, {{var3}}) |> 
      group_by({{var1}}, {{var3}}) |> 
      mutate(prop = n / sum(n)) 
  }
  
  d |> 
    ggplot(aes({{var1}}, {{var2}}, fill = prop)) + 
    geom_tile(color = "white", show.legend = FALSE) + 
    geom_text(aes(label = scales::percent(prop, accuracy = 0.2), color = after_scale(
      prismatic::best_contrast(fill, c("white", "black")))
    ), size = 3) + 
    scale_fill_distiller(direction = 1, palette = "Greys") +
    labs(caption = "\nThis table shows row percentages\n\nRead: X% of «variable 1» belongs in «variable 2»") +
    theme(panel.grid = element_blank()) +
    facet_wrap(var3, ncol = 2)
  
}

# scatterplot -------------------------------------------------------------

custom_scatterplot_2 <- function(d, var1, var2, type, survey_weights) {
  stopifnot(length(type) == 2)
  force(var1); force(var2)
  survey_weights <- as.logical(survey_weights)
  
  if (isTRUE(survey_weights)) {
    out <- d |> 
      ggplot(aes({{var1}}, {{var2}}, weight = WEIGHT)) + 
      geom_jitter(alpha = 1/3) +
      geom_smooth(se = TRUE)
  } else {
    out <- d |> 
      ggplot(aes({{var1}}, {{var2}})) + 
      geom_jitter(alpha = 1/3) +
      geom_smooth(se = TRUE)
  }
  return(out)
}

custom_scatterplot_3 <- function(df, var1, var2, var3, type, survey_weights) {
  stopifnot(length(type) == 3)
  force(var1); force(var2); force(var3)
  survey_weights <- as.logical(survey_weights)
  
  if (isTRUE(survey_weights)) {
    out <- df |> 
      ggplot(aes({{var1}}, {{var2}}, weight = WEIGHT)) + 
      geom_jitter(alpha = 1/4) +
      geom_smooth(aes(color = {{var3}}, fill = {{var3}}), se = TRUE)
  } else {
    out <- df |> 
      ggplot(aes({{var1}}, {{var2}})) + 
      geom_jitter(alpha = 1/4) +
      geom_smooth(aes(color = {{var3}}, fill = {{var3}}), se = TRUE)
  }
  return(out)
}


# diff-in-means plot ------------------------------------------------------

get_orientation <- function(d, var1, var2) {
  
  out <- map_chr(d[as.character(c(var1, var2))], class)
  
  if (out[[1]] == "factor" & out[[2]] == "numeric") {
    return("vertical")
  }
  
  if (out[[1]] == "numeric" & out[[2]] == "factor") {
    return("horizontal")
  }
  
}

diff_means_data_weighted_2 <- function(d, var1, var2) {
  
  if (get_orientation(d, var1, var2) == "vertical") {
    var_num <- var2
    var_cat <- var1
  }

  if (get_orientation(d, var1, var2) == "horizontal") {
    var_num <- var1
    var_cat <- var2
  }
  
  f <- reformulate(as.character(var_cat), as.character(var_num), intercept = FALSE)
  ols <- lm(f, data = d, weights = WEIGHT)
  
  new_df <- tidyr::expand_grid({{var_cat}} := unique(d[[var_cat]])) ## change dsub to d
  pred <- predict(ols, newdata = new_df, se.fit = TRUE)
  new_df[[var_num]] <- pred$fit
  new_df$se <- pred$se.fit
  
  new_df <- new_df |> 
    mutate(upper = {{var_num}} + 1.96*se, lower = {{var_num}} - 1.96*se) 
  
  return(new_df)
  
}

diff_means_data_weighted_3 <- function(d, var1, var2, var3) {
  
  if (get_orientation(d, var1, var2) == "vertical") {
    var_num <- var2
    var_cat <- var1
  }
  
  if (get_orientation(d, var1, var2) == "horizontal") {
    var_num <- var1
    var_cat <- var2
  }
  
  var_facet <- var3
  
  f <- reformulate(as.character(c(var_cat, var_facet)), as.character(var_num), intercept = FALSE)
  ols <- lm(f, data = d, weights = WEIGHT)
  
  new_df <- tidyr::expand_grid({{var_cat}} := unique(d[[var_cat]]), {{var_facet}} := unique(d[[var_facet]]))
  pred <- predict(ols, newdata = new_df, se.fit = TRUE)
  new_df[[var_num]] <- pred$fit
  new_df$se <- pred$se.fit
  
  new_df <- new_df |> 
    mutate(upper = {{var_num}} + 1.96*se, lower = {{var_num}} - 1.96*se) 
  
  return(new_df)
  
}


custom_diff_means_plot_2 <- function(d, var1, var2, type, survey_weights) {
  stopifnot(length(type) == 2)
  force(var1); force(var2)
  survey_weights <- as.logical(survey_weights)
  
  if (isTRUE(survey_weights)) {
    
    if (get_orientation(d, var1, var2) == "horizontal") {
      out <- diff_means_data_weighted_2(d, var1, var2) |> 
        ggplot(aes({{var1}}, {{var2}})) + 
        geom_pointrange(aes(xmin = lower, xmax = upper))
    }
    
    if (get_orientation(d, var1, var2) == "vertical") {
      out <- diff_means_data_weighted_2(d, var1, var2) |> 
        ggplot(aes({{var1}}, {{var2}})) + 
        geom_pointrange(aes(ymin = lower, ymax = upper))
    }
    
  } else {
    out <- d |> 
      ggplot(aes({{var1}}, {{var2}})) + 
      stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96)) 
  }
  return(out)
}


custom_diff_means_plot_3 <- function(d, var1, var2, var3, type, survey_weights) {
  stopifnot(length(type) == 3)
  force(var1); force(var2); force(var3)
  survey_weights <- as.logical(survey_weights)
  
  if (get_orientation(d, var1, var2) == "vertical") {
    var_num <- var2
    var_cat <- var1
  }
  
  if (get_orientation(d, var1, var2) == "horizontal") {
    var_num <- var1
    var_cat <- var2
  }
  
  var_facet <- var3
  
  if (isTRUE(survey_weights)) {
    
    if (get_orientation(d, var1, var2) == "horizontal") {
      out <- diff_means_data_weighted_3(d, var1, var2, var3) |> 
        ggplot(aes({{var1}}, {{var2}})) + 
        geom_pointrange(aes(xmin = lower, xmax = upper)) +
        facet_wrap(var3)
    }
    
    if (get_orientation(d, var1, var2) == "vertical") {
      out <- diff_means_data_weighted_3(d, var1, var2, var3) |> 
        ggplot(aes({{var1}}, {{var2}})) + 
        geom_pointrange(aes(ymin = lower, ymax = upper)) + 
        facet_wrap(var3)
    }
    
  } else {
    out <- d |> 
      ggplot(aes({{var1}}, {{var2}})) + 
      stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96)) +
      facet_wrap(var3)
  }
  
  return(out)
  
}




