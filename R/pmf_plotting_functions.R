#' Function to plot PMF factor profiles using data from 
#' \code{\link{read_pmf_factor_profiles}}. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Tibble from \code{\link{read_pmf_factor_profiles}}. 
#' 
#' @param by_model_run Should the plots be faceted by model runs? 
#' 
#' @return ggplot2 with bar geometries. 
#' 
#' @export
plot_pmf_factor_profile <- function(df, by_model_run = TRUE) {
  
  # Filter to percentage
  df <- filter(df, factor_profile == "percentage_of_species_sum")
  
  # Build plot
  plot <- df %>% 
    ggplot(aes(species, value, fill = factor)) +
    geom_bar(stat = "identity") + 
    theme_minimal() + 
    ylab("Species contribution (%)") + 
    xlab("Species")
  
  # Facet
  if (by_model_run) {
    plot <- plot + facet_wrap("model_run")
  }
  
  return(plot)
  
}


#' Function to plot PMF factor contributions using data from 
#' \code{\link{read_pmf_factor_profiles}}. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Tibble from \code{\link{read_pmf_factor_profiles}}. 
#' 
#' @param by_model_run Should the plots be faceted by model runs? 
#' 
#' @return ggplot2 with bar geometries. 
#' 
#' @export
plot_pmf_factor_contributions <- function(df, by_model_run = TRUE) {
  
  # Filter to concentrations
  df <- df %>% 
    filter(factor_profile == "concentration_of_species") %>% 
    group_by(model_run,
             factor_profile,
             factor) %>% 
    summarise(mean = mean(value),
              sum = sum(value)) %>% 
    ungroup() %>% 
    mutate(contribution = sum / sum(sum),
           contribution_percent = contribution * 100,
           label = round(contribution_percent, 1),
           label = stringr::str_c(label, " %"))
  
  # Stacked bar chart
  plot <- df %>% 
    ggplot(aes("", contribution_percent, fill = factor)) + 
    geom_bar(stat = "identity") + 
    geom_label(
      aes(label = label), 
      colour = "black",
      position = position_stack(vjust = 0.5),
      show.legend = FALSE
    ) + 
    theme_void() + 
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    coord_flip()
  
  # Facet
  if (by_model_run) {
    plot <- plot + facet_wrap("model_run")
  }
  
  return(plot)
  
}
