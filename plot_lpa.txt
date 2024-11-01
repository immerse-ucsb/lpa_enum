
plot_lpa <-
  function(model_name) {
    library(MplusAutomation)
    library(tidyverse)
    library(reshape2)
    library(cowplot)
    library(glue)
    
    
    
    pp_plots <-
      data.frame(model_name$parameters$unstandardized) %>%
      mutate(LatentClass = sub("^", "Class ", LatentClass)) %>%
      filter(paramHeader == "Means") %>%
      filter(LatentClass != "Class Categorical.Latent.Variables") %>%
      dplyr::select(est, LatentClass, param) %>%
      pivot_wider(names_from = LatentClass, values_from = est) %>%
      relocate(param, .after = last_col())
    
    c_size <-
      as.data.frame(model_name$class_counts$modelEstimated$proportion) %>%
      dplyr::rename("cs" = 1) %>%
      mutate(cs = round(cs * 100, 2))
    
    colnames(pp_plots) <-
      paste0(colnames(pp_plots[, 1:ncol(pp_plots) - 1]),
             glue(" ({c_size[1:ncol(pp_plots)-1,]}%)"))
    
    plot_data <- pp_plots %>%
      dplyr::rename("param" = ncol(pp_plots)) %>%
      reshape2::melt(id.vars = "param") %>% 
      mutate(param = fct_inorder(param))
    
    name <- str_to_title(model_name$input$title) 
    
    p <- plot_data %>%
      ggplot(
        aes(
          x = param,
          y = value,
          shape = variable,
          colour = variable,
          lty = variable,
          group = variable
        )
      ) +
      geom_point(size = 4) + geom_line() +
      scale_x_discrete("") +
      scale_colour_grey(end = .3) +
      labs(title = glue("{name} Profile Plot"), y = "Means") +
      theme_cowplot() +
      theme(
        text = element_text(family = "serif", size = 12),
        legend.key.width = unit(.5, "line"),
        legend.text = element_text(family = "serif", size = 12),
        legend.title = element_blank(),
        legend.position = "top"
      )
    p
    return(p)
  }