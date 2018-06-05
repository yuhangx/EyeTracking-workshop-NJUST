# support functions for gazelle.R analysis
#
# Anne Pier Salverda

# Add orthogonal polynomials to dataframe
add_orthogonal_polynomials = function(my_data, by_var = "", n_polynomials){
  if (by_var %in% names(my_data) == FALSE){
    stop(paste("by_var", toupper(by_var), "not found in dataframe"))
  }
  my_data$time_bins = my_data[[by_var]]
  time_bins = unique(my_data[[by_var]])
  lookup = data.frame(time_bins, time = seq(time_bins))
  my_data = merge(my_data, lookup, sort = FALSE)
  polynomials = poly(1:max(my_data$time), n_polynomials)
  my_data[, paste("op", 1:n_polynomials, sep = "")] = polynomials[my_data$time, 1:n_polynomials]
  return(my_data)
}

# Proportion of fixations, facet-wrapped
fix_prop_facet = function(fdat, by_var = "subject",
                           downsample = 20, fcondition = "exp",
                           time_onset = 0, time_offset = 1000,
                           plot_looks = c("target", "dot"),
                           return_data = FALSE, print_plot = TRUE) {
  if (by_var %in% names(fdat) == FALSE){
    stop(paste("by_var", toupper(by_var), "not found in dataframe"))
  }
  
  fdat  = fdat %>% 
    filter(condition == fcondition) %>%
    droplevels
  fdat$by_copy = fdat[[by_var]]
  
  plot_looks = unique(plot_looks)
  
  valid_looks = plot_looks %in% levels(fdat$look)
  if (sum(valid_looks) < length(plot_looks)){
    stop(paste("Look value(s)",
               paste(plot_looks[!valid_looks] %>% toupper, collapse = ", "),
               "not found in condition", fcondition %>% toupper, "\n"))
  }
  
  fdat.prop = fdat %>%
    mutate(time_rel_onset = plyr::round_any(time_rel_soundfile,
                                            downsample, f = floor)) %>%
    count(by_copy, time_rel_onset, look) %>%
    mutate(percent = n / sum(n)) %>%
    select(-n) %>%
    spread(look, percent, fill = 0) %>% 
    group_by(by_copy) %>%
    gather(look, proportion, -c(by_copy, time_rel_onset)) %>% 
    filter(look %in% plot_looks) %>% 
    mutate(look = factor(look, levels = plot_looks[valid_looks])) %>% 
    filter(time_rel_onset >= time_onset & time_rel_onset <= time_offset)
  
  facet_graph = ggplot(fdat.prop,
                       aes(x = time_rel_onset, y = proportion, color = look)) +
    geom_line(size = .5) +
    # annotate("rect", xmin = 0, xmax = 1000, ymin = 0, ymax = 1,
    #          alpha = .1, fill = "blue") +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_continuous(breaks = seq(-1000, 2000, 1000)) +
    scale_y_continuous(breaks = seq(0, 1, .5)) +
    labs(x = "Time (ms) relative to word onset",
         y = "Proportion of fixations") +
    #         title = paste("Mean proportion of fixations by", by_var)) +
    facet_wrap(~by_copy) +
    theme_bw() +
    theme(panel.margin = unit(0.8, "lines"))
  if (print_plot == TRUE) print(facet_graph)
  if (return_data == TRUE) return(fdat.prop)
}

# Mean proportion of fixations
fix_prop_mean = function(fdat, by_var = "subject",
                          downsample = 20, fcondition = "exp",
                          time_onset = 0, time_offset = 1000,
                          plot_looks = c("target", "dot")) {
  if (by_var %in% names(fdat) == FALSE){
    stop(paste("by_var", toupper(by_var), "not found in dataframe"))
  }
  
  fdat  = fdat %>% 
    filter(condition == fcondition) %>%
    droplevels
  fdat$by_copy = fdat[[by_var]]
  
  plot_looks = unique(plot_looks)
  
  valid_looks = plot_looks %in% levels(fdat$look)
  if (sum(valid_looks) < length(plot_looks)){
    stop(paste("Look value(s)",
               paste(plot_looks[!valid_looks] %>% toupper, collapse = ", "),
               "not found in condition", fcondition %>% toupper, "\n"))
  }
  
  fdat.prop = fdat %>%
    mutate(time_rel_onset = plyr::round_any(time_rel_soundfile,
                                            downsample, f = floor)) %>%
    count(by_copy, time_rel_onset, look) %>%
    mutate(percent = n / sum(n)) %>%
    select(-n) %>%
    spread(look, percent, fill = 0) %>%
    group_by(time_rel_onset) %>%
    select(-by_copy) %>% 
    gather(look, proportion, -time_rel_onset) %>% 
    group_by(time_rel_onset, look) %>% 
    summarize(mean_proportion = mean(proportion)) %>% 
    filter(look %in% plot_looks) %>% 
    mutate(look = factor(look, levels = plot_looks[valid_looks])) %>% 
    filter(time_rel_onset >= time_onset & time_rel_onset <= time_offset)
  
  ggplot(fdat.prop,
         aes(x = time_rel_onset, y = mean_proportion, color = look)) +
    geom_line(size = 1) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_continuous(breaks = seq(-1000, 2000, 200), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 1, .1), expand = c(0, 0)) +
    labs(x = "Time (ms) relative to word onset",
         y = "Proportion of fixations") +
#         title = paste("Mean proportion of fixations averaged across", by_var)) +
    theme_bw() +
    #    theme(legend.position = c(.05, .95), legend.justification = c(0, 1)) +
    theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
    theme(legend.background = element_rect(fill = "transparent", color = NA)) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "lines")) +
    theme(aspect.ratio = 1)
}

plot_model_data = function(mydat, model, mytitle){
  min_time = min(mydat$time_rel_onset)
  max_time = max(mydat$time_rel_onset)
  ggplot(mydat, aes(x = time_rel_onset, y = proportion, shape = look)) +
  stat_summary(fun.y = mean, geom = "point", size = 2) +
  scale_shape_manual(name = "look", values = c(19, 1)) +
  labs(x = "Time (ms) since word onset", y = "Proportion of fixations",
       title = mytitle) +
  scale_x_continuous(breaks = seq(min_time, max_time, 200)) +
  stat_summary(aes(y = fitted(model), linetype = look),
               fun.y = mean, geom = "line") +
  coord_cartesian(ylim = c(0, .25))
}
