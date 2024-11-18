ordinal_plot <- function(data,
                         left_var = "pre",
                         left_title = "Pre",
                         left_fill_title = "Response",
                         left_x_axis_label = "Response",
                         left_y_axis_label = "",
                         center_title = "Pre-Post",
                         center_fill_title = "Response",
                         center_x_axis_label = "",
                         center_y_axis_label = "",
                         right_var = "post",
                         right_title = "Post",
                         right_fill_title = "Response",
                         right_x_axis_label = "Response",
                         right_y_axis_label = "",
                         legend.position = "bottom",
                         id_var = "id") {
  
  vars <- c(id_var, left_var, right_var)
  
  max_prop <- dat %>%
    select(all_of(vars)) %>%
    pivot_longer(cols = c(!!sym(left_var), !!sym(right_var))) %>%
    group_by(name) %>%
    count(value) %>%
    mutate(freq = n / sum(n)) %>%
    .$freq %>%
    max
  
  max_prop <- plyr::round_any(max_prop, 0.05, f = ceiling)

  p1 <- dat %>%
    group_by(!!sym(left_var)) %>%
    tally() %>%
    mutate(freq = n / sum(n),
           res = str_c(n, "\n(", round(freq * 100, 1), "%)")) %>%
    ggplot(aes(x = as.factor(!!sym(left_var)), y = freq)) +
    geom_bar(
      aes(fill = as.factor(!!sym(left_var))),
      stat = "identity",
      alpha = 0.8,
      colour = "black"
    ) +
    theme_institute(base_size = 14) +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_y_continuous(
      labels = scales::percent_format(),
      breaks = seq(0, max_prop, by = 0.05),
      expand = expansion(mult = c(0, 0.1))
    ) +
    coord_cartesian(ylim = c(0, max_prop)) +
    scale_fill_viridis_d(option = "plasma",
                         end = 0.85,
                         direction = -1) +
    labs(
      title = left_title,
      fill = left_fill_title,
      x = left_x_axis_label,
      y = left_y_axis_label
    ) +
    geom_text(aes(label = res), vjust = -0.1, family = "Barlow Semi Condensed") +
    guides(fill = guide_legend(nrow = 1))

  p2 <- dat %>%
    rename(Pre = !!sym(left_var), Post = !!sym(right_var)) %>%
    make_long(Pre, Post) %>%
    mutate(node = factor(node, levels = c(7, 6, 5, 4, 3, 2, 1)),
           next_node = factor(next_node, levels = c(7, 6, 5, 4, 3, 2, 1))) %>%
    ggplot(aes(
      x = x,
      next_x = next_x,
      node = node,
      next_node = next_node,
      fill = factor(node)
    ), alpha = 0.8) +
    geom_sankey(alpha = 0.8, node.color = 'black') +
    geom_sankey_label(
      aes(label = node),
      alpha = 0.8,
      size = 3,
      color = "black",
      fill = "gray80"
    ) +
    scale_x_discrete(expand = c(0.05, 0.05)) +
    theme_institute(base_size = 14) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = legend.position,
      plot.title = element_text(hjust = 0.5)
    ) +
    guides(fill = guide_legend(reverse = T, nrow = 1)) +
    labs(title = center_title, fill = center_fill_title, x = center_x_axis_label)

  p3 <- dat %>%
    group_by(!!sym(right_var)) %>%
    tally() %>%
    mutate(freq = n / sum(n),
           res = str_c(n, "\n(", round(freq * 100, 1), "%)")) %>%
    ggplot(aes(x = as.factor(!!sym(right_var)), y = freq)) +
    geom_bar(
      aes(fill = as.factor(!!sym(right_var))),
      stat = "identity",
      alpha = 0.8,
      colour = "black"
    ) +
    theme_institute(base_size = 14) +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      plot.background = element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_y_continuous(
      labels = scales::percent_format(),
      breaks = seq(0, max_prop, by = 0.05),
      expand = expansion(mult = c(0, 0.1))
    ) +
    coord_cartesian(ylim = c(0, max_prop)) +
    scale_fill_viridis_d(option = "plasma",
                         end = 0.85,
                         direction = -1) +
    labs(
      title = right_title,
      fill = right_fill_title,
      x = right_x_axis_label,
      y = right_y_axis_label
    ) +
    geom_text(aes(label = res), vjust = -0.1, family = "Barlow Semi Condensed") +
    guides(fill = guide_legend(nrow = 1))

  p1 + p2 + p3
}
