# Based on
#
# False-positive psychology: Undisclosed flexibility in data collection and analysis allows presenting anything as significant
#

get_mean_woman_length <- function() { 171 }
get_sd_woman_length <- function() { 10 }

#' Create a table of `n` subjects, with half of type A and half of type B
create_subjects <- function(n_subjects, seed) {
  set.seed(seed)
  t <- tibble::tibble(
    preference = rep(c("red", "green"), times = n_subjects / 2), 
    length = rnorm(
      n = n_subjects, 
      mean = get_mean_woman_length(), 
      sd = get_sd_woman_length()
    )
  )
  t$preference <- as.factor(t$preference)
  t
}
testthat::expect_equal(create_subjects(10, 42), create_subjects(10, 42))

plot_subjects <- function(t) {
  binwidth <- 1
  xs <- seq(
    from = get_mean_woman_length() - (3.0 * get_sd_woman_length()), 
    to = get_mean_woman_length() + (3.0 * get_sd_woman_length()), 
    by = binwidth
  )
  t_ideal <- tibble::tibble(
    x = xs
  )
  t_ideal$y <- dnorm(
    t_ideal$x, 
    mean = get_mean_woman_length(), 
    sd = get_sd_woman_length()
  ) * n_subjects * binwidth / 2
  statistics <- broom::tidy(
    t.test(
      x = t[t$preference == "red", ]$length, 
      y = t[t$preference == "green", ]$length
    )
  )
  
  ggplot2::ggplot(
    t, 
    ggplot2::aes(x = length, color = preference)) + 
    ggplot2::geom_freqpoly(size = 3, binwidth = binwidth) + 
    ggplot2::geom_line(data = t_ideal, mapping = ggplot2::aes(x = x, y = y), color = "black", lty = "dashed") +
    ggplot2::scale_x_continuous(name = "Length (cm)") +
    ggplot2::scale_y_continuous(name = "Amount of women with that length") +
    ggplot2::labs(
      caption = paste0(
        "Number of women: ", n_subjects, ", p-value: ", format(statistics$p.value, digits = 3)
      )
    ) +
    ggplot2::scale_color_manual(values = c("red" = "red", "green" = "green")) +
    ggplot2::theme(text = ggplot2::element_text(size = 20))
}
plot_subjects(create_subjects(n_subjects = 1000, seed = 42))

# Create one example, 1000 women
for (n_subjects in c(1000, 100, 20)) {
  t <- create_subjects(n_subjects = n_subjects, seed = 1)
  plot_subjects(t)
  ggplot2::ggsave(paste0("distribution_", n_subjects, ".png"), width = 7, height = 7)
}



create_experiment_results <- function(n_subjects, n_seeds) {
  t_result <- tibble::tibble(seed = seq_len(n_seeds), p_value = NA)
  for (i in seq_len(n_seeds)) {
    seed <- t_result$seed[i]
    t <- create_subjects(n_subjects, seed = seed)
    p_value <- t.test(
      x = t[t$preference == "red", ]$length, 
      y = t[t$preference == "green", ]$length
    )$p.value
    t_result$p_value[i] <- p_value
  }
  t_result
}

n_subjects <- 1000
n_seeds <- 1000
t <- create_experiment_results(n_subjects = n_subjects, n_seeds = n_seeds)
n_significant <- sum(t$p_value < 0.05)
percentage_significant <- 100.0 * n_significant / n_seeds

ggplot2::ggplot(
  t, 
  ggplot2::aes(x = p_value)
) + 
  ggplot2::geom_histogram(binwidth = 0.01) + 
  ggplot2::scale_x_continuous(limits = c(0, 1), minor_breaks = seq(0.0, 1.0, by = 0.01)) + 
  ggplot2::geom_vline(xintercept = 0.05, lty = "dashed") +
  ggplot2::labs(
    caption = paste0(
      "Number of experiments: ", n_seeds, "\n",
      "Number of subjects in an experiment: ", n_subjects, "\n",
      "Significant findings: ", percentage_significant, "%"
    )
  ) +
  ggplot2::theme(text = ggplot2::element_text(size = 20))
ggplot2::ggsave(paste0("n_significant_findings_", n_subjects, "_", n_seeds, ".png"), width = 7, height = 7)


best_seed <- which(t$p_value == min(t$p_value))
t[best_seed, ]

plot_subjects(create_subjects(n_subjects = 1000, seed = best_seed))
ggplot2::ggsave(paste0("distribution_", n_subjects, "_", best_seed, ".png"), width = 7, height = 7)

