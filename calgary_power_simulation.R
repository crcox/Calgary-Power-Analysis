library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(jsonlite)


d <- read_csv("data/calgary-means-sd.csv") |>
    na.omit()

# I use this to split up the data and sample down to 300 concrete and 300
# abstract words. May not be needed in practice.
g <- group_by(d, WordType)

x <- map(group_split(g), ~{
    breaks <- quantile(.x$RTclean_mean, probs = seq(0, 1, 1/3))
    .x |>
        mutate(
            cond = cut(RTclean_mean, breaks, labels = c("low", "med", "high"))
        ) |>
        na.omit() |>
        group_by(cond) |>
        slice_sample(n = 100)
})

names(x) <- group_keys(g)$WordType

# Simulation function ----
sim_exp <- function(x, n_subj) {
    m <- map(x, function(df, n_subj) {
        replicate(
            n_subj,
            rnorm(300, df$RTclean_mean, df$RTclean_sd)
        ) |>
            apply(2, function(M) {
                tapply(M, INDEX = df$cond, FUN = mean)
            })
    }, n_subj = n_subj)

    d <- map(m, ~{
        expand.grid(
            cond = as.factor(row.names(.x)),
            subj = seq_len(n_subj)
        ) |>
            mutate(rt = c(.x))
    }) |>
        list_rbind(names_to = "WordType") |>
        mutate(WordType = as.factor(WordType))

    a <- summary(aov(rt ~ cond * WordType, data = d))[[1]]
    tibble(
        sample_size = n_subj,
        cond_Fval = a[["F value"]][1],
        type_Fval = a[["F value"]][2],
        inter_Fval = a[["F value"]][3],
        cond_pval = a[["Pr(>F)"]][1],
        type_pval = a[["Pr(>F)"]][2],
        inter_pval = a[["Pr(>F)"]][3]
    )
}


# Run simulation for a single sample size ----
# On my desktop, 10,000 iterations takes ~2 minutes
# So, however many sample sizes you want to run the simulation for, it will
# take twice that many minutes, give or take.
# I left the `system.time()` function in place so you can time it yourself.
sim_many_exp <- function(niter, sample_size, x) {
    map(seq_len(niter), function(iter, sample_size, x) {
        sim_exp(x, sample_size)
    }, sample_size = sample_size, x = x) |>
        list_rbind(names_to = "iter")
}

system.time(result <- sim_many_exp(niter = 100, sample_size = 50, x))


# Run simulation for multiple sample sizes ----
n_iterations <- 100
sample_sizes <- seq(10, 100, by = 10)
all_sim_results <- map(sample_sizes, sim_many_exp, niter = n_iterations, x = x) |>
    list_rbind()

tstamp <- format.Date(Sys.time(), "%Y%m%d-%H%M%S")
saveRDS(all_sim_results, file = paste0(tstamp, "_simulation.rds"))
jsonlite::write_json(
    list(niter = n_iterations, sample_sizes = sample_sizes),
    path = paste0(tstamp, "_simulation.json")
)
