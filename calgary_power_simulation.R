library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(jsonlite)


d <- read_csv("data/calgary-means-sd.csv") |>
    na.omit()

# I use this to split up the data and sample down to 300 concrete and 300
# abstract words. May not be needed in practice.
breaks <- d |>
    filter(WordType == "Concrete") |>
    _$RTclean_mean |>
    quantile(probs = seq(0, 1, 1/3))

x <- list(
    Concrete = filter(d, WordType == "Concrete") |>
        mutate(
            cond = cut(RTclean_mean, breaks, labels = c("low", "med", "high"))
        ) |>
        na.omit() |>
        group_by(cond) |>
        slice_sample(n = 100) |>
        ungroup(),
    Abstract = filter(d, WordType == "Abstract") |>
        mutate(
            cond = factor(NA, levels = c("low", "med", "high"))
        ) |>
        slice_sample(n = 300)
)

# Simulation function ----
sim_exp <- function(x, sample_size) {
    m <- imap(x, function(df, type, sample_size) {
        replicate(
            sample_size,
            rnorm(nrow(df), df$RTclean_mean, df$RTclean_sd)
        ) |>
            apply(2, function(M) {
                if (type == "Abstract") {
                    mean(M)
                } else {
                    tapply(M, INDEX = df$cond, FUN = mean)
                }
            })
    }, sample_size = sample_size)

    dlst <- map(m, ~{
        expand.grid(
            cond = if(is.matrix(.x)) as.factor(row.names(.x)) else "abs",
            subj = seq_len(sample_size)
        ) |>
            mutate(rt = c(.x))
    })
    d <- dlst$Concrete |>
        left_join(select(dlst$Abstract, subj, rt_abs = rt), by = "subj") |>
        mutate(rt_diff = rt - rt_abs)

    a <- summary(aov(rt_diff ~ cond, data = d))[[1]]
    tt <- map(split(d, d$cond), ~{
        t.test(.x$rt_diff)
    })
    tpw <- list(
        low_med = t.test(d$rt[d$cond == "low"], d$rt[d$cond == "med"]),
        low_high = t.test(d$rt[d$cond == "low"], d$rt[d$cond == "high"]),
        med_high = t.test(d$rt[d$cond == "med"], d$rt[d$cond == "high"])
    )
    tibble(
        sample_size = sample_size,
        cond_Fval = a[["F value"]][1],
        cond_pval = a[["Pr(>F)"]][1],
        low_abs_tval = tt$low[["statistic"]],
        low_abs_pval = tt$low[["p.value"]],
        med_abs_tval = tt$med[["statistic"]],
        med_abs_pval = tt$med[["p.value"]],
        high_abs_tval = tt$high[["statistic"]],
        high_abs_pval = tt$high[["p.value"]],
        low_med_tval = tt$low_med[["statistic"]],
        low_med_pval = tt$low_med[["p.value"]],
        low_high_tval = tt$low_high[["statistic"]],
        low_high_pval = tt$low_high[["p.value"]],
        med_high_tval = tt$med_high[["statistic"]],
        med_high_pval = tt$med_high[["p.value"]]
    )
}


tmp <- sim_exp(x, 20)


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
