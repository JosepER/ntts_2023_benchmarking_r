# benchmarks.R
# library(tidyverse)
library(dplyr)
library(haven)
library(dineq)
library(ineq)
library(bench)
library(Hmisc)
library(wINEQ)
library(assertthat)
library(purrr)

source("/home/additional_ineq_functions.R")


benchmark_iterations <- 50 # change to a value between 30 and 100
bootstrap_n <- 1000 # change to 1000

df_raw <- haven::read_dta("/home/data/it14ih.dta")

df <- df_raw[, c("hid", "dhi", "hwgt")]

# Enlarge the DF (to 102,000 obs)
df <- purrr::map_dfr(1:102, .f = function(x, df){
    df
}, df)


# Simple estimates --------------------------------------------------------
print("Starting simple benchmarks")

# gini
benchmark_gini <- bench::mark(benchmark_gini = dineq::gini.wtd(df$dhi, df$hwgt),
    iterations=benchmark_iterations,
    time_unit="ns")

# atkinson with ϵ > 1
benchmark_atkinson_large_e <- bench::mark(benchmark_atkinson_large_e = wINEQ::Atkinson(df$dhi, 
    df$hwgt,
    e = 1.2),
    iterations=benchmark_iterations,
    time_unit="ns")

# atkinson with ϵ < 1
benchmark_atkinson_small_e <- bench::mark(benchmark_atkinson_small_e = wINEQ::Atkinson(df$dhi, 
    df$hwgt,
    e = 0.8),
    iterations=benchmark_iterations,
    time_unit="ns")

# Fgt
benchmark_fgt <- bench::mark(benchmark_fgt = poverty_fgt(df$dhi,
    wtd.quantile(df$dhi, df$hwgt, 0.5) * 0.6,
    df$hwgt,
    alpha = 0.8),
    iterations=benchmark_iterations,
    time_unit="ns")

# headcount
benchmark_headcount <- bench::mark(benchmark_headcount = headcount(v = df$dhi, 
    z = wtd.quantile(df$dhi, df$hwgt, 0.5) * 0.6,
    w = df$hwgt),
    iterations=benchmark_iterations,
    time_unit="ns")

# poverty gap
benchmark_poverty_gap <- bench::mark(benchmark_poverty_gap = poverty_gap(v = df$dhi, 
    z = wtd.quantile(df$dhi, df$hwgt, 0.5) * 0.6,
    w = df$hwgt),
    iterations=benchmark_iterations,
    time_unit="ns")

# watts
benchmark_watts <- bench::mark(benchmark_watts = watts(df$dhi,
                                    alpha = wtd.quantile(df$dhi, df$hwgt, 0.5) * 0.6,
                                        df$hwgt),
                                iterations = benchmark_iterations,
    time_unit="ns")

# theil
benchmark_theil <- bench::mark(benchmark_theil = dineq::theil.wtd(df$dhi,
                                        df$hwgt),
                                iterations = benchmark_iterations,
    time_unit="ns")

# mld
benchmark_mld <- bench::mark(benchmark_mld = dineq::mld.wtd(df$dhi,
                                        df$hwgt),
                              iterations = benchmark_iterations,
    time_unit="ns")


# Estimates with bootstrap ------------------------------------------------

print("Starting benchmarks with bootstrap")

print("Bootstrap Gini")
# gini
benchmark_bootstrap_gini <- bench::mark(benchmark_bootstrap_gini = bootstrap(df, bootstrap_n) %>%
    purrr::map_dbl(~gini.wtd(.x$dhi, .x$hwgt)),
                              iterations = benchmark_iterations,
    time_unit="ns")

print("Bootstrap Atkinsons")
# atkinson with ϵ > 1
benchmark_bootstrap_atkinson_large_e <- bench::mark(benchmark_bootstrap_atkinson_large_e = bootstrap(df, bootstrap_n) %>%
    purrr::map_dbl(~wINEQ::Atkinson(.x$dhi, .x$hwgt, e = 1.2)),
                              iterations = benchmark_iterations,
    time_unit="ns")

# atkinson with ϵ < 1
benchmark_bootstrap_atkinson_small_e <- bench::mark(benchmark_bootstrap_atkinson_small_e = bootstrap(df, bootstrap_n) %>%
    purrr::map_dbl(~wINEQ::Atkinson(.x$dhi, .x$hwgt, e = 0.8)),
                                iterations=benchmark_iterations,
    time_unit="ns")

print("Bootstrap FGT")
# Fgt
benchmark_bootstrap_fgt <- bench::mark(benchmark_bootstrap_fgt = bootstrap(df, bootstrap_n) %>%
    purrr::map_dbl(~poverty_fgt(df$dhi,
        wtd.quantile(df$dhi, df$hwgt, 0.5) * 0.6,
        df$hwgt,
        alpha = 0.8)),
    iterations = benchmark_iterations,
    time_unit="ns")

print("Bootstrap headcount")
# headcount
benchmark_bootstrap_headcount <- bench::mark(benchmark_bootstrap_headcount = bootstrap(df, bootstrap_n) %>%
    purrr::map_dbl(~headcount(v = df$dhi,
        z = wtd.quantile(df$dhi, df$hwgt, 0.5) * 0.6,
        w = df$hwgt)),
    iterations = benchmark_iterations,
    time_unit="ns")

print("Bootstrap P Gap")
# poverty gap
benchmark_bootstrap_poverty_gap <- bench::mark(benchmark_bootstrap_poverty_gap = bootstrap(df, bootstrap_n) %>%
    purrr::map_dbl(~poverty_gap(v = df$dhi,
        z = wtd.quantile(df$dhi, df$hwgt, 0.5) * 0.6,
        w = df$hwgt)),
    iterations = benchmark_iterations,
    time_unit="ns")

print("Bootstrap Watts")
# watts
benchmark_bootstrap_watts <- bench::mark(benchmark_bootstrap_watts = bootstrap(df, bootstrap_n) %>%
    purrr::map_dbl(~watts(v = df$dhi,
        alpha = wtd.quantile(df$dhi, df$hwgt, 0.5) * 0.6,
        w = df$hwgt)),
    iterations = benchmark_iterations,
    time_unit="ns")

print("Bootstrap Theil")
# theil
benchmark_bootstrap_theil <- bench::mark(benchmark_bootstrap_theil = bootstrap(df, bootstrap_n) %>%
    purrr::map_dbl(~dineq::theil.wtd(x = df$dhi,
        weights = df$hwgt)),
    iterations = benchmark_iterations,
    time_unit="ns")

print("Bootstrap Mld")
# mld
benchmark_bootstrap_mld <- bench::mark(benchmark_bootstrap_mld = bootstrap(df, bootstrap_n) %>%
    purrr::map_dbl(~dineq::mld.wtd(x = df$dhi,
        weights = df$hwgt)),
    iterations = benchmark_iterations,
    time_unit="ns")


# By groups ---------------------------------------------------------------

df_group <- df_raw[,c("hid", "dhi", "hhtype", "nhhmem17", "hwgt")]

# Enlarge the DF (to 102,000 obs)
df_group <- purrr::map_dfr(1:102, .f = function(x, df_group){
    df_group
}, df_group)

# ** recode hhtype

# 100 = 1
# 210 = 2
# 220 and ageyoch_less18 = 3
# 230 and ageyoch_less18 = 4
# between(220, 930) = 5
# hhtype 220 and not ageyoch_less18 = 5
# hhtype 230 and not ageyoch_less18 = 5

df_group <- df_group %>%
    dplyr::mutate(htype = dplyr::case_when(hhtype == 100 ~ 1,
                                           hhtype == 210 ~ 2,
                                           (hhtype == 220) & nhhmem17 > 0 ~ 3,
                                           (hhtype == 230) & nhhmem17 > 0 ~ 4,
                                           (hhtype %in% c(220, 230)) & nhhmem17 == 0 ~ 5,
                                           dplyr::between(hhtype, 231, 930) ~ 5,
                                            TRUE ~ 6))

df_group <- df_group %>%
    dplyr::select(hid, dhi, hwgt, htype)

# grouped mld

benchmark_bootstrap_grouped_mld <- bench::mark(benchmark_bootstrap_grouped_mld = bootstrap(df_group, bootstrap_n) %>%
    purrr::map(~df_group %>%
                    group_by(htype) %>%
                    dplyr::summarise(estimate = dineq::mld.wtd(dhi, hwgt))),
    iterations = benchmark_iterations,
    time_unit="ns")


benchmarks <- bind_rows(benchmark_gini,
    benchmark_atkinson_large_e,
    benchmark_atkinson_small_e,
    benchmark_fgt,
    benchmark_headcount,
    benchmark_poverty_gap,
    benchmark_watts,
    benchmark_theil,
    benchmark_mld,
    benchmark_bootstrap_gini,
    benchmark_bootstrap_atkinson_large_e,
    benchmark_bootstrap_atkinson_small_e,
    benchmark_bootstrap_fgt,
    benchmark_bootstrap_headcount,
    benchmark_bootstrap_poverty_gap,
    benchmark_bootstrap_watts,
    benchmark_bootstrap_theil,
    benchmark_bootstrap_mld,
    benchmark_bootstrap_grouped_mld)

readr::write_csv(benchmarks, "/home/outputs/benchmarks_R.csv")
