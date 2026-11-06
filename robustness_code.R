library(jsonlite)
library(dplyr)
library(tibble)
library(digest)
library(lpSolve)
library(tidyr)
library(ggplot2)


### MMLU

dir_helm <- path.expand("~/helm_mmlu")
dir_runs <- file.path(dir_helm, "runs")
dir_v1   <- file.path(dir_runs, "v1.0.0")
dir_run <- list.files(dir_v1, full.names = TRUE)
dir_run <- dir_run[file.info(dir_run)$isdir]

spec_tbl <- function(dir_one) {
  fp <- file.path(dir_one, "run_spec.json")
  spec <- read_json(fp, simplifyVector = FALSE)
  rs <- if (!is.null(spec$run_spec)) spec$run_spec else spec
  scen <- rs$scenario_spec
  ad   <- rs$adapter_spec
  subj <- "unknown_subject"
  if (!is.null(scen$args$subject)) subj <- scen$args$subject
  ds_id <- paste0("mmlu:subject=", subj)
  mdl <- "unknown_model"
  if (!is.null(ad$model)) {
    mdl <- ad$model
  } else if (!is.null(ad$model_deployment)) {
    mdl <- ad$model_deployment
  }
  tibble(
    run_dir    = basename(dir_one),
    dataset_id = ds_id,
    model      = mdl
  )
}

spec_hash <- function(dir_one) {
  fp <- file.path(dir_one, "run_spec.json")
  spec <- read_json(fp, simplifyVector = FALSE)
  rs <- if (!is.null(spec$run_spec)) spec$run_spec else spec
  if (!is.null(rs$adapter_spec$model)) rs$adapter_spec$model <- NULL
  if (!is.null(rs$adapter_spec$model_deployment)) rs$adapter_spec$model_deployment <- NULL
  digest(rs, algo = "xxhash64")
}
run_tbl <- bind_rows(lapply(dir_run, spec_tbl))
run_tbl$cfg_hash <- vapply(dir_run, spec_hash, character(1))

metric_pool <- "exact_match"

metric_tbl <- function(stats, metric_pool) {
  nm <- vapply(
    stats,
    function(x) {
      if (is.null(x$name) || is.null(x$name$name) || length(x$name$name) == 0L) {
        NA_character_
      } else {
        as.character(x$name$name[1])
      }
    },
    character(1)
  )
  mu <- vapply(
    stats,
    function(x) {
      if (is.null(x$mean) || length(x$mean) == 0L) {
        NA_real_
      } else {
        as.numeric(x$mean[1])
      }
    },
    numeric(1)
  )
  out <- tibble(metric = nm, score = mu)
  out <- out[!is.na(out$metric) &
               !is.na(out$score) &
               out$metric %in% metric_pool, ]
  out
}

fp_stats <- vapply(
  run_tbl$run_dir,
  function(rd) file.path(dir_v1, rd, "stats.json"),
  character(1)
)
stats_raw <- lapply(fp_stats, function(fp) read_json(fp, simplifyVector = FALSE))

long_raw <- bind_rows(lapply(
  seq_along(stats_raw),
  function(i) {
    mt <- metric_tbl(stats_raw[[i]], metric_pool)
    mt$dataset_id <- run_tbl$dataset_id[i]
    mt$model      <- run_tbl$model[i]
    mt$cfg_hash   <- run_tbl$cfg_hash[i]
    mt
  }
))

setting <- select(
  ungroup(
    slice_max(
      group_by(
        count(long_raw, dataset_id, model, cfg_hash, name = "n_runs"),
        dataset_id, model
      ),
      n_runs,
      n = 1,
      with_ties = FALSE
    )
  ),
  dataset_id, model, cfg_hash
)

long <- summarise(
  group_by(
    select(
      inner_join(long_raw, setting, by = c("dataset_id", "model", "cfg_hash")),
      -cfg_hash
    ),
    dataset_id, model, metric
  ),
  score = mean(score, na.rm = TRUE),
  .groups = "drop"
)

long <- long[is.finite(long$score) & !is.na(long$score), ]

dat_em <- long %>%
  filter(metric == "exact_match") %>%
  select(dataset_id, model, score)

n_tasks_total <- n_distinct(dat_em$dataset_id)

models_all_tasks <- dat_em %>%
  distinct(dataset_id, model) %>%
  count(model, name = "n_tasks") %>%
  filter(n_tasks == n_tasks_total) %>%
  pull(model)

dat_em <- dat_em %>%
  filter(model %in% models_all_tasks)
score_wide <- dat_em %>%
  tidyr::pivot_wider(
    names_from = model,
    values_from = score
  ) %>%
  arrange(dataset_id)


score_wide <- score_wide %>%
  filter(complete.cases(across(-dataset_id)))

dataset_ids <- score_wide$dataset_id

score_mat <- score_wide %>%
  select(-dataset_id) %>%
  as.matrix()

storage.mode(score_mat) <- "double"

models <- colnames(score_mat)
m <- nrow(score_mat)


stopifnot(m == n_tasks_total)
stopifnot(all(complete.cases(score_mat)))
n_tasks_balanced <- nrow(score_wide)
stopifnot(m == n_tasks_balanced)
stopifnot(is.matrix(score_mat))
stopifnot(is.double(score_mat))
stopifnot(all(is.finite(score_mat)))
stopifnot(all(score_mat >= 0))
stopifnot(all(score_mat <= 1))
stopifnot(length(models) == ncol(score_mat))
stopifnot(length(dataset_ids) == nrow(score_mat))
stopifnot(!anyDuplicated(models))
stopifnot(!anyDuplicated(dataset_ids))

score_mat_mmlu <- score_mat
dataset_ids_mmlu <- dataset_ids
models_mmlu <- models

k_mean_one_target <- function(score_mat, target_col) {
  if (!is.matrix(score_mat)) {
    score_mat <- as.matrix(score_mat)
  }
  
  if (any(!is.finite(score_mat))) {
    stop("score_mat must be a complete finite matrix.")
  }
  
  x <- score_mat[, target_col]
  m_used <- nrow(score_mat)
  
  mu_all <- colMeans(score_mat)
  mu_target <- mu_all[target_col]
  delta_mean <- max(0, max(mu_all - mu_target))
  
  G <- 1 - x
  G_sorted <- sort(G, decreasing = TRUE)
  cs <- cumsum(G_sorted)
  
  threshold <- m_used * delta_mean
  tol <- 1e-10
  
  if (threshold <= 0) {
    k_mean <- 0L
  } else if (max(cs) + tol < threshold) {
    k_mean <- Inf
  } else {
    k_mean <- which(cs + tol >= threshold)[1]
  }
  
  s <- mean(x)
  k_mean_norm <- if (is.infinite(k_mean)) {
    1.0
  } else if (k_mean == 0L || delta_mean <= 0) {
    0.0
  } else if (s >= 1.0) {
    NA_real_
  } else {
    k_mean_max <- min(ceiling(m_used * delta_mean / (1 - s)), m_used)
    if (k_mean_max == 0L) NA_real_ else k_mean / k_mean_max
  }
  
  tibble(
    model = colnames(score_mat)[target_col],
    m_used = m_used,
    delta_mean = delta_mean,
    k_mean = k_mean,
    frac_mean = if (is.infinite(k_mean)) Inf else k_mean / m_used,
    baseline_str = s, 
    k_mean_norm = k_mean_norm
  )
}

win_rate_mat <- function(score_mat) {
  m  <- nrow(score_mat)
  n  <- ncol(score_mat)
  wr <- matrix(0, nrow = m, ncol = n, dimnames = dimnames(score_mat))
  for (i in seq_len(m)) {
    row     <- score_mat[i, ]
    wr[i, ] <- vapply(row, function(sc) mean(row <= sc), numeric(1))
  }
  wr
}


k_median_one_target <- function(score_mat, target_col) {
  if (!is.matrix(score_mat)) score_mat <- as.matrix(score_mat)
  if (any(!is.finite(score_mat))) stop("score_mat must be a complete finite matrix.")
  
  x      <- score_mat[, target_col]
  m_used <- nrow(score_mat)
  s      <- mean(x)
  
  h <- if (m_used %% 2L == 1L) (m_used + 1L) %/% 2L else m_used %/% 2L + 1L
  
  competing_medians <- apply(
    score_mat[, -target_col, drop = FALSE], 2,
    function(col) sort(col)[h]
  )
  tau <- max(competing_medians)
  
  G     <- 1 - x
  N_tau <- sum(x >= tau)
  C_tau <- sum(x < tau & (x + G) >= tau)
  
  n_needed_for_median <- m_used - h + 1L
  delta_median <- max(0L, n_needed_for_median - N_tau)
  
  k_median <- if (delta_median == 0L) {
    0L
  } else if (C_tau >= delta_median) {
    delta_median
  } else {
    Inf
  }
  
  k_median_norm <- if (is.infinite(k_median)) {
    1.0
  } else if (k_median == 0L) {
    0.0
  } else if (C_tau == 0L) {
    1.0
  } else {
    k_median / C_tau
  }
  
  tibble(
    model         = colnames(score_mat)[target_col],
    m_used        = m_used,
    baseline_str  = s,
    h             = h,
    tau           = tau,
    N_tau         = N_tau,
    C_tau         = C_tau,
    delta_median  = delta_median,
    k_median      = k_median,
    frac_median   = if (is.infinite(k_median)) Inf else k_median / m_used,
    k_median_norm = k_median_norm
  )
}

q_mat_one_target <- function(score_mat, target_col, wr0) {
  m_used    <- nrow(score_mat)
  n_models  <- ncol(score_mat)
  comp_cols <- setdiff(seq_len(n_models), target_col)
  
  q_mat <- matrix(
    0, nrow = m_used, ncol = length(comp_cols),
    dimnames = list(NULL, colnames(score_mat)[comp_cols])
  )
  for (di in seq_len(m_used)) {
    row_t             <- score_mat[di, ]
    row_t[target_col] <- 1.0
    wr_t_row          <- vapply(row_t, function(sc) mean(row_t <= sc), numeric(1))
    gain_A1           <- wr_t_row[target_col] - wr0[di, target_col]
    loss_A            <- wr0[di, comp_cols]    - wr_t_row[comp_cols]
    q_mat[di, ]       <- gain_A1 + loss_A
  }
  q_mat
}

k_win_pair_one_target <- function(score_mat, target_col,
                                  wr0 = NULL, w_mean0 = NULL) {
  if (!is.matrix(score_mat)) score_mat <- as.matrix(score_mat)
  if (any(!is.finite(score_mat))) stop("score_mat must be a complete finite matrix.")
  
  if (is.null(wr0)) wr0 <- win_rate_mat(score_mat)
  if (is.null(w_mean0)) w_mean0 <- colMeans(wr0)
  
  m_used    <- nrow(score_mat)
  n_models  <- ncol(score_mat)
  comp_cols <- setdiff(seq_len(n_models), target_col)
  x         <- score_mat[, target_col]
  s         <- mean(x)
  
  w_target <- w_mean0[target_col]
  d_win    <- pmax(0, w_mean0[comp_cols] - w_target)
  names(d_win) <- colnames(score_mat)[comp_cols]
  
  q_mat <- q_mat_one_target(score_mat, target_col, wr0)
  
  k_per_comp <- vapply(seq_along(comp_cols), function(ci) {
    dA <- d_win[ci]
    if (dA <= 0) return(0)
    q_sorted  <- sort(q_mat[, ci], decreasing = TRUE)
    cs        <- cumsum(q_sorted)
    threshold <- m_used * dA
    tol       <- 1e-10
    if (max(cs) + tol < threshold) return(Inf)
    which(cs + tol >= threshold)[1L]
  }, numeric(1))
  names(k_per_comp) <- colnames(score_mat)[comp_cols]
  
  k_win <- if (all(d_win <= 0)) 0L else max(k_per_comp)
  
  hardest <- if (k_win == 0 || all(d_win <= 0)) {
    NA_character_
  } else {
    names(which.max(k_per_comp))
  }
  
  k_win_norm <- if (is.infinite(k_win)) {
    1.0
  } else if (k_win == 0L || is.na(hardest)) {
    0.0
  } else {
    q_bar     <- mean(q_mat[, hardest])
    max_d     <- d_win[hardest]
    k_win_max <- if (q_bar <= 0) Inf else min(ceiling(m_used * max_d / q_bar), m_used)
    if (is.infinite(k_win_max) || k_win_max == 0L) NA_real_ else k_win / k_win_max
  }
  
  tibble(
    model        = colnames(score_mat)[target_col],
    m_used       = m_used,
    baseline_str = s,
    w_target     = w_target,
    max_d_win    = max(d_win),
    hardest_comp = hardest,
    k_win        = k_win,
    frac_win     = if (is.infinite(k_win)) Inf else k_win / m_used,
    k_win_norm   = k_win_norm
  )
}

k_win_global_one_target <- function(score_mat, target_col,
                                    wr0 = NULL, w_mean0 = NULL) {
  if (!is.matrix(score_mat)) score_mat <- as.matrix(score_mat)
  if (any(!is.finite(score_mat))) stop("score_mat must be a complete finite matrix.")
  
  if (is.null(wr0))     wr0     <- win_rate_mat(score_mat)
  if (is.null(w_mean0)) w_mean0 <- colMeans(wr0)
  
  m_used    <- nrow(score_mat)
  n_models  <- ncol(score_mat)
  comp_cols <- setdiff(seq_len(n_models), target_col)
  
  w_target <- w_mean0[target_col]
  d_win    <- pmax(0, w_mean0[comp_cols] - w_target)
  names(d_win) <- colnames(score_mat)[comp_cols]
  
  if (all(d_win <= 0)) {
    return(tibble(
      model             = colnames(score_mat)[target_col],
      m_used            = m_used,
      k_win_global      = 0L,
      frac_win_global   = 0,
      k_win_global_norm = 0.0,
      n_active          = 0L,
      lp_status         = 0L
    ))
  }
  
  q_mat  <- q_mat_one_target(score_mat, target_col, wr0)
  active <- which(d_win > 0)
  A_mat  <- t(q_mat[, active, drop = FALSE])
  rhs    <- m_used * d_win[active]
  
  res <- lpSolve::lp(
    direction    = "min",
    objective.in = rep(1, m_used),
    const.mat    = A_mat,
    const.dir    = rep(">=", nrow(A_mat)),
    const.rhs    = rhs,
    all.bin      = TRUE
  )
  
  if (res$status != 0L) stop("Integer program failed; status: ", res$status)
  k_win_global <- as.integer(round(res$objval))
  
  k_win_global_norm <- if (is.infinite(k_win_global)) {
    1.0
  } else if (k_win_global == 0L) {
    0.0
  } else {
    hardest <- names(which.max(d_win[active]))
    q_bar   <- mean(q_mat[, hardest])
    max_d   <- d_win[hardest]
    k_max   <- if (q_bar <= 0) Inf
    else min(ceiling(m_used * max_d / q_bar), m_used)
    if (is.infinite(k_max) || k_max == 0L) NA_real_ else k_win_global / k_max
  }
  
  tibble(
    model             = colnames(score_mat)[target_col],
    m_used            = m_used,
    k_win_global      = k_win_global,
    frac_win_global   = if (is.infinite(k_win_global)) Inf else k_win_global / m_used,
    k_win_global_norm = k_win_global_norm,
    n_active          = length(active),
    lp_status         = res$status
  )
}

maj_parts <- function(score_mat, target_col) {
  if (!is.matrix(score_mat)) score_mat <- as.matrix(score_mat)
  if (any(!is.finite(score_mat))) stop("score_mat must be a complete finite matrix.")
  
  m_used <- nrow(score_mat)
  n_models <- ncol(score_mat)
  comp_cols <- setdiff(seq_len(n_models), target_col)
  mu <- ceiling(m_used / 2)
  
  loss_mat <- score_mat[, comp_cols, drop = FALSE] > score_mat[, target_col]
  colnames(loss_mat) <- colnames(score_mat)[comp_cols]
  
  n_loss <- colSums(loss_mat)
  delta_maj <- as.integer(pmax(0, mu - (m_used - n_loss)))
  names(delta_maj) <- colnames(score_mat)[comp_cols]
  
  list(comp_cols = comp_cols, mu = mu, loss_mat = loss_mat, delta_maj = delta_maj)
}

k_maj_pair_one_target <- function(score_mat, target_col) {
  z      <- maj_parts(score_mat, target_col)
  m_used <- nrow(score_mat)
  
  k_maj_pair <- max(z$delta_maj)
  hardest    <- if (k_maj_pair == 0L) NA_character_ else names(which.max(z$delta_maj))
  
  k_maj_pair_norm <- if (k_maj_pair == 0L) {
    0.0
  } else {
    L0_hardest <- sum(z$loss_mat[, hardest])
    if (L0_hardest == 0L) 1.0 else k_maj_pair / L0_hardest
  }
  
  tibble(
    model           = colnames(score_mat)[target_col],
    m_used          = m_used,
    mu              = z$mu,
    hardest_maj     = hardest,
    k_maj_pair      = k_maj_pair,
    frac_maj_pair   = k_maj_pair / m_used,
    k_maj_pair_norm = k_maj_pair_norm
  )
}

k_maj_global_one_target <- function(score_mat, target_col) {
  z      <- maj_parts(score_mat, target_col)
  m_used <- nrow(score_mat)
  
  active <- which(z$delta_maj > 0)
  
  if (length(active) == 0L) {
    return(tibble(
      model             = colnames(score_mat)[target_col],
      m_used            = m_used,
      k_maj_global      = 0L,
      frac_maj_global   = 0,
      k_maj_global_norm = 0.0,
      n_maj_active      = 0L,
      maj_status        = 0L
    ))
  }
  
  A_mat <- t(1 * z$loss_mat[, active, drop = FALSE])
  rhs   <- z$delta_maj[active]
  
  res <- lpSolve::lp(
    direction    = "min",
    objective.in = rep(1, m_used),
    const.mat    = A_mat,
    const.dir    = rep(">=", nrow(A_mat)),
    const.rhs    = rhs,
    all.bin      = TRUE
  )
  
  if (res$status != 0L) stop("Integer program failed; status: ", res$status)
  k_maj_global <- as.integer(round(res$objval))

  union_L0 <- sum(rowSums(z$loss_mat[, active, drop = FALSE]) > 0L)
  
  k_maj_global_norm <- if (is.infinite(k_maj_global)) {
    1.0
  } else if (k_maj_global == 0L) {
    0.0
  } else if (union_L0 == 0L) {
    1.0
  } else {
    k_maj_global / union_L0
  }
  
  tibble(
    model             = colnames(score_mat)[target_col],
    m_used            = m_used,
    k_maj_global      = k_maj_global,
    frac_maj_global   = if (is.infinite(k_maj_global)) Inf else k_maj_global / m_used,
    k_maj_global_norm = k_maj_global_norm,
    n_maj_active      = length(active),
    maj_status        = res$status
  )
}


run_targets <- function(n_targets, run_one, use_parallel = FALSE) {
  if (use_parallel && .Platform$OS.type == "unix") {
    bind_rows(parallel::mclapply(
      seq_len(n_targets),
      run_one,
      mc.cores = max(1L, parallel::detectCores() - 1L, na.rm = TRUE)
    ))
  } else {
    bind_rows(lapply(seq_len(n_targets), run_one))
  }
}

n_targets_mmlu <- ncol(score_mat_mmlu)
wr0_mmlu <- win_rate_mat(score_mat_mmlu)
w_mean0_mmlu <- colMeans(wr0_mmlu)

mmlu_mean_results <- run_targets(n_targets_mmlu, function(j) {
  k_mean_one_target(score_mat_mmlu, j)
})

mmlu_median_results <- run_targets(n_targets_mmlu, function(j) {
  k_median_one_target(score_mat_mmlu, j)
})

mmlu_win_pair_results <- run_targets(n_targets_mmlu, function(j) {
  k_win_pair_one_target(score_mat_mmlu, j, wr0 = wr0_mmlu, w_mean0 = w_mean0_mmlu)
})

mmlu_win_global_results <- run_targets(n_targets_mmlu, function(j) {
  k_win_global_one_target(score_mat_mmlu, j, wr0 = wr0_mmlu, w_mean0 = w_mean0_mmlu)
})

mmlu_maj_pair_results <- run_targets(n_targets_mmlu, function(j) {
  k_maj_pair_one_target(score_mat_mmlu, j)
})

mmlu_maj_global_results <- run_targets(n_targets_mmlu, function(j) {
  k_maj_global_one_target(score_mat_mmlu, j)
})

stopifnot(all(mmlu_win_global_results$k_win_global >= mmlu_win_pair_results$k_win |
                is.infinite(mmlu_win_global_results$k_win_global)))
stopifnot(all(mmlu_maj_global_results$k_maj_global >= mmlu_maj_pair_results$k_maj_pair |
                is.infinite(mmlu_maj_global_results$k_maj_global)))

mmlu_robustness_results <- select(
  mmlu_mean_results,
  model, k_mean, frac_mean, k_mean_norm
)
mmlu_robustness_results <- left_join(
  mmlu_robustness_results,
  select(mmlu_median_results, model, k_median, frac_median, k_median_norm),
  by = "model"
)
mmlu_robustness_results <- left_join(
  mmlu_robustness_results,
  select(mmlu_win_global_results, model, k_win_global, frac_win_global, k_win_global_norm),
  by = "model"
)
mmlu_robustness_results <- left_join(
  mmlu_robustness_results,
  select(mmlu_maj_global_results, model, k_maj_global, frac_maj_global, k_maj_global_norm),
  by = "model"
)

write.csv(mmlu_robustness_results,
          file.path(dir_helm, "mmlu_robustness_results.csv"),
          row.names = FALSE)


### BBH

dir_bbh <- path.expand("~/bbh_analysis")
fp_csv  <- file.path(dir_bbh, "bbh_scores.csv")
if (!dir.exists(dir_bbh)) stop("Directory not found: ", dir_bbh)
if (!file.exists(fp_csv)) stop("File not found: ", fp_csv)

long_raw <- read.csv(fp_csv, stringsAsFactors = FALSE)
long_raw$metric <- "acc_norm"

long <- long_raw %>%
  mutate(score = as.numeric(score)) %>%
  filter(is.finite(score)) %>%
  group_by(dataset_id, model, metric) %>%
  summarise(score = mean(score, na.rm = TRUE), .groups = "drop")


dat_em <- long %>%
  filter(metric == "acc_norm") %>%
  select(dataset_id, model, score)

n_tasks_total <- n_distinct(dat_em$dataset_id)

models_all_tasks <- dat_em %>%
  distinct(dataset_id, model) %>%
  count(model, name = "n_tasks") %>%
  filter(n_tasks == n_tasks_total) %>%
  pull(model)

dat_em <- dat_em %>%
  filter(model %in% models_all_tasks)


score_wide <- dat_em %>%
  tidyr::pivot_wider(
    names_from = model,
    values_from = score
  ) %>%
  arrange(dataset_id)


score_wide <- score_wide %>%
  filter(complete.cases(across(-dataset_id)))

dataset_ids <- score_wide$dataset_id
score_mat <- score_wide %>%
  select(-dataset_id) %>%
  as.matrix()
storage.mode(score_mat) <- "double"

models <- colnames(score_mat)
m <- nrow(score_mat)

n_tasks_balanced <- nrow(score_wide)
stopifnot(m == n_tasks_balanced)
stopifnot(all(complete.cases(score_mat)))

score_mat_bbh <- score_mat
dataset_ids_bbh <- dataset_ids
models_bbh <- models


stopifnot(is.matrix(score_mat))
stopifnot(is.double(score_mat))
stopifnot(all(is.finite(score_mat)))
stopifnot(all(score_mat >= 0))
stopifnot(all(score_mat <= 1))
stopifnot(length(models) == ncol(score_mat))
stopifnot(length(dataset_ids) == nrow(score_mat))
stopifnot(!anyDuplicated(models))
stopifnot(!anyDuplicated(dataset_ids))


n_targets_bbh <- ncol(score_mat_bbh)
wr0_bbh <- win_rate_mat(score_mat_bbh)
w_mean0_bbh <- colMeans(wr0_bbh)

bbh_mean_results <- run_targets(n_targets_bbh, function(j) {
  k_mean_one_target(score_mat_bbh, j)
})

bbh_median_results <- run_targets(n_targets_bbh, function(j) {
  k_median_one_target(score_mat_bbh, j)
})

bbh_win_pair_results <- run_targets(n_targets_bbh, function(j) {
  k_win_pair_one_target(score_mat_bbh, j, wr0 = wr0_bbh, w_mean0 = w_mean0_bbh)
}, use_parallel = TRUE)

bbh_win_global_results <- run_targets(n_targets_bbh, function(j) {
  k_win_global_one_target(score_mat_bbh, j, wr0 = wr0_bbh, w_mean0 = w_mean0_bbh)
}, use_parallel = TRUE)

bbh_maj_pair_results <- run_targets(n_targets_bbh, function(j) {
  k_maj_pair_one_target(score_mat_bbh, j)
})

bbh_maj_global_results <- run_targets(n_targets_bbh, function(j) {
  k_maj_global_one_target(score_mat_bbh, j)
}, use_parallel = TRUE)

stopifnot(all(bbh_win_global_results$k_win_global >= bbh_win_pair_results$k_win |
                is.infinite(bbh_win_global_results$k_win_global)))
stopifnot(all(bbh_maj_global_results$k_maj_global >= bbh_maj_pair_results$k_maj_pair |
                is.infinite(bbh_maj_global_results$k_maj_global)))

bbh_robustness_results <- select(
  bbh_mean_results,
  model, k_mean, frac_mean, k_mean_norm
)
bbh_robustness_results <- left_join(
  bbh_robustness_results,
  select(bbh_median_results, model, k_median, frac_median, k_median_norm),
  by = "model"
)
bbh_robustness_results <- left_join(
  bbh_robustness_results,
  select(bbh_win_global_results, model, k_win_global, frac_win_global, k_win_global_norm),
  by = "model"
)
bbh_robustness_results <- left_join(
  bbh_robustness_results,
  select(bbh_maj_global_results, model, k_maj_global, frac_maj_global, k_maj_global_norm),
  by = "model"
)

write.csv(bbh_robustness_results,
          file.path(dir_bbh, "bbh_robustness_results.csv"),
          row.names = FALSE)

### Retain only the best model per uploader/family in BBH

uploader_name <- function(nms) {
  tolower(sub("/.*", "", nms))
}

dupe_cols <- function(sm, key_fn = uploader_name) {
  k   <- key_fn(colnames(sm))
  s   <- colMeans(sm)
  ord <- order(k, -s)
  ko  <- k[ord]
  keep <- !duplicated(ko)
  sm[, ord[keep], drop = FALSE]
}

score_mat_bbh_d <- dupe_cols(score_mat_bbh)
stopifnot(ncol(score_mat_bbh_d) >= 1L)
stopifnot(ncol(score_mat_bbh_d) == length(unique(uploader_name(colnames(score_mat_bbh)))))


n_targets_bbh_d <- ncol(score_mat_bbh_d)
wr0_bbh_d       <- win_rate_mat(score_mat_bbh_d)
w_mean0_bbh_d   <- colMeans(wr0_bbh_d)

### Re-run tables on new matrix

bbh_d_mean <- run_targets(n_targets_bbh_d, function(j) {
  k_mean_one_target(score_mat_bbh_d, j)
})
bbh_d_med <- run_targets(n_targets_bbh_d, function(j) {
  k_median_one_target(score_mat_bbh_d, j)
})
bbh_d_win <- run_targets(n_targets_bbh_d, function(j) {
  k_win_global_one_target(score_mat_bbh_d, j, wr0 = wr0_bbh_d, w_mean0 = w_mean0_bbh_d)
}, use_parallel = TRUE)
bbh_d_maj <- run_targets(n_targets_bbh_d, function(j) {
  k_maj_global_one_target(score_mat_bbh_d, j)
}, use_parallel = TRUE)

bbh_robustness_results_d <- select(bbh_d_mean, model, k_mean, frac_mean, k_mean_norm)
bbh_robustness_results_d <- left_join(
  bbh_robustness_results_d,
  select(bbh_d_med, model, k_median, frac_median, k_median_norm), by = "model")
bbh_robustness_results_d <- left_join(
  bbh_robustness_results_d,
  select(bbh_d_win, model, k_win_global, frac_win_global, k_win_global_norm), by = "model")
bbh_robustness_results_d <- left_join(
  bbh_robustness_results_d,
  select(bbh_d_maj, model, k_maj_global, frac_maj_global, k_maj_global_norm), by = "model")
write.csv(bbh_robustness_results_d,
          file.path(dir_bbh, "bbh_robustness_results_model_uploader.csv"),
          row.names = FALSE)

fm <- data.frame(
  model = colnames(score_mat_bbh),
  uploader = uploader_name(colnames(score_mat_bbh)),
  score = colMeans(score_mat_bbh),
  stringsAsFactors = FALSE
)

fm <- fm[order(fm$uploader, -fm$score), ]
fm$kept <- !duplicated(fm$uploader)
fk <- fm[fm$kept, ]
stopifnot(max(table(fk$uploader)) == 1L)
write.csv(fm, file.path(dir_bbh, "bbh_model_uploader.csv"), row.names = FALSE)

### Bootstrap

boot_summary_tbl <- function(rr, B = 10000L, ks = c(0, 1, 2, 3, 5, 10),
                             a = 0.05, seed = 1L) {
  set.seed(seed)
  cols <- c(
    mean   = "k_mean",
    median = "k_median",
    win    = "k_win_global",
    maj    = "k_maj_global"
  )
  stat_one <- function(idx) {
    unlist(lapply(setNames(names(cols), names(cols)), function(rule) {
      v <- rr[[cols[[rule]]]][idx]
      v <- v[!is.na(v)]
      
      shares <- vapply(ks, function(K) mean(v <= K), numeric(1))
      names(shares) <- paste0("share_k_le_", ks)
      
      c(
        shares,
        median_k = median(v),
        share_inf = mean(is.infinite(v))
      )
    }))
  }
  obs <- stat_one(seq_len(nrow(rr)))
  boot <- replicate(B, {
    idx <- sample.int(nrow(rr), nrow(rr), replace = TRUE)
    stat_one(idx)
  })
  
  ci <- t(apply(boot, 1, quantile, probs = c(a / 2, 1 - a / 2), na.rm = TRUE))
  tibble(
    stat = names(obs),
    estimate = unname(obs),
    lo = ci[, 1],
    hi = ci[, 2]
  )
}
boot_bb_d <- boot_summary_tbl(bbh_robustness_results_d)

boot_cluster_summary_tbl <- function(rr, cluster, B = 10000L,
                                     ks = c(0, 1, 2, 3, 5, 10),
                                     a = 0.05, seed = 1L) {
  set.seed(seed)
  cols <- c(
    mean   = "k_mean",
    median = "k_median",
    win    = "k_win_global",
    maj    = "k_maj_global"
  )
  clusters <- split(seq_len(nrow(rr)), cluster)
  G <- length(clusters)
  stat_one <- function(idx) {
    unlist(lapply(setNames(names(cols), names(cols)), function(rule) {
      v <- rr[[cols[[rule]]]][idx]
      v <- v[!is.na(v)]
      shares <- vapply(ks, function(K) mean(v <= K), numeric(1))
      names(shares) <- paste0("share_k_le_", ks)
      c(
        shares,
        median_k = median(v),
        share_inf = mean(is.infinite(v))
      )
    }))
  }
  obs <- stat_one(seq_len(nrow(rr)))
  boot <- replicate(B, {
    picked <- sample.int(G, G, replace = TRUE)
    idx <- unlist(clusters[picked], use.names = FALSE)
    stat_one(idx)
  })
  
  ci <- t(apply(boot, 1, quantile, probs = c(a / 2, 1 - a / 2), na.rm = TRUE))
  
  tibble(
    stat = names(obs),
    estimate = unname(obs),
    lo = ci[, 1],
    hi = ci[, 2]
  )
}

boot_bb <- boot_cluster_summary_tbl(
  bbh_robustness_results,
  cluster = uploader_name(bbh_robustness_results$model)
)

boot_mm <- boot_cluster_summary_tbl(
  mmlu_robustness_results,
  cluster = uploader_name(mmlu_robustness_results$model)
)

### ECDF

ecdf_wilson <- function(v, y_grid, a = 0.05) {
  v <- v[is.finite(v)]
  n <- length(v)
  F <- vapply(y_grid, function(y) sum(v <= y) / n, numeric(1))
  z <- qnorm(1 - a / 2)
  hw <- z * sqrt(F * (1 - F) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  ce <- (F + z^2 / (2 * n)) / (1 + z^2 / n)
  tibble(y = y_grid, lo = ce - hw, F = F, hi = ce + hw)
}


### Condorcet winner

condorcet_winner <- function(score_mat) {
  out <- lapply(seq_len(ncol(score_mat)), function(j) {
    wins <- colSums(score_mat[, j] > score_mat)
    losses <- colSums(score_mat[, j] < score_mat)
    if (all(wins[-j] > losses[-j])) {
      colnames(score_mat)[j]
    } else {
      NULL
    }
  })
  unlist(out, use.names = FALSE)
}

condorcet_winner(score_mat_mmlu)
condorcet_winner(score_mat_bbh)
condorcet_winner(score_mat_bbh_d)


### Spearman rho

rho_strength <- function(rr, sm, col_k, a = 0.05) {
  k  <- rr[[col_k]]
  s  <- colMeans(sm)[match(rr$model, colnames(sm))]
  ok <- is.finite(k) & is.finite(s)
  n  <- sum(ok)
  if (n < 4L) return(c(lo = NA, rho = NA, hi = NA, n = n))
  r  <- suppressWarnings(cor(s[ok], k[ok], method = "spearman"))
  z  <- atanh(r); se <- 1 / sqrt(n - 3); q <- qnorm(1 - a / 2)
  c(lo = tanh(z - q * se), rho = r, hi = tanh(z + q * se), n = n)
}

### Paired Wilcoxon Test

pair_wcx <- function(rr, a, b) {
  x  <- rr[[a]]; y <- rr[[b]]
  ok <- is.finite(x) & is.finite(y)
  w  <- wilcox.test(x[ok], y[ok], paired = TRUE, exact = FALSE)
  tibble(a = a, b = b, n = sum(ok),
         med_diff = median(x[ok] - y[ok]), p = w$p.value)
}

rule_pairs <- list(
  c("frac_mean",       "frac_median"),
  c("frac_mean",       "frac_win_global"),
  c("frac_mean",       "frac_maj_global"),
  c("frac_median",     "frac_win_global"),
  c("frac_median",     "frac_maj_global"),
  c("frac_win_global", "frac_maj_global")
)

W_mm   <- bind_rows(lapply(rule_pairs, function(p) pair_wcx(mmlu_robustness_results,   p[1], p[2])))
W_bb   <- bind_rows(lapply(rule_pairs, function(p) pair_wcx(bbh_robustness_results,    p[1], p[2])))
W_bb_d <- bind_rows(lapply(rule_pairs, function(p) pair_wcx(bbh_robustness_results_d,  p[1], p[2])))
W_mm$p_adj   <- p.adjust(W_mm$p,   method = "holm")
W_bb$p_adj   <- p.adjust(W_bb$p,   method = "holm")
W_bb_d$p_adj <- p.adjust(W_bb_d$p, method = "holm")

cols_norm <- setNames(
  c("k_mean_norm", "k_median_norm", "k_win_global_norm", "k_maj_global_norm"),
  c("mean", "med", "win", "maj")
)
rho_fz_mm   <- lapply(cols_norm, function(c) rho_strength(mmlu_robustness_results,   score_mat_mmlu,   c))
rho_fz_bb_d <- lapply(cols_norm, function(c) rho_strength(bbh_robustness_results_d,  score_mat_bbh_d,  c))

y_grid <- sort(unique(c(
  seq(0, 1, by = 0.02),
  c(1, 2, 3, 5) / nrow(score_mat_mmlu),
  c(1, 2, 3, 5) / nrow(score_mat_bbh),
  c(1, 2, 3, 5) / nrow(score_mat_bbh_d)
)))

ecdf_w_mm <- list(
  mean = ecdf_wilson(mmlu_robustness_results$frac_mean,        y_grid),
  med  = ecdf_wilson(mmlu_robustness_results$frac_median,      y_grid),
  win  = ecdf_wilson(mmlu_robustness_results$frac_win_global,  y_grid),
  maj  = ecdf_wilson(mmlu_robustness_results$frac_maj_global,  y_grid)
)
ecdf_w_bb_d <- list(
  mean = ecdf_wilson(bbh_robustness_results_d$frac_mean,        y_grid),
  med  = ecdf_wilson(bbh_robustness_results_d$frac_median,      y_grid),
  win  = ecdf_wilson(bbh_robustness_results_d$frac_win_global,  y_grid),
  maj  = ecdf_wilson(bbh_robustness_results_d$frac_maj_global,  y_grid)
)

### Additional analyses

cols_raw <- c("k_mean", "k_median", "k_win_global", "k_maj_global")
cols_all <- c(cols_raw, unname(cols_norm))

add_strength <- function(rr, sm, suite) {
  stopifnot(!anyDuplicated(rr$model))
  stopifnot(setequal(rr$model, colnames(sm)))
  rr$baseline_str <- colMeans(sm)[match(rr$model, colnames(sm))]
  rr$suite <- suite
  rr
}

rr_all <- bind_rows(
  add_strength(mmlu_robustness_results, score_mat_mmlu, "MMLU"),
  add_strength(bbh_robustness_results, score_mat_bbh, "BBH"),
  add_strength(bbh_robustness_results_d, score_mat_bbh_d, "BBH_d")
)

norm_sum <- rr_all %>%
  group_by(suite) %>%
  summarise(
    across(all_of(unname(cols_norm)), median),
    .groups = "drop"
  )

### Comparing aggregation rules

cmp_sum <- rr_all %>%
  group_by(suite) %>%
  summarise(
    n = n(),
    mean_ge = sum(k_win_global >= k_mean),
    mean_gt = sum(k_win_global > k_mean),
    med_ge = sum(k_win_global >= k_median),
    med_gt = sum(k_win_global > k_median),
    maj_ge = sum(k_win_global >= k_maj_global),
    maj_gt = sum(k_win_global > k_maj_global),
    .groups = "drop"
  )

### Models near the top of the leaderboard

top_rr <- rr_all %>%
  group_by(suite) %>%
  arrange(desc(baseline_str), model, .by_group = TRUE) %>%
  filter(row_number() <= ceiling(n() / 4)) %>%
  ungroup()

top_sum <- top_rr %>%
  group_by(suite) %>%
  summarise(
    n = n(),
    across(all_of(cols_all), median),
    .groups = "drop"
  )

### Effect of model size

sizes <- read.csv(
  file.path(dir_bbh, "bbh_sizes.csv"),
  stringsAsFactors = FALSE
)

stopifnot(!anyDuplicated(sizes$model))
stopifnot(setequal(sizes$model, colnames(score_mat_bbh)))
stopifnot(all(is.finite(sizes$model_num_parameters)))
stopifnot(all(sizes$model_num_parameters > 0))

size_rr <- rr_all %>%
  filter(suite %in% c("BBH", "BBH_d")) %>%
  left_join(sizes, by = "model") %>%
  mutate(
    size_b = model_num_parameters / 1e9,
    size_group = cut(
      size_b,
      breaks = c(0, 3, 8, 15, 40, Inf),
      labels = c("<=3B", "(3,8]B", "(8,15]B", "(15,40]B", ">40B"),
      include.lowest = TRUE
    )
  )

stopifnot(!anyNA(size_rr$size_group))

size_sum <- size_rr %>%
  group_by(suite, size_group) %>%
  summarise(
    n = n(),
    across(all_of(cols_all), median),
    .groups = "drop"
  )

size_rho <- size_rr %>%
  group_by(suite) %>%
  summarise(
    across(
      all_of(c("baseline_str", cols_all)),
      ~ cor(size_b, .x, method = "spearman")
    ),
    .groups = "drop"
  )

### Heterogeneous costs

fp_cost <- file.path(dir_helm, "mmlu_costs.csv")

if (file.exists(fp_cost)) {
  mmlu_costs <- read.csv(fp_cost, stringsAsFactors = FALSE)
} else {
  dir_run <- list.dirs(dir_v1, recursive = FALSE, full.names = TRUE)
  ds_id <- sub(",.*", "", basename(dir_run))
  idx <- match(dataset_ids_mmlu, ds_id)
  stopifnot(!anyNA(idx))
  
  fp <- file.path(dir_run[idx], "instances.json")
  stopifnot(all(file.exists(fp)))
  
  mmlu_costs <- tibble(
    dataset_id = dataset_ids_mmlu,
    c_D = vapply(fp, function(f) {
      z <- read_json(f, simplifyVector = FALSE)
      stopifnot(length(z) > 0L)
      stopifnot(all(vapply(
        z, function(x) identical(x$split, "test"), logical(1)
      )))
      length(z)
    }, numeric(1))
  )
}

fp_cost <- file.path(dir_bbh, "bbh_costs.csv")

if (file.exists(fp_cost)) {
  bbh_costs <- read.csv(fp_cost, stringsAsFactors = FALSE)
} else {
  fp <- file.path(
    path.expand("~/.cache/huggingface/hub"),
    "datasets--open-llm-leaderboard--results/snapshots",
    "aa81ecc38fdc5708254b833923368970efdf5ef5",
    "migtissera/Llama-3-70B-Synthia-v3.5",
    "results_2024-10-24T00-00-00.000000.json"
  )
  
  stopifnot(file.exists(fp))
  z <- read_json(fp, simplifyVector = FALSE)[["n-samples"]]
  
  bbh_costs <- tibble(
    dataset_id = dataset_ids_bbh,
    c_D = vapply(dataset_ids_bbh, function(d) {
      q <- z[[paste0("leaderboard_", d)]]
      stopifnot(!is.null(q$effective), !is.null(q$original))
      stopifnot(q$effective == q$original)
      as.numeric(q$effective)
    }, numeric(1))
  )
}

stopifnot(nrow(mmlu_costs) == 57L, sum(mmlu_costs$c_D) == 14042)
stopifnot(nrow(bbh_costs) == 24L, sum(bbh_costs$c_D) == 5761)

cost_cover <- function(c_D, A_mat, rhs) {
  active <- which(rhs > 0)
  if (length(active) == 0L) return(0)
  
  A_mat <- A_mat[active, , drop = FALSE]
  rhs <- rhs[active]
  
  res <- lpSolve::lp(
    direction = "min",
    objective.in = c_D,
    const.mat = A_mat,
    const.dir = rep(">=", nrow(A_mat)),
    const.rhs = rhs,
    all.bin = TRUE
  )
  
  if (res$status == 2L) return(Inf)
  if (res$status != 0L) stop("Integer program failed; status: ", res$status)
  
  x <- round(res$solution)
  stopifnot(all(abs(res$solution - x) < 1e-7))
  stopifnot(all(as.vector(A_mat %*% x) + 1e-7 >= rhs))
  stopifnot(abs(sum(c_D * x) - res$objval) < 1e-7)
  
  sum(c_D * x)
}

cost_one_target <- function(score_mat, target_col, c_D, wr0, w_mean0,
                            medians0) {
  m_used <- nrow(score_mat)
  comp_cols <- setdiff(seq_len(ncol(score_mat)), target_col)
  x <- score_mat[, target_col]
  G <- 1 - x
  
  delta_mean <- max(0, max(colMeans(score_mat) - mean(x)))
  cost_mean <- cost_cover(
    c_D, matrix(G, nrow = 1L), m_used * delta_mean
  )
  
  h <- m_used %/% 2L + 1L
  tau <- max(medians0[comp_cols])
  delta_median <- max(0L, m_used - h + 1L - sum(x >= tau))
  eligible <- which(x < tau & x + G >= tau)
  
  cost_median <- if (length(eligible) < delta_median) {
    Inf
  } else {
    sum(sort(c_D[eligible])[seq_len(delta_median)])
  }
  
  d_win <- pmax(0, w_mean0[comp_cols] - w_mean0[target_col])
  
  cost_win <- if (all(d_win <= 0)) {
    0
  } else {
    q_mat <- q_mat_one_target(score_mat, target_col, wr0)
    cost_cover(c_D, t(q_mat), m_used * d_win)
  }
  
  z <- maj_parts(score_mat, target_col)
  cost_maj <- cost_cover(c_D, t(1 * z$loss_mat), z$delta_maj)
  
  tibble(
    model = colnames(score_mat)[target_col],
    cost_mean = cost_mean,
    cost_median = cost_median,
    cost_win = cost_win,
    cost_maj = cost_maj
  )
}

cost_targets <- function(sm, dataset_ids, costs, suite) {
  stopifnot(length(dataset_ids) == nrow(sm))
  stopifnot(!anyDuplicated(costs$dataset_id))
  
  idx <- match(dataset_ids, costs$dataset_id)
  stopifnot(!anyNA(idx))
  
  c_D <- costs$c_D[idx]
  stopifnot(all(is.finite(c_D)), all(c_D > 0))
  
  wr0 <- win_rate_mat(sm)
  w_mean0 <- colMeans(wr0)
  h <- nrow(sm) %/% 2L + 1L
  medians0 <- apply(sm, 2, function(x) sort(x)[h])
  
  rr <- run_targets(ncol(sm), function(j) {
    cost_one_target(sm, j, c_D, wr0, w_mean0, medians0)
  })
  
  rr$suite <- suite
  rr$total_cost <- sum(c_D)
  rr
}

cost_rr <- bind_rows(
  cost_targets(
    score_mat_mmlu, dataset_ids_mmlu, mmlu_costs, "MMLU"
  ),
  cost_targets(
    score_mat_bbh_d, dataset_ids_bbh, bbh_costs, "BBH_d"
  )
)

cost_sum <- cost_rr %>%
  group_by(suite) %>%
  summarise(
    n = n(),
    total_cost = first(total_cost),
    across(c(cost_mean, cost_median, cost_win, cost_maj), median),
    .groups = "drop"
  )

cost_pct <- cost_sum %>%
  mutate(across(starts_with("cost_"), ~ 100 * .x / total_cost))

### Save results

sum_results <- function(rr, suite) {
  data.frame(
    suite = suite,
    rule = c("mean", "median", "win", "maj"),
    k5 = c(
      mean(rr$k_mean <= 5),
      mean(rr$k_median <= 5),
      mean(rr$k_win_global <= 5),
      mean(rr$k_maj_global <= 5)
    ),
    k10 = c(
      mean(rr$k_mean <= 10),
      mean(rr$k_median <= 10),
      mean(rr$k_win_global <= 10),
      mean(rr$k_maj_global <= 10)
    ),
    med_k = c(
      median(rr$k_mean),
      median(rr$k_median),
      median(rr$k_win_global),
      median(rr$k_maj_global)
    ),
    med_frac = c(
      median(rr$frac_mean),
      median(rr$frac_median),
      median(rr$frac_win_global),
      median(rr$frac_maj_global)
    )
  )
}

sum_all <- rbind(
  sum_results(mmlu_robustness_results, "mmlu"),
  sum_results(bbh_robustness_results, "bbh"),
  sum_results(bbh_robustness_results_d, "bbh_d")
)

write.csv(sum_all, file.path(dir_bbh, "summary_results.csv"), row.names = FALSE)
write.csv(boot_mm, file.path(dir_bbh, "boot_mmlu.csv"), row.names = FALSE)
write.csv(boot_bb, file.path(dir_bbh, "boot_bbh.csv"), row.names = FALSE)
write.csv(boot_bb_d, file.path(dir_bbh, "boot_bbh_model_uploader.csv"), row.names = FALSE)
write.csv(W_mm, file.path(dir_bbh, "wilcoxon_mmlu.csv"), row.names = FALSE)
write.csv(W_bb, file.path(dir_bbh, "wilcoxon_bbh.csv"), row.names = FALSE)
write.csv(W_bb_d, file.path(dir_bbh, "wilcoxon_bbh_model_uploader.csv"), row.names = FALSE)

saveRDS(list(
  mmlu_robustness_results = mmlu_robustness_results,
  bbh_robustness_results = bbh_robustness_results,
  bbh_robustness_results_d = bbh_robustness_results_d,
  sum_all = sum_all,
  boot_mm = boot_mm,
  boot_bb = boot_bb,
  boot_bb_d = boot_bb_d,
  W_mm = W_mm,
  W_bb = W_bb,
  W_bb_d = W_bb_d,
  rho_fz_mm = rho_fz_mm,
  rho_fz_bb_d = rho_fz_bb_d,
  ecdf_w_mm = ecdf_w_mm,
  ecdf_w_bb_d = ecdf_w_bb_d
), file.path(dir_bbh, "all_results.rds"))

### Save additional results

extra <- list(
  norm_sum = norm_sum,
  cmp_sum = cmp_sum,
  top_rr = top_rr,
  top_sum = top_sum,
  size_rr = size_rr,
  size_sum = size_sum,
  size_rho = size_rho,
  mmlu_costs = mmlu_costs,
  bbh_costs = bbh_costs,
  cost_rr = cost_rr,
  cost_sum = cost_sum,
  cost_pct = cost_pct
)

for (nm in names(extra)) {
  write.csv(
    extra[[nm]],
    file.path(dir_bbh, paste0(nm, ".csv")),
    row.names = FALSE
  )
}

write.csv(
  mmlu_costs,
  file.path(dir_helm, "mmlu_costs.csv"),
  row.names = FALSE
)

saveRDS(extra, file.path(dir_bbh, "additional_results.rds"))

writeLines(
  capture.output(sessionInfo()),
  file.path(dir_bbh, "sessionInfo.txt")
)

### Figure 1

rn <- c("Arithmetic mean", "Median", "Pairwise majority", "Mean win rate")
rc <- c("frac_mean", "frac_median", "frac_maj_global", "frac_win_global")
cl <- c(
  "Arithmetic mean" = "#F8766D",
  "Median" = "#00BA38",
  "Pairwise majority" = "#619CFF",
  "Mean win rate" = "#C77CFF"
)

mk <- function(rr, ss) {
  out <- list()
  for (i in seq_along(rc)) {
    v0 <- rr[[rc[i]]]
    n <- length(v0)
    v <- sort(unique(v0[is.finite(v0)]))
    if (length(v) == 0L) {
      x <- c(0, 100)
      y <- c(0, 0)
    } else {
      yy <- vapply(v, function(z) sum(v0 <= z, na.rm = TRUE) / n, numeric(1))
      x <- 100 * c(0, v, 1)
      y <- 100 * c(0, yy, sum(v0 <= 1, na.rm = TRUE) / n)
    }
    out[[i]] <- data.frame(suite = ss, rule = rn[i], x = x, y = y)
  }
  do.call(rbind, out)
}

ef <- rbind(
  mk(mmlu_robustness_results, "MMLU"),
  mk(bbh_robustness_results, "BIG-Bench Hard")
)

ef$rule <- factor(ef$rule, levels = rn)
ef$suite <- factor(ef$suite, levels = c("MMLU", "BIG-Bench Hard"))
fig1 <- ggplot(ef, aes(x = x, y = y, color = rule, group = rule)) +
  geom_step(linewidth = 0.7, lineend = "round") +
  facet_wrap(~suite, ncol = 2) +
  scale_color_manual(values = cl, breaks = rn) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    expand = expansion(mult = c(0, 0.018))
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = "Benchmark tasts included in training (%)",
    y = "Target models that can be made top-ranked (%)"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25, color = "grey88"),
    strip.text = element_text(size = 9, face = "bold"),
    axis.title = element_text(size = 5.5),
    axis.text = element_text(size = 5.8),
    legend.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 7.7),
    legend.key.width = grid::unit(1.05, "lines"),
    legend.spacing.x = grid::unit(0.15, "lines"),
    legend.margin = margin(-3, 0, -4, 0),
    panel.spacing = grid::unit(0.65, "lines"),
    plot.margin = margin(1, 5, 1, 2)
  )

ggsave(
  file.path(dir_bbh, "ecdf.pdf"), fig1,
  width = 5.5, height = 2.05, useDingbats = FALSE
)