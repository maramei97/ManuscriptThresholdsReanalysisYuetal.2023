# ================================================================
#  Threshold‑finding workflow  – unified version (2025‑07‑04 rev B)
#  ---------------------------------------------------------------
#  * find_threshold_models()  ->  “thresres” object (single call)
#  * print() / autoplot()     ->  console & visual summaries
#  ---------------------------------------------------------------
#  Requires: dplyr, ggplot2, tibble, tidyr, mgcv, chngpt, segmented,
#            glue, rlang
# ================================================================

## 0.  UTILITIES ──────────────────────────────────────────────────
ic_values <- function(fit, y = NULL) {
  aic <- AIC(fit);  bic <- BIC(fit)
  
  ## ---------- R² ----------------------------------------------
  if (inherits(fit, "lm")) {
    r2 <- summary(fit)$r.squared
    
  } else if (!is.null(fit$null.deviance) &&
             is.finite(fit$null.deviance) && fit$null.deviance > 0) {
    r2 <- 1 - fit$deviance / fit$null.deviance    # glm / gam case
    
  } else {                                        # fallback: SSE / SST
    if (is.null(y)) y <- stats::model.response(model.frame(fit))
    y_hat <- as.numeric(predict(fit, type = "response"))
    ssr   <- sum((y - y_hat)^2, na.rm = TRUE)
    sst   <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
    r2 <- if (sst > 0) 1 - ssr / sst else NA_real_
  }
  
  c(AIC = aic, BIC = bic, R2 = r2)
}


## 1.  LINEAR vs NON‑LINEAR SCREEN ───────────────────────────────
screen_nonlinear <- function(df, resp, pred,
                             delta = 2,
                             ic = c("AIC","BIC"),
                             use_mgcv = TRUE) {
  ic <- match.arg(ic)
  
  m_lin  <- lm(reformulate(pred, resp), data = df)
  m_quad <- lm(reformulate(c(pred, glue::glue("I({pred}^2)")), resp), data = df)
  
  if (use_mgcv) {
    k_safe <- max(3, min(10, length(unique(df[[pred]])) - 1))
    m_gam  <- mgcv::gam(as.formula(glue::glue("{resp} ~ s({pred}, k={k_safe})")),
                        data = df)
  } else {
    m_gam  <- gam::gam(as.formula(glue::glue("{resp} ~ s({pred})")), data = df)
  }
  
  ic_mat <- rbind(linear    = ic_values(m_lin),
                  quadratic = ic_values(m_quad),
                  gam       = ic_values(m_gam))
  
  list(models    = list(linear = m_lin, quadratic = m_quad, gam = m_gam),
       ic        = ic_mat,
       nonlinear = (ic_mat["linear", ic] - min(ic_mat[-1, ic])) > delta,
       winner    = rownames(ic_mat)[which.min(ic_mat[, ic])])
}

## 2.  CANDIDATE PIECE‑WISE FITS ─────────────────────────────────
fit_piecewise <- function(df, resp, pred,
                          types = c("step","hinge","segmented","stegmented","M11")) {
  
  fits  <- list();  stats <- list()
  
  for (t in types) {
    if (t %in% c("step","hinge")) {
      base_f <- as.formula(glue::glue("{resp} ~ 1"))
      seg_f  <- reformulate(pred, NULL);  df_fit <- df
    } else if (t == "stegmented") {                     # jump + slope
      thr <- paste0(pred,"_thr"); df_fit <- df; df_fit[[thr]] <- df[[pred]]
      base_f <- reformulate(pred, resp)
      seg_f  <- as.formula(paste("~ 1 +", thr))
    } else {                                            # segmented / M11
      thr <- paste0(pred,"_thr"); df_fit <- df; df_fit[[thr]] <- df[[pred]]
      base_f <- reformulate(pred, resp)
      seg_f  <- reformulate(thr, NULL)
    }
    
    fits[[t]]  <- chngpt::chngptm(base_f, seg_f, type = t,
                                  data = df_fit, family = "gaussian")
    stats[[t]] <- ic_values(fits[[t]])
  }
  
  ic_mat <- do.call(rbind, stats)
  best_t <- rownames(ic_mat)[which.min(ic_mat[,"AIC"])]
  
  list(fits = fits, ic = ic_mat,
       best_fit = fits[[best_t]], best_type = best_t)
}

## 3.  ψ CI CASCADE ──────────────────────────────────────────────
ci_break <- function(best_fit, df, resp, pred,
                     boot_theta, Bnp = 200, Bpar = 200, seed = 1) {
  
  ok <- !is.na(boot_theta)
  if (length(unique(boot_theta[ok])) > 1 && sum(ok) >= 10)
    return(list(ci = quantile(boot_theta[ok], c(.025,.975)),
                method = "non‑parametric bootstrap"))
  
  set.seed(seed)
  psi_hat <- best_fit$chngpt
  
  ## parametric bootstrap
  y_hat <- predict(best_fit, type = "response")
  sig   <- sd(residuals(best_fit))
  theta_par <- replicate(Bpar, {
    y_sim <- y_hat + rnorm(length(y_hat), sd = sig)
    df[[resp]] <- y_sim
    f <- tryCatch(update(best_fit, data = df), error = function(e) NULL)
    if (is.null(f)) NA else f$chngpt
  })
  ok2 <- !is.na(theta_par)
  if (length(unique(theta_par[ok2])) > 1 && sum(ok2) >= 10)
    return(list(ci = quantile(theta_par[ok2], c(.025,.975)),
                method = "parametric bootstrap"))
  
  ## jackknife
  n  <- nrow(df)
  psis <- sapply(seq_len(n), function(i) {
    f <- tryCatch(update(best_fit, data = df[-i,]), error = function(e) NULL)
    if (is.null(f)) NA else f$chngpt
  })
  psis <- psis[!is.na(psis)]
  if (length(psis) >= 8 && var(psis) > 0) {
    se <- sqrt((length(psis) - 1) * var(psis))
    ci <- psi_hat + c(-1,1) * qt(.975, df = length(psis) - 1) * se
    return(list(ci = ci, method = "delete‑1 jackknife"))
  }
  
  ## profile‑likelihood fallback
  warning("Fallback to profile‑likelihood CI (data scarce); interval may be wide.")
  ll_fun <- getFromNamespace("logLik.chngptm", "chngpt")
  xr <- range(df[[pred]], na.rm = TRUE)
  if (!all(is.finite(xr)))
    return(list(ci = c(NA_real_, NA_real_), method = "profile‑LR failed"))
  
  psi_grid <- seq(xr[1], xr[2], length.out = 100)
  ll <- sapply(psi_grid, function(p) ll_fun(best_fit, psi = p))
  idx <- psi_grid[ ll > max(ll, na.rm = TRUE) - qchisq(.95,1)/2 ]
  list(ci     = if (length(idx) >= 2) range(idx) else c(NA_real_, NA_real_),
       method = "profile likelihood")
}

## 4.  BREAK VALIDATION ──────────────────────────────────────────
validate_break <- function(best_fit, df, resp, pred,
                           psi = best_fit$chngpt,
                           B = 500,
                           method = c("davies","gap"),
                           seed = 1) {
  method <- match.arg(method);  set.seed(seed)
  
  left  <- df[df[[pred]] <= psi, ]
  right <- df[df[[pred]] >  psi, ]
  if (nrow(left) < 5 || nrow(right) < 5)
    return(list(test = method, p.value = NA_real_,
                note = "Too few points on one side of ψ"))
  
  if (method == "davies") {
    lm0 <- lm(reformulate(pred, resp), data = df)
    return(segmented::davies.test(lm0, seg.Z = ~ get(pred)))
  }
  
  diff_slopes <- replicate(B, {
    mL <- lm(reformulate(pred, resp), data = left [sample(nrow(left),  replace = TRUE), ])
    mR <- lm(reformulate(pred, resp), data = right[sample(nrow(right), replace = TRUE), ])
    coef(mR)[pred] - coef(mL)[pred]
  })
  list(test = "gap",
       p.value = mean(diff_slopes <= 0),
       boot = diff_slopes)
}

## 5.  MAIN WRAPPER ──────────────────────────────────────────────
find_threshold_models <- function(df, resp, pred,
                                  delta = 2,
                                  ic = c("AIC","BIC"),
                                  boot_B = 200,
                                  seed = 1,
                                  zoo_types = c("step","hinge",
                                                "segmented","stegmented","M11"),
                                  gam_k = 6,
                                  validate_method = c("gap","davies")) {
  
  set.seed(seed);  ic <- match.arg(ic);  validate_method <- match.arg(validate_method)
  
  scr <- screen_nonlinear(df, resp, pred, delta, ic)
  
  gam_fit <- mgcv::gam(as.formula(paste(resp, "~ s(", pred, ", k=", gam_k, ")")),
                       data = df)
  
  if (!scr$nonlinear) {
    out <- list(msg    = sprintf("%s ~ %s is linear (Δ%s ≤ %.1f)",
                                 resp, pred, ic, delta),
                screen = scr, gam = gam_fit)
    class(out) <- "thresres"; return(out)
  }
  
  zoo <- fit_piecewise(df, resp, pred, types = zoo_types)
  best_fit  <- zoo$best_fit
  best_type <- zoo$best_type
  
  thr_var <- paste0(pred,"_thr");  df2 <- df
  if (best_type %in% c("segmented","stegmented","M11"))
    df2[[thr_var]] <- df[[pred]]
  
  base_f <- if (best_type %in% c("step","hinge"))
    as.formula(glue::glue("{resp} ~ 1"))
  else reformulate(pred, resp)
  # seg_f  <- switch(best_type,
  #                  step       = ~ 1,
  #                  hinge      = reformulate(pred, NULL),
  #                  stegmented = as.formula(paste("~ 1 +", thr_var)),
  #                  reformulate(thr_var, NULL))
  seg_f  <- switch(best_type,
                   step       = reformulate(pred, NULL),   # <- FIX HERE
                   hinge      = reformulate(pred, NULL),
                   stegmented = as.formula(paste("~ 1 +", thr_var)),
                   reformulate(thr_var, NULL))
  best_boot <- chngpt::chngptm(base_f, seg_f, type = best_type,
                               data = df2, family = "gaussian",
                               var.type = "bootstrap",
                               ci.bootstrap.size = boot_B,
                               save.boot = TRUE)
  
  ci_res <- ci_break(best_boot, df2, resp, pred, best_boot$boot$theta, seed = seed)
  val_res <- validate_break(best_boot, df, resp, pred,
                            psi = best_boot$chngpt,
                            method = validate_method, seed = seed)
  
  out <- list(screen      = scr,
              gam         = gam_fit,
              zoo_ic      = zoo$ic,
              best        = best_boot,
              best_type   = best_type,
              psi_best    = best_boot$chngpt,
              psi_best_ci = ci_res$ci,
              ci_method   = ci_res$method,
              validate    = val_res)
  class(out) <- "thresres"; out
}

## 6.  PRINT METHOD ──────────────────────────────────────────────
print.thresres <- function(x, ...) {
  cat("Non‑linear winner (", attr(x$screen$ic, "dimnames")[[2]][1], "): ",
      x$screen$winner, "\n", sep = "")
  if (!is.null(x$best_type)) {
    ic_row <- x$zoo_ic[x$best_type, ]
    cat("Best piece‑wise:", x$best_type,
        "\n  ψ = ", round(x$psi_best,3),
        "   CI95 [", round(x$psi_best_ci[1],3), ",",
        round(x$psi_best_ci[2],3), "]  (", x$ci_method, ")",
        "\n  AIC = ", round(ic_row["AIC"],1),
        "   BIC = ", round(ic_row["BIC"],1),
        "   R² = ",  round(ic_row["R2"],2), "\n", sep = "")
    if (!is.null(x$validate$p.value))
      cat("  Validation p‑value (", x$validate$test, ") = ",
          signif(x$validate$p.value,3), "\n", sep = "")
  }
}
## 7.  AUTOPLOT METHOD  – jump gap + ψ̂ vertical line  ────────────
autoplot.thresres <- function(object, df, resp, pred,
                              n_grid = 200,
                              show = TRUE) {
  
  grid <- tibble::tibble(
    !!rlang::sym(pred) := seq(min(df[[pred]], na.rm = TRUE),
                              max(df[[pred]], na.rm = TRUE),
                              length.out = n_grid)
  )
  grid[[paste0(pred,"_thr")]] <- grid[[pred]]
  grid$pred_gam <- predict(object$gam, newdata = grid)
  
  gg <- ggplot2::ggplot(df, ggplot2::aes(.data[[pred]], .data[[resp]])) +
    ggplot2::geom_point(alpha = 0.6)
  
  ## ------------------------------------------------ linear best
  if (is.null(object$best)) {
    lin_mod <- object$screen$models$linear
    grid$pred_best <- predict(lin_mod, newdata = grid)
    best_label <- "linear"
    best_type  <- "linear"
  } else {                                             # piece‑wise best
    grid$pred_best <- predict(object$best, newdata = grid)
    best_label <- object$best_type
    best_type  <- object$best_type
  }
  
  ## ---- suppress vertical jump in the line itself --------------
  if (best_type %in% c("step","stegmented")) {
    idx <- which.min(abs(grid[[pred]] - object$psi_best))
    grid$pred_best[c(idx, idx + 1)] <- NA
  }
  
  gg <- gg +
    ggplot2::geom_line(data = grid,
                       ggplot2::aes(y = pred_best, colour = "best"),
                       linewidth = 1) +
    ggplot2::geom_line(data = grid,
                       ggplot2::aes(y = pred_gam,  colour = "gam"),
                       linewidth = 0.8)
  
  ## ---- draw ψ̂ as dotted v‑line when piece‑wise selected -------
  if (!is.null(object$best))
    gg <- gg + ggplot2::geom_vline(xintercept = object$psi_best,
                                   colour = "red", linetype = "dotted",linewidth = 1)
  
  gg <- gg +
    ggplot2::scale_colour_manual(values = c(best = "red", gam = "black"),
                                 labels = c(best = best_label, gam = "GAM smooth"),
                                 name   = "fit") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::xlab(pred) + ggplot2::ylab(resp)
  
  title_txt <- if (is.null(object$best)) {
    sprintf("%s vs %s  |  linear model chosen", resp, pred)
  } else {
    sprintf("%s vs %s  |  ψ = %.2f", resp, pred, object$psi_best)
  }
  gg <- gg + ggplot2::labs(title = title_txt)
  
  if (isTRUE(show)) {
    print(gg)
    invisible(gg)
  } else {
    gg
  }
}

## 8 summary table like S2 in paper ──────────────────────────────────────────────
summary_s2 <- function(res, var_name = NA_character_) {
  ## --- 1. pull the three IC/R² rows ----------------------------
  lin  <- res$screen$ic["linear", ]
  nonl <- apply(res$screen$ic[c("quadratic","gam"), c("AIC","BIC","R2")],
                2, min, na.rm = TRUE)
  best <- res$zoo_ic[res$best_type, c("AIC","BIC","R2")]

  ## --- 2. assemble tidy data‑frame -----------------------------
  out <- data.frame(
    metric     = c("AIC","BIC","R2"),
    Linear     = lin[ c("AIC","BIC","R2") ],
    NonLinear  = nonl[ c("AIC","BIC","R2") ],
    Best       = best[ c("AIC","BIC","R2") ],
    BestModel  = rep(res$best_type, 3),
    Psi        = rep(res$psi_best, 3),
    CI_low     = rep(res$psi_best_ci[1], 3),
    CI_high    = rep(res$psi_best_ci[2], 3),
    CI_method  = rep(res$ci_method, 3),
    P_val      = rep(res$validate$p.value, 3),
    Variable   = rep(var_name, 3),       # optional: populate if supplied
    row.names  = NULL,
    check.names = FALSE
  )
  out
}

