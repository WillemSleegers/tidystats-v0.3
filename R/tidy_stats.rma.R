#' Create a tidy stats data frame from an rma object from the metafor package
#'
#' \code{tidy_stats.rma} takes an rma object and converts the object to a tidy stats data frame.
#'
#' @param model Output of \code{metafor::rma()}.
#' @param args Unused.
#'
#' @export
tidy_stats.rma <- function(model, args = NULL) {

  # Heterogeneity statistics

  # Check whether the model is from a univariate or multivariate analysis
  if (class(model)[1] == "rma.uni") {
    heterogeneity <- tibble::data_frame(
      k = model$k,
      `tau^2` = model$tau2,
      `tau^2 SE` = model$se.tau2,
      tau = sqrt(model$tau2),
      `I^2` = model$I2,
      `H^2` = model$H2
    )

    if (!is.null(model$R2)) {
      heterogeneity$`R^2` = model$R2
    }

    heterogeneity$QE = model$QE
    heterogeneity$df = model$k-model$p
    heterogeneity$p = model$QEp
  } else {

    heterogeneity <- tibble::data_frame(
      k = model$k,
      `sigma^2` = model$sigma2,
      sigma = sqrt(model$sigma2),
      Q = model$QE,
      df = model$k - model$p,
      p = model$QEp
    )
  }

  heterogeneity <- gather(heterogeneity, "statistic", "value")
  heterogeneity$group <- "heterogeneity"

  output <- heterogeneity

  # Moderator statistics
  if (model$p > 1 && !is.na(model$QM)) {

    moderator <- tibble::data_frame(`F` = model$QM)

    if (is.na(model$dfs)) {
      moderator$df <- model$m
    } else {
      moderator$numerator_df <- model$m
      moderator$denominator_df <- model$dfs
    }

    moderator$p = model$QMp

    moderator <- gather(moderator, "statistic", "value")

    moderator$group <- "moderators"

    output <- bind_rows(output, moderator)
  }

  # Coefficient statistics
  model_results <- tibble::data_frame(
    group = "coefficients",
    term = names(model$beta[,1]),
    term_nr = 1:length(model$beta),
    b = model$beta[,1],
    SE = model$se,
    z = model$zval,
    p = model$pval,
    lower = model$ci.lb,
    upper = model$ci.ub
  )

  # Get confidence interval level
  CI_level <- model$call$level
  if (is.null(CI_level)) {
    CI_level <- 95
  }

  names(model_results) <- stringr::str_replace(names(model_results), "lower",
                                      paste0((100-CI_level)/2, "% CI"))
  names(model_results) <- stringr::str_replace(names(model_results), "upper",
                                      paste0(100-(100-CI_level)/2, "% CI"))

  if (is.element(model$test, c("knha","adhoc","t"))) {
    model_results <- dplyr::rename(model_results, t = z)
  }

  model_results <- model_results %>%
    tidyr::gather("statistic", "value", -term, -term_nr, -group) %>%
    dplyr::arrange(term_nr)

  # Combine statistics
  output <- dplyr::bind_rows(output, model_results)

  # Re-order columns
  output <- dplyr::select(output, group, term, term_nr, statistic, value)

  # Add method information
  if (class(model)[1] == "rma.uni") {
    if (model$method == "FE") {
      if (model$int.only) {
        output$method <- "Fixed-Effects Model {metafor}"
      } else {
        output$method <- "Fixed-Effects with Moderators Model {metafor}"
      }
    } else {
      if (model$int.only) {
        output$method <- "Random-Effects Model {metafor}"
      } else {
        output$method <- "Mixed-Effects Model {metafor}"
      }
    }
  } else if (class(model)[1] == "rma.mv") {
    output$method <- "Multivariate Meta-Analysis Model {metafor}"
  } else {
    output$method <- "Not yet supported method."
  }

  # Add notes
  output$notes <- paste(model$method, "method")

  return(output)
}


# Leftover ----------------------------------------------------------------

#
# sigma2 <- model$sigma2
# tau2   <- model$tau2
# rho    <- model$rho
# gamma2 <- model$gamma2
# phi    <- model$phi
# sigma  <- sqrt(model$sigma2)
# tau    <- sqrt(model$tau2)
# gamma  <- sqrt(model$gamma2)
#
# if (model$withS) {
#   nlvls  <- model$s.nlevels
#   names  <- model$s.names
#
#   temp <- tibble::data_frame(
#     term = names,
#     nlvls = nlvls,
#     estim = sigma2,
#     sqrt = sigma
#   )
# }
#
# if (model$withG) {
#   ### note: use g.nlevels.f[1] since the number of arms is based on all data (i.e., including NAs), but use
#   ### g.nlevels[2] since the number of studies is based on what is actually available (i.e., excluding NAs)
#
#   nlvls  <- model$g.nlevels.k
#   names  <- model$g.names
#
#   temp <- tibble::data_frame(
#     term = names,
#     nlvls = nlvls,
#     estim = tau2,
#     sqrt = tau
#   )
#
#
#   if (is.element(model$struct[1], c("CS","AR","CAR","ID","SPEXP","SPGAU","SPLIN","SPRAT","SPSPH"))) {
#
#     vc <- cbind(tau2, tau, ifelse(model$vc.fix$tau2, "yes", "no"))
#     vc <- rbind(vc, c(rho, "", ifelse(model$vc.fix$rho, "yes", "no")))
#     colnames(vc) <- c("estim", "sqrt", "fixed")
#     rownames(vc) <- c("tau^2    ", "rho")
#     if (model$struct[1] == "ID")
#       vc <- vc[1,,drop=FALSE]
#
#   }
#
#   if (is.element(model$struct[1], c("HCS","HAR","DIAG"))) {
#
#     vc <- cbind(tau2, tau, x$g.levels.k, ifelse(x$vc.fix$tau2, "yes", "no"), x$g.levels.f[[1]])
#     vc <- rbind(vc, c(rho, "", "", ifelse(x$vc.fix$rho, "yes", "no"), ""))
#     colnames(vc) <- c("estim", "sqrt", "k.lvl", "fixed", "level")
#     if (length(x$tau2) == 1) {
#       rownames(vc) <- c("tau^2   ", "rho")
#     } else {
#       rownames(vc) <- c(paste("tau^2.", seq_along(x$tau2), "  ", sep=""), "rho")
#     }
#     if (x$struct[1] == "DIAG")
#       vc <- vc[seq_along(tau2),,drop=FALSE]
#     tmp <- capture.output(print(vc, quote=FALSE, right=right, print.gap=2))
#     .print.table(tmp, mstyle)
#
#   }
#
#   if (is.element(model$struct[1], c("UN","UNHO"))) {
#
#     if (model$struct[1] == "UN") {
#       vc <- cbind(tau2, tau, model$g.levels.k, ifelse(model$vc.fix$tau2, "yes", "no"), model$g.levels.f[[1]])
#     } else {
#       vc <- cbind(rep(tau2, length(model$g.levels.k)), rep(tau, length(x$g.levels.k)), model$g.levels.k, ifelse(rep(model$vc.fix$tau2,length(model$g.levels.k)), "yes", "no"), model$g.levels.f[[1]])
#     }
#     colnames(vc) <- c("estim", "sqrt", "k.lvl", "fixed", "level")
#     if (length(x$g.levels.k) == 1) {
#       rownames(vc) <- c("tau^2")
#     } else {
#       rownames(vc) <- paste("tau^2.", seq_along(x$g.levels.k), "  ", sep="")
#     }
#     tmp <- capture.output(print(vc, quote=FALSE, right=right, print.gap=2))
#     .print.table(tmp, mstyle)
#     cat("\n")
#
#     if (length(x$rho) == 1) {
#       G <- matrix(NA_real_, nrow=2, ncol=2)
#     } else {
#       G <- matrix(NA_real_, nrow=x$g.nlevels.f[1], ncol=x$g.nlevels.f[1])
#     }
#     G[upper.tri(G)] <- rho
#     G[lower.tri(G)] <- t(G)[lower.tri(G)]
#     diag(G) <- 1
#     #G[upper.tri(G)] <- ""
#
#     if (length(x$rho) == 1) {
#       G.info <- matrix(NA_real_, nrow=2, ncol=2)
#     } else {
#       G.info <- matrix(NA_real_, nrow=x$g.nlevels.f[1], ncol=x$g.nlevels.f[1])
#     }
#     G.info[upper.tri(G.info)] <- x$g.levels.comb.k
#     G.info[lower.tri(G.info)] <- t(G.info)[lower.tri(G.info)]
#     G.info[upper.tri(G.info)] <- ifelse(x$vc.fix$rho, "yes", "no")
#     diag(G.info) <- "-"
#
#     vc <- cbind(G, "", G.info)
#     colnames(vc) <- c(paste("rho.", abbreviate(x$g.levels.f[[1]]), sep=""), "", abbreviate(x$g.levels.f[[1]])) ### FIXME: x$g.levels.f[[1]] may be numeric, in which case a wrapping 'header' is not recognized
#     rownames(vc) <- x$g.levels.f[[1]]
#     tmp <- capture.output(print(vc, quote=FALSE, right=right, print.gap=2))
#     .print.table(tmp, mstyle)
#
#   }
#
#   if (is.element(x$struct[1], c("GEN"))) {
#
#     vc <- cbind(tau2, tau, ifelse(x$vc.fix$tau2, "yes", "no"), "")
#     colnames(vc) <- c("estim", "sqrt", "fixed", "rho:")
#     rownames(vc) <- x$g.names[-length(x$g.names)]
#
#     G.info <- formatC(cov2cor(x$G), digits=digits, format="f")
#     diag(G.info) <- "-"
#     G.info[upper.tri(G.info)] <- ifelse(x$vc.fix$rho, "yes", "no")
#     colnames(G.info) <- abbreviate(x$g.names[-length(x$g.names)])
#     vc <- cbind(vc, G.info)
#     tmp <- capture.output(print(vc, quote=FALSE, right=right, print.gap=2))
#     .print.table(tmp, mstyle)
#
#   }
#
# }
#
# if (x$withH) {
#
#   ### note: use h.nlevels.f[1] since the number of arms is based on all data (i.e., including NAs), but use
#   ### h.nlevels[2] since the number of studies is based on what is actually available (i.e., excluding NAs)
#
#
#   if (is.element(x$struct[2], c("CS","AR","CAR","ID","SPEXP","SPGAU","SPLIN","SPRAT","SPSPH"))) {
#
#     vc <- cbind(gamma2, gamma, ifelse(x$vc.fix$gamma2, "yes", "no"))
#     vc <- rbind(vc, c(phi, "", ifelse(x$vc.fix$phi, "yes", "no")))
#     colnames(vc) <- c("estim", "sqrt", "fixed")
#     rownames(vc) <- c("gamma^2  ", "phi")
#     if (x$struct[2] == "ID")
#       vc <- vc[1,,drop=FALSE]
#     tmp <- capture.output(print(vc, quote=FALSE, right=right, print.gap=2))
#     .print.table(tmp, mstyle)
#
#   }
#
#   if (is.element(x$struct[2], c("HCS","HAR","DIAG"))) {
#
#     vc <- cbind(gamma2, gamma, x$h.levels.k, ifelse(x$vc.fix$gamma2, "yes", "no"), x$h.levels.f[[1]])
#     vc <- rbind(vc, c(phi, "", "", ifelse(x$vc.fix$phi, "yes", "no"), ""))
#     colnames(vc) <- c("estim", "sqrt", "k.lvl", "fixed", "level")
#     if (length(x$gamma2) == 1) {
#       rownames(vc) <- c("gamma^2 ", "phi")
#     } else {
#       rownames(vc) <- c(paste("gamma^2.", seq_along(x$gamma2), "  ", sep=""), "phi")
#     }
#     if (x$struct[2] == "DIAG")
#       vc <- vc[seq_along(gamma2),,drop=FALSE]
#     tmp <- capture.output(print(vc, quote=FALSE, right=right, print.gap=2))
#     .print.table(tmp, mstyle)
#
#   }
#
#   if (is.element(x$struct[2], c("UN","UNHO"))) {
#
#     if (x$struct[2] == "UN") {
#       vc <- cbind(gamma2, gamma, x$h.levels.k, ifelse(x$vc.fix$gamma2, "yes", "no"), x$h.levels.f[[1]])
#     } else {
#       vc <- cbind(rep(gamma2, length(x$h.levels.k)), rep(gamma, length(x$h.levels.k)), x$h.levels.k, ifelse(rep(x$vc.fix$gamma2,length(x$h.levels.k)), "yes", "no"), x$h.levels.f[[1]])
#     }
#     colnames(vc) <- c("estim", "sqrt", "k.lvl", "fixed", "level")
#     if (length(x$h.levels.k) == 1) {
#       rownames(vc) <- c("gamma^2")
#     } else {
#       rownames(vc) <- paste("gamma^2.", seq_along(x$h.levels.k), "  ", sep="")
#     }
#     tmp <- capture.output(print(vc, quote=FALSE, right=right, print.gap=2))
#     .print.table(tmp, mstyle)
#     cat("\n")
#
#     if (length(x$phi) == 1) {
#       H <- matrix(NA_real_, nrow=2, ncol=2)
#     } else {
#       H <- matrix(NA_real_, nrow=x$h.nlevels.f[1], ncol=x$h.nlevels.f[1])
#     }
#     H[upper.tri(H)] <- phi
#     H[lower.tri(H)] <- t(H)[lower.tri(H)]
#     diag(H) <- 1
#     #H[upper.tri(H)] <- ""
#
#     if (length(x$phi) == 1) {
#       H.info <- matrix(NA_real_, nrow=2, ncol=2)
#     } else {
#       H.info <- matrix(NA_real_, nrow=x$h.nlevels.f[1], ncol=x$h.nlevels.f[1])
#     }
#     H.info[upper.tri(H.info)] <- x$h.levels.comb.k
#     H.info[lower.tri(H.info)] <- t(H.info)[lower.tri(H.info)]
#     H.info[upper.tri(H.info)] <- ifelse(x$vc.fix$phi, "yes", "no")
#     diag(H.info) <- "-"
#
#     vc <- cbind(H, "", H.info)
#     colnames(vc) <- c(paste("phi.", abbreviate(x$h.levels.f[[1]]), sep=""), "", abbreviate(x$h.levels.f[[1]])) ### FIXME: x$h.levels.f[[1]] may be numeric, in which case a wrapping 'header' is not recognized
#     rownames(vc) <- x$h.levels.f[[1]]
#     tmp <- capture.output(print(vc, quote=FALSE, right=right, print.gap=2))
#     .print.table(tmp, mstyle)
#
#   }
#
#   if (is.element(x$struct[2], c("GEN"))) {
#
#     vc <- cbind(gamma2, gamma, ifelse(x$vc.fix$gamma2, "yes", "no"), "")
#     colnames(vc) <- c("estim", "sqrt", "fixed", "phi:")
#     rownames(vc) <- x$h.names[-length(x$h.names)]
#
#     H.info <- formatC(cov2cor(x$H), digits=digits, format="f")
#     diag(H.info) <- "-"
#     H.info[upper.tri(H.info)] <- ifelse(x$vc.fix$phi, "yes", "no")
#     colnames(H.info) <- abbreviate(x$h.names[-length(x$h.names)])
#     vc <- cbind(vc, H.info)
#     tmp <- capture.output(print(vc, quote=FALSE, right=right, print.gap=2))
#     .print.table(tmp, mstyle)
#
#   }
#
# }
#
# }
