#' Calculate parcel set and plot coefficient.
#'
#' @import rstan
#' @import reshape2
#' @import ggplot2
#' @import cowplot
#' @export
#'
plot_coeff <- function(fit,response,alpha = 0.05,title = "",brain_slices = NULL) {
  if (!inherits(fit, "stanfit"))
    stop("Not a stanfit object.")
  if (fit@mode != 0)
    stop("Stan model does not contain posterior draws.")

  beta = rstan::extract(fit,pars = "B")[[1]]
  num_sim = dim(beta)[1]
  col_number = 2
  beta_2_perc = apply(beta[,,col_number],MARGIN = 2,function(vec)
    quantile(vec,probs = c(alpha,
                           0.5,
                           1-alpha)))
  df_combo = data.frame(x = factor(response, levels = response),
                        t(beta_2_perc))
  names(df_combo) = c("channel_name","low","mid","high")
  df_combo$confidence = abs(df_combo$mid)/abs(df_combo$high-df_combo$low)
  detect_sign = function(a,b) {
    if(sign(a) > 0 & sign(b) > 0) { return("positive") }
    else if(sign(a) < 0 & sign(b) < 0) { return("negative") }
    else { return("unclear") }
  }
  df_combo$sign = sapply(1:nrow(df_combo),function(i)
    detect_sign(df_combo$low[i],df_combo$high[i])) %>%
    factor(.,levels = c("positive","negative","unclear"))
  df_combo$channel_name = factor(df_combo$channel_name,levels = df_combo$channel_name)

  # FDR
  beta_da_pos = lapply(1:num_sim,function(i) beta[i,,col_number] < 0) %>% Reduce('+', .)
  beta_da_neg = lapply(1:num_sim,function(i) beta[i,,col_number] > 0) %>% Reduce('+', .)
  # posterior expected FDR
  # see Mitra, Mueller, and Ji (2016) for an application to connectivity
  fdr_threshold = function(beta_da,FDR) {
    ps = beta_da / num_sim
    ps[ps==0] = 1/num_sim
    seq_kappa = seq(0.5,1,1/num_sim/10)
    seq_kappa = seq_kappa[-length(seq_kappa)]
    df_kappa = sapply(seq_kappa,function(kappa) {
      I = ifelse(test = ps>kappa,yes = 1,no = 0)
      FDR_kappa = sum((1-ps)*I)/sum(I)
      c(kappa=kappa,FDR_kappa=FDR_kappa)
    }) %>% t %>% data.frame
    kappa = df_kappa[which(df_kappa$FDR_kappa <= FDR)[1],"kappa"]
    ps > kappa
  }

  # fdr control
  #ids = fdr_threshold(c(beta_da_neg,beta_da_pos),alpha)

  # uncorrected
  ids = c(beta_da_neg,beta_da_pos)/num_sim <= alpha

  neg_ids = ids[1:length(beta_da_neg)] %>% which
  pos_ids = ids[(length(beta_da_neg)+1):length(ids)] %>% which
  if(!is.null(brain_slices)) {
    # coefficients
    p_coeff = ggplot(df_combo, aes(x=mid, y=channel_name)) +
      geom_vline(xintercept = 0, col = "red") +
      geom_point() +
      geom_segment(mapping=aes(x=low, y=channel_name, xend=high, yend=channel_name)) +
      labs(title = title) +
      xlab("coefficient") +
      theme(axis.title.y = element_blank())
    p_coeff

    # brain
    ps = lapply(neg_ids,function(i) plot_brain(brain_slices[i],title = paste0("R",i)) )
    p_brains_neg = do.call(plot_grid,c(ps,ncol = 4,nrow = 3)) + ggtitle("Parcel Set 1")
    ps = lapply(pos_ids,function(i) plot_brain(brain_slices[i],title = paste0("R",i)) )
    p_brains_pos = do.call(plot_grid,c(ps,ncol = 4,nrow = 3)) + ggtitle("Parcel Set 2")

    # combine
    plot_grid(p_brains_neg,
              p_coeff,
              p_brains_pos,
              ncol = 3,align = "h")
  } else {
    return(list(set1 = pos_ids,set2 = neg_ids))
  }
}
