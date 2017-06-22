#' Plot differential correlations.
#'
#' @import rstan
#' @import reshape2
#' @import ggplot2
#' @import cowplot
#' @export
#'
plot_corr_da = function(fit,channel_names,title) {
  if (!inherits(fit, "stanfit"))
    stop("Not a stanfit object.")
  if (fit@mode != 0)
    stop("Stan model does not contain posterior draws.")

  Omega = rstan::extract(fit,pars = "Omega")[[1]]
  Omega = Omega[,2,,]

  # compare between conditions
  num_sim = dim(Omega)[1]
  P = lapply(1:num_sim,function(i) Omega[i,,] > 0) %>% Reduce('+', .) / num_sim
  P = abs(2*P-1)

  # posterior expected FDR
  # see Mitra, Mueller, and Ji (2016), Bayesian Graphical Models for Differential Pathways
  fdr_threshold = function(FDR) {
    seq_kappa = seq(0.5,1,1/num_sim/10)
    seq_kappa = seq_kappa[-length(seq_kappa)]
    df_kappa = sapply(seq_kappa,function(kappa) {
      ps = P[lower.tri(P)]
      I = ifelse(test = ps>kappa,yes = 1,no = 0)
      FDR_kappa = sum((1-ps)*I)/sum(I)
      c(kappa=kappa,FDR_kappa=FDR_kappa)
    }) %>% t %>% data.frame
    kappa = df_kappa[which(df_kappa$FDR_kappa <= FDR)[1],"kappa"]
    P_thres = ifelse(P > kappa,yes = FDR,no = NA)
    P_thres
  }
  fdrs = c(0.01,0.001)
  otherwise = paste0(">",fdrs[1])
  fdr_list = lapply(fdrs,fdr_threshold)
  Omega_fdr = do.call(pmin,c(fdr_list,na.rm = TRUE))
  Omega_fdr[is.na(Omega_fdr)] = otherwise

  # plot pairwise matrix
  get_upper_tri = function(cormat) {
    cormat[lower.tri(cormat)] = NA
    diag(cormat) = otherwise
    cormat
  }
  rownames(Omega_fdr) = colnames(Omega_fdr) = channel_names
  Omega_long = melt(get_upper_tri(Omega_fdr), na.rm = TRUE)
  Omega_long$value = factor(Omega_long$value,levels = c(otherwise,fdrs))
  ggplot(data = Omega_long, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_brewer(name = "FDR",palette = "Greens") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    coord_fixed() +
    ggtitle(title)
}
