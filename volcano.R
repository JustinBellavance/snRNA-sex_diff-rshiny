##################################
# REACTIVE VOLCANO PLOT FUNCTION #
##################################
plotVolcano <- function(data, 
                        logfc_col, 
                        pvalue_col, 
                        gene_col, 
                        pvalue_thresh, 
                        logfc_thresh,
                        show_logfc_thresh,
                        show_pvalue_thresh,
                        highlight_genes = NULL,
                        x_label,
                        y_label,
                        legend_title,
                        xlim,
                        ylim,
                        de_vec,
                        is_male) {
  
  # check that columns exist
  if (!all(c(logfc_col, pvalue_col) %in% colnames(data))) {
    stop("provided column names do not match dataset")
  }
  
  # convert pval to -log10(pval)
  data <- mutate(data,
                 log_pval = -log10(data[[pvalue_col]]))
  
  # Reorder de_vec factors so that TRUE is first
  de_vec <- factor(de_vec, levels=c("TRUE", "FALSE"))

  is_male[de_vec == "FALSE"] <- "Not Significant"
  
  is_male <- factor(is_male, levels=c("TRUE", "FALSE", "Not Significant"))

  levels(is_male) <- c("Male", "Female", "Not Significant")
  
  # build base of plot
  volcano <- ggplot(data, aes(x = .data[[logfc_col]], y = log_pval))
  
  # if show_devec is true color by DE genes
  volcano <- volcano +
    geom_point(alpha = .6, aes(colour = is_male)) +
    scale_color_manual(values = c("#47A3FC","#FF69B4", "#808080" ))
  
  if (any(!is.null(c(xlim, ylim)))) {
    volcano <- volcano + coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE)
  }

   #if show_pvalue_thresh = true add vline layer
  if (show_pvalue_thresh) {
    volcano <- volcano + 
      geom_hline(yintercept = -log10(pvalue_thresh), linetype = "dashed", col = "grey", size = 1)
  }
  
  # if show_logfc_thresh = true add hline layer
  if (show_logfc_thresh) {
    volcano <- volcano + 
      geom_vline(xintercept = c(logfc_thresh, -logfc_thresh), linetype = "dashed", col = "grey", size = 1)
  }
  
  # if highlight_genes isn't null, add to plot
  # add error handling for missing gene_Col
  if (!is.null(highlight_genes)) {
    # create vector of gene ids for label
    labels <- data[data[[gene_col]] %in% highlight_genes,]
    volcano <- suppressWarnings(volcano +
        ggrepel::geom_label_repel(data = data[data[[gene_col]] %in% highlight_genes,], 
                                  aes(label = labels[[gene_col]]))) 
  }
  
  # add finishing touches to plot
  volcanoPlot <- volcano +
    labs(x = x_label, y = y_label, color = legend_title) +
    theme_classic(base_size = 12)
  
  # display plot
  volcanoPlot
}
