# function to make a manhattan plot in R

source("R/check_subroutines.R")

cycle5_df <- read.csv("R/test.csv")

manhattan_plot <- function(df, chr = "chr", pos = "pos", pval = "pval",
        color = NULL, upbuf = 0.05, ylim = NULL, mai = NULL, omi = NULL,
        plot_nrow = NULL, plot_ncol = NULL
        ) {
    # get data vectors
    chr_vec <- df[,chr]     # get the chromosome row as a vector
    pos_vec <- df[,pos]     # get positions as a vector
    pval_vec <- df[,pval]   # get p value vector

    chr_uniq_vec <- unique(chr_vec)     # get vector of unique elements
    n_uniq_chr <- length(chr_uniq_vec)  # get number of unique elements
    nlog_pval_vec <- -log(pval_vec)     # get the negative log of p values
    nlog_pval_max <- max(nlog_pval_vec) # get max negative log p value

    # get graph limits
    if(is.null(ylim)) {
        ylim <- c(0, (nlog_pval_max * (1 + upbuf)))
    }

    # calculate rows and columns for the plot
    if( is.null(plot_nrow) ) {
        if( is.null(plot_ncol) ) {
            plot_nrow <- 1
            plot_ncol <- n_uniq_chr
        }
        else {
            check_is_integer(plot_ncol, "plot_ncol")
            plot_nrow <- as.integer(ceiling(n_uniq_chr/plot_ncol))
        }
    }
    else {
        if( is.null(plot_ncol) ) {
            check_is_integer(plot_nrow, "plot_nrow")
            plot_ncol <- as.integer(ceiling(n_uniq_chr/plot_nrow))
        }
        else {
            check_is_integer(plot_ncol, "plot_ncol")
            check_is_integer(plot_nrow, "plot_nrow")
        }
    }

    # fill colors to have the number of chromosomes
    if(is.null(color)) {
        color <- rep_len("#000000", n_uniq_chr)
    }
    else {
        color <- rep_len(color, n_uniq_chr)
    }

    # if plot par margins are NULL, get default
    if(is.null(mai)) {
        mai <- c(0.5, 0.5, 0.1, 0.1)
    }
    if(is.null(omi)) {
        omi <- c(0.6, 0.6, 0.6, 0.6)
    }

    # begin building the plot
    par(
        mfrow = c(plot_nrow, plot_ncol),
        mai = mai,
        omi = omi
    )

    # for each chromosome, make plot
    for(i in 1:n_uniq_chr) {
        mask <- chr_vec == chr_uniq_vec[i]      # build mask

        tmp_pos <- pos_vec[mask]                # get chromosome positions
        tmp_nlog_pval <- nlog_pval_vec[mask]    # get -log(p) for the chromosome

        if(i == 1) {
            plot(
                x = tmp_pos,        # chromosome positions
                y = tmp_nlog_pval,  # chromosome p values
                bty = 'n',          # remove border lines
                ylim = ylim,        # set y limits
                xlab = paste0(      # add x labels
                    chr_uniq_vec[i]
                ),
                col = color[i]      # set color
            )
        }
        else {
            plot(
                x = tmp_pos,        # chromosome positions
                y = tmp_nlog_pval,  # chromosome p values
                ylim = ylim,        # set y limits
                bty = 'n',          # remove border lines
                yaxt = 'n',         # remove y axis
                ylab = "",          # set to empty string
                xlab = paste0(      # add x labels
                    chr_uniq_vec[i]
                ),
                col = color[i]      # set color
            )
        }
    }

    # add title to par plot
    mtext("Manhattan plot", outer=TRUE, cex = 1.5)

    # add line
    abline(h = 10)
}


manhattan_plot(cycle5_df, pos="gmap_pos", color = c("red", "blue"))
