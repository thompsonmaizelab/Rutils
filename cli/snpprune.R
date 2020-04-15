#!/usr/bin/env Rscript

# handle dependency loading
if (!requireNamespace("optparse", quietly = TRUE)) {
    install.packages("optparse")
}
library("optparse")
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
library("BiocManager")
if (!("gdsfmt" %in% rownames(installed.packages()))) {
    BiocManager::install("gdsfmt")
}
library("gdsfmt")
if (!("SNPRelate" %in% rownames(installed.packages()))) {
    BiocManager::install("SNPRelate")
}
library("SNPRelate")

# handle command line options
option_list <- list(
    optparse::make_option(
        c("-i", "--invcf"),
        type = "character",
        default = NULL,
        help = "input VCF file name",
        metavar = "character"
    ),
    optparse::make_option(
        c("-g", "--outgds"),
        type = "character",
        default = NULL,
        help = "output GDS file name",
        metavar = "character"
    ),
    optparse::make_option(
        c("-m", "--method"),
        type = "character",
        default = "biallelic.only",
        help = "method to use for snpgdsVCF2GDS",
        metavar = "character"
    ),
    optparse::make_option(
        c("-l", "--ld"),
        type = "numeric",
        default = 0.8,
        help = "LD threshold for pruning",
        metavar = "numeric"
    ),
    optparse::make_option(
        c("-w", "--window"),
        type = "integer",
        default = 50000L,
        help = "Slinding window size for calculating LD",
        metavar = "integer"
    ),
    optparse::make_option(
        c("-o", "--outRdata"),
        type = "character",
        default = NULL,
        help = "output Rdata file name",
        metavar = "character"
    ),
    optparse::make_option(
        c("-v", "--verbose"),
        action = "store_true",
        default = FALSE,
        help = "print verbose messages"
    )
)

# create parsing object
opt_parser <- optparse::OptionParser(option_list=option_list)

# parse arguments
opt <- optparse::parse_args(opt_parser)

# extract command line arguments
invcf <- opt$invcf
outgds <- opt$outgds
method <- opt$method
ld <- opt$ld
window <- opt$ld
outRdata <- opt$outRdata
verbose <- opt$verbose

################################################################################
########################### SNP prunning based on LD ###########################
################################################################################

################################################################################
# convert VCF file to GDS file
if (verbose) { print("Converting VCF to GDS..."); flush.console(); }
SNPRelate::snpgdsVCF2GDS(
    vcf.fn = invcf,     # input vcf file
    out.fn = outgds,    # output gds file
    method = method     # method to use
)
if (verbose) { print("... Done!"); flush.console(); }

################################################################################
# print file summary
if (verbose) {
    print("Summary of created GDS file:")
    SNPRelate::snpgdsSummary(outgds)
    flush.console()
}

################################################################################
# open genotype file
if (verbose) { print("Opening GDS..."); flush.console(); }
geno_gds <- SNPRelate::snpgdsOpen(outgds)
if (verbose) { print("... Done!"); flush.console(); }

################################################################################
# prune marker data
if (verbose) { print("Pruning markers..."); flush.console(); }
mkr_ix_list <- SNPRelate::snpgdsLDpruning(  # select markers
    geno_gds,                               # genotype file
    ld.threshold = ld,                      # use this r squared as a threshold
    slide.max.bp = window                   # use sliding window of 50kb
)
mkr_ix <- unlist(unname(mkr_ix_list))       # flatten list to get marker indices
if (verbose) { print("... Done!"); flush.console(); }

################################################################################
# extract marker data as matrix
if (verbose) { print("Building genotype matrix..."); flush.console(); }
# NOTE: homozygous reference is coded as 2!
geno_mat <- SNPRelate::snpgdsGetGeno(       # extract genotype matrix
    geno_gds,                               # genotype file
    snp.id = mkr_ix                         # marker indices
)
colnames(geno_mat) <- gdsfmt::read.gdsn(    # extract column (marker) names
    gdsfmt::index.gdsn(                     # get internal object reference
        geno_gds,                           # genotype file
        "snp.rs.id"                         # get "snp.rs.id" field
    )
)[mkr_ix]                                   # subset using marker indices
rownames(geno_mat) <- gdsfmt::read.gdsn(    # extract row (sample) names
    gdsfmt::index.gdsn(                     # get internal object reference
        geno_gds,                           # genotype file
        "sample.id"                         # get "sample.id" field
    )
)
if (verbose) { print("... Done!"); flush.console(); }

################################################################################
# save genotype matrix to file
if (verbose) { print("Saving genotype matrix..."); flush.console(); }
save(geno_mat, file=outRdata)
if (verbose) { print("... Done!"); flush.console(); }
