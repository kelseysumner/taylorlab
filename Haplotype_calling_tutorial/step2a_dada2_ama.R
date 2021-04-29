# ----------------------------------------- #
#       Haplotype Calling - Embatalk        #
#                Phase 1 and 2              #
#                 AMA Script                #
#               March 31, 2020              #
#                K. Sumner                  #
# ----------------------------------------- #


# directions for this tutorial can be found at: http://benjjneb.github.io/dada2/tutorial.html


#### --------- load packages ----------------- ####

# set up the libpaths
.libPaths(c("/gpfs/fs1/data/itlab/helmod/apps/Core/R_packages/3.5.1-gcb01", .libPaths()))
.libPaths( c("~/Rlibs", .libPaths() ) )

# load the dada2 library
library("dada2")


#### ----------------- AMA -------------------- ####

# read in the path to your folder of fastq files for AMA
path <- "/data/taylorlab/kms94/haplotype_tutorial/out/fastq/AMA/all_samples" 
list.files(path)

# filter and trim the fastq files
# Sort ensures forward/reverse reads are in same order
fnFs <- sort(list.files(path, pattern="_1.fastq.gz"))
fnRs <- sort(list.files(path, pattern="_2.fastq.gz"))

# check the files first
any(duplicated(c(fnFs, fnRs)))

# Extract sample names, assuming filenames have format: SAMPLENAME_X.fastq
# note: the string manipulations may have to be modified, especially the extraction of sample names from the file names
sample.names <- sapply(strsplit(fnFs, "_"), `[`, 1)
# Specify the full path to the fnFs and fnRs
fnFs <- file.path(path, fnFs)
fnRs <- file.path(path, fnRs)

# performing filtering and trimming
# first define the filenames for the filtered fastq.gz files
filt_path <- file.path(path, "filtered") # Place filtered files in filtered/ subdirectory
filtFs <- file.path(filt_path, paste0(sample.names, "_F_filt.fastq.gz"))
filtRs <- file.path(filt_path, paste0(sample.names, "_R_filt.fastq.gz"))
# filter the forward and reverse reads
# remember that dada2 requires no Ns
out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs, maxN=0, maxEE=c(1,1), truncQ=2, rm.phix=FALSE,
                     compress=TRUE, multithread=FALSE, matchIDs = TRUE) 

# output summary of read trimming and filtering
write.csv(out,"/data/taylorlab/kms94/haplotype_tutorial/out/haplotype_output/AMA_trimAndFilterTable.csv")

# remove samples that had less than 50 reads after sampling
keep <- out[,"reads.out"] > 50 # Or other cutoff
filtFs <- filtFs[keep]
filtRs <- filtRs[keep]

# pull out sample names for the filtered reads
sample.names <- sapply(strsplit(basename(filtFs), "_"), `[`, 1)
sample.namesR <- sapply(strsplit(basename(filtRs), "_"), `[`, 1)
if(!identical(sample.names, sample.namesR)) stop("Forward and reverse files do not match.")
names(filtFs) <- sample.names
names(filtRs) <- sample.names
# set the random seed
set.seed(100)
# learn the error rates for your amplicon data set
errF <- learnErrors(filtFs, multithread=TRUE)
errR <- learnErrors(filtRs, multithread=TRUE)

# Sample inference and merger of paired-end reads
sample.names <- names(filtFs)
mergers <- vector("list", length(sample.names))
names(mergers) <- sample.names
for(sam in sample.names) {
  cat("Processing:", sam, "\n")
  derepF <- derepFastq(filtFs[[sam]])
  ddF <- dada(derepF, err=errF, multithread=TRUE)
  derepR <- derepFastq(filtRs[[sam]])
  ddR <- dada(derepR, err=errR, multithread=TRUE)
  merger <- mergePairs(ddF, derepF, ddR, derepR)
  mergers[[sam]] <- merger
}
rm(derepF); rm(derepR)

# Construct sequence table
seqtab <- makeSequenceTable(mergers)

# remove the chimeras
seqtab.nochim <- removeBimeraDenovo(seqtab, method="consensus", multithread=TRUE, verbose=TRUE)
dim(seqtab.nochim)
sum(seqtab.nochim)/sum(seqtab)
# note: most of your reads should remain after chimera removal
# it is not uncommon for a majority of the sequence variants to be removed (which we observed)

# write out the results without chimeras
saveRDS(seqtab.nochim, "/data/taylorlab/kms94/haplotype_tutorial/out/haplotype_output/AMA_haplotypes.rds")

# track reads through the pipeline
# look at the number of reads that made it through each step in the pipeline
## NOTE: not working for when you have filtered reads header so took that out (have number of reads filtered in previous table)
getN <- function(x) sum(getUniques(x))
track <- cbind(sapply(mergers, getN), rowSums(seqtab), rowSums(seqtab.nochim))
colnames(track) <- c("merged", "tabled", "nonchim")
rownames(track) <- sample.names
write.csv(track, "/data/taylorlab/kms94/haplotype_tutorial/out/haplotype_output/AMA_trackReadsThroughPipeline.csv")










