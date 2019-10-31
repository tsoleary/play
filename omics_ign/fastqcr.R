# omics_ign Baxter read QC -----------------------------------------------------

# quality control of sequence reads fastq files - before fasta files

# fastq is a list of all the reads 
# name of the seuence barcode and ~ 100-200 letters
# fastqc us good but it doesn't organize the output files into separate samples
# fastqcr is a translated version of the command line program fastqc

#install.packages("fastqcr")
library(fastqcr)
library(tidyverse) # purrr & dplyr

fastqc(fg.dir = "~/path/to/reads.fastq", 
       qc.dir = "~path/to/output",
       threads = 2) #some number of threads that the computing cluster has

qc.dir <- system.file("fastqc_results", package = "fastqcr")

files <- list.files(qc.dir)

# pool all the samples together

qc <- qc_aggregate(qc.dir)

qc_stats(qc)
# sample # percent duplicate # percent GC # total seq # sequence length range

probs <- qc_problems(qc, "module", compact = FALSE)
# sequence length distribution is funky -- often gives warning

# to get a specific sample
qc_problems(qc, "sample", name = "S4")

# to get a specific module
qc_problems(qc, "module", name = "GC content")

# output all results in an .html document
qc_report(qc.dir, result.file = "mult-qc-result", experiment = "Experiment")

# plotting ---------------------------------------------------------------------
s1qc <- qc_read(paste(qc.dir, files[1], sep = "/"))
qc_plot(s1qc, "Per base sequence quality")

# only want to plot the samples that failed

s4qc <- qc_read(paste(qc.dir, files[4], sep = "/"))
s4probs <- qc_problems(qc, "sample", name = "S4") %>%
  filter(status == "FAIL") %>%
  select(module)

map(s4probs, qc_plot, qc = s4qc)
# there are two peaks of GC content -- it should be single modal

qc_plot(s4qc, modules = "all")

# plot all tests
modules <- summary(qc)[-10,1]
map(modules, qc_plot, qc = s4qc)