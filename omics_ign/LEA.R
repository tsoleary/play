# LEA package with Zoe ---------------------------------------------------------

require(LEA)

setwd(here::here("omics_ign"))

# landscape biological applications
# genomic structure across the landscape just using R
# can't handle super large data sets

data("tutorial")
write.lfmm(tutorial.R, "genotypes.lfmm")
write.geno(tutorial.R, "genotypes.geno")
write.env(tutorial.C, "gradients.env")

# PCA

pc <- pca("genotypes.lfmm", scale = TRUE)

tw <- tracy.widom(pc)
tw$pvalues[1:5]
plot(tw$percentage)
# look for the knee in the scree plot
# the first 4 or 5, so there are probably 4 or 5 groups that are useful for
# this analysis

# K = ancestral populations
# entropy checks which # of K is best for data

project <- NULL
project <- snmf("genotypes.geno",
                K = 1:10,
                entropy = TRUE,
                repetitions = 10,
                project = "new") # it will be below 10 based on the pca scree plot


plot(project, col = "blue", pch = 19, cex = 1.2)
# 4 is the knee of this plot which means that it is the mostly like number of 
# ancestral pops

# select the best run for K = 4
best <- which.min(cross.entropy(project, K = 4))
bp <- barchart(project, K = 4, run = best,
               border = NA, space = 0,
               xlab = "Ind.",
               ylab = "Ancestry",
               col = c("tomato", "lightblue", "olivedrab", "gold"))
axis(1, at = 1:length(bp$order),
     labels = bp$order, las = 1, cex.axis = 0.8)

# pop diff test
p <- snmf.pvalues(project,
                  entropy = TRUE,
                  ploidy = 2,
                  K = 4)
# manhattan plot
plot(-log(p$pvalues), pch = 19, cex = 0.7)


