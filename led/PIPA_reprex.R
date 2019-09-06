# PIPA 2018 Photoquad analysis -------------------------------------------------
# Kanton files analyzed by M. Fox in photogrid 
# Niku, Orona, Rawaki images analyzed by L. Dissly in Coralnet 

library(splitstackshape)
library(tidyverse)
library(RColorBrewer)
library(scales)
library(reshape2)

# create a data folder that only has csv files for each island
setwd("~/Desktop/WHOI")

# open each island and combine all the data 
df <- read.csv(file = "beans_reprex.csv", header = TRUE)

# break out the information in picture name 
df <- df %>%
  separate("Name", c("Island","Site","Transect","Picture"), sep = "_")

# make island names lowercase
df$Island <- str_to_lower(df$Island)

# get rid of the .jpg in picutre
df$Picture <- gsub(".JPG", "", df$Picture)

# make sure that each pic has 100 points
if (sum(rowSums(df[, 5:ncol(df)]) != 100) == 0){
  print("All rows have 100 points!")
} else {
  non_100_rows <- which(rowSums(df[, 5:ncol(df)]) != 100)
  print(paste("Warning: incorrect number of points in row", non_100_rows))
}

# tidy the data!
all_genera <- colnames(df)[5:ncol(df)]
df_tidy <- df %>%
  gather(Genus, Cover, all_genera)

# create functional groups based on Genus
beens <- c("beans", "beansios")
kool <- c("cool", "coolio")

# function to assign each Genus to a functional group
make_func_groups <- function (dat){
  funct_groups <- c()
  for (i in 1:length(dat$Genus)) {
    g <- dat$Genus[i]
    if ( is.element(g, kool) ) {
      funct_groups <- c(funct_groups, "Cool")
    } else if ( is.element(g, beens) ) {
      funct_groups <- c(funct_groups, "Beans")
    } else {
      funct_groups <- c(funct_groups, "Other")
    }
  }
  funct_groups <- as.factor(funct_groups)
}

df_tidy$funct <- make_func_groups(df_tidy) 

#  calculate average percent cover per site 
mean_genus <- df_tidy %>% 
  group_by(Island, Site, Genus) %>% # group by island, site, genus
  summarize(N = sum(!is.na(Cover)), # count number of pictures 
            Avg = mean(Cover,na.rm=T), # take the mean
            sd = sd(Cover,na.rm=T),
            se = sd/sqrt(N))

# calculate average percent cover per functional group
# you have to sum the functional group then average the sums! :)
fun_gp_means <- df_tidy %>% 
  group_by(Island, Site, Transect, Picture, funct) %>%
  summarize(sum = sum(Cover, na.rm = TRUE)) %>%
  group_by(Island, Site, funct) %>%
  summarize(Avg = mean(sum, na.rm = TRUE))

# keep only the rows in which Avg != 0
fun_gp_means <- fun_gp_means[which(fun_gp_means$Avg != 0), ]

# Plotting ---------------------------------------------------------------------

# Functional groups --------------------

# function for plotting the benthic composition by functional group
plot_funct_group <- function(dat, sub_title_g){
  ggplot(dat ,aes(x = as.factor(Site), y = Avg, fill = as.factor(funct))) +
    geom_bar(stat = "identity", colour = "black") +
    labs(title = "Benthic Composition", subtitle = sub_title_g) +
    labs(x = "Site", y = "Percent Cover", fill = "Functional Group") +
    scale_fill_manual(values = c("navy", "brown", "coral", "seagreen3", 
                                 "hotpink1", "turquoise4", "yellow"))+
    theme_classic() +
    theme(text = element_text(size = 14))+
    theme(axis.text.x = element_text(colour = "black"), 
          axis.text.y = element_text(colour = "black"))
}

# plot all at islands at once
plot_all <- function(dat, FUN) {
  plot_list <- list()
  is <- unique(dat$Island)
  for (i in is){
    
    if (i == "nik") {
      is_full <- "Niku"
    } else if (i == "oro") {
      is_full <- "Orona"
    } else if (i == "raw") {
      is_full <- "Rawaki"
    }
    
    df_g <- filter(dat, Island == i)
    df_g <- top_n(df_g, n = 8, wt = Avg)
    g <- FUN(df_g, sub_title_g = paste("Island:", is_full))
    plot(g)
    plot_list[[i]] <- g
  }
  return(plot_list)
}

# plot all in viewer in rstudio
plot_all(fun_gp_means, FUN = plot_funct_group)

# Coral genus --------------------

# look at the composition of only the genera within the coral functional group
coral_means <- df_tidy %>% 
  filter(funct == "Cool") %>%
  group_by(Island, Site, Genus) %>%
  summarize(sum = sum(Cover, na.rm = TRUE)) %>%
  mutate(Avg = prop.table(sum) * 100)

# keep only the rows in which Avg != 0
coral_means <- coral_means[which(coral_means$Avg != 0), ]

# function to plot the coral compostion by genus
plot_coral <- function(dat, sub_title_g){
  ggplot(dat ,aes(x = as.factor(Site), y = Avg, fill = as.factor(Genus))) +
    geom_bar(stat = "identity", colour = "black") +
    labs(title = "Coral Composition", subtitle = sub_title_g) +
    labs(x = "Site", y = "Percent Cover", fill = "Genus", 
         caption = "Note: Only top 8 coral genera from each site plotted!") +
    theme_classic() +
    theme(text = element_text(size = 14))+
    theme(axis.text.x = element_text(colour = "black"), 
          axis.text.y = element_text(colour = "black"))
}

# plot all in viewer in rstudio
plot_all(coral_means, FUN = plot_coral)

# save all plots in a group to a single .pdf file
plots_to_pdf <- function(plot_list, file_name){
  pdf(file_name, width = 6, height = 4)
  for (i in 1:length(plot_list)) {
    print(plot_list[[i]])
  }
  dev.off()
}

# compile the plots in a .pdf in the working directory
coral_plots <- plot_all(coral_means, FUN = plot_coral)
plots_to_pdf(coral_plots, "coral_plots_reprex.pdf") # file written in working directory

funct_plots <- plot_all(fun_gp_means, FUN = plot_funct_group)
plots_to_pdf(funct_plots, "funct_plots_reprex.pdf") # file written in working directory

