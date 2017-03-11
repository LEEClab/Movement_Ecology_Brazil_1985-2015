###########################################################################
#
# Understanding the scientific production on Movement Ecology in Brazil
# 
# Bernardo Niebuhr <bernardo_brandaum@yahoo.com.br>
# Milene Alves-Eigenheer <mileneaae@gmail.com>
#
# Citation: Alves-Eigenheer et al. Challenges and perspectives of 
#   movement ecology research in Brazil.
#   Submitted to Perspectives in Ecology and Conservation
#
# Feel free to use, modify, and share
# No rights in this world are reserved
###########################################################################

# Loading packages
require(maptools)
require(RColorBrewer)
require(descr)
require(plotrix)

# OPTION
# Plot figures?
plot_figures <- TRUE
fig_format_png <- TRUE # png if TRUE, tiff otherwise

# Setting working directory
datadir <- "/home/leecb/Documentos/UNESP/artigos/manuscrito_Alves_Eigenheer_et_al_2017_MEB_N&C/submissao2/new_data/Data"
mapdir <- "/home/leecb/Documentos/UNESP/artigos/manuscrito_Alves_Eigenheer_et_al_2017_MEB_N&C/submissao2/new_data/Data/shapes"
resultsdir <- "/home/leecb/Documentos/UNESP/artigos/manuscrito_Alves_Eigenheer_et_al_2017_MEB_N&C/submissao2/new_data/Results"

setwd(datadir)

# Reading literature survey data
literature <- read.table("MEB_SUPPL_MAT_Literature_survey_mov_ecology_Brazil.csv", header = T, sep = "\t")

#View(literature)
#paste(levels(literature$Journal), collapse = ", ")

####################################
# Analysis

####################################
# 1) Publication along time

# Organizing
studies_time.aux <- c(rep(0, 9), table(literature$Year_of_publication))
studies_time <- matrix(NA,length(studies_time.aux),2)
studies_time[,1] <- 1985:2015
studies_time[,2] <- studies_time.aux
studies_time <- as.data.frame(studies_time)
colnames(studies_time) <- c("Year", "N_studies")

# Plotting
if(plot_figures) {
  setwd(resultsdir)
  
  name1 <- "Figure_1_publication_year"
  if(fig_format_png) {
    png(paste(name1, "png", sep = "."), width = 15, height = 12, units = "cm", res = 300)
  } else {
    tiff(paste(name1, "tif", sep = "."), width = 15, height = 12, units = "cm", res = 300)
  }
  
  plot(studies_time$Year, studies_time$N_studies, cex.lab = 1.4,
       ylab = "Number of studies", xlab = "Year", pch = 20)
  lines(studies_time$Year, studies_time$N_studies)
  dev.off()
}

# Exporting table
write.table(studies_time, "Figure_1_publication_year.csv", sep = "\t", dec = ".", quote = F, row.names = F)

# Calculating statistics

# 50% of studies were published since...
studies_time$freq <- studies_time$N_studies/sum(studies_time$N_studies)
studies_time$cumsum <- cumsum(studies_time$freq)
studies_time
# The 50% was reached between 2009 and 2010

# Rate of increase in studies per year
studies_time$incease_year <- NA
for(i in 2:nrow(studies_time)) {
  if(studies_time$N_studies[(i-1)] != 0)  studies_time$incease_year[i] <- 100*(studies_time$N_studies[i]/studies_time$N_studies[(i-1)] - 1)
}

# Average rate of increase in studies per year, since 1999
mean(studies_time$incease_year[16:nrow(studies_time)], na.rm = T)

####################################
# 2) Maps of scientific production

# Change to map dir
setwd(mapdir)

# Load maps - states and scientific production (Literature survey and MEB)
statesBR  <- readShapePoly(fn='BRASIL.shp')
# Research centers
centers <- readShapePoints(fn='centros.shp')

# Updated table of number of studies per state
n_studies_state <- table(literature$Author_affiliations_Brazilian_state)[-c(3,5,7,8,11,12,14,15,20,22,29,31,32,34:36)]
# Changing DF to GO
n_studies_state[7] <- n_studies_state[7] + n_studies_state[5]
n_studies_state <- n_studies_state[-5] # removing DF

# Adding updated literature data to the attribute table of states map
statesBR@data$updated_literature_papers <- 0
cont <- 1
for(i in attr(n_studies_state, "names")) {
  statesBR@data[statesBR@data$UF == i,]$updated_literature_papers <- n_studies_state[cont]
  cont <- cont + 1
}

lev <- unique(sort(statesBR@data$updated_literature_papers))

cols_possible <- paste(rep('grey', 53), rev(seq(30,(30+53),1)), sep = "")
cols <- cols_possible[statesBR@data$updated_literature_papers+1]

n_part_MEB <- c(0.5, 0.8, 1.1, 1.7, 2.2, 6)[as.factor(statesBR@data$Participan)]
#n_part_MEB <- ifelse(is.na(statesBR@data$Participan), "", as.character(statesBR@data$Participan))
col = 'orange'

if(plot_figures) {
  setwd(resultsdir)
  
  name2 <- "Figure_2_publication_map"
  if(fig_format_png) {
    png(paste(name2, "png", sep = "."), width = 20, height = 20, units = "cm", res = 300)
  } else {
    tiff(paste(name2, "tif", sep = "."), width = 20, height = 20, units = "cm", res = 300)
  }
  
  plot(statesBR, col=cols, lty=1)
  for(i in 1:length(n_part_MEB)) {
    points(statesBR@polygons[[i]]@labpt[1], statesBR@polygons[[i]]@labpt[2], cex=n_part_MEB[i], col=col, pch = 19)
    # if(i == 11 | i == 24) {
    #   text(statesBR@polygons[[i]]@labpt[1]-0.5, statesBR@polygons[[i]]@labpt[2], label=n_part_MEB[i])
    # } else {
    #   if(i == 18) {
    #     text(statesBR@polygons[[i]]@labpt[1]+0.3, statesBR@polygons[[i]]@labpt[2]-0.1, label=n_part_MEB[i])
    #   } else {
    #     text(statesBR@polygons[[i]]@labpt[1], statesBR@polygons[[i]]@labpt[2], label=n_part_MEB[i])
    #   }
    # }
  }
  plot(centers, col = 2, pch=19, cex = 0.8, add=T)
  
  # Legends
  points(x=-70, y=-18, col = col, cex = 0.5, pch = 19)
  points(x=-68.5, y=-18, col = col, cex = 0.8, pch = 19)
  points(x=-67, y=-18, col = col, cex = 1.7, pch = 19)
  points(x=-64, y=-18, col = col, cex = 6, pch = 19)
  text(x=-70, y=-20.5, label = 1, cex = 0.9)
  text(x=-67, y=-20.5, label = 5, cex = 0.9)
  text(x=-64, y=-20.5, label = 79, cex = 0.9)
  text(x=-67,y=-15.5,label='I MEB attendees', cex=1)
  
  points(x = -71.5, y = -23, col = "red", cex = 1, pch = 19)
  text(x=-66,y=-23,label='Research groups', cex=1)
  
  color.legend(xl=-73.5, xr=-60,yb=-27,yt=-28.5, 
               legend=c('0','','55'), 
               rect.col=sort(cols, decreasing = T), gradient='x',
               cex=.8, pos=c(1,1,1))
  text(x=-67,y=-26,label='Number of publications in the literature', cex=1)
  dev.off()
}

# Calculating statistics

# MEB

# MEB Participants per state/region
part <- c()
for(i in 1:nrow(statesBR@data))
{
  if(!is.na(statesBR@data$Participan[i]))
  {
    part <- c(part, rep(as.character(statesBR@data$REGIAO[i]), statesBR@data$Participan[i]))
  }
}
part

# Number of participants
table(part)
# Percentage of participants
table(part)/sum(table(part))

# Literature 

# Number of studies with first authors from Brazilian institutions
sum(n_studies_state)
# Percentage of studies with first authors from Brazilian institutions
sum(n_studies_state)/nrow(literature)

# Studies per state/region
stud <- c()
for(i in 1:nrow(statesBR@data))
{
  if(statesBR@data$updated_literature_papers[i] > 0)
  {
    stud <- c(stud, rep(as.character(statesBR@data$REGIAO[i]), statesBR@data$updated_literature_papers[i]))
  }
}
stud

# Number of studies per region
table(stud)
# Percentage of studies per region
table(stud)/sum(table(stud))

# Studies made in SP, RJ, MG, RS, PR
sort(table(literature$Author_affiliations_Brazilian_state))
# Only Brazilian institutions - number of studies per state
sort(n_studies_state)
# Number of studies
cumsum(sort(n_studies_state, decreasing = T))
# Proportion of Brazilian studies
cumsum(sort(n_studies_state, decreasing = T))/sum(n_studies_state)

# Number of studies with first authors from foreign institutions
nrow(literature) - sum(n_studies_state)
(nrow(literature) - sum(n_studies_state))/nrow(literature)

# Studies from foreign institutions
n_studies_for <- table(literature$Author_affiliations_Brazilian_state)[c(3,5,7,8,11,12,14,15,20,22,29,31,32,34:36)]
sort(n_studies_for, decreasing = T)

# Number/percentage of foreign studies from USA or UK
cumsum(sort(n_studies_for, decreasing = T))
cumsum(sort(n_studies_for, decreasing = T))/sum(n_studies_for)

# Number of foreign studies with Brazilian collaboration
foreign_lit <- literature[!is.na(literature$Foreign_collaboration_Brazilian_state),]
with_collab <- foreign_lit[foreign_lit$Foreign_collaboration_Brazilian_state != 'NO',]
# Number of studies
nrow(with_collab)
# Percentage of studies
nrow(with_collab)/nrow(foreign_lit)

# Number of foreign studies without Brazilian collaboration
nrow(foreign_lit) - nrow(with_collab)
# Percentage of all studies
(nrow(foreign_lit) - nrow(with_collab))/nrow(literature)

#####
# Looking at the most cited papers

# 50 most cited papers
most_cited <- literature[order(literature$Citations, decreasing = T),][1:50,]
# Most cited papers from Brazilian insitition first authors
most_cit_bras <- most_cited[is.na(most_cited$Foreign_collaboration_Brazilian_state),]
# Number of papers
length(most_cit_bras)

####################################
# 3) Components of the mov ecol framework (Nathan et al. 2008)

# Literature

# Number of studies in each component
mov_ecol_lit <- data.frame(Component = colnames(literature)[9:14], Number_of_studies = rep(NA, 6))
mov_ecol_lit$Number_of_studies <- colSums(literature[9:14])

# Proportion of studies in each component
mov_ecol_lit$Prop_studies <- mov_ecol_lit$Number_of_studies/nrow(literature)

# MEB

# Number of people participating in MEB
meb_total <- 103
# Number of people that actually studied movement
meb_mov_tot <- 58

# Proportion of participants that actually studied movement
meb_mov_tot/meb_total

# Number of MEB particpants studying each component
# Data gathered from the table outside R
mov_ecol_MEB <- data.frame(mov_ecol_lit$Component, Number_participants = c(27, 10, 14, 8, 30, 15))
# Proportion of MEB particpants studying each component
mov_ecol_MEB$Proportion_participants <- mov_ecol_MEB$Number_participants/meb_mov_tot
mov_ecol_MEB

# Chi-square test - was there any difference between MEB and the literature?
comp.chi <- cbind(mov_ecol_lit$Number_of_studies, mov_ecol_MEB$Number_participants)
rownames(comp.chi) <- mov_ecol_lit$Component

chisq.test(comp.chi)

# Final table
comp_table <- cbind(mov_ecol_lit, mov_ecol_MEB[,-1])
colnames(comp_table)[2:5] <- c("Literature_number_studies", "Literature_prop_studies", 
                               "MEB_number_studies", "MEB_prop_studies")

# Exporting table
setwd(resultsdir)
write.table(comp_table, "Figure_3_mov_ecol_framework.csv", sep = "\t", dec = ".", quote = F, row.names = F)

####################################
# 4) Biomes studied

# Literature

# Number of studies in each biome
biome_lit <- table(literature$Biome_standardized)

biome <- data.frame(biome_lit)
colnames(biome) <- c("Biome", "Literature_number_studies")

# Adding simulation studies
biome <- rbind(biome, data.frame(Biome = "No_biome_simulation", Literature_number_studies = sum(is.na(literature$Biome_standardized))))

# Proportion of studies in each biome
biome$Literature_prop_studies <- biome$Literature_number_studies/nrow(literature)

# MEB Participants data

setwd(datadir)
meb <- read.table("MEB_SUPPL_MAT_MEB_participants.csv", header = T, sep = "\t")

# Number of people participating in MEB
meb_total <- nrow(meb)
# Number of people that actually studied movement
meb_mov_tot <- sum(meb$Works_with_movement == "Yes")

# Proportion of participants that actually studied movement
meb_mov_tot/meb_total

# Number of MEB particpants studying in each biome
biome_MEB <- data.frame(Biome = biome$Biome, MEB_number_studies = c(table(meb$Biome_standardized)[1:6], 0, table(meb$Biome_standardized)[7], NA))
# Number of studies with simulation
biome_MEB$MEB_number_studies[9] <- sum(meb$Works_with_movement == "Yes" & is.na(meb$Biome_standardized))

# Proportion of MEB particpants studying in each biome
biome_MEB$MEB_prop_studies <- biome_MEB$MEB_number_studies/meb_mov_tot
biome_MEB

# Chi-square test - was there any difference between MEB and the literature?
biome.chi <- cbind(biome$Literature_number_studies, biome_MEB$MEB_number_studies)
rownames(biome.chi) <- biome$Biome

chisq.test(biome.chi)

biome.chi2 <- cbind(biome$Literature_prop_studies, biome_MEB$MEB_prop_studies)
rownames(biome.chi2) <- biome$Biome
biome.chi2

# Final table
biome_table <- cbind(biome, biome_MEB[,-1])

# Exporting table
setwd(resultsdir)
write.table(biome_table, "Figure_4_biome.csv", sep = "\t", dec = ".", quote = F, row.names = F)


####################################
# 5) Taxon groups studied

# Literature

# Number of studies with each taxon
taxon_lit <- table(literature$Taxon)

taxon <- data.frame(taxon_lit)
colnames(taxon) <- c("Taxon", "Literature_number_studies")

# Proportion of studies with each taxon
taxon$Literature_prop_studies <- taxon$Literature_number_studies/nrow(literature)

# MEB Participants data

# Dividing participants who studied many taxa
pattern <- "/"
tax <- c()
cont <- 0
for(i in meb$Taxon) {
  # Did the participant studied more than one taxon?
  if(grepl(pattern, i)){
    # if yes
    tax <- c(tax, strsplit(i, pattern)[[1]])
    cont <- cont + 1
  } else {
    # if not
    tax <- c(tax, i)
  }
}
tax <- tax[!is.na(tax)]
tax

# How many participants studied more than one taxon?
cont

# Number of MEB particpants studying each taxon
tab <- table(tax)[-1]
taxon_MEB <- data.frame(Taxon = taxon$Taxon, MEB_number_participants = c(tab[1:5], cont, tab[6:8]))

# Proportion of MEB particpants whose focus was movement, studying each taxon
taxon_MEB$MEB_prop_participants <- taxon_MEB$MEB_number_participants/meb_mov_tot
taxon_MEB
rownames(taxon_MEB) <- 1:nrow(taxon_MEB)

# Chi-square test - was there any difference between MEB and the literature?
taxon.chi <- cbind(taxon$Literature_number_studies, taxon_MEB$MEB_number_participants)
rownames(taxon.chi) <- taxon$Taxon

chisq.test(taxon.chi)

taxon.chi2 <- cbind(taxon$Literature_prop_studies, taxon_MEB$MEB_prop_participants)
rownames(taxon.chi2) <- taxon$Taxon
taxon.chi2

# Final table
taxon_table <- cbind(taxon, taxon_MEB[,-1])

# Exporting table
setwd(resultsdir)
write.table(taxon_table, "Figure_5_taxon.csv", sep = "\t", dec = ".", quote = F, row.names = F)


####################################
# 6) Technologies used

# Literature

# Number of studies with each techonology
tech_lit <- table(literature$Technology_used)

techno <- data.frame(tech_lit)
colnames(techno) <- c("Techonology", "Literature_number_studies")

# Proportion of studies with each technology
techno$Literature_prop_studies <- techno$Literature_number_studies/nrow(literature)

# MEB Participants data

# Dividing participants who used many technologies
pattern <- ", "
tec <- c()
cont <- 0
for(i in meb$Techonology_used) {
  # Did the participant used more than one technology?
  if(grepl(pattern, i)){
    # if yes
    tec <- c(tec, strsplit(i, pattern)[[1]])
    cont <- cont + 1
  } else {
    # if not
    tec <- c(tec, i)
  }
}
tec <- tec[!is.na(tec)]
tec

# How many participants used more than one technology?
cont

# Number of MEB particpants using each technology
tab <- table(tec)
tech_MEB <- data.frame(Technology = techno$Techonology, MEB_number_participants = c(tab[1:6], 0, tab[7:9], 0, tab[10:11]))

# Proportion of MEB particpants whose focus was movement, using each technology
tech_MEB$MEB_prop_participants <- tech_MEB$MEB_number_participants/meb_mov_tot
tech_MEB

# Chi-square test - was there any difference between MEB and the literature?
techno.chi <- cbind(techno$Literature_number_studies, tech_MEB$MEB_number_participants)
rownames(techno.chi) <- techno$Techonology

chisq.test(techno.chi)

techno.chi2 <- cbind(techno$Literature_prop_studies, tech_MEB$MEB_prop_participants)
rownames(techno.chi2) <- techno$Techonology
techno.chi2

# Final table
techno_table <- cbind(techno, tech_MEB[,-1])

# Other technologies - summed proportion in the literature
sum(techno_table$Literature_prop_studies[c(1,2,4,5,7,12)])
# No techonogies used in the literaure - None or direct observation
sum(techno_table$Literature_prop_studies[c(3,10)])

# Other technologies - summed proportion in the I MEB
sum(techno_table$MEB_prop_participants[c(1,2,4,5,7,12)])
# No techonogies used in the literaure - None or direct observation
sum(techno_table$MEB_prop_participants[c(3,10)])

# Literature studies with modeling developed in Physics departments
nrow(literature[literature$Technology_used == "Modeling",])
literature[literature$Technology_used == "Modeling",]$Authors
# Looking each study, total = 25
# Proportion
25/nrow(literature[literature$Technology_used == "Modeling",])

# Exporting table
setwd(resultsdir)
write.table(techno_table, "Figure_6_technology.csv", sep = "\t", dec = ".", quote = F, row.names = F)

###################
# Missing:

# Challenges - to be standardized

# Citations per country - do we keep that?