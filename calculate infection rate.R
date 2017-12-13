## working directory
setwd("G:/NeuAll/Project/Vector-competence-ZIKV")

library(xlsx)
albopictus_1_1 <- read.xlsx("data/Aedes albopictus calabria.xlsx", 1)
albopictus_1_2 <- read.xlsx("data/Aedes albopictus calabria.xlsx", 3)

albopictus_2_1 <- read.xlsx("data/Aedes albopictus Deutschland.xlsx", 1)
albopictus_2_2 <- read.xlsx("data/Aedes albopictus Deutschland.xlsx", 2)
albopictus_2_3 <- read.xlsx("data/Aedes albopictus Deutschland.xlsx", 3)
albopictus_2_4 <- read.xlsx("data/Aedes albopictus Deutschland.xlsx", 4)

albopictus_3_1 <- read.xlsx("data/Aedes albopictus Freiburg.xlsx", 1)
albopictus_3_2 <- read.xlsx("data/Aedes albopictus Freiburg.xlsx", 2)
albopictus_3_3 <- read.xlsx("data/Aedes albopictus Freiburg.xlsx", 3)

albopictus_4_1 <- read.xlsx("data/Aedes albopictus Italien 2017.xlsx", 1)
albopictus_4_2 <- read.xlsx("data/Aedes albopictus Italien 2017.xlsx", 2)

albopictus_1_1_2 <- albopictus_1_1[ !is.na(albopictus_1_1[,2]), ]
albopictus_1_2_2 <- albopictus_1_2[ !is.na(albopictus_1_2[,2]), ]
albopictus_2_1_2 <- albopictus_2_1[ !is.na(albopictus_2_1[,2]), ]
albopictus_2_2_2 <- albopictus_2_2[ !is.na(albopictus_2_2[,2]), ]
albopictus_2_3_2 <- albopictus_2_3[ !is.na(albopictus_2_3[,2]), ]
albopictus_2_4_2 <- albopictus_2_4[ !is.na(albopictus_2_4[,2]), ]
albopictus_3_1_2 <- albopictus_3_1[ !is.na(albopictus_3_1[,2]), ]
albopictus_3_2_2 <- albopictus_3_2[ !is.na(albopictus_3_2[,2]), ]
albopictus_3_3_2 <- albopictus_3_3[ !is.na(albopictus_3_3[,2]), ]
albopictus_4_1_2 <- albopictus_4_1[ !is.na(albopictus_4_1[,2]), ]
albopictus_4_2_2 <- albopictus_4_2[ !is.na(albopictus_4_2[,2]), ]



albopictus_all <- data.frame(rbind(albopictus_1_1_2[,-c(1,21:22)],
                                   albopictus_1_2_2[,-c(1,21:22)],
                                   albopictus_2_1_2[,-c(1,21:22)],
                                   albopictus_2_2_2[,-c(1,21:22)],
                                   albopictus_2_3_2[,-c(1,21)],
                                   albopictus_2_4_2[,-c(1,21)],
                                   albopictus_3_1_2[,-c(1,21:22)],
                                   albopictus_3_2_2[,-c(1,21:22)],
                                   albopictus_3_3_2[,-c(1,21:22)],
                                   albopictus_4_1_2[,-c(1,21:22)],
                                   albopictus_4_2_2[,-c(1,21:22)]))
albopictus_all$temperature <- as.numeric(substr(albopictus_all$temperature, 1, 2))

albopictus_all <- albopictus_all[,1:19]

dimnames(albopictus_all)[[2]] <- c("experiment_no",  "start_date",      "species" ,        "origin",         
                                   "temperature",     "virus",           "input_total",     "blood_fed_total",
                                   "specimens", "body_part", "dpi",             "tube_id" ,       
                                   "ct_value" ,       "infection"  ,     "titre" ,          "titre_method" ,  
                                   "freezer" ,        "rack",            "box" )  

fdf2 <- subset(albopictus_all, virus %in% c("ZIKV"))
fdf2$infection <- as.numeric(as.vector(fdf2$infection))
fdf2$titre <- as.numeric(as.vector(fdf2$titre))
fdf2$ct_value <- as.numeric(as.vector(fdf2$ct_value))

source("R/rates.R")
fdf2$infection2 <- ifelse(fdf2$titre_method == "CPE" | fdf2$titre_method == "CPE/PCR", fdf2$infection, ifelse(fdf2$titre_method == "PCR",ifelse(fdf2$ct_value <= 35, 1, 0), NA))
fdf2$infection  <- fdf2$infection2

library(plyr)
rates_results <- rates(fdf2)
rates_results2 <- subset(rates_results, dpi >14)









japonicus_all <- data.frame(rbind(japonicus1_1,
                                  japonicus2_1,
                                  japonicus3_1))
japonicus_all$temperature <- as.numeric(substr(japonicus_all$temperature, 1, 2))

japonicus_all <- japonicus_all[,1:19]

dimnames(japonicus_all)[[2]] <- c("experiment_no",  "start_date",      "species" ,        "origin",         
                                  "temperature",     "virus",           "input_total",     "blood_fed_total",
                                  "specimens", "body_part", "dpi",             "tube_id" ,       
                                  "ct_value" ,       "infection"  ,     "titre" ,          "titre_method" ,  
                                  "freezer" ,        "rack",            "box" )  

#============================================================
# load external functions
#============================================================
source("R/general.R")
source("R/rates.R")
source("R/multiplefishertest.R")

japonicus_all$virus <- revalue(japonicus_all$virus,
                               c("Zika" = "ZIKV"))
fdf2 <- subset(japonicus_all, virus %in% c("ZIKV"))
#fdf2 <- subset(japonicus_all, virus %in% c("JEV"))

fdf2$infection <- as.numeric(as.vector(fdf2$infection))
fdf2$titre <- as.numeric(as.vector(fdf2$titre))
fdf2$ct_value <- as.numeric(as.vector(fdf2$ct_value))

source("R/rates.R")
fdf2$infection2 <- ifelse(fdf2$titre_method == "CPE" | fdf2$titre_method == "CPE/PCR", fdf2$infection, ifelse(fdf2$titre_method == "PCR",ifelse(fdf2$ct_value <= 35, 1, 0), NA))
fdf2$infection  <- fdf2$infection2

library(plyr)
rates_results <- rates(fdf2)
rates_results2 <- subset(rates_results, dpi >0 & dpi < 21)


oiki <- length(rates_results2$species)
fLL2_trans <- data.frame(species = rep(paste(rates_results2$species, rates_results2$origin), 2), 
                         typed = "saliva TR (%)***",
                   dpi = rep(paste(as.factor(rates_results2$dpi), "dpi", sep = " "), 2),
                   dpi_v = c(paste(as.factor(rates_results2$dpi), "dpi", sep = " "), rep("", oiki)),
                   perc2 = c(as.numeric(rates_results2$transmission_rate_A)/as.numeric(rates_results2$transmission_rate_B), rep(0, oiki)),
                   temperature = rep(as.factor(rates_results2$temperature), 2),
                   infection_status = c(rep("negative", oiki),rep("positive", oiki)),
                   infection = c(as.numeric(rates_results2$infection_rate_B),
                                 as.numeric(rates_results2$transmission_rate_A)))

fLL2_dissimination <- data.frame(species = rep(paste(rates_results2$species, rates_results2$origin), 2), 
                            typed = "leg DR (%)**",
                            dpi = rep(paste(as.factor(rates_results2$dpi), "dpi", sep = " "), 2),
                            dpi_v = c(paste(as.factor(rates_results2$dpi), "dpi", sep = " "), rep("", oiki)),
                            perc2 = c(as.numeric(rates_results2$dissimination_rate_A)/as.numeric(rates_results2$dissimination_rate_B), rep(0, oiki)),
                            temperature = rep(as.factor(rates_results2$temperature), 2),
                            infection_status = c(rep("negative", oiki),rep("positive", oiki)),
                            infection = c(as.numeric(rates_results2$infection_rate_B),
                                          as.numeric(rates_results2$dissimination_rate_A)))


fLL2_infecton <- data.frame(species = rep(paste(rates_results2$species, rates_results2$origin), 2), 
                         typed = "body IR (%)*",
                         dpi = rep(paste(as.factor(rates_results2$dpi), "dpi", sep = " "), 2),
                         dpi_v = c(paste(as.factor(rates_results2$dpi), "dpi", sep = " "), rep("", oiki)),
                         perc2 = c(as.numeric(rates_results2$infection_rate_A)/as.numeric(rates_results2$infection_rate_B), rep(0, oiki)),
                         temperature = rep(as.factor(rates_results2$temperature), 2),
                         infection_status = c(rep("negative", oiki),rep("positive", oiki)),
                         infection = c(as.numeric(rates_results2$infection_rate_B),
                                       as.numeric(rates_results2$infection_rate_A)))

fLL3 <- rbind(fLL2_trans, fLL2_dissimination, fLL2_infecton)

fLL77 <- fLL3 
fLL77$perc <- as.numeric(fLL77$perc2)*100
fLL88 <- fLL77[c(1:3, 7:9, 13:15),]
fLL88 <- fLL77[c(1:3, 7:9, 13:15),]
fLL88$typed <- revalue(fLL88$typed,
                       c("saliva TR (%)***"= "TR (%)***",
                         "leg DR (%)**" = "DR (%)**",
                         "body IR (%)*"= "IR (%)*"))
fLL88$typed = factor(fLL88$typed, levels=c("IR (%)*", "DR (%)**","TR (%)***"))

fLL3$perc2[is.nan(fLL3$perc2)] <- "na"
fLL3$perc <- c(paste(format(round(as.numeric(fLL3$perc2)*100, 1), nsmall = 1), " %", sep = ""))
fLL3$perc[c(4:6, 10:12, 16:18)] <- " "



library(ggplot2)
colnames(fLL3)
fLL3$infection_status
fLL3$typed = factor(fLL3$typed, levels=c("body IR (%)*", "leg DR (%)**","saliva TR (%)***"))

# Helper function for string wrapping. 
# Default 20 character target width.
swr = function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}

# Create line breaks in Year
fLL3$typed2 = swr(fLL3$typed)
fLL3$typed2[1:6] <- "saliva\nTR (%)***"
fLL3$typed2[7:12] <- "leg \nDR (%)**"
fLL3$typed2[13:18] <- "body \nIR (%)*"


png("tranmission_rate2.png",width = 5, height=3, units = 'in', res = 600)
ggplot(fLL3, aes(as.factor(temperature), infection, group = dpi, 
                 fill = infection_status)) +
  geom_bar(position = "dodge", stat = "identity", col = "black") +
  #facet_wrap(~ typed2, scales = "free_x", labeller=label_parsed, ncol = 3)  +
  facet_grid(~ typed2, scales = "free_x")  +
  #geom_text(aes(label = dpi_v, y = infection+0.6428571*5), size = 1.8, position =  position_dodge(width = 1))+
  #geom_text(aes(label = dpi_v, y = -1.4), size = 1.8, position =  position_dodge(width = 1))+
  geom_text(aes(label = perc, y = 0.5+infection+0.6428571*2), size = 2.5, position =  position_dodge(width = 1))+
  ylab("Specimens") +
  xlab(expression("Temperature ("*degree*C*")"))+
  scale_y_continuous(limits = c(0,37), expand = c(0, 0)) +
  # ggtitle("Transmission rate")+
  # geom_hline(yintercept = 0) +
  #  geom_vline(xintercept = 1.5) +
  scale_fill_manual(guide = guide_legend(title = "infection status"),values=c(positive="firebrick1",negative="steelblue"))+
  theme_bw()
dev.off()

png("tranmission_rate3.png",width = 3, height=3, units = 'in', res = 600)
ggplot(fLL88, aes(as.factor(temperature), perc)) +
  geom_bar(stat = "identity", fill = "black") +
  #facet_wrap(~ typed2, scales = "free_x", labeller=label_parsed, ncol = 3)  +
  facet_wrap(~ typed, scales = "free_x")  +
  #geom_text(aes(label = dpi_v, y = infection+0.6428571*5), size = 1.8, position =  position_dodge(width = 1))+
  #geom_text(aes(label = dpi_v, y = -1.4), size = 1.8, position =  position_dodge(width = 1))+
  #geom_text(aes(label = perc, y = 0.5+infection+0.6428571*2), size = 2.5, position =  position_dodge(width = 1))+
  ylab("Rate (%)") +
  xlab(expression("Temperature ("*degree*C*")"))+
  scale_y_continuous(limits = c(0,70), expand = c(0, 0)) +
  # ggtitle("Transmission rate")+
  # geom_hline(yintercept = 0) +
  #  geom_vline(xintercept = 1.5) +
  scale_fill_manual(guide = guide_legend(title = "infection status"),values=c(positive="firebrick1",negative="steelblue"))+
  theme_bw()
dev.off()

png("tranmission_rate2.png",width = 5, height=3, units = 'in', res = 600)
ggplot(fLL3, aes(as.factor(temperature), infection, group = dpi, 
                 fill = infection_status)) +
  geom_bar(position = "dodge", stat = "identity", col = "black") +
  #facet_wrap(~ typed2, scales = "free_x", labeller=label_parsed, ncol = 3)  +
  facet_grid(~ typed2, scales = "free_x")  +
  #geom_text(aes(label = dpi_v, y = infection+0.6428571*5), size = 1.8, position =  position_dodge(width = 1))+
  #geom_text(aes(label = dpi_v, y = -1.4), size = 1.8, position =  position_dodge(width = 1))+
  geom_text(aes(label = perc, y = 0.5+infection+0.6428571*2), size = 2.5, position =  position_dodge(width = 1))+
  ylab("Specimens") +
  xlab(expression("Temperature ("*degree*C*")"))+
  scale_y_continuous(limits = c(0,37), expand = c(0, 0)) +
  # ggtitle("Transmission rate")+
  # geom_hline(yintercept = 0) +
  #  geom_vline(xintercept = 1.5) +
  scale_fill_manual(guide = guide_legend(title = "infection status"),values=c(positive="firebrick1",negative="steelblue"))+
  theme_bw()
dev.off()


getwd()
png("titre.png",width = 4, height=4, units = 'in', res = 2000)
ggplot(rates_results2, aes(as.factor(temperature), as.numeric(titreMEAN))) +
  geom_point() +
  #geom_text(aes(label = dpi_v, y = infection+0.6428571*5), size = 1.8, position =  position_dodge(width = 1))+
  #geom_text(aes(label = dpi_v, y = -1.4), size = 1.8, position =  position_dodge(width = 1))+
  #geom_text(aes(label = perc, y = 0.5+infection+0.6428571*2), size = 2.5, position =  position_dodge(width = 1))+
  ylab("titre")+
  xlab(expression("temperature ("*degree*C*")"))+
  #scale_y_continuous(limits = c(0, 35), expand = c(0, 0)) +
  #ggtitle("Transmission rate")+
  #  geom_vline(xintercept = 1.5) +
  theme_bw()

dev.off()



####
####
albopictus1 <- read.xlsx("Aedes albopictus Deutschland.xlsx", 1)
albopictus2 <- read.xlsx("Aedes albopictus Deutschland.xlsx", 2)
albopictus3 <- read.xlsx("Aedes albopictus Deutschland.xlsx", 3)
albopictus4 <- read.xlsx("Aedes albopictus Deutschland.xlsx", 4)
albopictus5 <- read.xlsx("Aedes albopictus Italien 2017.xlsx", 1)
albopictus6 <- read.xlsx("Aedes albopictus Italien 2017.xlsx", 2)

albopictus1_1 <- albopictus1[ !is.na(albopictus1[,2]), ]
albopictus2_1 <- albopictus2[ !is.na(albopictus2[,2]), ]
albopictus3_1 <- albopictus3[ !is.na(albopictus3[,2]), ]
albopictus4_1 <- albopictus4[ !is.na(albopictus4[,2]), ]
albopictus5_1 <- albopictus5[ !is.na(albopictus5[,2]), ]
albopictus6_1 <- albopictus6[ !is.na(albopictus6[,2]), ]

albopictus_all <- data.frame(rbind(albopictus1_1[,-22],
                                  albopictus2_1[,-22],
                                  albopictus3_1,
                                  albopictus4_1,
                                  albopictus5_1[,-22],
                                  albopictus6_1[,-22]))
albopictus_all$temperature <- as.numeric(substr(albopictus_all$temperature, 1, 2))

albopictus_all <- albopictus_all[,2:20]

dimnames(albopictus_all)[[2]] <- c("experiment_no",  "start_date",      "species" ,        "origin",         
                                  "temperature",     "virus",           "input_total",     "blood_fed_total",
                                  "specimens", "body_part", "dpi",             "tube_id" ,       
                                  "ct_value" ,       "infection"  ,     "titre" ,          "titre_method" ,  
                                  "freezer" ,        "rack",            "box" )  

unique(albopictus_all$origin)
fdf2 <- subset(albopictus_all, virus %in% c("CHIKV"))
fdf2$infection <- as.numeric(as.vector(fdf2$infection))
fdf2$titre <- as.numeric(as.vector(fdf2$titre))
fdf2$ct_value <- as.numeric(as.vector(fdf2$ct_value))

source("R/rates.R")
fdf2$infection2 <- ifelse(fdf2$titre_method == "CPE" | fdf2$titre_method == "CPE/PCR", fdf2$infection, ifelse(fdf2$titre_method == "PCR",ifelse(fdf2$ct_value <= 35, 1, 0), NA))
fdf2$infection  <- fdf2$infection2

rates_results <- rates(fdf2)
rates_results2 <- subset(rates_results, dpi >1 & dpi < 21)

oiki <- length(rates_results2$species)
fLL2_trans <- data.frame(species = rep(paste(rates_results2$species, rates_results2$origin), 2), 
                         typed = "transmission",
                         dpi = rep(paste(as.factor(rates_results2$dpi), "dpi", sep = " "), 2),
                         dpi_v = c(paste(as.factor(rates_results2$dpi), "dpi", sep = " "), rep("", oiki)),
                         perc2 = c(as.numeric(rates_results2$transmission_rate_A)/as.numeric(rates_results2$transmission_rate_B), rep(0, oiki)),
                         temperature = rep(as.factor(rates_results2$temperature), 2),
                         infection_status = c(rep("negative", oiki),rep("positive", oiki)),
                         infection = c(as.numeric(rates_results2$transmission_rate_B),
                                       as.numeric(rates_results2$transmission_rate_A)))

fLL2_infecton <- data.frame(species = rep(paste(rates_results2$species, rates_results2$origin), 2), 
                            typed = "infection",
                            dpi = rep(paste(as.factor(rates_results2$dpi), "dpi", sep = " "), 2),
                            dpi_v = c(paste(as.factor(rates_results2$dpi), "dpi", sep = " "), rep("", oiki)),
                            perc2 = c(as.numeric(rates_results2$infection_rate_A)/as.numeric(rates_results2$infection_rate_B), rep(0, oiki)),
                            temperature = rep(as.factor(rates_results2$temperature), 2),
                            infection_status = c(rep("negative", oiki),rep("positive", oiki)),
                            infection = c(as.numeric(rates_results2$infection_rate_B),
                                          as.numeric(rates_results2$infection_rate_A)))

fLL3 <- rbind(fLL2_trans, fLL2_infecton)

fLL3$perc2[is.nan(fLL3$perc2)] <- "na"
fLL3$perc <- c(paste(format(round(as.numeric(fLL3$perc2[1:oiki])*100, 1), nsmall = 1), " %", sep = ""), rep("", oiki))

library(ggplot2)
colnames(fLL3)
fLL3$infection_status
fLL3$typed = factor(fLL3$typed, levels=c("infection","transmission"))


fLL3$species <- revalue(fLL3$species,
                        c("Aedes albopictus Calabria, Italy"= "Italy",
                          "Aedes albopictus Freiburg, Germany" = "Germany"))
fLL3$species = factor(fLL3$species, levels=c("Germany","Italy"))

#levels(fLL3$species)= c("Ae. albopictus, ITA"=expression(paste(italic("Ae. albopictus"), ", ITA")),
#                        "Ae. albopictus, GER"=expression(paste(italic("Ae. albopictus"), ", GER")))

png("tranmission_rate_chikv.png",width = 4.3, height=4, units = 'in', res = 600)
ggplot(fLL3, aes(as.factor(temperature), infection, group = dpi, 
                 fill = infection_status)) +
  geom_bar(position = "dodge", stat = "identity", col = "black") +
  facet_wrap(~ species+typed, scales = "free_x", labeller=label_parsed, ncol = 2)  +
  #geom_text(aes(label = dpi_v, y = infection+0.6428571*5), size = 1.8, position =  position_dodge(width = 1))+
  #geom_text(aes(label = dpi_v, y = -1.4), size = 1.8, position =  position_dodge(width = 1))+
  #geom_text(aes(label = perc, y = 0.5+infection+0.6428571*2), size = 2.5, position =  position_dodge(width = 1))+
  ylab("specimens") +
  xlab(expression("temperature ("*degree*C*")"))+
  scale_y_continuous(limits = c(0,25), expand = c(0, 0)) +
  # ggtitle("Transmission rate")+
  # geom_hline(yintercept = 0) +
  #  geom_vline(xintercept = 1.5) +
  scale_fill_manual(guide = guide_legend(title = "infection status"),values=c(positive="firebrick1",negative="steelblue"))+
  theme_bw()
dev.off()