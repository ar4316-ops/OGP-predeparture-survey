#Load packages
pacman::p_load(readxl, ggplot2, plyr, dplyr, tidyr, here)

#Load Slate list 
pds <- read_excel(here("data/Spring PDS List.xlsx"), sheet=1, guess_max = 1000000)

# Convert program names into student-friendly labels, setting those to NA that don't need 'special program' question. 
pds$SpecialProgram[pds$`Accepted Academic Track` == "NYU New York: NYU Shanghai Sophomore Study Away in NYC Fall 2022"] <- NA
pds$SpecialProgram[pds$`Accepted Academic Track` %in% c("Tisch FAMU - Director", "Tisch FAMU - Producer")] <- "Tisch FAMU"
pds$SpecialProgram[pds$`Accepted Academic Track` %in% c("Tisch Acting")] <- "Tisch Stanislavski, Brecht, and Beyond"
pds$SpecialProgram[pds$`Accepted Academic Track` %in% c("Tisch ReMu")] <- "Clive Davis Institute X Berlin"
pds$SpecialProgram[pds$`Accepted Academic Track` %in% c("GLS Jr")] <- "GLS"
pds$SpecialProgram[pds$`Accepted Academic Track` %in% c("Tisch Playwriting")] <- "Tisch Playwriting in London"
pds$SpecialProgram[pds$`Accepted Academic Track` %in% c("Tisch RADA")] <- "Shakespeare in Performance at RADA"
pds$SpecialProgram[pds$`Accepted Academic Track` %in% c("Tisch Screenwriting")] <- "Tisch Screenwriting in London"
pds$SpecialProgram[pds$`Accepted Academic Track` %in% c("Gallatin Fashion")] <- NA
pds$SpecialProgram[pds$`Accepted Academic Track` %in% c("Legal Studies")] <- NA
pds$SpecialProgram[pds$`Accepted Academic Track` %in% c("Teaching & Learning")] <- "Teaching & Learning"
pds$SpecialProgram[pds$`Accepted Academic Track` %in% c("NYU Paris: Music")] <- NA
pds$SpecialProgram[pds$`Accepted Academic Track` %in% c("Global Leadership Program")] <- NA
pds$SpecialProgram[pds$`Accepted Academic Track` %in% c("Global Media Scholars")] <- NA
pds$SpecialProgram[pds$`Accepted Academic Track` == c('Gallatin FashionTeaching & Learning')] <- NA
pds$SpecialProgram[pds$`Accepted Academic Track` == 'Global Leadership Program'] <- NA
pds$SpecialProgram[pds$`Accepted Academic Track` == 'Legal Studies'] <- NA
pds$SpecialProgram[pds$`Accepted Academic Track` == 'Teaching & Learning'] <- NA
pds$SpecialProgram[pds$`Accepted Academic Track` == 'Global Media Scholars'] <- NA
pds$SpecialProgram[pds$`Accepted Academic Track` == 'Gallatin Fashion'] <- NA
pds$SpecialProgram[pds$`Accepted Academic Track` == '180 StudiosTisch Directing the Actor'] <- NA


#set city/country labels as well as standardize DC
pds$Site[pds$Site == "NYU Washington DC"] <- "NYU Washington, DC"
pds$City <- substring(pds$Site, 5)
pds$Country[pds$City == "London"] <- "the United Kingdom"
pds$Country[pds$City == "Paris"] <- "France"
pds$Country[pds$City == "Madrid"] <- "Spain"
pds$Country[pds$City == "Berlin"] <- "Germany"
pds$Country[pds$City == "New York"] <- "the New York area"
pds$Country[pds$City == "Prague"] <- "the Czech Republic"
pds$Country[pds$City == "Florence"] <- "Italy"
pds$Country[pds$City == "Abu Dhabi"] <- "the United Arab Emirates"
pds$Country[pds$City == "Shanghai"] <- "China"
pds$Country[pds$City == "Washington, DC"] <- "the Washington, DC area"
pds$Country[pds$City == "Tel Aviv"] <- "Israel"
pds$Country[pds$City == "Los Angeles"] <- "the Los Angeles area"
pds$Country[pds$City == "Buenos Aires"] <- "Argentina"
pds$Country[pds$City == "Accra"] <- "Ghana"
pds$Country[pds$City == "Sydney"] <- "Australia"
pds$Country[pds$City == 'Tulsa'] <- 'Tulsa'

# Load UDW+ version (XML in repo)
pds_udw <- read.csv(here("data/PDS Distribution UDW.csv"), stringsAsFactors = F, check.names = F)
names(pds_udw)[1] <- "N.Number"

# Merge files
pds <- merge(pds, pds_udw, by = "N.Number", all.x = TRUE)

# Set Tulsa as an Anglophone site (can also be set in UDW+ and then this row removed)
pds$`Language or Anglophone`[pds$Site == 'NYU Tulsa'] <- 'Anglophone'

# Remove rows where no study agreement
pds[is.na(pds)] <- ""
pds <- subset(pds, pds$`Study Away Agreement Code` != '-')
pds <- subset(pds, pds$`Study Away Agreement Code` != '')
pds <- subset(pds, !(is.na(pds$`Study Away Agreement Code`)))
table(pds$Site, pds$`Study Away Agreement Code`)

# Next two rows can be used to subset for Sydney (sending early)
# pds_nosyd <- subset(pds, pds$Site != "NYU Sydney")
# pds_syd <- subset(pds, pds$Site == "NYU Sydney")

# Logos for email header
pds$logo[pds$Site == 'NYU Abu Dhabi'] <- 'https://nyu.qualtrics.com/ControlPanel/Graphic.php?IM=IM_38BSCmVmByPlztY'
pds$logo[pds$Site == 'NYU Accra'] <- 'https://nyu.qualtrics.com/ControlPanel/Graphic.php?IM=IM_1BvMCLGyOTVbGV8'
pds$logo[pds$Site == 'NYU Berlin'] <- 'https://nyu.qualtrics.com/ControlPanel/Graphic.php?IM=IM_6Gziu1a28DgIARM'
pds$logo[pds$Site == 'NYU Buenos Aires'] <- 'https://nyu.qualtrics.com/ControlPanel/Graphic.php?IM=IM_6nV1VkquCiz9Sui'
pds$logo[pds$Site == 'NYU Florence'] <- 'https://nyu.qualtrics.com/ControlPanel/Graphic.php?IM=IM_6eUcwwsNktatFWe'
pds$logo[pds$Site == 'NYU London'] <- 'https://nyu.qualtrics.com/ControlPanel/Graphic.php?IM=IM_4J849GaOEWpgN38'
pds$logo[pds$Site == 'NYU Los Angeles'] <- 'https://nyu.qualtrics.com/ControlPanel/Graphic.php?IM=IM_3kKpvHO2f2JdbhA'
pds$logo[pds$Site == 'NYU Madrid'] <- 'https://nyu.qualtrics.com/ControlPanel/Graphic.php?IM=IM_cTuqz15E8uY1Saq'
pds$logo[pds$Site == 'NYU New York'] <- 'https://nyu.qualtrics.com/ControlPanel/Graphic.php?IM=IM_9slDmJuegcvEgke'
pds$logo[pds$Site == 'NYU Paris'] <- 'https://nyu.qualtrics.com/ControlPanel/Graphic.php?IM=IM_6sB8rlpwP4KjApM'
pds$logo[pds$Site == 'NYU Prague'] <- 'https://nyu.qualtrics.com/ControlPanel/Graphic.php?IM=IM_en9IsrVIV6dkVrU'
pds$logo[pds$Site == 'NYU Shanghai'] <- 'https://nyu.qualtrics.com/ControlPanel/Graphic.php?IM=IM_3I6ejquikcX40SO'
pds$logo[pds$Site == 'NYU Sydney'] <- 'https://nyu.qualtrics.com/ControlPanel/Graphic.php?IM=IM_8kOxl4CFmuxmXbw'
pds$logo[pds$Site == 'NYU Tel Aviv'] <- 'https://nyu.qualtrics.com/ControlPanel/Graphic.php?IM=IM_zR18aT6ELQeFrJZ'
pds$logo[pds$Site == 'NYU Tulsa'] <- 'https://nyu.qualtrics.com/ControlPanel/Graphic.php?IM=IM_sr1NhvvHPeiLwEo'
pds$logo[pds$Site == 'NYU Washington, DC'] <- 'https://nyu.qualtrics.com/ControlPanel/Graphic.php?IM=IM_8kT0qo79jB824ho'


# Extra row to drop same location students for Spring. Shouldn't need if done in Slate query itself. 
#pds <- subset(pds, pds$FullSame != 'Academic Year - Same Location')

# Reframes language for first year students away from 'study away'. This will need to be updated with additional LGG tracks for Fall '26. 
pds <- pds %>% dplyr::mutate(ExperienceType = case_when(`Accepted Academic Track` %in% c('FYLO', 'LS FYA', 'Music Business') ~ 'first year at NYU at a global location',
                                                        TRUE ~ 'study away experience'))


write.csv(pds, here("data/Pre-Departure Survey List for Qualtrics.csv"), row.names = F)

# These can be used for Sydney/no Sydney 
# write.csv(pds_syd, here("data/Pre-Departure Survey List for Qualtrics (Sydney ONLY).csv"), row.names = F)
# write.csv(pds_nosyd, here("data/Pre-Departure Survey List for Qualtrics (NO Sydney).csv"), row.names = F)

