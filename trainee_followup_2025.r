# Trainee matching code: all mechanisms for 2001-2020

## load packages, helper function, and custom color palette
library(tidyverse)
source("qvr_processing.r")
mcols <- scan("nichd_palette.txt", what = "varchar", sep = "\n")


## read in and clean the QVR data
### this assumes you have a folder in your current working directory called 'trainees' that contains trainee data in .csv format for each fiscal year 
fnames <- list.files("trainees", pattern = "^trainees_20[01]", full.names = TRUE)
trainees <- lapply(fnames, read.csv, stringsAsFactors = FALSE)
trainees <- do.call(rbind, trainees)

trainees <- trainees %>% 
  mutate(
    trn_ic = strtrim(TRN.PROJECT.NUM, 2),
    trn_type = gsub("\\d{2}|[A-Z]\\d$|[A-Z]{2}$", "XX", TRN.ACTIVITY.CODE),
    org_name_clean = clean_org_names(ORG.NAME)
  )
trainees$trn_type[trainees$TRN.ACTIVITY.CODE == "K12"] <- "K12"

trainees <- trainees %>% 
  arrange(TRN.PROFILE.PERSON.ID, TRN.FY) %>% 
  filter(TRN.ACTIVITY.CODE != "K24")
trainees <- process_qvr_data(trainees, "TRN.PROFILE.PERSON.ID")
trainees <- trainees %>% 
  mutate(
    trn_subs_rsrch_grant_code = case_when(
      grepl("G", trn_subs_rsrch_grant_code) ~ "G",
      grepl("A", trn_subs_rsrch_grant_code) ~ "A",
      grepl("N", trn_subs_rsrch_grant_code) ~ "N"
    ),
    trn_subs_grant_code = case_when(
      grepl("G", trn_subs_grant_code) ~ "G",
      grepl("A", trn_subs_grant_code) ~ "A",
      grepl("N", trn_subs_grant_code) ~ "N"
    ),
    trn_start_fy = gsub(";.+", "", trn_fy),
    trn_end_fy = gsub(".+;", "", trn_fy)
    )
## final N = 10,678 for FY2001-2020


## create the search strings to do the author matching
affil_strings <- paste0("(AFFIL(", gsub(";", ") OR AFFIL(", trainees$org_name_clean), "))")
name_strings <- gsub("(.+?, .+? [A-Z])(.+)", "\\1", trainees$trainee_name)
name_strings <- gsub("\\(.+\\)", "", name_strings)
mstrings <- paste0("AUTHFIRST(", gsub(".+, ", "", name_strings), ") AND AUTHLASTNAME(", gsub(", .+", "", name_strings), ") AND ", affil_strings)
mstrings <- gsub("\\.", "", mstrings)
mstrings <- gsub(";", "", mstrings)
mstrings <- gsub(" \\)", "\\)", mstrings)
mstrings[1130:1140] ### check that the strings are formatted correctly


## run the author matching
source("scopus_author_dev.r")

## part 1 of the author matching procedure: search for all trainees
scopus1 <- list()
### this for loop averages around 45-47 searches per minute, or around 22 minutes per 1000 searches
### 10678 searches took just under four hours to complete
### this also assumes you have a folder in your current working directory called 'scopus data' that you want to save the search results to
for (i in 1:length(mstrings)) {
  if (i %% 200 == 0) message(paste("Retrieving string", i, "of", length(mstrings)))
  scopus1[[i]] <- authorSearch(mstrings[i], retMax = 3, outfile = paste0("scopus data/trainees1_", sprintf("%05d", i), ".txt"))
} 
### if you have to read files back in, use the following, otherwise, move on to line 71
###fnames <- list.files("scopus data", full.names = TRUE)
###scopus1 <- lapply(fnames, extractSearchXML)
scopus1 <- mapply(cbind, "trn_profile_person_id" = trainees$trn_profile_person_id, scopus1, SIMPLIFY = FALSE)
scopus1 <- do.call(rbind, scopus1)
write.csv(scopus1, file = "scopus_data_part1.csv", row.names = FALSE)

## part 2 of the author matching procedure: search again for trainees not matched in part 1
nomatch <- scopus1$trn_profile_person_id[which(is.na(scopus1$scopusAuthEID))] 
mstrings <- paste0("AUTHFIRST(", gsub(".+, ", "", name_strings[trainees$trn_profile_person_id %in% nomatch]), ") AND AUTHLASTNAME(", gsub(", .+", "", name_strings[trainees$trn_profile_person_id %in% nomatch]), ") AND SUBJAREA(BIOC OR MEDI OR SOCI OR NEUR OR IMMU OR PSYC)")
scopus2 <- list()
### again, this runs about 45-47 searches per minute
### this also assumes you have a folder in your current working directory called 'scopus data' that you want to save the search results to
for (i in 1:length(mstrings)) {
  if (i %% 200 == 0) message(paste("Retrieving string", i, "of", length(mstrings)))
  scopus2[[i]] <- authorSearch(mstrings[i], retMax = 3, outfile = paste0("scopus data/trainees2_", sprintf("%05d", i), ".txt"))
} 
scopus2 <- mapply(cbind, "trn_profile_person_id" = trainees$trn_profile_person_id[trainees$trn_profile_person_id %in% nomatch], scopus2, SIMPLIFY = FALSE)
scopus2 <- do.call(rbind, scopus2)
write.csv(scopus2, file = "scopus_data_part2.csv", row.names = FALSE)

## part 3 of the matching procedure: get all author profiles or all possible matches and merge the results into the information you already have
scopus_search <- rbind(scopus1, scopus2)
scopus_retrieve <- authorRetrieve(scopus_search$authID[!is.na(scopus_search$authID)], outfile = "scopus data/trainee_retrieve_data.txt")
scopus_results <- merge(scopus_search[,c("authID", colnames(scopus_search)[colnames(scopus_search) %in% colnames(scopus_retrieve) == FALSE])], scopus_retrieve, by = "authID", all.x = TRUE)
scopus_results <- unique(scopus_results)
scopus_results <- scopus_results[order(scopus_results$trn_profile_person_id),]
write.csv(scopus_results, file = "scopus_search_results_all.csv", row.names = FALSE)

## part 4, standardize the returned data and choose the most likely match for each trainee
scopus_results$authName2 <- paste0(scopus_results$authLast, ", ", scopus_results$authGiven)
scopus_results$authName2 <- stringi::stri_trans_general(scopus_results$authName2, "Latin-ASCII")
scopus_results <- scopus_results %>% 
  left_join(trainees[,c("trn_profile_person_id", "trainee_name")], by = "trn_profile_person_id")
scopus_results$trainee_name <- stringi::stri_trans_general(scopus_results$trainee_name, "Latin-ASCII")
scopus_results$trainee_name_match <- gsub("\\.|,", "", scopus_results$trainee_name)
scopus_results$trainee_name_match <- gsub("[ -]", ".*", scopus_results$trainee_name_match)
scopus_results$scopus_name_match <- gsub("\\.|,", "", scopus_results$authName2)
scopus_results$scopus_name_match <- gsub("[ -]", ".*", scopus_results$scopus_name_match)
scopus_results$name_match <- sapply(1:nrow(scopus_results), function(x) any(grepl(scopus_results$trainee_name_match[x], scopus_results$scopus_name_match[x], ignore.case = TRUE), grepl(scopus_results$scopus_name_match[x], scopus_results$trainee_name_match[x], ignore.case = TRUE)))
scopus_results$subject_match <- sapply(1:nrow(scopus_results), function(x) any(grepl("BIOC|MEDI|SOCI|NEUR|IMMU|PSYC|PHAR|NURS", scopus_results[x,c("subjarea1", "subjarea2", "subjarea3")])))

### the custom decision algorithm to choose the most likely match for each trainee
tmp1 <- split(scopus_results, scopus_results$trn_profile_person_id)
for (i in 1:length(tmp1)) {
  if (nrow(tmp1[[i]]) == 1) {
    tmp1[[i]] <- tmp1[[i]]
  }
  else if (nrow(tmp1[[i]]) > 1 && all(tmp1[[i]]$name_match == FALSE)) {
    tmp1[[i]] <- tmp1[[i]][1,]
    tmp1[[i]][,c(4:ncol(tmp1[[i]]))] <- NA
  }
  else {
    tmp1[[i]] <- tmp1[[i]][tmp1[[i]]$name_match == TRUE,] ## the trainee name matches
    if (nrow(tmp1[[i]]) > 1) tmp1[[i]] <- tmp1[[i]][tmp1[[i]]$subject_match == TRUE,] ## the profile has publications in relevant fields
    if (nrow(tmp1[[i]]) > 1) tmp1[[i]] <- tmp1[[i]][tmp1[[i]]$pubStartYear > 1989,] ## publication start year > 1989
    if (nrow(tmp1[[i]]) > 1) tmp1[[i]] <- tmp1[[i]][which.max(tmp1[[i]]$pubCount),] ## profile with the largest number of pubs
  }
}
tmp1 <- do.call(rbind, tmp1)

## part 5, merge the final set of author profiles back into the original trainee data and save the resulting trainee data
trainees <- trainees %>% 
  left_join(tmp1, by = "trn_profile_person_id")
trainees <- trainees %>% 
  mutate(active_pubs = ifelse(pubEndYear >= 2024, "Yes", "No"))
trainees$active_pubs[is.na(trainees$active_pubs)] <- "Unmatched"
trainees %>% count(active_pubs) 
#  active_pubs    n
#1          No 3604 / 10678 = 34%
#2   Unmatched 1165 / 10678 = 11%
#3         Yes 5909 / 10678 = 55%
write.csv(trainees, file = "scopus_data_final.csv", row.names = FALSE)
## end trainee matching process


## create additional trainee columns to allow for accurate counting of trainees by program and training year 
fnames <- list.files("projects/trainees", pattern = "^trainees_20[01]", full.names = TRUE)
trainees2 <- lapply(fnames, read.csv, stringsAsFactors = FALSE)
trainees2 <- do.call(rbind, trainees2)

trainees2 <- trainees2 %>% 
  mutate(
    trn_ic = strtrim(TRN.PROJECT.NUM, 2),
    trn_type = gsub("\\d{2}|[A-Z]\\d$|[A-Z]{2}$", "XX", TRN.ACTIVITY.CODE)
  )
trainees2$trn_type[trainees2$TRN.ACTIVITY.CODE == "K12"] <- "K12"

trainees2 <- trainees2 %>% 
  arrange(TRN.PROFILE.PERSON.ID, TRN.FY) %>% 
  filter(TRN.ACTIVITY.CODE != "K24") %>% 
  select(TRN.PROFILE.PERSON.ID, TRN.ACTIVITY.CODE, TRN.FY, trn_type)
trainees2 <- trainees2 %>% 
  group_by(TRN.PROFILE.PERSON.ID, TRN.ACTIVITY.CODE, TRN.FY, trn_type) %>% 
  count()
trainees2 <- trainees2 %>% 
  arrange(TRN.PROFILE.PERSON.ID, TRN.FY, TRN.ACTIVITY.CODE)
trainees2 <- trainees2 %>% 
  group_by(TRN.PROFILE.PERSON.ID) %>% 
  summarise(
    all_trn_fys = paste(TRN.FY, collapse = ";"),
    all_trn_codes = paste(TRN.ACTIVITY.CODE, collapse = ";"),
    all_trn_types = paste(trn_type, collapse = ";"),
    .groups = "drop"
  )

trainees <- trainees %>% 
  left_join(trainees2, by = c("trn_profile_person_id" = "TRN.PROFILE.PERSON.ID"))
write.csv(trainees, file = "scopus_data_final.csv", row.names = FALSE)
## end additional columns


## analyze the results
#library(tidyverse)
#mcols <- scan("branding/nichd_palette.txt", what = "varchar", sep = "\n")
#trainees <- read.csv("scopus_data_final.csv", stringsAsFactors = FALSE)

## print out tables for various criteria
trainees %>% separate_rows(trn_type, sep = ";") %>% count(trn_type)
#  trn_type     n
#1 FXX       1110
#2 K12       1017
#3 KXX       1006
#4 LXX       1200
#5 RXX        132
#6 TXX       7063
sum(grepl(";", trainees$trn_type)) ## 761 / 10678 = 7% of all trainees got multiple training awards from NICHD
sum(grepl(";", trainees$trn_type[grepl("FXX", trainees$trn_type)])) ## 205 / 1110 (18%)
sum(grepl(";", trainees$trn_type[grepl("K12", trainees$trn_type)])) ## 220 / 1017 (22%)
sum(grepl(";", trainees$trn_type[grepl("KXX", trainees$trn_type)])) ## 284 / 1006 (28%)
sum(grepl(";", trainees$trn_type[grepl("LXX", trainees$trn_type)])) ## 417 / 1200 (35%)
sum(grepl(";", trainees$trn_type[grepl("TXX", trainees$trn_type)])) ## 485 / 7063 (7%)

trainees %>% count(trn_subs_rsrch_grant_code)
trainees %>% count(trn_type)
trainees %>% count(trn_subs_rsrch_grant_code, active_pubs)
#  trn_subs_rsrch_grant_code active_pubs    n
#1                         A          No  271
#2                         A   Unmatched   78
#3                         A         Yes 1100
#4                         G          No  167
#5                         G   Unmatched   66
#6                         G         Yes 1995
#7                         N          No 3166
#8                         N   Unmatched 1021
#9                         N         Yes 2814
## 2814 / 5909 active pubs = 48%
trainees %>% count(trn_end_fy) ## 1132 in 2020, 522 in 2019, 453 in 2018; so 2107 ended post-2018; so 8571 ended pre-2018
trainees %>% filter(trn_end_fy < 2018) %>% count(trn_subs_rsrch_grant_code, active_pubs)
## 1949 N grants but active pubs + 784 A grants but active pubs = 2733. 2733 / 8571 = 32% pre-2018

summary(str_count(trainees$all_trn_fys, ";") + 1)
trainees %>% separate_rows(all_trn_fys, sep = ";") %>% count(all_trn_fys)


## summary data for figures 1-3
fy_type <- trainees %>% 
  separate_rows(all_trn_fys, all_trn_types, sep = ";") %>% 
  group_by(all_trn_fys, all_trn_types) %>% 
  summarise(
    num_trn = length(trn_profile_person_id),
    num_grant = length(trn_profile_person_id[trn_subs_rsrch_grant_code == "G"]),
    perc_grant = num_grant / num_trn,
    num_pub = length(trn_profile_person_id[active_pubs == "Yes"]),
    perc_pub = num_pub / num_trn,
    .groups = "drop"
  ) %>% 
  mutate(all_trn_fys = as.numeric(all_trn_fys)) %>% 
  filter(all_trn_types != "RXX") %>% 
  filter((all_trn_types == "K12" & all_trn_fys < 2009) == FALSE) %>% 
  filter((all_trn_types == "LXX" & all_trn_fys < 2003) == FALSE)
### this assumes you have a folder in your current working directory called 'csv data' that you want to save the summary tables to
write.csv(fy_type, file = "csv data/trnAllYr_summary.csv", row.names = FALSE)

## figure 1
p1 <- ggplot(fy_type, aes(all_trn_fys, num_trn, color = all_trn_types))
p1 + geom_line(size = 5) + scale_color_manual(values = mcols) + 
  xlim(2000,2020) + scale_y_continuous(limits = c(0,800)) + 
  labs(x = "Training Fiscal Year", y = "Number of Trainees", color = "Training\nType") + 
  theme_gray(base_size = 18)

## figure 2
p2 <- ggplot(fy_type, aes(all_trn_fys, perc_grant, color = all_trn_types))
p2 + geom_line(size = 5) + scale_color_manual(values = mcols) + 
  xlim(2000,2020) + scale_y_continuous(limits = c(0,0.8), breaks = seq(0,0.8,0.2), labels = scales::label_percent(accuracy = 1)) + 
  labs(x = "Training Fiscal Year", y = "Trainees Receiving an NIH RPG", color = "Training\nType") + 
  theme_gray(base_size = 18)

## figure 3
p3 <- ggplot(fy_type, aes(all_trn_fys, perc_pub, color = all_trn_types))
p3 + geom_line(size = 5) + scale_color_manual(values = mcols) + 
  xlim(2000,2020) + scale_y_continuous(limits = c(0.2,1), breaks = seq(0,1,0.2), labels = scales::label_percent(accuracy = 1)) + 
  labs(x = "Training Fiscal Year", y = "Trainees Actively Publishing", color = "Training\nType") + 
  theme_gray(base_size = 18)


## summary data for all types per year
trainees %>% count(active_pubs)
fy_all <- trainees %>% 
  separate_rows(all_trn_fys, sep = ";") %>% 
  group_by(all_trn_fys) %>% 
  summarise(
    num_trn = length(trn_profile_person_id),
    num_grant = length(trn_profile_person_id[trn_subs_rsrch_grant_code == "G"]),
    perc_grant = num_grant / num_trn,
    num_pub = length(trn_profile_person_id[active_pubs == "Yes"]),
    perc_pub = num_pub / num_trn,
    .groups = "drop"
  ) %>% 
  mutate(all_trn_fys = as.numeric(all_trn_fys))
### this assumes you have a folder in your current working directory called 'csv data' that you want to save the summary tables to
write.csv(fy_all, file = "csv data/trnAllYear_summary.csv", row.names = FALSE)


## data and visualization for figure 4
trainees <- trainees %>% 
  mutate(affil_type = case_when(
    grepl("Hosp|(Health|Medical|Cancer) (Care|Center|System)|Clinic\\b|Healthcare|Kaiser Perm|\\bU[A-Z]{1,3} Medical", affilName) ~ "Hospital",
    grepl("Universit|School|College|Departmen|Massachusetts Institute", affilName) ~ "Academic",
    TRUE ~ "Other"
  ))
trainees$affil_type[is.na(trainees$scopusAuthEID)] <- "Unmatched"
trainees %>% count(affil_type)
affil_sum <- trainees %>% 
  separate_rows(trn_type, sep = ";") %>% 
  group_by(trn_type, affil_type) %>% 
  count() %>% 
  ungroup()
affil_sum_all <- trainees %>% 
  separate_rows(trn_type, sep = ";") %>% 
  group_by(affil_type) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(trn_type = "All")
affil_sum <- affil_sum %>% bind_rows(affil_sum_all)
affil_sum$affil_type <- factor(affil_sum$affil_type, levels = sort(unique(affil_sum$affil_type), decreasing = TRUE))
affil_sum$trn_type <- factor(affil_sum$trn_type, levels = sort(unique(affil_sum$trn_type), decreasing = TRUE))
p3 <- ggplot(affil_sum[affil_sum$trn_type != "RXX",], aes(n, trn_type, fill = affil_type))
p3 + geom_col(position = "fill") + scale_fill_manual(values = mcols[4:1], guide = guide_legend(reverse = TRUE)) + 
  scale_x_continuous(breaks = seq(0,1,0.1), labels = scales::label_percent(accuracy = 1)) + 
  labs(x = "Percent of Trainees", y = "Training Type", fill = "Most Recent\nAffiliation Type") + 
  theme(text = element_text(size = 18))
affil_sum <- affil_sum %>% 
  group_by(trn_type) %>% 
  mutate(
    perc = n / sum(n)
  ) %>% 
  ungroup()
write.csv(affil_sum, file = "csv data/trainee_affilData.csv", row.names = FALSE)
trainees %>% 
  filter(active_pubs == "Yes") %>% 
  separate_rows(trn_type, sep = ";") %>% 
  count(affil_type) %>% 
  mutate(
    perc = n / sum(n)
  )
affil_countries <- trainees %>% 
  mutate(affilCountry = gsub(".+;", "", affilCountry)) %>% 
  count(affilCountry) %>% 
  mutate(perc = n / sum(n))


## data and visualization for figure 5
trn_flow <- trainees %>% 
  separate_rows(trn_type, sep = ";") %>% 
  count(trn_type, trn_subs_rsrch_grant_code, active_pubs)
trn_flow$trn_subs_rsrch_grant_code[trn_flow$trn_subs_rsrch_grant_code == "A"] <- "Applied"
trn_flow$trn_subs_rsrch_grant_code[trn_flow$trn_subs_rsrch_grant_code == "G"] <- "Awarded"
trn_flow$trn_subs_rsrch_grant_code[trn_flow$trn_subs_rsrch_grant_code == "N"] <- "None"
trn_flow$trn_subs_rsrch_grant_code <- factor(trn_flow$trn_subs_rsrch_grant_code, levels = c("Awarded", "Applied", "None"))
trn_flow$active_pubs <- factor(trn_flow$active_pubs, levels = c("Yes", "No", "Unmatched"))
library(ggalluvial)
p4 <- ggplot(trn_flow[trn_flow$trn_type != "RXX",], aes(y = n, axis1 = trn_type, axis2 = trn_subs_rsrch_grant_code, axis3 = active_pubs)) + geom_alluvium(aes(fill = trn_type)) + geom_stratum(width = 1/3)
p4 + geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5) + scale_fill_manual(values = mcols) + 
  scale_x_discrete(limits = c("Training Type", "NIH Grant Activity", "Actively Publishing"), expand = c(.05, .05)) + 
  scale_y_continuous(breaks = seq(0,12000,2000)) + 
  labs(y = "Number of Trainees", fill = "Training\nType") + theme_classic(base_size = 18)
write.csv(trn_flow, file = "csv data/trainee_alluvial_data.csv", row.names = FALSE)
trainees %>% 
  filter(trn_end_fy < 2018) %>% 
  separate_rows(trn_type, sep = ";") %>% 
  count(trn_subs_rsrch_grant_code, active_pubs) %>% 
  mutate(perc = n / sum(n))


## data and visualization for figure 6
measure_comp <- fy_type %>% 
  select(all_trn_fys, all_trn_types, perc_grant, perc_pub) %>% 
  pivot_longer(c(perc_grant, perc_pub), names_to = "measure", values_to = "amount")
p5 <- ggplot(measure_comp, aes(all_trn_fys, amount, color = measure)) + facet_wrap(vars(all_trn_types))
p5 + geom_line(size = 4) + scale_color_manual(values = mcols, labels = c("Awarded an NIH RPG", "Actively Publishing")) +  
  scale_y_continuous(breaks = seq(0,1,0.2), labels = scales::label_percent()) + 
  labs(x = "Training Fiscal Year", y = "Percent of Trainees", color = "Success Measure") + 
  theme_gray(base_size = 18) + theme(legend.justification = c(1,0), legend.position = c(0.98, 0.02))

