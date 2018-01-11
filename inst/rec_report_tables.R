library(dplyr)
library(stringr)
library(tabulizer)

infile <- "~/Downloads/rec_report_13-14.pdf"
tabs1 <- extract_tables(infile)
tabs2 <- lapply(tabs1, as_data_frame)
df1 <- bind_rows(tabs2)
df2 <- filter(df1, V3 != "")
table(df2$V5)
odd_date <- filter(df2, V5 != "")
odd_dat2 <- select(odd_date, -V1, -V4, -V10, -V15, -V16)
odd_dat3 <- cbind(odd_dat2[,1], NA, odd_dat2[,2:length(odd_dat2)])
oddities <- lapply(names(norm_dat),
                   function(x) length(norm_dat[[x]][norm_dat[[x]] == ""]))
oddities
norm_dat <- filter(df2, V5 == "")
df3 <- select(norm_dat, -V1, -V5, -V11, -V16)
names <- c(
  "Species", "Lead_Region", "Date_Listed", "Date_Active_Plan", "Plan_Status",
  "N_Recovery_Act_Impl", "Est_Recovery_Cost", "Est_Recovery_Time", "Listing_Class",
  "RPN", "Date_5yr", "Five_Yr_Recommend"
)
names(df3) <- names
names(odd_dat3) <- names
rec_rep_dat <- rbind(df3, odd_dat3)
rec_rep_dat <- filter(rec_rep_dat, Species != "Species Name")

rec_rep_dat$Plan_Status <- gsub(rec_rep_dat$Plan_Status,
                                pattern = "\\(.*\\)",
                                replacement = "")
rec_rep_dat$Conflict <- grepl(rec_rep_dat$RPN, pattern = "C")
rec_rep_dat$Fin_Plan <- ifelse(
  rec_rep_dat$Plan_Status %in% c("D", "N/A"),
  FALSE,
  TRUE
)
table(rec_rep_dat$Conflict, rec_rep_dat$Fin_Plan)



########
# Check 05-06 file...
infile <- "~/Downloads/rec_report_05-06.pdf"
tabs1 <- extract_tables(infile)
tabs2 <- lapply(tabs1, as_data_frame)
df1 <- bind_rows(tabs2)
rio::export(df1, file = "rec_report_05-06_init_export.tsv")

df2 <- filter(df1,
              V1 != "",
              !grepl(df1$V1, pattern = "GENERAL SPECIES"),
              !grepl(df1$V1, pattern = "Lead|Species"))
df2$len1 <- sapply(df2$V1, function(x) length(strsplit(x, split = " ")[[1]]))
df3 <- filter(df2, len1 > 1)
blanks <- grep(df3$V2, pattern = "^$")
couple <- blanks + 1

new_names <- sapply(
  blanks,
  function(x) {
    paste(df3$V1[x], df3$V1[x + 1])
  }
)

all_names <- 1:length(df3$V1)
for(i in seq_along(all_names)) {
  if(df3$V2[i] == "") {
    df3$V1[i + 1] <- paste(df3$V1[i], df3$V1[i + 1])
  }
}
df4 <- filter(df3, V2 != "")

# Get the lead region and species separated
df4$Lead_Region <- as.numeric(str_extract(df4$V1, "[0-9]+"))
df4$Species <- str_trim(str_extract(df4$V1, "[^\\d]+"))

# Split up V4
old_V4 <- str_split(df4$V4, " ")
new_V4 <- as_data_frame(do.call(rbind, old_V4))[, c(1:4)]
names(new_V4) <- c("Plan_Status", "Date_Active_Plan", "Est_Recovery_Time",
                   "Est_Recovery_Cost")

# Split up V6
old_V6 <- str_split(df4$V6, " ")

old_V6_2 <- lapply(
  seq_along(old_V6),
  function(x) {
    cur <- old_V6[[x]]
    if(length(cur) > 3) {
      return(c(cur[1], cur[2], paste(cur[3:length(cur)], collapse = " ")))
    } else {
      old_V6[[x]]
    }
  }
)
new_V6 <- as_data_frame(do.call(rbind, old_V6_2))[, c(1:3)]
new_V6$V3 <- ifelse(
  grepl(new_V6$V3, pattern = "outside the U.S."),
  "Presumed Extirpated in the U.S. and Extant Outside the U.S.",
  new_V6$V3
)
names(new_V6) <- c("RPN", "Recovery_Achieved", "Pop_Status")

rec_rep_0506 <- data_frame(
  Species = df4$Species,
  Lead_Region = df4$Lead_Region,
  Date_Listed = df4$V2,
  Date_First_Plan = df4$V3
)
rec_rep_0506 <- cbind(rec_rep_0506, new_V4)
rec_rep_0506$Listing_Class <- df4$V5
rec_rep_0506 <- cbind(rec_rep_0506, new_V6)
rec_rep_0506$Plan_Status <- gsub(rec_rep_0506$Plan_Status,
                                 pattern = "\\(.*\\)",
                                 replacement = "")
saveRDS(rec_rep_0506, file = "~/Downloads/2005_2006_recovery_report.rds")

# quick checks
table(rec_rep_0506$Date_Active_Plan)
table(rec_rep_0506$Plan_Status)

rec_rep_0506$Conflict <- grepl(rec_rep_0506$RPN, pattern = "c")
rec_rep_0506$Fin_Plan <- ifelse(
  rec_rep_0506$Plan_Status %in% c("D", "N/A", "U"),
  FALSE,
  TRUE
)
table(rec_rep_0506$Conflict, rec_rep_0506$Fin_Plan)
