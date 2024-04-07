
# Preamble ----------------------------------------------------------------
library(tidyverse) # using tidyverse 2.0.0 -- will not work for tidyr 1.2 and earlier
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(forcats) # should already be loaded with tidyverse
library(readxl)

# The goal of this script is to transform the data so that it can be used by the app.
# Either run this script before running the app
#   or load the objects created by this script before to running the app


# Load Initial Excel File  ------------------------------
file_name <- "double major and minor combinations.xlsx" # initial file name

d1 <- read_excel(path = file_name, sheet = "major enrollment") # about 30 seconds
d2 <- read_excel(path = file_name, sheet = "major degrees awarded")
d3 <- read_excel(path = file_name, sheet = "minor awarded")


# Alternative: Convert to CSV (for faster loading times) -----------------

write.csv(d1, file = "major_enrollment.csv")
write.csv(d2, file = "major_degrees_awarded.csv")
write.csv(d3, file = "minor_awarded.csv")


# Alternative: Load CSV files (instead of Excel File) ---------------------


d1 <- read.csv("major_enrollment.csv", numerals = "no.loss")
d2 <- read.csv("major_degrees_awarded.csv", numerals = "no.loss")
d3 <- read.csv("minor_awarded.csv", numerals = "no.loss")



# Correcting `major_perc` to be Consistent with `stu_id_hash` -----------------


d1 <- d1 %>%
  group_by(stu_id_hash, term_cd) %>%
  mutate(major_perc_corrected = 1/n()) %>%
  ungroup()

d2 <- d2 %>%
  group_by(stu_id_hash) %>%
  mutate(major_perc_corrected = 1/n()) %>%
  ungroup()

# different_major_perc_d1 <- d1_corrected %>%
#   filter(major_perc != major_perc_corrected)
# 
# different_major_perc_d2 <- d2_corrected %>%
#   filter(major_perc != major_perc_corrected)
# 
# unique_stu_id_hashes_with_differences_d1 <- different_major_perc_d1 %>%
#   distinct(stu_id_hash) # found 75 students with wrong major_perc
# 
# unique_stu_id_hashes_with_differences_d2 <- different_major_perc_d2 %>%
#   distinct(stu_id_hash) # found 776 students with wrong major_perc


# Creating `d1n`, `d2n`, and `dBIG` -----------------------------------------

# These objects have one observation per unique `stu_id_hash`

d1n <- nest(d1, .by = stu_id_hash)
names(d1n) <- c("stu_id_hash", "Enrollment")

d2n <- nest(d2, .by = stu_id_hash)
names(d2n) <- c("stu_id_hash", "Awarded")

dBIG <- right_join(d1n, d2n, by = "stu_id_hash")



# Cleaning `admit_status` ----------------------------------------------------


admit_status_cleaned <- sapply(dBIG$Enrollment, function(x) {
  if (is.null(x)) {
    return("F") # Assume a student was a freshman if they have no enrollment data (580 cases)
  } else {
    return(sort(x$admit_status)[1]) # There was one student who was listed as both F and A, count them as A
  }
})

admit_status_cleaned <-
  vapply(admit_status_cleaned, # this is for one student who's admit status was NULL-- assume they were a freshman
         function(x) {
           ifelse(x == "NULL", "F", x)
         },
         character(1))


# Creating dSMALL ---------------------------------------------------------

# This will be used to subset the data by user input in the app. 
# Only students who have graduated are included in dSMALL

year_graduated <- vapply(dBIG$Awarded, function(x) {
  x$year_leading_summer_7[1]
}, character(1))

majors_awarded <- lapply(dBIG$Awarded, function(x) {
  sort(unique(x$major_name))
})

majors_enrolled <- lapply(dBIG$Enrollment, function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  majors_enrolled <-
    x[x$major_perc_corrected < 1, c("term_order_4_apb", "major_name")]
  if (nrow(majors_enrolled) == 0) {
    return(NULL)
  } else {
    return(majors_enrolled)
  }
})

full_enrollment_index <- sapply(dBIG$Enrollment, function(x) {
  any(x$registered_terms_incl_summ == 1)
})

dSMALL <-
  tibble(
    stu_id_hash = dBIG$stu_id_hash,
    year_graduated,
    majors_awarded,
    majors_enrolled,
    full_enrollment_index,
    admit_status_cleaned
  )



# Creating ttd_df ---------------------------------------------------------

# This data set is used for calculating time to degree

major_bin2 <- factor(ifelse(d2$major_perc_corrected ==1, "One Major", "Multiple Majors"),
                    levels = c("One Major", "Multiple Majors"),
                    ordered = TRUE)

d2 <- d2 %>% 
  mutate(major_bin = major_bin2)

ttd <- d1 %>% 
  group_by(stu_id_hash) %>% 
  summarise(terms = max(registered_terms_incl_summ)) %>% 
  inner_join(dSMALL, by ="stu_id_hash") %>% 
  select(stu_id_hash, terms, admit_status_cleaned)

ttd_df <- d2 %>% 
  inner_join(ttd, by="stu_id_hash", relationship="many-to-many") %>% 
  group_by(major_name, major_bin, admit_status_cleaned, year_leading_summer_7) %>% 
  summarise(avg_terms = mean(terms)) %>% 
  select(major_name, major_bin, avg_terms, admit_status_cleaned, year_leading_summer_7) %>% 
  arrange(major_name)
ttd_df$admit_status <- ifelse(ttd_df$admit_status_cleaned == 'A', "Transfers", "Freshmen")




# Creating major_choices --------------------------------------------------

# This object gives an index of majors awarded for each row in `dSMALL`
# It's calculated here to save time in the app

major_choices <- sort(unique(d2$major_name))
major_index_df <- dSMALL["stu_id_hash"]
for (m in major_choices) {
  major_index_df <- cbind(major_index_df, 
         vapply(dSMALL$majors_awarded, 
                        function(x) {
                          any(x == m)
                        }, logical(1))
  )
  
}
names(major_index_df)[-1] <- major_choices


# Remove unnecessary objects ----------------------------------------------
rm(d1, d1n, dBIG, majors_awarded, majors_enrolled, ttd, admit_status_cleaned, m, major_bin2, year_graduated)


# Optional: Save Image of Workspace ---------------------------------------

# This can be done so that this script does not need to be run again
save.image("multiple_majors_process_data_IMAGE.RData") # about 13 MB



