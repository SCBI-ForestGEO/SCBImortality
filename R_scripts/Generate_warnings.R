# Generate warnings looking at the reports that are not looked at with testhat ####

## this script is run automatically when there is a push 

# clear environment ####
rm(list = ls())

# load libraries ####
library(here)

# check if files exist and generate a plot with the warning ####
warnings_to_look_at <- list(
  DBH_within_2cm =list(warning_message = "There are DBH measurements of dead trees that are not withing 2cm of previous census.",
                       file = file.path(here("testthat"), "reports/requires_field_fix/DBH_dead_suspicious.csv")
  )
)

all_warns <- NULL
for(i in seq_along(warnings_to_look_at)) {
  if(file.exists(warnings_to_look_at[[i]]$file)) all_warns <- c(all_warns, warnings_to_look_at[[i]]$warning_message)
}


png(file.path(here("testthat"), "reports/warnings.png"), width = 6, height = 2*length(all_warns), units = "in", res = 300)
par(mar = c(0,0,0,0))
plot(0,0, axes = F, xlab = "", ylab = "", type = "n")
text(0,0, paste(all_warns, collapse = "\n"), col = "red", cex = 0.6)
title("warnings!!!", col.main= "red", xpd = NULL, line = -1)
dev.off()

