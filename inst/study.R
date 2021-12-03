## ---- settings ----

devtools::load_all()
library(ggalluvial)
library(dplyr)
library(magrittr)


## ---- data_adam ----

test_dta <- adam_ww(50)
attach(test_dta, warn.conflicts = FALSE)

knitr::kable(head(adsl), caption = "Dummy ADSL dataset")
knitr::kable(head(adpasi), caption = "Dummy ADPASI dataset")


## ---- analysis_dataset ----

ads <- adpasi %>%
  filter(AVISIT %in% c("WEEK00", "WEEK01", "WEEK08", "WEEK52")) %>%
  mutate(
    time = factor(AVISIT),
    rsp = cut(AVAL, breaks = 3, labels = c("Low", "Mid", "High")),
    rsp = add_missing(rsp),
    subj = gsub("^SUBJECT (.*)$", x = USUBJID, replacement = "\\1")
  ) %>%
  select(subj, time, rsp) %>%
  arrange(rsp, time, subj)

knitr::kable(
  x = head(ads),
  # lintr demo: only use double-quotes
  caption = 'Outlook of the analysis dataset data'
)


## ---- graphic ----

# lintr demonstration: commented code should be removed
# test <- viridis::viridis(5)

# lintr demonstration: keep using snake_case for variable and functions names
colorScale <- color_scale <-  viridis::viridis(
  nlevels(ads$rsp),
  begin = .2, end = .8, option = "C",
  direction = -1
)
names(color_scale) <- levels(ads$rsp)

gg_sankey <- ads %>%
  ggplot(aes(x = time, stratum = rsp, alluvium = subj, fill = rsp)) +
  geom_stratum(colour = NA) +
  geom_flow(stat = "alluvium", color = "gray85", lwd = .01) +
  ggtitle("Alluvial plot: Response category by visit") +
  scale_fill_manual(values = color_scale) +
  theme_diane()


## ---- annotation ----

gg_sankey <- clean_slate(margin = unit(c(1, 1, 1, 1), "cm")) %>%
  add_header(left = "Topic", right = c("Not Yet Confidential", "Draft")) %>%
  add_title(c(
    "Figure 1 - Prototype of Alluvial plot",
    "Analysis set: 50 random subjects"
  )) %>%
  add_figure(gg_sankey, width = unit(5, "in"), height = unit(2, "in")) %>%
  add_footer("Program: prototype study.sankey", "Page 1 of 1") %T>%
  grid.draw()


## ---- preview ----

message("For the need of the example, the page size is .66 A4 ")
file <- paste0(tempfile(), ".pdf")
pdf(file, width = 11.7 * .66, height = 8.3 * .66)
grid.draw(gg_sankey)
dev.off()
preview(file)
