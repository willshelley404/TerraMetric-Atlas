# Prints key=value lines (month, prob, tier) from the snapshot, for the
# workflow's GITHUB_OUTPUT. Run from the repo root.
s <- readRDS("docs/recession_model_snapshot.rds")
cat(sprintf(
  "month=%s\nprob=%.1f\ntier=%s\n",
  format(s$current$month, "%b %Y"), s$current$prob, s$current$tier$label
))
