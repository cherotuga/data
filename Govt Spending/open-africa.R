# Upload to open.africa

library(ckanr)


groups <- group_list(as = "list")
head(groups, 10)  # Fetch and display only the first 10 groups
