# Upload to open.africa

library(ckanr)

ckanr_setup(url = "https://africaopendata.org", key = "ef74e174-daac-48d7-8c14-41eed3fea105")


groups <- group_list(as = "list")
head(groups, 10)  # Fetch and display only the first 10 groups
