
# Home PC
folder_root <- "/media/deckard/External/data/kraken"
# G Laptop - this needs to be mounted first
#folder_root <- "/Volumes/NTFS/data/kraken"
list.files(folder_root)

in_curr <- "XBTEUR"
list.files(file.path(folder_root, in_curr, "raw_data"))
to_long(folder_root, in_curr)

max(t)

x <- curr_files[1]
library(data.table)

system.time(read.csv(file.path(raw_dir, x),
         header = TRUE))
# user  system elapsed 
# 0.015   0.001   0.022 

system.time(t <- fread(file.path(raw_dir, x)))
# user  system elapsed 
# 0.006   0.000   0.007