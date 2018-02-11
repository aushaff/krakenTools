
# Home PC
#folder_root <- "/media/deckard/External/data/kraken"
# G Laptop - this needs to be mounted first
folder_root <- "/Volumes/NTFS/data/kraken"
list.files(folder_root)

curr <- "XBTEUR"
list.files(file.path(folder_root, curr, "raw_data"))
t <- to_long(folder_root, curr)
max(t)
