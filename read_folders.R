

#get all project folders
folders_1 = list.dirs(path = "Z:/Clients/mb/2013", full.names = F, recursive = F)
folders_2 = list.dirs(path = "Z:/Clients/mb/2014", full.names = F, recursive = F)
folders_3 = list.dirs(path = "Z:/Clients/mb/2015", full.names = F, recursive = F)
folders = c(folders_1, folders_2, folders_3)

# print one element per line
cat(folders,sep="\n")

files = list.files(path = "Z:/Clients/mb/2015", full.names = T, recursive = T, pattern = ".php$")

php = rep(NA, length(files))
ProjectID = rep(NA, length(files))
IDlist = rep(NA, length(files))

for (i in 1:length(files)){
  php = readLines(file(files[i]))
  slot = grep("2015-", readLines(file(files[i])))
  IDlist[i] = php[slot]
}

cat(IDlist,sep="\n")
