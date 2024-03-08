species_list_files = list.files("/home/mira/MIT-WHOI/github_repos/comm-sync/figures/sorting_species/full_annual/")
species_list= substr(species_list,1,nchar(species_list_files)-7)

print(species_list)


begin = "/home/mira/MIT-WHOI/github_repos/comm-sync/figures/sorting_species/partial annual/beginning/"
mid = "/home/mira/MIT-WHOI/github_repos/comm-sync/figures/sorting_species/partial annual/mid/"
end = "/home/mira/MIT-WHOI/github_repos/comm-sync/figures/sorting_species/partial annual/end/"

species_list_files = list.files(begin)
species_list= substr(species_list_files,1,nchar(species_list_files)-7)
for(i in species_list){print(i)}


species_list_files = list.files(mid)
species_list= substr(species_list_files,1,nchar(species_list_files)-7)
for(i in species_list){print(i)}


species_list_files = list.files(end)
species_list= substr(species_list_files,1,nchar(species_list_files)-7)
for(i in species_list){print(i)}

