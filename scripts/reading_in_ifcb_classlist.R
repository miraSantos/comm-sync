## Read in classlist file

groups = read.csv("C:\\Users\\Miraflor P Santos\\comm-sync\\data\\IFCB_classlist_type.csv")
head(groups)


###################################
create_groups <- function(func_group){
  reference=groups$CNN_classlist[(groups[func_group]==1)]
  index=intersect(colnames(df),reference)
  return(index)
}

diatom_index <- create_groups("Diatom_noDetritus")
dino_index <- create_groups("Dinoflagellate") #TO DO: exclude dinophycaeae
cocco_index <- create_groups("Coccolithophore")
cilia_index <- create_groups("Ciliate")
flagellate_index <- create_groups("flagellate")
protist_tricho_index <- create_groups("protist_tricho")


diatom_index <- groups$CNN_classlist[groups["Diatom_noDetritus"]==1]
dino_index <- groups$CNN_classlist[groups["Dinoflagellate"]==1]
