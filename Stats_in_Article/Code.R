#Install the Library
install.packages("mbX")

#load the library

library(mbX)


##Clean at Genus level:**

ezclean("rumen_feces_microbiome.csv",
        "rumen_fecal_metadata.txt",
        level = "G")



#Visualize by SampleType (Rumen vs. Feces), showing the top 10 genera

ezviz("rumen_feces_Genus_abundance.xlsx",
      "rumen_fecal_metadata.txt",
      level             = "G",
      selected_metadata = "SampleType",
      top_taxa          = 10)

# instead of using top_taxa, user can use the parameter threshold 
# lets use threshold value = 0.5, 
# this dumps all the genera whose abundance is less than 0.5 into Other_genera

ezviz("rumen_feces_Genus_abundance.xlsx",
      "rumen_fecal_metadata.txt",
      level             = "G",
      selected_metadata = "SampleType",
      threshold          = 0.5)
