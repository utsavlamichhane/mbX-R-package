install.packages("mbX")
library(mbX)







#change dir to study11
#study 11 with 20 samples
#ezclean
system.time(ezclean("microbiome11.csv", "metadata11.csv", "k"))

system.time(ezclean("microbiome11.csv", "metadata11.csv", "p"))

system.time(ezclean("microbiome11.csv", "metadata11.csv", "c"))

system.time(ezclean("microbiome11.csv", "metadata11.csv", "o"))

system.time(ezclean("microbiome11.csv", "metadata11.csv", "f"))

system.time(ezclean("microbiome11.csv", "metadata11.csv", "g"))

system.time(ezclean("microbiome11.csv", "metadata11.csv", "s"))


#ezviz
system.time(ezviz("microbiome11.csv", "metadata11.csv", "d", "Sex", top_taxa = 20))

system.time(ezviz("microbiome11.csv", "metadata11.csv", "p", "Sex", top_taxa = 20))


system.time(ezviz("microbiome11.csv", "metadata11.csv", "f", "Sex", top_taxa = 20))

system.time(ezviz("microbiome11.csv", "metadata11.csv", "g", "Sex", top_taxa = 20))

system.time(ezviz("microbiome11.csv", "metadata11.csv", "s", "Sex", top_taxa = 20))

 #_________________________________________________________________________________________
 
 
 #change dir to study12
 #study 12 with 137 samples
 #ezclean
 system.time(ezclean("microbiome12.csv", "metadata12.csv", "k"))
 system.time(ezclean("microbiome12.csv", "metadata12.csv", "p"))
 system.time(ezclean("microbiome12.csv", "metadata12.csv", "c"))
 system.time(ezclean("microbiome12.csv", "metadata12.csv", "o"))
 system.time(ezclean("microbiome12.csv", "metadata12.csv", "f"))
 system.time(ezclean("microbiome12.csv", "metadata12.csv", "g"))
 system.time(ezclean("microbiome12.csv", "metadata12.csv", "s"))
 
 #ezviz
 system.time(ezviz("microbiome12.csv", "metadata12.csv", "d", "SampType", top_taxa = 20))
 system.time(ezviz("microbiome12.csv", "metadata12.csv", "p", "SampType", top_taxa = 20))
 system.time(ezviz("microbiome12.csv", "metadata12.csv", "c", "SampType", top_taxa = 20))
 system.time(ezviz("microbiome12.csv", "metadata12.csv", "o", "SampType", top_taxa = 20))
 system.time(ezviz("microbiome12.csv", "metadata12.csv", "f", "SampType", top_taxa = 20))
 system.time(ezviz("microbiome12.csv", "metadata12.csv", "g", "SampType", top_taxa = 20))
 system.time(ezviz("microbiome12.csv", "metadata12.csv", "s", "SampType", top_taxa = 20))
 
# _________________________________________________________________________________________
 
 #change dir to study6
 # study 6 with 1176 samples
 # ezclean
 system.time(ezclean("microbiome6.csv", "metadata6.csv", "k"))
 system.time(ezclean("microbiome6.csv", "metadata6.csv", "p"))
 system.time(ezclean("microbiome6.csv", "metadata6.csv", "c"))
 system.time(ezclean("microbiome6.csv", "metadata6.csv", "o"))
 system.time(ezclean("microbiome6.csv", "metadata6.csv", "f"))
 system.time(ezclean("microbiome6.csv", "metadata6.csv", "g"))
 system.time(ezclean("microbiome6.csv", "metadata6.csv", "s"))
 
 # ezviz
 system.time(ezviz("microbiome6.csv", "metadata6.csv", "d", "NexteraKit", top_taxa = 20))
 system.time(ezviz("microbiome6.csv", "metadata6.csv", "p", "NexteraKit", top_taxa = 20))
 system.time(ezviz("microbiome6.csv", "metadata6.csv", "c", "NexteraKit", top_taxa = 20))
 system.time(ezviz("microbiome6.csv", "metadata6.csv", "o", "NexteraKit", top_taxa = 20))
 system.time(ezviz("microbiome6.csv", "metadata6.csv", "f", "NexteraKit", top_taxa = 20))
 system.time(ezviz("microbiome6.csv", "metadata6.csv", "g", "NexteraKit", top_taxa = 20))
 system.time(ezviz("microbiome6.csv", "metadata6.csv", "s", "NexteraKit", top_taxa = 20))
 