library(hash)
h=hash()
three_letter=c("UUU","UUC","UUA","UUG","CUU","CUC","CUA","CUG","AUU","AUC","AUA","AUG","GUU","GUC",
               "GUA","GUG","UCU","UCC","UCA","UCG","CCU","CCC","CCA","CCG","ACU","ACC","ACA","ACG",
               "GCU","GCC","GCA","GCG","UAU","UAC","UAA","UAG","CAU","CAC","CAA","CAG","AAU","AAC",
               "AAA","AAG","GAU","GAC","GAA","GAG","UGU","UGC","UGA","UGG","CGU","CGC","CGA","CGG",
               "AGU","AGC","AGA","AGG","GGU","GGC","GGA","GGG")
one_letter=c("F","F","L","L","L","L","L","L","I","I","I","M","V","V","V","V","S","S","S","S","P","P",
             "P","P","T","T","T","T","A","A","A","A","Y","Y","STOP","STOP","H","H","Q","Q","N","N","K",
             "K","D","D","E","E","C","C","STOP","W","R","R","R","R","S","S","R","R","G","G","G","G")
.set(h,three_letter,one_letter)
h[["UUU"]]
protein_seq=""
RNA="AUGCGGUACUUAAAAUUUCCCUAGCUAUC"
for(i in seq(from=1,to=(nchar(RNA)),by=3))
{
  window_3=substr(RNA,i,i+2)
  print(window_3)
  if(window_3=="UGA"| window_3=="UAA"| window_3=="UAG")
  {
    break
  }
  #print(h[[window_3]])
  protein_seq=paste(protein_seq,h[[window_3]])
}
print(protein_seq)