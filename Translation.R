DNA<-readline("Enter input DNA string: ");
#length of DNA string taken from user

l=nchar(DNA)
RNA="" #empty string which will store transcribed DNA

for(i in 1:l)
{ 
  #print(i)
  x=substr(DNA,i,i) #This will help to extract single character from string
  
  if(x == "A")
  {
    RNA=paste(RNA,"U",sep="")
  }
  else if(x == "T")
  {
    RNA=paste(RNA,"A",sep="")
  }
  else if(x =="G")
  {
    RNA=paste(RNA,"C",sep="")
  }
  else
  {
    RNA=paste(RNA,"G",sep="")
  }
}
#This is will print RNA seq from 3' to 5'
#print(RNA)
########################## !!SECOND PART OF CODE FROM RNA TO PROTEIN
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
#This will set hash table with values and key
.set(h,three_letter,one_letter)
#Empty protein sequence to store Protein
protein_seq=""
flag=0
for(i in seq(from=1,to=(nchar(RNA)),by=3))
{ 
  window_size=substr(RNA,i,i+2)
  if(window_size == "AUG") #if window is start codon then start translation
  {
    for(i in seq(from=i,to=(nchar(RNA)),by=3))
    {
      window_size=substr(RNA,i,i+2)
      if(window_size=="UGA"| window_size=="UAA"| window_size=="UAG")
      {
        break
      }
      else
        flag=1
        protein_seq=paste(protein_seq,h[[window_size]],sep="")
    }
    
  }
  else if(window_size=="UGA"| window_size=="UAA"| window_size=="UAG")
  {
    break
  }
  else
    next
}

cat("RNA_sequence:3'",RNA,"5'")

if(flag==1)
{
  cat("Nter-",protein_seq,"-Cter")
}else{
  print("Hey sorry we can't get protein sequence")
}




