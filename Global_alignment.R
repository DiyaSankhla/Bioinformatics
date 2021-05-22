# Take both input strings,penalty and match score
sequence1 <- readline("Enter first sequence(Horizontal): ");
sequence2 <- readline("Enter Second sequence(Vertical): ");
gap <- readline("Enter Gap penalty: ");
mismatch <- readline("Enter mismatch penalty: ");
match <- readline("Enter Match score: ");

##Convert string to integer as readline take string as input
gap=as.integer(gap)
mismatch=as.integer(mismatch)
match=as.integer(match)


#length of two sequences 
len1=nchar(sequence1)
len2=nchar(sequence2)

#Initialize a Scoring matrix M  
Score_M=matrix(0,nrow=len1+1,ncol=len2+1)

#Covert two string into character vector
sequence1=unlist(strsplit(sequence1,""))
sequence2=unlist(strsplit(sequence2,""))

#Assigning individual character as row and column name for respective seq

rownames(Score_M)=c(' ',sequence1)
colnames(Score_M)=c(' ',sequence2)

#Matrix to guide direction
#This matrix can be useful when we will traceback 
#and since for diagonal we have Two condition one is match and mismatch this can be specified here only

Dir=matrix(0,nrow=len1+1,ncol=len2+1)
rownames(Score_M)=c(' ',sequence1)
colnames(Score_M)=c(' ',sequence2)

#Assigning gap penalty to first row and first column on incremental order of gap value

Score_M[1,]=cumsum(c(0,rep(gap,times=len2)))
Score_M[,1]=cumsum(c(0,rep(gap,times=len1)))

#assigning score to matrix
for(i in 2:(len1+1))
{
  for(j in 2:(len2+1))
  {
    #assign score from horizontal and vertical direction
    horizontal=Score_M[i,j-1] + gap
    vertical=Score_M[i-1,j] + gap
    
    #assigning match/mismatch value from diagonal
    if(rownames(Score_M)[i]==colnames(Score_M)[j])
      diagonal=Score_M[i-1,j-1] + match
    else
      diagonal=Score_M[i-1,j-1] + mismatch
    
    #matrix will store max value from these direction
    Score_M[i,j]=max(horizontal,vertical,diagonal)
    
    #which.max gives the index of max value which will be stored in vector accordingly
    if(max(horizontal,vertical,diagonal)== diagonal)
      Dir[i,j]= "diagonal"
    else if(max(horizontal,vertical,diagonal)==vertical)
      Dir[i,j]="vertical"
    else
      Dir[i,j]="horizontal"
    
  }
}

#two alignment vectors
alignment1=character()
alignment2=character()

while(i>1 & j>1)
{
  ##We will get direction from Dir matrix for diagonal position
  
  if(Dir[i,j]=="diagonal")
  {
    alignment1=c(rownames(Score_M)[i],alignment1)
    alignment2=c(colnames(Score_M)[j],alignment2)
    j=j-1
    i=i-1
  }
  
  else if(Score_M[i-1,j]+gap == Score_M[i,j])
  {
    alignment1=c(rownames(Score_M)[i],alignment1)
    alignment2=c("-",alignment2)
    i=i-1
  }
  
  else
  {
    alignment1=c("-",alignment1)
    alignment2=c(colnames(Score_M)[j],alignment2)
    j=j-1
  }
}

x=paste(alignment1,collapse="")
y=paste(alignment2,collapse="")
cat("Optimal Alignment:","\n",x,"\n",y)
cat("Score of Optimal Alignemnt: ",Score_M[length(Score_M)])





