## glossary.R
## A glossary is a 3 colums data.frame 
## col[1] : words to be defined
## col[2] : simple definition of the word on his left : these definitions are visible in index.html when putting the mouse cursor over the word
## col[3] : a more complete definition of the word in column 1. These definitions will be writen in glossary.html with the saveHtmlGlossary function


## setDefinition enables to set a new definition into the glossary
## word : word to be defined
setDefinition = function(glossary, word, simpleDefinition, completeDefinition) {
	toAdd = data.frame(word=word, simpleDefinition=simpleDefinition, completeDefinition=completeDefinition)
	newGlossary = rbind(glossary,toAdd)
	return(newGlossary)
}


## making of the glossary. Insertion of the definitions here 
createGlossary = function() {	
	glossary = data.frame()
	
	## Average dynamic range
	simpleDef = "The average dynamic range is the mean of the replicate dynamic ranges (see replicate dynamic range) for the plate over all the replicates"
	completeDef = "The average dynamic range is the mean of the replicate dynamic ranges (see below) for the plate over all the replicates"
	glossary = setDefinition(glossary, "Average dynamic range", simpleDef, completeDef)  

	## Replicate dynamic range
	simpleDef = "The replicate dynamic range is an indicator of the gap between the positive and the negative controls"
	completeDef = "The replicate dynamic range provides an indicator of the gap between the positive and the negative controls. <br/><br/>Depending of the value of the 'scale' attribute from the \"summarizePlate\" function, the replicate dynamic range is either : <br/>- the difference between the arithmetic average on positive and negative controls <br/>- the ratio between the geometric averages on positive and negative controls. <br/><br/>By default the choice is based on the scale of the data : if the scale is positive, then it is the ratio of geometric average, otherwise, it is the difference of arithmetic averages."
	glossary = setDefinition(glossary, "Replicate dynamic range", simpleDef,completeDef)
	
	## Repeatability standard deviation
	simpleDef = "The repeatability standard deviation is the standard deviation for the random variable <i>standard deviation for a gene across all the replicates</i>. It is used to determine if the results for each replicate are similar or not. 0 is the perfect score, whereas a high value means the results are very different."
	completeDef =  "The repeatability standard deviation is the standard deviation for the random variable 'standard deviation for a gene across all the replicates'. It is used to determine if the results for each replicate are similar or not. <br/><br/>0 is the perfect score, whereas a high value means the results are very different."
	glossary = setDefinition(glossary, "Repeatability standard deviation", simpleDef, completeDef) 
	
	## "Spearman rank correlation "
	simpleDef = "The Spearman rank correlation coefficient is a mesure of correlation between two replicates. A result close to zero means there is no correlation between the two replicates, whereas a result close to 1 means there is a strong correlation between them."
	completeDef = "The Spearman rank correlation coefficient is a mesure of correlation between two replicates. <br/><br/>A result close to zero means there is no correlation between the two replicates, whereas a result close to 1 means there is a strong correlation between them."	
	glossary = setDefinition(glossary, "Spearman rank correlation ",simpleDef,completeDef)
	
	## "Spearman rank correlation (min - max)"
	simpleDef = "This values correspond to the min and the max of the Spearman rank correlation among replicates. The Spearman rank correlation coefficient is a mesure of correlation between replicates. A result close to zero means there is no correlation between the two replicates, whereas a result close to 1 means there is a strong correlation between them."
	completeDef = "This values correspond to the min and the max of the Spearman rank correlation among replicates"
	glossary = setDefinition(glossary, "Spearman rank correlation (min - max)",simpleDef,completeDef)
	
	return(glossary)
}


## getDefinition returns the (simple) definition associated to the word given in argument
getDefinition = function(word, glossary) {
	index = match(word, glossary[,1])
	return(glossary[,2][index])	
}


## Saving the glossary as a html file
saveHtmlGlossary = function(glossary, targetGlossary) {  
	targetGlossaryFile=openPage(targetGlossary, title = "Glossary")
	hwrite('Glossary',targetGlossaryFile,heading=1,center=T,br=T)
	htmlGlossary=data.frame(word=glossary$word,def=glossary$completeDefinition) ##we only want the complete definitions in the html report
	mcolor=dataframeColor(htmlGlossary)  
	hwrite(htmlGlossary,targetGlossaryFile,col.names=FALSE,row.names=FALSE,bgcolor=mcolor,border=1, center = TRUE, col.width = c("25%", "75%")) 
	closePage(targetGlossaryFile)
}
