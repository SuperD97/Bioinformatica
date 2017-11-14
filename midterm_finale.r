trasp = function()
{
	datapath = file.choose(new = FALSE)
	
	if(!is.na(datapath))
	{
	
	  if(!file.exists(datapath)) return("File non trovato")
	  
	  if(grepl(".\\.csv", fileC, perl = TRUE) && file.exists(fileC)){  
	  
  	  library(tibble)
  	  wholedata = read.csv(datapath, header = FALSE)
  	  wholedata = as_tibble(wholedata)              
  	  datatag = which(wholedata=="[Data]")[1] + 1
  	  if(is.na(datatag)) return("Struttura file non corretta")
  	 
  	  cols = c("Lane","Sample_ID","Sample_Name","Sample_Plate","Sample_Well","I7_Index_ID",
  			   "index","I5_Index_ID","index2","Sample_Project","Description")
  	  if(!all(wholedata[(datatag),] == cols) ) return("Non sono presenti tutte le colonne necessarie")
  
  	  startdata = wholedata[1:datatag,]
  	  bodydata = wholedata[(datatag+1):(nrow(wholedata)),]
  	  rm(wholedata)
  	  
  	  fcidrow = which(startdata=="FCID")[1]
  	  outrows = nrow(bodydata)
  	  startm = as.matrix(startdata)
  	  fcidcontent = startm[fcidrow, 2]
  	  fcidcol = rep(fcidcontent, outrows)
  	  
  	  lanecol = bodydata$V1
  
  	  sampleid = bodydata$V2
  	  
  	  samplerefrow = which(startdata[,1]=="SampleRef")[1]
  	  samplerefcontent = startm[samplerefrow, 2]
  	  samplerefcol = rep(samplerefcontent, outrows)  
  	  
  	  indexcol = paste(bodydata$V7, bodydata$V9, sep = "-")
  	  
  	  contrlrow = which(startdata[,1]=="Control")[1]
  	  contrlcontent = startm[contrlrow, 2]
  	  contrlfcol = rep(contrlcontent, outrows)  
  	  
  	  reciperow = which(startdata[,1]=="Recipe")[1]
  	  recipecontent = startm[reciperow, 2]
  	  recipecol = rep(recipecontent, outrows)  
  	 
  	  desccol = rep(NA, outrows)
  	  opcol = rep(NA, outrows)
  	  descrcol = rep(NA, outrows)
  	  
  	  outheaders = c("FCID","Lane","SampleID","SampleRef","Index","Description","Control",
  					 "Recipe","Operator","SampleProject")
  	  outdata = tibble(fcidcol, lanecol, sampleid, samplerefcol,
  						   indexcol, desccol, contrlfcol,
  						   recipecol, opcol, descrcol)
  	  outfile = paste(fullpath, "midterm.csv", sep = "")
  	  write.table(outdata, file = outfile, quote = FALSE, sep = ",", na = "", row.names = FALSE,
  				  col.names = outheaders)
  	  
  	  return("File scritto correttamente")
	  }else{
	    print("Il file selezionato non esiste oppure ha estensione diversa da csv")
	  }
	}else
	{
	  print("Operazione annullata")
	}
}

