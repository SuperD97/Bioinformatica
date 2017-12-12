prog3 = function(datapath, output) {
  
  #controllo che il file esista
  if(!is.na(datapath)){
    if(!file.exists(datapath)) return("File non trovato")
    
    
    #metto il file in un tibble
    library(tibble)
    library(RMySQL)
    mydb <- dbConnect(MySQL(), user='genome', host='genome-mysql.soe.ucsc.edu')
    mydb
    
    text = read.table(datapath, header = FALSE)
    text = as.matrix(text)
    row = nrow(text)
    
    output = paste(output,".txt")
    
    for(i in 1:row) {
      type = text[i,1]
      type = tolower(type)
      n = text[i,2]
      if(type == "uomo"){
        res <- dbSendQuery(mydb,"show databases")
        databases <- dbFetch(res)
        databases
        dbClearResult(res)
        
        databases = as.tibble(databases)
        databases = as.matrix(databases)
        databases = as.vector(databases)
        
        hg = grep("\\bhg[^a-z]{2}\\b", databases, value = TRUE)
        hg
        
        ultimo = hg[length(hg)]
        ultimo
        
        ul = paste("use ",ultimo, sep="")
        ul
        
        #estrazione name e name 2 dall'ultimo database
        res <- dbSendQuery(mydb, ul)
        dbFetch(res)
        dbClearResult(res)
        
        sel = paste("SELECT name FROM ncbiRefSeq where name = '",n,"' or name2 = '",n,"'",sep="")
        res <- dbSendQuery(mydb, sel)
        namePrimo <- dbFetch(res)
        dbClearResult(res)
        
        sel = paste("SELECT name2 FROM ncbiRefSeq where name = '",n,"' or name2 = '",n,"'",sep="")
        res <- dbSendQuery(mydb, sel)
        name2Primo <- dbFetch(res)
        dbClearResult(res)
        
        #scrittura output primo database
        testo = paste("Specie: ", type, " - db usato: ", ultimo, " - name: ", namePrimo, " - name2: ", name2Primo, sep = "")
        if(i == 1){
          write.table(testo, file = output, append = FALSE, quote = FALSE, sep = ",", na = "", row.names = FALSE, col.names = FALSE)
        }else {
          write.table(testo, file = output, append = TRUE, quote = FALSE, sep = ",", na = "", row.names = FALSE, col.names = FALSE)
        }
          
        
        
      }else if(type == "topo"){
        res <- dbSendQuery(mydb,"show databases")
        databases <- dbFetch(res)
        databases
        dbClearResult(res)
        
        databases = as.tibble(databases)
        databases = as.matrix(databases)
        databases = as.vector(databases)
        
        mm = grep("\\bmm[^a-z]{2}\\b", databases, value = TRUE)
        mm
        
        ultimo = mm[length(mm)]
        ultimo
        
        ul = paste("use ",ultimo, sep="")
        ul
        
        #estrazione name e name 2 dall'ultimo database
        res <- dbSendQuery(mydb, ul)
        dbFetch(res)
        dbClearResult(res)
        
        sel = paste("SELECT name FROM ncbiRefSeq where name = '",n,"' or name2 = '",n,"'",sep="")
        res <- dbSendQuery(mydb, sel)
        namePrimo <- dbFetch(res)
        dbClearResult(res)
        
        sel = paste("SELECT name2 FROM ncbiRefSeq where name = '",n,"' or name2 = '",n,"'",sep="")
        res <- dbSendQuery(mydb, sel)
        name2Primo <- dbFetch(res)
        dbClearResult(res)
        
        #scrittura output primo database
        testo = paste("Specie: ", type, " - db usato: ", ultimo, " - name: ", namePrimo, " - name2: ", name2Primo, sep = "")
        if(i == 1){
          write.table(testo, file = output, append = FALSE, quote = FALSE, sep = ",", na = "", row.names = FALSE, col.names = FALSE)
        }else {
          write.table(testo, file = output, append = TRUE, quote = FALSE, sep = ",", na = "", row.names = FALSE, col.names = FALSE)
        }
        
        
      }else if(type == "ratto"){
        res <- dbSendQuery(mydb,"show databases")
        databases <- dbFetch(res)
        databases
        dbClearResult(res)
        
        databases = as.tibble(databases)
        databases = as.matrix(databases)
        databases = as.vector(databases)
        
        rn = grep("\\brn[^a-z]{2}\\b", databases, value = TRUE)
        rn
        
        ultimo = rn[length(rn)]
        ultimo
        
        ul = paste("use ",ultimo, sep="")
        ul
        
        #estrazione name e name 2 dall'ultimo database
        res <- dbSendQuery(mydb, ul)
        dbFetch(res)
        dbClearResult(res)
        
        sel = paste("SELECT name FROM ncbiRefSeq where name = '",n,"' or name2 = '",n,"'",sep="")
        res <- dbSendQuery(mydb, sel)
        namePrimo <- dbFetch(res)
        dbClearResult(res)
        
        sel = paste("SELECT name2 FROM ncbiRefSeq where name = '",n,"' or name2 = '",n,"'",sep="")
        res <- dbSendQuery(mydb, sel)
        name2Primo <- dbFetch(res)
        dbClearResult(res)
        
        
        #scrittura output primo database
        testo = paste("Specie: ", type, " - db usato: ", ultimo, " - name: ", namePrimo, " - name2: ", name2Primo, sep = "")
        if(i == 1){
          write.table(testo, file = output, append = FALSE, quote = FALSE, sep = ",", na = "", row.names = FALSE, col.names = FALSE)
        }else {
          write.table(testo, file = output, append = TRUE, quote = FALSE, sep = ",", na = "", row.names = FALSE, col.names = FALSE)
        }
        
      }
      
    }
    
    return("File scritto correttamente")
    
  } else {
    print("Operazione annullata")
  }
  
  dbDisconnect(mydb)
}