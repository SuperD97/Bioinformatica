prog3 = function(datapath, type, output) {
  
  #controllo che il file esista
  if(!is.na(datapath)){
    if(!file.exists(datapath)) return("File non trovato")
    
    
    #metto il file in un tibble
    library(tibble)
    library(RMySQL)
    mydb <- dbConnect(MySQL(), user='genome', host='genome-mysql.soe.ucsc.edu')
    mydb
    
    text = read.table(datapath, header = FALSE)
    row = nrow(text)
    
    #
    for(i in 1:row) {
      r = text[i,]
      type = r[1]
      n = r[2]
      if(type == "Uomo"){
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
        penultimo = hg[length(hg) -1]
        penultimo
        
        ul = paste("use ",ultimo, sep="")
        ul
        
        pn = paste("use ",penultimo, sep="")
        pn
        
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
        
        #estrazione name e name2 dal penultimo database
        res <- dbSendQuery(mydb, pn)
        dbFetch(res)
        dbClearResult(res)
        
        sel = paste("SELECT name FROM ncbiRefSeq where name = '",n,"' or name2 = '",n,"'",sep="")
        res <- dbSendQuery(mydb, sel)
        nameSecondo <- dbFetch(res)
        dbClearResult(res)
        
        sel = paste("SELECT name2 FROM ncbiRefSeq where name = '",n,"' or name2 = '",n,"'",sep="")
        res <- dbSendQuery(mydb, sel)
        name2Secondo <- dbFetch(res)
        dbClearResult(res)
        
        #scrittura output primo database
        chiave = paste("Specie: ", type, " - db usato: ", ultimo, sep = "")
        names = c(namePrimo, name2Primo)
        testo = add_column(.data = names, key = chiave, .before = 1)
        if( i == 1 )
        {
          app = FALSE
          clnames = outheaders
        }else
        {
          app = TRUE
          clnames = FALSE
        }
        write.table(testo, file = output, append = app, quote = FALSE, sep = ",", na = "", row.names = FALSE, col.names = clnames)  
        
        #scrittura usando secondo database
        chiave = paste("Specie: ", type, " - db usato: ", penultimo, sep = "")
        names = c(nameSecondo, name2Secondo)
        testo = add_column(.data = names, key = chiave, .before = 1)
        
        write.table(testo, file = output, append = TRUE, quote = FALSE, sep = ",", na = "", row.names = FALSE, col.names = FALSE)  
        
      }else if(type == "Topo"){
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
        penultimo = mm[length(mm) -1]
        penultimo
        
        ul = paste("use ",ultimo, sep="")
        ul
        
        pn = paste("use ",penultimo, sep="")
        pn
        
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
        
        #estrazione name e name2 dal penultimo database
        res <- dbSendQuery(mydb, pn)
        dbFetch(res)
        dbClearResult(res)
        
        sel = paste("SELECT name FROM ncbiRefSeq where name = '",n,"' or name2 = '",n,"'",sep="")
        res <- dbSendQuery(mydb, sel)
        nameSecondo <- dbFetch(res)
        dbClearResult(res)
        
        sel = paste("SELECT name2 FROM ncbiRefSeq where name = '",n,"' or name2 = '",n,"'",sep="")
        res <- dbSendQuery(mydb, sel)
        name2Secondo <- dbFetch(res)
        dbClearResult(res)
        
        #scrittura output primo database
        chiave = paste("Specie: ", type, " - db usato: ", ultimo, sep = "")
        names = c(namePrimo, name2Primo)
        testo = add_column(.data = names, key = chiave, .before = 1)
        if( i == 1 )
        {
          app = FALSE
          clnames = outheaders
        }else
        {
          app = TRUE
          clnames = FALSE
        }
        write.table(testo, file = output, append = app, quote = FALSE, sep = ",", na = "", row.names = FALSE, col.names = clnames)  
        
        #scrittura usando secondo database
        chiave = paste("Specie: ", type, " - db usato: ", penultimo, sep = "")
        names = c(nameSecondo, name2Secondo)
        testo = add_column(.data = names, key = chiave, .before = 1)
        
        write.table(testo, file = output, append = TRUE, quote = FALSE, sep = ",", na = "", row.names = FALSE, col.names = FALSE)  
        
        
      }else if(type == "Ratto"){
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
        penultimo = hrn[length(rn) -1]
        penultimo
        
        ul = paste("use ",ultimo, sep="")
        ul
        
        pn = paste("use ",penultimo, sep="")
        pn
        
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
        
        #estrazione name e name2 dal penultimo database
        res <- dbSendQuery(mydb, pn)
        dbFetch(res)
        dbClearResult(res)
        
        sel = paste("SELECT name FROM ncbiRefSeq where name = '",n,"' or name2 = '",n,"'",sep="")
        res <- dbSendQuery(mydb, sel)
        nameSecondo <- dbFetch(res)
        dbClearResult(res)
        
        sel = paste("SELECT name2 FROM ncbiRefSeq where name = '",n,"' or name2 = '",n,"'",sep="")
        res <- dbSendQuery(mydb, sel)
        name2Secondo <- dbFetch(res)
        dbClearResult(res)
        
        #scrittura output primo database
        chiave = paste("Specie: ", type, " - db usato: ", ultimo, sep = "")
        names = c(namePrimo, name2Primo)
        testo = add_column(.data = names, key = chiave, .before = 1)
        if( i == 1 )
        {
          app = FALSE
          clnames = outheaders
        }else
        {
          app = TRUE
          clnames = FALSE
        }
        write.table(testo, file = output, append = app, quote = FALSE, sep = ",", na = "", row.names = FALSE, col.names = clnames)  
        
        #scrittura usando secondo database
        chiave = paste("Specie: ", type, " - db usato: ", penultimo, sep = "")
        names = c(nameSecondo, name2Secondo)
        testo = add_column(.data = names, key = chiave, .before = 1)
        
        write.table(testo, file = output, append = TRUE, quote = FALSE, sep = ",", na = "", row.names = FALSE, col.names = FALSE)  
        
      }
      
    }
    
    
    
    
    
    
    
  } else {
    print("Operazione annullata")
  }
  
  dbDisconnect(mydb)
}