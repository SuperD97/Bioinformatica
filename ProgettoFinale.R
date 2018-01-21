#Componenti Gruppo: Giorgio Bancheri, Adriano Crasnich, Gianmaria D'Ambrosi, Simone Rodaro, Marco Tugnizza e Davide Spanghero

esame = function(file, pathFile="", gruppo = 'Gruppo', parametro = 1, output = 'Risultato.txt'){
  
  
  library(tibble)
  library(RMySQL)
  
  #Var init
  #Ora di sistema
  time = Sys.time()
  row = 1
  nostrovalore = 0
  totValori = 0
  media = 0
  rapporto = 0
  dati = ""
  testo = ""
  log = ""
  
  #End var init
  
  #connessione al db di ucsc
  mydb <- dbConnect(MySQL(), user='genome', host='genome-mysql.soe.ucsc.edu')
  mydb
  
  #elenco input
  elenco_input = paste("Nome file: ", file, " - gruppo: ",gruppo, " - parametro: ", parametro, " - nome file di output: ",output)
  
  #controllo che il parametro inserito non sia un numero superiore a 53 o inferiore a 1
  if(parametro > 53 & parametro <1) {parametro = 1}
  
  #controllo la lunghezza della variabile file per vedere se ? un vettore oppure una normale stringa
  if(length(file)==1){
    
    #controllo che sia un file .txt
    if(grepl(".txt$", file)){
      
      #controllo l'esistenza del file
      if(file.exists(file)){
        
        #in caso affermativo salvo iil contenuto del file in un vettore
        testo = read.table(file, header = FALSE, sep = "\n")
        testo = as.tibble(testo)
        testo = as.matrix(testo)
        testo = as.vector(testo)
        row = length(testo)
        
      } else {
        log = "File non trovato"
        scriviLog(log, elenco_input, output, time)
        return
      }
      
    } else {
      testo = file;                       #vettore con una stringa
      row = length(testo)
    }
  } else {
    testo = as.vector(file);                          #vettore di pi? stringhe
    row = length(testo)
  }
  
  
  #ciclo le righe del file o del vettore, in caso il vettore sia composto da un solo elemento la variabile row inizializzata a 1 non viene modificata quindi
  #sostanzialmente ciclo su una riga sola, questo per non scrivere pi? volte lo stesso codice
  for(i in 1:row) {
    n = testo[i]
    
    #estraggo name , name2, chrom, strand, txStart, txEnd, cdsStart, cdsEnd dal database hg38 e dalla tabella wgEncodeGencodeBasicV27 selezionando per name o name2
    ris = dbSendQuery(mydb, "use hg38")
    dbFetch(ris)
    dbClearResult(ris)
    
    select = paste("SELECT name FROM wgEncodeGencodeBasicV27 where name = '",n,"' or name2 = '",n,"'",sep="")
    ris = dbSendQuery(mydb, select)
    name = dbFetch(ris)
    dbClearResult(ris)
    
    select = paste("SELECT name2 FROM wgEncodeGencodeBasicV27 where name = '",n,"' or name2 = '",n,"'",sep="")
    ris = dbSendQuery(mydb, select)
    name2 = dbFetch(ris)
    dbClearResult(ris)
    
    select = paste("SELECT chrom FROM wgEncodeGencodeBasicV27 where name = '",name,"'",sep="")
    ris = dbSendQuery(mydb, select)
    chrom = dbFetch(ris)
    dbClearResult(ris)
    
    select = paste("SELECT strand FROM wgEncodeGencodeBasicV27 where name = '",name,"'",sep="")
    ris = dbSendQuery(mydb, select)
    strand = dbFetch(ris)
    dbClearResult(ris)
    
    select = paste("SELECT txStart FROM wgEncodeGencodeBasicV27 where name = '",name,"'",sep="")
    ris = dbSendQuery(mydb, select)
    txStart = dbFetch(ris)
    dbClearResult(ris)
    
    select = paste("SELECT txEnd FROM wgEncodeGencodeBasicV27 where name = '",name,"'",sep="")
    ris = dbSendQuery(mydb, select)
    txEnd = dbFetch(ris)
    dbClearResult(ris)
    
    select = paste("SELECT cdsStart FROM wgEncodeGencodeBasicV27 where name = '",name,"'",sep="")
    ris = dbSendQuery(mydb, select)
    cdsStart = dbFetch(ris)
    dbClearResult(ris)
    
    select = paste("SELECT cdsEnd FROM wgEncodeGencodeBasicV27 where name = '",name,"'",sep="")
    ris = dbSendQuery(mydb, select)
    cdsEnd = dbFetch(ris)
    dbClearResult(ris)
    
    #controllo che il file esista nel percorso passato
    percorso = paste(pathFile, "/gtexTranscExpr")
    if(file.exists(percorso)){
      fileEsterno = read.table(percorso, header = FALSE, sep='\t')
      fileEsterno = as.matrix(fileEsterno)
      rowFE = nrow(fileEsterno)
      
      #se il file esiste, dopo aver contato le righe estraggo il mio valore di expScore e la media di tutti gli expScore nella posizione scelta (default=1)
      for(i in 1:rowFE){
        if(fileEsterno[i, 4] == name & fileEsterno[i, 1] == chrom){
          serie = fileEsterno[i, 9]
          singoli = strsplit(serie, ",")
          nostroValore = as.numeric(singoli[i])
        }
        serie = fileEsterno[i, 9]
        singoli = strsplit(serie, ",")
        totValori = totValori + as.numeric(singoli[i])
      }
      
      #dopo aver estratto il mio valore e la somma del valore in ogni riga, faccio la media e il rapporto tra il mio valore e la media
      media = totValori/53
      
      rapporto = nostrovalore/media
      
      nostroValore = as.character(nostroValore)
      media = as.character(media)
      rapporto = as.character(rapporto)
    } else {
      log = "Il file gtexTranscExpr non ? stato trovato, quindi i dati non sono stati trascritti"
    }
    
    #compongo il corpo del testo per la sezione dei dati
    testoFinale = paste("name: ", name, " - name2: ", name2 ," - chrom: ", chrom, " - strand: ", strand, " - txStart: ", txStart, " - txEnd: ", txEnd, " - cdsStart: ", cdsStart, " - cdsEnd: ", cdsEnd, " - expScores: ", nostrovalore, " - media: ", media, " - rapporto: ", rapporto, "\n\n", sep="")
    
    dati = paste(dati, testoFinale, sep="")
  }
  
  #chiamo la funzione per fare la scrittura dell'output su file
  scriviLog(log, elenco_input, output, time, dati = dati)
  
  dbDisconnect(mydb)
}

#creo una funzione che scriva su un file di testo i dati richiesti
scriviLog = function(log, elenco_input, output, time, dati = "") {
  
  #Un file di testo txt con nome costituito da: nome script ;data e ora dell'esecuzione dello script.  
  #Il file dovr? contente l'elenco degli input forniti, l'elenco  degli oggetti prodotti dalla funzione ed 
  #eventuali segnali di errore prodotti dalla funzione stessa
  
  
  
  #Nome script
  Script_Name = "ProgettoFinale"
  
  #Creo una'unica stringa contenente nome script, ora di accesso, oggetti prodotti e eventuali messaggi d'errore
  Data_2Write = paste(Script_Name, time, elenco_input, dati, log, sep="\n\n")
  
  #Creo un file di nome ProgettoFinale in cui trascrivo tutto
  write.table(Data_2Write, file = output, quote = FALSE, sep = "", na = "", row.names = FALSE, col.names = FALSE)
}
