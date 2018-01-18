#Componenti Gruppo: Giorgio Bancheri, Adriano Crasnich, Gianmaria D'Ambrosi, Simone Rodaro, Marco Tugnizza e Davide Spanghero

esame = function(file, gruppo = 'Gruppo', parametro = 1, output = 'Risultato.txt'){
  
  library(tibble)
  library(RMySQL)
  
  #Var init
  
  gtexPath = ""
  row = 1
  
  #End var init
  
  #connessione al db di ucsc
  mydb <- dbConnect(MySQL(), user='genome', host='genome-mysql.soe.ucsc.edu')
  mydb
  
  #controllo che il parametro inserito non sia un numero superiore a 53 o inferiore a 1
  if(parametro > 53 & parametro <1) {parametro = 1}
  
  #controllo la lunghezza della variabile file per vedere se ? un vettore oppure una normale stringa
  if(length(file)=1){
    
    #controllo che sia un file .txt
    if(grepl(".txt$", file)){
      
      #controllo l'esistenza del file
      if(file.exists(file)){
        
        #in caso affermativo salvo iil contenuto del file in un vettore
        testo = scan(file, what = character(), sep = "\n")
        row = nrow(testo)
        
      } else {
        log = "File non trovato"
        scriviLog(log, output)
        return
      }
      
    } else {
      testo = file;                       #vettore con una stringa
    }
  } else {
    testo = file;                         #vettore di pi? stringhe
  }
  
  #ciclo le righe del file o del vettore, in caso il vettore sia composto da un solo elemento la variabile row inizializzata a 1 non viene modificata quindi
  #sostanzialmente ciclo su una riga sola, questo per non scrivere più volte lo stesso codice
  for(i in 1:row) {
    n = text[i]
    
    #estraggo name e name2 dal database
    ris = dbSendQuery(mydb, "use hg38")
    dbFetch(ris)
    dbClearResult(ris)
    
    select = paste("SELECT name FROM wgEncodeGencodeBasicV27 where name = '",n,"' or name2 = '",n,"'",sep="")
    ris = dbSendQuery(mydb, select)
    name = dbFetch(ris)
    dbClearResult(ris)
    
    select = paste("SELECT name FROM wgEncodeGencodeBasicV27 where name = '",n,"' or name2 = '",n,"'",sep="")
    ris = dbSendQuery(mydb, select)
    name2 = dbFetch(ris)
    dbClearResult(ris)
    
    select = paste("SELECT chrom FROM wgEncodeGencodeBasicV27 where name = '",name,sep="")
    ris = dbSendQuery(mydb, select)
    chrom = dbFetch(ris)
    dbClearResult(ris)
    
    select = paste("SELECT strand FROM wgEncodeGencodeBasicV27 where name = '",name,sep="")
    ris = dbSendQuery(mydb, select)
    strand = dbFetch(ris)
    dbClearResult(ris)
    
    select = paste("SELECT txStart FROM wgEncodeGencodeBasicV27 where name = '",name,sep="")
    ris = dbSendQuery(mydb, select)
    txStart = dbFetch(ris)
    dbClearResult(ris)
    
    select = paste("SELECT txEnd FROM wgEncodeGencodeBasicV27 where name = '",name,sep="")
    ris = dbSendQuery(mydb, select)
    txEnd = dbFetch(ris)
    dbClearResult(ris)
    
    select = paste("SELECT cdsStart FROM wgEncodeGencodeBasicV27 where name = '",name,sep="")
    ris = dbSendQuery(mydb, select)
    cdsStart = dbFetch(ris)
    dbClearResult(ris)
    
    select = paste("SELECT cdsEnd FROM wgEncodeGencodeBasicV27 where name = '",name,sep="")
    ris = dbSendQuery(mydb, select)
    cdsEnd = dbFetch(ris)
    dbClearResult(ris)
    
    fileEsterno = read.table('gtexTranscExpr', header = FALSE, sep='\t')
    fileEsterno = as.matrix(fileEsterno)
    rowFE = nrow(fileEsterno)
    nostrovalore = ""
    totValori = ""
    
    for(i in 1:roeFE){
      if(fileEsterno[i, 4] = name & fileEsterno[i, 1] = chrom){
        serie = fileEsterno[i, 9]
        singoli = strsplit(serie, ",")
        nostroValore = singoli[parametro]
      }
      serie = fileEsterno[i, 9]
      singoli = strsplit(serie, ",")
      totValori = totValori + singoli[parametro]
    }
    
    media = totValori/53
    
    rapporto = nostrovalore/media
  }
  
  
  
}

#creo una funzione che scriva su un file di testo i dati richiesti
scriviLog = function(log, output, dati = "") {
  
  #Un file di testo txt con nome costituito da: nome script ;data e ora dell'esecuzione dello script.  
  #Il file dovr? contente l'elenco degli input forniti, l'elenco  degli oggetti prodotti dalla funzione ed 
  #eventuali segnali di errore prodotti dalla funzione stessa
  
  #Ora di sistema
  time = Sys.time()
  
  #Nome script
  Script_Name = "ProgettoFinale"
  
  #elenco input
  elenco_input = paste("Nome file: ", file, " gruppo: ",gruppo, " parametro: ", parametro, "nome file di output: ",output)
  
  #Creo una'unica stringa contenente nome script, ora di accesso, oggetti prodotti e eventuali messaggi d'errore
  Data_2Write = paste(Script_Name, time, elenco_input, dati, log, sep="\n")
  
  #Creo un file di nome ProgettoFinale in cui trascrivo tutto
  write.table(Data_2Write, file = output)
}
