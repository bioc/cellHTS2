## Creates the cellHTS object with the assembled data from 'KcViab' and 'KcViabSmall' experiment:


    fname <- c("KcViab", "KcViabSmall")
    for (f in fname){
      datadir <- paste("../", f, sep="")
      x <- readPlateList("Platelist.txt", name=f, path=datadir)
      x <- configure(x, confFile="Plateconf.txt", logFile="Screenlog.txt", descripFile="Description.txt", path=datadir)
      if (f==fname[1]){
        KcViab <- annotate(x, "GeneIDs_Dm_HFA_1.1.txt", path=datadir)
        save(KcViab, file=sprintf("../../data/%s.rda", f), compress=TRUE)
      }else{
        KcViabSmall <- annotate(x, "GeneIDs_Dm_HFAsubset_1.1.txt", path=datadir)
        save(KcViabSmall, file=sprintf("../../data/%s.rda", f), compress=TRUE)
      }
    }