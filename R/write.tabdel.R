## wrapper to write.table
write.tabdel = function(...)
    write.table(..., sep = "\t", row.names = FALSE, col.names = TRUE,
             quote = FALSE)

