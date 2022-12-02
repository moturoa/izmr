
pm_decrypt <- function(x, decrypt_secret = getOption("pm_decrypt_secret")){
  
  vapply(x, safer::decrypt_string, key = decrypt_secret,
         USE.NAMES = FALSE, FUN.VALUE = character(1)
  )
}
