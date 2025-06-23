export_to_csv <- function(object){
  object_char <- deparse(substitute(object))
  file <- str_glue("data/temp/{object_char}.csv")
  write_csv(object, file)
  return(file)
}
