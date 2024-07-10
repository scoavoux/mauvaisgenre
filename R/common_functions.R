# Common functions ------
initialize_s3 <- function(){
  require("paws")
  s3 <- paws::s3(config = list(
    credentials = list(
      creds = list(
        access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
        secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
        session_token = Sys.getenv("AWS_SESSION_TOKEN")
      )),
    endpoint = paste0("https://", Sys.getenv("AWS_S3_ENDPOINT")),
    region = Sys.getenv("AWS_DEFAULT_REGION")))
  
  return(s3)
}

set_ggplot_options <- function(){
  theme_set(theme_bw(base_size = 12))
  theme_update(panel.grid=element_blank())
}

recode_vars <- function(char, .scheme){
  e <- readr::read_csv(here::here("data", "codes.csv"), col_types = "ccc")
  e <- dplyr::filter(e, scheme == .scheme)
  val <- e$replacement
  names(val) <- e$orig
  return(val[char])
}

center_scale <- function(x){
  if(!is.numeric(x)){
    stop("Variable not numeric")
  } else {
    z <- (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)
  }
  return(z)
}

compute_pca <- function(df){
  require(FactoMineR)
  # Compute PCA on exo variables.
  # Might be used in several places so we make it a function.
  pca_mod <- select(df, sc_exo_press, sc_exo_radio, sc_exo_score) %>% 
    PCA(graph=FALSE)
 return(pca_mod)
}