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

recode_vars <- function(char, var){
  if(var == "genres"){
    val <- c("rock" 			= "Rock", 
             "raphiphop" 	= "Rap & Hip-Hop", 
             "frenchsongs" 	= "Chanson", 
             "reggae" 		= "Reggae", 
             "soulfunk" 		= "Soul & Funk", 
             "edm" 			= "EDM", 
             "pop" 			= "Pop", 
             "jazz" 			= "Jazz", 
             "frenchrap" 	= "French Rap", 
             "latino" 		= "Latino", 
             "rnb" 			= "R&B", 
             "metal" 		= "Metal", 
             "classical" 	= "Classical", 
             "african" 		= "African", 
             "country" 		= "Country", 
             "alternative" 	= "Alternative", 
             "folk" 			= "Folk", 
             "blues" 		= "Blues")
  } else if (var == "legitimacy"){
    val = c("sc_endo_isei" = "Endogenous legitimacy\n(ISEI)",
            "sc_endo_educ" = "Endogenous legitimacy\n(education)",
            "sc_exo_press" = "Exogenous legitimacy\n(media)",
            "sc_exo_score" = "Exogenous legitimacy\n(score)",
            "sc_exo_radio" = "Exogenous legitimacy\n(radio)",
            "sc_exo_pca"   = "Exogenous legitimacy\n(PCA)",
            "endo_isei_mean_pond" = "Endogenous legitimacy\n(ISEI, unscaled)", 
            "endo_share_high_education_pond" = "Endogenous legitimacy\n(education, unscaled)",
            "total_n_pqnt_texte" ="Exogenous legitimacy\n(media, unscaled)", 
            "senscritique_meanscore" = "Exogenous legitimacy\n(score, unscaled)", 
            "radio_leg" =  "Exogenous legitimacy\n(radio, unscaled)",
            "omni_survey_sum_genres_played" = "Richness played", 
            "omni_survey_sum_genres_liked" = "Richness liked",
            "omni_stream_genres_hhi" = "HHI genres streamed",
            "mean_exo_pca" = "Mean exo. leg.",
            "sd_exo_pca" = "SD exo. leg.",
            "omni_survey_cultural_holes_played" = "Cultural holes played",
            "omni_survey_cultural_holes_liked" = "Cultural holes liked")
  } else {
    stop("Type of variable unspecified or unknown")
  }
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