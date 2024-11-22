# XML ---------------------------------------------------------------------
df <- "PrixCarburants_annuel_2023.xml" %>% 
  XML::xmlParse() %>% 
  XML::xmlToList()


# Coordinates and stuff
locs <- lapply(df, function(i) {
  i[[".attrs"]] %>% 
    as.list() %>% 
    as.data.frame()
}) %>% 
  dplyr::bind_rows()

# les prix:
t0 <- Sys.time()
prix <- lapply(df, function(i) {
  elements <- i[names(i) %in% "prix"]
  lapply(elements, function(e) {
    if (!is.null(e)) {
      e %>%
        t() %>%
        as.data.frame()
    } else{
      NULL
    }
  }) |> 
    dplyr::bind_rows()
}) |> 
  dplyr::bind_rows()
t1 <- Sys.time()
print(t1 - t0)


write_csv("df.csv.gz")