library(tidyverse)
library(lubridate)

git_path = "/home/dfischer/Documents/anasac/" 

gits = list.dirs(git_path)
gits = gits[str_detect(gits,".git$")]
gits = gits[!str_detect(gits,"/home/pandora//\\.")]
gits = dirname(gits)

cabecera = data.frame()
for(g in gits){
  # g = gits[5]
  cab = system(paste("git -C",g,"log --date=format:'%Y-%m-%d %H:%M:%S' --pretty='%H\t%ad\t%al\t%s'"),intern = T)
  cab = do.call(rbind,str_split(cab,"\t")) %>% data.frame() %>% select(1:4)
  colnames(cab) = c("hash","date","user","comment")
  cab$date = ymd_hms(cab$date)
  cab$repo = basename(g)
  cabecera = bind_rows(cabecera,cab)
}

detalle = list
for(n_cab in 1:nrow(cabecera)){
  # n_cab = 1
  cab = cabecera[n_cab,]
  # ant = cabecera[n_cab + 1,]
  # print(cab)
  try({
    det = system(paste0("git -C ",git_path,cab$repo," show --numstat ",cab$hash),intern = T)
    det = det[-1:-6]
    det = do.call(rbind,str_split(det,"\t")) %>% data.frame() %>% select(1:3)
    # print(det)
    colnames(det) = c("added","removed","filename")
    det = det %>% mutate(
      repo = cab$repo,
      hash = cab$hash,
      added = as.numeric(added),
      removed = as.numeric(removed)
    )
    det$repo = cab$repo
    det$hash = cab$hash
    # detalle = bind_rows(detalle,det)
    detalle = append(detalle,det)
  })
}
detalle = bind_rows(detalle)

dataset = cabecera %>% inner_join(detalle, by =c("hash","repo"))
write_csv(dataset,"git.csv")



dataset = read.csv("git.csv")


dataset = dataset %>% group_by(hash) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

#lineas
dataset %>% 
  filter( !repo %in% c("backup_tabla_manual","sandobox_dfischer")) %>%
  summarise(added = sum(added,na.rm = T))

# por proj
dataset %>% 
  # filter(date>=as.Date("2022-01-01")) %>%
  group_by(repo) %>% 
  summarise(added = sum(added,na.rm = T),.groups = "drop") %>% 
  arrange(desc(added)) %>% 
  print(n=100) 

dataset %>% 
  filter(date>=as.Date("2022-01-01"), !repo %in% c("backup_tabla_manual","sandobox_dfischer")) %>%
  mutate(user = case_when(
    user == "danielfm123" ~ "dfischer",
    TRUE ~ user
  )) %>% 
  group_by(user,repo) %>% 
  summarise(added = sum(added,na.rm = T),.groups = "drop") %>% 
  arrange(desc(added)) %>% 
  spread(user,added) %>% 
  print(n=100) 


dataset %>% 
  filter(date>=as.Date("2022-01-01"), !repo %in% c("backup_tabla_manual","mantencion_atenea","script_configuracion","sandobox_dfischer"),
         user != "qlikview") %>%
  mutate(user = case_when(
    user == "danielfm123" ~ "dfischer",
    TRUE ~ user
  )) %>%
  group_by(user) %>% 
  summarise(added = sum(added,na.rm = T),.groups = "drop") %>% 
  mutate(added = 100*added/sum(added)) %>% 
  arrange(desc(added))

