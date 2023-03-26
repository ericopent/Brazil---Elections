library(tidyverse)
library(rjson)
library(magrittr)
library(jsonlite)
library(taskscheduleR)

# taskscheduler_create(taskname = "Eleicoes", rscript = "D:/Documentos/Documents/Eleicoes.R",
#                      schedule = "MINUTE", starttime = "19:20", modifier = 1)

# taskscheduleR::taskscheduler_delete("Eleicoes")

url = "https://resultados.tse.jus.br/oficial/ele2022/544/dados-simplificados/br/br-c0001-e000544-r.json"

pres_br = fromJSON(url, simplifyDataFrame = T)

pres_df = pres_br[["cand"]]

pres_df %<>%
  dplyr::select(nm, pvap)

colnames(pres_df) = c("Candidato", "% Votos")

pres_df$`% Votos` %<>% gsub(",", ".", .) %>% as.numeric()

pres_df %<>% arrange(`% Votos`)

pres_df %<>% dplyr::filter(`% Votos` >= 3)


pres_df$Candidato <- factor(pres_df$Candidato) %>%
  fct_reorder(pres_df$`% Votos`)


cols <- c("LULA"="#FF3030",
"JAIR BOLSONARO"="#1E90FF",
"CIRO GOMES"="#A2CD5A",
"SIMONE TEBET"="#FFB90F")



png("D:/Documentos/Documents/eleicoes.png", width=800, height=800)

print(pres_df %>% ggplot() +
  geom_col(aes(x = Candidato, y = `% Votos`, fill = Candidato))+
  scale_fill_manual(values = cols) +
  coord_cartesian(ylim = c(0, 100)) +
  geom_hline(yintercept = 50, col = "black", linetype = 'dashed', lwd = 1.2)+
  geom_text(aes(x = Candidato, y = `% Votos`, label = `% Votos`, hjust = 1.2), lwd = 4.5)+
  coord_flip()+
    labs(title = "Eleicoes",
         subtitle = paste0("Resultados parciais - Last update:", Sys.time() %>% format("%H:%M")),
          caption = "Erico Penteado e JUS-BR"))

dev.off()


library(gridExtra)
library(rvest)
library(rJava)
library(mailR)
library(htmlTable)
library(ggplot2)


