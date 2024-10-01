install.packages("tidyverse")
library(tidyverse)

# Definir o diretório onde os arquivos .csv estão localizados
diretorio <- "C:/Temp/Dados"

# Listar todos os arquivos .csv no diretório
arquivos_csv <- list.files(diretorio, pattern = "\\.csv$", full.names = TRUE)

# Ler e combinar todos os arquivos .csv em um único DataFrame
trips <- lapply(arquivos_csv, ler_arquivo_csv)%>%
  bind_rows() 

# Visualizar os primeiros registros
head(trips)

# Contar valores nulos (NA) em cada coluna
colSums(is.na(trips))

# Contar valores não nulos em cada coluna
colSums(!is.na(trips))

## Temos então 5.699.639 viagens de bicicleta

# Eliminar as colunas de latitude e longitude, que não serão utilizadas
trips <- trips %>% 
  select(-start_lat, -start_lng, -end_lat, -end_lng)

# Verificar se há algum registro com data de inicío da viagem fora do período indicado
trips_ordered <- arrange(trips, started_at)
head(trips_ordered)
trips_ordered <- arrange(trips, desc(started_at))
head(trips_ordered)

# Calcular a diferença entre 'ended_at' e 'started_at' e armazenar em uma nova coluna 'trip_duration'
trips$trip_duration <- difftime(trips$ended_at, trips$started_at, units = "secs")

# Converter a diferença de tempo para minutos da viagem (desprezando os segundos)
trips$trip_duration_minutes <- floor(as.numeric(trips$trip_duration) / 60)  # Minutos

# Criar uma nova coluna 'trip_duration_seconds' contendo os segundos totais da viagem
trips$trip_duration_seconds <- as.numeric(trips$trip_duration)

# Visualizar as primeiras linhas do dataframe com as colunas modificadas
head(trips[, c("started_at", "ended_at", "trip_duration_minutes", "trip_duration_seconds")])

# Criar a coluna 'day_of_week' com o nome do dia da semana
trips$day_of_week <- weekdays(trips$started_at, abbreviate = FALSE)

# Obter uma visão geral dos dados
summary(trips)
str(trips)

# Verificar se há registros cuja data/hora do início da viagem é posterior à data/hora do fim da viagem (inconsistências nos dados)
trips_fim_menor_inicio <- filter(trips, 
                           (trip_duration_seconds <= 0))

head(trips_fim_menor_inicio[, c("ride_id", "started_at", "ended_at", "trip_duration_minutes", "trip_duration_seconds")])

# Remover do data frame `trip` os registros cuja data fim da viagem é menor do que a data de início
trips <- anti_join(trips, trips_fim_menor_inicio, by = "ride_id")

str(trips)

## Temos agora 5.698.376 viagens de bicicleta

# Gerar um gráfico de pizza para verificar as quantidades e percentuais de bicicletas normais e elétricas

# Agrupar por tipo de bicicleta e calcular os totais e percentuais
bike_data <- trips %>%
  group_by(rideable_type) %>%
  summarise(total_trips = n()) %>%
  mutate(percent = total_trips / sum(total_trips) * 100)

# Criar o gráfico de pizza 
ggplot(bike_data, aes(x = "", y = total_trips, fill = rideable_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(total_trips, " (", round(percent, 1), "%)")),
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Tipo de Bicicleta", title = "Quantidade de Viagens por Tipo de Bicicleta") +
  theme_void()  # Remove os eixos para manter o foco no gráfico de pizza

# Exportar o gráfico para um arquivo PNG
ggsave("qt_viagens_por_tipo_bicicleta.png")

# Tentar preencher o nome da estação que esteja vazio (nulo) nos campos de origem e destino

# Gerar dataframe auxiliar com station_id e station_name únicos
station_names <- trips %>%
  filter(!is.na(start_station_name)) %>%
  select(start_station_id, start_station_name) %>%
  distinct()

# Verificar se há múltiplos nomes para o mesmo station_id
inconsistencias <- station_names %>%
  group_by(start_station_id) %>%
  filter(n_distinct(start_station_name) > 1)

print(inconsistencias)

# Resolver inconsistências usando o nome mais frequente
station_names_resolvido <- station_names %>%
  group_by(start_station_id) %>%
  summarise(start_station_name = names(which.max(table(start_station_name))))

# Atualizar o dataframe original com os nomes resolvidos
trips <- trips %>%
  left_join(station_names_resolvido, by = "start_station_id", suffix = c("", ".filled")) %>%
  mutate(start_station_name = ifelse(is.na(start_station_name), start_station_name.filled, start_station_name)) %>%
  select(-start_station_name.filled)

trips <- trips %>%
  left_join(station_names_resolvido, by = c("end_station_id" = "start_station_id"), suffix = c("", ".end_filled")) %>%
  mutate(end_station_name = ifelse(is.na(end_station_name), start_station_name.end_filled, end_station_name)) %>%
  select(-start_station_name.end_filled)

## Ainda restaram 968.128 viagens sem start_station e 1.005.407 sem end_station

# Gerar insights para análise

# Criar o gráfico de barras Total de viagens por tipo de usuário e tipo de bicicleta

# Gerar o gráfico
ggplot(trips, aes(x = member_casual, fill = rideable_type)) +
  geom_bar(position = "dodge") +
  labs(title = "Total de Viagens por Tipo de Usuário e Tipo de Bicicleta",
       x = "Tipo de Usuário",
       y = "Total de Viagens",
       fill = "Tipo de Bicicleta") +
  theme_minimal()
# Exportar imagem do gráfico
ggsave("qt_viagens_por_tipo_de_usuario_e_tipo_bicicleta.png")

# Calcular a quantidade e o percentual de viagens por tipo de usuário
viagens_por_usuario <- trips %>%
  group_by(member_casual) %>%
  summarise(total_viagens = n()) %>%
  mutate(percentual = total_viagens / sum(total_viagens) * 100)

print (viagens_por_usuario)

# Criar o gráfico de barras
ggplot(viagens_por_usuario, aes(x = member_casual, y = total_viagens, fill = member_casual)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentual, 1), "%")), vjust = -0.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Quantidade e Percentual de Viagens por Tipo de Usuário",
       x = "Tipo de Usuário",
       y = "Total de Viagens") +
  theme_minimal()

ggsave("qt_viagens_por_tipo_de_usuario.png")

# Calcular o tempo médio de viagem por tipo de usuário
tempo_medio_viagem <- trips %>%
  group_by(member_casual) %>%
  summarise(tempo_medio = mean(trip_duration_minutes, na.rm = TRUE))

print(tempo_medio_viagem)

# Criar o gráfico de barras
ggplot(tempo_medio_viagem, aes(x = member_casual, y = tempo_medio, fill = member_casual)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(tempo_medio, 1)), vjust = -0.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Tempo Médio de Viagem por Tipo de Usuário",
       x = "Tipo de Usuário",
       y = "Tempo Médio de Viagem (minutos)") +
  theme_minimal()

ggsave("tempo_médio_viagem_por_tipo_de_usuario.png")

# Calcular o tempo médio de viagem por tipo de usuário e tipo de bicicleta
tempo_medio_viagem1 <- trips %>%
  group_by(member_casual, rideable_type) %>%
  summarise(tempo_medio = mean(trip_duration_minutes, na.rm = TRUE))

print(tempo_medio_viagem)

# Criar o gráfico de barras
ggplot(tempo_medio_viagem1, aes(x = member_casual, y = tempo_medio, fill = rideable_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(tempo_medio, 1)), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Tempo Médio de Viagem por Tipo de Usuário e Tipo de Bicicleta",
       x = "Tipo de Usuário",
       y = "Tempo Médio de Viagem (minutos)",
       fill = "Tipo de Bicicleta") +
  theme_minimal()

ggsave("tempo_médio_viagem_por_tipo_de_usuario e tipo de bicicleta.png")

# Adicionar uma coluna de ano_mês ao dataframe
trips <- trips %>%
  mutate(month = as.Date(paste0(format(as.Date(started_at), "%Y-%m"), "-01")))

# Calcular o tempo médio de viagem por tipo de usuário, tipo de bicicleta e mês
tempo_medio_viagem2 <- trips %>%
  group_by(month, member_casual, rideable_type) %>%
  summarise(tempo_medio = mean(trip_duration_minutes, na.rm = TRUE))

print(tempo_medio_viagem2)

# Criar o gráfico de barras com facetas por tipo de usuário
ggplot(tempo_medio_viagem2, aes(x = month, y = tempo_medio, fill = rideable_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ member_casual) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", limits = as.Date(c("2023-09-01", "2024-08-31"))) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Tempo Médio de Viagem por Tipo de Usuário, Tipo de Bicicleta e Mês",
       x = "Mês",
       y = "Tempo Médio de Viagem (minutos)",
       fill = "Tipo de Bicicleta") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),  # Remove as linhas de grade principais
        panel.grid.minor = element_blank())  # Remove as linhas de grade menores

ggsave("tempo_médio_viagem_por_tipo_de_usuario_tipo_de_bicicleta_mes.png")

# Adicionar uma coluna de dia da semana ao dataframe e criar um fator ordenado
trips <- trips %>%
  mutate(day_of_week = factor(weekdays(as.Date(started_at)), 
                              levels = c("segunda-feira", "terça-feira", "quarta-feira", 
                                         "quinta-feira", "sexta-feira", "sábado", "domingo")))


# Calcular o tempo médio de viagem por tipo de usuário, tipo de bicicleta e dia da semana
tempo_medio_viagem3 <- trips %>%
  group_by(day_of_week, member_casual, rideable_type) %>%
  summarise(tempo_medio = mean(trip_duration_minutes, na.rm = TRUE))

print(tempo_medio_viagem3, n = 28)

# Criar o gráfico de barras com facetas por tipo de usuário, tipo de bicicleta e dia da semana 
ggplot(tempo_medio_viagem3, aes(x = day_of_week, y = tempo_medio, fill = rideable_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ member_casual) +
  scale_fill_manual(values = c("steelblue", "darkorange", "forestgreen")) +  
  labs(title = "Tempo Médio de Viagem por Tipo de Usuário, Tipo de Bicicleta e Dia da Semana",
       x = "Dia da Semana",
       y = "Tempo Médio de Viagem (minutos)",
       fill = "Tipo de Bicicleta") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
	  axis.text.y = element_text(size = 14),  # Aumentar tamanho da fonte dos rótulos do eixo Y
        axis.title = element_text(size = 16),  # Aumentar tamanho da fonte dos títulos dos eixos
        strip.text = element_text(size = 16),  # Aumentar tamanho da fonte dos títulos das facetas
        legend.title = element_text(size = 14),  # Aumentar tamanho da fonte do título da legenda
        legend.text = element_text(size = 14),  # Aumentar tamanho da fonte dos textos da legenda
        plot.title = element_text(size = 16, face = "bold"),  # Aumentar tamanho da fonte do título do gráfico
        panel.grid.major = element_blank(),  # Remove as linhas de grade principais
        panel.grid.minor = element_blank())  # Remove as linhas de grade menores

ggsave("tempo_médio_viagem_por_tipo_de_usuario_tipo_de_bicicleta_dia_semana.png")


# Adicionar uma coluna de intervalo de 3 horas ao dataframe
trips <- trips %>%
  mutate(hour_interval = cut(as.numeric(format(as.POSIXct(started_at), "%H")), 
                             breaks = seq(0, 24, by = 3), 
                             labels = c("00-03", "03-06", "06-09", "09-12", "12-15", "15-18", "18-21", "21-24"), 
                             include.lowest = TRUE))

# Calcular o tempo médio de viagem por tipo de usuário, tipo de bicicleta e intervalo de 3 horas
tempo_medio_viagem4 <- trips %>%
  group_by(hour_interval, member_casual, rideable_type) %>%
  summarise(tempo_medio = mean(trip_duration_minutes, na.rm = TRUE))

print(tempo_medio_viagem4)

# Criar o gráfico de barras com facetas por tipo de usuário, tipo de bicicleta e intervalo de horários
ggplot(tempo_medio_viagem4, aes(x = hour_interval, y = tempo_medio, fill = rideable_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ member_casual) +
  scale_fill_manual(values = c("purple", "cyan", "magenta")) +  # Cores diferentes
  labs(title = "Tempo Médio de Viagem por Tipo de Usuário, Tipo de Bicicleta e Intervalo de Horário",
       x = "Intervalo de Horário",
       y = "Tempo Médio de Viagem (minutos)",
       fill = "Tipo de Bicicleta") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),  # Remove as linhas de grade principais
        panel.grid.minor = element_blank())  # Remove as linhas de grade menores

ggsave("tempo_médio_viagem_por_tipo_de_usuario_tipo_de_bicicleta_horario.png")


# Calcular a média de viagens por mês, para grupo de usuários e tipo de bicicleta
media_qt_viagens_mes <- quantidade_viagens_mes %>% 
  group_by(member_casual, rideable_type) %>%  
  summarize(media_qt = mean(quantidade, na.rm = TRUE) 
  )
head(media_qt_viagens_mes)

# Calcular a quantidade de viagens por tipo de usuário, tipo de bicicleta e mês
quantidade_viagens_mes <- trips %>%
  group_by(month, member_casual, rideable_type) %>%
  summarise(quantidade = n().groups = 'drop')

print(quantidade_viagens_mes, n=50)

# Criar o gráfico de barras com facetas por tipo de usuário
ggplot(quantidade_viagens_mes, aes(x = month, y = quantidade, fill = rideable_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ member_casual) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", limits = as.Date(c("2023-09-01", "2024-08-31"))) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Quantidade de Viagens por Tipo de Usuário, Tipo de Bicicleta e Mês",
       x = "Mês",
       y = "Quantidade de Viagens",
       fill = "Tipo de Bicicleta") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("qt_viagens_por_tipo_de_usuario_tipo_de_bicicleta_mes.png")


# Calcular a quantidade de viagens por tipo de usuário, tipo de bicicleta e dia da semana
quantidade_viagens_dia <- trips %>%
  group_by(day_of_week, member_casual, rideable_type) %>%
  summarise(quantidade = n())

print(quantidade_viagens_dia)

# Criar o gráfico de barras com facetas por tipo de usuário e cores diferentes
ggplot(quantidade_viagens_dia, aes(x = day_of_week, y = quantidade, fill = rideable_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ member_casual) +
  scale_fill_manual(values = c("steelblue", "darkorange", "forestgreen")) +  # Cores diferentes
  labs(title = "Quantidade de Viagens por Tipo de Usuário, Tipo de Bicicleta e Dia da Semana",
       x = "Dia da Semana",
       y = "Quantidade de Viagens",
       fill = "Tipo de Bicicleta") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("qt_viagens_por_tipo_de_usuario_tipo_de_bicicleta_dia.png")


# Calcular o tempo médio de viagem por tipo de usuário, dia da semana e intervalo de 3 horas
quantidade_viagens_dia_horario <- trips %>%
  group_by(day_of_week, hour_interval, member_casual) %>%
  summarise(quantidade = n(), .groups = 'drop')

# Criar o gráfico de barras com facetas por intervalo de horas e colunas por tipo de usuário
ggplot(quantidade_viagens_dia_horario, aes(x = hour_interval, y = quantidade, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~day_of_week) +
  scale_fill_manual(values = c("darkorange", "steelblue", "forestgreen")) +  # Cores diferentes
  labs(title = "Quantidade de Viagens por Tipo de Usuário, Dia da Semana e Intervalo de Horário",
       x = "Intervalo de Horário",
       y = "Quantidade de Viagens (minutos)",
       fill = "Tipo de Usuário") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("qt_viagens_por_tipo_de_usuario_dia_semana_e_horario.png")

# Identificar as top 10 pares de estações de origem e de destino

# Filtrar dados para remover NAs nas colunas de estações
trips2 <- trips %>%
  filter(!is.na(start_station_name) & !is.na(end_station_name))

# Calcular a quantidade de viagens por trajeto
trajetos <- trips2 %>%
  group_by(start_station_name, end_station_name, member_casual) %>%
  summarise(quantidade = n(), .groups = 'drop')

# Calcular o total de viagens por grupo
total_viagens <- trips2 %>%
  group_by(member_casual) %>%
  summarise(total = n(), .groups = 'drop')

# Juntar os dados e calcular o percentual
trajetos <- trajetos %>%
  left_join(total_viagens, by = "member_casual") %>%
  mutate(percentual = (quantidade / total) * 100)

# Selecionar os top 10 trajetos mais frequentes para cada grupo
top_trajetos <- trajetos %>%
  group_by(member_casual) %>%
  slice_max(order_by = quantidade, n = 10) %>%
  ungroup() %>%
  arrange(member_casual, desc(quantidade))

print(top_trajetos)

View(top_trajetos)



