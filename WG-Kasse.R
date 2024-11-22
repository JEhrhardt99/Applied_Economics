library(ggplot2)
library(ggthemes)
# ---------------------------
# WG-Kasse: Verwaltung und Visualisierung
# ---------------------------

# Pakete laden
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")

library(tidyverse)
library(lubridate)

# 1. Datenrahmen für WG-Kasse erstellen
wg_kasse <- tibble(
  Datum = as.Date(character()),
  Kategorie = character(),
  Beschreibung = character(),
  Betrag = numeric(),
  Typ = character() # "Inflow" oder "Outflow"
)

# 2. Funktion zum Hinzufügen von Transaktionen
add_transaction <- function(datum, kategorie, beschreibung, betrag, typ) {
  # Prüfen, ob `wg_kasse` existiert, andernfalls initialisieren
  if (!exists("wg_kasse", envir = globalenv())) {
    assign("wg_kasse", tibble(
      Datum = as.Date(character()),
      Kategorie = character(),
      Beschreibung = character(),
      Betrag = numeric(),
      Typ = character()
    ), envir = globalenv())
  }
  
  # Neue Transaktion hinzufügen
  new_transaction <- tibble(
    Datum = as.Date(datum),
    Kategorie = kategorie,
    Beschreibung = beschreibung,
    Betrag = betrag,
    Typ = typ
  )
  
  # Daten aktualisieren
  assign("wg_kasse", bind_rows(globalenv()$wg_kasse, new_transaction), envir = globalenv())
}

# 3. Beispiel-Transaktionen hinzufügen
add_transaction("2024-11-01", "Miete", "Mietzahlung", -500, "Outflow")
add_transaction("2024-11-02", "Einkauf", "Lebensmittel", -100, "Outflow")
add_transaction("2024-11-05", "Sonstiges", "Putzmittel", -20, "Outflow")
add_transaction("2024-11-10", "Einnahmen", "Mitbewohner-Einzahlung", 620, "Inflow")

# 4. Übersicht der WG-Kasse anzeigen
print("Aktueller Zustand der WG-Kasse:")
print(wg_kasse)

# 5. Visualisierung erstellen
# a. Zeitverlauf der WG-Kasse
wg_kasse <- wg_kasse %>%
  arrange(Datum) %>%
  mutate(Kontostand = cumsum(Betrag))

ggplot(wg_kasse, aes(x = Datum, y = Kontostand)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Zeitverlauf der WG-Kasse",
    x = "Datum",
    y = "Kassenstand (€)"
  ) +
  theme_stata()

# b. Inflows vs Outflows
inflows_outflows <- wg_kasse %>%
  group_by(Typ) %>%
  summarise(Gesamtbetrag = sum(Betrag))

ggplot(inflows_outflows, aes(x = Typ, y = Gesamtbetrag, fill = Typ)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("Inflow" = "green", "Outflow" = "red")) +
  labs(
    title = "Übersicht: Inflows vs Outflows",
    x = "Typ",
    y = "Gesamtbetrag (€)"
  ) +
  theme_minimal()

# 6. Daten speichern
# write_csv(wg_kasse, "wg_kasse.csv")
# print("Die WG-Kasse wurde in 'wg_kasse.csv' gespeichert.")

