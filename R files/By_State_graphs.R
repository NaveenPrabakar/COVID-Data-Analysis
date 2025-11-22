#Use Cleaned Dataset
hospital <- read_csv("Data/cleaned_Hospital_Coverage.csv")

hospital |>
  group_by(State) |>
  summarize(
    total_covid = sum(
      total_adult_patients_hospitalized_confirmed_covid +
        total_pediatric_patients_hospitalized_confirmed_covid,
      na.rm = TRUE
    )
  ) |>
  ggplot(aes(x = reorder(State, total_covid), y = total_covid)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Total Current COVID Hospitalizations by State",
    x = "State",
    y = "Total Hospitalized COVID Patients"
  ) +
  theme_minimal()
