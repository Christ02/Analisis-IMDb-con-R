Análisis IMDb con R
================
Christian Barrios
2024-09-08

``` r
movies <- read.csv('movies.csv')
directors <- read.csv('directors.csv')
directors_genres <- read.csv('directors_genres.csv')
movies_directors <- read.csv('movies_directors.csv')
actors <- read.csv('actors.csv')
roles <- read.csv('roles.csv')
movies_genres <- read.csv('movies_genres.csv')
```

``` r
# 1.    Información general sobre la base de datos:

total_movies <- movies %>% distinct(id) %>% nrow()
total_directors <- directors %>% distinct(id) %>% nrow()

resumen <- tibble(
  Total_Peliculas = total_movies,
  Total_Directores = total_directors,
)

print(resumen)
```

    ## # A tibble: 1 × 2
    ##   Total_Peliculas Total_Directores
    ##             <int>            <int>
    ## 1          388269            86880

``` r
# 2.    ¿Cuál es el número promedio de géneros por director?
avg_genres_per_director <- directors_genres %>%
  group_by(director_id) %>%
  summarise(total_genres = n()) %>%
  summarise(avg_genres = mean(total_genres))

avg_genres_per_director
```

    ## # A tibble: 1 × 1
    ##   avg_genres
    ##        <dbl>
    ## 1       2.41

``` r
# 3.    Genere un nuevo reporte por “Role” con la siguiente información:

peliculas_por_rol <- roles %>%
  group_by(role) %>%
  summarise(num_peliculas = n_distinct(movie_id))

actores_por_rol <- roles %>%
  left_join(actors, by = c("actor_id" = "id")) %>%
  filter(gender == 'M') %>%  
  group_by(role) %>%
  summarise(num_actores = n_distinct(actor_id))

actrices_por_rol <- roles %>%
  left_join(actors, by = c("actor_id" = "id")) %>%
  filter(gender == 'F') %>%  
  group_by(role) %>%
  summarise(num_actrices = n_distinct(actor_id))

directores_por_rol <- roles %>%
  left_join(movies_directors %>% group_by(movie_id) %>% summarise(director_id = first(director_id)), by = "movie_id") %>%
  group_by(role) %>%
  summarise(num_directores = n_distinct(director_id))



reporte_por_rol <- peliculas_por_rol %>%
  left_join(actores_por_rol, by = "role") %>%
  left_join(actrices_por_rol, by = "role") %>%
  left_join(directores_por_rol, by = "role")

reporte_por_rol <- reporte_por_rol %>%
  mutate(
    num_actores = ifelse(is.na(num_actores), 0, num_actores),
    num_actrices = ifelse(is.na(num_actrices), 0, num_actrices),
    num_directores = ifelse(is.na(num_directores), 0, num_directores)
  )

print(reporte_por_rol)
```

    ## # A tibble: 1,174,675 × 5
    ##    role                    num_peliculas num_actores num_actrices num_directores
    ##    <chr>                           <int>       <dbl>        <dbl>          <int>
    ##  1 ""                             164782      189465       115354          37702
    ##  2 " (1985)"                           1           1            0              1
    ##  3 " (1991 reissue only)"              1           0            1              1
    ##  4 " (episode \"Protest u…             1           3            0              1
    ##  5 " (episode 4: The Crim…             1           0            1              1
    ##  6 " (episode Målbrott)"               1           3            0              1
    ##  7 " (episode one)"                    1           4            0              1
    ##  8 " (episode two)"                    1           2            0              1
    ##  9 " (segment \"A Boca\")"             1           2            0              1
    ## 10 " (segment \"A Suspeit…             1           1            0              1
    ## # ℹ 1,174,665 more rows

``` r
# 4.    Genere un nuevo reporte con la siguiente información:

peliculas_por_director <- movies_directors %>%
  group_by(director_id) %>%
  summarise(num_peliculas = n_distinct(movie_id))

actores_por_director <- movies_directors %>%
  left_join(roles, by = "movie_id") %>%
  group_by(director_id) %>%
  summarise(num_actores = n_distinct(actor_id))
```

    ## Warning in left_join(., roles, by = "movie_id"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 2 of `x` matches multiple rows in `y`.
    ## ℹ Row 119372 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
genero_comun_por_director <- movies_directors %>%
  left_join(movies_genres, by = "movie_id") %>%
  group_by(director_id, genre) %>%
  summarise(num_peliculas_genero = n()) %>%
  slice_max(num_peliculas_genero, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(director_id, genre)
```

    ## Warning in left_join(., movies_genres, by = "movie_id"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 7 of `x` matches multiple rows in `y`.
    ## ℹ Row 33555 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

    ## `summarise()` has grouped output by 'director_id'. You can override using the
    ## `.groups` argument.

``` r
reporte_directores <- directors %>%
  left_join(peliculas_por_director, by = c("id" = "director_id")) %>%
  left_join(actores_por_director, by = c("id" = "director_id")) %>%
  left_join(genero_comun_por_director, by = c("id" = "director_id"))

reporte_directores <- reporte_directores %>%
  mutate(
    num_peliculas = ifelse(is.na(num_peliculas), 0, num_peliculas),
    num_actores = ifelse(is.na(num_actores), 0, num_actores),
    genre = ifelse(is.na(genre), "", genre)
  )

print(reporte_directores%>% head(15))
```

    ##    id         first_name   last_name num_peliculas num_actores       genre
    ## 1   1               Todd           1             1           1            
    ## 2   2                Les 12 Poissons             1           2       Short
    ## 3   3            Lejaren    a'Hiller             2          15       Drama
    ## 4   4               Nian           A             1           1            
    ## 5   5           Khairiya   A-Mansour             1           1 Documentary
    ## 6   6            Ricardo    A. Solla             1           3       Drama
    ## 7   8 Kodanda Rami Reddy          A.            35          86      Action
    ## 8   9      Nageswara Rao          A.             1           1            
    ## 9  10               Yuri          A.             1           1      Comedy
    ## 10 11              Swamy      A.S.A.             1           2       Drama
    ## 11 12            Per (I)       Aabel             2          39      Comedy
    ## 12 13             Eivind       Aaeng             2          23            
    ## 13 14               Mang         Aag             1           1            
    ## 14 15            Sigfred     Aagaard             1          14            
    ## 15 16            Michael     Aaglund             1           1       Short

``` r
# 5.    Encuentre la distribución de “Roles” por las siguientes dimensiones:

dist_roles_pelicula <- roles %>%
  group_by(movie_id) %>%
  summarise(n_roles = n_distinct(role)) %>%
  group_by(n_roles) %>%
  summarise(n_movies = n()) %>%
  arrange(n_roles)

dist_roles_director <- movies_directors %>%
  left_join(roles, by = "movie_id") %>%
  group_by(director_id) %>%
  summarise(n_roles = n_distinct(role)) %>%
  group_by(n_roles) %>%
  summarise(n_directors = n()) %>%
  arrange(n_roles)
```

    ## Warning in left_join(., roles, by = "movie_id"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 2 of `x` matches multiple rows in `y`.
    ## ℹ Row 119372 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
dist_roles_combined <- full_join(dist_roles_pelicula, dist_roles_director, by = "n_roles") %>%
  arrange(n_roles)

dist_roles_combined <- dist_roles_combined %>%
  mutate(
    n_movies = ifelse(is.na(n_movies), 0, n_movies),
    n_directors = ifelse(is.na(n_directors), 0, n_directors)
  )

print(dist_roles_combined)
```

    ## # A tibble: 1,118 × 3
    ##    n_roles n_movies n_directors
    ##      <int>    <dbl>       <int>
    ##  1       1   112552       27248
    ##  2       2    26293        7687
    ##  3       3    15283        4499
    ##  4       4    11835        3316
    ##  5       5    11508        2892
    ##  6       6    10476        2420
    ##  7       7    10043        2006
    ##  8       8     9435        1789
    ##  9       9     8723        1722
    ## 10      10     8044        1676
    ## # ℹ 1,108 more rows
