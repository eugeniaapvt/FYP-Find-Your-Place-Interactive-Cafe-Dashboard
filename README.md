# FYP – Find Your Place

FYP (Find Your Place) adalah interactive R Shiny dashboard untuk membantu pengguna mengeksplorasi cafe di Surakarta berdasarkan lokasi, rating, harga, jam operasional, dan fasilitas.

## Features

### Interactive Cafe Map
Menampilkan lokasi cafe di Surakarta menggunakan interactive map.

### Cafe Search & Filter
Pengguna dapat mencari dan memfilter cafe berdasarkan:

- Nama cafe
- Rating
- Rentang harga
- Jam operasional
- Layanan online

### Statistics & Visualization
Dashboard menyediakan visualisasi mengenai:

- Distribusi rating cafe
- Status jam operasional
- Fasilitas cafe
- Opsi layanan delivery

### Cafe Recommendation
Menampilkan Top 3 cafe berdasarkan kombinasi rating dan jumlah ulasan.

### Cafe Details
Pengguna dapat melihat informasi detail cafe seperti:

- Rating
- Jumlah ulasan
- Alamat
- Rentang harga
- Jam operasional
- Fasilitas dan layanan

### Coffee Playlist
Menyediakan rekomendasi playlist Spotify berdasarkan mood pengguna.

## Data

Data mencakup informasi cafe di wilayah Surakarta, meliputi:

- Lokasi
- Rating
- Jumlah ulasan
- Rentang harga
- Jam operasional
- Fasilitas
- Layanan

Sumber data: Google Maps – data cafe wilayah Surakarta.

## Tools

- R
- Shiny
- shinydashboard
- Leaflet
- Plotly
- ggplot2
- dplyr
- DT
- shinyjs
- shinyWidgets

## How to Run

Install packages yang diperlukan:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "leaflet",
  "leaflet.extras",
  "DT",
  "dplyr",
  "ggplot2",
  "plotly",
  "shinyjs",
  "shinyWidgets",
  "htmltools"
))
```

Kemudian jalankan aplikasi:

```r
shiny::runApp()
```

## Results

Dashboard menghasilkan aplikasi interaktif yang memungkinkan pengguna mengeksplorasi, memfilter, membandingkan, dan memperoleh rekomendasi cafe di Surakarta.
