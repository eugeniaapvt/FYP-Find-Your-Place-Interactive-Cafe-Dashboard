# ============================================================
# FYP - FIND YOUR PLACE
# ============================================================

library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyjs)
library(shinyWidgets)
library(htmltools)

# ------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------
cafe <- read.csv("cafe_data_fix.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
cafe$id <- seq_len(nrow(cafe))

# ------------------------------------------------------------
# 2. COLOR PALETTE
# ------------------------------------------------------------
pal <- list(
  dusty_cocoa   = "#8A6C5F",
  warm_sand     = "#BFA38A",
  soft_oatmeal  = "#E0D9CF",
  deep_espresso = "#544339",
  golden_glow   = "#F5E9D3"
)

# ------------------------------------------------------------
# 2c. DATA COFFEE PLAYLIST (Spotify Embed - fitur baru)
# ------------------------------------------------------------
playlist_data <- list(
  "Study" = list(
    emoji = "\U0001F4DA",
    desc  = "Fokus & belajar",
    ids   = c("0AaYqDidguRP0hiUQqNPph", "0E4w6HfH6nbNoO70nLBe2X", "0ofsIXBCxkqYtt30YoJm1c"),
    names = c("Exam Week", "Teman Nugas", "Deep Focus")
  ),
  "Outdoor" = list(
    emoji = "\U0001F33F",
    desc  = "Santai di luar ruangan",
    ids   = c("7KDuTgQXnQPitC5ner7Ti8", "1chw8USLxI27ZQbWX6Xfwe"),
    names = c("Nongki FM", "Ngopi Sore")
  ),
  "Date" = list(
    emoji = "\U0001F495",
    desc  = "Ngobrol berdua",
    ids   = c("399X774S8n31XNofM6dRSj", "4BvMvsj2thwDvDyAkLNi09"),
    names = c("Indonesian Love Song", "RomCom Vibes")
  ),
  "Night" = list(
    emoji = "\U0001F303",
    desc  = "Nemenin malam",
    ids   = c("5BMh1H22LsB00yVOge6Us7", "2wd9FPPVCFuBgdgdnhnyti"),
    names = c("RnB Late Night Vibes", "Night Drive")
  ),
  "Aesthetic" = list(
    emoji = "\U0001F4F8",
    desc  = "Vibes estetik",
    ids   = c("370W0DReOdO2lfkf4OXeMs", "5QFXQLLBiSFe3uwvnnQn6q"),
    names = c("Aesthetic Vibes", "Soft Piano")
  ),
  "Rainy" = list(
    emoji = "\U0001F327",
    desc  = "Temani hujan",
    ids   = c("5x1KYjQjwMh4eZKGBrQgCo", "5QrWYCeUBedevP3CI6JNeR"),
    names = c("Rainy Day", "Gloomy Rain")
  )
)

# ------------------------------------------------------------
# 3. HELPER FUNCTIONS
# ------------------------------------------------------------
format_rp <- function(x) {
  paste0("Rp ", formatC(x, format = "d", big.mark = "."))
}

status_badge <- function(status) {
  bg <- switch(status,
               "Buka"       = "#7C9473",
               "Buka 24 Jam" = "#8A6C5F",
               "Tutup"      = "#B5524A",
               "#999999"
  )
  sprintf(
    '<span style="background:%s;color:#fff;padding:3px 10px;border-radius:12px;font-size:12px;font-weight:600;">%s</span>',
    bg, status
  )
}

make_popup <- function(row) {
  HTML(sprintf('
    <div style="font-family:Poppins,sans-serif;width:220px;">
      <img src="%s" style="width:100%%;height:110px;object-fit:cover;border-radius:8px 8px 0 0;"
           onerror="this.style.display=\'none\'">
      <div style="padding:8px 4px;">
        <div style="font-weight:700;font-size:14px;color:#544339;margin-bottom:3px;">%s</div>
        <div style="font-size:12px;color:#666;margin-bottom:4px;">%s</div>
        <div style="font-size:12px;margin-bottom:3px;">
          <span style="color:#D4A017;">&#9733;</span> %s &nbsp;|&nbsp; %s ulasan
        </div>
        <div style="font-size:12px;margin-bottom:4px;">%s</div>
        <div style="font-size:11px;color:#888;">%s</div>
      </div>
    </div>',
               row$URL_Foto, row$Nama_Cafe, row$Kategori,
               row$Rating, format(row$Jumlah_Ulasan, big.mark = "."),
               row$Rentang_Harga, status_badge(row$Status_Buka)
  ))
}

# ------------------------------------------------------------
# 4. CUSTOM CSS
# ------------------------------------------------------------
app_css <- "
@import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700;800&family=Playfair+Display:wght@600;700&family=Cormorant+Garamond:wght@300;400;600&display=swap');

body, .content-wrapper {
  font-family: 'Poppins', sans-serif;
  background-color: #E0D9CF !important;
}

/* ===== WELCOME PAGE ===== */
#welcome-page {
  position: fixed;
  inset: 0;
  background: #0e0906;
  z-index: 9999;
  display: flex;
  align-items: center;
  justify-content: center;
  overflow: hidden;
}
#background {
  position: relative;
  z-index: 2;
  text-align: center;
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0;
}
.w-eyebrow {
  font-family: 'Poppins', sans-serif;
  font-size: 11px;
  font-weight: 400;
  letter-spacing: 0.45em;
  text-transform: uppercase;
  color: #BFA38A;
  opacity: 0;
  animation: w-fade-up 0.9s ease forwards;
  animation-delay: 0.3s;
  margin-bottom: 22px;
}
.w-title {
  font-family: 'Cormorant Garamond', 'Playfair Display', serif;
  font-size: 88px;
  font-weight: 300;
  letter-spacing: 0.22em;
  color: #F5E9D3;
  line-height: 1;
  margin: 0;
  opacity: 0;
  animation: w-fade-up 1.1s ease forwards;
  animation-delay: 0.7s;
  text-shadow: 0 2px 40px rgba(0,0,0,0.4);
}
.w-divider {
  display: flex;
  align-items: center;
  gap: 16px;
  margin: 24px 0;
  opacity: 0;
  animation: w-fade 0.9s ease forwards;
  animation-delay: 1.3s;
}
.w-divider-line {
  width: 60px;
  height: 1px;
  background: linear-gradient(to right, transparent, rgba(191,163,138,0.5));
}
.w-divider-line.right {
  background: linear-gradient(to left, transparent, rgba(191,163,138,0.5));
}
.w-divider-dot {
  width: 4px;
  height: 4px;
  border-radius: 50%;
  background: #BFA38A;
}
.w-subtitle {
  font-family: 'Poppins', sans-serif;
  font-size: 13px;
  font-weight: 300;
  letter-spacing: 0.35em;
  text-transform: uppercase;
  color: #BFA38A;
  opacity: 0;
  animation: w-fade-up 0.9s ease forwards;
  animation-delay: 1.6s;
  margin-bottom: 10px;
}
.w-desc {
  font-family: 'Poppins', sans-serif;
  font-size: 13px;
  font-weight: 300;
  color: #C8BDB5;
  max-width: 380px;
  line-height: 1.8;
  opacity: 0;
  animation: w-fade 0.9s ease forwards;
  animation-delay: 2.0s;
  margin-bottom: 38px;
}
.w-btn-wrap {
  opacity: 0;
  animation: w-fade-up 0.8s ease forwards;
  animation-delay: 2.4s;
}
.btn-mulai {
  background: transparent;
  color: #F5E9D3 !important;
  border: 1px solid rgba(191,163,138,0.55);
  padding: 13px 52px;
  font-family: 'Poppins', sans-serif;
  font-size: 11px;
  font-weight: 500;
  letter-spacing: 0.3em;
  text-transform: uppercase;
  border-radius: 2px;
  cursor: pointer;
  transition: background 0.35s ease, border-color 0.35s ease;
  backdrop-filter: blur(4px);
}
.btn-mulai:hover {
  background: rgba(191,163,138,0.18);
  border-color: rgba(191,163,138,0.9);
  color: #F5E9D3 !important;
}
.w-scroll-hint {
  position: absolute;
  bottom: 32px;
  left: 50%;
  transform: translateX(-50%);
  z-index: 2;
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 6px;
  opacity: 0;
  animation: w-fade 1s ease forwards;
  animation-delay: 3.2s;
}
.w-scroll-line {
  width: 1px;
  height: 36px;
  background: linear-gradient(to bottom, rgba(191,163,138,0.6), transparent);
}
.w-scroll-label {
  font-size: 10px;
  letter-spacing: 0.3em;
  color: rgba(191,163,138,0.5);
  text-transform: uppercase;
}
@keyframes w-fade-up {
  from { opacity: 0; transform: translateY(18px); }
  to   { opacity: 1; transform: translateY(0); }
}
@keyframes w-fade {
  from { opacity: 0; }
  to   { opacity: 1; }
}

/* ===== NAVBAR / SIDEBAR ===== */
.skin-blue .main-header .navbar { background: #544339 !important; }
.skin-blue .main-header .logo {
  background: #3E322B !important;
  color: #F5E9D3 !important;
  font-weight: 700;
  letter-spacing: 1px;
  font-family: 'Playfair Display', serif;
}
.skin-blue .main-header .logo:hover { background: #3E322B !important; }
.skin-blue .main-sidebar { background: #544339 !important; }
.skin-blue .sidebar-menu > li > a { color: #E0D9CF !important; font-weight: 500; }
.skin-blue .sidebar-menu > li.active > a,
.skin-blue .sidebar-menu > li > a:hover {
  background: #8A6C5F !important;
  color: #F5E9D3 !important;
  border-left-color: #F5E9D3 !important;
}
.skin-blue .main-sidebar .user-panel { display: none; }

/* ===== BOXES ===== */
.box {
  border-radius: 14px !important;
  border-top: none !important;
  box-shadow: 0 4px 18px rgba(84,67,57,0.12) !important;
  background: #FFFFFF !important;
}
.box-header { border-radius: 14px 14px 0 0 !important; }
.box.box-solid.box-primary > .box-header {
  background: #8A6C5F !important;
  color: #F5E9D3 !important;
}
.box.box-solid.box-primary { border: 1px solid #8A6C5F !important; }
.small-box {
  border-radius: 14px !important;
  box-shadow: 0 4px 14px rgba(84,67,57,0.15) !important;
}

/* ===== BUTTONS ===== */
.btn-cocoa {
  background: #8A6C5F;
  color: #F5E9D3 !important;
  border: none;
  border-radius: 8px;
  font-weight: 600;
}
.btn-cocoa:hover { background: #544339; color: #F5E9D3 !important; }

/* ===== CAFE CARD (detail) ===== */
.cafe-card {
  background: #fff;
  border-radius: 16px;
  overflow: hidden;
  box-shadow: 0 6px 24px rgba(84,67,57,0.15);
  margin-bottom: 18px;
  transition: transform 0.2s ease;
}
.cafe-card:hover { transform: translateY(-4px); }
.cafe-card-img { width: 100%; height: 160px; object-fit: cover; background: #BFA38A; }
.cafe-card-body { padding: 14px 16px; }
.cafe-card-title {
  font-weight: 700; font-size: 15px; color: #544339; margin-bottom: 4px;
  font-family: 'Playfair Display', serif;
}
.cafe-card-cat {
  font-size: 11px; color: #8A6C5F; background: #F5E9D3;
  display: inline-block; padding: 2px 10px; border-radius: 10px; margin-bottom: 6px;
}
.rating-star { color: #D4A017; font-weight: 700; }

/* ===== TEAM CARD ===== */
.team-grid { display: flex; flex-wrap: wrap; gap: 16px; margin-top: 14px; }
.team-card {
  flex: 1 1 170px; max-width: 190px; background: #fff;
  border-radius: 16px; padding: 20px 14px; text-align: center;
  box-shadow: 0 4px 18px rgba(84,67,57,0.10); transition: transform 0.2s ease;
}
.team-card:hover { transform: translateY(-4px); }
.team-photo {
  width: 96px; height: 96px; border-radius: 50%; object-fit: cover;
  display: block; margin: 0 auto 12px auto;
  border: 3px solid #F5E9D3; background: #BFA38A;
}
.team-name { font-family: 'Playfair Display', serif; font-weight: 700; color: #544339; font-size: 14px; margin-bottom: 8px; }
.team-ig { display: inline-flex; align-items: center; gap: 5px; color: #8A6C5F; font-size: 12px; font-weight: 600; text-decoration: none; }
.team-ig:hover { color: #544339; }

/* ===== FILTER PANEL ===== */
.filter-panel {
  background: #fff; border-radius: 14px; padding: 18px;
  box-shadow: 0 4px 18px rgba(84,67,57,0.10); margin-bottom: 15px;
}
.filter-title {
  font-family: 'Playfair Display', serif; font-weight: 700;
  color: #544339; font-size: 16px; margin-bottom: 12px;
  border-bottom: 2px solid #F5E9D3; padding-bottom: 8px;
}

/* ===== HEATMAP LEGEND ===== */
.heatmap-legend {
  background: #fff; border-radius: 10px; padding: 10px 14px;
  margin-top: 10px; font-size: 12px; color: #544339;
}
.heatmap-legend-bar {
  height: 12px; border-radius: 6px;
  background: linear-gradient(to right, #3498db, #2ecc71, #f39c12, #e74c3c);
  margin: 6px 0 4px 0;
}
.heatmap-legend-labels { display: flex; justify-content: space-between; font-size: 11px; color: #888; }

/* ===== SLIDERS ===== */
.irs--shiny .irs-bar { background: #8A6C5F; border-top: 1px solid #8A6C5F; border-bottom: 1px solid #8A6C5F; }
.irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: #544339; }
.irs--shiny .irs-handle>i:first-child { background: #8A6C5F; }

/* ===== PAGE TITLE ===== */
.page-title { font-family: 'Playfair Display', serif; color: #544339; font-weight: 700; margin-bottom: 18px; }

/* ===== LEAFLET POPUP ===== */
.leaflet-popup-content-wrapper { border-radius: 10px; padding: 0; }
.leaflet-popup-content { margin: 0; }

/* ===== TOP 3 RECOMMENDATION ===== */
.rec-card {
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: 14px;
  padding: 16px 18px;
  margin-bottom: 14px;
  background: #FDFBF8;
  border-radius: 16px;
  box-shadow: 0 2px 12px rgba(84,67,57,0.10);
  transition: transform 0.18s ease, box-shadow 0.18s ease;
  position: relative;
  border-left: 5px solid #ccc;
}
.rec-card:hover {
  transform: translateY(-3px);
  box-shadow: 0 8px 22px rgba(84,67,57,0.18);
}
.rec-card.rank-1 { border-left-color: #D4A017; }
.rec-card.rank-2 { border-left-color: #9E9E9E; }
.rec-card.rank-3 { border-left-color: #CD7F32; }
.rec-rank-badge { font-size: 26px; width: 32px; text-align: center; flex-shrink: 0; }
.rec-img {
  width: 72px; height: 72px; border-radius: 12px;
  object-fit: cover; flex-shrink: 0; background: #BFA38A;
}
.rec-info { flex: 1; min-width: 0; }
.rec-name {
  font-weight: 700; font-size: 14px; color: #544339;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis; margin-bottom: 3px;
}
.rec-meta { font-size: 12px; color: #8A6C5F; margin-bottom: 8px; }
.rec-bar-bg { background: #E0D9CF; border-radius: 6px; height: 7px; width: 100%; }
.rec-bar-fill { height: 7px; border-radius: 6px; background: linear-gradient(90deg, #D4A017, #F5C842); }
.rec-score { text-align: right; flex-shrink: 0; min-width: 58px; }
.rec-score-num { font-size: 24px; font-weight: 800; color: #D4A017; line-height: 1; }
.rec-score-label { font-size: 10px; color: #aaa; font-weight: 400; }

/* ===== COFFEE PLAYLIST (fitur baru) ===== */
.playlist-intro {
  background: linear-gradient(135deg, #8A6C5F, #544339);
  border-radius: 16px; padding: 26px 32px; margin-bottom: 22px; color: #F5E9D3;
}
.playlist-grid { display: flex; flex-wrap: wrap; gap: 16px; margin-bottom: 26px; }
.playlist-card {
  flex: 1 1 150px; max-width: 180px; min-height: 130px;
  background: #fff !important; border: 2px solid transparent !important;
  border-radius: 16px !important; padding: 20px 12px !important; text-align: center;
  box-shadow: 0 4px 18px rgba(84,67,57,0.10); transition: transform 0.2s ease, box-shadow 0.2s ease, background 0.2s ease;
  color: #544339 !important; white-space: normal !important;
}
.playlist-card:hover, .playlist-card:focus {
  transform: translateY(-4px); box-shadow: 0 8px 22px rgba(84,67,57,0.18);
  background: #fff !important; color: #544339 !important;
}
.playlist-card.active-cat {
  border-color: #8A6C5F !important; background: #F5E9D3 !important;
}
.playlist-card-icon { font-size: 28px; margin-bottom: 8px; line-height: 1; }
.playlist-card-label {
  font-family: 'Playfair Display', serif; font-weight: 700;
  color: #544339; font-size: 15px; margin-bottom: 4px;
}
.playlist-card-desc { font-size: 11px; color: #8A6C5F; }
.playlist-section-title {
  font-family: 'Playfair Display', serif; color: #544339;
  font-weight: 700; font-size: 18px; margin: 6px 0 16px 0;
  border-bottom: 2px solid #F5E9D3; padding-bottom: 8px;
}
.playlist-embed-wrap { display: flex; flex-wrap: wrap; gap: 18px; }
.playlist-embed-box {
  flex: 1 1 320px; max-width: 380px; background: #fff;
  border-radius: 16px; padding: 14px; box-shadow: 0 4px 18px rgba(84,67,57,0.10);
}
.playlist-embed-title {
  font-size: 13px; font-weight: 600; color: #544339;
  margin-bottom: 8px; display: flex; align-items: center; gap: 6px;
}
.playlist-embed-box iframe { border-radius: 12px; display: block; }

/* ===== SCROLLBAR ===== */
::-webkit-scrollbar { width: 8px; }
::-webkit-scrollbar-thumb { background: #8A6C5F; border-radius: 10px; }
::-webkit-scrollbar-track { background: #E0D9CF; }
"

# ------------------------------------------------------------
# 5. WELCOME PAGE UI
# ------------------------------------------------------------
welcome_ui <- div(
  id = "welcome-page",
  div(
    class = "welcome-box",
    div(class = "w-eyebrow", "SURAKARTA COFFEE GUIDE"),
    h1(class = "w-title", "FYP"),
    div(class = "w-divider",
        div(class = "w-divider-line"),
        div(class = "w-divider-dot"),
        div(class = "w-divider-line right")
    ),
    div(class = "w-subtitle", "FIND YOUR PLACE"),
    div(class = "w-desc",
        "Eksplorasi cafe terbaik di Surakarta berdasarkan lokasi, rating, harga, dan fasilitas."),
    div(class = "w-btn-wrap",
        actionButton("btn_mulai", "Mulai Jelajah", class = "btn-mulai"))
  ),
  div(class = "w-scroll-hint",
      div(class = "w-scroll-line"),
      div(class = "w-scroll-label", "EXPLORE"))
)

# ------------------------------------------------------------
# 6. MAIN DASHBOARD UI
# ------------------------------------------------------------
main_ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = HTML("&#9749; FYP"), titleWidth = 230),
  dashboardSidebar(
    width = 230,
    sidebarMenu(
      id = "tabs",
      menuItem("Beranda",         tabName = "beranda",   icon = icon("home")),
      menuItem("Peta Cafe",       tabName = "peta",      icon = icon("map-marked-alt")),
      menuItem("Daftar & Filter", tabName = "daftar",    icon = icon("list")),
      menuItem("Statistik",       tabName = "statistik", icon = icon("chart-pie")),
      menuItem("Detail Cafe",     tabName = "detail",    icon = icon("mug-hot")),
      menuItem("Coffee Playlist", tabName = "playlist", icon = icon("music")),
      menuItem("Tentang",         tabName = "tentang",   icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(app_css))),
    tabItems(
      
      # ---------- BERANDA ----------
      tabItem(tabName = "beranda",
              fluidRow(
                column(12,
                       div(style = "background:linear-gradient(135deg,#8A6C5F,#544339);border-radius:16px;padding:30px 35px;margin-bottom:20px;color:#F5E9D3;",
                           h2(style = "font-family:'Playfair Display',serif;font-weight:700;margin-bottom:6px;", "Selamat Datang di FYP"),
                           p(style = "font-size:14px;opacity:0.9;max-width:650px;",
                             "Find Your Place membantu kamu menjelajahi cafe di Surakarta berdasarkan lokasi, harga, rating, dan fasilitas yang tersedia.")
                       )
                )
              ),
              fluidRow(
                valueBoxOutput("vb_total",    width = 3),
                valueBoxOutput("vb_rating",   width = 3),
                valueBoxOutput("vb_buka24",   width = 3),
                valueBoxOutput("vb_delivery", width = 3)
              ),
              fluidRow(
                box(title = "Sebaran Lokasi Cafe", status = "primary", solidHeader = TRUE,
                    width = 7, height = 500, leafletOutput("map_home", height = 430)),
                box(title = "Top Cafe Recommendation", status = "primary", solidHeader = TRUE,
                    width = 5, height = 500,
                    div(style = "padding:4px 2px;",
                        p(style = "font-size:11px;color:#aaa;margin-bottom:14px;font-style:italic;",
                          "Berdasarkan kualitas & popularitas"),
                        uiOutput("top3_card")
                    )
                )
              )
      ),
      
      # ---------- PETA ----------
      tabItem(tabName = "peta",
              h2(class = "page-title", "Peta Interaktif Cafe Surakarta"),
              fluidRow(
                column(3,
                       div(class = "filter-panel",
                           div(class = "filter-title", HTML("&#128269; Filter Peta")),
                           div(class = "heatmap-legend",
                               strong("Tingkat Kepadatan"),
                               div(class = "heatmap-legend-bar"),
                               div(class = "heatmap-legend-labels", span("Sepi"), span("Sedang"), span("Ramai"))
                           ),
                           hr(style = "border-color:#E0D9CF;"),
                           sliderInput("map_rating", "Rating Minimum", min = 3.5, max = 5, value = 3.5, step = 0.1),
                           checkboxGroupInput("map_status", "Jam Operasional", choices = NULL),
                           hr(style = "border-color:#E0D9CF;"),
                           actionButton("map_reset", "Reset Filter", class = "btn-cocoa", width = "100%")
                       )
                ),
                column(9,
                       box(width = 12, solidHeader = TRUE, status = "primary",
                           leafletOutput("map_main", height = 620))
                )
              )
      ),
      
      # ---------- DAFTAR & FILTER ----------
      tabItem(tabName = "daftar",
              h2(class = "page-title", "Daftar & Filter Cafe"),
              fluidRow(
                column(3,
                       div(class = "filter-panel",
                           div(class = "filter-title", HTML("&#9881; Filter Pencarian")),
                           textInput("f_search", "Cari nama cafe", placeholder = "Contoh: Coffee, Eatery..."),
                           sliderInput("f_rating", "Rentang Rating", min = 3.5, max = 5, value = c(3.5, 5), step = 0.1),
                           pickerInput("f_harga", "Rentang Harga", choices = NULL, multiple = TRUE,
                                       options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2")),
                           checkboxGroupInput("f_status", "Jam Operasional", choices = NULL),
                           checkboxGroupInput("f_order", "Order Online", choices = NULL),
                           hr(style = "border-color:#E0D9CF;"),
                           actionButton("f_reset", "Reset Filter", class = "btn-cocoa", width = "100%")
                       )
                ),
                column(9,
                       box(width = 12, solidHeader = TRUE, status = "primary",
                           title = textOutput("tabel_count"),
                           DTOutput("tabel_cafe"))
                )
              )
      ),
      
      # ---------- STATISTIK ----------
      tabItem(tabName = "statistik",
              h2(class = "page-title", "Statistik & Insight"),
              fluidRow(
                box(title = "Distribusi Rating", status = "primary", solidHeader = TRUE,
                    width = 6, plotlyOutput("plot_rating", height = 330)),
                box(title = "Status Jam Buka", status = "primary", solidHeader = TRUE,
                    width = 6, height = 390, uiOutput("plot_buka24_stat"))
              ),
              fluidRow(
                box(title = "Fasilitas yang Tersedia", status = "primary", solidHeader = TRUE,
                    width = 6, plotlyOutput("plot_fasilitas", height = 330)),
                box(title = "Opsi Layanan Delivery", status = "primary", solidHeader = TRUE,
                    width = 6, plotlyOutput("plot_delivery", height = 330))
              )
      ),
      
      # ---------- DETAIL CAFE ----------
      tabItem(tabName = "detail",
              h2(class = "page-title", "Detail Cafe"),
              fluidRow(
                column(4,
                       div(class = "filter-panel",
                           div(class = "filter-title", HTML("&#9749; Pilih Cafe")),
                           selectInput("detail_pilih", NULL, choices = NULL, width = "100%")
                       )
                )
              ),
              fluidRow(column(12, uiOutput("detail_card")))
      ),
      
      # ---------- COFFEE PLAYLIST (fitur baru) ----------
      tabItem(tabName = "playlist",
              div(class = "playlist-intro",
                  h2(style = "font-family:'Playfair Display',serif;font-weight:700;margin-bottom:6px;",
                     HTML("&#127925; Coffee Playlist")),
                  p(style = "font-size:14px;opacity:0.9;max-width:650px;margin-bottom:0;",
                    "Pilih mood kamu, dan kami kasih rekomendasi playlist Spotify yang pas buat nemenin waktu ngopi-mu.")
              ),
              fluidRow(
                column(12,
                       div(class = "playlist-grid",
                           lapply(names(playlist_data), function(cat) {
                             actionButton(
                               inputId = paste0("playlist_cat_", cat),
                               label = div(
                                 div(class = "playlist-card-icon", playlist_data[[cat]]$emoji),
                                 div(class = "playlist-card-label", cat),
                                 div(class = "playlist-card-desc", playlist_data[[cat]]$desc)
                               ),
                               class = paste("playlist-card", if (cat == "Study") "active-cat" else "")
                             )
                           })
                       )
                )
              ),
              fluidRow(column(12, uiOutput("playlist_embed_section")))
      ),
      
      # ---------- TENTANG ----------
      tabItem(tabName = "tentang",
              h2(class = "page-title", "Tentang FYP"),
              box(width = 12, solidHeader = TRUE, status = "primary",
                  h4(style = "color:#544339;font-weight:700;", "FYP - Find Your Place"),
                  p("FYP adalah aplikasi eksplorasi cafe di Surakarta yang dibangun menggunakan RShiny. ",
                    "Data mencakup lokasi (koordinat), rating, jumlah ulasan, rentang harga, jam operasional, dan fasilitas layanan."),
                  tags$ul(
                    tags$li(strong("Sumber data: "), "Google Maps - data cafe wilayah Surakarta"),
                    tags$li(strong("Jumlah tempat: "), nrow(cafe), " lokasi"),
                    tags$li(strong("Fitur: "), "Peta interaktif, filter pencarian, statistik, dan detail cafe")
                  )
              )
      )
    )
  )
)

# ------------------------------------------------------------
# 7. ROOT UI
# ------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML(app_css)),
    tags$title("FYP - Find Your Place")
  ),
  welcome_ui,
  hidden(div(id = "app-container", main_ui))
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # Transisi Welcome -> App
  observeEvent(input$btn_mulai, {
    shinyjs::hide("welcome-page", anim = TRUE, animType = "fade", time = 0.6)
    shinyjs::show("app-container", anim = TRUE, animType = "fade", time = 0.6)
  })
  
  # Inisialisasi pilihan filter
  status_choices <- c("Buka 24 Jam", "Tidak Buka 24 Jam")
  order_choices  <- sort(unique(cafe$Order_Online))
  harga_choices  <- sort(unique(cafe$Rentang_Harga))
  
  observe({
    updatePickerInput(session, "f_harga",    choices = harga_choices,   selected = harga_choices)
    updateCheckboxGroupInput(session, "map_status", choices = status_choices, selected = status_choices)
    updateCheckboxGroupInput(session, "f_status",   choices = status_choices, selected = status_choices)
    updateCheckboxGroupInput(session, "f_order",    choices = order_choices,  selected = order_choices)
    updateSelectInput(session, "detail_pilih", choices = setNames(cafe$id, cafe$Nama_Cafe))
  }, priority = 10)
  
  # Reset filter peta
  observeEvent(input$map_reset, {
    updateSliderInput(session, "map_rating", value = 3.5)
    updateCheckboxGroupInput(session, "map_status", selected = status_choices)
  })
  
  # Reset filter daftar
  observeEvent(input$f_reset, {
    updateTextInput(session, "f_search", value = "")
    updateSliderInput(session, "f_rating", value = c(3.5, 5))
    updatePickerInput(session, "f_harga",  selected = harga_choices)
    updateCheckboxGroupInput(session, "f_status", selected = status_choices)
    updateCheckboxGroupInput(session, "f_order",  selected = order_choices)
  })
  
  # ===== COFFEE PLAYLIST (fitur baru) =====
  selected_playlist_cat <- reactiveVal("Study")
  
  for (cat in names(playlist_data)) {
    local({
      cat_local <- cat
      observeEvent(input[[paste0("playlist_cat_", cat_local)]], {
        selected_playlist_cat(cat_local)
        lapply(names(playlist_data), function(c2) {
          shinyjs::removeClass(id = paste0("playlist_cat_", c2), class = "active-cat")
        })
        shinyjs::addClass(id = paste0("playlist_cat_", cat_local), class = "active-cat")
      }, ignoreInit = TRUE)
    })
  }
  
  output$playlist_embed_section <- renderUI({
    cat   <- selected_playlist_cat()
    pdata <- playlist_data[[cat]]
    
    div(
      h4(class = "playlist-section-title",
         HTML(paste0(pdata$emoji, " Rekomendasi Playlist untuk \"", cat, "\""))),
      div(class = "playlist-embed-wrap",
          lapply(seq_along(pdata$ids), function(i) {
            div(class = "playlist-embed-box",
                div(class = "playlist-embed-title",
                    tags$i(class = "fab fa-spotify", style = "color:#1DB954;"),
                    pdata$names[i]),
                tags$iframe(
                  src    = paste0("https://open.spotify.com/embed/playlist/", pdata$ids[i],
                                  "?utm_source=generator&theme=0"),
                  width  = "100%",
                  height = "352",
                  frameBorder = "0",
                  allow  = "autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
                  loading = "lazy"
                )
            )
          })
      )
    )
  })
  
  # Reactive: data peta
  data_map <- reactive({
    req(input$map_status)
    cafe %>% filter(
      Rating >= input$map_rating,
      case_when(
        "Buka 24 Jam" %in% input$map_status & "Tidak Buka 24 Jam" %in% input$map_status ~ TRUE,
        "Buka 24 Jam" %in% input$map_status ~ Status_Buka == "Buka 24 Jam",
        "Tidak Buka 24 Jam" %in% input$map_status ~ Status_Buka != "Buka 24 Jam",
        TRUE ~ FALSE
      )
    )
  })
  
  # Reactive: data tabel
  data_filtered <- reactive({
    req(input$f_status, input$f_order, input$f_harga)
    df <- cafe %>% filter(
      Rating        >= input$f_rating[1], Rating <= input$f_rating[2],
      Rentang_Harga %in% input$f_harga,
      case_when(
        "Buka 24 Jam" %in% input$f_status & "Tidak Buka 24 Jam" %in% input$f_status ~ TRUE,
        "Buka 24 Jam" %in% input$f_status ~ Status_Buka == "Buka 24 Jam",
        "Tidak Buka 24 Jam" %in% input$f_status ~ Status_Buka != "Buka 24 Jam",
        TRUE ~ FALSE
      ),
      Order_Online %in% input$f_order
    )
    if (nchar(trimws(input$f_search)) > 0)
      df <- df %>% filter(grepl(trimws(input$f_search), Nama_Cafe, ignore.case = TRUE))
    df
  })
  
  # Value boxes
  output$vb_total    <- renderValueBox(valueBox(nrow(cafe), "Total Cafe", icon = icon("mug-hot"), color = "yellow"))
  output$vb_rating   <- renderValueBox(valueBox(round(mean(cafe$Rating), 2), "Rata-rata Rating", icon = icon("star"), color = "orange"))
  output$vb_buka24   <- renderValueBox(valueBox(sum(cafe$Status_Buka == "Buka 24 Jam"), "Buka 24 Jam", icon = icon("clock"), color = "olive"))
  output$vb_delivery <- renderValueBox(valueBox(sum(cafe$Opsi_Delivery %in% c("Delivery", "Delivery tanpa kontak")), "Ada Delivery", icon = icon("motorcycle"), color = "maroon"))
  
  # Peta beranda
  output$map_home <- renderLeaflet({
    density_score <- sapply(seq_len(nrow(cafe)), function(i)
      sum(abs(cafe$Latitude - cafe$Latitude[i]) < 0.006 &
            abs(cafe$Longitude - cafe$Longitude[i]) < 0.006) - 1)
    pal_d <- colorNumeric(c("#3498db","#2ecc71","#f39c12","#e74c3c"), domain = density_score)
    leaflet(cafe) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = mean(cafe$Longitude), lat = mean(cafe$Latitude), zoom = 13) %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius = 7,
                       color = "#ffffff", weight = 1.5,
                       fillColor = pal_d(density_score), fillOpacity = 0.85,
                       popup = lapply(seq_len(nrow(cafe)), function(i) make_popup(cafe[i,]))) %>%
      addLegend("bottomright", pal = pal_d, values = density_score,
                title = "Kepadatan Area", labFormat = labelFormat(suffix = " cafe"), opacity = 0.85)
  })
  
  # Peta tab peta
  output$map_main <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
      setView(lng = mean(cafe$Longitude), lat = mean(cafe$Latitude), zoom = 13)
  })
  observe({
    df <- data_map()
    proxy <- leafletProxy("map_main") %>% clearMarkers() %>% clearHeatmap()
    if (nrow(df) == 0) return(invisible(NULL))
    density_score <- sapply(seq_len(nrow(df)), function(i)
      sum(abs(df$Latitude - df$Latitude[i]) < 0.006 &
            abs(df$Longitude - df$Longitude[i]) < 0.006) - 1)
    pal_d <- colorNumeric(c("#3498db","#2ecc71","#f39c12","#e74c3c"), domain = density_score)
    proxy %>% addCircleMarkers(data = df, lng = ~Longitude, lat = ~Latitude, radius = 7,
                               color = "#ffffff", weight = 1.5,
                               fillColor = pal_d(density_score), fillOpacity = 0.85,
                               popup = lapply(seq_len(nrow(df)), function(i) make_popup(df[i,])))
  })
  
  # Top 3 Recommendation -- skor = Rating * log10(ulasan + 1)
  output$top3_card <- renderUI({
    df <- cafe %>%
      mutate(skor = Rating * log10(Jumlah_Ulasan + 1)) %>%
      arrange(desc(skor)) %>%
      head(3)
    
    skor_max <- max(df$skor)
    skor_min <- min(df$skor)
    rank_icons <- c("\U0001F947", "\U0001F948", "\U0001F949")
    rank_class <- c("rank-1", "rank-2", "rank-3")
    
    cards <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      pct <- if (skor_max == skor_min) 100 else
        round(60 + 40 * (row$skor - skor_min) / (skor_max - skor_min))
      ulasan_fmt <- if (row$Jumlah_Ulasan >= 1000)
        paste0(round(row$Jumlah_Ulasan / 1000, 1), "K")
      else as.character(row$Jumlah_Ulasan)
      
      div(class = paste("rec-card", rank_class[i]),
          div(class = "rec-rank-badge", rank_icons[i]),
          tags$img(src = row$URL_Foto, class = "rec-img",
                   onerror = "this.style.background='#BFA38A';this.src='';"),
          div(class = "rec-info",
              div(class = "rec-name", row$Nama_Cafe),
              div(class = "rec-meta",
                  HTML(paste0("\u2605 ", row$Rating,
                              " \u00b7 ", ulasan_fmt, " ulasan",
                              " \u00b7 ", row$Rentang_Harga))),
              div(class = "rec-bar-bg",
                  div(class = "rec-bar-fill", style = paste0("width:", pct, "%;")))
          ),
          div(class = "rec-score",
              div(class = "rec-score-num", row$Rating),
              div(class = "rec-score-label", "rating")
          )
      )
    })
    
    div(cards)
  })
  
  # Tabel
  output$tabel_count <- renderText({
    paste0("Menampilkan ", nrow(data_filtered()), " dari ", nrow(cafe), " cafe")
  })
  output$tabel_cafe <- renderDT({
    df <- data_filtered() %>%
      select(Nama_Cafe, Kategori, Rating, Jumlah_Ulasan, Jam_Tutup_Buka, Opsi_Delivery, Alamat)
    datatable(df,
              colnames = c("Nama Cafe","Kategori","Rating","Ulasan","Jam Operasional","Delivery","Alamat"),
              rownames = FALSE, filter = "none",
              options = list(
                pageLength = 10, scrollX = TRUE, dom = "ltp",
                language = list(
                  info       = "Menampilkan _START_ - _END_ dari _TOTAL_ data",
                  paginate   = list(previous = "Sebelumnya", `next` = "Selanjutnya"),
                  lengthMenu = "Tampilkan _MENU_ data",
                  zeroRecords = "Tidak ada data ditemukan"
                ),
                columnDefs = list(list(className = "dt-center", targets = c(2, 3)))
              ),
              class = "stripe hover"
    ) %>%
      formatStyle("Rating",
                  background = styleColorBar(c(3.5, 5), "#F5E9D3"),
                  backgroundSize = "98% 88%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
  
  # ===== STATISTIK =====
  
  # 1. Distribusi Rating (histogram)
  output$plot_rating <- renderPlotly({
    p <- ggplot(cafe, aes(x = Rating)) +
      geom_histogram(binwidth = 0.1, fill = "#8A6C5F", color = "#F5E9D3") +
      labs(x = "Rating", y = "Jumlah Cafe") +
      theme_minimal() + theme(text = element_text(color = "#544339"))
    ggplotly(p)
  })
  
  # 2. Pie chart: Status Jam Buka (Buka 24 Jam vs Tidak)
  output$plot_buka24_stat <- renderUI({
    total   <- nrow(cafe)
    buka24  <- sum(cafe$Status_Buka == "Buka 24 Jam")
    tidak   <- total - buka24
    pct24   <- round(buka24 / total * 100, 1)
    pct_tdk <- round(tidak  / total * 100, 1)
    
    div(style = "padding: 10px 6px;",
        
        div(style = "margin-bottom: 22px;",
            div(style = "display:flex; justify-content:space-between; margin-bottom:6px;",
                span(style = "font-weight:600; color:#544339; font-size:13px;",
                     HTML("&#9679; Buka 24 Jam")),
                span(style = "font-weight:800; color:#544339; font-size:20px;",
                     paste0(buka24, " cafe"))
            ),
            div(style = "background:#E0D9CF; border-radius:8px; height:14px; width:100%;",
                div(style = paste0("width:", pct24, "%; height:14px; border-radius:8px;",
                                   "background: linear-gradient(90deg, #544339, #8A6C5F);"))
            ),
            div(style = "text-align:right; font-size:11px; color:#aaa; margin-top:4px;",
                paste0(pct24, "% dari total cafe"))
        ),
        
        div(style = "margin-bottom: 24px;",
            div(style = "display:flex; justify-content:space-between; margin-bottom:6px;",
                span(style = "font-weight:600; color:#BFA38A; font-size:13px;",
                     HTML("&#9679; Tidak Buka 24 Jam")),
                span(style = "font-weight:800; color:#BFA38A; font-size:20px;",
                     paste0(tidak, " cafe"))
            ),
            div(style = "background:#E0D9CF; border-radius:8px; height:14px; width:100%;",
                div(style = paste0("width:", pct_tdk, "%; height:14px; border-radius:8px;",
                                   "background: linear-gradient(90deg, #BFA38A, #E0D9CF);"))
            ),
            div(style = "text-align:right; font-size:11px; color:#aaa; margin-top:4px;",
                paste0(pct_tdk, "% dari total cafe"))
        ),
        
        div(style = paste0(
          "background: linear-gradient(135deg, #544339, #8A6C5F);",
          "border-radius: 12px; padding: 14px 18px;",
          "display:flex; justify-content:space-between; align-items:center;"
        ),
        div(
          div(style = "color:#F5E9D3; font-size:11px; letter-spacing:0.1em;
                     text-transform:uppercase; opacity:0.8;", "Total Cafe Terdaftar"),
          div(style = "color:#F5E9D3; font-size:36px; font-weight:800; line-height:1.1;", total)
        ),
        div(style = "font-size:36px; opacity:0.6;", HTML("&#9749;"))
        )
    )
  })
  
  # 3. Fasilitas yang tersedia (bar horizontal)
  output$plot_fasilitas <- renderPlotly({
    fas <- data.frame(
      Fasilitas = c("Order Online","Dine In","Ada Delivery","Drive-through","Buka 24 Jam"),
      Jumlah = c(
        sum(cafe$Order_Online == "Ya"),
        sum(cafe$Dine_In == "Ya"),
        sum(cafe$Opsi_Delivery %in% c("Delivery","Delivery tanpa kontak")),
        sum(cafe$Layanan_Tambahan == "Drive-through"),
        sum(cafe$Status_Buka == "Buka 24 Jam")
      )
    )
    p <- ggplot(fas, aes(reorder(Fasilitas, Jumlah), Jumlah,
                         text = paste0(Fasilitas, ": ", Jumlah))) +
      geom_col(fill = "#544339", width = 0.55) + coord_flip() +
      labs(x = NULL, y = "Jumlah Cafe") +
      theme_minimal() + theme(text = element_text(color = "#544339"))
    ggplotly(p, tooltip = "text")
  })
  
  # 4. Opsi layanan delivery (pie chart)
  output$plot_delivery <- renderPlotly({
    df <- cafe %>%
      count(Opsi_Delivery, name = "Jumlah") %>%
      arrange(desc(Jumlah))
    
    warna_delivery <- colorRampPalette(c("#544339", "#8A6C5F", "#BFA38A", "#E0D9CF"))(nrow(df))
    
    plot_ly(df,
            labels = ~Opsi_Delivery,
            values = ~Jumlah,
            type   = "pie",
            marker = list(colors = warna_delivery,
                          line   = list(color = "#FFFFFF", width = 2)),
            textinfo     = "label+percent",
            hovertemplate = "%{label}: %{value} cafe<extra></extra>"
    ) %>%
      layout(
        showlegend = TRUE,
        legend     = list(orientation = "h", x = 0, y = -0.15),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font = list(color = "#544339")
      )
  })
  
  # Detail cafe
  output$detail_card <- renderUI({
    req(input$detail_pilih)
    row <- cafe %>% filter(id == as.integer(input$detail_pilih))
    if (nrow(row) == 0) return(NULL)
    row <- row[1, ]
    div(class = "cafe-card",
        style = "max-width:700px;margin:0 auto;display:flex;flex-wrap:wrap;",
        div(style = "flex:0 0 280px;",
            tags$img(src = row$URL_Foto, class = "cafe-card-img",
                     style = "height:100%;min-height:260px;",
                     onerror = "this.src='';this.style.background='#BFA38A';")),
        div(class = "cafe-card-body", style = "flex:1;min-width:280px;padding:24px;",
            span(class = "cafe-card-cat", row$Kategori),
            h3(class = "cafe-card-title", style = "font-size:22px;margin-top:8px;", row$Nama_Cafe),
            p(style = "color:#666;font-size:13px;", icon("map-marker-alt"), " ", row$Alamat),
            div(style = "margin:10px 0;",
                span(class = "rating-star", HTML("&#9733;"), " ", row$Rating),
                span(style = "color:#999;font-size:13px;",
                     " (", format(row$Jumlah_Ulasan, big.mark = "."), " ulasan)")),
            div(style = "margin:10px 0;",
                span(
                  style = paste0(
                    "display:inline-block;padding:4px 14px;border-radius:12px;",
                    "font-size:12px;font-weight:600;color:#fff;background:",
                    ifelse(row$Status_Buka == "Buka 24 Jam", "#7C9473", "#E6B400"), ";"
                  ),
                  ifelse(row$Status_Buka == "Buka 24 Jam", "Buka 24 Jam", "Tidak Buka 24 Jam")
                )
            ),
            tags$table(
              style = "margin-top:12px;font-size:13px;color:#544339;width:100%;border-collapse:collapse;",
              tags$tr(
                tags$td(style = "padding:5px 0;width:48%;", strong("Rentang Harga")),
                tags$td(style = "padding:5px 0;", row$Rentang_Harga)
              ),
              tags$tr(
                tags$td(style = "padding:5px 0;", strong("Order Online")),
                tags$td(style = "padding:5px 0;", row$Order_Online)
              ),
              tags$tr(
                tags$td(style = "padding:5px 0;", strong("Dine In")),
                tags$td(style = "padding:5px 0;", row$Dine_In)
              ),
              tags$tr(
                tags$td(style = "padding:5px 0;", strong("Layanan Tambahan")),
                tags$td(style = "padding:5px 0;", row$Layanan_Tambahan)
              ),
              tags$tr(
                tags$td(style = "padding:5px 0;", strong("Opsi Delivery")),
                tags$td(style = "padding:5px 0;", row$Opsi_Delivery)
              )
            ),
            div(style = "margin-top:18px;",
                tags$a(
                  href   = paste0(
                    "https://www.google.com/maps/search/?api=1&query=",
                    utils::URLencode(paste(row$Nama_Cafe, "Surakarta"))
                  ),
                  target = "_blank",
                  style  = paste0(
                    "display:inline-flex;align-items:center;gap:8px;",
                    "background:#544339;color:#F5E9D3;text-decoration:none;",
                    "padding:10px 20px;border-radius:10px;font-size:13px;font-weight:600;",
                    "transition:background 0.2s ease;"
                  ),
                  HTML("&#128205; Buka di Google Maps")
                )
            )
        )
    )
  })
}

# ============================================================
shinyApp(ui = ui, server = server)