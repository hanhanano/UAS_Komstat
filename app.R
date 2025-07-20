library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(leaflet)
library(RcmdrMisc)
library(car)
library(boot)
library(dplyr)
library(tidyr)
library(sf)
library(readr)
library(gridExtra)
library(e1071)
library(lmtest)
library(officer)
library(flextable)
library(nortest)
library(tseries)
library(moments)
library(boot)
library(rsconnect)
  #rsconnect::setAccountInfo(
    #name='Clear-IN',
    #token='F0304BDDB9AE2B005EEC8D127CAC3DE5',
    #secret='PcMly6f1VnMMO0jlAAXGicsoshAS8MELeFXoVwt0'
  #)
#writeManifest() sekali aja dipanggil

ui <- dashboardPage(
  dashboardHeader(title = "CLEAR - IN"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen", icon = icon("edit")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-bar")),
      menuItem("Uji Asumsi Data", tabName = "asumsi", icon = icon("flask")),
      menuItem("Resampling", tabName = "resampling", icon = icon("random")),
      menuItem("Statistik Inferensia", tabName = "inferensia", icon = icon("flask")),
      menuItem("Regresi Linear Berganda", tabName = "regresi", icon = icon("chart-line")),
      br(),
      actionButton("printAllTabs", "🖨️ Cetak Semua Tab")

    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      /* Base colors - elegant neutral palette */
      :root {
        --primary-color: #2c3e50;
        --secondary-color: #34495e;
        --accent-color: #3498db;
        --light-gray: #ecf0f1;
        --medium-gray: #bdc3c7;
        --dark-gray: #7f8c8d;
        --white: #ffffff;
        --shadow-medium: rgba(0, 0, 0, 0.12);
      }

      /* Main layout */
      .content-wrapper, .right-side {
        background-color: #BDDDFF;
        min-height: 100vh;
      }

      /* Header and navigation */
      .main-header .navbar {
        background: var(--primary-color) !important;
        border-bottom: 1px solid var(--secondary-color);
        box-shadow: 0 2px 8px var(--shadow-medium);
      }

      .main-header .logo {
        background: var(--secondary-color) !important;
        color: var(--white) !important;
        font-weight: 600;
        letter-spacing: 0.5px;
      }

      /* Sidebar */
      .skin-blue .main-sidebar {
        background-color: var(--secondary-color) !important;
        box-shadow: 2px 0 8px var(--shadow-medium);
      }

      .stat-card {
        margin-bottom: 12px;
      }

      .equal-height-row {
        display: flex;
      }

      .equal-height-row .shiny-html-output,
      .equal-height-row .box,
      .equal-height-row .upload-card {
        flex: 1;
        display: flex;
        flex-direction: column;
}


      /* Full Download */
      @media print {
        .tab-content > .tab-pane {
          display: block !important;
          height: auto !important;
          opacity: 1 !important;
          visibility: visible !important;
          position: relative !important;
        }
      }

    "))
    ),

    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function () {
        document.getElementById('printAllTabs').onclick = function() {
          window.print();
        };
      });
    ")),

    tabItems(
      tabItem(tabName = "beranda",
              fluidRow(
                column(12,
                       div(class = "hero-section",
                           style = "background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%);
                                    color: white; padding: 40px 30px; border-radius: 8px;
                                    margin-bottom: 30px; text-align: center;
                                    box-shadow: 0 4px 20px rgba(0,0,0,0.1);",
                           h1("Clear - IN",
                              style = "font-size: 2.75em; font-weight: 400; margin: 0; letter-spacing: 0.5px;"),
                           h2("Comprehensive Laboratory for Exploratory Analysis & Research — Inferensia",
                              style = "font-size: 1.5em; font-weight: 300; margin-bottom: 10px; letter-spacing: 1px;"),
                           h4("(Platform Komprehensif untuk Analisis Statistik dengan Metode Resampling)",
                              style = "font-weight: 300; opacity: 0.9; margin-bottom: 0;")
                       )
                )
              ),
              fluidRow(
                column(3,
                       div(class = "stat-card",
                           style = "background: white; padding: 25px; border-radius: 8px;
                                    text-align: center; box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-left: 4px solid #3498db; transition: transform 0.2s ease;
                                    border-top: 1px solid rgba(52,152,219,0.1);",
                           icon("chart-line", class = "fa-2x", style = "color: #3498db; margin-bottom: 15px;"),
                           h3("6", style = "font-size: 2.2em; font-weight: 600; color: #2c3e50; margin: 10px 0 5px 0;"),
                           p("Menu Analisis", style = "color: #7f8c8d; font-weight: 500; margin: 0;")
                       )
                ),
                column(3,
                       div(class = "stat-card",
                           style = "background: white; padding: 25px; border-radius: 8px;
                                    text-align: center; box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-left: 4px solid #27ae60; transition: transform 0.2s ease;
                                    border-top: 1px solid rgba(39,174,96,0.1);",
                           icon("cube", class = "fa-2x", style = "color: #27ae60; margin-bottom: 15px;"),
                           h3("15+", style = "font-size: 2.2em; font-weight: 600; color: #2c3e50; margin: 10px 0 5px 0;"),
                           p("R Packages", style = "color: #7f8c8d; font-weight: 500; margin: 0;")
                       )
                ),
                column(3,
                       div(class = "stat-card",
                           style = "background: white; padding: 25px; border-radius: 8px;
                                    text-align: center; box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-left: 4px solid #e74c3c; transition: transform 0.2s ease;
                                    border-top: 1px solid rgba(231,76,60,0.1);",
                           icon("database", class = "fa-2x", style = "color: #e74c3c; margin-bottom: 15px;"),
                           h3("511", style = "font-size: 2.2em; font-weight: 600; color: #2c3e50; margin: 10px 0 5px 0;"),
                           p("Data Records", style = "color: #7f8c8d; font-weight: 500; margin: 0;")
                       )
                ),
                column(3,
                       div(class = "stat-card",
                           style = "background: white; padding: 25px; border-radius: 8px;
                                    text-align: center; box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-left: 4px solid #9b59b6; transition: transform 0.2s ease;
                                    border-top: 1px solid rgba(155,89,182,0.1);",
                           icon("flask", class = "fa-2x", style = "color: #9b59b6; margin-bottom: 15px;"),
                           h3("3", style = "font-size: 2.2em; font-weight: 600; color: #2c3e50; margin: 10px 0 5px 0;"),
                           p("Metode Resampling", style = "color: #7f8c8d; font-weight: 500; margin: 0;")
                       )
                )
              ),
              tags$style(HTML("
                    .stat-card:hover {
                    transform: translateY(-5px);
                    box-shadow: 0 6px 25px rgba(0,0,0,0.15) !important;
                    }
                ")),

              br(),
              fluidRow(
                column(12,
                       tags$style(HTML("
                        .metadata-container {
                            background: white;
                            border-radius: 15px;
                            padding: 30px;
                            margin-bottom: 20px;
                            box-shadow: 0 10px 30px rgba(0,0,0,0.1);
                        }

                        .metadata-title {
                            color: #2c3e50;
                            font-weight: 600;
                            font-size: 2em;
                            text-align: center;
                            margin-bottom: 30px;
                        }

                        .variable-card {
                            background: rgba(255,255,255,0.95);
                            border-radius: 12px;
                            padding: 20px;
                            margin-bottom: 20px;
                            box-shadow: 0 5px 15px rgba(0,0,0,0.1);
                            transition: transform 0.3s ease, box-shadow 0.3s ease;
                            height: 160px;
                            border-left: 4px solid #3498db;
                        }

                        .variable-card:hover {
                            transform: translateY(-5px);
                            box-shadow: 0 8px 25px rgba(0,0,0,0.15);
                        }

                        .variable-label {
                            color: #667eea;
                            font-size: 18px;
                            font-weight: bold;
                            margin-bottom: 10px;
                            border-bottom: 2px solid #f0f0f0;
                            padding-bottom: 8px;
                        }

                        .variable-name {
                            color: #4a5568;
                            font-size: 14px;
                            margin-bottom: 8px;
                        }

                        .variable-desc {
                            color: #666;
                            font-size: 13px;
                            line-height: 1.5;
                        }

                        .variable-name strong, .variable-desc strong {
                            color: #2d3748;
                            font-weight: 600;
                        }
                        ")),

                       div(class = "metadata-container",
                           h3("Metadata Variabel Dataset", class = "metadata-title"),
                           fluidRow(
                             column(4,
                                    div(class = "variable-card",
                                        div("DISTRICTCODE", class = "variable-label"),
                                        p(strong("Variabel: "), "Kode Distrik", class = "variable-name"),
                                        p(strong("Penjelasan: "), "Kode wilayah/distrik", class = "variable-desc")
                                    )
                             ),
                             column(4,
                                    div(class = "variable-card",
                                        div("CHILDREN", class = "variable-label"),
                                        p(strong("Variabel: "), "Anak-anak", class = "variable-name"),
                                        p(strong("Penjelasan: "), "Persentase populasi berusia di bawah lima tahun", class = "variable-desc")
                                    )
                             ),
                             column(4,
                                    div(class = "variable-card",
                                        div("FEMALE", class = "variable-label"),
                                        p(strong("Variabel: "), "Perempuan", class = "variable-name"),
                                        p(strong("Penjelasan: "), "Persentase populasi perempuan", class = "variable-desc")
                                    )
                             )
                           ),
                           fluidRow(
                             column(4,
                                    div(class = "variable-card",
                                        div("ELDERLY", class = "variable-label"),
                                        p(strong("Variabel: "), "Lansia", class = "variable-name"),
                                        p(strong("Penjelasan: "), "Persentase populasi berusia 65 tahun ke atas", class = "variable-desc")
                                    )
                             ),
                             column(4,
                                    div(class = "variable-card",
                                        div("FHEAD", class = "variable-label"),
                                        p(strong("Variabel: "), "Kepala Rumah Tangga Perempuan", class = "variable-name"),
                                        p(strong("Penjelasan: "), "Persentase rumah tangga dengan kepala keluarga perempuan", class = "variable-desc")
                                    )
                             ),
                             column(4,
                                    div(class = "variable-card",
                                        div("FAMILYSIZE", class = "variable-label"),
                                        p(strong("Variabel: "), "Anggota Rumah Tangga", class = "variable-name"),
                                        p(strong("Penjelasan: "), "Rata-rata jumlah anggota rumah tangga dalam satu distrik", class = "variable-desc")
                                    )
                             )
                           ),
                           fluidRow(
                             column(4,
                                    div(class = "variable-card",
                                        div("NOELECTRIC", class = "variable-label"),
                                        p(strong("Variabel: "), "Rumah Tangga Tanpa Listrik", class = "variable-name"),
                                        p(strong("Penjelasan: "), "Persentase rumah tangga yang tidak menggunakan listrik sebagai sumber penerangan", class = "variable-desc")
                                    )
                             ),
                             column(4,
                                    div(class = "variable-card",
                                        div("LOWEDU", class = "variable-label"),
                                        p(strong("Variabel: "), "Pendidikan Rendah", class = "variable-name"),
                                        p(strong("Penjelasan: "), "Persentase populasi berusia 15 tahun ke atas dengan pendidikan rendah", class = "variable-desc")
                                    )
                             ),
                             column(4,
                                    div(class = "variable-card",
                                        div("GROWTH", class = "variable-label"),
                                        p(strong("Variabel: "), "Pertumbuhan Populasi", class = "variable-name"),
                                        p(strong("Penjelasan: "), "Persentase perubahan populasi", class = "variable-desc")
                                    )
                             )
                           ),
                           fluidRow(
                             column(4,
                                    div(class = "variable-card",
                                        div("POVERTY", class = "variable-label"),
                                        p(strong("Variabel: "), "Kemiskinan", class = "variable-name"),
                                        p(strong("Penjelasan: "), "Persentase penduduk miskin", class = "variable-desc")
                                    )
                             ),
                             column(4,
                                    div(class = "variable-card",
                                        div("ILLITERATE", class = "variable-label"),
                                        p(strong("Variabel: "), "Buta Huruf", class = "variable-name"),
                                        p(strong("Penjelasan: "), "Persentase populasi yang tidak dapat membaca dan menulis", class = "variable-desc")
                                    )
                             ),
                             column(4,
                                    div(class = "variable-card",
                                        div("NOTRAINING", class = "variable-label"),
                                        p(strong("Variabel: "), "Pelatihan", class = "variable-name"),
                                        p(strong("Penjelasan: "), "Persentase rumah tangga yang tidak mendapat pelatihan bencana", class = "variable-desc")
                                    )
                             )
                           ),
                           fluidRow(
                             column(4,
                                    div(class = "variable-card",
                                        div("DPRONE", class = "variable-label"),
                                        p(strong("Variabel: "), "Rawan Bencana", class = "variable-name"),
                                        p(strong("Penjelasan: "), "Persentase rumah tangga yang tinggal di daerah rawan bencana", class = "variable-desc")
                                    )
                             ),
                             column(4,
                                    div(class = "variable-card",
                                        div("RENTED", class = "variable-label"),
                                        p(strong("Variabel: "), "Kepemilikan Rumah", class = "variable-name"),
                                        p(strong("Penjelasan: "), "Persentase rumah tangga yang menyewa rumah", class = "variable-desc")
                                    )
                             ),
                             column(4,
                                    div(class = "variable-card",
                                        div("NOSEWER", class = "variable-label"),
                                        p(strong("Variabel: "), "Drainase", class = "variable-name"),
                                        p(strong("Penjelasan: "), "Persentase rumah tangga yang tidak memiliki sistem drainase", class = "variable-desc")
                                    )
                             )
                           ),
                           fluidRow(
                             column(4,
                                    div(class = "variable-card",
                                        div("TAPWATER", class = "variable-label"),
                                        p(strong("Variabel: "), "Sumber Air", class = "variable-name"),
                                        p(strong("Penjelasan: "), "Persentase rumah tangga yang menggunakan air ledeng", class = "variable-desc")
                                    )
                             ),
                             column(4,
                                    div(class = "variable-card",
                                        div("POPULATION", class = "variable-label"),
                                        p(strong("Variabel: "), "Populasi", class = "variable-name"),
                                        p(strong("Penjelasan: "), "Jumlah Populasi", class = "variable-desc")
                                    )
                             )
                           )
                       )
                )
              ),
              fluidRow(
                column(6,
                       div(class = "features-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #3498db; min-height: 400px;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("star", class = "fa-lg", style = "color: #3498db; margin-right: 10px;"),
                               h3("Fitur",
                                  style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                           ),

                           div(style = "font-size: 15px; line-height: 1.6;",

                               div(class = "feature-item",
                                   style = "margin-bottom: 20px; padding: 15px; background: #e8f4fd; border-radius: 6px; border-left: 4px solid #3498db;",
                                   h5(icon("upload"), " Manajemen Data",
                                      style = "color: #2c3e50; font-weight: 600; margin-bottom: 8px;"),
                                   p("Upload file CSV/Excel, transformasi variabel, dan kategorisasi data dengan antarmuka yang intuitif.",
                                     style = "margin: 0; color: #5a6c7d;")
                               ),

                               div(class = "feature-item",
                                   style = "margin-bottom: 20px; padding: 15px; background: #e8f5e8; border-radius: 6px; border-left: 4px solid #27ae60;",
                                   h5(icon("chart-bar"), " Eksplorasi Data",
                                      style = "color: #2c3e50; font-weight: 600; margin-bottom: 8px;"),
                                   p("Statistik deskriptif komprehensif, visualisasi interaktif, dan pemetaan data geografis.",
                                     style = "margin: 0; color: #5a6c7d;")
                               ),

                               div(class = "feature-item",
                                   style = "margin-bottom: 20px; padding: 15px; background: #fdf2f2; border-radius: 6px; border-left: 4px solid #e74c3c;",
                                   h5(icon("check-circle"), " Uji Asumsi Statistik",
                                      style = "color: #2c3e50; font-weight: 600; margin-bottom: 8px;"),
                                   p("Pengujian normalitas dan homogenitas dengan interpretasi otomatis hasil uji.",
                                     style = "margin: 0; color: #5a6c7d;")
                               ),

                               div(class = "feature-item",
                                   style = "margin-bottom: 20px; padding: 15px; background: #f4f1f8; border-radius: 6px; border-left: 4px solid #9b59b6;",
                                   h5(icon("random"), " Metode Resampling",
                                      style = "color: #2c3e50; font-weight: 600; margin-bottom: 8px;"),
                                   p("Jackknife, Bootstrap, dan Permutasi untuk estimasi parameter dan pengujian hipotesis yang robust.",
                                     style = "margin: 0; color: #5a6c7d;")
                               ),

                               div(class = "feature-item",
                                   style = "margin-bottom: 0; padding: 15px; background: #fff8e1; border-radius: 6px; border-left: 4px solid #f39c12;",
                                   h5(icon("calculator"), " Analisis Lanjutan",
                                      style = "color: #2c3e50; font-weight: 600; margin-bottom: 8px;"),
                                   p("Uji statistik inferensia dan regresi linier berganda dengan diagnostik lengkap.",
                                     style = "margin: 0; color: #5a6c7d;")
                               )
                           )
                       )
                ),
                column(6,
                       div(class = "guide-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #17a2b8; min-height: 400px;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("compass", class = "fa-lg", style = "color: #17a2b8; margin-right: 10px;"),
                               h3("Panduan Memulai",
                                  style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                           ),

                           div(style = "font-size: 15px; line-height: 1.7;",

                               div(class = "step-item",
                                   style = "display: flex; align-items: flex-start; margin-bottom: 25px;",
                                   div(style = "background: #3498db; color: white; width: 32px; height: 32px; border-radius: 50%;
                                                display: flex; align-items: center; justify-content: center;
                                                font-weight: 600; margin-right: 15px; flex-shrink: 0; margin-top: 5px;
                                                box-shadow: 0 2px 5px rgba(52,152,219,0.3);", "1"),
                                   div(
                                     h6("Upload Dataset",
                                        style = "font-size: 18px; color: #2c3e50; font-weight: 600; margin-bottom: 5px;"),
                                     p("Masukkan data Anda melalui menu 'Manajemen Data' dengan format CSV dan json.",
                                       style = "margin: 0; color: #5a6c7d;")
                                   )
                               ),

                               div(class = "step-item",
                                   style = "display: flex; align-items: flex-start; margin-bottom: 25px;",
                                   div(style = "background: #27ae60; color: white; width: 32px; height: 32px; border-radius: 50%;
                                                display: flex; align-items: center; justify-content: center;
                                                font-weight: 600; margin-right: 15px; flex-shrink: 0; margin-top: 5px;
                                                box-shadow: 0 2px 5px rgba(39,174,96,0.3);", "2"),
                                   div(
                                     h6("Preprocessing Data",
                                        style = "font-size: 18px; color: #2c3e50; font-weight: 600; margin-bottom: 5px;"),
                                     p("Lakukan transformasi variabel atau kategorisasi data sesuai kebutuhan analisis.",
                                       style = "margin: 0; color: #5a6c7d;")
                                   )
                               ),

                               div(class = "step-item",
                                   style = "display: flex; align-items: flex-start; margin-bottom: 25px;",
                                   div(style = "background: #e74c3c; color: white; width: 32px; height: 32px; border-radius: 50%;
                                                display: flex; align-items: center; justify-content: center;
                                                font-weight: 600; margin-right: 15px; flex-shrink: 0; margin-top: 5px;
                                                box-shadow: 0 2px 5px rgba(231,76,60,0.3);", "3"),
                                   div(
                                     h6("Eksplorasi Awal",
                                        style = "font-size: 18px; color: #2c3e50; font-weight: 600; margin-bottom: 5px;"),
                                     p("Jelajahi karakteristik data dengan statistik deskriptif dan visualisasi.",
                                       style = "margin: 0; color: #5a6c7d;")
                                   )
                               ),

                               div(class = "step-item",
                                   style = "display: flex; align-items: flex-start; margin-bottom: 25px;",
                                   div(style = "background: #9b59b6; color: white; width: 32px; height: 32px; border-radius: 50%;
                                                display: flex; align-items: center; justify-content: center;
                                                font-weight: 600; margin-right: 15px; flex-shrink: 0; margin-top: 5px;
                                                box-shadow: 0 2px 5px rgba(155,89,182,0.3);", "4"),
                                   div(
                                     h6("Uji Asumsi",
                                        style = "font-size: 18px; color: #2c3e50; font-weight: 600; margin-bottom: 5px;"),
                                     p("Periksa normalitas dan homogenitas data sebelum melakukan analisis inferensia.",
                                       style = "margin: 0; color: #5a6c7d;")
                                   )
                               ),

                               div(class = "step-item",
                                   style = "display: flex; align-items: flex-start; margin-bottom: 0;",
                                   div(style = "background: #f39c12; color: white; width: 32px; height: 32px; border-radius: 50%;
                                                display: flex; align-items: center; justify-content: center;
                                                font-weight: 600; margin-right: 15px; flex-shrink: 0; margin-top: 5px;
                                                box-shadow: 0 2px 5px rgba(243,156,18,0.3);", "5"),
                                   div(
                                     h6("Analisis Statistik",
                                        style = "font-size: 18px; color: #2c3e50; font-weight: 600; margin-bottom: 5px;"),
                                     p("Terapkan metode resampling dan uji statistik sesuai tujuan penelitian Anda.",
                                       style = "margin: 0; color: #5a6c7d;")
                                   )
                               )
                           )
                       )
                )
              ),
              fluidRow(
                column(12,
                       div(class = "footer-section",
                           style = "background: white; padding: 20px; border-radius: 8px;
                                    text-align: center; box-shadow: 0 2px 8px rgba(0,0,0,0.08);
                                    border-top: 3px solid #3498db; margin-top: 20px;",
                           p(style = "color: #7f8c8d; font-size: 14px; margin: 0;",
                             icon("info-circle"), " Dashboard ini dikembangkan untuk memudahkan analisis statistik dengan pendekatan yang robust dan reliable."
                           )
                       )
                )
              )
      ),
      tabItem(tabName = "manajemen",
              fluidRow(
                column(12,
                       div(class = "page-header",
                           style = "background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%);
                                    color: white; padding: 25px 30px; border-radius: 8px;
                                    margin-bottom: 30px; text-align: center;
                                    box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
                           h2(icon("edit"), " Manajemen Data",
                              style = "font-size: 1.8em; font-weight: 400; margin: 0; letter-spacing: 0.5px;"),
                           p("Upload, transform, dan kategorisasi dataset untuk analisis statistik",
                             style = "margin: 8px 0 0 0; opacity: 0.9; font-size: 14px;")
                       )
                )
              ),
              fluidRow(
                class = "equal-height-row",
                style = "margin-bottom: 30px;",
                column(4,
                       div(class = "upload-card",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #3498db; height: 100%;
                                    transition: transform 0.2s ease, box-shadow 0.2s ease;
                                    margin-bottom: 15px;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("file-csv", class = "fa-lg", style = "color: #3498db; margin-right: 10px;"),
                               div(
                                 h4("Data Primer", style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.2em;"),
                                 p("Upload file CSV utama", style = "color: #7f8c8d; font-size: 13px; margin: 5px 0 0 0;")
                               )
                           ),

                           fileInput("file", NULL,
                                     accept = c(".csv"),
                                     buttonLabel = list(icon("upload"), " Pilih File"),
                                     placeholder = "Belum ada file dipilih",
                                     width = "100%"
                           ),

                           div(style = "background: #f8f9fa; padding: 15px; border-radius: 6px; margin-top: 15px;",
                               div(style = "margin-bottom: 15px;",
                                   checkboxInput("header", "Header di baris pertama", TRUE)
                               ),

                               div(style = "margin-bottom: 0;",
                                   tags$label("Pemisah kolom:", style = "font-weight: 500; color: #2c3e50; font-size: 13px;"),
                                   div(style = "margin-top: 8px;",
                                       radioButtons("sep", NULL,
                                                    choices = list("Koma (,)" = ",", "Titik Koma (;)" = ";", "Tab" = "\t"),
                                                    selected = ",",
                                                    inline = TRUE
                                       )
                                   )
                               )
                           )
                       )
                ),

                column(4,
                       div(class = "upload-card",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #27ae60; height: 100%;
                                    transition: transform 0.2s ease, box-shadow 0.2s ease;
                                    margin-bottom: 15px;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("project-diagram", class = "fa-lg", style = "color: #27ae60; margin-right: 10px;"),
                               div(
                                 h4("Data MDS (Optional)", style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.2em;"),
                                 p("Matrix distance", style = "color: #7f8c8d; font-size: 13px; margin: 5px 0 0 0;")
                               )
                           ),

                           fileInput("file_mds", NULL,
                                     accept = c(".csv"),
                                     buttonLabel = list(icon("upload"), " Pilih File"),
                                     placeholder = "Belum ada file dipilih",
                                     width = "100%"
                           ),

                           div(style = "background: #f8f9fa; padding: 15px; border-radius: 6px; margin-top: 15px;",
                               div(style = "margin-bottom: 15px;",
                                   checkboxInput("header2", "Header di baris pertama", TRUE)
                               ),

                               div(style = "margin-bottom: 0;",
                                   tags$label("Pemisah kolom:", style = "font-weight: 500; color: #2c3e50; font-size: 13px;"),
                                   div(style = "margin-top: 8px;",
                                       radioButtons("sep2", NULL,
                                                    choices = list("Koma (,)" = ",", "Titik Koma (;)" = ";", "Tab" = "\t"),
                                                    selected = ",",
                                                    inline = TRUE
                                       )
                                   )
                               )
                           ),
                           p("*Masih belum ada implementasinya dalam dashboard ini", style = "color: #FF1E27; font-weight: bold; font-size: 13px; margin: 5px 0 0 0;")
                       )
                ),

                column(4,
                       div(class = "upload-card",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #e74c3c; height: 100%;
                                    transition: transform 0.2s ease, box-shadow 0.2s ease;
                                    margin-bottom: 15px;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("map-marked-alt", class = "fa-lg", style = "color: #e74c3c; margin-right: 10px;"),
                               div(
                                 h4("Data Geografis", style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.2em;"),
                                 p("File GeoJSON untuk pemetaan", style = "color: #7f8c8d; font-size: 13px; margin: 5px 0 0 0;")
                               )
                           ),

                           fileInput("file_geo", NULL,
                                     accept = c(".json", ".geojson"),
                                     buttonLabel = list(icon("upload"), " Pilih File"),
                                     placeholder = "Belum ada file dipilih",
                                     width = "100%"
                           ),

                           div(style = "background: #f8f9fa; padding: 15px; border-radius: 6px; margin-top: 15px; text-align: center;",
                               div(style = "color: #7f8c8d; font-size: 13px;",
                                   icon("info-circle"), " Format yang didukung:",
                                   br(), "JSON, GeoJSON"
                               )
                           )
                       )
                )
              ),
              tags$style(HTML("
                    .upload-card:hover {
                    transform: translateY(-3px);
                    box-shadow: 0 6px 20px rgba(0,0,0,0.12) !important;
                    }

                    /* Additional margin fixes */
                    .row {
                    margin-left: -15px;
                    margin-right: -15px;
                    }

                    .col-sm-4, .col-sm-6, .col-sm-12 {
                    padding-left: 15px;
                    padding-right: 15px;
                    }

                    /* Responsive margin adjustments */
                    @media (max-width: 768px) {
                    .upload-card {
                        margin-bottom: 20px !important;
                    }
                    }
                ")),
              fluidRow(
                style = "margin-bottom: 30px;",
                column(6,
                       div(class = "processing-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #3498db; margin-bottom: 15px;
                                    min-height: 450px;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("cogs", class = "fa-lg", style = "color: #3498db; margin-right: 10px;"),
                               h3("Transformasi Variabel",
                                  style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                           ),

                           div(style = "background: #f8f9fa; padding: 20px; border-radius: 6px; margin-bottom: 20px;",
                               uiOutput("varSelectUI")
                           ),

                           hr(style = "border-color: #e0e0e0; margin: 20px 0;"),

                           div(style = "margin-bottom: 20px;",
                               h5("Variabel untuk Transformasi:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                               uiOutput("transformSelectUI")
                           ),

                           div(style = "margin-bottom: 20px;",
                               selectInput("transformType", "Jenis Transformasi:",
                                           choices = list(
                                             "Logaritma Natural (ln)" = "log",
                                             "Akar Kuadrat (√)" = "sqrt",
                                             "Kuadrat (x²)" = "square",
                                             "Standardisasi (Z-score)" = "standardize",
                                             "Invers (1/x)" = "inverse"
                                           ),
                                           width = "100%"
                               )
                           ),

                           div(style = "margin-bottom: 20px;",
                               actionButton("apply_transform",
                                            list(icon("play-circle"), " Terapkan Transformasi"),
                                            class = "btn-primary",
                                            style = "width: 100%; padding: 12px;
                                                    background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
                                                    border: none; border-radius: 6px; font-weight: 600;
                                                    box-shadow: 0 3px 10px rgba(52,152,219,0.3);"
                               )
                           ),

                           div(style = "background: #e8f4fd; border-radius: 6px; padding: 15px;
                                        border-left: 4px solid #3498db;",
                               h6("Hasil Transformasi:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                               div(style = "background: white; padding: 12px; border-radius: 4px;
                                            border: 1px solid #e0e0e0; font-family: 'Courier New', monospace;",
                                   verbatimTextOutput("transformResult")
                               )
                           )
                       )
                ),

                column(6,
                       div(class = "processing-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #17a2b8; margin-bottom: 15px;
                                    min-height: 450px;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("tags", class = "fa-lg", style = "color: #17a2b8; margin-right: 10px;"),
                               h3("Kategorisasi Data",
                                  style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                           ),

                           div(style = "background: #f8f9fa; padding: 20px; border-radius: 6px; margin-bottom: 20px;",
                               uiOutput("categVarSelect")
                           ),

                           div(style = "margin-bottom: 20px;",
                               numericInput("numCategories", "Jumlah Kategori:",
                                            value = 3, min = 2, max = 5, width = "100%"
                               )
                           ),

                           div(style = "margin-bottom: 20px;",
                               selectInput("categMethod", "Metode Kategorisasi:",
                                           choices = list(
                                             "Quantile (berdasarkan persentil)" = "quantile",
                                             "Equal Width (interval sama)" = "equal",
                                             "Manual Cut (batas manual)" = "manual"
                                           ),
                                           width = "100%"
                               )
                           ),

                           conditionalPanel(
                             condition = "input.categMethod == 'manual'",
                             div(style = "margin-bottom: 20px;",
                                 textInput("manualBreaks", "Nilai Batas:",
                                           placeholder = "Contoh: 10,50,90",
                                           width = "100%"
                                 )
                             )
                           ),

                           div(style = "margin-bottom: 20px;",
                               actionButton("apply_categ",
                                            list(icon("play-circle"), " Terapkan Kategorisasi"),
                                            class = "btn-info",
                                            style = "width: 100%; padding: 12px;
                                                    background: linear-gradient(135deg, #17a2b8 0%, #138496 100%);
                                                    border: none; border-radius: 6px; font-weight: 600;
                                                    box-shadow: 0 3px 10px rgba(23,162,184,0.3);"
                               )
                           ),

                           div(style = "background: #e8f7f8; border-radius: 6px; padding: 15px;
                                        border-left: 4px solid #17a2b8;",
                               h6("Hasil Kategorisasi:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                               div(style = "background: white; padding: 12px; border-radius: 4px;
                                            border: 1px solid #e0e0e0; font-family: 'Courier New', monospace;",
                                   verbatimTextOutput("categResult")
                               )
                           )
                       )
                )
              ),
              fluidRow(
                column(12,
                       div(class = "table-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #27ae60; margin-bottom: 30px;",

                           div(style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 20px;",
                               div(style = "display: flex; align-items: center;",
                                   icon("table", class = "fa-lg", style = "color: #27ae60; margin-right: 10px;"),
                                   h3("Data Hasil Pemrosesan",
                                      style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                               ),
                               downloadButton("downloadData",
                                              list(" Unduh Data Terproses"),
                                              class = "btn-success",
                                              style = "background: linear-gradient(135deg, #27ae60 0%, #2ecc71 100%); border: none; color: white; padding: 10px 20px; border-radius: 6px; font-weight: 600; box-shadow: 0 3px 10px rgba(39,174,96,0.3);"
                               )
                           ),

                           div(style = "background: #f8f9fa; border-radius: 6px; padding: 15px;
                                        border: 1px solid #e0e0e0;",
                               DTOutput("processedDataTable")
                           )
                       )
                )
              )
      ),
      tabItem(tabName = "eksplorasi",
              fluidRow(
                column(12,
                       div(class = "page-header",
                           style = "background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%);
                                    color: white; padding: 25px 30px; border-radius: 8px;
                                    margin-bottom: 25px; text-align: center;
                                    box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
                           h2(icon("chart-bar"), " Eksplorasi Data",
                              style = "font-size: 1.8em; font-weight: 400; margin: 0; letter-spacing: 0.5px;"),
                           p("Jelajahi karakteristik data dengan statistik deskriptif dan visualisasi interaktif",
                             style = "margin: 8px 0 0 0; opacity: 0.9; font-size: 14px;")
                       )
                )
              ),
              fluidRow(
                column(12,
                       div(class = "map-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #f39c12; margin-bottom: 20px;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("map-marked-alt", class = "fa-lg", style = "color: #f39c12; margin-right: 10px;"),
                               h3("Peta Visualisasi Data Geografis",
                                  style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                           ),

                           div(style = "background: #fff8e1; padding: 15px; border-radius: 6px;
                                        border-left: 4px solid #f39c12; margin-bottom: 20px;",
                               p(icon("info-circle"), " Peta akan ditampilkan jika data mengandung koordinat geografis (latitude/longitude)",
                                 style = "margin: 0; color: #e65100; font-size: 14px;")
                           ),

                           div(style = "margin-bottom: 20px;",
                               h5("Pilih Variabel untuk Visualisasi:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                               uiOutput("mapVarSelect")
                           ),

                           div(style = "border: 2px solid #f5f5f5; border-radius: 8px; padding: 10px; margin-bottom: 20px;",
                               leafletOutput("mapVisualization", height = "500px")
                           ),

                           div(class = "interpretation-box",
                               style = "background: #f8f9fa; border-radius: 6px; padding: 20px;
                                        border-left: 4px solid #f39c12; margin-top: 15px;",
                               h5(icon("lightbulb"), " Interpretasi Peta:",
                                  style = "color: #2c3e50; font-weight: 600; margin-bottom: 12px;"),
                               div(style = "background: white; padding: 15px; border-radius: 4px; border: 1px solid #e0e0e0;",
                                   textOutput("mapInterpretation")
                               )
                           )
                       )
                )
              ),
              fluidRow(
                column(6,
                       div(class = "stats-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #3498db; height: 100%;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("calculator", class = "fa-lg", style = "color: #3498db; margin-right: 10px;"),
                               h3("Statistik Deskriptif",
                                  style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                           ),

                           div(style = "margin-bottom: 15px;",
                               uiOutput("descVarSelect")
                           ),

                           div(style = "margin-bottom: 20px;",
                               h5("Opsi Pengelompokan:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                               div(style = "background: #f8f9fa; padding: 15px; border-radius: 6px;",
                                   radioButtons("descGroupBy", NULL,
                                                choices = list("Tidak dikelompokkan" = "none",
                                                               "Kelompokkan berdasarkan variabel" = "group"),
                                                selected = "none",
                                                inline = FALSE),

                                   conditionalPanel(
                                     condition = "input.descGroupBy == 'group'",
                                     div(style = "margin-top: 10px; padding-top: 10px; border-top: 1px solid #e0e0e0;",
                                         h6("Pilih Variabel Pengelompokan:", style = "color: #2c3e50; font-weight: 500; margin-bottom: 8px;"),
                                         uiOutput("descGroupSelect")
                                     )
                                   )
                               )
                           ),

                           div(style = "margin-bottom: 15px;",
                               h5("Hasil Statistik:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                               div(style = "background: #f8f9fa; border-radius: 6px; padding: 10px;",
                                   DTOutput("descTable")
                               )
                           ),

                           div(class = "interpretation-box",
                               style = "background: #e8f4fd; border-radius: 6px; padding: 20px;
                                        border-left: 4px solid #3498db; margin-top: 15px;",
                               h5(icon("lightbulb"), " Interpretasi Statistik:",
                                  style = "color: #2c3e50; font-weight: 600; margin-bottom: 12px;"),
                               div(style = "background: white; padding: 15px; border-radius: 4px; border: 1px solid #e0e0e0;",
                                   textOutput("descInterpretation")
                               )
                           )
                       )
                ),

                column(6,
                       div(class = "viz-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #27ae60; height: 100%;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("chart-bar", class = "fa-lg", style = "color: #27ae60; margin-right: 10px;"),
                               h3("Visualisasi Data",
                                  style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                           ),

                           div(style = "margin-bottom: 15px;",
                               uiOutput("plotVarSelect")
                           ),

                           div(style = "margin-bottom: 20px;",
                               h5("Jenis Visualisasi:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                               selectInput("plotType", NULL,
                                           choices = list(
                                             "Histogram (Distribusi)" = "hist",
                                             "Boxplot (Sebaran & Outlier)" = "box",
                                             "Density Plot (Kepadatan)" = "density",
                                             "Scatter Plot (Korelasi)" = "scatter",
                                             "Bar Chart (Kategori)" = "bar"
                                           ),
                                           width = "100%")
                           ),
                           conditionalPanel(
                             condition = "input.plotType == 'scatter'",
                             div(style = "background: #f8f9fa; padding: 15px; border-radius: 6px; margin-bottom: 15px;",
                                 h6("Variabel untuk Sumbu Y:", style = "color: #2c3e50; font-weight: 500; margin-bottom: 8px;"),
                                 uiOutput("scatterVarSelect")
                             )
                           ),

                           conditionalPanel(
                             condition = "input.plotType == 'bar'",
                             div(style = "background: #f8f9fa; padding: 15px; border-radius: 6px; margin-bottom: 15px;",
                                 h6("Kelompokkan berdasarkan:", style = "color: #2c3e50; font-weight: 500; margin-bottom: 8px;"),
                                 uiOutput("barGroupSelect")
                             )
                           ),

                           div(style = "margin-bottom: 15px;",
                               h5("Hasil Visualisasi:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                               div(style = "background: #f8f9fa; border-radius: 6px; padding: 15px; text-align: center;",
                                   plotlyOutput("plotEksplorasi", height = "400px")
                               )
                           ),

                           div(class = "interpretation-box",
                               style = "background: #e8f5e8; border-radius: 6px; padding: 20px;
                                        border-left: 4px solid #27ae60; margin-top: 15px;",
                               h5(icon("lightbulb"), " Interpretasi Visualisasi:",
                                  style = "color: #2c3e50; font-weight: 600; margin-bottom: 12px;"),
                               div(style = "background: white; padding: 15px; border-radius: 4px; border: 1px solid #e0e0e0;",
                                   textOutput("plotInterpretation")
                               )
                           )
                       )
                )
              )
      ),
      tabItem(tabName = "asumsi",
              fluidRow(
                column(12,
                       div(class = "page-header",
                           style = "background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%);
                                    color: white; padding: 25px 30px; border-radius: 8px;
                                    margin-bottom: 25px; text-align: center;
                                    box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
                           h2(icon("flask"), " Uji Asumsi Data",
                              style = "font-size: 1.8em; font-weight: 400; margin: 0; letter-spacing: 0.5px;"),
                           p("Validasi asumsi statistik untuk memastikan keakuratan analisis data",
                             style = "margin: 8px 0 0 0; opacity: 0.9; font-size: 14px;")
                       )
                )
              ),
              fluidRow(
                column(6,
                       div(class = "normality-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #dc3545; height: 100%;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("chart-area", class = "fa-lg", style = "color: #dc3545; margin-right: 10px;"),
                               h3("Uji Normalitas",
                                  style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                           ),

                           div(style = "margin-bottom: 15px;",
                               uiOutput("normalVarSelect")
                           ),

                           div(class = "hypothesis-box",
                               style = "background: #f8f9fa; border-radius: 6px; padding: 20px;
                                        border-left: 4px solid #dc3545; margin-bottom: 20px;",
                               h5("Hipotesis Uji Normalitas:", style = "color: #495057; font-weight: bold;"),
                               div(style = "margin-left: 10px;",
                                   p(HTML("<strong>H₀:</strong> Data mengikuti distribusi normal"),
                                     style = "margin: 5px 0; color: #6c757d;"),
                                   p(HTML("<strong>H₁:</strong> Data tidak mengikuti distribusi normal"),
                                     style = "margin: 5px 0; color: #6c757d;"),
                                   hr(style = "margin: 10px 0; border-color: #dee2e6;"),
                                   p(HTML("<strong>Kriteria Keputusan:</strong>"),
                                     style = "margin: 5px 0; color: #495057; font-weight: bold;"),
                                   p(HTML("• Jika <em>p-value</em> > α (0,05) → Terima H₀ (data normal)"),
                                     style = "margin: 2px 0 2px 15px; color: #28a745; font-size: 14px;"),
                                   p(HTML("• Jika <em>p-value</em> ≤ α (0,05) → Tolak H₀ (data tidak normal)"),
                                     style = "margin: 2px 0 2px 15px; color: #dc3545; font-size: 14px;")
                               )
                           ),

                           div(style = "margin-bottom: 20px;",
                               h5("Pilih Uji Normalitas:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                               div(style = "background: #f8f9fa; padding: 15px; border-radius: 6px;",
                                   radioButtons("normTestType", NULL,
                                                choices = c("Shapiro-Wilk" = "shapiro",
                                                            "Kolmogorov-Smirnov" = "ks",
                                                            "Anderson-Darling" = "ad",
                                                            "Jarque-Bera" = "jb"),
                                                selected = "shapiro")
                               )
                           ),

                           div(class = "result-box",
                               style = "background: #fff; border-radius: 6px; padding: 20px;
                                        border: 1px solid #e0e0e0; margin-bottom: 20px;",
                               div(class = "result-header",
                                   style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                   h4("Uji Normalitas",
                                      style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.2em;"),
                                   downloadButton("downloadNormality", "Download .docx",
                                                  style = "background: #dc3545; color: white; border: none;
                                    padding: 8px 16px; border-radius: 4px; font-size: 12px;",
                                                  icon = icon("download"))
                               ),
                               div(class = "result-content",
                                   style = "background: #f8f9fa; padding: 15px; border-radius: 4px; border: 1px solid #e0e0e0;",
                                   verbatimTextOutput("normalityTest")
                               )
                           ),

                           div(style = "margin-bottom: 20px;",
                               h5("Q-Q Plot:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                               div(style = "background: #f8f9fa; border-radius: 6px; padding: 15px; text-align: center;",
                                   plotOutput("qqPlot", height = "300px")
                               )
                           ),

                           div(class = "interpretation-box",
                               style = "background: #fef5e7; border-radius: 6px; padding: 20px;
                                        border-left: 4px solid #dc3545; margin-top: 15px;",
                               h5(icon("lightbulb"), " Interpretasi:",
                                  style = "color: #2c3e50; font-weight: 600; margin-bottom: 12px;"),
                               div(style = "background: white; padding: 15px; border-radius: 4px; border: 1px solid #e0e0e0;",
                                   textOutput("interpretasiNormalitas")
                               )
                           )
                       )
                ),

                column(6,
                       div(class = "homogeneity-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #dc3545; height: 100%;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("balance-scale", class = "fa-lg", style = "color: #dc3545; margin-right: 10px;"),
                               h3("Uji Homogenitas Antar Kategori",
                                  style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                           ),

                           div(style = "margin-bottom: 15px;",
                               uiOutput("homogenVarSelect")
                           ),

                           div(style = "margin-bottom: 15px;",
                               h5("Kelompokkan berdasarkan:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                               uiOutput("homogenGroupSelect")
                           ),

                           div(class = "hypothesis-box",
                               style = "background: #f8f9fa; border-radius: 6px; padding: 20px;
                                        border-left: 4px solid #dc3545; margin-bottom: 20px;",
                               h5("Hipotesis Uji Homogenitas:", style = "color: #495057; font-weight: bold;"),
                               div(style = "margin-left: 10px;",
                                   p(HTML("<strong>H₀:</strong> Varians antar grup adalah homogen (σ₁² = σ₂² = ... = σₖ²)"),
                                     style = "margin: 5px 0; color: #6c757d;"),
                                   p(HTML("<strong>H₁:</strong> Minimal ada satu varians grup yang berbeda"),
                                     style = "margin: 5px 0; color: #6c757d;"),
                                   hr(style = "margin: 10px 0; border-color: #dee2e6;"),
                                   p(HTML("<strong>Kriteria Keputusan:</strong>"),
                                     style = "margin: 5px 0; color: #495057; font-weight: bold;"),
                                   p(HTML("• Jika <em>p-value</em> > α (0,05) → Terima H₀ (varians homogen)"),
                                     style = "margin: 2px 0 2px 15px; color: #28a745; font-size: 14px;"),
                                   p(HTML("• Jika <em>p-value</em> ≤ α (0,05) → Tolak H₀ (varians tidak homogen)"),
                                     style = "margin: 2px 0 2px 15px; color: #dc3545; font-size: 14px;")
                               )
                           ),

                           div(style = "margin-bottom: 20px;",
                               h5("Pilih Uji Homogenitas:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                               div(style = "background: #f8f9fa; padding: 15px; border-radius: 6px;",
                                   radioButtons("homogenTestType", NULL,
                                                choices = c("Levene" = "levene",
                                                            "Bartlett" = "bartlett"),
                                                selected = "levene")
                               )
                           ),

                           div(class = "result-box",
                               style = "background: #fff; border-radius: 6px; padding: 20px;
                                        border: 1px solid #e0e0e0; margin-bottom: 20px;",
                               div(class = "result-header",
                                   style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                   h4("Uji Homogenitas",
                                      style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.2em;"),
                                   downloadButton("downloadHomogeneity", "Download .docx",
                                                  style = "background: #dc3545; color: white; border: none;
                                    padding: 8px 16px; border-radius: 4px; font-size: 12px;",
                                                  icon = icon("download"))
                               ),
                               div(class = "result-content",
                                   style = "background: #f8f9fa; padding: 15px; border-radius: 4px; border: 1px solid #e0e0e0;",
                                   verbatimTextOutput("homogeneityTest")
                               )
                           ),

                           div(style = "margin-bottom: 20px;",
                               h5("Visualisasi:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                               div(style = "background: #f8f9fa; border-radius: 6px; padding: 15px; text-align: center;",
                                   plotOutput("homogeneityPlot", height = "300px")
                               )
                           ),

                           div(class = "interpretation-box",
                               style = "background: #fef5e7; border-radius: 6px; padding: 20px;
                                        border-left: 4px solid #dc3545; margin-top: 15px;",
                               h5(icon("lightbulb"), " Interpretasi:",
                                  style = "color: #2c3e50; font-weight: 600; margin-bottom: 12px;"),
                               div(style = "background: white; padding: 15px; border-radius: 4px; border: 1px solid #e0e0e0;",
                                   textOutput("interpretasiHomogenitas")
                               )
                           )
                       )
                )
              ),
              fluidRow(
                column(12,
                       div(class = "multi-homogeneity-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #007bff; margin-top: 20px;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("equals", class = "fa-lg", style = "color: #007bff; margin-right: 10px;"),
                               h3("Uji Homogenitas Antar Variabel",
                                  style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                           ),

                           fluidRow(
                             column(4,
                                    div(style = "background: #f8f9fa; border-radius: 6px; padding: 20px; margin-bottom: 20px;",
                                        h5("Pengaturan Analisis:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),

                                        div(style = "margin-bottom: 15px;",
                                            uiOutput("homogenVarMultiSelect")
                                        ),

                                        div(style = "margin-bottom: 15px;",
                                            h6("Pilih Jenis Uji:", style = "color: #495057; font-weight: 500; margin-bottom: 8px;"),
                                            selectInput("homogenTestTypeMulti", NULL,
                                                        choices = list(
                                                          "Levene Test" = "levene",
                                                          "Bartlett Test" = "bartlett"
                                                        ),
                                                        selected = "levene")
                                        )
                                    ),

                                    div(class = "hypothesis-box",
                                        style = "background: #e7f3ff; border-radius: 6px; padding: 20px;
                                                border-left: 4px solid #007bff;",
                                        h5("Hipotesis Uji Homogenitas Antar Variabel:", style = "color: #495057; font-weight: bold;"),
                                        div(style = "margin-left: 10px;",
                                            p(HTML("<strong>H₀:</strong> Varians antar variabel adalah homogen (σ²<sub>var1</sub> = σ²<sub>var2</sub> = ... = σ²<sub>vark</sub>)"),
                                              style = "margin: 5px 0; color: #6c757d;"),
                                            p(HTML("<strong>H₁:</strong> Minimal ada satu varians variabel yang berbeda"),
                                              style = "margin: 5px 0; color: #6c757d;"),
                                            hr(style = "margin: 10px 0; border-color: #dee2e6;"),
                                            p(HTML("<strong>Kriteria Keputusan:</strong>"),
                                              style = "margin: 5px 0; color: #495057; font-weight: bold;"),
                                            p(HTML("• Jika <em>p-value</em> > α (0,05) → Terima H₀ (varians homogen)"),
                                              style = "margin: 2px 0 2px 15px; color: #28a745; font-size: 14px;"),
                                            p(HTML("• Jika <em>p-value</em> ≤ α (0,05) → Tolak H₀ (varians tidak homogen)"),
                                              style = "margin: 2px 0 2px 15px; color: #dc3545; font-size: 14px;")
                                        )
                                    )
                             ),

                             column(8,
                                    h4("Hasil Uji Homogenitas Antar Variabel",
                                       style = "color: #2c3e50; font-weight: 600; margin-bottom: 20px;"),
                                    div(class = "result-box",
                                        style = "background: #fff; border-radius: 6px; padding: 20px;
                                                border: 1px solid #e0e0e0; margin-bottom: 20px;",
                                        div(class = "result-header",
                                            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                            h4("Uji Homogenitas Antar Variabel",
                                               style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.2em;"),
                                            downloadButton("downloadHomogeneityMulti", "Download .docx",
                                                           style = "background: #007bff; color: white; border: none;
                                        padding: 8px 16px; border-radius: 4px; font-size: 12px;",
                                                           icon = icon("download"))
                                        ),
                                        div(class = "result-content",
                                            style = "background: #f8f9fa; padding: 15px; border-radius: 4px; border: 1px solid #e0e0e0;",
                                            verbatimTextOutput("homogeneityTestMulti")
                                        )
                                    )
                             )
                           ),

                           hr(style = "margin: 30px 0; border-color: #dee2e6;"),

                           fluidRow(
                             column(6,
                                    h4("Visualisasi",
                                       style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                                    div(style = "background: #f8f9fa; border-radius: 6px; padding: 15px; text-align: center;",
                                        plotOutput("homogeneityPlotMulti", height = "400px")
                                    )
                             ),

                             column(6,
                                    h4("Interpretasi",
                                       style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                                    div(class = "interpretation-box",
                                        style = "background: #e7f3ff; border-radius: 6px; padding: 20px;
                                                border-left: 4px solid #007bff; height: 420px;",
                                        div(style = "background: white; padding: 15px; border-radius: 4px; border: 1px solid #e0e0e0; height: 100%;",
                                            textOutput("interpretasiHomogenitasMulti")
                                        )
                                    )
                             )
                           )
                       )
                )
              )
      ),
      tabItem(tabName = "resampling",
              fluidRow(
                column(12,
                       div(class = "page-header",
                           style = "background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%);
                                    color: white; padding: 25px 30px; border-radius: 8px;
                                    margin-bottom: 25px; text-align: center;
                                    box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
                           h2(icon("random"), " Metode Resampling",
                              style = "font-size: 1.8em; font-weight: 400; margin: 0; letter-spacing: 0.5px;"),
                           p("Teknik inferensi statistik non-parametrik dengan bootstrap, jackknife, dan permutasi",
                             style = "margin: 8px 0 0 0; opacity: 0.9; font-size: 14px;")
                       )
                )
              ),
              fluidRow(
                column(12,
                       div(class = "jackknife-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #007bff; margin-bottom: 20px;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("circle-check", class = "fa-lg", style = "color: #007bff; margin-right: 10px;"),
                               h3("Jackknife Resampling",
                                  style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                           ),

                           fluidRow(
                             column(12,
                                    div(style = "background: #f8f9fa; border-radius: 6px; padding: 20px; margin-bottom: 20px;",
                                        fluidRow(
                                          column(4,
                                                 uiOutput("jackknifeVarSelect")
                                          ),
                                          column(4,
                                                 h5("Pilih Statistik:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                                                 selectInput("jackknifeStatistic", NULL,
                                                             choices = list("Mean" = "mean",
                                                                            "Median" = "median",
                                                                            "Standard Deviation" = "sd",
                                                                            "Varians" = "var",
                                                                            "IQR" = "iqr"))
                                          ),
                                          column(4,
                                                 h5("Eksekusi:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                                                 div(style = "margin-top: 8px;",
                                                     actionButton("runJackknife", "Jalankan Jackknife",
                                                                  style = "background: #007bff; color: white; border: none;
                                                                        padding: 10px 20px; border-radius: 5px; font-weight: 500;",
                                                                  icon = icon("play-circle"))
                                                 )
                                          )
                                        )
                                    )
                             )
                           ),

                           div(style = "border-top: 1px solid #e0e0e0; padding-top: 20px;",
                               fluidRow(
                                 column(6,
                                        div(class = "result-box",
                                            style = "background: #fff; border-radius: 6px; padding: 20px;
                                                    border: 1px solid #e0e0e0; margin-bottom: 20px;",
                                            div(class = "result-header",
                                                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                                h4("Hasil Jackknife Resampling",
                                                   style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.2em;"),
                                                downloadButton("downloadJackknife", "Download .docx",
                                                               style = "background: #007bff; color: white; border: none; padding: 8px 16px; border-radius: 4px; font-size: 12px;",
                                                               icon = icon("download"))
                                            ),
                                            div(class = "result-content",
                                                style = "background: #f8f9fa; padding: 15px; border-radius: 4px; border: 1px solid #e0e0e0;",
                                                verbatimTextOutput("jackknifeResult")
                                            )
                                        )
                                 ),
                                 column(6,
                                        div(style = "margin-bottom: 20px;",
                                            h4("Visualisasi",
                                               style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                                            div(style = "background: #f8f9fa; border-radius: 6px; padding: 15px; text-align: center;",
                                                plotOutput("jackknifePlot", height = "300px")
                                            )
                                        ),
                                        div(class = "interpretation-box",
                                            style = "background: #e7f3ff; border-radius: 6px; padding: 20px;
                                                    border-left: 4px solid #007bff;",
                                            h5(icon("lightbulb"), " Interpretasi:",
                                               style = "color: #2c3e50; font-weight: 600; margin-bottom: 12px;"),
                                            div(style = "background: white; padding: 15px; border-radius: 4px; border: 1px solid #e0e0e0;",
                                                textOutput("jackknifeInterpretation")
                                            )
                                        )
                                 )
                               )
                           )
                       )
                )
              ),
              fluidRow(
                column(12,
                       div(class = "bootstrap-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #28a745; margin-bottom: 20px;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("bootstrap", class = "fa-lg", style = "color: #28a745; margin-right: 10px;"),
                               h3("Bootstrap Resampling",
                                  style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                           ),

                           fluidRow(
                             column(12,
                                    div(style = "background: #f8f9fa; border-radius: 6px; padding: 20px; margin-bottom: 20px;",
                                        fluidRow(
                                          column(3,
                                                 uiOutput("bootstrapVarSelect")
                                          ),
                                          column(3,
                                                 h5("Pilih Statistik:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                                                 selectInput("bootstrapStatistic", NULL,
                                                             choices = list("Mean" = "mean",
                                                                            "Median" = "median",
                                                                            "Standard Deviation" = "sd",
                                                                            "Varians" = "var",
                                                                            "IQR" = "iqr"))
                                          ),
                                          column(2,
                                                 h5("Replikasi:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                                                 numericInput("bootstrapR", NULL,
                                                              value = 1000, min = 100, max = 10000)
                                          ),
                                          column(2,
                                                 h5("Confidence Interval:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                                                 selectInput("bootstrapCI", NULL,
                                                             choices = c("95%" = 0.95, "90%" = 0.90, "99%" = 0.99),
                                                             selected = 0.95)
                                          ),
                                          column(2,
                                                 h5("Eksekusi:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                                                 div(style = "margin-top: 8px;",
                                                     actionButton("runBootstrap", "Jalankan Bootstrap",
                                                                  style = "background: #28a745; color: white; border: none; padding: 10px 20px; border-radius: 5px; font-weight: 500;",
                                                                  icon = icon("play-circle"))
                                                 )
                                          )
                                        )
                                    )
                             )
                           ),

                           div(style = "border-top: 1px solid #e0e0e0; padding-top: 20px;",
                               fluidRow(
                                 column(6,
                                        div(class = "result-box",
                                            style = "background: #fff; border-radius: 6px; padding: 20px;
                                                    border: 1px solid #e0e0e0; margin-bottom: 20px;",
                                            div(class = "result-header",
                                                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                                h4("Hasil Bootstrap Resampling",
                                                   style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.2em;"),
                                                downloadButton("downloadBootstrap", "Download .docx",
                                                               style = "background: #28a745; color: white; border: none; padding: 8px 16px; border-radius: 4px; font-size: 12px;",
                                                               icon = icon("download"))
                                            ),
                                            div(class = "result-content",
                                                style = "background: #f8f9fa; padding: 15px; border-radius: 4px; border: 1px solid #e0e0e0;",
                                                verbatimTextOutput("bootstrapResult")
                                            )
                                        )
                                 ),
                                 column(6,
                                        div(style = "margin-bottom: 20px;",
                                            h4("Visualisasi",
                                               style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                                            div(style = "background: #f8f9fa; border-radius: 6px; padding: 15px; text-align: center;",
                                                plotOutput("bootstrapPlot", height = "300px")
                                            )
                                        ),
                                        div(class = "interpretation-box",
                                            style = "background: #e8f5e8; border-radius: 6px; padding: 20px;
                                                    border-left: 4px solid #28a745;",
                                            h5(icon("lightbulb"), " Interpretasi:",
                                               style = "color: #2c3e50; font-weight: 600; margin-bottom: 12px;"),
                                            div(style = "background: white; padding: 15px; border-radius: 4px; border: 1px solid #e0e0e0;",
                                                textOutput("bootstrapInterpretation")
                                            )
                                        )
                                 )
                               )
                           )
                       )
                )
              ),
              fluidRow(
                column(12,
                       div(class = "permutation-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #ffc107; margin-bottom: 20px;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("shuffle", class = "fa-lg", style = "color: #ffc107; margin-right: 10px;"),
                               h3("Permutation Test",
                                  style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                           ),

                           fluidRow(
                             column(12,
                                    div(style = "background: #f8f9fa; border-radius: 6px; padding: 20px; margin-bottom: 20px;",
                                        fluidRow(
                                          column(3,
                                                 uiOutput("permutationVarSelect")
                                          ),
                                          column(3,
                                                 h5("Kelompokkan berdasarkan:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                                                 uiOutput("permutationGroupSelect")
                                          ),
                                          column(2,
                                                 h5("Jumlah Permutasi:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                                                 numericInput("permutationR", NULL,
                                                              value = 999, min = 99, max = 9999)
                                          ),
                                          column(2,
                                                 h5("Hipotesis Alternatif:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                                                 div(style = "background: white; padding: 10px; border-radius: 4px; border: 1px solid #dee2e6;",
                                                     radioButtons("permutationAlternative", NULL,
                                                                  choices = c("Two-sided" = "two.sided",
                                                                              "Greater" = "greater",
                                                                              "Less" = "less"),
                                                                  selected = "two.sided")
                                                 )
                                          ),
                                          column(2,
                                                 h5("Eksekusi:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                                                 div(style = "margin-top: 8px;",
                                                     actionButton("runPermutation", "Jalankan Permutasi",
                                                                  style = "background: #ffc107; color: #212529; border: none;
                                          padding: 10px 20px; border-radius: 5px; font-weight: 500;",
                                                                  icon = icon("play-circle"))
                                                 )
                                          )
                                        )
                                    )
                             )
                           ),

                           div(style = "border-top: 1px solid #e0e0e0; padding-top: 20px;",
                               fluidRow(
                                 column(6,
                                        div(class = "result-box",
                                            style = "background: #fff; border-radius: 6px; padding: 20px;
                                                    border: 1px solid #e0e0e0; margin-bottom: 20px;",
                                            div(class = "result-header",
                                                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                                h4("Hasil Uji Permutasi",
                                                   style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.2em;"),
                                                downloadButton("downloadPermutation", "Download .docx",
                                                               style = "background: #ffc107; color: #212529; border: none;
                                          padding: 8px 16px; border-radius: 4px; font-size: 12px;",
                                                               icon = icon("download"))
                                            ),
                                            div(class = "result-content",
                                                style = "background: #f8f9fa; padding: 15px; border-radius: 4px; border: 1px solid #e0e0e0;",
                                                verbatimTextOutput("permutationResult")
                                            )
                                        )
                                 ),
                                 column(6,
                                        div(style = "margin-bottom: 20px;",
                                            h4("Visualisasi",
                                               style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                                            div(style = "background: #f8f9fa; border-radius: 6px; padding: 15px; text-align: center;",
                                                plotOutput("permutationPlot", height = "300px")
                                            )
                                        ),
                                        div(class = "interpretation-box",
                                            style = "background: #fff8e1; border-radius: 6px; padding: 20px;
                                                    border-left: 4px solid #ffc107;",
                                            h5(icon("lightbulb"), " Interpretasi:",
                                               style = "color: #2c3e50; font-weight: 600; margin-bottom: 12px;"),
                                            div(style = "background: white; padding: 15px; border-radius: 4px; border: 1px solid #e0e0e0;",
                                                textOutput("permutationInterpretation")
                                            )
                                        )
                                 )
                               )
                           )
                       )
                )
              )
      ),
      tabItem(tabName = "inferensia",
              fluidRow(
                column(12,
                       div(class = "page-header",
                           style = "background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%);
                                    color: white; padding: 25px 30px; border-radius: 8px;
                                    margin-bottom: 25px; text-align: center;
                                    box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
                           h2(icon("flask"), " Inferensia Statistik",
                              style = "font-size: 1.8em; font-weight: 400; margin: 0; letter-spacing: 0.5px;"),
                           p("Lakukan berbagai uji statistik untuk menguji hipotesis dan membuat kesimpulan dari data",
                             style = "margin: 8px 0 0 0; opacity: 0.9; font-size: 14px;")
                       )
                )
              ),
              fluidRow(
                column(4,
                       div(class = "settings-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #3498db; height: fit-content;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("cogs", class = "fa-lg", style = "color: #3498db; margin-right: 10px;"),
                               h3("Pengaturan Uji Statistik",
                                  style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                           ),

                           div(style = "margin-bottom: 20px;",
                               h5("Jenis Uji Statistik:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 10px;"),
                               selectInput("uji_statistik", NULL, choices = c(
                                 "Uji Rata-rata 1 Populasi",
                                 "Uji Rata-rata 2 Populasi (Independen)",
                                 "Uji Rata-rata 2 Populasi (Berpasangan)",
                                 "Uji Proporsi 1 Populasi",
                                 "Uji Proporsi 2 Populasi",
                                 "Uji Ragam 1 Populasi",
                                 "Uji Ragam 2 Populasi",
                                 "Uji ANOVA 1 Arah",
                                 "Uji ANOVA 2 Arah"
                               ), width = "100%")
                           ),

                           div(style = "border-top: 1px solid #e0e0e0; padding-top: 15px; margin: 20px 0;"),

                           div(style = "background: #f8f9fa; padding: 15px; border-radius: 6px; margin-bottom: 20px;",
                               uiOutput("inferensia_input_ui")
                           ),

                           div(style = "border-top: 1px solid #e0e0e0; padding-top: 15px; margin: 20px 0;"),

                           div(style = "margin-bottom: 20px;",
                               h5("Pengaturan Uji:", style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),

                               div(style = "margin-bottom: 15px;",
                                   h6("Hipotesis Alternatif:", style = "color: #2c3e50; font-weight: 500; margin-bottom: 8px;"),
                                   selectInput("alternative", NULL,
                                               choices = c("Dua sisi" = "two.sided",
                                                           "Lebih besar" = "greater",
                                                           "Lebih kecil" = "less"),
                                               selected = "two.sided", width = "100%")
                               ),

                               div(style = "margin-bottom: 15px;",
                                   h6("Taraf Signifikansi (α):", style = "color: #2c3e50; font-weight: 500; margin-bottom: 8px;"),
                                   numericInput("alpha", NULL,
                                                0.05, min = 0.001, max = 0.1, step = 0.01, width = "100%")
                               )
                           ),

                           div(style = "margin-top: 25px;",
                               actionButton("ujiTombol", "Lakukan Uji Statistik",
                                            icon = icon("flask"),
                                            style = "background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
                                                    color: white; border: none; padding: 12px 20px;
                                                    border-radius: 6px; font-weight: 600; width: 100%;
                                                    transition: all 0.3s ease; font-size: 14px;
                                                    box-shadow: 0 2px 8px rgba(52, 152, 219, 0.3);",
                                            onmouseover = "this.style.transform='translateY(-2px)'; this.style.boxShadow='0 4px 12px rgba(52, 152, 219, 0.4)';",
                                            onmouseout = "this.style.transform='translateY(0px)'; this.style.boxShadow='0 2px 8px rgba(52, 152, 219, 0.3)';")
                           )
                       )
                ),

                column(8,
                       div(class = "results-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #2ecc71; min-height: 500px;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("chart-line", class = "fa-lg", style = "color: #2ecc71; margin-right: 10px;"),
                               h3("📑 Hasil Uji Statistik",
                                  style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                           ),

                           div(style = "background: #f8f9fa; border-radius: 8px; padding: 0; margin-bottom: 20px; border: 1px solid #e0e0e0;",
                               div(style = "background: #ffffff; border-bottom: 1px solid #e0e0e0; border-radius: 8px 8px 0 0; padding: 15px 20px;",
                                   h4("Hasil Uji", style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.2em;")
                               ),
                               div(style = "padding: 20px;",
                                   div(class = "result-header",
                                       style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px;
                                                padding-bottom: 15px; border-bottom: 2px solid #e8f5e8;",
                                       h4("Hasil Uji Statistik",
                                          style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.3em;"),
                                       downloadButton("downloadHasilUji", "Download .docx",
                                                      icon = icon("download"),
                                                      style = "background: linear-gradient(135deg, #27ae60 0%, #229954 100%);
                                                                color: white; border: none; padding: 8px 16px;
                                                                border-radius: 4px; font-weight: 500; font-size: 13px;
                                                                transition: all 0.3s ease;
                                                                box-shadow: 0 2px 6px rgba(39, 174, 96, 0.3);",
                                                      onmouseover = "this.style.transform='translateY(-1px)'; this.style.boxShadow='0 3px 8px rgba(39, 174, 96, 0.4)';",
                                                      onmouseout = "this.style.transform='translateY(0px)'; this.style.boxShadow='0 2px 6px rgba(39, 174, 96, 0.3)';")
                                   ),
                                   div(class = "result-content",
                                       style = "background: white; border-radius: 6px; padding: 20px;
                                                border: 2px solid #f0f0f0; margin-bottom: 20px;
                                                font-family: 'Courier New', monospace;",
                                       verbatimTextOutput("hasilUji")
                                   ),
                                   div(class = "interpretation-section",
                                       style = "background: #e8f6fd; border-radius: 6px; padding: 20px;
                                                border-left: 4px solid #2ecc71; margin-top: 15px;",
                                       h5(icon("lightbulb"), " Interpretasi:",
                                          style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px; font-size: 1.1em;"),
                                       div(style = "background: white; padding: 15px; border-radius: 4px;
                                                    border: 1px solid #e0e0e0; font-family: 'Courier New', monospace;",
                                           verbatimTextOutput("interpretasiUji")
                                       )
                                   )
                               )
                           )
                       )
                )
              )
      ),
      tabItem(tabName = "regresi",
              fluidRow(
                column(12,
                       div(class = "page-header",
                           style = "background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%);
                                    color: white; padding: 25px 30px; border-radius: 8px;
                                    margin-bottom: 25px; text-align: center;
                                    box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
                           h2(icon("chart-line"), " Regresi Linear Berganda",
                              style = "font-size: 1.8em; font-weight: 400; margin: 0; letter-spacing: 0.5px;"),
                           p("Analisis hubungan linear antara variabel dependen dan independen dengan uji asumsi lengkap",
                             style = "margin: 8px 0 0 0; opacity: 0.9; font-size: 14px;")
                       )
                )
              ),
              fluidRow(
                column(12,
                       div(class = "config-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #e74c3c; margin-bottom: 20px;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("cogs", class = "fa-lg", style = "color: #e74c3c; margin-right: 10px;"),
                               h3("Pengaturan Regresi Linear Berganda",
                                  style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                           ),

                           fluidRow(
                             column(6,
                                    div(style = "background: #f8f9fa; padding: 20px; border-radius: 6px; margin-right: 10px;",
                                        uiOutput("var_y_mlr")
                                    )
                             ),
                             column(6,
                                    div(style = "background: #f8f9fa; padding: 20px; border-radius: 6px; margin-left: 10px;",
                                        uiOutput("var_x_mlr")
                                    )
                             )
                           ),

                           br(),
                           div(class = "text-center",
                               actionButton("btn_run_mlr", "Jalankan Regresi",
                                            class = "btn-success btn-lg",
                                            icon = icon("play"),
                                            style = "background: linear-gradient(135deg, #27ae60 0%, #2ecc71 100%);
                                                    border: none; padding: 12px 30px; border-radius: 6px;
                                                    font-weight: 600; box-shadow: 0 3px 10px rgba(39,174,96,0.3);
                                                    transition: all 0.3s ease;")
                           )
                       )
                )
              ),
              fluidRow(
                column(12,
                       div(class = "results-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #27ae60; margin-bottom: 20px;",

                           div(style = "display: flex; align-items: center; margin-bottom: 20px;",
                               icon("chart-bar", class = "fa-lg", style = "color: #27ae60; margin-right: 10px;"),
                               h3("Hasil Analisis",
                                  style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                           ),

                           tabsetPanel(
                             id = "mlr_tabs",
                             tabPanel("Ringkasan Model",
                                      br(),
                                      div(class = "result-box",
                                          style = "background: #f8f9fa; border-radius: 6px; padding: 20px;
                                                    border-left: 4px solid #3498db; margin-bottom: 15px;",
                                          div(class = "result-header",
                                              style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                              h4("Ringkasan Model MLR",
                                                 style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.2em;"),
                                              downloadButton("downloadMLR", "Download .docx",
                                                             icon = icon("download"),
                                                             style = "background: #3498db; border: none; color: white;
                                                                    padding: 8px 15px; border-radius: 4px; font-size: 12px;
                                                                    box-shadow: 0 2px 5px rgba(52,152,219,0.3);")
                                          ),
                                          div(class = "result-content",
                                              style = "background: white; padding: 15px; border-radius: 4px;
                                                        border: 1px solid #e0e0e0; font-family: 'Courier New', monospace;",
                                              verbatimTextOutput("mlr_summary")
                                          )
                                      )
                             ),
                             tabPanel("Uji Asumsi",
                                      br(),
                                      div(style = "margin-bottom: 25px;",
                                          h4("1. Uji Normalitas Residual",
                                             style = "color: #3498db; font-weight: 600; margin-bottom: 15px;"),
                                          div(class = "result-box",
                                              style = "background: #e8f4fd; border-radius: 6px; padding: 20px;
                                                        border-left: 4px solid #3498db; margin-bottom: 15px;",
                                              div(class = "result-header",
                                                  style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                                  h5("Uji Normalitas Residual",
                                                     style = "color: #2c3e50; font-weight: 600; margin: 0;"),
                                                  downloadButton("downloadNormalitasMLR", "Download .docx",
                                                                 icon = icon("download"),
                                                                 style = "background: #3498db; border: none; color: white; padding: 6px 12px; border-radius: 4px; font-size: 11px;")
                                              ),
                                              div(class = "result-content",
                                                  style = "background: white; padding: 15px; border-radius: 4px;
                                                            border: 1px solid #e0e0e0; font-family: 'Courier New', monospace;",
                                                  verbatimTextOutput("uji_normalitas")
                                              )
                                          )
                                      ),
                                      div(style = "margin-bottom: 25px;",
                                          h4("2. Uji Multikolinearitas",
                                             style = "color: #3498db; font-weight: 600; margin-bottom: 15px;"),
                                          div(class = "result-box",
                                              style = "background: #e8f4fd; border-radius: 6px; padding: 20px;
                                                        border-left: 4px solid #3498db; margin-bottom: 15px;",
                                              div(class = "result-header",
                                                  style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                                  h5("Uji Multikolinearitas (VIF)",
                                                     style = "color: #2c3e50; font-weight: 600; margin: 0;"),
                                                  downloadButton("downloadMultikolinearitas", "Download .docx",
                                                                 icon = icon("download"),
                                                                 style = "background: #3498db; border: none; color: white;
                                          padding: 6px 12px; border-radius: 4px; font-size: 11px;")
                                              ),
                                              div(class = "result-content",
                                                  style = "background: white; padding: 15px; border-radius: 4px;
                                                            border: 1px solid #e0e0e0; font-family: 'Courier New', monospace;",
                                                  verbatimTextOutput("uji_multikolinearitas")
                                              )
                                          )
                                      ),
                                      div(style = "margin-bottom: 25px;",
                                          h4("3. Uji Homoskedastisitas",
                                             style = "color: #3498db; font-weight: 600; margin-bottom: 15px;"),
                                          div(class = "result-box",
                                              style = "background: #e8f4fd; border-radius: 6px; padding: 20px;
                                                        border-left: 4px solid #3498db; margin-bottom: 15px;",
                                              div(class = "result-header",
                                                  style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                                  h5("Uji Homoskedastisitas",
                                                     style = "color: #2c3e50; font-weight: 600; margin: 0;"),
                                                  downloadButton("downloadHomoskedastisitas", "Download .docx",
                                                                 icon = icon("download"),
                                                                 style = "background: #3498db; border: none; color: white; padding: 6px 12px; border-radius: 4px; font-size: 11px;")
                                              ),
                                              div(class = "result-content",
                                                  style = "background: white; padding: 15px; border-radius: 4px;
                                                            border: 1px solid #e0e0e0; font-family: 'Courier New', monospace;",
                                                  verbatimTextOutput("uji_homoskedastisitas")
                                              )
                                          )
                                      ),
                                      div(style = "margin-bottom: 25px;",
                                          h4("4. Uji Autokorelasi",
                                             style = "color: #3498db; font-weight: 600; margin-bottom: 15px;"),
                                          div(class = "result-box",
                                              style = "background: #e8f4fd; border-radius: 6px; padding: 20px;
                                                        border-left: 4px solid #3498db; margin-bottom: 15px;",
                                              div(class = "result-header",
                                                  style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                                  h5("Uji Autokorelasi",
                                                     style = "color: #2c3e50; font-weight: 600; margin: 0;"),
                                                  downloadButton("downloadAutokorelasi", "Download .docx",
                                                                 icon = icon("download"),
                                                                 style = "background: #3498db; border: none; color: white; padding: 6px 12px; border-radius: 4px; font-size: 11px;")
                                              ),
                                              div(class = "result-content",
                                                  style = "background: white; padding: 15px; border-radius: 4px;
                           b                                order: 1px solid #e0e0e0; font-family: 'Courier New', monospace;",
                                                  verbatimTextOutput("uji_autokorelasi")
                                              )
                                          )
                                      )
                             ),
                             tabPanel("Plot Diagnostik",
                                      br(),
                                      div(style = "margin-bottom: 30px;",
                                          h4("Plot Diagnostik Lengkap",
                                             style = "text-align: center; color: #2c3e50; font-weight: 600; margin-bottom: 20px;"),
                                          div(style = "background: #f8f9fa; border-radius: 8px; padding: 20px;
                                                        border: 2px solid #e0e0e0;",
                                              plotOutput("plot_diagnostik", height = "600px")
                                          )
                                      ),

                                      hr(style = "border-color: #e0e0e0; margin: 30px 0;"),
                                      fluidRow(
                                        column(4,
                                               div(style = "background: white; border-radius: 8px; padding: 15px;
                                                            box-shadow: 0 2px 8px rgba(0,0,0,0.05); margin-bottom: 15px;",
                                                   h5("Residuals vs Fitted",
                                                      style = "text-align: center; color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                                                   plotOutput("plot_residual_fitted", height = "300px")
                                               )
                                        ),
                                        column(4,
                                               div(style = "background: white; border-radius: 8px; padding: 15px;
                                                            box-shadow: 0 2px 8px rgba(0,0,0,0.05); margin-bottom: 15px;",
                                                   h5("Q-Q Plot Normalitas",
                                                      style = "text-align: center; color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                                                   plotOutput("plot_qq", height = "300px")
                                               )
                                        ),
                                        column(4,
                                               div(style = "background: white; border-radius: 8px; padding: 15px;
                                                            box-shadow: 0 2px 8px rgba(0,0,0,0.05); margin-bottom: 15px;",
                                                   h5("Histogram Residuals",
                                                      style = "text-align: center; color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                                                   plotOutput("plot_histogram_residual", height = "300px")
                                               )
                                        )
                                      )
                             ),
                             tabPanel("Prediksi",
                                      br(),
                                      fluidRow(
                                        column(6,
                                               div(style = "background: #f8f9fa; border-radius: 8px; padding: 20px;
                                                            border-left: 4px solid #f39c12; margin-right: 10px;",
                                                   h4("Input Prediksi",
                                                      style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                                                   uiOutput("prediksi_ui")
                                               )
                                        ),
                                        column(6,
                                               div(style = "background: white; border-radius: 8px; padding: 20px;
                                                            box-shadow: 0 2px 8px rgba(0,0,0,0.05); margin-left: 10px;",
                                                   h4("Hasil Prediksi",
                                                      style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                                                   div(style = "background: #e8f5e8; padding: 15px; border-radius: 6px;
                                                                border-left: 4px solid #27ae60; margin-bottom: 20px;",
                                                       verbatimTextOutput("hasil_prediksi")
                                                   ),

                                                   div(style = "background: #fff3cd; border-radius: 6px; padding: 15px;
                                                                border-left: 4px solid #ffc107;",
                                                       h5(icon("info-circle"), " Catatan:",
                                                          style = "color: #856404; font-weight: 600; margin-bottom: 10px;"),
                                                       p("• Interval prediksi menunjukkan rentang nilai dengan tingkat kepercayaan 95%",
                                                         style = "margin: 5px 0; color: #856404; font-size: 13px;"),
                                                       p("• Semakin sempit interval, semakin akurat prediksi model",
                                                         style = "margin: 5px 0; color: #856404; font-size: 13px;"),
                                                       p("• Pastikan nilai input berada dalam rentang data training",
                                                         style = "margin: 5px 0; color: #856404; font-size: 13px;")
                                                   )
                                               )
                                        )
                                      )
                             )
                           )
                       )
                )
              ),
              fluidRow(
                column(12,
                       div(class = "guide-section",
                           style = "background: white; border-radius: 8px; padding: 25px;
                                    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                                    border-top: 4px solid #17a2b8; margin-bottom: 20px;",

                           div(style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 20px;",
                               div(style = "display: flex; align-items: center;",
                                   icon("info-circle", class = "fa-lg", style = "color: #17a2b8; margin-right: 10px;"),
                                   h3("Panduan Interpretasi",
                                      style = "color: #2c3e50; font-weight: 600; margin: 0; font-size: 1.4em;")
                               ),
                           ),

                           div(style = "background: #f8f9fa; border-radius: 6px; padding: 20px;",
                               h4("Interpretasi Hasil Regresi:",
                                  style = "color: #2c3e50; font-weight: 600; margin-bottom: 20px;"),

                               div(style = "margin-bottom: 20px;",
                                   h5("1. R-squared & Adjusted R-squared",
                                      style = "color: #17a2b8; font-weight: 600; margin-bottom: 10px;"),
                                   p("• R-squared: Proporsi varians Y yang dijelaskan oleh model (0-1)",
                                     style = "margin: 5px 0; color: #495057;"),
                                   p("• Adjusted R-squared: R-squared yang disesuaikan dengan jumlah variabel",
                                     style = "margin: 5px 0; color: #495057;"),
                                   p("• Nilai > 0.7 dianggap baik, > 0.9 sangat baik",
                                     style = "margin: 5px 0; color: #495057;")
                               ),

                               div(style = "margin-bottom: 20px;",
                                   h5("2. Signifikansi Statistik",
                                      style = "color: #17a2b8; font-weight: 600; margin-bottom: 10px;"),
                                   p("• p-value < 0.05: Koefisien signifikan pada α = 5%",
                                     style = "margin: 5px 0; color: #495057;"),
                                   p("• p-value < 0.01: Koefisien sangat signifikan pada α = 1%",
                                     style = "margin: 5px 0; color: #495057;")
                               ),

                               div(style = "margin-bottom: 20px;",
                                   h5("3. Asumsi Regresi Linear",
                                      style = "color: #17a2b8; font-weight: 600; margin-bottom: 10px;"),
                                   p("• Normalitas: Residual harus berdistribusi normal",
                                     style = "margin: 5px 0; color: #495057;"),
                                   p("• Homoskedastisitas: Varians residual konstan",
                                     style = "margin: 5px 0; color: #495057;"),
                                   p("• Tidak ada multikolinearitas: VIF < 5",
                                     style = "margin: 5px 0; color: #495057;"),
                                   p("• Tidak ada autokorelasi: Durbin-Watson sekitar 2",
                                     style = "margin: 5px 0; color: #495057;")
                               ),

                               div(style = "margin-bottom: 0;",
                                   h5("4. Tindak Lanjut jika Asumsi Dilanggar",
                                      style = "color: #17a2b8; font-weight: 600; margin-bottom: 10px;"),
                                   p("• Normalitas dilanggar: Transformasi data atau bootstrap",
                                     style = "margin: 5px 0; color: #495057;"),
                                   p("• Heteroskedastisitas: Robust standard errors",
                                     style = "margin: 5px 0; color: #495057;"),
                                   p("• Multikolinearitas: Hapus variabel atau regularisasi",
                                     style = "margin: 5px 0; color: #495057;"),
                                   p("• Autokorelasi: Model time series atau lag variables",
                                     style = "margin: 5px 0; color: #495057;")
                               )
                           )
                       )
                )
              )
      )

    )
  )
)

server <- function(input, output, session) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || x == "") y else x
  values <- reactiveValues(
    originalData = NULL,
    processedData = NULL,
    transformedVars = list(),
    categorizedVars = list(),
    mdsData = NULL,            # Data hasil MDS
    geoData = NULL             # Data GeoJSON
  )
  observeEvent(input$file, {
    req(input$file)

    tryCatch({
      df <- read.csv(input$file$datapath,
                     header = input$header,
                     sep = input$sep,
                     stringsAsFactors = FALSE)

      df$DISTRICTCODE <- as.character(df$DISTRICTCODE)  # Pastikan character
      df$KODE_JOIN <- paste0(substr(df$DISTRICTCODE, 1, 2), ".", substr(df$DISTRICTCODE, 3, 4))

      values$originalData <- df
      values$processedData <- df

      showNotification("Data berhasil diunggah!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  observeEvent(input$file_mds, {
    req(input$file_mds)

    tryCatch({
      df_mds <- read.csv(input$file_mds$datapath,
                         header = input$header2,
                         sep = input$sep2,
                         stringsAsFactors = FALSE)

      values$mdsData <- df_mds
      showNotification("Data MDS berhasil diunggah!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  observeEvent(input$file_geo, {
    req(input$file_geo)

    tryCatch({
      geo <- sf::st_read(input$file_geo$datapath, quiet = TRUE)
      sf::st_crs(geo) <- 4326
      if ("KDPKAB" %in% names(geo)) {
        geo$KODE_JOIN <- geo$KDPKAB
      } else {
        stop("Kolom 'KDPKAB' tidak ditemukan dalam GeoJSON.")
      }

      values$geoData <- geo
      showNotification("GeoJSON berhasil diunggah!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  output$rawDataTable <- renderDT({
    req(values$originalData)
    datatable(values$originalData,
              options = list(
                pageLength = 5,
                scrollX = TRUE,
                dom = 'tip'
              ),
              rownames = FALSE)
  })
  output$varSelectUI <- renderUI({
    req(values$processedData)
    current_vars <- names(values$processedData)
    checkboxGroupInput("selectedVars", "Pilih variabel yang akan digunakan:",
                       choices = current_vars,
                       selected = current_vars) # Select all current variables by default
  })
  output$transformSelectUI <- renderUI({
    req(values$processedData)
    numeric_vars <- names(values$processedData)[sapply(values$processedData, is.numeric)]
    if(length(numeric_vars) == 0) return(NULL)
    current_selection <- isolate(input$varTransform)
    if (is.null(current_selection) || !(current_selection %in% numeric_vars)) {
      current_selection <- numeric_vars[1] # Default to first if no valid selection
    }

    selectInput("varTransform", "Pilih variabel numerik untuk transformasi:",
                choices = numeric_vars,
                selected = current_selection) # Set selected value
  })
  output$categVarSelect <- renderUI({
    req(values$processedData)
    numeric_vars <- names(values$processedData)[sapply(values$processedData, is.numeric)]
    if(length(numeric_vars) == 0) return(NULL)

    selectInput("varCateg", "Pilih variabel numerik untuk kategorisasi:",
                choices = numeric_vars)
  })
  observe({
    req(values$processedData)
    kab_list <- sort(unique(values$processedData$KODE_JOIN))
    updateSelectInput(session, "mapKabupaten",
                      choices = c("Semua Kabupaten" = "ALL", kab_list))
  })
  observeEvent(input$apply_transform, {
    req(input$varTransform, input$transformType)

    var_data <- values$processedData[[input$varTransform]]
    var_name <- input$varTransform
    if(!is.numeric(var_data)) {
      showNotification("Variabel harus numerik untuk transformasi!", type = "error")
      return()
    }

    if(input$transformType == "log" && any(var_data <= 0, na.rm = TRUE)) {
      showNotification("Data harus positif untuk transformasi log!", type = "error")
      return()
    }

    if(input$transformType == "sqrt" && any(var_data < 0, na.rm = TRUE)) {
      showNotification("Data tidak boleh negatif untuk transformasi akar kuadrat!", type = "error")
      return()
    }

    transformed_data <- switch(input$transformType,
                               "log" = log(var_data),
                               "sqrt" = sqrt(var_data),
                               "square" = var_data^2,
                               "standardize" = as.numeric(scale(var_data)),
                               "inverse" = 1/var_data
    )

    new_var_name <- paste0(var_name, "_", input$transformType)
    temp_processed_data <- values$processedData
    temp_processed_data[[new_var_name]] <- transformed_data
    values$processedData <- temp_processed_data # This reassignment forces reactivity

    updateCheckboxGroupInput(session, "selectedVars",
                             choices = names(values$processedData),
                             selected = c(input$selectedVars, new_var_name))

    values$transformedVars[[new_var_name]] <- input$transformType

    output$transformResult <- renderPrint({
      cat("=== HASIL TRANSFORMASI ===\n")
      cat("Variabel asli:", var_name, "\n")
      cat("Transformasi:", input$transformType, "\n")
      cat("Variabel baru:", new_var_name, "\n\n")
      cat("Ringkasan data sebelum transformasi:\n")
      print(summary(var_data))
      cat("\nRingkasan data setelah transformasi:\n")
      print(summary(transformed_data))
    })

    showNotification("Transformasi berhasil diterapkan!", type = "message")
  })
  observeEvent(input$apply_categ, {
    req(input$varCateg, input$numCategories)

    var_data <- values$processedData[[input$varCateg]]
    var_name <- input$varCateg

    if(!is.numeric(var_data)) {
      showNotification("Variabel harus numerik untuk kategorisasi!", type = "error")
      return()
    }

    if(input$categMethod == "quantile") {
      breaks <- quantile(var_data, probs = seq(0, 1, length.out = input$numCategories + 1), na.rm = TRUE)
      breaks <- unique(breaks) # Pastikan break points unik
      if(length(breaks) < input$numCategories + 1) {
        showNotification("Tidak bisa membuat kategori unik. Coba metode lain atau kurangi jumlah kategori.", type = "error")
        return()
      }
    } else if(input$categMethod == "equal") {
      breaks <- seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE), length.out = input$numCategories + 1)
    } else if(input$categMethod == "manual") {
      breaks <- as.numeric(unlist(strsplit(input$manualBreaks, ",")))
      if(length(breaks) != input$numCategories + 1) {
        showNotification("Jumlah nilai batas harus sama dengan jumlah kategori + 1!", type = "error")
        return()
      }
    }

    categorized_data <- cut(var_data, breaks = breaks, include.lowest = TRUE,
                            labels = paste0("Kategori_", 1:input$numCategories))

    new_var_name <- paste0(var_name, "_kategori")
    temp_processed_data <- values$processedData
    temp_processed_data[[new_var_name]] <- categorized_data
    values$processedData <- temp_processed_data # This reassignment forces reactivity

    updateCheckboxGroupInput(session, "selectedVars",
                             choices = names(values$processedData),
                             selected = c(input$selectedVars, new_var_name))

    values$categorizedVars[[new_var_name]] <- input$categMethod

    output$categResult <- renderPrint({
      cat("=== HASIL KATEGORISASI ===\n")
      cat("Variabel asli:", var_name, "\n")
      cat("Metode:", input$categMethod, "\n")
      cat("Jumlah kategori:", input$numCategories, "\n")
      cat("Variabel baru:", new_var_name, "\n\n")
      cat("Batas kategori:\n")
      print(breaks)
      cat("\nDistribusi kategori:\n")
      print(table(categorized_data, useNA = "ifany"))
    })

    showNotification("Kategorisasi berhasil diterapkan!", type = "message")
  })
  output$processedDataTable <- renderDT({
    req(values$processedData, input$selectedVars) # Ensure selectedVars is available
    filtered_data <- values$processedData %>%
      select(all_of(input$selectedVars))

    datatable(filtered_data,
              options = list(
                pageLength = 5,
                scrollX = TRUE,
                dom = 'tip'
              ),
              rownames = FALSE)
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("processed_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(input$selectedVars) # Ensure selectedVars is available
      download_data <- values$processedData %>%
        select(all_of(input$selectedVars))
      write.csv(download_data, file, row.names = FALSE)
    }
  )
  output$descVarSelect <- renderUI({
    req(values$processedData)
    numeric_vars <- names(values$processedData)[sapply(values$processedData, is.numeric)]
    selectInput("descVars", "Pilih variabel numerik:",
                choices = numeric_vars,
                multiple = TRUE)
  })

  output$descGroupSelect <- renderUI({
    req(values$processedData)
    categ_vars <- names(values$processedData)[sapply(values$processedData, function(x) {
      (is.factor(x) || is.character(x)) && length(unique(x[!is.na(x)])) > 1
    })]
    selectInput("descGroupVar", "Pilih variabel pengelompok:", choices = categ_vars)
  })

  output$descTable <- renderDT({
    req(input$descVars) # Ensure numeric variables are selected

    df <- values$processedData

    if(input$descGroupBy == "none") {
      desc_stats <- df %>%
        select(all_of(input$descVars)) %>%
        summarise(across(everything(),
                         list(Mean = ~mean(., na.rm = TRUE),
                              Median = ~median(., na.rm = TRUE),
                              SD = ~sd(., na.rm = TRUE),
                              Min = ~min(., na.rm = TRUE),
                              Max = ~max(., na.rm = TRUE),
                              N = ~sum(!is.na(.)),
                              Missing = ~sum(is.na(.)))))
      desc_long <- desc_stats %>%
        pivot_longer(everything(), names_to = "Variable_Stat", values_to = "Value") %>%
        separate(Variable_Stat, c("Variable", "Statistic"), sep = "_") %>%
        pivot_wider(names_from = Statistic, values_from = Value) %>%
        select(Variable, N, Missing, Mean, Median, SD, Min, Max)

    } else {
      req(input$descGroupVar) # Ensure grouping variable is selected
      df[[input$descGroupVar]] <- as.factor(df[[input$descGroupVar]])
      df <- df[!is.na(df[[input$descGroupVar]]), ]
      numeric_vars_to_summarise <- intersect(input$descVars, names(df)[sapply(df, is.numeric)])

      if (length(numeric_vars_to_summarise) == 0) {
        return(datatable(data.frame(Pesan = "Tidak ada variabel numerik yang valid untuk diringkas."),
                         options = list(dom = 't')))
      }

      desc_stats <- df %>%
        group_by(!!sym(input$descGroupVar)) %>%
        summarise(across(all_of(numeric_vars_to_summarise), # Use the filtered list of numeric vars
                         list(Mean = ~mean(., na.rm = TRUE),
                              Median = ~median(., na.rm = TRUE),
                              SD = ~sd(., na.rm = TRUE),
                              Min = ~min(., na.rm = TRUE),
                              Max = ~max(., na.rm = TRUE),
                              N = ~sum(!is.na(.)),
                              Missing = ~sum(is.na(.))), .names = "{.col}_{.fn}"), .groups = "drop")
      desc_long <- desc_stats %>%
        pivot_longer(cols = -!!sym(input$descGroupVar), names_to = "Variable_Stat", values_to = "Value") %>%
        mutate(Value = as.numeric(Value)) %>% # Ensure numeric type
        separate(Variable_Stat, c("Variable", "Statistic"), sep = "_", extra = "merge") %>% # Use extra = "merge" for safety
        pivot_wider(names_from = Statistic, values_from = Value) %>%
        mutate(Variable = as.character(Variable)) %>%
        select(!!sym(input$descGroupVar), Variable, N, Missing, Mean, Median, SD, Min, Max)
    }

    datatable(desc_long,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Brtip',
                buttons = c('copy', 'csv', 'excel', 'pdf')
              ),
              extensions = 'Buttons',
              rownames = FALSE)
  })

  output$descInterpretation <- renderText({
    req(input$descVars)

    if(input$descGroupBy == "none") {
      paste("Tabel menunjukkan statistik deskriptif untuk", length(input$descVars),
            "variabel numerik. Perhatikan perbandingan mean dan median untuk mengidentifikasi skewness data.")
    } else {
      req(input$descGroupVar)
      paste("Tabel menunjukkan statistik deskriptif untuk", length(input$descVars),
            "variabel numerik yang dikelompokkan berdasarkan", input$descGroupVar,
            ". Perhatikan variasi statistik antar kelompok.")
    }
  })
  output$plotVarSelect <- renderUI({
    req(values$processedData)
    numeric_vars <- names(values$processedData)[sapply(values$processedData, is.numeric)]
    selectInput("plotVar", "Pilih variabel:", choices = numeric_vars)
  })

  output$scatterVarSelect <- renderUI({
    req(values$processedData)
    numeric_vars <- names(values$processedData)[sapply(values$processedData, is.numeric)]
    numeric_vars <- setdiff(numeric_vars, input$plotVar)
    selectInput("scatterVar", "Pilih variabel kedua:", choices = numeric_vars)
  })

  output$barGroupSelect <- renderUI({
    req(values$processedData)
    categ_vars <- names(values$processedData)[sapply(values$processedData, function(x) {
      (is.factor(x) || is.character(x)) && length(unique(x[!is.na(x)])) > 1
    })]
    selectInput("barGroupVar", "Pilih variabel pengelompok:", choices = categ_vars)
  })
  output$plotEksplorasi <- renderPlotly({
    req(input$plotVar, input$plotType)

    df <- values$processedData
    var <- input$plotVar

    if(input$plotType == "hist") {
      p <- ggplot(df, aes_string(x = var)) +
        geom_histogram(fill = "#FF8C00", color = "white", bins = 30, alpha = 0.8) +
        theme_minimal() +
        labs(title = paste("Histogram:", var),
             x = var, y = "Frekuensi")

    } else if(input$plotType == "box") {
      p <- ggplot(df, aes_string(y = var)) +
        geom_boxplot(fill = "#FF8C00", color = "black", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Boxplot:", var),
             y = var)

    } else if(input$plotType == "density") {
      p <- ggplot(df, aes_string(x = var)) +
        geom_density(fill = "#FF8C00", alpha = 0.5, color = "#FF8C00") +
        theme_minimal() +
        labs(title = paste("Density Plot:", var),
             x = var, y = "Density")

    } else if(input$plotType == "scatter") {
      req(input$scatterVar)
      p <- ggplot(df, aes_string(x = var, y = input$scatterVar)) +
        geom_point(color = "#FF8C00", alpha = 0.6) +
        theme_minimal() +
        labs(title = paste("Scatter Plot:", var, "vs", input$scatterVar),
             x = var, y = input$scatterVar)

    } else if(input$plotType == "bar") {
      req(input$barGroupVar)
      df[[input$barGroupVar]] <- as.factor(df[[input$barGroupVar]])
      df <- df[!is.na(df[[input$barGroupVar]]), ] # Remove NAs from grouping variable

      summary_df <- df %>%
        group_by(!!sym(input$barGroupVar)) %>%
        summarise(Mean = mean(!!sym(var), na.rm = TRUE),
                  SE = sd(!!sym(var), na.rm = TRUE)/sqrt(n()), .groups = "drop")

      p <- ggplot(summary_df, aes_string(x = input$barGroupVar, y = "Mean")) +
        geom_bar(stat = "identity", fill = "#FF8C00", alpha = 0.8) +
        geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                      width = 0.2, color = "black") +
        theme_minimal() +
        labs(title = paste("Rata-rata", var, "per", input$barGroupVar),
             x = input$barGroupVar, y = paste("Rata-rata", var))
    }

    ggplotly(p) #%>%
  })

  output$plotInterpretation <- renderText({
    req(input$plotVar, input$plotType)

    if(input$plotType == "hist") {
      paste("Histogram menunjukkan distribusi frekuensi dari variabel", input$plotVar,
            ". Perhatikan bentuk distribusi (simetris, miring ke kanan/kiri) dan adanya outlier.")
    } else if(input$plotType == "box") {
      paste("Boxplot menunjukkan distribusi statistik dari variabel", input$plotVar,
            ". Kotak menunjukkan IQR (interquartile range), garis tengah adalah median, dan titik-titik adalah outlier.")
    } else if(input$plotType == "density") {
      paste("Density plot menunjukkan estimasi kepadatan probabilitas dari variabel", input$plotVar,
            ". Kurva yang lebih halus menunjukkan distribusi yang lebih kontinu.")
    } else if(input$plotType == "scatter") {
      paste("Scatter plot menunjukkan hubungan antara variabel", input$plotVar, "dan", input$scatterVar,
            ". Perhatikan pola hubungan (linear, non-linear, atau tidak ada hubungan).")
    } else if(input$plotType == "bar") {
      paste("Bar plot menunjukkan rata-rata variabel", input$plotVar, "pada setiap kategori", input$barGroupVar,
            ". Error bar menunjukkan standar error dari mean.")
    }
  })
  output$normalVarSelect <- renderUI({
    req(values$processedData)
    numeric_vars <- names(values$processedData)[sapply(values$processedData, is.numeric)]
    if(length(numeric_vars) == 0) {
      return(div("Tidak ada variabel numerik tersedia"))
    }
    selectInput("normVar", "Pilih variabel:", choices = numeric_vars)
  })

  output$normalityTest <- renderPrint({
    req(input$normVar, input$normTestType)

    data <- values$processedData[[input$normVar]]
    data <- data[!is.na(data) & is.finite(data)] # Handle infinite values

    if(length(data) < 3) {
      cat("Error: Data terlalu kecil untuk uji normalitas (n < 3)\n")
      return()
    }
    if(length(unique(data)) == 1) {
      cat("Error: Data konstan, tidak dapat diuji normalitas\n")
      return()
    }

    cat("Jumlah observasi:", length(data), "\n")
    cat("Mean:", round(mean(data), 4), "| SD:", round(sd(data), 4), "\n")
    cat("Skewness:", round(moments::skewness(data), 4), "| Kurtosis:", round(moments::kurtosis(data), 4), "\n\n")

    if(input$normTestType == "shapiro") {
      if(length(data) > 5000) {
        set.seed(123) # For reproducible sampling
        data_sample <- sample(data, 5000)
        cat("Catatan: Data lebih dari 5000 observasi, menggunakan sample 5000\n\n")
        result <- shapiro.test(data_sample)
      } else {
        result <- shapiro.test(data)
      }
      print(result)
    } else if(input$normTestType == "ks") {
      result <- ks.test(scale(data), "pnorm")
      print(result)
    } else if(input$normTestType == "ad") {
      result <- nortest::ad.test(data)
      print(result)
    } else if(input$normTestType == "jb") {
      result <- tseries::jarque.bera.test(data)
      print(result)
    }
  })

  output$qqPlot <- renderPlot({
    req(input$normVar)

    data <- values$processedData[[input$normVar]]
    data <- data[!is.na(data) & is.finite(data)]

    if(length(data) < 2) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="", main="Data tidak cukup untuk plot")
      return()
    }

    par(mfrow=c(1,2))
    qqnorm(data, col = "#FF8C00", main = paste("QQ Plot:", input$normVar),
           xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", pch=16)
    qqline(data, col = "red", lwd = 2)
    hist(data, probability = TRUE, col = "#FFE5B4", border = "white",
         main = paste("Histogram:", input$normVar), xlab = input$normVar)
    curve(dnorm(x, mean = mean(data), sd = sd(data)), add = TRUE, col = "red", lwd = 2)

    par(mfrow=c(1,1))
  })

  output$interpretasiNormalitas <- renderText({
    req(input$normVar, input$normTestType)

    data <- values$processedData[[input$normVar]]
    data <- data[!is.na(data) & is.finite(data)]

    if(length(data) < 3) return("Data terlalu kecil untuk uji normalitas (n < 3)")
    if(length(unique(data)) == 1) return("Data konstan, tidak dapat diuji normalitas")

    n <- length(data)
    if(input$normTestType == "shapiro") {
      if(n > 5000) {
        set.seed(123)
        data_sample <- sample(data, 5000)
        pval <- tryCatch(shapiro.test(data_sample)$p.value, error = function(e) NA)
      } else {
        pval <- tryCatch(shapiro.test(data)$p.value, error = function(e) NA)
      }
      test_name <- "Shapiro-Wilk"
      note <- if(n > 5000) " (menggunakan sample 5000)" else ""
    } else if(input$normTestType == "ks") {
      pval <- tryCatch(ks.test(scale(data), "pnorm")$p.value, error = function(e) NA)
      test_name <- "Kolmogorov-Smirnov"
      note <- ""
    } else if(input$normTestType == "ad") {
      pval <- tryCatch(nortest::ad.test(data)$p.value, error = function(e) NA)
      test_name <- "Anderson-Darling"
      note <- ""
    } else if(input$normTestType == "jb") {
      pval <- tryCatch(tseries::jarque.bera.test(data)$p.value, error = function(e) NA)
      test_name <- "Jarque-Bera"
      note <- ""
    }

    if(is.na(pval)) return("Tidak dapat menghitung uji normalitas.")
    size_note <- ""
    if(n < 30) {
      size_note <- " Perhatian: Ukuran sampel kecil (n < 30), hasil mungkin kurang reliabel."
    } else if(n > 1000) {
      size_note <- " Perhatian: Ukuran sampel besar (n > 1000), uji mungkin mendeteksi deviasi kecil yang tidak praktis signifikan."
    }

    if(pval > 0.05) {
      paste0("Berdasarkan uji ", test_name, note, ", data terdistribusi normal (p-value = ",
             round(pval, 4), " > 0.05). Asumsi normalitas terpenuhi.", size_note)
    } else {
      paste0("Berdasarkan uji ", test_name, note, ", data tidak terdistribusi normal (p-value = ",
             round(pval, 4), " ≤ 0.05). Pertimbangkan transformasi data (log, sqrt, Box-Cox) atau gunakan uji non-parametrik.", size_note)
    }
  })
  output$homogenVarSelect <- renderUI({
    req(values$processedData)
    numeric_vars <- names(values$processedData)[sapply(values$processedData, is.numeric)]
    if(length(numeric_vars) == 0) {
      return(div("Tidak ada variabel numerik tersedia"))
    }
    selectInput("homogenVar", "Pilih variabel numerik (dependent):", choices = numeric_vars)
  })

  output$homogenGroupSelect <- renderUI({
    req(values$processedData)
    categ_vars <- names(values$processedData)[sapply(values$processedData, function(x) {
      if(is.factor(x) || is.character(x)) {
        unique_vals <- unique(x[!is.na(x)])
        return(length(unique_vals) >= 2 && length(unique_vals) <= 20) # Max 20 groups for practical purposes
      }
      return(FALSE)
    })]

    if(length(categ_vars) == 0) {
      return(div(class = "alert alert-warning", "Tidak ada variabel kategorikal yang sesuai untuk pengelompokan"))
    }
    selectInput("homogenGroup", "Pilih variabel grup (independent):", choices = categ_vars)
  })

  output$homogeneityTest <- renderPrint({
    req(input$homogenVar, input$homogenGroup, input$homogenTestType)

    df <- values$processedData
    df <- df[complete.cases(df[[input$homogenVar]], df[[input$homogenGroup]]), ]
    df <- df[is.finite(df[[input$homogenVar]]), ]

    if(nrow(df) < 2) {
      cat("Error: Data terlalu kecil untuk uji homogenitas\n")
      return()
    }
    df[[input$homogenGroup]] <- as.factor(df[[input$homogenGroup]])
    groups <- levels(df[[input$homogenGroup]])

    if(length(groups) < 2) {
      cat("Error: Harus ada minimal 2 grup untuk uji homogenitas\n")
      return()
    }
    group_sizes <- table(df[[input$homogenGroup]])
    cat("Ukuran sampel per grup:\n")
    print(group_sizes)

    if(any(group_sizes < 2)) {
      cat("\nPerhatian: Beberapa grup memiliki ukuran sampel < 2\n")
    }
    cat("\nDeskriptif statistik per grup:\n")
    for(group in groups) {
      group_data <- df[df[[input$homogenGroup]] == group, input$homogenVar]
      cat("Grup", group, ":\n")
      cat("  N =", length(group_data),
          "| Mean =", round(mean(group_data), 4),
          "| SD =", round(sd(group_data), 4),
          "| Var =", round(var(group_data), 4), "\n")
    }
    overall_var <- var(df[[input$homogenVar]])
    cat("\nVarian keseluruhan:", round(overall_var, 4), "\n")

    formula <- as.formula(paste(input$homogenVar, "~", input$homogenGroup))

    cat("\n=== HASIL UJI HOMOGENITAS ===\n")
    if(input$homogenTestType == "levene") {
      result <- car::leveneTest(formula, data = df)
      print(result)
    } else if(input$homogenTestType == "bartlett") {
      result <- bartlett.test(formula, data = df)
      print(result)
    } else if(input$homogenTestType == "fligner") {
      result <- fligner.test(formula, data = df)
      print(result)
    }
  })

  output$homogeneityPlot <- renderPlot({
    req(input$homogenVar, input$homogenGroup)

    df <- values$processedData
    df <- df[complete.cases(df[[input$homogenVar]], df[[input$homogenGroup]]), ]
    df <- df[is.finite(df[[input$homogenVar]]), ]

    if(nrow(df) < 2) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="", main="Data tidak cukup untuk plot")
      return()
    }

    df[[input$homogenGroup]] <- as.factor(df[[input$homogenGroup]])
    par(mfrow=c(2,2))
    boxplot(df[[input$homogenVar]] ~ df[[input$homogenGroup]],
            col = rainbow(length(levels(df[[input$homogenGroup]]))),
            main = paste("Boxplot:", input$homogenVar, "per", input$homogenGroup),
            xlab = input$homogenGroup, ylab = input$homogenVar)
    group_vars <- aggregate(df[[input$homogenVar]],
                            by = list(df[[input$homogenGroup]]),
                            FUN = function(x) {
                              if(length(x) > 1) var(x, na.rm = TRUE) else 0
                            })
    if(all(is.finite(group_vars$x)) && any(group_vars$x > 0)) {
      barplot(group_vars$x, names.arg = group_vars$Group.1,
              col = "lightblue", main = "Varian per Grup",
              xlab = input$homogenGroup, ylab = "Varian")
    } else {
      plot(1, type="n", axes=FALSE, xlab="", ylab="",
           main="Tidak dapat menghitung varian")
      text(1, 1, "Varian tidak dapat dihitung\n(data konstan atau tidak cukup)", cex=0.8)
    }
    tryCatch({
      if(length(levels(df[[input$homogenGroup]])) > 2) {
        model <- aov(df[[input$homogenVar]] ~ df[[input$homogenGroup]])
        plot(model, which = 1, main = "Residuals vs Fitted")
      } else {
        groups <- levels(df[[input$homogenGroup]])
        group1_data <- df[df[[input$homogenGroup]] == groups[1], input$homogenVar]
        group2_data <- df[df[[input$homogenGroup]] == groups[2], input$homogenVar]
        if(length(group1_data) > 1 && length(group2_data) > 1 &&
           sd(group1_data) > 0 && sd(group2_data) > 0) {

          d1 <- density(group1_data)
          d2 <- density(group2_data)

          plot(d1, main = "Distribusi per Grup", col = "red", lwd = 2,
               xlim = range(c(d1$x, d2$x)), ylim = range(c(d1$y, d2$y)))
          lines(d2, col = "blue", lwd = 2)
          legend("topright", legend = groups, col = c("red", "blue"), lwd = 2)
        } else {
          plot(1, type="n", axes=FALSE, xlab="", ylab="",
               main="Tidak dapat membuat density plot")
          text(1, 1, "Data tidak cukup atau konstan\nuntuk density plot", cex=0.8)
        }
      }
    }, error = function(e) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="", main="Error dalam plot")
      text(1, 1, paste("Error:", e$message), cex=0.8)
    })
    tryCatch({
      group_means <- aggregate(df[[input$homogenVar]],
                               by = list(df[[input$homogenGroup]]),
                               FUN = mean, na.rm = TRUE)
      group_sds <- aggregate(df[[input$homogenVar]],
                             by = list(df[[input$homogenGroup]]),
                             FUN = sd, na.rm = TRUE)

      means <- group_means$x
      sds <- group_sds$x
      groups_names <- group_means$Group.1
      sds[is.na(sds)] <- 0
      sds[!is.finite(sds)] <- 0
      y_lower <- means - sds
      y_upper <- means + sds
      if(all(is.finite(c(y_lower, y_upper)))) {
        ylim_range <- range(c(y_lower, y_upper))

        plot(1:length(means), means, type = "b", col = "red", pch = 16,
             ylim = ylim_range,
             main = "Mean ± SD per Grup", xlab = "Grup", ylab = "Nilai",
             xaxt = "n")
        axis(1, at = 1:length(means), labels = groups_names)

        # Add error bars only if SD > 0
        valid_sds <- sds > 0 & is.finite(sds)
        if(any(valid_sds)) {
          arrows(which(valid_sds), means[valid_sds] - sds[valid_sds],
                 which(valid_sds), means[valid_sds] + sds[valid_sds],
                 code = 3, angle = 90, length = 0.1, col = "red")
        }
      } else {
        plot(1, type="n", axes=FALSE, xlab="", ylab="",
             main="Tidak dapat membuat plot Mean ± SD")
        text(1, 1, "Nilai tidak finite untuk plot", cex=0.8)
      }
    }, error = function(e) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="", main="Error dalam Mean±SD plot")
      text(1, 1, paste("Error:", e$message), cex=0.8)
    })

    par(mfrow=c(1,1))
  })

  output$interpretasiHomogenitas <- renderText({
    req(input$homogenVar, input$homogenGroup, input$homogenTestType)

    df <- values$processedData
    df <- df[complete.cases(df[[input$homogenVar]], df[[input$homogenGroup]]), ]
    df <- df[is.finite(df[[input$homogenVar]]), ]

    if(nrow(df) < 2) return("Data terlalu kecil untuk uji homogenitas")

    df[[input$homogenGroup]] <- as.factor(df[[input$homogenGroup]])

    if(length(levels(df[[input$homogenGroup]])) < 2) {
      return("Harus ada minimal 2 grup untuk uji homogenitas")
    }

    formula <- as.formula(paste(input$homogenVar, "~", input$homogenGroup))
    if(input$homogenTestType == "levene") {
      pval <- tryCatch(car::leveneTest(formula, data = df)$`Pr(>F)`[1], error = function(e) NA)
      test_name <- "Levene"
      note <- " (robust terhadap non-normalitas)"
    } else if(input$homogenTestType == "bartlett") {
      pval <- tryCatch(bartlett.test(formula, data = df)$p.value, error = function(e) NA)
      test_name <- "Bartlett"
      note <- " (sensitif terhadap non-normalitas)"
    } else if(input$homogenTestType == "fligner") {
      pval <- tryCatch(fligner.test(formula, data = df)$p.value, error = function(e) NA)
      test_name <- "Fligner-Killeen"
      note <- " (non-parametrik, robust)"
    }

    if(is.na(pval)) return("Tidak dapat menghitung uji homogenitas.")
    group_sizes <- table(df[[input$homogenGroup]])
    size_warning <- ""
    if(any(group_sizes < 5)) {
      size_warning <- " Perhatian: Beberapa grup memiliki ukuran sampel kecil (< 5)."
    }
    group_vars <- aggregate(df[[input$homogenVar]],
                            by = list(df[[input$homogenGroup]]),
                            FUN = var, na.rm = TRUE)$x
    var_ratio <- max(group_vars) / min(group_vars)
    ratio_note <- paste0(" Rasio varian max/min: ", round(var_ratio, 2))

    if(var_ratio > 10) {
      ratio_note <- paste0(ratio_note, " (sangat heterogen)")
    } else if(var_ratio > 4) {
      ratio_note <- paste0(ratio_note, " (cukup heterogen)")
    }

    if(pval > 0.05) {
      paste0("Berdasarkan uji ", test_name, note, ", varian antar grup homogen (p-value = ",
             round(pval, 4), " > 0.05). Asumsi homogenitas varian terpenuhi.",
             ratio_note, size_warning)
    } else {
      paste0("Berdasarkan uji ", test_name, note, ", varian antar grup tidak homogen (p-value = ",
             round(pval, 4), " ≤ 0.05). Pertimbangkan transformasi data, gunakan uji Welch ANOVA, atau uji non-parametrik seperti Kruskal-Wallis.",
             ratio_note, size_warning)
    }
  })
  output$homogenVarMultiSelect <- renderUI({
    req(values$processedData)
    numeric_vars <- names(values$processedData)[sapply(values$processedData, is.numeric)]
    if(length(numeric_vars) < 2) {
      return(div("Minimal 2 variabel numerik diperlukan untuk uji homogenitas antar variabel"))
    }
    selectInput("homogenVarsMulti", "Pilih variabel numerik (minimal 2):",
                choices = numeric_vars,
                multiple = TRUE,
                selected = numeric_vars[1:min(length(numeric_vars), 5)])
  })

  output$homogeneityTestMulti <- renderPrint({
    req(input$homogenVarsMulti, input$homogenTestTypeMulti)

    if(length(input$homogenVarsMulti) < 2) {
      cat("Error: Pilih minimal 2 variabel untuk uji homogenitas antar variabel\n")
      return()
    }

    tryCatch({
      available_vars <- names(values$processedData)
      selected_vars <- input$homogenVarsMulti[input$homogenVarsMulti %in% available_vars]

      if(length(selected_vars) < 2) {
        cat("Error: Variabel yang dipilih tidak ditemukan dalam dataset\n")
        return()
      }

      df <- values$processedData[, selected_vars, drop = FALSE]
      df <- df[complete.cases(df), ]
      df <- df[apply(df, 1, function(x) all(is.finite(x))), ]

      if(nrow(df) < 2) {
        cat("Error: Data terlalu kecil setelah cleaning\n")
        return()
      }

      cat("=== UJI HOMOGENITAS ANTAR VARIABEL ===\n")
      cat("Jumlah observasi yang digunakan:", nrow(df), "\n")
      cat("Variabel yang diuji:", paste(selected_vars, collapse = ", "), "\n\n")
      cat("Deskriptif statistik per variabel:\n")
      for(var in selected_vars) {
        var_data <- df[[var]]
        cat("Variabel", var, ":\n")
        cat("  N =", length(var_data),
            "| Mean =", round(mean(var_data, na.rm = TRUE), 4),
            "| SD =", round(sd(var_data, na.rm = TRUE), 4),
            "| Var =", round(var(var_data, na.rm = TRUE), 4), "\n")
      }
      variances <- sapply(df, function(x) var(x, na.rm = TRUE))
      cat("\nVarian per variabel:\n")
      for(i in 1:length(variances)) {
        cat(names(variances)[i], ":", round(variances[i], 4), "\n")
      }
      max_var <- max(variances)
      min_var <- min(variances)
      var_ratio <- max_var / min_var
      cat("\nRasio varian (max/min):", round(var_ratio, 4), "\n")

      cat("\n=== HASIL UJI ===\n")
      df_long <- data.frame(
        value = unlist(df),
        variable = factor(rep(names(df), each = nrow(df)))
      )

      if(input$homogenTestTypeMulti == "levene") {
        result <- car::leveneTest(value ~ variable, data = df_long)
        cat("Levene Test untuk Homogenitas Varian Antar Variabel:\n")
        print(result)
      } else if(input$homogenTestTypeMulti == "bartlett") {
        result <- bartlett.test(value ~ variable, data = df_long)
        cat("Bartlett Test untuk Homogenitas Varian Antar Variabel:\n")
        print(result)
      }
      cat("\n=== PERBANDINGAN VARIAN ===\n")
      variance_df <- data.frame(
        Variabel = names(variances),
        Varian = round(variances, 4),
        SD = round(sqrt(variances), 4)
      )
      print(variance_df)
      if(length(selected_vars) == 2) {
        cat("\n=== F-TEST (Pairwise) ===\n")
        f_test <- var.test(df[[1]], df[[2]])
        cat("F-test antara", names(df)[1], "dan", names(df)[2], ":\n")
        print(f_test)
      }

    }, error = function(e) {
      cat("Error dalam analisis:", e$message, "\n")
    })
  })

  output$homogeneityPlotMulti <- renderPlot({
    req(input$homogenVarsMulti)

    if(length(input$homogenVarsMulti) < 2) return()

    tryCatch({
      available_vars <- names(values$processedData)
      selected_vars <- input$homogenVarsMulti[input$homogenVarsMulti %in% available_vars]

      df <- values$processedData[, selected_vars, drop = FALSE]
      df <- df[complete.cases(df), ]
      df <- df[apply(df, 1, function(x) all(is.finite(x))), ]

      if(nrow(df) < 2) {
        plot(1, type="n", axes=FALSE, xlab="", ylab="", main="Data tidak cukup")
        return()
      }

      par(mfrow=c(2,2))
      df_long <- data.frame(
        value = unlist(df),
        variable = factor(rep(names(df), each = nrow(df)))
      )

      boxplot(value ~ variable, data = df_long,
              col = rainbow(length(selected_vars)),
              main = "Boxplot: Perbandingan Variabel",
              xlab = "Variabel", ylab = "Nilai", las = 2)
      variances <- sapply(df, function(x) var(x, na.rm = TRUE))
      barplot(variances, col = "lightblue",
              main = "Perbandingan Varian",
              xlab = "Variabel", ylab = "Varian", las = 2)
      sds <- sapply(df, function(x) sd(x, na.rm = TRUE))
      barplot(sds, col = "lightgreen",
              main = "Perbandingan Standard Deviasi",
              xlab = "Variabel", ylab = "SD", las = 2)
      if(ncol(df) <= 6) {
        all_values <- unlist(df)
        x_range <- range(all_values, na.rm = TRUE)

        plot(1, type="n", xlim = x_range, ylim = c(0, 1),
             main = "Distribusi Variabel (Overlay)",
             xlab = "Nilai", ylab = "Density")

        colors <- rainbow(ncol(df))
        for(i in 1:ncol(df)) {
          if(sd(df[[i]], na.rm = TRUE) > 0) {
            density_curve <- density(df[[i]])
            lines(density_curve, col = colors[i], lwd = 2)
          }
        }

        legend("topright", legend = names(df), col = colors, lwd = 2, cex = 0.8)
      } else {
        plot(1, type="n", axes=FALSE, xlab="", ylab="",
             main="Terlalu banyak variabel")
        text(1, 1, paste("Jumlah variabel:", ncol(df)), cex=0.8)
      }

      par(mfrow=c(1,1))

    }, error = function(e) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="", main="Error dalam plot")
      text(1, 1, paste("Error:", e$message), cex=0.8)
    })
  })

  output$interpretasiHomogenitasMulti <- renderText({
    req(input$homogenVarsMulti, input$homogenTestTypeMulti)

    if(length(input$homogenVarsMulti) < 2) {
      return("Pilih minimal 2 variabel untuk uji homogenitas")
    }

    tryCatch({
      available_vars <- names(values$processedData)
      selected_vars <- input$homogenVarsMulti[input$homogenVarsMulti %in% available_vars]

      df <- values$processedData[, selected_vars, drop = FALSE]
      df <- df[complete.cases(df), ]
      df <- df[apply(df, 1, function(x) all(is.finite(x))), ]

      if(nrow(df) < 2) return("Data terlalu kecil")
      df_long <- data.frame(
        value = unlist(df),
        variable = factor(rep(names(df), each = nrow(df)))
      )
      if(input$homogenTestTypeMulti == "levene") {
        pval <- car::leveneTest(value ~ variable, data = df_long)$`Pr(>F)`[1]
        test_name <- "Levene"
        note <- " (robust terhadap non-normalitas)"
      } else {
        pval <- bartlett.test(value ~ variable, data = df_long)$p.value
        test_name <- "Bartlett"
        note <- " (sensitif terhadap non-normalitas)"
      }
      variances <- sapply(df, function(x) var(x, na.rm = TRUE))
      var_ratio <- max(variances) / min(variances)
      ratio_note <- paste0(" Rasio varian max/min: ", round(var_ratio, 2))

      if(var_ratio > 10) {
        ratio_note <- paste0(ratio_note, " (sangat heterogen)")
      } else if(var_ratio > 4) {
        ratio_note <- paste0(ratio_note, " (cukup heterogen)")
      }

      size_note <- paste0(" Jumlah observasi: ", nrow(df))

      if(pval > 0.05) {
        paste0("Berdasarkan uji ", test_name, note, ", varian antar variabel homogen (p-value = ",
               round(pval, 4), " > 0.05). Variabel-variabel memiliki varian yang tidak berbeda signifikan.",
               ratio_note, ".", size_note)
      } else {
        paste0("Berdasarkan uji ", test_name, note, ", varian antar variabel tidak homogen (p-value = ",
               round(pval, 4), " ≤ 0.05). Terdapat perbedaan varian yang signifikan antar variabel. ",
               "Pertimbangkan standardisasi atau transformasi data sebelum analisis lebih lanjut.",
               ratio_note, ".", size_note)
      }

    }, error = function(e) {
      return(paste("Error dalam interpretasi:", e$message))
    })
  })
  output$jackknifeVarSelect <- renderUI({
    req(values$processedData)
    numeric_vars <- names(values$processedData)[sapply(values$processedData, is.numeric)]

    if(length(numeric_vars) == 0) {
      return(div(class = "alert alert-warning", "Tidak ada variabel numerik yang tersedia"))
    }

    selectInput("jackknifeVar", "Pilih variabel:",
                choices = numeric_vars,
                selected = numeric_vars[1])
  })

  observeEvent(input$runJackknife, {
    req(input$jackknifeVar, input$jackknifeStatistic)

    tryCatch({
      data <- values$processedData[[input$jackknifeVar]]
      data <- data[!is.na(data)]  # Remove NA values
      n <- length(data)
      if(n < 3) {
        showNotification("Data terlalu kecil untuk jackknife (minimal n = 3)", type = "error")
        return()
      }
      if(length(unique(data)) == 1) {
        showNotification("Data tidak bervariasi (semua nilai sama)", type = "warning")
        return()
      }
      stat_func <- switch(input$jackknifeStatistic,
                          "mean" = function(x) if(length(x) > 0) mean(x, na.rm = TRUE) else NA,
                          "median" = function(x) if(length(x) > 0) median(x, na.rm = TRUE) else NA,
                          "sd" = function(x) if(length(x) > 1) sd(x, na.rm = TRUE) else NA,
                          "var" = function(x) if(length(x) > 1) var(x, na.rm = TRUE) else NA,
                          "iqr" = function(x) if(length(x) > 0) IQR(x, na.rm = TRUE) else NA,
                          "q25" = function(x) if(length(x) > 0) quantile(x, 0.25, na.rm = TRUE) else NA,
                          "q75" = function(x) if(length(x) > 0) quantile(x, 0.75, na.rm = TRUE) else NA,
                          "mad" = function(x) if(length(x) > 0) mad(x, na.rm = TRUE) else NA
      )
      original_stat <- stat_func(data)

      if(is.na(original_stat) || !is.finite(original_stat)) {
        showNotification("Tidak dapat menghitung statistik untuk data ini", type = "error")
        return()
      }
      withProgress(message = 'Menjalankan Jackknife...', value = 0, {
        jackknife_stats <- numeric(n)

        for(i in 1:n) {
          jackknife_stats[i] <- stat_func(data[-i])
          incProgress(1/n, detail = paste("Iterasi", i, "dari", n))
        }
      })
      valid_stats <- jackknife_stats[is.finite(jackknife_stats)]

      if(length(valid_stats) < n * 0.8) {
        showNotification("Terlalu banyak hasil tidak valid dalam jackknife", type = "warning")
      }
      jackknife_mean <- mean(valid_stats, na.rm = TRUE)
      bias <- (n - 1) * (jackknife_mean - original_stat)
      corrected_estimate <- original_stat - bias
      se <- sqrt(((n-1)/n) * sum((valid_stats - jackknife_mean)^2, na.rm = TRUE))
      alpha <- 0.05
      t_val <- qt(1 - alpha/2, df = n-1)
      ci_lower <- corrected_estimate - t_val * se
      ci_upper <- corrected_estimate + t_val * se

      output$jackknifeResult <- renderPrint({
        cat("=== JACKKNIFE RESAMPLING ===\n")
        cat("Variabel:", input$jackknifeVar, "\n")
        cat("Statistik:", input$jackknifeStatistic, "\n")
        cat("Jumlah observasi:", n, "\n")
        cat("Observasi valid:", length(valid_stats), "\n\n")
        cat("Estimasi original:", formatC(original_stat, digits = 6, format = "f"), "\n")
        cat("Rata-rata jackknife:", formatC(jackknife_mean, digits = 6, format = "f"), "\n")
        cat("Bias:", formatC(bias, digits = 6, format = "f"), "\n")
        cat("Estimasi terkoreksi:", formatC(corrected_estimate, digits = 6, format = "f"), "\n")
        cat("Standard error:", formatC(se, digits = 6, format = "f"), "\n")
        cat("95% CI:", "[", formatC(ci_lower, digits = 4, format = "f"), ",",
            formatC(ci_upper, digits = 4, format = "f"), "]\n")
        cat("Efisiensi relatif:", formatC((se^2) / (sd(data)^2/n), digits = 4, format = "f"), "\n")
      })

      output$jackknifePlot <- renderPlot({
        plot_data <- data.frame(
          Index = 1:length(valid_stats),
          Statistic = valid_stats
        )

        p1 <- ggplot(plot_data, aes(x = Statistic)) +
          geom_histogram(fill = "#FF8C00", color = "white", bins = min(30, length(valid_stats)/3),
                         alpha = 0.8) +
          geom_vline(xintercept = original_stat, color = "red", linetype = "dashed", size = 1) +
          geom_vline(xintercept = jackknife_mean, color = "blue", linetype = "dotted", size = 1) +
          theme_minimal() +
          labs(title = "Distribusi Jackknife Replicates",
               x = paste("Statistik", input$jackknifeStatistic),
               y = "Frekuensi") +
          annotate("text", x = original_stat, y = Inf,
                   label = "Original", color = "red", vjust = 2, hjust = 1.1) +
          annotate("text", x = jackknife_mean, y = Inf,
                   label = "JK Mean", color = "blue", vjust = 3, hjust = 1.1)

        p2 <- ggplot(plot_data, aes(x = Index, y = Statistic)) +
          geom_line(color = "#FF8C00", alpha = 0.7) +
          geom_point(color = "#FF8C00", alpha = 0.8, size = 1) +
          geom_hline(yintercept = original_stat, color = "red", linetype = "dashed") +
          theme_minimal() +
          labs(title = "Jackknife Statistics by Leave-One-Out",
               x = "Observasi yang dihilangkan", y = paste("Statistik", input$jackknifeStatistic))

        grid.arrange(p1, p2, ncol = 2)
      })

      output$jackknifeInterpretation <- renderText({
        bias_pct <- abs(bias/original_stat) * 100

        interpretation <- paste(
          "Jackknife memberikan estimasi bias sebesar", formatC(bias, digits = 4, format = "f"),
          "(", formatC(bias_pct, digits = 2, format = "f"), "% dari nilai original)",
          "dan standard error", formatC(se, digits = 4, format = "f"), ".",
          "Estimasi terkoreksi adalah", formatC(corrected_estimate, digits = 4, format = "f"), "."
        )

        if(bias_pct < 1) {
          interpretation <- paste(interpretation, "Bias sangat kecil, estimator cukup unbiased.")
        } else if(bias_pct < 5) {
          interpretation <- paste(interpretation, "Bias moderat, koreksi jackknife bermanfaat.")
        } else {
          interpretation <- paste(interpretation, "Bias cukup besar, perlu kehati-hatian dalam interpretasi.")
        }

        interpretation
      })

      showNotification("Jackknife berhasil dijalankan!", type = "message")

    }, error = function(e) {
      showNotification(paste("Error dalam jackknife:", e$message), type = "error")
    })
  })
  output$bootstrapVarSelect <- renderUI({
    req(values$processedData)
    numeric_vars <- names(values$processedData)[sapply(values$processedData, is.numeric)]

    if(length(numeric_vars) == 0) {
      return(div(class = "alert alert-warning", "Tidak ada variabel numerik yang tersedia"))
    }

    selectInput("bootstrapVar", "Pilih variabel:",
                choices = numeric_vars,
                selected = numeric_vars[1])
  })

  observeEvent(input$runBootstrap, {
    req(input$bootstrapVar, input$bootstrapStatistic, input$bootstrapR)

    tryCatch({
      data <- values$processedData[[input$bootstrapVar]]
      data <- data[!is.na(data)]
      n <- length(data)
      if(n < 2) {
        showNotification("Data terlalu kecil untuk bootstrap (minimal n = 2)", type = "error")
        return()
      }

      if(input$bootstrapR < 100) {
        showNotification("Gunakan minimal 100 replikasi untuk hasil yang stabil", type = "warning")
      }
      stat_func <- switch(input$bootstrapStatistic,
                          "mean" = function(x, i) mean(x[i], na.rm = TRUE),
                          "median" = function(x, i) median(x[i], na.rm = TRUE),
                          "sd" = function(x, i) sd(x[i], na.rm = TRUE),
                          "var" = function(x, i) var(x[i], na.rm = TRUE),
                          "iqr" = function(x, i) IQR(x[i], na.rm = TRUE),
                          "q25" = function(x, i) quantile(x[i], 0.25, na.rm = TRUE),
                          "q75" = function(x, i) quantile(x[i], 0.75, na.rm = TRUE),
                          "mad" = function(x, i) mad(x[i], na.rm = TRUE),
                          "skewness" = function(x, i) {
                            if(require(moments, quietly = TRUE)) {
                              skewness(x[i])
                            } else {
                              x_i <- x[i]
                              n <- length(x_i)
                              if(n < 3) return(NA)
                              m <- mean(x_i)
                              s <- sd(x_i)
                              sum(((x_i - m)/s)^3) / n
                            }
                          },
                          "kurtosis" = function(x, i) {
                            if(require(moments, quietly = TRUE)) {
                              kurtosis(x[i])
                            } else {
                              x_i <- x[i]
                              n <- length(x_i)
                              if(n < 4) return(NA)
                              m <- mean(x_i)
                              s <- sd(x_i)
                              sum(((x_i - m)/s)^4) / n - 3
                            }
                          }
      )
      withProgress(message = 'Menjalankan Bootstrap...', value = 0, {
        progress_steps <- 20
        step_size <- ceiling(input$bootstrapR / progress_steps)
        boot_results <- boot(data, stat_func, R = input$bootstrapR)
        incProgress(1, detail = paste("Bootstrap selesai:", input$bootstrapR, "replikasi"))
      })
      ci_level <- as.numeric(input$bootstrapCI)
      ci_methods <- c("norm", "basic", "perc", "bca")
      ci_results <- list()

      for(method in ci_methods) {
        ci_results[[method]] <- tryCatch({
          boot.ci(boot_results, conf = ci_level, type = method)
        }, error = function(e) NULL)
      }
      bca_ci <- if(!is.null(ci_results$bca)) {
        ci_results$bca$bca[4:5]
      } else if(!is.null(ci_results$perc)) {
        ci_results$perc$percent[4:5]
      } else {
        c(NA, NA)
      }
      boot_stats <- boot_results$t
      boot_stats <- boot_stats[is.finite(boot_stats)]

      output$bootstrapResult <- renderPrint({
        cat("=== BOOTSTRAP RESAMPLING ===\n")
        cat("Variabel:", input$bootstrapVar, "\n")
        cat("Statistik:", input$bootstrapStatistic, "\n")
        cat("Jumlah replikasi:", input$bootstrapR, "\n")
        cat("Replikasi valid:", length(boot_stats), "\n")
        cat("Tingkat kepercayaan:", ci_level*100, "%\n\n")

        cat("Estimasi original:", formatC(boot_results$t0, digits = 6, format = "f"), "\n")
        cat("Rata-rata bootstrap:", formatC(mean(boot_stats), digits = 6, format = "f"), "\n")
        cat("Standard error:", formatC(sd(boot_stats), digits = 6, format = "f"), "\n")
        cat("Bias:", formatC(mean(boot_stats) - boot_results$t0, digits = 6, format = "f"), "\n")
        if(!is.na(bca_ci[1])) {
          cat(paste0(ci_level*100, "% BCa CI: ["),
              formatC(bca_ci[1], digits = 4, format = "f"), ",",
              formatC(bca_ci[2], digits = 4, format = "f"), "]\n")
        }

        if(!is.null(ci_results$perc)) {
          perc_ci <- ci_results$perc$percent[4:5]
          cat(paste0(ci_level*100, "% Percentile CI: ["),
              formatC(perc_ci[1], digits = 4, format = "f"), ",",
              formatC(perc_ci[2], digits = 4, format = "f"), "]\n")
        }
        cat("\n--- DIAGNOSTICS ---\n")
        cat("Skewness bootstrap dist:", formatC(skewness(boot_stats), digits = 3, format = "f"), "\n")
        cat("Kurtosis bootstrap dist:", formatC(kurtosis(boot_stats), digits = 3, format = "f"), "\n")
        cat("% extreme values:", formatC(mean(abs(scale(boot_stats)) > 3) * 100, digits = 1, format = "f"), "%\n")
      })

      output$bootstrapPlot <- renderPlot({
        plot_data <- data.frame(Statistic = boot_stats)
        p1 <- ggplot(plot_data, aes(x = Statistic)) +
          geom_histogram(aes(y = ..density..), fill = "#FF8C00", color = "white",
                         bins = min(50, length(boot_stats)/10), alpha = 0.8) +
          geom_density(color = "blue", size = 1, alpha = 0.7) +
          geom_vline(xintercept = boot_results$t0, color = "red", linetype = "dashed", size = 1) +
          geom_vline(xintercept = mean(boot_stats), color = "green", linetype = "dotted", size = 1) +
          theme_minimal() +
          labs(title = "Bootstrap Distribution dengan Density",
               x = paste("Statistik", input$bootstrapStatistic),
               y = "Density")
        p2 <- ggplot(plot_data, aes(sample = Statistic)) +
          stat_qq(color = "#FF8C00", alpha = 0.7) +
          stat_qq_line(color = "red", linetype = "dashed") +
          theme_minimal() +
          labs(title = "Q-Q Plot (Normality Check)",
               x = "Theoretical Quantiles",
               y = "Sample Quantiles")
        trace_data <- data.frame(
          Iteration = 1:min(1000, length(boot_stats)),
          Statistic = boot_stats[1:min(1000, length(boot_stats))]
        )

        p3 <- ggplot(trace_data, aes(x = Iteration, y = Statistic)) +
          geom_line(color = "#FF8C00", alpha = 0.7) +
          geom_hline(yintercept = boot_results$t0, color = "red", linetype = "dashed") +
          theme_minimal() +
          labs(title = "Bootstrap Trace Plot (first 1000)",
               x = "Bootstrap Sample", y = paste("Statistik", input$bootstrapStatistic))

        grid.arrange(p1, p2, p3, ncol = 2, nrow = 2, layout_matrix = rbind(c(1,2), c(3,3)))
      })

      output$bootstrapInterpretation <- renderText({
        bias <- mean(boot_stats) - boot_results$t0
        se <- sd(boot_stats)
        cv <- abs(se / boot_results$t0) * 100  # Coefficient of variation

        interpretation <- paste(
          "Bootstrap dengan", input$bootstrapR, "replikasi menunjukkan distribusi sampling statistik.",
          "Standard error bootstrap adalah", formatC(se, digits = 4, format = "f"),
          "(CV =", formatC(cv, digits = 2, format = "f"), "%) dan bias estimasi adalah",
          formatC(bias, digits = 4, format = "f"), "."
        )

        if(!is.na(bca_ci[1])) {
          interpretation <- paste(interpretation,
                                  "Interval kepercayaan BCa", ci_level*100, "% adalah [",
                                  formatC(bca_ci[1], digits = 4, format = "f"), ",",
                                  formatC(bca_ci[2], digits = 4, format = "f"), "].")
        }
        if(length(boot_stats) < input$bootstrapR * 0.95) {
          interpretation <- paste(interpretation, "Perhatian: beberapa bootstrap sample gagal.")
        }

        if(abs(skewness(boot_stats)) > 1) {
          interpretation <- paste(interpretation, "Bootstrap distribution cukup skewed, pertimbangkan transformasi data.")
        }

        interpretation
      })

      showNotification("Bootstrap berhasil dijalankan!", type = "message")

    }, error = function(e) {
      showNotification(paste("Error dalam bootstrap:", e$message), type = "error")
    })
  })
  output$permutationVarSelect <- renderUI({
    req(values$processedData)
    numeric_vars <- names(values$processedData)[sapply(values$processedData, is.numeric)]

    if(length(numeric_vars) == 0) {
      return(div(class = "alert alert-warning", "Tidak ada variabel numerik yang tersedia"))
    }

    selectInput("permutationVar", "Pilih variabel:",
                choices = numeric_vars,
                selected = numeric_vars[1])
  })

  output$permutationGroupSelect <- renderUI({
    req(values$processedData)
    categ_vars <- names(values$processedData)[sapply(values$processedData, function(x) {
      if(is.factor(x) || is.character(x)) {
        unique_vals <- length(unique(x[!is.na(x)]))
        return(unique_vals >= 2 && unique_vals <= 10)  # Reasonable number of groups
      }
      return(FALSE)
    })]

    if(length(categ_vars) == 0) {
      return(div(class = "alert alert-warning", "Tidak ada variabel kategorik yang sesuai (2-10 grup)"))
    }

    selectInput("permutationGroup", "Pilih variabel grup:",
                choices = categ_vars,
                selected = categ_vars[1])
  })

  observeEvent(input$runPermutation, {
    req(input$permutationVar, input$permutationGroup, input$permutationR)

    tryCatch({
      df <- values$processedData
      df <- df[complete.cases(df[[input$permutationVar]], df[[input$permutationGroup]]), ]
      if(nrow(df) < 6) {
        showNotification("Data terlalu kecil untuk permutation test (minimal n = 6)", type = "error")
        return()
      }

      groups <- unique(df[[input$permutationGroup]])
      k <- length(groups)

      if(k < 2) {
        showNotification("Harus ada minimal 2 grup untuk permutation test", type = "error")
        return()
      }

      if(k > 10) {
        showNotification("Terlalu banyak grup (>10), hasil mungkin tidak reliable", type = "warning")
      }
      group_sizes <- table(df[[input$permutationGroup]])
      min_group_size <- min(group_sizes)

      if(min_group_size < 2) {
        showNotification("Setiap grup harus memiliki minimal 2 observasi", type = "error")
        return()
      }
      formula <- as.formula(paste(input$permutationVar, "~", input$permutationGroup))
      aov_result <- aov(formula, data = df)
      aov_summary <- summary(aov_result)
      original_f <- aov_summary[[1]]$`F value`[1]
      group_means <- tapply(df[[input$permutationVar]], df[[input$permutationGroup]], mean, na.rm = TRUE)
      max_diff <- max(group_means) - min(group_means)
      withProgress(message = 'Menjalankan Permutation Test...', value = 0, {
        permutation_f <- numeric(input$permutationR)
        permutation_max_diff <- numeric(input$permutationR)

        for(i in 1:input$permutationR) {
          df_perm <- df
          df_perm[[input$permutationGroup]] <- sample(df[[input$permutationGroup]])
          aov_perm <- aov(formula, data = df_perm)
          permutation_f[i] <- summary(aov_perm)[[1]]$`F value`[1]
          perm_means <- tapply(df_perm[[input$permutationVar]], df_perm[[input$permutationGroup]], mean, na.rm = TRUE)
          permutation_max_diff[i] <- max(perm_means) - min(perm_means)

          if(i %% max(1, input$permutationR %/% 20) == 0) {
            incProgress(1/20, detail = paste("Permutasi", i, "dari", input$permutationR))
          }
        }
      })
      p_value_f <- mean(permutation_f >= original_f, na.rm = TRUE)
      p_value_diff <- mean(permutation_max_diff >= max_diff, na.rm = TRUE)
      eta_squared <- aov_summary[[1]]$`Sum Sq`[1] / sum(aov_summary[[1]]$`Sum Sq`)

      output$permutationResult <- renderPrint({
        cat("=== PERMUTATION TEST ===\n")
        cat("Variabel:", input$permutationVar, "\n")
        cat("Grup:", input$permutationGroup, "\n")
        cat("Jumlah grup:", k, "\n")
        cat("Jumlah observasi:", nrow(df), "\n")
        cat("Jumlah permutasi:", input$permutationR, "\n\n")

        cat("--- GROUP STATISTICS ---\n")
        for(i in 1:k) {
          group_data <- df[df[[input$permutationGroup]] == groups[i], input$permutationVar]
          cat(paste0("Grup ", groups[i], ": n=", length(group_data),
                     ", mean=", formatC(mean(group_data), digits = 3, format = "f"),
                     ", sd=", formatC(sd(group_data), digits = 3, format = "f")), "\n")
        }

        cat("\n--- TEST STATISTICS ---\n")
        cat("F-statistic original:", formatC(original_f, digits = 4, format = "f"), "\n")
        cat("Max mean difference:", formatC(max_diff, digits = 4, format = "f"), "\n")
        cat("Effect size (η²):", formatC(eta_squared, digits = 4, format = "f"), "\n")

        cat("\n--- P-VALUES ---\n")
        cat("P-value (F-test):", formatC(p_value_f, digits = 4, format = "f"), "\n")
        cat("P-value (Max diff):", formatC(p_value_diff, digits = 4, format = "f"), "\n")

        cat("\n--- CONCLUSION ---\n")
        alpha <- 0.05
        significant_f <- p_value_f < alpha
        significant_diff <- p_value_diff < alpha

        cat("F-test:", ifelse(significant_f, "Signifikan", "Tidak signifikan"), "pada α =", alpha, "\n")
        cat("Max diff test:", ifelse(significant_diff, "Signifikan", "Tidak signifikan"), "pada α =", alpha, "\n")
        if(eta_squared < 0.01) {
          cat("Effect size: Kecil (< 1% variance explained)\n")
        } else if(eta_squared < 0.06) {
          cat("Effect size: Sedang (1-6% variance explained)\n")
        } else if(eta_squared < 0.14) {
          cat("Effect size: Besar (6-14% variance explained)\n")
        } else {
          cat("Effect size: Sangat besar (> 14% variance explained)\n")
        }
      })

      output$permutationPlot <- renderPlot({
        p1 <- ggplot(data.frame(F_Statistic = permutation_f), aes(x = F_Statistic)) +
          geom_histogram(fill = "#FFD700", color = "white", bins = min(50, input$permutationR/20), alpha = 0.8) +
          geom_vline(xintercept = original_f, color = "red", linetype = "dashed", size = 1) +
          geom_vline(xintercept = mean(permutation_f), color = "blue", linetype = "dotted", size = 1) +
          theme_minimal() +
          labs(title = "Distribusi Permutasi F-Statistic",
               x = "F-Statistic", y = "Frekuensi") +
          annotate("text", x = original_f, y = Inf,
                   label = paste("Original\np =", formatC(p_value_f, digits = 3, format = "f")),
                   color = "red", vjust = 2, hjust = 1.1)
        p2 <- ggplot(data.frame(Max_Diff = permutation_max_diff), aes(x = Max_Diff)) +
          geom_histogram(fill = "#98FB98", color = "white", bins = min(50, input$permutationR/20), alpha = 0.8) +
          geom_vline(xintercept = max_diff, color = "red", linetype = "dashed", size = 1) +
          geom_vline(xintercept = mean(permutation_max_diff), color = "blue", linetype = "dotted", size = 1) +
          theme_minimal() +
          labs(title = "Distribusi Max Mean Difference",
               x = "Max Mean Difference", y = "Frekuensi") +
          annotate("text", x = max_diff, y = Inf,
                   label = paste("Original\np =", formatC(p_value_diff, digits = 3, format = "f")),
                   color = "red", vjust = 2, hjust = 1.1)
        p3 <- ggplot(df, aes_string(x = input$permutationGroup, y = input$permutationVar)) +
          geom_boxplot(fill = "#87CEEB", alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5, color = "#4682B4") +
          stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
          theme_minimal() +
          labs(title = "Data Original by Group",
               x = input$permutationGroup,
               y = input$permutationVar) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        if(input$permutationR >= 100) {
          cumulative_p <- cumsum(permutation_f >= original_f) / (1:input$permutationR)
          p4_data <- data.frame(
            Iteration = 1:input$permutationR,
            Cumulative_P = cumulative_p
          )

          p4 <- ggplot(p4_data, aes(x = Iteration, y = Cumulative_P)) +
            geom_line(color = "#FF6347", size = 0.8) +
            geom_hline(yintercept = 0.05, color = "red", linetype = "dashed", alpha = 0.7) +
            geom_hline(yintercept = p_value_f, color = "blue", linetype = "dotted", size = 1) +
            theme_minimal() +
            labs(title = "Konvergensi P-value",
                 x = "Jumlah Permutasi", y = "Cumulative P-value") +
            ylim(0, max(0.2, max(cumulative_p[1:min(100, length(cumulative_p))])))

          grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
        } else {
          grid.arrange(p1, p2, p3, ncol = 2, nrow = 2, layout_matrix = rbind(c(1,2), c(3,3)))
        }
      })

      output$permutationInterpretation <- renderText({
        interpretation <- ""

        if(p_value_f < 0.001) {
          interpretation <- paste("Permutation test menunjukkan perbedaan yang sangat signifikan antara grup (p < 0.001).")
        } else if(p_value_f < 0.01) {
          interpretation <- paste("Permutation test menunjukkan perbedaan yang sangat signifikan antara grup (p =",
                                  formatC(p_value_f, digits = 3, format = "f"), ").")
        } else if(p_value_f < 0.05) {
          interpretation <- paste("Permutation test menunjukkan perbedaan yang signifikan antara grup (p =",
                                  formatC(p_value_f, digits = 3, format = "f"), ").")
        } else if(p_value_f < 0.10) {
          interpretation <- paste("Permutation test menunjukkan perbedaan yang marginally significant antara grup (p =",
                                  formatC(p_value_f, digits = 3, format = "f"), ").")
        } else {
          interpretation <- paste("Permutation test menunjukkan tidak ada perbedaan yang signifikan antara grup (p =",
                                  formatC(p_value_f, digits = 3, format = "f"), ").")
        }

        # Add effect size interpretation
        if(eta_squared >= 0.14) {
          interpretation <- paste(interpretation, "Effect size sangat besar (η² =",
                                  formatC(eta_squared, digits = 3, format = "f"), "), perbedaan praktis sangat meaningful.")
        } else if(eta_squared >= 0.06) {
          interpretation <- paste(interpretation, "Effect size besar (η² =",
                                  formatC(eta_squared, digits = 3, format = "f"), "), perbedaan praktis meaningful.")
        } else if(eta_squared >= 0.01) {
          interpretation <- paste(interpretation, "Effect size sedang (η² =",
                                  formatC(eta_squared, digits = 3, format = "f"), "), ada perbedaan praktis.")
        } else {
          interpretation <- paste(interpretation, "Effect size kecil (η² =",
                                  formatC(eta_squared, digits = 3, format = "f"), "), perbedaan praktis minimal.")
        }

        # Add reliability note
        if(input$permutationR < 1000) {
          interpretation <- paste(interpretation, "Catatan: Gunakan ≥1000 permutasi untuk p-value yang lebih stabil.")
        }

        # Add assumption check
        group_vars <- tapply(df[[input$permutationVar]], df[[input$permutationGroup]], var, na.rm = TRUE)
        if(max(group_vars, na.rm = TRUE) / min(group_vars, na.rm = TRUE) > 4) {
          interpretation <- paste(interpretation, "Perhatian: Variance antar grup cukup berbeda, pertimbangkan transformasi data.")
        }

        interpretation
      })

      showNotification("Permutation test berhasil dijalankan!", type = "message")

    }, error = function(e) {
      showNotification(paste("Error dalam permutation test:", e$message), type = "error")
    })
  })
  output$cvVarSelect <- renderUI({
    req(values$processedData)
    numeric_vars <- names(values$processedData)[sapply(values$processedData, is.numeric)]

    if(length(numeric_vars) < 2) {
      return(div(class = "alert alert-warning", "Minimal 2 variabel numerik diperlukan untuk cross-validation"))
    }

    list(
      selectInput("cvResponse", "Pilih variabel response (Y):",
                  choices = numeric_vars, selected = numeric_vars[1]),
      selectInput("cvPredictors", "Pilih variabel prediktor (X):",
                  choices = numeric_vars, multiple = TRUE, selected = numeric_vars[2])
    )
  })

  observeEvent(input$runCV, {
    req(input$cvResponse, input$cvPredictors, input$cvFolds, input$cvRepeats)

    tryCatch({
      df <- values$processedData
      response_var <- input$cvResponse
      predictor_vars <- input$cvPredictors
      complete_vars <- c(response_var, predictor_vars)
      df_complete <- df[complete.cases(df[complete_vars]), ]

      if(nrow(df_complete) < input$cvFolds * 2) {
        showNotification("Data terlalu kecil untuk cross-validation yang dipilih", type = "error")
        return()
      }

      n <- nrow(df_complete)
      k_folds <- input$cvFolds
      n_repeats <- input$cvRepeats
      cv_results <- list()
      all_predictions <- numeric()
      all_actuals <- numeric()

      withProgress(message = 'Menjalankan Cross-Validation...', value = 0, {

        for(repeat_i in 1:n_repeats) {
          shuffled_indices <- sample(1:n)
          fold_size <- floor(n / k_folds)

          fold_predictions <- numeric(n)
          fold_actuals <- numeric(n)

          for(fold in 1:k_folds) {
            start_idx <- (fold - 1) * fold_size + 1
            end_idx <- ifelse(fold == k_folds, n, fold * fold_size)
            test_indices <- shuffled_indices[start_idx:end_idx]
            train_indices <- shuffled_indices[-c(start_idx:end_idx)]
            train_data <- df_complete[train_indices, ]
            test_data <- df_complete[test_indices, ]
            formula_str <- paste(response_var, "~", paste(predictor_vars, collapse = " + "))
            model <- lm(as.formula(formula_str), data = train_data)
            predictions <- predict(model, newdata = test_data)
            fold_predictions[test_indices] <- predictions
            fold_actuals[test_indices] <- test_data[[response_var]]
          }

          cv_results[[repeat_i]] <- list(
            predictions = fold_predictions,
            actuals = fold_actuals,
            rmse = sqrt(mean((fold_predictions - fold_actuals)^2)),
            mae = mean(abs(fold_predictions - fold_actuals)),
            r_squared = cor(fold_predictions, fold_actuals)^2
          )

          all_predictions <- c(all_predictions, fold_predictions)
          all_actuals <- c(all_actuals, fold_actuals)

          incProgress(1/n_repeats, detail = paste("Repeat", repeat_i, "dari", n_repeats))
        }
      })
      rmse_values <- sapply(cv_results, function(x) x$rmse)
      mae_values <- sapply(cv_results, function(x) x$mae)
      r2_values <- sapply(cv_results, function(x) x$r_squared)
      overall_rmse <- sqrt(mean((all_predictions - all_actuals)^2))
      overall_mae <- mean(abs(all_predictions - all_actuals))
      overall_r2 <- cor(all_predictions, all_actuals)^2

      output$cvResult <- renderPrint({
        cat("=== CROSS-VALIDATION RESULTS ===\n")
        cat("Response variable:", response_var, "\n")
        cat("Predictor variables:", paste(predictor_vars, collapse = ", "), "\n")
        cat("CV scheme:", k_folds, "-fold,", n_repeats, "repeats\n")
        cat("Total observations:", n, "\n\n")

        cat("--- PERFORMANCE METRICS ---\n")
        cat("RMSE: mean =", formatC(mean(rmse_values), digits = 4, format = "f"),
            ", sd =", formatC(sd(rmse_values), digits = 4, format = "f"), "\n")
        cat("MAE:  mean =", formatC(mean(mae_values), digits = 4, format = "f"),
            ", sd =", formatC(sd(mae_values), digits = 4, format = "f"), "\n")
        cat("R²:   mean =", formatC(mean(r2_values), digits = 4, format = "f"),
            ", sd =", formatC(sd(r2_values), digits = 4, format = "f"), "\n")

        cat("\n--- OVERALL PERFORMANCE ---\n")
        cat("Overall RMSE:", formatC(overall_rmse, digits = 4, format = "f"), "\n")
        cat("Overall MAE:", formatC(overall_mae, digits = 4, format = "f"), "\n")
        cat("Overall R²:", formatC(overall_r2, digits = 4, format = "f"), "\n")
        cv_rmse <- sd(rmse_values) / mean(rmse_values)
        cat("CV coefficient (RMSE):", formatC(cv_rmse, digits = 4, format = "f"),
            ifelse(cv_rmse < 0.1, " (stable)", ifelse(cv_rmse < 0.2, " (moderate)", " (unstable)")), "\n")
      })

      output$cvPlot <- renderPlot({
        p1 <- ggplot(data.frame(Actual = all_actuals, Predicted = all_predictions),
                     aes(x = Actual, y = Predicted)) +
          geom_point(alpha = 0.6, color = "#4682B4") +
          geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
          geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
          theme_minimal() +
          labs(title = paste("Predicted vs Actual (R² =", formatC(overall_r2, digits = 3, format = "f"), ")"),
               x = "Actual Values", y = "Predicted Values") +
          coord_equal()
        residuals <- all_predictions - all_actuals
        p2 <- ggplot(data.frame(Predicted = all_predictions, Residuals = residuals),
                     aes(x = Predicted, y = Residuals)) +
          geom_point(alpha = 0.6, color = "#FF6347") +
          geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
          geom_smooth(method = "loess", se = TRUE, color = "blue", alpha = 0.3) +
          theme_minimal() +
          labs(title = "Residuals vs Predicted",
               x = "Predicted Values", y = "Residuals")
        metrics_df <- data.frame(
          Metric = rep(c("RMSE", "MAE", "R²"), each = n_repeats),
          Value = c(rmse_values, mae_values, r2_values)
        )

        p3 <- ggplot(metrics_df, aes(x = Metric, y = Value, fill = Metric)) +
          geom_boxplot(alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.6) +
          theme_minimal() +
          labs(title = "CV Performance Distribution",
               x = "Metric", y = "Value") +
          theme(legend.position = "none")
        if(n_repeats > 1) {
          learning_df <- data.frame(
            Repeat = rep(1:n_repeats, 3),
            Metric = rep(c("RMSE", "MAE", "R²"), each = n_repeats),
            Value = c(rmse_values, mae_values, r2_values)
          )

          p4 <- ggplot(learning_df, aes(x = Repeat, y = Value, color = Metric)) +
            geom_line(size = 1) +
            geom_point(size = 2) +
            facet_wrap(~Metric, scales = "free_y") +
            theme_minimal() +
            labs(title = "Performance Across Repeats",
                 x = "Repeat Number", y = "Metric Value") +
            theme(legend.position = "none")

          grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
        } else {
          grid.arrange(p1, p2, p3, ncol = 2, nrow = 2, layout_matrix = rbind(c(1,2), c(3,3)))
        }
      })

      output$cvInterpretation <- renderText({
        interpretation <- paste(
          "Cross-validation dengan", k_folds, "fold dan", n_repeats, "repeats menunjukkan",
          "performa model dengan R² =", formatC(overall_r2, digits = 3, format = "f"), "."
        )

        if(overall_r2 > 0.8) {
          interpretation <- paste(interpretation, "Model menunjukkan fit yang sangat baik.")
        } else if(overall_r2 > 0.6) {
          interpretation <- paste(interpretation, "Model menunjukkan fit yang baik.")
        } else if(overall_r2 > 0.4) {
          interpretation <- paste(interpretation, "Model menunjukkan fit yang moderate.")
        } else {
          interpretation <- paste(interpretation, "Model menunjukkan fit yang lemah.")
        }

        cv_stability <- sd(r2_values) / mean(r2_values)
        if(cv_stability < 0.1) {
          interpretation <- paste(interpretation, "Model sangat stabil across folds.")
        } else if(cv_stability < 0.2) {
          interpretation <- paste(interpretation, "Model cukup stabil across folds.")
        } else {
          interpretation <- paste(interpretation, "Model kurang stabil, pertimbangkan regularization atau feature selection.")
        }

        interpretation
      })

      showNotification("Cross-validation berhasil dijalankan!", type = "message")

    }, error = function(e) {
      showNotification(paste("Error dalam cross-validation:", e$message), type = "error")
    })
  })
  output$mapVarSelect <- renderUI({
    req(values$processedData, values$geoData)
    indikator1 <- names(values$processedData)[sapply(values$processedData, is.numeric)]
    #indikator2 <- names(values$mdsData)[sapply(values$mdsData, is.numeric)] opsional untuk pengembangan kedepan jika ada
    #indikator_all <- setdiff(c(indikator1, indikator2), c("DISTRICTCODE", "POPULATION"))
    daftar_kabupaten <- sort(unique(values$geoData$WADMKK))

    tagList(
      selectInput("mapRegion", "Pilih Kabupaten/Kota:",
                  choices = c("Seluruh Indonesia", daftar_kabupaten),
                  selected = "Seluruh Indonesia"),
      selectInput("mapVariable", "Pilih Indikator Peta:",
                  choices = indikator1, selected = indikator1[1])
    )
  })

  output$mapVisualization <- renderLeaflet({
    req(values$geoData, values$processedData, input$mapVariable)

    df <- values$processedData
    mds <- values$mdsData
    geo <- values$geoData
    df$DISTRICTCODE <- as.character(df$DISTRICTCODE)
    df$KODE_JOIN <- paste0(substr(df$DISTRICTCODE, 1, 2), ".", substr(df$DISTRICTCODE, 3, 4))
    gabung <- left_join(geo, bind_cols(df, mds), by = "KODE_JOIN")
    if (!"WADMKK" %in% names(gabung)) return(NULL)
    if (is.null(input$mapVariable) || !(input$mapVariable %in% names(gabung))) return(NULL)
    if (!is.null(input$mapRegion) && input$mapRegion != "Seluruh Indonesia") {
      gabung <- gabung %>% filter(WADMKK == input$mapRegion)
    }
    var <- input$mapVariable
    pal <- colorNumeric("YlGnBu", domain = gabung[[var]], na.color = "#ccc")

    leaflet(gabung) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(gabung[[var]]),
        color = "#444", weight = 1, fillOpacity = 0.7,
        label = ~paste0(WADMKK, ", ", WADMPR, "<br>", var, ": ", round(gabung[[var]], 2)),
        highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
      ) %>%
      addLegend(
        position = "topright",
        pal = pal,
        values = gabung[[var]],
        title = var
      )
  })


  output$mapInterpretation <- renderText({
    req(input$mapVariable)
    paste("Peta ini menggambarkan distribusi indikator", input$mapVariable, "di", input$mapRegion)
  })
  dataInput <- reactive({
    req(values$processedData)
    values$processedData
  })
  getNumericColumns <- reactive({
    req(dataInput())
    df <- dataInput()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    return(numeric_cols)
  })
  getCategoricalColumns <- reactive({
    req(dataInput())
    df <- dataInput()
    cat_cols <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    return(cat_cols)
  })
  getBinaryColumns <- reactive({
    req(dataInput())
    df <- dataInput()
    binary_cols <- names(df)[sapply(df, function(x) {
      if(is.numeric(x)) {
        unique_vals <- unique(x[!is.na(x)])
        return(length(unique_vals) == 2 && all(unique_vals %in% c(0, 1)))
      }
      return(FALSE)
    })]
    return(binary_cols)
  })

  output$inferensia_input_ui <- renderUI({
    req(input$uji_statistik)

    switch(input$uji_statistik,
           "Uji Rata-rata 1 Populasi" = tagList(
             selectInput("kolomNumerik", "Pilih Variabel Numerik:",
                         choices = getNumericColumns(), selected = NULL),
             numericInput("mu0", "Nilai Hipotesis (μ₀):", 0),
             div(class = "hypothesis-box",
                 h5("Hipotesis Uji Rata-rata 1 Populasi:", style = "color: #495057; font-weight: bold;"),
                 div(style = "margin-left: 10px;",
                     p(HTML("<strong>H₀:</strong> μ = μ₀ (rata-rata populasi sama dengan nilai yang dihipotesiskan)"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     p(HTML("<strong>H₁:</strong> μ ≠ μ₀ (rata-rata populasi tidak sama dengan nilai yang dihipotesiskan)"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     hr(style = "margin: 10px 0; border-color: #dee2e6;"),
                     p(HTML("<strong>Kriteria Keputusan:</strong>"),
                       style = "margin: 5px 0; color: #495057; font-weight: bold;"),
                     p(HTML("• Jika <em>p-value</em> > α (0,05) → Terima H₀ (rata-rata = μ₀)"),
                       style = "margin: 2px 0 2px 15px; color: #28a745; font-size: 14px;"),
                     p(HTML("• Jika <em>p-value</em> ≤ α (0,05) → Tolak H₀ (rata-rata ≠ μ₀)"),
                       style = "margin: 2px 0 2px 15px; color: #dc3545; font-size: 14px;")
                 )
             ),
             helpText("Uji apakah rata-rata populasi sama dengan nilai tertentu")
           ),

           "Uji Rata-rata 2 Populasi (Independen)" = tagList(
             selectInput("kolomNumerik", "Variabel Numerik 1:",
                         choices = getNumericColumns()),
             selectInput("kolomNumerik2", "Variabel Numerik 2:",
                         choices = getNumericColumns()),
             div(class = "hypothesis-box",
                 h5("Hipotesis Uji Rata-rata 2 Populasi (Independen):", style = "color: #495057; font-weight: bold;"),
                 div(style = "margin-left: 10px;",
                     p(HTML("<strong>H₀:</strong> μ₁ = μ₂ (rata-rata kedua populasi sama)"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     p(HTML("<strong>H₁:</strong> μ₁ ≠ μ₂ (rata-rata kedua populasi berbeda)"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     hr(style = "margin: 10px 0; border-color: #dee2e6;"),
                     p(HTML("<strong>Kriteria Keputusan:</strong>"),
                       style = "margin: 5px 0; color: #495057; font-weight: bold;"),
                     p(HTML("• Jika <em>p-value</em> > α (0,05) → Terima H₀ (μ₁ = μ₂)"),
                       style = "margin: 2px 0 2px 15px; color: #28a745; font-size: 14px;"),
                     p(HTML("• Jika <em>p-value</em> ≤ α (0,05) → Tolak H₀ (μ₁ ≠ μ₂)"),
                       style = "margin: 2px 0 2px 15px; color: #dc3545; font-size: 14px;")
                 )
             ),
             helpText("Membandingkan rata-rata dua variabel numerik yang independen"),
             checkboxInput("equal_var", "Asumsi varians sama", value = FALSE)
           ),

           "Uji Rata-rata 2 Populasi (Berpasangan)" = tagList(
             selectInput("kolomNumerik", "Variabel 1:",
                         choices = getNumericColumns()),
             selectInput("kolomNumerik2", "Variabel 2:",
                         choices = getNumericColumns()),
             div(class = "hypothesis-box",
                 h5("Hipotesis Uji Rata-rata 2 Populasi (Berpasangan):", style = "color: #495057; font-weight: bold;"),
                 div(style = "margin-left: 10px;",
                     p(HTML("<strong>H₀:</strong> μ<sub>D</sub> = 0 atau μ₁ = μ₂ (tidak ada perbedaan rata-rata)"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     p(HTML("<strong>H₁:</strong> μ<sub>D</sub> ≠ 0 atau μ₁ ≠ μ₂ (ada perbedaan rata-rata)"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     hr(style = "margin: 10px 0; border-color: #dee2e6;"),
                     p(HTML("<strong>Kriteria Keputusan:</strong>"),
                       style = "margin: 5px 0; color: #495057; font-weight: bold;"),
                     p(HTML("• Jika <em>p-value</em> > α (0,05) → Terima H₀ (tidak ada perbedaan)"),
                       style = "margin: 2px 0 2px 15px; color: #28a745; font-size: 14px;"),
                     p(HTML("• Jika <em>p-value</em> ≤ α (0,05) → Tolak H₀ (ada perbedaan)"),
                       style = "margin: 2px 0 2px 15px; color: #dc3545; font-size: 14px;")
                 )
             ),
             helpText("Membandingkan rata-rata dua variabel yang berpasangan (paired)")
           ),

           "Uji Proporsi 1 Populasi" = tagList(
             numericInput("n_total", "Jumlah Total Pengamatan (n):", 100, min = 1),
             numericInput("n_success", "Jumlah Sukses/Berhasil:", 50, min = 0),
             numericInput("p0", "Proporsi Hipotesis (p₀):", 0.5, min = 0, max = 1, step = 0.01),
             div(class = "hypothesis-box",
                 h5("Hipotesis Uji Proporsi 1 Populasi:", style = "color: #495057; font-weight: bold;"),
                 div(style = "margin-left: 10px;",
                     p(HTML("<strong>H₀:</strong> p = p₀ (proporsi populasi sama dengan nilai yang dihipotesiskan)"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     p(HTML("<strong>H₁:</strong> p ≠ p₀ (proporsi populasi tidak sama dengan nilai yang dihipotesiskan)"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     hr(style = "margin: 10px 0; border-color: #dee2e6;"),
                     p(HTML("<strong>Kriteria Keputusan:</strong>"),
                       style = "margin: 5px 0; color: #495057; font-weight: bold;"),
                     p(HTML("• Jika <em>p-value</em> > α (0,05) → Terima H₀ (proporsi = p₀)"),
                       style = "margin: 2px 0 2px 15px; color: #28a745; font-size: 14px;"),
                     p(HTML("• Jika <em>p-value</em> ≤ α (0,05) → Tolak H₀ (proporsi ≠ p₀)"),
                       style = "margin: 2px 0 2px 15px; color: #dc3545; font-size: 14px;")
                 )
             ),
             helpText("Masukkan jumlah total pengamatan dan jumlah sukses secara manual")
           ),

           "Uji Proporsi 2 Populasi" = tagList(
             h5("Kelompok 1:"),
             numericInput("n1_total", "Jumlah Total Pengamatan Kelompok 1:", 100, min = 1),
             numericInput("n1_success", "Jumlah Sukses Kelompok 1:", 45, min = 0),
             h5("Kelompok 2:"),
             numericInput("n2_total", "Jumlah Total Pengamatan Kelompok 2:", 100, min = 1),
             numericInput("n2_success", "Jumlah Sukses Kelompok 2:", 55, min = 0),
             div(class = "hypothesis-box",
                 h5("Hipotesis Uji Proporsi 2 Populasi:", style = "color: #495057; font-weight: bold;"),
                 div(style = "margin-left: 10px;",
                     p(HTML("<strong>H₀:</strong> p₁ = p₂ (proporsi kedua populasi sama)"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     p(HTML("<strong>H₁:</strong> p₁ ≠ p₂ (proporsi kedua populasi berbeda)"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     hr(style = "margin: 10px 0; border-color: #dee2e6;"),
                     p(HTML("<strong>Kriteria Keputusan:</strong>"),
                       style = "margin: 5px 0; color: #495057; font-weight: bold;"),
                     p(HTML("• Jika <em>p-value</em> > α (0,05) → Terima H₀ (p₁ = p₂)"),
                       style = "margin: 2px 0 2px 15px; color: #28a745; font-size: 14px;"),
                     p(HTML("• Jika <em>p-value</em> ≤ α (0,05) → Tolak H₀ (p₁ ≠ p₂)"),
                       style = "margin: 2px 0 2px 15px; color: #dc3545; font-size: 14px;")
                 )
             ),
             helpText("Masukkan data untuk kedua kelompok secara manual")
           ),


           "Uji Ragam 1 Populasi" = tagList(
             selectInput("kolomNumerik", "Pilih Variabel Numerik:",
                         choices = getNumericColumns()),
             numericInput("var0", "Variansi Hipotesis (σ²₀):", 1, min = 0.001),
             div(class = "hypothesis-box",
                 h5("Hipotesis Uji Ragam 1 Populasi:", style = "color: #495057; font-weight: bold;"),
                 div(style = "margin-left: 10px;",
                     p(HTML("<strong>H₀:</strong> σ² = σ₀² (ragam populasi sama dengan nilai yang dihipotesiskan)"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     p(HTML("<strong>H₁:</strong> σ² ≠ σ₀² (ragam populasi tidak sama dengan nilai yang dihipotesiskan)"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     hr(style = "margin: 10px 0; border-color: #dee2e6;"),
                     p(HTML("<strong>Kriteria Keputusan:</strong>"),
                       style = "margin: 5px 0; color: #495057; font-weight: bold;"),
                     p(HTML("• Jika <em>p-value</em> > α (0,05) → Terima H₀ (ragam = σ₀²)"),
                       style = "margin: 2px 0 2px 15px; color: #28a745; font-size: 14px;"),
                     p(HTML("• Jika <em>p-value</em> ≤ α (0,05) → Tolak H₀ (ragam ≠ σ₀²)"),
                       style = "margin: 2px 0 2px 15px; color: #dc3545; font-size: 14px;")
                 )
             ),
             helpText("Uji apakah variansi populasi sama dengan nilai tertentu")
           ),

           "Uji Ragam 2 Populasi" = tagList(
             selectInput("kolomNumerik", "Variabel Numerik 1 (Sampel 1):",
                         choices = getNumericColumns()),
             selectInput("kolomNumerik2", "Variabel Numerik 2 (Sampel 2):",
                         choices = getNumericColumns()),
             div(class = "hypothesis-box",
                 h5("Hipotesis Uji Ragam 2 Populasi:", style = "color: #495057; font-weight: bold;"),
                 div(style = "margin-left: 10px;",
                     p(HTML("<strong>H₀:</strong> σ₁² = σ₂² (ragam kedua populasi sama)"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     p(HTML("<strong>H₁:</strong> σ₁² ≠ σ₂² (ragam kedua populasi berbeda)"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     hr(style = "margin: 10px 0; border-color: #dee2e6;"),
                     p(HTML("<strong>Kriteria Keputusan:</strong>"),
                       style = "margin: 5px 0; color: #495057; font-weight: bold;"),
                     p(HTML("• Jika <em>p-value</em> > α (0,05) → Terima H₀ (σ₁² = σ₂²)"),
                       style = "margin: 2px 0 2px 15px; color: #28a745; font-size: 14px;"),
                     p(HTML("• Jika <em>p-value</em> ≤ α (0,05) → Tolak H₀ (σ₁² ≠ σ₂²)"),
                       style = "margin: 2px 0 2px 15px; color: #dc3545; font-size: 14px;")
                 )
             ),
             helpText("Membandingkan variansi dua sampel independen menggunakan Uji F. F = max(s₁²,s₂²)/min(s₁²,s₂²)")
           ),

           "Uji ANOVA 1 Arah" = tagList(
             selectInput("kolomNumerik", "Variabel Dependen (Numerik):",
                         choices = getNumericColumns()),
             selectInput("kolomKategori", "Variabel Independen (Kategorikal):",
                         choices = getCategoricalColumns()),
             div(class = "hypothesis-box",
                 h5("Hipotesis Uji ANOVA 1 Arah:", style = "color: #495057; font-weight: bold;"),
                 div(style = "margin-left: 10px;",
                     p(HTML("<strong>H₀:</strong> μ₁ = μ₂ = μ₃ = ... = μₖ (semua rata-rata kelompok sama)"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     p(HTML("<strong>H₁:</strong> Minimal ada satu μᵢ yang berbeda"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     hr(style = "margin: 10px 0; border-color: #dee2e6;"),
                     p(HTML("<strong>Kriteria Keputusan:</strong>"),
                       style = "margin: 5px 0; color: #495057; font-weight: bold;"),
                     p(HTML("• Jika <em>p-value</em> > α (0,05) → Terima H₀ (semua rata-rata sama)"),
                       style = "margin: 2px 0 2px 15px; color: #28a745; font-size: 14px;"),
                     p(HTML("• Jika <em>p-value</em> ≤ α (0,05) → Tolak H₀ (ada perbedaan rata-rata)"),
                       style = "margin: 2px 0 2px 15px; color: #dc3545; font-size: 14px;")
                 )
             ),
             helpText("Membandingkan rata-rata beberapa kelompok independen. H₀: μ₁ = μ₂ = μ₃ = ... = μₖ")
           ),

           "Uji ANOVA 2 Arah" = tagList(
             selectInput("kolomNumerik", "Variabel Dependen (Numerik):",
                         choices = getNumericColumns()),
             selectInput("kolomKategori", "Faktor A (Kategorikal):",
                         choices = getCategoricalColumns()),
             selectInput("kolomKategori2", "Faktor B (Kategorikal):",
                         choices = getCategoricalColumns()),
             checkboxInput("include_interaction", "Sertakan Interaksi", value = TRUE),
             div(class = "hypothesis-box",
                 h5("Hipotesis Uji ANOVA 2 Arah:", style = "color: #495057; font-weight: bold;"),
                 div(style = "margin-left: 10px;",
                     p(HTML("<strong>H₀<sub>A</sub>:</strong> Tidak ada efek faktor A (α₁ = α₂ = ... = αₐ = 0)"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     p(HTML("<strong>H₀<sub>B</sub>:</strong> Tidak ada efek faktor B (β₁ = β₂ = ... = βᵦ = 0)"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     p(HTML("<strong>H₀<sub>AB</sub>:</strong> Tidak ada efek interaksi A×B"),
                       style = "margin: 5px 0; color: #6c757d;"),
                     hr(style = "margin: 10px 0; border-color: #dee2e6;"),
                     p(HTML("<strong>Kriteria Keputusan:</strong>"),
                       style = "margin: 5px 0; color: #495057; font-weight: bold;"),
                     p(HTML("• Jika <em>p-value</em> > α (0,05) → Terima H₀ (tidak ada efek)"),
                       style = "margin: 2px 0 2px 15px; color: #28a745; font-size: 14px;"),
                     p(HTML("• Jika <em>p-value</em> ≤ α (0,05) → Tolak H₀ (ada efek signifikan)"),
                       style = "margin: 2px 0 2px 15px; color: #dc3545; font-size: 14px;")
                 )
             ),
             helpText("ANOVA dua arah dengan/tanpa interaksi. Menguji efek utama dan interaksi antara dua faktor.")
           )
    )
  })

  observeEvent(input$ujiTombol, {
    req(input$uji_statistik)

    tryCatch({
      df <- dataInput()
      alt <- input$alternative
      hasil <- NULL
      additional_info <- list()

      if (input$uji_statistik == "Uji Rata-rata 1 Populasi") {
        req(input$kolomNumerik, input$mu0)
        x <- df[[input$kolomNumerik]]
        x <- x[!is.na(x)]

        hasil <- t.test(x, mu = input$mu0, alternative = alt, conf.level = 1 - input$alpha)
        additional_info$sample_mean <- mean(x)
        additional_info$sample_sd <- sd(x)
        additional_info$sample_size <- length(x)

      } else if (input$uji_statistik == "Uji Rata-rata 2 Populasi (Independen)") {
        req(input$kolomNumerik, input$kolomNumerik2)
        x1 <- df[[input$kolomNumerik]]
        x2 <- df[[input$kolomNumerik2]]
        x1 <- x1[!is.na(x1)]
        x2 <- x2[!is.na(x2)]

        hasil <- t.test(x1, x2, var.equal = input$equal_var,
                        alternative = alt, conf.level = 1 - input$alpha)

        additional_info$group_means <- c(mean(x1), mean(x2))
        additional_info$group_sds <- c(sd(x1), sd(x2))
        additional_info$group_sizes <- c(length(x1), length(x2))
        additional_info$group_names <- c(input$kolomNumerik, input$kolomNumerik2)

      } else if (input$uji_statistik == "Uji Rata-rata 2 Populasi (Berpasangan)") {
        req(input$kolomNumerik, input$kolomNumerik2)
        x1 <- df[[input$kolomNumerik]]
        x2 <- df[[input$kolomNumerik2]]

        valid_pairs <- complete.cases(x1, x2)
        x1 <- x1[valid_pairs]
        x2 <- x2[valid_pairs]

        hasil <- t.test(x1, x2, paired = TRUE, alternative = alt, conf.level = 1 - input$alpha)
        additional_info$differences <- x1 - x2
        additional_info$mean_diff <- mean(x1 - x2)
        additional_info$n_pairs <- length(x1)

      } else if (input$uji_statistik == "Uji Proporsi 1 Populasi") {
        req(input$n_total, input$n_success, input$p0)
        if (input$n_success > input$n_total) {
          showNotification("Jumlah sukses tidak boleh lebih besar dari total!", type = "error")
          return()
        }

        n_total <- input$n_total
        n_success <- input$n_success

        hasil <- prop.test(x = n_success, n = n_total, p = input$p0,
                           alternative = alt, correct = FALSE, conf.level = 1 - input$alpha)

        additional_info$sample_prop <- n_success / n_total
        additional_info$n_success <- n_success
        additional_info$n_total <- n_total
        additional_info$n_failure <- n_total - n_success

      } else if (input$uji_statistik == "Uji Proporsi 2 Populasi") {
        req(input$n1_total, input$n1_success, input$n2_total, input$n2_success)
        if (input$n1_success > input$n1_total || input$n2_success > input$n2_total) {
          showNotification("Jumlah sukses tidak boleh lebih besar dari total untuk masing-masing kelompok!", type = "error")
          return()
        }

        n1 <- input$n1_total
        n2 <- input$n2_total
        x1 <- input$n1_success
        x2 <- input$n2_success

        additional_info$group_names <- c("Kelompok 1", "Kelompok 2")

        hasil <- prop.test(x = c(x1, x2), n = c(n1, n2),
                           alternative = alt, correct = FALSE, conf.level = 1 - input$alpha)

        additional_info$sample_props <- c(x1/n1, x2/n2)
        additional_info$n_success <- c(x1, x2)
        additional_info$n_total <- c(n1, n2)
        additional_info$n_failure <- c(n1 - x1, n2 - x2)

      } else if (input$uji_statistik == "Uji Ragam 1 Populasi") {
        req(input$kolomNumerik, input$var0)
        x <- df[[input$kolomNumerik]]
        x <- x[!is.na(x)]

        n <- length(x)
        s2 <- var(x)
        chi_val <- (n - 1) * s2 / input$var0
        df_chi <- n - 1
        if (alt == "two.sided") {
          p_val <- 2 * min(pchisq(chi_val, df = df_chi), 1 - pchisq(chi_val, df = df_chi))
        } else if (alt == "greater") {
          p_val <- 1 - pchisq(chi_val, df = df_chi)
        } else { # "less"
          p_val <- pchisq(chi_val, df = df_chi)
        }

        hasil <- list(
          statistic = c(`X-squared` = chi_val),
          p.value = p_val,
          parameter = c(df = df_chi),
          method = "Chi-square Test for Variance",
          data.name = input$kolomNumerik,
          alternative = alt
        )

        additional_info$sample_var <- s2
        additional_info$sample_sd <- sqrt(s2)
        additional_info$sample_size <- n
        additional_info$hypothesized_var <- input$var0

      } else if (input$uji_statistik == "Uji Ragam 2 Populasi") {
        req(input$kolomNumerik, input$kolomNumerik2)
        x1 <- df[[input$kolomNumerik]]
        x2 <- df[[input$kolomNumerik2]]
        x1 <- x1[!is.na(x1)]
        x2 <- x2[!is.na(x2)]

        n1 <- length(x1)
        n2 <- length(x2)
        s1_squared <- var(x1)
        s2_squared <- var(x2)
        f_val <- max(s1_squared, s2_squared) / min(s1_squared, s2_squared)
        df1 <- ifelse(s1_squared >= s2_squared, n1 - 1, n2 - 1)
        df2 <- ifelse(s1_squared >= s2_squared, n2 - 1, n1 - 1)
        if (alt == "two.sided") {
          p_val <- 2 * (1 - pf(f_val, df1, df2))
        } else if (alt == "greater") {
          if (s1_squared > s2_squared) {
            f_val <- s1_squared / s2_squared
            df1 <- n1 - 1
            df2 <- n2 - 1
            p_val <- 1 - pf(f_val, df1, df2)
          } else {
            f_val <- s2_squared / s1_squared
            df1 <- n2 - 1
            df2 <- n1 - 1
            p_val <- pf(f_val, df1, df2)
          }
        } else { # "less"
          if (s1_squared < s2_squared) {
            f_val <- s2_squared / s1_squared
            df1 <- n2 - 1
            df2 <- n1 - 1
            p_val <- 1 - pf(f_val, df1, df2)
          } else {
            f_val <- s1_squared / s2_squared
            df1 <- n1 - 1
            df2 <- n2 - 1
            p_val <- pf(f_val, df1, df2)
          }
        }

        hasil <- list(
          statistic = c(F = f_val),
          p.value = p_val,
          parameter = c(`num df` = df1, `denom df` = df2),
          method = "F Test for Equality of Variances",
          data.name = paste(input$kolomNumerik, "and", input$kolomNumerik2),
          alternative = alt
        )

        additional_info$sample_vars <- c(s1_squared, s2_squared)
        additional_info$sample_sds <- c(sqrt(s1_squared), sqrt(s2_squared))
        additional_info$sample_sizes <- c(n1, n2)
        additional_info$sample_names <- c(input$kolomNumerik, input$kolomNumerik2)
        additional_info$f_ratio <- f_val

      } else if (input$uji_statistik == "Uji ANOVA 1 Arah") {
        req(input$kolomNumerik, input$kolomKategori)
        y <- df[[input$kolomNumerik]]
        if(is.factor(df[[input$kolomKategori]]) || is.character(df[[input$kolomKategori]])) {
          unique_vals <- unique(df[[input$kolomKategori]][!is.na(df[[input$kolomKategori]])])
          if(length(unique_vals) >= 2 && length(unique_vals) <= 20) {
            x <- as.factor(df[[input$kolomKategori]])
          } else {
            stop("Kolom tidak memenuhi kriteria: harus memiliki 2-20 nilai unik")
          }
        } else {
          stop("Kolom harus berupa factor atau character")
        }
        complete_cases <- complete.cases(y, x)
        y <- y[complete_cases]
        x <- x[complete_cases]
        group_counts <- table(x)
        if(length(group_counts) < 2) {
          showNotification("Minimal harus ada 2 kelompok untuk ANOVA!", type = "error")
          return()
        }
        anova_result <- aov(y ~ x)
        hasil <- summary(anova_result)
        f_stat <- hasil[[1]]["x", "F value"]
        p_value <- hasil[[1]]["x", "Pr(>F)"]
        df_between <- hasil[[1]]["x", "Df"]
        df_within <- hasil[[1]]["Residuals", "Df"]
        hasil <- list(
          statistic = c(F = f_stat),
          p.value = p_value,
          parameter = c(`num df` = df_between, `denom df` = df_within),
          method = "One-way Analysis of Variance (ANOVA)",
          data.name = paste(input$kolomNumerik, "by", input$kolomKategori),
          alternative = "at least one group mean is different"
        )
        group_stats <- aggregate(y, by = list(Group = x), FUN = function(z) {
          c(mean = mean(z), sd = sd(z), n = length(z))
        })

        additional_info$group_means <- group_stats$x[,"mean"]
        additional_info$group_sds <- group_stats$x[,"sd"]
        additional_info$group_sizes <- group_stats$x[,"n"]
        additional_info$group_names <- as.character(group_stats$Group)
        additional_info$total_n <- length(y)
        additional_info$num_groups <- length(unique(x))
        additional_info$anova_table <- anova_result

      } else if (input$uji_statistik == "Uji ANOVA 2 Arah") {
        req(input$kolomNumerik, input$kolomKategori, input$kolomKategori2)
        y <- df[[input$kolomNumerik]]
        if(is.factor(df[[input$kolomKategori]]) || is.character(df[[input$kolomKategori]])) {
          unique_vals <- unique(df[[input$kolomKategori]][!is.na(df[[input$kolomKategori]])])
          if(length(unique_vals) >= 2 && length(unique_vals) <= 20) {
            factor_a <- as.factor(df[[input$kolomKategori]])
          } else {
            stop("Kolom tidak memenuhi kriteria: harus memiliki 2-20 nilai unik")
          }
        } else {
          stop("Kolom harus berupa factor atau character")
        }

        if(is.factor(df[[input$kolomKategori2]]) || is.character(df[[input$kolomKategori2]])) {
          unique_vals <- unique(df[[input$kolomKategori2]][!is.na(df[[input$kolomKategori2]])])
          if(length(unique_vals) >= 2 && length(unique_vals) <= 20) {
            factor_b <- as.factor(df[[input$kolomKategori2]])
          } else {
            stop("Kolom tidak memenuhi kriteria: harus memiliki 2-20 nilai unik")
          }
        } else {
          stop("Kolom harus berupa factor atau character")
        }
        complete_cases <- complete.cases(y, factor_a, factor_b)
        y <- y[complete_cases]
        factor_a <- factor_a[complete_cases]
        factor_b <- factor_b[complete_cases]
        combination_table <- table(factor_a, factor_b)
        if(any(combination_table == 0)) {
          showNotification("Warning: Ada kombinasi faktor yang tidak memiliki data!", type = "warning")
        }
        if(input$include_interaction) {
          anova_result <- aov(y ~ factor_a * factor_b)
          formula_text <- paste(input$kolomNumerik, "~", input$kolomKategori, "*", input$kolomKategori2)
        } else {
          anova_result <- aov(y ~ factor_a + factor_b)
          formula_text <- paste(input$kolomNumerik, "~", input$kolomKategori, "+", input$kolomKategori2)
        }

        hasil_summary <- summary(anova_result)
        anova_table <- hasil_summary[[1]]

        hasil <- list(
          method = "Two-way Analysis of Variance (ANOVA)",
          data.name = formula_text,
          anova_summary = hasil_summary,
          alternative = "at least one effect is significant"
        )
        group_stats <- aggregate(y, by = list(FactorA = factor_a, FactorB = factor_b),
                                 FUN = function(z) {
                                   if(length(z) > 0) {
                                     c(mean = mean(z), sd = sd(z), n = length(z))
                                   } else {
                                     c(mean = NA, sd = NA, n = 0)
                                   }
                                 })

        additional_info$combination_stats <- group_stats
        additional_info$factor_a_name <- input$kolomKategori
        additional_info$factor_b_name <- input$kolomKategori2
        additional_info$total_n <- length(y)
        additional_info$include_interaction <- input$include_interaction
        additional_info$anova_table <- anova_result
        marginal_a <- aggregate(y, by = list(FactorA = factor_a), FUN = mean)
        marginal_b <- aggregate(y, by = list(FactorB = factor_b), FUN = mean)

        additional_info$marginal_means_a <- setNames(marginal_a$x, marginal_a$FactorA)
        additional_info$marginal_means_b <- setNames(marginal_b$x, marginal_b$FactorB)
      }
      values$test_result <- hasil
      values$additional_info <- additional_info
      values$current_test <- input$uji_statistik
      values$alpha_level <- input$alpha

      output$hasilUji <- renderPrint({
        print(hasil)
        if(values$current_test %in% c("Uji ANOVA 1 Arah", "Uji ANOVA 2 Arah")) {
          cat("\n=== TABEL ANOVA ===\n")
          if(!is.null(additional_info$anova_table)) {
            print(summary(additional_info$anova_table))
          }
        }

        if(length(additional_info) > 0) {
          cat("\n=== STATISTIK DESKRIPTIF ===\n")
          for(name in names(additional_info)) {
            if(is.numeric(additional_info[[name]])) {
              cat(paste0(name, ": ", paste(round(additional_info[[name]], 4), collapse = ", "), "\n"))
            } else {
              cat(paste0(name, ": ", paste(additional_info[[name]], collapse = ", "), "\n"))
            }
          }
        }
      })

      output$interpretasiUji <- renderText({
        if (is.null(hasil$p.value) && is.null(hasil$anova_summary)) return("")

        alpha_val <- input$alpha
        test_type <- input$uji_statistik
        if (test_type == "Uji ANOVA 2 Arah") {
          if(!is.null(values$test_result$anova_summary)) {
            anova_summary <- values$test_result$anova_summary[[1]]

            interpretations <- c()
            decisions <- c()
            factor_a_name <- values$additional_info$factor_a_name
            factor_b_name <- values$additional_info$factor_b_name
            cat("Rownames ANOVA table:", rownames(anova_summary), "\n")
            if(factor_a_name %in% rownames(anova_summary)) {
              p_factor_a <- anova_summary[factor_a_name, "Pr(>F)"]
              f_factor_a <- anova_summary[factor_a_name, "F value"]

              if(!is.na(p_factor_a)) {
                decisions <- c(decisions, paste("Faktor", factor_a_name,
                                                ": F =", round(f_factor_a, 3),
                                                ", p =", round(p_factor_a, 4)))
                if(p_factor_a < alpha_val) {
                  interpretations <- c(interpretations, paste("✓ Faktor", factor_a_name, "memiliki efek signifikan (p < α)"))
                } else {
                  interpretations <- c(interpretations, paste("✗ Faktor", factor_a_name, "tidak memiliki efek signifikan (p ≥ α)"))
                }
              }
            }
            if(factor_b_name %in% rownames(anova_summary)) {
              p_factor_b <- anova_summary[factor_b_name, "Pr(>F)"]
              f_factor_b <- anova_summary[factor_b_name, "F value"]

              if(!is.na(p_factor_b)) {
                decisions <- c(decisions, paste("Faktor", factor_b_name,
                                                ": F =", round(f_factor_b, 3),
                                                ", p =", round(p_factor_b, 4)))
                if(p_factor_b < alpha_val) {
                  interpretations <- c(interpretations, paste("✓ Faktor", factor_b_name, "memiliki efek signifikan (p < α)"))
                } else {
                  interpretations <- c(interpretations, paste("✗ Faktor", factor_b_name, "tidak memiliki efek signifikan (p ≥ α)"))
                }
              }
            }
            if(values$additional_info$include_interaction) {
              interaction_rows <- rownames(anova_summary)[grep(":", rownames(anova_summary))]

              if(length(interaction_rows) > 0) {
                interaction_row <- interaction_rows[1]  # ambil yang pertama
                p_interaction <- anova_summary[interaction_row, "Pr(>F)"]
                f_interaction <- anova_summary[interaction_row, "F value"]

                if(!is.na(p_interaction)) {
                  decisions <- c(decisions, paste("Interaksi", factor_a_name, "×", factor_b_name,
                                                  ": F =", round(f_interaction, 3),
                                                  ", p =", round(p_interaction, 4)))
                  if(p_interaction < alpha_val) {
                    interpretations <- c(interpretations, paste("✓ Terdapat efek interaksi signifikan antara", factor_a_name, "dan", factor_b_name, "(p < α)"))
                  } else {
                    interpretations <- c(interpretations, paste("✗ Tidak terdapat efek interaksi signifikan antara", factor_a_name, "dan", factor_b_name, "(p ≥ α)"))
                  }
                }
              }
            }

            decision <- paste("HASIL UJI (α =", alpha_val, "):\n", paste(decisions, collapse = "\n"))
            conclusion <- paste("INTERPRETASI:\n", paste(interpretations, collapse = "\n"))

            return(paste(decision, conclusion, sep = "\n\n"))
          } else {
            return("Error: Hasil ANOVA tidak tersedia untuk interpretasi")
          }
        }
        p_val <- round(hasil$p.value, 6)
        decision <- if (p_val < alpha_val) {
          paste0("Keputusan: H₀ DITOLAK (p-value = ", p_val, " < α = ", alpha_val, ")")
        } else {
          paste0("Keputusan: H₀ GAGAL DITOLAK (p-value = ", p_val, " ≥ α = ", alpha_val, ")")
        }
        if (test_type == "Uji Ragam 1 Populasi") {
          if (p_val < alpha_val) {
            conclusion <- switch(input$alternative,
                                 "two.sided" = "Terdapat bukti statistik yang signifikan bahwa variansi populasi tidak sama dengan nilai yang dihipotesiskan.",
                                 "greater" = "Terdapat bukti statistik yang signifikan bahwa variansi populasi lebih besar dari nilai yang dihipotesiskan.",
                                 "less" = "Terdapat bukti statistik yang signifikan bahwa variansi populasi lebih kecil dari nilai yang dihipotesiskan."
            )
          } else {
            conclusion <- switch(input$alternative,
                                 "two.sided" = "Tidak terdapat bukti statistik yang cukup untuk menyatakan bahwa variansi populasi berbeda dari nilai yang dihipotesiskan.",
                                 "greater" = "Tidak terdapat bukti statistik yang cukup untuk menyatakan bahwa variansi populasi lebih besar dari nilai yang dihipotesiskan.",
                                 "less" = "Tidak terdapat bukti statistik yang cukup untuk menyatakan bahwa variansi populasi lebih kecil dari nilai yang dihipotesiskan."
            )
          }
        } else if (test_type == "Uji Ragam 2 Populasi") {
          if (p_val < alpha_val) {
            conclusion <- switch(input$alternative,
                                 "two.sided" = "Terdapat bukti statistik yang signifikan bahwa variansi kedua populasi tidak sama (berbeda).",
                                 "greater" = "Terdapat bukti statistik yang signifikan bahwa variansi populasi pertama lebih besar dari variansi populasi kedua.",
                                 "less" = "Terdapat bukti statistik yang signifikan bahwa variansi populasi pertama lebih kecil dari variansi populasi kedua."
            )
          } else {
            conclusion <- switch(input$alternative,
                                 "two.sided" = "Tidak terdapat bukti statistik yang cukup untuk menyatakan bahwa variansi kedua populasi berbeda.",
                                 "greater" = "Tidak terdapat bukti statistik yang cukup untuk menyatakan bahwa variansi populasi pertama lebih besar dari variansi populasi kedua.",
                                 "less" = "Tidak terdapat bukti statistik yang cukup untuk menyatakan bahwa variansi populasi pertama lebih kecil dari variansi populasi kedua."
            )
          }
        } else if (test_type == "Uji ANOVA 1 Arah") {
          if (p_val < alpha_val) {
            conclusion <- "Terdapat bukti statistik yang signifikan bahwa rata-rata minimal satu kelompok berbeda dari yang lain. Perlu dilakukan uji post-hoc untuk mengetahui kelompok mana yang berbeda."
          } else {
            conclusion <- "Tidak terdapat bukti statistik yang cukup untuk menyatakan bahwa ada perbedaan rata-rata antar kelompok."
          }
        } else {
          conclusion <- if (p_val < alpha_val) {
            "Terdapat bukti statistik yang signifikan untuk mendukung hipotesis alternatif."
          } else {
            "Tidak terdapat bukti statistik yang cukup untuk mendukung hipotesis alternatif."
          }
        }

        return(paste(decision, "\nKesimpulan:", conclusion))
      })

    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      output$hasilUji <- renderText(paste("Error:", e$message))
    })
  })
  output$var_y_mlr <- renderUI({
    req(values$processedData)
    numeric_vars <- names(select_if(values$processedData, is.numeric))
    selectInput("var_y_mlr", "Variabel Dependen (Y):",
                choices = numeric_vars,
                selected = NULL)
  })

  output$var_x_mlr <- renderUI({
    req(values$processedData)
    numeric_vars <- names(select_if(values$processedData, is.numeric))
    checkboxGroupInput("var_x_mlr", "Variabel Independen (X):",
                       choices = numeric_vars,
                       selected = NULL)
  })
  mlr_results <- reactiveValues(
    model = NULL,
    run_analysis = FALSE
  )
  observeEvent(input$btn_run_mlr, {
    req(input$var_y_mlr, input$var_x_mlr, values$processedData)

    tryCatch({
      formula_str <- paste(input$var_y_mlr, "~", paste(input$var_x_mlr, collapse = " + "))
      formula_obj <- as.formula(formula_str)
      model <- lm(formula_obj, data = values$processedData)
      mlr_results$model <- model
      mlr_results$run_analysis <- TRUE

      showNotification("Analisis regresi berhasil dijalankan!", type = "message")

    }, error = function(e) {
      showNotification(paste("Error dalam analisis:", e$message), type = "error")
      mlr_results$run_analysis <- FALSE
    })
  })
  mlr_model <- reactive({
    req(mlr_results$run_analysis, mlr_results$model)
    return(mlr_results$model)
  })
  output$mlr_summary <- renderPrint({
    if(!mlr_results$run_analysis) {
      cat("=== REGRESI LINEAR BERGANDA ===\n\n")
      cat("Pilih variabel dependen dan independen, kemudian klik tombol 'Jalankan Regresi' untuk memulai analisis.\n\n")
      cat("LANGKAH-LANGKAH:\n")
      cat("1. Pilih 1 variabel dependen (Y)\n")
      cat("2. Pilih minimal 1 variabel independen (X)\n")
      cat("3. Klik tombol 'Jalankan Regresi'\n")
      cat("4. Lihat hasil di tab yang tersedia\n")
      return()
    }

    req(mlr_model())
    model <- mlr_model()

    cat("=== RINGKASAN MODEL REGRESI LINEAR BERGANDA ===\n\n")
    cat("Formula:", deparse(formula(model)), "\n\n")
    summary_model <- summary(model)
    print(summary_model)

    cat("\n=== INTERPRETASI ===\n")
    cat("R-squared:", round(summary_model$r.squared, 4),
        "- Model menjelaskan", round(summary_model$r.squared * 100, 2),
        "% variasi dalam variabel dependen\n")
    cat("Adjusted R-squared:", round(summary_model$adj.r.squared, 4), "\n")
    cat("F-statistic:", round(summary_model$fstatistic[1], 4),
        "dengan p-value:", format.pval(pf(summary_model$fstatistic[1],
                                          summary_model$fstatistic[2],
                                          summary_model$fstatistic[3],
                                          lower.tail = FALSE), eps = 0.001), "\n\n")
    cat("INTERPRETASI KOEFISIEN:\n")
    coef_table <- summary_model$coefficients
    for(i in 1:nrow(coef_table)) {
      var_name <- rownames(coef_table)[i]
      coef_val <- coef_table[i, 1]
      p_val <- coef_table[i, 4]

      if(var_name == "(Intercept)") {
        cat("- Intercept:", round(coef_val, 4),
            ifelse(p_val < 0.05, "(signifikan)", "(tidak signifikan)"), "\n")
      } else {
        cat("- ", var_name, ": Setiap peningkatan 1 unit akan",
            ifelse(coef_val > 0, "meningkatkan", "menurunkan"),
            "Y sebesar", abs(round(coef_val, 4)),
            ifelse(p_val < 0.05, "(signifikan)", "(tidak signifikan)"), "\n")
      }
    }
  })
  output$uji_normalitas <- renderPrint({
    if(!mlr_results$run_analysis) {
      cat("Jalankan analisis regresi terlebih dahulu untuk melihat uji normalitas residual.\n")
      return()
    }

    req(mlr_model())
    model <- mlr_model()
    residuals <- residuals(model)

    cat("=== UJI NORMALITAS RESIDUAL (SHAPIRO-WILK) ===\n\n")

    shapiro_test <- shapiro.test(residuals)
    print(shapiro_test)

    cat("\nINTERPRETASI:\n")
    if(shapiro_test$p.value > 0.05) {
      cat("P-value (", round(shapiro_test$p.value, 4), ") > 0.05\n")
      cat("KESIMPULAN: Residual berdistribusi normal (asumsi normalitas TERPENUHI)\n")
    } else {
      cat("P-value (", round(shapiro_test$p.value, 4), ") ≤ 0.05\n")
      cat("KESIMPULAN: Residual TIDAK berdistribusi normal (asumsi normalitas TIDAK TERPENUHI)\n")
      cat("SARAN: Pertimbangkan transformasi data atau gunakan metode non-parametrik\n")
    }
  })
  output$uji_multikolinearitas <- renderPrint({
    if(!mlr_results$run_analysis) {
      cat("Jalankan analisis regresi terlebih dahulu untuk melihat uji multikolinearitas.\n")
      return()
    }

    req(mlr_model(), length(input$var_x_mlr) > 1)

    cat("=== UJI MULTIKOLINEARITAS (VIF - Variance Inflation Factor) ===\n\n")

    tryCatch({
      library(car)
      vif_values <- vif(mlr_model())

      cat("Nilai VIF untuk setiap variabel:\n")
      print(round(vif_values, 4))

      cat("\nINTERPRETASI:\n")
      cat("VIF < 5: Tidak ada multikolinearitas\n")
      cat("5 ≤ VIF < 10: Multikolinearitas sedang\n")
      cat("VIF ≥ 10: Multikolinearitas tinggi (bermasalah)\n\n")

      for(i in 1:length(vif_values)) {
        var_name <- names(vif_values)[i]
        vif_val <- vif_values[i]

        if(vif_val < 5) {
          status <- "BAIK (tidak ada multikolinearitas)"
        } else if(vif_val < 10) {
          status <- "PERHATIAN (multikolinearitas sedang)"
        } else {
          status <- "BERMASALAH (multikolinearitas tinggi)"
        }

        cat("- ", var_name, ": VIF =", round(vif_val, 4), "-", status, "\n")
      }

      if(any(vif_values >= 10)) {
        cat("\nSARAN: Pertimbangkan untuk menghapus variabel dengan VIF tinggi atau gunakan teknik regularisasi\n")
      }

    }, error = function(e) {
      cat("Error dalam menghitung VIF. Pastikan package 'car' terinstall.\n")
      cat("Install dengan: install.packages('car')\n")
    })
  })
  output$uji_homoskedastisitas <- renderPrint({
    if(!mlr_results$run_analysis) {
      cat("Jalankan analisis regresi terlebih dahulu untuk melihat uji homoskedastisitas.\n")
      return()
    }

    req(mlr_model())

    cat("=== UJI HOMOSKEDASTISITAS (BREUSCH-PAGAN) ===\n\n")

    tryCatch({
      library(lmtest)
      bp_test <- bptest(mlr_model())
      print(bp_test)

      cat("\nINTERPRETASI:\n")
      if(bp_test$p.value > 0.05) {
        cat("P-value (", round(bp_test$p.value, 4), ") > 0.05\n")
        cat("KESIMPULAN: Varians residual homogen (asumsi homoskedastisitas TERPENUHI)\n")
      } else {
        cat("P-value (", round(bp_test$p.value, 4), ") ≤ 0.05\n")
        cat("KESIMPULAN: Terjadi heteroskedastisitas (asumsi homoskedastisitas TIDAK TERPENUHI)\n")
        cat("SARAN: Gunakan robust standard errors atau transformasi data\n")
      }

    }, error = function(e) {
      cat("Error dalam uji Breusch-Pagan. Pastikan package 'lmtest' terinstall.\n")
      cat("Install dengan: install.packages('lmtest')\n")
    })
  })
  output$uji_autokorelasi <- renderPrint({
    if(!mlr_results$run_analysis) {
      cat("Jalankan analisis regresi terlebih dahulu untuk melihat uji autokorelasi.\n")
      return()
    }

    req(mlr_model())

    cat("=== UJI AUTOKORELASI (DURBIN-WATSON) ===\n\n")

    tryCatch({
      library(lmtest)
      dw_test <- dwtest(mlr_model())
      print(dw_test)

      cat("\nINTERPRETASI:\n")
      dw_stat <- dw_test$statistic
      cat("Nilai Durbin-Watson:", round(dw_stat, 4), "\n")

      if(dw_stat >= 1.5 && dw_stat <= 2.5) {
        cat("KESIMPULAN: Tidak ada autokorelasi (asumsi independensi TERPENUHI)\n")
      } else if(dw_stat < 1.5) {
        cat("KESIMPULAN: Terdapat autokorelasi positif (asumsi independensi TIDAK TERPENUHI)\n")
        cat("SARAN: Pertimbangkan menambah lag variables atau gunakan model time series\n")
      } else {
        cat("KESIMPULAN: Terdapat autokorelasi negatif (asumsi independensi TIDAK TERPENUHI)\n")
        cat("SARAN: Periksa model spesifikasi atau gunakan metode koreksi\n")
      }

      if(dw_test$p.value <= 0.05) {
        cat("P-value (", round(dw_test$p.value, 4), ") ≤ 0.05: Autokorelasi signifikan secara statistik\n")
      }

    }, error = function(e) {
      cat("Error dalam uji Durbin-Watson. Pastikan package 'lmtest' terinstall.\n")
      cat("Install dengan: install.packages('lmtest')\n")
    })
  })
  output$plot_diagnostik <- renderPlot({
    if(!mlr_results$run_analysis) {
      plot.new()
      text(0.5, 0.5, "Jalankan analisis regresi\nterlebih dahulu",
           cex = 1.5, col = "gray60", family = "sans")
      return()
    }

    req(mlr_model())

    par(mfrow = c(2, 2))
    plot(mlr_model())
    par(mfrow = c(1, 1))
  })
  output$plot_residual_fitted <- renderPlot({
    if(!mlr_results$run_analysis) {
      plot.new()
      text(0.5, 0.5, "Jalankan analisis\nregresi terlebih dahulu",
           cex = 1.2, col = "gray60")
      return()
    }

    req(mlr_model())
    model <- mlr_model()

    plot(fitted(model), residuals(model),
         xlab = "Fitted Values", ylab = "Residuals",
         main = "Residuals vs Fitted Values",
         pch = 16, col = "steelblue")
    abline(h = 0, col = "red", lty = 2)
    mtext("Pola acak mengindikasikan homoskedastisitas",
          side = 3, line = 0.5, cex = 0.8, col = "gray50")
  })
  output$plot_qq <- renderPlot({
    if(!mlr_results$run_analysis) {
      plot.new()
      text(0.5, 0.5, "Jalankan analisis\nregresi terlebih dahulu",
           cex = 1.2, col = "gray60")
      return()
    }

    req(mlr_model())
    model <- mlr_model()

    qqnorm(residuals(model), main = "Q-Q Plot Residuals",
           pch = 16, col = "steelblue")
    qqline(residuals(model), col = "red", lty = 2)
    mtext("Titik yang mengikuti garis menunjukkan normalitas",
          side = 3, line = 0.5, cex = 0.8, col = "gray50")
  })
  output$plot_histogram_residual <- renderPlot({
    if(!mlr_results$run_analysis) {
      plot.new()
      text(0.5, 0.5, "Jalankan analisis\nregresi terlebih dahulu",
           cex = 1.2, col = "gray60")
      return()
    }

    req(mlr_model())
    model <- mlr_model()

    hist(residuals(model),
         main = "Histogram Residuals",
         xlab = "Residuals",
         col = "lightblue",
         border = "white",
         breaks = 20)
    x <- seq(min(residuals(model)), max(residuals(model)), length = 100)
    y <- dnorm(x, mean = mean(residuals(model)), sd = sd(residuals(model)))
    y <- y * length(residuals(model)) * diff(range(residuals(model))) / 20
    lines(x, y, col = "red", lwd = 2)
    mtext("Distribusi mendekati normal menunjukkan asumsi normalitas terpenuhi",
          side = 3, line = 0.5, cex = 0.8, col = "gray50")
  })
  output$prediksi_ui <- renderUI({
    if(!mlr_results$run_analysis) {
      div(
        style = "text-align: center; padding: 20px; color: gray;",
        h4("Fitur Prediksi"),
        p("Jalankan analisis regresi terlebih dahulu untuk menggunakan fitur prediksi.")
      )
    } else {
      req(input$var_x_mlr, mlr_model())

      inputs <- lapply(input$var_x_mlr, function(var) {
        numericInput(paste0("pred_", var),
                     label = paste("Nilai", var, ":"),
                     value = mean(values$processedData[[var]], na.rm = TRUE),
                     step = 0.1)
      })

      tagList(
        h4("Prediksi Nilai Y"),
        inputs,
        br(),
        actionButton("btn_prediksi", "Hitung Prediksi", class = "btn-primary")
      )
    }
  })
  observeEvent(input$btn_prediksi, {
    req(input$var_x_mlr, mlr_model())
    pred_data <- data.frame(row.names = 1)
    for(var in input$var_x_mlr) {
      pred_data[[var]] <- input[[paste0("pred_", var)]]
    }
    prediction <- predict(mlr_model(), pred_data, interval = "prediction", level = 0.95)

    output$hasil_prediksi <- renderText({
      paste0("Prediksi nilai ", input$var_y_mlr, ": ", round(prediction[1], 4), "\n",
             "Interval prediksi 95%: [", round(prediction[2], 4), ", ",
             round(prediction[3], 4), "]")
    })
  })
  capture_output <- function(expr) {
    output_text <- capture.output(expr, type = "output")
    paste(output_text, collapse = "\n")
  }
  create_professional_docx <- function(title, content, filename, alpha = 0.05) {
    doc <- read_docx() %>%
      body_add_par("LAPORAN HASIL ANALISIS STATISTIK", style = "heading 1") %>%
      body_add_par("", style = "Normal") %>%
      body_add_par(paste("Tanggal:", format(Sys.Date(), "%d %B %Y")), style = "Normal") %>%
      body_add_par(paste("Tingkat Signifikansi (α):", alpha), style = "Normal") %>%
      body_add_par("", style = "Normal") %>%
      body_add_par(title, style = "heading 2") %>%
      body_add_par("", style = "Normal") %>%
      body_add_par(content, style = "Normal") %>%
      body_add_par("", style = "Normal") %>%
      body_add_par("---", style = "Normal") %>%
      body_add_par("Catatan: Hasil analisis ini dihasilkan menggunakan software R. Interpretasi hasil harus dilakukan dengan mempertimbangkan konteks penelitian dan asumsi-asumsi yang mendasari uji statistik yang digunakan.",
                   style = "Normal")

    temp_file <- tempfile(fileext = ".docx")
    print(doc, target = temp_file)
    return(temp_file)
  }
  interpret_p_value <- function(p_value, alpha = 0.05, test_name = "uji statistik") {
    alpha_percent <- alpha * 100
    if (is.na(p_value)) {
      return("P-value tidak dapat dihitung.")
    }

    if (p_value < alpha) {
      return(paste0("Dengan tingkat kepercayaan ", (100-alpha_percent), "% (α = ", alpha, "), p-value = ",
                    format(p_value, scientific = TRUE, digits = 4), " < α = ", alpha,
                    ", sehingga dapat disimpulkan bahwa hasil ", test_name, " adalah SIGNIFIKAN. ",
                    "Hal ini menunjukkan terdapat cukup bukti untuk menolak hipotesis nol (H₀)."))
    } else {
      return(paste0("Dengan tingkat kepercayaan ", (100-alpha_percent), "% (α = ", alpha, "), p-value = ",
                    format(p_value, scientific = TRUE, digits = 4), " ≥ α = ", alpha,
                    ", sehingga dapat disimpulkan bahwa hasil ", test_name, " adalah TIDAK SIGNIFIKAN. ",
                    "Hal ini menunjukkan tidak terdapat cukup bukti untuk menolak hipotesis nol (H₀)."))
    }
  }
  output$downloadNormality <- downloadHandler(
    filename = function() {
      paste("Laporan_Uji_Normalitas_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      req(input$normVar, input$normTestType)
      alpha <- if(exists("input$alpha")) input$alpha else 0.05

      content <- capture_output({
        data <- values$processedData[[input$normVar]]
        data <- data[!is.na(data) & is.finite(data)]

        if(length(data) < 3) {
          cat("ERROR: Data tidak mencukupi untuk analisis normalitas (n < 3)\n")
          cat("SARAN: Tambahkan lebih banyak observasi untuk melakukan uji normalitas yang valid.\n")
          return()
        }

        if(length(unique(data)) == 1) {
          cat("ERROR: Data bersifat konstan (semua nilai sama)\n")
          cat("SARAN: Periksa kembali data input, karena uji normalitas tidak dapat dilakukan pada data konstan.\n")
          return()
        }
        cat("════════════════════════════════════════════════════════════════════════\n")
        cat("                            UJI NORMALITAS DATA                          \n")
        cat("════════════════════════════════════════════════════════════════════════\n\n")

        cat("INFORMASI ANALISIS:\n")
        cat("• Variabel yang diuji     : ", input$normVar, "\n")
        cat("• Jenis uji normalitas    : ", switch(input$normTestType,
                                                   "shapiro" = "Shapiro-Wilk Test",
                                                   "ks" = "Kolmogorov-Smirnov Test",
                                                   "ad" = "Anderson-Darling Test",
                                                   "jb" = "Jarque-Bera Test"), "\n")
        cat("• Jumlah observasi        : ", length(data), "\n")
        cat("• Tingkat signifikansi    : α = ", alpha, "\n\n")

        cat("STATISTIK DESKRIPTIF:\n")
        cat("• Mean (rata-rata)        : ", round(mean(data), 4), "\n")
        cat("• Standar Deviasi         : ", round(sd(data), 4), "\n")
        cat("• Minimum                 : ", round(min(data), 4), "\n")
        cat("• Maksimum                : ", round(max(data), 4), "\n")
        cat("• Skewness (kemencengan)  : ", round(moments::skewness(data), 4), "\n")
        cat("• Kurtosis (keruncingan)  : ", round(moments::kurtosis(data), 4), "\n\n")

        cat("HIPOTESIS:\n")
        cat("• H₀ (Hipotesis Nol)      : Data berdistribusi normal\n")
        cat("• H₁ (Hipotesis Alternatif): Data tidak berdistribusi normal\n\n")

        cat("HASIL UJI STATISTIK:\n")
        cat("────────────────────────────────────────────────────────────────────────\n")
        result <- NULL
        test_name <- ""

        if(input$normTestType == "shapiro") {
          test_name <- "Shapiro-Wilk"
          if(length(data) > 5000) {
            set.seed(123)
            data_sample <- sample(data, 5000)
            cat("CATATAN: Data lebih dari 5000 observasi, menggunakan sample acak 5000 observasi\n\n")
            result <- shapiro.test(data_sample)
          } else {
            result <- shapiro.test(data)
          }
        } else if(input$normTestType == "ks") {
          test_name <- "Kolmogorov-Smirnov"
          result <- ks.test(scale(data), "pnorm")
        } else if(input$normTestType == "ad") {
          test_name <- "Anderson-Darling"
          result <- nortest::ad.test(data)
        } else if(input$normTestType == "jb") {
          test_name <- "Jarque-Bera"
          result <- tseries::jarque.bera.test(data)
        }

        print(result)
        cat("\n")

        cat("INTERPRETASI HASIL:\n")
        cat("────────────────────────────────────────────────────────────────────────\n")
        interpretation <- interpret_p_value(result$p.value, alpha, paste("uji normalitas", test_name))
        cat(interpretation, "\n\n")

        if(result$p.value < alpha) {
          cat("KESIMPULAN:\n")
          cat("Berdasarkan hasil uji normalitas ", test_name, " dengan tingkat kepercayaan ",
              (100-alpha*100), "%, dapat disimpulkan bahwa variabel '", input$normVar,
              "' TIDAK berdistribusi normal.\n\n")

          cat("IMPLIKASI DAN SARAN:\n")
          cat("• Data tidak memenuhi asumsi normalitas untuk uji parametrik\n")
          cat("• Pertimbangkan transformasi data (log, akar kuadrat, Box-Cox)\n")
          cat("• Gunakan uji non-parametrik sebagai alternatif\n")
          cat("• Periksa adanya outlier yang mungkin mempengaruhi distribusi\n")
        } else {
          cat("KESIMPULAN:\n")
          cat("Berdasarkan hasil uji normalitas ", test_name, " dengan tingkat kepercayaan ",
              (100-alpha*100), "%, dapat disimpulkan bahwa variabel '", input$normVar,
              "' berdistribusi normal.\n\n")

          cat("IMPLIKASI:\n")
          cat("• Data memenuhi asumsi normalitas untuk uji parametrik\n")
          cat("• Dapat menggunakan uji-t, ANOVA, regresi linear, dan uji parametrik lainnya\n")
          cat("• Distribusi data mendekati kurva normal standar\n")
        }
      })

      temp_file <- create_professional_docx("UJI NORMALITAS", content, file, alpha)
      file.copy(temp_file, file, overwrite = TRUE)
    }
  )
  output$downloadHomogeneity <- downloadHandler(
    filename = function() {
      paste("Laporan_Uji_Homogenitas_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      req(input$homogenVar, input$homogenGroup, input$homogenTestType)
      alpha <- if(exists("input$alpha")) input$alpha else 0.05

      content <- capture_output({
        df <- values$processedData
        df <- df[complete.cases(df[[input$homogenVar]], df[[input$homogenGroup]]), ]
        df <- df[is.finite(df[[input$homogenVar]]), ]

        cat("════════════════════════════════════════════════════════════════════════\n")
        cat("                     UJI HOMOGENITAS VARIAN (HOMOSKEDASTISITAS)         \n")
        cat("════════════════════════════════════════════════════════════════════════\n\n")

        if(nrow(df) < 2) {
          cat("ERROR: Data tidak mencukupi untuk uji homogenitas\n")
          return()
        }

        df[[input$homogenGroup]] <- as.factor(df[[input$homogenGroup]])
        groups <- levels(df[[input$homogenGroup]])

        if(length(groups) < 2) {
          cat("ERROR: Diperlukan minimal 2 kelompok untuk uji homogenitas\n")
          return()
        }

        cat("INFORMASI ANALISIS:\n")
        cat("• Variabel dependen       : ", input$homogenVar, "\n")
        cat("• Variabel pengelompokan  : ", input$homogenGroup, "\n")
        cat("• Jenis uji homogenitas   : ", switch(input$homogenTestType,
                                                   "levene" = "Levene Test",
                                                   "bartlett" = "Bartlett Test",
                                                   "fligner" = "Fligner-Killeen Test"), "\n")
        cat("• Jumlah kelompok         : ", length(groups), "\n")
        cat("• Total observasi         : ", nrow(df), "\n")
        cat("• Tingkat signifikansi    : α = ", alpha, "\n\n")

        group_sizes <- table(df[[input$homogenGroup]])
        cat("UKURAN SAMPEL PER KELOMPOK:\n")
        for(i in 1:length(group_sizes)) {
          cat("• Kelompok '", names(group_sizes)[i], "' : ", group_sizes[i], " observasi\n")
        }
        cat("\n")

        if(any(group_sizes < 2)) {
          cat("PERINGATAN: Beberapa kelompok memiliki ukuran sampel kurang dari 2 observasi\n\n")
        }

        cat("STATISTIK DESKRIPTIF PER KELOMPOK:\n")
        cat("────────────────────────────────────────────────────────────────────────\n")
        variances <- c()
        for(group in groups) {
          group_data <- df[df[[input$homogenGroup]] == group, input$homogenVar]
          group_var <- var(group_data)
          variances <- c(variances, group_var)
          cat("Kelompok '", group, "':\n")
          cat("  • Jumlah data (n)       : ", length(group_data), "\n")
          cat("  • Mean                  : ", round(mean(group_data), 4), "\n")
          cat("  • Standar Deviasi       : ", round(sd(group_data), 4), "\n")
          cat("  • Varian                : ", round(group_var, 4), "\n\n")
        }

        max_var <- max(variances)
        min_var <- min(variances)
        var_ratio <- max_var / min_var

        cat("PERBANDINGAN VARIAN ANTAR KELOMPOK:\n")
        cat("• Varian terbesar         : ", round(max_var, 4), "\n")
        cat("• Varian terkecil         : ", round(min_var, 4), "\n")
        cat("• Rasio varian (max/min)  : ", round(var_ratio, 4), "\n")

        if(var_ratio > 4) {
          cat("• INDIKASI: Kemungkinan terdapat heteroskedastisitas (varian tidak homogen)\n\n")
        } else {
          cat("• INDIKASI: Varian relatif homogen antar kelompok\n\n")
        }

        cat("HIPOTESIS:\n")
        cat("• H₀ (Hipotesis Nol)      : Varian antar kelompok adalah homogen (sama)\n")
        cat("• H₁ (Hipotesis Alternatif): Varian antar kelompok adalah heterogen (tidak sama)\n\n")

        cat("HASIL UJI STATISTIK:\n")
        cat("────────────────────────────────────────────────────────────────────────\n")

        formula <- as.formula(paste(input$homogenVar, "~", input$homogenGroup))
        result <- NULL
        test_name <- ""

        if(input$homogenTestType == "levene") {
          test_name <- "Levene"
          result <- car::leveneTest(formula, data = df)
          cat("Levene Test untuk Homogenitas Varian:\n")
          print(result)
          p_value <- result$`Pr(>F)`[1]
        } else if(input$homogenTestType == "bartlett") {
          test_name <- "Bartlett"
          result <- bartlett.test(formula, data = df)
          print(result)
          p_value <- result$p.value
        } else if(input$homogenTestType == "fligner") {
          test_name <- "Fligner-Killeen"
          result <- fligner.test(formula, data = df)
          print(result)
          p_value <- result$p.value
        }

        cat("\nINTERPRETASI HASIL:\n")
        cat("────────────────────────────────────────────────────────────────────────\n")
        interpretation <- interpret_p_value(p_value, alpha, paste("uji homogenitas", test_name))
        cat(interpretation, "\n\n")

        if(p_value < alpha) {
          cat("KESIMPULAN:\n")
          cat("Berdasarkan hasil uji homogenitas ", test_name, " dengan tingkat kepercayaan ",
              (100-alpha*100), "%, dapat disimpulkan bahwa varian antar kelompok pada variabel '",
              input$homogenVar, "' berdasarkan '", input$homogenGroup,
              "' adalah TIDAK HOMOGEN (heteroskedastisitas).\n\n")

          cat("IMPLIKASI DAN SARAN:\n")
          cat("• Asumsi homoskedastisitas untuk ANOVA tidak terpenuhi\n")
          cat("• Pertimbangkan transformasi data atau gunakan uji non-parametrik\n")
          cat("• Untuk regresi, gunakan robust standard errors\n")
          cat("• Periksa adanya outlier yang mempengaruhi varian\n")
        } else {
          cat("KESIMPULAN:\n")
          cat("Berdasarkan hasil uji homogenitas ", test_name, " dengan tingkat kepercayaan ",
              (100-alpha*100), "%, dapat disimpulkan bahwa varian antar kelompok pada variabel '",
              input$homogenVar, "' berdasarkan '", input$homogenGroup,
              "' adalah HOMOGEN (homoskedastisitas).\n\n")

          cat("IMPLIKASI:\n")
          cat("• Asumsi homoskedastisitas untuk ANOVA terpenuhi\n")
          cat("• Dapat melanjutkan dengan uji parametrik (ANOVA, t-test)\n")
          cat("• Hasil analisis varian akan valid dan dapat diandalkan\n")
        }
      })

      temp_file <- create_professional_docx("UJI HOMOGENITAS VARIAN", content, file, alpha)
      file.copy(temp_file, file, overwrite = TRUE)
    }
  )

  output$downloadHomogeneityMulti <- downloadHandler(
    filename = function() {
      paste("Uji_Homogenitas_Antar_Variabel_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      req(input$homogenVarsMulti, input$homogenTestTypeMulti)

      alpha <- 0.05  # You can make this dynamic from UI if needed

      content <- capture_output({
        cat("=== UJI HOMOGENITAS ANTAR VARIABEL ===\n")
        cat("Variabel yang diuji:", paste(input$homogenVarsMulti, collapse = ", "), "\n")
        cat("Jenis Uji:", toupper(input$homogenTestTypeMulti), "TEST\n")
        cat("Hipotesis:\n")
        cat("H0: Varian antar variabel adalah homogen (sama)\n")
        cat("H1: Varian antar variabel adalah heterogen (tidak sama)\n\n")

        if(length(input$homogenVarsMulti) < 2) {
          cat("ERROR: Minimal 2 variabel diperlukan untuk uji homogenitas antar variabel\n")
          return()
        }

        available_vars <- names(values$processedData)
        selected_vars <- input$homogenVarsMulti[input$homogenVarsMulti %in% available_vars]

        if(length(selected_vars) < 2) {
          cat("ERROR: Variabel yang dipilih tidak ditemukan dalam dataset\n")
          return()
        }

        df <- values$processedData[, selected_vars, drop = FALSE]
        df <- df[complete.cases(df), ]
        df <- df[apply(df, 1, function(x) all(is.finite(x))), ]

        if(nrow(df) < 2) {
          cat("ERROR: Data terlalu kecil setelah pembersihan data\n")
          return()
        }

        cat("DESKRIPSI DATA:\n")
        cat("Jumlah observasi yang valid:", nrow(df), "\n")
        cat("Variabel yang dianalisis:", paste(selected_vars, collapse = ", "), "\n\n")

        cat("STATISTIK DESKRIPTIF PER VARIABEL:\n")
        for(var in selected_vars) {
          var_data <- df[[var]]
          cat(sprintf("%-15s: N=%d, Mean=%.4f, SD=%.4f, Var=%.4f\n",
                      var, length(var_data),
                      mean(var_data, na.rm = TRUE),
                      sd(var_data, na.rm = TRUE),
                      var(var_data, na.rm = TRUE)))
        }

        variances <- sapply(df, function(x) var(x, na.rm = TRUE))
        max_var <- max(variances)
        min_var <- min(variances)
        var_ratio <- max_var / min_var

        cat(sprintf("\nRASIO VARIAN (max/min): %.4f\n", var_ratio))
        if(var_ratio > 4) {
          cat("PERINGATAN: Rasio varian > 4, menunjukkan kemungkinan heterogenitas\n")
        }

        cat("\n=== HASIL UJI STATISTIK ===\n")
        df_long <- data.frame(
          value = unlist(df),
          variable = factor(rep(names(df), each = nrow(df)))
        )

        if(input$homogenTestTypeMulti == "levene") {
          result <- car::leveneTest(value ~ variable, data = df_long)
          cat("UJI LEVENE UNTUK HOMOGENITAS VARIAN:\n")
          print(result)

          p_value <- result$`Pr(>F)`[1]
          f_stat <- result$`F value`[1]
          df1 <- result$Df[1]
          df2 <- result$Df[2]

          cat(sprintf("\nSTATISTIK UJI: F(%d,%d) = %.4f\n", df1, df2, f_stat))
          cat(sprintf("P-VALUE: %.6f\n", p_value))

          cat("\n=== KESIMPULAN ===\n")
          if(p_value <= alpha) {
            cat(sprintf("Dengan tingkat kepercayaan %.0f%% (α = %.3f), dapat disimpulkan bahwa:\n",
                        (1-alpha)*100, alpha))
            cat("H0 DITOLAK - Terdapat perbedaan varian yang signifikan antar variabel\n")
            cat("Variabel-variabel memiliki varian yang TIDAK HOMOGEN (heterogen)\n")
            cat("Rekomendasi: Gunakan uji statistik yang tidak mengasumsikan homogenitas varian\n")
          } else {
            cat(sprintf("Dengan tingkat kepercayaan %.0f%% (α = %.3f), dapat disimpulkan bahwa:\n",
                        (1-alpha)*100, alpha))
            cat("H0 DITERIMA - Tidak terdapat perbedaan varian yang signifikan antar variabel\n")
            cat("Variabel-variabel memiliki varian yang HOMOGEN\n")
            cat("Rekomendasi: Asumsi homogenitas varian terpenuhi untuk analisis parametrik\n")
          }

        } else if(input$homogenTestTypeMulti == "bartlett") {
          result <- bartlett.test(value ~ variable, data = df_long)
          cat("UJI BARTLETT UNTUK HOMOGENITAS VARIAN:\n")
          print(result)

          p_value <- result$p.value
          chi_stat <- result$statistic
          df <- result$parameter

          cat(sprintf("\nSTATISTIK UJI: χ²(%d) = %.4f\n", df, chi_stat))
          cat(sprintf("P-VALUE: %.6f\n", p_value))

          cat("\n=== KESIMPULAN ===\n")
          if(p_value <= alpha) {
            cat(sprintf("Dengan tingkat kepercayaan %.0f%% (α = %.3f), dapat disimpulkan bahwa:\n",
                        (1-alpha)*100, alpha))
            cat("H0 DITOLAK - Terdapat perbedaan varian yang signifikan antar variabel\n")
            cat("Variabel-variabel memiliki varian yang TIDAK HOMOGEN (heterogen)\n")
            cat("Rekomendasi: Gunakan uji statistik yang tidak mengasumsikan homogenitas varian\n")
          } else {
            cat(sprintf("Dengan tingkat kepercayaan %.0f%% (α = %.3f), dapat disimpulkan bahwa:\n",
                        (1-alpha)*100, alpha))
            cat("H0 DITERIMA - Tidak terdapat perbedaan varian yang signifikan antar variabel\n")
            cat("Variabel-variabel memiliki varian yang HOMOGEN\n")
            cat("Rekomendasi: Asumsi homogenitas varian terpenuhi untuk analisis parametrik\n")
          }
        }
      })

      temp_file <- create_professional_docx("UJI HOMOGENITAS ANTAR VARIABEL", content, file, alpha)
      file.copy(temp_file, file, overwrite = TRUE)
    }
  )

  output$downloadJackknife <- downloadHandler(
    filename = function() {
      paste("Hasil_Jackknife_Resampling_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      req(input$jackknifeVar, input$jackknifeStatistic)

      alpha <- 0.05

      content <- capture_output({
        cat("=== JACKKNIFE RESAMPLING ANALYSIS ===\n")
        cat("Variabel:", input$jackknifeVar, "\n")
        cat("Statistik yang diestimasi:", input$jackknifeStatistic, "\n")
        cat("Metode: Leave-One-Out Jackknife\n\n")
        data <- values$processedData[[input$jackknifeVar]]
        data <- data[!is.na(data) & is.finite(data)]
        n <- length(data)

        cat("DESKRIPSI DATA:\n")
        cat("Jumlah observasi:", n, "\n")
        cat("Mean asli:", round(mean(data), 4), "\n")
        cat("SD asli:", round(sd(data), 4), "\n\n")

        if(input$jackknifeStatistic == "mean") {
          jackknife_stats <- numeric(n)
          for(i in 1:n) {
            jackknife_stats[i] <- mean(data[-i])
          }

          original_stat <- mean(data)
          jackknife_mean <- mean(jackknife_stats)
          jackknife_se <- sqrt((n-1)/n * sum((jackknife_stats - jackknife_mean)^2))
          bias <- (n-1) * (jackknife_mean - original_stat)
          bias_corrected <- original_stat - bias

          cat("=== HASIL JACKKNIFE RESAMPLING ===\n")
          cat("Statistik asli (mean):", round(original_stat, 4), "\n")
          cat("Estimasi Jackknife (mean):", round(jackknife_mean, 4), "\n")
          cat("Standard Error Jackknife:", round(jackknife_se, 4), "\n")
          cat("Bias estimasi:", round(bias, 6), "\n")
          cat("Estimasi terkoreksi bias:", round(bias_corrected, 4), "\n")
          t_val <- qt(1 - alpha/2, df = n-1)
          ci_lower <- bias_corrected - t_val * jackknife_se
          ci_upper <- bias_corrected + t_val * jackknife_se

          cat(sprintf("\n=== INTERVAL KEPERCAYAAN %.0f%% ===\n", (1-alpha)*100))
          cat(sprintf("Batas bawah: %.4f\n", ci_lower))
          cat(sprintf("Batas atas: %.4f\n", ci_upper))

          cat("\n=== KESIMPULAN ===\n")
          cat(sprintf("Dengan tingkat kepercayaan %.0f%%, estimasi mean populasi\n", (1-alpha)*100))
          cat(sprintf("menggunakan metode Jackknife adalah %.4f ± %.4f\n", bias_corrected, jackknife_se))
          cat(sprintf("Interval kepercayaan: [%.4f, %.4f]\n", ci_lower, ci_upper))

          if(abs(bias) < 0.01 * abs(original_stat)) {
            cat("Bias estimasi relatif kecil, menunjukkan estimator yang baik\n")
          } else {
            cat("Bias estimasi cukup besar, pertimbangkan koreksi bias\n")
          }
        }

        # Add similar logic for other statistics (variance, median, etc.)
      })

      temp_file <- create_professional_docx("ANALISIS JACKKNIFE RESAMPLING", content, file, alpha)
      file.copy(temp_file, file, overwrite = TRUE)
    }
  )

  output$downloadBootstrap <- downloadHandler(
    filename = function() {
      paste("Hasil_Bootstrap_Resampling_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      req(input$bootstrapVar, input$bootstrapStatistic)

      alpha <- 0.05
      n_bootstrap <- 1000  # You can make this dynamic from UI

      content <- capture_output({
        cat("=== BOOTSTRAP RESAMPLING ANALYSIS ===\n")
        cat("Variabel:", input$bootstrapVar, "\n")
        cat("Statistik yang diestimasi:", input$bootstrapStatistic, "\n")
        cat("Jumlah replikasi Bootstrap:", n_bootstrap, "\n")
        cat("Metode: Non-parametric Bootstrap\n\n")
        data <- values$processedData[[input$bootstrapVar]]
        data <- data[!is.na(data) & is.finite(data)]
        n <- length(data)

        cat("DESKRIPSI DATA:\n")
        cat("Jumlah observasi:", n, "\n")
        cat("Mean asli:", round(mean(data), 4), "\n")
        cat("SD asli:", round(sd(data), 4), "\n\n")
        set.seed(123)  # For reproducibility
        if(input$bootstrapStatistic == "mean") {
          bootstrap_stats <- replicate(n_bootstrap, {
            boot_sample <- sample(data, n, replace = TRUE)
            mean(boot_sample)
          })

          original_stat <- mean(data)
          bootstrap_mean <- mean(bootstrap_stats)
          bootstrap_se <- sd(bootstrap_stats)
          bias <- bootstrap_mean - original_stat
          bias_corrected <- original_stat - bias

          cat("=== HASIL BOOTSTRAP RESAMPLING ===\n")
          cat("Statistik asli (mean):", round(original_stat, 4), "\n")
          cat("Estimasi Bootstrap (mean):", round(bootstrap_mean, 4), "\n")
          cat("Standard Error Bootstrap:", round(bootstrap_se, 4), "\n")
          cat("Bias estimasi:", round(bias, 6), "\n")
          cat("Estimasi terkoreksi bias:", round(bias_corrected, 4), "\n")
          ci_lower_percentile <- quantile(bootstrap_stats, alpha/2)
          ci_upper_percentile <- quantile(bootstrap_stats, 1 - alpha/2)

          ci_lower_basic <- 2*original_stat - ci_upper_percentile
          ci_upper_basic <- 2*original_stat - ci_lower_percentile

          cat(sprintf("\n=== INTERVAL KEPERCAYAAN %.0f%% ===\n", (1-alpha)*100))
          cat("METODE PERCENTILE:\n")
          cat(sprintf("  Batas bawah: %.4f\n", ci_lower_percentile))
          cat(sprintf("  Batas atas: %.4f\n", ci_upper_percentile))

          cat("METODE BASIC BOOTSTRAP:\n")
          cat(sprintf("  Batas bawah: %.4f\n", ci_lower_basic))
          cat(sprintf("  Batas atas: %.4f\n", ci_upper_basic))

          cat("\n=== KESIMPULAN ===\n")
          cat(sprintf("Dengan tingkat kepercayaan %.0f%%, estimasi mean populasi\n", (1-alpha)*100))
          cat(sprintf("menggunakan metode Bootstrap adalah %.4f ± %.4f\n", bootstrap_mean, bootstrap_se))
          cat(sprintf("Interval kepercayaan (Percentile): [%.4f, %.4f]\n", ci_lower_percentile, ci_upper_percentile))

          if(abs(bias) < 0.01 * abs(original_stat)) {
            cat("Bias estimasi relatif kecil, menunjukkan estimator yang robust\n")
          } else {
            cat("Bias estimasi cukup besar, gunakan estimasi terkoreksi bias\n")
          }
        }
      })

      temp_file <- create_professional_docx("ANALISIS BOOTSTRAP RESAMPLING", content, file, alpha)
      file.copy(temp_file, file, overwrite = TRUE)
    }
  )

  output$downloadPermutation <- downloadHandler(
    filename = function() {
      paste("Hasil_Permutation_Test_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      req(input$permutationVar, input$permutationGroup)

      alpha <- 0.05
      n_permutations <- 1000  # You can make this dynamic from UI

      content <- capture_output({
        cat("=== PERMUTATION TEST ANALYSIS ===\n")
        cat("Variabel dependen:", input$permutationVar, "\n")
        cat("Variabel grouping:", input$permutationGroup, "\n")
        cat("Jumlah permutasi:", n_permutations, "\n")
        cat("Hipotesis:\n")
        cat("H0: Tidak ada perbedaan rata-rata antar grup\n")
        cat("H1: Terdapat perbedaan rata-rata antar grup\n\n")
        if(is.null(values$processedData)) {
          cat("ERROR: Data tidak ditemukan. Pastikan data sudah diupload.\n")
          return()
        }

        df <- values$processedData
        if(!(input$permutationVar %in% names(df))) {
          cat("ERROR: Variabel", input$permutationVar, "tidak ditemukan dalam dataset.\n")
          return()
        }

        if(!(input$permutationGroup %in% names(df))) {
          cat("ERROR: Variabel", input$permutationGroup, "tidak ditemukan dalam dataset.\n")
          return()
        }
        df <- df[complete.cases(df[[input$permutationVar]], df[[input$permutationGroup]]), ]

        if(nrow(df) < 4) {
          cat("ERROR: Data terlalu sedikit untuk analisis permutasi (minimal 4 observasi).\n")
          return()
        }
        if(!is.numeric(df[[input$permutationVar]])) {
          cat("ERROR: Variabel", input$permutationVar, "harus berupa data numerik.\n")
          return()
        }
        df[[input$permutationGroup]] <- as.factor(df[[input$permutationGroup]])
        groups <- levels(df[[input$permutationGroup]])

        if(length(groups) < 2) {
          cat("ERROR: Variabel grouping harus memiliki minimal 2 kategori.\n")
          return()
        }

        if(length(groups) > 2) {
          cat("PERINGATAN: Permutation test ini menggunakan 2 grup pertama saja.\n")
          cat("Grup yang digunakan:", groups[1], "dan", groups[2], "\n\n")
          df <- df[df[[input$permutationGroup]] %in% groups[1:2], ]
          df[[input$permutationGroup]] <- droplevels(df[[input$permutationGroup]])
          groups <- levels(df[[input$permutationGroup]])
        }
        group1_data <- df[df[[input$permutationGroup]] == groups[1], input$permutationVar]
        group2_data <- df[df[[input$permutationGroup]] == groups[2], input$permutationVar]
        group1_data <- group1_data[is.finite(group1_data)]
        group2_data <- group2_data[is.finite(group2_data)]

        if(length(group1_data) < 2 || length(group2_data) < 2) {
          cat("ERROR: Setiap grup harus memiliki minimal 2 observasi valid.\n")
          cat("Grup", groups[1], ":", length(group1_data), "observasi\n")
          cat("Grup", groups[2], ":", length(group2_data), "observasi\n")
          return()
        }

        cat("DESKRIPSI DATA:\n")
        cat("Total observasi valid:", length(group1_data) + length(group2_data), "\n")
        cat("Grup", groups[1], ":", "N =", length(group1_data),
            ", Mean =", round(mean(group1_data), 4),
            ", SD =", round(sd(group1_data), 4), "\n")
        cat("Grup", groups[2], ":", "N =", length(group2_data),
            ", Mean =", round(mean(group2_data), 4),
            ", SD =", round(sd(group2_data), 4), "\n")
        original_diff <- mean(group1_data) - mean(group2_data)
        cat("Perbedaan mean asli:", round(original_diff, 4), "\n\n")
        cat("=== MELAKUKAN PERMUTATION TEST ===\n")
        set.seed(123)  # For reproducibility
        all_data <- c(group1_data, group2_data)
        n1 <- length(group1_data)
        n2 <- length(group2_data)

        tryCatch({
          permutation_diffs <- replicate(n_permutations, {
            shuffled <- sample(all_data)
            perm_group1 <- shuffled[1:n1]
            perm_group2 <- shuffled[(n1+1):(n1+n2)]
            mean(perm_group1) - mean(perm_group2)
          })
          p_value <- mean(abs(permutation_diffs) >= abs(original_diff))

          cat("=== HASIL PERMUTATION TEST ===\n")
          cat("Statistik uji (perbedaan mean):", round(original_diff, 4), "\n")
          cat("P-value (two-tailed):", round(p_value, 6), "\n")
          cat("Jumlah permutasi yang berhasil:", n_permutations, "\n")

          cat("\n=== DISTRIBUSI PERMUTASI ===\n")
          cat("Mean distribusi permutasi:", round(mean(permutation_diffs), 6), "\n")
          cat("SD distribusi permutasi:", round(sd(permutation_diffs), 4), "\n")
          cat("Range distribusi: [", round(min(permutation_diffs), 4),
              ",", round(max(permutation_diffs), 4), "]\n")
          extreme_values <- sum(abs(permutation_diffs) >= abs(original_diff))
          cat("Jumlah permutasi dengan |diff| >= |diff_observed|:", extreme_values, "\n")

          cat("\n=== KESIMPULAN ===\n")
          if(p_value <= alpha) {
            cat(sprintf("Dengan tingkat kepercayaan %.0f%% (α = %.3f), dapat disimpulkan bahwa:\n",
                        (1-alpha)*100, alpha))
            cat("H0 DITOLAK - Terdapat perbedaan rata-rata yang signifikan antar grup\n")
            cat(sprintf("Grup '%s' dan '%s' memiliki perbedaan mean yang signifikan\n", groups[1], groups[2]))

            if(original_diff > 0) {
              cat(sprintf("Grup '%s' memiliki rata-rata yang lebih tinggi daripada grup '%s'\n", groups[1], groups[2]))
            } else {
              cat(sprintf("Grup '%s' memiliki rata-rata yang lebih tinggi daripada grup '%s'\n", groups[2], groups[1]))
            }

            cat("Rekomendasi: Perbedaan yang diamati tidak disebabkan oleh kebetulan\n")
          } else {
            cat(sprintf("Dengan tingkat kepercayaan %.0f%% (α = %.3f), dapat disimpulkan bahwa:\n",
                        (1-alpha)*100, alpha))
            cat("H0 DITERIMA - Tidak terdapat perbedaan rata-rata yang signifikan antar grup\n")
            cat(sprintf("Grup '%s' dan '%s' tidak memiliki perbedaan mean yang signifikan\n", groups[1], groups[2]))
            cat("Rekomendasi: Perbedaan yang diamati dapat disebabkan oleh variasi acak\n")
          }

        }, error = function(e) {
          cat("ERROR dalam perhitungan permutation test:\n")
          cat("Pesan error:", conditionMessage(e), "\n")
          cat("Pastikan data numerik dan tidak mengandung nilai ekstrem.\n")
        })
      })

      temp_file <- create_professional_docx("ANALISIS PERMUTATION TEST", content, file, alpha)
      file.copy(temp_file, file, overwrite = TRUE)
    }
  )

  output$downloadHasilUji <- downloadHandler(
    filename = function() {
      paste("Ringkasan_Hasil_Uji_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      alpha <- 0.05

      content <- capture_output({
        cat("=== RINGKASAN HASIL UJI STATISTIK ===\n")
        cat("Laporan Komprehensif Analisis Statistik\n\n")
        cat("DAFTAR UJI YANG TELAH DILAKUKAN:\n")
        cat("1. Uji Normalitas Data\n")
        cat("2. Uji Homogenitas Varian\n")
        cat("3. Analisis Resampling (Bootstrap/Jackknife)\n")
        cat("4. Uji Permutasi\n\n")

        cat("RINGKASAN DATASET:\n")
        if(!is.null(values$processedData)) {
          df <- values$processedData
          cat("Jumlah observasi:", nrow(df), "\n")
          cat("Jumlah variabel:", ncol(df), "\n")
          cat("Variabel numerik:", paste(names(df)[sapply(df, is.numeric)], collapse = ", "), "\n")
          cat("Missing values: Ada" , sum(is.na(df)), "nilai hilang\n\n")
        }

        cat("=== REKOMENDASI UMUM ===\n")
        cat("1. Pastikan asumsi statistik terpenuhi sebelum analisis lanjutan\n")
        cat("2. Gunakan metode non-parametrik jika asumsi normalitas tidak terpenuhi\n")
        cat("3. Pertimbangkan transformasi data jika diperlukan\n")
        cat("4. Validasi hasil dengan metode resampling\n")
        cat("5. Interpretasi hasil harus mempertimbangkan konteks penelitian\n\n")

        cat("=== CATATAN METODOLOGI ===\n")
        cat(sprintf("• Tingkat signifikansi yang digunakan: α = %.3f\n", alpha))
        cat(sprintf("• Tingkat kepercayaan: %.0f%%\n", (1-alpha)*100))
        cat("• Metode koreksi multiple comparison: Tidak diterapkan\n")
        cat("• Software: R Statistical Computing\n")
        cat("• Tanggal analisis:", format(Sys.Date(), "%d %B %Y"), "\n")
      })

      temp_file <- create_professional_docx("RINGKASAN HASIL UJI STATISTIK", content, file, alpha)
      file.copy(temp_file, file, overwrite = TRUE)
    }
  )
  output$downloadMLR <- downloadHandler(
    filename = function() {
      paste("Laporan_Regresi_Linear_Berganda_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      alpha <- if(exists("input$alpha")) input$alpha else 0.05

      if(!mlr_results$run_analysis) {
        content <- "PERHATIAN: Analisis regresi linear berganda belum dijalankan.\nSilakan jalankan analisis terlebih dahulu untuk mendapatkan hasil."
      } else {
        req(mlr_model())
        model <- mlr_model()
        content <- capture_output({
          summary_model <- summary(model)

          cat("════════════════════════════════════════════════════════════════════════\n")
          cat("                    ANALISIS REGRESI LINEAR BERGANDA                    \n")
          cat("════════════════════════════════════════════════════════════════════════\n\n")

          cat("INFORMASI MODEL:\n")
          cat("• Formula model           : ", deparse(formula(model)), "\n")
          cat("• Jumlah observasi        : ", nobs(model), "\n")
          cat("• Jumlah parameter        : ", length(coef(model)), "\n")
          cat("• Tingkat signifikansi    : α = ", alpha, "\n\n")

          cat("RINGKASAN STATISTIK MODEL:\n")
          cat("────────────────────────────────────────────────────────────────────────\n")
          print(summary_model)
          cat("\n")

          cat("EVALUASI KEBAIKAN MODEL (GOODNESS OF FIT):\n")
          cat("────────────────────────────────────────────────────────────────────────\n")
          r_squared <- summary_model$r.squared
          adj_r_squared <- summary_model$adj.r.squared
          f_statistic <- summary_model$fstatistic[1]
          f_pvalue <- pf(summary_model$fstatistic[1], summary_model$fstatistic[2],
                         summary_model$fstatistic[3], lower.tail = FALSE)

          cat("• R-squared               : ", round(r_squared, 4),
              " (", round(r_squared * 100, 2), "% variasi dijelaskan)\n")
          cat("• Adjusted R-squared      : ", round(adj_r_squared, 4), "\n")
          cat("• F-statistic             : ", round(f_statistic, 4), "\n")
          cat("• P-value F-test          : ", format(f_pvalue, scientific = TRUE, digits = 4), "\n\n")

          cat("INTERPRETASI KEBAIKAN MODEL:\n")
          if(r_squared >= 0.7) {
            cat("• Model memiliki kemampuan prediksi yang SANGAT BAIK\n")
          } else if(r_squared >= 0.5) {
            cat("• Model memiliki kemampuan prediksi yang BAIK\n")
          } else if(r_squared >= 0.3) {
            cat("• Model memiliki kemampuan prediksi yang CUKUP\n")
          } else {
            cat("• Model memiliki kemampuan prediksi yang LEMAH\n")
          }

          cat("• Model secara keseluruhan ",
              ifelse(f_pvalue < alpha, "SIGNIFIKAN", "TIDAK SIGNIFIKAN"),
              " dalam menjelaskan variabel dependen\n\n")

          cat("ANALISIS KOEFISIEN REGRESI:\n")
          cat("────────────────────────────────────────────────────────────────────────\n")
          coef_table <- summary_model$coefficients

          for(i in 1:nrow(coef_table)) {
            var_name <- rownames(coef_table)[i]
            coef_val <- coef_table[i, 1]
            std_error <- coef_table[i, 2]
            t_val <- coef_table[i, 3]
            p_val <- coef_table[i, 4]

            cat("Variabel: ", var_name, "\n")
            cat("  • Koefisien (β)         : ", round(coef_val, 4), "\n")
            cat("  • Standard Error        : ", round(std_error, 4), "\n")
            cat("  • t-value               : ", round(t_val, 4), "\n")
            cat("  • P-value               : ", format(p_val, scientific = TRUE, digits = 4), "\n")

            if(var_name == "(Intercept)") {
              cat("  • Interpretasi          : Nilai konstanta ketika semua variabel independen = 0\n")
              cat("  • Signifikansi          : ",
                  ifelse(p_val < alpha, "SIGNIFIKAN", "TIDAK SIGNIFIKAN"), "\n\n")
            } else {
              direction <- ifelse(coef_val > 0, "positif", "negatif")
              cat("  • Interpretasi          : Setiap peningkatan 1 unit pada ", var_name,
                  " akan ", ifelse(coef_val > 0, "meningkatkan", "menurunkan"),
                  " variabel dependen sebesar ", abs(round(coef_val, 4)), " unit\n")
              cat("  • Arah hubungan         : ", toupper(direction), "\n")
              cat("  • Signifikansi          : ",
                  ifelse(p_val < alpha, "SIGNIFIKAN", "TIDAK SIGNIFIKAN"), "\n")

              if(p_val < alpha) {
                cat("  • Kesimpulan            : Dengan tingkat kepercayaan ", (100-alpha*100),
                    "%, variabel ", var_name, " memiliki pengaruh yang signifikan\n\n")
              } else {
                cat("  • Kesimpulan            : Dengan tingkat kepercayaan ", (100-alpha*100),
                    "%, variabel ", var_name, " TIDAK memiliki pengaruh yang signifikan\n\n")
              }
            }
          }

          cat("KESIMPULAN UMUM:\n")
          cat("────────────────────────────────────────────────────────────────────────\n")
          significant_vars <- rownames(coef_table)[coef_table[, 4] < alpha & rownames(coef_table) != "(Intercept)"]

          if(length(significant_vars) > 0) {
            cat("Berdasarkan analisis regresi linear berganda dengan tingkat kepercayaan ",
                (100-alpha*100), "%, variabel yang memiliki pengaruh signifikan adalah: ",
                paste(significant_vars, collapse = ", "), ".\n\n")
          } else {
            cat("Berdasarkan analisis regresi linear berganda dengan tingkat kepercayaan ",
                (100-alpha*100), "%, tidak ada variabel independen yang memiliki pengaruh signifikan.\n\n")
          }

          cat("Model mampu menjelaskan ", round(r_squared * 100, 2),
              "% variasi dalam variabel dependen, sedangkan sisanya (",
              round((1 - r_squared) * 100, 2), "%) dijelaskan oleh faktor lain di luar model.\n\n")

          cat("SARAN UNTUK ANALISIS LANJUTAN:\n")
          cat("• Lakukan uji asumsi klasik (normalitas, multikolinearitas, homoskedastisitas, autokorelasi)\n")
          cat("• Periksa adanya outlier dan influential observations\n")
          cat("• Pertimbangkan transformasi variabel jika diperlukan\n")
          cat("• Validasi model dengan data baru (cross-validation)\n")
        })
      }

      temp_file <- create_professional_docx("ANALISIS REGRESI LINEAR BERGANDA", content, file, alpha)
      file.copy(temp_file, file, overwrite = TRUE)
    }
  )
  output$downloadNormalitasMLR <- downloadHandler(
    filename = function() {
      paste("Laporan_Uji_Normalitas_Residual_MLR_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      alpha <- if(exists("input$alpha")) input$alpha else 0.05

      if(!mlr_results$run_analysis) {
        content <- "PERHATIAN: Analisis regresi linear berganda belum dijalankan.\nSilakan jalankan analisis regresi terlebih dahulu untuk melakukan uji normalitas residual."
      } else {
        req(mlr_model())
        model <- mlr_model()
        residuals_data <- residuals(model)

        content <- capture_output({
          cat("════════════════════════════════════════════════════════════════════════\n")
          cat("              UJI NORMALITAS RESIDUAL - REGRESI LINEAR BERGANDA         \n")
          cat("════════════════════════════════════════════════════════════════════════\n\n")

          cat("INFORMASI ANALISIS:\n")
          cat("• Model regresi           : ", deparse(formula(model)), "\n")
          cat("• Jumlah residual         : ", length(residuals_data), "\n")
          cat("• Jenis uji normalitas    : Shapiro-Wilk Test\n")
          cat("• Tingkat signifikansi    : α = ", alpha, "\n\n")

          cat("PENTINGNYA UJI NORMALITAS RESIDUAL:\n")
          cat("Uji normalitas residual adalah salah satu asumsi klasik dalam regresi linear.\n")
          cat("Residual yang berdistribusi normal menunjukkan bahwa model telah menangkap\n")
          cat("pola hubungan dengan baik dan tidak ada bias sistematis.\n\n")

          cat("STATISTIK DESKRIPTIF RESIDUAL:\n")
          cat("────────────────────────────────────────────────────────────────────────\n")
          cat("• Mean residual           : ", round(mean(residuals_data), 6), "\n")
          cat("• Standar Deviasi         : ", round(sd(residuals_data), 4), "\n")
          cat("• Minimum residual        : ", round(min(residuals_data), 4), "\n")
          cat("• Maksimum residual       : ", round(max(residuals_data), 4), "\n")
          cat("• Skewness                : ", round(moments::skewness(residuals_data), 4), "\n")
          cat("• Kurtosis                : ", round(moments::kurtosis(residuals_data), 4), "\n\n")

          cat("HIPOTESIS UJI:\n")
          cat("• H₀ (Hipotesis Nol)      : Residual berdistribusi normal\n")
          cat("• H₁ (Hipotesis Alternatif): Residual tidak berdistribusi normal\n\n")

          cat("HASIL UJI SHAPIRO-WILK:\n")
          cat("────────────────────────────────────────────────────────────────────────\n")
          shapiro_test <- shapiro.test(residuals_data)
          print(shapiro_test)
          cat("\n")

          cat("INTERPRETASI HASIL:\n")
          cat("────────────────────────────────────────────────────────────────────────\n")
          interpretation <- interpret_p_value(shapiro_test$p.value, alpha, "normalitas residual Shapiro-Wilk")
          cat(interpretation, "\n\n")

          if(shapiro_test$p.value > alpha) {
            cat("KESIMPULAN:\n")
            cat("Berdasarkan hasil uji Shapiro-Wilk dengan tingkat kepercayaan ",
                (100-alpha*100), "%, dapat disimpulkan bahwa residual model regresi linear berganda ",
                "berdistribusi NORMAL.\n\n")

            cat("IMPLIKASI POSITIF:\n")
            cat("• ✓ Asumsi normalitas residual TERPENUHI\n")
            cat("• ✓ Estimasi parameter model dapat diandalkan\n")
            cat("• ✓ Uji signifikansi (t-test dan F-test) valid\n")
            cat("• ✓ Interval kepercayaan untuk koefisien akurat\n")
            cat("• ✓ Prediksi model memiliki properties statistik yang baik\n")

          } else {
            cat("KESIMPULAN:\n")
            cat("Berdasarkan hasil uji Shapiro-Wilk dengan tingkat kepercayaan ",
                (100-alpha*100), "%, dapat disimpulkan bahwa residual model regresi linear berganda ",
                "TIDAK berdistribusi normal.\n\n")

            cat("IMPLIKASI NEGATIF:\n")
            cat("• ✗ Asumsi normalitas residual TIDAK TERPENUHI\n")
            cat("• ✗ Estimasi parameter mungkin kurang efisien\n")
            cat("• ✗ Uji signifikansi mungkin tidak akurat\n")
            cat("• ✗ Interval kepercayaan mungkin bias\n")
            cat("• ✗ Prediksi model kurang reliable\n\n")

            cat("SARAN PERBAIKAN:\n")
            cat("1. Periksa adanya outlier dalam data\n")
            cat("2. Pertimbangkan transformasi variabel (log, akar kuadrat, Box-Cox)\n")
            cat("3. Tambah variabel independen yang relevan\n")
            cat("4. Periksa spesifikasi model (mungkin perlu interaksi atau polynomial)\n")
            cat("5. Gunakan robust regression jika transformasi tidak membantu\n")
          }
        })
      }

      temp_file <- create_professional_docx("UJI NORMALITAS RESIDUAL", content, file, alpha)
      file.copy(temp_file, file, overwrite = TRUE)
    }
  )
  output$downloadMultikolinearitas <- downloadHandler(
    filename = function() {
      paste("Laporan_Uji_Multikolinearitas_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      alpha <- if(exists("input$alpha")) input$alpha else 0.05

      if(!mlr_results$run_analysis) {
        content <- "PERHATIAN: Analisis regresi linear berganda belum dijalankan.\nSilakan jalankan analisis regresi terlebih dahulu untuk melakukan uji multikolinearitas."
      } else {
        req(mlr_model())
        model <- mlr_model()

        content <- capture_output({
          cat("════════════════════════════════════════════════════════════════════════\n")
          cat("                UJI MULTIKOLINEARITAS - REGRESI LINEAR BERGANDA         \n")
          cat("════════════════════════════════════════════════════════════════════════\n\n")

          cat("INFORMASI ANALISIS:\n")
          cat("• Model regresi           : ", deparse(formula(model)), "\n")
          cat("• Metode deteksi          : Variance Inflation Factor (VIF)\n")
          cat("• Tingkat signifikansi    : α = ", alpha, "\n\n")

          cat("PENTINGNYA UJI MULTIKOLINEARITAS:\n")
          cat("Multikolinearitas terjadi ketika terdapat korelasi tinggi antar variabel independen.\n")
          cat("Hal ini dapat menyebabkan ketidakstabilan estimasi koefisien dan interpretasi yang\n")
          cat("menyesatkan dalam analisis regresi.\n\n")
          if(length(coef(model)) > 2) {  # More than just intercept + 1 variable
            vif_values <- car::vif(model)

            cat("HASIL VARIANCE INFLATION FACTOR (VIF):\n")
            cat("────────────────────────────────────────────────────────────────────────\n")

            if(is.matrix(vif_values)) {
              vif_df <- as.data.frame(vif_values)
              for(i in 1:nrow(vif_df)) {
                var_name <- rownames(vif_df)[i]
                vif_val <- vif_df[i, "GVIF^(1/(2*Df))"]
                cat("• ", var_name, " : ", round(vif_val, 4), "\n")
              }
            } else {
              for(i in 1:length(vif_values)) {
                var_name <- names(vif_values)[i]
                vif_val <- vif_values[i]
                cat("• ", var_name, " : ", round(vif_val, 4), "\n")
              }
            }
            cat("\n")

            cat("KRITERIA INTERPRETASI VIF:\n")
            cat("• VIF < 5      : TIDAK ada multikolinearitas\n")
            cat("• VIF 5-10     : Multikolinearitas SEDANG (perlu perhatian)\n")
            cat("• VIF > 10     : Multikolinearitas TINGGI (masalah serius)\n\n")

            cat("DIAGNOSIS MULTIKOLINEARITAS:\n")
            cat("────────────────────────────────────────────────────────────────────────\n")

            if(is.matrix(vif_values)) {
              max_vif <- max(vif_values[, "GVIF^(1/(2*Df))"])
              problem_vars <- rownames(vif_values)[vif_values[, "GVIF^(1/(2*Df))"] > 5]
            } else {
              max_vif <- max(vif_values)
              problem_vars <- names(vif_values)[vif_values > 5]
            }

            if(max_vif < 5) {
              cat("✓ KESIMPULAN: Dengan tingkat kepercayaan ", (100-alpha*100),
                  "%, TIDAK terdapat masalah multikolinearitas dalam model.\n\n")

              cat("IMPLIKASI POSITIF:\n")
              cat("• ✓ Estimasi koefisien stabil dan reliable\n")
              cat("• ✓ Standard error koefisien tidak inflated\n")
              cat("• ✓ Interpretasi koefisien dapat dipercaya\n")
              cat("• ✓ Model memiliki power statistik yang baik\n")

            } else if(max_vif <= 10) {
              cat("⚠ KESIMPULAN: Dengan tingkat kepercayaan ", (100-alpha*100),
                  "%, terdapat indikasi multikolinearitas SEDANG dalam model.\n\n")

              if(length(problem_vars) > 0) {
                cat("VARIABEL DENGAN MULTIKOLINEARITAS SEDANG:\n")
                cat("• ", paste(problem_vars, collapse = ", "), "\n\n")
              }

              cat("IMPLIKASI:\n")
              cat("• Standard error koefisien sedikit meningkat\n")
              cat("• Estimasi masih dapat diterima tetapi kurang efisien\n")
              cat("• Interpretasi individual koefisien perlu hati-hati\n\n")

              cat("SARAN TINDAK LANJUT:\n")
              cat("• Monitor perubahan koefisien jika ada penambahan/pengurangan variabel\n")
              cat("• Pertimbangkan untuk menghapus salah satu variabel yang berkorelasi tinggi\n")
              cat("• Gunakan principal component analysis (PCA) jika diperlukan\n")

            } else {
              cat("✗ KESIMPULAN: Dengan tingkat kepercayaan ", (100-alpha*100),
                  "%, terdapat masalah multikolinearitas TINGGI dalam model.\n\n")

              if(length(problem_vars) > 0) {
                cat("VARIABEL DENGAN MULTIKOLINEARITAS TINGGI:\n")
                cat("• ", paste(problem_vars, collapse = ", "), "\n\n")
              }

              cat("IMPLIKASI NEGATIF:\n")
              cat("• ✗ Standard error koefisien sangat tinggi\n")
              cat("• ✗ Estimasi koefisien tidak stabil\n")
              cat("• ✗ Interpretasi koefisien tidak reliable\n")
              cat("• ✗ Model kehilangan power statistik\n\n")

              cat("SARAN PERBAIKAN WAJIB:\n")
              cat("1. Hapus salah satu variabel yang berkorelasi tinggi\n")
              cat("2. Kombinasikan variabel yang berkorelasi menjadi indeks\n")
              cat("3. Gunakan teknik regularisasi (Ridge/Lasso Regression)\n")
              cat("4. Aplikasikan Principal Component Analysis (PCA)\n")
              cat("5. Kumpulkan data tambahan jika memungkinkan\n")
            }

          } else {
            cat("CATATAN: Model hanya memiliki satu variabel independen.\n")
            cat("Uji multikolinearitas tidak diperlukan untuk model dengan satu prediktor.\n")
          }
        })
      }

      temp_file <- create_professional_docx("UJI MULTIKOLINEARITAS", content, file, alpha)
      file.copy(temp_file, file, overwrite = TRUE)
    }
  )
  output$downloadHomoskedastisitas <- downloadHandler(
    filename = function() {
      paste("Laporan_Uji_Homoskedastisitas_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      alpha <- if(exists("input$alpha")) input$alpha else 0.05

      if(!mlr_results$run_analysis) {
        content <- "PERHATIAN: Analisis regresi linear berganda belum dijalankan.\nSilakan jalankan analisis regresi terlebih dahulu untuk melakukan uji homoskedastisitas."
      } else {
        req(mlr_model())
        model <- mlr_model()

        content <- capture_output({
          cat("════════════════════════════════════════════════════════════════════════\n")
          cat("            UJI HOMOSKEDASTISITAS - REGRESI LINEAR BERGANDA             \n")
          cat("════════════════════════════════════════════════════════════════════════\n\n")

          cat("INFORMASI ANALISIS:\n")
          cat("• Model regresi           : ", deparse(formula(model)), "\n")
          cat("• Jenis uji               : Breusch-Pagan Test\n")
          cat("• Tingkat signifikansi    : α = ", alpha, "\n\n")

          cat("PENTINGNYA UJI HOMOSKEDASTISITAS:\n")
          cat("Homoskedastisitas adalah asumsi bahwa varian residual konstan untuk semua\n")
          cat("nilai prediksi. Pelanggaran asumsi ini (heteroskedastisitas) dapat membuat\n")
          cat("standard error bias dan uji signifikansi tidak akurat.\n\n")

          cat("HIPOTESIS UJI:\n")
          cat("• H₀ (Hipotesis Nol)      : Varian residual homogen (homoskedastisitas)\n")
          cat("• H₁ (Hipotesis Alternatif): Varian residual tidak homogen (heteroskedastisitas)\n\n")
          bp_test <- lmtest::bptest(model)

          cat("HASIL UJI BREUSCH-PAGAN:\n")
          cat("────────────────────────────────────────────────────────────────────────\n")
          print(bp_test)
          cat("\n")

          cat("INTERPRETASI HASIL:\n")
          cat("────────────────────────────────────────────────────────────────────────\n")
          interpretation <- interpret_p_value(bp_test$p.value, alpha, "homoskedastisitas Breusch-Pagan")
          cat(interpretation, "\n\n")
          residuals_data <- residuals(model)
          fitted_values <- fitted(model)

          cat("STATISTIK DIAGNOSTIK TAMBAHAN:\n")
          cat("────────────────────────────────────────────────────────────────────────\n")
          cat("• Mean squared residual   : ", round(mean(residuals_data^2), 6), "\n")
          cat("• Variance of residuals   : ", round(var(residuals_data), 6), "\n")
          abs_resid_fitted_cor <- cor(abs(residuals_data), fitted_values)
          cat("• Korelasi |residual| vs fitted: ", round(abs_resid_fitted_cor, 4), "\n\n")

          if(bp_test$p.value > alpha) {
            cat("KESIMPULAN:\n")
            cat("Berdasarkan hasil uji Breusch-Pagan dengan tingkat kepercayaan ",
                (100-alpha*100), "%, dapat disimpulkan bahwa model regresi memenuhi asumsi ",
                "HOMOSKEDASTISITAS.\n\n")

            cat("IMPLIKASI POSITIF:\n")
            cat("• ✓ Asumsi homoskedastisitas TERPENUHI\n")
            cat("• ✓ Standard error estimasi koefisien akurat\n")
            cat("• ✓ Uji signifikansi (t-test dan F-test) valid\n")
            cat("• ✓ Interval kepercayaan koefisien reliable\n")
            cat("• ✓ Efisiensi estimator OLS optimal\n")

          } else {
            cat("KESIMPULAN:\n")
            cat("Berdasarkan hasil uji Breusch-Pagan dengan tingkat kepercayaan ",
                (100-alpha*100), "%, dapat disimpulkan bahwa model regresi mengalami ",
                "HETEROSKEDASTISITAS.\n\n")

            cat("IMPLIKASI NEGATIF:\n")
            cat("• ✗ Asumsi homoskedastisitas TIDAK TERPENUHI\n")
            cat("• ✗ Standard error estimasi bias (biasanya underestimated)\n")
            cat("• ✗ Uji signifikansi tidak akurat (Type I error meningkat)\n")
            cat("• ✗ Interval kepercayaan tidak reliable\n")
            cat("• ✗ Estimator OLS tidak efisien lagi\n\n")

            cat("SARAN PERBAIKAN:\n")
            cat("1. TRANSFORMASI DATA:\n")
            cat("   • Log transformation untuk variabel yang skewed\n")
            cat("   • Square root transformation\n")
            cat("   • Box-Cox transformation\n\n")
            cat("2. ROBUST STANDARD ERRORS:\n")
            cat("   • Gunakan White's heteroscedasticity-consistent standard errors\n")
            cat("   • HC0, HC1, HC2, atau HC3 corrections\n\n")
            cat("3. WEIGHTED LEAST SQUARES (WLS):\n")
            cat("   • Berikan bobot inversely proportional terhadap varian\n\n")
            cat("4. MODEL SPECIFICATION:\n")
            cat("   • Tambah variabel independen yang relevan\n")
            cat("   • Pertimbangkan interaksi atau polynomial terms\n")
          }

          if(abs(abs_resid_fitted_cor) > 0.3) {
            cat("\nCATATAN TAMBAHAN:\n")
            cat("Korelasi antara nilai absolut residual dengan fitted values (",
                round(abs_resid_fitted_cor, 4), ") menunjukkan adanya pola heteroskedastisitas.\n")
          }
        })
      }

      temp_file <- create_professional_docx("UJI HOMOSKEDASTISITAS", content, file, alpha)
      file.copy(temp_file, file, overwrite = TRUE)
    }
  )
  output$downloadAutokorelasi <- downloadHandler(
    filename = function() {
      paste("Laporan_Uji_Autokorelasi_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      alpha <- if(exists("input$alpha")) input$alpha else 0.05

      if(!mlr_results$run_analysis) {
        content <- "PERHATIAN: Analisis regresi linear berganda belum dijalankan.\nSilakan jalankan analisis regresi terlebih dahulu untuk melakukan uji autokorelasi."
      } else {
        req(mlr_model())
        model <- mlr_model()

        content <- capture_output({
          cat("═════════════════════════════════════════\n")
          cat("         UJI AUTOKORELASI - REGRESI LINEAR BERGANDA               \n")
          cat("═════════════════════════════════════════\n\n")

          cat("INFORMASI ANALISIS:\n")
          cat("• Model regresi           : ", deparse(formula(model)), "\n")
          cat("• Jenis uji               : Durbin-Watson Test\n")
          cat("• Tingkat signifikansi    : α = ", alpha, "\n\n")

          cat("PENTINGNYA UJI AUTOKORELASI:\n")
          cat("Autokorelasi terjadi ketika residual pada observasi berurutan saling berkorelasi.\n")
          cat("Hal ini umum terjadi pada data time series dan dapat membuat standard error\n")
          cat("estimasi bias serta uji signifikansi tidak akurat.\n\n")

          cat("HIPOTESIS UJI:\n")
          cat("• H₀ (Hipotesis Nol)      : Tidak ada autokorelasi (residual independen)\n")
          cat("• H₁ (Hipotesis Alternatif): Ada autokorelasi dalam residual\n\n")
          dw_test <- lmtest::dwtest(model)
          dw_statistic <- dw_test$statistic

          cat("HASIL UJI DURBIN-WATSON:\n")
          cat("────────────────────────────────────────────────────────────────────────\n")
          print(dw_test)
          cat("\n")

          cat("INTERPRETASI NILAI DURBIN-WATSON:\n")
          cat("────────────────────────────────────────────────────────────────────────\n")
          cat("• Nilai DW statistic      : ", round(dw_statistic, 4), "\n")

          if(dw_statistic < 1.5) {
            dw_interpretation <- "AUTOKORELASI POSITIF KUAT"
            dw_meaning <- "Residual berturut-turut cenderung memiliki nilai sama (positif-positif atau negatif-negatif)"
          } else if(dw_statistic < 2.0) {
            dw_interpretation <- "KEMUNGKINAN AUTOKORELASI POSITIF"
            dw_meaning <- "Indikasi lemah adanya autokorelasi positif"
          } else if(dw_statistic <= 2.5) {
            dw_interpretation <- "TIDAK ADA AUTOKORELASI"
            dw_meaning <- "Residual independen (ideal untuk regresi linear)"
          } else if(dw_statistic < 3.0) {
            dw_interpretation <- "KEMUNGKINAN AUTOKORELASI NEGATIF"
            dw_meaning <- "Indikasi lemah adanya autokorelasi negatif"
          } else {
            dw_interpretation <- "AUTOKORELASI NEGATIF KUAT"
            dw_meaning <- "Residual berturut-turut cenderung berlawanan tanda"
          }

          cat("• Interpretasi DW         : ", dw_interpretation, "\n")
          cat("• Penjelasan              : ", dw_meaning, "\n\n")

          cat("KRITERIA DURBIN-WATSON:\n")
          cat("• DW ≈ 2.0    : Tidak ada autokorelasi (IDEAL)\n")
          cat("• DW < 2.0    : Indikasi autokorelasi positif\n")
          cat("• DW > 2.0    : Indikasi autokorelasi negatif\n")
          cat("• DW < 1.0    : Autokorelasi positif KUAT\n")
          cat("• DW > 3.0    : Autokorelasi negatif KUAT\n\n")

          cat("HASIL UJI SIGNIFIKANSI:\n")
          cat("────────────────────────────────────────────────────────────────────────\n")
          interpretation <- interpret_p_value(dw_test$p.value, alpha, "autokorelasi Durbin-Watson")
          cat(interpretation, "\n\n")
          residuals_data <- residuals(model)
          n_obs <- length(residuals_data)

          if(n_obs > 1) {
            lag1_correlation <- cor(residuals_data[-1], residuals_data[-n_obs])
            cat("STATISTIK DIAGNOSTIK TAMBAHAN:\n")
            cat("────────────────────────────────────────────────────────────────────────\n")
            cat("• Korelasi lag-1 residual : ", round(lag1_correlation, 4), "\n")

            if(abs(lag1_correlation) > 0.3) {
              cat("• Indikasi                : Korelasi serial cukup tinggi\n")
            } else {
              cat("• Indikasi                : Korelasi serial rendah\n")
            }
            cat("\n")
          }

          if(dw_test$p.value > alpha && abs(dw_statistic - 2) < 0.5) {
            cat("KESIMPULAN:\n")
            cat("Berdasarkan hasil uji Durbin-Watson dengan tingkat kepercayaan ",
                (100-alpha*100), "%, dapat disimpulkan bahwa model regresi TIDAK mengalami ",
                "masalah autokorelasi.\n\n")

            cat("IMPLIKASI POSITIF:\n")
            cat("• ✓ Asumsi independensi residual TERPENUHI\n")
            cat("• ✓ Standard error estimasi akurat\n")
            cat("• ✓ Uji signifikansi reliable\n")
            cat("• ✓ Interval kepercayaan valid\n")
            cat("• ✓ Model sesuai untuk data cross-sectional\n")

          } else {
            cat("KESIMPULAN:\n")
            cat("Berdasarkan hasil uji Durbin-Watson dengan tingkat kepercayaan ",
                (100-alpha*100), "%, dapat disimpulkan bahwa model regresi mengalami ",
                "masalah AUTOKORELASI.\n\n")

            cat("IMPLIKASI NEGATIF:\n")
            cat("• ✗ Asumsi independensi residual TIDAK TERPENUHI\n")
            cat("• ✗ Standard error estimasi bias (biasanya underestimated)\n")
            cat("• ✗ Uji signifikansi tidak akurat\n")
            cat("• ✗ Interval kepercayaan tidak reliable\n")
            cat("• ✗ Efisiensi estimator OLS suboptimal\n\n")

            cat("SARAN PERBAIKAN:\n")
            if(dw_statistic < 2.0) {
              cat("UNTUK AUTOKORELASI POSITIF:\n")
            } else {
              cat("UNTUK AUTOKORELASI NEGATIF:\n")
            }
            cat("1. TRANSFORMASI MODEL:\n")
            cat("   • Tambahkan lag variabel dependen sebagai regressor\n")
            cat("   • Gunakan first-difference transformation\n")
            cat("   • Pertimbangkan Cochrane-Orcutt procedure\n\n")
            cat("2. SPESIFIKASI MODEL:\n")
            cat("   • Tambah variabel independen yang relevan\n")
            cat("   • Pertimbangkan interaksi temporal\n")
            cat("   • Evaluasi functional form model\n\n")
            cat("3. ROBUST ESTIMATION:\n")
            cat("   • Gunakan Newey-West standard errors\n")
            cat("   • HAC (Heteroscedasticity and Autocorrelation Consistent) estimators\n\n")
            cat("4. TIME SERIES METHODS:\n")
            cat("   • ARIMA modeling\n")
            cat("   • Vector Autoregression (VAR)\n")
            cat("   • Error Correction Models (ECM)\n")
          }
        })
      }

      temp_file <- create_professional_docx("UJI AUTOKORELASI", content, file, alpha)
      file.copy(temp_file, file, overwrite = TRUE)
    }
  )


}

shinyApp(ui, server)
