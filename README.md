# Dashboard CLEAR-IN

**Comprehensive Local Exploratory Analysis and Reporting Interface for Indonesia**

Platform analisis statistik terintegrasi untuk data kerentanan sosial di tingkat kabupaten/kota seluruh Indonesia.

## Deskripsi

Dashboard CLEAR-IN adalah aplikasi web interaktif berbasis R Shiny yang dikembangkan untuk memenuhi tugas Ujian Akhir Semester untuk mata kuliah Komputasi Statistik. Selain itu, juga berfungsi untuk memfasilitasi analisis mendalam terhadap Social Vulnerability Index (SOVI) Indonesia. Platform ini menyediakan tools komprehensif untuk eksplorasi data, analisis statistik, dan visualisasi geospasial.

## Fitur Utama

### Manajemen Data
- Preview dataset dalam format tabel interaktif
- Ringkasan statistik deskriptif otomatis
- Fitur validasi dan seleksi data
- Transformasi data kontinyu menjadi kategorik


### Eksplorasi Data
- Visualisasi geospasial menggunakan Leaflet
- Scatter plot interaktif untuk analisis


### Analisis Statistik
- **Uji Asumsi**: Normalitas, homogenitas dengan berbagai metode
- **Resampling**: Bootstrap, Jackknife, Permutation testing
- **Inferensia**: Uji t, uji proporsi, ANOVA, uji ragam
- **Regresi**: Multiple linear regression dengan diagnostik lengkap

### Visualisasi
- Peta interaktif dengan Leaflet
- Grafik diagnostik (histogram, Q-Q plot, boxplot)

## Dataset

Dashboard mengintegrasikan tiga sumber data utama:

1. **Data SOVI** - Social Vulnerability Index tingkat kabupaten/kota
2. **Data MDS (Matrix Penimbang)** - Hasil Multidimensional Scaling untuk visualisasi pola
3. **Data Geospasial** - Batas administratif kabupaten/kota Indonesia (GeoJSON)

## Struktur Aplikasi

```
dashboard/
├── app.R              
├── .RData              
├── manifest.json              
├── UAS_Komstat.Rproj              
├── data/
│   ├── sovi_data.csv
│   ├── mds_results.csv
│   ├── distance.csv
│   └── 38 Provinsi indonesia - Kabupaten.json
├── rsconnect/shinyapps.io/raihantz
│   └── uas-komstat.dcf
└──
```

## Instalasi dan Penggunaan

### Prerequisites

```r
# Install required packages
install.packages(c(
  "shiny", "shinydashboard", "DT", "leaflet", 
  "plotly", "ggplot2", "dplyr", "sf", "readr",
  "nortest", "car", "stats", "RcmdrMisc", "boot", 
  "tidyr", "gridExtra", "e1071", "lmtest", "officer",
  "flextable", "nortest", "tseries", "moments", "rsconnect"
))
```

### Menjalankan Aplikasi

```r
git clone https://github.com/hanhanano/UAS_Komstat.git
setwd("UAS_Komstat")
shiny::runApp()
```

## Cara Penggunaan

1. **Beranda**: Baca panduan penggunaan dan informasi dataset
2. **Manajemen Data**: Preview dan validasi data yang akan dianalisis
3. **Eksplorasi Data**: Eksplorasi visual melalui peta dan scatter plot
4. **Uji Asumsi**: Verifikasi asumsi statistik sebelum analisis lanjutan
5. **Resampling**: Aplikasikan metode resampling untuk estimasi robust
6. **Statistik Inferensia**: Lakukan pengujian hipotesis sesuai kebutuhan
7. **Regresi**: Bangun dan evaluasi model prediktif

## Fitur Export

- Cetak semua tab dalam satu dokumen PDF
- Cetak tab aktif untuk dokumentasi spesifik
- Export hasil analisis dalam format yang dapat direproduksi

## Lisensi

Proyek ini dikembangkan untuk kepentingan pendidikan.

**Catatan**: Dashboard ini merupakan tools penelitian. Hasil analisis harus diinterpretasikan dengan mempertimbangkan konteks dan keterbatasan data.