
header <- dashboardHeader(
  title = "Woman in Indonesia"
)

sidebar <- dashboardSidebar(
  collapsed = F,
  sidebarMenu(
    menuItem(
      text = "Overview",
      tabName = "Overview",
      icon = icon("globe-asia")
    ),
    menuItem(
      text = "Pendidikan",
      tabName = "Pendidikan",
      icon = icon("book")
    ),
    menuItem(
      text = "Pekerjaan",
      tabName = "Pekerjaan",
      icon = icon("book")
    ), 
    menuItem("Source Code", icon = icon("file-code-o"), 
             href = "https://github.com/saramoniica/pendidikan-kesempatan-kerja/")
  )
)

body <- dashboardBody(
  
  shinyDashboardThemes(
    theme = "grey_light"
  ),
  
  tabItems(
    
    # TAB 1  
    
    tabItem(
      tabName = "Overview",
      fluidPage(
        h2(tags$b("Pendidikan dan Kesempatan Kerja Perempuan di Indonesia")),
        br(),
        div(style = "text-align:justify", 
            p("Pada tahun 2021, perempuan Indonesia bersekolah rata-rata 8,17 tahun dan menyelesaikan pendidikan formal hingga tahun kedua sekolah menengah pertama. 
              Hingga 65% wilayah Indonesia berada di atas rata-rata angka partisipasi perempuan di tingkat nasional, 
              yang sebagian besar berada di bagian barat Indonesia. Sebaliknya, Indonesia bagian timur didominasi oleh daerah 
              dengan rata-rata lama sekolah yang rendah untuk para perempuan. 
              DKI Jakarta adalah salah satu dari lima daerah dengan rata-rata angka partisipasi sekolah tertinggi untuk anak perempuan, diikuti 
              dengan Kepulauan Riau, Kepulauan Maluku, Sulawesi Utara, dan Kalimantan Timur. 
              Lima daerah dengan rata-rata lama sekolah perempuan tersingkat adalah Papua, Nusa Tenggara Barat, Kalimantan Barat, 
              Nusa Tenggara Timur, dan Jawa Tengah."),
            br()
        )
      ),
      fluidPage(
        box(width = 8,
            title = "Perbandingan Antara Angka Harapan dan Rata-Rata Lama Sekolah berdasarkan Provinsi ",
            plotlyOutput("plothr")
        ),
        valueBox(width = 4,
                 value = " DI Yogyakarta : 15.64", 
                 "Angka Harapan Lama Sekolah Tertinggi", 
                 icon = icon("city"),
                 color = "blue"),
        valueBox(width = 4,
                 "DKI Jakarta : 10.83	",
                 "Rata-rata Lama Sekolah Tertinggi",
                 icon = icon("book"),
                 color = "teal")
      ),
      fluidPage(
        box(width = 8,
            title = "Persentase Pengangguran Berdasarkan Tingkat Pengangguran pada Tahun 2021",
            plotlyOutput("plot2")
            ),
        box(width = 4,
            height = 450,
            h3("Perserntase Tingat Pengangguran Terbuka Berdasarkan Jenjang Pendidikan"),
            div(style = "text-align:justify",
                p("Grafik disamping menunjukkan persentase pengangguran terbuka berdasarkan jenjang pendidikan. Berdasarkan data yang telah diambil tiga teratas 
                  diduduki oleh SMK atau SMA Kejuruan, diikuti dengan SMA Umum serta SMP, sedangkan grafik terendah ialah Tidak Lulus/Belum Pernah/Tamat SD kemungkinan
                  besar hal ini dikarenakan belum cukupnya umur untuk mencari kerja dan tidak masuk kedalam kategori usia kerja"),
                br(),
                p("Source :", a(href = "https://www.https://www.bps.go.id/",
                    "Badan Pusat Statistik"))
            )
          )
        )
    ),
    
    # TAB 2
    
    tabItem(
      tabName = "Pendidikan",
      fluidPage(
        tabBox(width = 9,
          title = tags$b("Perbandingan Harapan dan Realisasi Lama Sekolah"),
          id = "tabset1",
          side = "right",
          tabPanel(tags$b("Rata-Rata Lama Sekolah"),
            plotlyOutput("plotrls")),
          tabPanel(tags$b("Harapan Lama Sekolah"), 
               plotlyOutput("plothls"))
          ),
      box(width = 3, selectInput( inputId = "input_tahun", label = "Pilih Tahun - Plot Harapan dan Rata-rata Lama Sekolah", choices = unique(hls_rls_clean$tahun))),
      fluidPage(
      box(width = 3, selectInput(inputId = "input_provinsi", label = "Pilih Provinsi", choices = unique(pdk$provinsi))),
      box(width = 3,selectInput(inputId = "input_jenis", label = "Pilih Tingkat Pendidikan", choices = unique(pdk$jenis))),
      box(width = 6,selectInput( inputId = "input_prov", label = "Pilih Provinsi - Plot Buta Huruf", choices = unique(b$provinsi))),
      box(width = 6,
            title = "Tingkat Penyelesaian Pendidikan pada Perempuan  di Setiap Provinsi",
            plotlyOutput("plotpdk")),
      box(width = 6,
          title = "Tingkat Penduduk dengan Jenis Kelamin Perempuan diatas 10 tahun yang Buta Huruf di Setiap Provinsi",
          plotlyOutput("plotbclean"))
        )
      )
    ),
    # TAB 3
    
    tabItem(
      tabName = "Pekerjaan",
      fluidPage(
        box(width = 6, selectInput( inputId = "input_ptp", label = "Pilih Jenjang Pendidikan", choices = unique(pt_pend$tingkat_pendidikan))),
        box(width = 6, checkboxGroupInput(
                        inputId = "ptk_ptb",
                        label = "Pilih Variabel",
                        choices = c("Perempuan" = "perempuan", "Total Keseluruhan" = "total_keseluruhan"),
                        selected = "Perempuan"))),
      fluidPage(
        box(width = 6,
            title = "Jumlah Pengangguran Terbuka menurut Tingkat Pendidikan",
            plotlyOutput("plotptp")),
        box(width = 6,
            title = "Jumlah Pengangguran Terbuka dengan Jenis Kelamin Perempuan",
            plotlyOutput("plotptk_ptb"))),
        fluidPage(
                 box(width = 12, selectInput(inputId = "input_provPP", label = "Pilih Provinsi", choices = unique(p_spp_prov$provinsi))),
                 tabBox(width = 12,
                        title = tags$b("Perbandingan"),
                        id = "tabset1",
                        side = "right",
                        tabPanel(tags$b("Tingkat Pengeluaran Pekapita Perempuan di Setiap Provinsi"),
                                 plotlyOutput("plotp_provinsi")),
                        tabPanel(tags$b("Tingkat Sumbangan Pendapatan Pekapita Perempuan di Setiap Provinsi"),
                                 plotlyOutput("plotspp"))))
          )
        )
      )


# Combining Dashboard Part
dashboardPage(
  header = header,
  body = body,
  sidebar = sidebar
)
