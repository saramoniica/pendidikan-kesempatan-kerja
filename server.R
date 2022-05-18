shinyServer(function(input, output) {

  ##-----Plot PT PAGE 2
  output$plot2 <-  renderPlotly({
    
    pt_pend_2021 <- pt_pend %>% filter(tahun == "2021")
    pt_pend_2021 <- pt_pend_2021 %>% mutate(label = glue("Tingkat Pengangguran Terbuka dalam Persen : {persen}"))

    plot2 <- ggplot(pt_pend_2021, mapping = aes(x = reorder(tingkat_pendidikan, persen), y = persen, fill = tingkat_pendidikan, group = 1, text = label)) +
      geom_bar(stat = "identity", show.legend = F) +
      scale_fill_viridis(discrete = T, option = "E") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      labs(title = NULL,
           x = NULL,
           y = NULL,
           fill = NULL) +
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 30, hjust = 1),
            plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "#ffffff"),
            axis.line.y = element_line(colour = "grey"),
            axis.line.x = element_blank(),
            panel.grid = element_blank()) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust=1), )
    
    ggplotly(plot2, tooltip = "text")
    
  })  
  ##-----Plot HR PAGE 1
  output$plothr <-  renderPlotly({
    
    hlsrlsarr <- hlsrlsarr %>% arrange(value) %>% mutate(label = glue("Provinsi : {provinsi}
                                                                   Angka (Tahun) : {value}"))
    
    plothr <- hlsrlsarr %>%  ggplot(mapping = aes(x= reorder(provinsi,value), y= value, name = variabel, group = variabel, fill = variabel)) +
      geom_area(position = "identity", colour="black", size=0.2, alpha=.2) +
      geom_point(aes(text = label)) +
      scale_fill_viridis(discrete = T, option = "E") +
      labs(title = NULL,
           x = NULL,
           y = NULL,
           fill = NULL) +
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 30, hjust = 1),
            plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "#ffffff"),
            axis.line.y = element_line(colour = "grey"),
            axis.line.x = element_blank(),
            panel.grid = element_blank()) +
      theme(legend.position="top", axis.text.x = element_text(angle = 45, hjust=1))
    
    ggplotly(plothr, tooltip = "text")
    
  })  
  
  ##-----Plot BCLEAN
  output$plotbclean <-  renderPlotly({
    
    b_clean <- b_clean %>% select(-c("nama_variabel", "jenis_kelamin")) %>%  filter(provinsi == input$input_prov) %>%mutate(label=glue(" Provinsi: {provinsi}
                                                                                          Total dalam Persen (%)  Persen (%)  Persen (%)  : {persen}"))

    plotbclean <- b_clean %>% ggplot(aes(x= tahun, y=reorder(persen, tahun), group=1)) +
      geom_area(alpha=.3) +
      geom_line() +
      geom_point(aes(text = label)) +
      scale_fill_viridis(discrete = TRUE, option = "E") +
      labs(title = NULL,
           x = NULL,
           y = NULL,
           fill = NULL) +
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 30, hjust = 1),
            plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "#ffffff"),
            axis.line.y = element_line(colour = "grey"),
            axis.line.x = element_blank(),
            panel.grid = element_blank()) +
      theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust=1))
    
   ggplotly(plotbclean, tooltip = "text")

  })
  
  ##-------- Plot PDK = 
  
  output$plotpdk <- renderPlotly({
    
    pdk <- pdk  %>% filter(provinsi == input$input_provinsi, jenis == input$input_jenis) %>%  mutate(label =glue("Tahun :{tahun}
                                                              Tingkat Pendidikan : {jenis}
                                                              Total dalam Persen (%)  Persen (%)  Persen (%)  : {persen}"))
    # Plot
    plotpdk <- pdk %>% ggplot(mapping = aes(x= tahun, y= reorder(persen, tahun), fill = jenis)) +
      geom_area(aes(group=1),position = "identity", colour="black", size=0.5, alpha=.3) +
      geom_point(aes(text = label)) +
      scale_fill_viridis(discrete = TRUE, option = "E") +
      labs(title = NULL,
           x = NULL,
           y = NULL,
           fill = NULL) +
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 30, hjust = 1),
            plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "#ffffff"),
            axis.line.y = element_line(colour = "grey"),
            axis.line.x = element_blank(),
            panel.grid = element_blank()) +
      theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust=1))
    
    ggplotly(plotpdk, tooltip = "text")
    
  })
  
  
  
  ##--------- Plot PTK_PTB = 
  output$plotptk_ptb <- renderPlotly({
    
  
    if(identical(input$ptk_ptb, "perempuan")) {
      ptk_ptb <- ptk_ptb %>% filter(variabel == "perempuan") %>% mutate(label=glue("Jenis Kelamin: {variabel}
                                                Total Orang (dalam Ribuan) : {value}"))
      plotptk_ptb <- ggplot(ptk_ptb, mapping = aes(x = tahun, y =value, group=variabel, color=variabel )) +
        geom_line(stat = "identity", show.legend = F) +
        geom_point(aes(text = label)) +
        scale_fill_viridis(discrete = T, option = "E") +
        labs(title = NULL,
             x = NULL,
             y = NULL,
             fill = NULL) +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(angle = 30, hjust = 1),
              plot.title = element_text(face = "bold"),
              panel.background = element_rect(fill = "#ffffff"),
              axis.line.y = element_line(colour = "grey"),
              axis.line.x = element_blank(),
              panel.grid = element_blank()) +
        theme(legend.position = "none", axis.text.x = element_text(angle = 0, hjust=1))
    } else {
      if(identical(input$ptk_ptb, "total_keseluruhan")) {
        ptk_ptb <- ptk_ptb %>% filter(variabel == "total_keseluruhan") %>% mutate(label=glue("Jenis Kelamin: {variabel}
                                                Total Orang (dalam Ribuan) : {value}"))
        plotptk_ptb <- ggplot(ptk_ptb, mapping = aes(x = tahun, y = reorder(value, tahun), group=variabel, color=variabel )) +
          geom_line(stat = "identity", show.legend = F) +
          geom_point(aes(text = label)) +
          scale_fill_viridis(discrete = T, option = "E") +
          labs(title = NULL,
               x = NULL,
               y = NULL,
               fill = NULL) +
          theme(legend.title = element_blank(),
                axis.text.x = element_text(angle = 30, hjust = 1),
                plot.title = element_text(face = "bold"),
                panel.background = element_rect(fill = "#ffffff"),
                axis.line.y = element_line(colour = "grey"),
                axis.line.x = element_blank(),
                panel.grid = element_blank()) +
          theme(legend.position = "none", axis.text.x = element_text(angle = 0, hjust=1))
      } else {
        ptk_ptb <- ptk_ptb %>% mutate(label=glue("Jenis Kelamin: {variabel}
                                                Total Orang (dalam Ribuan) : {value}"))
        plotptk_ptb <- ggplot(ptk_ptb, mapping = aes(x = tahun, y = reorder(value, tahun), group=variabel, color=variabel )) +
          geom_line(stat = "identity", show.legend = F) +
          geom_point(aes(text = label)) +
          scale_fill_viridis(discrete = T, option = "E") +
          labs(title = NULL,
               x = NULL,
               y = NULL,
               fill = NULL) +
          theme(legend.title = element_blank(),
                axis.text.x = element_text(angle = 30, hjust = 1),
                plot.title = element_text(face = "bold"),
                panel.background = element_rect(fill = "#ffffff"),
                axis.line.y = element_line(colour = "grey"),
                axis.line.x = element_blank(),
                panel.grid = element_blank()) +
          theme(legend.position = "none", axis.text.x = element_text(angle = 0, hjust=1))
      }}
    
    ggplotly(plotptk_ptb, tooltip="text")  
    
  })

  ##--------- Plot PTP = 
  output$plotptp <- renderPlotly ({
    pt_pend <- pt_pend %>% filter(tingkat_pendidikan == input$input_ptp) %>% mutate(label = glue(" Tingkat Pendidikan : {tingkat_pendidikan}
                                                                                                                Total dalam Persen (%)  Persen (%)  : {persen}"))
    
    plotptp <- ggplot(pt_pend, mapping = aes(x = tahun, y = persen, group=tingkat_pendidikan, color=tingkat_pendidikan )) +
      geom_line(color = "blue", stat = "identity", show.legend = F) +
      geom_point(color = "blue", aes(text = label)) +
      scale_fill_viridis(discrete = T, option = "E") +
      labs(title = NULL,
           x = NULL,
           y = NULL,
           fill = NULL) +
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 30, hjust = 1),
            plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "#ffffff"),
            axis.line.y = element_line(colour = "grey"),
            axis.line.x = element_blank(),
            panel.grid = element_blank()) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 0, hjust=1), )
    
    ggplotly(plotptp, tooltip="text")
  })

  ##--------- Plot P_Provinsi = 
  output$plotp_provinsi <- renderPlotly({
    
    p_spp_prov <- p_spp_prov %>% filter(provinsi == input$input_provPP) %>%  mutate(label =glue("Total (dalam Ribuan Rupiah) : {total}")) 
    
    plotp_provinsi <- p_spp_prov[p_spp_prov$nama_variabel == "Pengeluaran per Kapita",] %>% ggplot(aes(x= tahun, y=reorder(total, tahun), group=1, fill = provinsi)) +
      geom_col(alpha=.3) +
      geom_line() +
      geom_point(aes(text = label)) +
      scale_fill_viridis(discrete = TRUE, option = "E") +
      labs(title = NULL,
           x = NULL,
           y = NULL,
           fill = NULL) +
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 30, hjust = 1),
            plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "#ffffff"),
            axis.line.y = element_line(colour = "grey"),
            axis.line.x = element_blank(),
            panel.grid = element_blank()) +
      theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust=1))
    
    ggplotly(plotp_provinsi, tooltip="text")
  })
  
  ##--------- Plot SPP = 
  output$plotspp <- renderPlotly({
    
    p_spp_prov <- p_spp_prov %>% filter(provinsi == input$input_provPP) %>%  mutate(label =glue("Total (dalam persen) : {persen}")) 
    
    plotspp <- p_spp_prov[p_spp_prov$nama_variabel == "Sumbangan Pendapatan Perempuan",] %>% ggplot(aes(x= tahun, y=reorder(persen, tahun), group=1, fill = provinsi)) +
      geom_col(alpha=.3) +
      geom_line() +
      geom_point(aes(text = label)) +
      scale_fill_viridis(discrete = TRUE, option = "E") +
      labs(title = NULL,
           x = NULL,
           y = NULL,
           fill = NULL) +
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 30, hjust = 1),
            plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "#ffffff"),
            axis.line.y = element_line(colour = "grey"),
            axis.line.x = element_blank(),
            panel.grid = element_blank()) +
      theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust=1))
    
    ggplotly(plotspp, tooltip="text")
  })
  
  ##--------- Plot HLS=
  output$plothls <- renderPlotly({
    hls_clean <- hls_clean %>% filter(tahun == input$input_tahun) %>% 
    mutate(label =glue(" Kategori : {nama_variabel} 
    Lama(Tahun) : {angka}"))
    
    plothls <- ggplot(hls_clean) +
      geom_histogram(aes(x = provinsi, y = angka, fill=provinsi, text = label), 
               stat = "identity", position = "dodge", show.legend = F) +
      scale_fill_viridis(discrete = T, option = "E") +
      scale_y_continuous(breaks = seq(5,35,5), limits=c(0,20),
                         expand = expansion(mult = c(0.001, 0.001)),
                         sec.axis = dup_axis(name = NULL, labels = NULL)) +
      scale_x_discrete(expand = expansion(mult = c(0.0001, 0.0001)))+
      labs(title = NULL,
           x = NULL,
           y = NULL,
           fill = NULL) +
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 30, hjust = 1),
            plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "#ffffff"),
            axis.line.y = element_line(colour = "grey"),
            axis.line.x = element_blank(),
            panel.grid = element_blank()) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 60, hjust=1), )
    
    ggplotly(plothls, tooltip="text")
  })
  
    ##--------- Plot RLS=
    output$plotrls <- renderPlotly({
      rls_clean <- rls_clean %>% filter(tahun == input$input_tahun) %>% 
        mutate(label =glue(" Kategori : {nama_variabel} 
                            Lama(Tahun) : {angka}"))
      
      plotrls <- ggplot(rls_clean) +
        geom_histogram(aes(x = provinsi, y = angka, fill=provinsi, text = label), 
                       stat = "identity", position = "dodge", show.legend = F) +
        scale_fill_viridis(discrete = T, option = "E") +
        scale_y_continuous(breaks = seq(5,35,5), limits=c(0,20),
                           expand = expansion(mult = c(0.001, 0.001)),
                           sec.axis = dup_axis(name = NULL, labels = NULL)) +
        scale_x_discrete(expand = expansion(mult = c(0.0001, 0.0001)))+
        labs(title = NULL,
             x = NULL,
             y = NULL,
             fill = NULL) +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(angle = 30, hjust = 1),
              plot.title = element_text(face = "bold"),
              panel.background = element_rect(fill = "#ffffff"),
              axis.line.y = element_line(colour = "grey"),
              axis.line.x = element_blank(),
              panel.grid = element_blank()) +
        theme(legend.position = "none", axis.text.x = element_text(angle = 60, hjust=1), )
      
      ggplotly(plotrls, tooltip="text")
    
})
})
