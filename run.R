# load packages
library(shiny)
library(tidyverse)
library(readxl)
library(DT)
library(data.table)

# display chinese
Sys.setlocale("LC_ALL","Chinese")
options(shiny.usecairo = FALSE)
font_home <- function(path = '') file.path('~', '.fonts', path)
if (Sys.info()[['sysname']] == 'Linux' &&
    system('locate wqy-zenhei.ttc') != 0 &&
    !file.exists(font_home('wqy-zenhei.ttc'))) {
  if (!file.exists('wqy-zenhei.ttc'))
    shiny:::download(
      'https://github.com/rstudio/shiny-examples/releases/download/v0.10.1/wqy-zenhei.ttc',
      'wqy-zenhei.ttc'
    )
  dir.create(font_home())
  file.copy('wqy-zenhei.ttc', font_home())
  system2('fc-cache', paste('-f', font_home()))
}
rm(font_home)

# load data
outbreak_trace <- read_rds("outbreak_trace.rds")
stages <- c("S", "E", "Ip","Io", "Iu", "Ro_pos", "Ru_pos","R_neg")
age_dist <- read_rds("age_dist.rds")
age_groups <- paste(seq(0, 75, 5),seq(0, 75, 5)+4, sep="-"); age_groups[16] <- "75+"
nage <- length(age_groups)
nepi <- length(stages)
age_dist %>% 
  group_by(CNTY_CODE) %>% 
  summarise(tot = sum(tot)) -> pop
op <- as.list(pop$CNTY_CODE %>% unique)
names(op) <- pop$CNTY_CODE %>% unique
R0sample <- seq(1,5,0.5)
nruns <- length(R0sample)
prf_ref <- read_excel("cnty_reference.xlsx") %>% mutate(Location = gsub("\u00A0","",Location))
seir_tab <- data.table("comp" = stages,
                       "Chinese" = c("易感人群",
                                     "暴露人群",
                                     "已感染，将产生但尚未产生症状人群",
                                     "已发生症状人群",
                                     "无症状人群",
                                     "传染性已消失但核酸持续阳性之有症状感染者",
                                     "传染性已消失但核酸持续阳性之无症症状感染者",
                                     "完全康复人群（无传染性且核酸阴性）"),
                       "English" = c("Susceptible",
                                     "Exposed",
                                     "Presymptomatic",
                                     "Symptomatic",
                                     "Asymptomatic",
                                     "Removed individuals from symptomatic infections who remain PCR+",
                                     "Removed individuals from asymptomatic infections who remain PCR+",
                                     "Removed individuals who are PCR-")) %>% 
  setNames(c("Compartment | 人群分类",
             "中文",
             "English"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(HTML("COVID-19监测水平与暴发规模<br/>COVID-19 Testing Intensity and Outbreak Size")),
  tags$meta(name = "author", content = "Yang Liu"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("CNTY",
                  HTML("行政区划编码：<br/>Prefecture code:"),
                  choices = op,
                  selected = 110100),
      selectInput("R0",
                  HTML("即时再生数：<br/>Instantaneous reproduction number:"),
                  choices = as.list(seq(1,5,0.5)) %>% setNames(seq(1,5,0.5)),
                  selected = 2),
      numericInput("prop.fc",
                   HTML("符合发热症状患者占总人口比(0-1):<br/> Baseline population at fever clinics (0-1):"),
                   value = 0.03,
                   min = 0,
                   max = 0.2,
                   step = 0.01),
      numericInput("n.fc",
                   HTML("发热门诊日接诊率(每千人):<br/> Fever clinics service capacity (per 1000 residents):"),
                   value = 0.07,
                   min = 0,
                   max = 0.2,
                   step = 0.01),
      sliderInput("testr.fc",
                  HTML("发热门诊日检测率(0-1)：<br/> Fever clinics daily testing proportion (0-1):"),
                  min = 0,
                  max = 1,
                  step = 0.1,
                  value = 0.5),
      numericInput("prop.resp",
                   HTML("呼吸道疾病患者占总人口比(0-1):<br/> Baseline population at respiratory departments (0-1):"),
                   value = 0.1,
                   min = 0,
                   max = 1,
                   step = 0.1),
      numericInput("n.resp",
                   HTML("呼吸科日接诊率(每千人):<br/> Respiratory departments service capacity (per 1000 residents):"),
                   value = 1.35,
                   min = 0,
                   max = 3,
                   step = 0.05),
      sliderInput("testr.resp",
                  HTML("呼吸科日检测率(0-1)：<br/> Respiratory departments daily testing proportion (0-1):"),
                  min = 0,
                  max = 1,
                  value = 0.1,
                  step = 0.05),
      sliderInput("testr.all",
                  HTML("普通人群日检测率(0-0.01)：<br/> General population daily testing proportion (0-1):"),
                  min = 0,
                  max = 0.01,
                  value = 0),
      sliderInput("sen.sym",
                  HTML("有症状人群PCR敏感性(0-1)：<br/> PCR sensitivity among symptomatic individuals (0-1):"),
                  min = 0,
                  max = 1,
                  value = 0.7,
                  step = 0.1),
      sliderInput("sen.asym",
                  HTML("无症状人群PCR敏感性(0-1)：<br/> PCR sensitivity among individuals without symptoms (0-1):"),
                  min = 0,
                  max = 1,
                  value = 0.5,
                  step = 0.1)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          title = "背景 | Background",
          h4(shiny::includeMarkdown("description.md")),
        ),
        tabPanel(
          title = "人口结构 | Age Structure",
          plotOutput("age_dist")
        ),
        tabPanel(
          title = "暴发历史 | Outbreak History",
          plotOutput("history"),
          dataTableOutput("seir_tab")
        ),
        tabPanel(
          title = "监测方案 | Surveillance Strategy",
          plotOutput("strategy"),
          h4(textOutput("strategy_cn")),
          br(),
          h4(textOutput("strategy_en"))
        ),
        tabPanel(
          title = "监测效果 | Detection Capacity",
          plotOutput("detect"),
          br(),
          h4(textOutput("detect_description_cn")),
          br(),
          h4(textOutput("detect_description_en")),
          br(),
          plotOutput("effect"),
          h4(textOutput("effect_description_cn")),
          br(),
          h4(textOutput("effect_description_en"))
          # verbatimTextOutput("stats")
        )
      )
    )
  )
)

server <- function(input, output) {
  CNTY_tmp <- reactive({input$CNTY})
  age_dist_tmp <- reactive({age_dist %>% filter(CNTY_CODE == CNTY_tmp()) %>% pull(tot)})
  pop_tmp <- reactive({pop %>% filter(CNTY_CODE == CNTY_tmp()) %>% pull(tot) %>% as.integer})
  title_tmp <- reactive({prf_ref %>% 
      filter(CNTY_CODE == CNTY_tmp()) %>% 
      .$Location %>% 
      enc2utf8()})
  byComp_tmp <- reactive({lapply(1:nepi, function(y){
    sapply(1:nruns, function(x)   as.matrix(outbreak_trace[,,x] * rep(age_dist_tmp(), each = nepi)) %>% 
             .[seq(y,
                   nepi*nage,
                   nepi),] %>%
             colSums()) %>%
      data.frame() %>% 
      rownames_to_column(var = "t") %>% 
      pivot_longer(cols = starts_with("X"),
                   names_to = "Rt") %>% 
      mutate(Rt = parse_number(Rt),
             Rt = R0sample[Rt],
             t = as.numeric(t)) 
  }) %>% 
      setNames(stages) %>% 
      bind_rows(.id = "comp") %>% 
      mutate(comp = factor(comp, levels = stages))})
  R0_tmp <- reactive({input$R0})
  
  # parameters for fever clinics
  prop.fc_tmp <- reactive({as.numeric(input$prop.fc)})
  n.fc_tmp <- reactive({as.numeric(input$n.fc)})
  testr.fc_tmp <- reactive({as.numeric(input$testr.fc)})
  test.fc_tmp <- reactive({round(pop_tmp()*n.fc_tmp()*testr.fc_tmp()/1000)})
  
  # parameters for respiratory departments
  prop.resp_tmp <- reactive({input$prop.resp})
  n.resp_tmp <- reactive({input$n.resp})
  testr.resp_tmp <- reactive({input$testr.resp})
  test.resp_tmp <- reactive({round(pop_tmp()*n.resp_tmp()*testr.resp_tmp()/1000)})
  
  # parameters respiratory 
  testr.all_tmp <- reactive({input$testr.all})
  test.all_tmp <- reactive({pop_tmp()*testr.all_tmp()})
  
  strategy_cn <- reactive({paste0("在",
                                  title_tmp(),
                                  "，目前假设要求",
                                  testr.fc_tmp()*100,
                                  "%的发热门诊病人接受核酸检测；",
                                  testr.resp_tmp()*100,
                                  "%的呼吸科病人接受核酸检测。",
                                  "在假设的医疗系统当中，发热门诊每日接诊量为",
                                  round(n.fc_tmp()*pop_tmp()/1000),
                                  "人，呼吸科每日接诊量为",
                                  round(n.resp_tmp()*pop_tmp()/1000),
                                  "人。在这种假设之下",
                                  title_tmp(),
                                  "每日所需核酸试剂总数为",
                                  round(test.fc_tmp() + test.resp_tmp() + test.all_tmp()),
                                  "，其中发热门诊占",
                                  test.fc_tmp(),
                                  "支，占比",
                                  round(test.fc_tmp()*100/(test.fc_tmp() + test.resp_tmp() + test.all_tmp())),
                                  "%；呼吸科占",
                                  test.resp_tmp(),
                                  "支，占比",
                                  round(test.resp_tmp()*100/(test.fc_tmp() + test.resp_tmp() + test.all_tmp())),
                                  "%；普通人群占",
                                  test.all_tmp(),
                                  "支，占比",
                                  round(test.all_tmp()*100/(test.fc_tmp() + test.resp_tmp() + test.all_tmp())),
                                  "%。\r\n"
                                  )})
  
  strategy_en <- reactive({paste0("In ",
                                  title_tmp(),
                                  " (prefecture name), current input assumes ",
                                  testr.fc_tmp()*100,
                                  "% fever clinic patients will receive PCR tests; ",
                                  testr.resp_tmp()*100,
                                  "% respiratory department patients will receive PCR tests.",
                                  " In the hypothetical healthcare system, fever clinics receive ",
                                  round(n.fc_tmp()*pop_tmp()/1000),
                                  "patients per day, and respiratory departments receive ",
                                  round(n.resp_tmp()*pop_tmp()/1000),
                                  " patients per day. Given these inputs, ",
                                  title_tmp(),
                                  " (prefecture name) needs a total of ",
                                  round(test.fc_tmp() + test.resp_tmp() + test.all_tmp()),
                                  "COVID-19 PCR testing kits daily, among which ",
                                  test.fc_tmp(),
                                  " are consumed at fever clinics, accounting for ",
                                  round(test.fc_tmp()*100/(test.fc_tmp() + test.resp_tmp() + test.all_tmp())),
                                  "%; ",
                                  test.resp_tmp(),
                                  " are consumed at respiratory clinics, accounting for ",
                                  round(test.resp_tmp()*100/(test.fc_tmp() + test.resp_tmp() + test.all_tmp())),
                                  "%; ",
                                  test.all_tmp(),
                                  " are consumed by the general population, accounting for ",
                                  round(test.all_tmp()*100/(test.fc_tmp() + test.resp_tmp() + test.all_tmp())),
                                  "%。"
  )})
  
  # probability of detection
  p.undetect <- reactive({
    byComp_tmp() %>% 
      filter(Rt == R0_tmp(),
             t <= 50) %>% 
      dplyr::select(comp, t, value) %>% 
      pivot_wider(names_from = comp,
                  values_from = value) %>% 
      mutate(sym = Io,
             asym = Ip + Iu + Ro_pos + Ru_pos,
             inc =  pop_tmp() - S)%>% 
      dplyr::select(t, sym, asym, inc) %>% 
      mutate(n.ILI = pop_tmp() * prop.fc_tmp(),
             n.res = pop_tmp() * prop.resp_tmp(),
             p.undetect.fc = phyper(q = 0.1,
                                    m = sym,
                                    n = n.ILI,
                                    k = test.fc_tmp()),
             p.undetect.rd = phyper(q = 0.1,
                                    m = sym,
                                    n = n.res,
                                    k = test.resp_tmp()),
             p.undetect.all = phyper(q = 0.1,
                                     m = sym + asym,
                                     n = pop_tmp() - sym - asym,
                                     k = test.all_tmp()),
             p.undetect = p.undetect.fc * p.undetect.rd * p.undetect.all)
  })
  
  # scope of epidemics
  scope <- reactive({
    data.frame("t" = sapply(1:1000, 
                            function(x) {which(rbinom(50,1, 1-(p.undetect() %>% 
                                                                 pull(p.undetect))) == 1) %>% 
                                min})) %>% 
      left_join(p.undetect() %>% dplyr::select(t, inc), by = "t")
  })
  
  output$age_dist <- renderPlot({
    cbind(age_groups, age_dist_tmp()) %>% 
      data.frame %>% 
      setNames(c("AG","AD")) %>% 
      mutate(AD = as.numeric(AD),
             AG = factor(AG,levels = age_groups)) %>% 
      ggplot(., aes(x = AG,
                    y = AD)) +
      geom_col() +
      labs(title = title_tmp(),
           x = "年龄组 | Age Groups",
           y = "分年龄组人口数 | Population Size",
           caption = paste("总人口数 | Total Population:", sum(age_dist_tmp()))) +
      theme_bw() +
      theme(plot.title = element_text(size = 25),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.caption = element_text(size =15))
  })
  
  output$history <- renderPlot({
    byComp_tmp() %>% 
      filter(Rt == R0_tmp()) %>% 
      ggplot(., aes(x = t,
                    y = value,
                    group = Rt),
             environment = environment()) +
      geom_line() +
      facet_wrap(~comp, ncol = 4) +
      theme_bw() +
      labs(x = "时间（天） | Time (day)",
           y = "人数 | Counts") +
      theme(strip.text = element_text(size = 20),
            strip.background = element_rect(fill = NA),
            panel.grid = element_blank(),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 15))
  })
  
  output$seir_tab <- renderDataTable({
    seir_tab
  })
  
  output$strategy <- renderPlot({
    data.frame(
      setting = c("发热门诊 | Fever Clinics",
                  "呼吸科 | Respiratory Departments",
                  "普通人群 | General Population"),
      counts = c(test.fc_tmp(),
                 test.resp_tmp(),
                 test.all_tmp())) %>% 
      ggplot(., aes(x = "",
                    y = counts,
                    fill = setting))+
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Blues") + 
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            legend.position = "bottom",
            panel.border = element_blank(),
            panel.background = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank(),
            plot.caption = element_text(size = 20),
            plot.title=element_text(size=20, face="bold")) +
      labs(title = paste("每日所需核酸试剂总数 | Total Tests Needed Daily:", round(test.fc_tmp() + test.resp_tmp() + test.all_tmp())),
           fill = "场景 | Settings: ")
  })
  
  output$strategy_cn <- renderText({strategy_cn()})
  output$strategy_en <- renderText({strategy_en()})
  
  output$detect <- renderPlot({
    p.undetect() %>% 
      ggplot(., aes(x = t,
                    y = (1 - p.undetect)*100)) +
      geom_line(size = 1.5) +
      theme_bw() +
      theme(axis.title = element_text(size = 20),
            axis.text = element_text(size = 20),
            plot.title = element_text(size = 20),
            panel.grid = element_blank()) + 
      labs(x = "由第一例COVID-19为起点经历天数 \n Days elapsed since the infection introduction",
           title = "每日监测到COVID-19病例的概率 \n Probability of COVID-19 Detection on day t",
           y = "概率(%)")
  })
  
  output$detect_description_cn <- renderText({
    paste0("本应用仅考虑干预措施不变的情况。如上图，随着时间的推移，COVID-19在人群中传播规模逐渐增大 - 监测系统能监测到新发病例的可能性也随之变大。首发病例后第十天能监测到COVID-19传播的概率是",round((1-p.undetect()$p.undetect[10])*100,2),"%；首发病例后第二十天能监测到COVID-19传播的概率是",round((1-p.undetect()$p.undetect[20])*100,2),"%；首发病例后第三十天能监测到COVID-19传播的概率是",round((1-p.undetect()$p.undetect[30])*100,2),"%。In this application, ")
  })
  
  output$detect_description_en <- renderText({
    paste0("This application only considers the unmitigated condition. As shown above, as the outbreak progresses, the epidemic size increases - the surveillance system can become better at detecting new COVID-19 cases. The probability of detection on day 10 after the first case is ",round((1-p.undetect()$p.undetect[10])*100,2)," %; on day 20 after the first case is ",round((1-p.undetect()$p.undetect[20])*100,2),"%; on day 30 after the first case is ",round((1-p.undetect()$p.undetect[30])*100,2),"%.")
  })
  
  
  output$effect <- renderPlot({# 
    # p.undetect() %>% pull(p.undetect)
    metric1 <- scope()$inc %>% mean
    metric2 <- scope()$inc %>% median
    
    scope() %>% 
      ggplot(., aes(x = inc)) +
      geom_density(size = 1.5,
                   adjust = 1.5) +
      xlim(0,10000) +
      labs(x = "预计暴发规模（例） | Expected Epidemic size (cases)",
           y = "概率分布 | Probability density",
           title = "监测到第一例新发病例时预计爆发规模 | Epidemic size by the time of first COVID-19 detection",
           caption = "红色 - 平均值； 橙色 - 中位数 | Red - Mean; Orange - Median.") +
      theme_bw() +
      theme(axis.title = element_text(size = 20),
            axis.text = element_text(size = 20),
            plot.caption = element_text(size = 20),
            plot.title = element_text(size = 20)) +
      geom_vline(xintercept = c(metric1, metric2), 
                 color = c("red", "orange"), 
                 linetype = 2,
                 size = 2)
  })
  
  output$effect_description_cn <- renderText({# 
    # p.undetect() %>% pull(p.undetect)
    metric1 <- scope()$inc %>% mean %>% round
    metric2 <- scope()$inc %>% quantile(., 0.25) %>% round
    metric3 <- scope()$inc %>%  quantile(., 0.75) %>% round
    metric4 <- scope()$inc %>% quantile(., 0.025) %>% round
    metric5 <- scope()$inc %>%  quantile(., 0.975) %>% round
    
    paste0("根据结果，在监测到第一例COVID-19新发病例时，预期人群中的爆发规模平均水平已经达到",
           metric1,
           "例。其中2.5% - 97.5%区间为[",
           metric2,
           ", ",
           metric3,
           "]例；25% - 75%区间为[",
           metric4,
           ",",
           metric5,
           "]例。")
  })
  
  output$effect_description_en <- renderText({# 
    # p.undetect() %>% pull(p.undetect)
    metric1 <- scope()$inc %>% mean %>% round
    metric2 <- scope()$inc %>% quantile(., 0.25) %>% round
    metric3 <- scope()$inc %>%  quantile(., 0.75) %>% round
    metric4 <- scope()$inc %>% quantile(., 0.025) %>% round
    metric5 <- scope()$inc %>%  quantile(., 0.975) %>% round
    
    paste0("Based on results, upon detecting the first COVID-19 case, the epidemic size in the population is expected to have reached ",
           metric1,
           ". The 2.5% - 97.5% range is [",
           metric2,
           ", ",
           metric3,
           "]; the 25% - 75% range is [",
           metric4,
           ",",
           metric5,
           "].")
  })
    
  # output$stats <- renderPrint({
  #   c(quantile(scope() %>% pull(inc), c(0, 0.025, 0.25, 0.5, 0.75,0.975)),
  #     `Mean` = mean(scope() %>% pull(inc)))
  # })
  
}

# Create Shiny app
shinyApp(ui, server)

