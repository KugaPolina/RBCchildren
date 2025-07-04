library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(shiny.i18n)
library(shinythemes)
library(shinydashboard)

prediction_model <- readRDS('model_glm_Hb.RDS')
i18n <- Translator$new(translation_json_path = "syn.json")
i18n$set_translation_language("ru")

ui <- fluidPage(
  shiny.i18n::usei18n(i18n),
  div(style = "absolute: right;
    text-align: center;
    width: 70px",
      selectInput('selected_language',
                  i18n$t(""),
                  choices = i18n$get_languages(),
                  selected = i18n$get_key_translation())
  ),
  
  headerPanel(i18n$t('Прогностический калькулятор дозы эритроцитной взвеси для детей с онкологическими и гематологическими заболеваниями')),
  
  usei18n(i18n),
  sidebarPanel(
    numericInput('Age', i18n$t('Возраст, мес.'), 12),
    numericInput('Weight', i18n$t('Вес, кг'), 10),  # Добавлено поле для ввода веса
    
    selectInput('Diagnosis', i18n$t('Заболевание'), 
                choices = c("МДС, ЮММЛ", "Иное"), 
                selected = i18n$get_key_translation()),
    selectInput('Organomegaly', i18n$t('Органомегалия'), c("Гепатоспленомегалия", "Иное")),
    selectInput('BMI', i18n$t('Индекс массы тела'), c("Избыток массы тела", "Иное")),
    selectInput('Temperature', i18n$t('Температура'), c("Фебрильная", "Иное")),
    theme = shinytheme("flatly")
  ),
  
  mainPanel(
    h4(i18n$t("- доза для коррекции анемии, не связанной с геморрагическим синдромом")),
    h4(i18n$t("- масса тела реципиента ≤30 кг")),
    h4(i18n$t("- эритроцитная взвесь (лейкоредуцированная)")),
    plotOutput('plot1',height = "400px"),
    h3(textOutput(outputId = "textRes1")),  
    h4(textOutput(outputId = "textRes4")),  
    p(textOutput(outputId = "textRes2")),  
    p(textOutput(outputId = "textRes3")),
    h3(textOutput(outputId = "textRes5"))  # Добавлен вывод рассчитанной дозы
  ),
  
  tags$footer(style = "float: center;
                       text-align: center;
                       font-size: 17px",
              i18n$t("© Куга П.С., клиника «НИИ Детской онкологии, гематологии и трансплантологии им.Р.М.Горбачевой» ФГБОУ ВО ПСПбГМУ им.И.П.Павлова  МЗ РФ.")),
  
  tags$footer(HTML("
                    <!-- Footer -->
                           <footer class='page-footer font-large indigo'>
                           <!-- Copyright -->
                           <div class='footer-copyright text-center py-3'> 
                           <p href='doctorpolinakuga@gmail.com'>E-mail: doctorpolinakuga@gmail.com</p>
                           </div>
                           <!-- Copyright -->
                           </footer>
                           <!-- Footer -->"))
)

server <- function(input, output, session) {
  i18n_r <- reactive({
    i18n
  })
  
  observeEvent(input$selected_language, {
    print(paste("Language change!", input$selected_language))
    shiny.i18n::update_lang(input$selected_language)
  })
  
  asda <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% i18n$get_languages()) {
      i18n$set_translation_language(selected)
    }
    i18n
  })
  
  observe({
    updateSelectInput(session, "Diagnosis", label = asda()$t("Заболевание"),
                      choices = asda()$t(c("МДС, ЮММЛ", "Иное")))
    updateSelectInput(session, "Organomegaly", label = asda()$t("Органомегалия"),
                      choices = asda()$t(c("Гепатоспленомегалия", "Иное")))
    updateSelectInput(session, "BMI", label = asda()$t("Индекс массы тела"),
                      choices = asda()$t(c("Избыток массы тела", "Иное")))
    updateSelectInput(session, "Temperature", label = asda()$t("Температура"),
                      choices = asda()$t(c("Фебрильная", "Иное")))
  })
  
  selectedData <- reactive({
    data.frame(Age = input$Age/12,
               Diagnosis = 1*(input$Diagnosis == asda()$t("МДС, ЮММЛ")),
               Organomegaly = 1*(input$Organomegaly == asda()$t("Гепатоспленомегалия")),
               BMI = 1*(input$BMI == asda()$t("Избыток массы тела")),
               Temperature = 1*(input$Temperature == asda()$t("Фебрильная")),
               V_weight =5:30
    )
  })
  
  prediction_results <- reactive({
    tmp <- predict(prediction_model, 
                   newdata = selectedData(),
                   interval = "prediction", 
                   level = 0.9) %>% 
      data.frame(.)
    
    cbind(tmp, selectedData())
  })
  
  output$plot1 <- renderPlot({
    ggplot(prediction_results(), aes(x = V_weight, 
                                     y = fit)) +
      geom_line(color = "black") +
      geom_line(mapping = aes(y = lwr), color = "red", linetype = "dashed") +
      geom_line(mapping = aes(y = upr), color = "red", linetype = "dashed") +
      geom_path(data = data.frame(x = c(approx(prediction_results()$lwr, prediction_results()$V_weight, 10)$y, 
                                        approx(prediction_results()$lwr, prediction_results()$V_weight, 10)$y, -Inf),
                                  y = c(  -Inf, 10, 10)),
                aes(x, y), 
                color = "blue", 
                linetype = 2) +
      geom_path(data = data.frame(x = c(approx(prediction_results()$upr, prediction_results()$V_weight, 30)$y, 
                                        approx(prediction_results()$upr, prediction_results()$V_weight, 30)$y, -Inf),
                                  y = c(  -Inf, 30, 30)),
                aes(x, y), 
                color = "blue", 
                linetype = 2) +
      geom_label(data = data.frame(x = approx(prediction_results()$upr, prediction_results()$V_weight, 30)$y, y = 30),
                 aes(x-1,y+2,label=round(x,1))) +
      geom_label(data = data.frame(x = approx(prediction_results()$lwr, prediction_results()$V_weight, 10)$y, y = 10),
                 aes(x-1,y+2,label=round(x,1))) +
      geom_ribbon(data = prediction_results() %>% filter(lwr < 10),
                  aes(ymin = lwr,  ymax=10), 
                  fill="grey", 
                  alpha=0.3) +
      geom_ribbon(data = prediction_results() %>% filter(upr >= 30),
                  aes( ymin=30, ymax = upr), 
                  fill="red", 
                  alpha=0.3) +
      xlab(asda()$t('Доза эритроцитной взвеси, мл/кг ')) +
      ylab(asda()$t('Диапазон вероятного прироста Hb, г/л ')) +
      scale_x_continuous(breaks = seq(5,30,1), limits = c(5,15)) +
      scale_y_continuous(breaks = seq(0,70,5), limits = c(0,50)) +
      theme_bw() +
      theme(strip.text.x = element_text(size = 16, face = 'bold'),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.title = element_text(size=14))   
  })
  
  output$textRes1 <- renderText({
    paste0( 
      as.character(round(approx(prediction_results()$lwr, prediction_results()$V_weight, 10)$y, 1)),
      " - ", as.character(round(approx(prediction_results()$upr, prediction_results()$V_weight, 30)$y, 1)), 
      asda()$t(" мл/кг"))  
  })  
  
  # Добавлен расчет дозы с учетом веса
  output$textRes5 <- renderText({
    min_dose <- round(approx(prediction_results()$lwr, prediction_results()$V_weight, 10)$y, 1)
    max_dose <- round(approx(prediction_results()$upr, prediction_results()$V_weight, 30)$y, 1)
    weight <- input$Weight
    
    min_volume <- round(min_dose * weight, 1)
    max_volume <- round(max_dose * weight, 1)
    
    paste0(asda()$t("Рекомендуемый объем: "), 
           min_volume, " - ", max_volume, 
           asda()$t(" мл (для веса "), weight, " кг)")
    
 })

output$textRes4 <- renderText({
  paste0(asda()$t("Эффективная и безопасная доза эритроцитной взвеси в текущих клинических условиях, с вероятностью более 90%"))
})  

output$textRes2 <- renderText({
  paste0(asda()$t("* В дозе менее "), 
         as.character(round(approx(prediction_results()$lwr, prediction_results()$V_weight, 10)$y, 1)),
         asda()$t(" мл/кг - трансфузия может быть неэффективна (прирост Hb < 10г/л), вероятнее потребуется повторная трансфузия для достижения клинического эффекта"))  
})  

output$textRes3 <- renderText({
  paste0(asda()$t("* В дозе более "), 
         as.character(round(approx(prediction_results()$upr, prediction_results()$V_weight, 30)$y, 1)),
         asda()$t(" мл/кг - возрастает вероятность неконтролируемого подъема уровня Hb > 30г/л, что может повысить риск развития осложнений, связанных с резким подъемом гематокрита"))  
})  
}

shinyApp(ui, server)