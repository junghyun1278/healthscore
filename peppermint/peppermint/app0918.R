library(shiny)
library(shinyalert)
library(dashboardthemes)
library(shinydashboard)
library(shiny)
library(shinythemes)
library(ggplot2)
library(wordcloud2)
library(dplyr)
library(data.table)
library(shinyWidgets)
oo<-fread("score.csv")
yy<- fread("peppermint.csv")
jj<- fread("2015chuncheon.csv")
Logged <- FALSE
my_username <- "test"
my_password <- "ab"

##테마 설정

theme_flat_red <- shinyDashboardThemeDIY(
    
    appFontFamily = "Arial"
    ,appFontColor = "rgb(42,102,98)"
    ,primaryFontColor = "rgb(0,0,0)"
    ,infoFontColor = "rgb(0,0,0)"
    ,successFontColor = "rgb(0,0,0)"
    ,warningFontColor = "rgb(0,0,0)"
    ,dangerFontColor = "rgb(0,0,0)"
    ,bodyBackColor = "rgb(255,255,254)"
    
    ,logoBackColor = "rgb(45,59,66)"
    
    ,headerButtonBackColor = "rgb(72,96,106)"
    ,headerButtonIconColor = "rgb(255,255,255)"
    ,headerButtonBackColorHover = "rgb(45,59,66)"
    ,headerButtonIconColorHover = "rgb(207,57,92)"
    
    ,headerBackColor = "rgb(72,96,106)"
    ,headerBoxShadowColor = ""
    ,headerBoxShadowSize = "0px 0px 0px"
    
    ,sidebarBackColor = "rgb(207,57,92)"
    ,sidebarPadding = 0
    
    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 0
    
    ,sidebarShadowRadius = ""
    ,sidebarShadowColor = "0px 0px 0px"
    
    ,sidebarUserTextColor = "rgb(255,255,255)"
    
    ,sidebarSearchBackColor = "rgb(255,255,255)"
    ,sidebarSearchIconColor = "rgb(207,57,92)"
    ,sidebarSearchBorderColor = "rgb(255,255,255)"
    
    ,sidebarTabTextColor = "rgb(255,255,255)"
    ,sidebarTabTextSize = 14
    ,sidebarTabBorderStyle = "none"
    ,sidebarTabBorderColor = "none"
    ,sidebarTabBorderWidth = 0
    
    ,sidebarTabBackColorSelected = "rgb(143,35,60)"
    ,sidebarTabTextColorSelected = "rgb(255,255,255)"
    ,sidebarTabRadiusSelected = "0px"
    
    ,sidebarTabBackColorHover = "rgb(186,51,83)"
    ,sidebarTabTextColorHover = "rgb(255,255,255)"
    ,sidebarTabBorderStyleHover = "none"
    ,sidebarTabBorderColorHover = "none"
    ,sidebarTabBorderWidthHover = 0
    ,sidebarTabRadiusHover = "0px"
    
    ,boxBackColor = "rgb(248,248,248)"
    ,boxBorderRadius = 0
    ,boxShadowSize = "0px 0px 0px"
    ,boxShadowColor = ""
    ,boxTitleSize = 18
    ,boxDefaultColor = "rgb(248,248,248)"
    ,boxPrimaryColor = "rgb(15,124,191)"
    ,boxInfoColor = "rgb(225,225,225)"
    ,boxSuccessColor = "rgb(59,133,95)"
    ,boxWarningColor = "rgb(178,83,149)"
    ,boxDangerColor = "rgb(207,57,92)"
    
    ,tabBoxTabColor = "rgb(248,248,248)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(42,102,98)"
    ,tabBoxTabTextColorSelected = "rgb(207,57,92)"
    ,tabBoxBackColor = "rgb(248,248,248)"
    ,tabBoxHighlightColor = "rgb(207,57,92)"
    ,tabBoxBorderRadius = 0
    
    ,buttonBackColor = "rgb(207,57,92)"
    ,buttonTextColor = "rgb(255,255,255)"
    ,buttonBorderColor = "rgb(207,57,92)"
    ,buttonBorderRadius = 0
    
    ,buttonBackColorHover = "rgb(186,51,83)"
    ,buttonTextColorHover = "rgb(255,255,255)"
    ,buttonBorderColorHover = "rgb(186,51,83)"
    
    ,textboxBackColor = "rgb(255,255,255)"
    ,textboxBorderColor = "rgb(118,118,118)"
    ,textboxBorderRadius = 0
    ,textboxBackColorSelect = "rgb(255,255,255)"
    ,textboxBorderColorSelect = "rgb(118,118,118)"
    
    ,tableBackColor = "rgb(248,248,248)"
    ,tableBorderColor = "rgb(235,235,235)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1
    
)


##테스트 로그인 페이지
Logged <- FALSE
my_username <- "test"
my_password <- "ab"
login <- box(title = "Login",textInput("userName", "아이디", value = "test"),
             passwordInput("passwd", "비밀번호", value = "ab"),
             br(),actionButton("Login", "Log in"),
             width =3)


##팀원 소개 아이콘

header <- dashboardHeader(title = "코치코치", dropdownMenu(type = "messages",
                                                       notificationItem(
                                                           text = "노홍민",
                                                           icon("crown")
                                                       ),
                                                       notificationItem(
                                                           text = "전준석",
                                                           icon("user")
                                                           
                                                       ),
                                                       notificationItem(
                                                           text = "김민수",
                                                           icon("user")
                                                           
                                                       ),notificationItem(
                                                           text = "김정현",
                                                           icon("user")
                                                           
                                                       ),notificationItem(
                                                           text = "김다영",
                                                           icon("user")
                                                           
                                                       )
))


##사이드 바 ui
sidebar <- uiOutput("sidebarpanel")
body <- uiOutput("body")


##점수 계산 함수

#스마트폰 함수

smfunc<-function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o){(as.numeric(a)+as.numeric(b)+as.numeric(c)+as.numeric(d)+as.numeric(e)+as.numeric(f)+as.numeric(g)+as.numeric(h)+as.numeric(i)+as.numeric(j)+as.numeric(k)+as.numeric(l)+as.numeric(m)+as.numeric(n)+as.numeric(o))}

#우울함수

woolfunc<-function(a,b,c,d,e,f,g,h,i){abs((as.numeric(a)*as.numeric(b))-3.5)/((as.numeric(a)*as.numeric(b))-3.5)*
        (as.numeric(a)+as.numeric(b)+as.numeric(c)+as.numeric(d)+as.numeric(e)+as.numeric(f)+as.numeric(g)+as.numeric(h)+
             as.numeric(i))}
#불안함수

bulfunc <- function(a,b,c,d,e,f,g){as.numeric(a)+as.numeric(b)+as.numeric(c)+as.numeric(d)+as.numeric(f)+as.numeric(g)}

#알콜함수

alfunc <- function(a,b,c,d,e,f,g,h,i,j){
    as.numeric(a)+as.numeric(b)+as.numeric(c)+as.numeric(d)+as.numeric(e)+as.numeric(f)+
        as.numeric(g)+as.numeric(h)+as.numeric(i)+as.numeric(j)}

# 도박 함수

dofunc <- function(a,b,c,d,e,f,g,h,i){
    as.numeric(a)+as.numeric(b)+as.numeric(c)+as.numeric(d)+as.numeric(e)+as.numeric(f)+as.numeric(g)+as.numeric(h)+as.numeric(i)}

# 니코틴 함수

nicfunc <- function(a,b,c,d,e,f){
    as.numeric(a)+as.numeric(b)+as.numeric(c)+as.numeric(d)+as.numeric(e)+as.numeric(f)}

#스마트폰 점수

smscore<-function(a,b,c,d,e,f,g,h){((-93.905) + as.numeric(a)*2.959+as.numeric(b)*3.029+as.numeric(c)*3.441+as.numeric(d)*2.822+as.numeric(e)*6.18+as.numeric(f)*4.635+as.numeric(g)*7.417+as.numeric(h)*0.949)*(-1.3889)+50}

#도박 점수

doscore<-function(a,b,c,d,e,f,g){((-9.203) + as.numeric(a)*3.615+as.numeric(b)*3.44+as.numeric(c)*3.658+as.numeric(d)*4.021+as.numeric(e)*5.755+as.numeric(f)*5.378+as.numeric(g)*4.13)*(-2.5316)+66.329}

#알콜 점수

alscore<-function(a,b,c,d,e,f,g,h,i){((-7.050) + as.numeric(a)*0.386+as.numeric(b)*0.290+as.numeric(c)*0.480+as.numeric(d)*0.630+as.numeric(e)*0.471+as.numeric(f)*0.738+as.numeric(g)*0.373+as.numeric(h)*0.315+as.numeric(i)*0.495)*(-6.513)+59.8219}

#고혈압 점수

bpscore <- function(a,b,c,d,e,f,g,h,i,j,k){round({
    (-50.88 + a*0.237 + b*0.243 + c*0.047 + as.numeric(d)*0.229 + 
         as.numeric(e)*0.426 + f*(-0.084) + as.numeric(g)*(-0.036) +
         h*0.0009 + {i/(j*j)*10000}*(-0.044) + k*0.014)*(-1.515)+54.61
},1)}

#당뇨 함수
dangscore <- function(a,b,c,d,e,f,g,h,i,j,k,l){round(
    {
        (-7.239 + as.numeric(a)*(-0.034) + b*0.111 + c*0.013 + as.numeric(d)*3.147 +
             e*(-0.004) + {f/(g*g)*10000}*0.033 + as.numeric(h)*0.265 + i*0.003 +
             as.numeric(j)*0.029 + as.numeric(k)*0.035 + l*0.0005)*(-11.1694)+26.9742
    },1)}

#비만 함수
obesscore <- function(a,b,c,d,e,f,g,h,i,j){round(
    {
        (-36.49 + a*0.281 + as.numeric(b)*3.669 + c*0.131 + d*0.202 + e*0.009 +
             f*0.007 + g*0.0019 + h*0.0015 + i*0.006 + j*0.013)*(-1.877)+67.670
    },1)}

#간함수
ganscore <- function(a,b,c,d,e,f,g,h,i,j) {round(
    {
        (-5.34 + as.numeric(a)*(-1.127) + b*0.121 + c*0.008 + d*0.004 + e*0.008 +
             as.numeric(f)*0.401 + as.numeric(g)*0.122 + h*0.023 + i*0.009 +
             as.numeric(j)*0.699)*-10.435+47.229
    }, 1)}



#고지혈증 함수
gojiscore <- function(a,b,c,d,e,f,g,h,i,j,k,l,m,n) {round(
    {
        (-13.98 + a*0.064 + b*(-0.055) + c*0.009 + d*0.005 + e*0.009 + as.numeric(f)*0.634 +
             as.numeric(g)*0.045 + h*0.011 + as.numeric(i) + j*0.051 +
             as.numeric(k)*(-0.008) + l*0.004 + {m/(n*n)*10000}*0.038)*(-2.093)+59.234
    }, 1)}

#신장 함수
sinscore <- function(a,b,c,d,e,f,g,h,i) {round(
    {
        (-5.208 + as.numeric(a)*(-0.509) + as.numeric(b)*0.682 + c*0.239 +
             d*(-0.011) + as.numeric(e)*0.366 + as.numeric(f)*0.955 + g*0.445 + 
             {h/(i*i)*10000}*0.023)*(-2.842)+82.943
    }, 1)}




##UI

ui <- dashboardPage(header, sidebar, body)

##SERVER
server <- function(input, output, session) {
    USER <<- reactiveValues(Logged = Logged)
    output$good <- renderText("ID=test   PW=ab")
    observe({ 
        if (USER$Logged == FALSE) {
            if (!is.null(input$Login)) {
                if (input$Login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    Id.username <- which(my_username == Username)
                    Id.password <- which(my_password == Password)
                    if (length(Id.username) > 0 & length(Id.password) > 0) {
                        if (Id.username == Id.password) {
                            USER$Logged <<- TRUE
                        } 
                    }
                } 
            }
        }    
    })
    
    output$sidebarpanel <- renderUI({
        if (USER$Logged == TRUE) { 
            dashboardSidebar( 
                width = 230,
                sidebarMenu(id = "sidebarid",
                            menuItem("개인 정보 입력란",tabName = "page1",
                                     icon = icon("file-medical")),
                            menuItem("신체 건강 점수 확인",tabName = "page2", 
                                     icon = icon("user")),
                            menuItem("정신 건강",tabName = "page3", 
                                     icon = icon("brain")),
                            menuItem("정신건강 점수 확인", tabName = "page4",
                                     icon = icon("heart")),
                            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                            tags$div(tags$span(style="font-size: xx-large; float: right; color: maroon;", "peppermint")
                                     
                            )
                            
                            
                ))}})
    
    
    
    output$body <- renderUI({
        if (USER$Logged == TRUE) {
            dashboardBody( 
                theme_flat_red
                ,
                tabItems(
                    tabItem(tabName = "page1",
                            fluidPage( 
                                
                                tabsetPanel(id="tabset", 
                                            tabPanel(title = "기본 정보", value ="one",
                                                     textInput("name", "이름", value ="노홍민"),
                                                     numericInput("age", "나이", value = 27),
                                                     radioButtons("sex",'성별', choices = list('남자'=  0,'여자' = 1),selected = 0,inline = T),
                                                     checkboxInput("checkbox", label = "개인정보 및 의료정보 수집에 동의합니다.",  value = TRUE),
                                                     actionButton('jumpToP2', 'NEXT')),
                                            tabPanel(title ="진료 정보" , value ="two",
                                                     box(numericInput("a", "신장", value = 177),
                                                         numericInput("b", "체중", value = 62),
                                                         numericInput("c", "허리 둘레(cm)", value = 70),
                                                         numericInput("d", "수축기 혈압", value = 100),
                                                         numericInput("e", "이완기 혈압", value = 60),
                                                         numericInput("f", "식전 혈당(공복 혈당)", value = 85),
                                                         numericInput("g", "총콜레스테롤", value = 190),
                                                         numericInput("h", "트리글리세라이드", value = 100),
                                                         numericInput("i", "HDL콜레스테롤", value = 62),
                                                         numericInput("j", "LDL콜레스테롤", value = 118),
                                                         actionButton('ju1', 'PREV'),actionButton('jumpToP3', 'NEXT'),
                                                         width =3),
                                                     box(numericInput("k", "혈색소", value = 50),
                                                         radioButtons('radiobtn', '요단백', choices = list('음성(－)' = 1,'약약성(±)' = 2,'양성(＋1)' = 3,
                                                                                                        '양성(＋2)' = 4, '양성(＋3)' = 5, '양성(＋4)' = 6), 
                                                                      selected = 1, inline = T),
                                                         numericInput("n", "혈청크레아티닌", value = 0.5),
                                                         numericInput("m", "(혈청지오티)AST", value = 20),
                                                         
                                                         numericInput("o", "(혈청지피티)ALT", value = 19),
                                                         numericInput("p", "감마지티피", value = 20),width =3)),
                                            tabPanel(title = "병력 정보", value ="three",
                                                     radioButtons('radiobtn1', '(본인)뇌졸중과거병력유무', choices = list('미해당'=  0,'해당' = 1),
                                                                  selected = 0, inline = T),
                                                     radioButtons('radiobtn2', '(본인)심장병과거병력유무', choices = list('미해당'=  0,'해당' = 1),
                                                                  selected = 0, inline = T),
                                                     radioButtons('radiobtn3', '(본인)고혈압과거병력유무', choices = list('미해당'=  0,'해당' = 1),
                                                                  selected = 0, inline = T),
                                                     radioButtons('radiobtn4', '(본인)당뇨병과거병력유무', choices = list('미해당'=  0,'해당' = 1),
                                                                  selected = 0, inline = T),
                                                     radioButtons('radiobtn5', '(본인)고지혈증(이상지질혈증)과거병력유무',
                                                                  choices = list('미해당'=  0,'해당' = 1),
                                                                  selected = 0, inline = T),
                                                     radioButtons('radiobtn6', '(본인)폐결핵과거병력유무', choices = list('미해당'=  0,'해당' = 1),
                                                                  selected = 0, inline = T),
                                                     radioButtons('radiobtn7', '(본인)(본인)기타(암포함)질환 과거병력유무',
                                                                  choices = list('미해당'=  0,'해당' = 1),
                                                                  selected = 0, inline = T),
                                                     radioButtons('radiobtn8', '(가족력)뇌졸중환자유무', choices = list('미해당'=  0,'해당' = 1),
                                                                  selected = 0, inline = T),
                                                     radioButtons('radiobtn9', '(가족력)심장병환자유무', choices = list('미해당'=  0,'해당' = 1),
                                                                  selected = 0, inline = T),
                                                     radioButtons('radiobtn10', '(가족력)고혈압환자유무', choices = list('미해당'=  0,'해당' = 1),
                                                                  selected = 1, inline = T),
                                                     radioButtons('radiobtn11', '(가족력)당뇨병환자유무', choices = list('미해당'=  0,'해당' = 1),
                                                                  selected = 1, inline = T),
                                                     radioButtons('radiobtn12', '(가족력)기타(암포함)환자유무', choices = list('미해당'=  0,'해당' = 1),
                                                                  selected = 1, inline = T),
                                                     
                                                     actionButton('ju2', 'PREV'),actionButton('jumpToP4', 'NEXT')),
                                            
                                            tabPanel(title ="생활 습관 정보" ,value ="five",
                                                     radioButtons('radiobtn13', '흡연상태', choices = list('피우지 않는다' = 1,
                                                                                                       '과거에 피웠으나 지금은 끊었다' = 2,
                                                                                                       '현재도 피운다' = 3),
                                                                  selected = 2, inline = T),
                                                     
                                                     numericInput("dddd", "(과거)흡연기간(년)", value = 5),
                                                     numericInput("aa","(과거)하루흡연량(개피)", value = 20),
                                                     numericInput("ab","(현재)흡연기간(년)", value = 0),
                                                     numericInput("ac", "(현재)하루흡연량(개피)", value = 0),
                                                     radioButtons('radiobtn14', '음주습관',
                                                                  choices = list('0일' = 1,'1일' = 2 ,'2일' = 3,
                                                                                 '3일' = 4, '4일' = 5,
                                                                                 '5일' = 6, '6일' = 7, '7일' = 8),
                                                                  selected = 3, inline = T),
                                                     numericInput("ae","1회 음주량(잔)", value = 3),
                                                     radioButtons('radiobtn15', '1주_20분이상 격렬한 운동',
                                                                  choices = list('0일' = 1,'1일' = 2 ,'2일' = 3,
                                                                                 '3일' = 4, '4일' = 5, '5일' = 6,
                                                                                 '6일' = 7, '7일' = 8),
                                                                  selected = 2, inline = T),
                                                     radioButtons('radiobtn16', '1주_30분이상 중간정도 운동',
                                                                  choices = list('0일' = 1,'1일' = 2 ,'2일' = 3,'3일' = 4, '4일' = 5,
                                                                                 '5일' = 6, '6일' = 7, '7일' = 8),
                                                                  selected = 4, inline = T),
                                                     radioButtons('radiobtn17', '1주_총30분이상 걷기 운동',
                                                                  choices = list('0일' = 1,'1일' = 2 ,'2일' = 3, '3일' = 4, '4일' = 5,
                                                                                 '5일' = 6, '6일' = 7, '7일' = 8),
                                                                  selected = 2, inline = T),
                                                     
                                                     useShinyalert(),  # Set up shinyalert
                                                     actionButton('ju4', 'PREV'),actionButton('jumpToP6', '입력 완료'))
                                            
                                ))),
                    
                    tabItem(tabName = "page2",
                            
                            fluidPage(box( status = "warning",
                                           fluidRow(       
                                               box(infoBoxOutput("bbbb1"),
                                                   infoBoxOutput("oooo1"),
                                                   
                                                   
                                                   
                                                   infoBoxOutput("ddddd1"),
                                                   infoBoxOutput("ssss1"),
                                                   infoBoxOutput("gggg1"),
                                                   infoBoxOutput("jjjj1"),
                                                   valueBoxOutput("tttt"), width = 500)),
                                           tags$style("#bbbb1 {width:300px;}"),
                                           tags$style("#oooo1 {width:300px;}"),
                                           tags$style("#ddddd1 {width:300px;}"),
                                           tags$style("#ssss1 {width:300px;}"),
                                           tags$style("#gggg1 {width:300px;}"),
                                           tags$style("#jjjj1 {width:300px;}"),
                                           tags$style("#tttt {width:300px;}"), height = "500px"
                                           
                            ),
                            box(status = "warning",tabBox(
                                
                                # The id lets us use input$tabset1 on the server to find the current tab
                                id = "tabset1", width = 600, height = "250px",
                                tabPanel("종합점수", 
                                         fluidRow(box(
                                             title = "총점", status = "info", solidHeader = TRUE, 
                                             width = 6, plotOutput("boxplot1" , height = 200))
                                         ),
                                         fluidRow(valueBoxOutput("tttt1"))),
                                tabPanel("고혈압 점수", 
                                         fluidRow(box(
                                             title = "수축기 혈압", status = "info", solidHeader = TRUE,
                                             width = 3,height = "260px",
                                             collapsible = TRUE,
                                             
                                             plotOutput("boxplot2", height = 200)),
                                             
                                             box(
                                                 title = "이완기 혈압", status = "info", solidHeader = TRUE,
                                                 width = 3,height = "260px",
                                                 collapsible = TRUE,
                                                 
                                                 plotOutput("box2", height = 200)),
                                             box(
                                                 title = "설명 보기", status = "warning", solidHeader = TRUE,
                                                 width = 6, height = "260px",
                                                 collapsible = TRUE,
                                                 "빨간색 선은 자신의 위치입니다.",br(),br(),"고혈압의 원인은 유전적인 요인(가족력)이 가장 흔하며 노화, 비만, 짜게 먹는 습관, 운동부족, 스트레스 등이 고혈압을 일으킨다고 밝혀져 있습니다.",br(),br(),"고혈압의 흔한 증상으로는 혈압이 갑자기 올라가거나 정신적, 육체적 과로로 피로한 경우 두통, 어지럼증, 피로감 등의 증상이 나타날 수 있습니다."
                                             )
                                             
                                             
                                             
                                             
                                         ),
                                         fluidRow(
                                             valueBoxOutput("bbbb"))),
                                tabPanel("당뇨 점수", 
                                         fluidRow(box(
                                             title = "공복 혈당", status = "info", solidHeader = TRUE,
                                             width = 3, height = "260px",
                                             collapsible = TRUE,
                                             plotOutput("boxplot3", height = 200)),
                                             
                                             box(
                                                 title = "총 콜레스테롤", status = "info", solidHeader = TRUE,
                                                 width = 3, height = "260px",
                                                 collapsible = TRUE,
                                                 plotOutput("box3", height = 200)),
                                             box(
                                                 title = "설명 보기", status = "warning", solidHeader = TRUE,
                                                 width = 6, height = "260px",
                                                 collapsible = TRUE,
                                                 "빨간색 선은 자신의 위치입니다.",br(),br(),"부모가 당뇨병이 있으면 당뇨병이 없는 경우보다 자식이 당뇨병에 걸릴 확률이 높습니다. 유전적인 요인 외에도 비만, 연령, 식생활, 운동부족, 스트레스, 약물 등의 환경적인 요인도 당뇨병 발생에 매우 중요한 역할을 합니다",br(),br(),
                                                 "당뇨병의 특징적인 3대 증상은 다뇨, 다음, 다식입니다."
                                             )
                                             
                                             
                                             
                                             
                                             
                                             
                                         ),
                                         fluidRow(
                                             valueBoxOutput("ddddd"))),
                                tabPanel("비만 점수", 
                                         fluidRow(box(
                                             title = "BMI", status = "info", solidHeader = TRUE,
                                             width = 3, height = "260px",
                                             collapsible = TRUE,
                                             plotOutput("boxplot4", height = 200)),
                                             
                                             box(
                                                 title = "허리둘레", status = "info", solidHeader = TRUE,
                                                 width = 3, height = "260px",
                                                 collapsible = TRUE,
                                                 plotOutput("box4", height = 200)),
                                             box(
                                                 title = "설명 보기", status = "warning", solidHeader = TRUE,
                                                 width = 6, height = "260px",
                                                 collapsible = TRUE,
                                                 "빨간색 선은 자신의 위치입니다.",br(),br(),"불규칙한 식습관, 과다한 음식 섭취, 운동 부족, 내분비계통 질환, 유전적 요인, 정신적 요인 및 약물 등이 비만의 원인입니다. "
                                             )
                                             
                                             
                                             
                                             
                                         ),
                                         fluidRow(
                                             valueBoxOutput("oooo"))),
                                tabPanel("간 점수", 
                                         fluidRow(box(
                                             title = "혈청 지오티(AST)", status = "info", solidHeader = TRUE,
                                             width = 3, height = "260px",
                                             collapsible = TRUE,
                                             plotOutput("boxplot5", height = 200)),
                                             
                                             box(
                                                 title = "혈청 지피티(ALT)", status = "info", solidHeader = TRUE,
                                                 width = 3, height = "260px",
                                                 collapsible = TRUE,
                                                 plotOutput("box5", height = 200)),
                                             box(
                                                 title = "설명 보기", status = "warning", solidHeader = TRUE,
                                                 width = 6, height = "260px",
                                                 collapsible = TRUE,
                                                 "빨간색 선은 자신의 위치입니다.",br(),br(),"간염은 간질환의 가장 큰 원인으로, 간염성 80%, 알코올성 20%정도입니다. ",br(),br(), "간 질환의 증상으로는 열과 복통, 배가 불러오는 증상을 보이며 피로와 나른함, 식욕부진, 메스꺼움 등을 느끼는 것입니다."
                                             )
                                             
                                             
                                             
                                             
                                         ),
                                         fluidRow(
                                             valueBoxOutput("gggg"))),
                                tabPanel("고지혈증 점수", 
                                         fluidRow(box(
                                             title = "총 콜레스테롤", status = "info", solidHeader = TRUE,
                                             width = 3, height = "260px",
                                             collapsible = TRUE,
                                             plotOutput("boxplot6", height = 200)),
                                             
                                             box(
                                                 title = "트리글리세라이드", status = "info", solidHeader = TRUE,
                                                 width = 3, height = "260px",
                                                 collapsible = TRUE,
                                                 plotOutput("box6", height = 200)),
                                             box(
                                                 title = "설명 보기", status = "warning", solidHeader = TRUE,
                                                 width = 6, height = "260px",
                                                 collapsible = TRUE,
                                                 "빨간색 선은 자신의 위치입니다.",br(),br(),"고지혈증의 원인은 식습관과 가족력이 대부분이고 기타 비만이나 당뇨, 신부전, 갑상선기능저하증, 쿠싱병, 이 외에 다양한 질환들에서 이차적으로 고지혈증을 유발하게 됩니다. 또한 운동부족이나 음주, 여성호르몬, 일부 항고혈압약제도 고지혈증을 일으킬 수 있으므로 고지혈증을 유발하게 된 원인에 대해 자세한 병력 및 투약 내역을 확인해야 합니다."
                                             )
                                             
                                             
                                             
                                             
                                         ),
                                         fluidRow(
                                             valueBoxOutput("jjjj"))),
                                tabPanel("신장 점수", 
                                         fluidRow(box(
                                             title = "혈청 크레아틴", status = "info", solidHeader = TRUE,
                                             width = 3, height = "260px",
                                             collapsible = TRUE,
                                             plotOutput("boxplot7", height = 200)),
                                             
                                             box(
                                                 title = "총 콜레스테롤", status = "info", solidHeader = TRUE,
                                                 width = 3, height = "260px",
                                                 collapsible = TRUE,
                                                 plotOutput("box7", height = 200)),
                                             box(
                                                 title = "설명 보기", status = "warning", solidHeader = TRUE,
                                                 width = 6, height = "260px",
                                                 collapsible = TRUE,
                                                 "빨간색 선은 자신의 위치입니다.",br(),br(),"당뇨병이 원인인 경우가 40% 정도로 가장 흔하고, 고혈압이나 신장염 등이 각각 15% 정도를 차지합니다.",br(),br(),"피로함, 피부 가려움, 어둡고 악취가 나는 소변, 밤 중에 소변을 보고 싶은 욕구가 강해지는 것, 요실금, 등이 있습니다."
                                             )
                                             
                                             
                                             
                                             
                                             
                                         ),
                                         fluidRow(
                                             valueBoxOutput("ssss")))
                                
                            ), height = "500px"),
                            fluidRow(downloadButton("save", "검사결과저장"))
                            )),
                    
                    tabItem(tabName = "page3", 
                            fluidRow( tabsetPanel(id="tabset2",
                                                  tabPanel(title = "알코올 중독", value = "al",
                                                           mainPanel(
                                                               radioButtons('al1', '1.술을 얼마나 마십니까?', choices = list('전혀안마심'=  0,'한달에 한번이하' = 1,
                                                                                                                    '한달에 2~4회' = 2,'일주일에 2~3회' = 3,'일주일에 4회이상' = 4),
                                                                            selected = 4, inline = T),
                                                               radioButtons('al2', '2.평소 술을 마시는 날 몇 잔 정도나 마십니까?', choices = list('1~2잔'=  0,'3~4잔' = 1,
                                                                                                                                 '5~6잔' = 2,'7~9잔' = 3,'10잔 이상' = 4),
                                                                            selected = 3, inline = T),
                                                               radioButtons('al3', '3.술을 마실때 소주1병 또는 맥주4병 이상 마시는 음주는 얼마나 자주 있습니까?', choices = list('전혀없다'=  0,'한달에 한번 미만' = 1,
                                                                                                                                                   '한달에 한번' = 2,'일주일에 한번' = 3,'매일같이' = 4),
                                                                            selected = 3, inline = T),
                                                               radioButtons('al4', '4.지난1년간, 술을 한번 마시기 시작하면 멈출 수 없었던 때가 얼마나 자주 있었습니까?', choices = list('전혀없다'=  0,'한달에 한번 미만' = 1,
                                                                                                                                                       '한달에 한번' = 2,'일주일에 한번' = 3,'매일같이' = 4),
                                                                            selected = 0, inline = T),
                                                               radioButtons('al5', '5.지난 1년간 당신은 평소 할 수 있었던 일을 음주 때문에 실패한 적이 얼마나 자주 있었습니까?', choices = list('전혀없다'=  0,'한달에 한번 미만' = 1,
                                                                                                                                                            '한달에 한번' = 2,'일주일에 한번' = 3,'매일같이' = 4),
                                                                            selected = 2, inline = T),
                                                               radioButtons('al6', '6.지난 1년간 술을 마신 다음날 아침에 다시 해장술이 필요했던 적이 얼마나 자주 있었습니까?', choices = list('전혀없다'=  0,'한달에 한번 미만' = 1,
                                                                                                                                                          '한달에 한번' = 2,'일주일에 한번' = 3,'매일같이' = 4),
                                                                            selected = 3, inline = T),
                                                               radioButtons('al7', '7.지난 1년간 음주 후에 죄책감이 들거나 후회를 한적이 얼마나 자주 있었습니까?', choices = list('전혀없다'=  0,'한달에 한번 미만' = 1,
                                                                                                                                                   '한달에 한번' = 2,'일주일에 한번' = 3,'매일같이' = 4),
                                                                            selected = 2, inline = T),
                                                               radioButtons('al8', '8.지난 1년간 음주 떄문에 전날 밤에 있었던 일이 기억나지 않았던 적이 얼마나 자주 있었습니까?', choices = list('전혀없다'=  0,'한달에 한번 미만' = 1,
                                                                                                                                                            '한달에 한번' = 2,'일주일에 한번' = 3,'매일같이' = 4),
                                                                            selected = 1, inline = T),
                                                               radioButtons('al9', '9.음주로 인해 자신이나 다른 사람을 다치게 한 적이 있었습니까?', choices = list('전혀없다'=  0,'있지만 지난 1년 간에는 없었다' = 2,
                                                                                                                                          '지난 1년 내 있었다' = 4),
                                                                            selected = 0, inline = T),
                                                               radioButtons('al10', '10.친척이나 친구 또는 의사가 당신이 술 마시는 것을 걱정하거나 술 끊기를 권유한 적이 있었습니까?', choices = list('없음'=  0,'있지만 지난 1년간에는 없었다' = 2,
                                                                                                                                                               '지난 1년 내 있었다' = 4),
                                                                            selected = 4, inline = T),
                                                               actionButton('jung1', 'NEXT')
                                                               
                                                           ),
                                                           
                                                           textOutput('t1')),
                                                  tags$head(tags$style("#t1{color: maroon;
                                 font-size: 40px;
                                 font-style: italic;
                                 }"
                                                  )
                                                  ),
                                                  
                                                  
                                                  tabPanel(title= "니코틴 의존 검사", value = "ccc4",
                                                           mainPanel(
                                                               radioButtons('o1', '1.아침에 일어나서 얼마 만에 첫 담배를 피우십니까?', choices = list('5분이내'=  3,'6~30분' = 2,
                                                                                                                                  '31~60분' = 1,'60분 이후' = 0),
                                                                            selected = 3, inline = T),
                                                               radioButtons('o2', '2.지하철, 버스, 병원, 영화관 등과 같은 금연 구역에서 흡연 욕구를 참는 것이 어렵습니까?', choices = list('예'=  1,'아니오' = 0),
                                                                            selected = 1, inline = T),
                                                               radioButtons('o3', '3.가장 포기하기 싫은 담배, 즉 가장 좋아하는 담배는 어떤 것입니까?', choices = list('아침 첫 담배'=  1,'그 외 담배' = 0),
                                                                            selected = 0, inline = T),
                                                               radioButtons('o4', '4.하루에 담배를 몇 개비나 피우 십니까?', choices = list('31개비 이상'=  3,'21~30개비' = 2,'11~20개비' = 1,'10개비 이하' = 0),
                                                                            selected = 1, inline = T),
                                                               radioButtons('o5', '5.깨어나서 처음 몇 시간 동안 피우는 흡연량이 그 외의 시간에 피우는 흡연량 보다 많습니까?', choices = list('예'=  1,'아니오' = 0),
                                                                            selected = 0, inline = T),
                                                               radioButtons('o6', '6.아파서 거의 하루 종일 누워 있거나, 감기나 독감에 걸려 호흡이 곤란할 때도 담배를 피우십니까?', choices = list('예'=  1,'아니오' = 0),
                                                                            selected = 0, inline = T),
                                                               actionButton('ju5', 'PREV'),actionButton('ton', 'NEXT')
                                                               
                                                           ),
                                                           textOutput('text')),
                                                  tags$head(tags$style("#text{color: maroon;
                                 font-size: 40px;
                                 font-style: italic;
                                 }"
                                                  )
                                                  ),
                                                  
                                                  tabPanel(title = "도박 중독", value = "di",
                                                           mainPanel(
                                                               radioButtons('di1', '1.돈내기 게임 떄문에 단체 활동이나 연습에 빠진 적이 있나요?', choices = list('없다'=  0,'가끔 있다' = 1,
                                                                                                                                         '자주 있다' = 2,'거의 항상 있다' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('di2', '2.돈내기 게임을 같이 하는 친구들과 어울리느라 다른 친구들 과의 약속을 어긴 적이 있나요?', choices = list('없다'=  0,'가끔 있다' = 1,
                                                                                                                                                         '자주 있다' = 2,'거의 항상 있다' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('di3', '3.돈내기 게임을 위해 계획을 세운 적이 있나요?', choices = list('없다'=  0,'가끔 있다' = 1,
                                                                                                                                 '자주 있다' = 2,'거의 항상 있다' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('di4', '4.돈내기 게임 때문에 기분이 나빴던 적이 있나요?', choices = list('없다'=  0,'가끔 있다' = 1,
                                                                                                                                  '자주 있다' = 2,'거의 항상 있다' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('di5', '5.전에 잃은 돈을 되찾기 위해 다시 돈내기 게임을 한 적이 있나요?', choices = list('없다'=  0,'가끔 있다' = 1,
                                                                                                                                            '자주 있다' = 2,'거의 항상 있다' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('di6', '6.돈내기 게임 하는 것을 부모나 가족 또는 선생님에게 숨긴 적이 있나요?', choices = list('없다'=  0,'가끔 있다' = 1,
                                                                                                                                               '자주 있다' = 2,'거의 항상 있다' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('di7', '7.지난 3개월 동안 돈내기 게임으로 인해 내게 문제가 생겼다고 느낀 적이 있나요?',choices = list('없다'=  0,'가끔 있다' = 1,
                                                                                                                                                   '자주 있다' = 2,'거의 항상 있다' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('di8', '8.밥이나 옷, 영화표 구입 등에 써야 할 용돈을 돈내기 게임에 쓴 적이 있나요?',choices = list('없다'=  0,'가끔 있다' = 1,
                                                                                                                                                  '자주 있다' = 2,'거의 항상 있다' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('di9', '9.돈내기 게임을 위해 남의 돈이나 돈이 될 만한 물건을 몰래 가져온 적이 있나요?',choices = list('없다'=  0,'가끔 있다' = 1,
                                                                                                                                                   '자주 있다' = 2,'거의 항상 있다' = 3),
                                                                            selected = 0, inline = T),
                                                               actionButton('ju6', 'PREV'),actionButton('jung2', 'NEXT')
                                                           ),
                                                           textOutput('t2')),
                                                  tags$head(tags$style("#t2{color: maroon;
                                 font-size: 40px;
                                 font-style: italic;
                                 }"
                                                  )
                                                  ),
                                                  
                                                  
                                                  
                                                  tabPanel(title = "스마트폰 중독", value = "sm",
                                                           mainPanel(
                                                               radioButtons('sm1', '1.스마트폰의 지나친 사용으로 학교 성적이나 업무 능률이 떨어진다.', choices = list('전혀 그렇지 않다'=  1,'그렇지 않다' = 2,
                                                                                                                                           '그렇다'=3,'매우 그렇다' = 4),
                                                                            selected = 3, inline = T),
                                                               radioButtons('sm2', '2.스마트폰을 사용하지 못하면 온 세상을 잃을 것 같은 생각이 든다', choices = list('전혀 그렇지 않다'=  1,'그렇지 않다' = 2,
                                                                                                                                           '그렇다'=3,'매우 그렇다' = 4),
                                                                            selected = 2, inline = T),
                                                               radioButtons('sm3', '3.스마트폰을 사용할 떄 "그만해야지"라고 생각은 하면서도 계속한다.', choices = list('전혀 그렇지 않다'=  1,'그렇지 않다' = 2,
                                                                                                                                            '그렇다'=3,'매우 그렇다' = 4),
                                                                            selected = 3, inline = T),
                                                               radioButtons('sm4', '4.스마트폰이 없어도 불안하지 않다.', choices = list('전혀 그렇지 않다'=  4,'그렇지 않다' = 3,
                                                                                                                          '그렇다'=2,'매우 그렇다' = 1),
                                                                            selected = 2, inline = T),
                                                               radioButtons('sm5', '5.수시로 스마트폰을 사용하다가 지적을 받은 적이 있다.', choices = list('전혀 그렇지 않다'=  1,'그렇지 않다' = 2,
                                                                                                                                     '그렇다'=3,'매우 그렇다' = 4),
                                                                            selected = 2, inline = T),
                                                               radioButtons('sm6', '6.가족이나 친구들과 함께 있는 것보다 스마트폰을 사용하는 것이 더 즐겁다.', choices = list('전혀 그렇지 않다'=  1,'그렇지 않다' = 2,
                                                                                                                                                '그렇다'=3,'매우 그렇다' = 4),
                                                                            selected = 2, inline = T),
                                                               radioButtons('sm7', '7.스마트폰 사용시간을 줄이려고 해보았지만 실패한다.', choices = list('전혀 그렇지 않다'=  1,'그렇지 않다' = 2,
                                                                                                                                   '그렇다'=3,'매우 그렇다' = 4),
                                                                            selected = 2, inline = T),
                                                               radioButtons('sm8', '8.스마트폰을 너무 자주 또는 오래한다고 가족이나 친구들로 부터 불평을 들은 적이 있다.', choices = list('전혀 그렇지 않다'=  4,'그렇지 않다' = 3,
                                                                                                                                                       '그렇다'=2,'매우 그렇다' = 1),
                                                                            selected = 2, inline = T),
                                                               radioButtons('sm9', '9.스마트폰을 사용할수 없게 된다면 견디기 힘들 것이다.', choices = list('전혀 그렇지 않다'=  1,'그렇지 않다' = 2,
                                                                                                                                     '그렇다'=3,'매우 그렇다' = 4),
                                                                            selected = 3, inline = T),
                                                               radioButtons('sm10', '10.스마트폰 사용에 많은 시간을 보내지 않는다.', choices = list('전혀 그렇지 않다'=  4,'그렇지 않다' = 3,
                                                                                                                                  '그렇다'=2,'매우 그렇다' = 1),
                                                                            selected = 3, inline = T),
                                                               radioButtons('sm11', '11.인터넷 사용으로 인해 학교 성적이 떨어졌다.', choices = list('전혀 그렇지 않다'=  1,'그렇지 않다' = 2,
                                                                                                                                  '그렇다'=3,'매우 그렇다' = 4),
                                                                            selected = 2, inline = T),
                                                               radioButtons('sm12', '12.스마트폰을 사용하느라 지금 하고 있는 일에 집중이 안된 적이 있다', choices = list('전혀 그렇지 않다'=  1,'그렇지 않다' = 2,
                                                                                                                                              '그렇다'=3,'매우 그렇다' = 4),
                                                                            selected = 3, inline = T),
                                                               radioButtons('sm13', '13.스마트폰 사용에 많은 시간을 보내는 것이 습관화 되었다.', choices = list('전혀 그렇지 않다'=  4,'그렇지 않다' = 3,
                                                                                                                                         '그렇다'=2,'매우 그렇다' = 1),
                                                                            selected = 2, inline = T),
                                                               radioButtons('sm14', '14.스마트폰이 없으면 안절부절 못하고 초조해진다.', choices = list('전혀 그렇지 않다'=  1,'그렇지 않다' = 2,
                                                                                                                                   '그렇다'=3,'매우 그렇다' = 4),
                                                                            selected = 2, inline = T),
                                                               radioButtons('sm15', '15.스마트폰 사용이 지금 하고 있는 일에 방해가 되지 않는다.', choices = list('전혀 그렇지 않다'=  1,'그렇지 않다' = 2,
                                                                                                                                          '그렇다'=3,'매우 그렇다' = 4),
                                                                            selected = 2, inline = T),
                                                               actionButton('ju7', 'PREV'),actionButton('jung4', 'NEXT')
                                                           ),
                                                           textOutput('t4')),
                                                  tags$head(tags$style("#t4{color: maroon;
                                 font-size: 40px;
                                 font-style: italic;
                                 }"
                                                  )
                                                  ),
                                                  
                                                  
                                                  tabPanel(title = "불안 장애 검사", value = "ccc2",
                                                           mainPanel(
                                                               radioButtons('w1', '1.초조하거나 불안하거나 조마조마하게 느낀다.', choices = list('전혀아니다'=  0,'조금 그렇다' = 1,
                                                                                                                              '꽤 그런 편이다'=2,'거의 매일 그렇다' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('w2', '2.걱정하는 것을 멈추거나 조절할 수가 없다.', choices = list('전혀아니다'=  0,'조금 그렇다' = 1,
                                                                                                                              '꽤 그런 편이다'=2,'거의 매일 그렇다' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('w3', '3.여러 가지 것들에 대해 걱정을 너무 많이 한다.', choices = list('전혀아니다'=  0,'조금 그렇다' = 1,
                                                                                                                                 '꽤 그런 편이다'=2,'거의 매일 그렇다' = 3),
                                                                            selected = 1, inline = T),
                                                               radioButtons('w4', '4.편하게 있기가 어렵다.', choices = list('전혀아니다'=  0,'조금 그렇다' = 1,
                                                                                                                   '꽤 그런 편이다'=2,'거의 매일 그렇다' = 3),
                                                                            selected = 1, inline = T),
                                                               radioButtons('w5', '5.너무 안절부절 못해서 가만히 있기가 힘들다.', choices = list('전혀아니다'=  0,'조금 그렇다' = 1,
                                                                                                                               '꽤 그런 편이다'=2,'거의 매일 그렇다' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('w6', '6.쉽게 짜증이 나거나 쉽게 성을 내게 된다.', choices = list('전혀아니다'=  0,'조금 그렇다' = 1,
                                                                                                                              '꽤 그런 편이다'=2,'거의 매일 그렇다' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('w7', '7.마치 끔찍한 일이 생길 것처럼 두렵게 느껴진다.', choices = list('전혀아니다'=  0,'조금 그렇다' = 1,
                                                                                                                                 '꽤 그런 편이다'=2,'거의 매일 그렇다' = 3),
                                                                            selected = 0, inline = T),
                                                               actionButton('ju8', 'PREV'),actionButton('butt', 'NEXT')
                                                           ),
                                                           textOutput('kk')),
                                                  tags$head(tags$style("#kk{color: maroon;
                                 font-size: 40px;
                                 font-style: italic;
                                 }"
                                                  )
                                                  ),
                                                  
                                                  
                                                  tabPanel(title = "우울증 건강 검사", value = "ccc1",
                                                           mainPanel(
                                                               radioButtons('q1', '1.최근 2주 동안 기분이 가라앉거나, 우울하거나, 희망이 없다고 느꼈다.', choices = list('없음'=  0,'2~6일' = 1,
                                                                                                                                              '7~12일'=2,'거의 매일' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('q2', '2.평소 하던 일에 대한 흥미가 없어지거나 즐거움을 느끼지 못했다.', choices = list('없음'=  0,'2~6일' = 1,
                                                                                                                                          '7~12일'=2,'거의 매일' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('q3', '3.잠들기가 어렵거나 자주 깼다. 혹은 너무 많이 잤다.', choices = list('없음'=  0,'2~6일' = 1,
                                                                                                                                    '7~12일'=2,'거의 매일' = 3),
                                                                            selected = 1, inline = T),
                                                               radioButtons('q4', '4.평소 보다 식욕이 줄었다. 혹은 평소보다 많이 먹었다.', choices = list('없음'=  0,'2~6일' = 1,
                                                                                                                                     '7~12일'=2,'거의 매일' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('q5', '5.다른 사람들이 눈치 챌 정도로 평소보다 말과 행동이 느려졌다. 혹은 너무 안절부절 못해서 가만히 있지 못한다.', choices = list('없음'=  0,'2~6일' = 1,
                                                                                                                                                                    '7~12일'=2,'거의 매일' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('q6', '6.피곤하고 기운이 없었다.', choices = list('없음'=  0,'2~6일' = 1,
                                                                                                                    '7~12일'=2,'거의 매일' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('q7', '7.내가 잘못 했거나, 실패 했다는 생각이 들었다. 혹은 자신과 가족을 실망 시켰다고 생각 했다.', choices = list('없음'=  0,'2~6일' = 1,
                                                                                                                                                           '7~12일'=2,'거의 매일' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('q8', '8.신문을 읽거나 tv를 보는 것과 같은 일상적인 일에도 집중 할 수가 없었다.', choices = list('없음'=  0,'2~6일' = 1,
                                                                                                                                                 '7~12일'=2,'거의 매일' = 3),
                                                                            selected = 0, inline = T),
                                                               radioButtons('q9', '9.차라리 죽는 것이 더 낫겠다고 생각햇다. 혹은 자해할 생각을 했다.', choices = list('없음'=  0,'2~6일' = 1,
                                                                                                                                            '7~12일'=2,'거의 매일' = 3),
                                                                            selected = 0, inline = T),
                                                               useShinyalert(),  
                                                               actionButton('ju9', 'PREV'),actionButton('bu', '입력 완료')
                                                           ),
                                                           textOutput('tt')),
                                                  tags$head(tags$style("#tt{color: maroon;
                                 font-size: 40px;
                                 font-style: italic;
                                 }"
                                                  )
                                                  )
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  
                            ))),
                    
                    
                    tabItem(tabName = "page4",
                            fluidRow(mainPanel(
                                fluidRow( infoBoxOutput("wwww"),
                                          infoBoxOutput("bulan"),
                                          infoBoxOutput("nic")
                                          
                                ),
                                fluidRow( infoBoxOutput("alcohol"),
                                          infoBoxOutput("gamble"),
                                          infoBoxOutput("smart")
                                ),
                                fluidRow(valueBoxOutput("jungchong"))
                                
                                
                            )))
                    
                    
                    
                    
                    
                    
                    
                ) 
                
                
            )    
        }
        else {
            dashboardBody( theme_flat_red,
                           login
                           
            )  
            
            
        }
    })
    
    observeEvent(input$jumpToP2, {
        updateTabsetPanel(session, "tabset",
                          selected = "two")
    }) 
    observeEvent(input$jumpToP3, {
        updateTabsetPanel(session, "tabset",
                          selected = "three")
    }) 
    observeEvent(input$jumpToP4, {
        updateTabsetPanel(session, "tabset",
                          selected = "four")
    }) 
    observeEvent(input$jumpToP5, {
        updateTabsetPanel(session, "tabset",
                          selected = "five")
    }) 
    
    
    observeEvent(input$ju1, {
        updateTabsetPanel(session, "tabset",
                          selected = "one")
    }) 
    observeEvent(input$ju2, {
        updateTabsetPanel(session, "tabset", 
                          selected = "two")
    })
    observeEvent(input$ju3, {
        updateTabsetPanel(session, "tabset", 
                          selected = "three")
    })
    
    observeEvent(input$ju4, {
        updateTabsetPanel(session, "tabset",
                          selected = "four")
    })
    
    observeEvent(input$ju5, {
        updateTabsetPanel(session, "tabset2",
                          selected = "al")
    }) 
    
    observeEvent(input$ju6, {
        updateTabsetPanel(session, "tabset2",
                          selected = "ccc4")
    }) 
    observeEvent(input$ju7, {
        updateTabsetPanel(session, "tabset2",
                          selected = "di")
    }) 
    observeEvent(input$ju8, {
        updateTabsetPanel(session, "tabset2",
                          selected = "sm")
    }) 
    observeEvent(input$ju9, {
        updateTabsetPanel(session, "tabset2",
                          selected = "ccc2")
    }) 
    observeEvent(input$jung1, {
        updateTabsetPanel(session, "tabset2",
                          selected = "ccc4")
    }) 
    observeEvent(input$ton, {
        updateTabsetPanel(session, "tabset2",
                          selected = "di")
    }) 
    observeEvent(input$jung2, {
        updateTabsetPanel(session, "tabset2",
                          selected = "sm")
    }) 
    observeEvent(input$jung4, {
        updateTabsetPanel(session, "tabset2",
                          selected = "ccc2")
    }) 
    observeEvent(input$butt, {
        updateTabsetPanel(session, "tabset2",
                          selected = "ccc1")
    }) 
    
    
    
    
    output$b1 <- renderText({
        input$a1
    })
    output$b2 <- renderText({
        input$a2
    })
    output$b3 <- renderText({
        input$a3
    })
    output$b4 <- renderText({
        input$a4
    })
    output$b5 <- renderText({
        input$a5
    })
    output$b6 <- renderText({
        input$a6
    })
    output$b7 <- renderText({
        input$a7
    })
    output$b8 <- renderText({
        input$a8
    })
    output$b9 <- renderText({
        input$a9
    })
    output$b10 <- renderText({
        input$a10
    })
    
    
    
    
    output$one <- renderText({paste0(input$name, input$age, input$sex, input$checkbox)  })
    
    output$two <- renderText({ paste0(input$a,input$b,input$c,input$d,input$e,input$f,input$g,input$h,input$i,input$j)  })
    
    
    output$three <- renderText({ paste0(input$k, input$radiobtn,input$n,input$m, input$o,input$p,input$radiobtn1,input$radiobtn2,input$radiobtn3,input$radiobtn4)  })
    
    
    output$four <- renderText({ paste0(input$radiobtn5,input$radiobtn6,input$radiobtn7,input$radiobtn8,
                                       input$radiobtn9, input$radiobtn10, input$radiobtn11, input$radiobtn12,input$radiobtn13)})
    
    
    output$five <- renderText({ paste0(input$dddd, input$aa, input$ab,input$ac,input$radiobtn14, input$ae,
                                       input$radiobtn15,input$radiobtn16, input$radiobtn17) })
    
    
    output$six <- renderText({ paste0(input$o1,input$o2,input$o3,input$o4,input$o5,input$o6) })
    
    
    output$seven <- renderText({ paste0(input$a11,input$a1,input$a2,input$a3,input$a4,input$a5,input$a6,input$a7,
                                        input$a8,input$a9,input$a10)})
    
    
    output$seven1 <- renderText({ paste0(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9) })
    
    
    
    observeEvent(input$bu, {
        
        shinyalert(title = "입력 완료", type = "success")
    })    
    observeEvent(input$jumpToP6, {
        
        shinyalert(title = "입력 완료", type = "success")
    })    
    
    agegp = 0
    output$age <- renderText({
        if(input$age %in% c(1,2,3,4)){
            agegp = 1
        }else if(input$age %in% c(5,6,7,8,9)){
            agegp = 2
        }
        
        else if(input$age %in% c(10,11,12,13,14)){
            agegp = 3
        }
        
        else if(input$age %in% c(15,16,17,18,19)){
            agegp = 4
        }
        
        
        else if(input$age %in% c(20,21,22,23,24)){
            agegp = 5
        }
        
        
        else if(input$age %in% c(25,26,27,28,29)){
            agegp = 6
        }
        
        
        else if(input$age %in% c(30,31,32,33,34)){
            agegp = 7
        }
        
        else if(input$age %in% c(35,36,37,38,39)){
            agegp = 8
        }
        
        else if(input$age %in% c(40,41,42,43,44)){
            agegp = 9
        }
        
        else if(input$age %in% c(45,46,47,48,49)){
            agegp = 10
        }
        
        else if(input$age %in% c(50,51,52,53,54)){
            agegp = 11
        }
        
        else if(input$age %in% c(55,56,57,58,59)){
            agegp = 12
        }
        
        else if(input$age %in% c(60,61,62,63,64)){
            agegp = 13
        }
        
        else if(input$age %in% c(65,66,67,68,69)){
            agegp = 14
        }
        
        
        else if(input$age %in% c(70,71,72,73,74)){
            agegp = 15
        }
        
        
        
        else if(input$age %in% c(75,76,77,78,79)){
            agegp = 16
        }
        
        
        
        
        else if(input$age %in% c(80,81,82,83,84)){
            agegp = 17
        }
        
        
        
        else if(input$age >=85){
            agegp = 18
        }
        
        
        
        
    }) 
    
    
    
    
    
    ###총점
    
    
    
    output$tttt <- renderValueBox({
        valueBox(
            renderText({paste0(round({
                bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)*0.193 + 
                    
                    dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)*0.201 + 
                    
                    obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)*0.143 +
                    
                    ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)*0.153 +
                    
                    gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)*0.119 +  
                    
                    sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn, input$n, input$b, input$a)*0.191}
                ,1), "점")}
            ), renderText({if(
                round({
                    bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)*0.193 + 
                        
                        dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)*0.201 + 
                        
                        obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)*0.143 +
                        
                        ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)*0.153 +
                        
                        gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)*0.119 +  
                        
                        sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn, input$n, input$b, input$a)*0.191}
                    ,1)
                <67.7){return('위험')}
                
                else if(
                    round({
                        bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)*0.193 + 
                            
                            dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)*0.201 + 
                            
                            obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)*0.143 +
                            
                            ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)*0.153 +
                            
                            gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)*0.119 +  
                            
                            sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn, input$n, input$b, input$a)*0.191}
                        ,1)
                    >75.8){return('정상')}
                
                
                else{return('안심')}}), icon = icon({if(
                    round({
                        bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)*0.193 + 
                            
                            dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)*0.201 + 
                            
                            obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)*0.143 +
                            
                            ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)*0.153 +
                            
                            gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)*0.119 +  
                            
                            sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn, input$n, input$b, input$a)*0.191}
                        ,1)
                    <67.7){("frown")}
                    
                    else if(
                        round({
                            bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)*0.193 + 
                                
                                dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)*0.201 + 
                                
                                obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)*0.143 +
                                
                                ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)*0.153 +
                                
                                gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)*0.119 +  
                                
                                sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn, input$n, input$b, input$a)*0.191}
                            ,1)
                        >75.8){("grin-hearts")}
                    
                    
                    else{("meh")}
                }),
            color = ({if(
                round({
                    bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)*0.193 + 
                        
                        dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)*0.201 + 
                        
                        obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)*0.143 +
                        
                        ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)*0.153 +
                        
                        gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)*0.119 +  
                        
                        sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn, input$n, input$b, input$a)*0.191}
                    ,1)
                <67.7){("maroon")}
                
                else if(
                    round({
                        bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)*0.193 + 
                            
                            dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)*0.201 + 
                            
                            obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)*0.143 +
                            
                            ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)*0.153 +
                            
                            gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)*0.119 +  
                            
                            sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn, input$n, input$b, input$a)*0.191}
                        ,1)
                    >75.8){("green")}
                
                
                else{("yellow")}
            })
        )
    })
    
    
    
    ###고혈압
    
    output$bbbb <- renderValueBox({
        valueBox(
            renderText({paste0(bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c), "점")
            }
            
            ), renderText({if(
                bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)
                <55.5){return('평균 이하입니다')}
                
                else if(
                    bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)
                    >75.8){return('평균 이상입니다')}
                
                
                else{return('평균입니다')}}), icon = icon({if(
                    bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)
                    <55.5){("frown")}
                    
                    else if(
                        bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)
                        >75.8){("grin-hearts")}
                    
                    
                    else{("meh")}
                }),
            color = ({if(
                bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)
                <55.5){("maroon")}
                
                else if(
                    bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)
                    >75.8){("green")}
                
                
                else{("yellow")}
            })
        )
    })
    
    
    
    
    ###당뇨  
    
    output$ddddd <- renderValueBox({
        valueBox(
            renderText({paste0(dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h), "점")
            }
            ), renderText({if(
                dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)
                <75.1){return('평균 이하입니다')}
                
                else if(
                    dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)
                    >83.5){return('평균 이상입니다')}
                
                
                else{return('평균입니다')}
            }) 
            
            ,icon = icon({if(
                dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)
                <75.1){("frown")}
                
                else if(
                    dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)
                    >83.5){("grin-hearts")}
                
                
                else{("meh")}
            }),
            color = ({if(
                dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)
                <75.1){("maroon")}
                
                else if(
                    dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)
                    >83.5){("green")}
                
                
                else{("yellow")}
            })
        )
    })
    
    
    ###비만
    
    
    output$oooo <- renderValueBox({
        valueBox(
            renderText({paste0(obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k), "점")
            }
            )
            , renderText({if(
                obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)
                <66.2){return('평균 이하입니다')}
                
                else if(
                    obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)
                    >76.1){return('평균 이상입니다')}
                
                
                else{return('평균입니다')}
            }) 
            
            ,icon = icon({if(
                obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)
                <66.2){("frown")}
                
                else if(
                    obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)
                    >76.1){("grin-hearts")}
                
                
                else{("meh")}
            }),
            color = ({if(
                obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)
                <66.2){("maroon")}
                
                else if(
                    obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)
                    >76.1){("green")}
                
                
                else{("yellow")}
            })
        )
    })
    
    
    ###간
    
    
    
    output$gggg <- renderValueBox({
        valueBox(
            renderText({paste0(ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5), "점") 
            }
            )
            , renderText({if(
                ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)
                <69.6){return('평균 이하입니다')}
                
                else if(
                    ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)
                    >86.6){return('평균 이상입니다')}
                
                
                else{return('평균입니다')}})
            ,icon = icon({if(
                ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)
                <69.6){("frown")}
                
                else  if(
                    ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)
                    >86.6){("grin-hearts")}
                
                
                else{("meh")}}),
            color = ({if(
                ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)
                <69.6){("maroon")}
                
                else if(
                    ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)
                    >86.6){("green")}
                
                
                else{("yellow")}
            })
        )
    })
    
    
    
    
    
    ###고지혈증
    
    
    output$jjjj <- renderValueBox({
        valueBox(
            renderText({paste0(gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a), "점")
            }
            )
            , renderText({if(
                gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)
                <57.4){return('평균 이하입니다')}
                
                else if(
                    gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)
                    >65.2){return('평균 이상입니다')}
                
                
                else{return('평균입니다')}
            }) 
            
            ,icon = icon({if(
                gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)
                <57.4){("frown")}
                
                else if(
                    gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)
                    >65.2){("grin-hearts")}
                
                
                else{("meh")}
            }),
            color = ({if(
                gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)
                <57.4){("maroon")}
                
                else if(
                    gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)
                    >65.2){("green")}
                
                
                else{("yellow")}
            })
        )
    })
    
    
    
    
    
    
    ###신장
    
    
    output$ssss <- renderValueBox({
        valueBox(
            renderText({paste0(sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn,
                                        input$n, input$b, input$a), "점")
            }
            )
            , renderText({if(
                sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn,
                         input$n, input$b, input$a)
                <75.1){return('평균 이하입니다')}
                
                else if(
                    sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn,
                             input$n, input$b, input$a)
                    >83.5){return('평균 이상입니다')}
                
                
                else{return('평균입니다')}
            }) 
            
            ,icon = icon({if(
                sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn,
                         input$n, input$b, input$a)
                <75.1){("frown")}
                
                else if(
                    sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn,
                             input$n, input$b, input$a)
                    >83.5){("grin-hearts")}
                
                
                else{("meh")}
            }),
            color = ({if(
                sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn,
                         input$n, input$b, input$a)
                <75.1){("maroon")}
                
                else if(
                    sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn,
                             input$n, input$b, input$a)
                    >83.5){("green")}
                
                
                else{("yellow")}
            })
        )
    })
    
    #####################################################################################################################################################################################################################################################################################################    
    
    
    output$tttt1 <-renderValueBox({
        valueBox(
            renderText({paste0(round({
                bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)*0.193 + 
                    
                    dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)*0.201 + 
                    
                    obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)*0.143 +
                    
                    ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)*0.153 +
                    
                    gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)*0.119 +  
                    
                    sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn, input$n, input$b, input$a)*0.191}
                ,1), "점")}
            ), renderText({if(
                round({
                    bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)*0.193 + 
                        
                        dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)*0.201 + 
                        
                        obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)*0.143 +
                        
                        ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)*0.153 +
                        
                        gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)*0.119 +  
                        
                        sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn, input$n, input$b, input$a)*0.191}
                    ,1)
                <67.7){return('위험')}
                
                else if(
                    round({
                        bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)*0.193 + 
                            
                            dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)*0.201 + 
                            
                            obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)*0.143 +
                            
                            ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)*0.153 +
                            
                            gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)*0.119 +  
                            
                            sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn, input$n, input$b, input$a)*0.191}
                        ,1)
                    >75.8){return('정상')}
                
                
                else{return('안심')}}), icon = icon({if(
                    round({
                        bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)*0.193 + 
                            
                            dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)*0.201 + 
                            
                            obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)*0.143 +
                            
                            ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)*0.153 +
                            
                            gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)*0.119 +  
                            
                            sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn, input$n, input$b, input$a)*0.191}
                        ,1)
                    <67.7){("frown")}
                    
                    else if(
                        round({
                            bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)*0.193 + 
                                
                                dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)*0.201 + 
                                
                                obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)*0.143 +
                                
                                ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)*0.153 +
                                
                                gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)*0.119 +  
                                
                                sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn, input$n, input$b, input$a)*0.191}
                            ,1)
                        >75.8){("grin-hearts")}
                    
                    
                    else{("meh")}
                }),
            color = ({if(
                round({
                    bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)*0.193 + 
                        
                        dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)*0.201 + 
                        
                        obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)*0.143 +
                        
                        ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)*0.153 +
                        
                        gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)*0.119 +  
                        
                        sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn, input$n, input$b, input$a)*0.191}
                    ,1)
                <67.7){("maroon")}
                
                else if(
                    round({
                        bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)*0.193 + 
                            
                            dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)*0.201 + 
                            
                            obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)*0.143 +
                            
                            ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)*0.153 +
                            
                            gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)*0.119 +  
                            
                            sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn, input$n, input$b, input$a)*0.191}
                        ,1)
                    >75.8){("green")}
                
                
                else{("yellow")}
            })
        )
    })
    
    
    
    
    
    
    output$bbbb1 <- renderInfoBox({
        infoBox(
            "고혈압 점수",
            renderText({paste0(bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c), "점")
            }
            
            ), icon = icon({if(
                bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)
                <55.5){("frown")}
                
                else if(
                    bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)
                    >75.8){("grin-hearts")}
                
                
                else{("meh")}
            }),
            color = ({if(
                bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)
                <55.5){("maroon")}
                
                else if(
                    bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)
                    >75.8){("green")}
                
                
                else{("yellow")}
            })
        )
    })
    
    
    
    
    
    
    #dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)
    
    ###당뇨  
    
    output$ddddd1 <- renderInfoBox({
        infoBox(
            "당뇨 점수", renderText({paste0(dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h), "점")
            }
            ) 
            
            ,icon = icon({if(
                dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)
                <75.1){("frown")}
                
                else if(
                    dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)
                    >83.5){("grin-hearts")}
                
                
                else{("meh")}
            }),
            color = ({if(
                dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)
                <75.1){("maroon")}
                
                else if(
                    dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)
                    >83.5){("green")}
                
                
                else{("yellow")}
            })
            ,fill = TRUE)
    })
    
    
    ###비만
    
    
    
    
    output$oooo1 <- renderInfoBox({
        infoBox(
            "비만 점수"
            , renderText({paste0(obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k), "점")
            }
            ) 
            
            ,icon = icon({if(
                obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)
                <66.2){("frown")}
                
                else if(
                    obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)
                    >76.1){("grin-hearts")}
                
                
                else{("meh")}
            }),
            color = ({if(
                obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)
                <66.2){("maroon")}
                
                else if(
                    obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)
                    >76.1){("green")}
                
                
                else{("yellow")}
            })
            ,fill = TRUE )
    })
    
    
    ###간
    
    output$gggg1 <- renderInfoBox({
        infoBox(
            "간 점수"
            , renderText({paste0(ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5), "점") 
            }
            )
            ,icon = icon({if(
                ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)
                <69.6){("frown")}
                
                else  if(
                    ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)
                    >86.6){("grin-hearts")}
                
                
                else{("meh")}}),
            color = ({if(
                ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)
                <69.6){("maroon")}
                
                else if(
                    ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)
                    >86.6){("green")}
                
                
                else{("yellow")}
            })
        )
    })
    
    
    
    
    
    ###고지혈증
    
    
    
    output$jjjj1 <- renderInfoBox({
        infoBox(
            "고지혈증 점수"
            ,renderText({paste0(gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a), "점")
            }
            ) 
            
            ,icon = icon({if(
                gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)
                <57.4){("frown")}
                
                else if(
                    gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)
                    >65.2){("grin-hearts")}
                
                
                else{("meh")}
            }),
            color = ({if(
                gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)
                <57.4){("maroon")}
                
                else if(
                    gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)
                    >65.2){("green")}
                
                
                else{("yellow")}
            })
            ,fill = TRUE)
    })
    
    
    
    
    ###신장
    
    
    
    
    
    
    
    
    
    output$ssss1 <- renderInfoBox({
        infoBox(
            "신장 점수"
            , renderText({paste0(sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn,
                                          input$n, input$b, input$a), "점")
            }
            ) 
            
            ,icon = icon({if(
                sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn,
                         input$n, input$b, input$a)
                <75.1){("frown")}
                
                else if(
                    sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn,
                             input$n, input$b, input$a)
                    >83.5){("grin-hearts")}
                
                
                else{("meh")}
            }),
            color = ({if(
                sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn,
                         input$n, input$b, input$a)
                <75.1){("maroon")}
                
                else if(
                    sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn,
                             input$n, input$b, input$a)
                    >83.5){("green")}
                
                
                else{("yellow")}
            })
        )
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    output$save <-  downloadHandler(
        filename = function() { paste('나의 건강 점수',Sys.Date(), '.csv', sep='') },
        content = function(file){
            write.csv(data.frame(날짜 = Sys.Date(),
                                   고혈압점수 = bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c),
                                   당뇨점수 = dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h),
                                   비만점수 = obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k),
                                   고지혈증점수 = gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a),
                                   간점수 = ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5),
                                   신장점수 = sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn, input$n, input$b, input$a),
                                   종합점수 = round({
                                       bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)*0.193 + 
                                           
                                           dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)*0.201 + 
                                           
                                           obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)*0.143 +
                                           
                                           ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)*0.153 +
                                           
                                           gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)*0.119 +  
                                           
                                           sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn, input$n, input$b, input$a)*0.191}
                                       ,1)
            ), file)}) 
    
    
    USER <<- reactiveValues(Logged = Logged)
    
    observe({ 
        if (USER$Logged == FALSE) {
            if (!is.null(input$Login)) {
                if (input$Login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    Id.username <- which(my_username == Username)
                    Id.password <- which(my_password == Password)
                    if (length(Id.username) > 0 & length(Id.password) > 0) {
                        if (Id.username == Id.password) {
                            USER$Logged <<- TRUE
                        } 
                    }
                } 
            }
        }    
    })
    
    
    
    
    output$wwww <- renderInfoBox({
        infoBox(
            "우울 점수",
            renderText({if(woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)>=20){return('심한 증상')}
                
                else if(woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)<20 & woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)>=15 ){return('중한 증상')}
                
                else if(woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)<15 & woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)>=10 ){return('경한 증상')}
                
                else if(woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)<10 & woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)>=5 ){return('가벼운 증상')}
                
                else{return('우울 아님')}
            })
            
            , icon = icon({if(woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)>=20){("frown")}
                
                else if(woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)<20 & woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)>=15 ){("frown")}
                
                else if(woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)<15 & woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)>=10 ){("meh")}
                
                else if(woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)<10 & woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)>=5 ){("meh")}
                
                else{("grin-hearts")}
            }),
            color = ({if(woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)>=20){("maroon")}
                
                else if(woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)<20 & woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)>=15 ){("maroon")}
                
                else if(woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)<15 & woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)>=10 ){("yellow")}
                
                else if(woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)<10 & woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)>=5 ){("yellow")}
                
                else{("green")}
            })
        )
    })
    
    
    
    
    
    
    
    output$bulan <-renderInfoBox({
        infoBox(
            "불안 점수",
            renderText({if(bulfunc(input$w1,input$w2,input$w3,input$w4,input$w5,input$w6,input$w7)>6){return('심한 의존')}
                else if(bulfunc(input$w1,input$w2,input$w3,input$w4,input$w5,input$w6,input$w7)<3){return('중독문제 없음')}   
                else{return('경한 의존')}
            }), icon = icon({if(bulfunc(input$w1,input$w2,input$w3,input$w4,input$w5,input$w6,input$w7)>6){("frown")}
                else if(bulfunc(input$w1,input$w2,input$w3,input$w4,input$w5,input$w6,input$w7)<3){("grin-hearts")}   
                else{("meh")}
            }),
            color = ({if(bulfunc(input$w1,input$w2,input$w3,input$w4,input$w5,input$w6,input$w7)>6){("maroon")}
                else if(bulfunc(input$w1,input$w2,input$w3,input$w4,input$w5,input$w6,input$w7)<3){("green")}   
                else{("yellow")}
            })
        )
    })
    output$nic <-renderInfoBox({
        infoBox(
            "니코틴 의존 점수",
            renderText({if(nicfunc(input$o1,input$o2,input$o3,input$o4,input$o5,input$o6)>6){return('심한 의존')}
                else if(nicfunc(input$o1,input$o2,input$o3,input$o4,input$o5,input$o6)<3){return('중독문제 없음')}   
                else{return('경한 의존')}
            }), icon = icon({if(nicfunc(input$o1,input$o2,input$o3,input$o4,input$o5,input$o6)>6){("frown")}
                else if(nicfunc(input$o1,input$o2,input$o3,input$o4,input$o5,input$o6)<3){("grin-hearts")}   
                else{("meh")}
            }),
            color = ({if(nicfunc(input$o1,input$o2,input$o3,input$o4,input$o5,input$o6)>6){("maroon")}
                else if(nicfunc(input$o1,input$o2,input$o3,input$o4,input$o5,input$o6)<3){("green")}   
                else{("yellow")}
            })
        )
    })
    
    output$smart <- renderInfoBox({
        infoBox(
            "스마트폰 의존 점수",
            renderText({if(smfunc(input$sm1,input$sm2,input$sm3,input$sm4,input$sm5,input$sm6,input$sm7,input$sm8,input$sm9,input$sm10,input$sm11,input$sm12,input$sm13,input$sm14,input$sm15)>44){return('고위험')}
                else if(smfunc(input$sm1,input$sm2,input$sm3,input$sm4,input$sm5,input$sm6,input$sm7,input$sm8,input$sm9,input$sm10,input$sm11,input$sm12,input$sm13,input$sm14,input$sm15)<42){return('정상 사용자')}   
                else{return('주의')}
            }), icon = icon({if(smfunc(input$sm1,input$sm2,input$sm3,input$sm4,input$sm5,input$sm6,input$sm7,input$sm8,input$sm9,input$sm10,input$sm11,input$sm12,input$sm13,input$sm14,input$sm15)>44){("frown")}
                else if(smfunc(input$sm1,input$sm2,input$sm3,input$sm4,input$sm5,input$sm6,input$sm7,input$sm8,input$sm9,input$sm10,input$sm11,input$sm12,input$sm13,input$sm14,input$sm15)<42){("grin-hearts")}   
                else{("meh")}
            }),
            color = ({if(smfunc(input$sm1,input$sm2,input$sm3,input$sm4,input$sm5,input$sm6,input$sm7,input$sm8,input$sm9,input$sm10,input$sm11,input$sm12,input$sm13,input$sm14,input$sm15)>44){("maroon")}
                else if(smfunc(input$sm1,input$sm2,input$sm3,input$sm4,input$sm5,input$sm6,input$sm7,input$sm8,input$sm9,input$sm10,input$sm11,input$sm12,input$sm13,input$sm14,input$sm15)<42){("green")}   
                else{("yellow")}
            })
        )
    })
    
    output$gamble <- renderInfoBox({
        infoBox(
            "도박 중독 점수",
            renderText({if(dofunc(input$di1,input$di2,input$di3,input$di4,input$di5,input$di6,input$di7,input$di8,input$di9)>9){return('높은 위험')}
                else if(dofunc(input$di1,input$di2,input$di3,input$di4,input$di5,input$di6,input$di7,input$di8,input$di9)==0){return('중독문제 없음')}   
                else{return('낮은 위험')}
            }), icon = icon({if(dofunc(input$di1,input$di2,input$di3,input$di4,input$di5,input$di6,input$di7,input$di8,input$di9)>9){("frown")}
                else if(dofunc(input$di1,input$di2,input$di3,input$di4,input$di5,input$di6,input$di7,input$di8,input$di9)==0){("grin-hearts")}   
                else{("meh")}
            }),
            color = ({if(dofunc(input$di1,input$di2,input$di3,input$di4,input$di5,input$di6,input$di7,input$di8,input$di9)>9){("maroon")}
                else if(dofunc(input$di1,input$di2,input$di3,input$di4,input$di5,input$di6,input$di7,input$di8,input$di9)==0){("green")}   
                else{("yellow")}
            })
        )
    })
    
    output$alcohol <- renderInfoBox({
        infoBox(
            "알코올 의존 점수",
            renderText({if(alfunc(input$al1,input$al2,input$al3,input$al4,input$al5,input$al6,input$al7,input$al8,input$al9,input$al10)>25){return('알코올 사용 장애')}
                else if(alfunc(input$al1,input$al2,input$al3,input$al4,input$al5,input$al6,input$al7,input$al8,input$al9,input$al10)<10){return('정상 음주')}   
                else{return('위험 음주')}
            }), icon = icon({if(alfunc(input$al1,input$al2,input$al3,input$al4,input$al5,input$al6,input$al7,input$al8,input$al9,input$al10)>25){("frown")}
                else if(alfunc(input$al1,input$al2,input$al3,input$al4,input$al5,input$al6,input$al7,input$al8,input$al9,input$al10)<10){("grin-hearts")}   
                else{("meh")}
            }),
            color = ({if(alfunc(input$al1,input$al2,input$al3,input$al4,input$al5,input$al6,input$al7,input$al8,input$al9,input$al10)>25){("maroon")}
                else if(alfunc(input$al1,input$al2,input$al3,input$al4,input$al5,input$al6,input$al7,input$al8,input$al9,input$al10)<10){("green")}   
                else{("yellow")}
            })
        )
    })
    
    
    
    output$jungchong <- renderValueBox({
        valueBox(renderText({paste0(round({
            alscore(input$al1,input$al2,input$al3,input$al5,input$al7,input$al8,input$al9,input$al6,input$sm13)*0.216 + 
                
                doscore(input$di6,input$di7,input$di9,input$di2,input$di1,input$di3,input$di5)*0.214 + 
                
                smscore(input$sm1,input$sm2,input$sm5,input$sm7,input$sm10,input$sm13,input$sm11,input$al9)*0.08 +
                
                ((-100/10)*nicfunc(input$o1,input$o2,input$o3,input$o4,input$o5,input$o6)+100)*0.1 +
                
                ((-100/21)*bulfunc(input$w1,input$w2,input$w3,input$w4,input$w5,input$w6,input$w7)+100)*0.192 +  
                
                ((-100/27)*woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)+100)*0.198}
            ,1), "점")}
        ),
        
        renderText({if(
            round({
                alscore(input$al1,input$al2,input$al3,input$al5,input$al7,input$al8,input$al9,input$al6,input$sm13)*0.216 + 
                    
                    doscore(input$di6,input$di7,input$di9,input$di2,input$di1,input$di3,input$di5)*0.214 + 
                    
                    smscore(input$sm1,input$sm2,input$sm5,input$sm7,input$sm10,input$sm13,input$sm11,input$al9)*0.08 +
                    
                    ((-100/10)*nicfunc(input$o1,input$o2,input$o3,input$o4,input$o5,input$o6)+100)*0.1 +
                    
                    ((-100/21)*bulfunc(input$w1,input$w2,input$w3,input$w4,input$w5,input$w6,input$w7)+100)*0.192 +  
                    
                    ((-100/27)*woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)+100)*0.198}
                ,1)
            <67.7){return('위험')}
            
            else if(
                round({
                    alscore(input$al1,input$al2,input$al3,input$al5,input$al7,input$al8,input$al9,input$al6,input$sm13)*0.216 + 
                        
                        doscore(input$di6,input$di7,input$di9,input$di2,input$di1,input$di3,input$di5)*0.214 + 
                        
                        smscore(input$sm1,input$sm2,input$sm5,input$sm7,input$sm10,input$sm13,input$sm11,input$al9)*0.08 +
                        
                        ((-100/10)*nicfunc(input$o1,input$o2,input$o3,input$o4,input$o5,input$o6)+100)*0.1 +
                        
                        ((-100/21)*bulfunc(input$w1,input$w2,input$w3,input$w4,input$w5,input$w6,input$w7)+100)*0.192 +  
                        
                        ((-100/27)*woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)+100)*0.198}
                    ,1)
                >75.8){return('정상')}
            
            
            else{return('안심')}}),
        icon = icon({if(
            round({
                alscore(input$al1,input$al2,input$al3,input$al5,input$al7,input$al8,input$al9,input$al6,input$sm13)*0.216 + 
                    
                    doscore(input$di6,input$di7,input$di9,input$di2,input$di1,input$di3,input$di5)*0.214 + 
                    
                    smscore(input$sm1,input$sm2,input$sm5,input$sm7,input$sm10,input$sm13,input$sm11,input$al9)*0.08 +
                    
                    ((-100/10)*nicfunc(input$o1,input$o2,input$o3,input$o4,input$o5,input$o6)+100)*0.1 +
                    
                    ((-100/21)*bulfunc(input$w1,input$w2,input$w3,input$w4,input$w5,input$w6,input$w7)+100)*0.192 +  
                    
                    ((-100/27)*woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)+100)*0.198}
                ,1)
            <75.1){("frown")}
            
            else if(
                round({
                    alscore(input$al1,input$al2,input$al3,input$al5,input$al7,input$al8,input$al9,input$al6,input$sm13)*0.216 + 
                        
                        doscore(input$di6,input$di7,input$di9,input$di2,input$di1,input$di3,input$di5)*0.214 + 
                        
                        smscore(input$sm1,input$sm2,input$sm5,input$sm7,input$sm10,input$sm13,input$sm11,input$al9)*0.08 +
                        
                        ((-100/10)*nicfunc(input$o1,input$o2,input$o3,input$o4,input$o5,input$o6)+100)*0.1 +
                        
                        ((-100/21)*bulfunc(input$w1,input$w2,input$w3,input$w4,input$w5,input$w6,input$w7)+100)*0.192 +  
                        
                        ((-100/27)*woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)+100)*0.198}
                    ,1)
                >83.5){("grin-hearts")}
            
            
            else{("meh")}
        }),
        color = ({if(
            round({
                alscore(input$al1,input$al2,input$al3,input$al5,input$al7,input$al8,input$al9,input$al6,input$sm13)*0.216 + 
                    
                    doscore(input$di6,input$di7,input$di9,input$di2,input$di1,input$di3,input$di5)*0.214 + 
                    
                    smscore(input$sm1,input$sm2,input$sm5,input$sm7,input$sm10,input$sm13,input$sm11,input$al9)*0.08 +
                    
                    ((-100/10)*nicfunc(input$o1,input$o2,input$o3,input$o4,input$o5,input$o6)+100)*0.1 +
                    
                    ((-100/21)*bulfunc(input$w1,input$w2,input$w3,input$w4,input$w5,input$w6,input$w7)+100)*0.192 +  
                    
                    ((-100/27)*woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)+100)*0.198}
                ,1)
            <75.1){("maroon")}
            
            else if(
                round({
                    alscore(input$al1,input$al2,input$al3,input$al5,input$al7,input$al8,input$al9,input$al6,input$sm13)*0.216 + 
                        
                        doscore(input$di6,input$di7,input$di9,input$di2,input$di1,input$di3,input$di5)*0.214 + 
                        
                        smscore(input$sm1,input$sm2,input$sm5,input$sm7,input$sm10,input$sm13,input$sm11,input$al9)*0.08 +
                        
                        ((-100/10)*nicfunc(input$o1,input$o2,input$o3,input$o4,input$o5,input$o6)+100)*0.1 +
                        
                        ((-100/21)*bulfunc(input$w1,input$w2,input$w3,input$w4,input$w5,input$w6,input$w7)+100)*0.192 +  
                        
                        ((-100/27)*woolfunc(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9)+100)*0.198}
                    ,1)
                >83.5){("green")}
            
            
            else{("yellow")}
        })
        )
    })
    
    
    #####plots#############
    
    
    output$boxplot1 <- renderPlot({
        data_frame(tot = oo$tot) %>%
            ggplot(., aes(tot)) + xlab("") + ylab("") + theme_minimal() + scale_y_continuous(breaks = NULL) +
            geom_density(color="plum", fill="pink", alpha=0.75) + 
            scale_x_continuous(limits = c(0,100)) +
            theme_classic() +
            geom_vline(aes(xintercept=round({
                bpscore(input$d,input$e,input$age,input$sex,input$radiobtn4,input$k,input$radiobtn15,input$h,input$b,input$a,input$c)*0.193 + 
                    
                    dangscore(input$sex,input$age,input$f,input$radiobtn4,input$g,input$b,input$a,input$radiobtn11,input$o,input$radiobtn17,input$radiobtn,input$h)*0.201 + 
                    
                    obesscore(input$b, input$sex, input$c, input$age, input$o, input$d, input$h, input$g, input$e, input$k)*0.143 +
                    
                    ganscore(input$sex,input$age,input$p,input$o,input$m,input$radiobtn7,input$radiobtn14,input$ae,input$c,input$radiobtn5)*0.153 +
                    
                    gojiscore(input$g,input$i,input$p,input$f,input$e,input$radiobtn5,input$radiobtn14,input$ac,input$radiobtn2,input$k,input$radiobtn17,input$age, input$b, input$a)*0.119 +  
                    
                    sinscore(input$sex, input$radiobtn4, input$age, input$g, input$radiobtn11, input$radiobtn, input$n, input$b, input$a)*0.191}
                ,1)),
                color="red", linetype="solid", size=1)})
    
    
    
    
    
    
    output$boxplot2 <- renderPlot({
        ggplot(yy, aes(x=1, y = BP_HIGH)) + 
            xlab("") + ylab("") +
            theme_minimal() +
            geom_boxplot(fill="pink", colour="black", outlier.color = 'black',outlier.shape = 20)+ 
            ylim(50,200) +
            scale_x_continuous(breaks = NULL) +
            geom_hline(aes(yintercept=input$d),
                       color="red", linetype="solid", size=1)})
    
    output$box2 <- renderPlot({
        ggplot(yy, aes(x=1, y = BP_LWST)) + 
            xlab("") + ylab(" ") + 
            theme_minimal() +
            geom_boxplot(fill="pink", colour="black", outlier.color = 'black',outlier.shape = 20)+ 
            ylim(30,130) +
            scale_x_continuous(breaks = NULL) +
            geom_hline(aes(yintercept=input$e),
                       color="red", linetype="solid", size=1)})
    
    
    output$boxplot3 <- renderPlot({
        ggplot(yy, aes(x=1, y = BLDS)) + 
            xlab("") + ylab(" ")+
            geom_boxplot(fill="pink", colour="black", outlier.color = 'black',outlier.shape = 20)+ 
            ylim(25,230) + theme_minimal() +
            scale_x_continuous(breaks = NULL) +
            geom_hline(aes(yintercept=input$f),
                       color="red", linetype="solid", size=1)})
    
    output$box3 <- renderPlot({
        ggplot(yy, aes(x=0, y = TOT_CHOLE)) + 
            xlab("") + ylab(" ")+
            geom_boxplot(fill="pink", colour="black",   outlier.color = 'black',outlier.shape = 20)+ 
            ylim(50,500) + theme_minimal() +
            scale_x_continuous(breaks = NULL) +
            geom_hline(aes(yintercept=input$g),
                       color="red", linetype="solid", size=1)})
    
    output$boxplot4 <- renderPlot({
        ggplot(yy, aes(x=0, y = BMI)) + 
            xlab("") + ylab(" ")+
            geom_boxplot(fill="pink", colour="black",   outlier.color = 'black',outlier.shape = 20)+ 
            ylim(10,50) + theme_minimal() + 
            scale_x_continuous(breaks = NULL) +
            geom_hline(aes(yintercept=(input$b/(input$a*input$a)*10000)),
                       color="red", linetype="solid", size=1)})
    
    output$box4 <- renderPlot({
        ggplot(yy, aes(x=0, y = WAIST)) + 
            xlab("") + ylab(" ")+ theme_minimal() +
            geom_boxplot(fill="pink", colour="black",   outlier.color = 'black',outlier.shape = 20) +
            scale_x_continuous(breaks = NULL) +
            geom_hline(aes(yintercept=input$c),
                       color="red", linetype="solid", size=1)})
    
    output$boxplot5 <- renderPlot({
        ggplot(yy, aes(x=0, y = SGOT_AST)) + 
            xlab("") + ylab(" ")+ theme_minimal() +
            geom_boxplot(fill="pink", colour="black",   outlier.color = 'black',outlier.shape = 20) + 
            ylim(0,100) +
            scale_x_continuous(breaks = NULL) +
            geom_hline(aes(yintercept=input$m),
                       color="red", linetype="solid", size=1)})
    
    output$box5 <- renderPlot({
        ggplot(yy, aes(x=0, y = SGPT_ALT)) + 
            xlab("혈청 지피티(ALT)") + ylab(" ")+ theme_minimal() +
            geom_boxplot(fill="pink", colour="black",   outlier.color = 'black',outlier.shape = 20) + 
            ylim(0,100) +
            scale_x_continuous(breaks = NULL) +
            geom_hline(aes(yintercept=input$o),
                       color="red", linetype="solid", size=1)})
    
    output$boxplot6 <- renderPlot({
        ggplot(yy, aes(x=0, y = TOT_CHOLE)) + 
            xlab("") + ylab(" ")+ theme_minimal() +
            geom_boxplot(fill="pink", colour="black",   outlier.color = 'black',outlier.shape = 20)+ 
            ylim(50,500) +
            scale_x_continuous(breaks = NULL) +
            geom_hline(aes(yintercept=input$g),
                       color="red", linetype="solid", size=1)})
    
    output$box6 <- renderPlot({
        ggplot(yy, aes(x=0, y = TRIGLYCERIDE)) + 
            xlab("") + ylab(" ")+ theme_minimal() +
            geom_boxplot(fill="pink", colour="black",   outlier.color = 'black',outlier.shape = 20) +
            scale_x_continuous(breaks = NULL) +
            geom_hline(aes(yintercept=input$h),
                       color="red", linetype="solid", size=1) 
    })
    
    output$boxplot7 <- renderPlot({
        ggplot(yy, aes(x=0, y = CREATININE)) + 
            xlab("") + ylab(" ")+
            geom_boxplot(fill="pink", colour="black",   outlier.color = 'black',outlier.shape = 20)+ 
            ylim(0,4) + theme_minimal() +
            scale_x_continuous(breaks = NULL) +
            geom_hline(aes(yintercept=input$n),
                       color="red", linetype="solid", size=1)})
    
    output$box7 <- renderPlot({
        ggplot(yy, aes(x=0, y = TOT_CHOLE)) + 
            xlab("") + ylab(" ")+
            geom_boxplot(fill="pink", colour="black",   outlier.color = 'black',outlier.shape = 20)+ 
            ylim(50,500) + theme_minimal() +
            scale_x_continuous(breaks = NULL) +
            geom_hline(aes(yintercept=input$g),
                       color="red", linetype="solid", size=1)
    })
    
    
    
    
    
}






shinyApp(ui = ui, server = server)