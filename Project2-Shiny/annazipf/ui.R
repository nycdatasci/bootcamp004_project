library(shiny)
library(leaflet)
library(shinyBS)


shinyUI(navbarPage("Medicare doctors in the USA", id="nav",

                   
  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    
                    h2("Medicare explorer"),
                    
                    selectInput("specialty", "Select a specialty", names, multiple = TRUE),
                    selectInput("state", "State", stateselect, selected = "NY", multiple = TRUE),
                    checkboxGroupInput('sex', "Gender", gender, 
                                       selected = gender)   , 
                    plotOutput("timep", height = 250)
              
      )
    )
  ),
  
  tabPanel("Find a doctor",
           fluidRow(
             column(3,
selectInput("statestab", "States", stateselect, selected = 'NY', multiple=FALSE)
             ),
             column(3,
         conditionalPanel("City",
selectInput("cities", "Cities", cities ,  multiple=TRUE)
                    )
             ),
             column(3,
selectInput("specials", "Specialty", names, multiple=TRUE)
             ),
            column(3,
       selectInput("medschool", "Medical school", medschool, multiple=TRUE)
              ),
              hr(), 
            DT::dataTableOutput("drtable")
           )
        ) ,


tabPanel("Density map",
                    div(class="outer",
                        
                        tags$head(
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ), 
                        leafletOutput("denmap", width="100%", height="100%") ) ),


tabPanel("Basic stats",
         
            plotOutput("plot", height = 350),
            plotOutput("schoolp", height = 350)
             ),

tabPanel("Changes/Disparity",
             plotOutput("timespp", height = 350),
             plotOutput("genderp", height = 350,  
                        hover = hoverOpts(id ="plot_hover1")),
               verbatimTextOutput("hover_info1")),
          
         
         
tabPanel("Stability",
         fluidPage(
             plotOutput("changep", height = 350),
             bsTooltip("changepisolate", "test",
              placement = "bottom"),
             plotOutput("changepisolate", height = 350 , hover = hoverOpts(id="plot_hover2", 
                nullOutside = TRUE)),
             conditionalPanel(
               condition = "!is.null(input.plot_hover2)",
               verbatimTextOutput("hover_info2"))
         )
         ) ,
tabPanel("Notes",
         fluidPage( HTML('Since 1966, Medicare has been the national American health insurance program,  
providing health insurance to Americans aged 65 and older as well as younger people with disabilities
and end stage renal disease. Medicare provides insurance to about 48 million Americans, 40 million
of who are 65 and older, 8 million of whom are younger and disabled. It represents approximately
half ($183 billion) of total inpatient hospital costs in the entire United States.

The data for this app is gathered from the medicare.gov Physician Compare Website provided
by the Centers for Medicare & Medicaid Services. It includes professions, ages, addresses,
primary & secondary specialties, and alma mater information on over <strong> 2 million </strong>
healthcare professionals 
in the United States, graduating between 1773 and 2015, participating in the following fields:<br>
 [1] "CLINICAL NURSE SPECIALIST"       <br>                  
                         [2] "PHYSICAL MEDICINE AND REHABILITATION"    <br>         
                         [3] "OPTOMETRY"                  <br>                      
                         [4] "NURSE PRACTITIONER"             <br>                  
                         [5] "CLINICAL SOCIAL WORKER"             <br>              
                         [6] "CLINICAL PSYCHOLOGIST"                  <br>          
                         [7] "EMERGENCY MEDICINE"                         <br>      
                         [8] "CHIROPRACTIC"                                   <br>  
                         [9] "CARDIOVASCULAR DISEASE (CARDIOLOGY)"              <br>
                         [10] "FAMILY PRACTICE"                               <br>   
                         [11] "PSYCHIATRY"                                       <br>
                         [12] "INTERNAL MEDICINE"                                <br>
                         [13] "PLASTIC AND RECONSTRUCTIVE SURGERY"               <br>
                         [14] "ORAL SURGERY (DENTIST ONLY)"                      <br>
                         [15] "PODIATRY"                                         <br>
                         [16] "GENERAL SURGERY"                                  <br>
                         [17] "NEUROLOGY"                                        <br>
                         [18] "NEUROSURGERY"                                     <br>
                         [19] "AUDIOLOGIST"                                      <br>
                         [20] "OPHTHALMOLOGY"                                    <br>
                         [21] "GENERAL PRACTICE"                                 <br>
                         [22] "OBSTETRICS/GYNECOLOGY"                            <br>
                         [23] "PATHOLOGY"                                        <br>
                         [24] "PULMONARY DISEASE"                                <br>
                         [25] "ANESTHESIOLOGY"                                   <br>
                         [26] "REGISTERED DIETITIAN OR NUTRITION PROFESSIONAL"   <br>
                         [27] "MAXILLOFACIAL SURGERY"                            <br>
                         [28] "INFECTIOUS DISEASE"                               <br>
                         [29] "UROLOGY"                                          <br>
                         [30] "ALLERGY/IMMUNOLOGY"                               <br>
                         [31] "SPEECH LANGUAGE PATHOLOGIST"                     <br> 
                         [32] "DERMATOLOGY"                                      <br>
                         [33] "CERTIFIED REGISTERED NURSE ANESTHETIST"           <br>
                         [34] "SLEEP LABORATORY/MEDICINE"                        <br>
                         [35] "PEDIATRIC MEDICINE"                               <br>
                         [36] "INTERVENTIONAL PAIN MANAGEMENT"                   <br>
                         [37] "ORTHOPEDIC SURGERY"                               <br>
                         [38] "PAIN MANAGEMENT"                                  <br>
                         [39] "OSTEOPATHIC MANIPULATIVE MEDICINE"                <br>
                         [40] "VASCULAR SURGERY"                                 <br>
                         [41] "HOSPICE/PALLIATIVE CARE"                          <br>
                         [42] "HAND SURGERY"                                     <br>
                         [43] "DIAGNOSTIC RADIOLOGY"                             <br>
                         [44] "HEMATOLOGY/ONCOLOGY"                              <br>
                         [45] "SURGICAL ONCOLOGY"                                <br>
                         [46] "OTOLARYNGOLOGY"                                   <br>
                         [47] "RADIATION ONCOLOGY"                               <br>
                         [48] "GASTROENTEROLOGY"                                 <br>
                         [49] "ENDOCRINOLOGY"                                    <br>
                         [50] "CARDIAC SURGERY"                                  <br>
                         [51] "GYNECOLOGICAL ONCOLOGY"                           <br>
                         [52] "COLORECTAL SURGERY (PROCTOLOGY)"                  <br>
                         [53] "GERIATRIC MEDICINE"                               <br>
                         [54] "PHYSICIAN ASSISTANT"                              <br>
                         [55] "NEPHROLOGY"                                       <br>
                         [56] "PREVENTATIVE MEDICINE"                            <br>
                         [57] "CERTIFIED NURSE MIDWIFE"                          <br>
                         [58] "INTERVENTIONAL RADIOLOGY"                         <br>
                         [59] "GERIATRIC PSYCHIATRY"                             <br>
                         [60] "ADDICTION MEDICINE"                               <br>
                         [61] "RHEUMATOLOGY"                                     <br>
                         [62] "NUCLEAR MEDICINE"                                 <br>
                         [63] "PERIPHERAL VASCULAR DISEASE"                      <br>
                         [64] "NEUROPSYCHIATRY"                                  <br>
                         [65] "MEDICAL ONCOLOGY"                                 <br>
                         [66] "CRITICAL CARE (INTENSIVISTS)"                     <br>
                         [67] "THORACIC SURGERY"                                 <br>
                         [68] "HEMATOLOGY"                                       <br>
                         [69] "SPORTS MEDICINE"                                  <br>
                         [70] "UNDEFINED PHYSICIAN TYPE (SPECIFY)"               <br>
                         [71] "PHYSICAL THERAPY"                                 <br>
                         [72] "OCCUPATIONAL THERAPY"                             <br>
                         [73] "CARDIAC ELECTROPHYSIOLOGY"                        <br>
                         [74] "ANESTHESIOLOGY ASSISTANT"                         <br>
                         [75] "UNDEFINED NON-PHYSICIAN TYPE (SPECIFY)"       <br>    
                         [76] "INTERVENTIONAL CARDIOLOGY"         <br>               
                         [77] "SINGLE OR MULTISPECIALTY CLINIC OR GROUP PRACTICE"
                         ')
                 
           ))



))