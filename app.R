
# Définir OPENAI_API_KEY dans les variables d’environnement (jamais dans le script).

# La clé OpenAI doit être configurée dans OPENAI_API_KEY sur le serveur.

# ============================================================
# COMEXT LUXEMBOURG - V25 (BASE app(1).R) - PDF QUARTO/TYPST
# Shiny + DuckDB + Parquet + Plotly + ellmer
# ============================================================

library(shiny)
library(DBI)
library(duckdb)
library(ggplot2)
library(plotly)
library(DT)
library(ellmer)
library(commonmark)
library(leaflet)
library(sf)
library(dplyr)
library(rnaturalearth)

# ============================================================
# 1. CONFIGURATION
# ============================================================

DOSSIER <- getwd()

HS_LABELS <- file.path(DOSSIER, "comext_hs_labels.csv")
COUNTRY_LABELS <- file.path(
  DOSSIER, "comext_country_labels.csv"
)

if (!file.exists(HS_LABELS) ||
    !file.exists(COUNTRY_LABELS)) {
  stop("Fichiers de nomenclature manquants dans C:/COMEXT_LU")
}

hs_labels <- read.csv(
  HS_LABELS,
  colClasses = "character",
  fileEncoding = "UTF-8-BOM",
  check.names = FALSE
)

country_labels <- read.csv(
  COUNTRY_LABELS,
  colClasses = "character",
  fileEncoding = "UTF-8-BOM",
  check.names = FALSE
)

nom_pays <- function(codes) {
  noms <- country_labels$nom[
    match(codes, country_labels$code)
  ]
  ifelse(
    is.na(noms) | !nzchar(noms),
    paste0("Code ", codes),
    paste0(noms, " (", codes, ")")
  )
}

nom_produit <- function(codes, niveau, version) {
  ref <- hs_labels[
    hs_labels$version == version &
      nchar(hs_labels$code) == niveau,
  ]
  labels <- ref$libelle[match(codes, ref$code)]
  labels[is.na(labels) | !nzchar(labels)] <-
    "Libelle non disponible"
  paste0(codes, " - ", labels)
}

COLS <- c(
  "Importations" = "#2674B8",
  "Exportations" = "#E18B28",
  "Solde commercial" = "#4B8065"
)

FLUX <- names(COLS)

# ============================================================
# 2. FONCTIONS STATISTIQUES
# ============================================================

variation <- function(x, lag_n) {
  z <- c(rep(NA_real_, lag_n), head(x, -lag_n))
  ans <- 100 * (x / z - 1)
  ans[!is.finite(ans)] <- NA_real_
  ans
}


# ============================================================
# 3. INTERFACE SHINY
# ============================================================

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background: #f5f7fa;
      }

      .container-fluid {
        padding-left: 22px;
        padding-right: 22px;
      }

      .app-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 14px;
        margin: 14px 0 12px;
      }

      .app-header h2 {
        font-size: 24px;
        font-weight: 650;
        margin: 0;
        color: #263648;
      }

      .file-status {
        font-size: 12px;
        color: #637184;
      }

      .filters-card {
        background: white;
        border: 1px solid #dce3ec;
        border-radius: 12px;
        padding: 14px 18px 6px;
        margin-bottom: 14px;
      }

      .filters-card .form-group {
        margin-bottom: 10px;
      }

      .filters-card label {
        font-size: 12px;
        font-weight: 650;
        color: #45556a;
      }

      .advanced-filters {
        border-top: 1px solid #e9edf2;
        padding: 9px 0 2px;
      }

      .advanced-filters summary {
        cursor: pointer;
        color: #2168a5;
        font-weight: 600;
        font-size: 13px;
      }

      .source-note { font-size:12px; color:#637184; margin:12px 0 4px; }
      .chart-card > .tabbable > .nav-tabs {
        border-bottom:2px solid #dce3ec;
        padding-bottom:5px;
        margin-bottom:12px;
      }
      .chart-card > .tabbable > .nav-tabs > li > a {
        font-weight:650; font-size:15px; padding:11px 18px;
      }
      .chart-card .tab-content .tabbable > .nav-tabs > li > a {
        font-size:13px; padding:8px 12px;
      }

      .chart-card {
        background: white;
        border: 1px solid #dce3ec;
        border-radius: 12px;
        padding: 10px 14px 18px;
        min-height: 560px;
      }

      .chat-panel {
        max-width: 1050px;
        margin: 0 auto;
        padding: 14px 5px;
      }

      .prompts-exemples { margin-top: 17px; margin-bottom: 16px; }
      .prompts-exemples h4 { font-size: 16px; margin-bottom: 4px; }
      .prompt-exemple {
        background: #f7f9fc; border: 1px solid #e0e6ee;
        border-radius: 8px; padding: 12px 14px; margin-bottom: 12px;
        min-height: 140px;
      }
      .prompt-exemple strong { font-size: 13px; color: #263648; }
      .prompt-exemple p { font-size: 12px; color: #526277; margin: 7px 0 10px; }

      .chat-answer {
        border: 1px solid #e0e6ee;
        background: white;
        border-radius: 10px;
        padding: 18px;
        min-height: 125px;
        line-height: 1.55;
        overflow-x: auto;
      }

      .chat-answer table {
        border-collapse: collapse;
        width: 100%;
      }

      .chat-answer th,
      .chat-answer td {
        border: 1px solid #dfe5ec;
        padding: 6px 9px;
      }

      .chat-answer th {
        background: #f3f6fa;
      }
    ")),
    
    tags$script(HTML("
      // Dictee vocale via Web Speech API (Chrome / Edge selon support).
      (function () {
        var recognition = null;
        var listening = false;

        function status(message) {
          var el = document.getElementById('dictee_statut');
          if (el) el.textContent = message;
        }
        function microphone(active) {
          var btn = document.getElementById('dicter_ia');
          if (!btn) return;
          btn.classList.toggle('btn-danger', active);
          btn.classList.toggle('btn-default', !active);
          btn.setAttribute('aria-pressed', active ? 'true' : 'false');
        }
        document.addEventListener('click', function (event) {
          var btn = event.target.closest('#dicter_ia');
          if (!btn) return;
          event.preventDefault();

          var SpeechRecognition = window.SpeechRecognition ||
                                  window.webkitSpeechRecognition;
          if (!SpeechRecognition) {
            status('Dictee indisponible dans ce navigateur. Essayez Chrome ou Edge.');
            return;
          }
          if (listening && recognition) {
            recognition.stop();
            return;
          }
          recognition = new SpeechRecognition();
          recognition.lang = 'fr-FR';
          recognition.continuous = false;
          recognition.interimResults = false;
          recognition.maxAlternatives = 1;
          recognition.onstart = function () {
            listening = true;
            microphone(true);
            status('Ecoute en cours... Parlez maintenant. Cliquez de nouveau pour arreter.');
          };
          recognition.onresult = function (e) {
            var texte = e.results[0][0].transcript;
            // L'utilisateur pourra relire le texte avant envoi.
            Shiny.setInputValue('dictee_texte', texte, {priority: 'event'});
            status('Dictee terminee. Verifiez le texte, puis cliquez sur Poser la question.');
          };
          recognition.onerror = function (e) {
            var erreurs = {
              'not-allowed': 'Acces au microphone refuse. Verifiez les autorisations du navigateur.',
              'audio-capture': 'Aucun microphone disponible.',
              'no-speech': 'Aucune parole detectee. Reessayez.',
              'network': 'Service vocal indisponible (reseau).',
              'service-not-allowed': 'Service de reconnaissance vocale non autorise.'
            };
            status(erreurs[e.error] || ('Erreur de dictee : ' + e.error));
          };
          recognition.onend = function () {
            listening = false;
            microphone(false);
          };
          try { recognition.start(); }
          catch (e) { status('Impossible de lancer la dictee : ' + e.message); }
        });
      })();
    ")),
  ),
  
  # ----------------------------------------------------------
  # EN-TETE
  # ----------------------------------------------------------
  
  div(
    class = "app-header",
    
    tags$h2(
      "COMEXT Luxembourg",
      tags$small(" | Tableaux de bord et Assistant IA")
    )
    

  ),
  
  # ----------------------------------------------------------
  # FILTRES
  # ----------------------------------------------------------
  
  div(
    class = "filters-card",
    
    fluidRow(
      
      column(
        3,
        selectInput(
          "indicateur",
          "Indicateur à afficher",
          choices = c(
            "Importations, exportations et solde commercial" = "DUO",
            "Importations" = "Importations",
            "Exportations" = "Exportations",
            "Solde commercial" = "Solde commercial"
          ),
          selected = "DUO"
        )
      ),
      
      column(
        3,
        selectInput(
          "type_variation",
          "Variation",
          choices = c(
            "Mensuelle (m/m-1)" = "mensuelle",
            "Glissement annuel (m/m-12)" = "annuelle"
          )
        )
      ),
      
      column(6, uiOutput("periode_ui"))
    ),
    
    tags$details(
      class = "advanced-filters",
      
      tags$summary(
        "Filtres complémentaires : pays et produits"
      ),
      
      fluidRow(
        
        column(
          4,
          selectInput(
            "partner",
            "Pays partenaire",
            choices = c(
              "Tous les partenaires" = "__ALL__"
            )
          )
        ),
        
        column(
          3,
          selectInput(
            "niveau",
            "Subdivision par produits",
            choices = c(
              "Tous les produits" = "TOUS",
              "SH2" = "2",
              "SH4" = "4",
              "SH6" = "6"
            )
          )
        ),
        
        column(
          5,
          selectizeInput(
            "produit",
            "Produit : code et libellé",
            choices = c(
              "Tous les produits" = "__ALL__"
            )
          )
        )
      )
    )
  ),
  
  # ----------------------------------------------------------
  # ONGLETS
  # ----------------------------------------------------------
  
  div(
    class = "chart-card",
    
    tabsetPanel(
      id = "rubrique_principale",
      type = "tabs",

      tabPanel(
        title = "Évolution",
        icon = icon("chart-line"),
        tabsetPanel(
          id = "onglet_evolution",
      tabPanel(
        "Niveaux mensuels",
        p(class = "source-note", "Source : Eurostat COMEXT — fichiers mensuels. Montants en millions d’euros."),
        plotlyOutput("niveaux", height = "500px")
      ),

      tabPanel(
        "Variations mensuelles",
        p(class = "source-note", "Taux en %. Pour le solde, interprétation délicate si la valeur de référence est négative ou proche de zéro."),
        plotlyOutput("variations", height = "520px")
      ),

      tabPanel(
        "Niveaux annuels",
        p(class = "source-note", "Source : Eurostat COMEXT — fichiers annuels. Montants en millions d’euros (années complètes)."),
        plotlyOutput("niveaux_annuels", height = "500px")
      ),

      tabPanel(
        "Variations annuelles",
        p(class = "source-note", "Source : Eurostat COMEXT — fichiers annuels. Variations des trois indicateurs en % (solde : interprétation prudente si négatif)."),
        plotlyOutput("variations_annuelles", height = "500px")
      ),

          tabPanel(
            "Tableaux",
            tabsetPanel(
              id = "onglet_tableau",
              tabPanel("Mensuel", DTOutput("tableau")),
              tabPanel("Annuel",
                       p(class = "source-note", "Source : Eurostat COMEXT — fichiers annuels. Montants en millions d’euros."),
                       DTOutput("tableau_annuel"))
            )
          )
        )
      ),

      tabPanel(
        title = "Structure",
        icon = icon("chart-bar"),
        tabsetPanel(
          id = "onglet_structure",
      tabPanel(
        "Structure par produit",
        fluidRow(
          column(3, uiOutput("structure_annee_ui")),
          column(3, selectInput("structure_flux", "Flux commercial",
             choices = c("Exportations" = "2", "Importations" = "1"),
             selected = "2")),
          column(3, selectInput("structure_niveau", "Niveau produit",
             choices = c("SH2" = "2", "SH4" = "4", "SH6" = "6"),
             selected = "2")),
          column(3, numericInput("structure_top", "Top N", value = 10,
                                 min = 3, max = 30, step = 1))
        ),
        p(class = "source-note",
          "Source : Eurostat COMEXT - fichier annuel. Seuls les principaux produits sont affiches ; leurs parts restent calculees sur le total des produits. Le filtre pays du tableau de bord s'applique."),
        plotlyOutput("structure_produits_graph", height = "530px"),
        DTOutput("structure_produits_table")
      ),

      tabPanel(
        "Structure par partenaire",
        fluidRow(
          column(3, uiOutput("structure_annee_ui_pays")),
          column(3, selectInput("structure_flux_pays", "Flux commercial",
             choices = c("Exportations" = "2", "Importations" = "1"),
             selected = "2")),
          column(3, numericInput("structure_top_pays", "Top N", value = 10,
                                 min = 3, max = 30, step = 1))
        ),
        p(class = "source-note",
          "Source : Eurostat COMEXT - fichier annuel. Les parts sont calculees parmi les pays individuels identifies par le referentiel ; les aggregats de partenaires sont exclus. Le filtre produit du tableau de bord s'applique."),
        plotlyOutput("structure_pays_graph", height = "530px"),
        DTOutput("structure_pays_table")
      ),

      tabPanel(
        "Évolution de la structure",
        p(class = "source-note",
          "Parts annuelles des principales catégories, calculées à partir des Parquet annuels. Sélection des catégories de référence sur la dernière année disponible. La comparaison SH entre 2021 et 2022 doit être interprétée avec prudence."),
        fluidRow(
          column(3, selectInput("evol_struct_dimension", "Regroupement",
                                choices = c("Produits" = "produit", "Partenaires" = "pays"))),
          column(3, selectInput("evol_struct_flux", "Flux commercial",
                                choices = c("Exportations" = "2", "Importations" = "1"), selected = "2")),
          column(3, conditionalPanel("input.evol_struct_dimension == 'produit'",
                                selectInput("evol_struct_niveau", "Niveau SH",
                                            choices = c("SH2" = "2", "SH4" = "4", "SH6" = "6"), selected = "2"))),
          column(3, numericInput("evol_struct_top", "Nombre de catégories", 5, min = 2, max = 12))
        ),
        fluidRow(
          column(4, selectInput("evol_struct_graph_type", "Type de graphique",
                                choices = c("Courbes des parts" = "courbes",
                                            "Barres empilées (parts du total)" = "empile"),
                                selected = "courbes"))
        ),
        plotlyOutput("evol_structure_graph", height = "560px"),
        h4("Parts annuelles (%)"),
        DTOutput("evol_structure_table")
      )
        )
      ),

      tabPanel(
        title = "Carte des échanges",
        icon = icon("globe"),
        fluidRow(
          column(3, selectInput("map_flow", "Flux commercial",
            choices = c("Importations"="1", "Exportations"="2", "Solde"="3"), selected="3")),
          column(3, selectInput("map_type", "Périodicité",
            choices = c("Annuelle"="Annual", "Mensuelle"="Monthly"), selected="Annual")),
          column(3, uiOutput("map_year_ui")),
          column(3, uiOutput("map_month_ui"))
        ),
        p(class="source-note", "Source : Eurostat COMEXT. Carte choroplèthe inspirée de Map.R. Montants en euros courants ; gris : aucune observation. Le solde correspond aux exportations moins les importations."),
        leafletOutput("map_comext", height="680px"),
        p(class="source-note", "Cliquez sur un pays pour voir la valeur du flux et les trois principaux chapitres SH2 (sur la sélection de l'année ou du mois).")
      ),

      tabPanel(
        title = "Références",
        icon = icon("book-open"),
DTOutput("nomenclature_table")
      ),

      tabPanel(
        title = "Assistant IA",
        icon = icon("comments"),
div(
          class = "chat-panel",
          
          h3("Interroger les données COMEXT"),
          
          p(
            "Posez une question en langage naturel. ",
            "L'assistant utilise DuckDB pour les ",
            "résultats statistiques."
          ),
          
          textAreaInput(
            "question_ia",
            "Votre question",
            rows = 3,
            width = "100%",
            placeholder = paste(
              "Exemple : Quels sont les cinq",
              "principaux partenaires à",
              "l'exportation en 2024 ?"
            )
          ),
          
          actionButton(
            "dicter_ia",
            "Dicter",
            icon = icon("microphone"),
            class = "btn-default",
            title = "Demarrer ou arreter la dictee vocale"
          ),
          
          actionButton(
            "envoyer_ia",
            "Poser la question",
            class = "btn-primary"
          ),
          
          actionButton(
            "reset_ia",
            "Nouvelle conversation"
          ),
          
          p(
            id = "dictee_statut",
            role = "status",
            style = "font-size:12px;color:#637184;margin-top:6px;min-height:0;",
            ""
          ),
          
          # Prompts pré-définis : clic = insertion dans la saisie, sans envoi automatique.
          div(
            class = "prompts-exemples",
            h4("Exemples de questions et de notes"),
            p(class = "source-note",
              "Cliquez sur Utiliser pour insérer un exemple dans la question. " ,
              "Vous pouvez le modifier avant de l'envoyer."),
            fluidRow(
              column(6, div(class = "prompt-exemple",
                strong("Marchés clés par produits"),
                p("Pour le Luxembourg, identifier les marchés clés par segment de produits entre 2020 et 2025."),
                actionButton("prompt_cles", "Utiliser", icon = icon("arrow-up"), class = "btn-default btn-sm")
              )),
              column(6, div(class = "prompt-exemple",
                strong("Ukraine depuis 2022"),
                p("Je suis journaliste à RTL. Comment les échanges commerciaux du Luxembourg avec l’Ukraine ont-ils évolué depuis 2022 ?"),
                actionButton("prompt_ukraine", "Utiliser", icon = icon("arrow-up"), class = "btn-default btn-sm")
              ))
            ),
            fluidRow(
              column(6, div(class = "prompt-exemple",
                strong("Analyse pour investisseurs"),
                p("Je suis investisseur luxembourgeois et souhaite décrypter les moteurs du commerce extérieur du Luxembourg. Rédige une note d'analyse sur ce sujet."),
                actionButton("prompt_investisseur", "Utiliser", icon = icon("arrow-up"), class = "btn-default btn-sm")
              )),
              column(6, div(class = "prompt-exemple",
                strong("Flux stratégiques"),
                p("Révéler les flux commerciaux à forte valeur stratégique pour le Luxembourg sur la période 2022–2025."),
                actionButton("prompt_strategique", "Utiliser", icon = icon("arrow-up"), class = "btn-default btn-sm")
              ))
            )
          ),

          br(),
          uiOutput("reponse_ia"),
          tags$hr(),
          h4("Notes statistiques au format PDF"),
          p(class = "source-note",
            "Dans la conversation, demandez « Rédige une note PDF sur les exportations 2020-2025, " ,
            "avec les principaux partenaires et produits ». Les filtres du tableau de bord " ,
            "ne s'appliquent pas aux notes. Le PDF est généré automatiquement."),
          p(class = "source-note", "La progression apparaît pendant le traitement. La durée dépend de la réponse IA et de la création du PDF."),
          textOutput("note_statut"),
          uiOutput("note_telechargement")
        )
      )
    ),
    
    br(),
    textOutput("avertissement")
  )
)

# ============================================================
# 4. SERVEUR SHINY
# ============================================================

server <- function(input, output, session) {
  
  # Connexion DuckDB propre à chaque session
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir = ":memory:"
  )
  
  session$onSessionEnded(function() {
    try(
      dbDisconnect(con, shutdown = TRUE),
      silent = TRUE
    )
  })
  
  qs <- function(s) {
    as.character(dbQuoteString(con, s))
  }
  
  # ----------------------------------------------------------
  # 4.1 FICHIERS PARQUET
  # ----------------------------------------------------------
  
  fichiers <- reactive({
      
      validate(need(
        dir.exists(DOSSIER),
        paste("Dossier inexistant :", DOSSIER)
      ))
      
      f <- list.files(
        DOSSIER,
        pattern = "^LU_[0-9]{6}\\.parquet$",
        full.names = TRUE,
        ignore.case = TRUE
      )
      
      validate(need(
        length(f) > 0,
        "Aucun fichier Parquet trouvé."
      ))
      
      p <- sub(
        "^LU_([0-9]{6})\\.parquet$",
        "\\1",
        basename(f),
        ignore.case = TRUE
      )
      
      ok <- grepl(
        "^[0-9]{4}(0[1-9]|1[0-2])$",
        p
      )
      
      x <- data.frame(
        path = normalizePath(
          f[ok],
          winslash = "/"
        ),
        periode = p[ok]
      )
      
      validate(need(
        nrow(x) > 0 &&
          !anyDuplicated(x$periode),
        "Périodes invalides ou dupliquées."
      ))
      
      x[order(x$periode), , drop = FALSE]
      
  })
  

  # ----------------------------------------------------------
  # 4.2 PERIODES
  # ----------------------------------------------------------
  
  output$periode_ui <- renderUI({
    
    p <- fichiers()$periode
    
    fluidRow(
      column(
        6,
        selectInput(
          "debut", "Premier mois",
          choices = p,
          selected = min(p)
        )
      ),
      column(
        6,
        selectInput(
          "fin", "Dernier mois",
          choices = p,
          selected = max(p)
        )
      )
    )
  })
  
  # ----------------------------------------------------------
  # 4.3 STRUCTURE ET SOURCE SQL
  # ----------------------------------------------------------
  
  schema <- reactive({
    
    x <- fichiers()
    
    dbGetQuery(
      con,
      paste0(
        "DESCRIBE SELECT * FROM read_parquet(",
        qs(x$path[1]),
        ")"
      )
    )
  })
  
  source_sql <- function(x) {
    
    paste0(
      "read_parquet([",
      paste(
        vapply(x$path, qs, character(1)),
        collapse = ","
      ),
      "], union_by_name=true)"
    )
  }
  
  # ----------------------------------------------------------
  # 4.4 PARTENAIRES
  # ----------------------------------------------------------
  
  observeEvent(fichiers(), {
    
    validate(need(
      all(c(
        "PARTNER", "FLOW", "PERIOD",
        "VALUE_EUR", "PRODUCT_NC"
      ) %in% schema()$column_name),
      "Colonnes COMEXT manquantes."
    ))
    
    p <- dbGetQuery(
      con,
      paste0(
        "SELECT DISTINCT PARTNER FROM ",
        source_sql(fichiers()),
        " WHERE PARTNER IS NOT NULL"
      )
    )$PARTNER
    
    p <- p[order(nom_pays(p))]
    
    updateSelectInput(
      session,
      "partner",
      choices = c(
        "Tous les partenaires" = "__ALL__",
        setNames(p, nom_pays(p))
      ),
      selected = "__ALL__"
    )
  })
  
  # ----------------------------------------------------------
  # 4.5 PRODUITS
  # ----------------------------------------------------------
  
  observeEvent(
    list(input$niveau, input$debut, fichiers()),
    {
      
      req(input$niveau)
      
      if (input$niveau == "TOUS") {
        
        updateSelectizeInput(
          session,
          "produit",
          choices = c(
            "Tous les produits" = "__ALL__"
          ),
          selected = "__ALL__"
        )
        
      } else {
        
        n <- as.integer(input$niveau)
        
        sql <- paste0(
          "SELECT DISTINCT LEFT(PRODUCT_NC,",
          n,
          ") AS code FROM ",
          source_sql(fichiers()),
          " WHERE regexp_matches(",
          "PRODUCT_NC,'^[0-9]{8}$')",
          " ORDER BY code"
        )
        
        codes <- dbGetQuery(con, sql)$code
        
        annee_ref <- if (
          !is.null(input$debut) &&
          nzchar(input$debut)
        ) {
          as.integer(substr(input$debut, 1, 4))
        } else {
          2022L
        }
        
        version <- if (annee_ref < 2022) {
          "HS17"
        } else {
          "HS22"
        }
        
        labels <- nom_produit(
          codes, n, version
        )
        
        updateSelectizeInput(
          session,
          "produit",
          choices = c(
            "Tous les codes" = "__ALL__",
            setNames(codes, labels)
          ),
          selected = "__ALL__",
          server = TRUE
        )
      }
    }
  )
  
  # ----------------------------------------------------------
  # 4.6 FILTRES REACTIFS
  # ----------------------------------------------------------
  
  filtres <- reactive({
    
    req(
      input$debut,
      input$fin,
      input$partner,
      input$niveau,
      input$produit
    )
    
    list(
      debut = input$debut,
      fin = input$fin,
      partner = input$partner,
      niveau = input$niveau,
      produit = input$produit,
      fichiers = fichiers()
    )
  })
  
  filtres_stables <- shiny::debounce(
    filtres,
    millis = 1000
  )
  
  # ----------------------------------------------------------
  # 4.7 AGREGATIONS DUCKDB
  # ----------------------------------------------------------
  
  resultat <- reactive({
    
    par <- filtres_stables()
    x <- par$fichiers
    
    validate(need(
      par$debut <= par$fin,
      "Période incorrecte."
    ))
    
    date_debut <- as.Date(
      paste0(par$debut, "01"), "%Y%m%d"
    )
    
    date_fin <- as.Date(
      paste0(par$fin, "01"), "%Y%m%d"
    )
    
    historique <- format(
      seq(
        date_debut,
        by = "-12 months",
        length.out = 2
      )[2],
      "%Y%m"
    )
    
    sel <- x[
      x$periode >= historique &
        x$periode <= par$fin,
      ,
      drop = FALSE
    ]
    
    validate(need(
      nrow(sel) > 0,
      "Aucun fichier disponible."
    ))
    
    where <- c(
      "FLOW IN ('1','2')",
      "regexp_matches(PERIOD,'^[0-9]{6}$')",
      "regexp_matches(PRODUCT_NC,'^[0-9]{8}$')"
    )
    
    if (par$partner != "__ALL__") {
      where <- c(
        where,
        paste0(
          "PARTNER=", qs(par$partner)
        )
      )
    }
    
    if (
      par$niveau != "TOUS" &&
      par$produit != "__ALL__"
    ) {
      
      n <- as.integer(par$niveau)
      
      where <- c(
        where,
        paste0(
          "LEFT(PRODUCT_NC,", n, ")=",
          qs(par$produit)
        )
      )
    }
    
    sql <- paste0(
      "SELECT PERIOD AS periode, FLOW AS flow, ",
      "SUM(TRY_CAST(NULLIF(TRIM(VALUE_EUR),'') ",
      "AS DOUBLE))/1e6 AS montant, ",
      "COUNT(*) AS n_lignes, ",
      "COUNT(*) FILTER (WHERE ",
      "NULLIF(TRIM(VALUE_EUR),'') IS NOT NULL ",
      "AND TRY_CAST(NULLIF(TRIM(VALUE_EUR),'') ",
      "AS DOUBLE) IS NULL) AS n_invalides ",
      "FROM ", source_sql(sel),
      " WHERE ", paste(where, collapse = " AND "),
      " GROUP BY PERIOD,FLOW ORDER BY PERIOD,FLOW"
    )
    
    brut <- dbGetQuery(con, sql)
    
    validate(need(
      nrow(brut) > 0,
      "Aucune observation."
    ))
    
    calendrier <- data.frame(
      date = seq(
        as.Date(
          paste0(historique, "01"),
          "%Y%m%d"
        ),
        date_fin,
        by = "month"
      )
    )
    
    calendrier$periode <- format(
      calendrier$date, "%Y%m"
    )
    
    imp <- brut[
      brut$flow == "1",
      c("periode", "montant")
    ]
    
    exp <- brut[
      brut$flow == "2",
      c("periode", "montant")
    ]
    
    names(imp)[2] <- "Importations"
    names(exp)[2] <- "Exportations"
    
    wide <- merge(
      calendrier, imp,
      by = "periode",
      all.x = TRUE,
      sort = FALSE
    )
    
    wide <- merge(
      wide, exp,
      by = "periode",
      all.x = TRUE,
      sort = FALSE
    )
    
    wide <- wide[order(wide$date), ]
    
    wide$`Solde commercial` <-
      wide$Exportations - wide$Importations
    
    wide$annee <- format(wide$date, "%Y")
    wide$mois <- as.integer(
      format(wide$date, "%m")
    )
    
    for (nom in FLUX) {
      wide[[paste0(nom, "_m")]] <-
        variation(wide[[nom]], 1)
      
      wide[[paste0(nom, "_a")]] <-
        variation(wide[[nom]], 12)
    }
    
    visible <- (
      wide$date >= date_debut &
        wide$date <= date_fin
    )
    
    list(
      series = wide[visible, , drop = FALSE],
       controles = brut
    )
  })
  
  # ----------------------------------------------------------
  # 4.8 FONCTIONS GRAPHIQUES
  # ----------------------------------------------------------
  
  long <- function(
    df, noms, suffixe = "", unite = "M€") {
    
    do.call(
      rbind,
      lapply(noms, function(nom) {
        
        y <- df[[paste0(nom, suffixe)]]
        
        data.frame(
          date = df$date,
          periode = df$periode,
          indicateur = nom,
          valeur = y,
          texte = paste0(
            df$periode, "<br>",
            nom, " : ",
            ifelse(is.na(y), "ND", format(round(y, 2), big.mark = " ", decimal.mark = ",")), " ", unite
          )
        )
      })
    )
  }
  
  trace_plot <- function(d, titre, unite, axe = "date") {
    
    p <- ggplot(
      d,
      aes(
        x = .data[[axe]],
        y = valeur,
        colour = indicateur,
        group = indicateur,
        text = texte
      )
    ) +
      geom_hline(
        yintercept = 0,
        colour = "grey80"
      ) +
      geom_line(linewidth = 0.9, na.rm = TRUE) +
      geom_point(size = 1.5, na.rm = TRUE) +
      scale_colour_manual(values = COLS) +
      labs(
        title = titre,
        x = if (axe == "date") "Mois" else "Année",
        y = unite,
        colour = "Série"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")

    if (axe == "annee") {
      p <- p + scale_x_continuous(breaks = sort(unique(d$annee)))
    }

    ggplotly(p, tooltip = "text") |>
      layout(hovermode = "closest") |>
      config(
        displaylogo = FALSE,
        responsive = TRUE
      )
  }
  
  series_choisies <- reactive({
    
    req(input$indicateur)
    
    if (input$indicateur == "DUO") {
      c("Importations", "Exportations", "Solde commercial")
    } else {
      input$indicateur
    }
  })
  
  # ----------------------------------------------------------
  # 4.9 NIVEAUX MENSUELS
  # ----------------------------------------------------------
  
  output$niveaux <- renderPlotly({
    
    trace_plot(
      long(
        resultat()$series,
        series_choisies()
      ),
      "Commerce extérieur — niveaux mensuels",
      "Montant (millions d’euros)"
    )
  })
  
  # ----------------------------------------------------------
  # 4.10 VARIATIONS
  # ----------------------------------------------------------
  
  output$variations <- renderPlotly({
    an <- identical(input$type_variation, "annuelle")
    suffixe <- if (an) "_a" else "_m"
    d <- long(resultat()$series, series_choisies(), suffixe, "%")
    validate(need(any(is.finite(d$valeur)), "Variations indisponibles."))
    titre <- if (an) "Commerce extérieur — glissement annuel" else
                     "Commerce extérieur — variation mensuelle"
    trace_plot(d, titre, "Variation (%)")
  })


  # ============================================================
  # 4.11 FICHIERS ANNUELS EUROSTAT
  # Lecture uniquement dans C:/COMEXT_LU/ANNUEL
  # ============================================================
  
  fichiers_annuels <- reactive({
    
    # Utiliser la liste des fichiers detectes au lancement
    fichiers()
    
    # Dossier contenant exclusivement les fichiers annuels
    dossier_annuel <- file.path(DOSSIER, "ANNUEL")
    
    validate(
      need(
        dir.exists(dossier_annuel),
        paste("Dossier annuel introuvable :", dossier_annuel)
      )
    )
    
    # Rechercher les fichiers annuels
    f <- list.files(
      dossier_annuel,
      pattern = "^LU_ANNUEL_[0-9]{4}\\.parquet$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    validate(
      need(
        length(f) > 0,
        "Aucun fichier annuel trouve."
      )
    )
    
    # Extraire l'annee depuis le nom du fichier
    annees <- as.integer(
      sub(
        "^LU_ANNUEL_([0-9]{4})\\.parquet$",
        "\\1",
        basename(f),
        ignore.case = TRUE
      )
    )
    
    # Construire le tableau utilise par DuckDB
    x <- data.frame(
      annee = annees,
      path = normalizePath(f, winslash = "/"),
      stringsAsFactors = FALSE
    )
    
    # Verification des doublons dans le dossier annuel
    validate(
      need(
        !anyDuplicated(x$annee),
        "Plusieurs fichiers pour une meme annee."
      )
    )
    
    # Classer les fichiers par annee
    x <- x[order(x$annee), , drop = FALSE]
    
    return(x)
  })
  
  annuel <- reactive({
    par <- filtres_stables()
    an_debut <- as.integer(substr(par$debut, 1, 4))
    an_fin <- as.integer(substr(par$fin, 1, 4))
    fa <- fichiers_annuels()
    # Inclure l'annee precedente afin de calculer le premier taux.
    sel <- fa[fa$annee >= an_debut-1L & fa$annee <= an_fin,,drop=FALSE]
    validate(need(nrow(sel)>0, "Aucun fichier annuel pour la periode demandee."))
    
    # On traite les annees fichier par fichier pour ne pas melanger
    # les schemas; UNION_BY_NAME est conserve comme dans les mois.
    where <- c("FLOW IN ('1','2')",
               "regexp_matches(PRODUCT_NC,'^[0-9]{8}$')")
    if (par$partner != '__ALL__')
      where <- c(where, paste0("PARTNER=", qs(par$partner)))
    if (par$niveau != 'TOUS' && par$produit != '__ALL__')
      where <- c(where, paste0("LEFT(PRODUCT_NC,", as.integer(par$niveau),
                               ")=", qs(par$produit)))
    
    requetes <- vapply(seq_len(nrow(sel)), function(i) {
      paste0("SELECT ",sel$annee[i]," AS annee, FLOW, ",
             "SUM(TRY_CAST(NULLIF(TRIM(VALUE_EUR),'') AS DOUBLE))/1e6 AS montant, ",
             "COUNT(*) FILTER (WHERE NULLIF(TRIM(VALUE_EUR),'') IS NOT NULL ",
             "AND TRY_CAST(NULLIF(TRIM(VALUE_EUR),'') AS DOUBLE) IS NULL) AS invalides ",
             "FROM read_parquet(", qs(sel$path[i]), ") WHERE ",
             paste(where,collapse=" AND "), " GROUP BY FLOW")
    },character(1))
    donnees <- dbGetQuery(con,paste(requetes,collapse=" UNION ALL "))
    
    calendrier <- data.frame(annee=seq(min(sel$annee),an_fin))
    imp <- donnees[donnees$FLOW=='1',c('annee','montant')]
    exp <- donnees[donnees$FLOW=='2',c('annee','montant')]
    names(imp)[2] <- 'Importations'
    names(exp)[2] <- 'Exportations'
    d <- merge(calendrier,imp,by='annee',all.x=TRUE)
    d <- merge(d,exp,by='annee',all.x=TRUE)
    d <- d[order(d$annee),,drop=FALSE]
    d$`Solde commercial` <- d$Exportations-d$Importations
    # Taux annuels des trois indicateurs, y compris le solde.
    # Une base nulle ou manquante donne NA (voir variation()).
    for (nom in FLUX) {
      d[[paste0(nom, '_variation')]] <- variation(d[[nom]], 1)
    }
    d[d$annee>=an_debut & d$annee<=an_fin,,drop=FALSE]
  })
  
  # Transformer les resultats annuels au format long pour Plotly.
  annuel_long <- function(d,noms,suffixe='',unite='M€') {
    do.call(rbind,lapply(noms,function(nom) {
      y <- d[[paste0(nom,suffixe)]]
      data.frame(annee=d$annee,indicateur=nom,valeur=y,
                 texte=paste0(d$annee,'<br>',nom,': ',
                              ifelse(is.na(y),'ND',format(round(y,2),big.mark=' ',
                                                          decimal.mark=',')),' ',
                              unite))
    }))
  }
  
  output$niveaux_annuels <- renderPlotly({
    d <- annuel_long(annuel(), series_choisies())
    validate(need(any(is.finite(d$valeur)), "Aucune valeur annuelle disponible."))
    trace_plot(d, "Commerce extérieur — niveaux annuels",
               "Montant (millions d’euros)", axe = "annee")
  })

  output$variations_annuelles <- renderPlotly({
    d <- annuel_long(annuel(), series_choisies(), "_variation", "%")
    validate(need(any(is.finite(d$valeur)),
                  "Variation indisponible : deux années consécutives sont nécessaires."))
    trace_plot(d, "Commerce extérieur — variations annuelles",
               "Variation (%)", axe = "annee")
  })

  # ==========================================================
  # V17 - STRUCTURE DES ECHANGES (FICHIERS ANNUELS)
  # ==========================================================

  output$structure_annee_ui <- renderUI({
    yrs <- fichiers_annuels()$annee
    selectInput("structure_annee", "Annee complete", choices = yrs,
                selected = max(yrs))
  })
  output$structure_annee_ui_pays <- renderUI({
    yrs <- fichiers_annuels()$annee
    selectInput("structure_annee_pays", "Annee complete", choices = yrs,
                selected = max(yrs))
  })

  # Un agregat est realise dans DuckDB ; seules les categories
  # agregees sont ramenees dans la session R.
  calcul_structure <- function(annee, flux, dimension, niveau = "2") {
    fa <- fichiers_annuels()
    path <- fa$path[fa$annee == as.integer(annee)]
    if (length(path) != 1L) stop("Fichier annuel indisponible pour cette annee.")
    if (!flux %in% c("1", "2")) stop("Flux invalide.")
    if (!dimension %in% c("produit", "pays")) stop("Dimension invalide.")
    if (!niveau %in% c("2", "4", "6")) stop("Niveau SH invalide.")

    # Les filtres deja presents dans le tableau de bord sont conserves,
    # sauf la dimension classee elle-meme, pour eviter un classement
    # artificiellement limite a une seule categorie.
    p <- filtres_stables()
    where <- c("FLOW = " %+% qs(flux),
               "regexp_matches(PRODUCT_NC,'^[0-9]{8}$')")
    if (dimension == "produit" && p$partner != "__ALL__")
      where <- c(where, paste0("PARTNER = ", qs(p$partner)))
    if (dimension == "pays" && p$niveau != "TOUS" &&
        p$produit != "__ALL__") {
      where <- c(where, paste0("LEFT(PRODUCT_NC,", as.integer(p$niveau),
                              ") = ", qs(p$produit)))
    }
    if (dimension == "pays") {
      # Eviter les zones/totaux agreges (dont EU) si on calcule une
      # repartition par pays. Utiliser des codes ISO2 alphabetiques
      # avec le referentiel, en retirant les zones non nationales.
      codes <- unique(country_labels$code)
      codes <- codes[grepl("^[A-Z]{2}$", codes) &
                     !codes %in% c("EU", "EA", "EZ", "XC", "XL", "XS", "XW", "XX", "ZZ")]
      where <- c(where, paste0("PARTNER IN (",
         paste(vapply(codes, qs, character(1)), collapse = ","), ")"))
      champ <- "PARTNER"
    } else {
      champ <- paste0("LEFT(PRODUCT_NC,", as.integer(niveau), ")")
    }
    sql <- paste0(
      "SELECT ", champ, " AS code, ",
      "SUM(TRY_CAST(NULLIF(TRIM(VALUE_EUR),'') AS DOUBLE))/1e6 AS montant, ",
      "COUNT(*) FILTER (WHERE NULLIF(TRIM(VALUE_EUR),'') IS NOT NULL AND ",
      "TRY_CAST(NULLIF(TRIM(VALUE_EUR),'') AS DOUBLE) IS NULL) AS invalides ",
      "FROM read_parquet(", qs(path), ") WHERE ",
      paste(where, collapse = " AND "),
      " GROUP BY 1 ORDER BY montant DESC NULLS LAST")
    d <- dbGetQuery(con, sql)
    d <- d[!is.na(d$montant) & !is.na(d$code), , drop = FALSE]
    if (!nrow(d)) return(d)
    d$part <- 100 * d$montant / sum(d$montant)
    if (dimension == "pays") {
      d$libelle <- nom_pays(d$code)
    } else {
      ver <- if (as.integer(annee) < 2022) "HS17" else "HS22"
      d$libelle <- nom_produit(d$code, as.integer(niveau), ver)
    }
    d
  }

  # L'operateur est local au serveur et sert uniquement au texte SQL.
  `%+%` <- function(a, b) paste0(a, b)

  produits_structure <- reactive({
    req(input$structure_annee, input$structure_flux,
        input$structure_niveau)
    calcul_structure(input$structure_annee, input$structure_flux,
                     "produit", input$structure_niveau)
  })
  pays_structure <- reactive({
    req(input$structure_annee_pays, input$structure_flux_pays)
    calcul_structure(input$structure_annee_pays,
                     input$structure_flux_pays, "pays")
  })

  table_structure <- function(d, top) {
    if (!nrow(d)) return(d)
    n <- max(1L, min(30L, as.integer(top)))
    t <- head(d, n)
    t <- t[, c("code", "libelle", "montant", "part"), drop = FALSE]
    names(t) <- c("Code", "Categorie", "Montant (M€)", "Part (%)")
    t
  }
  graph_structure <- function(d, top, titre, afficher_autres = TRUE) {
    validate(need(nrow(d) > 0, "Aucun echange trouve pour ces filtres."))
    n <- max(1L, min(30L, as.integer(top)))
    top_d <- head(d, n)
    autre <- sum(tail(d$montant, -n), na.rm = TRUE)
    if (afficher_autres && autre > 0) {
      top_d <- rbind(top_d, data.frame(
        code = "AUTRES", montant = autre, invalides = NA_integer_,
        part = 100 * autre / sum(d$montant), libelle = "Autres"))
    }
    # Libelles raccourcis sur le graphique, complets au survol.
    top_d$axe <- ifelse(nchar(top_d$libelle) > 54,
                       paste0(substr(top_d$libelle, 1, 51), "..."), top_d$libelle)
    top_d$axe <- make.unique(top_d$axe)
    top_d$texte <- paste0(top_d$libelle, "<br>Montant : ",
      format(round(top_d$montant, 2), big.mark = " ", decimal.mark = ","),
      " M€<br>Part : ", sprintf("%.2f", top_d$part), " %")
    g <- ggplot(top_d, aes(x = reorder(axe, part), y = part, text = texte)) +
      geom_col(fill = "#2674B8", width = 0.72) + coord_flip() +
      labs(title = titre, x = NULL, y = "Part (%)") +
      theme_minimal(base_size = 12)
    ggplotly(g, tooltip = "text") |>
      config(displaylogo = FALSE, responsive = TRUE)
  }

  output$structure_produits_graph <- renderPlotly({
    graph_structure(produits_structure(), input$structure_top,
      paste("Structure par produit -", input$structure_annee),
      afficher_autres = FALSE)
  })
  output$structure_pays_graph <- renderPlotly({
    graph_structure(pays_structure(), input$structure_top_pays,
      paste("Structure par partenaire -", input$structure_annee_pays))
  })
  output$structure_produits_table <- renderDT({
    d <- table_structure(produits_structure(), input$structure_top)
    validate(need(nrow(d)>0, "Aucune categorie disponible."))
    datatable(d, rownames = FALSE, extensions = "Buttons",
      options = list(pageLength = 10, scrollX = TRUE, dom = "Bfrtip",
                     buttons = c("copy", "csv", "excel"))) |>
      formatRound(c("Montant (M€)", "Part (%)"), digits = 2)
  })
  output$structure_pays_table <- renderDT({
    d <- table_structure(pays_structure(), input$structure_top_pays)
    validate(need(nrow(d)>0, "Aucun pays disponible."))
    datatable(d, rownames = FALSE, extensions = "Buttons",
      options = list(pageLength = 10, scrollX = TRUE, dom = "Bfrtip",
                     buttons = c("copy", "csv", "excel"))) |>
      formatRound(c("Montant (M€)", "Part (%)"), digits = 2)
  })

  # ==========================================================
  # V19 - EVOLUTION DE LA STRUCTURE DES ECHANGES
  # ==========================================================
  # Les parts sont calculees chaque annee sur toutes les categories,
  # sans limiter le denominateur au Top N.
  evolution_structure <- reactive({
    req(input$evol_struct_dimension, input$evol_struct_flux,
        input$evol_struct_top, input$evol_struct_niveau)
    fa <- fichiers_annuels()
    par <- filtres_stables()
    debut <- as.integer(substr(par$debut, 1, 4))
    fin <- as.integer(substr(par$fin, 1, 4))
    fs <- fa[fa$annee >= debut & fa$annee <= fin, , drop = FALSE]
    validate(need(nrow(fs) > 0, "Aucun fichier annuel pour cette période."))
    dim <- input$evol_struct_dimension
    niv <- input$evol_struct_niveau
    flux <- input$evol_struct_flux
    if (!dim %in% c("produit", "pays") || !niv %in% c("2", "4", "6") ||
        !flux %in% c("1", "2")) stop("Sélection invalide")

    # Les codes pays sont filtres comme dans l'onglet structure.
    filtres_sql <- c(paste0("FLOW = ", qs(flux)),
                     "regexp_matches(PRODUCT_NC,'^[0-9]{8}$')")
    if (dim == "produit" && par$partner != "__ALL__")
      filtres_sql <- c(filtres_sql, paste0("PARTNER = ", qs(par$partner)))
    if (dim == "pays") {
      if (par$niveau != "TOUS" && par$produit != "__ALL__")
        filtres_sql <- c(filtres_sql, paste0("LEFT(PRODUCT_NC,",
          as.integer(par$niveau), ") = ", qs(par$produit)))
      codes <- unique(country_labels$code)
      codes <- codes[grepl("^[A-Z]{2}$", codes) &
        !codes %in% c("EU", "EA", "EZ", "XC", "XL", "XS", "XW", "XX", "ZZ")]
      filtres_sql <- c(filtres_sql, paste0("PARTNER IN (",
        paste(vapply(codes, qs, character(1)), collapse=","), ")"))
      champ <- "PARTNER"
    } else {
      champ <- paste0("LEFT(PRODUCT_NC,", as.integer(niv), ")")
    }
    resultats <- lapply(seq_len(nrow(fs)), function(i) {
      sql <- paste0("SELECT ", fs$annee[i], " AS annee, ", champ,
        " AS code, SUM(TRY_CAST(NULLIF(TRIM(VALUE_EUR),'') AS DOUBLE))/1e6 AS montant ",
        "FROM read_parquet(", qs(fs$path[i]), ") WHERE ",
        paste(filtres_sql, collapse=" AND "), " GROUP BY 2")
      dbGetQuery(con, sql)
    })
    d <- do.call(rbind, resultats)
    d <- d[!is.na(d$montant) & !is.na(d$code), , drop=FALSE]
    validate(need(nrow(d) > 0, "Pas de données pour cette sélection."))
    # Part par annee dans l'ensemble des categories de cette annee.
    total <- aggregate(montant ~ annee, d, sum)
    names(total)[2] <- "total"
    d <- merge(d, total, by="annee", all.x=TRUE)
    d$part <- ifelse(d$total > 0, 100*d$montant/d$total, NA_real_)
    # Top base sur la derniere annee avec observations.
    reference <- max(d$annee)
    ref <- d[d$annee == reference, c("code", "montant")]
    ref <- ref[order(-ref$montant), , drop=FALSE]
    n <- max(2L, min(12L, as.integer(input$evol_struct_top)))
    codes_top <- head(ref$code, n)
    d$groupe <- ifelse(d$code %in% codes_top, d$code, "AUTRES")
    x <- aggregate(part ~ annee + groupe, d, sum)
    grille <- expand.grid(annee = sort(unique(fs$annee)),
                         groupe = c(codes_top, "AUTRES"),
                         stringsAsFactors=FALSE)
    x <- merge(grille, x, by=c("annee", "groupe"), all.x=TRUE)
    # Une categorie absente d'une annee disponible a une part nulle.
    x$part[is.na(x$part)] <- 0
    if (dim == "pays") {
      noms <- nom_pays(codes_top)
    } else {
      version <- if (reference < 2022) "HS17" else "HS22"
      noms <- nom_produit(codes_top, as.integer(niv), version)
    }
    labels <- setNames(noms, codes_top)
    labels <- c(labels, AUTRES="Autres")
    x$libelle <- unname(labels[x$groupe])
    # Libelles complets pour le survol et le tableau ; libelles courts
    # dans la legende pour ne pas comprimer la surface du graphique.
    x$libelle_complet <- unname(labels[x$groupe])
    raccourcir <- function(nom, maximum=32L) {
      ifelse(nchar(nom) > maximum,
             paste0(substr(nom, 1L, maximum - 1L), "…"), nom)
    }
    noms_courts <- vapply(seq_along(codes_top), function(i) {
      code <- codes_top[i]
      complet <- noms[i]
      # Le code est conserve meme si sa description est tronquee.
      desc <- sub(paste0("^", code, "\\s*[-–—]\\s*"), "", complet)
      paste0(code, " — ", raccourcir(desc, 28L))
    }, character(1))
    labels_courts <- c(setNames(noms_courts, codes_top), AUTRES="Autres")
    x$libelle_court <- unname(labels_courts[x$groupe])
    x$libelle_court <- factor(x$libelle_court, levels=unname(labels_courts))
    # La colonne libelle est conservee pour les eventuels usages existants.
    x$libelle <- x$libelle_complet
    x
  })

  output$evol_structure_graph <- renderPlotly({
    d <- evolution_structure()
    # Supprimer le groupe residuel des graphiques de produits uniquement.
    # Les pourcentages restent rapportes au total original des echanges.
    if (identical(input$evol_struct_dimension, "produit")) {
      d <- d[d$groupe != "AUTRES", , drop = FALSE]
    }
    req(input$evol_struct_graph_type)
    d$texte <- paste0(
      "Année : ", d$annee,
      "<br>Catégorie : ", d$libelle_complet,
      "<br>Part : ", format(round(d$part, 2), decimal.mark=","), " %"
    )

    if (identical(input$evol_struct_graph_type, "empile")) {
      # Les barres affichent la somme des parts retenues dans le total.
      # Pour les produits, leur hauteur n'atteint donc pas forcement 100 %.
      g <- ggplot(d, aes(x=factor(annee), y=part,
                         fill=libelle_court, text=texte)) +
        geom_col(width=0.72, colour="white", linewidth=0.15) +
        scale_y_continuous(limits=c(0, 100), breaks=seq(0,100,20),
                           expand=expansion(mult=c(0,0.01))) +
        labs(title="Évolution de la structure des échanges",
             x="Année", y="Part dans le total (%)", fill="Catégorie") +
        theme_minimal(base_size=12) +
        theme(legend.position="bottom", legend.title=element_blank(),
              legend.text=element_text(size=9),
              panel.grid.major.x=element_blank()) +
        guides(fill=guide_legend(nrow=2, byrow=TRUE))
    } else {
      g <- ggplot(d, aes(x=annee, y=part, colour=libelle_court,
                         group=libelle_court, text=texte)) +
        geom_line(linewidth=1, na.rm=TRUE) +
        geom_point(size=2, na.rm=TRUE) +
        scale_x_continuous(breaks=sort(unique(d$annee))) +
        labs(title="Évolution de la structure des échanges",
             x="Année", y="Part dans le total (%)", colour="Catégorie") +
        theme_minimal(base_size=12) +
        theme(legend.position="bottom", legend.title=element_blank(),
              legend.text=element_text(size=9)) +
        guides(colour=guide_legend(nrow=2, byrow=TRUE))
    }

    ggplotly(g, tooltip="text") |>
      layout(
        hovermode="closest",
        legend=list(orientation="h", x=0, xanchor="left",
                    y=-0.22, yanchor="top", font=list(size=10)),
        margin=list(l=70, r=20, t=60, b=145),
        autosize=TRUE
      ) |>
      config(displaylogo=FALSE, responsive=TRUE)
  })

  output$evol_structure_table <- renderDT({
    d <- evolution_structure()
    d <- d[, c("annee", "libelle_complet", "part")]
    names(d) <- c("Année", "Catégorie", "Part (%)")
    datatable(d, rownames=FALSE, extensions="Buttons",
      options=list(pageLength=15, scrollX=TRUE, dom="Bfrtip",
                   buttons=c("copy", "csv", "excel"))) |>
      formatRound("Part (%)", digits=2)
  })

  output$tableau_annuel <- renderDT({
    d <- annuel()
    names(d)[names(d) == "annee"] <- "Année"
    names(d) <- sub("_variation$", " : variation annuelle (%)", names(d))
    datatable(d,rownames=FALSE,extensions='Buttons',
              options=list(pageLength=12,scrollX=TRUE,dom='Bfrtip',
                           buttons=c('copy','csv','excel'))) |>
      formatRound(names(d)[sapply(d,is.numeric) & names(d)!='Année'],digits=2)
  })
  
  # ----------------------------------------------------------
  # 4.14 TABLEAU MENSUEL
  # ----------------------------------------------------------
  
  output$tableau <- renderDT({
    
    x <- resultat()$series
    
    vars <- c(
      FLUX,
      paste0(FLUX, "_m"),
      paste0(FLUX, "_a")
    )
    
    d <- x[, c("periode", vars)]
    
    names(d) <- c(
      "Mois",
      FLUX,
      paste0(FLUX, " : mensuel (%)"),
      paste0(FLUX, " : annuel (%)")
    )
    
    datatable(
      d,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      )
    ) |>
      formatRound(
        names(d)[-1],
        digits = 2
      )
  })
  
  # ----------------------------------------------------------
  # 4.15 NOMENCLATURES
  # ----------------------------------------------------------
  
  output$nomenclature_table <- renderDT({
    
    n <- input$niveau
    
    validate(need(
      !is.null(n) && n != "TOUS",
      "Sélectionner SH2, SH4 ou SH6."
    ))
    
    ver <- if (
      !is.null(input$debut) &&
      as.integer(substr(input$debut, 1, 4)) < 2022
    ) {
      "HS17"
    } else {
      "HS22"
    }
    
    d <- hs_labels[
      hs_labels$version == ver &
        nchar(hs_labels$code) == as.integer(n),
      c("code", "libelle")
    ]
    
    names(d) <- c(
      "Code", "Libellé (anglais)"
    )
    
    datatable(
      d,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
  })
  
  # ==========================================================
  # 5. ASSISTANT IA COMEXT
  # ==========================================================
  
  ia_pret <- nzchar(
    Sys.getenv("OPENAI_API_KEY")
  )
  
  # ----------------------------------------------------------
  # 5.1 PREPARER LES REQUETES DU CHATBOT
  # ----------------------------------------------------------
  
  filtre_comext_ia <- function(
    debut,
    fin,
    partenaire = "__ALL__",
    niveau = "TOUS",
    produit = "__ALL__") {
    
    dates <- fichiers()
    
    if (
      !grepl("^[0-9]{6}$", debut) ||
      !grepl("^[0-9]{6}$", fin) ||
      debut > fin
    ) {
      stop("Période invalide : AAAAMM requis.")
    }
    
    if (!niveau %in% c("TOUS", "2", "4", "6")) {
      stop("Niveau de produit invalide.")
    }
    
    if (
      partenaire != "__ALL__" &&
      !grepl("^[A-Za-z0-9_]{2,5}$", partenaire)
    ) {
      stop("Code partenaire invalide.")
    }
    
    if (
      produit != "__ALL__" &&
      (
        niveau == "TOUS" ||
        !grepl("^[0-9]+$", produit) ||
        nchar(produit) != as.integer(niveau)
      )
    ) {
      stop("Code produit invalide.")
    }
    
    selection <- dates[
      dates$periode >= debut &
        dates$periode <= fin,
      ,
      drop = FALSE
    ]
    
    if (nrow(selection) == 0) {
      stop("Aucun fichier disponible.")
    }
    
    wh <- c(
      "FLOW IN ('1','2')",
      "regexp_matches(PRODUCT_NC,'^[0-9]{8}$')",
      paste0("PERIOD >= ", qs(debut)),
      paste0("PERIOD <= ", qs(fin))
    )
    
    if (partenaire != "__ALL__") {
      wh <- c(
        wh,
        paste0("PARTNER = ", qs(partenaire))
      )
    }
    
    if (
      niveau != "TOUS" &&
      produit != "__ALL__"
    ) {
      wh <- c(
        wh,
        paste0(
          "LEFT(PRODUCT_NC,",
          as.integer(niveau),
          ") = ",
          qs(produit)
        )
      )
    }
    
    list(
      source = source_sql(selection),
      where = paste(wh, collapse = " AND ")
    )
  }
  
  # ----------------------------------------------------------
  # 5.2 OUTIL : SERIES MENSUELLES
  # ----------------------------------------------------------
  
  get_comext_mensuel <- function(
    debut,
    fin,
    partenaire = "__ALL__",
    niveau = "TOUS",
    produit = "__ALL__") {
    
    tryCatch({
      
      f <- filtre_comext_ia(
        debut, fin,
        partenaire, niveau, produit
      )
      
      sql <- paste0(
        "SELECT PERIOD AS mois, ",
        "FLOW AS code_flux, ",
        "SUM(TRY_CAST(NULLIF(TRIM(VALUE_EUR),'') ",
        "AS DOUBLE))/1e6 AS montant_m_eur, ",
        "COUNT(*) AS lignes, ",
        "COUNT(*) FILTER (WHERE ",
        "NULLIF(TRIM(VALUE_EUR),'') IS NOT NULL ",
        "AND TRY_CAST(NULLIF(TRIM(VALUE_EUR),'') ",
        "AS DOUBLE) IS NULL) ",
        "AS valeurs_non_convertibles ",
        "FROM ", f$source,
        " WHERE ", f$where,
        " GROUP BY PERIOD,FLOW ",
        "ORDER BY PERIOD,FLOW"
      )
      
      res <- dbGetQuery(con, sql)
      
      if (!nrow(res)) {
        return("Aucune observation trouvée.")
      }
      
      res$flux <- ifelse(
        res$code_flux == "1",
        "Importations",
        "Exportations"
      )
      
      res[, c(
        "mois", "flux", "montant_m_eur",
        "lignes", "valeurs_non_convertibles"
      )]
      
    }, error = function(e) {
      paste(
        "Données indisponibles :",
        conditionMessage(e)
      )
    })
  }
  
  # ----------------------------------------------------------
  # 5.3 OUTIL : CLASSEMENTS
  # ----------------------------------------------------------
  
  get_comext_classement <- function(
    debut,
    fin,
    flux,
    dimension = "PARTNER",
    n = 10L,
    partenaire = "__ALL__",
    niveau = "TOUS",
    produit = "__ALL__") {
    
    tryCatch({
      
      f <- filtre_comext_ia(
        debut, fin,
        partenaire, niveau, produit
      )
      
      if (!flux %in% c("1", "2")) {
        stop("Flux invalide : 1 ou 2.")
      }
      
      if (!dimension %in% c(
        "PARTNER", "SH2", "SH4", "SH6"
      )) {
        stop("Dimension invalide.")
      }
      
      n <- as.integer(n)
      
      if (is.na(n) || n < 1 || n > 30) {
        stop("n doit être compris entre 1 et 30.")
      }
      
      dimsql <- switch(
        dimension,
        PARTNER = "PARTNER",
        SH2 = "LEFT(PRODUCT_NC,2)",
        SH4 = "LEFT(PRODUCT_NC,4)",
        SH6 = "LEFT(PRODUCT_NC,6)"
      )
      
      sql <- paste0(
        "SELECT ", dimsql, " AS code, ",
        "SUM(TRY_CAST(NULLIF(TRIM(VALUE_EUR),'') ",
        "AS DOUBLE))/1e6 AS montant_m_eur, ",
        "COUNT(*) FILTER (WHERE ",
        "NULLIF(TRIM(VALUE_EUR),'') IS NOT NULL ",
        "AND TRY_CAST(NULLIF(TRIM(VALUE_EUR),'') ",
        "AS DOUBLE) IS NULL) ",
        "AS valeurs_non_convertibles ",
        "FROM ", f$source,
        " WHERE ", f$where,
        " AND FLOW=", qs(flux),
        " GROUP BY 1 ",
        "ORDER BY montant_m_eur DESC NULLS LAST ",
        "LIMIT ", n
      )
      
      res <- dbGetQuery(con, sql)
      
      if (!nrow(res)) {
        return("Aucune observation trouvée.")
      }
      
      res$libelle <- if (
        dimension == "PARTNER"
      ) {
        nom_pays(res$code)
      } else {
        
        ver <- if (
          as.integer(substr(debut, 1, 4)) < 2022
        ) {
          "HS17"
        } else {
          "HS22"
        }
        
        nom_produit(
          res$code,
          as.integer(sub("SH", "", dimension)),
          ver
        )
      }
      
      res[, c(
        "code", "libelle", "montant_m_eur",
        "valeurs_non_convertibles"
      )]
      
    }, error = function(e) {
      paste(
        "Données indisponibles :",
        conditionMessage(e)
      )
    })
  }
  
  # ----------------------------------------------------------
  # 5.4 EXPOSER LES OUTILS A ELLMER
  # ----------------------------------------------------------
  
  outil_mensuel <- ellmer::tool(
    
    get_comext_mensuel,
    
    name = "get_comext_mensuel",
    
    description = paste(
      "Retourne les importations et exportations",
      "mensuelles COMEXT en millions d'euros.",
      "Utiliser pour les évolutions temporelles,",
      "les totaux et les soldes commerciaux."
    ),
    
    arguments = list(
      
      debut = ellmer::type_string(
        description = "Premier mois AAAAMM."
      ),
      
      fin = ellmer::type_string(
        description = "Dernier mois AAAAMM."
      ),
      
      partenaire = ellmer::type_string(
        description = "Code pays ou __ALL__.",
        required = FALSE
      ),
      
      niveau = ellmer::type_string(
        description = "TOUS, 2, 4 ou 6.",
        required = FALSE
      ),
      
      produit = ellmer::type_string(
        description = "Code produit ou __ALL__.",
        required = FALSE
      )
    )
  )
  
  outil_classement <- ellmer::tool(
    
    get_comext_classement,
    
    name = "get_comext_classement",
    
    description = paste(
      "Classe les partenaires ou les produits",
      "selon les montants commerciaux.",
      "Utiliser pour les principaux partenaires",
      "ou les principaux produits."
    ),
    
    arguments = list(
      
      debut = ellmer::type_string(
        description = "Premier mois AAAAMM."
      ),
      
      fin = ellmer::type_string(
        description = "Dernier mois AAAAMM."
      ),
      
      flux = ellmer::type_string(
        description = "1 = imports, 2 = exports."
      ),
      
      dimension = ellmer::type_string(
        description = "PARTNER, SH2, SH4 ou SH6.",
        required = FALSE
      ),
      
      n = ellmer::type_integer(
        description = "Nombre de résultats, 1 à 30.",
        required = FALSE
      ),
      
      partenaire = ellmer::type_string(
        description = "Code pays ou __ALL__.",
        required = FALSE
      ),
      
      niveau = ellmer::type_string(
        description = "TOUS, 2, 4 ou 6.",
        required = FALSE
      ),
      
      produit = ellmer::type_string(
        description = "Code produit ou __ALL__.",
        required = FALSE
      )
    )
  )
  
  # ----------------------------------------------------------
  # 5.5 PROMPT SYSTEME
  # ----------------------------------------------------------
  
  prompt_ia <- paste(
    
    "Tu es un assistant statisticien spécialisé",
    "dans le commerce extérieur du Luxembourg.",
    
    "Tu travailles dans une application Shiny",
    "utilisant les données Eurostat COMEXT.",
    
    "Pour toute question empirique concernant",
    "les échanges commerciaux, utilise les",
    "outils R/DuckDB disponibles.",
    
    "N'invente jamais un montant, un classement,",
    "un taux de variation ou une tendance.",
    
    "Les données concernent le pays déclarant LU.",
    
    "FLOW 1 correspond aux importations.",
    "FLOW 2 correspond aux exportations.",
    
    "Le solde commercial est égal aux",
    "exportations moins les importations.",
    
    "Respecte les périodes, pays et produits",
    "demandés par l'utilisateur.",
    
    "Les outils retournent des millions d'euros.",
    
    "Un mois sans observation n'est pas",
    "automatiquement égal à zéro.",
    
    "Ne calcule pas de taux de variation",
    "sur une base nulle ou manquante.",
    
    "Les classements limités à n catégories",
    "ne représentent pas nécessairement",
    "l'ensemble des échanges.",
    
    "Les codes SH2, SH4 et SH6 proviennent",
    "des codes NC8 de détail.",
    
    "HS2017 s'applique avant 2022.",
    "HS2022 s'applique à partir de 2022.",
    
    "Les nomenclatures peuvent évoluer.",
    "Signale les problèmes de comparabilité.",
    
    "Ne confonds jamais évolution en valeur",
    "et évolution en volume.",
    
    "Pour les questions conceptuelles,",
    "tu peux utiliser tes connaissances générales.",
    
    "Pour les questions portant sur les données,",
    "effectue un appel à un outil R.",
    
    "Les questions suivantes peuvent reprendre",
    "les paramètres des questions précédentes.",
    
    "Toute nouvelle demande empirique nécessite",
    "une nouvelle interrogation des données.",
    
    "Les filtres visibles du tableau de bord",
    "ne sont pas automatiquement transmis",
    "à l'assistant.",
    
    "Ne prétends pas avoir calculé une CVS",
    "ou une décomposition STL avec les outils IA,",
    "car ces fonctions ne sont pas exposées.",
    
    "Si les données sont absentes, indique-le.",
    
    "Ne confonds pas corrélation et causalité.",
    
    "Réponds en français, de manière claire",
    "et concise.",
    
    "Pour les tableaux, utilise le format Markdown.",
    
    "Mentionne Eurostat COMEXT comme source",
    "des données utilisées.",
    
    sep = "\n"
  )
  
  # ----------------------------------------------------------
  # 5.6 CREATION DU CHATBOT
  # ----------------------------------------------------------
  
  creer_chat <- function() {
    
    chat <- ellmer::chat_openai(
      system_prompt = prompt_ia
    )
    
    chat$register_tool(outil_mensuel)
    chat$register_tool(outil_classement)
    
    chat
  }
  
  # Conversation indépendante par session
  conversation_ia <- reactiveVal(NULL)
  
  # Aucun cadre de reponse n'est affiche avant une question.
  reponse_ia <- reactiveVal(NULL)

  ia_pret <- nzchar(
    Sys.getenv("OPENAI_API_KEY")
  )
  
  # ----------------------------------------------------------
  # 5.7 AFFICHER LA REPONSE
  # ----------------------------------------------------------
  
  output$reponse_ia <- renderUI({
    
    texte <- reponse_ia()
    if (is.null(texte) || !nzchar(texte)) return(NULL)
    
    if (!ia_pret) {
      
      return(
        div(
          class = "chat-answer",
          strong("Clé API absente."),
          p(
            "Définissez OPENAI_API_KEY dans",
            "le fichier .Renviron, puis",
            "redémarrez R."
          )
        )
      )
    }
    
    html <- tryCatch(
      commonmark::markdown_html(
        texte,
        extensions = TRUE
      ),
      error = function(e) {
        paste0(
          "<p>",
          htmltools::htmlEscape(texte),
          "</p>"
        )
      }
    )
    
    div(
      class = "chat-answer",
      HTML(html)
    )
  })
  
  # ============================================================
  # V29 - NOTES PDF : LES PARAMETRES VIENNENT UNIQUEMENT DU PROMPT
  # ============================================================
  # Une note n'appelle jamais resultat(), annuel(), filtres_stables()
  # ni calcul_structure(), car ces fonctions utilisent l'interface.
  # LLM -> plan JSON -> validation R -> calculs DuckDB -> rédaction
  # basée sur les résultats R -> PDF créé immédiatement.
  note_courante <- reactiveVal(NULL)
  note_message <- reactiveVal("Aucune note PDF demandée.")
  output$note_statut <- renderText(note_message())
  session$onSessionEnded(function() {
    n <- isolate(note_courante())
    if (!is.null(n) && !is.null(n$pdf) && file.exists(n$pdf)) unlink(n$pdf)
  })

  demande_note <- function(question) {
    nouvelle <- grepl("\\b(note|rapport|document|pdf)\\b", question,
                      ignore.case = TRUE, perl = TRUE) &&
      grepl("(r[eé]dig|pr[eé]par|g[eé]n[eé]r|produi|cr[eé]|pdf|export|ajout|modifi|corrig)",
            question, ignore.case = TRUE, perl = TRUE)
    modification <- !is.null(isolate(note_courante())) &&
      grepl("(ajout|modifi|corrig|retir|supprim|raccourci|d[eé]velopp|refai|actualis)",
            question, ignore.case = TRUE, perl = TRUE)
    nouvelle || modification
  }

  texte_nombre <- function(x, digits = 2) {
    if (length(x) != 1L || is.na(x) || !is.finite(x)) return("ND")
    format(round(x, digits), big.mark = " ", decimal.mark = ",", trim = TRUE)
  }

  # Lecture stricte d'une réponse JSON : pas d'inférence silencieuse.
  parser_json_ia <- function(texte) {
    texte <- trimws(as.character(texte))
    texte <- sub("^```(?:json)?\\s*", "", texte, perl = TRUE)
    texte <- sub("\\s*```$", "", texte, perl = TRUE)
    jsonlite::fromJSON(texte, simplifyVector = TRUE)
  }

  # Recherche hiérarchique dans les nomenclatures officielles.
  # 1) sélection SH2 (tous les chapitres), 2) niveau de détail SH4/SH6
  # sur les seuls chapitres pertinents. L'IA ne peut choisir que des codes
  # effectivement présents dans le référentiel CSV.
  mapper_produits <- function(demande, p) {
    versions <- if (p$debut < 2022 && p$fin >= 2022)
      c("HS17", "HS22") else if (p$fin < 2022) "HS17" else "HS22"
    ref <- hs_labels
    ref$code <- trimws(as.character(ref$code))
    ref$libelle <- as.character(ref$libelle)
    ref$version <- trimws(as.character(ref$version))
    ref <- ref[ref$version %in% versions & nchar(ref$code) %in% c(2L,4L,6L) &
                 grepl("^[0-9]+$", ref$code) & !is.na(ref$libelle), , drop=FALSE]
    if (!nrow(ref)) stop("Référentiel SH introuvable ou versions HS17/HS22 absentes.")
    # Signalement explicite du sujet de produit par l'agent de mapping.
    # Le plan classique n'est pas une source fiable de codes.
    explicit <- regmatches(demande, gregexpr("(?i)\\bSH[246]\\s*[:= -]?\\s*[0-9]{2,6}\\b", demande, perl=TRUE))[[1]]
    liste_chapitres <- unique(ref[ nchar(ref$code)==2L, c("code","libelle")])
    catalogue <- paste(paste(liste_chapitres$code, liste_chapitres$libelle, sep=" — "), collapse="\n")
    config <- paste(
      "Tu es un outil de correspondance entre langage naturel et nomenclature SH.",
      "Retourne exclusivement du JSON: {\"produits_demandes\":boolean,\"niveau\":\"2|4|6|TOUS\",\"chapitres\":[\"NN\"],\"justification\":\"...\"}.",
      "Utilise seulement les chapitres présents dans le catalogue fourni.",
      "Si l'utilisateur ne cible AUCUN produit (commerce total), produits_demandes=false, niveau=TOUS, chapitres=[].",
      "Si une famille de produits est visée, produits_demandes=true et choisis ses chapitres SH2.",
      "Choisis le niveau 4 ou 6 seulement si la catégorie demandée est clairement plus étroite qu'un chapitre SH2;",
      "sinon choisis 2. Ne confonds pas la demande de VENTILATION avec le périmètre produit.",
      "N'invente aucun code. Un concept économique transversal peut nécessiter plusieurs chapitres.",
      "Il n’existe aucune limite de 15 chapitres : les familles larges peuvent couvrir tout le catalogue.",
      "Un intitulé comme produits alimentaires demande une définition opérationnelle explicite dans justification.")
    agent <- ellmer::chat_openai(system_prompt=config)
    requete_mapping <- paste0(
      "DEMANDE : ", demande, "\nPLAN IA : ", as.character(p$consignes),
      "\nCATALOGUE SH2 :\n", catalogue)
    answer <- parser_json_ia(as.character(agent$chat(requete_mapping)))

    # Les grandes familles de produits peuvent relever de dizaines de
    # chapitres : aucune limite arbitraire au nombre de chapitres SH2.
    normaliser_chapitres <- function(reponse) {
      codes <- unique(trimws(as.character(unlist(reponse$chapitres, use.names = FALSE))))
      codes <- codes[!is.na(codes) & nzchar(codes)]
      codes <- sub("^(?i)SH\\s*2\\s*[-:=]?\\s*", "", codes, perl = TRUE)
      codes <- ifelse(grepl("^[0-9]$", codes), paste0("0", codes), codes)
      codes
    }
    chap <- normaliser_chapitres(answer)
    valides <- function(codes) length(codes) > 0L &&
      all(codes %in% liste_chapitres$code)

    # Un premier refus du modèle n'est pas assimilé à une absence de produits.
    # Nouvelle tentative contrainte par le catalogue officiel.
    if (isTRUE(answer$produits_demandes) && !valides(chap)) {
      reprise <- paste0(
        "La sélection précédente ne contient aucun ensemble valide de codes SH2. ",
        "La demande porte bien sur des produits. Réessaie. ",
        "Une famille générale peut couvrir PLUS DE 15 chapitres : sélectionne ",
        "tous les chapitres pertinents du catalogue, sans inventer de code. ",
        "Si le périmètre conceptuel est contestable, choisis une définition ",
        "opérationnelle explicite dans justification. ",
        "Retourne exclusivement le même schéma JSON.\n",
        requete_mapping)
      answer <- parser_json_ia(as.character(agent$chat(reprise)))
      chap <- normaliser_chapitres(answer)
    }

    if (!isTRUE(answer$produits_demandes)) {
      # La demande explicite une famille, mais le modèle a indiqué 'aucun produit' :
      # ne pas transformer silencieusement une note sectorielle en commerce total.
      mots_produit <- grepl("(?i)\\b(produit|marchandise|aliment|p[eé]trole|gaz|acier|v[eé]hicule|pharma|[eé]nergie)\\b",
                            demande, perl = TRUE)
      if (mots_produit)
        stop("Mapping SH non résolu : une catégorie de produits a été demandée, mais aucun code n'a été identifié. Précisez son périmètre.")
      return(list(niveau="TOUS", codes_par_version=list(HS17=character(),HS22=character()),libelles=character()))
    }
    niveau <- as.character(answer$niveau)
    if (!niveau %in% c("2","4","6")) stop("Niveau de mapping SH non valide.")
    if (!valides(chap)) {
      invalides <- setdiff(chap, liste_chapitres$code)
      stop(paste0("Mapping SH2 non résolu. Codes non reconnus : ",
                  paste(invalides, collapse = ", "),
                  ". Vérifier la sélection dans la nomenclature."))
    }
    # Pour les familles conceptuelles larges, rester au SH2 pour préserver
    # la couverture plutôt que forcer une ventilation restrictive au SH4/SH6.
    if (length(chap) > 1L && niveau != "2") niveau <- "2"
    # Recherche d'un sous-ensemble SH4, puis éventuellement SH6.
    # Les candidats sont limités aux sous-catégories de chapitres validés.
    extraire <- function(version, n, prefixes, concept) {
      candidats <- ref[ref$version==version & nchar(ref$code)==n &
        substr(ref$code,1,nchar(prefixes[1])) %in% prefixes, c("code","libelle"), drop=FALSE]
      candidats <- unique(candidats)
      if (!nrow(candidats)) stop(paste("Aucun code SH",n,"pour",version))
      if (nrow(candidats)>600L)
        stop("La catégorie couvre trop de sous-codes pour une sélection précise; préciser la catégorie ou utiliser SH2.")
      agent2 <- ellmer::chat_openai(system_prompt=paste(
        "Tu sélectionnes les codes SH correspondant EXACTEMENT à la catégorie demandée.",
        "Retourne UNIQUEMENT {\"codes\":[\"...\"],\"justification\":\"...\"}.",
        "Choisis UNIQUEMENT parmi les codes du catalogue. Exclure les catégories non pertinentes.",
        "Ne renvoie pas tous les codes d'un chapitre pour une demande ciblée.",
        "Si les libellés ne permettent pas une sélection fiable, retourne une liste vide."))
      res <- parser_json_ia(as.character(agent2$chat(paste0(
        "DEMANDE : ",concept,"\nVersion : ",version,"\nCODES AUTORISÉS :\n",
        paste(paste(candidats$code, candidats$libelle, sep=" — "), collapse="\n")))))
      choisis <- unique(as.character(unlist(res$codes, use.names=FALSE)))
      if (!length(choisis) || !all(choisis %in% candidats$code))
        stop(paste("Mapping SH",n,"non résolu pour",version,"; veuillez préciser le produit."))
      choisis
    }
    par_version <- list(HS17=character(),HS22=character())
    for (version in versions) {
      if (niveau=="2") {
        codes <- chap
      } else {
        codes4 <- extraire(version,4,chap,demande)
        codes <- if (niveau=="4") codes4 else extraire(version,6,codes4,demande)
      }
      connus <- ref$code[ref$version==version & nchar(ref$code)==as.integer(niveau)]
      if (!all(codes %in% connus)) stop(paste("Mapping non valide dans", version))
      par_version[[version]] <- codes
    }
    # Pour une période sur une seule version, renseigner l'autre version aussi
    # uniquement pour simplifier les structures internes (elle ne sera pas interrogée).
    if (length(versions)==1L) par_version[[setdiff(c("HS17","HS22"), versions)]] <- character()
    labels <- unique(unlist(lapply(versions, function(v) {
      codes <- par_version[[v]]
      paste0(v, ": ", codes, " — ", ref$libelle[match(paste(v,codes),paste(ref$version,ref$code))])
    })))
    list(niveau=niveau, codes_par_version=par_version, libelles=labels)
  }

  # L'IA détermine une intention ; R vérifie toutes les valeurs avant SQL.
  plan_note <- function(demande) {
    if (!ia_pret) stop("OPENAI_API_KEY absente : impossible d'interpréter la demande de note.")
    if (grepl("(mensuel|mensuelle|par mois|saisonnalit[eé]|trimestriel|trimestrielle)",
              demande, ignore.case=TRUE, perl=TRUE))
      stop("Cette première V22 produit des notes sur les séries annuelles. Les analyses mensuelles ou de saisonnalité nécessitent un outil dédié.")
    disponibles <- fichiers_annuels()$annee
    partenaires <- unique(country_labels[, c("code", "nom")])
    liste_pays <- paste(utils::head(paste0(partenaires$code, "=", partenaires$nom), 250L),
                        collapse = "; ")
    instruction <- paste(
      "Tu transformes une demande de note COMEXT en paramètres d'analyse pour R.",
      "Retourne UNIQUEMENT un objet JSON valide, sans markdown ni prose.",
      "Champs obligatoires : titre (texte), debut (année entière), fin (année entière),",
      "flux ('1' imports, '2' exports, 'both' pour les deux),",
      "partenaire (code pays exact ou '__ALL__'),",
      "niveau ('TOUS','2','4','6'), produit (code ou '__ALL__'),",
      "top (entier 1..15), graphiques (booléen),",
      "analyse_partenaires (booléen), analyse_produits (booléen),",
      "consignes (texte fidèle à la demande: structure, questions, longueur et commentaires).",
      "Période disponible (annuel) :", paste(range(disponibles), collapse = " à "), ".",
      "Si l'utilisateur omet la période, choisis toute la période disponible.",
      "Si l'utilisateur parle de ses échanges sans préciser le flux : 'both'.",
      "IMPORTANT : LU est TOUJOURS le pays DECLARANT, jamais le pays partenaire par défaut.",
      "Les expressions 'au Luxembourg', 'du Luxembourg', 'luxembourgeois',",
      "'importations au Luxembourg' ou 'exportations luxembourgeoises' désignent",
      "le DECLARANT et ne doivent PAS définir partenaire='LU'.",
      "Si aucun pays partenaire étranger n'est explicitement demandé,",
      "partenaire DOIT être '__ALL__'.",
      "Ne fixe un code partenaire que pour un pays explicitement nommé comme",
      "contrepartie (par exemple 'depuis la Belgique', 'vers la France',",
      "'avec l'Allemagne'). Une demande 'par pays partenaire' signifie",
      "analyse_partenaires=true ET partenaire='__ALL__' si aucun pays n'est ciblé.",
      "Pour un pays donné, n'utilise qu'un code du dictionnaire suivant :", liste_pays,
      "Pour les produits désignés par un nom, renseigne produit='__ALL__' et niveau='TOUS' :",
      "un second agent consultera obligatoirement le référentiel officiel SH avant SQL.",
      "Pour une demande sans catégorie de produit, utilise aussi produit='__ALL__'.",
      "Ne fabrique aucun chiffre COMEXT. Les valeurs proviendront uniquement de R."
    )
    agent <- ellmer::chat_openai(system_prompt = instruction)
    previous <- note_courante()
    contexte <- if (!is.null(previous)) paste0(
      "\nNOTE PRÉCÉDENTE (pour tenir compte d'éventuelles modifications) :\n",
      previous$demande, "\nPLAN PRÉCÉDENT :\n",
      jsonlite::toJSON(previous$plan, auto_unbox = TRUE)) else ""
    raw <- as.character(agent$chat(paste0("DEMANDE :\n", demande, contexte)))
    p <- parser_json_ia(raw)
    champs <- c("titre", "debut", "fin", "flux", "partenaire", "niveau", "produit",
                "top", "graphiques", "analyse_partenaires", "analyse_produits", "consignes")
    if (!all(champs %in% names(p))) stop("Plan IA incomplet. Veuillez préciser votre demande.")
    p$debut <- as.integer(p$debut); p$fin <- as.integer(p$fin)
    p$top <- as.integer(p$top)
    if (anyNA(c(p$debut,p$fin,p$top)) || p$debut > p$fin ||
        p$debut < min(disponibles) || p$fin > max(disponibles))
      stop("Période incorrecte ou indisponible dans les Parquet annuels.")
    if (p$top < 1 || p$top > 15) stop("Top N invalide.")
    if (!p$flux %in% c("1", "2", "both")) stop("Flux de note non reconnu.")
    # Neutraliser la confusion courante DECLARANT LU / PARTNER LU.
    # Si la demande cite explicitement un AUTRE pays partenaire et que l'IA
    # renvoie LU, ne pas élargir silencieusement le champ à tous les pays.
    p$partenaire <- toupper(trimws(as.character(p$partenaire)))
    if (identical(p$partenaire, "LU")) {
      normaliser <- function(x) {
        x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
        tolower(x)
      }
      demande_norm <- normaliser(demande)
      # Les noms des pays du référentiel permettent de repérer si un autre
      # pays a effectivement été cité. Ne pas confondre 'Luxembourg' avec LU.
      noms_etrangers <- unique(country_labels$nom[
        country_labels$code != "LU" &
        grepl("^[A-Z]{2}$", country_labels$code) &
        !is.na(country_labels$nom) & nchar(country_labels$nom) >= 4L
      ])
      noms_etrangers <- normaliser(noms_etrangers)
      noms_etrangers <- noms_etrangers[nzchar(noms_etrangers)]
      # Comparaison par mots entiers pour limiter les faux positifs.
      pays_mentions <- any(vapply(noms_etrangers, function(nom) {
        grepl(nom, demande_norm, fixed = TRUE)
      }, logical(1)))
      if (pays_mentions) {
        stop(paste(
          "Interprétation incohérente : l'IA a sélectionné PARTNER=LU",
          "alors que la demande mentionne un autre pays partenaire.",
          "Reformulez le pays partenaire ou relancez la note."
        ))
      }
      p$partenaire <- "__ALL__"
      p$consignes <- paste0(as.character(p$consignes),
        " Périmètre confirmé par R : LU est le pays déclarant ;",
        " aucun partenaire étranger spécifique n'a été demandé,",
        " donc tous les pays partenaires sont retenus.")
    }
    if (p$partenaire != "__ALL__" &&
        !p$partenaire %in% country_labels$code) stop("Partenaire non reconnu.")
    # V30 : mapping GENERIQUE des produits (SH2/SH4/SH6), aucun cas en dur.
    # Le mapping est distinct du plan : l'agent de mapping voit les vrais libelles.
    p$niveau <- trimws(as.character(p$niveau))
    p$niveau <- sub("^SH", "", toupper(p$niveau))
    p$produit <- trimws(as.character(p$produit))
    if (toupper(p$produit) %in% c("TOUS", "ALL", "__ALL__", ""))
      p$produit <- "__ALL__"
    p$codes_par_version <- list(HS17=character(), HS22=character())
    p$libelles_retenus <- character()
    mapping <- mapper_produits(demande, p)
    p$codes_par_version <- mapping$codes_par_version
    p$libelles_retenus <- mapping$libelles
    if (p$niveau != "TOUS") {
      p$consignes <- paste0(as.character(p$consignes),
        " Définir précisément le périmètre des produits sélectionnés par codes SH,",
        " ne pas le présenter comme une classification économique exhaustive",
        " sans validation externe.")
    }
    p$niveau <- mapping$niveau
    p$produit <- if (p$niveau == "TOUS") "__ALL__" else
      paste(unique(unlist(p$codes_par_version, use.names=FALSE)), collapse=",")
    if (p$niveau != "TOUS") {
      p$consignes <- paste0(as.character(p$consignes),
        " Périmètre SH vérifié dans le référentiel : niveau SH", p$niveau,
        "; codes par version : HS17=",
        paste(p$codes_par_version$HS17, collapse=","),
        "; HS22=", paste(p$codes_par_version$HS22, collapse=","),
        ". Ne pas assimiler une catégorie plus large à un sous-produit précis.")
    }

    # Une demande de ventilation est indépendante du filtre produit.
    if (grepl("pays partenaire|par partenaire|selon le pays|par pays",
              demande, ignore.case = TRUE, perl = TRUE)) p$analyse_partenaires <- TRUE
    if (grepl("par produit|cat[eé]gorie de produit|ventilation par produit|code SH2",
              demande, ignore.case = TRUE, perl = TRUE)) p$analyse_produits <- TRUE

    # Valider seulement APRES normalisation.
    if (!p$niveau %in% c("TOUS", "2", "4", "6"))
      stop(paste0("Niveau SH invalide dans le plan IA : ", p$niveau))
    versions_utilisees <- if (p$debut < 2022 && p$fin >= 2022)
      c("HS17","HS22") else if (p$fin < 2022) "HS17" else "HS22"
    if (p$niveau != "TOUS" && !all(vapply(p$codes_par_version[versions_utilisees],
        function(z) length(z)>0 && all(grepl(paste0("^[0-9]{",p$niveau,"}$"),z)),
        logical(1)))) stop("Codes SH non validés pour les nomenclatures utilisées.")
    for (nom in c("graphiques", "analyse_partenaires", "analyse_produits"))
      if (!is.logical(p[[nom]]) || length(p[[nom]]) != 1 || is.na(p[[nom]]))
        stop(paste("Option du plan invalide :", nom))
    p$titre <- substr(trimws(as.character(p$titre)), 1, 180)
    p$consignes <- substr(as.character(p$consignes), 1, 4000)
    if (!nzchar(p$titre)) stop("Titre de note vide.")
    p
  }

  # Fonction SQL indépendante des contrôles Shiny.
  base_sql_note <- function(p, version) {
    w <- c("FLOW IN ('1','2')", "regexp_matches(PRODUCT_NC,'^[0-9]{8}$')")
    if (p$flux != "both") w <- c(w, paste0("FLOW = ", qs(p$flux)))
    if (p$partenaire != "__ALL__") w <- c(w, paste0("PARTNER = ", qs(p$partenaire)))
    if (p$niveau != "TOUS") {
      codes <- p$codes_par_version[[version]]
      if (!length(codes)) stop(paste("Aucun mapping produit pour", version))
      w <- c(w, paste0("LEFT(PRODUCT_NC,", as.integer(p$niveau), ") IN (",
        paste(vapply(codes, qs, character(1)), collapse=","), ")"))
    }
    paste(w, collapse = " AND ")
  }

  calculs_note <- function(p) {
    fa <- fichiers_annuels()
    fs <- fa[fa$annee >= p$debut & fa$annee <= p$fin, , drop = FALSE]
    if (!nrow(fs)) stop("Aucun fichier annuel correspondant au prompt.")
    q <- vapply(seq_len(nrow(fs)), function(i) paste0(
      "SELECT ", fs$annee[i], " AS annee, FLOW, ",
      "SUM(TRY_CAST(NULLIF(TRIM(VALUE_EUR),'') AS DOUBLE))/1e6 AS montant, ",
      "COUNT(*) FILTER (WHERE NULLIF(TRIM(VALUE_EUR),'') IS NOT NULL AND ",
      "TRY_CAST(NULLIF(TRIM(VALUE_EUR),'') AS DOUBLE) IS NULL) AS invalides ",
      "FROM read_parquet(", qs(fs$path[i]), ") WHERE ",
      base_sql_note(p, if (fs$annee[i]<2022) "HS17" else "HS22"),
      " GROUP BY FLOW"), character(1))
    a <- dbGetQuery(con, paste(q, collapse = " UNION ALL "))
    if (!nrow(a)) {
      # Afficher la sélection effective pour diagnostiquer les plans IA erronés.
      # Aucun élargissement silencieux des critères de l'utilisateur.
      diag_par_fichier <- vapply(seq_len(nrow(fs)), function(i) {
        qdiag <- paste0("SELECT COUNT(*) AS n FROM read_parquet(",
                        qs(fs$path[i]), ") WHERE FLOW IN ('1','2')")
        tryCatch(as.numeric(dbGetQuery(con, qdiag)$n[1]),
                 error=function(e) NA_real_)
      }, numeric(1))
      stop(paste0(
        "Aucune ligne COMEXT pour la sélection interprétée : période ",
        p$debut, "–", p$fin, "; flux=", p$flux,
        "; partenaire=", p$partenaire,
        "; niveau SH=", p$niveau,
        "; code produit=", p$produit,
        ". Nombre de lignes brutes par année : ",
        paste(paste0(fs$annee, "=", diag_par_fichier), collapse=", "),
        ". Vérifier le code et la couverture des fichiers annuels."
      ))
    }
    a$flux <- ifelse(a$FLOW == "1", "Importations", "Exportations")
    a <- a[order(a$annee, a$flux), c("annee", "flux", "montant", "invalides"), drop = FALSE]
    a$variation_pct <- NA_real_
    for (fl in unique(a$flux)) {
      ids <- which(a$flux == fl)
      if (length(ids) > 1) for (j in 2:length(ids)) {
        i <- ids[j]; prec <- ids[j-1]
        if (a$annee[i] == a$annee[prec]+1 && is.finite(a$montant[prec]) && a$montant[prec] != 0)
          a$variation_pct[i] <- 100 * (a$montant[i]/a$montant[prec]-1)
      }
    }
    # Classements de l'année terminale : les calculs sont réalisés dans DuckDB.
    classements <- function(dim) {
      path <- fs$path[nrow(fs)]
      # Pour la ventilation produit, descendre d'un niveau sous le filtre.
      niveau_ventilation <- if (p$niveau %in% c("TOUS", "6")) {
        if (p$niveau == "6") "6" else "2"
      } else if (p$niveau == "2") "4" else "6"
      champ <- if (dim == "pays") "PARTNER" else
        paste0("LEFT(PRODUCT_NC,", niveau_ventilation, ")")
      w <- base_sql_note(p, if (fs$annee[nrow(fs)] < 2022) "HS17" else "HS22")
      if (dim == "pays") {
        codes <- unique(country_labels$code)
        codes <- codes[grepl("^[A-Z]{2}$", codes) &
          !codes %in% c("EU","EA","EZ","XC","XL","XS","XW","XX","ZZ")]
        w <- paste0(w, " AND PARTNER IN (", paste(vapply(codes, qs, character(1)), collapse=","), ")")
      }
      sql <- paste0("SELECT ", champ, " AS code, FLOW, ",
        "SUM(TRY_CAST(NULLIF(TRIM(VALUE_EUR),'') AS DOUBLE))/1e6 AS montant ",
        "FROM read_parquet(", qs(path), ") WHERE ", w,
        " GROUP BY 1,2 ORDER BY montant DESC")
      d <- dbGetQuery(con, sql)
      d <- d[!is.na(d$montant) & !is.na(d$code), , drop = FALSE]
      if (!nrow(d)) return(d)
      d$flux <- ifelse(d$FLOW == "1", "Importations", "Exportations")
      d <- do.call(rbind, lapply(split(d, d$flux), function(z) {
        z$part <- if (sum(z$montant) > 0) 100*z$montant/sum(z$montant) else NA_real_
        utils::head(z, p$top)
      }))
      rownames(d) <- NULL
      d$libelle <- if (dim == "pays") nom_pays(d$code) else
        nom_produit(d$code, as.integer(niveau_ventilation),
                    if (p$fin < 2022) "HS17" else "HS22")
      d[, c("flux", "code", "libelle", "montant", "part"), drop = FALSE]
    }
    list(annuel = a,
         pays = if (p$analyse_partenaires) classements("pays") else NULL,
         produits = if (p$analyse_produits) classements("produits") else NULL)
  }

  # Dossier de faits réduit : le modèle n'a pas accès aux fichiers pour inventer des valeurs.
  dossier_note <- function(p, d) {
    formater <- function(z) {
      if (is.null(z) || !nrow(z)) return("Non demandé / indisponible")
      paste(capture.output(print(z, row.names = FALSE, digits = 9)), collapse = "\n")
    }
    paste("DEMANDE UTILISATEUR :", p$consignes, "\nPÉRIODE :", p$debut, "-", p$fin,
          "\nUNITÉ : millions d'euros, prix courants (valeur)",
          "\nFILTRES : flux", p$flux, "; partenaire", p$partenaire,
          "; produit", p$produit, "; SH", p$niveau,
          "; libellés SH validés : ", paste(p$libelles_retenus, collapse=" | "),
          "\nSÉRIES ANNUELLES VALIDÉES PAR DUCKDB :\n", formater(d$annuel),
          "\nCLASSEMENT PARTENAIRES (année finale) :\n", formater(d$pays),
          "\nCLASSEMENT PRODUITS (année finale) :\n", formater(d$produits))
  }

  rediger_note <- function(p, d) {
    if (!ia_pret) stop("Clé OpenAI absente : rédaction impossible.")
    consigne <- paste(
      "Tu rédiges une note statistique COMEXT en français, en Markdown.",
      "SOURCE DE VÉRITÉ : tous les nombres COMEXT, taux, parts, classements et tendances",
      "doivent provenir EXCLUSIVEMENT du dossier chiffré calculé par R/DuckDB ci-dessous.",
      "N'invente aucun résultat et ne complète pas les observations manquantes.",
      "Les connaissances générales sont AUTORISÉES pour expliquer l'économie, les mécanismes",
      "et le contexte historique/institutionnel; sépare toujours faits et hypothèses.",
      "Ne cite pas de statistiques externes chiffrées, de faits récents non vérifiés,",
      "ni de références bibliographiques non vérifiées.",
      "Écris une introduction, une analyse appuyée sur les faits, une mise en perspective",
      "économique prudente, les limites méthodologiques et une conclusion.",
      "Le tableau et les figures seront ajoutés par R séparément : tu peux les commenter",
      "mais ne fabrique pas de tableau Markdown chiffré.",
      "Les données sont en valeur et non en volume; la nomenclature SH change en 2022.",
      "Ne dis jamais qu'un résultat est causé par un événement sans preuve.",
      "Mentionne Eurostat COMEXT comme source des données; toutes les autres",
      "interprétations sont des analyses et non des mesures COMEXT."
    )
    redacteur <- ellmer::chat_openai(system_prompt = consigne)
    as.character(redacteur$chat(paste("Rédige la note suivante :\n", dossier_note(p,d))))
  }

  # V25 - PDF via Quarto + Typst : pas de navigateur Chrome requis.
  # Quarto CLI (avec Typst intégré) doit être installé SUR LE SERVEUR.
  # La production reste synchrone et indépendante des filtres du tableau de bord.
  trouver_quarto <- function() {
    q <- Sys.which("quarto")
    if (nzchar(q) && file.exists(q)) return(unname(q))
    if (requireNamespace("quarto", quietly = TRUE)) {
      q <- tryCatch(quarto::quarto_path(), error = function(e) "")
      if (length(q) == 1L && nzchar(q) && file.exists(q)) return(q)
    }
    stop(paste0(
      "Quarto CLI introuvable sur ce serveur. ",
      "Le package R quarto seul ne suffit pas : installer/configurer Quarto ",
      "dans l'environnement d'exécution Posit Connect Cloud."
    ))
  }

  # Tableaux Markdown générés exclusivement à partir des objets de calcul R.
  # Aucune valeur statistique n'est régénérée par le modèle IA.
  tableau_md_note <- function(d, max_lignes = 30L) {
    if (is.null(d) || !nrow(d)) return("*Aucune donnée disponible.*")
    d <- utils::head(d, max_lignes)
    if ("invalides" %in% names(d)) d$invalides <- NULL
    noms <- names(d)
    noms[noms == "annee"] <- "Année"
    noms[noms == "flux"] <- "Flux"
    noms[noms == "libelle"] <- "Libellé"
    noms[noms == "montant"] <- "Montant (M€)"
    noms[noms == "variation_pct"] <- "Variation (%)"
    noms[noms == "part"] <- "Part (%)"
    names(d) <- noms
    valeurs <- lapply(d, function(x) {
      if (is.numeric(x)) {
        vapply(x, texte_nombre, character(1))
      } else {
        as.character(x)
      }
    })
    d <- as.data.frame(valeurs, check.names = FALSE, stringsAsFactors = FALSE)
    echapper <- function(z) {
      z[is.na(z)] <- "ND"
      z <- gsub("\\", "\\\\", z, fixed = TRUE)
      z <- gsub("|", "\\|", z, fixed = TRUE)
      gsub("[\r\n]+", " ", z)
    }
    colonnes <- vapply(names(d), echapper, character(1))
    entete <- paste0("| ", paste(colonnes, collapse = " | "), " |")
    separateur <- paste0("| ", paste(rep("---", ncol(d)), collapse = " | "), " |")
    lignes <- vapply(seq_len(nrow(d)), function(i) {
      val <- vapply(d[i, , drop = FALSE], as.character, character(1))
      paste0("| ", paste(echapper(val), collapse = " | "), " |")
    }, character(1))
    paste(c(entete, separateur, lignes), collapse = "\n")
  }

  creer_pdf_note <- function(n, chemin) {
    cli <- trouver_quarto()
    td <- tempfile("comext_pdf_v25_")
    dir.create(td, recursive = TRUE)
    on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

    # Graphique déterministe issu des données de R; fonctionne aussi sur une seule année.
    image_md <- ""
    if (isTRUE(n$plan$graphiques)) {
      d <- n$donnees$annuel
      if (!nrow(d)) stop("Aucune donnée annuelle pour le graphique PDF.")
      g <- ggplot2::ggplot(d, ggplot2::aes(x = annee, y = montant, colour = flux, group = flux))
      if (length(unique(d$annee)) >= 2L) {
        g <- g + ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 2)
      } else {
        g <- ggplot2::ggplot(d, ggplot2::aes(x = flux, y = montant, fill = flux)) +
          ggplot2::geom_col(width = 0.6) + ggplot2::labs(x = "Flux") +
          ggplot2::guides(fill = "none")
      }
      g <- g + ggplot2::labs(y = "Millions d'euros", colour = "Flux") +
        ggplot2::theme_minimal(base_size = 11)
      if (length(unique(d$annee)) >= 2L)
        g <- g + ggplot2::scale_x_continuous(breaks = sort(unique(d$annee))) +
          ggplot2::labs(x = "Année")
      img <- file.path(td, "evolution.png")
      ggplot2::ggsave(img, g, width = 7.2, height = 3.8, dpi = 160)
      image_md <- "\n\n## Graphique des échanges\n\n![Évolution des flux commerciaux calculée par R](evolution.png){width=95%}\n"
    }

    # Neutraliser les éventuels marqueurs de figures fictives du texte IA :
    # le seul graphique publié est celui calculé par R.
    texte <- as.character(n$texte)
    texte <- gsub("(?m)^\\s*\\*?\\*?\\[?(?:Figure|Graphique)\\s*[0-9]+[^\\n]*à insérer[^\\n]*$",
                  "", texte, perl = TRUE, ignore.case = TRUE)
    titre_yaml <- gsub("[\r\n]+", " ", n$plan$titre)
    titre_yaml <- gsub('"', "'", titre_yaml, fixed = TRUE)
    en_tete <- c(
      "---",
      paste0('title: "', titre_yaml, '"'),
      'subtitle: "Note statistique — Eurostat COMEXT"',
      paste0('date: "', format(Sys.Date(), "%d/%m/%Y"), '"'),
      "lang: fr",
      "format:",
      "  typst:",
      "    toc: false",
      "    number-sections: false",
      "    papersize: a4",
      "    margin:",
      "      x: 18mm",
      "      y: 18mm",
      "execute:",
      "  enabled: false",
      "---",
      ""
    )
    sections <- c(
      paste0("*Période : ", n$plan$debut, "–", n$plan$fin,
             ". Montants en millions d'euros courants.*"),
      "", texte, "",
      "## Données annuelles calculées avec R / DuckDB", "",
      tableau_md_note(n$donnees$annuel), "", image_md
    )
    if (!is.null(n$donnees$pays)) {
      sections <- c(sections, "", "## Principaux partenaires", "",
                    tableau_md_note(n$donnees$pays), "")
    }
    if (!is.null(n$donnees$produits)) {
      sections <- c(sections, "", "## Principaux produits", "",
                    tableau_md_note(n$donnees$produits), "")
    }
    sections <- c(sections, "", "---", "",
      "*Source des données et des résultats chiffrés : Eurostat COMEXT, traitements R/DuckDB. Les commentaires contextuels ne constituent pas des mesures COMEXT.*")
    qmd <- file.path(td, "note.qmd")
    writeLines(enc2utf8(c(en_tete, sections)), qmd, useBytes = TRUE)
    # V28 : exécuter Quarto dans le dossier temporaire de la note.
    # system2() hérite du répertoire de travail global de Shiny et, selon
    # le contexte Quarto, --output-dir ne suffit pas à imposer ce chemin.
    # processx::run(wd = td) isole le dossier de travail de chaque processus.
    if (!requireNamespace("processx", quietly = TRUE)) {
      stop("Le package R 'processx' est nécessaire à la génération PDF.")
    }
    resultat <- tryCatch(
      processx::run(
        command = cli,
        args = c("render", "note.qmd", "--to", "typst",
                 "--output", "note.pdf", "--no-execute"),
        wd = td,
        error_on_status = FALSE,
        echo = FALSE,
        timeout = 180000
      ),
      error = function(e) stop("Impossible de lancer Quarto : ", conditionMessage(e))
    )
    pdf_produit <- file.path(td, "note.pdf")
    code <- resultat$status
    if (!identical(as.integer(code), 0L) || !file.exists(pdf_produit)) {
      details <- paste(c(resultat$stdout, resultat$stderr), collapse = "\n")
      stop(paste0(
        "Échec Quarto/Typst (code ", code, "; PDF présent : ",
        file.exists(pdf_produit), "). Journal : ",
        substr(details, max(1L, nchar(details) - 1800L), nchar(details))
      ))
    }
    ok_copie <- file.copy(pdf_produit, chemin, overwrite = TRUE)
    if (!ok_copie || !file.exists(chemin) ||
        is.na(file.info(chemin)$size) || file.info(chemin)$size < 1000) {
      stop("Le PDF Quarto/Typst n'a pas pu être copié dans le fichier à télécharger.")
    }
    invisible(chemin)
  }

  # Les pourcentages correspondent aux étapes terminées (pas à une estimation
  # du temps restant). Shiny affiche une jauge pendant l'opération synchrone.
  preparer_note <- function(demande) {
    note_message("Préparation de la note en cours…")
    shiny::withProgress(message = "Génération de la note PDF", value = 0, {
      tryCatch({
        shiny::setProgress(value = 0.05, detail = "1/4 — Interprétation et mapping des codes SH…")
        p <- plan_note(demande)
        shiny::setProgress(value = 0.25, detail = "2/4 — Extraction et calcul des résultats COMEXT (DuckDB)…")
        d <- calculs_note(p)
        shiny::setProgress(value = 0.50, detail = "3/4 — Rédaction et mise en perspective économique (IA)…")
        texte <- rediger_note(p, d)
        if (!nzchar(trimws(texte))) stop("L'IA a retourné une note vide.")
        n <- list(plan=p, donnees=d, texte=texte, demande=demande, date=Sys.time())
        chemin <- tempfile("COMEXT_note_V25_", fileext=".pdf")
        shiny::setProgress(value = 0.75, detail = "4/4 — Mise en page et création du PDF…")
        creer_pdf_note(n, chemin)
        n$pdf <- chemin
        ancien <- isolate(note_courante())
        note_courante(n)
        if (!is.null(ancien) && !is.null(ancien$pdf) && file.exists(ancien$pdf))
          unlink(ancien$pdf)
        shiny::setProgress(value = 1, detail = "PDF prêt")
        note_message(paste0("Note prête : ", n$plan$titre, ". Cliquez sur « Télécharger le PDF »."))
        n
      }, error = function(e) {
        ancienne <- isolate(note_courante())
        complement <- if (!is.null(ancienne) && file.exists(ancienne$pdf))
          " La note précédente reste téléchargeable." else ""
        note_message(paste0("Note non générée : ", conditionMessage(e), complement))
        NULL
      })
    })
  }

  output$note_telechargement <- renderUI({
    n <- note_courante()
    if (is.null(n) || is.null(n$pdf) || !file.exists(n$pdf)) return(NULL)
    downloadButton("telecharger_note_pdf", "Télécharger le PDF", class="btn-success")
  })
  output$telecharger_note_pdf <- downloadHandler(
    filename=function() paste0("COMEXT_note_",format(Sys.Date(),"%Y%m%d"),".pdf"),
    content=function(file) {
      n <- note_courante(); req(n, file.exists(n$pdf))
      if (!file.copy(n$pdf, file, overwrite=TRUE)) stop("Copie PDF impossible.")
    }, contentType="application/pdf")

  # ----------------------------------------------------------
  # 5.8 NOUVELLE CONVERSATION
  # ----------------------------------------------------------
  
  observeEvent(input$reset_ia, {
    
    conversation_ia(NULL)
    
    reponse_ia(NULL)
    old <- note_courante()
    if (!is.null(old) && !is.null(old$pdf) && file.exists(old$pdf)) unlink(old$pdf)
    note_courante(NULL)
    note_message("Aucune note PDF demandée.")
  })
  
  # ----------------------------------------------------------
  # 5.9 RECEPTION DE LA DICTEE DEPUIS LE NAVIGATEUR
  # ----------------------------------------------------------
  
  observeEvent(input$dictee_texte, {
    texte <- trimws(input$dictee_texte)
    if (nzchar(texte)) {
      updateTextAreaInput(session, "question_ia", value = texte)
    }
  })
  
  # Exemples de prompts : insérer le texte dans la zone de saisie.
  # Aucun appel à l'IA ni génération PDF n'est déclenché par ces boutons.
  prompts_predefinis <- c(
    prompt_cles = paste(
      "Pour le Luxembourg : identifier les marchés clés par segment de produits entre 2020 et 2025.",
      "Utilise exclusivement les données COMEXT de l'application pour les chiffres."
    ),
    prompt_ukraine = paste(
      "Je suis journaliste à RTL. J'aimerais savoir comment les échanges commerciaux",
      "du Luxembourg avec l'Ukraine ont évolué depuis 2022.",
      "Présente les importations, les exportations et le solde sur les années disponibles."
    ),
    prompt_investisseur = paste(
      "Je suis investisseur luxembourgeois et souhaite décrypter les moteurs du commerce extérieur",
      "luxembourgeois. Rédige une note PDF d'analyse économique sur ce sujet,",
      "avec graphiques et principaux partenaires et produits."
    ),
    prompt_strategique = paste(
      "Rédige une note PDF révélant les flux commerciaux à forte valeur stratégique",
      "pour le Luxembourg sur la période 2022-2025.",
      "Présente les principaux partenaires et produits et distingue les constats COMEXT",
      "des hypothèses économiques."
    )
  )
  lapply(names(prompts_predefinis), function(id) {
    observeEvent(input[[id]], {
      updateTextAreaInput(session, "question_ia", value = unname(prompts_predefinis[[id]]))
    })
  })

  # ----------------------------------------------------------
  # 5.9 POSER UNE QUESTION
  # ----------------------------------------------------------
  
  observeEvent(input$envoyer_ia, {
    question <- trimws(input$question_ia)
    req(nzchar(question))

    # Une demande de note génère le PDF immédiatement et sans utiliser les filtres.
    if (demande_note(question)) {
      preparer_note(question)
      # Le statut de la note et le lien sont affichés dans le bloc dédié.
      # Ne pas dupliquer le contenu ni le message dans le panneau de réponse.
      reponse_ia(NULL)
      return(invisible(NULL))
    }
    if (!ia_pret) {
      reponse_ia("OPENAI_API_KEY absente : assistant IA indisponible.")
      return(invisible(NULL))
    }
    reponse_ia("Analyse en cours...")
    shiny::withProgress(message = "Réponse de l'assistant IA", value = 0, {
      tryCatch({
        shiny::setProgress(value = 0.15, detail = "Préparation de la conversation…")
        chat <- conversation_ia()
        if (is.null(chat)) {
          chat <- creer_chat()
          conversation_ia(chat)
        }
        shiny::setProgress(value = 0.35, detail = "Interrogation de l'IA et, si nécessaire, des données DuckDB…")
        rep <- chat$chat(question)
        shiny::setProgress(value = 1, detail = "Réponse reçue")
        reponse_ia(as.character(rep))
      }, error = function(e) {
        reponse_ia(paste("Erreur de l'assistant :", conditionMessage(e)))
      })
    })
  })

  # ----------------------------------------------------------
  # 6. MESSAGE DE CONTROLE
  # ----------------------------------------------------------
  

  # ----------------------------------------------------------
  # CARTE DES ECHANGES — adaptation de Map.R
  # Agrégation DuckDB directement sur les Parquet.
  # ----------------------------------------------------------
  geometries_carte <- reactive({
    # Polygones mondiaux via Natural Earth, indépendants du lecteur S: de Map.R.
    geo <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    code <- if ("iso_a2_eh" %in% names(geo)) geo$iso_a2_eh else geo$iso_a2
    code <- as.character(code)
    autre <- as.character(geo$iso_a2)
    code[is.na(code) | code == "-99"] <- autre[is.na(code) | code == "-99"]
    code[code == "EL"] <- "GR"  # nomenclature Eurostat
    code[code == "UK"] <- "GB"
    code[code == "KO"] <- "XK"
    code[code == "RS"] <- "XS"
    geo$PARTNER <- code
    geo <- geo[!is.na(geo$PARTNER) & nchar(geo$PARTNER) == 2, ]
    geo <- geo[!duplicated(geo$PARTNER), ]
    sf::st_transform(geo, 4326)
  })

  output$map_year_ui <- renderUI({
    if (identical(input$map_type, "Annual")) {
      yrs <- fichiers_annuels()$annee
    } else {
      yrs <- unique(substr(fichiers()$periode, 1, 4))
    }
    yrs <- sort(unique(as.integer(yrs[!is.na(yrs)])))
    selectInput("map_year", "Année", choices = yrs,
                selected = if (length(yrs)) max(yrs) else NULL)
  })

  output$map_month_ui <- renderUI({
    if (!identical(input$map_type, "Monthly")) return(NULL)
    req(input$map_year)
    m <- substr(fichiers()$periode[
      substr(fichiers()$periode, 1, 4) == as.character(input$map_year)
    ], 5, 6)
    selectInput("map_month", "Mois", choices = sort(unique(m)),
                selected = if (length(m)) max(m) else NULL)
  })

  map_source <- reactive({
    req(input$map_type, input$map_year)
    if (identical(input$map_type, "Annual")) {
      d <- fichiers_annuels()
      path <- d$path[d$annee == as.integer(input$map_year)]
    } else {
      req(input$map_month)
      periode <- paste0(input$map_year, input$map_month)
      d <- fichiers()
      path <- d$path[d$periode == periode]
    }
    validate(need(length(path) == 1L, "Fichier COMEXT indisponible pour la période."))
    paste0("read_parquet(", qs(path), ")")
  })

  # Sélection détaillée NC8, sans TOTAL/XXX, et sans agrégats de partenaires.
  carte_stats <- reactive({
    req(input$map_flow)
    src <- map_source()
    pays <- geometries_carte()$PARTNER
    pays <- unique(pays[grepl("^[A-Z]{2}$", pays)])
    cond_pays <- paste(vapply(pays, qs, character(1)), collapse = ",")
    wh <- paste0("PARTNER IN (", cond_pays, ") AND FLOW IN ('1','2') ",
                 "AND regexp_matches(PRODUCT_NC, '^[0-9]{8}$')")
    # Une seule source, mensuelle OU annuelle : aucun double comptage.
    sql <- paste0(
      "SELECT PARTNER, FLOW, LEFT(PRODUCT_NC,2) AS SH2, ",
      "SUM(TRY_CAST(NULLIF(TRIM(CAST(VALUE_EUR AS VARCHAR)), '') AS DOUBLE)) AS montant ",
      "FROM ", src, " WHERE ", wh,
      " GROUP BY PARTNER, FLOW, LEFT(PRODUCT_NC,2)"
    )
    dat <- DBI::dbGetQuery(con, sql)
    validate(need(nrow(dat) > 0, "Aucun résultat cartographique pour cette période."))
    dat$FLOW <- as.character(dat$FLOW)
    dat$PARTNER <- as.character(dat$PARTNER)
    dat$SH2 <- as.character(dat$SH2)
    dat$montant <- as.numeric(dat$montant)
    dat <- dat[is.finite(dat$montant), , drop = FALSE]
    dat
  })

  carte_pays <- reactive({
    dat <- carte_stats()
    # Les flux 1/2 sont agrégés séparément; le solde est calculé après.
    agg <- stats::aggregate(montant ~ PARTNER + FLOW, dat, sum)
    imp <- agg[agg$FLOW == "1", c("PARTNER", "montant")]
    exp <- agg[agg$FLOW == "2", c("PARTNER", "montant")]
    names(imp)[2] <- "imp"
    names(exp)[2] <- "exp"
    d <- merge(imp, exp, by="PARTNER", all=TRUE)
    d$imp[is.na(d$imp)] <- 0
    d$exp[is.na(d$exp)] <- 0
    d$valeur <- switch(as.character(input$map_flow),
      "1"=d$imp, "2"=d$exp, "3"=d$exp-d$imp)

    # Top 3 SH2 du flux choisi ; pour le solde, présenter imports et exports.
    version <- if (as.integer(input$map_year) < 2022) "HS17" else "HS22"
    labels <- hs_labels[hs_labels$version == version & nchar(hs_labels$code) == 2,
                        c("code", "libelle"), drop=FALSE]
    noms_sh <- setNames(labels$libelle, labels$code)
    top_text <- function(p, fl) {
      x <- dat[dat$PARTNER == p & dat$FLOW == fl, , drop=FALSE]
      x <- x[order(-x$montant), , drop=FALSE]
      x <- head(x, 3L)
      if (!nrow(x)) return("Aucune donnée")
      lab <- unname(noms_sh[x$SH2]); lab[is.na(lab)] <- "Libellé indisponible"
      paste(sprintf("<b>%s</b> — %s : %s €",
        htmltools::htmlEscape(x$SH2), htmltools::htmlEscape(lab),
        format(round(x$montant), big.mark=" ", scientific=FALSE, trim=TRUE)), collapse="<br>")
    }
    d$top <- vapply(d$PARTNER, function(p) {
      if (input$map_flow == "1") return(paste0("<b>Importations</b><br>",top_text(p,"1")))
      if (input$map_flow == "2") return(paste0("<b>Exportations</b><br>",top_text(p,"2")))
      paste0("<b>Exportations</b><br>", top_text(p,"2"),
             "<br><br><b>Importations</b><br>", top_text(p,"1"))
    }, character(1))
    geo <- geometries_carte()
    idx <- match(geo$PARTNER, d$PARTNER)
    geo$valeur <- d$valeur[idx]
    geo$top <- d$top[idx]
    geo$nom_partenaire <- nom_pays(geo$PARTNER)
    geo
  })

  # Cartographie : echelles independantes pour deficits et excedents.
  # Chaque legende utilise SES propres minimum et maximum observes.
  output$map_comext <- leaflet::renderLeaflet({
    geo <- carte_pays()
    vv <- as.numeric(geo$valeur)
    valides <- vv[is.finite(vv)]
    est_solde <- identical(as.character(input$map_flow), "3")

    formater_millions <- function(x) {
      format(round(x / 1e6, 1), big.mark = " ",
             decimal.mark = ",", scientific = FALSE, trim = TRUE)
    }
    # Une legende calculee depuis EXACTEMENT la palette et la transformation
    # utilisees pour colorier les polygones correspondants.
    barre_legende <- function(titre, pal, vmin, vmax, note = NULL) {
      couleurs <- pal(seq(0, 1, length.out = 101L))
      stops <- paste0(couleurs, " ", seq(0, 100, length.out = 101L), "%")
      degrade <- paste0("linear-gradient(to right, ",
                        paste(stops, collapse = ", "), ")")
      htmltools::tags$div(
        style = "margin-bottom:9px;",
        htmltools::tags$div(style = "font-weight:600;margin-bottom:4px;", titre),
        htmltools::tags$div(style = paste0(
          "height:16px;border:1px solid #aaa;background:", degrade, ";")),
        htmltools::tags$div(
          style = "display:flex;justify-content:space-between;margin-top:4px;gap:12px;",
          htmltools::tags$span(paste0(formater_millions(vmin), " M€")),
          htmltools::tags$span(paste0(formater_millions(vmax), " M€"))
        ),
        if (!is.null(note)) htmltools::tags$div(
          style = "color:#666;font-size:10px;margin-top:3px;", note)
      )
    }
    if (est_solde) {
      neg <- valides[valides < 0]
      pos <- valides[valides > 0]
      geo$couleur <- rep("#f7f7f7", length(vv))  # solde nul
      geo$couleur[!is.finite(vv)] <- "#d9d9d9"
      blocs <- list()

      if (length(neg)) {
        # Signe negatif : min = plus gros deficit; max = deficit proche de zero.
        min_neg <- min(neg)
        max_neg <- max(neg)
        # Echelle asinh propre aux seuls deficits, convertie sur [0,1].
        e_neg <- stats::median(abs(neg))
        if (!is.finite(e_neg) || e_neg <= 0) e_neg <- 1
        calc_neg <- function(x) {
          bas <- asinh(abs(max_neg) / e_neg)
          haut <- asinh(abs(min_neg) / e_neg)
          if (haut == bas) return(rep(1, length(x)))
          pmin(1, pmax(0, (asinh(abs(x) / e_neg) - bas) / (haut - bas)))
        }
        # 0 = rouge clair, 1 = rouge fonce.
        pal_neg <- leaflet::colorNumeric(
          c("#fde0dd", "#fcae91", "#fb6a4a", "#cb181d", "#67000d"),
          domain = c(0, 1), na.color = "#d9d9d9")
        idx <- which(is.finite(vv) & vv < 0)
        geo$couleur[idx] <- pal_neg(calc_neg(vv[idx]))
        # La barre va du deficit le plus fort (fonce) vers le plus faible (clair).
        pal_neg_leg <- function(u) pal_neg(1 - u)
        blocs[[length(blocs) + 1L]] <- barre_legende(
          "Déficits (solde négatif)", pal_neg_leg, min_neg, max_neg)
      }
      if (length(pos)) {
        min_pos <- min(pos)
        max_pos <- max(pos)
        e_pos <- stats::median(pos)
        if (!is.finite(e_pos) || e_pos <= 0) e_pos <- 1
        calc_pos <- function(x) {
          bas <- asinh(min_pos / e_pos)
          haut <- asinh(max_pos / e_pos)
          if (haut == bas) return(rep(1, length(x)))
          pmin(1, pmax(0, (asinh(x / e_pos) - bas) / (haut - bas)))
        }
        pal_pos <- leaflet::colorNumeric(
          c("#e5f5e0", "#a1d99b", "#74c476", "#238b45", "#00441b"),
          domain = c(0, 1), na.color = "#d9d9d9")
        idx <- which(is.finite(vv) & vv > 0)
        geo$couleur[idx] <- pal_pos(calc_pos(vv[idx]))
        blocs[[length(blocs) + 1L]] <- barre_legende(
          "Excédents (solde positif)", pal_pos, min_pos, max_pos)
      }
      blocs[[length(blocs) + 1L]] <- htmltools::tags$div(
        style = "font-size:10px;color:#666;",
        "Deux échelles indépendantes (arcsinh), chacune avec ses bornes observées ; gris clair : solde nul.")
      legende_html <- htmltools::tags$div(
        style = paste0("background:white;padding:10px 12px;",
          "box-shadow:0 1px 7px #999;border-radius:5px;",
          "font:12px Arial,sans-serif;width:310px;"),
        htmltools::tags$div(
          style = "font-weight:bold;margin-bottom:8px;", "Solde commercial (M€)"),
        blocs
      )
    } else {
      # Importations/exportations : conserver l'echelle initiale de la V36.
      positifs <- valides[valides > 0]
      echelle <- if (length(positifs)) stats::median(positifs) else 1
      if (!is.finite(echelle) || echelle <= 0) echelle <- 1
      maximum <- if (length(valides)) max(valides) else 1
      if (!is.finite(maximum) || maximum <= 0) maximum <- 1
      limite <- asinh(maximum / echelle)
      pal <- leaflet::colorNumeric(
        if (as.character(input$map_flow) == "1")
          c("#f1f7fd", "#c6dbef", "#6baed6", "#2171b5", "#08306b")
        else c("#f1f8f2", "#c7e9c0", "#74c476", "#238b45", "#00441b"),
        domain = c(0, 1), na.color = "#d9d9d9")
      normaliser <- function(x) pmax(0, pmin(1, asinh(pmax(x, 0) / echelle) / limite))
      geo$couleur <- pal(normaliser(vv))
      geo$couleur[!is.finite(vv)] <- "#d9d9d9"
      legende_html <- htmltools::tags$div(
        style = paste0("background:white;padding:10px 12px;",
          "box-shadow:0 1px 7px #999;border-radius:5px;",
          "font:12px Arial,sans-serif;width:310px;"),
        barre_legende(if (as.character(input$map_flow) == "1")
                        "Importations" else "Exportations", pal, 0, maximum),
        htmltools::tags$div(style="color:#666;font-size:10px;",
          "Échelle non linéaire (arcsinh).")
      )
    }

    montants <- ifelse(is.na(vv), "Pas de données",
      paste0(format(round(vv), big.mark = " ", scientific = FALSE,
                    trim = TRUE), " €"))
    geo$popup_carte <- sprintf("<b>%s</b><br><br><b>Valeur :</b> %s<br><br>%s",
      htmltools::htmlEscape(geo$nom_partenaire), montants,
      ifelse(is.na(geo$top), "Aucune donnée", geo$top))
    leaflet::leaflet(geo, options=leaflet::leafletOptions(minZoom=2, preferCanvas=TRUE)) |>
      leaflet::addPolygons(fillColor=~couleur, fillOpacity=.82,
        color="#5f6975",weight=.6,
        popup=~popup_carte,
        highlightOptions=leaflet::highlightOptions(weight=2,color="#222",bringToFront=TRUE)) |>
      leaflet::addControl(html=htmltools::HTML(as.character(legende_html)),
                          position="bottomright") |>
      leaflet::addLegend(position="bottomleft",
        colors="#d9d9d9", labels="Pas de données",
        title="Couverture", opacity=.85) |>
      leaflet::fitBounds(-25, 30, 45, 72)
  })

  output$avertissement <- renderText({
    
    req(resultat())
    
    z <- sum(
      resultat()$controles$n_invalides,
      na.rm = TRUE
    )
    
    if (z > 0) {
      paste(
        "Attention :",
        z,
        "valeurs VALUE_EUR non convertibles."
      )
    } else {
      paste(
        "Contrôle : aucune valeur VALUE_EUR",
        "non convertible dans les observations retenues."
      )
    }
  })
}

# ============================================================
# 7. LANCEMENT
# ============================================================

shinyApp(ui, server)

