# COMEXT Luxembourg — Tableau de bord des échanges commerciaux

Application **R Shiny** permettant d'explorer les échanges commerciaux du Luxembourg à partir de fichiers **Eurostat COMEXT** au format Parquet. Elle combine visualisations interactives, tableaux, analyse par produit et pays partenaire, cartographie et assistant IA.

> Documentation établie à partir du fichier `app.R` fourni (version indiquée dans le code : V25).

## Fonctionnalités

- **Évolution des échanges** : niveaux et variations mensuels et annuels des importations, exportations et du solde commercial.
- **Tableaux** : consultation des résultats mensuels et annuels.
- **Structure des échanges** : ventilation par produit et par partenaire, ainsi qu'évolution de la structure annuelle.
- **Carte des échanges** : représentation géographique des échanges avec les partenaires commerciaux.
- **Références** : consultation des nomenclatures de produits et de pays.
- **Assistant IA** : interrogation des données en langage naturel, avec intégration d'OpenAI via `ellmer`.
- **Notes PDF** : génération de documents avec Quarto et Typst (nécessite une installation serveur).

Les valeurs monétaires sont notamment présentées en **millions d'euros** dans les graphiques et tableaux d'analyse.

## Prérequis

- R et un environnement capable d'exécuter une application Shiny.
- Les packages R : `shiny`, `DBI`, `duckdb`, `ggplot2`, `plotly`, `DT`, `ellmer`, `commonmark`, `leaflet`, `sf`, `dplyr`, `rnaturalearth`.
- Des fichiers COMEXT Parquet et les deux fichiers de nomenclature décrits ci-dessous.
- **Optionnel** : une clé API OpenAI pour les fonctionnalités d'IA.
- **Pour les PDF** : Quarto CLI avec prise en charge de Typst, installé et accessible sur le serveur.

Installation des packages R :

```r
install.packages(c(
  "shiny", "DBI", "duckdb", "ggplot2", "plotly", "DT",
  "ellmer", "commonmark", "leaflet", "sf", "dplyr", "rnaturalearth"
))
```

> Selon le système d'exploitation, `sf` peut nécessiter des bibliothèques géospatiales système. L'installation des packages ne remplace pas celle de Quarto CLI.

## Organisation des fichiers

Le script utilise `getwd()` comme dossier de base. Lancez donc l'application depuis le dossier qui contient les données et les fichiers de référence.

```text
projet/
├── app.R
├── comext_hs_labels.csv
├── comext_country_labels.csv
├── LU_202401.parquet
├── LU_202402.parquet
├── ...
└── ANNUEL/
    ├── LU_ANNUEL_2023.parquet
    ├── LU_ANNUEL_2024.parquet
    └── ...
```

### Données attendues

| Fichier | Rôle |
| --- | --- |
| `LU_YYYYMM.parquet` | Données mensuelles, avec un mois valide de `01` à `12` |
| `ANNUEL/LU_ANNUEL_YYYY.parquet` | Données annuelles, utilisées notamment pour les analyses de structure |
| `comext_hs_labels.csv` | Libellés des produits selon la version et le code de nomenclature SH |
| `comext_country_labels.csv` | Correspondance entre codes pays et noms des pays |

Le script vérifie notamment la présence des colonnes mensuelles `PARTNER`, `FLOW`, `PERIOD`, `VALUE_EUR` et `PRODUCT_NC`. Les fichiers CSV sont lus avec encodage `UTF-8-BOM` et leurs colonnes sont traitées comme du texte. Les noms de colonnes utilisés par le code incluent `code`, `nom` pour les pays, et `version`, `code`, `libelle` pour les produits.

Les données COMEXT **ne sont pas incluses** dans le seul fichier `app.R` : elles doivent être fournies séparément.

## Démarrage

1. Placez `app.R`, les fichiers CSV et les Parquet dans l'arborescence indiquée.
2. Ouvrez R ou RStudio et définissez le répertoire de travail sur le dossier du projet.
3. Exécutez :

```r
shiny::runApp(".")
```

L'application ouvre une interface Shiny dans laquelle vous pouvez sélectionner les périodes, flux, produits et partenaires, puis naviguer entre les onglets.

## Configuration de l'assistant IA

L'application lit la clé API depuis la variable d'environnement `OPENAI_API_KEY`. **Ne mettez jamais cette clé directement dans `app.R` ni dans un dépôt Git.**

Exemple pour la session R courante :

```r
Sys.setenv(OPENAI_API_KEY = "votre-cle-api")
```

Pour un déploiement, configurez cette variable dans les secrets ou variables d'environnement de la plateforme d'hébergement. Sans clé, l'assistant IA est indisponible ; les fonctionnalités d'analyse classiques peuvent être utilisées indépendamment.

## Export PDF

La génération de notes PDF utilise **Quarto CLI** et **Typst**. Vérifiez que la commande suivante fonctionne dans l'environnement d'exécution :

```bash
quarto --version
```

Le code recherche l'exécutable `quarto` dans le `PATH`, puis tente éventuellement `quarto::quarto_path()`. Installer uniquement le package R `quarto` ne garantit pas la disponibilité du moteur de rendu. La génération est réalisée côté serveur.

## Architecture technique

- **Shiny** : interface et logique réactive.
- **DuckDB / DBI** : lecture et interrogation SQL des fichiers Parquet, avec une connexion en mémoire par session utilisateur.
- **Plotly / ggplot2** : visualisations statistiques.
- **DT** : tableaux interactifs.
- **Leaflet / sf / rnaturalearth** : carte géographique.
- **ellmer / commonmark** : intégration de l'assistant IA et affichage des réponses.
- **Quarto / Typst** : production des notes PDF.

La connexion DuckDB est fermée à la fin de chaque session Shiny.

## Points d'attention

- **Répertoire de travail** : les chemins sont construits à partir de `getwd()`. Un démarrage depuis un autre dossier peut empêcher le chargement des fichiers.
- **Nomenclatures obligatoires** : l'application s'arrête au lancement si l'un des deux CSV est absent.
- **Noms de fichiers stricts** : respectez les formats `LU_YYYYMM.parquet` et `LU_ANNUEL_YYYY.parquet` ; les périodes mensuelles dupliquées sont rejetées.
- **Qualité des données** : l'interface signale les valeurs `VALUE_EUR` non convertibles pour les observations retenues.
- **Comparabilité** : le code attire l'attention sur la comparaison des nomenclatures SH entre 2021 et 2022 et sur l'interprétation des variations d'un solde commercial négatif ou proche de zéro.
- **Déploiement** : les données, les dépendances géospatiales, les variables d'environnement et Quarto doivent être disponibles dans l'environnement serveur.

## Source des données

**Eurostat COMEXT** — statistiques du commerce international de biens. Ce projet exploite des extractions locales au format Parquet ; la méthode de téléchargement ou de mise à jour de ces fichiers n'est pas définie dans `app.R`.

## Licence

Aucune licence n'est précisée dans le fichier source fourni. Ajoutez un fichier `LICENSE` si vous souhaitez définir des conditions de réutilisation.
