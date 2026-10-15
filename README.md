# README - Extraction et Modélisation des Concepts SDMX

## Introduction
Ce script en **R** permet d'extraire et de modéliser les concepts à partir d'un fichier XML contenant les **Dataflows**, **Concepts**, et **DataStructures** au format SDMX. Il génère deux fichiers CSV :
- Un fichier listant tous les concepts et leur association aux Dataflows.
- Un fichier contenant uniquement les concepts qui ont déjà été modélisés.

## Prérequis
Avant d'exécuter ce script, assurez-vous d'avoir installé **R** et les packages suivants :

### Installation des packages requis
```r
install.packages(c("xml2", "tidyverse"))
```

Ensuite, chargez les librairies dans votre session R :
```r
library(xml2)
library(tidyverse)
```

## Structure du Script
Le script suit ces étapes principales :

### 1. Chargement du fichier XML
Le script charge le fichier XML contenant les données SDMX. Assurez-vous d'avoir le fichier `All_Dataflow(2).xml` à l'emplacement suivant :
```
S:/Projets/ModelisationSDMX/_01 Projet PR2237/Script Concept registry/
```
Si le fichier se trouve ailleurs, modifiez la variable `xml_file` dans le script.

### 2. Extraction des Dataflows et Concepts
Le script extrait les **Dataflows** et les **Concepts** du fichier XML.

### 3. Construction du tableau des concepts
Chaque concept est identifié par :
- **ID du concept**
- **Nom du concept en anglais et en français**
- **ID de la codelist associée**
- **ID du schéma de concept**

### 4. Association des Concepts aux Dataflows
Une fonction `extract_concepts_from_dataflow` permet d'associer les concepts aux Dataflows correspondants.

### 5. Extraction des dimensions, attributs et mesures
Le script extrait les composantes des **DataStructures (DSDs)**, en classant chaque concept dans l'une des catégories suivantes :
- **Dimension**
- **Attribut**
- **Mesure**

### 6. Fusion des données et export des résultats
Deux fichiers CSV sont générés :
- **extraction_concepts_dataflows.csv** : Tous les concepts avec leur classification et leur association aux Dataflows.
- **extraction_concepts_modélisé_dataflows.csv** : Concepts déjà modélisés.

Les fichiers sont sauvegardés à l'emplacement suivant :
```
S:/Projets/ModelisationSDMX/_01 Projet PR2237/Script Concept registry/
```

## Exécution du Script
Pour exécuter le script, ouvrez **R** ou **RStudio** et lancez la commande suivante :
```r
source("chemin/vers/le/script.R")
```
Remplacez `chemin/vers/le/script.R` par le chemin correct vers le fichier.

## Problèmes Fréquents
### 1. Erreur "Fichier introuvable"
- Vérifiez que le fichier XML est bien au bon emplacement.
- Modifiez la variable `xml_file` si nécessaire.

### 2. Problème de packages
- Assurez-vous que `xml2` et `tidyverse` sont installés et chargés correctement.
- Essayez de réinstaller les packages :
```r
install.packages("xml2")
install.packages("tidyverse")
```

### 3. Fichier de sortie vide ou incomplet
- Vérifiez que le fichier XML contient bien les données attendues.
- Utilisez `View(final_data)` dans R pour voir les résultats avant l'export.

## Contribution
Si vous souhaitez améliorer ce script, clonez le dépôt, modifiez le code et soumettez une **pull request**.

Bonne extraction ! 🚀

