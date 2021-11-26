# OUV_Projet
> Ocaml Projet 

## PARTIE 1 
###  1.1 Polynôme sous forme linéaire
> Le monôme c · x^d est donné par le couple (c, d) ∈ Z × N. Un polynôme est une liste de monômes.
- [X] Q1.1 Structure de données permettant de manipuler des polynômes
- [X] Q1.2 Fonction **canonique** 
- [X] Q1.3 Fonction **poly_add**
- [X] Q1.4 Fonction **poly_prod**
### 1.2 Expression arborescente
> ![image](https://user-images.githubusercontent.com/76997880/143219579-51aedf47-b944-403a-92f5-a8d41a29a2d8.png)
- [X] Q1.5 Structure de données permettant de manipuler ces arbres.
- [X] Q1.6 Expression représentant l’arbre à gauche de la Figure 1.
- [X] Q1.7 Fonction **arb2poly** : arbre -> polynome canonique
### 1.3 Synthèse d’expressions arborescentes
- [X] Q1.8 Fonction **extraction_alea**
- [ ] Q1.9 Fonction **gen_permutation**
- [ ] Q1.10 Fonction **ABR**
- [ ] Q1.11 Fonction etiquetage
- [ ] Q1.12 Fonction gen_arb

## PARTIE 2
> Pour les trois questions suivantes, prendre successivement n = 100, . . . , 1000 avec un pas de 100, et représenter les temps de calculs sur les mêmes courbes (soit addition soit produit).

- [ ] Q2.13 Générer n ABR de taille 20, puis les transformer avec les fonctions etiquetage et **gen_arb**.
- [ ] Q2.14 On veut calculer le polynôme canonique issu de la somme des n arbres précédents.
  - Proposer et analyser plusieurs (au moins 3) stratégies et représenter le temps de calcul nécessaire sur un graphique addition.
- [ ] Q2.15 On veut calculer le polynôme canonique issu du produit des n arbres précédents.
  - Proposer et analyser plusieurs (au moins 3) stratégies et représenter le temps de calcul nécessaire sur un graphique produit.
- [ ] Q2.16 Générer 15 ABR de tailles respectives 1, 1, 2, 4, 8, . . . 2^13 puis les transformer avec les fonctions etiquetage et gen_arb.
- [ ] Q2.17 On veut calculer le polynôme canonique issu de la somme des 15 arbres précédents.
  - Proposer et analyser plusieurs (au moins 3) stratégies et calculer le temps de calcul nécessaire.
- [ ] Q2.18 On veut calculer le polynôme canonique issu du produit des 15 arbres précédents.
  - Proposer et analyser plusieurs (au moins 3) stratégies et représenter le temps de calcul nécessaire.
- [ ] Q2.19 (Facultatif) Répéter les questions 2.16, 2.17 et 2.18 plusieurs fois afin d’obtenir des temps moyens.
