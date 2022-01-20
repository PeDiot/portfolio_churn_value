# Méthodes d'estimation à partir de la bdd `Telco`

<br/> 

## Estimation de la durée de vie d'un client dans le portefeuille

<br/> 

Couple de variables cibles : `Churn value` et `Tenure Months`

Plusieurs types de variables explicatives : 

- démographiques (`Gender`, `Senior Citizen`, etc.)
- géographiques (`Country`, `City`, `Lat`, etc.)
- transaction (`Phone Service`, `Multiple Lines`, etc.)

Application des modèles de durée : 

- Kaplan-Meier
- Exponentielle 
- Cox
- Weibull
- Gamma 
- Gompertz
- Modèles à risques concurrents en utilisant la variable `Churn Reason`

Nb: [tutoriel](https://www.r-bloggers.com/2020/01/survival-analysis-fitting-weibull-models-for-improving-device-reliability-in-r/) pour implémenter modèle Weibull 


Parfois il faudra écrire la vraisemblance à la main et l'optimiser avec `maxLik` ou `optim` $\rightarrow$ différentes fonctions de risques selon la loi paramétrique 

<br/> 

## Estimation de la CLV d'un client 

<br/> 

Variable cible `CLTV` 

Variables explicatives démographiques, géographiques et de transaction 

Voir si on peut injecter la durée de vie prédite comme variable explicative de la CLV

2 objectifs :

- trouver le meilleur modèle de régression (arbre de régression, forêt aléatoire, réseau de neurones, *gradient boosting*) pour prédire la CLV
- estimer un modèle économétrique explicatif de la CLV 

$\rightarrow$ quelle serait la CLV estimée d'un nouveau client ?

<br/>

## Estimation de la valeur du portefeuille de clients 

<br/>

### Modélisation statique 

<br/> 

Utilisation de la modélisation de la valeur d'un portefeuille de brevets 

La valeur du portefeuille est :
$$P_{N} = \sum_{i=1}^N a_{i} v_{i}$$

Ici $a_{i}$ vaut 1 si le client $i$ est actif et $v_{i}$ représente la valeur du client 

Si on calcule la valeur du portefeuille au moment où l'étude est réalisée (fin du troisième trimestre) on peut utiliser la variable `CLTV` présente dans la bdd pour la variable $v$ 


On peut aussi chercher à **modéliser** la variance d'un portefeuille de clients : 

- variabilité sur la valeur 
- variabilité sur l'occurence 

<br/> 

### Modélisation dynamique 

<br/> 

Estimer la valeur du portefeuille pour chaque instant $t$, on a alors : 
$$P_{N, t} = \sum_{i=1}^N a_{i, t} v_{i, t}$$

Pour déterminer la valeur des $a_{i, t}$ il suffit d'*étendre* la base de données. 

- On considère que les clients sont tous entrés dans le portefeuille au même moment.
- Pour chaque instant $t$, on détermine si le client $i$ est actif en regardant si $T_i > t$ avec $T_i$ la durée de vie du client $i$ 

Pour déterminer les $v_{i, t}$, l'estimation est plus complexe pour deux raisons :
- La variable `CLTV` est fixe dans le temps 
- Les valeurs des variables explicatives qui pourrait nous aider à estimer la CLV sont aussi fixes dans le temps 

Un idée pourrait être de diviser la valeur prise par `CLTV` pour le client $i$ par le nombre de périodes durant lesquelles il a été actif pour obtenir une CLV par période ($v_it$) 

Le but final sera d'estimer  pour chaque instant $t$ :
- le rendement espéré du portefeuille $\mathrm{E} [P_{N,t}]$
- la variance associée à la valeur du portefeuille $\mathrm{E} [V_{N,t}]$


<br/>

On pourrait choisir une autre représentation de la valeur du portefeuille : 

$$P_N = \sum_{i=1}^N P(V)V$$

- Pour estimer $V$ on peut implémenter un modèle utilisant les variables explicatives dans la base de données : $V = f(X)$ 
- On peut ensuite injecter $\hat{V}$ dans un modèle de durée ou ML pour prédire la probabilité de churn : $P(V) = 0$ si churn
- On peut aussi trouver le couple $(V, P(V))$ pour lequel l'entreprise à la plus grande part de marché (faire une segmentation du portefeuille selon ) 

<br/>

Ou encore 

$$\max_{\theta} \sum_{i=1}^N P(\theta) V(\theta)$$

## Bilan financier de la valeur du portefeuille à partir des données 

Prédiction de la survie + Prédiction de la CLV $\rightarrow$ Estimation de la valeur totale du portefeuille

Pour un client :

$$V_i = \sum_{t=1}^T \frac{\hat{\text{v}}_i \times \hat{S}_{t,i}}{(1+a)^t}$$

avec : 

- $\hat{S}_{t,i}$ la probabilité de survie estimée du client $i$ à l'instant $t$, obtenue par modèles de durée 
- $\hat{\text{v}}_i$ la valeur estimée du client $i$, obtenue par modèles de régression entre `CLTV` et les variables explicatives de la base de données 

Nb: trouver une valeur de $a$ utilisée dans la littérature liée au marketing

Faire la somme pour l'ensemble des clients:

$$P_N = \sum_{i=1}^N V_i$$




