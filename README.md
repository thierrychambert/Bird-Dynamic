# Bird-Dynamic

Procédure à suivre pour faire l’analyse d’impact sur une espèce choisie :

Etape 1 : Projections démographiques pour le scénario de référence sans impact éolien
Dans le dossier « 00_Run Analyses », lancer le script « 01_run_model01_sc0.R ».
Pour sélectionner l’espèce à analyser, il suffit de changer la valeur « sel_esp » sur la ligne 10 de ce script. La numérotation des espèces est indiquée dans le tableau ci-dessous. (cette information est également directement visible dans R dans l’objet « BD_list$espece_BD ») 

Etape 2 : Analyse d’impact des collisions 
Dans le dossier « 00_Run Analyses », lancer le script « 02_simulations with collision impact.R ».
Pour sélectionner l’espèce à analyser, il suffit de changer la valeur « sel_esp » sur la ligne 10 de ce script. La numérotation des espèces est indiquée ci-dessous (info visible dans R dans l’objet « BD_list$espece_BD ») 
Veillez à fournir vos propres données d’estimation des mortalités par espèce, par les parcs éoliens concernés. Ces données doivent être fournies en format .csv dans le dossier « 000_Inputs\data mortality ». Il faut un fichier csv par espèce. 
Un fichier réduit des données de mortalités sur le Cormoran huppé (espèce #04) issues du projet Bird RISK et utilisés dans le projet Bird Dynamic sont fournies dans ce dossier à titre d’exemple. Vous pouvez vous baser sur ce fichier csv pour voir le format à respecter.

Numérotation des espèces :
1	Fou de Bassan
2	Goéland marin
3	Mouette tridactyle
4	Cormoran huppé
5	Goéland brun
6	Goéland argenté
7	Puffin des Baléares
8	Grand cormoran
9	Mouette mélanocéphale
10	Mouette rieuse
11	Sterne de Dougall
12	Goéland cendré
13	Sterne caugek
14	Sterne pierregarin
15	Pingouin torda
16	Guillemot de Troïl
17	Guifette noire
18	Océanite tempête
19	Goéland leucophée
20	Fulmar boréal
21	Puffin des Anglais
22	Macareux moine

