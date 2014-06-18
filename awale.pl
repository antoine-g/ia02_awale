%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Projet IA02 Awalé													%
% Auteurs : Anaig Maréchal, Antoine Giraudmaillet					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pour lancer le jeu appelez le prédicat init/0						%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prédicats utilitaires généraux									%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Parcourt l'ensemble des élements de la liste passée en second argument
% element(Element, Liste)
element(X,[X|_]).
element(X,[_|Q]):-element(X,Q).
 
% Retourne la valeur de la case d'indice i (1=<i>=12)
% val_case(Liste, Indice, Valeur_i)
val_case([T|_], 1, Res) :- !, Res = T.
val_case([_|Y], I, Res) :- Z is I-1, val_case(Y, Z, Res).
 
% Incremente la case d'indice i de 1
% incr_case(Liste, Indice, Nv_liste)
incr_case([T|Q], 1, [R|Q]) :- !, R is T + 1.
incr_case([X|Y], I, [X|P]) :- Z is I-1, incr_case(Y, Z, P).
 
% Remplace le N ème élement de la liste par la valeur X
% remplacer(N,Liste,X,Nv_liste)
remplacer(1,[_|Q],X,[X|Q]).
remplacer(N,[T|Q],X,[T|Qr]):- M is N-1, remplacer(M,Q,X,Qr).

% Suppression de toutes les graines de la case d'indice i
% vider_case(Liste, Indice, Nv_liste)
vider_case([_|Q],1, [R|Q]):- !, R = 0.
vider_case([X|Y], I, [X|P]) :- Z is I-1, vider_case(Y, Z, P).
 
% Concaténation de 2 listes
% concat(Liste1, Liste2, Liste_concat)
concat([],L,L).
concat([X|L1],L2,[X|L3]):- concat(L1,L2,L3).

% Retourner une liste (inverser l'ordre de ses élements)
% Utile pour le retournement du plateau (à l'affichage lors du changement de joueur)
% retourner_liste(Liste, Liste_res)
retourner_liste([],[]).
retourner_liste([T|Q],L):- retourner_liste(Q,L1), concat(L1,[T],L).

% Echanger 2 sous-listes
echanger_sous_listes([L1,L2],[L2,L1]).

% vérifie que les cases de la liste comprises entre l'indice Deb et Fin ne contiennent que des 0
que_des_zeros([T|Q],Cour,Deb,Fin):-
    Cour >= Deb,
    Cour =< Fin,
    T=0,
    Cour1 is Cour+1,
    que_des_zeros(Q,Cour1,Deb,Fin),!.
 
que_des_zeros([T|_],Cour,_,Fin):-
    Cour>=Fin,
    T=0,
    true.
 
que_des_zeros([_|Q],Cour,Deb,Fin):-
    Cour < Deb,
    Cour1 is Cour+1,
    que_des_zeros(Q,Cour1,Deb,Fin).
 
que_des_zeros(L,Deb,Fin):- que_des_zeros(L,1,Deb,Fin).

% détermine l'indice du premier élement non nul de la liste, 1er élement à l'indice 1
premiere_case_non_nulle([0|Q],Res):- premiere_case_non_nulle(Q,Res1), Res is Res1+1,!.
premiere_case_non_nulle([_|_],1):-!.
premiere_case_non_nulle(_,0):-!.

% Somme toutes les valeurs contenues dans une liste
somme_liste([],0).
somme_liste([T|Q],Res):- somme_liste(Q,Res1), Res is Res1 + T.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prédicats spécifiques au jeu										%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Passage de la representation une liste (1 à 12) vers 2 sous-listes
% passage_sous_listes(Liste, [S_liste1,S_liste2])
passage_sous_listes([E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11,E12],[[E1,E2,E3,E4,E5,E6],[E7,E8,E9,E10,E11,E12]]).
       
% Passage de 2 sous-listes de 6 éléments en une liste de 12
% passage_une_liste([S_liste1,S_liste2], Liste)
passage_une_liste([[E1,E2,E3,E4,E5,E6],[E7,E8,E9,E10,E11,E12]],[E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11,E12]).

% Séparation en 2 sous-listes ordonnées pour le joueur 2
sous_listes_plat_j2(L, Res):- 
	passage_sous_listes(L,[L1,L2]),
	retourner_liste(L1,Tmp),
	Res=[Tmp,L2].

% Séparation en 2 sous-listes ordonnées pour le joueur 1
sous_listes_plat_j1(L, Res):- 
	passage_sous_listes(L,[L1,L2]),
	retourner_liste(L2,Tmp),
	echanger_sous_listes([L1,Tmp],Res).

% Affichage d'une liste ressemblant à des cases d'awalé (juste pour un côté du plateau)
afficher_jolie_liste([]) :- nl.
afficher_jolie_liste([T|Q]) :-
    write('('),
    write(T),
    write(')'),
    afficher_jolie_liste(Q).
 
% Affiche l'ensemble du plateau de jeux, orienté en fonction du joueur
afficher_plateau(Plat,joueur1):-
    sous_listes_plat_j1(Plat,[L1,L2]),
    write('Joueur2: '),
    afficher_jolie_liste(L1),
    write('Joueur1: '),
    afficher_jolie_liste(L2),!.
 
afficher_plateau(Plat,joueur2):-
    sous_listes_plat_j2(Plat,[L1,L2]),
    write('Joueur1: '),
    afficher_jolie_liste(L1),
    write('Joueur2: '),
    afficher_jolie_liste(L2),!.

% Permet de lancer le jeu en initialisant les données et affichant un menu
init:- P = [4,4,4,4,4,4,4,4,4,4,4,4],
    retractall(plat_courant(_)),
    retractall(score_courant1(_)),
    retractall(score_courant2(_)),
    retractall(joueur_courant(_)),
	retractall(quitter(_)),
	retractall(nb_coups_total(_)),
	retractall(nb_coups_sans_gain(_)),
    asserta(plat_courant(P)),
    asserta(score_courant1(0)),
    asserta(score_courant2(0)),
    asserta(joueur_courant(joueur1)),
	asserta(quitter(0)),
	asserta(nb_coups_total(0)),
	asserta(nb_coups_sans_gain(0)),
    afficher_plateau(P, joueur1),
    nl,
    write('1. Partie humain-humain\n'),
    write('2. Partie humain-IA\n'),
	write('3. Partie IA-IA\n'),
    write('Reponse:'),
    read(Rep),
    traitement_reponse(Rep).

% Réponse à la question de init, lance la partie correspondante
traitement_reponse(1):- partie_humain_humain.
traitement_reponse(2):- partie_humain_ia.
traitement_reponse(3):- partie_ia_ia.

% Distribue les graines de la case d'origine, en évitant la case interdite pour donner un nouveau plateau
% et renvoyer l'indice de la case finale
% Ce prédicat ne vide pas la case d'origine
% distribuer_graines2(Case_courante, Case_interdure, Nb_graines, Plat, Res_plat, Case_finale)
distribuer_graines2(Case_courante, _, 0, Plat, Res_plat, Case_finale) :-
    Res_plat = Plat,
    Case_finale is Case_courante - 1.
   
distribuer_graines2(13, Case_interdite, Nb_graines, Plat, Res_plat, Case_finale) :-
    distribuer_graines2(1, Case_interdite, Nb_graines, Plat, Res_plat, Case_finale).
   
distribuer_graines2(Case_courante, Case_interdite, Nb_graines, Plat, Res_plat, Case_finale) :-
    %Nb_graines > 0,
    Case_courante =< 12,
    Case_courante =\= Case_interdite,
    incr_case(Plat, Case_courante, Plat_temp),
    Case_temp is Case_courante + 1,
    Nb_graines_temp is Nb_graines - 1,
    distribuer_graines2(Case_temp, Case_interdite, Nb_graines_temp, Plat_temp, Res_plat, Case_finale),!.
 
distribuer_graines2(Case_courante, Case_interdite, Nb_graines, Plat, Res_plat, Case_finale) :-
    %Nb_graines > 0,
    %Case_courante = Case_interdite,
    Case1 is Case_courante + 1,
    distribuer_graines2(Case1, Case_interdite, Nb_graines, Plat, Res_plat, Case_finale).

% Deplacer les billes d'une case i en jouant : elle distribue les graines et vide la case d'origine
deplacer_case(joueur1,_,Case,_,_):-
    Case > 6,
    asserta(erreur('Impossible de jouer cette case\n')),
    !,fail.
   
deplacer_case(joueur2,_,Case,_,_):-
    Case < 7,
    asserta(erreur('Impossible de jouer cette case\n')),
    !,fail.
    
deplacer_case(_, Plat, Case, Res_plat, Case_finale):-
    val_case(Plat, Case, Val),
    distribuer_graines2(Case,Case,Val,Plat,Tmp_plat, Case_finale),
    vider_case(Tmp_plat, Case, Res_plat).
   
% Récupérer les billes éventuellement gagnées par le joueur suite à son déplacement
recuperer_billes(joueur1,Case_courante, Plat, Plat, Score, Score):-
	Case_courante<7.

recuperer_billes(joueur2,Case_courante, Plat, Plat, Score, Score):-
	Case_courante>=7.

recuperer_billes(J,Case_courante, Plat, Res_plat, Score, Res_score):-
    val_case(Plat, Case_courante, Val),
    Val =< 3,
    Val >= 2,
    Tmp_score is Score + Val,
    vider_case(Plat, Case_courante, Tmp_plat),
    Case_prec is Case_courante -1,
    recuperer_billes(J,Case_prec, Tmp_plat, Res_plat, Tmp_score, Res_score),!.
   
recuperer_billes(_,_, Plat, Res_plat, Score, Res_score):-
    Res_score = Score,
    Res_plat = Plat.

% Vérifie si le joueur peut introduire au moins une bille dans le champ adverse en jouant toutes les 
% cases de son plateau
% Echoue si une introduction est possible
test_intro_impossible(Case, Plat,joueur1):-
    Case>=1,
    Case=<6,
    deplacer_case(joueur1,Plat,Case,Plat_tmp,_),!,
    que_des_zeros(Plat_tmp, 7,12),
    Case1 is Case+1,
    test_intro_impossible(Case1, Plat,joueur1).

test_intro_impossible(Case, _,joueur1):-
    Case>6,
    write('Plus de coups possibles: fin du jeu'),
    afficher_gagnant,!.
    
test_intro_impossible(Case, Plat,joueur2):-
    Case>=7,
    Case=<12,
    deplacer_case(joueur2,Plat,Case,Plat_tmp,_),!,
    que_des_zeros(Plat_tmp, 1,6),
    Case1 is Case+1,
    test_intro_impossible(Case1, Plat,joueur2).
    
test_intro_impossible(Case, _,joueur2):-
    Case>12,
    write('Plus de coups possibles: fin du jeu'),
    afficher_gagnant,!.
    
% Vérifie que le champ de l'adversaire ne contient pas que des zéros et dans le cas contraire empêche un mouvement qui n'introduit pas de graine
champ_adverse_vide_et_pas_insertion(Plat, joueur1):-
    que_des_zeros(Plat, 7, 12),
    \+test_intro_impossible(1, Plat,joueur1),
    asserta(erreur('Vous devez introduire au moins une graine dans le champ adverse!')),!,fail.

champ_adverse_vide_et_pas_insertion(Plat, joueur2):-
    que_des_zeros(Plat, 1, 6),
    \+test_intro_impossible(7, Plat,joueur2),
    asserta(erreur('Vous devez introduire au moins une graine dans le champ adverse!')),!,fail.

champ_adverse_vide(Plat, joueur1):-
    que_des_zeros(Plat, 7, 12),
	asserta(erreur('Vous devez introduire au moins une graine dans le champ adverse!')).

champ_adverse_vide(Plat, joueur2):-
    que_des_zeros(Plat, 1, 6),
	asserta(erreur('Vous devez introduire au moins une graine dans le champ adverse!')).
 
% créé l'état suivant de la partie pour le jeu de Case,
% Peut être appelé avec un _ pour générer tous les états possibles (Utile pour l'IA)
% Véfifie que la case jouée n'est pas vide, que si le plateau adverse est vide le coup amène une insertion
% et que le champ adverse n'est pas vide une fois les graines ramassées
etat_suivant(Case,_,Plat):-
	plat_courant(Plat),
	joueur_courant(joueur1),
	val_case(Plat,Case,Val),
	Val = 0,
	asserta(erreur('La case choisie est vide!')),!,fail.

etat_suivant(Case,_,Plat):-
	plat_courant(Plat),
	joueur_courant(joueur2),
	Case1 is Case+6,
	val_case(Plat,Case1,Val),
	Val = 0,
	asserta(erreur('La case choisie est vide!')),!,fail.
	
etat_suivant(Case,Score_res,Plat_res):-
	joueur_courant(J),
	J = joueur1,
	score_courant1(Score),
	plat_courant(Plat),
	deplacer_case(J,Plat,Case,Plat_tmp,Case_finale),
	\+champ_adverse_vide_et_pas_insertion(Plat_tmp, J),
	recuperer_billes(J,Case_finale,Plat_tmp, Plat_res,Score,Score_res),
	\+champ_adverse_vide(Plat_res, J),!.
 
etat_suivant(Case,Score_res,Plat_res):-
	joueur_courant(J),
	J = joueur2,
	Case_tmp is Case+6,
	score_courant2(Score),
	plat_courant(Plat),
	deplacer_case(J,Plat,Case_tmp,Plat_tmp,Case_finale),
	\+champ_adverse_vide_et_pas_insertion(Plat_tmp, J),
	recuperer_billes(J,Case_finale,Plat_tmp, Plat_res,Score,Score_res),
	\+champ_adverse_vide(Plat_res, J),!.

% Détermine l'indice du dernier élément non nul dans un liste de 6 élements, (indices de 1 à 6)
derniere_case_non_nulle(L,Case):-
	retourner_liste(L,LI),
	premiere_case_non_nulle(LI,C),
	Case is 6-C+1.

% Détermine l'indice de la dernière case non nulle dans le champ du joueur courant (1 à 6)
derniere_case_non_nulle_champ_courant(Case):-
	joueur_courant(joueur1),
	plat_courant(Plat),
	passage_sous_listes(Plat,[L|_]),
	derniere_case_non_nulle(L,Case).

derniere_case_non_nulle_champ_courant(Case):-
	joueur_courant(joueur2),
	plat_courant(Plat),
	passage_sous_listes(Plat,[_|[L]]),
	derniere_case_non_nulle(L,Case).

% Met à jour la valeur de la meilleure case à jouer si c'est nécessaire
% échoue volontairement à tous les coups sauf à la dernière case non nulle du joueur courant
% pour assurer que toutes les posssibilités sont étudiées
% cette version est randomisée pour un choix aléatoire parmi les coups équivalents
maj_eventuelle_meilleure_case(Nv_score,Case,Nv_plat):-
	meilleur_score(Score),
	Nv_score > Score,
	maj_meilleure_case(Nv_score,Case,Nv_plat),!,
	derniere_case_non_nulle_champ_courant(Case).

maj_eventuelle_meilleure_case(Nv_score,Case,Nv_plat):-
	meilleur_score(Score),
	Nv_score = Score,
	meilleure_case(0),
	maj_meilleure_case(Nv_score,Case,Nv_plat),!,
	derniere_case_non_nulle_champ_courant(Case).

maj_eventuelle_meilleure_case(Nv_score,Case,Nv_plat):-
	meilleur_score(Score),
	Nv_score = Score,
	random(R),R > 0.75,
	maj_meilleure_case(Nv_score,Case,Nv_plat),!,
	derniere_case_non_nulle_champ_courant(Case).

maj_eventuelle_meilleure_case(_,Case,_):- derniere_case_non_nulle_champ_courant(Case).

% Remplace la meilleure case stockée, le plateau et le score associé
maj_meilleure_case(Nv_score,Case,Nv_plat):-
	retractall(meilleur_score(_)),
	retractall(meilleure_case(_)),
	retractall(meilleur_plateau(_)),
	asserta(meilleur_score(Nv_score)),
	asserta(meilleure_case(Case)),
	asserta(meilleur_plateau(Nv_plat)).

% Ce prédicat est appelé sur chaque case de 1 à 6 par generer_etats
% et permet d'étudier toutes les possibilités de jeu à partir d'un état courant
generer_etats_interne(Case):-
	etat_suivant(Case,Score_res,Plat_res),
	maj_eventuelle_meilleure_case(Score_res,Case,Plat_res).

generer_etats_interne(Case):-
	derniere_case_non_nulle_champ_courant(Case).
 
% Génère tous les états possibles après un coup depuis l'état courant,
% initialise les variables meilleure_case, meilleur_score et meilleur_plateau
generer_etats:-
	retractall(meilleur_score(_)),
	retractall(meilleure_case(_)),
	retractall(meilleur_plateau(_)),
	joueur_courant(joueur2),
	score_courant2(Score),
	plat_courant(Plat),
	asserta(meilleur_score(Score)),
	asserta(meilleure_case(0)),
	asserta(meilleur_plateau(Plat)),
	element(X,[1,2,3,4,5,6]),
	generer_etats_interne(X),!.
 
generer_etats:-
	retractall(meilleur_score(_)),
	retractall(meilleure_case(_)),
	retractall(meilleur_plateau(_)),
	joueur_courant(joueur1),
	score_courant1(Score),
	plat_courant(Plat),
	asserta(meilleur_score(Score)),
	asserta(meilleure_case(0)),
	asserta(meilleur_plateau(Plat)),
	element(X,[1,2,3,4,5,6]),
	generer_etats_interne(X),!.

% Appelle la génération de tous les états possibles en stockant le meilleur,
% Utilise le meilleur état stocké pour simuler un tour de jeu de l'IA
tour_de_jeu_ia:-
	joueur_courant(J),
	J = joueur1,
	generer_etats,
	meilleur_plateau(Plat),
	meilleure_case(Case),
	write('L\'IA1 joue la case '),
	write(Case),
	nl,
	afficher_plateau(Plat,J),
	meilleur_score(Score),
	maj_compteurs(Score,J),
	retractall(plat_courant(_)),
	retractall(score_courant1(_)),
	retractall(joueur_courant(_)),
	asserta(plat_courant(Plat)),
	asserta(score_courant1(Score)),
	asserta(joueur_courant(joueur2)),
	afficher_scores,
	afficher_plateau(Plat,joueur2),!.
             
tour_de_jeu_ia:-
	joueur_courant(J),
	J = joueur2,
	generer_etats,
	meilleur_plateau(Plat),
	meilleure_case(Case),
	write('L\'IA2 joue la case '),
	write(Case),
	nl,
	afficher_plateau(Plat,J),
	meilleur_score(Score),
	maj_compteurs(Score,J),
	retractall(plat_courant(_)),
	retractall(score_courant2(_)),
	retractall(joueur_courant(_)),
	asserta(plat_courant(Plat)),
	asserta(score_courant2(Score)),
	asserta(joueur_courant(joueur1)),
	afficher_scores,
	afficher_plateau(Plat,joueur1),!.

% A partir de la case à jouer passée par un joueur humain, ce prédicat 
% enregistre le nouvel état de la partie généré par etat_suivant (plateau, scores, compteurs)
passage_nouvel_etat(Case):-
	joueur_courant(J),
	J = joueur1,
	etat_suivant(Case,Score_res2,Plat_res2),
	maj_compteurs(Score_res2,J),
	retractall(plat_courant(_)),
	retractall(score_courant1(_)),
	retractall(joueur_courant(_)),
	asserta(plat_courant(Plat_res2)),
	asserta(score_courant1(Score_res2)),
	asserta(joueur_courant(joueur2)),
	afficher_plateau(Plat_res2,J),
	write('Joueur suivant=joueur2\n\n'),
	afficher_plateau(Plat_res2,joueur2),!.
    
passage_nouvel_etat(Case):-
	joueur_courant(J),
	J = joueur2,
	etat_suivant(Case,Score_res2,Plat_res2),
	maj_compteurs(Score_res2,J),
	retractall(plat_courant(_)),
	retractall(score_courant2(_)),
	retractall(joueur_courant(_)),
	asserta(plat_courant(Plat_res2)),
	asserta(score_courant2(Score_res2)),
	asserta(joueur_courant(joueur1)),
	afficher_plateau(Plat_res2,J),
	write('Joueur suivant=joueur1\n'),
	afficher_plateau(Plat_res2,joueur1),!.

% Partie effective du tour de jeu humain sans l'interraction avec le joueur
tour_de_jeu_humain_interne(c):-
	generer_etats,
	meilleure_case(Case),
	write('Case conseillee: '),
	write(Case),nl,!.
 
tour_de_jeu_humain_interne(Case):-
	Case =\= 0,
    nl,
    passage_nouvel_etat(Case),
    afficher_scores.

tour_de_jeu_humain_interne(0):-
    afficher_gagnant,
	retractall(quitter(_)),
	asserta(quitter(1)),!,fail.

% Interraction avec le joueur et mise à jour de l'état de la partie en fonction de la case jouée
tour_de_jeu_humain:-
    \+tester_scores,
	\+tester_cycle,
    write('Jouer case n° (1 à 6, 0 pour quitter, c pour conseil):'),
    read(Case),
    tour_de_jeu_humain_interne(Case),!.

tour_de_jeu_humain:-
    quitter(B),
	B = 1,
    !,fail.

tour_de_jeu_humain:-
    erreur(E),
    retractall(erreur(_)),
    write(E),nl.

% Boucle faisant alterner un tour de jeu humain et un tour de jeu IA
% tant que la partie ne doit pas se terminer
partie_humain_ia:-
    joueur_courant(joueur1),!,
    \+tester_scores,
	\+tester_cycle,
    tour_de_jeu_humain,
    partie_humain_ia.

partie_humain_ia:-
    joueur_courant(joueur2),!,
    \+tester_scores,
	\+tester_cycle,
    tour_de_jeu_ia,
    partie_humain_ia.

% Boucle faisant alterner 2 tours de jeu humains
% tant que la partie ne doit pas se terminer
partie_humain_humain:-
    \+tester_scores,
    tour_de_jeu_humain,
    partie_humain_humain,!.

% Boucle faisant alterner 2 tours de jeu IA
% tant que la partie ne doit pas se terminer
partie_ia_ia:-
	\+tester_scores,
	\+tester_cycle,
    tour_de_jeu_ia,
    partie_ia_ia,!.

% Teste si un joueur a gagné la partie en capturant au moins 25 graines,
% échoue dans le cas contraire
tester_scores:-
    score_courant1(S),
    S >= 25,
    afficher_gagnant.
 
tester_scores:-
    score_courant2(S),
    S >= 25,
    afficher_gagnant.

% Teste si un le nombre de points stagne durant plus de 12 coups
% ce qui est interprété comme un cycle
% échoue dans le cas contraire
tester_cycle:-
	nb_coups_sans_gain(N),
	N > 12,
	write('\nLe jeu semble cycler la partie va s\'arreter, chaque joueur prend les graines de son champ.\n'),
	ramasser_graines_de_son_champ,
	afficher_gagnant.

% Appelé en cas de cycle pour que chaque joueur récupère l'ensemble des graines de son champ
ramasser_graines_de_son_champ:-
	plat_courant(Plat),
	passage_sous_listes(Plat, [L1,L2]),
	somme_liste(L1,Res1),
	somme_liste(L2,Res2),
	score_courant1(Score1),
	score_courant2(Score2),
	Nv_score1 is Score1 + Res1,
	Nv_score2 is Score2 + Res2,
	retractall(score_courant1(_)),
	retractall(score_courant2(_)),
	asserta(score_courant1(Nv_score1)),
	asserta(score_courant2(Nv_score2)),
	retractall(plat_courant(_)),
	asserta(plat_courant([0,0,0,0,0,0,0,0,0,0,0,0])).

% Met à jour 2 compteurs : nb total de tours qui sert à afficher la valeur à la fin de la partie
% et nb de coups sans gain qui permet de détecter les cycles
maj_compteurs(Nv_score,joueur1):-
	score_courant1(Score),
	Score = Nv_score,
	nb_coups_sans_gain(N),
	N1 is N+1,
	retractall(nb_coups_sans_gain(_)),
	asserta(nb_coups_sans_gain(N1)),
	incr_nb_coups_total,!.

maj_compteurs(Nv_score,joueur2):-
	score_courant2(Score),
	Score = Nv_score,
	nb_coups_sans_gain(N),
	N1 is N+1,
	retractall(nb_coups_sans_gain(_)),
	asserta(nb_coups_sans_gain(N1)),
	incr_nb_coups_total,!.

maj_compteurs(_,_):-
	retractall(nb_coups_sans_gain(_)),
	asserta(nb_coups_sans_gain(0)),
	incr_nb_coups_total.

% Augmente le nombre total de coups joués de 1
incr_nb_coups_total:-
	nb_coups_total(NT),
	NT1 is NT+1,
	retractall(nb_coups_total(_)),
	asserta(nb_coups_total(NT1)).
 
% Affiche les scores des joueurs à l'écran
afficher_scores:-
	score_courant1(Score1),
	score_courant2(Score2),
	write('Score1:'),
	write(Score1),nl,
	write('Score2:'),
	write(Score2),nl.

% A la fin de la partie, affiche comme gagnant le joueur ayant obtenu le plus de points
afficher_gagnant:-
    score_courant1(Score1),
    score_courant2(Score2),
    Score1 > Score2,
	afficher_scores,
	afficher_nb_coups_total,
    write('\nLe joueur 1 a gagné !'),!.
 
afficher_gagnant:-
    score_courant1(Score1),
    score_courant2(Score2),
    Score1 < Score2,
	afficher_scores,
	afficher_nb_coups_total,
    write('\nLe joueur 2 a gagné !'),!.
 
afficher_gagnant:-
    score_courant1(Score1),
    score_courant2(Score2),
    Score1 = Score2,
	afficher_scores,
	afficher_nb_coups_total,
    write('\nEx-aequo !'),!.

% Affiche le nombre de coups joués durant la partie
afficher_nb_coups_total:-
	nb_coups_total(N),
	write('\nNombres de coups joués:'),
	write(N).