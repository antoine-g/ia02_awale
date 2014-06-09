element(X,[X|_]).
element(X,[_|Q]):-element(X,Q).
 
% valeur de la case d'indice i (1=<i>=12)
val_case([T|_], 1, Res) :- !, Res = T.
val_case([_|Y], I, Res) :- Z is I-1, val_case(Y, Z, Res).
 
% incrementer la case d'indice i
incr_case([T|Q], 1, [R|Q]) :- !, R is T + 1.
incr_case([X|Y], I, [X|P]) :- Z is I-1, incr_case(Y, Z, P).
 
%ou remplacer le n ème élément de L par X, on obtient NL remplacer(N,L,X,NL)
%version OK
remplacer(1,[_|Q],X,[X|Q]).
remplacer(N,[T|Q],X,[T|Qr]):- M is N-1, remplacer(M,Q,X,Qr).
 
% distribution des graines d'une case pour obtenir le nouvel état du plateau
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
 
% suppression de toutes les graines de la case i
vider_case([_|Q],1, [R|Q]):- !, R = 0.
vider_case([X|Y], I, [X|P]) :- Z is I-1, vider_case(Y, Z, P).
 
% ajoutery à la variable x
ajouter(X,Y,Res):- Res is X + Y.
 
% concaténation de 2 listes
concat([],L,L).
concat([X|L1],L2,[X|L3]):- concat(L1,L2,L3).
 
% passage de la reprensation une liste (1 à 12) vers 2 sous-listes
passage_sous_listes([E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11,E12],[[E1,E2,E3,E4,E5,E6],[E7,E8,E9,E10,E11,E12]]).
 
%echanger sous-listes
echanger_sous_listes([L1,L2],[L2,L1]).
 
%retourner une liste
retourner_liste([],[]).
retourner_liste([T|Q],L):- retourner_liste(Q,L1), concat(L1,[T],L).
       
% passage de 2 sous-listes en une liste
passage_une_liste([[E1,E2,E3,E4,E5,E6],[E7,E8,E9,E10,E11,E12]],[E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11,E12]).
 
%Séparation en 2 sous-listes ordonnées pour le joueur 2
sous_listes_plat_j2(L, Res):- 
	passage_sous_listes(L,[L1,L2]),
	retourner_liste(L1,Tmp),
	Res=[Tmp,L2].
 
%Séparation en 2 sous-listes ordonnées pour le joueur 1
sous_listes_plat_j1(L, Res):- 
	passage_sous_listes(L,[L1,L2]),
	retourner_liste(L2,Tmp),
	echanger_sous_listes([L1,Tmp],Res).
                               
% affichage d'une jolie liste
afficher_jolie_liste([]) :- nl.
afficher_jolie_liste([T|Q]) :-
    write('('),
    write(T),
    write(')'),
    afficher_jolie_liste(Q).
 
afficher_plateau(L,Joueur):-
    Joueur = joueur1,
    sous_listes_plat_j1(L,[L1,L2]),
    write('Joueur2: '),
    afficher_jolie_liste(L1),
    write('Joueur1: '),
    afficher_jolie_liste(L2),!.
 
afficher_plateau(L,Joueur):-
    Joueur = joueur2,
    sous_listes_plat_j2(L,[L1,L2]),
    write('Joueur1: '),
    afficher_jolie_liste(L1),
    write('Joueur2: '),
    afficher_jolie_liste(L2),!.
                           
% deplacer les billes d'une case i en jouant
deplacer_case(joueur1,_,Case,_,_):-
    Case > 6,
    write('Impossible de jouer cette case\n'),
    !,fail.
   
deplacer_case(joueur2,_,Case,_,_):-
    Case < 7,
    write('Impossible de jouer cette case\n'),
    !,fail.
    
deplacer_case(_, Plat, Case, Res_plat, Case_finale):-
    val_case(Plat, Case, Val),
    distribuer_graines2(Case,Case,Val,Plat,Tmp_plat, Case_finale),
    vider_case(Tmp_plat, Case, Res_plat).
 
%Vérifier si on reste sur son champ
harmoniser(Case, New_case):- Case>6,
    New_case is Case-6,!.
   
harmoniser(Case, Case).
   
% récupérer les billes associées
recuperer_billes(J,Case_courante, Plat, Plat, Score, Score):-
	J=joueur1,
	Case_courante<7.

recuperer_billes(J,Case_courante, Plat, Plat, Score, Score):-
	J=joueur2,
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
   
% etat suivant
init:- P = [4,4,4,4,4,4,4,4,4,4,4,4],
    retractall(plat_courant(_)),
    retractall(score_courant1(_)),
    retractall(score_courant2(_)),
    retractall(prochain_joueur(_)),
	retractall(quitter(_)),
	retractall(nb_coups_total(_)),
	retractall(nb_coups_sans_gain(_)),
    asserta(plat_courant(P)),
    asserta(score_courant1(0)),
    asserta(score_courant2(0)),
    asserta(prochain_joueur(joueur1)),
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
 
init2:- P = [4,4,4,4,4,4,0,0,0,0,0,1],
    retractall(plat_courant(_)),
    retractall(score_courant1(_)),
    retractall(score_courant2(_)),
    retractall(prochain_joueur(_)),
    asserta(plat_courant(P)),
    asserta(score_courant1(0)),
    asserta(score_courant2(0)),
    asserta(prochain_joueur(joueur1)),
    afficher_plateau(P, joueur1).

% Réponse à la question de init
traitement_reponse(1):- partie_humain_humain.
traitement_reponse(2):- partie_humain_ia.
traitement_reponse(3):- partie_ia_ia.
 
% vérifie que les cases de la liste comprises entre l'indice i et j ne contiennent que des 0
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

% vérifie si le joueur peut introduire au moins une bille dans le champ adverse
test_intro_impossible(Case, Plat,J):-
    J=joueur1,
    Case>=1,
    Case=<6,
    deplacer_case(J,Plat,Case,Plat_tmp,_),!,
    que_des_zeros(Plat_tmp, 7,12),
    Case1 is Case+1,
    test_intro_impossible(Case1, Plat,J).

test_intro_impossible(Case, _,J):-
    J=joueur1,
    Case>6,
    write('Plus de coups possibles: fin du jeu'),
    afficher_gagnant,!.
    
test_intro_impossible(Case, Plat,J):-
    J=joueur2,
    Case>=7,
    Case=<12,
    deplacer_case(J,Plat,Case,Plat_tmp,_),!,
    que_des_zeros(Plat_tmp, 1,6),
    Case1 is Case+1,
    test_intro_impossible(Case1, Plat,J).
    
test_intro_impossible(Case, _,J):-
    J=joueur2,
    Case>12,
    write('Plus de coups possibles: fin du jeu'),
    afficher_gagnant,!.
    
% Vérifie que le champ de l'adversaire ne contient pas que des zéros et dans le cas contraire empêche un mouvement qui n'introduit pas de graine
champ_adverse_vide_et_pas_insertion(Plat, J):-
    J = joueur1,
    que_des_zeros(Plat, 7, 12),
    \+test_intro_impossible(1, Plat,J),
    asserta(erreur('Vous devez introduire au moins une graine dans le champ adverse!')),!,fail.

champ_adverse_vide_et_pas_insertion(Plat, J):-
    J = joueur2,
    que_des_zeros(Plat, 1, 6),
    \+test_intro_impossible(7, Plat,J),
    asserta(erreur('Vous devez introduire au moins une graine dans le champ adverse!')),!,fail.

champ_adverse_vide(Plat, J):-
    J = joueur1,
    que_des_zeros(Plat, 7, 12),
	asserta(erreur('Vous devez introduire au moins une graine dans le champ adverse!')).

champ_adverse_vide(Plat, J):-
    J = joueur2,
    que_des_zeros(Plat, 1, 6),
	asserta(erreur('Vous devez introduire au moins une graine dans le champ adverse!')).
   
passage_nouvel_etat(Case):-
	prochain_joueur(J),
	J = joueur1,
	etat_suivant(Case,Score_res2,Plat_res2),
	maj_compteurs(Score_res2,J),
	retractall(plat_courant(_)),
	retractall(score_courant1(_)),
	retractall(prochain_joueur(_)),
	asserta(plat_courant(Plat_res2)),
	asserta(score_courant1(Score_res2)),
	asserta(prochain_joueur(joueur2)),
	afficher_plateau(Plat_res2,J),
	write('Joueur suivant=joueur2\n\n'),
	afficher_plateau(Plat_res2,joueur2),!.
    
passage_nouvel_etat(Case):-
	prochain_joueur(J),
	J = joueur2,
	etat_suivant(Case,Score_res2,Plat_res2),
	maj_compteurs(Score_res2,J),
	retractall(plat_courant(_)),
	retractall(score_courant2(_)),
	retractall(prochain_joueur(_)),
	asserta(plat_courant(Plat_res2)),
	asserta(score_courant2(Score_res2)),
	asserta(prochain_joueur(joueur1)),
	afficher_plateau(Plat_res2,J),
	write('Joueur suivant=joueur1\n'),
	afficher_plateau(Plat_res2,joueur1),!.
 
% créé l'état suivant du plateau et de la partie pour le jeu de la case Case
etat_suivant(Case,Score,Plat):-
	plat_courant(Plat),
	prochain_joueur(J),
	J = joueur1,
	val_case(Plat,Case,Val),
	Val = 0,
	asserta(erreur('La case choisie est vide!')),!,fail.

etat_suivant(Case,Score,Plat):-
	plat_courant(Plat),
	prochain_joueur(J),
	J = joueur2,
	Case1 is Case+6,
	val_case(Plat,Case1,Val),
	Val = 0,
	asserta(erreur('La case choisie est vide!')),!,fail.
	
etat_suivant(Case,Score_res,Plat_res):-
	prochain_joueur(J),
	J = joueur1,
	score_courant1(Score),
	plat_courant(Plat),
	deplacer_case(J,Plat,Case,Plat_tmp,Case_finale),
	\+champ_adverse_vide_et_pas_insertion(Plat_tmp, J),
	recuperer_billes(J,Case_finale,Plat_tmp, Plat_res,Score,Score_res),
	\+champ_adverse_vide(Plat_res, J),!.
 
etat_suivant(Case,Score_res,Plat_res):-
	prochain_joueur(J),
	J = joueur2,
	Case_tmp is Case+6,
	score_courant2(Score),
	plat_courant(Plat),
	deplacer_case(J,Plat,Case_tmp,Plat_tmp,Case_finale),
	\+champ_adverse_vide_et_pas_insertion(Plat_tmp, J),
	recuperer_billes(J,Case_finale,Plat_tmp, Plat_res,Score,Score_res),
	\+champ_adverse_vide(Plat_res, J),!.
 
% détermine l'indice de la dernière case non nulle dans le champ du joueur courant
derniere_case_non_nulle_champ_courant(Case):-
	prochain_joueur(Joueur),Joueur=joueur1,plat_courant(Plat),passage_sous_listes(Plat,[L|_]),derniere_case_non_nulle(L,Case).

derniere_case_non_nulle_champ_courant(Case):-
	prochain_joueur(Joueur),Joueur=joueur2,plat_courant(Plat),passage_sous_listes(Plat,[_|[L]]),derniere_case_non_nulle(L,Case).

% détermine l'indice du premier élement non nul de la liste, 1er élement à l'indice 1
premiere_case_non_nulle([0|Q],Res):- premiere_case_non_nulle(Q,Res1), Res is Res1+1,!.
premiere_case_non_nulle([_|_],1):-!.
premiere_case_non_nulle(_,0):-!.

% détermine l'indice du dernier élément non nul dans un liste de 6 élements, (indices de 1 à 6)
derniere_case_non_nulle(L,Case):-
	retourner_liste(L,LI),
	premiere_case_non_nulle(LI,C),
	Case is 6-C+1.

% met à jour la valeur de la meilleure case à jouer si c'est nécessaire
% échoue volontairement à tous le coups sauf à la dernière case non nulle du joueur
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

maj_meilleure_case(Nv_score,Case,Nv_plat):-
	retractall(meilleur_score(_)),
	retractall(meilleure_case(_)),
	retractall(meilleur_plateau(_)),
	asserta(meilleur_score(Nv_score)),
	asserta(meilleure_case(Case)),
	asserta(meilleur_plateau(Nv_plat)).

% ce prédicat est appelé sur chaque case de 1 à 6 par generer_etats
generer_etats_interne(Case):-
	etat_suivant(Case,Score_res,Plat_res),
	maj_eventuelle_meilleure_case(Score_res,Case,Plat_res).
 
% génère tous les états possibles après un coup depuis l'état courant,
% initialise les variables meilleure_case et meilleur_score
generer_etats:-
	retractall(meilleur_score(_)),
	retractall(meilleure_case(_)),
	retractall(meilleur_plateau(_)),
	prochain_joueur(J),
	J = joueur2,
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
	prochain_joueur(J),
	J = joueur1,
	score_courant1(Score),
	plat_courant(Plat),
	asserta(meilleur_score(Score)),
	asserta(meilleure_case(0)),
	asserta(meilleur_plateau(Plat)),
	element(X,[1,2,3,4,5,6]),
	generer_etats_interne(X),!.
             
tour_de_jeu_ia:-
	prochain_joueur(J),
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
	retractall(prochain_joueur(_)),
	asserta(plat_courant(Plat)),
	asserta(score_courant1(Score)),
	asserta(prochain_joueur(joueur2)),
	afficher_scores,
	afficher_plateau(Plat,joueur2),!.
             
tour_de_jeu_ia:-
	prochain_joueur(J),
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
	retractall(prochain_joueur(_)),
	asserta(plat_courant(Plat)),
	asserta(score_courant2(Score)),
	asserta(prochain_joueur(joueur1)),
	afficher_scores,
	afficher_plateau(Plat,joueur1),!.
	
maj_compteurs(Nv_score,J):-
	J = joueur1,
	score_courant1(Score),
	Score = Nv_score,
	nb_coups_sans_gain(N),
	N1 is N+1,
	retractall(nb_coups_sans_gain(_)),
	asserta(nb_coups_sans_gain(N1)),
	incr_nb_coups_total,!.

maj_compteurs(Nv_score,J):-
	J = joueur2,
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

incr_nb_coups_total:-
	nb_coups_total(NT),
	NT1 is NT+1,
	retractall(nb_coups_total(_)),
	asserta(nb_coups_total(NT1)).
 
afficher_scores:-
	score_courant1(Score1),
	score_courant2(Score2),
	write('Score1:'),
	write(Score1),nl,
	write('Score2:'),
	write(Score2),nl.

partie_humain_ia:-
    \+tester_scores,
	\+tester_cycle,
    prochain_joueur(J),
    J=joueur1,
    tour_de_jeu_humain,
    partie_humain_ia,!.

partie_humain_ia:-
    \+tester_scores,
	\+tester_cycle,
    prochain_joueur(J),
    J=joueur2,
    tour_de_jeu_ia,
    partie_humain_ia,!.

partie_humain_humain:-
    \+tester_scores,
    tour_de_jeu_humain,
    partie_humain_humain,!.

partie_ia_ia:-
	\+tester_scores,
	\+tester_cycle,
    tour_de_jeu_ia,
    partie_ia_ia,!.
    
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
 
tester_scores:-
    score_courant1(S),
    S >= 25,
    afficher_gagnant.
 
tester_scores:-
    score_courant2(S),
    S >= 25,
    afficher_gagnant.

tester_cycle:-
	nb_coups_sans_gain(N),
	N > 12,
	write('\nLe jeu semble cycler la partie va s\'arreter, chaque joueur prend les graines de son champ.\n'),
	ramasser_graines_de_son_champ,
	afficher_gagnant.
	
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

somme_liste([],0).
somme_liste([T|Q],Res):- somme_liste(Q,Res1), Res is Res1 + T.

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

afficher_nb_coups_total:-
	nb_coups_total(N),
	write('\nNombres de coups joués:'),
	write(N).