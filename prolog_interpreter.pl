% Oleksandra Tumak

% UWAGA: będę rozważać takie drzewo wywodu w którym każde dziecko jest pojedynczym
% atomem z treści klauzuli (odcięcia nie będziemy wstawiać do drzewa), czyli wierzchołek
% ma tyle dzieci ile atomów jest w treści dopasowanej klauzuli (bez !), a głębokość
% każdego dzicka jest o 1 większa, w tym jeśli rozważamy call(Body) to też to będzie
% wierzchołek, którego dziećmi są pojedyncze atomy z Body.  

% myTrace(+Goal)
% obsługuje polecenie:
% c (lub po prostu enter) - pełne śledzenie
% a - zakończenie śledzenia
% s - skip
% e - zakończenie śledzenia i pracy pod Prologiem

% rozpoznaje call, fail, exit, redo

% IMPLEMENTACJA myTrace(+Goal)

% myTrace(+ Goal) 
% zrobiłam usunięcie starych flag exit_flag na początku, bo pozostaje flaga po
% ostatnim "exit" i czekamy na reakcje użytkownika, jeśli on akceptuje
% odpowiedź, to nie wiem jak możemy usunąć flagę, bo program kończy się w tej chwili

% Ostatecznie dodałam jeszcze cleanGenFlag na początek, przy normalnym działaniu nie
% powinno być potrzebne, ale gdy (np dla tego że zapomniałam ustawić dynamic) program
% wywalił się z jakichś nieprzewidywanych powodów pozostają stare flagi.

myTrace(Goal) :-
                removeUserRedo,
                cleanExitFlag, 
                cleanGenFlag, 
                myTraceBody(Goal, false, 1, 1, []), 
                setUserRedo(true),
                cleanGenFlag.


% myTraceGoal(+ Goal, + Skip, + Depth, + Width, + Chain)
% Tutaj i wszędzie dalej:
% Skip mówi czy zlecono przelecieć tą część bez śledzenia
% Depth definiuje aktualne zagłebienie dowodu, czyli głębokość wierzchołka Goal w drzewie
% Width definiuje "numer" atomu (od lewej do prawej, odcięcia nie wliczamy) w treści
% klauzuli (czyli numer wierzchołka jako dziecka w rozważanym drzewie)
% Chain oznacza ścieżkę od korzenia drzewa wywodu do miejsca gdzie jesteśmy
% Trzy ostatnie parametry będziemy chcieli użyć do spamiętywania "Redo"

myTraceGoal(true, _, _, _, _) :- !.
myTraceGoal(Goal, true, Depth, Width, Chain) :- incGenFlag(_X),
                     (
                         myTraceDoGoal(Goal, true, Depth, Width, Chain),
                         if(
                             user_redo(true),
                             (
                                 writeExit(Goal, Depth, Width, [Goal|Chain]),
                                 read_input_fail_exit
                             ),
                             (setExitFlag(Depth, Width, [Goal|Chain]), removeGenFlag(_X))
                         )
                     );
                     if(
                         user_redo(true),
                         (writeFail(Goal), read_input_fail_exit, fail),
                         (removeGenFlag(_X), fail)
                     ).
myTraceGoal(Goal, false, Depth, Width, Chain) :- writeCall(Goal),
                     (
                         read_input(Goal, Depth, Width, Chain),
                         (
                             writeExit(Goal, Depth, Width, [Goal|Chain]),
                             read_input_fail_exit
                         )
                     );
                     (
                         writeFail(Goal),
                         read_input_fail_exit,
                         fail
                     ).

% myTraceDoGoal(+ Goal, + Skip, + Depth, + Width, + Chain)
myTraceDoGoal(call(Body), Skip, Depth, _, Chain) :- 
                                   !, 
                                   D2 is Depth+1, 
                                   myTraceBody(Body, Skip, D2, 1, Chain).
myTraceDoGoal(Goal, _, _, _, _) :- system(Goal), !, call(Goal).
myTraceDoGoal(Goal, Skip, Depth, Width, Chain) :- 
                             clause(Goal, Body),
                             if(
                                 try_gen_num(Chain), % sprawdzenie czy nie pora na Redo
                                 (
                                     D2 is Depth+1,
                                     myTraceBody(Body, 
                                                 Skip, 
                                                 D2, 
                                                 1, 
                                                 [Goal|Chain], 
                                                 NewWidth, 
                                                 AfterCut, 
                                                 HadCut),
                                     (
                                         HadCut = yes,
                                          !,
                                         myTraceBody(AfterCut, 
                                                     Skip, 
                                                     D2, 
                                                     NewWidth, 
                                                     [Goal|Chain])
                                      ;
                                         HadCut = no
                                     )
                                 ),
                                 (
                                   D2 is Depth+1,
                                   if(writeRedo(Skip, Depth, Width),
                                      if(
                                         Skip,
                                         if(
                                             user_redo(true),
                                             read_input_redo(
                                                              false, 
                                                              Body, 
                                                              D2, 
                                                              1, 
                                                              [Goal|Chain]
                                                            ),
                                             myTraceBody(
                                                          Body, 
                                                          true, 
                                                          D2, 
                                                          Width, 
                                                          [Goal|Chain]
                                                        )
                                         ),
                                         read_input_redo(Skip, Body, D2, 1, [Goal|Chain])
                                      ),
                                      fail
                                   )
                                 )
                             ).
    
try_gen_num(Chain) :- length(Chain, N), N2 is N+1, gen_num(N2), !.

% myTraceBody(+ Body, + Skip, + Depth, + Width, + Chain)
myTraceBody(Body, Skip, Depth, Width, Chain) :- 
               myTraceBody(Body, Skip, Depth, Width, Chain, NewWidth, AfterCut, HadCut),
               (
                     HadCut = yes,
                     !,
                     myTraceBody(AfterCut, Skip, Depth, NewWidth, Chain)
                ;
                     HadCut = no
               ).

% myTraceBody(+ Body, + Skip, + Depth, + Width, + Chain, -NewWidth, -AfterCut, -HadCut)
% NewWidth to ostatni numer dziecka po wykonaniu treści do odcięcia
myTraceBody((!, AfterCut), _, _, Width, _, Width, AfterCut, yes) :-  !.
myTraceBody(((Disj1; _Disj2), Body), Skip, Depth, Width, Chain, NewWidth, AfterCut, HadCut) :-
        myTraceBody(Disj1, Skip, Depth, Width, Chain, NewWidth0, After, HadCutDisj),
        (
           HadCutDisj = yes,
           !,
           HadCut = yes,
           NewWidth = NewWidth0,
           scal(After, Body, AfterCut)
         ;
           HadCutDisj = no,
           myTraceBody(Body, Skip, Depth, NewWidth0, Chain, NewWidth, AfterCut, HadCut)
        ).

myTraceBody(((_Disj1; Disj2), Body), Skip, Depth, Width, Chain, NewWidth, AfterCut, HadCut) :-  !,
        myTraceBody(Disj2, Skip, Depth, Width, Chain, NewWidth0, After, HadCutDisj),
        (
            HadCutDisj = yes,
            !,
            HadCut = yes,
            NewWidth = NewWidth0,
            scal(After, Body, AfterCut)
         ;
            HadCutDisj = no,
            myTraceBody(Body, Skip, Depth, NewWidth0, Chain, NewWidth, AfterCut, HadCut)
        ).
myTraceBody((Goal, Body), Skip, Depth, Width, Chain, NewWidth, AfterCut, HadCut) :-  !,
        		myTraceGoal(Goal, Skip, Depth, Width, Chain),
        		W2 is Width+1,
		        myTraceBody(Body, Skip, Depth, W2, Chain, NewWidth, AfterCut, HadCut).

myTraceBody((Disj1; _Disj2), Skip, Depth, Width, Chain, NewWidth, AfterCut, HadCut) :-
        		myTraceBody(Disj1, Skip, Depth, Width, Chain, NewWidth, AfterCut, HadCut).

myTraceBody((_Disj1; Disj2), Skip, Depth, Width, Chain, NewWidth, AfterCut, HadCut) :-  !,
        		myTraceBody(Disj2, Skip, Depth, Width, Chain, NewWidth, AfterCut, HadCut).

myTraceBody(!, _, _, Width, _, Width, true, yes) :-  !.
myTraceBody(Goal, Skip, Depth, Width, Chain, W2, true, no) :- 
                                          myTraceGoal(Goal, Skip, Depth, Width, Chain),
                                          W2 is Width+1.

% WYPISYWANIE KOMUNIKATÓW

% przy exit lub fail usuwamy dodatkowo flagę gen_num
writeCall(Goal) :- incGenFlag(X),
		   format('    ~d    Call: ~p ? ', [X, Goal]).

writeExit(Goal, Depth, Width, Chain) :- setExitFlag(Depth, Width, Chain),
		   removeGenFlag(X),
		   !,
		   format('    ~d    Exit: ~p ? ', [X, Goal]).

writeFail(Goal) :- getGenFlag(X),
		   removeGenFlag(X),
		   format('    ~d    Fail: ~p ? ', [X, Goal]).

% writeRedo(+ Skip, + Depth, + Width) wypisujemy wszystkie redo które trzeba, żeby cofnąć
% się do wierzchołka o głebokości Depth i numerze Width
writeRedo(Skip, Depth, Width) :- getExitFlag(Depth, Width, Chain),
                           !,
                           reverse(Chain, Rev),
                           getGenFlag(X),
                           skip(X, Rev, RevNew),
                           if(
                               user_redo(true),
                               writeRedoChain(false, RevNew),
                               writeRedoChain(Skip, RevNew)
                           ), !.
                           
skip(0, Rev, Rev).
skip(N, [_|Rev], RevNew) :- N > 0, N1 is N-1, skip(N1, Rev, RevNew).


writeRedoChain(_, []).
writeRedoChain(true, [_|Chain]) :- incGenFlag(_X), writeRedoChain(true, Chain).
writeRedoChain(false, [Goal|Chain]) :- incGenFlag(X),
                                format('\n    ~d    Redo: ~p ? ', [X, Goal]),
                                writeRedoChain(false, Chain).

% WCZYTYWANIE + REAGOWANIE NA INPUT

% helpInfo(+ Komunikat, + UnexpectedInput)
% informacje pomocnicze przy nieprawidłowym poleceniu

helpInfo(_, Unexpected) :- 
               format("Unexpected input ~s. Try again. ", [Unexpected]), nl,
               format("You can write: ", []), nl,
               format("    a - abort", []), nl,
               format("    e - exit", []), nl,
               format("    c (or enter) - continue", []), nl,
               fail.
helpInfo(call, _) :- format("    s - skip", []).
helpInfo(redo, _) :- format("    s - skip", []).
helpInfo(failexit, _).

% deal_with_input(+ Input, + Goal, + Depth, + Width, + Chain)
% reakcja na wczytane polecenie do komunikatu "call"
read_input(Goal, Depth, Width, Chain) :- 
                                 read_line(C), 
                                 !, 
                                 deal_with_input(C, Goal, Depth, Width, Chain).

deal_with_input([0'c], Goal, Depth, Width, Chain) :- 
                                      !, 
                                      myTraceDoGoal(Goal, false, Depth, Width, Chain).
deal_with_input([0's], Goal, Depth, Width, Chain) :- 
                                      !, 
                                      myTraceDoGoal(Goal, true, Depth, Width, Chain).
deal_with_input([], Goal, Depth, Width, Chain) :- % read enter
                                      !, 
                                      myTraceDoGoal(Goal, false, Depth, Width, Chain). 
deal_with_input([0'a], _, _, _, _) :- !, cleanGenFlag, cleanExitFlag, abort.
deal_with_input([0'e], _, _, _, _) :- !, cleanGenFlag, cleanExitFlag, halt.
deal_with_input(Unexpected, Goal, Depth, Width, Chain) :- 
                                      helpInfo(call, Unexpected),
                                      read_input(Goal, Depth, Width, Chain).

% deal_with_input_fail_exit
% reakcja na wczytane polecenie do komunikatu "exit" lub "fail"
read_input_fail_exit :- read_line(C), !, deal_with_input_fail_exit(C).

deal_with_input_fail_exit([]) :- !. % read enter
deal_with_input_fail_exit([0'c]) :- !.
deal_with_input_fail_exit([0'a]) :- !, cleanGenFlag, cleanExitFlag, abort.
deal_with_input_fail_exit([0'e]) :- !, cleanGenFlag, cleanExitFlag, halt.
deal_with_input_fail_exit(Unexpected) :- helpInfo(failexit, Unexpected),
                                         read_input_fail_exit.

% deal_with_input_redo(+ Skip, + Body, + Depth, + Width, + Chain)
% reakcja na wczytane polecenie do komunikatu "redo"
read_input_redo(Skip, Body, Depth, Width, Chain) :- 
                                     read_line(C), 
                                     !, 
                                     deal_with_input_redo(Skip, C, Body, Depth, Width, Chain).

deal_with_input_redo(Skip, [], Body, Depth, Width, Chain) :- % read enter
                                     !, 
                                     myTraceBody(Body, Skip, Depth, Width, Chain).
deal_with_input_redo(Skip, [0'c], Body, Depth, Width, Chain) :- 
                                     !, 
                                     myTraceBody(Body, Skip, Depth, Width, Chain).
deal_with_input_redo(_, [0'a], _, _, _, _) :- !, cleanGenFlag, cleanExitFlag, abort.
deal_with_input_redo(_, [0'e], _, _, _, _) :- !, cleanGenFlag, cleanExitFlag, halt.
deal_with_input_redo(_, [0's], Body, Depth, Width, Chain) :- 
                                     !,
                                     myTraceBody(Body, true, Depth, Width, Chain).
deal_with_input_redo(Skip, Unexpected, Body, Depth, Width, Chain) :- 
                                     helpInfo(redo, Unexpected),
                                     read_input_redo(Skip, Body, Depth, Width, Chain).

% OPERACJE NA FLAGACH

% flaga gen_num = numer "zagłębienia" obliczenia, 
% czyli odpowiednik tej drugiej liczby w prawdziwym trace

:- dynamic(gen_num/1). 

setGenFlag(X) :- asserta(gen_num(X)).

getGenFlag(X) :- gen_num(X), !.
getGenFlag(0).

removeGenFlag(X) :- retract(gen_num(X)).

incGenFlag(X1) :- getGenFlag(X),
	      X1 is X+1,
	      setGenFlag(X1).

% nie chcemy zapomnieć o tej fladzę całkowicie, bo wtedy nie możemy
% wywołać getGenFlag, zatem nie abolish tylko retractall
cleanGenFlag :- retractall(gen_num(_)).

% flaga exit_flag = Chain to ścieżka od korzenia to atomu o głebokości 
% Depth i numerze Width
:- dynamic(exit_flag/3).

setExitFlag(Depth, Width, Chain) :- asserta(exit_flag(Depth, Width, Chain)).

getExitFlag(Depth, Width, Chain) :- retract(exit_flag(Depth, Width, Chain)).

cleanExitFlag :- retractall(exit_flag(_, _, _)).

% flaga user_redo/1 jest ustawiania gdy udało się obliczyć odpowiedź, ale
% użytkownik nie potwierdził, zatem być może są jakieś punkty nawrotu ->
% jeśli przed chwilą byliśmy w trybie "skip" to teraz też będziemy, a może już nie
% chcemy -- dajemy użytkowniku ponownie szanse zadecydować

:- dynamic(user_redo/1).

setUserRedo(X) :- retractall(user_redo(_)), asserta(user_redo(X)).

removeUserRedo :- retractall(user_redo(_)).


% INNE POMOCNICZE PREDYKATY

% scal(Conj1, Conj2, Conj1 o Conj2)
scal((A, B), C, (A, BC)) :-  !, scal(B, C, BC).
scal(A, B, (A, B)).

% system(+ Atom)
system(P) :-  predicate_property(P, built_in).

% reverse(+ List, - Rev)
reverse(L, R) :-reverse(L, [], R).

reverse([], R, R).
reverse([X|L], A, R) :- reverse(L, [X|A], R).
