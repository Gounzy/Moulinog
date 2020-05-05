:- module(moulinette, []).
:- use_module(library(csv)).
:- encoding(utf8).
:- discontiguous param/2.
:- discontiguous question/3.

%%%%%%%%%%%%%%%%%%%%%
%%%% QUESTIONS CSV %%
%%% GENERATION %%%%%%
%%%%%%%%%%%%%%%%%%%%%
generate_csv:-
  read_csv_names('names.csv', Names),
  length(Names, L),
  choose_all_questions([1,2,3,4,5],L,Questions),
  print_questions_as_csv([1,2,3,4,5],Names,Questions),
  format('output.csv has been successfully created'),
  !.

%%%%%%%%%%%%%%%%%%%%%
%%%%%%% BASH SCRIPT %
%%% GENERATION %%%%%%
%%%%%%%%%%%%%%%%%%%%%
bash(NbPages, Password):-
  generate_bash(NbPages, "CARANO", "gonzague.yernaux@unamur.be", "gyernaux", Password).
generate_bash(NbPages, ZipPW, From, EId, Password):-
    read_csv_names('names.csv', Names),
    read_csv_mails('mails.csv', Mails),
    length(Names, L),
    length(Mails, L),
    reverse(Names, NamesReverted),
    reverse(Mails, MailsReverted),
    open('bash.txt', write, Out),
    loop_pdftk_tar_mail(NbPages, NamesReverted, MailsReverted, L, Out, ZipPW, From, EId, Password),
    close(Out),
    format('bash.txt has been successfully created').

loop_pdftk_tar_mail(_, [], [], 0, _, _, _, _, _):-!.
loop_pdftk_tar_mail(NbPages,[N|Names], [M|Mails], NbStudents, Out, ZipPW, From, EId, Password):-
  NbStudents1 is NbStudents - 1,
  %NbStudents =< 4,
  !,
  loop_pdftk_tar_mail(NbPages, Names,Mails,NbStudents1, Out, ZipPW, From, EId, Password),
  FirstPage is (NbStudents1 * NbPages) + 1,
  LastPage is FirstPage + NbPages,
  string_concat("pdf", NbStudents, PdfStr1),
  string_concat(PdfStr1, ".pdf", PdfStr),
  string_concat("secure", N, ZipStr1),
  string_concat(ZipStr1, ".zip", ZipStr),

  % Extract PDF for one student
  write(Out, '\n
pdftk full-pdf.pdf cat '),
  write(Out, FirstPage),
  write(Out, '-'),
  write(Out, LastPage),
  write(Out, ' output '),
  write(Out, PdfStr),

  % Zip the PDF with password
  write(Out, '\n7z a -tzip -p'),
  write(Out, ZipPW),
  write(Out,' -mem=AES256 "'),
  write(Out, ZipStr),
  write(Out, '" '),
  write(Out, PdfStr),

  % Send it by email
  write(Out,'\n
$file = "'),
          working_directory(WD, WD),
          write(Out, WD),
          write(Out, 'pdf/'),
          write(Out, ZipStr),
          write(Out, '"
$att = new-object Net.Mail.Attachment($file)
$msg = new-object Net.Mail.MailMessage
$msg.From = "gonzague.yernaux@unamur.be"
$msg.To.Add("'),
          write(Out, M),
          write(Out, '")
$msg.Subject = "Examen FCO"
$msg.Body = "Message envoyé depuis un script shell"
$msg.Attachments.add($att)
$SMTPServer = "smtp.unamur.be"
$SMTPClient = New-Object Net.Mail.SmtpClient($SmtpServer, 587)
$SMTPClient.EnableSsl = $true
$SMTPClient.Credentials = New-Object System.Net.NetworkCredential("'),
          write(Out, EId),
          write(Out, '", "'),
            write(Out,Password),
          write(Out,'");
$SMTPClient.Send($msg)').

% loop_pdftk_tar_mail(NbPages,[_|Names], [_|Mails], NbStudents, Out, ZipPW, From, EId, PW):-
%     NbStudents1 is NbStudents - 1,
%     loop_pdftk_tar_mail(NbPages, Names,Mails,NbStudents1, Out, ZipPW, From, EId, PW).

%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% CSV %%%%%
%%%%%%% INPUT %%%%%%%
%%%%%%%%%%%%%%%%%%%%%
read_csv_names(File, Names):-
  csv_read_file(File, Rows),
  rows_to_list(Rows, Names),
  !.
rows_to_list([], []).
rows_to_list([R|Rows], [N|Names]):-
  row_to_name(R,N),
  rows_to_list(Rows, Names).
row_to_name(row(String), Name):-
  sub_string(String, Before, _, After, ";"), !,
  sub_string(String, 0, Before, _, Name1),
  sub_string(String, _, After, 0, Name2),
  string_concat(Name1, ' ', Name11),
  string_concat(Name11, Name2, Name).

read_csv_mails(File, Mails):-
  csv_read_file(File, Rows),
  rows_to_simple_list(Rows,Mails).
rows_to_simple_list([], []).
rows_to_simple_list([row(X)|Rows], [X|List]):-
  rows_to_simple_list(Rows, List).

%%%%%%%%%%%%%%%%%%%%%
%%%%% QUESTIONS %%%%%
%%%%%% GENERATION %%%
%%%%%%%%%%%%%%%%%%%%%
choose_all_questions([], _, []).
choose_all_questions([C|Categories], N, [QuestionsC|Questions]):-
  choose_n_questions(C, N, QuestionsC),
  choose_all_questions(Categories, N, Questions),
  !.

choose_n_questions(Category, N, Questions):-
  questions(Category,CategoryQuestions),
  take_n_and_restart(CategoryQuestions, N, Questions).

take_n_and_restart([], _, []).
take_n_and_restart(_, N, []):-
  N < 1.
take_n_and_restart(List, N, Elems):-
  take_n(List, N, Elems1),
  length(Elems1, L1),
  N1 is N - L1,
  take_n_and_restart(List, N1, Elems2),
  append(Elems1, Elems2, Elems).

take_n([], _, []).
take_n(_, 0, []).
take_n(List, N, [Elem|Taken]):-
  random_element(List, Elem),
  select(Elem,List,NList),
  N1 is N - 1,
  take_n(NList,N1,Taken).

questions(CategoryId, Questions):-
  findall(Q, question(CategoryId, _, Q), QuestionsPlain),
  zip_questions_with_params(QuestionsPlain, QuestionsWithParams),
  parametrize(QuestionsWithParams, Questions).

zip_questions_with_params([], []).
zip_questions_with_params([Q|Qs], [Q-Params|QsWithParams]):-
  extract_params(Q, Params),
  zip_questions_with_params(Qs, QsWithParams).

extract_params([], []).
extract_params([E|List], Params):-
  not(number(E)),
  extract_params(List, Params).
extract_params([E|List], [E|Params]):-
  number(E),
  extract_params(List, Params).

parametrize([], []).
parametrize([Q-P|QuestionsWithParams], Questions):-
  parametrize(QuestionsWithParams, Questions1),
  findall(Question,flatten_parametrize(Q-P, Question), Questions2),
  append(Questions1, Questions2, Questions).

flatten_parametrize(Question-[], QuestionOut):-flatten_list(Question,QuestionOut).
flatten_parametrize(Question-[P|Params], QuestionOut):-
  findall(Param, param(P, Param),ListParams),
  member(Elem, ListParams),
  replace_occurences(Question, P, Elem, NewQuestion),
  flatten_parametrize(NewQuestion-Params, QuestionOut).

replace_occurences(Xs, X, _, Xs):-
  not(member(X, Xs)),
  !.
replace_occurences(Xs, X, Y, Ys):-
  replace(Xs, X, Y, Xss),
  replace_occurences(Xss, X, Y, Ys).

replace(Xs,X,Y,Ys) :-
   same_length(Xs,Ys),
   append(Prefix,[X|Suffix],Xs),
   maplist(dif(X),Prefix),
   append(Prefix,[Y|Suffix],Ys).

flatten_list(List, String):-
  atomic_list_concat(List, Atom), atom_string(Atom, String).

%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% CSV %%%%%
%%%%%% OUTPUT %%%%%%%
%%%%%%%%%%%%%%%%%%%%%
print_questions_as_csv(Headers,Names, Questions):-
    open('output.csv',write,Out),
    retractall(outstream(_)),
    assert(outstream(Out)),
    write(Out, "N°|Nom"),
    print_headers_as_csv(Headers,Out),
    print_questions_as_csv(1,Names,Questions, Out),
    close(Out).
print_questions_as_csv(_, [], [[]|_],_).
print_questions_as_csv(_,[], [],_).
print_questions_as_csv(N,Names,Questions,Out):-
  write(Out, N),
  print_and_remove_first(Names, NNames),
  maplist(print_and_remove_first, Questions, NQuestions),
  write(Out, '\n'),
  N1 is N + 1,
  print_questions_as_csv(N1, NNames, NQuestions,Out).

print_headers_as_csv([], Out):-
  write(Out, '\n').
print_headers_as_csv([H|Headers], Out):-
  write(Out, "|Question "),
  write(Out, H),
  print_headers_as_csv(Headers, Out).

print_and_remove_first([], []).
print_and_remove_first([Question|QuestionsList],QuestionsList):-
  outstream(Out),
  write(Out, '|"'),
  write(Out, Question),
  write(Out, '"').


%%%%%%%%%%%%%%%%%%%%%
%%%%% QUESTIONS %%%%%
%%%%%% & PARAMS %%%%%
%%%%%%%%%%%%%%%%%%%%%
question(1, 1, [10, 11, 12, 13, 14, 17, 18, "."]).
question(1, 2, [10, "d'entiers", 12, 15, 17, 18, "."]).
question(1, 3, [10, "de caractères", 12, 16, 17, 18, "."]).

param(10, "On cherche à réaliser une fonction parcourant un tableau ").
param(11, "d'entiers").
param(11, "de caractères").

param(12, " de façon récursive, afin ").

param(13, "d'y chercher un élément. La fonction renverra true (1) si l'élément se trouve dans le tableau, false (0) sinon.").
param(13, "d'y compter le nombre d'occurrences d'un élément donné. La fonction renverra donc un entier N, qui correspond au nombre de fois que l'élément recherché apparaît dans le tableau.").
param(13, "de copier tous ses éléments dans un second tableau.").
param(13, "de copier tous ses éléments, dans l'ordre inverse, dans un second tableau.").
param(13, "de vérifier si le tableau est un palindrome, c'est-à-dire peut se lire de gauche à droite et de droite à gauche indifféremment. Indice : parcourez le tableau deux cases à la fois, et vérifiez dans votre fonction si le premier élément du tableau est le même que le dernier élément ; puis rappelez la fonction récursivement. La fonction renvoie true (1) si le tableau est un palindrome, false (0) sinon. Exemple : [n, m, l, m, n], [n, m, n] et [n] sont des palindromes ; [n, m, l] et [l,m] n'en sont pas.").
param(13, "de remplacer toutes les occurrences d'un certain élément donné X, par un autre élément donné Y, et vice-versa.").
param(13, ", étant donné trois éléments X, Y et Z, de remplacer dans le tableau toutes les occurrences de X par Y; toutes les occurrences de Y par Z, et toutes les occurrences de Z par X.").
param(13, "de remplacer tous les éléments du tableau par un élément unique X. Le tableau devra donc, après appel de la fonction, ne contenir qu'une seule valeur (X).").

param(14, " \r\nDonnez le code haut niveau (Python/C/pseudo-code/...) de cette fonction, et implémentez-la en code MIPS 32 bits. \r\nConseil : identifiez bien les différents paramètres de votre fonction.").

param(15, "de calculer le produit de tous ses éléments.").
param(15, "de calculer la somme en dent de scie des éléments du tableau. La somme en dent de scie est définie, pour un tableau aux éléments t1, t2, t3, t4, t5, ..., tn, comme t1 - t2 + t3 - t4 + t5 - ...").
param(15, "de vérifier si tous ses éléments sont pairs. La fonction renverra true (1) si tous les éléments du tableau sont pairs, false (0) sinon.").
param(15, "de vérifier si tous ses éléments sont impairs. La fonction renverra true (1) si tous les éléments du tableau sont impairs, false (0) sinon.").
param(15, "de compter le nombre d'éléments pairs du tableau. La fonction renverra donc un entier N, qui correspond au nombre d'éléments pairs du tableau.").
param(15, "de compter le nombre d'éléments impairs du tableau. La fonction renverra donc un entier N, qui correspond au nombre d'éléments impairs du tableau.").
param(15, "de multiplier par un certain nombre N chacun de ses éléments. Le tableau sera donc modifié (récursivement !), chaque élément X devant être remplacé par son N*X.").

param(16, "de remplacer toutes les lettres majuscules par la même lettre en minuscule.").
param(16, "de remplacer toutes les lettres minuscules par la même lettre en majuscule.").

param(17, "\r\nAttention : pour réaliser votre fonction, vous n'avez pas le droit d'utiliser ").
param(18, "l'instruction beq").
param(18, "l'instruction sub").
param(18, "le registre $zero").

question(2, 1, ["Vous travaillez sur un système de test où la mémoire cache - initialement vide - contient ",21,". Sachant, de plus, que la mémoire est en ", 23 ,", qu'il y a ",24," par bloc, et que l'adresse (décimale) de myarray est ",25,", et l'adresse (décimale) de mynewarray est ", 26 , ", on vous demande \r\n (1) d'observer le code MIPS suivant et de le commenter pour en expliquer le sens, et \r\n (2) de simuler les effets de son exécution sur votre mémoire cache. Pour cela, schématisez la mémoire cache et expliquez clairement son évolution au fil de l'exécution du code. \r\n", 27, 28, 29]).

param(21, "4 blocs, et est organisée en direct mapping").
param(21, "8 blocs, et est organisée en direct mapping").
param(21, "4 blocs, et est organisée en 2-way set associative (politique de remplacement LRU)").
param(21, "8 blocs, et est organisée en 2-way set associative (politique de remplacement LRU)").
param(21, "8 blocs, et est organisée en 4-way set associative (politique de remplacement LRU)").
param(23, "write-back").
param(23, "write-through").
param(24, "un mot").
param(24, "deux mots").
param(24, "quatre mots").
param(25, "428500996").
param(25, "268500992").
param(26, "268500000").
param(26, "428500000").
param(27, Code):-mips_start_code(Code).
mips_start_code(Code) :-
  Code = ".data
  myOldArray:		.word	4, 5, 13, 13, 9
  myNewArray:		.word 0, 0, 0, 0, 0
.text
main:
   	la $s0, myOldArray
   	addi $s1, $zero, 5
   	la $s2, myNewArray
   	add $t0, $zero, $zero".
mips_start_code(Code) :-
  Code = ".data
  myOldArray:		.word	13, 5, 9, 13, 13
  myNewArray:		.word 0, 0, 0, 0, 0
.text
main:
   	la $s0, myOldArray
   	addi $s1, $zero, 5
   	la $s2, myNewArray
   	add $t0, $zero, $zero".
mips_start_code(Code) :-
  Code = ".data
  myOldArray:		.word	4, 13, 13, 5, 13, 9
  myNewArray:		.word 0, 0, 0, 0, 0, 0
.text
main:
   	la $s0, myOldArray
   	addi $s1, $zero, 6
   	la $s2, myNewArray
   	add $t0, $zero, $zero".
mips_start_code(Code) :-
  Code = ".data
  myOldArray:		.word	13, 4, 13, 4, 13, 5
  myNewArray:		.word 0, 0, 0, 0, 0, 0
.text
main:
   	la $s0, myOldArray
   	addi $s1, $zero, 6
   	la $s2, myNewArray
   	add $t0, $zero, $zero".
param(28, Code):- mips_loop_code(Code).
mips_loop_code(Code):-
  Code = "
LOOP:
   	slt $t1, $t0, $s1
   	beq $t1, $zero, EXIT
   	add $t2, $t0, $t0
   	add $t2, $t2, $t2
   	add $t3, $s2, $t2
   	add $t2, $s0, $t2
   	lw $s3, 0($t2)
   	addi $t4, $zero, 13
   	beq $s3, $t4, ENDLOOP
   	sw $s3, 0($t3)".
param(29, Code):- mips_end_code(Code).
mips_end_code(Code):-
  Code = "
ENDLOOP:
   	addi $t0, $t0, 1
   	j LOOP".
mips_end_code(Code):-
  Code = "
ENDLOOP:
   	addi $t0, $t0, 2
   	j LOOP".

question(3, 1, ["Supposons que nous recevions le message hexadécimal suivant : ", 33, 39, 31, 32, 34, 35, 36, ". \r\nDéterminez si ce message est acceptable en sachant que l'expéditeur du message y a intégré le code correctif de Hamming. S'il s'avère que le message est erroné, il vous est demandé de le corriger dans la mesure du possible."]).
question(3, 2, ["Supposons que nous recevions le message hexadécimal suivant : ", 30, 31, 32, 34, 35, 36, 37, ". \r\nDéterminez si ce message est acceptable en sachant que l'expéditeur du message y a intégré la parité croisée LRC/VRC. S'il s'avère que le message est erroné, il vous est demandé de le corriger dans la mesure du possible."]).

hexa_1("6"). hexa_1("B"). hexa_1("C"). hexa_1("F").
hexa("7"). hexa("8"). hexa("D"). hexa("E").
param(30, X):- param(54,X).
param(31, X):- hexa(X).
param(32, X):- hexa_1(X).
param(34, X):- hexa_1(X).
param(35, X):- hexa(X).
param(36, X):- hexa_1(X).
param(37, X):- hexa(X).
param(33, ""). param(33, ""). % To get 50/50 chance between LRC/VRC and Hamming
param(39, ""). param(39, ""). param(39, ""). param(39, ""). % To get 50/50 chance between LRC/VRC and Hamming

question(4,1, ["Soit une fonction logique F(A,B,C,D) produisant 1 aux entrées suivantes : ", 41, "et 0 pour tous les autres cas. \r\nEtablissez la table de vérité de cette fonction, dérivez-en le diagramme de Karnaugh et, enfin, créez-en le cicuit logique minimal."]).

param(41, X):-
  NbKarnaughToGenerate is 40,
  generate_n_karnaugh(NbKarnaughToGenerate, Strs),
  !,
  random_element(Strs, X).
random_element(Strs, X):-
  length(Strs, L),
  random(0, L, Index),
  nth0(Index, Strs, X).

generate_n_karnaugh(0, []).
generate_n_karnaugh(N, [String|Strings]):-
  N > 0,
  N1 is N - 1,
  generate_n_karnaugh(N1, Strings),
  generate_karnaugh(N,String),
  not(member(String, Strings)).

generate_karnaugh(Nth, String):-
  generate_function(Vals),
  sort(Vals, Values),
  length(Values, L),
  (Nth > 30 -> Min is 5 ; (Nth > 20 -> Min is 6 ; (Nth > 10 -> Min is 7 ; Min is 8))),
  L >= Min,
  possible_karnaugh(Values),
  values_to_string(Values, String).

bit(0).
bit(1).

generate_function([]).
generate_function([f(A,B,C,D)|Values]):-
  A is random(2), B is random(2), C is random(2), D is random(2),
  generate_function(Values).

values_to_string(Values, String):- values_to_string(Values, "", String).
values_to_string([], String, String).
values_to_string([V|Values], CurrentString, String):-
  term_to_atom(V, A),
  atom_string(A, S),
  string_concat(S, ", ", NS),
  string_concat(CurrentString, NS, NCurrentString),
  values_to_string(Values, NCurrentString, String).

possible_karnaugh(Values):-
    select(Value1, Values, OtherValues),
    member(Value2, OtherValues),
    one_diff(Value1, Value2).

one_diff(f(A1,B1,C1,_), f(A1,B1,C1,_)).
one_diff(f(A1,B1,_,D1), f(A1,B1,_,D1)).
one_diff(f(A1,_,C1,D1), f(A1,_,C1,D1)).
one_diff(f(_,B1,C1,D1), f(_,B1,C1,D1)).

question(5, 1, [50, "On s'intéresse au processeur, ", 52, ". Soit l’instruction ", 51, ". \r\nModifiez le processeur ", 52, " pour qu’il puisse supporter cette instruction. Précisez le format de l’instruction ainsi que la valeur des signaux de contrôle."]).
question(5, 2, [50, "On s'intéresse au processeur ", 52 , ". On fait appel à vous, car le processeur semble ne pas fonctionner correctement. En fait, l'on soupçonne que le signal de contrôle ", 53, " est forcé à la valeur " , 54, ". Il vous est demandé d'imaginer une technique pour identifier ce problème, c'est-à-dire pouvoir affirmer si oui ou non le problème est avéré."]).
question(5, 3, [50, "On s'intéresse au processeur ", 52 , ". On fait appel à vous, car le processeur semble ne pas fonctionner correctement. En fait, l'on soupçonne que le signal de contrôle ", 55, " est forcé à la valeur 0. Il vous est demandé d'imaginer une technique pour identifier ce problème, c'est-à-dire pouvoir affirmer si oui ou non le problème est avéré."]).
question(5, 4, [50, "On s'intéresse au processeur ", 52 ,". Quel(s) changement(s) faudrait-il apporter à ce processeur pour qu'il supporte uniquement les instructions ", 56, " ? Discutez."]).

param(50, "En annexe figurent les schémas des processeurs mono-cycle et pipeline. ").

param(51, "maxabs $reg1, $reg2, $reg3, qui stocke dans le registre $reg1 la valeur parmi celles contenues dans les registres $reg2 et $reg3 qui a la plus grande valeur absolue").
param(51, "addm $rd, $rs, $rt, ShiftAmount, qui réalise le calcul suivant : $rd = $rt + Memory[$rs + ShiftAmount]").
param(51, "beqm $rt, $rs, $rd, ShiftAmount, qui réalise l'opération suivante : if ($rt == Memory[$rs + ShiftAmount]), then PC = $rd").
param(51, "swi $rd, $rs, $rt, qui réalise l'opération suivante : Memory[$rs + $rt] = $rd").
param(51, "eob $t0, $t1 effectuant l'opération suivante : stocke dans $t1 un nouveau mot de 32 bits dont tous les bits aux positions impaires sont mis à 1, et tous les autres bits ont la même valeur que les bits homologues (aux mêmes positions (paires)) dans $t0.").
param(51, "lbw $dest, address qui charge un octet depuis la mémoire dans les 8 bits de poids faible d'un registre").
param(51, "nj $s1, delta réalisant l'opération PC = $s1 + delta").
param(51, "lwi $rt, $rd($rs) réalisant l'opération $rt = Memory[$rs] + Memory[$rd]").
param(51, "nbe $s1, $s2, delta qui réalise l'opération suivante : if $s1 != delta then PC += 4 ; else PC = $s2").
param(51, "mbl $s1, $s2, delta qui réalise l'opération suivante : if Memory[$s1] < Memory[$s2] then PC = PC + delta").
param(51, "leq $s1, $s2, $s3 qui réalise l'opération suivante : if $s1 == $s2 then $s1 = Memory[$s3 + ShiftAmount]").
param(51, "sne $s1, $s2, cste qui réalise l'opération suivante : if ($s1 != cste) && ($s2 != cste) then PC +=8").
param(51, "maxabs $s1, $s2, $s3 qui réalise l'opération suivante : $s1 = MAX[abs($s2), abs($s3)]").
param(51, "sio $s1, $s2, $s3 réalisant l'opération suivante : if $s3 is odd then $s1 = $s2").

param(52, "mono-cycle").
param(52, "pipeline").

param(53, "AluSrc").
param(53, "MemToReg").
param(53, "RegDst").

param(54, "0").
param(54, "1").

param(55, "MemWrite").
param(55, "MemRead").
param(55, "RegWrite").

param(56, "and et sw").
param(56, "or et lw").
param(56, "add et sw").
param(56, "add et lw").
param(56, "and et lw").
