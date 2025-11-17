:- dynamic participants/1.
:- dynamic statement/2.
:- dynamic required_true/1.
:- dynamic came/1.

% set_participants(+List) - встановити список учасників (atoms)
set_participants(List) :-
    retractall(participants(_)),
    assertz(participants(List)).

% set_required_true(+N)
set_required_true(N) :-
    integer(N), N >= 0,
    retractall(required_true(_)),
    assertz(required_true(N)).

% add_statement(+Id, +GoalTerm)
% GoalTerm — терм, який може містити виклики came(Who).
% Приклад: add_statement(s1, ( (came(sister), came(father)) ; (\+came(sister), \+came(father)) )).
add_statement(Id, Goal) :-
    retractall(statement(Id,_)),
    assertz(statement(Id, Goal)).

remove_statement(Id) :-
    retractall(statement(Id,_)).

% subsets(+List, -Subsets) standard power set generator
subsets(List, Subsets) :-
    findall(S, subset_of(List, S), Subsets).

subset_of([], []).
subset_of([H|T], [H|R]) :-
    subset_of(T, R).
subset_of([_|T], R) :-
    subset_of(T, R).

% eval_statements(-TrueCount)
eval_statements(Count) :-
    findall(1,
            ( statement(_, Goal),
              ( catch(call(Goal), _, fail) )   % якщо Goal кидає помилку — вважаємо false
            ),
            Trues),
    length(Trues, Count).

% Виконує retractall(came(_)) і assertz(came(X)) для кожного X в Went.
set_came_for_variant(Went) :-
    % Очистити попередні факти
    retractall(came(_)),
    % Для кожного учасника зі списку Went додати факт came/1
    forall(member(Person, Went),
           assertz(came(Person))).

% solve(-Solutions)
% Solutions — список варіантів у вигляді came_list([p1,p2,...]) що задовольняють required_true(N)
solve(Solutions) :-
    participants(Ps),
    subsets(Ps, Subs),
    required_true(N),
    findall(came_list(Went),
            ( member(Went, Subs),
              set_came_for_variant(Went),
              eval_statements(Count),
              Count =:= N
            ),
            Solutions).

% приклад ініціалізації для Варіанту 16

init :-
    set_participants([sister, mother, father]),
    set_required_true(1),
    retractall(statement(_, _)),
    add_statement(s1, ((came(sister), came(father)) ; (\+came(sister), \+came(father)) )),
    add_statement(s2, ( \+came(sister) ; came(mother) )),
    add_statement(s3, ( (came(sister), came(mother)) ; (came(father), \+came(sister), \+came(mother)) )).
