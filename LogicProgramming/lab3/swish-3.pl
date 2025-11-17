% project(Name, Description)
project('MentorFind', 'Front-end розробка веб-порталу').
project('MentorFindAPI', 'Розробка REST API для бекенду').
project('MentorFindApp', 'Клієнтський мобільний застосунок').

% bug(ID, project, reporter, assignee, status, priority, history, tags, comments)
bug(1, project('MentorFind', 'Front-end розробка веб-порталу'),
    reporter('Максим Студітський', 'maks@gmail.com'),
    assignee('Віктор Стегней'),
    status(open),
    priority(high),
    history(['створено', 'призначено виконавця']),
    tags(['UI', 'urgent']),
    comments(list(['Проблема з кнопкою входу', 'Відтворюється у Safari']))).

bug(2, project('MentorFindAPI', 'Розробка REST API для бекенду'),
    reporter('Віктор Стегней', 'viktor@gmail.com'),
    assignee('none'),
    status(open),
    priority(medium),
    history(['створено']),
    tags(['backend']),
    comments('none')).

bug(3, project('MentorFindApp', 'Клієнтський мобільний застосунок'),
    reporter('Денис Штельма', 'denys@gmail.com'),
    assignee('Віктор Стегней'),
    status(in_progress),
    priority(high),
    history(['створено', 'призначено виконавця', 'у роботі']),
    tags(['android', 'crash']),
    comments(list(['Краш при запуску', 'Можливо проблема з токеном']))).

bug(4, project('MentorFind', 'Front-end розробка веб-порталу'),
    reporter('Віктор Стегней', 'viktor@gmail.com'),
    assignee('Денис Штельма'),
    status(resolved),
    priority(low),
    history(['створено', 'призначено', 'виправлено']),
    tags(['css', 'minor']),
    comments('none')).

bug(5, project('MentorFindAPI', 'Розробка REST API для бекенду'),
    reporter('Віктор Стегней', 'viktor@gmail.com'),
    assignee('Максим Студітський'),
    status(in_progress),
    priority(critical),
    history(['створено', 'в роботі']),
    tags(['security', 'auth']),
    comments(list(['JWT токен не оновлюється']))).

bug(6, project('MentorFindAPI', 'Розробка REST API для бекенду'),
    reporter('Віктор Стегней', 'viktor@gmail.com'),
    assignee('none'),
    status(open),
    priority(low),
    history(['створено']),
    tags(['documentation']),
    comments('none')).

bug(7, project('MentorFindApp', 'Клієнтський мобільний застосунок'),
    reporter('Денис Штельма', 'denys@gmail.com'),
    assignee('Віктор Стегней'),
    status(in_review),
    priority(high),
    history(['створено', 'в роботі', 'на перевірці']),
    tags(['ios', 'ui']),
    comments(list(['Не працює свайп вліво', 'Проблема у gesture recognizer']))).

bug(8, project('MentorFind', 'Front-end розробка веб-порталу'),
    reporter('Максим Студітський', 'maks@gmail.com'),
    assignee('Денис Штельма'),
    status(open),
    priority(medium),
    history(['створено']),
    tags(['accessibility']),
    comments(list(['Текст кнопок нечитабельний у темній темі']))).

bug(9, project('MentorFindApp', 'Клієнтський мобільний застосунок'),
    reporter('Денис Штельма', 'denys@gmail.com'),
    assignee('Віктор Стегней'),
    status(resolved),
    priority(low),
    history(['створено', 'призначено', 'виправлено']),
    tags(['performance']),
    comments('none')).

bug(10, project('MentorFindAPI', 'Розробка REST API для бекенду'),
    reporter('Віктор Стегней', 'viktor@gmail.com'),
    assignee('Максим Студітський'),
    status(in_progress),
    priority(medium),
    history(['створено', 'в роботі']),
    tags(['database', 'query']),
    comments(list(['Оптимізувати SQL-запит']))).

bug_status(ID, Status) :-
    bug(ID, _, _, _, status(Status), _, _, _, _).

bug_assignee(ID, Assignee) :-
    bug(ID, _, _, assignee(Assignee), _, _, _, _, _).

bugs_by_project(ProjectName, IDs) :-
    findall(ID, bug(ID, project(ProjectName, _), _, _, _, _, _, _, _), IDs).

bugs_by_priority(Priority, IDs) :-
    findall(ID, bug(ID, _, _, _, _, priority(Priority), _, _, _), IDs).

bug_comments(ID, Comments) :-
    bug(ID, _, _, _, _, _, _, _, Comments).

count_bugs_by_status(Status, Count) :-
    findall(Status, bug(_, _, _, _, status(Status), _, _, _, _), L),
    length(L, Count).

priority_value(critical, 4).
priority_value(high, 3).
priority_value(medium, 2).
priority_value(low, 1).

bug_with_highest_priority(ID) :-
    findall(Priority-ID, (bug(ID, _, _, _, _, priority(Priority), _, _, _), priority_value(Priority, _)), List),
    maplist([P-I, V-I]>>(priority_value(P, V)), List, ValList),
    keysort(ValList, Sorted),
    reverse(Sorted, [ _-ID | _ ]).

all_tags(Tags) :-
    findall(T, (bug(_, _, _, _, _, _, _, tags(L), _), member(T, L)), All),
    sort(All, Tags).

count_bugs(Count) :-
    findall(ID, bug(ID, _, _, _, _, _, _, _, _), List),
    count_list(List, Count).

count_list([], 0).
count_list([_|T], Count) :-
    count_list(T, C1),
    Count is C1 + 1.


