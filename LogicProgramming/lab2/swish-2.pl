first_element([Element|_], Element).

last_element([Element], Element).
last_element([_|TruncatedList], Element) :- last_element(TruncatedList, Element).

prefix(Prefix, List) :- append(Prefix, _, List).

suffix(Suffix, List) :- append(_, Suffix, List).

nth_element(1, [Element|_], Element).
nth_element(N, [_|TruncatedList], Element) :-
    N > 1,
    N1 is N - 1,
    nth_element(N1, TruncatedList, Element).

nth_element_tr(N, List, Element) :- nth_elem_tr(N, List, 1, Element).
nth_element_tr(N, [Element|_], N, Element).
nth_element_tr(N, [_|TruncatedList], I, Element) :-
    I < N,
    I1 is I + 1,
    nth_element_tr(N, TruncatedList, I1, Element).

sum_list([], 0).
sum_list([Element|TruncatedList], Sum) :-
    sum_list(TruncatedList, Sum1),
    Sum is Sum1 + Element.

sum_list_tr(List, Sum) :- sum_list_tr(List, 0, Sum).
sum_list_tr([], Acc, Acc).
sum_list_tr([Element|TruncatedList], Acc, Sum) :-
    Acc1 is Acc + Element,
    sum_list_tr(TruncatedList, Acc1, Sum).

product_list([], 1).
product_list([Element|TruncatedList], Product) :-
    product_list(TruncatedList, Product1),
    Product is Product1 * Element.

product_list_tr(List, Product) :- product_list_tr(List, 1, Product).
product_list_tr([], Acc, Acc).
product_list_tr([Element|TruncatedList], Acc, Product) :-
    Acc1 is Acc * Element,
    product_list_tr(TruncatedList, Acc1, Product).

max_list([Element], Element).
max_list([Element|TruncatedList], Max) :-
    max_list(TruncatedList, TruncatedMax),
    (Element > TruncatedMax -> Max = Element ; Max = TruncatedMax).

max_list_tr([Element|TruncatedList], Max) :-
    max_list_tr(TruncatedList, Element, Max).
max_list_tr([], Acc, Acc).                 
max_list_tr([Element|TruncatedList], Acc, Max) :-
    (Element > Acc -> Acc1 = Element ; Acc1 = Acc),    
    max_list_tr(TruncatedList, Acc1, Max).

min_list([Element], Element).
min_list([Element|TruncatedList], Min) :-
    min_list(TruncatedList, TruncatedMin),
    (Element < TruncatedMin -> Min = Element ; Min = TruncatedMin).

min_list_tr([Element|TruncatedList], Min) :-
    min_list_tr(TruncatedList, Element, Min).
min_list_tr([], Acc, Acc).                 
min_list_tr([Element|TruncatedList], Acc, Min) :-
    (Element < Acc -> Acc1 = Element ; Acc1 = Acc),    
    min_list_tr(TruncatedList, Acc1, Min).

sublist(Sub, List) :-
    append(_, Rest, List),
    append(Sub, _, Rest).

permutation([], []).
permutation(List, [Element|TruncatedList]) :-
    select(Element, List, Rest),
    permutation(Rest, TruncatedList).

reverse_list([], []).
reverse_list([Element|TruncatedList], ReversedList) :-
    reverse_list(TruncatedList, ReversedTruncatedList),
    append(ReversedTruncatedList, [Element], ReversedList).

palindrome(List) :- reverse_list(List, List).

% 16.1. Побудувати список виду [a-4, b-3, c-2, d-1].
subtract_index(List, Result) :-
    subtract_index(List, 1, Result).

subtract_index([], _, []).
subtract_index([X|Xs], Index, [Y|Ys]) :-
    Y is X - Index,
    NextIndex is Index + 1,
    subtract_index(Xs, NextIndex, Ys).


% 16.2. Перевірка симетричності відношення, заданого матрицею.

symmetric(Matrix) :-
    check_symmetry(Matrix, Matrix, 1).

check_symmetry([], _, _).
check_symmetry([Row|Rows], Matrix, I) :-
    check_row(Row, Matrix, I, 1),
    I1 is I + 1,
    check_symmetry(Rows, Matrix, I1).

check_row([], _, _, _).
check_row([Elem|Elems], Matrix, I, J) :-
    nth1(J, Matrix, ColRow),   
    nth1(I, ColRow, Elem),     
    J1 is J + 1,
    check_row(Elems, Matrix, I, J1).