% male(Person)
male("Стегней Віктор Іванович").
male("Стегней Олександр Іванович").
male("Стегней Іван Іванович").
male("Стегней Євген Іванович").
male("Стегней Дмитро Євгенович").
male("Стегней Іван Васильович").
male("Стегней Василь Васильович").
male("Стегней Василь Ігорович").
male("Машкара Михайло Васильович").
male("Машкара Василь Васильович").

% female(Person)
female("Стегней Тетяна Михайлівна").
female("Стегней Катерина Михайлівна").
female("Стегней Ганна Михайлівна").
female("Машкара Наталя Михайлівна").
female("Машкара Марія Григорівна").
female("Машкара Катерина Ігорівна").
female("Машкара Любов Василівна").


% parent(Parent, Child)
parent("Стегней Іван Іванович", "Стегней Віктор Іванович").
parent("Стегней Тетяна Михайлівна", "Стегней Віктор Іванович").
parent("Стегней Іван Іванович", "Стегней Олександр Іванович").
parent("Стегней Тетяна Михайлівна", "Стегней Олександр Іванович").
parent("Машкара Марія Григорівна", "Стегней Тетяна Михайлівна").
parent("Машкара Михайло Васильович", "Стегней Тетяна Михайлівна").
parent("Машкара Марія Григорівна", "Машкара Наталя Михайлівна").
parent("Машкара Михайло Васильович", "Машкара Наталя Михайлівна").
parent("Машкара Катерина Ігорівна", "Машкара Михайло Васильович").
parent("Машкара Василь Васильович", "Машкара Михайло Васильович").
parent("Машкара Катерина Ігорівна", "Машкара Любов Василівна").
parent("Машкара Василь Васильович", "Машкара Любов Василівна").
parent("Стегней Ганна Михайлівна", "Стегней Іван Іванович").
parent("Стегней Іван Васильович", "Стегней Іван Іванович").
parent("Стегней Ганна Михайлівна", "Стегней Євген Іванович").
parent("Стегней Іван Васильович", "Стегней Євген Іванович").
parent("Стегней Євген Іванович", "Стегней Дмитро Євгенович").
parent("Стегней Катерина Михайлівна", "Стегней Дмитро Євгенович").
parent("Стегней Василь Ігорович", "Стегней Іван Васильович").
parent("Стегней Василь Ігорович", "Стегней Василь Васильович").

mother(Parent,Child) :- female(Parent), parent(Parent,Child).

father(Parent,Child) :- male(Parent), parent(Parent,Child).

sibling(Child1,Child2) :- 
    parent(Parent,Child1), 
    parent(Parent,Child2), 
    Child1 \= Child2.

uncle(Uncle,Child) :-
    male(Uncle),
    sibling(Uncle,UnclesSibling),
    parent(UnclesSibling,Child).

aunt(Aunt,Child) :-
    female(Aunt),
    sibling(Aunt,AuntsSibling),
    parent(AuntsSibling,Child).

cousin(Child1,Child2) :-
    parent(Parent1,Child1),
    parent(Parent2,Child2),
    sibling(Parent1,Parent2).

relative(Person1,Person2) :-
    parent(Person1,Person2);
    parent(Person2,Person1);
    sibling(Person1,Person2);
    cousin(Person1,Person2);
    uncle(Person1,Person2);
    aunt(Person1,Person2);
	uncle(Person2,Person1);
    aunt(Person2,Person1).

ancestor(Ancestor,Child) :-
    parent(Ancestor,Child).
ancestor(Ancestor,Child) :-
    parent(Ancestor,AncestorsChild),
    ancestor(AncestorsChild,Child).

descendant(Descendant,Parent) :-
    parent(Parent,Descendant).
descendant(Descendant,Parent) :-
    parent(Parent,DescendantsParent),
    descendant(Descendant,DescendantsParent).

