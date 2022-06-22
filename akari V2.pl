
size(8,8).

%wall(Row, Column).

wall(1,6).
wall(2,2).
wall(2,3).
wall(3,7).
wall(4,1).
wall(4,5).
wall(5,4).
wall(5,8).
wall(6,2).
wall(7,6).
wall(7,7).
wall(8,3).

%wall_num(Row, Column, Num).


wall_num(1,6,1).
wall_num(2,2,3).
wall_num(3,7,0).
wall_num(5,4,4).
wall_num(5,8,0).
wall_num(6,2,2).
wall_num(7,6,1).

:-dynamic light/2.
light(0,0).


cell(X,Y):-X>0,X<9,Y>0,Y<9.

neighbor(X,Y,NX,Y):- NX is X-1, cell(NX,Y).

neighbor(X,Y,NX,Y):- NX is X+1 , cell(NX,Y).

neighbor(X,Y,X,NY) :- NY is Y-1 , cell(X,NY).

neighbor(X,Y,X,NY) :- NY is Y+1 , cell(X,NY).

neighbors(X,Y,L) :- findall([NX,NY],neighbor(X,Y,NX,NY),L). 

%كلشي عناصر على يساري 
left(_,1,[]).
left(X,Y,[]) :- Y1 is Y-1 , wall(X,Y1). 
left(X,Y,L) :- Y1 is Y-1 ,  left(X,Y1,L1) , append([[X,Y1]],L1,L).

%كلشي عناصر على يميني
right(_,8,[]).
right(X,Y,[]) :- Y1 is Y+1 , wall(X,Y1). 
right(X,Y,L) :- Y1 is Y+1 ,  right(X,Y1,L1) , append([[X,Y1]],L1,L).

%كلشي عناصر فوقي
up(1,_,[]).
up(X,Y,[]) :- X1 is X-1 , wall(X1,Y). 
up(X,Y,L) :- X1 is X-1 ,  up(X1,Y,L1) , append([[X1,Y]],L1,L).


%كلشي عناصر تحتي
down(8,_,[]).
down(X,Y,[]) :- X1 is X+1 , wall(X1,Y). 
down(X,Y,L) :- X1 is X+1 ,  down(X1,Y,L1) , append([[X1,Y]],L1,L).


column(X,Y,L) :- left(X,Y,L1) , right(X,Y,L2) , append(L1,L2,L).
row(X,Y,L) :- up(X,Y,L1) , down(X,Y,L2) , append(L1,L2,L).
all(X,Y,L) :- column(X,Y,L1) , row(X,Y,L2) , append(L1,L2,L).


is_light2([],0).
is_light2([H|T],Z) :- H = [X,Y] , not(light(X,Y)) , is_light2(T,Z).
is_light2([H|T],Z) :- H = [X,Y] , light(X,Y) , is_light2(T,Z1) , Z is Z1 + 1.

number_of_lights(X,Y,Z) :- all(X,Y,L) , is_light2(L,Z),!.

is_light(X,Y) :- number_of_lights(X,Y,Z) , Z>0 , not(wall(X,Y)).

neighbors_light(X,Y) :- neighbors(X,Y,L) , is_light2(L,Z) , wall_num(X,Y,Z).

all_cells_lighted(8,8,[[8,8]]).
all_cells_lighted(X,Y,L) :- cell(X,Y) , X1 is X+1 , all_cells_lighted(X1,Y,L1) , append([[X,Y]],L1,L).
all_cells_lighted(X,Y,L) :- X=:=9 , Y<9 , Y1 is Y+1 , all_cells_lighted(1,Y1,L).


all_light([]).
all_light([[X,Y]|T]) :- is_light(X,Y) , all_light(T).
all_light([[X,Y]|T]) :- light(X,Y) , all_light(T).  
all_light([[X,Y]|T]) :- wall(X,Y) , all_light(T).  


get_lights([],[]).
get_lights([[X,Y]|T],L) :- light(X,Y) , get_lights(T,L1) , append([[X,Y]],L1,L).
get_lights([[X,Y]|T],L) :- not(light(X,Y)) , get_lights(T,L).


get_wall_num([],[]).
get_wall_num([[X,Y]|T],L) :- wall_num(X,Y,_) , get_wall_num(T,L1) , append([[X,Y]],L1,L).
get_wall_num([[X,Y]|T],L) :- not(wall_num(X,Y,_)) , get_wall_num(T,L).


test_2_light([]).
test_2_light([[X,Y]|T]) :- not(is_light(X,Y)) , test_2_light(T). 


test_wall_num([]).
test_wall_num([[X,Y]|T]) :- neighbors_light(X,Y) , test_wall_num(T).

print_char(X,Y) :- wall_num(X,Y,Z) , ! , write('  ') , write(Z) , write('  ').
print_char(X,Y) :- wall(X,Y) , ! , write('  W  ').
print_char(X,Y) :- light(X,Y) , ! , write('  L  ').
print_char(X,Y) :- is_light(X,Y) , ! , write('  +  ').
print_char(_,_) :- write('  _  ').

print_grid([],_). 
print_grid([[X,Y]|T],S) :- S<9 , S1 is S+1 , print_char(Y,X) , print_grid(T,S1).
print_grid([[X,Y]|T],S) :- S=:=9 , S1 is 1 , nl , nl , print_grid([[X,Y]|T],S1). 


check([X,Y]) :- wall_num(X,Y,_) , ! , nl , write('You cannot put a light in a wall_num cell !!') , nl , nl.
check([X,Y]) :- wall(X,Y) , ! , nl , write('You cannot put a light in a wall cell !!') , nl , nl.
check([X,Y]) :- light(X,Y) , ! , nl , write('this cell has already a light in it') , nl , nl.
check([X,Y]) :- is_light(X,Y) , ! , nl , write('You cannot put a light in this cell because another light lights the same cell'), nl , nl.
check([X,Y]) :- nl , write('Light has been added'), nl , nl , asserta(light(X,Y)).

delete_light([X,Y]) :- not(light(X,Y)) , ! , nl , write('This cell has no light in it ??!!') , nl , nl.
delete_light([X,Y]) :- light(X,Y) , ! , nl , write('Light has been removed') , nl , nl , retract(light(X,Y)).

add :-  write('Enter your light position [X,Y] to add where X = Column and Y = Raw : '),
        read(Pos), check(Pos).

remove :-   write('Enter your light position [X,Y] to remove where X = Column and Y = Raw : '),
            read(Pos), delete_light(Pos).

do_action(X,L) :- X =:= 1 , add , solve(L) -> write('You Win :)').
do_action(X,L) :- X =:= 0 , remove , solve(L) -> write('You Win :)').
do_action(_,_) :- play. 


solve(L):-  all_light(L) ,
            get_lights(L,Z) , test_2_light(Z),
            get_wall_num(L,Q) , test_wall_num(Q), 
            retractall(light(_,_)).

play :- all_cells_lighted(1,1,L) , print_grid(L,1) , nl , nl,
        write('Enter 1 if you want to add a light and 0 if you want to delete : '),
        read(Add_or_Delete),
        do_action(Add_or_Delete,L).
/*

light(1,2).
light(1,7).
light(2,1).
light(2,8).
light(3,2).
light(4,4).
light(4,6).
light(5,3).
light(5,5).
light(6,1).
light(6,4).
light(7,2).
light(7,8).
light(8,6).

*/
