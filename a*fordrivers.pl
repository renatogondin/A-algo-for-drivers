% no swish, sem isso aqui embaixo ele não funciona o not
:- op(900,fy,'not').

%as coordenadas X e Y são  bairros
bairro(0,4,4).
bairro(1,2,8).
bairro(2,8,8).
bairro(3,0,7).
bairro(4,1,7).
bairro(5,5,6).
bairro(6,7,6).
bairro(7,3,5).
bairro(8,6,5).
bairro(9,5,3).
bairro(10,8,3).
bairro(11,1,2).
bairro(12,2,2).
bairro(13,3,1).
bairro(14,6,1).
bairro(15,0,0).
bairro(16,7,0).

% caminhos dos bairros e distâncias
caminho(0,9,1).
caminho(0,7,1).
caminho(0,13,4).
caminho(1,2,9).
caminho(1,3,3).
caminho(1,4,1).
caminho(1,5,7).
caminho(1,7,3).
caminho(2,5,6).
caminho(2,6,2).
caminho(2,10,6).
caminho(3,4,1).
caminho(3,11,8).
caminho(3,15,10).
caminho(4,7,5).
caminho(4,11,7).
caminho(5,6,4).
caminho(5,7,5).
caminho(5,8,1).
caminho(5,9,3).
caminho(6,8,1).
caminho(6,9,5).
caminho(6,10,3).
caminho(6,14,7).
caminho(7,11,4).
caminho(7,12,5).
caminho(8,9,2).
caminho(9,13,4).
caminho(9,14,2).
caminho(10,14,5).
caminho(10,16,4).
caminho(11,12,1).
caminho(11,13,3).
caminho(11,15,2).
caminho(12,13,2).
caminho(13,14,6).
caminho(13,15,4).
caminho(14,16,1).
caminho(15,16,12).



calcula(Origem,Chegada,Perc,Total):-

estimated(Origem,Chegada,H),

calcula_helper([c(H/0,[Origem])],Chegada,P,Total),

reverse(P,Perc).

calcula_helper(Trajetos,Chegada,Trajeto,Total):-

menor(Trajetos,Menor,Restantes),

proximos(Menor,Chegada,Restantes,Trajeto,Total).

proximos(c(_/Distancia,Trajeto),Chegada,_,Trajeto,Distancia):-

Trajeto=[Chegada|_].

proximos(c(_,[Chegada|_]),Chegada,Restantes,Trajeto,Total):-!,

calcula_helper(Restantes,Chegada,Trajeto,Total).

proximos(c(_/Distancia,[Ult|T]),Chegada,Trajetos,Trajeto,Total):-

findall(c(H1/D1,[Z,Ult|T]),next(Ult,T,Z,Distancia,Chegada,H1/D1),Lista),

append(Lista,Trajetos,NovosTrajetos),

calcula_helper(NovosTrajetos,Chegada,Trajeto,Total).

menor([Trajeto|Trajetos],Menor,[Trajeto|Resto]):-

menor(Trajetos,Menor,Resto),

menorWay(Menor,Trajeto),!.

menor([Trajeto|Z],Trajeto,Z).

menorWay(c(H1/D1,_),c(H2/D2,_)):-
    C1 is H1+D1, C2 is H2 + D2, C1 < C2.

next(X,T,Y,Distancia,Chegada,H/Dist):-
    (caminho(X,Y,Z);caminho(Y,X,Z)),
     not member(Y,T),
Dist is Distancia + Z,

estimated(Y,Chegada,H).

estimated(B1,B2,Estimado):-

bairro(B1,X1,Y1),

bairro(B2,X2,Y2),

D1 is X1-X2,

D2 is Y1-Y2,

Estimado is sqrt(D1*D1 + D2*D2). 

% calcula(a,r,Melhor_Trajeto,Distância_Percorrida).