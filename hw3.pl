/*
 * Program: hw3.pl
 * Authors: Yunyi Ding and Brian Lin
 * On this homework, we worked together for 10 hours,
 * Yunyi worked independently for 5 hours,
 * and Brian worked independently for 5 hours.
*/

father(al, bud).
father(al, kelly).
mother(peggy, kelly).
mother(peggy, bud).
mother(martha, peggy).

grandma(X, Y) :- mother(Z, Y), mother(X, Z).

descendants(X, Y) :- mother(X, Y); father(X, Y); mother(X, Z), descendants(Z, Y); father(X, Z), descendants(Z, Y).

siblings(X, Y) :- mother(M, X), mother(M, Y), father(F, X), father(F, Y), X \= Y.

/*    ASCII-ART for the NFA:
 *
 *    (q0)  ---a-->  (q1)  ---b-->  (q2*)
 *     |
 *     a
 *     |
 *     V  + --<-- +
 *    (q3*)        a
 *        + -->-- +
 */

/*Transition relation:*/
transition(q0,q1,a).
transition(q1,q2,b).
transition(q0,q3,a).
transition(q3,q3,a).

/*Accepting states:*/
accepting(q2).
accepting(q3).

accepts(X, [H|T]) :- accepting(W), transition(X, W, H), T == []; transition(X, Y, H), accepts(Y, T).
