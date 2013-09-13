#!/usr/bin/perl

#full logic for string: full regexp,
#  how ? pad: comparer des langages (comparer des automates)
#  decidable? cachera: yes
#  algo? cachera: intersection, complement
#        pad: need aussi determinasation(enlever epsilon), completude (pour le complementaire  l'auto doit etre complet))
#  ref?: hopcroft ? wolper ? autebert ?

# 'a |= b'  means that 'a' is more specialised than 'b'
# which means that the langage of 'b' is greater (or equal) than the langage of 'a'
# which is true when (~b /\ a) = empty (draw set diagram to understand it)

#usage:
# regexp_logic.pl 'a+' 'a*'  => yes
# regexp_logic.pl 'a*' 'a+'  => no

# need the FSA utilities from Gertjan van Noord
#  available at: http://www.let.rug.nl/~vannoord/Fsa

#TODO: perl regexp to fsa regexp

#TODO: syntax ?
# xxx:=~... => forbid valued attribute with =~ at the beginning
# sugar: <, >, <>


#OPT: multi engine, growing complexity: =~ perl then ferre engine then fsa full engine
#OPT: look if a normal string, in that case just call =~ of perl
#OPT: use specialised version of ferre when can do it
#OPT: dont call many times fsa, do it via a prolog predicate in one pass

my $a = $ARGV[0];
my $b = $ARGV[1];

`rm -f /tmp/fsa_*`;
`fsa -r "$a" 2>/dev/null > /tmp/fsa_a`;
`fsa -r "$b" 2>/dev/null | fsa -complement 2>/dev/null > /tmp/fsa_b`;
`fsa -intersect /tmp/fsa_a /tmp/fsa_b 2>/dev/null > /tmp/fsa_result`;

my $res = `grep '% final states' /tmp/fsa_result`;

if ($res eq "[], % final states\n")
  { print "yes" } # empty automaton => yes
else {
    my $res = `grep 'end final states' /tmp/fsa_result`;
    if ($res eq "], % end final states\n")
      { print "no" }
    else
      { print "pb, seems not valid regexp" }
}
