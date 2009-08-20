package RecordMe;

use strict;
use warnings;

use Scalar::Util;

use Test::Mimic::Library qw< execute get_references HISTORY >;
use Test::Mimic::Library::PlayScalar;
use Test::Mimic::Library::PlayArray;
use Test::Mimic::Library::PlayHash;

BEGIN {
    my $references = get_references();

    tie( %RecordMe::hash_state, q<Test::Mimic::Library::PlayHash>, $references->[0]->[HISTORY] );
    tie( $RecordMe::scalar_state, q<Test::Mimic::Library::PlayScalar>, $references->[3]->[HISTORY] );
    tie( @RecordMe::ISA, q<Test::Mimic::Library::PlayArray>, $references->[1]->[HISTORY] );
    tie( @RecordMe::array_state, q<Test::Mimic::Library::PlayArray>, $references->[2]->[HISTORY] );
}

{
    my %ancestors = qw( Grandpa 400 RecordMe 400 Grandma 400 Mom 400 Dad 400 );

    sub isa {
        my ( $self, $type ) = @_;

        if ( Scalar::Util::reftype($self) ) {
            my $name = Scalar::Util::blessed($self);
            if ($name) {
                return exists( $ancestors{$name} );
            }
            else {
                return ();
            }
        }
        else {
            return exists( $ancestors{$self} );
        }
    }
}

{
    my $behavior =  { "Will this key cause collisions?" => [ [
               [
                 1,
                 {}
               ],
               [
                 300,
                 [
                   200,
                   'alive and well'
                 ]
               ],
               [
                 1,
                 {}
               ],
               [
                 301,
                 [
                   200,
                   "throw threw at t/RecordMe.pm line 42.\n"
                 ]
               ]
             ] ] };

    sub throw {
        return execute( $behavior, @_ );
    }
}

{
    my $behavior =  {};

    sub dad {
        return execute( $behavior, @_ );
    }
}

{
    my $behavior =  { "Will this key cause collisions?" => [
               ( undef ) x 2,
               [
                 [
                   3,
                   { 0 => [
                     201,
                     5
                   ] }
                 ],
                 [ 300 ],
                 [
                   3,
                   { 0 => [
                     201,
                     5
                   ] }
                 ],
                 [ 300 ],
                 [
                   3,
                   { 0 => [
                     201,
                     7
                   ] }
                 ],
                 [ 300 ],
                 [
                   3,
                   { 0 => [
                     201,
                     7
                   ] }
                 ],
                 [ 300 ],
                 [
                   3,
                   { 0 => [
                     201,
                     7
                   ] }
                 ],
                 [ 300 ],
                 [
                   3,
                   {
                     0 => [
                            201,
                            7
                          ],
                     1 => [
                            201,
                            8
                          ]
                   }
                 ],
                 [ 300 ],
                 [
                   3,
                   { 0 => [
                     201,
                     10
                   ] }
                 ],
                 [ 300 ],
                 [
                   3,
                   {
                     0 => [
                            201,
                            10
                          ],
                     2 => [
                            201,
                            11
                          ]
                   }
                 ],
                 [ 300 ],
                 [
                   3,
                   {
                     0 => [
                            201,
                            10
                          ],
                     2 => [
                            201,
                            13
                          ]
                   }
                 ],
                 [ 300 ]
               ]
             ] };

    sub put {
        return execute( $behavior, @_ );
    }
}

{
    my $behavior =  { "Will this key cause collisions?" => [
               [
                 [
                   1,
                   {}
                 ],
                 [
                   300,
                   [
                     200,
                     'positive'
                   ]
                 ],
                 [
                   1,
                   {}
                 ],
                 [
                   300,
                   [
                     200,
                     'zero'
                   ]
                 ],
                 [
                   1,
                   {}
                 ],
                 [
                   300,
                   [
                     200,
                     'negative'
                   ]
                 ]
               ],
               [
                 [
                   1,
                   {}
                 ],
                 [
                   300,
                   [
                     202,
                     [
                       'ARRAY',
                       [ [
                         200,
                         'positive'
                       ] ]
                     ]
                   ]
                 ]
               ],
               [
                 [
                   1,
                   {}
                 ],
                 [ 300 ],
                 [
                   1,
                   {}
                 ],
                 [ 300 ],
                 [
                   1,
                   {}
                 ],
                 [ 300 ]
               ]
             ] };

    sub pos_or_neg {
        return execute( $behavior, @_ );
    }
}

{
    my $behavior =  {};

    sub import {
        return execute( $behavior, @_ );
    }
}

{
    my $behavior =  { "Will this key cause collisions?" => [ [
               [
                 2,
                 {}
               ],
               [
                 300,
                 [
                   200,
                   'mom'
                 ]
               ]
             ] ] };

    sub mom {
        return execute( $behavior, @_ );
    }
}

{
    my $behavior =  {};

    sub grandpa {
        return execute( $behavior, @_ );
    }
}

{
    my $behavior =  { "Will this key cause collisions?" => [ [
               [
                 2,
                 { 0 => [
                   201,
                   5
                 ] }
               ],
               [
                 300,
                 [
                   200,
                   'bill'
                 ]
               ],
               [
                 2,
                 { 0 => [
                   201,
                   5
                 ] }
               ],
               [
                 300,
                 [
                   200,
                   'jane'
                 ]
               ]
             ] ] };

    sub get {
        return execute( $behavior, @_ );
    }
}

{
    my $behavior =  { "Will this key cause collisions?" => [ [
               [
                 1,
                 {}
               ],
               [
                 300,
                 [
                   201,
                   4
                 ]
               ],
               [
                 1,
                 {}
               ],
               [
                 300,
                 [
                   201,
                   6
                 ]
               ],
               [
                 1,
                 {}
               ],
               [
                 300,
                 [
                   201,
                   9
                 ]
               ]
             ] ] };

    sub new {
        return execute( $behavior, @_ );
    }
}

{
    my $behavior =  { "Will this key cause collisions?" => [ [
               [
                 2,
                 {}
               ],
               [
                 300,
                 [
                   200,
                   'grandma'
                 ]
               ]
             ] ] };

    sub grandma {
        return execute( $behavior, @_ );
    }
}

