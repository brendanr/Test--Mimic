package RecordMe;

use strict;
use warnings;

use Mom;
use Dad;

our @ISA = qw( Mom Dad );
our $scalar_state;
our @array_state;
our %hash_state;

sub import {
    print "Importing\n";
}

sub new {
    bless {};
}

sub pos_or_neg {
    if ( $_[0] > 0 ) {
        'positive';
    } elsif ( $_[0] < 0 ) {
        'negative';
    } else {
        'zero';
    }
}

sub put {
    $_[0]->{$_[1]} = $_[2];
}

sub get {
    $_[0]->{$_[1]};
}

sub throw {
    if ($_[0]) {
        die "throw threw";
    } else {
        return "alive and well";
    }
}

1;
