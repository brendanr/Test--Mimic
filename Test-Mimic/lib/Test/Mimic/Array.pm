package Test::Mimic::Array;

use strict;
use warnings;

use base qw<Tie::Array>;

use constant {
    # Instance variables
    RECORDS => 0,
    HISTORY => 1,
    
    # History fields
    FETCH_F     => 0,
    FETCHSIZE_F => 1,
    EXISTS_F    => 2,
};

# basic methods
sub TIEARRAY {

}

sub FETCH {
    my ( $self, $index ) = @_;
    
}

sub STORE {
    # not a read, do nothing
}

sub FETCHSIZE {
    my ($self) = @_;
    
}

sub STORESIZE {
    # not a read, do nothing
}

# other methods
sub DELETE {
    # not a read, do nothing
}

sub EXISTS {
    my ( $self, $index ) = @_;
    
}

#POP, PUSH, SHIFT, UNSHIFT, CLEAR and SPLICE will be inherited from Tie::Array

# optional methods
sub UNTIE {
    
}

sub DESTROY {
    
}


1;