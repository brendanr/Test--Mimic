package Test::Mimic::Scalar;

use strict;
use warnings;

use constant {
    # Instance variables
    RECORDS => 0,
    HISTORY => 1,
};

sub TIESCALAR {
}

sub FETCH {
    my ( $self ) = @_;
}

sub STORE {
    # not a read, do nothing
}

# optional methods
sub UNTIE {
    
}

sub DESTROY {
    
}

1;