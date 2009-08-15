package Test::Mimic::Library::PlayScalar;

use strict;
use warnings;

use constant {
    # Instance variable indices
    RECORDS => 0,
    HISTORY => 1,
};

sub TIESCALAR {
    my ( $class, $records, $history ) = @_;

    # Initialize instance variables.
    my $self = [];
    $self->[RECORDS] = $records;
    $self->[HISTORY] = $history;

    return bless( $self, $class );
}

sub FETCH {
    my ( $self ) = @_;

    return Test::Mimic::Library::play( $self->[RECORDS], shift( @{ $self->[HISTORY] } ) );
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
