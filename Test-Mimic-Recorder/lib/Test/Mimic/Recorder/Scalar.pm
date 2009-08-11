package Test::Mimic::Recorder::Scalar;

use strict;
use warnings;

use constant {
    # Instance variables
    VALUE   => 0,
    RECORDS => 1,
    HISTORY => 2,
};

sub TIESCALAR {
    my ( $class, $records, $history, $val ) = @_;
    
    # Initialize instance variables.
    my $self = [];
    $self->[VALUE] = $val;
    $self->[RECORDS] = $records;
    $self->[HISTORY] = $history;
    
    bless( $self, $class );
}

sub FETCH {
    my ( $self ) = @_;
    
    my $value = $self->[VALUE];
    if ( ! $Test::Mimic::Recorder::Recording ) {
        push( @{ $self->[HISTORY] }, Test::Mimic::Recorder::_Implementation::_monitor( $self->[RECORDS], $value ) );
    }
    
    return $value;
}

sub STORE {
    my ( $self, $value ) = @_;
    $self->[VALUE] = $value;
}

# optional methods
sub UNTIE {
    
}

sub DESTROY {
    
}

1;
