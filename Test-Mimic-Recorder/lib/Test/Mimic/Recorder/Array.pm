package Test::Mimic::Recorder::Array;

use strict;
use warnings;

use base qw<Tie::Array>;

use constant {
    # Instance variables
    VALUE   => 0,
    RECORDS => 1,
    HISTORY => 2,
    
    # History fields
    FETCH_F     => 0,
    FETCHSIZE_F => 1,
    EXISTS_F    => 2,
};

# basic methods
sub TIEARRAY {
    my ( $class, $records, $history, $val ) = @_;
    
    # Initialize instance variables.
    my $self = [];
    @{ $self->[VALUE] = [] } = @{$val}; # Copy the array
    $self->[RECORDS] = $records;
    for my $field ( FETCH_F, FETCHSIZE_F, EXISTS_F ) {
        $history->[$field] = [];
    }
    $self->[HISTORY] = $history;
    
    bless( $self, $class );
}

sub FETCH {
    my ( $self, $index ) = @_;
    
    my $value = $self->[VALUE]->[$index];
    if ( ! $Test::Mimic::Recorder::SuspendRecording ) {
        my $index_history = ( $self->[HISTORY]->[FETCH_F]->[$index] ||= [] );
        push( @{$index_history}, Test::Mimic::Recorder::_Implementation::_watch( $self->[RECORDS],
            $value ) );
    }
    
    return $value;
}

sub STORE {
    my ( $self, $index, $value ) = @_;
    
    $self->[VALUE]->[$index] = $value;
}

sub FETCHSIZE {
    my ($self) = @_;
    
    my $size = scalar( @{ $self->[VALUE] } );
    if ( ! $Test::Mimic::Recorder::SuspendRecording ) {
        push( @{ $self->[HISTORY]->[FETCHSIZE_F] }, Test::Mimic::Recorder::_Implementation::_watch(
            $self->[RECORDS], $size ) );
    }
    
    return $size;
}

sub STORESIZE {
    my ( $self, $size ) = @_;
    
    $#{ $self->[VALUE] } = $size - 1; #Set the index of the last element.
}

# other methods
sub DELETE {
    my ( $self, $index ) = @_;
    
    delete $self->[VALUE]->[$index];
}

sub EXISTS {
    my ( $self, $index ) = @_;
    
    my $result = exists $self->[VALUE]->[$index];
    if ( ! $Test::Mimic::Recorder::SuspendRecording ) {
        my $exists_history = ( $self->[HISTORY]->[EXISTS_F]->[$index] ||= [] );
        push( @{$exists_history}, Test::Mimic::Recorder::_Implementation::_watch( $self->[RECORDS],
            $result ) );
    }
    
    return $result;
}

#POP, PUSH, SHIFT, UNSHIFT, CLEAR and SPLICE will be inherited from Tie::Array

# optional methods
sub UNTIE {
    
}

sub DESTROY {
    
}

1;
