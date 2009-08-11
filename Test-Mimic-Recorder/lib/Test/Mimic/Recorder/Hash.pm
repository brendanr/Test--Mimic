package Test::Mimic::Recorder::Hash;

use strict;
use warnings;

use base qw<Tie::Hash>;

use constant {
    # Instance variables
    VALUE   => 0,
    RECORDS => 1,
    HISTORY => 2,
    
    # History fields
    FETCH_F     => 0,
    KEYS_F      => 1,
    EXISTS_F    => 2,
    SCALAR_F    => 3,
};

sub TIEHASH {
    my ( $class, $records, $history, $val ) = @_;
    
    # Initialize instance variables.
    my $self = [];
    %{ $self->[VALUE] = {} } = %{$val}; # Copy the hash
    $self->[RECORDS] = $records;
    for my $field ( FETCH_F, EXISTS_F ) {
        $history->[$field] = {};
    }
    for my $field ( KEYS_F, SCALAR_F ) {
        $history->[$field] = [];
    }
    $self->[HISTORY] = $history;
    
    bless( $self, $class );
}

sub STORE {
    my ( $self, $key, $value ) = @_;
    
    $self->[VALUE]->{$key} = $value;
}

sub FETCH {
    my ( $self, $key ) = @_;
    
    my $value = $self->[VALUE]->{$key};
    if ( ! $Test::Mimic::Recorder::SuspendRecording ) {
        my $key_history = ( $self->[HISTORY]->[FETCH_F]->{$key} ||= [] ); 
        push( @{$key_history}, Test::Mimic::Recorder::_Implementation::_monitor( $self->[RECORDS], $value ) );
    }
    
    return $value;
}

sub FIRSTKEY {
    my ($self) = @_;
    
    keys %{ $self->[VALUE] }; # Reset hash iterator.
    return $self->NEXTKEY($self);
}

sub NEXTKEY {
    my ( $self, $last_key ) = @_;
    
    my $key = each %{ $self->[VALUE] };
    if ( ! $Test::Mimic::Recorder::SuspendRecording ) {
        #push( @{ $self->[HISTORY]->[KEYS_F] }, Test::Mimic::Recorder::_Implementation::_monitor( $self->[RECORDS], $key ) ); 
        push( @{ $self->[HISTORY]->[KEYS_F] }, $key ); 
    }
    
    return $key;
}

sub EXISTS {
    my ( $self, $key ) = @_;
    
    my $result = exists $self->[VALUE]->{$key};
    if ( ! $Test::Mimic::Recorder::SuspendRecording ) {
        my $exists_history = ( $self->[HISTORY]->[EXISTS_F]->{$key} ||= [] );
        #push( @{$exists_history}, Test::Mimic::Recorder::_Implementation::_monitor( $self->[RECORDS], $result ) );
        push( @{$exists_history}, $result );
    }
    
    return $result;
}

sub DELETE {
    my ( $self, $key ) = @_;
    
    delete $self->[VALUE]->{$key};
}

# CLEAR will be inherited from Tie::Hash

sub SCALAR {
    my ( $self ) = @_;
    
    my $result = scalar %{ $self->[VALUE] };
    if ( ! $Test::Mimic::Recorder::SuspendRecording ) {
        #push( @{ $self->[HISTORY]->[SCALAR_F] }, Test::Mimic::Recorder::_Implementation::_monitor( $self->[RECORDS], $result ) );
        push( @{ $self->[HISTORY]->[SCALAR_F] }, $result );
    }
    
    return $result;
}

1;
