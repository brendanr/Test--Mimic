package Test::Mimic::Library::PlayHash;

use strict;
use warnings;

use base qw<Tie::Hash>;

use constant {
    # Instance variables
    HISTORY => 0,
    
    # History fields
    FETCH_F     => 0,
    KEYS_F      => 1,
    EXISTS_F    => 2,
    SCALAR_F    => 3,
};

sub TIEHASH {
    my ( $class, $history ) = @_;

    my $self = [];
    $self->[HISTORY] = $history;

    return bless( $self, $class );
}

sub STORE {
    # not a read, do nothing
}

sub FETCH {
    my ( $self, $key ) = @_;

    require Data::Dump::Streamer;my($a,$b,$c)=caller;print STDERR "FETCH: $a $b $c\n";#DEBUG

    return Test::Mimic::Library::play( shift( @{ $self->[HISTORY]->[FETCH_F]->{$key} } ) );
}

sub FIRSTKEY {
    my ($self) = @_;

    require Data::Dump::Streamer;my($a,$b,$c)=caller;print STDERR "FIRST: $a $b $c\n";#DEBUG
    return $self->NEXTKEY(); 
}

sub NEXTKEY {
    my ( $self, $last_key ) = @_;

    require Data::Dump::Streamer;my($a,$b,$c)=caller;print STDERR "KEY: $a $b $c\n";#DEBUG
    return shift( @{ $self->[HISTORY]->[KEYS_F] } );
}

sub EXISTS {
    my ( $self, $key ) = @_;

    return shift( @{ $self->[HISTORY]->[EXISTS_F]->{$key} } );
}

sub DELETE {
    # not a read, do nothing
}

sub CLEAR {
    # not a read, do nothing
}

sub SCALAR {
    my ( $self ) = @_;

    return shift( @{ $self->[HISTORY]->[SCALAR_F] } );
}

1;
