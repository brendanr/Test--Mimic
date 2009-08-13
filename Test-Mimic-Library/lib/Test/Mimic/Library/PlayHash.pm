package Test::Mimic::Library::PlayHash;

use strict;
use warnings;

use base qw<Tie::Hash>;

use constant {
    # Instance variables
    RECORDS => 0,
    HISTORY => 1,
    
    # History fields
    FETCH_F     => 0,
    KEYS_F      => 1,
    EXISTS_F    => 2,
    SCALAR_F    => 3,
};

sub TIEHASH {
    
}

sub STORE {
    # not a read, do nothing
}

sub FETCH {
    my ( $self, $key ) = @_;

}

sub FIRSTKEY {
    my ($self) = @_;

}

sub NEXTKEY {
    my ( $self, $last_key ) = @_;

}

sub EXISTS {
    my ( $self, $key ) = @_;

}

sub DELETE {
    # not a read, do nothing
}

# CLEAR will be inherited from Tie::Hash

sub SCALAR {
    my ( $self ) = @_;
    
}

1;
