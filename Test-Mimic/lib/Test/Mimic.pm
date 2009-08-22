package Test::Mimic;

use 5.006001;
use strict;
use warnings;

use Test::Mimic::Library qw< load_records >;

our $VERSION = '0.01';

# Preloaded methods go here.

{
    my @pristine_INC;
    
    sub prepare_for_use {
        if ( defined(@pristine_INC) ) {
            @INC = @pristine_INC;
            @pristine_INC = ();
        }
    }
    
    sub require_from {
        my ( $package, $dir ) = @_;
        
        @pristine_INC = @INC;
        @INC = ($dir);
        
        # Load the package
        my $success = eval( "require $package; 1" );

        # Undo the @INC change
        prepare_for_use();

        # Die if the require was not successful.
        if ( ! $success ) {
            die "Unable to require package <$package> from <$dir>: $@";
        }
    }
}

sub import {
    shift(@_); # We don't want to mimic ourself. ;)

    load_records( 'fake_lib/history.rec' );
    
    for my $package_to_mimic (@_) {
        require_from( $package_to_mimic, 'fake_lib' );
    }
}

1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

Test::Mimic - Perl extension for blah blah blah

=head1 SYNOPSIS

  use Test::Mimic;
  blah blah blah

=head1 DESCRIPTION

Stub documentation for Test::Mimic, created by h2xs. It looks like the
author of the extension was negligent enough to leave the stub
unedited.

Blah blah blah.

=head2 EXPORT

None by default.



=head1 SEE ALSO

Mention other useful documentation such as the documentation of
related modules or operating system documentation (such as man pages
in UNIX), or any relevant external documentation such as RFCs or
standards.

If you have a mailing list set up for your module, mention it here.

If you have a web site set up for your module, mention it here.

=head1 AUTHOR

Brendan Roof, E<lt>broof@E<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by Brendan Roof

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.8 or,
at your option, any later version of Perl 5 you may have available.


=cut
