package Test::Mimic;

use 5.006001;
use strict;
use warnings;

use Test::Mimic::Library qw<
    load_records
    init_records
    load_preferences
>;
use Test::Mimic::Generator;

our $VERSION = '0.01';

# Preloaded methods go here.

{
    my @pristine_INC;
    
    sub prepare_for_use {
        if (@pristine_INC) {
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

        return $success;
    }
}


#    my $preferences_example = {
#        'save'      => '.test_mimic_data',
#        'string'    => sub {},
#        'destring'  => sub {},
#
#        'key'           => sub {},
#        'monitor_args'  => sub {},
#        'play_args'     => sub {},
#        'return'        => 0,
#
#        'packages'  => {
#            'Foo::Bar'  => {
#                'arrays'    => [ qw< x y z > ],
#                'hashes'    => [ qw< x y z > ],
#                'scalars'   => [ qw< x y z > ],
#
#                'key'           => sub {},
#                'monitor_args'  => sub {},
#                'play_args'     => sub {},
#                'return'        => 0,
#
#                'subs' => {
#                    'foo' => {
#                        'key'           => sub {},
#                        'monitor_args'  => sub {},
#                        'play_args'     => sub {},
#                        'return'        => 0,
#                    },
#                },
#            },
#        },
#    };

my $save_to;
my $recording_required;

sub import {
    my ( $class, $preferences ) = @_;

    if ( ! defined( $preferences->{'packages'} ) ) {
        die 'No packages selected to mimic.';
    }

    $save_to = $preferences->{'save'} ||= '.test_mimic_data';

    # Setup the library to behave per user preferences.
    my $history = $save_to . '/history_for_playback.rec';
    if ( -e $history ) { # This won't be true if we haven't recorded at all before.
        load_records($history);
    }
    else {
        init_records();
    }
    load_preferences($preferences);

    # Attempt to load mimicked versions of each package. Note those that have not been recorded.
    my $lib_dir = $save_to . '/lib';
    my @to_record;
    for my $package_to_mimic ( keys %{ $preferences->{'packages'} } ) {
        if ( ! require_from( $package_to_mimic, $lib_dir ) ) {
            push( @to_record, $package_to_mimic );
        }
    }

    # Record the missing packages.
    if ( @to_record != 0 ) {
        $recording_required = 1;
        require Test::Mimic::Recorder;
        my $recorder_prefs = { 'save' => $save_to };
        for my $package (@to_record) {
            $recorder_prefs->{'packages'}->{$package} = $preferences->{'packages'}->{$package};
        }
        Test::Mimic::Recorder->import($recorder_prefs);
    }
}

END {
    if ($recording_required) {
        my $generator = Test::Mimic::Generator->new();
        $generator->load($save_to);
        $generator->write($save_to);
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
