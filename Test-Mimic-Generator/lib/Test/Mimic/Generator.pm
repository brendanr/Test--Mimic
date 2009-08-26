package Test::Mimic::Generator;

use 5.006001;
use strict;
use warnings;

our $VERSION = 0.001_001;

sub get_object_package {
    my ($class) = @_;
    return $class . '::Object';
}

sub new {
    my ($class) = @_;
    return bless( [], $class->get_object_package() );
}

package Test::Mimic::Generator::_Implementation;

use Test::Mimic::Library qw< stringify stringify_by DATA descend >;
use Cwd qw<abs_path>;
use File::Copy;

BEGIN {
    my $offset = 0;
    for my $field ( qw< TYPEGLOBS EXTRA OPERATION_SEQUENCE READ_DIR > ) {
        eval("sub $field { return $offset; }");
        $offset++;
    }
}

sub Test::Mimic::Generator::Object::load {
    my ($self, $dir_name) = @_;

    open( my $fh, '<', $dir_name . '/additional_info.rec' ) or die "Could not open file: $!";
    
    my $recorded_data;
    {
        local $/;
        undef $/;
        $recorded_data = <$fh>;
    }

    close($fh) or die "Could not close file: $!";
    
    my $ARRAY1;
    eval($recorded_data);
    $self->[TYPEGLOBS] = $ARRAY1->[0]; #This could change later, so I'm listing all the assigns explicitly.
    $self->[EXTRA] = $ARRAY1->[1];
    $self->[OPERATION_SEQUENCE] = $ARRAY1->[2];
    $self->[READ_DIR] = $dir_name;
}

sub Test::Mimic::Generator::Object::set_isa {

}

sub Test::Mimic::Generator::Object::set_sub {

}

sub Test::Mimic::Generator::Object::set_var {

}

sub Test::Mimic::Generator::Object::enforce_operation_sequence {

}

sub Test::Mimic::Generator::Object::write {
    my ( $self, $write_dir, @packages ) = @_;

    # Either select all recorded packages to write or verify that the requested packages were recorded.
    if ( @packages == 0 ) { # If no packages were selected explicitly...
        @packages = keys %{ $self->[TYPEGLOBS] };
    }
    else {
        for my $package (@packages) {
            if ( ! exists( $self->[TYPEGLOBS]->{$package} ) ) {
                die "The $package package was not found in the loaded recording.";
            }
        }
    }
    
    my $top_level = abs_path();
    
    # Move to the $write_dir/lib directory, creating dirs as needed.
    descend($write_dir);
    descend('lib');
   
    # Consider each package, construct and write the .pm file.
    my $start_path = abs_path();
    for my $package (@packages) {
                
        # Gets the name of the .pm file, descends to where it will be located.
        my @dirs = split( /::/, $package );
        my $filename = pop(@dirs) . '.pm';
        for my $dir (@dirs ) {
            descend($dir);
        }
        
        # Open, write and close the .pm file.
        open( my $fh, '>', $filename ) or die "Could not open file: $!";
        _create($package,  $self->[TYPEGLOBS]->{$package}, $self->[EXTRA]->{$package}, $fh );
        close($fh) or die "Could not close file: $!";

        # Move to the top of our fake library hierarchy.
        chdir($start_path) or die "Could not change the current working directory: $!";
    }

    # Rename the history file so that the controller recognizes it.
    chdir($top_level) or die "Could not change the current working directory: $!"; 
    copy( $self->[READ_DIR] . '/history_from_recorder.rec', $write_dir . '/history_for_playback.rec' )
        or die "Unable to copy file: $!";
    # NOTE: In the future we may modify the contents of the file as well.
}

{ 
    # A few useful constant maps.
    my %TYPE_TO_SIGIL = ( 'ARRAY' => '@', 'HASH' => '%', 'SCALAR' => '$' );
    my %TYPE_TO_TIE = (
        'ARRAY'     => 'Test::Mimic::Library::PlayArray',
        'HASH'      => 'Test::Mimic::Library::PlayHash',
        'SCALAR'    => 'Test::Mimic::Library::PlayScalar',
    );

    sub _create {
        my ( $package, $pseudo_symbol_table, $extra, $fh ) = @_;

        my $header_code = join( "\n",
            'package ' . $package  . ';',
            '',
            'use strict;',
            'use warnings;',
            '',
            'BEGIN {',  #TODO: Check to see if Test::Mimic is loaded, allow requiring fake pack directly etc.
            '    Test::Mimic::prepare_for_use();',
            '}',
            '',
            'use Scalar::Util;',
            '',
            'use Test::Mimic::Library qw< execute get_references HISTORY >;',
            'use Test::Mimic::Library::PlayScalar;',
            'use Test::Mimic::Library::PlayArray;',
            'use Test::Mimic::Library::PlayHash;',
            '',
            '',
        );
        print $fh $header_code;

        # Create code to tie package variables.
        my $package_var_code = join( "\n",
            'BEGIN {',
            '    my $references = get_references();',
            '',
        );
        for my $typeglob ( keys %{$pseudo_symbol_table} ) {

            my $full_name = $package . $typeglob;

            # Tie the current typeglob
            my %slots = %{ $pseudo_symbol_table->{$typeglob} };
            delete $slots{'CODE'};
            # NOTE: You may (some day) need to delete other types too.
            for my $type ( keys %slots ) {
                $package_var_code .= "\n" . '    tie( '
                    . $TYPE_TO_SIGIL{$type} . $package . '::' . $typeglob # Full name including sigil
                    . ', q<' . $TYPE_TO_TIE{$type} 
                    . '>, $references->['
                    . $pseudo_symbol_table->{$typeglob}->{$type}->[DATA]    # Index for the reference, ...->[ENCODE_TYPE]
                                                                            # must be VOLATILE. Check?
                    . ']->[HISTORY] );';
            }
        }
        $package_var_code .= "\n" . '}' . "\n\n";
        print $fh $package_var_code;

        my @ancestors = %{ $extra->{'ISA'} };
        my $isa_code = join( "\n",
            '{',
            '    my %ancestors = qw< ' . "@ancestors" . ' >;', # Interpolation is needed here.
            '',
            '    sub isa {',
            '        my ( $self, $type ) = @_;',
            '',    
            '        if ( Scalar::Util::reftype($self) ) {',
            '            my $name = Scalar::Util::blessed($self);',
            '            if ($name) {',
            '                return exists( $ancestors{$name} );',
            '            }',
            '            else {',
            '                return ();',
            '            }',
            '        }',
            '        else {',
            '            return exists( $ancestors{$self} );',
            '        }',
            '    }',
            '}',
            '',
            '',
        );
        # TODO: Make this dependent on user options.
        print $fh $isa_code;

        # Create code for user defined subroutines.
        for my $typeglob ( keys %{$pseudo_symbol_table} ) {
            if ( exists( $pseudo_symbol_table->{$typeglob}->{'CODE'} ) ) {
                my $sub_code = '{' . "\n";  # Of course, I could say "{\n". I am being overly verbose in an
                                            # attempt to very explicitly separate out strings that
                                            # interpolate. This is a problem because the perl code that I am
                                            # writing often uses scalars that could be accidentally
                                            # interpolated. If I come back to this line and add a scalar (or
                                            # array) I don't want it to bite me.

                # Create a list of lines of code for the behavior hash.
                my $behavior_code = stringify( $pseudo_symbol_table->{$typeglob}->{'CODE'} );
                my @behavior_lines = split( /\n/, $behavior_code );
                $behavior_lines[0] =~ s/^.*?=/my \$behavior = /; # Change name of dumped hash.
                
                for my $line (@behavior_lines) {
                    $sub_code .= '    ' . $line . "\n";
                }

                $sub_code .= join( "\n",
                    '',
                    '    sub ' . $typeglob . ' {',
                    '        return execute( q<' . $package . '>, q<' . $typeglob . '>, $behavior, \@_ );',
                    '    }',
                    '}',
                    '',
                    '',
                );

                print $fh $sub_code;
            }
        }
    }
}

1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

Test::Mimic::Generator - Perl extension for blah blah blah

=head1 SYNOPSIS

  use Test::Mimic::Generator;
  blah blah blah

=head1 DESCRIPTION

Stub documentation for Test::Mimic::Generator, created by h2xs. It looks like the
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

Brendan Roof, E<lt>broof@whitepages.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by Brendan Roof.

Made possible by a generous contribution from WhitePages, Inc.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.8 or,
at your option, any later version of Perl 5 you may have available.


=cut
