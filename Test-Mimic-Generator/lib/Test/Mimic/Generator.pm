package Test::Mimic::Generator;

use 5.006001;
use strict;
use warnings;

use Data::Dump::Streamer qw<:undump>;
use Test::Mimic::Recorder;
use Cwd qw<abs_path>;

our $VERSION = 0.001_001;

sub get_object_package {
    my ($class) = @_;
    return $class . '::Object';
}

sub new {
    my ($class) = @_;
    return bless( [], $class->GetObjectPackage() );
}

package Test::Mimic::Generator::_Implementation;

BEGIN {
    my $offset = 0;
    for my $field ( qw< REFERENCES TYPEGLOBS EXTRA OPERATION_SEQUENCE > ) {
        eval("sub $field { return $offset; }");
        $offset++;
    }
}

sub Test::Mimic::Generator::Object::load {
    my ($self, $file_name) = @_;

    open( my $fh, '<', $file_name ) or die "Could not open file: $!";
    
    my $recorded_data;
    {
        local $/;
        undef $/;
        $recorded_data = <$fh>;
    }

    close($fh) or die "Could not close file: $!";
    
    my $ARRAY1;
    eval($recorded_data);
    $self->[REFERENCES] = $ARRAY1->[0]; #This could change later, so I'm listing all the assigns explicitly.
    $self->[TYPEGLOBS] = $ARRAY1->[1];
    $self->[EXTRA] = $ARRAY1->[2];
    $self->[OPERATION_SEQUENCE] = $ARRAY1->[3];
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
    
    # Move to the $write_dir directory, creating if needed.
    _descend($write_dir);
   
    # Consider each package, construct and write the .pm file.
    my $start_path = abs_path();
    for my $package (@packages) {
                
        # Gets the name of the .pm file, descends to the location where it will be located.
        my @dirs = split( /::/, $packages );
        my $filename = pop(@dirs) . '.pm';
        for my $dir (@dirs ) {
            _descend($dir);
        }
        
        # Open, write and close the .pm file.
        open( my $fh, '>', $filename ) or die "Could not open file: $!";
        _create($package,  $self->[TYPEGLOBS]->[$package], $self->[EXTRA]->[$package], $fh );
        close($fh) or die "Could not close file: $!";

        # Move to the top of our fake library hierarchy.
        chdir($start_path) or die "Could not change the current working directory: $!";
    }


}

# Changes the current working directory to $dir. If $dir does not exist then it will be created.
# If it exists, but it is not a directory or any other error occurs _descend will die.
sub _descend {
    my ($dir) = @_;

    # Move to the $dir directory, creating if needed.
    if  ( -e $dir ) {
        if ( ! ( -d $dir ) ) {
            die "$dir exists, but it is not a directory.";
        }
    }
    else {
        mkdir( $dir ) or die "Could not create directory: $!";
    }
    chdir($dir) or die "Could not change the current working directory: $!";
}

sub _create {
    my ( $package, $pseudo_symbol_table, $extra, $fh ) = @_;

    my @ancestors = @{ $extra->{'ISA'} };
    my $header = join( "\n",
        "package $package;",
        '',
        'use strict;',
        'use warnings;',
        '',
        'use Scalar::Util;',
        'use Test::Mimic;', # Will this cause problems?
        'use Test::Mimic::Recorder;',
        '',
        'sub isa {',
        '    my ( $self, $type ) = @_;',
        '',
        "    my %ancestors = qw( @ancestors );",
        '',    
        '    if ( Scalar::Util::reftype($self) ) {',
        '        my $name = Scalar::Util::blessed($self);',
        '        if ($name) {',
        '            return exists( $ancestors{$name} );',
        '        }',
        '        else {',
        '            return ();',
        '        }',
        '    }',
        '    else {',
        '        return exists( $ancestors{$self} );',
        '    }',
        '}',
    );

    my $

    my $package_var_code = join( "\n",
        'BEGIN {',
        '    for 

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
